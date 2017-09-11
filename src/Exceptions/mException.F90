! This file intends to bring proper exception handling to FORTRAN. Within any procedure,
! the Exception$throw subroutine may be called with a valid exception object pointer.
! This causes the execution control flow to be interrupted. Unless the exception is
! caught, the program is ended. By enclosing a block of statements with try-except, it is
! possible to catch raised exceptions of a certain type. The same holds true for
! try-finally, which allows to execute a finalization block regardless of what exception
! was raised inside.
! To make clear that the commands introduced in this module - which require linking
! against the external ExceptionHandler library - are different from ordinary Fortran
! functions, they should not be called directly. Instead, include the Exceptions.FPP in a
! position where you would place USE direcives. Then, adapt the following example:
! try2
!    throw(Exception())
! except(E)
!    type is (EAccessViolation)
!       print *, "We got an access violation: ", E%Message
! endExcept
! finally
!    print *, "This is a finally block"
! endFinally
! You may leave out the except or the finally block and then have to replace try2 by try.

module ExceptionModule
   use ifcore
   use ifport
   use ifestablish
   use foriosdef
#  define exceptions$core
#  include <Exceptions.FPP>
   use LogModule
   use, intrinsic :: iso_fortran_env, only: stderr => error_unit
   use, intrinsic :: iso_c_binding

   implicit none

   ! Internals
   enum, bind(C)
      enumerator :: regTry = 0, regThrow = 1, regJump = 2
   end enum

   type, public :: TryBlockHandler
      logical, private :: FDone = .false.
   contains
      final :: TryBlockHandler$leaveTryBlock
   end type

#  include "handlerInterface.F90"

   class(Exception), pointer, private :: Exception$CurrentException => null()
   !$OMP THREADPRIVATE(Exception$CurrentException)

   integer, parameter, public :: $exc$inTry = .false.
   type(EOutOfMemory), target, private :: &
      HeapExceptions$IOS_INSVIRMEM, HeapExceptions$IOS_NOMEMFORIO, &
      HeapExceptions$IOS_F6700, HeapExceptions$IOS_F6717, HeapExceptions$IOS_F6763, &
      HeapExceptions$IOS_F6767, HeapExceptions$IOS_F6769, HeapExceptions$IOS_F6772, &
      HeapExceptions$IOS_F6773, HeapExceptions$IOS_F6783

   private :: &
      ! Extern
      ExceptionHandler$throwException, ExceptionHandler$leaveTryBlock, &
      ExceptionHandler$deregisterFromHandler, ExceptionHandler$throwFromHandler, &
      ExceptionHandler$getClassName, &
      ! Signal handlers
      Exception$sigDefault, Exception$sigFPE, Exception$establishHandler, &
      ! Error handler
      Exception$defaultHandler, &
      ! Misc
      Exception$throwHeapException, Exception$startup, Exception$finalize
   public :: &
      ! Extern - should not be public, but the preprocessor needs them
      ExceptionHandler$registerTryBlock, ExceptionHandler$registerAbnormalFlow, &
      ExceptionHandler$prepareLeaveTryBlock, ExceptionHandler$deregisterTryBlock, &
      ExceptionHandler$jumpBack, &
      ! Except helpers - should not be public, but the preprocessor needs them
      Exception$deregisterFromHandler, Exception$throwFromHandler, &
      Exception$getException, Exception$throw, Exception$deallocate, &
      Exception$deallocateIfInactive, &
      ! Functions that may be called from the outside
      Exception$getClassName
   contains
      subroutine TryBlockHandler$leaveTryBlock(this)
         type(TryBlockHandler) :: this

         if(.not. this%FDone) then
            this%FDone = .true.
            call ExceptionHandler$leaveTryBlock()
         end if
      end subroutine

#     include "signalErrorHandler.F90"

#     include "rtlErrorHandler.F90"

      !> Triggers the default error handler: An error message is written to stdErr, a
      !! traceback is generated and the program is terminated. Requires that an exception
      !! is currently active.
      !! Do not call this function directly.
      subroutine Exception$defaultHandler()
         integer :: Code
         character(:), allocatable :: ExceptionClassName

         ! If there was no try block available, we need to assume that the error was
         ! fatal and terminate the program.
         Code = Exception$CurrentException%Code
         call Exception$getClassName(Exception$CurrentException, ExceptionClassName)
         write(stdErr, "(A)"), "[Unhandeled Exception]"
         write(stdErr, '("[", A, ", Code ", I0, "]: ", A)'), ExceptionClassName, Code, &
            Exception$CurrentException%Message
         deallocate(ExceptionClassName)
         call Exception$deallocate(Exception$CurrentException)
         ! Since we are terminating in the brutal way, the finalization handler will not
         ! be called.
         call Exception$finalize()
#ifdef _DEBUG
         call tracebackQQ("Traceback:", user_exit_code = -1)
         ! When in debug mode, the program might not have been called from the command
         ! line (as is common under Windows); in this case, wait for the programmer to
         ! hit enter before the console disappears and no-one known what happened.
         print "(A)", "Press [Enter] to terminate the application."
         read *
#else
         call tracebackQQ("Traceback:", user_exit_code = Code)
#endif
         error stop ! Also for non-debug mode, just in case the error code was -1
      end subroutine

      !> Marks the end of a handler block and must be called if the handler is left
      !! normally or via a jump, which in this case will be performed.
      !! Do not call this function directly. Use the preprocessor macros.
      !! @param integer(int_ptr_kind()), intent(in) :: AJumpTo
      !!    The pointer to the stateBuf structure previously obtained by
      !!    deregisterTryBlock.
      subroutine Exception$deregisterFromHandler(AJumpTo)
         integer(int_ptr_kind()), intent(in) :: AJumpTo

         call ExceptionHandler$deregisterFromHandler(AJumpTo)
         ! We did not jump and we did not throw (for else, throwFromHandler would have
         ! been called instead). Do we still have an exception pending?
         if(associated(Exception$CurrentException)) then
            call ExceptionHandler$throwException()
            call Exception$defaultHandler()
         end if
      end subroutine

      !> Must be called after ExceptionHandler$registerTryBlock returned regThrow if the
      !! try block belonged to a handler. The old jump point is freed and execution
      !! passes to the next higher handler. If there is none, the default handler is
      !! invoked.
      !! Do not call this function directly. Use the preprocessor macros.
      !! @param integer(int_ptr_kind()), intent(in) :: AJumpTo
      !!    The pointer to the stateBuf structure previously obtained by
      !!    deregisterTryBlock.
      subroutine Exception$throwFromHandler(AJumpTo)
         integer(int_ptr_kind()), intent(in) :: AJumpTo

         call ExceptionHandler$throwFromHandler(AJumpTo)
         call Exception$defaultHandler()
      end subroutine

      !> Returns the exception object that was caught within a try-block or null if
      !! there was none. After calling the function, the exception is removed from the
      !! list of thrown exceptions.
      !! Do not call this function directly. Use the preprocessor macros instead.
      !! @param class(Exception), pointer, intent(out) :: OTo
      !!    The exception object will be assigned to this allocatable slot.
      subroutine Exception$getException(OTo)
         !DIR$ ATTRIBUTES FORCEINLINE :: Exception$getException
         class(Exception), pointer, intent(out) :: OTo

         OTo => Exception$CurrentException
         nullify(Exception$CurrentException)
      end subroutine

      !> Throws an exception. Execution flow is interrupted and control is passed to the
      !! next higher try-block. If there is none, the program is terminated.
      !! Do not call this function directly. Use the preprocessor macros instead.
      !! @param class(Exception), pointer, intent(in) :: AException
      !!    A pointer to an allocatad exception object that is thrown. This must not be a
      !!    heap exception.
      !!    After calling this function, the allocated object will be managed by the
      !!    exception handler and deallocated when necessary.
      subroutine Exception$throw(AException)
         class(Exception), pointer, intent(in) :: AException

#ifdef _DEBUG
         select type(AException)
            class is(EHeapException)
               raiseAssertHelp(__FILE__, __LINE__)
         end select
#endif
         if(associated(Exception$CurrentException)) then
            ! This should only happen if within a finally block another exception is
            ! thrown
            call Exception$deallocate(Exception$CurrentException)
         end if
         Exception$CurrentException => AException
         call ExceptionHandler$throwException()
         ! If there was a try block, the call will never return, but instead directly
         ! pass execution control to the appropriate parts of the code, so the following
         ! subroutine would not be called.
         call Exception$defaultHandler()
      end subroutine

      !> Throws a heap exception. The exception object must be provided as an immutable
      !! target, which will not be cloned.
      !! @param class(EHeapException), pointer, intent(in) :: AException
      !!    An exception object which is already allocated and which will only be removed
      !!    at the very end of the program by the compiler itself.
      subroutine Exception$throwHeapException(AException)
         class(EHeapException), target, intent(in) :: AException

         if(associated(Exception$CurrentException)) then
            ! This should only happen if within a finally block another exception is
            ! thrown
            call Exception$deallocate(Exception$CurrentException)
         end if
         Exception$CurrentException => AException
         call ExceptionHandler$throwException()
         ! If there was a try block, the call will never return, but instead directly
         ! pass execution control to the appropriate parts of the code, so the following
         ! subroutine would not be called.
         call Exception$defaultHandler()
      end subroutine

      !> Deallocates an exception pointer, unless it is a heap exception.
      !! @param class(Exception), pointer, intent(inout) :: AObj
      !!    The pointer which will become deallocated or dissociated.
      subroutine Exception$deallocate(AObj)
         !DIR$ ATTRIBUTES FORCEINLINE :: Exception$deallocate
         class(Exception), pointer, intent(inout) :: AObj

         select type(dummy => AObj)
            class is(EHeapException)
               nullify(AObj)
            class default
               deallocate(AObj)
         end select
      end subroutine

      !> Deallocates an exception pointer, unless it is currently in use.
      !! @param class(Exception), pointer, intent(inout) :: AObj
      !!    The pointer which will become deallocated or dissociated.
      subroutine Exception$deallocateIfInactive(AObj)
         !DIR$ ATTRIBUTES FORCEINLINE :: Exception$deallocateIfInactive
         class(Exception), pointer, intent(inout) :: AObj

         if(associated(AObj, Exception$CurrentException)) then
            nullify(AObj)
         else
            select type(dummy => AObj)
               class is(EHeapException)
                  nullify(AObj)
               class default
                  deallocate(AObj)
            end select
         end if
      end subroutine

      !> Returns the class name of a polymorphic exception object. This is the class name
      !! the compiler internally uses, which is upper case and contains the scope in
      !! which the class was defined, separated by #.
      !! @param class(Exception), intent(in) :: AException
      !!    A valid polymorphic variable
      !! @param character(:), allocatable, intent(out) :: OClassName
      !!    A slot that will be allocated and filled with the class name
      subroutine Exception$getClassName(AException, OClassName)
         class(Exception), intent(in) :: AException
         character(:), allocatable, intent(out) :: OClassName

         interface
            function strlen(AString) bind(C, name="strlen")
               !DEC$ ATTRIBUTES REFERENCE :: AString
               import c_char, c_size_t

               character(kind=c_char, len=1), intent(in) :: AString
               integer(c_size_t) :: strlen
            end function

            ! This is a function returning a pointer, but we don't care about the result.
            ! Since the result is passed in the volatile register EAX/RAX, this is not a
            ! problem.
            subroutine memcpy(ADestination, ASource, ANum) bind(C, name="memcpy")
               import c_char, c_size_t

               character(kind=c_char, len=1), value, intent(in) :: ADestination
               character(kind=c_char, len=1), intent(in) :: ASource
               integer(c_size_t), value :: ANum
            end subroutine
         end interface

         character(kind=c_char, len=1), pointer :: Buffer
         integer(c_size_t) :: L

         call ExceptionHandler$getClassName(AException, Buffer)
         L = strlen(Buffer)
         allocate(character(kind=c_char, len=L) :: OClassName)
         ! Don't use a Fortran routine, since this would involve range checking. The
         ! range is only one character, so just copy without asking too much.
         call memcpy(OClassName, Buffer, L * c_char)
      end subroutine

#     include "exceptionInitialization.F90"
end module