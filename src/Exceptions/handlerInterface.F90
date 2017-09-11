interface
   !> Marks the beginning of a try-block. Anything within this block is protected
   !! against exception raising.
   !! Do not call this function directly. Use the preprocessor macros.
   !! @return integer(kind(regTry))
   !!    regTry is returned when this function is first called; regThrow is returned when
   !!    execution passed back to this function and all registers were restored due to an
   !!    exception. regJump is returned when execution passed back to this function due
   !!    to a control flow statement without restoring any registers.
   function ExceptionHandler$registerTryBlock() bind(C, name="registerTryBlock")
      import regTry

      integer(kind(regTry)) :: ExceptionHandler$registerTryBlock
   end function

   !> Jumps back to the last address stored by registerTryBlock, returning regThrow from
   !! this function. If there was no block registered, do nothing. Execution will only
   !! return in the latter case.
   !! Do not call this function. It is automatically called by Exception$throw.
   subroutine ExceptionHandler$throwException() bind(C, name="throwException")
   end subroutine

   !> Registers a back-jump point before a control flow changing statement. Must not be
   !! called when there is no enclosing try statement available.
   !! Do not call this function directly. Use the preprocessor macros.
   subroutine ExceptionHandler$registerAbnormalFlow() &
      bind(C, name="registerAbnormalFlow")
   end subroutine

   !> Prepares the normal exit of a try block; hence, this must be the very last
   !! statement of the try block. Any jumps registered so far are freed, since they did
   !! not lead outside the block.
   !! Do not call this function directly. Use the preprocessor macros.
   subroutine ExceptionHandler$prepareLeaveTryBlock() &
      bind(C, name="prepareLeaveTryBlock")
   end subroutine

   !> Checks whether a jump is pending and executes it. The jump is not deregistered.
   !! In this case, excution will not return.
   !! Do not call this function. It is automatically called by the TryBlockHandler.
   subroutine ExceptionHandler$leaveTryBlock() bind(C, name="leaveTryBlock")
   end subroutine

   !> Deregisters a try block. The try state is freed; if a jump state is available, it
   !! is assigned to the next higher try. The address of the jump state is returned and
   !! must be stored for later reference.
   !! Do not call this function directly. Use the preprocessor macros.
   !! @return integer(int_ptr_kind())
   !!    A pointer to a stateBuf structure, holding a jump state.
   integer(int_ptr_kind()) function ExceptionHandler$deregisterTryBlock() &
      bind(C, name="deregisterTryBlock")
   end function

   !> Calls deregisterTryBlock; if the handler itself triggered a new jump point, this
   !! one replaces the old one (if available). Execution passes to the last jump point or
   !! returns, if there was none.
   !! Do not call this function directly. Use the preprocessor macros.
   !! @param integer(int_ptr_kind())
   !!    The pointer to the stateBuf structure previously obtained by
   !!    deregisterTryBlock.
   subroutine ExceptionHandler$deregisterFromHandler(APreviousState) &
      bind(C, name="deregisterFromHandler")
      !DEC$ ATTRIBUTES REFERENCE :: APreviousState
      ! Passing by value does not work for some reason; in case this is fixed in the
      ! future, make the reference explicit such that we don't break compatiblity
      integer(int_ptr_kind()), intent(in) :: APreviousState
   end subroutine

   !> Calls deregisterTryBlock, clears any jump point (if available) and calls
   !! throwException.
   !! Do not call this function directly. Use the preprocessor macros.
   !! @param integer(int_ptr_kind())
   !!    The pointer to the stateBuf structure previously obtained by
   !!    deregisterTryBlock.
   subroutine ExceptionHandler$throwFromHandler(APreviousState) &
      bind(C, name="throwFromHandler")
      !DEC$ ATTRIBUTES REFERENCE :: APreviousState
      ! Passing by value does not work for some reason; in case this is fixed in the
      ! future, make the reference explicit such that we don't break compatiblity
      integer(int_ptr_kind()), intent(in) :: APreviousState
   end subroutine

   !> Performs a direct jump to the given state and frees it, if it was not assigned
   !! to a higher-level try structure.
   !! Do not call this function directly. Use the preprocessor macros.
   !! @param integer(int_ptr_kind())
   !!    The pointer to the stateBuf structure previously obtained by
   !!    deregisterTryBlock.
   subroutine ExceptionHandler$jumpBack(AToState) bind(C, name="jumpBack")
      !DEC$ ATTRIBUTES REFERENCE :: AToState
      ! Passing by value does not work for some reason; in case this is fixed in the
      ! future, make the reference explicit such that we don't break compatiblity
      integer(int_ptr_kind()), intent(in) :: AToState
   end subroutine

   !> Retrieves a pointer to the class name of an exception object. This relies on the
   !! internal structure which Intel Fortran uses to implement polymorphism.
   !! @param class(Exception), intent(in) :: AException
   !!    A valid polymorphic variable
   !! @param character(1), pointer :: PClassName
   !!    This will be set to point to the first character of the class name. The pointer
   !!    must not be deallocated, as it belongs to the object itself. The string is null-
   !!    terminated. The returned class name is the internal one, which is upper case and
   !!    contains the scope in which the class was defined, separated by #.
   subroutine ExceptionHandler$getClassName(AException, PClassName)
      ! We need to be sure of the calling convention, regardless of what kind of
      ! optimization is used. But we cannot use bind(C), since polymorphic variables are
      ! not interoperable (and in fact, if the compiler changes, everything will break
      ! down!). The naming convention is such that on Windows x86, exported functions are
      ! prefixed by an underscore, while they are not on x64 and Linux.
#if _M_IX86=700
      !DEC$ ATTRIBUTES C, ALIAS:'_getClassName' :: ExceptionHandler$getClassName
#else
      !DEC$ ATTRIBUTES C, ALIAS:'getClassName' :: ExceptionHandler$getClassName
#endif
      ! Knowledge of the polymorphic properties crucially depends on passing the
      ! arguments as references themselves.
      !DEC$ ATTRIBUTES REFERENCE :: AException, PClassName
      import Exception, c_char

      class(Exception), intent(in) :: AException
      character(kind=c_char, len=1), pointer :: PClassName
   end subroutine
end interface