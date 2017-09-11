#define throw call Exception$throw
/**
  * Defining this symbol allows to disable the exception handling. The result is that any
  * exception which is thrown cannot be caught whatsoever.
  */
#ifndef disableExceptions
/**
  * Implement the commands necessary for structured exception handling
  */
#  define try \
   if(ExceptionHandler$registerTryBlock() == regTry) then; \
      block; \
         type(TryBlockHandler) :: $exc$tbh; \
         logical, parameter :: $exc$inTry = .true.
/**/
#  define finally \
         call ExceptionHandler$prepareLeaveTryBlock(); \
      endBlock; \
   endIf; \
   associate($exc$jumpBack => ExceptionHandler$deregisterTryBlock()); \
      select case(ExceptionHandler$registerTryBlock()); \
         case(regTry); block; \
            type(TryBlockHandler) :: $exc$tbh; \
            logical, parameter :: $exc$inTry = .true.
/**/
#  ifdef _DEBUG
#     define endFinally \
            call ExceptionHandler$prepareLeaveTryBlock(); \
         endBlock; \
         case(regThrow); \
            call Exception$throwFromHandler($exc$jumpBack); \
         case(regJump); \
         case default; \
            raiseAssertHelp(__FILE__, __LINE__); \
      endSelect; \
      call Exception$deregisterFromHandler($exc$jumpBack); \
   endAssociate
#  else
#     define endFinally \
               call ExceptionHandler$prepareLeaveTryBlock(); \
            endBlock; \
         case(regThrow); \
            call Exception$throwFromHandler($exc$jumpBack); \
      endSelect; \
      call Exception$deregisterFromHandler($exc$jumpBack); \
   endAssociate
#  endif
/**/
#  define except(E) \
         call ExceptionHandler$prepareLeaveTryBlock(); \
      endBlock; \
   endIf; \
   block; \
      class(Exception), pointer :: $exc$e; \
      call Exception$getException($exc$e); \
      associate($exc$jumpBack => ExceptionHandler$deregisterTryBlock()); \
         if(associated($exc$e)) then; \
            select case(ExceptionHandler$registerTryBlock()); \
               case(regTry); block; \
                  type(TryBlockHandler) :: $exc$tbh; \
                  logical, parameter :: $exc$inTry = .true.; \
                  select type(E => $exc$e); \
                     class default; \
                        call Exception$throw($exc$e)
/**/
#  ifdef _DEBUG
#     define endExcept \
                  endSelect; \
                  call ExceptionHandler$prepareLeaveTryBlock(); \
               endBlock; \
               case(regThrow); \
                  call Exception$deallocateIfInactive($exc$e); \
                  call Exception$throwFromHandler($exc$jumpBack); \
               case(regJump); \
               case default; \
                  associate($exc$tmp => ExceptionHandler$deregisterTryBlock()); \
                  endAssociate; \
                  call Exception$deallocate($exc$e); \
                  raiseAssertHelp(__FILE__, __LINE__); \
            endSelect; \
            call Exception$deallocate($exc$e); \
            call Exception$deregisterFromHandler($exc$jumpBack); \
         else; \
            call ExceptionHandler$jumpBack($exc$jumpBack); \
         endIf; \
      endAssociate; \
   endBlock
#  else
#     define endExcept \
                  endSelect; \
                  call ExceptionHandler$prepareLeaveTryBlock(); \
               endBlock; \
               case(regThrow); \
                  call Exception$deallocateIfInactive($exc$e); \
                  call Exception$throwFromHandler($exc$jumpBack); \
            endSelect; \
            call Exception$deallocate($exc$e); \
            call Exception$deregisterFromHandler($exc$jumpBack); \
         else; \
            call ExceptionHandler$jumpBack($exc$jumpBack); \
         endIf; \
      endAssociate; \
   endBlock
#  endif
/**
  * We need to redefine all statement that can affect execution flow. They should first
  * take care of executing the finally chain. If you want to use the statements without
  * this substiution since you know for sure it does not violate any try bounds, use
  * mixed case.
  */
#  define return \
   if($exc$inTry) then; \
      call ExceptionHandler$registerAbnormalFlow(); \
   endIf; \
   return
#  define cycle \
   if($exc$inTry) then; \
      call ExceptionHandler$registerAbnormalFlow(); \
   endIf; \
   cycle
#  define exit \
   if($exc$inTry) then; \
      call ExceptionHandler$registerAbnormalFlow(); \
   endIf; \
   exit
#  define goto \
   if($exc$inTry) then; \
      call ExceptionHandler$registerAbnormalFlow(); \
   endIf; \
   goto
#else
/**
  * Implement the macros in dummy form, such that the syntactic structure still exists,
  * but not commands are emitted to the handler.
  */
#  define try \
   block
#  define finally \
   endBlock; \
   block
#  define endFinally \
   endBlock; \
#  define except(E) \
   endBlock; \
   block; \
      class(Exception), pointer :: $exc$e; \
      if(.false.) then; \
         select type(E => $exc$e); \
            class default;
#  define endExcept \
         endSelect; \
      endIf; \
   endBlock
#endif
/**
  * Provide convenience command
  */
#define try2 try; try
#define raise call Exception$throw($exc$e)
/**
  * Make all the commands available for those who prefer upper case.
  */
#  define THROW throw
#  define TRY try
#  define FINALLY finally
#  define ENDFINALLY endFinally
#  define endfinally endFinally
#  define EXCEPT(E) except(E)
#  define ENDEXCEPT endExcept
#  define endexcept endExcept
#  define RETURN return
#  define CYCLE cycle
#  define EXIT exit
#  define GOTO goto
#  define TRY2 try2
#  define RAISE raise
/**
  * And the same for starting with a capital letter.
  */
#  define Throw throw
#  define Try try
#  define Finally finally
#  define EndFinally endFinally
#  define Endfinally endFinally
#  define Except(E) except(E)
#  define EndExcept endExcept
#  define Endexcept endExcept
#  define Return return
#  define Cycle cycle
#  define Exit exit
#  define Goto goto
#  define Try2 try2
#  define Raise raise
/**
  * If you want to use the original Fortran directives without wrapping, use weird case.
  * For example, you might capitalize the last letter only: returN, cyclE, exiT, gotO
  */