#define expect(x) if(executionStage == x) then; executionStage = executionStage +1; else; print '("Execution stage: ", I0)', executionStage; read *; stop; endIf
#define never() print '("[Never] Execution stage: ", I0)', executionStage; read *; stop;

module TestException
#  include <Exceptions.FPP>

   implicit none

   declareException(ETestError, Exception)

   contains
      implementException(ETestError)
end module

program ExceptionTester
#  include <Exceptions.FPP>
   use TestException

   implicit none

   integer :: executionStage

   print *, "OrdinaryTry"
   executionStage = 0
   call ordinaryTry()
   expect(4)

   print *, "withThrow"
   executionStage = 0
   try
      call withThrow()
         call ExceptionHandler$prepareLeaveTryBlock()
   except(E)
      type is(ETestError)
         expect(3)
   endExcept
   expect(4)

   print *, "withControlFlow"
   executionStage = 0
   call withControlFlow()
   expect(3)

   print *, "withDoubleControlFlow"
   executionStage = 0
   call withDoubleControlFlow()
   expect(7)

   print *, "withControlFlowThrow"
   executionStage = 0
   try
      call withControlFlowThrow()
   except(E)
      type is(ETestError)
         expect(4)
   endExcept
   expect(5)

   print *, "exceptMismatch"
   executionStage = 0
   try
      call exceptMismatch()
   except(E)
      type is(ETestError)
         expect(3)
   endExcept
   expect(4)

   print *, "All tests passed."
   read *

   contains
      subroutine ordinaryTry()
         expect(0)
         try
            expect(1)
         finally
            expect(2)
         endFinally
         expect(3)
      end subroutine

      subroutine withThrow()
         expect(0)
         try
            expect(1)
            throw(ETestError("Error in withThrow"))
            never()
         finally
            expect(2)
         endFinally
         never()
      end subroutine

      subroutine withControlFlow()
         expect(0)
         try
            expect(1)
            return
            never()
         finally
            expect(2)
         endFinally
         never()
      end subroutine

      subroutine withDoubleControlFlow()
         expect(0)
         try
            expect(1)
            try
               expect(2)

               goto 50
               never()
50             expect(3)

               return
               never()
            finally
               expect(4)
               executionStage = 0
               call ordinaryTry()
               expect(4)

               goto 20
            endFinally

            never()
         finally
            expect(5)

            goto 10
         endFinally

         never()
20       never()
10       expect(6)
      end subroutine

      subroutine withControlFlowThrow()
         expect(0)
         try
            expect(1)
            return
            never()
         finally
            expect(2)
            goto 20
            never()
20          expect(3)
            throw(ETestError("Error in withControlFlowThrow"))
            never()
         endFinally
         never()
      end subroutine

      subroutine exceptMismatch()
         expect(0)
         try2
            expect(1)
            throw(ETestError("Error in exceptMismatch"))
         except(E)
            type is(EAccessViolation)
               never()
            class is(EMathError)
               never()
         endExcept
         finally
            expect(2)
         endFinally
         never()
      end subroutine
end program