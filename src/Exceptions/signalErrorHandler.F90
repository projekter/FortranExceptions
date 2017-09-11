!> Default signal handler. Changes signals into exceptions.
!! Do not call this function.
!! @param integer(2), intent(in) :: ASignal
!!    The signal code
!!    SIG$ABORT, SIG$FPE, SIG$ILL, SIG$INT, SIG$SEGV, SIG$TERM
!! @return integer(4)
!! @throws EAbort
!! @throws EExternalException
!! @throws EExternal
recursive integer(4) function Exception$sigDefault(ASignal) result(Res)
   !DIR$ ATTRIBUTES C :: Exception$sigDefault
   integer(2), intent(in) :: ASignal

   ! Before this handler is called, _XcptFilter resets the exception action table; hence,
   ! we need to call signalQQ again if we want to catch the next occurrence.
   if(signalQQ(ASignal, Exception$sigDefault) == SIG$ERR) then
      call logEvent("Unable to re-install signal handler,")
   end if
   ! These instructions are all potentially dangerous, because they allocate a
   ! new object on the heap, which should not be done in a signal handler. This
   ! could be circumvented by pre-allocating the necessary objects at the start
   ! of the program. However, by casting signals into exceptions, they are made
   ! catchable, and certainly the user will do some heap operations. Perhaps it
   ! would be better not to allow catching any of these signals? It cannot be
   ! assumed that a segmentation fault is catchable.
   select case(ASignal)
      case(SIG$ABORT)
         throw(EAbort())
      case(SIG$ILL)
         throw(EExternal("Illegal instruction"))
      case(SIG$INT)
         throw(EControlC("Execution interrupted via Ctrl + C"))
      case(SIG$SEGV)
         throw(EExternal("Segmentation fault"))
      case(SIG$TERM)
         throw(EExternal("Termination request"))
      case default
         ! The kind of the exception constructor is different; ifort does not convert
         ! automatically, but instead produces an internal error if int() is not used.
         throw(EExternalException("External exception", int(ASignal)))
   end select
   Res = 1
end function

!> Default float-pointing signal handler. Changes FPU signals into exceptions.
!! Do not call this function.
!! @param integer(2), intent(in) :: ASignal
!!    Must be SIG$FPE
!! @param integer(2), intent(in) :: AFPEType
!!    The type of float pointing signal. One of the FPE$ constants.
!! @throws EInvalidOp
!! @throws EZeroDivide
!! @throws EOverflow
!! @throws EUnderflow
!! @throws EInexact
!! @throws EMathError
!! @throws EExternalException
!! @return integer(4)
recursive integer(4) function Exception$sigFPE(ASignal, AFPEType) result(Res)
   !DIR$ ATTRIBUTES C :: Exception$sigFPE
   integer(2), intent(in) :: ASignal, AFPEType

   assert(ASignal == SIG$FPE)
   ! Before this handler is called, _XcptFilter resets the exception action table; hence,
   ! we need to call signalQQ again if we want to catch the next occurrence.
   if(signalQQ(ASignal, Exception$sigFPE) == SIG$ERR) then
      call logEvent("Unable to re-install signal handler,")
   end if
   select case(AFPEType)
      case(FPE$INVALID)
         throw(EInvalidOp("Invalid floating point operation"))
      case(FPE$DENORMAL)
         throw(EMathError("Floating point error: Denormal"))
      case(FPE$ZERODIVIDE)
         throw(EZeroDivide("Floating point division by zero"))
      case(FPE$OVERFLOW)
         throw(EOverflow("Floating point overflow"))
      case(FPE$UNDERFLOW)
         throw(EUnderflow("Floating point underflow"))
      case(FPE$INEXACT)
         throw(EInexact("Floating point error: Inexact"))
      case(FPE$UNEMULATED)
         throw(EMathError("Floating point error: Unemulated"))
      case(FPE$SQRTNEG)
         throw(EMathError("Floating point error: Sqrt negative"))
      case(FPE$STACKOVERFLOW)
         throw(EInvalidOp("Floating point error: Stack overflow"))
      case(FPE$STACKUNDERFLOW)
         throw(EInvalidOp("Floating point error: Stack underflow"))
      case(FPE$EXPLICITGEN)
         throw(EMathError("Floating point error: User-generated error (raiseQQ)"))
      case default
         ! The kind of the exception constructor is different; ifort does not convert
         ! automatically, but instead produces an internal error if int() is not used.
         throw(EExternalException("External exception: Floating point error", &
            int(AFPEType)))
   end select
   Res = 1
end function