!> This function is automatically called upon program initialization by the handler
!! library (as Fortran does not provide means to have initialization sections).
!! It allows the exception handler framework to react on exceptions which were not
!! explicitly triggered via throw(), but for example by an invalid operation.
subroutine Exception$startup() bind(C, name="Exception$startup")
   ! Make sure all heap exceptions are properly initialized now; because if they are
   ! called later on, we do not know whether the heap operation of construction will be
   ! possible.
   HeapExceptions$IOS_INSVIRMEM%Message = "severe (41): Insufficient virtual memory"
   HeapExceptions$IOS_INSVIRMEM%Code = 41
   HeapExceptions$IOS_NOMEMFORIO%Message = "severe (98): cannot allocate memory for the &
      &file buffer - out of memory"
   HeapExceptions$IOS_NOMEMFORIO%Code = 98
   HeapExceptions$IOS_F6700%Message = "severe (632): Heap space limit exceeded"
   HeapExceptions$IOS_F6700%Code = 632
   HeapExceptions$IOS_F6717%Message = "severe (672): Out of memory"
   HeapExceptions$IOS_F6717%Code = 672
   HeapExceptions$IOS_F6763%Message = "severe (718): Cannot allocate temporary array -- &
      &out of memory"
   HeapExceptions$IOS_F6763%Code = 718
   HeapExceptions$IOS_F6767%Message = "severe (722): Cannot ALLOCATE scalar POINTER -- o&
      &ut of memory"
   HeapExceptions$IOS_F6767%Code = 722
   HeapExceptions$IOS_F6769%Message = "severe (724): Cannot ALLOCATE POINTER array -- ou&
      &t of memory"
   HeapExceptions$IOS_F6769%Code = 724
   HeapExceptions$IOS_F6772%Message = "severe (727): Cannot ALLOCATE allocatable array -&
      &- out of memory"
   HeapExceptions$IOS_F6772%Code = 727
   HeapExceptions$IOS_F6773%Message = "severe (728): Cannot allocate automatic object --&
      & out of memory"
   HeapExceptions$IOS_F6773%Code = 728
   HeapExceptions$IOS_F6783%Message = "severe (738): Heap storage exhausted"
   HeapExceptions$IOS_F6783%Code = 738

   ! Set signal handlers
   if(signalQQ(SIG$ABORT, Exception$sigDefault) == SIG$ERR) then
      call logEvent("Unable to install ABORT signal handler")
   end if
   if(signalQQ(SIG$FPE, Exception$sigFPE) == SIG$ERR) then
      call logEvent("Unable to install FPE signal handler")
   end if
   if(signalQQ(SIG$ILL, Exception$sigDefault) == SIG$ERR) then
      call logEvent("Unable to install ILL signal handler")
   end if
   if(signalQQ(SIG$INT, Exception$sigDefault) == SIG$ERR) then
      call logEvent("Unable to install INT signal handler")
   end if
   if(signalQQ(SIG$SEGV, Exception$sigDefault) == SIG$ERR) then
      call logEvent("Unable to install SEGV signal handler")
   end if
   if(signalQQ(SIG$TERM, Exception$sigDefault) == SIG$ERR) then
      call logEvent("Unable to install TERM signal handler")
   end if

   ! Set RTL handler
   if(.not. establishQQ(Exception$establishHandler, 0)) then
      call logEvent("Unable to install RTL error detection handler")
   end if
end subroutine

!> This shutdown routine will finalize all the allocated strings of our heap exception
!! error messages; since global and module variables are not finalized, this is necessary
!! if we don't want a profiler to report any memory leaks. In principle, it is not
!! required since the operating system frees the memory anyway; however, if a developer
!! wants to find memory leaks of _their_ code, _our_ exception library should not be
!! responsible for any of those.
!! The finalization will happen upon normal termination and if an exception ended the
!! application. It cannot be carried out if "stop" or "error stop" is directly used.
subroutine Exception$finalize() bind(C, name="Exception$finalize")
   ! Just to be sure we did not already finalize.
   if(allocated(HeapExceptions$IOS_INSVIRMEM%Message)) then
      deallocate(HeapExceptions$IOS_INSVIRMEM%Message, &
         HeapExceptions$IOS_NOMEMFORIO%Message, &
         HeapExceptions$IOS_F6700%Message, &
         HeapExceptions$IOS_F6717%Message, &
         HeapExceptions$IOS_F6763%Message, &
         HeapExceptions$IOS_F6767%Message, &
         HeapExceptions$IOS_F6769%Message, &
         HeapExceptions$IOS_F6772%Message, &
         HeapExceptions$IOS_F6773%Message, &
         HeapExceptions$IOS_F6783%Message)
   end if
end subroutine