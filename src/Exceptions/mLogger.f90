module LogModule
   use, intrinsic :: iso_fortran_env, only : stderr => error_unit
                                          
   implicit none        
   
   integer, public :: LogVerbosity = 0 
   
   public :: logEvent
   
   contains
      !> Writes a message to stderr with a certain level of verbosity.
      !! @param character(*), intent(in) :: AMessage
      !!    The message to be written
      !! @param integer, optional, intent(in) :: AVerbosity
      !!    The level of verbosity of the message. If this value is greater
      !!    than ExceptionBase$LogVerbosity, the message will be suppressed. If
      !!    the parameter is left out, the message is forced to be written.
      subroutine logEvent(AMessage, AVerbosity)
         character(*), intent(in) :: AMessage
         integer, optional, intent(in) :: AVerbosity
         
         if((.not. present(AVerbosity)) .or. &
            (AVerbosity <= LogVerbosity)) then
            write(stderr, "(A)"), AMessage
         end if
      end subroutine
end module