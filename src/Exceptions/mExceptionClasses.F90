#include <ExceptionsDeclaration.FPP>

module ExceptionClassesModule
   implicit none

   ! The following hierarchy of exceptions was designed following FreePascal where
   ! appropriate. The descriptions of the exception types were extracted from the
   ! FreePascal reference and adjusted, where necessary. Note that sometimes the Delphi
   ! documentation might give a better insight; however, its license does not allow it to
   ! be used here. Check it out manually at docwiki.embarcadero.com.

   ! sysutilh.inc

   !> Base class of all exceptions
   type, public :: Exception
      ! These should be protected, in the sense that any descendent class can access them
      ! (as it is necessary if new exception classes are defined outside of this module,
      ! which need a proper initialization). Since Fortran does not support protected
      ! visibility, we need to make them public.
      character(:), allocatable, public :: Message
      integer, public :: Code = -1
   end type
   interface Exception
      procedure Exception$create
   end interface
   private :: Exception$create

   !> External Exception
   !! EExternal is the base exception for all external exceptions, as reported by the CPU
   !! or operating system, as opposed to internal exceptions, which are raised by the
   !! program itself.
   declareException(EExternal, Exception)

   !> Integer operation error
   !! EIntError is used when the operating system or CPU signals an integer operation
   !! error, e.g., an overflow.
   declareException(EIntError, EExternal)
   !> Division by zero error
   !! EDivByZero is used when the operating system or CPU signals a division by zero
   !! error.
   declareException(EDivByZero, EIntError)
   !> Range check error
   !! ERangeError is raised by the exception handler framework runtime library if range
   !! checking is on, and a range check error occurs. By default, this is enabled in
   !! Debug mode and disabled in Release mode.
   declareException(ERangeError, EIntError)
   !> Integer overflow error
   !! EIntOverflow is used when the operating system or CPU signals a integer overflow
   !! error.
   declareException(EIntOverflow, EIntError)

   !> Mathematical error
   !! EMathError is used when the operating system or CPU signals a floating point
   !! overflow error.
   declareException(EMathError, EExternal)
   !> Invalid operation
   !! EInvalidOp is raised when an invalid operation is encountered.
   declareException(EInvalidOp, EMathError)
   !> Division by zero error
   !! EZeroDivide occurs when a float division by zero occurs.
   declareException(EZeroDivide, EMathError)
   !> Float overflow error
   !! EOverflow occurs when a float operation overflows. (i.e. result is too big to
   !! represent).
   declareException(EOverflow, EMathError)
   !> Float underflow error
   !! EOverflow occurs when a float operation underflows (i.e. result is too small to
   !! represent).
   declareException(EUnderflow, EMathError)
   !> Inexact floating-point arithmetic
   !! EInexact is raised when a floating-point arithmetic or conversion operation gives
   !! a result that differs from the mathematically exact result. This trap is reported
   !! if the rounded result of an IEEE operation is not exact.
   declareException(EInexact, EMathError)

   !> Input/Output error
   !! EInOutError is raised when a IO routine returns an error.
   declareException(EInOutError, Exception)

   !> Heap memory error
   !! EHeapMemoryError is raised when an error occurs in heap (dynamically allocated)
   !! memory.
   !! Note: The exception library allocates these exception at the start of the
   !! application and frees them at the very end. Do not raise these exception manually!
   type, abstract, extends(Exception) :: EHeapException
   end type

   !> External exception
   !! EExternalException is raised an unknown external exception is raised.
   declareException(EExternalException, EExternal)
   !> Syntax errors in environment variables
   !! EInvalidSyntax is raised when the runtime library detects invalid content in an
   !! environment variable.
   declareException(EInvalidSyntax, EExternal)

   !> Out of memory error
   !! EOutOfMemory occurs when memory can no longer be allocated on the heap. An instance
   !! of EOutOfMemory is allocated on the heap at program startup, so it is available
   !! when needed.
   type, extends(EHeapException) :: EOutOfMemory
   end type

   !> Access Violation error
   !! EAccessViolation is raised when the OS reports an Access Violation, i.e. when
   !! invalid memory is accessed.
   declareException(EAccessViolation, EExternal)
   !> Privileged instruction error
   !! EPrivilege is raised when the OS reports that an invalid instruction was executed.
   declareException(EPrivilege, EExternal)
   !> Stack overflow error
   !! EStackOverflow occurs when the stack has grown too big (e.g. by infinite
   !! recursion).
   declareException(EStackOverflow, EExternal)
   !> Control-C (abort) was pressed on the console.
   !! EControlC is raised when the user has pressed CTRL-C in a console application.
   declareException(EControlC, EExternal)

   !> Conversion error
   !! EConvertError is raised by the various conversion routines in the runtime library
   !! unit. The message will contain more specific error information.
   declareException(EConvertError, Exception)
   !> Invalid format specifier
   !! A Fortran format specifier was invalid or incompatible with a given argument.
   declareException(EFormatError, EInOutError)
   !> Input/output error due to NAMELIST groups.
   declareException(ENameListError, EInOutError)

   !> Self-triggered program termination
   declareException(EAbort, Exception)
   !> Assertion failed error
   !! EAssertionFailed is raised when an application that is compiled with assertions,
   !! encounters an invalid assertion.
   !! Note: To raise this exception, use the assert preprocessor macro. The exception is
   !! raised only in code compiled with the _DEBUG directive.
   declareException(EAssertionFailed, Exception)

   !> Read-only property error
   !! EPropReadOnly is raised when an attempt is made to write to a read-only property.
   declareException(EPropReadOnly, Exception)
   !> Write-only property error
   !! EPropWriteOnly is raised when an attempt is made to read from a write-only
   !! property.
   declareException(EPropWriteOnly, Exception)

   !> EOSErrorOperating system error
   !! EOSError is raised when some Operating System call fails.
   declareException(EOSError, Exception)

   !> Exception raised in case of a not implemented feature.
   !! ENotImplemented can be used to raise an exception when a particular call had been
   !! defined, but was not implemented.
   declareException(ENotImplemented, Exception)

   !> Invalid argument passed to a function
   !! EArgumentException is raised by many character conversion/handling routines to
   !! indicate an erroneous argument was passed to the function (usually indicating an
   !! invalid codepoint in a unicode string).
   declareException(EArgumentException, Exception)
   !> Argument out of valid range passed to a function
   !! EArgumentOutOfRangeException is raised by many character conversion/handling
   !! routines to indicate an erroneous argument was passed to the function (indicating
   !! an invalid character index in a unicode string).
   declareException(EArgumentOutOfRangeException, EArgumentException)
   !> Exception raised when a required argument is NULL.
   !! EArgumentNilException is an exception raised when an argument is NULL when it
   !! should not be NULL.
   declareException(EArgumentNullException, EArgumentException)

   !> Exception raised when a feature is not supported.
   !! ENotSupportedException is an exception raised when a function or procedure is not
   !! supported for a certain platform.
   declareException(ENotSupportedException, Exception)
   !> Exception raised when a file is not found.
   !! EFileNotFoundException is an exception raised when a file is referenced that does
   !! not exist.
   declareException(EFileNotFoundException, Exception)

   !> Invalid operand
   !! Not always, all possible values are allowed as parameters. Use this exception to
   !! report an invalid parameter.
   declareException(EInvalidOpException, Exception)

   !> Error raised when instantiating a class that should not be constructed
   declareException(ENoConstructException, Exception)

   ! classesh.inc
   !> Exception raised when an error occurs during read or write operations on a stream
   !! An EStreamError is raised when an error occurs during reading from or writng to a
   !! stream: Possible causes are
   !! 1. Not enough data is available in the stream.
   !! 2. Trying to seek beyond the beginning or end of the stream.
   !! 3. Trying to set the capacity of a memory stream and no memory is available.
   !! 3. Trying to write to a read-only stream, such as a resource stream.
   !! 4. Trying to read from a write-only stream.
   declareException(EStreamError, Exception)
   !> Exception raised when an error occurred during creation of a file stream.
   !! When the operating system reports an error during creation of a new file, a
   !! EFCreateError is raised.
   declareException(EFCreateError, EStreamError)
   !> Exception raised when an error occurred during creation of a file stream
   !! When the operating system reports an error during the opening of a file, a
   !! EFOpenError is raised.
   declareException(EFOpenError, EStreamError)

   !> Exception raised when an error occurs in lists handling.
   !! If an error occurs in a list methods, then a EListError exception is raised. This
   !! can occur in one of the following cases:
   !! 1. There is not enough memory to expand the list.
   !! 2. The list tried to grow beyond its maximal capacity.
   !! 3. An attempt was made to reduce the capacity of the list below the current element
   !!    count.
   !! 4. An attempt was made to set the list count to a negative value.
   !! 5. A non-existent element of the list was referenced. (i.e. the list index was out
   !!    of bounds)
   !! 6. An attempt was made to move an item to a position outside the list's bounds.
   declareException(EListError, Exception)

   contains
      implementException(Exception)

      implementException(EExternal)

      implementException(EIntError)
      implementException(EDivByZero)
      implementException(ERangeError)
      implementException(EIntOverflow)

      implementException(EMathError)
      implementException(EInvalidOp)
      implementException(EZeroDivide)
      implementException(EOverflow)
      implementException(EUnderflow)
      implementException(EInexact)

      implementException(EInOutError)

      implementException(EExternalException)
      implementException(EInvalidSyntax)

      implementException(EAccessViolation)
      implementException(EPrivilege)
      implementException(EStackOverflow)
      implementException(EControlC)

      implementException(EConvertError)
      implementException(EFormatError)
      implementException(ENameListError)

      implementException(EAbort)
      implementException(EAssertionFailed)

      implementException(EPropReadOnly)
      implementException(EPropWriteOnly)

      implementException(EOSError)

      implementException(ENotImplemented)

      implementException(EArgumentException)
      implementException(EArgumentOutOfRangeException)
      implementException(EArgumentNullException)

      implementException(ENotSupportedException)
      implementException(EFileNotFoundException)

      implementException(EInvalidOpException)

      implementException(ENoConstructException)

      implementException(EStreamError)
      implementException(EFCreateError)
      implementException(EFOpenError)

      implementException(EListError)
end module