# Fortran Exception Library

## Motivation
The Fortran language specification does not provide the means to do exception handling in the most cases. For some routines such as the I/O functions, it is possible to implement separate error handlers which basically act in a `goto` manner. However, there is no possibility to propagate exceptions at different levels or to implement memory protection blocks in a comfortable way.
Using features introduced in Fortran 2003 and 2008, together with routines from an external C++/Assembler library, all this becomes possible. The Fortran Exception Library enables the programmer to use the common try-except-finally syntax known from most major languages.

## Requirements
At the moment, this library is available only for Intel Fortran. It was tested with `ifort 16.0.4` on Linux and with `ifort 17.0.2` on Windows, both on x86-32 and x86-64 architecture. The C++ compiler used was `icc` in the same version.
If you are a `gfortran` user, you are welcome to contribute a port for this compiler. It is not clear whether the same functionality is achievable,  but see the implementation section for details.

## Example
```Fortran
program TestExceptions
#  include <Exceptions.FPP>

   implicit none
   
   try
      call someBadSubroutine()
      print *, "Never here"
   except(E)
      type is(ENotSupportedException)
         print *, "Got an exception: ", E%Message
   endExcept
   
   contains
      subroutine someBadSubroutine()
         throw(ENotSupportedException("This is not supported."))
      end subroutine
end program
```
Also see the examples in the test case.

## Basic usage

### Compilation of the library
Some pre-compiled versions are available in the release section; however, you may also compile the library on your own.

#### Windows (Visual Studio)
Open the solution `Exceptions.sln` in Visual Studio. Ensure you choose the `Release` configuration (unless you want to debug the library) and select the appropriate platform, `x64` or `x86`. Compile the complete solution; dependencies are already set up such that the compilation will happen in the correct order. You may also want to use the batch build to create all configurations. To check that everything worked well, run `ExceptionTester` and make sure all tests are passed.

#### Linux (command line)
Make sure the `icc` and `ifort` compiler are known. Move to `src/ExceptionHandler` and compile the C++ handler:
```console
[ExceptionHandler]$ icc -O3 -std=c++11 -fasm-blocks -c -o ../../lib/<arch>/ExceptionHandler.o CExceptions.cpp
```
Here, `<arch>` shogitgituld be replaced with either `x86-64` or `x86-32`, depending on your platform target and the compiler used. Make sure the paths all exist!
After that, compile the Fortran library. Move to `src/Exceptions`.
```console
[Exceptions]$ ifort -O3 -fpp -w -I../../include -module ../../modules/<arch> \
   -c mLogger.f90 mExceptionClasses.F90 mException.F90
[Exceptions]$ mv *.o ../../lib/<arch>
```
Finally, we will create a static library out of those. Move to `lib/<arch>`.
```console
[<arch>]$ xiar rc libExceptions.a *.o
```

### Usage of the library

#### Fortran code
The Fortran part of the library consists of two modules and a set of preprocessor directives. They are altogether loaded by the preprocessor statement
```Fortran
#include <Exceptions.FPP>
```
This expression has to be put at a position where otherwise, a `use` statement would be put, so at the beginning of a module, program, subroutine or function. It is not a problem if there are several modules in the same file which all need to include this file.
After this line, all the features described below are available and ready for use.

#### Compilation of the code (Visual Studio)
Several options have to be set in the project properties.
- The preprocessor needs to be turned on (Fortran > Preprocessor > Preprocess Source File; all configurations and platforms).
- The preprocessor and the compiler have to be aware of the include directory which contains the `FPP` files. Add the `include/` directory (Fortran > General or Preprocessor > Additional Include Directories; all configurations and platforms).
- The same holds true for the module files of the Fortran part. Since these are architecture specific, `modules/<arch>` needs to be added to the include directory as well.
- The library has to be provided to the linker. Since you typically do not want to have the exception projects in your solution (in which case you only needed to set the dependency on `Exceptions` and also didn't have to do the previous step), you need to specify the complete path of the `libExceptions.lib` file located in the `lib/<arch>` directory directly by adding it as explicit dependencies (Linker > Input > Additional Dependencies). The `ExceptionHandler.lib` dependency is not needed, since it is automatically linked together with the `Exceptions` project into `libExceptions.lib`.

It is recommended to add the `/w` option to the command line of the compiler; for else, the preprocessor would issue lots of warnings (see implementation notes).

#### Compilation of the code (Linux command line)
To simplify the commands, a module file is provided; when put in the appropriate module directory, load the module via `module load fortException`. Before this, make sure that you provide the correct absolute path to the exception library root directory in the module file. Then, the required include paths for the preprocessor as well as the library path are automatically set. You then only have to specify that the program should also be linked against the library, i.e.
```console
$ ifort -lstdc++ -fpp -w <arguments such as output and source files> -lExceptions
```
This makes sure the standard C++ library is included together with the exceptions library itself (which has to come after the last file which uses exceptions, so best to append it to the command line). The preprocessor is turned on by `-fpp` and preprocessor warnings are suppressed by `-w`.

## Implementation details
- **Initialization**<br />
  The C++ and the Fortran part of the library are tightly connected together. At the beginning of the program, some Fortran startup code is called<sup>1</sup>. Apart from initialization of a few variables, the `signalQQ` and most importantly, the `establishQQ` function are called. These are non-standard extensions of Intel Visual Fortran and allow to implement custom error handlers when the system or the runtime library detects a problem. By doing so, errors which are normally fatal (such as integer division by zero) are converted into ordinary exception objects and can be caught in the application.<br />
  <sup>1</sup> Since Fortran does not support initialization functions, this call is invoked from a C++ global constructor.
- **Starting the protected block**<br />
  When the preprocessor encounters the `try` statement, it calls the C++ handler routine. The current CPU status (all registers which are required by the calling convention, possibly also FPU registers and control word) is stored in a stack which is thread-local. The `try` statement starts the `try` block, which has to be followed by _one_ handler. If you want to use both `except` and `finally`, you may use the shorthand `try2`, which just translates into two nested try blocks.
- **Throwing an exception**<br />
  In case an exception is thrown, the C++ handler retrieves the last saved state on the stack and basically performs a longjump. Execution will continue at the `try` statement, which then jumps _over_ the `try` block into the handler following. At the same time, the Fortran layer has set a thread-global pointer to the exception object which was passed to `throw`.<br />
  The syntax is `throw(<ExceptionClass>([Message], [Code]))`, where `<ExceptionClass>` is any of the predefined or a self-defined<sup>2</sup> exception class. Both the string `Message` and the integer `Code` are optional. The function which is invoked by issuing the class name of the exception creates a new exception object on the heap and copies the message string into. It returns a pointer to the object. If you ever use these functions outside of the `throw` directive, it is your responsibility to free the allocated memory afterwards. Else, the framework takes care of releasing the memory as soon as the object is no longer needed - so don't pass exception objects pointers to `throw` which you expect to be valid after the exception had been caught.<br />
  <sup>2</sup> There are convenience macros provided for creating a new exception class. To see some examples, refer to the `mExceptionClasses.F90`. Basically, you need to issue `declareException(<NewClass>, <ParentClass>)` in the declaration section and `implementException(<NewClass>)` in the `contains` section.
- **Except**<br />
  By issuing `except(E)` - where `E` can be any valid variable name -, the beginning of a handler block is declared. The current exception object will be retrieved by the library and assigned to the variable `E`, which only exists within this block. `except` must be treated as a `select type(E)` statement, which means that the very first statement in the block needs to be `type is` or `class is`. `class default` is prohibited; in case you want to catch all exceptions, simply use `class is(Exception)`, since `Exception` is the base class for all exceptions. The `except` block will be skipped if no exception was thrown in its preceding `try` block (at any level of nesting). If an exception was caught in the `except` block - so there was a matching type selector -, then the exception object itself is deallocated and execution continues after the block as if there were no exception. If no matching type selector was found or the command `raise` was executed in a matching type selector<sup>3</sup>, the exception is re-thrown and higher-lying handler blocks can handle the error.<br />
  The block is ended by `endExcept`.<br />
  <sup>3</sup> Note that you cannot use `throw(E)`, since `E` is a construct association and not a pointer and hence cannot be passed to `throw`.
- **Finally**<br />
  By issuing `finally`, the beginning of a handler is declared. This handler will _always_ be executed after the `try` block, regardless of what happened inside. Therefore, the exception object (if one was thrown at all) is not available within this block. If an exception was thrown, it will be re-raised after the execution of this block.<br />
  The block is ended by `endFinally`.
- **Control flow interrupting statements**<br />
  The usual sequential execution of a program can be altered in Fortran by several means. The `return` statement jumps to the end of the current execution unit, `exit` terminates a loop (or a named block), `cycle` skips the rest of the current loop iteration and continues with the next and if you ever really need it, `goto` allows to directly jump to labeled positions<sup>4</sup>. The exception library tries a lot so that these instructions can be used even within blocks while upholding the condition that `finally` is always executed. This means that if any of those statements is executed _within_ a `try` block with the effect that the rest of the block is skipped (jumps within the block work just in the normal way), the `finally` block will be executed _first_; after that, the jump produced by these statements is carried out.<br />
  This is implemented by overriding the keywords listed above with the preprocessor. Basically, a call to the C++ library is performed just before the jump statement. This call is responsible for storing the address of the jump instruction to the last state on the exception stack.<sup>5</sup><br />
  When the jump is then carried out, the `try` block is left. Internally it is implemented as a `block` which - apart from the aforementioned constant - also holds a static finalizable helper object. Hence, the compiler invokes the finalization routine, which calls the C++ library. The library notices that the block was left in an anomalous way and performs a longjump to the handler.<br />
  Due to the longjump, the CPU state is restored as it was when the `try` was first executed, only that now the `try` block is skipped and the handler is executed<sup>6</sup>.<br />
  At the end of the handler, the C++ library is once again invoked and jumps back to the `return`/`exit`/`cycle`/`goto` statement, which is executed once again. Note that this jump restores the CPU state as it was when the jump was about to be performed before. However, due to the execution of the handler, the stack was decremented and incremented in between. Hence, the stack content might be cluttered and any local stack variable which was not yet known in the outer `try` block<sup>7</sup> might be invalid. Luckily, this does not matter anymore<sup>8</sup> and the next execution of the jump command triggers our finalization handler again. But it knows that it was already executed and does nothing. This propagates through any level of nested blocks.<br />
  The **essence** of the previous description: The library takes care of any jump commands in a highly non-trivial manner. Don't introduce any finalizable variables (and allocatables also need finalization) on a scope which is only valid within a part of a `try` block (i.e. don't use Fortran `block`s with finalizable variables when you do a jump out of such a block). The library overwrites compiler instructions to achieve this; hence, the preprocessor will issue lots of warnings. It is recommended to use the `/w` (Windows) or `-w` parameter in order to disable these warnings.<br />
  **What else should be avoided?**
  - Computed `goto` statements allow jumping to one out of several destinations, depending on a certain criterion. Don't use them.<br />
    As described above, the jump statement needs to be evaluated several times. As long as the computation does not depend on block-local user-introduced variables (regardless of whether they are finalizable or not), this should in principle work. But it is a waste of time to compute the destination several times. Just use `if` constructs (and not arithmetic `if`s!), so that the call to store the state can be injected to a place where the computation was already carried out.<br />
  - Do not use alternate returns that lead out of a block.<br />
    The library has no means to detect alternate returns (which are obsolescent anyway). The same holds true for branch specifiers (as in `open`, `allocate`). Branch targets within the block are perfectly fine. If an alternate return or branch specifier jumps out of a block, the library is left in an inconsistent state, since leaving the block did not pop an element from the state stack. Hence, the outcome will be completely unpredicable, together with memory leaks occurring for sure.
  - If you want to execute one of the jump statements above, but you know for sure that it is not and will never be contained (at execution unit level) by a `try`, `except` or `finally` block, you may want to bypass the substitution of the preprocessor. This is possible, as the preprocessor is case-sensitive but Fortran is not. All preprocessor defines of the exception library are given in **lower case**, **camel case** (if multiple words are joined), with a **starting upper case letter**, the **same for camel case** and all in **upper case**. Therefore, just use some weird capitalization to access the original statement. Recommended is changing the last letter, e.g. `returN` if you write in lower case or `RETURn` if you write in upper case. You must _not_ use this spelling inside a block if it the jump goes outside.
 
  <sup>4</sup> Note that it is not possible to jump into a `try`, `except` or `finally` block from outside.<br />
  <sup>5</sup> Note that this code is also generated if the statements are not within a `try` block, since the preprocessor cannot distinguish them; however, with optimization turned on, the compiler should remove this call - it is conditionally executed only if a constant is `.true.`. This constant is defined in the Fortran library as `.false.`, but it is covered by a block-local constant defined as `.true.`.<br />
  <sup>6</sup> Internally, the handlers operate in yet another `try` level, such that exceptions and jumps within are noticed.<br />
  <sup>7</sup> This can happen if the programmer introduced yet another ordinary Fortran `block` with variables.<br />
  <sup>8</sup> Unless this programmer-introduced inner `block` contains a _finalizable_ variable, which thus was already finalized when first leaving the block and now can contain anything. This will cause a mess. Don't do it.
- **Default error handler**<br />
  When an exception is not caught, the default error handler is invoked. It prints the class name<sup>9</sup> of the exception together with its code and message as well as a traceback.<br />
  Note that at least in `ifort 17`, there is a bug in the `tracebackQQ` subroutine. If your application uses the `complex` type of any kind at any place, the traceback function causes a crash. A [bug report](https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/731217) is submitted, but not cared for by Intel at the moment. You probably want to comment out calling the traceback in `mException.F90`, subroutine `Exception$defaultHandler`. The program execution stops immediately, though first the library finalizes.<br />
  <sup>9</sup> Obtaining the class name of an object is not possible in standard Fortran, nor does Intel Fortran provide a non-standard extension for this. Hence, the function `getClassName` relies on the compiler-internal structure, which might change at any time whithout any notice. At the moment, the implementation seems to work for `ifort 16` and `ifort 17`, where separate offsets are provided for the x86-32 and x86-64 version; however, on both Windows and Linux, the structure looks like the same. Therefore, it is crucial that this function is tested against every new version.
- **Assertions**<br />
  The library provides the macro `assert(condition)`, which raises an `EAssertionFailed` exception in case `condition` is `.false.`. The message of the exception object contains the file name and line in which the assertion was triggered. Assertions are only activated when the symbol `_DEBUG` is defined; in release mode, they just evaluate to `continue` (which makes them available as branch targets in both configurations).
- **Finalization**<br />
  At the end of the program, the variables from the beginning are deallocated. This will always be done upon normal termination or when an exception ended the program. However, if you issue any of the `stop`, `error stop` commands or one of the non-standard extensions which all do the same, the finalization cannot be performed. In this case, if you scan for memory leaks, you will find an expected leakage of 10 strings. Anything different from this which originates in the library is a bug and should be reported.
- **Thread-safety**<br />
  The library is thread-safe, since the global information in both C++ and Fortran are stored in thread-local variables. In C++, this is achieved via the `thread_local` statement; Fortran uses the `!$OMP THREADPRIVATE(...)` directive. Hence, the Fortran version is not _really_ thread-safe if you launch multiple threads via API functions instead of OpenMPI. This will put the Fortran and C++ backend in an inconsistent state and therefore must not be done.<br />
  It is always possible to use multiple threads if the main thread is the only one which ever uses the exception framework.<br />
  Due to the fact that the programmer does not have to write any code in order to launch multiple threads and for example parallelize loops or assignments, the behavior of the library might be somewhat unexpected, though it is perfectly understandable if one thinks about the fact that every thread in most programming language (unless some threading library is used) is a separate worker which should have its own exception handling, where the thread procedure would not be implemented in the same place as the initializing main thread. However, consider the following scenario:
  ```Fortran
  try
     !$OMP PARALLEL DO
        do I = 1, 10
           if(I == 5) then
             throw(Exception("What now?"))
           end if
        end do
     !$OMP END PARALLEL DO
  except
     class is(Exception)
        print *, "Something went wrong"
  endExcept
  ```
  One of the worker threads will throw an exception; but since the handler was declared in the main thread, the worker does not know of it. It therefore encounters an unhandled exception and will cause the program to stop. Of course, by putting the `try`-`except` block inside the loop, this problem would not arise. But then, this would be extremely inefficient for large loops, since the protection schemes do cost some time. The behavior one typically wishes is this: Execute the loop in parallel, but when an exception occurs, terminate all workers and handle the problem in the main thread. Perhaps this is something that can be done; feel free to contribute.
- **Performance**<br />
  On the one hand, any kind of exception handling will slow down the program. On the other hand, the programming logic can greatly benefit from the use of a proper structured exception handling. And of course, the development process can also speed up a lot if the developer has powerful tools for the error analysis at hand (for example, the usual segmentation fault should now rarely occur and instead translate to a understandable and in most cases also catchable exception; the same holds e.g. for division by zero). It is therefore important to know which statements have an impact on performance and which don't.<br />
  You may throw an exception out of any procedure which carries out CPU intensive calculation without needing to fear any consequences (unless, of course, you optimize for a specific microcontroller and the throw statement is just too large to fit in the µop-cache; but typically, pure Fortran cannot be optimized to such a degree). This of course assumes that you use exceptions for _exceptional cases_ - so when you want the calculation to be aborted by the exception. If you just throw an exception with every iteration of the innermost loop, then there is no help.<br />
  You will be able to notice a performance impact if you open error handler blocks within a loop. The blocks involve calling C++ functions which are probably located far away and will therefore purge the code cache. Additionally, the code has to access thread-local variables, which is inefficient in itself. This should not prevent you from using the exception library, as it won't have a noticable impact unless it is repeated thousands of times.