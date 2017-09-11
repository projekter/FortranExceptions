#include <stack>
#include <exception>
#include <stdexcept>
//#include <setjmp.h>
// We use our own implementation of setjmp, because the one of Intel is unreliable; in
// particular, it messes up the function stack frame (the stack frame is likely to be
// overwritten after setting the jump point and not restored when the back jump happens).
// The user-defined functions basically do the same, however, they can be called from
// within naked functions, which do not have stack frames to mess up.

#if defined(__i386) || defined(_M_IX86)
   #define includeFile "CExceptionsx86-32.cpp"
#elif defined(__x86_64__) || defined(_M_X64)
   #define includeFile "CExceptionsx86-64.cpp"
#else
   #error Unknown platform. Please add required assembly files and define conditionals.
#endif

#include includeFile

typedef void *stateBuf[bufLen];
typedef std::stack<stateBuf*> stateStack;

thread_local stateStack statesAtTry;

// Using a global constructor, we make sure that the initializing function is called
// automatically upon program start.
extern "C" void Exception$startup();
extern "C" void Exception$finalize();
class initializeFortran {
   public:
      initializeFortran() {
         Exception$startup();
      };

      ~initializeFortran() {
         Exception$finalize();
      }
};
initializeFortran initializer;

extern "C" {
   void registerTryBlock();
   void throwException();
   void registerAbnormalFlow();
   void prepareLeaveTryBlock();
   void leaveTryBlock();
   stateBuf *deregisterTryBlock();
   void deregisterFromHandler(stateBuf **oldJump);
   void throwFromHandler(stateBuf **oldJump);
   void jumpBack(stateBuf **toState);

   void getClassName(void *obj, void *output);
}

// This is a function for registerTryBlock, since the method address cannot be resolved
// by assembler. Additionally, we don't need to worry about implementing thread_local.
void pushToStack(stateBuf *value) {
   statesAtTry.push(value);
}

void throwException() {
   // The exception object is not dealt with in this external library. The only job it
   // has to do is to do the jump to the previous position. This means that if there is a
   // try block, this function will never return and any Fortran statement that follows
   // the call to this function will not be executed - unless there was no try block.
   // This allows to implement a generic error handler (print the error and stop the
   // program) if there was no try.
   if(statesAtTry.empty()) {
      return;
   }
   stateBuf *buf = statesAtTry.top();
   // Do not pop the exception yet; this is done in the handler.
   // If jumps were performed within the try block, this created an anormal jump block.
   // We now leave the block forcefully, but not with a control flow jump, so remove any
   // reference to them.
   if((*buf)[bufLen -1]) {      // if we had any
      free((*buf)[bufLen -1]);  // free its memory
      (*buf)[bufLen -1] = NULL; // and remove knowledge
   }
   // This is almost the same as in jumpBack, only that we don't free our memory, which
   // is left to the handler
   __asm {
      mov param1, buf
      restore

      // Return and skip the try block
      mov eax, 1
      ret
   }
}

// This is a helper for the naked registerAbnormalFlow
void *_registerAbnormalFlow() {
   if(statesAtTry.empty()) {
      throw std::logic_error("Invalid control flow structure: Abnormal flow was register"
                             "ed without enclosing try statement");
   }
   stateBuf *buf = statesAtTry.top(); // RAX holds a pointer to the value
   if(!(*buf)[bufLen -1]) {
      (*buf)[bufLen -1] = malloc(sizeof(stateBuf));
   }
   return (*buf)[bufLen -1];
}

__declspec(naked) void registerAbnormalFlow() {
   // We need a naked function to get the return address in a optimization settings
   // indepent way; but with naked functions, it is very dangerous to include C code.
   // Hence, use this little helper to obtain the return address and jump to a "proper"
   // function.
   __asm {
      shadowCall(_registerAbnormalFlow)
      store
      ret
   }
}

// We prepare to leave the try block in a normal way. Therefore, any control flow
// statements that occurred so far were only jumps within the block and should be
// discarded.
void prepareLeaveTryBlock() {
   if(statesAtTry.empty()) {
      throw std::logic_error("Invalid control flow structure: Try block end found withou"
                             "t proper beginning");
   }
   stateBuf *buf = statesAtTry.top();
   if((*buf)[bufLen -1]) {      // if we had any
      free((*buf)[bufLen -1]);  // free its memory
      (*buf)[bufLen -1] = NULL; // and remove knowledge
   }
}

void leaveTryBlock() {
   if(statesAtTry.empty()) {
      throw std::logic_error("Invalid control flow structure: Try block end found withou"
                             "t proper beginning");
   }
   stateBuf *buf = statesAtTry.top();
   if((*buf)[bufLen -1]) {
      // The block was left abnormally. We need to jump to the handler. Freeing the
      // memory will be done by the handler.
      __asm {
         mov param1, buf
         restore

         // Jump and skip the block
         mov eax, 2
         ret
      }
   }
}

stateBuf *deregisterTryBlock() {
   if(statesAtTry.empty()) {
      throw std::logic_error("Invalid control flow structure: No open try block");
   }
   stateBuf *buf = statesAtTry.top();
   statesAtTry.pop();
   stateBuf *ret = (stateBuf*)((*buf)[bufLen -1]);
   free(buf);
   if(ret && ((*ret)[bufLen -1] = (void*)!statesAtTry.empty())) {
      // Assign the jump to the next higher try block. If this try block is within a
      // scoping unit that is unaffected by the jump, this does not matter - the data is
      // either overwritten (new control flow command) or deleted (normal execution or
      // throw). The jump will only be executed if the higher block is directly affected
      // by the current control flow statement.
      buf = statesAtTry.top();
      if((*buf)[bufLen -1]) {
         // If we already have assigned such a statement, it was caused by a harmless
         // inner jump and is now superfluous.
         free((*buf)[bufLen -1]);
      }
      (*buf)[bufLen -1] = ret;
   }
   return ret;
}

void deregisterFromHandler(stateBuf **oldJump) {
   if(stateBuf *newJump = deregisterTryBlock()) {
      // If there was a previous jump (-> *oldJump != NULL), it is now freed, provided it
      // was assigned to an upper try structure. This is guaranteed, unless we are at the
      // very top of the try chain, in which case we must free now.
      if(statesAtTry.empty()) {
         free(*oldJump);
      }
      // We jump to the new state.
      jumpBack(&newJump);
   }else{
      jumpBack(oldJump);
   }
}

void throwFromHandler(stateBuf **oldJump) {
   // We ended a handler via throwing an exception. Therefore, any internal jumps should
   // be cleared already.
   if(deregisterTryBlock()) {
      throw std::logic_error("Invalid control flow structure: Throwing from a handler, b"
                             "ut the jump chain is not cleaned up");
   }
   // If there is no higher-level try structure, we must free the state.
   if(statesAtTry.empty()) {
      free(*oldJump);
   }
   // Else, throwException does the job.
   throwException();
}

#include includeFile