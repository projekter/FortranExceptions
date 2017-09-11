/**
  * Implement an assertion command which, based on whether we are in debug mode or not,
  * does nothing or throws an assertion failure with information about its origin
  */
#ifdef _DEBUG
#  define raiseAssert(f, l) call Exception$throw(EAssertionFailed("Assertion failure in " // f // ":" // #l))
#  define raiseAssertHelp(f, l) raiseAssert(f, l)
#  define assert(cond) if(.not.(cond)) raiseAssertHelp(__FILE__, __LINE__)
#else
#  define assert(cond) continue
#endif
#define ASSERT(cond) assert(cond)
#define Assert(cond) assert(cond)