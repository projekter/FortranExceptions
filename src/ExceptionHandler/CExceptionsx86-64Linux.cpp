#ifndef _in_x86_64_Linux
   #define _in_x86_64_Linux

   #define bufLen 10

   // To change quickly between different calling conventions which pass parameters in
   // registers, define the first two registers holding the parameters here. Note that
   // return values are always stored in RAX.
   #define param1 rdi
   #define param2 rsi

   // non-volatile: rbx, rsp, rbp, r12 - r15, mxcsr6 - mxcsr15, FPU control word

   // We will use inlined versions of setjmp and longjmp. These don't mess around the
   // stack when called by a naked function.

   // Inline setjmp macro. RAX is the register that contains the base pointer.
   // RDX is changed.
   #define store                      \
      __asm mov     rdx, [rsp]        \
      __asm mov     [rax], rdx        \
      __asm mov     [rax + 0x08], rbx \
      __asm mov     [rax + 0x10], rsp \
      __asm mov     [rax + 0x18], rbp \
      __asm mov     [rax + 0x20], r12 \
      __asm mov     [rax + 0x28], r13 \
      __asm mov     [rax + 0x30], r14 \
      __asm mov     [rax + 0x38], r15 \
      __asm stmxcsr [rax + 0x40] /* 32 bits only */ \
      __asm fnstcw  [rax + 0x44] /* 16 bits only */ \
      __asm mov QWORD PTR [rax + 0x48], 0 /* We did not encounter an abnormal control
                                             flow yet */

   // Inline longjmp macro. param1 is the register that contains the base pointer.
   // This must be the register which contains the first parameter in the current calling
   // convention! RAX is changed.
   #define restore                   \
      __asm mov rbx, [param1 + 0x08] \
      __asm mov rsp, [param1 + 0x10] \
      __asm mov rbp, [param1 + 0x18] \
      __asm mov r12, [param1 + 0x20] \
      __asm mov r13, [param1 + 0x28] \
      __asm mov r14, [param1 + 0x30] \
      __asm mov r15, [param1 + 0x38] \
      __asm ldmxcsr  [param1 + 0x40] /* 32 bits only */ \
      __asm fnclex                   \
      __asm fldcw    [param1 + 0x44] /* 16 bits only */ \
      __asm mov rax, [param1]        \
      __asm mov [rsp], rax

   // AMD ABI does not require any shadow space
   #define shadowCall(f) \
      __asm call f
#endif