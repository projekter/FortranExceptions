#ifndef _in_x86_64_Windows
   #define _in_x86_64_Windows

   #define bufLen 32

   // To change quickly between different calling conventions which pass parameters in
   // registers, define the first two registers holding the parameters here. Note that
   // return values are always stored in RAX.
   #define param1 rcx
   #define param2 rdx

   // non-volatile: rbx, rsp, rbp, rsi, rdi, r12 - r15, xmm6 - xmm15,
   // mxcsr6 - mxcsr15, FPU control word

   // We will use inlined versions of setjmp and longjmp. These don't mess around the
   // stack when called by a naked function.

   // Inline setjmp macro. RAX is the register that contains the base pointer.
   // RDX is changed.
   #define store                        \
      __asm mov     rdx, [rsp]          \
      __asm mov     [rax], rdx          \
      __asm mov     [rax + 0x08], rbx   \
      __asm mov     [rax + 0x10], rsp   \
      __asm mov     [rax + 0x18], rbp   \
      __asm mov     [rax + 0x20], rsi   \
      __asm mov     [rax + 0x28], rdi   \
      __asm mov     [rax + 0x30], r12   \
      __asm mov     [rax + 0x38], r13   \
      __asm mov     [rax + 0x40], r14   \
      __asm mov     [rax + 0x48], r15   \
      __asm movdqa  [rax + 0x50], xmm6  \
      __asm movdqa  [rax + 0x60], xmm7  \
      __asm movdqa  [rax + 0x70], xmm8  \
      __asm movdqa  [rax + 0x80], xmm9  \
      __asm movdqa  [rax + 0x90], xmm10 \
      __asm movdqa  [rax + 0xA0], xmm11 \
      __asm movdqa  [rax + 0xB0], xmm12 \
      __asm movdqa  [rax + 0xC0], xmm13 \
      __asm movdqa  [rax + 0xD0], xmm14 \
      __asm movdqa  [rax + 0xE0], xmm15 \
      __asm stmxcsr [rax + 0xF0] /* 32 bits only */ \
      __asm fnstcw  [rax + 0xF4] /* 16 bits only */ \
      __asm mov QWORD PTR [rax + 0xF8], 0 /* We did not encounter an abnormal control
                                             flow yet */

   // Inline longjmp macro. param1 is the register that contains the base pointer.
   // This must be the register which contains the first parameter in the current calling
   // convention! RAX is changed.
   #define restore                        \
      __asm mov rbx,      [param1 + 0x08] \
      __asm mov rsp,      [param1 + 0x10] \
      __asm mov rbp,      [param1 + 0x18] \
      __asm mov rsi,      [param1 + 0x20] \
      __asm mov rdi,      [param1 + 0x28] \
      __asm mov r12,      [param1 + 0x30] \
      __asm mov r13,      [param1 + 0x38] \
      __asm mov r14,      [param1 + 0x40] \
      __asm mov r15,      [param1 + 0x48] \
      __asm movdqa xmm6,  [param1 + 0x50] \
      __asm movdqa xmm7,  [param1 + 0x60] \
      __asm movdqa xmm8,  [param1 + 0x70] \
      __asm movdqa xmm9,  [param1 + 0x80] \
      __asm movdqa xmm10, [param1 + 0x90] \
      __asm movdqa xmm11, [param1 + 0xA0] \
      __asm movdqa xmm12, [param1 + 0xB0] \
      __asm movdqa xmm13, [param1 + 0xC0] \
      __asm movdqa xmm14, [param1 + 0xD0] \
      __asm movdqa xmm15, [param1 + 0xE0] \
      __asm ldmxcsr       [param1 + 0xF0] /* 32 bits only */ \
      __asm fnclex                        \
      __asm fldcw         [param1 + 0xF4] /* 16 bits only */ \
      __asm mov rax, [param1]             \
      __asm mov [rsp], rax

   // Windows requires 32 bits of shadow space before a call. This macro works, but of
   // course only for functions which did not need to pass arguments on the stack!
   #define shadowCall(f)  \
      __asm sub rsp, 0x20 \
      __asm call f        \
      __asm add rsp, 0x20

#endif