#ifndef _in_x86_32
   #define _in_x86_32

   #define bufLen 7

   // Though in x32 cdecl, the parameters are passed on the stack, we don't do this when
   // calling our macros. Define these values here to go to some volatile register in
   // order to have more platform-independent code.
   #define param1 ecx
   #define param2 edx

   // non-volatile: ebx, esp, ebp, esi, edi

   // We will use inlined versions of setjmp and longjmp. These don't mess around the
   // stack when called by a naked function.

   // Inline setjmp macro. EAX is the register that contains the base pointer.
   // EDX is changed.
   #define store                     \
         __asm mov edx, [esp]        \
         __asm mov [eax], edx        \
         __asm mov [eax + 0x04], ebx \
         __asm mov [eax + 0x08], esp \
         __asm mov [eax + 0x0C], ebp \
         __asm mov [eax + 0x10], esi \
         __asm mov [eax + 0x14], edi \
         __asm mov DWORD PTR [eax + 0x18], 0 /* We did not encounter an abnormal control
                                                flow yet */

   // Inline longjmp macro. param1 is the register that contains the base pointer.
   // This must be the register which contains the first parameter in the current calling
   // convention! EAX is changed.
   #define restore                      \
         __asm mov ebx, [param1 + 0x04] \
         __asm mov esp, [param1 + 0x08] \
         __asm mov ebp, [param1 + 0x0C] \
         __asm mov esi, [param1 + 0x10] \
         __asm mov edi, [param1 + 0x14] \
         __asm mov eax, [param1]        \
         __asm mov [esp], eax

   #define shadowCall(f) \
         __asm call f

#else
   __declspec(naked) void registerTryBlock() {
      __asm {
         // Allocate new structure to EAX
         push bufLen * 4
         shadowCall(malloc)
         add esp, 4

         // Store the state to EAX
         store

         // Push it to the list
         push eax
         shadowCall(pushToStack)
         add esp, 4

         // Return and execute the try block
         xor eax, eax
         ret
      }
   }

   __declspec(naked) void jumpBack(stateBuf **toState) {
      __asm {
         mov param1, [esp +4]
         mov param1, [param1]
         // This function is always called, even if there is no jump to perform.
         test param1, param1
         jz done

         // The input buffer address is param1; this register will not be modified.
         restore

         // Check whether the state was assigned to the next try block
         cmp DWORD PTR [param1 + 4 * (bufLen -1)], 0

         // And only free the memory if this was not the case (because we are in the
         // outermost block)
         jne skip
         push param1
         shadowCall(free)
         add esp, 4

skip:
         // Return and skip the try block
         mov eax, 2
done:
         ret
      }
   }

   __declspec(naked) void getClassName(void *obj, void *output) {
      __asm {
         mov param1, [esp +4]
         // The class information position is stored at the offset 0x18
         mov param1, [param1 + 0x18]
         // And the class name is at exactly this position
         mov param1, [param1]
         // Write it to the output
         mov param2, [esp +8]
         mov [param2], param1
         // It is a null-terminated string. We're done.
         ret
      }
   }
#endif