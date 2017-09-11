#if _M_X64
   #include "CExceptionsx86-64Windows.cpp"
#elif __x86_64__
   #include "CExceptionsx86-64Linux.cpp"
#else
   #error Incompatiable files
#endif

#ifndef _in_x86_64
   #define _in_x86_64
#else
   __declspec(naked) void registerTryBlock() {
      __asm {
         // Allocate new structure to RAX
         mov param1, bufLen * 8
         shadowCall(malloc)

         // Store the state to RAX
         store

         // Push it to the list
         mov param1, rax
         shadowCall(pushToStack)

         // Return and execute the try block
         xor eax, eax
         ret
      }
   }

   __declspec(naked) void jumpBack(stateBuf **toState) {
      __asm {
         mov param1, [param1]
         // This function is always called, even if there is no jump to perform.
         test param1, param1
         jz done

         // The input buffer address is param1; this register will not be modified.
         restore

         // Check whether the state was assigned to the next try block
         cmp QWORD PTR [param1 + 8 * (bufLen -1)], 0

         // And only free the memory if this was not the case (because we are in the
         // outermost block)
         jne skip
         shadowCall(free) // Still RCX points at the correct position.

skip:
         // Return and skip the try block
         mov eax, 2
done:
         ret
      }
   }

   __declspec(naked) void getClassName(void *obj, void *output) {
      __asm {
         // The class information position is stored at the offset 0x30
         mov param1, [param1 + 0x30]
         // And the class name is at exactly this position
         mov param1, [param1]
         // Write it to the output
         mov [param2], param1
         // It is a null-terminated string. We're done.
         ret
      }
   }
#endif