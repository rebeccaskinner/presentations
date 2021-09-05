        global _start
_start:
        mov rax, 60             ; "_exit" syscall
        xor rdi, rdi
        inc rdi
        syscall
