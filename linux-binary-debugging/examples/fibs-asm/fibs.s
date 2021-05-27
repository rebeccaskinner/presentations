        global main
        extern printf
        section .text

main:
        ;;  Setup: use ecx to store the total number of iterations
        ;; (90). We'll zero out rax and rbx and set them to 0 and 1
        ;; respectively, and they will hold our "current" and "next"
        ;;  numbers.

        push rbx
        mov ecx, 90
        xor rax, rax
        xor rbx, rbx
        inc rbx

        ;;  We're not using a proper function call here,
        ;; just a lable that we can jump to. In the next
        ;; example we'll re-build this with a proper
        ;; function call, using x86-64 calling conventions
print:
        push rax                ; caller needs to save it's register
        push rcx                ; caller needs to save it's register

        lea rdi, [rel format]   ; load the address of the format string into rdi (1st parameter)
        mov rsi, rax            ; move the current number into rsi (second parameter)
        xor rax, rax            ; rax should hold the number of floats passed to a variadic function (none in this case)
        call printf             ; printf(format, current number)
        pop rcx                 ; restore rcx
        pop rax                 ; restore rax

        ;; calculate the next fibbonacci number
        mov rdx, rax            ; move the current number into a temporary buffer
        mov rax, rbx            ; make the next number current
        add rbx, rdx            ; the next number is the last number plus the current number

        dec ecx                 ; count down
        jnz print               ; if we aren't at zero, loop

        mov rax,60
        xor rdi, rdi
        pop rbx                 ; restore rbx
        syscall

        section .data
format: db "%20ld", 10, 0
