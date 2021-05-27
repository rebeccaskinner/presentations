        global fib
        section .text

        ;; fib: Int -> Int
        ;;  Given a number, n, return the nth fibonacci number
        ;;  from the sequence: 0,1,1,2,3...
fib:
        push rcx
        xor rax, rax
        xor rcx, rcx
        inc rcx
loop:
        mov rdx, rax
        mov rax, rcx
        add rcx, rdx
        dec rdi
        jnz loop
        pop rcx
        ret

        global fibsum
        section .text

        ;;  fibsum: (Int, Int) -> Int
        ;;   Given two numbers, (a,b) return fib(a) + fib(b)
fibsum:
        xor rax,rax
        xor rcx, rcx
        call fib
        mov rcx, rax
        mov rdi, rsi
        call fib
        add rax, rcx
        ret
