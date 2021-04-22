global print_n

section .text
print_n:
    mov rdx, rsi  ; number of characters to write as parameter 1
    mov rsi, rdi  ; pointer to string as parameter 0

    mov rax, 1  ; syscall for write
    mov rdi, 1  ; file handle for stdout

    syscall

    ret