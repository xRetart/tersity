global print_character

section .text
print_character:
	push rdi  ; push first parameter
	mov rsi, rsp  ; set buffer to stack pointer
	add rsp, 8  ; restore stack pointer

	mov rax, 1  ; syscall for write
	mov rdi, 1  ; file handle for stdout
	mov rdx, 1  ; print just one character

	syscall

	ret