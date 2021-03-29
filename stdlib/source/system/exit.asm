global exit

section .text
exit:
	mov rax, 60  ; syscall id for exit
	; rdi already valid because of parameter
	syscall