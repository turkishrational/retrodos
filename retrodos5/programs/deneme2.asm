; PROMPT $R test for PCDOS 7.1 COMMAND.COM - Erdogan Tan - August 6, 2024

	[org 100h]
	
	;mov	ah,09h
	;mov	dx,program_name
	;int	21h
	call	print_msg

	mov	al,255  ; Return Code
	mov	ah,4Ch
	int	21h
hang:
	;sti
	jmp	short hang

print_msg:
	mov	ah,0Eh
	mov	bx,7
	mov	si,program_name
nextchr:
	lodsb
	or	al,al
	jz	short pmsg_end
	int	10h
	jmp	short nextchr
pmsg_end:
	retn

program_name:
	db 0Dh,0Ah
	db "IBM PCDOS 7.1 COMMAND.COM (Prompt ",24h,"R) Return Code Test Program"
	db 0Dh,0Ah
	db "(Erdogan Tan - 06/08/2024)"
	;db 0Dh,0Ah,"$"
	db 0Dh,0Ah,0 