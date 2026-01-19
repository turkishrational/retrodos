; NASM test (deneme) file
; MSDOS.SYS principle simulation (for testing NASM's response)

; Erdogan Tan - 6/12/2022

; A sample MSDOS '.COM' file
; nasm deneme.asm -l deneme.txt -o deneme.com -Z error.txt
; (DATA section is used for '.SYS' file -like MSDOS kernel- simulation) 

; [BITS 16]	; (x86 real mode)

; DOS CODE segment simulation

section .text	; code segment (DOSCODE)

	[org 100h] ; 3DD0h for MSDOS 5.0 kernel (DOSCODE)
		   ; (BIOSCODE+BIOSDATA+DOSDATA size)
start:
	jmp	short kod3
kod1ptr:
	dw	kod1
kod0:
	int	20h
kod1:
	mov	ax, cs
	mov	dx, kod4
	add	dx, 15
	shr	dx, 4
	add	ax, dx
	mov	ds, ax 
	mov	si, [deneme1] ; mov si, deneme2
	mov	ah, 0Eh
	mov	bx, 7
	; CRLF
	mov	al, 0Dh
	int	10h
	mov	al, 0Ah
	int	10h
kod2:
	; print 'DENEME.' on screen
	lodsb
	int	10h
	cmp	al, '.'
	jne	short kod2
	;times	130 db 90h
	;jmp	kod0
	; CRLF & exit
	mov	al, 0Dh
	int	10h
	mov	al, 0Ah
	int	10h
	jmp	kod0
kod3:
	jmp	word [kod1ptr]

	nop
kod4:

align 16

; DOS data segment simulation

section .data vstart=0	; data segment (DOSDATA)
	
deneme0: dw deneme1
deneme1: dw deneme2
deneme2:
	db 'deneme...'
here:
	burasi equ $
deneme3:
	dw burasi ; dw here
 	db 0Dh, 0Ah, 0