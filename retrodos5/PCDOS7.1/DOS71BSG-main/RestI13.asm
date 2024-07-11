
;--- restore int 13h set by MS-DOS 7.1 GPT-modified boot sector

	.286
	.model tiny
	.386

?LOG     equ 1
INT13OFS equ 3BCh       ; int 13h handler offset set by modifed boot sector

iodat   struct
cmdlen  db      ?       ;+ 0: length structure
unit    db      ?       ;+ 1:
cmd     db      ?       ;+ 2
status  dw      ?       ;+ 3
        db      8 dup (?); reserved
media   db      ?       ;+ 13
trans   dd      ?       ;+ 14
count   dw      ?       ;+ 18   on init:offset parameter
start   dw      ?       ;+ 20   on init:segment parameter
drive   db      ?       ;+ 22
iodat   ends

CStr macro text:vararg
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm

	.code

	dw 0ffffh
	dw 0ffffh
	dw 8000h				  ;attribute
	dw offset devstrat		  ;device strategy
	dw offset devint		  ;device interrupt
	db 'RESTI13$'			  ;device name

cptr  dd 1 dup(?)

devstrat proc far
	mov word ptr cs:[cptr+0],bx
	mov word ptr cs:[cptr+2],es
	ret
devstrat endp

devint proc far
	pusha
	push ds
	push es
	lds bx,cs:[cptr]
	mov [bx].iodat.status,8103h
	cmp [bx].iodat.cmd,00
	jnz devi1
	mov [bx].iodat.status,0100h
	mov word ptr [bx+0eh],0000
	mov word ptr [bx+10h],cs
	call main
devi1:
	pop ds
	pop es
	popa
	ret
devint endp

if ?LOG
	include printf.inc
endif

main PROC
	xor ax, ax
	mov ds, ax
	cmp word ptr ds:[INT13OFS], 9C50h	; push ax, pushf
	jnz @F
	cmp byte ptr ds:[INT13OFS+2], 9ah	; call far16
	jnz @F
	lds dx, ds:[INT13OFS+3]
	push ds
	push dx
	mov ax, ds
	mov es, ax
	mov bx, dx
	mov ah, 13h
	int 2Fh
	pop dx
	pop ax
if ?LOG
	mov cx, ds   ; old now in cx:bx
	push cs
	pop ds
	invoke printf, CStr("resti13: old=%X:%X, new=%X:%X",10), cx, bx, ax, dx
endif
	mov ax, ax
	mov es, ax
	mov di, INT13OFS
	mov cx, 400h - INT13OFS
	rep stosb
@@:
	ret
main endp

	END
