; Erdogan Tan - Retro DOS 1.0 Diskette Type Check
; 19/02/2018

	[org 100h]

	mov	dl, 0
	mov	ah, 8
	int	13h
	jc	short nodisks 
	and	bl, bl
	jz	short nodisks
	cmp	bl, 5
	ja	short nodisks	
	mov	[disktype], bl
	add	byte [numdisks], dl
	mov	si, msgnumdiskettes
	call	print_msg
	mov	si, diskettemsg
	call	print_msg
	mov	bl, [disktype]
	shl	bl, 1
	add	bx, dtyptab-2
	mov	si, [bx]
	call	print_msg
	cmp	byte [numdisks], "1"
	jna	short terminate
	mov	byte [disknum], "2"
	mov	si, diskettemsg
	call	print_msg
	mov	dl, 1
	mov	ah, 8
	int	13h
	jc	short nodisks
	and	bl, bl
	jz	short nodisks
	cmp	bl, 5
	ja	short nodisks
	shl	bl, 1
	add	bx, dtyptab-2
	mov	si, [bx]
	call	print_msg
terminate:
	int	20h

nodisks:
	mov	si, msgnodisks
	call	print_msg
	int	20h

print_msg:
	mov	ah, 0Eh
	mov	bx, 7
printloop:
	lodsb
	and	al, al
	jz	short return
	int	10h
	jmp	short printloop
return:
	retn

dtyptab:
	dw _01h
	dw _02h
	dw _03h
	dw _04h
	dw _05h	

msgnumdiskettes:
	db 'Number of Diskettes: '
numdisks:
	db '0',13,10,0	
diskettemsg:
	db 'Diskette '
disknum:
	db '1 : ',0

msgnodisks:
	db 'UNKNOWN DISK!',13,10,0

_01h:
	db '360KB, 40 Track, 5 1/4"',13,10,0
_02h:
	db '1200KB, 80 Track, 5 1/4"',13,10,0
_03h:
	db '720KB, 80 Track, 3 1/2"',13,10,0
_04h:
	db '1.44MB, 80 Track, 3 1/2"',13,10,0
_05h:
	db '2.88MB, 80 Track, 3 1/2"',13,10,0 

disktype:
	db 0