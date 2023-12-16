
;*** boot sector(s) FAT32, DOS 7.1

;--- 1. boot sector is read at 0000:7C00h
;--- 2. sector 0 is read    at 0000:0700h
;--- 3. 2 more sectors (fs info, bs2) are read at 0000:7E00
;---    jmp to 8000h ( bs2 )
;--- 4. directory sectors are read to 0000:0700h until IO.SYS is found
;---    FAT is read to get next cluster at 0000:7E00h
;--- 5. first 4 sectors of IO.SYS are read at 0000:0700h
;---    jmp to IO.SYS 0070:0200h


	.286
	.model tiny
	option casemap:none
	.386

ifdef MAKEGPTBS
?FORCELBA	equ 1	;1=force LBA access, no CHS
?ICINT13	equ 1	;1=intercept int 13h
else
?FORCELBA	equ 0
?ICINT13	equ 0
endif

PTOFS		equ 1BEh;offset partition table in MBR
SIZEPTE		equ 16	;size partition table entry

BSADDR		equ 7C00h	;address where boot sectors (3) are loaded
MBRADDR		equ 0700h	;address where MBR is loaded
ROOTADDR	equ 0700h	;address where root directory sectors are loaded
FATADDR		equ 7E00h	;address where FAT sectors are loaded
IOSYSADDR	equ 0700h	;address where IO.SYS sectors are loaded

;--- BIOS Parameter Block

BPB struct
bytes_sector     dw 512	;+0
sectors_cluster  db ?
reserved_sectors dw 32	;+3
num_fats         db 2
root_entries     dw 0	;+6 root entries for fat12/16
sectors_16       dw 0	;+8 total sectors for fat12, else 0 
media_byte       db 248
sectors_fat      dw 0	;+11 sectors/fat for fat12/16
sectors_track    dw 63
no_of_tracks     dw 255
hidden_sectors   dd ?	;+17 start of partition
sectors_32       dd ?	;+21 total sectors fat16/32
BPB ends

EBPB_FAT32 struct
	BPB <>
sectors_fat32	dd ?	;+25 sectors/fat for fat32
flags			dw ?	;+29 ???
version			dw 0	;+31
root_startcl	dd 2	;+33
fs_info_start	dw 1	;+37 start sector of FS information sector
bs_copy_start	dw 6	;+39
				db 12 dup (?) ;+41
phys_drive		db 128	;+53
				db ?	;+54
ext_boot_sig	db 29h	;+55
volume_id		dd ?	;+56
volume_label	db 11 dup (' ')	;+60 if boot_sig is != 0x28
fs_type			db "FAT32   "	;+71 file system type
EBPB_FAT32 ends

;--- the BP frame struct

BSECT struct
	org -8
dwFatSecNo	dd ?	;rel. FAT sector# currently read ( at FATADDR )
dwStartData dd ?	;start data region
	db 2 dup (?)	;jmp short
bType db 1			;"nop", used to store access method (0E=LBA)
	db 8 dup (?)	;name "MSWIN4.1"
	EBPB_FAT32 <>
BSECT ends

	.code

	org BSADDR

;--- sector# 0: the boot sector read in by the MBR loader
;--- reads MBR to check if LBA access can be used.
;--- reads sectors 1 & 2.
;--- jumps to start of sector 2.

start:
	jmp start2
	nop
	db "MSWIN4.1"

bpb EBPB_FAT32 <>

start2:
	cli
	xor   cx, cx
	mov   ss, cx
	mov   sp, BSADDR-8
	mov   es, cx
	mov   bp, 4*1Eh		; 0078h
	lds   si, [bp+0]	; int 1Eh
	push  ds
	push  si
	push  ss
	push  bp
	mov   di, 0522h
	mov   [bp+0], di	; set int 1Eh vector to 0000:0522
	mov   [bp+2], cx
	mov   cl, 0Bh		; copy structure ( diskette parameter )
	cld
	rep   movsb
	mov   ds, cx

;--- now CS=SS=DS=ES=0000

	mov   bp, BSADDR
	mov   byte ptr [di-02], 0Fh
	mov   ax, [bp].BSECT.sectors_track	; [bp+18h]
	mov   [di-07h], al
	cmp   [bp].BSECT.phys_drive, cl	; [bp+40h]
	jge   L7CB5					; jump if drive is a floppy
	mov   ax, cx
	cwd
	mov   bx, MBRADDR			; read MBR at 0000:0700
	call  L7D30
	jc    L7CB5
	sub   bx, 3Ah				; bx = 0900h - 3A = 8C6 (=part_table+8)
	mov   eax, [bpb.hidden_sectors]
L7CA2:							; <---
	cmp   eax, [bx]				; check startLBA if correct partition found?
	mov   dl, [bx-04h]			; get partition type
ife ?FORCELBA
	jnz   skippe
	or    dl, 2					; set flag to use LBA for reads
else
	nop							; use NOPs so offset won't change
	nop
	mov dl, 0Eh					; 0E=use LBA reads
	nop
endif
	mov   [bp].BSECT.bType, dl
skippe:
	add   bl, SIZEPTE
	jnc   L7CA2					; next partition entry
L7CB5:
	mov   di, 2					; 2 tries
	cmp   [bp].BSECT.sectors_fat, 0		; [bp+16h]
	jnz   error1
	mov   ax, word ptr [bp].BSECT.hidden_sectors+0	; [bp+1Ch]
	mov   dx, word ptr [bp].BSECT.hidden_sectors+2	; [bp+1Eh]
	mov   cx, 3
L7CC7:					; <--- next try
	dec   cx
	inc   ax
	jnz   @F
	inc   dx
@@:
	mov   bx, BSADDR+200h; read sector 1 (fs info) and 2 (2. boot sector)
	call  read_sectors
	jnc   bs_ok
	mov   al, 0F8h		; ???
	dec   di			; was it second try?
	jz    L7CF6
	mov   ax, [bp].BSECT.bs_copy_start	; [bp+32h]
	xor   dx, dx
	mov   cx, 3
	cmp   cx, ax
	ja    error1			; error ( location of copy of BS must >= 3 AND < reserved sectors )
	mov   si, [bp].BSECT.reserved_sectors	; [bp+0Eh]
	cmp   cx, si
	jnc   error1			; error ( location must be within "reserved" sectors )
	sub   si, cx			; ??? ( subtraction is useless here since SI isn't used later )
	add   ax, word ptr [bp].BSECT.hidden_sectors+0	; [bp+1Ch]
	adc   dx, word ptr [bp].BSECT.hidden_sectors+2	; [bp+1Eh]
	jmp   L7CC7				; try again, this time with backup copy
L7CF6:
	jnc   error1			; "ungueltiges system"
	jmp   error2			; "e/a fehler"
bs_ok:
	cmp   [bp].BSECT.version, 0 ; [bp+2Ah]
	ja    error1			; "ungueltiges system"
	jmp   readiosys			; jmp to 2. boot sector ( to load first 4 sectors of IO.SYS )

error1:
	mov   si, offset L7D7E	; "ungueltiges system"
emsgexit:				; <--- display err msg SI
	lodsb
	cbw
	add   si, ax
@@:
	lodsb
	test  al, al
	jz    L7D26		; done display
	cmp   al, 0FFh
	jz    @F		; first string ok, now display second string
	mov   ah, 0Eh
	mov   bx, 0007h
	int   10h
	jmp   @B		; next char
@@:
	mov   si, offset L7D81	; "Datentraeger wechseln und Taste druecken"
	jmp   emsgexit	;--->
error2:
	mov   si, offset L7D7F	; "E/A-Fehler"
	jmp   emsgexit		;--->
L7D26:
	cbw
	int   16h		; wait for a key
	pop   si		; restore int 1Eh
	pop   ds
	pop   dword ptr [si]
	int   19h		; reboot

;--- read MBR, CX=0 

L7D30:
	inc   cx

;--- read CX sectors; es:bx=destination, dx:ax=sector#
;--- updates BX

read_sectors proc		; <--- next sector (L7D31)
	push  si
	pushd byte ptr 0	; DAP.LBA sector#.32-63
	push  dx			; DAP.LBA sector#.16-31
	push  ax			; DAP.LBA sector#.00-15
	push  es			; DAP.buffer_segm
	push  bx			; DAP.buffer_offset
	pushw byte ptr 1	; DAP.wSectors
	pushw byte ptr 10h	; DAP.size (+res)
	mov   si, sp
	pusha
	cmp   [bp].BSECT.bType, 0Eh	; read LBA or CHS?
	jnz   @F
	mov   ah, 42h	; read LBA
	jmp   callint13
@@:
	xchg  ax, cx
	xchg  ax, dx
	xor   dx, dx
	div   [bp].BSECT.sectors_track	; [bp+18h]
	xchg  ax, cx
	div   [bp].BSECT.sectors_track	; [bp+18h]
	inc   dx
	xchg  cx, dx
	div   [bp].BSECT.no_of_tracks 	; [bp+1Ah]
	mov   dh, dl
	mov   ch, al
	ror   ah, 2
	or    cl, ah
	mov   ax, 201h	; read CHS
callint13:
	mov   dl, [bp].BSECT.phys_drive	; [bp+40h]
	int   13h
	popa
	lea   sp, [si+8*2]				; get rid of stack parms (DAP)
	pop   si
	jc    exit
	inc   ax
	jnz   @F
	inc   dx
@@:
	add   bx, [bp].BSECT.bytes_sector; [bp+0Bh]
	dec   cx
	jnz   read_sectors				; next sector
exit:
	ret
read_sectors endp

L7D7E   db offset msg1 - ($+1)	; offset to "Ungueltiges System"
L7D7F   db offset msg2 - ($+1)	; offset to "E/A-Fehler"
L7D80   db offset msg1 - ($+1)	; offset to "Ungueltiges System"
L7D81   db offset msg3 - ($+1)	; offset to "Datentraeger wechseln..."
msg1    db 0Dh, 0Ah, "Ungueltiges System ", -1
msg2    db 0Dh, 0Ah, "E/A-Fehler    ", -1
msg3    db 0Dh, 0Ah, "Datentraeger wechseln und Taste druecken", 0Dh, 0Ah, 0
        db 0, 0	; not used
namekrnl db "IO      SYS"
        db "MSDOS   SYS"	; not used

	dw offset L7D7E - offset start	; not used
	db 0	; not used

	db "WINBOOT SYS"	; not used
	dw 0	; not used

	org BSADDR+1FEh
	dw 0AA55h

;--- sector# 1: the "Filesystem Info" sector

	db "RRaA"

	org BSADDR+200h+1E4h
	db "rrAa"
	dd -1
	dd -1
	org BSADDR+200h+1FEh
	dw 0AA55h

;--- sector# 2: the "second" boot sector of FAT32 disks

;--- reads first 4 sectors of IO.SYS at 0000:0700h
;--- for this, it has to read the root directory and the FAT
;--- if everything ok, jumps to 0070:0200h (start IO.SYS+200h)
;--- in: BP = BSADDR (ptr BSECT)
;---     SP= BP-16
;---     CS=SS=DS=ES=0000

;--- lots of CLIs/STIs in this code - pretty useless.

readiosys proc
	CLI
	MOVZX   EAX, [bp].BSECT.num_fats		; [BP+10h]
	MOV     ECX, [bp].BSECT.sectors_fat32	; [BP+24h]
	MUL     ECX
	ADD     EAX, [bp].BSECT.hidden_sectors	; [BP+1Ch]
	MOVZX   EDX, [bp].BSECT.reserved_sectors; [BP+0Eh]
	ADD     EAX, EDX
	XOR     CX, CX
	MOV     [bp].BSECT.dwStartData, EAX		; start of data region
	MOV     [bp].BSECT.dwFatSecNo, -1		; no FAT sector read yet
	CLI
	MOV     EAX, [bp].BSECT.root_startcl	; [BP+2Ch]
	CMP     EAX, 2
	JB      error1
	CMP     EAX, 0FFFFFF8h
	JAE     error1
	SHLD    EDX, EAX, 16
	STI
nextcluster:					; <--- next cluster
	PUSH    DX
	PUSH    AX
	CLI
	SHL     EAX, 16			; "shl eax,16"+"shrd eax,edx,16" = 9 bytes
	SHRD    EAX, EDX, 16	; "mov eax, [bp-20]"             = 4 bytes!
	SUB     EAX, 2
	MOVZX   EBX, [bp].BSECT.sectors_cluster	; [BP+0Dh]
	MOV     SI, BX
	MUL     EBX								; eax=rel. sector# of cluster
	ADD     EAX, [bp].BSECT.dwStartData		; eax=abs. sector# of cluster
	SHLD    EDX, EAX, 16					; dx:ax=abs. sector#
	STI
nextdirsect:			; <--- read next directory sector
	MOV     BX, ROOTADDR
	MOV     DI, BX
	MOV     CX, 1
	CALL    read_sectors; read 1 directory sector ( sets BX = BX+200h! )
	JB      error2		; "e/a fehler"
nextdirentry:			; <--- next dir entry
	CMP     [DI], CH 	; end of directory?
	JZ      done_root
	MOV     CL, 8+3
	PUSH    SI
	MOV     SI, offset namekrnl	; "IO      SYS"
	REPE    CMPSB
	POP     SI
	JZ      found
	ADD     DI, CX
	ADD     DI, 21		; 21+11=32 ( size of dir entry )
	CMP     DI, BX		; end of dir sector reached?
	JB      nextdirentry; ---> check next dir entry
	DEC     SI			; another sector in current cluster?
	JNZ     nextdirsect	; ---> read next sector
	POP     AX
	POP     DX
	CALL    getnxtcl	; get next cluster
	JB      nextcluster	; ---> check next cluster
;--- stack is already adjusted, so actually the "add sp,4" below is a bug in THIS case.
done_root:
	ADD     SP, 4		; adjusting stack
	JMP     error1		; "ungueltiges system"
found:					; "io.sys" found!
	ADD     SP, 4		; io.sys pops 4 words from stack - to be able to restore int 1Eh?
	MOV     SI, [DI+09h]; get start cluster ( IO.SYS may expect this value in SI:DI! )
	MOV     DI, [DI+0Fh]
	MOV     AX, SI
	CLI
	SHL     EAX, 16		; "mov ax,si", "shl eax,16", "mov ax,di" = 8 bytes
	MOV     AX, DI		; "push si", "push di", "pop eax"        = 4 bytes!
	CMP     EAX, 2
	JB      invalid
	CMP     EAX, 0FFFFFF8h
	JAE     invalid
	DEC     EAX			; start cluster is valid, now convert to sector#
	DEC     EAX
	MOVZX   ECX, [bp].BSECT.sectors_cluster	;[BP+0Dh]
	MUL     ECX
	ADD     EAX, [bp].BSECT.dwStartData
	SHLD    EDX, EAX, 16
	STI
	MOV     BX, IOSYSADDR
	PUSH    BX
	MOV     CX, 4				;read 4 sectors of IO.SYS at 0070:0000
	CALL    read_sectors
	POP     BX
	JB      error2	; "e/a fehler"
	CMP     WORD PTR [BX], 5A4Dh;check "MZ"
	JNZ     invalid
	CMP     WORD PTR [BX+0200h], 4A42h	; this is "BJ", actually "inc dx", "dec dx"
	JZ      iosys_ok
invalid:
	MOV     SI, offset L7D80	;"ungueltiges system"
	JMP     emsgexit	;error
iosys_ok:
if ?ICINT13
	call installi13
endif
;	JMP     0070h:0200h
	db 0eah
	dw 0200h, 0070h		;jump to loaded part of IO.SYS, registers BP, SP, SI, DI must be correct!

readiosys endp

;--- get next cluster of root dir
;--- in DX:AX=current cluster#
;--- out: C if new cluster# is valid, DX:AX=next cluster#

getnxtcl proc
	ADD     AX, AX		; cluster# * 4, since size of FAT entry in FAT32 is 4!
	ADC     DX, DX
	ADD     AX, AX
	ADC     DX, DX
	CALL    readfatsec
	CLI
	MOV     EAX, ES:[BX+DI]	; eax=next cluster ( the ES segment prefix is useless here! )
	AND     EAX, 0FFFFFFFh
	SHLD    EDX, EAX, 16
	CMP     EAX, 0FFFFFF8h	; beyond max. cluster#?
	STI
	RET
getnxtcl endp

;--- read FAT sector
;--- in:  DX:AX=offset in FAT of current cluster's entry
;--- out: DI+BX=addr (in FAT buffer) of current cluster's entry

readfatsec proc
	MOV     DI, FATADDR
	CLI
	SHL     EAX, 16
	SHRD    EAX, EDX, 16					; now offset in EAX
	MOVZX   ECX, [bp].BSECT.bytes_sector	; [BP+0Bh]
	XOR     EDX, EDX
	DIV     ECX								; EAX = rel. sector# of entry in FAT, DX=offset in sector
	CMP     EAX, [bp].BSECT.dwFatSecNo		; is the FAT sector already read in?
	JZ      done
	MOV     [bp].BSECT.dwFatSecNo, EAX
	ADD     EAX, [bp].BSECT.hidden_sectors	; [BP+1Ch]
	MOVZX   ECX, [bp].BSECT.reserved_sectors; [BP+0Eh]
	ADD     EAX, ECX

;--- is the first FAT to be used?
	MOVZX   EBX, [bp].BSECT.flags			; [BP+28h]
	AND     BX, 0Fh
	JZ      @F
	CMP     BL, [bp].BSECT.num_fats			; [BP+10h]
	JAE     error1							; "ungueltiges system"
	PUSH    DX
	MOV     ECX, EAX
	MOV     EAX, [bp].BSECT.sectors_fat32	; [BP+24h]
	MUL     EBX
	ADD     EAX, ECX
	POP     DX
@@:
	PUSH    DX
	SHLD    EDX, EAX, 16
	STI
	MOV     BX, DI
	MOV     CX, 1
	CALL    read_sectors
	POP     DX
	JB      error2		; "e/a fehler"
done:
	STI
	MOV     BX, DX		; BX=offset within FAT sector for current entry
	RET
readfatsec endp

if ?ICINT13

;--- install an int 13h handler that emulates an MBR with a FAT32 partition in first PT entry

installi13 proc
	pusha
	mov eax, [bp].BSECT.hidden_sectors
	mov @startlba, eax
	mov eax, [bp].BSECT.sectors_32
	mov @sizelba, eax
	mov di, 3BCh
	movzx eax, di
	xchg eax, ds:[13h*4]
	mov cs:[oldint13], eax
	mov si, offset myint13
	mov cx, sizeint13
	rep movsb
	popa
	ret

myint13:
	push ax
	pushf
	db 9ah	; 9A = opcode for "call far16"
oldint13 dd 0
	push bp
	mov bp,sp
	jc done
	cmp byte ptr [bp+2+1],2	;CHS read?
	jnz @F
	cmp dx, 0080h	; first HD, head 0?
	jnz @F
	cmp cx, 1		; cylinder 0, sector 1?
	jnz @F
	mov byte ptr es:[bx+PTOFS+4], 0Ch	; set partition type ( FAT32, LBA )
	mov dword ptr es:[bx+PTOFS+8], 0	; set LBA start
@startlba equ dword ptr $ - 4
	mov dword ptr es:[bx+PTOFS+12], 0	; set size
@sizelba equ dword ptr $ - 4
@@:
	clc
done:
	rcr byte ptr [bp+8], 1	; return carry flag value
	rol byte ptr [bp+8], 1
	pop bp
	add sp, 2
	iret
sizeint13 equ $ - offset myint13

installi13 endp

endif

	org BSADDR+400h+1FEh
	dw 0AA55h
	end
