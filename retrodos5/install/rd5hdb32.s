; ****************************************************************************
; RD5HDB32.ASM (RD5HDB32.COM) - Retro DOS v5 Hard Disk FAT32 Boot Sect Utility
;							  (for MSDOS/WINDOWS)
; ----------------------------------------------------------------------------
; Last Update: 03/05/2024
; ----------------------------------------------------------------------------
; Beginning: 03/05/2024
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (rd5hdb32.s)
; ----------------------------------------------------------------------------
; Modified from 'trhdboot.s'(TRHDBOOT.COM) and source code by Erdogan Tan
; (12/09/2020) - TRDOS 386 v2 hard disk boot sector modification utility -
; ----------------------------------------------------------------------------
; Modified from 'rd5hdb16.s'(RD5HDB16.COM) and source code by Erdogan Tan
; (20/09/2024) - Retro DOS v5 hard disk FAT16 boot sect modification utility -
; ****************************************************************************
; assembling: nasm rd5hdb32.s -l rd5hdb32.txt -o RD5HDB32.COM -Z error.txt

; ----------------------------------------------------------------------------
; equations
; ----------------------------------------------------------------------------

; boot sector parameters

bsOemName	equ 3	; ('MSWIN4.1') --> 'RETRODOS'  ; 03/05/2024
bsBytesPerSec	equ 11	; 512 (word)
bsSecPerClust	equ 13
bsResSectors	equ 14
bsFATs		equ 16
bsRootDirEnts	equ 17
bsSectors	equ 19
bsMedia		equ 21	; 0F8h
bsFATsecs	equ 22
bsSecPerTrack	equ 24
bsHeads		equ 26
bsHidden1	equ 28
bsHidden2	equ 30
bsHugeSectors	equ 32
; FAT 16 bs & FAT 12 bs
bsDriveNumber	equ 36	; 80h
bsReserved1	equ 37
bsBpbSignature	equ 38	; 29h (byte)
bsVolumeID	equ 39
bsVolumeLabel	equ 43
bsFileSysType	equ 54	; 'FAT16   '  (8 bytes)
; FAT 32 bs
BPB_FATSz32	equ 36
BPB_ExtFlags	equ 40
BPB_FSVer	equ 42
BPB_RootClus	equ 44
BPB_FSInfo	equ 48
BPB_BkBootSec	equ 50
BPB_Reserved	equ 52
BS_DrvNum	equ 64	; 80h
BS_Reserved1	equ 65
BS_BootSig	equ 66	; 29h (byte)
BS_VolID	equ 67
BS_VolLab	equ 71
BS_FilSysType	equ 82	; 'FAT32   '  (8 bytes)

; Masterboot / Partition Table at Beginning+1BEh
ptBootable      equ 0
ptBeginHead     equ 1
ptBeginSector   equ 2
ptBeginCylinder equ 3
ptFileSystemID	equ 4
ptEndHead       equ 5
ptEndSector     equ 6
ptEndCylinder   equ 7
ptStartSector   equ 8
ptSectors       equ 12

partition_table equ 1BEh

; ----------------------------------------------------------------------------
; code
; ----------------------------------------------------------------------------

[BITS 16]
[ORG 100h]

	;cli
	;cld
	;push	cs
	;pop	ss
	;mov	sp, 0FFFEh
	;sti

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, 80h			; PSP command tail
	mov	cl, [si]
	or	cl, cl                               
	jz	short T_9		; jump if zero
T_1:
	inc	si

	mov	al, [si]
	cmp	al, ' '			; is it SPACE ?
	jne	short T_2

	dec	cl
	jnz	short T_1
	jmp	short T_9
T_2:
	cmp	al, 'C'
	jb	short T_9
	je	short T_6
	cmp	al, 'D'
	jna	short T_6
	cmp	al, 'Z'
	jna	short T_9
T_4:	
	cmp	al, 'c'			; a - z 
	jb	short T_9
	je	short T_5
	cmp	al, 'd'
	ja	short T_9
T_5:
	sub	al, 'a'-'A'		; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_6:
	add	al, 80h-'C'
T_7:
	mov	[drv], al	; 80h or 81h

	mov	ah, al
	sub	ah, 80h-'0'
	mov	[RD5_Drive], ah ; '0' or '1'

	jmp	short T_10

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_9:
	mov	si, RD5_Welcome
	call	print_msg
	;cmp	cl, 0
        ;ja	short T_35
	jmp	T_35

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_10:
	mov	ah, 08h
	;mov	dl, [drv]		; drive (80h or 81h)
	mov	dl, al
	int	13h			; return disk parameters

	push	cs
	pop	es			; restore es

	or	ah, ah
	jnz	short T_12		; error
	
	mov	al, cl
	and	al, 63
	mov	[sectors], al
	shr	cl, 6 
	xchg	ch, cl
	inc	cx
	mov	[cylinders], cx
	inc	dh
	mov	[heads], dh
	mul	dh
		; ax = heads * spt
	mul	cx ; * cylinders
		; dx:ax = chs limit
	mov	[CHS_limit], ax
	mov	[CHS_limit+2], dx

	; check for (valid) primary dos partition

	;mov	byte [RetryCount], 4

	;mov	ax, 0201h		; read disk
	mov	bx, MBR			; location of masterboot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [drv]
T_11:
	mov	ax, 0201h
	int	13h
	;jc	short T_12
	jnc	short T_13		; read masterboot sector, OK

	dec	byte [RetryCount]
	jnz	short T_11
T_12:
	mov	byte [zbyte], 0
	call	T_37			; write error message
	jmp	T_35 			; terminate

T_13:
	cmp	word [MBR+510], 0AA55h 
        jne	short T_12

	mov	si, MBR+(partition_table+ptFileSystemID)
T_14:
	call	validate_dos_FAT32_partition
	jnc	short T_15
 
	add	si, 16
	cmp	si, MBR+partition_table+ptFileSystemID+64
	jb	short T_14

	mov	si, RD5_fatp_notfound
	call	print_msg
	jmp	T_35
	
T_15:
	; valid primary dos (FAT32) partition
	; ah = partition type

	; 03/05/2024
	mov	byte [fattype], 3  ; FAT32
T_17:	
	mov	byte [RetryCount], 5

	add	si, ptStartSector-ptFileSystemID
	mov	ax, [si]
	mov	dx, [si+2]
	mov	[dosp_start], ax
	mov	[dosp_start+2], dx
	add	si, ptSectors-ptStartSector
	mov	cx, [si]
	mov	bx, [si+2]
	mov	[dosp_size], cx
	mov	[dosp_size+2], bx
	add	cx, ax
	adc	bx, dx
	jc	short T_12

	cmp	bx, [CHS_limit+2]
	mov	bx, bootsector
	ja	short T_20 ; LBA read/write
	jb	short T_18
	cmp	cx, [CHS_limit]
	ja	short T_20
T_18:
	; CHS read

	sub	si, ptSectors-ptBeginHead

	mov	dh, [si] ; head
	inc	si
	mov	cx, [si] ; sector
		; cl = sector, ch = cylinder
	;mov	bx, bootsector
	mov	dl, [drv]

	mov	[_dh], dh
	mov	[_cx], cx
T_19:
	mov	ax, 0201h ; read one sector
	int	13h
	jnc	short T_22 ; OK
	dec	byte [RetryCount]
	jnz	short T_19
	jmp	T_12
T_20:
	; LBA read
	mov	byte [lba], 1
	;mov	ax, [dosp_start]
	;mov	dx, [dosp_start+2]
T_21:
	;pusha				; db 60h
	db	60h
	;push 	0                       ; db 6Ah, 00h
	db	6Ah, 0
	;push	0                       ; db 6Ah, 00h
	db	6Ah, 0
	push    dx
	push    ax
	push    es
	push    bx
	;push	1			; db 6Ah, 01h
	db	6Ah, 01h                     
	;push	10h                     ; db 6Ah, 10h
	db	6Ah, 10h

	mov     dl, [drv]
	mov     ah, 42h
	mov     si, sp
	int     13h

	;popa
	db	61h
	;popa
	db	61h
	jnc     short T_22
                
        dec	byte [RetryCount]
	jz	T_12

	mov	ax, [dosp_start]
	mov	dx, [dosp_start+2]
	jmp	short T_21	 

T_22:
	cmp	word [bootsector+510], 0AA55h
	jne	short T_23

	cmp	word [bootsector+bsBytesPerSec], 512
	jne	short T_23

	cmp	byte [bootsector+bsMedia], 0F8h
	je	short T_24
T_23:
	mov	si, RD5_invalid_bootsector
	call	print_msg
	jmp	T_35
T_24:
	; 03/05/2024
	cmp	word [bootsector+bsFATsecs], 0
	ja	short T_23	; not FAT32 fs

	cmp	byte [bootsector+BS_BootSig], 29h
	jne	short T_23
	cmp	dword [bootsector+BS_FilSysType], 'FAT3'
	jne	short T_23
	cmp	byte [bootsector+BS_FilSysType+4], '2'
	jne	short T_23

	mov	cx, 82-11 ; byte count to be copied 
T_25:
	mov	si, RD5_Do_you_want
	call	print_msg
T_26:
	;xor	ax, ax
	;int	16h			; wait for keyboard command
	;cmp	al, 'y'
	;je	short T_27		; retry
	;cmp	al, 'Y'
	;je	short T_27
	;cmp	al, 'n'
	;je	short T_35 		; exit
	;cmp	al, 'N'
	;je	short T_35
	;cmp	al, 'C'-40h
	;je	short T_35
	;cmp	al, 27
	;je	short T_35
	;jmp	short T_26

	call	get_answer
	cmp	al, 'Y'
	je	short T_27

	mov	si, _no_str
	call	print_msg

	jmp	T_35
T_27:
	mov	si, _yes_str
	call	print_msg

	;mov	si, RD5_CRLF
	;call	print_msg

	; set 'RETRODOS' as OEM name
	mov	di, RD5_FAT32_hd_bs
	add	di, bsOemName
	mov	ax, 'RE'
	stosw
	mov	ax, 'TR'
	stosw
	mov	ax, 'OD'
	stosw
	mov	ax, 'OS'
	stosw
	
	; DI points to retrodosv5bs+bsBytesPerSec
	mov	si, bootsector+bsBytesPerSec
	
	rep	movsb

	mov	si, RD5_PressKeyWhenReady
	call	print_msg
T_28:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'M'-40h		; Enter (OK) key
	je	short T_29		; write
	cmp	al, 'C'-40h
	je	short T_35		; no write (exit)
	cmp	al, 27
	je	short T_35
	jmp	short T_28

T_29:
	mov	si, RD5_CRLF
	call	print_msg
T_30:
	;xor	ax, ax
	;int	1Ah			; get time of day
	
	;mov	si, volume_id
	
	;mov	[si], dx
	;mov	[si+2], cx		; set unique volume ID
	
	;mov	ah, 02h			; Return Current Time
	;int	1Ah
	;xchg	ch, cl
	;xchg	dh, dl
	
	;add	cx, dx
	;add	[si+2], cx
		
	;mov	ah, 04h			; Return Current Date
	;int	1Ah
	;xchg	ch, cl
	;xchg	dh, dl
	
	;add	cx, dx  
	;add	[si+2], cx

	;mov	ax, [vol_id]
	;mov	dx, [vol_id+2]

	mov	bx, RD5_FAT32_hd_bs	; location of boot code
	; es: bx = boot sector buffer address
	
	;mov	si, bx	
	;add	si, bsVolumeID

	;cmp	byte [fattype], 3
	;jne	short T_31

	;add	si, BS_VolID-bsVolumeID
;T_31:
	;mov	[si], ax
	;mov	[si+2], dx

	cmp	byte [lba], 1
	jnb	short T_32 ; LBA write

	mov	dl, [drv] ; drive
	mov	dh, [_dh] ; head
	mov	cx, [_cx] ; cl = sector, ch = cylinder (low 8 bits)
T_31:
	mov	ax, 0301h		; write to disk
	; es: bx = boot sector buffer address

	int	13h
	jnc	short T_34		; ok

	; error

	dec	byte [RetryCount]
	jnz	short T_31
	call	T_37
	jmp	short T_36

T_32:
	mov	ax, [dosp_start]
	mov	dx, [dosp_start+2]
T_33:
	;pusha				; db 60h
	db	60h
	;push 	0                       ; db 6Ah, 00h
	db	6Ah, 0
	;push	0                       ; db 6Ah, 00h
	db	6Ah, 0
	push    dx
	push    ax
	push    es
	push    bx
	;push	1			; db 6Ah, 01h
	db	6Ah, 01h                     
	;push	10h                     ; db 6Ah, 10h
	db	6Ah, 10h

	mov     dl, [drv]
	mov     ah, 43h ; LBA write
	xor	al, al ; verify off
	mov     si, sp
	int     13h

	;popa
	db	61h
	;popa
	db	61h
	jnc     short T_34

	dec	byte [RetryCount]
	jnz	short T_32
	call	T_37
	jmp	short T_36

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; success. try again ?
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_34:
	cmp	byte [fattype], 3 ; FAT32
	je	short T_40

	mov	si, RD5_disk_WrittenSuccesfully
	call	print_msg
T_35:
	mov	si, RD5_CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h
T_36:
	;xor	ax, ax
	;int	16h			; wait for keyboard command
	;cmp	al, 'y'
	;je	short TX_15		; retry
	;cmp	al, 'Y'
	;je	short TX_15
	;cmp	al, 'n'
	;je	short T_35 		; exit
	;cmp	al, 'N'
	;je	short T_35
	;cmp	al, 'C'-40h
	;je	short T_35                   
	;cmp	al, 27
	;je	short T_35
	;jmp	short T_36

	call	get_answer
	cmp	al, 'Y'
	je	short T_29
	jmp	short T_35

T_37:
	mov	si, RD5_disk_NotReadyOrError
	;;call	print_msg
	;;jmp	short T_36
	;jmp	short print_msg

print_msg:
T_38:
	lodsb				; Load byte at DS:SI to AL
	and	al, al
	jz	short T_39
	mov	ah, 0Eh
	mov	bx, 07h
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short T_38
T_39:
_NO_:
	retn

T_40:
	; write 2nd sector of FAT32 bs code (1024 bytes)

	mov	byte [RetryCount], 4
	mov	byte [fattype], 0

	mov	bx, RD5_FAT32_hd_bs+512

	cmp	byte [lba], 0
	jna	short T_41

	mov	ax, [dosp_start]
	mov	dx, [dosp_start+2]
	add	ax, 2 ; sector 2 in the partition (after FSINFO sector)
	adc	dx, 0
	jmp	short T_33
T_41:
	; convert FAT32 bootsector+2 address to CHS
	;mov	dl, [drv] ; drive
	;mov	dh, [_dh] ; head
	;mov	cx, [_cx] ; cl = sector, ch = cylinder (low 8 bits)
	mov	ax, cx
	and	al, 63
	cmp	al, [sectors]
	jnb	short T_43
	inc	al
 	cmp	al, [sectors]
	jnb	short T_42
	inc	cl
	inc	cl
	jmp	T_31
T_42:
	mov	al, 1	; sector 1
	jmp	short T_44
T_43:
	mov	al, 2	; sector 2
T_44:
	inc	dh
	cmp	dh, [heads]
	jb	T_31
	sub	dh, dh  ; head 0
	shr	cl, 6
	xchg	ch, cl
	inc	cx	; next cylinder
	;and	cx, 1023
	;cmp	cx, [cylinders]
	;jnb	short T_37
	xchg	cl, ch
	shl	cl, 6
	or	cl, al	
	jmp	T_31
		 
get_answer:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'y'
	je	short _yes		; retry
	cmp	al, 'Y'
	je	short _YES_
	cmp	al, 'n'
	je	short _NO_ 		; exit
	cmp	al, 'N'
	je	short _NO_
	cmp	al, 'C'-40h
	je	short _NO_
	cmp	al, 27
	je	short _NO_
	jmp	short get_answer
_yes:
	mov	al, 'Y'
_YES_:
	retn

validate_dos_FAT32_partition:
	
	; INPUT:
	;   si = partition table entry offset + file system ID 
	; OUTPUT:
	;   cf = 0 -> ah = primary DOS partition ID
	;			 (0Bh or 0Ch)
	;   cf = 1 -> not a DOS (Windows) FAT32 partition

	; 03/05/2024
	cmp	ah, 0Ch	; FAT32 LBA partition
	je	short v_1
	ja	short v_2

	cmp	ah, 0Bh	; FAT32 CHS partition
v_1:
	retn
v_2:
	stc
	retn

; ----------------------------------------------------------------------------
; initialized data
; ----------------------------------------------------------------------------

align 2

;retrodosv5bs:
;	dw RD5_FAT16_hd_bs
;	dw 0

;volume_id:
;	dd 0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  FAT boot sector code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 03/05/2024
RD5_FAT32_hd_bs:
	incbin	'RD5HDBS3.BIN'  ; 29/04/2024

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

drv:	db 0

;sectors: db 0
;heads	: db 0
;cylinders: dw 0

_dh:	db 0
_cx:	dw 0

RD5_Welcome:
	db 0Dh, 0Ah
	db 'Retro DOS v5.0 Hard Disk (FAT32) Boot Sector Update Utility '
	db 0Dh, 0Ah
	db "RDHDBOOT v4.0.240420  (c) Erdogan TAN 2018-2024"
	db 0Dh,0Ah
	db 0Dh,0Ah
	db 'Usage: rd5hdb32 c: (or d:)'
	db 0

RD5_Do_you_want:
	db 0Dh, 0Ah
	db "WARNING ! ", 0Dh, 0Ah 
	db "(If you say 'Yes', MSDOS or WINDOWS will not be bootable on this disk !) "
	db 0Dh, 0Ah, 0Dh, 0Ah
	; 03/05/2024
	db "Do you want to update FAT32 boot sector to Retro DOS v5 format ? (Y/N) "
	db 0

_yes_str:
	db 'YES '
	db 0Dh, 0Ah, 0
_no_str:
	db 'NO '
	db 0Dh, 0Ah, 0

RD5_PressKeyWhenReady:
	db 0Dh, 0Ah
	db 'Press Enter to write boot sector on disk hd'
RD5_Drive:
	db '?. ', 0

RD5_disk_WrittenSuccesfully:
	db 0Dh, 0Ah
	; 03/05/2024
	db 'Boot sector successfully updated to Retro DOS v5 format...'
RD5_CRLF:
	db 0Dh, 0Ah, 0

RD5_disk_NotReadyOrError:
	db 0Dh, 0Ah
	db 'Disk error or drive not ready ! '
zbyte:	db 'Try again ? (Y/N) '
	db 0

RD5_invalid_bootsector:
	db 0Dh, 0Ah
	; 03/05/2024
	db 'Invalid boot sector (not a valid FAT32 fs disk) ! '
	db 0

RD5_fatp_notfound:
	db 0Dh, 0Ah
	; 03/05/2024
	db 'MBR does not contain a FAT32 (primary DOS) partition ! '
fattype:
	db 0
RetryCount:
	db 4

lba:	db 0

align 4

CHS_limit: 
	dw 0
	;dw 0

sign:	dw 417

; ----------------------------------------------------------------------------
; uninitialized data
; ----------------------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

alignb 4

sectors: resb 1
heads:	 resb 1
cylinders: resw 1

dosp_start: resd 1
dosp_size:  resd 1

MBR:
bootsector:
	resb 512

end_bss: