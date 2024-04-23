; ****************************************************************************
; RD5HDB12.ASM (RD5HB12.COM) - Retro DOS v5 Hard Disk FAT12 Boot Sect Utility
; 					 	          (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 20/04/2024
; ----------------------------------------------------------------------------
; Beginning: 16/05/2018
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Modified from 'rdhdboot.s'(RDHDBOOT.COM) source code by Erdogan Tan
; (25/10/2023) - Retro DOS v2-v4 hard disk boot sector modification utility -
; ****************************************************************************
; nasm rd5hdb12.s -l rd5hdb12.txt -o RD5HDB12.COM

bsOemName	equ 3
bsBytesPerSec	equ 11
bsSecPerClust	equ 13
bsResSectors	equ 14
bsFATs		equ 16
bsRootDirEnts	equ 17
bsSectors	equ 19
bsMedia		equ 21
bsFATsecs	equ 22
bsSecPerTrk	equ 24
bsHeads		equ 26
bsHidden1	equ 28
bsHidden2	equ 30
bsHugeSectors	equ 32
bsDriveNumber   equ 36
bsReserved1	equ 37
bsBpbSignature	equ 38
bsVolumeID      equ 39
bsVolumeLabel   equ 43
bsFileSysType	equ 54
bsReserved2	equ 62
bsDataStart	equ 64
bsRootDirStart	equ 66
bsRootDirSects	equ 68
; 22/10/2023
bsDirEntsPerSec equ 70

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

	; 21/10/2023 - 22/10/2023

[BITS 16]
[ORG 100h]

	;cli
	;cld
	;push	cs
	;pop	ss
	;mov	sp, 0FFFEh
	;sti
	
	mov	bx, SizeOfFile+100
        add	bx, 15
        shr	bx, 1
        shr	bx, 1
	shr	bx, 1
	shr	bx, 1
        mov	ah, 4Ah ; modify memory allocation
        ;push	cs
        ;pop	es
        int	21h 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, 80h			; PSP command tail
 	lodsb
	or	al, al 			; command tail length
	jz	short R_05		; jump if zero
R_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short R_01
	jb	short R_05
	
	; check disk drive name

	cmp	al, 'C'
        jb	short R_05
        cmp	al, 'Z' + 1		; C - Z
        jb	short R_02                   
        cmp	al, 'c'			; C - z 
        jb	short R_05
        cmp	al, 'z' + 1
        jnb	short R_05

        sub	al, 'c'-'C'		; to upper case

R_02:
	add	al, 80h-'C'
	mov	[DriveNum],al
	mov	dl, al
	xor	al, al
R_03:
	mov	ah, al
	lodsb
	cmp	al, 20h
	je	short R_03
	jb	short R_04
	cmp	al, ':'
	jne	short R_05
	and	ah, ah
	jnz	short R_05
R_04:
	cmp	ah, 20h
	jna	short R_06
R_05:
	mov	si, RetroDOS_Welcome
	call	print_msg
	jmp	R_10	; Exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 21/10/2023 (short jumps)

R_06:
	;mov	dl, [DriveNum]	
	mov	ah, 8 ; Get drive parameters
	int	13h
	jc	short R_15   ; Drive not ready error

	inc	dh
	mov	[Heads], dh
	mov	ah, cl
	and	cl, 3Fh
	mov	[Sectors], cl
	shr	ah, 6
	mov	al, ch
	inc	ax
	mov	[Cylinders], ax

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Read masterboot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	push	ds
	pop	es

	mov	bx, SectorBuffer

	mov	dl, [DriveNum]	
	mov	ax, 0201h ; Read 1 sector
	mov	cx, 1 ; cylinder = 0, sector = 1
	xor	dh, dh ; head = 0
	int	13h
	jc	short R_16  ; Read error

	cmp	word [bx+510], 0AA55h
	jne	short R_17  ; Not a valid MBR

	add	bx, partition_table
	mov	cl, 4
R_07:
	cmp	byte [bx+ptFileSystemID], 1  ; MSDOS (FAT12) Partition ID
	je	short R_08
	dec	cl
	jz	short R_18    ; MSDOS (FAT12) partition not found
	add	bx, 16 ; Next table row
	jmp	short R_07

	; 21/10/2023
R_15:	
	mov	si, msg_drv_not_ready_err
	jmp	short R_14
R_16:
	mov	si, msg_disk_read_err
	jmp	short R_14
R_17:
	mov	si, msg_not_valid_mbr
	jmp	short R_14
R_18:
	mov	si, msg_msdos_p_not_found
	jmp	short R_14

R_08:
	mov	ax, [bx+ptStartSector]
	mov	dx, [bx+ptStartSector+2]
	and	dx, dx
	jnz	short R_19 ; Not a valid MSDOS (FAT12) partition

	cmp	word [bx+ptSectors+2], 0
	ja	short R_19 ; Not a valid MSDOS (FAT12) partition
	mov	dx, ax
	add	dx, [bx+ptSectors]
	jc	short R_19 ; Not a valid MSDOS (FAT12) partition

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Read volume/partition boot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	call	convert_to_chs
	jc	short R_19    ; Not a valid MSDOS (FAT12) partition

	; CL =  sector
	; CH =  cylinder
	; DH =  head
	; DL =  drive
	
	; BX = sector buffer offset
	; AL =  1 (sector count)

	mov	ah, 2	    ; read disk sector
	int	13h
	jc	short R_16  ; Read error

	cmp	word [bx+510], 0AA55h 
	jne	short R_19  ; Not a valid MSDOS (FAT12) partition

	cmp	word [bx+bsBytesPerSec], 512
 	jne	short R_19  ; Not a valid MSDOS (FAT12) partition

	cmp	byte [bx+bsMedia], 0F8h
 	jne	short R_19  ; Not a valid MSDOS (FAT12) partition

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Overwrite question
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_overwrite_question
	call	print_msg

	; 21/10/2023
	; get answer
R_09:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'C'-40h
	je	short R_10 ; Exit
	cmp	al, 27
	je	short R_10 ; Exit
	and	al, 0DFh
	cmp	al, 'Y'			; Yes?
	je	short R_12		; write
	cmp	al, 'N'			; No?
	jne	short R_09
					; no write (exit)
	mov	si, msg_NO
	call	print_msg 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Nextline & Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

R_10:
	mov	si, CRLF
R_21:
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

R_11:
	mov	si, RetroDOS_Welcome
	call	print_msg
	jmp	short R_21 ; Exit

R_19:	
	mov	si, msg_not_valid_vbr
	; 21/10/2023
	;call	print_msg
	;jmp	short R_14
R_14:
	call	print_msg
	int	20h

	; 21/10/2023
R_20:
	mov	si, msg_disk_write_err
	jmp	short R_14

R_12:
	mov	si, msg_YES
	call	print_msg

	mov	bx, SectorBuffer
	
	push	cx

	lea	si, [bx+bsOemName]
	mov	di, RETRODOS_FAT12_HD_BS + bsOemName
	mov	cx, bsBpbSignature - bsOemName
	rep	movsb

	cmp	byte [bx+bsBpbSignature], 29h
	jne	short R_13

	mov	cl, bsFileSysType - bsBpbSignature
	rep	movsb
R_13:
	; Calculate Retro DOS extended BS parameters
	push	dx
	mov	ax, [bx+bsFATsecs]
	mov	cl, [bx+bsFATs]
	;mul	cx
	dec	cl
	shl	ax, cl ; * 2
	add	ax, [bx+bsResSectors]
	mov	[RETRODOS_FAT12_HD_BS+bsRootDirStart], ax
	mov	cx, [bx+bsRootDirEnts]

	; 22/10/2023
	mov	dx, 15
	;add	cx, 15
	add	cx, dx
	
	shr	cx, 4 ; 16 entries per sector
	mov	[RETRODOS_FAT12_HD_BS+bsRootDirSects], cx

	; 22/10/2023
	inc	dx ; dx = 16
	mov	[RETRODOS_FAT12_HD_BS+bsDirEntsPerSec], dx

	add	ax, cx
	mov	[RETRODOS_FAT12_HD_BS+bsDataStart], ax

	cmp	byte [bx+bsBpbSignature], 29h
	je	short R_22

	;push	dx
	mov	di, RETRODOS_FAT12_HD_BS+bsVolumeLabel
	call	write_volume_name
	mov	si, RETRODOS_FAT12_HD_BS+bsVolumeID
	call	write_volume_serial
	;pop	dx

R_22:
	pop	dx
	pop	cx

	mov	si, msg_writing_boot_sector
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write/Update volume/partition boot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	ax, 0301h ; write disk sector
	mov	bx, RETRODOS_FAT12_HD_BS
	int	13h
 	;jnc	short R_20
	; 21/10/2023
	jc	short R_20
;R_20:
	mov	si, msg_OK
	jmp	R_21

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Convert LBA sector address to CHS address  (for INT 13h R/W)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_to_chs:
	; check partition limit
	mov	bx, ax
	mov	al, [Sectors]
	mov	ah, [Heads]
	mul	ah
	mov	cx, [Cylinders]
	mul	cx
	or	dx, dx
	jz	short ctchs1
	xor	dx, dx
	jmp	short ctchs2
ctchs1:
	cmp	ax, bx
	jb	short ctchs3  ; Not a valid MSDOS (FAT12) partition
ctchs2:
	mov	ax, bx
	; AX = LBA sector address  (DX = 0)
	div 	word [Sectors]
	inc	dl
	mov	cl, dl ; sector
	xor	dl, dl ; (dh = 0)
	div	word [Heads] ; [BPB_NumHeads]
	mov	dh, dl ; head
	mov	ch, al ; cylinder (low 8 bits)
	;ror	ah, 2
	shl	ah, 6
	or	cl, ah ; sector | (high 2 bits of cyl)
	mov	bx, SectorBuffer
	mov	dl, [DriveNum]
	mov	al, 1
ctchs3:
	retn		

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set & write volume name
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_volume_name:
	;mov	byte [vname_length], 11
svn_fs:
	; DI = (BS) Volume Label address
	mov	si, Msg_Volume_Name
	call	print_msg

	; get cursor position
	; bh = 0  ; video page
	mov     ah, 3 ; get cursor pos
	int     10h
	mov	[Cursor_Pos], dx

	call	rw_char
	;jc	short svn_1
	jc	short svn_5
svn_0:
	lodsb
	cmp	al, 20h
	je	short svn_0
	mov	bx, di ; *
	;ja	short svn_2
	jb	short svn_5
;svn_1:
;	mov	si, no_name
;	lodsb
svn_2:
	mov	cx, 11; [vname_length]
	jmp	short svn_4
svn_3:
	lodsb
	cmp	al, 20h
	jb	short svn_6
svn_4:
	stosb
	loop	svn_3
svn_5:
	mov	cx, 11 ; [vname_length]
	mov	si, bx ; *
	mov	di, StrVolumeName
	rep	movsb
	;mov	byte [di], 0

	mov	dx, [Cursor_Pos]
	mov	bx, 7
	mov	ah, 2
	int	10h  ; Set Cursor Position

	mov	si, StrVolumeName
	call	print_msg
	mov	si, CRLF
	;call	print_msg
	;retn
	; 21/10/2023
	jmp	print_msg
svn_6:
	mov	al, 20h
svn_7:
	stosb
	loop	svn_7
	jmp	short svn_5

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set & write volume serial number (volume ID)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_volume_serial:
	; SI = (BS) Volume Serial Number (binary) address

	;xor	ax, ax
	;int	1Ah			; get time of day

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

	;xchg	ch,cl
	;xchg	dh,dl

	;add	cx, dx
	;add	[si+2], cx

	; According to Microsoft DOS 6.0 serial number
	; production method...
	; < Create unique 32 bit serial number >

	; Create_Serial_ID (MSDOS 6.0 Source code, MSFOR.ASM)
	; (20/04/1987)
	;
	;  Get date (INT 21h, AH=2Bh)
	;  Get time (INT 21h, AH=2Ch)
	;  Serial_ID+0 = DX reg date + DX reg time
	;  Serial_ID+2 = CX reg date + CX reg time
	;  Serial_Num_Low = Serial_ID+2
	;  Serial_Num_High = Serial_ID+0

	mov	ah, 04h		; Return Current Date
	int	1Ah

	; DL = Day (BCD)	(20h)
	; DH = Month (BCD)	(12h)
	; CH = Century (BCD)	(20h)
	; CL = Year (BCD) 	(17h)

	mov	al, dl
	call	bcd_to_bin
	mov	dl, al 
	mov	al, dh
	call	bcd_to_bin
	mov	dh, al 
	mov	al, cl
	call	bcd_to_bin
	mov	cl, al 
	mov	al, ch
	call	bcd_to_bin
	mov	ch, al

	; DH = Month (1-10)
	; DL = Day (1-31)
	; CX = Year (1900-2099)

	push	dx 
	push	cx

	mov	ah, 02h		; Return Current Time
	int	1Ah
	
	; DH = Seconds (BCD)	(59h)
	; CL = Minutes (BCD)	(59h)
	; CH = Hours (BCD)	(23h)
	; DL = Daylight savings time option (1=yes)

	mov	al, dh
	call	bcd_to_bin
	mov	dh, al 
	mov	al, cl
	call	bcd_to_bin
	mov	cl, al 
	mov	al, ch
	call	bcd_to_bin
	mov	ch, al 

	; CH = Hour (0-23)
	; CL = Minutes (0-59)
	; DH = Seconds (0-59)
	; ((DL = Hundredths (0-99) - MSDOS!))
	; DL = 0 or 1 (here!)

	mov	ax, cx
	pop	cx
	add	ax, cx

	mov	[si+2], ax

	mov	ax, dx
	pop	dx
	add	ax, dx

	mov	[si], ax

	xor	ah, ah		; Read time counter
	int	1Ah

	; CX = High word of clock count
	; DX = Low word of clock count
	; AL = 0 if 24 hours has not passed, else 1

	; NOTES: 
	; (Ref: vitaly_filatov.tripod.com/ng/asm/asm_029.1.html)
	;
   	; Following formulas convert the clock count to
        ; the time of day:
 	;	Hour      = Clock / 65543 (1007h)
	;	Remainder = Clock MOD 65543
 	;
	;	Minutes   = Remainder / 1092 (444h)
	;	Remainder = Remainder MOD 1092
	;
	;	Second    = Remainder / 18.21
	;	Remainder = Remainder MOD 18.21
	;
	;	Hundredths = CINT(Remainder * 100)

	add	[si], dl

	; SI = Volume serial number address (4 bytes) 
	mov	al, [si]
	call	bin_to_hex
	mov	[Vol_Serial2+2], ax
	mov	al, [si+1]
	call	bin_to_hex
	mov	[Vol_Serial2], ax
	mov	al, [si+2]
	call	bin_to_hex
	mov	[Vol_Serial1+2], ax
	mov	al, [si+3]
	call	bin_to_hex
	mov	[Vol_Serial1], ax

	mov	si, Msg_Volume_Serial
	
	; 21/10/2023
	;call	print_msg
	;retn
	;jmp	short print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Print messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_msg:

print_msg_LOOP:
	lodsb                           ; Load byte at DS:SI to AL
	and     al, al
	jz      short print_msg_OK
	mov	ah, 0Eh
	mov     bx, 07h
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short print_msg_LOOP

;print_msg_OK:
	;retn

	; 21/10/2023
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Convert bcd (cmos date/time format) number to binary number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bcd_to_bin:
	push	bx
	db	0D4h,10h  ; Undocumented inst. AAM
			  ; AH = AL / 10h
			  ; AL = AL MOD 10h
	mov	bl, al
	mov	al, 10
	mul	ah
	add	al, bl
	pop	bx
print_msg_OK:	; 21/10/2023
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Convert byte to hexadecimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bin_to_hex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	push	bx
	xor	bx, bx
	mov	bl, al
	shr	bl, 4
	mov	bl, [bx+hexchrs]
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [bx+hexchrs]
	pop	bx
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Read & Write characters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rw_char:
	; OUTPUT -> DS:SI = Entered String (ASCIIZ)
	mov     si, StrVolumeName
	mov     bx, 7
	mov     ah, 3
	int     10h
	mov     [Cursor_Pos], dx
read_next_char:
	xor     ah, ah
	int     16h
	and     al, al
	jz      short loc_arrow
	cmp     al, 0E0h
	je      short loc_arrow
	cmp     al, 8
	jne     short char_return
loc_back:
	mov     ah, 3
	int     10h
	cmp     dl, byte [Cursor_Pos]
	jna     short loc_beep
prev_column:
	dec     dl
set_cursor_pos:
	mov     ah, 2
	int     10h
	mov     bl, dl
	sub     bl, byte [Cursor_Pos]
	mov     cx, 1
	mov     ah, 9
	mov     al, 20h
	mov     [si+bx], al
loc_write_it:
	mov     bl, 7
	int     10h
	mov     dx, [Cursor_Pos]
	jmp     short read_next_char
loc_beep:
	mov     ah, 0Eh
	mov     al, 7
	int     10h
	jmp     short read_next_char
loc_arrow:
	cmp     ah, 4Bh
	je      short loc_back
	cmp     ah, 53h
	je      short loc_back
	jmp     short read_next_char
char_return:
	mov     ah, 3
	int     10h
check_char_type:
	cmp     al, 20h
	jb      short loc_escape
	mov     ah, dl
	sub     ah, byte [Cursor_Pos]
	;cmp	ah, 10 
	;ja	short loc_beep
	cmp     ah, 11 ; [vname_length]
	jnb	short loc_beep
	cmp     al, 'z'
	ja      short read_next_char
	cmp     al, 'a'
	jb      short pass_capitalize
	and     al, 0DFh
pass_capitalize:
	mov     bl, ah 
	xor     ah, ah
	mov     [si+bx], ax
	mov     bl, 7
	mov     ah, 0Eh
	int     10h
	jmp     short read_next_char
pass_escape:
	cmp     al, 0Dh	; 13 ; ENTER
	jne     short read_next_char
	;mov	ah, 0Eh
	;int	10h
	;mov	al, 0Ah
	;int	10h
	retn
loc_escape:
	cmp     al, 1Bh	; 27 ; ESC
	jne     short pass_escape
	stc
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Data
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RETRODOS_FAT12_HD_BS: 
	;incbin	'RD2HDBS.BIN'	; boot sector : 25/10/2023
	incbin	'RD5HDBS1.BIN'	; FAT12 hd boot sector ; 20/04/2024

	db	0

hexchrs:
	db	'0123456789ABCDEF'

align 2

Sectors:
	dw	0
Heads:
	dw	0
Cylinders:
	dw	0

DriveNum:
	db	0

	; 22/10/2023

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Retro DOS v5.0 Fixed Disk Boot Sector Update Utility'
	db	0Dh, 0Ah
	;db	"v1.1.231025  (c) Erdogan TAN 2018-2023"
	db	"RDHDBOOT v1.2.240420  (c) Erdogan TAN 2018-2024"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: rd5hdb12 c: (or d:)'
	db	0

msg_drv_not_ready_err: 
	db	0Dh, 0Ah
	db	"Drive not ready !"
	db	0Dh, 0Ah, 0

msg_disk_read_err: 
	db	0Dh, 0Ah
	db	"Disk read error !"
	db	0Dh, 0Ah, 0

msg_not_valid_mbr:
	db	0Dh, 0Ah
	db	"Not a valid masterboot sector !"
	db	0Dh, 0Ah, 0 

msg_msdos_p_not_found:
	db	0Dh, 0Ah
	db	"MSDOS (FAT12) partition not found !"
	db	0Dh, 0Ah, 0 

msg_not_valid_vbr:
	db	0Dh, 0Ah
	;db	"Not a valid MSDOS (FAT12) partition ! (for Retro DOS v2)"
	; 20/04/2024
	db	"Not a valid MSDOS (FAT12) partition ! (for Retro DOS v5)"
	db	0Dh, 0Ah, 0 

msg_disk_write_err: 
	db	0Dh, 0Ah
	db	"Disk write error !"
	db	0Dh, 0Ah, 0

msg_overwrite_question:
	db	0Dh, 0Ah
	db	'WARNING !', 0Dh, 0Ah
	db	'Do you want to overwrite boot sector (Yes/No)? ', 0

msg_NO:
	db	' NO ..', 0Dh, 0Ah, 0
msg_YES:
	db	' YES ..', 0Dh, 0Ah, 0

msg_writing_boot_sector:
	;;db	"Updating boot sector to Retro DOS v2 format ...", 0
	;; 22/10/2023
	;db	"Updating boot sector to Retro DOS v4 (v2 compatible) format ...", 0
	; 20/04/2024
	db	"Updating boot sector to Retro DOS v5 format ...", 0

StrVolumeName:
	times 	12 db  0

Msg_Volume_Name:
	db	0Dh, 0Ah
	db	"Volume Name: ", 0

Msg_Volume_Serial:
	db	"Volume Serial No: "
Vol_Serial1:
	db	"0000"
	db	"-"
Vol_Serial2:
	db	"0000"
	db	0Dh, 0Ah, 0

msg_OK:
	db	' OK.'
CRLF:
	db	0Dh, 0Ah, 0

align 2

Cursor_Pos:
	dw	0

align 2

SectorBuffer:
	times	512 db 0F6h

	db	0
	;db	'(c) Erdogan TAN 2018-2023' ; 21/10/2023
	db	'(c) Erdogan TAN 2018-2024' ; 20/04/2024
	db	0

SizeOfFile equ $-100