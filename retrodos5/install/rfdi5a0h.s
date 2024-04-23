; ****************************************************************************
; RD5FDIMG.ASM (RD5FDIMG.COM) - Retro DOS v5.0 Floppy Disk Image Formatting
;						  Utility for MSDOS/WINDOWS
; ----------------------------------------------------------------------------
; Only for 1.44MB (3.5") Floppy Disks
; ****************************************************************************
; Last Update: 20/04/2024
; ----------------------------------------------------------------------------
; Beginning: 11/02/2018
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Retro DOS Operating System Project by ERDOGAN TAN (Beginning: 04/02/2018)
; ----------------------------------------------------------------------------
; Modified from 'rfdi1440.s'(RFDI1440.COM) src code by Erdogan Tan, 25/10/2023
; ****************************************************************************
; nasm rfdi5a0h.s -l rfdi5a0h.txt -o RD5FDIMG.COM

; RFDI5A0H.COM - Retro DOS v5.0 - 20/04/2024

; rfdi1440.s - Retro DOS v4.0-v4.2 - 25/10/2023

bsDriveNumber   equ RETRODOS_FAT12_FDBS + 36
bsVolumeID      equ RETRODOS_FAT12_FDBS + 39
bsVolumeLabel   equ RETRODOS_FAT12_FDBS + 43

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

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
; get fd image file name
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, 80h			; PSP command tail
 	lodsb
	or	al, al 			; command tail length
	jz	R_17			; jump if zero
R_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short R_01
	jb	R_17
	
	; check fd image file name
R_02:
       	mov	di, img_file_name
	stosb
R_03:
	lodsb
	;cmp	al, 0Dh ; ENTER (CR) key
	cmp	al, 20h ; ' '
	jna	short R_04
	stosb
	cmp	di, img_file_name + 12
	jb	short R_03
	cmp	byte [si], 20h 
	ja	short R_11
R_04:
	;sub	al, al
	;stosb

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; File name capitalization
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, img_file_name
	mov	di, si
	mov	bx, si
R_05:
	lodsb
	cmp	al, 'a'
	jnb	short R_07
	and	al, al
	jz	short R_08
	cmp	al, '.'
	jne	short R_06
	mov	bx, di ; dot position
R_06:
	stosb
	jmp	short R_05
R_07:
	cmp	al, 'z'
	ja	short R_06
	and	al, 0DFh ; NOT 32
	stosb
	jmp	short R_05
R_08:
	mov	[di], al
	dec	di
	cmp	bx, di
	jnb	short R_11
	sub	di, bx
	sub	bx, img_file_name
	cmp	di, 3
	jna	short R_09
	and	bx, bx
	jnz	short R_11
	jmp	short R_10
R_09:
	cmp	bx, 8
	jna	short R_10
R_11:
	mov	si, msg_inv_file_name
R_12:	
	call	print_msg
	jmp	short R_16

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Find image file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
R_10:
	mov	dx, img_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS-DOS Function = Find First File
	int	21h
	jc	R_20

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check image file features
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

R_14:
	mov	si, DTA_Attrib
	mov	al, [si]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	jnz	short R_19
	mov	si, DTA_FileSize
	lodsw
	cmp	word [SI], 16h
	jne	short R_19
	cmp	ax, 8000h ; 1.44 MB floppy disk image (168000h bytes)
	jne	short R_19

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Overwrite question
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_overwrite_question1
	call	print_msg
	mov	si, DTA_FileName
	call	print_msg
	mov	si, msg_overwrite_question2
	call	print_msg
	mov	si, msg_yes_no
	call	print_msg

	; get answer
R_15:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'C'-40h
	je	short R_16 ; Exit
	cmp	al, 27
	je	short R_16 ; Exit
	and	al, 0DFh
	cmp	al, 'Y'			; Yes?
	je	short R_18		; write
	cmp	al, 'N'			; No?
	jne	short R_15
					; no write (exit)  
	mov	si, Msg_NO
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Nextline & Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

R_16:
	mov	si, CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

R_17:
	mov	si, RetroDOS_Welcome
	call	print_msg
	jmp	short R_16 ; Exit

R_18:
	mov	si, Msg_YES
	call	print_msg
	jmp	short R_21

R_19:
	mov	si, msg_inv_image_file
	jmp	short R_12

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Create image file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

R_20:
	; create a new fd image file
	;mov	dx, img_file_name
	mov	cx, 0 ; File Attributes
	mov	ah, 3Ch ; MS-DOS Function = Create File
	int	21h
	jc	R_11

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing root directory sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

R_21:
	mov	si, CRLF
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Open image file for writing
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	al, 2 ; open for reading and writing
	;mov	dx, img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jc	R_11

	mov	[img_file_handle], ax

	mov	si, Msg_Writing_Root_Dir
	call	print_msg

	mov	ax, 19  ; Root Directory Address
	mov	bx, FDFORMAT_FATBUFFER_S9
R_22:
	call	write_fd_sector
	jc	R_29
	inc	ax
 	cmp	ax, 32 ; 19+14 = 33 (14 sectors, 224 entries)
	jna	short R_22

	mov	si, Msg_OK
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing data sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, Msg_Writing_Data_Sectors
	call	print_msg
	mov	ah, 3
	mov	bx, 7
	int	10h ; Return Cursor Position
	; DL = Column, DH= Line
	mov	[Cursor_Pos], dx
	mov	ax, 33  ; First Data Sector
R_23:
	push	ax
	inc	ax ; 1 based printing of 0 based sectors
	mov	si, Sector_Str + 3
	call	bin_to_decimal
	mov	dx, [Cursor_Pos]
	mov	ah, 2
	int	10h  ; Set Cursor Position
	call	print_msg
	pop	ax
	mov	bx, FDFORMAT_SECBUFFER
	call	write_fd_sector
	jc	R_29

	mov	bx, 7
	inc	ax
	cmp	ax, 2880
	jb	short R_23

	mov	si, Msg_3dot_OK
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing FAT sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, Msg_Writing_FAT_Sectors
	call	print_msg
	mov	ax, 1  ; FAT Beginning Address
	mov	bx, FDFORMAT_FATBUFFER
	call	write_fd_sector
	jc	R_29
	mov	bx, FDFORMAT_FATBUFFER_S9
R_24:
	inc	ax
	call	write_fd_sector
 	jc	short R_29
	cmp	ax, 9
	jb	short R_24
	mov	bx, FDFORMAT_FATBUFFER
	inc	ax
	call	write_fd_sector
	jc	short R_29
	mov	bx, FDFORMAT_FATBUFFER_S9
R_25:
	inc	ax 
        call	write_fd_sector
	jc	short R_29
	cmp	ax, 18
	jb	short R_25

	mov	si, Msg_OK
	call	print_msg

	mov	si, Msg_Volume_Name
	call	print_msg
	call	rw_char
	jc	short R_28
	mov	al, [si]
	cmp	al, 20h
	jna	short R_28
	mov	di, bsVolumeLabel
	mov	cx, 11
	inc	si
	jmp	short R_27

R_26:
	lodsb
	inc	di
	cmp	al, 20h
	jna	short R_32
R_27:
	mov 	[di], al
	loop	R_26

R_28:
	mov	si, Msg_Writing_Boot_Sector
	call	print_msg

	mov	si, bsVolumeID

	xor	ax, ax
	int	1Ah			; get time of day
	mov	[si], dx
	mov	[si+2], cx		; set unique volume ID

	mov	ah, 02h			; Return Current Time
	int	1Ah
	xchg	ch, cl
	xchg	dh, dl

	add	cx, dx  
	add	[si+2], cx
               
	mov	ah, 04h			; Return Current Date
	int	1Ah
	xchg	ch,cl
	xchg	dh,dl

	add	cx, dx
	add	[si+2], cx

	xor	ax, ax ; Boot sector
	mov	bx, RETRODOS_FAT12_FDBS	; location of boot code

	call	write_fd_sector
	jnc	short R_30
R_29:
	mov	al, ah ;  error code
	call	bin_to_hex
	mov 	[error_code], ax

	mov	si, CRLF
	call	print_msg

	mov	si, Msg_Error
	jmp	short R_31

R_30:
	mov	si, Msg_OK
R_31:
	call	print_msg

	int	20h	; Exit

R_32:
	mov	byte [di], 20h
	inc	di
	loop	R_32
	jmp	short R_28

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

print_msg_OK:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writing a block (sector) to floppy disk image file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_fd_sector:
	; writing a block (sector) to floppy disk image file
	; Only for 1.44 MB FAT12 Floppy Disks
	; INPUT -> AX = Logical Block (Sector) Address
	; ES:BX = Sector Buffer
	; OUTPUT ->
	; cf = 0 -> AX = Logical Block (Sector) Address
	; cf = 1 -> AH = Error Number
	;
	push	ax ; sector
	push	bx ; buffer
	mov	dx, 512
	mul	dx
	mov	cx, dx
	mov	dx, ax
	sub	al, al ; specified offset is from the beginning of the file
	mov	ah, 42h ; seek (move file pointer)
	mov 	bx, [img_file_handle]
	int	21h
	;mov	bx, [img_file_handle]
	mov	cx, 512
	pop	dx  ; buffer address
	mov	ah, 40h ; write to file
	int	21h
	mov	bx, dx
	jc	short image_file_wr_err
	pop	ax ; sector
	retn
	
image_file_wr_err:
	pop	dx ; sector
	retn
	
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Convert byte to decimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bin_to_decimal:
	; INPUT: DS:SI = Target location
	;        AX= Binary Number (Integer)
	; OUTPUT: Decimal char at DS:SI
	; SI decremented after every division
	; till AX<10.
	; CX, DX will be changed.
	;
	mov	cx, 10
loc_btd_re_divide:
	xor	dx, dx
	div	cx
	add	dl,"0"
	mov	[si], dl
	cmp	ax, 0
	jna	short pass_btd_re_divide
	dec	si
	jmp	short loc_btd_re_divide
pass_btd_re_divide:
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
	push	ebx
	xor	ebx, ebx
	mov	bl, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs]
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	pop	ebx	
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
	mov     cx,1
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
	cmp     ah, 10
	ja      short loc_beep
	cmp     al, "z"
	ja      short read_next_char
	cmp     al, "a"
	jb      short pass_capitalize
	and     al, 0DFh
pass_capitalize:
	mov     bl, ah  ; 30/07/2011
	xor     ah, ah
	mov     [si+bx], ax
	mov     bl, 7
	mov     ah, 0Eh
	int     10h
	jmp     short read_next_char
pass_escape:
	cmp     al, 0Dh
	jne     short read_next_char
	mov     ah, 0Eh
	int     10h
	mov     al, 0Ah
	int     10h
	retn
loc_escape:
	cmp     al, 1Bh
	jne     short pass_escape
	stc
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Data
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	db	0

RETRODOS_FAT12_FDBS:
	;incbin 'FDBS1440.BIN'	; 25/10/2023
	incbin	'FDBS5A0H.BIN'	; 20/04/2024

	db	0

hexchrs:
	db	'0123456789ABCDEF'

img_file_handle:
	dw	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'RETRO DOS v5 1.44MB FAT12 Floppy Disk Image Format Utility'
	;db	"v3.0.231025  (c) Erdogan TAN 2018-2023"
	db	"RDFDIMAGE v4.0.240420  (c) Erdogan TAN 2018-2024"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: rd5fdimg <image file name> '
	db	0

msg_inv_file_name: 
	db	0Dh, 0Ah
	db	"Invalid file name !", 0Dh, 0Ah
	db	"(File name must fit to 8.3 DOS format) !"
	db	0Dh, 0Ah, 0

msg_inv_image_file:
	db	0Dh, 0Ah
	db	"Invalid floppy disk image file !", 0Dh, 0Ah
	db	"(File size must be 1474560 bytes) !"
	db	0Dh, 0Ah, 0  

msg_overwrite_question1:
	db	0Dh, 0Ah
	db	'Do you want to overwrite '
	db	27h
	db	0

msg_overwrite_question2:
	db	27h
	db	' file '
	db	0

msg_yes_no:
	db	'(Yes/No)? ', 0

Msg_Writing_Boot_Sector:
	db	"Writing retrodos boot sector...", 0

Msg_Writing_Root_Dir:
	db	"Writing root directory sectors...", 0

Msg_Writing_Data_Sectors:
	db	"Writing data sector: ", 0

Sector_Str:
	db	"0000", 0
Cursor_Pos:
	dw	0

Msg_Writing_FAT_Sectors:
	db	"Writing FAT sectors...", 0

StrVolumeName:
	times 	12 db  0

Msg_Volume_Name:
	db	"Volume Name: ", 0

Msg_3dot_OK:
	db	"..."
Msg_OK:
	db	' OK.'
CRLF:
	db	0Dh, 0Ah, 0
Msg_YES:
	db	' YES', 0
Msg_NO:
	db	' NO', 0

Msg_Error:
	db	0Dh, 0Ah
	db	'Error ! '
	db	'('
error_code:
	dw	3030h
	db	'h'
	db	') '
	db	0

FDFORMAT_SECBUFFER:
	times	512 db 0F6h
FDFORMAT_FATBUFFER:
	db	0F0h
	db	0FFh
	db	0FFh
FDFORMAT_FATBUFFER_S9:
	times	512 db 0

	;db	'(c) Erdogan TAN 2018-2023'
	db	'(c) Erdogan TAN 2018-2024'

img_file_name:  
	times	13 db 0

SizeOfFile equ $-100