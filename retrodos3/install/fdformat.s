; ****************************************************************************
; FDFORMAT.ASM (FDFORMAT.COM) - TRDOS 386 Floppy Disk Formatting Utility
; 						      (for MSDOS/WINDOWS)
; ----------------------------------------------------------------------------
; Only for 1.44MB (3.5") Floppy Disks
; ****************************************************************************
; Last Update: 28/10/2023  (Previous: 12/02/2018)
; ----------------------------------------------------------------------------
; Beginning: 23/11/2017
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from TRDOS Operating System v2.0 (80386) source code by Erdogan Tan
; TRFDBOOT.S (TRFDBOOT.COM), 06/07/2016
;
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; FDFORMAT1.ASM (31/07/2011)
; ****************************************************************************
; nasm fdformat.asm -l fdformat.lst -o FDFORMAT.COM


bsDriveNumber   equ TRDOS_FAT12_fd_bs + 36
bsVolumeID      equ TRDOS_FAT12_fd_bs + 39
bsVolumeLabel   equ TRDOS_FAT12_fd_bs + 43

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
	jz	short T_07		; jump if zero

T_01:
	inc	si

	mov	al, [si]
	cmp	al, ' '			; is it SPACE ?
	jne	short T_03

	dec	cl                                  
	jnz	short T_01                  
	jmp	short T_07

T_02:
	add	al, 'A'-'0'		; 0 based -> A based
	jmp	short T_04

T_03:
	cmp	al, '0'			; 0 - 9
	jb	short T_07
	cmp	al, '9'			; allow number for drive
	jna	short T_02
             
	cmp	al, 'A'
	jb	short T_07
	cmp	al, 'Z'			; A - Z
	jna	short T_04                    
	cmp	al, 'a'			; a - z 
	jb	short T_07                  
	cmp	al, 'z'                           
	ja	short T_07                 

	sub	al, 'a'-'A'		; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write drive letter
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_04:
	mov	[TrDOS_Drive], al

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check disk parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	sub	al, 'A'			; make it zero based 
	mov	dl, al                           
        ;mov	si, bsDriveNumber
	;mov	[si], al
	mov	[bsDriveNumber], al
	mov	ah, 08h
	int	13h			; return disk parameters
	;jc	T_19
	jc	short T_04_err

	push	cs
	pop	es			; restore es

	cmp	bl, 04			; Drive Type
  	;jb	T_19
	jnb	short T_04_ok
	; 28/10/2023
T_04_err:
	jmp	T_19
T_04_ok:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Format question
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, Msg_DoYouWantToFormat
	call	print_msg

T_05:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'C'-40h
	je	short T_06 ; Exit                   
	cmp	al, 27
	je	short T_06 ; Exit
	and	al, 0DFh
	cmp	al, 'Y'				; Yes?
	je	short T_08			; write
	cmp	al, 'N'				; No?
	jne	short T_05          
						; no write (exit)  
	mov	si, Msg_NO
	call	print_msg 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Nextline & Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_06:
	mov	si, CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

T_07:
	mov	si, TrDOS_Welcome
	call	print_msg
	jmp	short T_06 ; Exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing root directory sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_08:
	mov	si, Msg_YES
	call	print_msg
T_09:
	mov	si, CRLF
	call	print_msg

	mov	si, Msg_Writing_Root_Dir
	call	print_msg

	mov	ax, 19  ; Root Directory Address
	mov	bx, FDFORMAT_FATBUFFER_S9
T_10:
	call	write_fd_sector
	;jc	T_19
	jc	short T_12_err
	inc	AX
 	cmp	AX, 32
	jna	short T_10

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
T_11:
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
	jnc	short T_12
	and	ah, 16h  ; Errors: 2h, 4h, 10h
	;jz	T_19 ; Drive not ready msg
	jz	short T_12_err

	; DX = LBA sector value
	push	dx
	call	mark_bad_cluster
	pop	ax
T_12:
	mov	bx, 7
	inc	ax
	cmp	ax, 2880
	jb	short T_11

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
	;jc	T_19
	jnc	short T_12_ok
	; 28/10/2023
T_12_err:
	jmp	T_19
T_12_ok:
	mov	bx, FDFORMAT_FATBUFFER_S9
T_13:
	inc	ax
	call	write_fd_sector
 	;jc	T_19
	jc	short T_12_err
	cmp	ax, 9
	jb	short T_13
	mov	bx, FDFORMAT_FATBUFFER
	inc	ax
	call	write_fd_sector
	;jc	T_19
	jc	short T_12_err
	mov	bx, FDFORMAT_FATBUFFER_S9
T_14:
	inc	ax 
        call	write_fd_sector
	jc	short T_19
	cmp	ax, 18
	jb	short T_14

	mov	si, Msg_OK
	call	print_msg

	mov	si, Msg_Volume_Name
	call	print_msg
	call	rw_char
	jc	short T_17
	mov	al, [si]
	cmp	al, 20h
	jna	short T_17
	mov	di, bsVolumeLabel
	mov	cx, 11
	inc	si  
	jmp	short T_16  

T_15:
	lodsb
	inc	di
	cmp	al, 20h
	jna	short T_22
T_16:
	mov 	[di], al
	loop	T_15

T_17:
	mov	si, Msg_Writing_Boot_Sector
	call	print_msg

	mov	byte [RetryCount], 4
T_18:
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

	mov	ax, 0301h		; write to disk
	mov	bx, TRDOS_FAT12_fd_bs	; location of boot code

	mov	cx, 1			; cylinder = 0
                     			; sector = 1
	mov	dh, 0			; head = 0
	mov	si, bsDriveNumber
	mov	dl, byte [si]
	int	13h
	jnc	short T_24
	dec	byte [RetryCount]
	jnz	short T_18

T_19:
	mov	si, Disk_NotReadyOrError
	call	print_msg

T_20:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'C'-40h
	je	short T_21 ; Exit                   
	cmp	al, 27
 	je	short T_21 ; Exit
	and	al, 0DFh
	cmp	al, 'Y'
	je	short T_23		; Retry
	cmp	al, 'N'
	jne	short short T_20

T_21:					; Exit
	mov	si, CRLF
	call	print_msg

	int	20h

T_22:
	mov	byte [DI], 20h
	inc	di
	loop	T_22
	jmp	short T_17

T_23:
	mov	si, bsDriveNumber
	mov	dl, byte [si]
	mov	ah, 08h
	int	13h			; return disk parameters
	jc	short T_19

	push	cs
	pop	es			; restore es

	cmp	bl, 04h			; Drive Type
	jb	short T_19

	jmp	T_09

T_24:
	mov	si, Msg_OK
	call	print_msg
	jmp	short T_21

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

write_fd_sector:
	; Only for 1.44 MB FAT12 Floppy Disks
	; INPUT -> AX = Logical Block Address
	; ES:BX = Sector Buffer
	; OUTPUT ->
	; cf = 0 -> AX = Logical Block Address
	; cf = 1 -> DX = Logical Block Address
	; cf = 1 -> AH = Error Number
	;
	mov	cx, 4  ; Retry Count
loc_write_fdisk_chs:
	push	ax                      ; Linear sector number
	push	cx                      
	mov	dx, 18                  ; Sectors per Track
	div	dl
	mov	cl, ah                  ; Sector (zero based)
	inc	cl                      ; To make it 1 based
	shr	al, 1                   ; Convert Track to Cylinder
	adc	dh, 0                   ; Head (0 or 1)
	;mov	si, bsDriveNumber
	;mov	dl, [si]
	mov	dl, [bsDriveNumber]
	mov	ch, al                   
	mov	ax, 0301h
	int	13h                     ; BIOS Service func ( ah ) = 3
                                        ; Write disk sectors
	mov 	[Error_Code], ah
	pop	cx
	pop	ax

	jnc	short pass_write_fdisk_chs_error
	loop	loc_write_fdisk_chs
	mov	dx, ax
	mov	ah, [Error_Code]
	stc
pass_write_fdisk_chs_error:
	retn

mark_bad_cluster:
	; Only for FAT12 Floppy Disks (Full FAT Buffer)
	; INPUT -> AX = Cluster/Sector Number
	; OUTPUT -> 0FF7h = BAD Cluster Value
	mov     dx, 3
	mul     dx
	shr     ax, 1 ; Divide by 2
	mov     bx, ax  ; FAT Buffer Byte Offset
	mov     dx, 0FF7h ; "BAD CLUSTER" sign
loc_update_fat12_cell:
	mov     ax, [FDFORMAT_FATBUFFER+BX]
	jnc     short uc_FAT12_nc_even
	and     ax, 0Fh
	shl     dx, 1
	shl     dx, 1
	shl     dx, 1
	shl     dx, 1
	or      dx, ax
	mov     [FDFORMAT_FATBUFFER+bx], dx
	retn
uc_FAT12_nc_even:
	and     ax, 0F000h
	and     dh, 0Fh
	or      dx, ax
	mov     [FDFORMAT_FATBUFFER+bx], dx
	retn

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
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TrDOS_Welcome:
	db	0Dh, 0Ah
	db	'TR-DOS 1.44 MB FAT12 Floppy Disk Format Utility'
	db	0Dh, 0Ah
	db	"v3.0.231028  (c) Erdogan TAN 2005-2023"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: fdformat [drive] '
	db	0

Msg_DoYouWantToFormat:
	db	07h
        db	0Dh, 0Ah
	db	'WARNING!'
	db	0Dh, 0Ah
	db	'All data on the drive will be erased.'
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'Do you want to format drive '
TrDOS_Drive:
	db	'A: (Yes/No)? ', 0

Msg_Writing_Boot_Sector:
	db	0Dh, 0Ah
	db	"Writing trdos boot sector...", 0

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
;
Disk_NotReadyOrError:
	db	0Dh, 0Ah
	db	'Disk error or drive not ready. Try again? (Y/N) '
	db	0

FDFORMAT_SECBUFFER:
	times	512 db 0F6h
FDFORMAT_FATBUFFER:
	db	0F0h
	db	0FFh
	db	0FFh
FDFORMAT_FATBUFFER_S9:
	times	512 db 0
 
	db	'(c) Erdogan TAN 1998-2023' ; 28/10/2023

RetryCount:
	db	0

TRDOS_FAT12_fd_bs:
	incbin 'TRFDBS.BIN' ; 28/10/2023

	; 28/10/2023
Error_Code:
	db	0