; ****************************************************************************
; FDFORMAT.ASM (FDFORMAT.COM) - Retro DOS v4 Floppy Disk Formatting Utility
; (RDFDFORM.S - RDFDFORM.COM)						      (for MSDOS/WINDOWS)
; ----------------------------------------------------------------------------
; Last Update: 28/10/2023
; ----------------------------------------------------------------------------
; Beginning: 26/10/2023
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Modified from 'rdfdboot.s'(RDFHDBOOT.COM) source code by Erdogan Tan
; (25/10/2023) - RETRODOS v4 foppy disk boot sector update utility -
; ----------------------------------------------------------------------------
; Modified from 'fdformat.s'(FDFORMAT.COM) source code by Erdogan Tan
; (12/08/2018) - TRDOS 386 1440KB floppy disk formatting utility -
; ****************************************************************************
; nasm fdformat.asm -l fdformat.lst -o FDFORMAT.COM

; boot sector parameters

bsOemName	equ 3
bsBytesPerSec	equ 11 ; 512 (word)
bsSecPerClust	equ 13
bsResSectors	equ 14
bsFATs		equ 16
bsRootDirEnts	equ 17
bsSectors	equ 19
bsMedia		equ 21
bsFATsecs	equ 22
bsSecPerTrack	equ 24 ; 18 (word)
bsHeads		equ 26 ; 2 (word)
bsHidden1	equ 28
bsHidden2	equ 30
bsHugeSectors	equ 32
bsDriveNumber	equ 36
bsReserved1	equ 37
bsBpbSignature	equ 38 ; 29h (byte)
bsVolumeID	equ 39
bsVolumeLabel	equ 43
bsFileSysType	equ 54 ; 'FAT12   '  (8 bytes)
bsReserved2	equ 62
; TRDOS 386 v2.0 2018 Extensions
bsDataStart	equ 64
bsRootDirStart	equ 66
bsRootDirSects	equ 68
bsDirEntsPerSec equ 70

[BITS 16]
[ORG 100h]

	cli
	cld
	push	cs
	pop	ss
	mov	sp, 0FFFEh
	sti

;-----------------------------------------------------------------
; see if drive specified
;-----------------------------------------------------------------

	mov	si, 80h			; PSP command tail
	mov	cl, [si]
	inc	si
	or	cl, cl                               
	jnz	short T_01		; jump if not zero
R_10:
	jmp	T_07
T_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	jne	short T_03
R_01:
	dec	cl                                  
	jnz	short T_01                  
	jmp	short R_10
T_02:
	cmp	al, '-'
	jne	short R_10

	; 28/10/2023
	dec	cl ; previous lodsb
	dec	cl ; following lodsb
	jng	short R_10

	lodsb
	cmp	al, '5'
	ja	short R_10
	cmp	al, '1'
	jb	short R_10
	cmp	byte [option], 0
	ja	short R_10
	sub	al, '0'
	mov	[option], al		; save numeric character
	cmp	byte [si], 'x'
	;jne	short T_01
	; 28/10/2023
	jne	short R_02	

	; 28/10/2023
	dec	cl  ; following lodsb	
	jz	short R_10

	inc	byte [hard_format]	; INT 13h Function 05h
	
	; 28/10/2023
	;lodsb	; al = 'x'
	inc	si	
	;jmp	short T_01
R_02:
	; 28/10/2023
	lodsb
	cmp	al, ' '
	jne	short R_10 ; must be '-#x ' or '-# ', but not !
	jmp	short R_01 ; dec cl for previous lodsb
T_03:
	cmp	al, 'A'
	jb	short T_02
	cmp	al, 'Z'			; A - Z
	jna	short T_04                    
	cmp	al, 'a'			; a - z 
	jb	short R_10               
	cmp	al, 'z'                           
	ja	short R_10               

	cmp	byte [si], 20h		; 'A ' accepted
	jna	short R_11

	cmp	byte [si], ':'		; 'A:' is normal
	jne	short R_10
R_11:
	sub	al, 'a'-'A'		; to upper case

;-----------------------------------------------------------------
; write drive letter
;-----------------------------------------------------------------

T_04:
	mov	[RD_Drive], al		; save disk name 

;-----------------------------------------------------------------
; Check disk parameters
;-----------------------------------------------------------------

	sub	al, 'A'			; make it zero based 
	mov	dl, al                           
	mov	[DriveNumber], al 	; bios disk (drive) number	
	mov	ah, 08h
	int	13h			; return disk parameters
	jc	short R_15   ; Drive not ready error

	push	cs
	pop	es			; restore es

	mov	bh, 5

	cmp	bl, bh ; 5		; Drive Type
	ja	short T_25   ; unknown diskette drive
	or	bl, bl
	jz	short T_25   ; invalid

	mov	[drive_type], bl
	mov	[disk_type], bl

T_25:
	; check for valid FAT12 BS
	mov	[RetryCount], bh ; 5
	
	mov	bx, bootsector		; location of boot code

	mov	cx, 1 ; *		; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [DriveNumber] 
R_05:
	mov	ax, 0201h		; read disk
	int	13h
	;jc	short T_14	
	jnc	short R_06		; read boot sector, OK

	; reset
	xor	ax, ax
	int	13h

	dec	byte [RetryCount]
	jnz	short R_05

	; Compare option against default capacity
	xor	cx, cx ; cx = 0
	xor	bx, bx
	mov	bl, [option]
	or	bl, bl
	jnz	short R_04

	cmp	byte [drive_type], 0
	jna	short R_15	; disk read error 
	; [disk_type] = [drive_type]
	jmp	T_32	; skip inserted and requested msgs

R_04:
	; option > 0
	cmp	byte [drive_type], 0 ; unknown ?
	jna	short R_15  ; error
	jmp	short T_27  ; skip inserted disk recognization

R_15:	
	mov	si, Disk_NotReadyOrError
R_14:
	call	print_msg

	mov	si, CRLF
	call	print_msg

	int	20h

hang:
	hlt
	jmp	short hang

compatibility:
	; al = disk type (option)
	push	ax
	xor	bh, bh
	cmp	[drive_type], bh ; 0
	jna	short compat_retn
	mov	bl, al
	shl	bx, 1
	add	bx, drive_compat-2
	mov	ax, [bx] ; compatible drives (for the disk)
	cmp	al, [drive_type] ; itself
	je	short compat_retn
	cmp	ah, [drive_type] ; other one
	jna	short compat_retn
	stc	
compat_retn:
	pop	ax
	retn

R_06:
	; check if the disk is already formatted DOS FAT12 disk
	; (It's type will be used if it is compatible with the drive)
	; ((for example: 720KB disk is compatible with 1.44MB drive))

R_13:	
	cmp	word [bootsector+bsBytesPerSec], 512
	jne	short T_27
	cmp	byte [bootsector+bsBpbSignature], 29h
	jne	short T_27
	mov	dx, [bootsector+bsSectors]
	mov	al, 1
	cmp	dx, 720
	je	short T_26
	inc	al ; 2
	cmp	dx, 2400
	je	short T_26
	inc	al ; 3
	cmp	dx, 1440
	je	short T_26
	inc	al ; 4
	cmp	dx, 2880
	je	short T_26
	inc	al ; 5
	cmp	dx, 5760
	jne	short T_27

T_26:
	; al = disk/drive type
	call	compatibility
	jc	short T_27
	mov	[inserted], al	; inserted disk type
	mov	[disk_type], al
T_27:
	mov	al, [option]
	or	al, al
	jz	short T_29
	call	compatibility
	jnc	short T_28
R_03:	
	mov	si, option_not_compat_msg
	jmp	R_14
T_28:
	mov	[requested], al
	mov	[disk_type], al
T_29:
	cmp	byte [drive_type], 0
	jna	short T_30
T_32:
	;mov	al, [DriveNumber]
	;add	al, 'A'
	mov	al, [RD_Drive]
	mov	[DrvNum_Str], al
	mov	si, DrvType_Info_Msg
	call	print_msg
	;sub	al, al ; drive type will be used
	call	get_drive_type_str
	;call	print_msg
T_30:
	cmp	byte [inserted], 0
	jna	short T_31

	mov	si, DiskType_nfo_msg
	call	print_msg
	mov	al, [inserted] ; disk type will be used
	call	get_drive_type_str
	;call	print_msg

T_31:
	cmp	byte [requested], 0
	jna	short T_33

	mov	si, ReqDrvType_Msg1
	call	print_msg
	mov	si, ReqDrvType_Msg2
	call	print_msg
	mov	al, [requested]
	call	get_drive_type_str
	;call	print_msg

;-----------------------------------------------------------------
; Format question
;-----------------------------------------------------------------

T_33:
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

;-----------------------------------------------------------------
; Nextline & Exit
;-----------------------------------------------------------------

T_06:
	mov	si, CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

T_07:
	mov	si, RD_Welcome
	call	print_msg
	jmp	short T_06 ; Exit

;-----------------------------------------------------------------
; writing root directory sectors
;-----------------------------------------------------------------

T_08:
	mov	si, Msg_YES
	call	print_msg
T_09:
	mov	si, CRLF
	call	print_msg

	cmp	byte [Error_Code], 0
	ja	short R_17

	call	set_rdfd_bs
R_17:
	cmp	byte [step], 0
	ja	short R_18

	;mov	byte [step], 0

	cmp	byte [hard_format], 0
	jna	short R_18

	mov	si, Msg_Formatting_Tracks
	call	print_msg

	call	format_tracks	
	jnc	short R_18
R_07:
	jmp	T_19
R_18:
	cmp	byte [step], 1
	ja	short R_19

	mov	byte [step], 1

	mov	si, Msg_Writing_Root_Dir
	call	print_msg

	mov	ax, [RD_FAT12_fd_bs+bsRootDirStart]
	mov	bx, FDFORMAT_FATBUFFER_S9
	mov	si, [RD_FAT12_fd_bs+bsRootDirSects]
	add	si, ax
T_10:
	call	write_fd_sector
	jc	short R_07
	inc	ax
 	cmp	ax, si
	jb	short T_10

	mov	si, Msg_OK
	call	print_msg

;-----------------------------------------------------------------
; writing data sectors
;-----------------------------------------------------------------

R_19:
	cmp	byte [step], 2
	ja	short R_20

	; If hard format is selected, do not write data sectors
	cmp	byte [hard_format], 0
	ja	short R_20

	mov	byte [step], 2

	mov	si, Msg_Writing_Data_Sectors
	call	print_msg
	mov	ah, 3
	mov	bx, 7
	int	10h ; Return Cursor Position
	; DL = Column, DH= Line
	mov	[Cursor_Pos], dx
	mov	ax, [RD_FAT12_fd_bs+bsDataStart]
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
	jz	short R_07 ; Drive not ready msg

	; DX = LBA sector value
	push	dx
	call	mark_bad_cluster
	pop	ax
T_12:
	mov	bx, 7
	inc	ax
	cmp	ax, [RD_FAT12_fd_bs+bsSectors]
	jb	short T_11

	mov	si, Msg_3dot_OK
	call	print_msg

;-----------------------------------------------------------------
; writing FAT sectors
;-----------------------------------------------------------------

R_20:
	cmp	byte [step], 3
	ja	short T_17

	mov	byte [step], 3

	mov	si, Msg_Writing_FAT_Sectors
	call	print_msg
	mov	ax, 1  ; FAT Beginning Address
	mov	bx, FDFORMAT_FATBUFFER
	call	write_fd_sector
	jc	short R_08
	mov	bx, FDFORMAT_FATBUFFER_S9
	mov	si, [RD_FAT12_fd_bs+bsFATsecs]
T_13:
	inc	ax
	call	write_fd_sector
 	jc	short R_08
	cmp	ax, si
	jb	short T_13
	mov	bx, FDFORMAT_FATBUFFER
	inc	ax
	call	write_fd_sector
	jc	short R_08
	mov	bx, FDFORMAT_FATBUFFER_S9
	shl	si, 1 ; 2nd FAT
T_14:
	inc	ax 
        call	write_fd_sector
	jnc	short R_09
R_08:
	jmp	T_19
R_09:
	cmp	ax, si
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
	mov	di, RD_FAT12_fd_bs+bsVolumeLabel
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
	mov	byte [step], 4

	mov	si, Msg_Writing_Boot_Sector
	call	print_msg

	mov	byte [RetryCount], 4
T_18:
	mov	si, RD_FAT12_fd_bs+bsVolumeID

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
	mov	bx, RD_FAT12_fd_bs	; location of boot code

	mov	cx, 1			; cylinder = 0
                     			; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [DriveNumber]
	int	13h
	jnc	short T_24
	dec	byte [RetryCount]
	jnz	short T_18

T_19:
	mov	si, Disk_NotReadyOrError
	call	print_msg
	mov	si, Try_Again_Msg
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
	jmp	T_17

T_23:
	mov	byte [Error_Code], 0FFh
	jmp	T_09

T_24:
	mov	si, Msg_OK
	call	print_msg
	jmp	short T_21

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
	mov	dx, [RD_FAT12_fd_bs+bsSecPerTrack]
					; Sectors per Track
	div	dl
	mov	cl, ah                  ; Sector (zero based)
	inc	cl                      ; To make it 1 based
	shr	al, 1                   ; Convert Track to Cylinder
	adc	dh, 0                   ; Head (0 or 1)
	;mov	si, RD_FAT12_fd_bs+bsDriveNumber
	;mov	dl, [si]
	mov	dl, [DriveNumber]
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
	;        AX = Binary Number (Integer)
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

;-----------------------------------------------------------------

get_drive_type_str:
	mov	si, DiskTypes
	or	al, al
	jnz	short R_21
	mov	al, [drive_type]
R_21:
	dec	al
	mov	ah, DiskTypeStrSize
	mul	ah
	add	si, ax
	;retn

;-----------------------------------------------------------------

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

;-----------------------------------------------------------------

set_rdfd_bs:
	mov	al, [disk_type]
	and	al, al
	jnz	short R_22
	mov	al, [drive_type]
R_22:	
	dec	al
	cmp	al, 4
	jne	short R_23
	mov	si, RD_FAT12_fd_bs+512 ; 2.88MB boot sector
	mov	di, RD_FAT12_fd_bs
	mov	cx, 256
	rep	movsw
	jmp	short R_25
R_23:
	cmp	al, 3
	je	short R_25 ; default BS is already 1.44MB bs
	; al = 0 to 2  ; **

	; RD_FAT12_fd_bs address contains 1.44MB boot sector

	; same boot sector code for 360KB to 1440KB
	; (only bios/boot parameters blocks are different) 
	mov	cx, RD_BPB_size	
	mul	cl
	mov	di, RD_FAT12_fd_bs+13 ; 1st 13 bytes excluded
	mov	si, BPB_table
	add	si, ax
	rep	movsb
R_25:
	retn

;-----------------------------------------------------------------

ft_6:
	mov	si, format_error_msg
	jmp	R_14	

format_tracks:
	mov	cl, [RD_FAT12_fd_bs+bsSecPerTrack]
	mov	[spt], cl
	mov	bx, 80
	cmp	byte [RD_FAT12_fd_bs+bsMedia], 0FDh ; 360KB ?
	jne	short ft_0
	shr	bx, 1 ; 80 -> 40
ft_0:
	mov	si, bx
	shl	si, 1 ; 2*80 (heads*tracks)

	mov	ah, 18h ; set floppy disk type before format
	dec	bl ; 80 -> 79, 40 -> 39
	mov	ch, bl ; maximum number of tracks
	; cl = sectors per track
	mov	dl, [DriveNumber]
	int	13h
	jc	short ft_6
	or	ah, ah
	jnz	short ft_6
	
	; low level format is supported

	; change int 1Eh as temporary
	xor	bx, bx
	mov	ds, bx
	mov	bx, 1Eh*4
	
	; save previous int 1Eh vector
	push	word [bx+2]
	push	word [bx]
		
	mov	[bx], di
	push	es
	pop	word [bx+2]
	push	cs
	pop	ds
	
	push	ds
	pop	es
ft_1:
	xor	dx, dx ; track	
	mov	ah, 0 ; head 0
ft_2:
	mov	cx, [spt]
	mov	di, tracktable
ft_3:
	mov	al, dl ; track
	stosw
	inc	di ; skip sector and bytes per sector (2=512) 	
	inc	di
	loop	ft_3

	mov	di, dx ; track

	; format track DL
	mov	dh, ah
	mov	ah, 05h ; format disk track
	mov	al, [spt]
	mov	ch, dl
	mov	dl, [DriveNumber]
	mov	bx, tracktable
	int	13h
	jc	short ft_5
	
	mov	al, ch ; track number
	xor	ah, ah
	mov	bl, 10
	div	bl
	add	ax, '00'
	mov	[track_str], ax
	mov	al, dh
	add	al, '0'
	mov	[track_str+3], al	

	push	si
	mov	si, track_str-1 ; CR (column 0)
	call	print_msg
	pop	si	

	dec	si
	jz	short ft_4

	mov	ah, dh
	mov	dx, di
	add	dl, ah ; if ah = 1, dl = next track
	xor	ah, 1 ; 0 -> 1, 1 -> 0
	jmp	short ft_2
ft_4:
	mov	si, erase_str
	call	print_msg
	mov	si, Msg_OK+1
	call	print_msg
ft_5:
	; restore previous int 1Eh table
	mov	bx, 0
	mov	ds, bx
	mov	bx, 1Eh*4
	pop	word [bx]
	pop	word [bx+2]
	push	cs
	pop	ds

	retn

;-----------------------------------------------------------------
;  track table for hard format
;-----------------------------------------------------------------

spt: dw 36			
tracktable:
	db 0, 0, 1, 2		
	db 0, 0, 2, 2
	db 0, 0, 3, 2
	db 0, 0, 4, 2
	db 0, 0, 5, 2
	db 0, 0, 6, 2
	db 0, 0, 7, 2
	db 0, 0, 8, 2
	db 0, 0, 9, 2
	db 0, 0, 10, 2
	db 0, 0, 11, 2
	db 0, 0, 12, 2
	db 0, 0, 13, 2
	db 0, 0, 14, 2
	db 0, 0, 15, 2
	db 0, 0, 16, 2
	db 0, 0, 17, 2
	db 0, 0, 18, 2
	db 0, 0, 19, 2
	db 0, 0, 20, 2
	db 0, 0, 21, 2
	db 0, 0, 22, 2
	db 0, 0, 23, 2
	db 0, 0, 24, 2
	db 0, 0, 25, 2
	db 0, 0, 26, 2
	db 0, 0, 27, 2
	db 0, 0, 28, 2
	db 0, 0, 29, 2
	db 0, 0, 30, 2
	db 0, 0, 31, 2
	db 0, 0, 32, 2
	db 0, 0, 33, 2
	db 0, 0, 34, 2
	db 0, 0, 35, 2
	db 0, 0, 36, 2	
	
;-----------------------------------------------------------------
;  bpb tables (except 1.44MB and 2.88 MB diskettes)
;-----------------------------------------------------------------	

BPB_table:
_360K:		 ; al = 0 ; ** (drive type = 1)

;                 jmp short BS_01 ; 0EB,46h
;                 nop		  ; 90h
;bsOemName:       db 'RETRODOS'
;bsBytesPerSec:   dw 512
bs0SecPerClust:   db 2
bs0ResSectors:    dw 1
bs0FATs:          db 2
bs0RootDirEnts:   dw 112
bs0Sectors:       dw 720
bs0Media:         db 0FDh
bs0FATsecs:       dw 2
bs0SecPerTrack:   dw 9
bs0Heads:         dw 2
bs0Hidden1:       dw 0
bs0Hidden2:       dw 0
bs0HugeSectors:   dd 720
bs0DriveNumber:   db 0
bs0Reserved1:     db 0
bs0BpbSignature:  db 29h
bs0VolumeID:      dd 0
bs0VolumeLabel:   db 'NO NAME    '
bs0FileSysType:   db 'FAT12   '
; Retro DOS v4 Extensions
bs0Reserved2:	  dw 'v4'
bs0DataStart:     dw 12
bs0RootDirStart:  dw 5
bs0RootDirSects:  dw 7
RD_BPB_size	equ $ - BPB_table
;bsDirEntsPerSec: dw 16
;BS_01

_1200K:		 ; al = 1 ; ** (drive type = 2)

;                 jmp short BS_01 ; 0EB,46h
;                 nop	          ; 90h
;bsOemName:       db 'RETRODOS'
;bsBytesPerSec:   dw 512
bs1SecPerClust:   db 1
bs1ResSectors:    dw 1
bs1FATs:          db 2
bs1RootDirEnts:   dw 224
bs1Sectors:       dw 2400
bs1Media:         db 0F9h
bs1FATsecs:       dw 7
bs1SecPerTrack:   dw 15
bs1Heads:         dw 2
bs1Hidden1:       dw 0
bs1Hidden2:       dw 0
bs1HugeSectors:   dd 2400
bs1DriveNumber:   db 0
bs1Reserved1:     db 0
bs1BpbSignature:  db 29h 
bs1VolumeID:      dd 0
bs1VolumeLabel:   db 'NO NAME    '
bs1FileSysType:   db 'FAT12   '
; Retro DOS v4 Extensions
bs1Reserved2:	  dw 'v4'
bs1DataStart:     dw 29
bs1RootDirStart:  dw 15
bs1RootDirSects:  dw 14
;bsDirEntsPerSec: dw 16
;BS_01

_720K:		; al = 2 ; ** (drive type = 3)

;                 jmp short BS_01 ; 0EB,46h
;                 nop	          ; 90h
;bsOemName:       db 'RETRODOS'
;bsBytesPerSec:   dw 512
bs2SecPerClust:   db 2
bs2ResSectors:    dw 1
bs2FATs:          db 2
bs2RootDirEnts:   dw 112
bs2Sectors:       dw 1440
bs2Media:         db 0F9h
bs2FATsecs:       dw 3
bs2SecPerTrack:   dw 9
bs2Heads:         dw 2
bs2Hidden1:       dw 0
bs2Hidden2:       dw 0
bs2HugeSectors:   dd 1440
bs2DriveNumber:   db 0
bs2Reserved1:     db 0
bs2BpbSignature:  db 29h
bs2VolumeID:      dd 0
bs2VolumeLabel:   db 'NO NAME    '
bs2FileSysType:   db 'FAT12   '
; Retro DOS v4 Extensions
bs2Reserved2:	  dw 'v4'
bs2DataStart:     dw 14
bs2RootDirStart:  dw 7
bs2RootDirSects:  dw 7
;bsDirEntsPerSec: dw 16
;BS_01

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

RD_Welcome:
	db	0Dh, 0Ah
	db	'Retro DOS v4 FAT12 Floppy Disk Format Utility '
	db	0Dh, 0Ah
	db	'v1.0.231028  (c) Erdogan TAN 2023 '
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: fdformat <drive> ', 0Dh, 0Ah
	db	0Dh, 0Ah
	db	' Example: fdformat A: ', 0Dh, 0Ah
	db	0Dh, 0Ah	
	db	'Optional: fdformat -type(x) <drive> ', 0Dh, 0Ah
	db	0Dh, 0Ah
	db	' Example: fdformat -3 A:  (720KB soft format) '
	db	0Dh, 0Ah
	db	'          fdformat -3x A: (720KB hard format) '     
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'Options: 1  (5.25", 360KB) '
	db	0Dh, 0Ah
	db	'         2  (5.25", 1200KB) '
	db	0Dh, 0Ah
	db	'         3  (3.5", 720KB) '
  	db	0Dh, 0Ah
	db	'         4  (3.5", 1440KB) '
	db	0Dh, 0Ah
	db	'         5  (3.5", 2880KB) '
	db	0Dh, 0Ah, 0
    
Msg_DoYouWantToFormat:
	db	07h
	db	0Dh, 0Ah
        db	0Dh, 0Ah
	db	'WARNING!'
	db	0Dh, 0Ah
	db	'All data on the disk will be erased.'
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'Do you want to format the disk in drive '
RD_Drive:
	db	'A: (Yes/No)? ', 0

Msg_Writing_Boot_Sector:
	db	0Dh, 0Ah
	db	"Writing Retro DOS v4 boot sector...", 0

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
	db	'Disk error or drive not ready! ', 0
Try_Again_Msg:
	db	'Try again? (Y/N) '
	db	0

option_not_compat_msg:
	db	0Dh, 0Ah
	db	'Selected disk type option is not compatible with the drive!'
	db	0Dh, 0Ah, 0

unknown_dtype_msg:
	db	0Dh, 0Ah
	db	'Unknown disk/drive type!'
	db	0

format_error_msg:
	db	0Dh, 0Ah
	db	'Format error!'
	db	0

Error_Code:
	db	0

Msg_Formatting_Tracks:
	db	0Dh, 0Ah
	db	"Formatting tracks...", 0Dh, 0Ah, 0

	db	0Dh ; CR
track_str:
	db	'00 0', 0
erase_str:
	db	0Dh, '     ', 0Dh, 0	
	
hard_format:	; INT 13h (INT 40h) Function 05h format
	db	0

option:	db	0

step:	db	0

DriveNumber:
	db	0
drive_type:
	db 	0
inserted:
	db	0
requested:
	db	0
disk_type:
	db	0

	; index = (selected disk type - 1) * 2 
drive_compat:	; disk type in drive type compatibilities
	db	1, 2 ; drive type 1 & drive type 2 is proper
	db	2, 0 ; drive type 2 is proper
	db	3, 4 ; drive type 3 & drive type 4 is proper	
	db	4, 5 ; drive type 4 & drive type 5 is proper
	db	5, 0 ; drive type 5 is proper

DiskTypes:
	db	'1 - 5.25",  360 KB ',0
DiskTypeStrSize equ $ - DiskTypes
	db	'2 - 5.25", 1200 KB ',0
 	db	'3 - 5.25",  720 KB ',0
	db	'4 - 3.5",  1440 KB ',0
 	db	'5 - 3.5",  2880 KB ',0	

DrvType_Info_Msg:
	db	0Dh, 0Ah
	db	'Drive '
DrvNum_Str:
	db	'@:'
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Drive Type: ", 0

ReqDrvType_Msg1:
	db	0Dh, 0Ah
	db	"Requested ", 0

DiskType_nfo_msg:
	db 	0Dh, 0Ah
	db	"Inserted "
ReqDrvType_Msg2:
	db	"Disk Type: ", 0

RD_FAT12_fd_bs:
	incbin 'FDBS1440.BIN' ; 25/10/2023
	incbin 'FDBS2880.BIN' ; 25/10/2023

FDFORMAT_SECBUFFER:
	times	512 db 0F6h
FDFORMAT_FATBUFFER:
	db	0F0h
	db	0FFh
	db	0FFh
FDFORMAT_FATBUFFER_S9:
	times	512 db 0
 
	db	'(c) Erdogan TAN 2023'

RetryCount:
	db	0

; ----------------------------------------------------------------------------
; uninitialized data
; ----------------------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

alignb 2

bootsector:
	resb	512

end_bss:
