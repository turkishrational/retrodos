; ****************************************************************************
; FD2IMG.ASM (RD2IMG.COM) - Retro DOS v4 Floppy Disk Image Utility
;         (Reads floppy disk -all- sectors and writes to raw disk image file.)
; ****************************************************************************
; Last Update: 31/10/2023
; ----------------------------------------------------------------------------
; Beginning: 29/10/2023
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Retro DOS Operating System Project by ERDOGAN TAN (Beginning: 04/02/2018)
; ----------------------------------------------------------------------------
; Derived from 'rfdimage.s'(RFDIMAGE.COM) source code by Erdogan Tan
; (25/10/2023)
; ****************************************************************************
; nasm fd2img.s -l fd2img.lst -o FD2IMG.COM

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

[BITS 16]
[ORG 100h]

	cli
	cld
	push	cs
	pop	ss
	mov	sp, 0FFFEh
	sti

;-----------------------------------------------------------------
; get fd image file name
;-----------------------------------------------------------------

	mov	si, 80h			; PSP command tail
 	lodsb
	or	al, al 			; command tail length                            
	jz	short R_17		; jump if zero
	mov 	cx, ax
R_01:
	dec	cx
	jz	short R_17

	lodsb

	cmp	al, ' '			; is it SPACE ?
	jb	short R_17
	je	short R_01

	; check floppy disk name
T_01:
	cmp	al, 'A'
	jb	short R_17
	je	short T_03
	cmp	al, 'B'
	jna	short T_03
	cmp	al, 'Z'
	jna	short R_17
	
	cmp	al, 'a'			; a - z 
	jb	short R_17                  
	je	short T_02
	cmp	al, 'b'
	ja	short T_05
T_02:
	and	al, 0DFh		; to upper case
T_03:
	sub	al, 'A'
	cmp	byte [si], ':'
	jne	short T_04

	dec	cx
	jz	short R_17
	inc	si
T_04:
	cmp	byte [si], 20h
	je	short T_06	
	;jmp	short R_17 

R_17:
	mov	si, RetroDOS_Welcome
	;call	print_msg
	jmp	R_16 ; Exit

T_05:
	cmp	al, 'f'
	jne	short R_17
	dec	cx
	dec	cx
	jng	short R_17
	lodsb
	cmp	al, 'd'
	jne	short R_17
	lodsb

	; 30/10/2023
	cmp	byte [si], 20h
	jne	short R_17
	inc	si
	dec	cx
		
	sub	al, '0'
	jz	short T_19

	cmp	al, 1
	jne	short R_17
T_06:	
	mov	[drivenumber], al

T_19:
	; 30/10/2023
	lodsb
	cmp	al, 20h
	ja	short R_02
	jb	short R_17

	dec	cx
	jnz	short T_19
	jmp	short R_17

	; check fd image file name
R_02:
       	mov	di, img_file_name
	stosb
R_03:
	dec	cx
	jz	short R_27

	lodsb
	;cmp	al, 0Dh ; ENTER (CR) key
	cmp	al, 20h ; ' '
	jna	short R_04
	stosb
	cmp	di, img_file_name + 64
	jb	short R_03
R_27:
	cmp	byte [si], 20h 
	ja	short R_11
R_04:
	sub	al, al
	stosb

;-----------------------------------------------------------------
; get drive parameters
;-----------------------------------------------------------------

T_07:
	mov	ah, 08h
	mov	dl, [drivenumber]	; (0 or 1)
	int	13h			; return disk parameters
	jnc	short T_09
T_08:
	; Drive not ready error
	mov	si, msg_drv_not_ready_err
	;call	print_msg
	jmp	R_16 
T_09:
	mov	[disktype], bl
	
	push	cs
	pop	es

	; (this extra check may not be necessary)
	and	bl, bl
	jz	short T_08
	cmp	bl, 5
	ja	short T_08

	;---------------------------------------------------------

	; read boot sector to test (disk is ready in drive)
	mov	ax, 0201h		; read disk
	mov	bx, trackbuffer		; buffer
	mov	dl, [drivenumber]
	xor	dh, dh			; head 0
	mov	cx, 1			; cylinder 0, sector 1 
	int	13h
	jc	short T_08

T_10:
	mov	bx, [disktype]	 ; bh = 0
	add	bx, SPT_table-1
	mov	al, [bx]
	mov	bx, trackbuffer
T_11:
	mov	[SPT], al
	
	; read track 0 (will return with error is SPT is wrong)	
	mov	ah, 02h
	;mov	al, [SPT]
	;mov	dl, [drivenumber]
	;xor	dh, dh			; head 0
	;mov	cx, 1			; cylinder 0, sector 1 
	int	13h
	jnc	short R_10

	; chance SPT and try again
	mov	ah, [disktype]
	dec	byte [disktype]
	mov	al, 18
	cmp	ah, 4
	je	short T_12  ; 18 -> 9 sectors per track
	ja	short T_11  ; 36 -> 18 sectors per track
	cmp	ah, 2
	jne	short T_08  
	; 15 -> 9 sectors per track	
T_12:
	shr	ax, 1 ; al = 9
	jmp	short T_11

R_11:
	mov	si, msg_inv_file_name
	;call	print_msg
	jmp	short R_16

;-----------------------------------------------------------------
; Check image file path
;-----------------------------------------------------------------
	
R_10:
	; 31/10/2023
	mov	dx, img_file_name
	mov	ax, 4300h ; get file attributes	
	int	21h
	jc	short R_20

;-----------------------------------------------------------------
; Check image file attributes
;-----------------------------------------------------------------

R_14:
	and	cl, 1Fh ; directory, volume label, system, hidden, read only
	jnz	short R_19

	call	capitalize ; uppercase file name

;-----------------------------------------------------------------
; Overwrite question
;-----------------------------------------------------------------

	mov	si, msg_overwrite_question1
	call	print_msg

	mov	si, uc_FileName ; uppercase file name
	
	cmp	byte [si], 0	; 0 = not converted due to invalid char
	ja	short R_26	
	mov	si, img_file_name ; file name as written by user (default)
R_26:
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
	je	short R_12 ; Exit                   
	cmp	al, 27
	je	short R_12 ; Exit
	and	al, 0DFh
	cmp	al, 'Y'			; Yes?
	je	short R_18		; write
	cmp	al, 'N'			; No?
	jne	short R_15      
					; no write (exit)  
	mov	si, Msg_NO

;-----------------------------------------------------------------
; Nextline & Exit
;-----------------------------------------------------------------

R_16:
	call	print_msg
R_12:
	mov	si, CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

R_18:
	mov	si, Msg_YES
	call	print_msg

;-----------------------------------------------------------------
; Create image file
;-----------------------------------------------------------------

R_20:
	; 31/10/2023
	call	check_file_name
	jc	short R_11

	; create a new fd image file
	mov	dx, img_file_name
	mov	cx, 0 ; File Attributes
	mov	ah, 3Ch ; MS-DOS Function = Create File
	int	21h
	jnc	short R_28
R_19:
	mov	si, msg_permission_denied
	;call	print_msg
	jmp	short R_16

;-----------------------------------------------------------------
; read tracks and write into image file
;-----------------------------------------------------------------

R_21:
	mov	si, CRLF
	call	print_msg

;-----------------------------------------------------------------
; Open image file for writing
;-----------------------------------------------------------------

R_28:
	; 30/10/2023
	mov	al, 2 ; open for reading and writing
	;mov	dx, img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jc	short R_19 ; file open error
T_13:
	mov	[img_file_handle], ax

	mov	si, msg_disk_type
	call	print_msg

	mov	al, disktypestrsize
	mul	byte [disktype]
	mov	si, disktypestr-disktypestrsize
	add	si, ax
	call	print_msg
	mov	si, CRLF
	call	print_msg
	
	; al = 0
	mov	ah, [SPT]
	shl	ah, 1 ; ax = [SPT] * 512
	mov	[tracksize], ax
	mov	ax, 80
	cmp	byte [disktype], 1
	ja	short T_14
	shr	ax, 1 ; mov al, 40
T_14:
	mov	[numtracks], ax

;-----------------------------------------------------------------
; writing sectors
;-----------------------------------------------------------------

	mov	si, msg_writing
	call	print_msg

	mov	ah, 3
	;mov	bx, 7
	int	10h ; Return Cursor Position
	; DL = Column, DH = Line
	mov	[cursorpos], dx
T_15:	
	call	calculate_percentage

	mov	si, percent_str
	call	print_msg
R_22:
	;sub	al, al
	mov	bx, [img_file_handle]
	mov	cx, [tracksize] ; SPT*512
	mov	dx, trackbuffer ; buffer
	mov	ah, 40h ; write to file	
	int	21h
	jc	short T_18

	xor	byte [head], 1
	jz	short R_23

	call	read_track_h1
	jc	short R_24
	jmp	short R_22
R_23:
	mov	ch, [track]
	inc	ch
	cmp	ch, [numtracks]
	jnb	short R_25
	
	mov	[track], ch

	call	read_track_h2
	jnc	short T_17
R_24:
	mov	si, msg_disk_read_err
	;call	print_msg
T_16:
	jmp	R_16

T_17:
	mov	dx, [cursorpos]
	mov	ah, 2
	mov	bx, 7
	int	10h  ; Set Cursor Position
	jmp	short T_15

T_18:
	; image file writing error
	mov	si, msg_writing_err
	;jmp	R_16
	jmp	short T_16
R_25:
	mov	si, Msg_OK
	;jmp	R_16
	jmp	short T_16

;-----------------------------------------------------------------
; Check File name
;-----------------------------------------------------------------

	; 31/10/2023
check_file_name:
	call	skip_path
	jc	short cfn_4

	; si = file name offset (after the last '\' or ':'
	; bx = si
	
	xor	dx, dx
	;xor	ah, ah
cfn_1:	
	lodsb
	and	al, al
	jz	short cfn_6
	;mov	ah, al ; the last char > 0
	inc	dh

	call	check_inv_fname_chars
	jc	short cfn_4

	cmp	al, '.'
	jne	short cfn_2
	or	dl, dl
	jnz	short cfn_3 ; 2 dots	
	cmp	dh, 2
	jb	short cfn_4 ; the 1st char of fname is a dot
	cmp	dh, 9
	ja	short cfn_3 ; > 8 chars before dot
	mov	dl, dh	; dot position
	jmp	short cfn_1 
cfn_2:
	mov	cl, dh
	or 	dl, dl
	jz	short cfn_5 ; not a dot in fname chars
	cmp	cl, 12
	ja	short cfn_3
	sub	cl, dl
	cmp	cl, 3	    ; > 3 chars after dot	
	jna	short cfn_1
cfn_3:
	stc
cfn_4:
	retn
cfn_5:
	cmp	cl, 8
	jna	short cfn_1
	stc
	retn		
cfn_6:
	or	dh, dh
	jz	short cfn_3 ; not any char after the last '\'
	; dh = count of file name chars
	; dl = dot position (2 to 9)
	cmp	dh, dl
	;cmp	ah, '.'
	je	short cfn_3 ; the last chr of fname is a dot	
cifnc_retn:
	clc
	retn

check_inv_fname_chars:
	mov	di, invalid_fname_chars
	mov	cx, sizeInvFnChars
	repne	scasb
	jcxz	cifnc_retn
	stc
	retn

invalid_fname_chars:
	db 22h, 27h, 28h, 29h, 2Ah, 2Bh, 2Ch, 2Fh
	db 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh, 40h
	db 5Bh, 5Ch, 5Dh, 5Eh, 60h
sizeInvFnChars  equ ($ - invalid_fname_chars)

skip_path:
	mov	si, img_file_name
	xor	dx, dx
	xor	al, al
skp_1:
	mov	ah, al
	mov	bx, si
skp_2:
	lodsb
	inc	dx
	cmp	al, ':'
	je	short skp_4
	cmp	al, '\'
	je	short skp_5
	and	al, al
	jz	short skp_3
	call	check_inv_fname_chars
	jnc	short skp_2
	retn
skp_3:
	mov	si, bx
	retn
skp_4:
	cmp	dl, 2
	je	short skp_1
	; not '?:'
	stc
	retn
skp_5:
	inc	bx
	inc	bx
	cmp	bx, si
	jne	short skp_1
	cmp	al, ah
	jne	short skp_1
	; '\\'
	stc
	retn

;-----------------------------------------------------------------
; File name capitalization
;-----------------------------------------------------------------

	; 31/10/2023
capitalize:
	; skip path at first	
	;mov	si, img_file_name
	call	skip_path
	jc	short R_08 ; invalid path name	
			   ; (may be correct for dos/windows
			   ; but contains invalid -defined- chars)	
		; (it will be used without uppercase conversion)
		; ((dos/windows 'create file' or 'open file' 
		;   system call will return with error if it is 
		;  invalid file and path name for dos/windows.))
 	
	mov	di, uc_FileName
	mov	cx, 12
R_05:
	lodsb
	cmp	al, 'a'
	jnb	short R_07
	and	al, al
	jz	short R_09
R_06:
	stosb
	jmp	short R_05 		
R_07:
	cmp	al, 'z'
	ja	short R_06
	and	al, 0DFh ; NOT 32
	jmp	short R_06
R_08:
	xor	al, al ; 0
R_09:
	stosb
	retn	
	
;-----------------------------------------------------------------
; Print messages
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
; read disk track
;-----------------------------------------------------------------

read_track_h1:
	mov	ch, [track]
read_track_h2:
	; read track (will return with error if SPT is wrong)	
	mov	bx, dx	; trackbuffer
	mov	ah, 02h
	mov	al, [SPT]
	mov	dl, [drivenumber]
	mov	dh, [head]		; head 0 or 1
	; ch = cylinder/track
	mov	cl, 1			; sector 1 
	int	13h
	mov	al, 0
	retn

;-----------------------------------------------------------------
; calculate and set writing percentage
;-----------------------------------------------------------------

calculate_percentage:
	mov	di, percent_str
	mov	al, [track]
	inc	al
	mov	cl, [numtracks] ; 80 or 40
	cmp	al, cl
	jb	short cperc_1
	mov	al, '1'
	stosb
	xor	ax, ax
	jmp	short cperc_2
cperc_1:
	mov	ah, 100
	mul	ah
	div	cl
	; al = %
	xor	ah, ah
	mov	cl, 10
	div	cl
cperc_2:
	add	ax, '00' ; 3030h
	stosw
	mov	ax, '%'
	stosw
	retn

;-----------------------------------------------------------------
;  Data
;-----------------------------------------------------------------

	; 30/10/2023

drivenumber:
	db	0
disktype:
	dw	0

SPT:	db	0
SPT_table:
	db	9, 15, 9, 18, 36

track:	db	0
head:	db	0

percent_str:
	db	'00%', 0

;-----------------------------------------------------------------
;  Messages
;-----------------------------------------------------------------

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Floppy Disk Image Utility (disk copy to img file)'
	db	0Dh, 0Ah
	db	"v1.0.231031  (c) Erdogan TAN 2023"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: fd2img <fd name> <image file name> '
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'  Example: fd2img a: a.img '
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'Disk names: A:, B: or fd0 (A:), fd1 (B:) ' 
	db	0Dh, 0Ah
	db	0

msg_inv_file_name: 
	db	0Dh, 0Ah
	db	"Invalid file (or path) name ! "
	db	0Dh, 0Ah, 0

msg_permission_denied: 
	db	0Dh, 0Ah
	db	"Permission denied ! (File open/create error) ! "
	db	0Dh, 0Ah
	db	"(Access error or it is a directory or volume name) !"
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

msg_drv_not_ready_err: 
	db	0Dh, 0Ah
	db	"Drive not ready !"
	db	0Dh, 0Ah, 0

msg_disk_read_err: 
	db	0Dh, 0Ah
	db	"Disk read error !"
	db	0Dh, 0Ah, 0

msg_writing_err:
	db	0Dh, 0Ah
	db	"Image file writing error !"
	db	0Dh, 0Ah, 0	

msg_yes_no:
	db	'(Yes/No)? ', 0		

msg_writing:
	db	"Writing tracks: ", 0

msg_disk_type:
	db	0Dh, 0Ah
	db	'Disk Type: '
	db	0

disktypestr:
	db	'1 - 5.25",  360 KB',0
disktypestrsize equ $ - disktypestr
	db	'2 - 5.25", 1200 KB',0
 	db	'3 - 5.25",  720 KB',0
	db	'4 - 3.5",  1440 KB',0
 	db	'5 - 3.5",  2880 KB',0	

Msg_YES:
	db	' YES', 0
Msg_NO:
	db	' NO', 0

Msg_OK:
	db	' OK.'
CRLF:
	db	0Dh, 0Ah, 0

	db	'(c) Erdogan TAN 2023'

;SizeOfFile equ $-100

;-----------------------------------------------------------------
; uninitialized data
;-----------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

alignb 2

img_file_handle:
	resw	1
img_file_name:  
	resb	66
uc_FileName:	; 31/10/2023
	resb	14
cursorpos:
	resw	1
numtracks:
	resw	1
tracksize:
	resw	1
trackbuffer:
	resb	36*512

end_bss: