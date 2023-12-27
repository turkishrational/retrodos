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
	ja	short R_17 ; 31/10/2023
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

;-----------------------------------------------------------------
; Find image file
;-----------------------------------------------------------------
	
R_10:
	mov	dx, img_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS-DOS Function = Find First File
	int	21h
	jc	short R_05

;-----------------------------------------------------------------
; Check image file features
;-----------------------------------------------------------------

	mov	si, DTA_Attrib
	mov	al, [si]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	jnz	short R_05

	; disk sizes: (in bytes)
	; 2.88 MB = 2D0000h
	; 1.44 MB = 168000h
	;  720 KB = 0B4000h
	; 1200 KB = 12C000h
	;  360 KB = 05A000h

	mov	si, [disktype]
	mov	di, si
	shl	si, 1
	shl	si, 1
	add	si, imagesizetbl-4
	lodsw
	mov	cx, ax
	mov	bx, [si]	

	mov	si, DTA_FileSize
	lodsw
	mov	dx, [si]

	test	ax, 4095 ; 0FFFh
	jnz	short R_06

	cmp	dx, bx
	jne	short R_07
	cmp	ax, cx
	jne	short R_07
		
	; same image/disk size
	;---------------------------------------------------------
	
	; read track (will return with error if the inserted disk
	;	      is not same with the disk image)

	add	di, SPT_table-1
	mov	al, [di]
	mov	[SPT], al
	
	; read track 1 to test (disk is ready in drive)
	; al = SPT
	mov	ah, 02h			; read disk
	mov	bx, trackbuffer		; buffer
	mov	dl, [drivenumber]
	xor	dh, dh			; head 0
	mov	cx, 101h		; track 1, sector 1 
	int	13h
	jnc	short T_13

	jmp	T_08	; drive not ready error

R_07:
	mov	cx, 10
	mov	di, imagesizetbl
	repne	scasw
	jcxz	R_06	

	mov	si, msg_improper_size
	jmp	short R_16

R_06:
	mov	si, msg_inv_image_file
	jmp	short R_16

R_05:
	mov	si, msg_file_notfound
	jmp	short R_16

;-----------------------------------------------------------------
; Check current drive
;-----------------------------------------------------------------

T_13:
	mov	ah, 19h	; get current/default drive
	int	21h	

	cmp	al, dl	; cmp al, [drivenumber]
	jne	short R_11

	mov	si, msg_permission_denied
	jmp	short R_16

;-----------------------------------------------------------------
; Overwrite question
;-----------------------------------------------------------------

R_11:
	mov	al, [drivenumber]
	add	al, 'A'
	mov	[drv_chr], al

	mov	si, msg_overwrite_question
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

R_19:
	mov	si, msg_permission_denied
	;call	print_msg
	jmp	short R_16

R_18:
	mov	si, Msg_YES
	call	print_msg
	;jmp	short R_20

;-----------------------------------------------------------------
; Read image file ands write onto disk
;-----------------------------------------------------------------

R_20:
	mov	si, CRLF
	call	print_msg

;-----------------------------------------------------------------
; Open image file for reading
;-----------------------------------------------------------------

	; 31/10/2023

	;mov	al, 0 ; open for reading
	mov	dx, img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jnc	short R_21
T_18:
	mov	si, msg_reading_err
	jmp	R_16
R_21:
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
	mov	ah, 3Fh ; read file	
	int	21h
	jc	short T_18

	call	write_track
	jc	short R_24

	xor	byte [head], 1
	jnz	short R_22
R_23:
	inc	byte [track]
	mov	al, [numtracks]
	cmp	al, [track]
	jna	short R_25
	
	mov	dx, [cursorpos]
	mov	ah, 2
	mov	bx, 7
	int	10h  ; Set Cursor Position
	jmp	short T_15

R_24:
	mov	si, msg_writing_err
	;call	print_msg
	jmp	R_16

R_25:
	mov	si, Msg_OK
	jmp	R_16

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
; write disk track
;-----------------------------------------------------------------

write_track:
	mov	bx, dx	; trackbuffer
	mov	ah, 03h
	mov	al, [SPT]
	mov	dl, [drivenumber]
	mov	dh, [head]		; head 0 or 1
	mov	ch, [track]
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

	; disk sizes: (in bytes)
	; 2.88 MB = 2D0000h
	; 1.44 MB = 168000h
	;  720 KB = 0B4000h
	; 1200 KB = 12C000h
	;  360 KB = 05A000h

	; 31/10/2023
imagesizetbl:
	dd	05A000h
	dd	120000h
	dd	0B4000h
	dd	168000h
	dd	2D0000h

;-----------------------------------------------------------------
;  Messages
;-----------------------------------------------------------------

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Floppy Disk Image Utility (image copy to disk)'
	db	0Dh, 0Ah
	db	"v1.0.231031  (c) Erdogan TAN 2023"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: img2fd <fd name> <image file name> '
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'  Example: img2fd a: a.img '
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	'Disk names: A:, B: or fd0 (A:), fd1 (B:) ' 
	db	0Dh, 0Ah
	db	0

msg_inv_image_file:
	db	0Dh, 0Ah
	db	"Invalid floppy disk image file !", 0Dh, 0Ah
	db	"(does not correspond to any fd image) !"
	db	0Dh, 0Ah, 0

msg_file_notfound:
	db	0Dh, 0Ah
	db	"File not found !", 0Dh, 0Ah
	db	0Dh, 0Ah, 0

msg_improper_size:
	db	0Dh, 0Ah
	db	"The fd image and the floppy disk does not fit !"
	db	0Dh, 0Ah, 0

msg_permission_denied: 
	db	0Dh, 0Ah
	db	"Permission denied ! ", 0Dh, 0Ah
	db	"(Current drive selected) !"
	db	0Dh, 0Ah, 0

msg_overwrite_question:
	db	0Dh, 0Ah
	db	"Do you want to overwrite drive "
drv_chr:
	db	"@: (Yes/No)? ", 0

msg_drv_not_ready_err: 
	db	0Dh, 0Ah
	db	"Drive not ready !"
	db	0Dh, 0Ah, 0

msg_reading_err: 
	db	0Dh, 0Ah
	db	"File read error !"
	db	0Dh, 0Ah, 0

msg_writing_err:
	db	0Dh, 0Ah
	db	"Disk write error !"
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
cursorpos:
	resw	1
numtracks:
	resw	1
tracksize:
	resw	1
trackbuffer:
	resb	36*512

end_bss:		
  