; ****************************************************************************
; RDFD1200.ASM (RDFD1200.COM) - Retro DOS v4 Floppy Disk Boot Sector Utility
; 							 (for MSDOS/WINDOWS)
; ----------------------------------------------------------------------------
; Last Update: 25/10/2023
; ----------------------------------------------------------------------------
; Beginning: 25/10/2023
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (rdfdboot.s)
; ----------------------------------------------------------------------------
; Modified from 'rdhdboot.s'(RDHDBOOT.COM) source code by Erdogan Tan
; (25/10/2023) - RETRODOS v4 hard disk FAT12 fs boot sector update utility -
; ----------------------------------------------------------------------------
; Modified from 'trfdboot.s'(TRFDBOOT.COM) source code by Erdogan Tan
; (06/09/2020) - TRDOS 386 1440KB floppy disk boot sector update utility -
; ****************************************************************************
; assembling: nasm rdfdboot.s -l rdfdboot.lst -o RDFDBOOT.COM -Z error.txt

; ----------------------------------------------------------------------------
; equations
; ----------------------------------------------------------------------------

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

; ----------------------------------------------------------------------------
; code
; ----------------------------------------------------------------------------

[BITS 16]
[ORG 100h]

	cli
	cld
	push	cs
	pop	ss
	mov	sp, 0FFFEh
	sti

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, 80h			; PSP command tail
	mov	cl, [si]
	or	cl, cl                               
	jz	short T_05		; jump if zero

T_01:
	inc	si

	mov	al, [si]
	cmp	al, ' '			; is it SPACE ?
	jne	short T_02

	dec	cl                                  
	jnz	short T_01                  
	jmp	short T_05

T_02:
	cmp	al, 'A'
	jb	short T_05
	je	short T_03
	cmp	al, 'B'
	jna	short T_03
	cmp	al, 'Z'
	jna	short T_05
T_18:	
	cmp	al, 'a'			; a - z 
	jb	short T_05                  
	je	short T_19
	cmp	al, 'b'
	ja	short T_05
T_19:
	sub	al, 'a'-'A'		; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_03:
	mov	[RD_Drive], al
	jmp	short T_09

T_04:
	add	al, 'A'-'0'		; 0 based -> A based
	jmp	short T_03 

T_24:
	inc	si
	cmp	byte [si], ' '
	jna	short T_04		

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_05:
	mov	si, RetroDOS_Welcome
	jmp	T_20

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_09:
	mov	ah, 08h
	mov	dl, [RD_Drive]	; drive
	sub	dl, 'A'			; make it zero based
	mov	[RD_FAT12_fd_bs+bsDriveNumber], dl 
	int	13h			; return disk parameters
	jc	short R_15   ; Drive not ready error

	push	cs
	pop	es			; restore es

	cmp	bl, 2			; Drive Type
	ja	short T_25   ; not a 360KB diskette drive

	; check for valid FAT12 BS
	mov	byte [RetryCount], 5

	;mov	ax, 0201h		; read disk
	mov	bx, bootsector		; location of boot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	;mov	dl, [RD_Drive]	; drive 
	;sub	dl, 'A'			; make it zero based
	;;mov	[RD_FAT12_fd_bs+bsDriveNumber], dl
	mov	dl, [RD_FAT12_fd_bs+bsDriveNumber]
T_21:
	mov	ax, 0201h
	int	13h
	;jc	short T_14	
	jnc	short T_06		; read boot sector, OK

	dec	byte [RetryCount]
	jnz	short T_21
T_25:
R_16:
	mov	si, msg_disk_read_err
	jmp	short R_14
R_15:	
	mov	si, msg_drv_not_ready_err
R_14:
	call	print_msg
	int	20h

T_06:
	cmp	word [bootsector+bsBytesPerSec], 512
	jne	short T_22
	cmp	byte [bootsector+bsBpbSignature], 29h
	jne	short T_22
	; 25/10/2023
	; check 1.2 MB FD (required) FAT12 BPB parameters
	cmp	byte [bootsector+bsMedia], 0F9h
	jne	short T_22	
	cmp	byte [bootsector+bsSecPerClust], 1
	jne	short T_22
	cmp	word [bootsector+bsSectors], 2400
	jne	short T_22

	; Overwrite question

	mov	si, msg_overwrite_question
	call	print_msg

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
T_20:
	call	print_msg

	; Next line and Exit
R_10:
T_12:
	mov	si, RD_CRLF
R_21:
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h 
hang:
	hlt
	jmp	short hang

R_20:
	mov	si, msg_disk_write_err
	jmp	short R_14

T_22:
	mov	si, RD_invalid_bootsector
	jmp	short R_14

R_12:
	mov	si, msg_YES
	call	print_msg
	
	; copying the disk's BPB to the new Retrodos v4 boot sector
	lea	si, [bootsector+bsOemName] 
	mov	di, RD_FAT12_fd_bs + bsOemName
	mov	cx, bsFileSysType - bsOemName
	rep	movsb
R_13:
	; Calculating Retro DOS v4 extended BS parameters
	mov	ax, [RD_FAT12_fd_bs+bsFATsecs]
	mov	cl, [RD_FAT12_fd_bs+bsFATs]
	;mul	cx
	dec	cl
	shl	ax, cl ; * 2
	add	ax, [RD_FAT12_fd_bs+bsResSectors]
	mov	[RD_FAT12_fd_bs+bsRootDirStart], ax
	mov	cx, [RD_FAT12_fd_bs+bsRootDirEnts]

	mov	dx, 15
	;add	cx, 15
	add	cx, dx
	
	shr	cx, 4 ; 16 entries per sector
	mov	[RD_FAT12_fd_bs+bsRootDirSects], cx

	inc	dx ; dx = 16
	mov	[RD_FAT12_fd_bs+bsDirEntsPerSec], dx

	add	ax, cx
	mov	[RD_FAT12_fd_bs+bsDataStart], ax	

	; "Updating boot sector to .." message

	mov	si, msg_writing_boot_sector
	call	print_msg

	; writing boot sector
	mov	dl, [RD_FAT12_fd_bs+bsDriveNumber]		
	mov	dh, 0 ; head 0
	; ch = 0      ; track 0 (cylinder 0)
	mov	cl, 1 ; sector 1

	mov	ax, 0301h ; write disk sector
	
	mov	bx, RD_FAT12_fd_bs
	int	13h
	jc	short R_20
;R_20:
	mov	si, msg_OK
	call	print_msg
	
	mov	ax, 65535
wloop:
	dec	ax
	jnz	short wloop	
	jmp	T_12

	; print/write messages (on console screen)
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

; ----------------------------------------------------------------------------
; initialized data
; ----------------------------------------------------------------------------

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Retro DOS v4 - 1.2 MB Floppy Disk Boot Sector Update Utility'
	db	0Dh, 0Ah
	db	"v1.0.231025  (c) Erdogan TAN 2023"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: rdfdboot a: (or b:)'
	db	0

msg_overwrite_question:
	db	0Dh, 0Ah
	db	'WARNING !', 0Dh, 0Ah
	db	'Do you want to overwrite the boot sector of drive '
RD_Drive:
	db	'A: (Yes/No)? ', 0

msg_drv_not_ready_err: 
	db	0Dh, 0Ah
	db	"Drive not ready !"
	db	0Dh, 0Ah, 0

msg_disk_read_err: 
	db	0Dh, 0Ah
	db	"Disk read error !"
	db	0Dh, 0Ah, 0

msg_disk_write_err: 
	db	0Dh, 0Ah
	db	"Disk write error !"
	db	0Dh, 0Ah, 0

RD_invalid_bootsector:
	db 0Dh, 0Ah
	db 'Invalid boot sector!'
	db 0Dh,0Ah
	db '(! not a valid 1.2 MB FAT12 floppy disk !)'
	db 0
msg_NO:
	db	' NO ..', 0Dh, 0Ah, 0
msg_YES:
	db	' YES ..', 0Dh, 0Ah, 0

msg_writing_boot_sector:
	db	"Updating boot sector to Retro DOS v4 (v2 compatible) format ...", 0

msg_OK:
	db	' OK.'
RD_CRLF:
	db	0Dh, 0Ah, 0

align 2
	dw	417

RD_FAT12_fd_bs:	; Boot Sector code

; Boot Sector Last Update: 25/10/2023
incbin	"FDBS1200.BIN"	; Kernel file: 'MSDOS.SYS'
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