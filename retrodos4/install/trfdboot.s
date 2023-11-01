; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.7 - trfdboot.s
; ----------------------------------------------------------------------------
; Last Update: 30/10/2023  (Previous: 06/09/2020)
; ----------------------------------------------------------------------------
; Beginning: 25/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trfdboot.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRFDBOOT.ASM (31/07/2011)
; ****************************************************************************

; TRFDBOOT.ASM
; Turkish Rational DOS
; Disk Operation System v1.0 Project
; 1.44 MB Floppy Disk Boot Code
;
; Copyright (C) 1998-2011  Erdogan TAN  [ Last Modification: 31/07/2011 ]
; 12/06/2011 -> Boot sector bugfix (dl = bsDriveNumber <-> DS=CS)
; 27/09/2009 -> 'Operation System' has been changed to 'Operating System'
; 2005 -> trfdboot utility
; 1998 -> beginning year of trdos project
;
; ****************************************************************************
; assembling: nasm trdfboot.s -l trdfdboot.lst -o TRDFDBOOT.COM -Z error.txt

; previous version: trfdboot.s v4.1 -- 12/02/2018

; ----------------------------------------------------------------------------
; equations
; ----------------------------------------------------------------------------

; boot sector parameters

;bsDriveNumber	equ TRDOS_FAT12_fd_bs + 24h
;bsVolumeID	equ TRDOS_FAT12_fd_bs + 27h

bsOemName	equ 3          
bsBytesPerSec	equ 11  ; 512 (word)
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
	;cmp	al, '0'			; 0 - 9
	;jb	short T_05
	;cmp	al, '9'			; allow number for drive
	;jna	short T_04                    

	inc	si
	cmp	byte [si], ':'
	je	short T_23
	cmp	byte [si], ' '
	jna	short T_23

	cmp	al, 'f'
	jne	short T_05  	
	cmp	byte [si], 'd'
	jne	short T_05
	inc	si
	mov	al, [si]
	cmp	al, '0'
	je	short T_24
	;jb	short T_05
	cmp	al, '1'
	je	short T_24
	jmp	short T_05

T_23:
	cmp	al, 'A'
	jb	short T_05
	je	short T_03
	;cmp	al, 'Z'			; A - Z
	;jna	short T_03                    
	cmp	al, 'B'
	jna	short T_03
	cmp	al, 'Z'
	jna	short T_05
T_18:	
	cmp	al, 'a'			; a - z 
	jb	short T_05                  
	je	short T_19
	;cmp	al, 'z'                           
	;ja	short T_05     
	cmp	al, 'b'
	ja	short T_05
T_19:
	sub	al, 'a'-'A'		; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_03:
	mov	[TrDOS_Drive], al
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
	mov	si, TrDOS_Welcome
	call	print_msg
	;cmp	cl, 0
        ;ja	short T_12
	jmp	T_12

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_09:
	mov	ah, 08h
	mov	dl, [TrDOS_Drive]	; drive
	sub	dl, 'A'			; make it zero based 
	int	13h			; return disk parameters

	push	cs
	pop	es			; restore es

	cmp	bl, 4			; Drive Type
	;jne	short T_14
	jne	T_25

	; check for valid FAT12 BS
	mov	byte [RetryCount], 6

	;mov	ax, 0201h		; read disk
	mov	bx, bootsector		; location of boot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	mov	dl, [TrDOS_Drive]	; drive 
	sub	dl, 'A'			; make it zero based
	mov	[TRDOS_FAT12_fd_bs+bsDriveNumber], dl
T_21:
	mov	ax, 0201h
	int	13h
	;jc	short T_14	
	jnc	short T_06		; read boot sector, OK

	dec	byte [RetryCount]
	jnz	short T_21
T_25:
	call	T_20			; write error message
	jmp	T_12 			; terminate

T_06:
	cmp	word [bootsector+bsBytesPerSec], 512
	jne	short T_22
	cmp	byte [bootsector+bsBpbSignature], 29h
	jne	short T_22
	cmp	dword [bootsector+bsFileSysType], 'FAT1'
	jne	short T_22
	cmp	byte [bootsector+bsFileSysType+4], '2'
	jne	short T_22
	; 30/10/2023
	cmp	byte [bootsector+bsSectors], 2880
	jne	short T_22

	mov	si, TrDOS_PressKeyWhenReady
	call	print_msg

T_07:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'M'-40h		; Enter (OK) key
	je	short T_08		; write
	cmp	al, 'C'-40h
	je	short T_12		; no write (exit)
	cmp	al, 27
	je	short T_12
	jmp	short T_07

T_22:
	mov	si, TrDOS_invalid_bootsector
	call	print_msg
	jmp	short T_12

T_08:
	mov	si, TrDOS_CRLF
	call	print_msg

	mov	byte [RetryCount], 4

T_10:
	xor	ax, ax
	int	1Ah			; get time of day

	mov	si, TRDOS_FAT12_fd_bs+bsVolumeID 

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
	xchg	ch, cl
	xchg	dh, dl
	
	add	cx, dx  
	add	[si+2], cx
              
	mov	ax, 0301h		; write to disk
	mov	bx, TRDOS_FAT12_fd_bs	; location of boot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	;mov	dl, [TrDOS_Drive]	; drive 
	;sub	dl, 'A'			; make it zero based
	;mov	[bsDriveNumber], dl
	mov	dl, [TRDOS_FAT12_fd_bs+bsDriveNumber]
	int	13h
	jc	short T_14		; if everything is ok -->

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; success. try again ?
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_11:
	mov	si, TrDOS_disk_WrittenSuccesfully
	call	print_msg

T_12:
	mov	si, TrDOS_CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

T_13:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'y'
	je	short T_15		; retry
	cmp	al, 'Y'
	je	short T_15
	cmp	al, 'n'
	je	short T_12 		; exit
	cmp	al, 'N'
	je	short T_12
	cmp	al, 'C'-40h
	je	short T_12                   
	cmp	al, 27
	je	short T_12
	jmp	short T_13

T_14:
	dec	byte [RetryCount]
	jnz	short T_10
	call	T_20
	jmp	short T_13
T_20:
	mov	si, TrDOS_disk_NotReadyOrError
	;call	print_msg
	;jmp	short T_13
	jmp	print_msg

T_15:
	mov	si, TrDOS_CRLF
	call	print_msg
	jmp	T_10

print_msg:
T_16:
	lodsb				; Load byte at DS:SI to AL
	and	al, al            
	jz	short T_17       
	mov	ah, 0Eh			
	mov	bx, 07h             
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short T_16          

T_17:
	retn

; ----------------------------------------------------------------------------
; initialized data
; ----------------------------------------------------------------------------

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TrDOS_Welcome:
	db 0Dh, 0Ah
	db 'TR-DOS Floppy Disk Boot Sector Update Utility v5.0'
	db 0Dh, 0Ah
	db '(c) Erdogan TAN 1998-2023'
	db 0Dh,0Ah
	db 0Dh,0Ah
	db 'Usage: trfdboot <drive> '
	db 0Dh,0Ah, 0Dh, 0Ah
	db 'Drive names: '
	db 0Dh, 0Ah
	db '      fd0 or A: ..for 1st floppy disk drive '
	db 0Dh, 0Ah
	db '      fd1 or B: ..for 2nd floppy disk drive '
	db 0Dh, 0Ah, 0

	db '12/02/2018'
	db ' ' 	
	db '06/09/2020'
	db 0

TrDOS_PressKeyWhenReady:
	db 0Dh, 0Ah
	db 'Press Enter to write boot sector on disk '
TrDOS_Drive:
	db 'A: ', 0

TrDOS_disk_WrittenSuccesfully:
	db 0Dh, 0Ah
	db 'Boot sector successfully updated to TRDOS v2.0 format...'
TrDOS_CRLF:
	db 0Dh, 0Ah, 0

TrDOS_disk_NotReadyOrError:
	db 0Dh, 0Ah
	db 'Disk error or drive not ready! Try again? (Y/N) '
	db 0

TrDOS_invalid_bootsector:
	db 0Dh, 0Ah
	db 'Invalid boot sector (not a valid FAT12 fs disk)! '
	db 0

align 2
	dw 417

TRDOS_FAT12_fd_bs:	; Boot Sector code

; Boot Sector Last Update: 30/10/2023
; 29/01/2016
incbin	"TRFDBS.BIN"	; Kernel file: 'TRDOS386.SYS'
RetryCount:
	db 0

; ----------------------------------------------------------------------------
; uninitialized data
; ----------------------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

alignb 2

bootsector:
	resb 512

end_bss: