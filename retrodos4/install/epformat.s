; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.7 - epformat.s
; ----------------------------------------------------------------------------
; Extended DOS Partition (FAT File System) Format Utility for TRDOS 386 v2 OS.
; ----------------------------------------------------------------------------
; Last Update: 28/10/2023
; ----------------------------------------------------------------------------
; Beginning: 22/09/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (epformat.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from TRDOS Operating System v2.0 (80386) source code by Erdogan Tan
; hdformat.s (14/09/2020)
; ****************************************************************************
; Derived from hdimage.s (08/03/2019), Retro DOS v2 Hard Disk BS Utility

; HDFORMAT.ASM
; Turkish Rational DOS
; Disk Operation System v2.0 Project
; Primary DOS Partition (FAT12, FAT16, FAT32 fs) FORMAT Code
;
; Copyright (C) 2020  Erdogan TAN 
; ****************************************************************************
; assembling: nasm epformat.s -l epformat.lst -o EPFORMAT.COM -Z error.txt

; Note: Only for formatting logical DOS drives in extended DOS partitions.

; ----------------------------------------------------------------------------
; equations
; ----------------------------------------------------------------------------

; boot sector parameters

bsOemName	equ 3	; ('MSWIN4.1') --> 'TRDOS386'       
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
	jz	short T_3		; jump if zero
T_1:
	inc	si

	mov	al, [si]
	cmp	al, ' '			; is it SPACE ?
	jne	short T_2

	dec	cl                                  
	jnz	short T_1                  
	jmp	short T_3
T_2:
	inc	si

	cmp	al, 'h'
	jne	short T_3 	
	cmp	byte [si], 'd'
	jne	short T_3
	inc	si
	mov	al, [si]
	cmp	al, '0'
	je	short T_4
	jb	short T_3
	cmp	al, '3'
	jna	short T_4

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_3:
	mov	si, TrDOS_Welcome
	call	print_msg
	;cmp	cl, 0
        ;ja	short T_44
	jmp	T_44

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_4:
	inc	si
	cmp	byte [si], ' '
	ja	short T_3		

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	[TrDOS_Drive], al	; '0' .. '3'

	mov	[drv_str], al

	mov	ah, 08h
	mov	dl, al
	add	dl, 80h -'0'		; make it 80h based 
	mov	[drv], dl
	int	13h			; return disk parameters

	push	cs
	pop	es			; restore es

	or	ah, ah
	jnz	short T_6		; error
	
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
	mov	[csize], ax
	mul	cx ; * cylinders
		; dx:ax = chs limit
	mov	[CHS_limit], ax
	mov	[CHS_limit+2], dx

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; read MBR
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; check for (valid) extended dos partition

	;mov	byte [RetryCount], 4
	mov	di, 5

	;mov	ax, 0201h		; read disk
	mov	bx, MBR			; location of masterboot code

	mov	cx, 1			; cylinder = 0
					; sector = 1
	mov	dh, 0			; head = 0
	;mov	dl, [TrDOS_Drive]	; drive 
	;add	dl, 80h -'0'		; make it 80h based 
	mov	dl, [drv]
T_5:
	mov	ax, 0201h
	int	13h
	;jc	short T_46	
	jnc	short T_7		; read masterboot sector, OK
	
 	; reset hard disk(s)
	xor	ah, ah
	;mov	dl, [drv]
	int	13h

	;dec	byte [RetryCount]
	dec	di
	jnz	short T_5

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write disk error message and terminate
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_6:
	mov	byte [zbyte], 0 ; message without (Y/N) question 

	call	T_46			; write error message
	jmp	T_44 			; terminate

T_7:
	cmp	word [MBR+510], 0AA55h 
        jne	short T_6

	mov	si, MBR+(partition_table+ptFileSystemID)
T_8:
	call	validate_extended_dos_partition
	jnc	short T_10	
 
	add	si, 16
	cmp	si, MBR+partition_table+ptFileSystemID+64
	jb	short T_8
T_9:
	mov	si, TRDOS_fatp_notfound
	;call	print_msg
	;jmp	T_44
	jmp	T_40
T_10:
	; AL = EP type (05h or 0Fh)
	cmp	al, 0Fh
	jna	short T_11
	inc	byte [lba]
T_11: 
	mov	ax, [EP_Start]
	mov	dx, [EP_Start+2]
T_12:
	mov	[EP_Start_x], ax
	mov	[EP_Start_x+2], dx
T_13:
	mov	bx, bootsector
	call	read_hd_sector
	jnc	short T_14
	
	cmp	byte [ldd_count], 0
	ja	short T_18	
	jmp	short T_6
T_14:
	cmp	word [bootsector+510], 0AA55h 
        je	short T_15

	cmp	byte [ldd_count], 0
	ja	short T_18	
	jmp	short T_9 ; there is not a valid extd dos part.
T_15:
	mov	si, bootsector+(partition_table+ptFileSystemID)
	call	validate_dos_partition
	jnc	short T_16

	cmp	byte [ldd_count], 0
	ja	short T_18	

	mov	si, TrDOS_ep_ldd_defect ; not a logical dos drive
	jmp	T_40

T_16:
	inc	byte [ldd_count]
	sub	si, ptFileSystemID
	mov	ax, [EP_Start_x]
	mov	dx, [EP_Start_x+2]
	add	[si+ptStartSector], ax
	adc	[si+ptStartSector+2], dx

	mov	di, [lddt_ptr]
	mov	cx, 8
	rep 	movsw

	mov	cl, [si+ptFileSystemID]
	cmp	cl, 05h
	je	short T_17
	cmp	cl, 0Fh
	jne	short T_18 ; there is not a next logical dos drive	
	inc	byte [lba] ; LBA type disk r/w
T_17:
	cmp	byte [ldd_count], 3
	ja	short T_18  ; max. 4 logical dos drive
	mov	[lddt_ptr], di
	mov	ax, [EP_Start]
	mov	dx, [EP_Start+2]
	add	ax, [si+ptStartSector]
	adc	dx, [si+ptStartSector+2]
	jmp	T_12

T_18:
	mov	si, ldd_table
	call	print_msg

	xor	bh, bh

	mov	bl, 1
	call	fill_ldd_row
	mov	si, ldd_row
	call	print_msg
	mov	bl, 2 
	cmp	[ldd_count], bl  ; 2
	jb	short T_19 ; direct question (only one ldd)
	call	fill_ldd_row
	mov	si, ldd_row
	call	print_msg
	mov	bl, 3 
	cmp	[ldd_count], bl  ; 3
	jb	short T_19 ; select menu
	call	fill_ldd_row
	mov	si, ldd_row
	call	print_msg
	mov	bl, 4
	cmp	[ldd_count], bl  ; 4
	jb	short T_19 ; select menu
	call	fill_ldd_row
	mov	si, ldd_row
	call	print_msg

T_19:
	mov	si, ldd_dline  ; print bottom line
	call	print_msg	

	cmp	byte [ldd_count], 1
	ja	short T_20

	; the first logical dos partition in extended dos partition
	mov	bx, lddt  ; start of logical dos drives
			  ; (dos partitiona) table
	
	jmp	short T_25	; pass select menu
T_20:
	mov	al, [ldd_count]
	add	al, '0'
	mov	[ldd_select_pn], al ; last logical dos drive number

T_21:
	mov	si, ldd_select_msg
	call	print_msg
	
T_22:
	xor	ax, ax
	int	16h			; wait for keyboard command
	
	cmp	al, 27 ; ESC key
	je	T_44 ; CRLF and Exit

	cmp	al, 32 ; SPACE key (or control keys or CR key etc.)
	ja	short T_24
	je	short T_23

	cmp	al, 13 ; CR/ENTER key
	jne	short T_22  ; don't beep 

	; Beeper
T_23:	
	mov	al, 07h  ; beep
	mov	ah, 0Eh			
	mov	bx, 07h             
	int	10h
	
	jmp	short T_22

T_24:
	mov	ah, [ldd_count]
	add	ah, '0'
	cmp	al, '1'
	jb	short T_21
	cmp	al, ah
	ja	short T_21
	
	mov	[TrDos_ldn], al

	sub	al, '1'
	shl	al, 4 ; * 16
	mov	bl, al
	xor	bh, bh
	add	bx, lddt ; logical dos drive (partition) table
T_25:
	mov	[lddt_save], bx
	mov	ah, 1
	mov	al, [bx+ptFileSystemID]  ; Partition ID
	cmp	al, ah  ; 1 ; FAT12 file system
	jna	short T_26	
	inc	ah  ; 2
	cmp	al, 6	; FAT 16 file system (>32MB)
	je	short T_26
	jb	short T_26 ; 4 ; FAT16 file sytem (<=32MB)
	cmp	al, 0Eh
	je	short T_26 ; FAT16 LBA file sytem (>32MB)
	inc	ah ; 3
	; FAT32 CHS or FAT 32 LBA file system
T_26:
	mov	byte [fattype], ah
	mov	[fsID], al

	cmp	ah, 2
	je	short T_29 ; FAT16 BS (default offset addr)
	jb	short T_28
	; set format code pointer to FAT32 format code
	mov	word [trdos386fc], format_FAT32_fs
	; set FS type string
	mov	word [fattype_str],'32'	; 'FAT32'
	; ok.. read boot sector
	jmp	short T_29

T_27:
	; Partition size defect 
	; (less than the minimum number of sectors required)
	mov	si, TrDOS_psize_defect
	;call	print_msg
	;jmp	T_44
	jmp	T_40

T_28:
	; set format code pointer to FAT12 format code
	mov	word [trdos386fc], format_FAT12_fs
	mov	byte [fattype_str+1],'2' ; 'FAT12'

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; read primary dos partition's boot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_29:	
	;mov	byte [RetryCount], 5

	mov	si, [lddt_save] ; pt row for logical dos drive

	mov	ax, [si+ptStartSector]
	mov	dx, [si+ptStartSector+2]
	mov	[dosp_start], ax
	mov	[dosp_start+2], dx
	mov	cx, [si+ptSectors]
	mov	bx, [si+ptSectors+2]
	mov	[dosp_size], cx
	mov	[dosp_size+2], bx

	; check minimum partition size
	cmp	byte [fattype], 3 ;  FAT32 FS
	jnb	short T_31 ; yes
T_30:
	or	bx, bx
	jnz	short T_32

	cmp	cx, [csize] ; sectors per cylinder
	jnb	short T_32
	jmp	short T_27
T_31:
	cmp	bx, 1 ; >= 32MB ?
	ja	short T_32
	jb	short T_27

	cmp	cx, 0415h  ; must be >= 66581 sectors
	jb	short T_27
T_32:
	add	cx, ax
	adc	bx, dx
	jc	T_6

	cmp	bx, [CHS_limit+2]	
	mov	bx, bootsector
	ja	short T_34 ; LBA read/write
	jb	short T_33
	cmp	cx, [CHS_limit]
	ja	short T_34
T_33:
	; CHS read

	;mov	ax, [dosp_start]
	;mov	dx, [dosp_start+2]

	call	read_chs_sector
	jc	T_6
	jmp	short T_35
T_34:
	mov	byte [lba], 1 ; LBA r/w is required

	;mov	ax, [dosp_start]
	;mov	dx, [dosp_start+2]
	
	call	read_lba_sector
	jc	T_6	 
T_35:
	cmp	word [bootsector+510], 0AA55h
	jne	short T_38

	cmp	word [bootsector+bsBytesPerSec], 512
	jne	short T_38

	mov	byte [bootsector+bsMedia], 0F8h
	jne	short T_38

	cmp	byte [fattype], 2
	ja	short T_37

	cmp	byte [bootsector+bsBpbSignature], 29h
	jne	short T_38
	cmp	dword [bootsector+bsFileSysType], 'FAT1'
	jne	short T_38

	mov	al, [bootsector+bsFileSysType+4]
	cmp	al, '6'
	je	short T_36

	cmp	al, '2'
	jne	short T_38

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; format question (and warning msg)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_36:
	mov	si, TrDOS_Format_warning ; warning is required
	jmp	short T_39
T_37:
	cmp	byte [bootsector+BS_BootSig], 29h
	jne	short T_38
	cmp	dword [bootsector+BS_FilSysType], 'FAT3'
	jne	short T_38
	cmp	byte [bootsector+BS_FilSysType+4], '2'
	je	short T_36
T_38:
	mov	si, TrDOS_Do_you_want ; no need to warning
T_39:
	call	print_msg

	call	get_answer
	cmp	al, 'Y'
	je	short T_41

	mov	si, _no_str
T_40:
	call	print_msg

	jmp	short T_44
T_41:
	mov	si, _yes_str
	call	print_msg

	mov	si, TrDOS_PressKeyWhenReady
	call	print_msg
T_42:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'M'-40h		; Enter (OK) key
	je	short T_43		; write
	cmp	al, 'C'-40h
	je	short T_44		; no write (exit)
	cmp	al, 27
	je	short T_44
	jmp	short T_42

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; clear fat buffer and start formatting
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_43:
	mov	si, TrDOS_CRLF
	call	print_msg

	; Clear buffer in BSS 
	mov	di, HDFORMAT_FATBUFFER
	xor	ax, ax
	mov	cx, 256
	rep	stosw

	; Clear volume name field
	mov	di, StrVolumeName
	mov	cl, 12
	rep	stosb

	mov	dl, [fsID] ; Partition ID

	jmp	word [trdos386fc]

T_44:
	mov	si, TrDOS_CRLF
Exit:
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h
T_45:
	call	get_answer
	cmp	al, 'Y'
	je	short T_43
	jmp	short T_44

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; disk r/w error or disk not ready
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

T_46:
	mov	si, TrDOS_disk_NotReadyOrError
	;;call	print_msg
	;;jmp	short T_45
	;jmp	short print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; print message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_msg:
T_47:
	lodsb				; Load byte at DS:SI to AL
	and	al, al            
	jz	short T_48       
	mov	ah, 0Eh			
	mov	bx, 07h             
	int	10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
	jmp     short T_47         
T_48:
_NO_:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Yes/No
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get and set partition type for formatting
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

validate_extended_dos_partition:
	
	; INPUT:
	;   si = partition table entry offset + file system ID 
	; OUTPUT:
	;   cf = 0 -> al = extended DOS partition ID
	;			 (05h,0Fh)		
	;	      ;ah = 0
	;
	;	[EP_Start] is set
	;
	;   cf = 1 -> not an extended DOS partition	
	;
	; Modified registers: ax, cx

	;sub	ah, ah ; mov ah, 0

	mov 	al, [si]

	cmp	al, 05h	; Extended DOS partition (CHS)
	je	short VEP_clc
	
	cmp	al, 0Fh	; Extended DOS partition (CHS)
	je	short VEP_clc
VEP_stc:
	stc
	retn
VEP_clc:
	mov	cx, [si+ptStartSector-ptFileSystemID]
	mov	[EP_Start], cx
	mov	cx, [si+2+ptStartSector-ptFileSystemID]
	mov	[EP_Start+2], cx
	;mov	cx, [si+2+ptSectors-ptFileSystemID]
	;mov	[EP_Size+2], cx
	;mov	cx, [si+ptSectors-ptFileSystemID]
	;mov	[EP_Size], cx
	retn 

validate_dos_partition:
	
	; INPUT:
	;   si = partition table entry, partition ID offset
	; OUTPUT:
	;   cf = 0 -> al = primary DOS partition ID
	;			 (01h,04h,06h,0Bh,0Ch,0Eh)		
	;
	;   cf = 1 -> not a primary DOS partition	

	mov 	al, [si]

	cmp	al, 01h	; FAT12 partition
	jna	short V_2

	cmp 	al, 06h ; FAT16 CHS partition (>=32MB)
	ja	short V_1
	je	short V_2

	cmp	al, 04h	; FAT16 CHS partition (< 32MB)
	ja	short V_4
	retn
V_1:
	cmp	al, 0Ch	; FAT32 LBA partition
	je	short V_2
	ja	short V_3

	cmp	al, 0Bh	; FAT32 CHS partition 
V_2:
	retn 
V_3:
	cmp	al, 0Eh	; FAT16 LBA partition
	je	short V_2
V_4:
	stc
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; preparing text row for logical dos drive
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

fill_ldd_row:
	; bl = selected logical drive number 
	; bh = 0

	;xor	bh, bh
	
	mov	al, bl
	add	al, '0'
	mov	[ldd_row_dn], al
	dec	bl
	jz	short flddtr_0
	shl	bl, 4 ; * 16
flddtr_0:
	add	bx, lddt
	mov	al, [bx+ptFileSystemID]
	cmp 	al, 1				
	jne	short flddtr_1
	mov	word [ldd_row_fs], "12"
	jmp	short flddtr_4
flddtr_1:
	cmp	al, 06h		
	ja	short flddtr_2
flddtr_2:
	mov	word [ldd_row_fs], "16"
	jmp	short flddtr_4
flddtr_3:
	cmp	al, 0Eh
	jne	short flddtr_2
	; al = 0Bh or 0Ch
	mov	word [ldd_row_fs], "32"	
flddtr_4:
	mov	ax, [bx+ptSectors]
	mov	dx, [bx+ptSectors+2]	
	cmp	dx, 100h ; 8GB limit
	jb	short flddtr_6 ; display size as MB or KB
flddtr_5:
	mov	ax, dx
	shr	ax, 5 ; / 32
	; GB
	call	convert_to_decimal	
	mov	ax, 'GB'
	jmp	short flddtr_9
flddtr_6:
	or	dx, dx
	jnz	short flddtr_7 ; MB
	cmp	ax, 2048
	jnb	short flddtr_8 ; MB
	shr	ax, 1 ; / 2
	; KB
	call	convert_to_decimal
	; di points to unit location
	mov	ax, 'KB' 		
	jmp	short flddtr_9
flddtr_7:
	test	dl, 1Fh ; flat ? 
	jz	short flddtr_5
	shl	dx, 5  ; convert GB to MB
flddtr_8:
	shr	ax, 11 ; / 2048 
	or	ax, dx ; MB
	; MB
	call	convert_to_decimal	
	mov	ax, 'MB'
flddtr_9:
	stosw	; volume size unit (KB,MB,GB)
	 ;CRLF
	mov	ax, 0A0Dh ; al = 0Dh, ah = 0Ah
	stosw
	sub al, al ; 0
	stosb
	retn

convert_to_decimal:
	mov	bp, sp
	mov	cx, 10
cvd_loop1:
	xor	dx, dx
	div	cx
	push	dx
	or	ax, ax
	jnz	short cvd_loop1	
	mov	di, ldd_row_sz	
cvd_loop2:
	pop	ax
	add	al, '0'
	stosb
	cmp	sp, bp
	jne	short cvd_loop2

	; di points to unit location
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; disk read
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

read_hd_sector:
	cmp	byte [lba], 0
	ja	short  read_lba_sector

read_chs_sector:
	; Derived from 'proc_write_chs_sector' in HDFORMAT.ASM (30/07/2011)
	; (TRDOS v1, Singlix FS formatting utility)
	mov	byte [rw], 2 ; read
	jmp	short chs_rw

write_chs_sector:
	; Derived from 'proc_write_chs_sector' in HDFORMAT.ASM (30/07/2011)
	; (TRDOS v1, Singlix FS formatting utility)
	mov	byte [rw], 3 ; write
	;jmp	short chs_rw
chs_rw:
	push	si
        push	cx        
chs_rw_0:
	mov	di, 5                    
chs_rw_1:               
	push	dx	; Linear sector #
	push	ax	; DX_AX = Linear address (sectors)
	mov	cx, [sectors]
	push	bx

	call    div32	; 32 bit divide

	mov	cx, bx	; Sector (zero based)
	inc	cx	; To make it 1 based
	push	cx
	mov     cx, [heads]
	call	div32	; Convert track to head & cyl
	mov	dh, bl	; BX = Head (max. FFh)
	pop	cx	; AX=Cyl, DH=Head, CX=Sector
	pop     bx	; ES:BX = Buffer

	mov	dl, [drv]
	mov	ch, al                   
	ror	ah, 1	; Rotate right
	ror	ah, 1                   
	or	cl, ah                   
chs_rw_2:
	mov	ah, [rw] ; 02h = read, 03h = write
	mov	al, 01h
	int	13h	; BIOS Service func (ah) = 2/3
			; Read/Write disk sectors
			; AL-sec num CH-track CL-sec
			; DH-head DL-drive ES:BX-buffer
			; CF-flag AH-status AL-sectors written/read
			; If CF = 1 then AH = Error code (>0) 
        
	;mov	[error], ah
	jnc     short chs_rw_3
	dec	di                 
	jz	short chs_rw_3 
        
	xor	ah, ah                   
	;mov	dl, [drv]
	int	13h	; BIOS Service func (ah) = 0
			; Reset disk system
	jmp	short chs_rw_2

chs_rw_3:
	pop	ax
	pop	dx
	pop	cx
	pop	si	
	retn		; db 0C3h

read_lba_sector:
	; trhdboot.s (2020), hdformat.asm (2011)
	mov	byte [rw], 42h
	jmp	short lba_rw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; disk write
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_hd_sector:
	cmp	byte [lba], 0
	jna	short  write_chs_sector

write_lba_sector:
	; trhdboot.s (2020), hdformat.asm (2011)
	mov	byte [rw], 43h
	;jmp	short lba_rw
lba_rw:
	mov	di, 5
lba_rw_1:
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

	mov     si, sp
	mov     dl, [drv]
	xor	al, al	; verify off 
lba_rw_2:
	mov     ah, [rw] ; 42h = LBA read, 43h = LBA write
	;xor	al, al	; verify off 
	int     13h

	;mov	[error], ah
	jnc     short lba_rw_3

	dec	di                 
	jz	short lba_rw_3 
        
	xor	ah, ah                   
	;mov	dl, [drv]
	int	13h	; BIOS Service func (ah) = 0
			; Reset disk system

	;mov	word [si+2], 1 ; set r/w count to 1 again
	mov	byte [si+2], 1

	jmp	short lba_rw_2

lba_rw_3:
	;popa
	db	61h
	;popa
	db	61h
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; FAT32 FORMATTING
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; ((TRDOS 386 criter))
; Minimum size of FAT32 FS = 65525 + 512 + 512 + 32
; >= 66581 sectors (or >= 65525 data clusters)
	
format_FAT32_fs:
	;mov	ax, 000Ch ; db 0Ch, 00h ; 'or al, 0'
	;cmp	dl, al ; 0Ch
	;je	short FAT32_lba_format
	;mov	ax, 0C00Bh ; db 0Bh, 0C0h ; 'or ax, ax'
;FAT32_lba_format:
	; Put TRDOS 386 FAT32 partition magic word 
	; at offset 5Ah, in TRDOS386 FAT32 boot sector 0.
	mov	bp, TRDOS_FAT32_hd_bs
	lea	di, [bp+3]
	mov	si, bs_oem_name
	mov	cx, 4
	rep	movsw 
	;mov	[bp+5Ah], ax	; [loc_5A]
	mov	word [bp+5Ah], 0C00Bh
	mov	ax, [sectors]
	mov	[bp+18h], ax	; [BPB_SecPerTrk]
	mov	ax, [heads]
	mov	[bp+1Ah], ax	; [BPB_NumHeads]
	mov	ax, [dosp_start]
	mov	[bp+1Ch], ax	; [BPB_HiddSec]
	mov	ax, [dosp_start+2]
	mov	[bp+1Eh], ax	; [BPB_HiddSec+2]
	mov	ax, [dosp_size]
	mov	[bp+20h], ax	; [BPB_TotSec32]
	mov	dx, [dosp_size+2]
	mov	[bp+22h], dx	; [BPB_TotSec32+2]
	
	; Sectors per cluster calculation
	; (According to MS FAT32 FS specification.)
	mov	cl, 8  ; 8 sectors per cluster
	cmp	dx, 8  ; >= 532480 sectors
	ja	short FAT32_f_2 ; 8 sectors per cluster
	jb	short FAT32_f_1 ; 1 sector per cluster	
	cmp	ax, 2000h ; dx_ax = (8*65536)+8192
	jnb	short FAT32_f_2
FAT32_f_1:
	mov	cl, 1	; 1 sector per cluster		
FAT32_f_2:
	mov	[bp+0Dh], cl	 ; [BPB_SecPerClus]
	;mov	byte [bp+10h], 2 ; [BPB_NumFATs] 
	;mov	word [bp+0Eh], 32 ; [BPB_RsvdSecCnt] 

	; Calculating FAT size in sectors
	; (According to MS FAT32 FS Specification, 2000)

	; DX_AX = partition (volume) size in sectors
	sub	ax, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 32
	sbb	dx, 0
		; TmpVal1 = DiskSize - (BPB_ResvdSecCnt +
		;	     		RootDirsectors)
		; RootDirSectors = 0 (for FAT32 FS)
	mov	bx, cx ; ch = 0
	shl	bx, 8 ; * 256
	mov	cl, [bp+10h] ; [BPB_NumFATs] 
	add	bx, cx	
		; TmpVal2 = (256*BPB_SecPerClus)+BPB_NumFATs
	shr	bx, 1
		; TmpVal2 = TmpVal2/2
	mov	cx, bx
	dec	bx  ; TmpVal2-1
	add	ax, bx
	adc	dx, 0
	call	div32
		; FATSz = (TmpVal1+(TmpVal2-1))/TmpVal2
	; DX_AX = FAT size in sectors
	mov	[bp+24h], ax	; [BPB_FATSz32]
	mov	[bp+26h], dx	; [BPB_FATSz32+2]
	; * 2
	mov	bx, dx
	add	ax, ax
	adc	bx, dx
	; BX_AX = [BPB_NumFATs] * [BPB_FATSz32]
	mov	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 32
	add	cx, ax
	adc	bx, 0
	; BX_CX = [BPB_RsvdSecCnt]+[BPB_NumFATs]*[BPB_FATSz32]
	mov	ax, [bp+20h]	; [BPB_TotSec32]
	mov	dx, [bp+22h]	; [BPB_TotSec32+2]
	sub	ax, cx
	sbb	dx, bx
	mov	[data_start], cx
	mov	[data_start+2], bx
	; DX_AX = Data sectors
	mov	[data_sectors], ax
	mov	[data_sectors+2], dx
	mov	cl, [bp+0Dh]	 ; [BPB_SecPerClus]
	xor	ch, ch
	call	div32 ; DX_AX/CX
	; DX_AX = Count of clusters (rounded down)
	mov	[cluster_count], ax
	mov	[cluster_count+2], dx
		
	lea	di, [bp+71] ; [BS_VolLab]
	call	write_volume_name
	lea	si, [bp+67] ; [BS_VolID]
	call	write_volume_serial
	call	write_cluster_count

	call	write_formatting_msg
	mov	al, 0
	call	write_format_percent_x

	mov	ax, [bp+1Ch]	; [BPB_HiddSec]
	mov	dx, [bp+1Eh]	; [BPB_HiddSec+2]
	add	[data_start], ax
	adc	[data_start+2], dx
FAT32_f_3:
	; DX_AX = FAT32 Boot Sector address
	mov	bx, TRDOS_FAT32_hd_bs
	; ES:BX = Boot Sector 1 Buffer
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	mov	bx, HDFORMAT_FSINFO_BUFF
	; ES:BX = FS INFO Sector Buffer (= BS+1)
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent	
	add	ax, 1
	adc	dx, 0	
	mov	bx, TRDOS_FAT32_hd_bs + 512
	; ES:BX = Boot Sector 2 Buffer
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	mov	cx, 3
FAT32_f_4:
	push	cx
	add	ax, 1
	adc	dx, 0
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	pop	cx
	dec	cl
	jnz	short FAT32_f_4
	add	ax, 1
	adc	dx, 0
	mov	cx, [bp+1Ch]	; [BPB_HiddSec]
	mov	bx, [bp+1Eh]	; [BPB_HiddSec+2]
	add	cx, 12
	adc	bx, 0
	; write BACKUP sectors
	; (6,7,8 boot+fsi and 9,10,11 empty sectors) 
	cmp	dx, bx
	jb	short FAT32_f_3
	cmp	ax, cx
	jb	short FAT32_f_3
	; write remain part of reserved sectors
	mov	cx, [bp+0Eh]	; [BPB_RsvdSecCnt]
	sub	cx, 12
	jna	short FAT32_f_6
FAT32_f_5:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cx
	jnz	short FAT32_f_5
FAT32_f_6:
	; write FAT sectors
	mov	cx, [data_start] ; lba/abs addr
	mov	bx, [data_start+2] ; lba/abs addr
	push	bx
	push	cx
	mov	bx, HDFORMAT_FATBUFFER
	; ES:BX = FAT Sector Buffer
	mov	cl, [bp+15h] ; [BPB_Media]
	mov	ch, 0FFh
	mov	[bx], cx
	mov	cl, ch ; cx = 0FFFFh
	mov	[bx+2], cx
	mov	[bx+4], cx
	mov	[bx+6], cx
	; Root dir cluster number = 2
	; 0FFFFFFFh = end of cluster chain 
	mov	[bx+8], cx  ; 0FFFFh
	and	ch, 0Fh
	mov	[bx+10], cx ; 0FFFh
	;inc	cx
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	;mov	bx, HDFORMAT_FATBUFFER
	mov	cx, 0
	mov	[bx], cx
	mov	[bx+2], cx
	mov	[bx+4], cx
	mov	[bx+6], cx
	mov	[bx+8], cx
	mov	[bx+10], cx
	jmp	short FAT32_f_8
FAT32_f_7:	
	push	bx
	push	cx	
	mov	bx, HDFORMAT_FATBUFFER
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
FAT32_f_8:	
	pop	cx
	pop	bx
	add	ax, 1
	adc	dx, 0
	cmp	dx, bx
	jb	short FAT32_f_7
	cmp	ax, cx
	jb	short FAT32_f_7

	; write	root directory (1st cluster)
	; as empty sectors
	mov	cl, [bp+0Dh]	 ; [BPB_SecPerClus]
	xor	ch, ch
	sub	[data_sectors], cx
	sbb	word [data_sectors+2], 0
FAT32_f_9:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cl
	jnz	short FAT32_f_9

	; write DATA sectors 
	; (after root directory 1st cluster)
	mov	cx, [data_sectors]
	mov	bx, [data_sectors+2] 
			; NOTE: Partition size must be >= 512 MB
			;	for FAT32 FS  ((BX >= 15))		
FAT32_f_10:	
	push	bx
	push	cx	
	mov	bx, HDFORMAT_SECBUFFER
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	pop	cx
	pop	bx
	add	ax, 1
	adc	dx, 0
	dec	cx
	jnz	short FAT32_f_10
	dec	bx
	jnz	short FAT32_f_10

	; If there are, format remain sectors which are
	; at beyond of data clusters, with zero bytes.
	
	mov	cx, [bp+1Ch]	; [BPB_HiddSec]
	mov	bx, [bp+1Eh]	; [BPB_HiddSec+2]
FAT16_f_18:	
	add	cx, [bp+20h]	; [BPB_TotSec32]
	adc	bx, [bp+22h]	; [BPB_TotSec32+2]
FAT16_f_19:
FAT12_f_8:
	; are there remain sectors (in partition) ?
	sub	cx, ax
	sbb	bx, dx
	; 11/02/2019
	; BX must be 0 (Because, 1 cluster <= 32KB. So, 
	;	        remain sectors must not be more than 32K)
	jnz	short FAT32_f_12 ; There is a wrong thing !!!
				 ; If BX is not zero,	
				 ; it is better to skip this stage...)
	or	cx, cx		
	jz	short FAT32_f_12 ; no.. 
				 ; (good! FAT contains all data sectors)
FAT32_f_11:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	pop	cx
	add	ax, 1
	adc	dx, 0
	dec	cx
	jnz	short FAT32_f_11

FAT32_f_12:
	; End of FAT format routine...
end_of_formatting:
	mov	al, 100
	call	write_format_percent_x
	;mov	si, CRLF
	;call	print_msg
	mov	si, _msg_OK
	;call	print_msg
	jmp	Exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set & write volume name
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_fs_volume_name:
	mov	byte [vname_length], 64
	jmp	short svn_fs

write_volume_name:
	mov	byte [vname_length], 11
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
	jc	short svn_1
svn_0:
	lodsb
	cmp	al, 20h
	ja	short svn_2
	je	short svn_0 
svn_1:
	mov	si, no_name
	lodsb
svn_2:
	;mov	di, [bp+47h) ; [BS_VolLab] ; FAT32
	;mov	di, [bp+2Bh) ; [BS_VolLab] ; FAT16 (&FAT12)
	mov	bx, di ; *
	xor	ch, ch
	mov	cl, [vname_length] ; 11
	jmp	short svn_4
svn_3:
	lodsb
	cmp	al, 20h
	jb	short svn_6
svn_4:
	stosb
	loop	svn_3
svn_5:
	mov	cl, [vname_length] ; 11
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
	call	print_msg
	retn
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
	; (Ref: vitaly_filatov.tripod.com/ng/asm/asT_3029.1.html)
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
	call	print_msg

	retn

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
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write formatting percentage
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_formatting_msg:
	mov	ax, [dosp_size]
	mov	dx, [dosp_size+2]

	; DX_AX = Total sectors for percentage  
	mov	cx, 100	
	call	div32
	mov	[format_percent], ax

	mov	si, msg_formatting
	call	print_msg

	; get cursor position
	; bh = 0  ; video page
	mov     ah, 3 ; get cursor pos
	int     10h
	mov	[Cursor_Pos], dx

	mov	byte [prev_percent], 255

	retn

write_format_percent:
	; DX_AX = Current sector (which has been written)

	push	ax
	push	dx
	push	bx
	push	cx
	push	si

	sub	ax, [bp+1Ch]	; [BPB_HiddSec]
	sbb	dx, [bp+1Eh]	; [BPB_HiddSec+2]
wpc_t:
	mov	cx, [format_percent]
	call	div32
	; AL = percentage value between 1 to 100
wpc_x:
	cmp	al, [prev_percent]
	je	short wpc_y
	mov	[prev_percent], al
	mov	dx, [Cursor_Pos]
	mov	bx, 7
	mov	ah, 2
	int	10h  ; Set Cursor Position
	xor	dx, dx
	xor	ah, ah
	;mov	al, [prev_percent]
	mov	si, format_percent_str + 2
	call	bin_to_decimal
	call	print_msg
wpc_y:
	pop	si
	pop	cx
	pop	bx
	pop	dx
	pop	ax
	retn

write_format_percent_x:
	; AL = % number

	push	ax
	push	dx
	push	bx
	push	cx
	push	si

	jmp	short wpc_x

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; format error 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

formatting_error:
	mov	sp, [old_sp]

	mov	al, ah ;  error code
	call	bin_to_hex
	mov 	[error_code], ax

	mov	si, CRLF
	call	print_msg

	mov	si, Msg_Error
	;call	print_msg
	jmp	Exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write cluster count
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_cluster_count:
	mov	si, msg_cluster_count
	call	print_msg
	mov	ax, [cluster_count]
	mov	dx, [cluster_count+2]
	mov	si, cluster_count_str+6
	call	bin_to_decimal
	call	print_msg
	retn 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; FAT16 FORMATTING
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; ((TRDOS 386 criter))
; Minimum size of FAT16 FS = [heads]*[sectors]
; (1 cylinder) or 4096 sectors (for TRDOS 386)

format_FAT16_fs:
	mov	ax, 0706h ; db 06h, 07h ; 'push es, pop es'
	cmp	dl, al ; 06h ; Big CHS partition (>= 32MB)
	je	short FAT16_big_chs_format
	;mov	ax, 070Eh ; db 0Eh, 07h	; 'push cs, pop es'
	;cmp	dl, al ; 0Eh ; LBA partition
	;je	short FAT16_lba_format
FAT16_chs_format:  
	; Partition Type: 04h, CHS (<32 MB) partition
	mov	ax, 0004h ; db 04h, 00h ; 'add al, 0'
FAT16_big_chs_format:
;FAT16_lba_format:
	; Put TRDOS 386 FAT16 partition magic word 
	; at offset 3Eh, in TRDOS386 FAT16 boot sector.
	mov	bp, TRDOS_FAT16_hd_bs
	lea	di, [bp+3]
	mov	si, bs_oem_name
	mov	cx, 4
	rep	movsw 
	mov	[bp+3Eh], ax	; [loc_3E]

	mov	ax, [sectors]
	mov	[bp+18h], ax	; [BPB_SecPerTrk]
	mov	ax, [heads]
	mov	[bp+1Ah], ax	; [BPB_NumHeads]
	mov	ax, [dosp_start]
	mov	[bp+1Ch], ax	; [BPB_HiddSec]
	mov	ax, [dosp_start+2]
	mov	[bp+1Eh], ax	; [BPB_HiddSec+2]
	mov	ax, [dosp_size]
	mov	dx, [dosp_size+2]
	and	dx, dx
	jnz	short FAT16_f_0
	mov	[bp+13h], ax	; [BPB_TotSec16]
	; CX = 0
	;mov	[bp+20h], cx	; [BPB_TotSec32] =  0
	;mov	[bp+22h], cx	; [BPB_TotSec32+2] = 0
	jmp	short FAT16_f_1
FAT16_f_0:
	mov	[bp+20h], ax	; [BPB_TotSec32]
	mov	[bp+22h], dx	; [BPB_TotSec32+2]
	; CX = 0
	;mov	[bp+13h], cx ; [BPB_TotSec16] = 0
FAT16_f_1:
	; Sectors per cluster calculation
	; (According to MS FAT32 FS specification.)
	mov	cl, 2  ; 2 sectors per cluster
	or	dx, dx
	jnz	short FAT16_f_2 ; >2 sectors (>16MB)
	cmp	ax, 32680
	jna	short FAT16_f_10 ; 2 sectors, <=16MB
	; > 16MB
	jmp	short FAT16_f_9 ; 4 sectors per cluster
FAT16_f_2:
	cmp	dx, 4  ; >= 262144 sectors ; >=128MB
	ja	short FAT16_f_3 ; >4 sectors per cluster
	jb	short FAT16_f_9 ; 4 sectors per cluster	
	or	ax, ax ; dx_ax = (4*65536)+0
	jz	short FAT16_f_9 ; 4 sectors per cluster
	jmp	short FAT16_f_8 ; 8 sectors per cluster
FAT16_f_3:
	cmp	dx, 8  ; >= 524288 sectors ; >=256MB
	ja	short FAT16_f_4 ; >8 sectors per cluster
	jb	short FAT16_f_8 ; 8 sectors per cluster	
	and	ax, ax ; dx_ax = (8*65536)+0
	jz	short FAT16_f_8 ; 8 sectors per cluster
	jmp	short FAT16_f_7 ; 16 sectors per cluster
FAT16_f_4:
	cmp	dx, 16 ; >= 1048576 sectors ; >=512MB
	ja	short FAT16_f_5 ; >16 sectors per cluster
	jb	short FAT16_f_7 ; 16 sectors per cluster	
	and	ax, ax ; dx_ax = (16*65536)+0
	jz	short FAT16_f_7 ; 16 sectors per cluster
	jmp	short FAT16_f_6 ; 32 sectors per cluster
FAT16_f_5:
	cmp	dx, 32 ; >= 2097152 sectors ; >=1GB
	jb	short FAT16_f_6 ; 32 sectors per cluster
	or	ax, ax		; dx_ax = (32*65536)+0
	jz	short FAT16_f_6 ; 32 sectors per cluster
	; >1GB (<=2GB)
	; 64 sectors per cluster
	shl	cl, 1
FAT16_f_6:
	; 32 sectors per cluster (for <= 2GB volumes)
	shl	cl, 1	
FAT16_f_7:
	; 16 sectors per cluster (for <= 1GB volumes)
	shl	cl, 1
FAT16_f_8:
	; 8 sectors per cluster (for <= 512MB volumes)
	shl	cl, 1	
FAT16_f_9:
	; 4 sectors per cluster (for <= 256MB volumes)
	shl	cl, 1	
FAT16_f_10:	
	; 2 sectors per cluster (for <= 128MB volumes)
	mov	[bp+0Dh], cl	 ; [BPB_SecPerClus]
	;mov	byte [bp+10h], 2 ; [BPB_NumFATs] 
	;mov	word [bp+0Eh], 1 ; [BPB_RsvdSecCnt] 
	;mov	word [bp+11h], 512 ; [BPB_RootEntCnt]
	
	; Calculating FAT size in sectors
	; (According to MS FAT32 FS Specification, 2000)

	; DX_AX = partition (volume) size in sectors
	mov	bx, [bp+11h]	; [BPB_RootEntCnt] = 512
	add	bx, 15 ; bx = 527
	shr	bx, 4 ; /16 = 527/16 = 32
		; ((32*BX)+511)/512
	mov	[root_dir_secs], bx
	add	bx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
	sub	ax, bx
	sbb	dx, 0
		; TmpVal1 = DiskSize - (BPB_ResvdSecCnt +
		;	     		RootDirsectors)
	;mov	bx, cx ; ch = 0
	;shl	bx, 8 ; * 256
	mov	bh, cl
	xor	bl, bl
	mov	cl, 2 ; [BPB_NumFATs] 
	add	bx, cx	
		; TmpVal2 = (256*BPB_SecPerClus)+BPB_NumFATs
	mov	cx, bx
	dec	bx  ; TmpVal2-1
	add	ax, bx
	adc	dx, 0
	call	div32
		; FATSz = (TmpVal1+(TmpVal2-1))/TmpVal2
	; AX = FAT size in sectors
	; DX = 0
	mov	[bp+16h], ax	; [BPB_FATSz16]
	; * 2
	shl	ax, 1
	; AX = [BPB_NumFATs] * [BPB_FATSz16]
	mov	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
	add	cx, ax
	; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	add	cx, [root_dir_secs] ; + RootDirsectors
	sub	bx, bx ; BX = 0
	; BX_CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	;	  + RootDirSectors
	mov	ax, [bp+13h]	; [BPB_TotSec16]
	;sub	dx, dx 
	; DX = 0
	and	ax, ax
	jnz	short FAT16_f_11
	mov	ax, [bp+20h]	; [BPB_TotSec32]
	mov	dx, [bp+22h]	; [BPB_TotSec32+2]
FAT16_f_11:
	sub	ax, cx
	sbb	dx, bx
	mov	[data_start], cx
	mov	[data_start+2], bx
	; DX_AX = Data sectors
	mov	[data_sectors], ax
	mov	[data_sectors+2], dx
	mov	cl, [bp+0Dh]	 ; [BPB_SecPerClus]
	xor	ch, ch
	call	div32 ; DX_AX/CX
	; AX = Count of clusters (rounded down)
	; DX = 0
	mov	[cluster_count], ax
	mov	[cluster_count+2], dx

	lea	di, [bp+43] ; [BS_VolLab]
	call	write_volume_name
	lea	si, [bp+39] ; [BS_VolID]
	call	write_volume_serial
	call	write_cluster_count	

	call	write_formatting_msg
	mov	al, 0
	call	write_format_percent_x

	mov	ax, [bp+1Ch]	; [BPB_HiddSec]
	mov	dx, [bp+1Eh]	; [BPB_HiddSec+2]

	add	[data_start], ax
	adc	[data_start+2], dx

	; DX_AX = FAT16 Boot Sector address
	mov	bx, TRDOS_FAT16_hd_bs
	; ES:BX = Boot Sector Buffer
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	; write remain part of reserved sectors
	mov	cx, [bp+0Eh] ; [BPB_RsvdSecCnt]
	;sub	cx, 1
	;jna	short FAT16_f_13
	dec	cx
	jz	short FAT16_f_13
FAT16_f_12:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cx ; dec cl
	jnz	short FAT16_f_12
FAT16_f_13:
	; write FAT sectors
	mov	cx, [data_start] ; lba/abs addr
	mov	bx, [data_start+2] ; lba/abs addr

	sub	cx, [root_dir_secs]
	sbb	bx, 0

	push	bx
	push	cx
	mov	bx, HDFORMAT_FATBUFFER
	; ES:BX = FAT Sector Buffer
	mov	cl, [bp+15h] ; [BPB_Media]
	mov	ch, 0FFh
	mov	[bx], cx ; 0FFF8h
	mov	cl, ch ; cx = 0FFFFh
	mov	[bx+2], cx
	;inc	cx
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	;mov	bx, HDFORMAT_FATBUFFER
	mov	cx, 0
	mov	[bx], cx
	mov	[bx+2], cx
	jmp	short FAT16_f_15
FAT16_f_14:	
	push	bx
	push	cx	
	mov	bx, HDFORMAT_FATBUFFER
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
FAT16_f_15:	
	pop	cx
	pop	bx
	add	ax, 1
	adc	dx, 0
	cmp	dx, bx
	jb	short FAT16_f_14
	cmp	ax, cx
	jb	short FAT16_f_14

	; write	root directory sectors
	; as empty sectors
	mov	cx, [root_dir_secs]
FAT16_f_16:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cx
	jnz	short FAT16_f_16	

	; write DATA sectors 
	; (after root directory sectors)
	mov	cx, [data_sectors]
	mov	bx, [data_sectors+2]
	inc	bx ; 0 -> 1, 1-> 2
FAT16_f_17:	
	push	bx
	push	cx	
	mov	bx, HDFORMAT_SECBUFFER
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	pop	cx
	pop	bx
	add	ax, 1
	adc	dx, 0
	dec	cx
	jnz	short FAT16_f_17
	dec	bx
	jnz	short FAT16_f_17

	; If there are, format remain sectors which are
	; at beyond of data clusters, with zero bytes.
	
	mov	cx, [bp+1Ch]	; [BPB_HiddSec]
	mov	bx, [bp+1Eh]	; [BPB_HiddSec+2]

	cmp	word [bp+13h], 0 ; [BPB_TotSec16]
	jz	FAT16_f_18
	add	cx, [bp+13h]	; [BPB_TotSec16]
	adc	bx, 0
	jmp	FAT16_f_19

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; FAT12 FORMATTING
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; ((TRDOS 386 criter))
; Minimum size of FAT12 FS = [heads]*[sectors]
; (1 cylinder) 

format_FAT12_fs:
	mov	bp, TRDOS_FAT12_hd_bs
	lea	di, [bp+3]
	mov	si, bs_oem_name
	mov	cx, 4
	rep	movsw 
	mov	ax, [sectors]
	mov	[bp+18h], ax	; [BPB_SecPerTrk]
	mov	ax, [heads]
	mov	[bp+1Ah], ax	; [BPB_NumHeads]
	mov	ax, [dosp_start]
	mov	[bp+1Ch], ax	; [BPB_HiddSec]
	mov	ax, [dosp_start+2]
	mov	[bp+1Eh], ax	; [BPB_HiddSec+2]
	mov	ax, [dosp_size]
	mov	[bp+13h], ax	; [BPB_TotSec16]

	xor	si, si ; reset (FAT size fix) flag
	mov	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
	mov	dx, [bp+11h]	; [BPB_RootEntCnt] = 512
	add	dx, 15	; (16-1) (512-1)
	shr	dx, 4	; /16  (*32/512)
	; AX = Root dir sectors
	; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	add	cx, dx ; + RootDirsectors ; + 32
	mov	[root_dir_secs], cx ; = 33

	;sub	ax, 33  ; 1 reserved sector, 32 root dir sectors
			; .. now AX has number of data sectors
			;	 		+ 2* (FAT sectors)
	sub	ax, cx	
FAT12_f_10:
	; Sectors per cluster calculation
	; (According to MS FAT32 FS specification.)
	;mov	cx, 1  ; 1 sector per cluster
	mov	cl, 1  ; CH = 0
	; 28/10/2023 ; (BugFix)
	push	ax
FAT12_f_0:
	cmp	ax, 4085 ; Max. cluster count for FAT12
	jb	short FAT12_f_1
	shl	cl, 1 ; *2
	shr	ax, 1 ; /2
	jmp	short FAT12_f_0
FAT12_f_1:
	; 28/10/2023
	pop	ax
	mov	[bp+0Dh], cl	 ; [BPB_SecPerClus]
	;mov	byte [bp+10h], 2 ; [BPB_NumFATs] 
	;mov	word [bp+0Eh], 1 ; [BPB_RsvdSecCnt] 
	;mov	word [bp+11h], 512 ; [BPB_RootEntCnt]
	
	; Calculating FAT size in sectors
	; AX = partition (volume, data) size in sectors
	; CX = sectors per clusters
	xor	dx, dx
	div	cx
	; AX = cluster count (only for FAT size calc)
	; DX = 0
	add	ax, 2  ; cluster 2 to ...
	mov	dx, ax
	shl	dx, 1
	add	ax, dx ; *3
	shr	ax, 1  ; /2
	adc	ax, 0  ; +0.5 -> +1

	; AX = FAT bytes for 12 bit cluster numbers
	
	mov	cx, 512		; [BPB_BytesPerSec]
	add	ax, cx		
	dec	ax		; [BPB_BytesPerSec] - 1
	sub	dx, dx
	div	cx
	mov	[bp+16h], ax	; [BPB_FATSz16]
	; * 2
	shl	ax, 1
	; AX = [BPB_NumFATs] * [BPB_FATSz16]

	;mov	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
	;add	cx, ax
	;mov	ax, [bp+11h]	; [BPB_RootEntCnt] = 512
	;add	ax, 15	; (16-1) (512-1)
	;shr	ax, 4	; /16  (*32/512)
	;; AX = Root dir sectors
	;; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	;add	cx, ax ; + RootDirsectors
	;mov	[root_dir_secs], ax

	;mov	cx, 33
	mov	cx, [root_dir_secs]
	add	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
		; cx = root directory sectors + reserved sectors
	add	cx, ax
	; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	;	  + RootDirSectors
	mov	ax, [bp+13h]	; [BPB_TotSec16]
	sub	ax, cx
		 ; AX = data sectors
		 ; cH = 0

	; fix FAT size (better method)
	or	si, si
	jnz	short FAT12_f_9

	mov	si, ax  ; ax = data sectors
	jmp	short FAT12_f_10

FAT12_f_9:
	xor	dx, dx
	mov	[data_start], cx
	mov	[data_start+2], dx ; 0
	; DX_AX = Data sectors
	mov	[data_sectors], ax
	mov	[data_sectors+2], dx ; 0
	mov	cl, [bp+0Dh]	 ; [BPB_SecPerClus]
	sub	ch, ch
	div	cx
	; AX = Count of clusters (rounded down)
	sub	dx, dx ; 0
	mov	[cluster_count], ax
	mov	[cluster_count+2], dx ; 0

	lea	di, [bp+43] ; [BS_VolLab]
	call	write_volume_name
	lea	si, [bp+39] ; [BS_VolID]
	call	write_volume_serial
	call	write_cluster_count	

	call	write_formatting_msg
	mov	al, 0
	call	write_format_percent_x

	mov	ax, [bp+1Ch]	; [BPB_HiddSec]
	mov	dx, [bp+1Eh]	; [BPB_HiddSec+2]

	add	[data_start], ax
	adc	[data_start+2], dx

	; DX_AX = FAT12 Boot Sector address
	mov	bx, TRDOS_FAT12_hd_bs
	; ES:BX = Boot Sector Buffer
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	; write remain part of reserved sectors
	mov	cx, [bp+0Eh] ; [BPB_RsvdSecCnt]
	;sub	cx, 1
	;jna	short FAT12_f_3
	dec	cx
	jz	short FAT12_f_3
FAT12_f_2:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cx ; dec cl
	jnz	short FAT12_f_2
FAT12_f_3:
	; write FAT sectors
	mov	cx, [data_start] ; lba/abs addr
	mov	bx, [data_start+2] ; lba/abs addr

	sub	cx, [root_dir_secs]
	sbb	bx, 0

	push	bx
	push	cx
	mov	bx, HDFORMAT_FATBUFFER
	; ES:BX = FAT Sector Buffer
	mov	cl, [bp+15h] ; [BPB_Media]
	mov	ch, 0FFh
	mov	[bx], cx ; 0FFF8h
	mov	[bx+2], ch ; 0FFFFF8h
	;xor	cx, cx
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	;mov	bx, HDFORMAT_FATBUFFER
	mov	cx, 0
	mov	[bx], cx
	mov	[bx+2], cl
	jmp	short FAT12_f_5
FAT12_f_4:	
	push	bx
	push	cx	
	mov	bx, HDFORMAT_FATBUFFER
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
FAT12_f_5:	
	pop	cx
	pop	bx
	add	ax, 1
	adc	dx, 0
	cmp	dx, bx
	jb	short FAT12_f_4
	cmp	ax, cx
	jb	short FAT12_f_4

	; write	root directory sectors
	; as empty sectors
	mov	cx, [root_dir_secs]
FAT12_f_6:
	push	cx
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cx ; dec cl
	jnz	short FAT12_f_6

	; write DATA sectors 
	; (after root directory sectors)
	mov	cx, [data_sectors]
	;mov	bx, [data_sectors+2]
	;inc	bx
FAT12_f_7:	
	;push	bx
	push	cx	
	mov	bx, HDFORMAT_SECBUFFER
	call	write_hd_sector
	jc	formatting_error
	call	write_format_percent
	pop	cx
	;pop	bx
	add	ax, 1
	adc	dx, 0
	dec	cx
	jnz	short FAT12_f_7
	;dec	bx
	;jnz	short FAT12_f_7

	; If there are, format remain sectors which are
	; at beyond of data clusters, with zero bytes.
	
	mov	cx, [bp+1Ch]	; [BPB_HiddSec]
	mov	bx, [bp+1Eh]	; [BPB_HiddSec+2]

	add	cx, [bp+13h]	; [BPB_TotSec16]
	adc	bx, 0
	jmp	FAT12_f_8

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
	cmp     ah, [vname_length]
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
; 32 bit division
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

div32:
	; DX_AX/CX
	; Result: DX_AX, BX (remainder) 
	mov	bx, ax
	;or	dx, ax ; * DX_AX = 0 ?       
	;jz	short div32_retn ; yes, do not divide! 
	mov	ax, dx
        xor	dx, dx
        div	cx	; at first, divide DX
			; remainder is in DX 
	xchg	ax, bx	; now quotient is in BX
  			; and initial AX value is in AX
	div	cx	; now, DX_AX has been divided and
			; AX has quotient
			; DX has remainder
	xchg	dx, bx	; finally, BX has remainder
;div32_retn:
        retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Convert byte to decimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bin_to_decimal:
	; INPUT: DS:SI = Target location
	;        DX_AX = Binary Number (Integer)
	; OUTPUT: Decimal char at DS:SI
	; 	 SI decremented after every division
	; 	 till AX<10.
	; CX, DX, BX will be changed.
	;
	mov	cx, 10
btd_0:
	; DX_AX = Dividend
	; CX = Divisor
	call	div32
	; DX_AX = Quotient
	; BX = remainder
	add	bl, '0'
	mov	[si], bl
	and	dx, dx
	jz	short btd_2
btd_1:
	dec	si
	jmp	short btd_0
btd_2:
	or	ax, ax
	jnz	short btd_1

	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Convert byte to hexadecimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

byte_to_hex:
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

; ----------------------------------------------------------------------------
; initialized data
; ----------------------------------------------------------------------------

align 2

trdos386fc:
	dw format_FAT16_fs
	dw 0

;volume_id:
;	dd 0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  FAT boot sector code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TRDOS_FAT32_hd_bs:
	incbin	'FAT32_BS.BIN'
TRDOS_FAT16_hd_bs: 
	incbin	'FAT16_BS.BIN'
TRDOS_FAT12_hd_bs: 
	incbin	'FAT12_BS.BIN'

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lba:	db 0

drv:	db 0

sectors: db 0
	 db 0
heads:	 db 0
	 db 0
cylinders: dw 0

TrDOS_Welcome:
	db 0Dh, 0Ah
	db 'TR-DOS 386 v2 Hard Disk Partition Formatting Utility '
	db 0Dh, 0Ah
	db '(for logical dos drives in extended dos partitions)	'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db '(c) Erdogan TAN 2020-2023'
	db 0Dh,0Ah
	db 0Dh,0Ah
	db 'Usage: epformat <drive> '
	db 0Dh,0Ah, 0Dh, 0Ah
	db 'Drive names: '
	db 0Dh, 0Ah
	db '  hd0 ..for extended dos partition on 1st disk '
	db 0Dh, 0Ah
	db '  hd1 ..for extended dos partition on 2nd disk '
	db 0Dh, 0Ah
	db '  hd2 ..for extended dos partition on 3rd disk '
	db 0Dh, 0Ah
	db '  hd3 ..for extended dos partition on 4th disk '
	db 0Dh, 0Ah, 0

	db '25/09/2020'
	db 0

TrDOS_Format_warning:
	db 0Dh, 0Ah
	db "WARNING ! ", 0Dh, 0Ah 
	db "(If you say 'Yes', all of data in the logical DOS drive will be lost !) "
TrDOS_Do_you_want:
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Do you want to format logical DOS drive as TRDOS 386 FAT" 
fattype_str:
	db "16 fs ? (Y/N) "
	db 0

_yes_str:
	db 'YES '
	db 0Dh, 0Ah, 0
_no_str:
	db 'NO '
	db 0Dh, 0Ah, 0

_msg_OK:
	;db	07h
	db	0Dh, 0Ah
	db	"OK."
TrDOS_CRLF:
	db	0Dh, 0Ah, 0

TrDOS_PressKeyWhenReady:
	db 0Dh, 0Ah
	db 'Press Enter to format logical DOS drive '
TrDos_ldn:
	db '1 on hd'
TrDOS_Drive:
	db '?. ', 0

TrDOS_disk_NotReadyOrError:
	db 0Dh, 0Ah
	db 'Disk error or drive not ready ! '
zbyte:	db 'Try again ? (Y/N) '
	db 0

TrDOS_psize_defect:
	db 0Dh, 0Ah
	db 'MBR partition size defect ! '
	db 0Dh, 0Ah
	db '(less than the minimum number of sectors required) '	
	db 0

TrDOS_ep_ldd_defect:
	db 0Dh, 0Ah
	db 'There is not a logical DOS drive ! '
	db 0Dh, 0Ah
ldd_count:
	db 0

TRDOS_fatp_notfound:
	db 0Dh, 0Ah
	db 'MBR does not contain an extended DOS partition ! '	
fattype:
	db 0
;RetryCount:
;	db 4

;error: db 0

;align 2
align 4

hexchrs:
	db	'0123456789ABCDEF'

Cursor_Pos: ; dw 0
CHS_limit:  ; dword	 
	dw	0
	;dw	0

sign:	dw	417	; magic word

ldd_table:
	db	0Dh, 0Ah
	db	"=======================================", 0Dh, 0Ah
	db	"      HD"
drv_str:
	db	"0 EXTENDED DOS PARTITION       ", 0Dh, 0Ah
	db	"=======================================", 0Dh, 0Ah
	db	" Logical DOS Drive   Type      SIZE    ", 0Dh, 0Ah
	db	"---------------------------------------", 0Dh, 0Ah, 0
ldd_row:
	db	"                 "
ldd_row_dn:
	db	"1   FAT"
ldd_row_fs:
	db	"12     "
ldd_row_sz:
	db	"128GB   ", 0Dh, 0Ah, 0
ldd_dline:
	db	"=======================================", 0Dh, 0Ah	
	db	0

ldd_select_msg:
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"Select logical DOS drive number (1 to " 
ldd_select_pn:
	db	"4) to format. " 	
	db 	0Dh, 0Ah
	db	"(or press ESC to cancel) ", 0Dh, 0Ah, 0

;align 4 

msg_sectors_crlf:
	db	" sector"
msg_sectors_crlf_s:
	db	"s"
	db	0Dh, 0Ah, 0

vname_length:
	db	0

bs_oem_name:
	db	'TRDOS2.0', 0

align 2

no_name:
	db 	'NO NAME    ', 0

Msg_Volume_Name:
	db	0Dh, 0Ah
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

msg_cluster_count:
	db	"Cluster Count: ", 0
cluster_count_str:
	db	"0000000"
	db	0Dh, 0Ah, 0
msg_formatting:
	db	"Formatting ", 0
format_percent_str:
	db	"000%"
	db	0					

Msg_3dot_OK:
	db	'...'
Msg_OK:
	db	' OK.'
CRLF:
	db	0Dh, 0Ah, 0

Msg_Error:
	db	0Dh, 0Ah
	db	'Error ! '
	db	'('
error_code:
	dw	3030h
	db	'h'
	db	') '
	db	0Dh, 0Ah
	db	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  initialized buffers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

HDFORMAT_SECBUFFER:
	times	512 db 0F6h
HDFORMAT_FSINFO_BUFF:
	dd	41615252h  ; FSI_LeadSig
	times	480 db 0   ; FSI_Reserved1
	dd	61417272h  ; FSI_StrucSig
	dd	0FFFFFFFFh ; FSI_Free_Count
	dd	000000002h ; FSI_Nxt_Free
	times	12 db 0	   ; FSI_Reserved2
	dd	0AA550000h ; FSI_TrailSig

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lddt_ptr dw lddt	

;SizeOfFile equ $-100	  	

; ----------------------------------------------------------------------------
; uninitialized data
; ----------------------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

alignb 4

fsID:	resb 1
rw:	resb 1
csize:	resw 1 ; heads*spt (sectors per cylinder)

dosp_start: resd 1 ; start sector of the (primary) dos partition
dosp_size:  resd 1 ; partition size in sectors	 

MBR:
bootsector:
; 	resb 512
HDFORMAT_FATBUFFER:
HDFORMAT_EMPTY_BUFF:
	resb 512

;HDFORMAT_FATBUFFER:
;HDFORMAT_EMPTY_BUFF:
;	resb 512

; logical dos drives table
lddt:	resb 4*16 ; 64 bytes	

EP_Start:	resd 1
EP_Start_x:	resd 1

data_start:	resd 1
data_sectors:	resd 1
cluster_count:	resd 1
root_dir_secs:	resw 1
format_percent: resw 1
prev_percent:	resb 1
rsvdbyte:	resb 1

old_sp:		resw 1

StrVolumeName:	resb 12

lddt_save:	resw 1

end_bss: