; ****************************************************************************
; FDISK3R5.ASM (FDISK3R5.COM) - Retro DOS v5 Hard Disk Partitioning Utility
; fdisk3.s						(for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 15/07/2024
; ----------------------------------------------------------------------------
; Beginning: 11/10/2020
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; Modified from 'FDISK3.ASM' (FDISK3.COM) source code by Erdogan Tan
; (04/05/2024) - TRDOS 386 v2 fixed disk partitioning utility -
; ****************************************************************************
; nasm fdisk3r5.s -l fdisk3r5.txt -o FDISK3R5.COM -Z error.txt

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

; BIOS Disk Parameters
DPDiskNumber  equ 0h
DPDType       equ 1h
DPReturn      equ 2h
DPHeads       equ 3h
DPCylinders   equ 4h
DPSecPerTrack equ 6h
DPDisks       equ 7h
DPTableOff    equ 8h
DPTableSeg    equ 0Ah
DPNumOfSecs   equ 0Ch

; BIOS INT 13h Extensions (LBA extensions)
; Just After DP Data (DPDiskNumber+)
DAP_PacketSize equ 10h  ; If extensions present, this byte will be >=10h
DAP_Reserved1 equ 11h   ; Reserved Byte 
DAP_NumOfBlocks equ 12h ; Value of this byte must be 0 to 127
DAP_Reserved2 equ 13h   ; Reserved Byte
DAP_Destination equ 14h ; Address of Transfer Buffer as SEGMENT:OFFSET
DAP_LBA_Address equ 18h ; LBA=(C1*H0+H1)*S0+S1-1
                        ; C1= Selected Cylinder Number
                        ; H0= Number Of Heads (Maximum Head Number + 1)
                        ; H1= Selected Head Number
                        ; S0= Maximum Sector Number
                        ; S1= Selected Sector Number
                        ; QUAD WORD
; DAP_Flat_Destination equ 20h ; 64 bit address, if value in 4h is FFFF:FFFFh
                             ; QUAD WORD (Also, value in 0h must be 18h) 
                             ; TR-DOS will not use 64 bit Flat Address

; INT 13h Function 48h "Get Enhanced Disk Drive Parameters"
; Just After DP Data (DPDiskNumber+)
GetDParams_48h equ 20h ; Word. Data Lenght, must be 26 (1Ah) for short data.
GDP_48h_InfoFlag equ 22h ; Word
; Bit 1 = 1 -> The geometry returned in bytes 4-15 is valid.
GDP_48h_NumOfPCyls equ 24h ; Double Word. Number physical cylinders.
GDP_48h_NumOfPHeads equ 28h ; Double Word. Number of physical heads.
GDP_48h_NumOfPSpT equ 2Ch ; Double word. Num of physical sectors per track.
GDP_48h_LBA_Sectors equ 30h ; 8 bytes. Number of physical/LBA sectors.
GDP_48h_BytesPerSec equ 38h ; Word. Number of bytes in a sector.

pTableOffset equ 1BEh ; 446

	; Convert LBA to CHS
	; 08/02/2019

	; LBA = ((C1*H0+H1)*S0)+S1-1
	;
	; C1 = Selected Cylinder Number
	; H0 = Number of Heads (Maximum Head Number + 1)
	; H1 = Selected Head Number
	; S0 = Maximum Sector Number
	; S1 = Selected Sector Number
	;
	; Phoenix, Enchanced Disk Drive Specicifications, v1.1, Page 8)
		

[BITS 16]
[ORG 100h]

	;cli
	;cld
	;push	cs
	;pop	ss
	;mov	sp, 0FFFEh
	;sti
	
	;mov	bx, SizeOfFile+100
	
	mov	bx, bss_end

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
; clear BSS
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 03/02/2019

	;mov	cx, bss_end
	mov	cx, bss_clear_end ; 15/02/2019

	mov	di, bss_start
	sub	cx, di
	inc	cx
	shr	cx, 1
	xor	ax, ax
	rep	stosw 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display program name & version
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, TrDOS_Welcome
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get hard disk name
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 11/10/2020

	mov	si, 80h			; PSP command tail
 	lodsb
	or	al, al 			; command tail length
	jz	short A_05		; jump if zero
A_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short A_01
	jb	short A_05
	
	; check disk name

	cmp	al, 'h'
	jne	short A_03
	cmp	byte [si], 'd'
	jne	short A_03
	inc	si
	lodsb
	cmp	al, '0'
	je	short A_02
	jb	short A_03
	cmp	al, '3'
	ja	short A_03
A_02:
	cmp	byte [si], ' '
	jb	short A_04
	ja	short A_03
	inc	si
	jmp	short A_02
A_03:
	mov	si, TrDOS_Usage
	jmp	_p_exit
A_04:
	add	al, 80h - '0'
	mov	[DrvNum], al	; 80h .. 83h

A_05:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get hard disk parameters 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 11/10/2020

	; check bios int 13h extensions
	mov	ah, 41h
	mov	bx, 55AAh
	mov	dl, 80h
	int	13h
	jc	short A_06
	cmp	bx, 0AA55h
	jne	short A_06

	mov	byte [int13h_x], 1
A_06:
	;mov	dl, 80h	 ; hd0
	mov	ah, 08h  ; return disk parameters
	int	13h	
	jnc	short A_10
A_07:
	cmp 	byte [DrvNum], 80h  ; hard disk name option ?
	jb	short A_09 ; no, default
A_08:
	mov	si, msg_drv_not_ready ; drive not ready
	jmp	_p_exit
A_09:
	mov	si, msg_not_any_disks ; there is not a hard disk
	jmp	_p_exit
A_10:
	or	ah, ah	 ; ah = 0 ?	
	jnz	short A_07  ; no, there is an error !
	
	mov	[hdc], dl ; number of hard disks

	mov	al, [DrvNum]

	cmp	al, 80h
	jnb	short A_11 ; option
	; default
	mov	byte [DrvNum], 80h
	jmp	A_14
A_11:
	; hard disk name option
	add	dl, 7Fh

	cmp	al, dl
	ja	short A_08

	cmp	dl, 80h
	jna	short A_13 ; hd0 parameters are ready
A_12:
	mov	dl, al ; [DrvNum]

	mov	ah, 08h  ; return (get) disk parameters
	int	13h	
	jc	short A_07
	or	ah, ah
	jnz	short A_07
A_13:
	mov	al, cl
	and	al, 3Fh ; 63
	shr	cl, 6
	xchg	ch, cl
	inc	cx
	inc	cx ; 15/10/2020 ; BIOS BugFix 
				; (for reserved last cylinder)
	mov	[cylinders], cx
	;mov	word [cylinders+2], 0 ; 16/10/2020
	inc	dh
	mov	[heads], dh	
	mov	[sectors], al
	mul	dh
	mov	[hs], ax ; heads*sectors ; 15/10/2020
	mul	cx
	mov	[disksize], ax
	mov	[disksize+2], dx
	sub	ax, 1	; 17/10/2020
	sbb	dx, 0
	mov	[chs_limit], ax	
	mov	[chs_limit+2], dx

	cmp	byte [int13h_x], 1
	jb	A_20

	mov	si, gdp_buffer
	;mov	word [si], 30 ; buffer length (minimum)
	mov	word [si], 26 ; buffer length (minimum)
	mov	ah, 48h
	mov	dl, [DrvNum]
	int	13h
	jc	A_20
	and	ah, ah
	jnz	A_20

	; number of physical sectors
	mov	ax, [si+16]
	mov	dx, [si+18]
	mov	[disksize], ax
	mov	[disksize+2], dx

	; 15/10/2020
	mov	cx, [hs]
	call	div32
	; 16/10/2020
	;mov	[lba_cyls], ax
	;mov	[lba_cyls+2], dx
	mov	[cylinders], ax
	mov	[cylinders+2], dx
	mov	[lba_chs_remain], bx

	jmp	A_20	
A_14:
	cmp	dl, 1
	jna	short A_13 ; [DrvNum] = 80h

	add	dl, '0' ; '2' to '4'
	mov	byte [TrDOS_dnmax], dl

	mov	si, TrDOS_Options
	call	print_msg

	; check bios int 13h extensions
	cmp	byte [int13h_x], 1
	jnb	short A_16
A_15:
	mov	dl, [DrvNum]
	mov	ah, 08h  ; return disk parameters
	int	13h	
	jc	short A_17
	or	ah, ah
	jnz	short A_17

	mov	al, cl
	and	al, 3Fh ; 63
	shr	cl, 6
	xchg	ch, cl
	inc	cx
	inc	dh
	mul	dh
	mul	cx

	; dx:ax = disk size

	call	display_hd_row	

	dec	byte [hdc]
	jz	short A_17
	inc	byte [DrvNum]
	jmp	short A_15
A_16:
	mov	si, gdp_buffer
	;mov	word [si], 30 ; buffer length (minimum)
	mov	word [si], 26 ; buffer length (minimum)
	mov	ah, 48h
	mov	dl, [DrvNum]
	int	13h
	jc	short A_15
	and	ah, ah
	jnz	short A_15

	; number of phsical sectors
	mov	ax, [si+16]
	mov	dx, [si+18]

	; dx:ax = disk size

	call	display_hd_row	

	dec	byte [hdc]
	jz	short A_17
	inc	byte [DrvNum]
	jmp	short A_16
A_17:
	mov	si, CRLF
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; hard disk number input (getchar)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 11/10/2020
	
A_18:
	xor	ax, ax
	int	16h			; wait for keyboard command

	cmp	ax, 0			; CTRL+BREAK
	jna	short A_19

	cmp	al, 'C'-40h		; 3
	je	short A_19             	; CTRL+C
	cmp	al, 27			; ESCape
	je	short A_19

	cmp	al, '1'			; "(1) hd0" 
	jb	short A_18		; retry
	cmp	al, [TrDOS_dnmax]	; ('2' to '4')	
	ja	short A_18		; retry
	add	al, 80h-'1'		; bios disk num (80h-83h)
	mov	[DrvNum], al
	jmp	A_12			; get disk parameters
A_19:
	jmp	_exit			; Exit

A_20:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; read masterboot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 12/10/2020

	mov	bx, MasterBootBuff
	mov	cx, 1	; cylinder = 0
			; sector = 1
	mov	dh, 0	; head = 0
	mov	dl, [DrvNum]
	mov	ax, 0201h ; read one sector
	int	13h
	jnc	short A_21

	mov	si, msg_drv_not_ready ; drive not ready
	jmp	A_26
A_21:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display CHS values and disk size (LBA) and partition table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 12/10/2020

	mov	ax, 3  ; clear screen
	int	10h

	call	init_partition_table ; 13/10/2020

	call	display_chs_table

	mov	ax, [disksize]
	mov	dx, [disksize+2]

	cmp	dx, 256 ; >= 8GB  
	jnb	short A_22

	;mov	cx, 2048 ; /2048 (2048 sectors = 1 MB)
	;div	cx

	; shift dx:ax to 8 bits right (/256)
	mov	al, ah
	mov	ah, dl
	shr	ax, 3 ; /8 
		; result = (dx:ax)/2048

	; ax =  0 to 8191

	mov	byte [TrDOS_hdrow_unit], 'M'
	jmp	short A_23


A_29:	; 16/10/2020
	call	partition_table_fix
	jnc	short A_21

	int	20h
;here:	
	;jmp	short here

A_22:
	; 1024 MB = 1GB (2097152 sectors)
	; DX/32 --> GB 
	mov	ax, dx
	shr	ax, 5 ; /32

	; ax = 8 to 2047
	mov	byte [TrDOS_hdrow_unit], 'G'
A_23:
	;xor	dx, dx
	mov	si, TrDOS_hdrow_capacity
	call	convert_to_decimal

	mov	si, TrDOS_disksize
	call	print_msg

	mov	si, TrDOS_hdrow_capacity
	call	print_msg

	mov	si, TrDOS_hdrow_unit
	call	print_msg

	; 16/10/2020
	;cmp	word [lba_cyls+2], 0
	cmp	word [cylinders+2], 0
	ja	short A_27  ; Huge disk ! number of (lba) cylinders > 65335
			    ; Current FDISK version (v3) can not be used !

	mov	si, CRLF
	call	print_msg

	call	dpt_1

	mov	si, CRLF
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display edit or exit option, getchar
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_edit_or_exit
	call	print_msg
A_24:
	xor	ah, ah
	int	16h

	cmp	al, 13 ; CR (ENTER) key
	je	short A_28 

	cmp	al, 27 ; ESCape key
	je	short A_25

	cmp	al, 32 ; SPACE key
	je	short A_28

	cmp	al, 3 ; CTRL+C
	je	short A_25

	cmp	ax, 0 ; CTRL+BREAK
	ja	short A_24
A_25:
	mov	si, CRLF
A_26:
	call	print_msg

	int	20h
;here:
;	jmp	short here

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display fdisk program disk capacity limit message and then exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_27:
	; 16/10/2020
	; 15/10/2020
	; Display FDISK (v3) capacity limit (error) message

	mov	ax, 65535
	mul	word [hs]
		; 1024 MB = 1GB (2097152 sectors)
	; DX/32 --> GB 
	mov	ax, dx
	shr	ax, 5 ; /32

	; ax = 8 to 2047
	mov	byte [TrDOS_hdrow_unit], 'G'
	;mov	byte [TrDOS_hdrow_unit+1], 'B'
	mov	byte [TrDOS_hdrow_unit+2], '.'

	;xor	dx, dx
	mov	si, TrDOS_hdrow_capacity	
	call	convert_to_decimal

	mov	si, msg_fdisk_capacity_err
	call 	print_msg

	mov	si, TrDOS_hdrow_capacity
	call	print_msg

	mov	si, TrDOS_hdrow_unit
	call	print_msg
	
	jmp	_exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; check defective pte and then init extended partition table(s)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_28:
	call	check_defective_partition
	jc	A_29 ; ds:si -> defective pte

	; 17/10/2020
	; 26/02/2019
	cmp	byte [epnumber], 0
	jna	short A_30

	call	init_ext_partition_table
A_30:
	; 17/10/2020
	; 04/02/2019
	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021

	; 17/10/2020
	cmp	byte [pcount], 0
	jna	A_38 ; empty partition table

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; check CHS parms against start sector address and partition size 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_31:
	; Check disk parameters with current CHS settings
	xor	bx, bx
	xor	cx, cx
A_32:
	; 17/10/2020
	cmp	byte [part_table_sys_id+bx], 0
	jna	short A_33

	; 27/10/2020
	;mov	ax, [sectors]
	;mul	word [heads]
	; dx = 0, ax = heads*sectors

	mov	dx, [part_table_start_cyl+bx]
	; 27/10/2020
	cmp	dx, 1023 ; fixed cylinder number (> 1023)
			 ; (partition's start sector is used for fixing)
	ja	short A_33 ; no need to check CHS parms here
			; (also, CHS to LBA calculation is not applicable)
	mov	ax, [hs] ; heads*sectors

	mul	dx
		; dx:ax = cylinder*heads*spt
	mov	si, ax
	mov	di, dx
	mov	al, [part_table_start_head+bx]
	mul	byte [sectors]
	add	si, ax
	adc	di, 0
		; di:si = (cylinder*heads*spt) + head*spt
	mov	al, [part_table_start_sector+bx]
	xor	ah, ah
	dec	al ; 1 -> 0
	add	si, ax
	adc	di, 0
		; di:si = (cylinder*heads*spt) + head*spt + sector - 1
		; (start sector as converted to LBA)

	cmp	di, [part_table_rel_sec_hw+bx]
	jne	short A_34 ; Invalid !

	cmp	si, [part_table_rel_sec_lw+bx]
	jne	short A_34 ; Invalid !
		; di:si = partition's start sector (LBA)

	; 27/10/2020
	;mov	ax, [sectors]
	;mul	word [heads]
	; dx = 0, ax = heads*sectors

	mov	dx, [part_table_end_cyl+bx]

	; 27/10/2020
	cmp	dx, 1023 ; fixed cylinder number (> 1023)
			 ; (partition's end sector is used for fixing)
	ja	short A_33 ; no need to check CHS parms here
			; (also, CHS to LBA calculation is not applicable)
	mov	ax, [hs] ; heads*sectors

	mul	dx
		; dx:ax = cylinder*heads*spt
	mov	si, ax
	mov	di, dx
	mov	al, [part_table_end_head+bx]
	mul	byte [sectors]
	add	si, ax
	adc	di, 0
		; di:si = (cylinder*heads*spt) + head*spt
	mov	al, [part_table_end_sector+bx]
	xor	ah, ah
	;dec	al ; 63 -> 62
	add	si, ax
	adc	di, 0
		; di:si = (cylinder*heads*spt) + head*spt + sector
		; (end sector + 1 as converted to LBA)
	mov	ax, [part_table_num_sec_lw+bx]
	mov	dx, [part_table_num_sec_hw+bx]
	
	add	ax, [part_table_rel_sec_lw+bx]
	adc	dx, [part_table_rel_sec_hw+bx]
		; dx:ax = partition's end sector (LBA) + 1

	cmp	ax, si
	jne	short A_34 ; Invalid !

	cmp	dx, di
	jne	short A_34 ; Invalid !
A_33:
	inc	cl

	cmp	cl, 4
	jnb	short A_35

	add	bx, 18  ; partition table structure size
	
	jmp	A_32
A_34:
	mov	si, msg_defective_pt
	call	print_msg
	jmp	short A_38

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; sort MBR partitions
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_35:
	; LBA and CHS values of all partitions are OK (here)

	; sort MBR (primary) partitions

	call	sort_partition_table

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check partition (start, size) overlaps after sorting 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; Checking partition overlaps (via start and end cylinders)

	xor	bx, bx
	mov	dx, 18
A_36:
	inc	bl

	mov	al, [bx+sort]
	mul	dl
	mov	si, ax	; current

	cmp	byte [part_table_sys_id+si], 0
	jna	short A_37

	mov	al, [bx+sort-1]
	mul	dl
	mov	di, ax  ; previous

	mov	ax, [part_table_end_cyl+di]
	cmp	ax, [part_table_start_cyl+si]
	jb	short A_37
	; 29/10/2020
	ja	short A_34 ; overlap !

	; 17/10/2020
	;and	ax, ax
	;jnz	short A_34 ; overlap error (except empty entries)

	; 29/10/2020
	; same cylinder
	; check end-start sectors for overlap

	and	ax, ax
	jz	short A_37 ; empty pt entries
			; (previous pte is empty)

	mov	ax, [part_table_rel_sec_lw+di] ; previous
	mov	dx, [part_table_rel_sec_hw+di]
	add	ax, [part_table_num_sec_lw+di]
	adc	dx, [part_table_num_sec_hw+di]
	;jc	short A_34
		 ; dx:ax = end sector + 1
	cmp	dx, [part_table_rel_sec_hw+si] ; current
	jb	short A_37
	ja	short A_34 ; overlap error !	
	cmp	ax, [part_table_rel_sec_lw+si]
	jnb	short A_34 ; overlap error ! 	
A_37:
	cmp	bl, 3
	jb	short A_36

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display partition table editing options
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_38:
	mov	si, mbr_editing_options
	call	print_msg

	mov	si, enter_option_number_msg
	call	print_msg

	cmp	byte [ldrives], 0
	jna	short A_39

	mov	si, CRLF
	call	print_msg

	mov	si, str_display_ebr_pt
	call	print_msg
A_39:	
	;mov	si, CRLF
	;call	print_msg

A_40:
	xor	ah, ah
	int	16h

	cmp	al, '0'
	je	short A_41

	cmp	al, '1'
	je	create_partition
	cmp	al, '2'
	je	activate_partition
	cmp	al, '3'
	je	delete_partition
	cmp	al, '4'
	je	write_pt_mbr
	
	cmp	al, 27 ; ESCape
	je	short A_41

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display (EPT) logical drives/partitions if 'SPACE' is pressed
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 28/02/2019
	cmp	byte [ldrives], 0
	jna	short A_40

	cmp	al, 20h ; SPACE
	jne	short A_40

	jmp	display_extended_pt

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; exit if ESC key or '0' is pressed
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_41:
	; terminate
	int	20h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display partition table again
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A_42:
	; 24/02/2019
	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021
	jmp	short A_38

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display a row for hard disk name, number and capacity 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 11/10/2020

display_hd_row:
	; dx:ax = disk size

	cmp	dx, 256 ; >= 8GB  
	jnb	short dhdr_1

	;mov	cx, 2048 ; /2048 (2048 sectors = 1 MB)
	;div	cx

	; shift dx:ax to 8 bits right (/256)
	mov	al, ah
	mov	ah, dl
	shr	ax, 3 ; /8 
		; result = (dx:ax)/2048

	; ax =  0 to 8191

	mov	byte [TrDOS_hdrow_unit], 'M'
	jmp	short dhdr_2
	
dhdr_1:
	; 1024 MB = 1GB (2097152 sectors)
	; DX/32 --> GB 
	mov	ax, dx
	shr	ax, 5 ; /32

	; ax = 8 to 2047
	mov	byte [TrDOS_hdrow_unit], 'G'
dhdr_2:
	;xor	dx, dx
	mov	si, TrDOS_hdrow_capacity
	call	convert_to_decimal

	inc	byte [TrDOS_hdrow_n] ; next number for "(#)"
	
	mov	si, TrDOS_hdrow
	call	print_msg
	mov	si, TrDOS_hdrow_capacity ; db "#### ", 0
	call	print_msg
	mov	si, TrDOS_hdrow_unit ; "GB" or "MB"
	call	print_msg

	inc	byte [TrDOS_hdrow_i] ; next number for "hd#"

	retn	

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; convert binary number to decimal character string
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 11/10/2020

convert_to_decimal:
	; ax = binary number
	; si = decimal string start addr (max. 5 digits + space)
	mov	cx, 10
	mov	bp, sp
ctd_1:
	xor	dx, dx
	div	cx
	push	dx ; 0 to 9
	or	ax, ax
	jnz	short ctd_1
ctd_2:
	pop	ax
	add	al, '0'
	mov	[si], al
	inc	si
	cmp	bp, sp
	ja	short ctd_2
	;mov	byte [si], 20h ; space before size unit char
	;inc	si 
	;mov	byte [si], 0 ; ASCIIZ string terminator (Z)
	mov	word [si], 20h
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; check defective partition signature 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 16/10/2020

check_defective_partition:
	mov	bx, part_table_boot_ind
	xor	si, si 
chk_dp_1:
	cmp	byte [bx], 0FFh ; invalid/defective/partition sign
	jne	short chk_dp_2
	shl	si, 4 ; * 16
	add	si, MasterBootBuff+446
	stc
	retn
chk_dp_2:
	;cmp	bx, part_table_boot_ind+(18*4)
	cmp	si, 3
	jnb	short chk_dp_3
	add	bx, 18
	inc	si
	jmp	short chk_dp_1 
chk_dp_3:
	;sub	si, si
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; create a disk partition (on MBR partition table)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 18/10/2020

create_partition:
	; 19/10/2020
	; 10/02/2019 (hdimage.s)
	xor	ax, ax
	mov	byte [wholedisk], al ; 0 ; Reset wholedisk flag

	; 19/10/2020
	cmp	byte [pcount], al ; 0 ; number of (valid) partitions
	jna	short cp_1
;cp_0:
	cmp	byte [pcount], 4
	jnb	short cp_2 ; full pt !

	; al = 0
	call	find_part_free_space
		 ; Following values are for the max. free space on disk

	and	cx, cx
	jz	short cp_3 ; there is not free space on disk

	;mov	[c_cylinders], cx  ; count of free cylinders in the gap
	mov	[c_fspc_offset], bx  ; offset from beginning of 'fspc:'
	;mov	[c_gap], al ; gap (space index) number of this free space
	;		    ; 0 -> before partition 1 (as PTE)
	;		    ; 1-2-3 -> between partitions 1 to 4
	;		    ; 4 -> after partition 4 (as PTE)

	; Start to job with non-aligned (full) free sectors of this max. space
	
	mov	ax, [free_space.sectors_unused+bx]
	mov	dx, [free_space.sectors_unused+2+bx]

	mov	[pp_Sectors], ax
	mov	[pp_Sectors+2], dx
cp_1:
	jmp	B_01 ; New/Empty disk, create partition menu

cp_2:
	; 13/02/2019
	; There is not a free pt entry to create a new partition

	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021

	mov	si, msg_full_pt
	call	print_msg
	mov	si, msg_to_create_new_p
	call	print_msg

	jmp	ap_0
cp_3:
	; 19/10/2020
	cmp	byte [epnumber], 0
	ja	short create_logical_drives
;cp_4:
	; 15/02/2019
	; There is not (enough) free space to create a new partition

	mov	si, msg_no_free_space
	call	print_msg
	jmp	ap_0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; create logical -dos- drives
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020

create_logical_drives:
	; 05/03/2019
	call	check_ext_free_space
	jc	short cld_5
cld_1:
	; 05/03/2019
	cmp	byte [ldrives], 4
	jb	short cld_2
	jmp	eetc_2
cld_2:
	; 01/11/2020
	mov	[ep_free_sectors], ax
	mov	[ep_free_sectors+2], dx

	mov	si, msg_c_ldd_q
	call	print_msg
cld_3:	
	sub	ah, ah
	int	16h

	cmp	al, 27 ; ESCAPE key
	;je	cp_esc
	je	short cld_6 ; 19/10/2020
	and	al, 0DFh
	cmp	al, 'N'
	je	short cld_4
	cmp	al, 'Y'
	jne	short cld_3
	mov	si, _msg_YES
	call	print_msg

	jmp	edit_ext_table_create_x
cld_4:	
	mov	si, _msg_NO
	call	print_msg
cld_6:
	;jmp	cp_esc
	; 19/10/2020
	jmp	A_42
cld_5:	
	mov	si, msg_c_part_error
	call	print_msg

	; 21/03/2021
	;cmp	byte [ldrives], 3
	;;jna	short cld_2
	; 21/03/2021
	;jna	short ap_0

	;mov	si, msg_c_ldd_error
	;call	print_msg

	jmp	short ap_0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set active partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020 
	
activate_partition:
	; 12/02/2019
	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021

	cmp	byte [pcount], 0
	ja	short ap_1

	mov	si, msg_empty_pt
	call	print_msg
ap_0:
	mov	si, msg_press_any_key
	call	print_msg

	xor	ah, ah
	int	16h
ap_5:
	;jmp	ap_esc
	; 19/10/2020
	jmp	A_42

ap_1:
	; Set valid_ppnums (MBR partition IDs) list
	mov	si, MasterBootBuff+pTableOffset ; MasterBootBuff+446
	mov	bx, valid_ppnums
	mov	cl, 4
ap_2:
	mov	al, [si+ptFileSystemID]
	mov	[bx], al
	dec	cl
	jz	short ap_3
	inc	bx
	add	si, 16
	jmp	short ap_2
ap_3:
	mov	si, msg_enter_pn_to_act
	call	print_msg
ap_getchar:
	xor	ah, ah
	int	16h
	
	; 19/10/2020
	cmp	al, 27
	;je	ap_esc
	je	short ap_5

	;cmp	al, '0'
	;;je	ap_esc
	;je	short ap_5

	cmp	al, '1'
	jb	short ap_getchar
	cmp	al, '4'
	ja	short ap_getchar

	xor	ah, ah
	mov	dx, ax
	
	sub	dl, '1'
	mov	si, dx
	cmp	byte [si+valid_ppnums], ah  ; 0
	jna	short ap_getchar ; Empty partition table entry, it is not shown

	mov	bx, 07h
	mov	ah, 09h
	mov	cx, 1
	int	10h

	mov	si, MasterBootBuff+pTableOffset
	mov	di, part_table_boot_ind ; (**)

	; Clear all of possible active partition flags
	mov	[si], dh ; 0
	mov	[si+16], dh ; 0
	mov	[si+32], dh ; 0
	mov	[si+48], dh ; 0

	; This may not be needed !? 
	; (**) (Primary partitions structure/list)
	mov	[di], dh ; 0	
	mov	[di+18], dh ; 0
	mov	[di+36], dh ; 0
	mov	[di+54], dh ; 0

	and 	dl, dl
	jz	short ap_4

	mov	ax, dx

	shl	al, 4 ; * 16
	add	si, ax

	mov	al, 18
	mul	dl ; 1 to 3
	add	di, ax
ap_4:
	; Then	set active partition as requested by user
	mov	byte [si], 80h
	mov	byte [di], 80h ; (**)

	mov	si, CRLF ; Next line	
	call	print_msg

	; NOTE: Only the MBR buffer has been changed
	; (Masterboot sector will not be updated unless/till
	;  MBR partition table -and MBR code- will be written
	;  to disk image as result of 'write part. table' command.)

	;; wait for 1 second
	;mov	ah, 02h
	;int	1Ah
	;mov	[GetChar], dh
;ap_wait:
	;mov	ah, 02h
	;int	1Ah
	;cmp	dh, [GetChar]
	;je	short ap_wait

	; 17/10/2020
	jmp	A_42 ; display partition table again

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; delete selected partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020

delete_partition:
	; 10/02/2019
	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021

	cmp	byte [pcount], 0
	ja	short dp_1

	mov	si, msg_empty_pt
	call	print_msg

	mov	si, msg_press_any_key
	call	print_msg

	xor	ah, ah
	int	16h

	jmp	short dp_esc

dp_1:
	; Set valid_ppnums (MBR partition IDs) list
	mov	si, MasterBootBuff+pTableOffset ; MasterBootBuff+446
	mov	bx, valid_ppnums
	mov	cl, 4
dp_2:
	mov	al, [si+ptFileSystemID]
	mov	[bx], al
	dec	cl
	jz	short dp_3
	inc	bx
	add	si, 16
	jmp	short dp_2
dp_3:
	mov	si, msg_enter_pn_to_del
	call	print_msg
dp_getchar1:
	xor	ah, ah
	int	16h
	
	; 19/10/2020
	cmp	al, 27
	je	short dp_esc
	cmp	al, '0'
	je	short dp_esc
	;cmp	al, '1'
	jb	short dp_getchar1
	cmp	al, '4'
	ja	short dp_getchar1

	xor	bh, bh
	mov	bl, al
	;dec	bl
	sub	bl, '1'
	cmp	byte [bx+valid_ppnums], bh ; 0 ; 12/02/2019
	jna	short dp_getchar1  ; Empty partition table entry, it is not shown
	
	mov	[del_part_num], bl ; Selected partition number to be deleted.
	mov	[chr_del_pnum1], al ; Partition number for "Enter ..." text
	mov	[chr_del_pnum2], al ; Partition number for "Do you want ..." text

	mov	si, chr_del_pnum1 ; write partition number (user input)
	call	print_msg

	;xor	al, al
	mov	[chr_del_pnum1], al ; 0 ; reset

	mov	si, msg_delete_partition_q ; question for deleting (with warning)
	call	print_msg

dp_getchar2:
	xor	ah, ah
	int	16h

	cmp	al, 27
	je	short dp_esc

	and	al, 0DFh
	cmp	al, 'Y'
	je	short dp_yes
	cmp	al, 'N'
	jne	short dp_getchar2
dp_no:
	mov	si, msg_NO ;  Write 'NO' (it may be shown for a moment)
	call	print_msg
	
	mov	si, CRLF  ; Next line
	call	print_msg
	
	;jmp	short dp_esc

;cp_esc:
;ap_esc:
;wptmbr_esc:
dp_esc:
	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	;call	display_partition_table
	;jmp	A_38 ; 17/10/2020

	; 19/10/2020
	jmp	A_42

dp_yes:
	mov	si, msg_YES ;  Write 'YES' (it may be shown for a moment)
	call	print_msg

	mov	di, MasterBootBuff+pTableOffset
	mov	al, [del_part_num]
	and 	al, al
	jz	short dp_4
	cbw
	;xor	ah, ah
	shl	al, 4 ; * 16
	add	di, ax
dp_4:
	mov	si, CRLF ; Next line
	call	print_msg

	; 27/02/2019
	cmp	byte [di+ptFileSystemID], 05h ; EXTENDED (CHS)  
	;jne	short dp_5
	je	short dp_6
	; 24/10/2020
	cmp	byte [di+ptFileSystemID], 0Fh ; EXTENDED (LBA)
	jne	short dp_5
dp_6:
	cmp	byte [ldrives], al ; 0
	jna	short dp_5

	mov	si, msg_ext_part_del_error
	call	print_msg
	mov	si, msg_cancel_continue
	call	print_msg
     	
	xor	ah, ah
	int	16h
	
	cmp	al, 27 ; ESCape
	je	short dp_esc

	call	delete_logical_drives
	
	; 28/02/2019
	cmp	byte [ldrives], 1
	jnb	short dp_esc

	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021

	mov	si, msg_delete_ext_part
	call	print_msg

dp_ask_again:
	sub	ah, ah
	int	16h

	cmp	al, 27 ; ESCape
	je	short dp_esc	

	cmp	al, 13 ; ENTER/CR
	jne	short dp_ask_again

	mov	di, MasterBootBuff+pTableOffset
	mov	al, [del_part_num]
	cbw
	shl	al, 4 ; * 16
	add	di, ax
dp_5:
	xor	ax, ax

	; clear partition table entry in MBR buffer
	mov	cx, 8
	rep	stosw
	
	; NOTE: Only the MBR buffer will be cleared
	; (Masterboot sector will not be updated unless/till
	;  MBR partition table -and MBR code- will be written
	;  to disk image as result of 'write part. table' command.)

	jmp	A_21 ; 17/10/2020
 	 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write partition table (and MBR code) onto disk
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 18/10/2020

write_pt_mbr:
	; 18/10/2020 (fdisk3.s)
	; 11/02/2019 (hdimage.s)
	;xor	al, al  ; MBR/PRIMARY PARTITIONS
	call	display_partition_table ; 12/03/2021

	mov	si, msg_write_masterboot_sector
	call	print_msg

	mov	[GetChar], al ; 0

	mov	si, option_input
	call	print_msg

	mov	ah, 3
	;mov	bx, 7
	int	10h ; Return Cursor Position
	; DL = Column
	
	dec	dl ; previous char ; ']'
	dec	dl ; previous char ; '[ ]'

	mov	ah, 2
	;mov	bx, 7
	int	10h ; Set Cursor Position

	; write char at current cursor position

	mov	al, '0' ; Write default char

	;;mov	bx, 07h
	mov	ah, 09h
	mov	cx, 1
	int	10h

wptmbr_getchar:
	xor	ah, ah
	int	16h

	cmp	al, 1Bh ; 27
	;je	wptmbr_esc ; 19/10/2020
	je	A_42

	cmp	al, 0Dh ; 13
	je	short wptmbr_1
                
	cmp	al, '1'
	jb	short wptmbr_getchar

	cmp	al, '2'
	ja 	short wptmbr_getchar

	mov	[GetChar], al

	;;mov	bx, 07h
	mov	ah, 09h
	mov	cx, 1
	int	10h
	jmp	short wptmbr_getchar

wptmbr_1:
	cmp	byte [GetChar], 0
	jna	short wptmbr_getchar

	mov	si, msg_writing_ptable
	call	print_msg

	cmp	byte [GetChar], '2'
	jne	short wptmbr_2 ; Do not write Singlix MBR code

	; Write CHS parameters in Singlix (FS specific) MBR
	
	mov	si, TRDOS386_MASTERBOOT_SECTOR
	mov	cx, 446/2 ; Copy Singlix FS 1 MBR code
			  ; except Partition Table
	mov	di, MasterBootBuff
	rep	movsw
	
	; This (Below) is not needed; because, if MBR magicword
	; would not be 0AA55h, we would not be able to come here!
	;;add	di, 64  ; skip partition table	
	;;mov	ax, 0AA55h
	;;stosw
	;mov 	word [MBIDCode], 0AA55h

	; 12/02/2019
	; Copy CHS parameters to Singlix MBR (on disk)
	
	mov	di, MasterBootBuff+420

	mov	ax, [cylinders]
	stosw 			; cylinders
	mov	ax, [heads]
	stosw 			; heads
	mov	ax, [sectors]
	stosw 			; sectors

	; 13/03/2021
	; copy 32 bit disk size (total LBA sectors) to MBR offset 426
	mov	si, disksize
	movsw
	movsw

wptmbr_2:
	;xor	ax, ax ; 0
	;xor	dx, dx ; 0
	;; DX_AX = Masterboot Sector = 0
	;mov	bx, MasterBootBuff
	;; ES:BX = Sector Buffer
	;call	write_hd_sector
	;jc	short print_error_code 
			; ! display error msg and then exit !

	mov	byte [rcnt], 5  ; retry count

	mov	bx, MasterBootBuff

	mov	cx, 1	; cylinder = 0
			; sector = 1
	mov	dh, 0	; head = 0
	mov	dl, [DrvNum]
wptmbr_3:
	mov	ax, 0301h ; write one sector
	int	13h	
	jnc     short wptmbr_4
             
	dec	byte [rcnt]
	jz	short print_error_code
        
	xor	ah, ah
	;mov	dl, [DrvNum]
	int	13h	; BIOS Service func (ah) = 0
			; Reset disk system
	jmp	short wptmbr_3

wptmbr_4:
	mov	si, _msg_OK
	call	print_msg

	; wait for 1 second
	mov	ah, 02h
	int	1Ah
	mov	[GetChar], dh
wptmbr_wait:
	mov	ah, 02h
	int	1Ah
	cmp	dh, [GetChar]
	je	short wptmbr_wait
	
	mov	si, msg_press_any_key
	call	print_msg
                
	xor	ah, ah	; "Press any key to continue"
	int	16h

	mov	si, CRLF
	call	print_msg

	;jmp	wptmbr_esc
	; 19/10/2020
	jmp	A_42

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; print error message and exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 18/10/2020 (fdisk3.s)
	
print_error_code:
	; 25/02/2019 (hdimage.s)

	mov	al, ah	; error code
	call	bin_to_hex
	mov 	[error_code], ax

	mov	si, CRLF
	call	print_msg

	mov	si, Msg_Error
	call	print_msg
	
	int	20h	; Exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; exit, print_msg & exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020

_crlf_exit:
	mov	si, CRLF
_p_exit:
	; 11/10/2020
	call	print_msg
_exit:
	mov	ax, 4C00h		; terminate
	int	21h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display create partition menu & get partition type input
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020

create_partition_input:
	; 15/02/2019
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h
	
	mov	si, msg_create_partition_h ; header
	call	print_msg
	mov	si, msg_create_partition_m ; menu
	call	print_msg
cpi_1:
	xor	ah, ah
	int	16h
	cmp	al, 27 ; ESC key	
	je	short cpi_4  ; 25/02/2019
	cmp	al, '0'
	je	short cpi_4  ; 25/02/2019
	jb	short cpi_1
	cmp	al, '4'
	ja	short cpi_1

	xor	ah, ah
	sub	al, '0'

	;mov	[pp_type], al

	;mov	byte [pp_type_user], 0

	cmp	al, 1 ; DOS partition
	jne	short cpi_5 ; ah = 0 
			    ; (al = 2 -> singlix, al = 3 -> retro unix)
cpi_2:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h
	
	mov	si, msg_create_dos_partition_h ; header
	call	print_msg
	mov	si, msg_create_dos_partition_m ; menu
	call	print_msg
cpi_3:
	xor	ah, ah
	int	16h
	cmp	al, 27 ; ESC key
	je	short cpi_4
	cmp	al, '0'
	je	short cpi_4
	jb	short cpi_3
	cmp	al, '3'
	ja	short cpi_3
	
	xor	ah, ah
	sub	al, '0'
	cmp	al, 1
	je	short cpi_6  ;	ah = 0 (al = primary dos partition)

	;mov	byte [pp_type], 5

	cmp	al, 2
	jna	short cpi_7

	mov	ax, 6	; ah = 0 (al = logical dos drive/partition)
	retn

cpi_4:
	xor	ax, ax	; ah = 0 (al = 0 -> ESCape/Cancel)
	retn
cpi_5:
	cmp	al, 4	 ; option num. for partition type (user) input
	jne	short cpi_6

	call	partition_type_input
	
	; 29/10/2020
	or	al, al
	;jz	short cpi_6 ; invalid (zero) input or ESC key
	jz	short cpi_4

	mov	ah, 4 ; user/another type partition flag
	; al = Partition ID
	push	ax
	mov	si, msg_press_any_key 
	call	print_msg
	sub	ah, ah
	int	16h
	pop	ax
cpi_6:
	retn
cpi_7:
	mov	ax, 5	; ah = 0 (al = extended dos partition)
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; create primary (dos or non-dos) partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020 
;	(This procedure must be called after 'find_part_free_space')

create_primary_partition:  
	; 16/02/2019

	; INPUT:
	;	none 
	;  (CHS parameters and free space calculation result will be used.) 
	;
	; OUTPUT:
	;	Partition table in MBR buffer will be updated.
	;
	; (Modified registers: ax, bx, cx, dx, si, di)

	; Create primary dos or non-dos partition,
	; make partition table entry

	; 19/10/2020
	cmp	byte [pcount], 0  ; count of (valid) partitions
	ja	short cpp_0

	; cylinder = 0, head = 1, sector = 1
	; LBA = (((cylinder*heads)+head)*sectors)+sector-1

	mov	si, MasterBootBuff+pTableOffset

	mov	ax, [sectors] ; LBA = 17 or 63
	xor	dx, dx
	jmp	short cpp_4
cpp_0:
	; 16/02/2019
	call	get_first_free_pte
		 ; CX = First free PTE number, 0 to 3 
		 ;    (CX = 3 if there is not a free PTE, and CF = 1)
		 ;    ((But CF = 1 is not possible here because pcount < 4))
 	 	 ; SI = PTE address/offset in MBR buffer

	;mov	si, MasterBootBuff+pTableOffset
	;shl	cl, 4 ; * 16
	;add	si, cx

	; 18/02/2019
	mov	di, [c_fspc_offset]
	mov	bx, [di+free_space.start]
	mov	al, [heads]
	mul	byte [sectors]
	mul	bx
		; DX:AX = LBA of start cylinder, head 0, sector 1
	
	mov	cl, [cylinder_boundary]

	;cmp	cl, 0 ; Default (partition size unit is one of C, G, M)
		      ; boundary alignment is forced as default
	;je	short cpp_4

	; 20/02/2019
	;and	cl, cl
	;jz 	short cpp_1

	cmp	cl, 'Y'
	je	short cpp_4 ; cylinder boundary option (answer) = YES

	cmp	cl, 'N'
	jne	short cpp_1 ; cylinder boundary option (answer) = YES/NO

	; cylinder boundary option (answer) = NO 
	mov	ax, [di+free_space.startsector]
	mov	dx, [di+free_space.startsector+2]
	jmp	short cpp_4
cpp_1:
	;cmp	cl, 27 ; ESCape
	;jne	short cpp_4 ; 'Y'

	; check	cylinder boundary alignment of the start sector

	; if start sector is not aligned, end sector must not be aligned
	; (this rule is valid for ESCape key from the user) 

	mov	byte [cylinder_boundary], 'Y' ; YES for end sector check

	mov	cx, [di+free_space.startsector]

	or	bx, bx

	mov	bx, [di+free_space.startsector+2]

	jnz	short cpp_2 ; start cylinder > 0

	;and	bx, bx
	;jnz	short cpp_3

	cmp	cx, [sectors]
	je	short cpp_4 ; start sector is as aligned 
	jmp	short cpp_3
cpp_2:
	cmp	ax, cx
	jne	short cpp_3
	cmp	dx, bx
	je	short cpp_4 
cpp_3:
	mov	ax, cx
	mov	dx, bx
	mov	byte [cylinder_boundary], 'N'
cpp_4:
	mov	[si+ptStartSector], ax
	mov	[si+ptStartSector+2], dx

	; save start sector (for partition formatting procedure)
	mov	[pp_StartSector], ax
	mov	[pp_StartSector+2], dx

	; 14/03/2021
	mov	cx, [ppn_Sectors]
	mov	bx, [ppn_Sectors+2]

	; 19/10/2020
	cmp	byte [pcount], 0
	ja	short cpp_5

	;xor	cx, cx
	;mov	[si+ptBeginCylinder], cx ; 0
	;inc	cl  ; 1
	;mov	[si+ptBeginSector], cl ; 1
	;mov	[si+ptBeginHead], cl ; 1
	; 14/03/2021
	mov	word [si+ptBeginCylinder], 0
	mov	byte [si+ptBeginSector], 1
	mov	byte [si+ptBeginHead], 1

	; set active partition flag
	;mov	byte [si+ptBootable], 80h ; bootable/active partition
	mov	byte [si], 80h ; bootable/active partition

	; 14/03/2021
	; 18/02/2019
	;mov	cx, [ppn_Sectors]
	;mov	bx, [ppn_Sectors+2]

	cmp	byte [wholedisk], 0
	jna	cpp_12

	; 26/10/2020
	add	ax, cx
	adc	dx, bx
	sub	ax, 1
	sbb	dx, 0 

	; 24/10/2020
	cmp	dx, [chs_limit+2]
	jb	short cpp_17
	ja	short cpp_16
	cmp	ax, [chs_limit]
	jna	short cpp_17
cpp_16:
	mov	byte [si+ptEndHead], 0FEh
	mov	byte [si+ptEndSector], 0FFh
	mov	byte [si+ptEndCylinder], 0FFh
	jmp	cpp_15
cpp_17:
	mov	al, [heads]
	dec	al
	mov	[si+ptEndHead], al
	mov	al, [sectors]
	mov	[si+ptEndSector], al
	mov	ax, [cylinders]
	dec	ax
	mov	[si+ptEndCylinder], al
	shl	ah, 6
	or	[si+ptEndSector], ah

	;jmp	cpp_16
	jmp	cpp_15 ; 06/03/2019
cpp_5:
	; 18/02/2019
	cmp	byte [cylinder_boundary], 'Y'
	jne	short cpp_7

	xor	bl, bl ; head  = 0

	mov	cx, [di+free_space.start]

	; 06/03/2019
	;or	ax, ax
	;jnz	short cpp_6

	;and	cx, cx  ; start cylinder = 0 ?
	;jnz	short cpp_6

	or	ax, cx
	;jnz	short cpp_6
	jz	short cpp_24 ; 31/10/2020

	; 31/10/2020
	mov	al, [sectors]
	mul	byte [heads]
	mul	cx
	; dx:ax = LBA sector address
	; bl = head = 0
	; cx = cylinder number
	; ax = sector number
	jmp	short cpp_6
cpp_24:
	mov	ax, [sectors]
	; cylinder 0, head 1, sector 1 (LBA = 17 or 63)
	inc	bl  ; head = 1
	xor	dx, dx ; 0
cpp_6:
	; 31/10/2020
	mov	[si+ptStartSector], ax
	mov	[si+ptStartSector+2], dx

	mov	[pp_StartSector], ax
	mov	[pp_StartSector+2], dx

	or	cx, cx  ; start cylinder ?
	jnz	short cpp_25 ; > 0
	
	;and	bl, bl  ; head = 0 ?
	;jz	short cpp_25

	; cylinder = 0, head = 1, dx:ax = [sectors]

	;sub	[pp_Sectors], ax
	;sbb	[pp_Sectors+2], cx ; 0

	sub	[ppn_Sectors], ax
	sbb	[ppn_Sectors+2], cx ; 0
cpp_25:
	mov	ax, cx
	; 29/10/2020
	mov	byte [si+ptBeginSector], 1 ; sector = 1	
	jmp	short cpp_8
cpp_7:
	; 18/02/2019

	; [wholedisk] = 0

	; Fix partition size for MSDOS 3.3 (Retro DOS 3.0) compatibility.
	; (DOS partition size will be changed -down- to 65535 sectors,
	; if it is 65536 sectors.)
	
	call	fix_32mb_dos_psize
		; bx:cx = [ppn_sectors] = partition size
		; dx:ax = start sector's LBA 

	;cylinder = LBA / (heads_per_cylinder * sectors_per_track)
	;temp = LBA % (heads_per_cylinder * sectors_per_track)
	;head = temp / sectors_per_track
	;sector = temp % sectors_per_track + 1
	
	; Convert LBA sector address to CHS parameters
	mov	cx, [sectors]
	call	div32
	; BX = Sector number - 1
	inc	bl ; sector number (1 based)
	mov	[si+ptBeginSector], bl	
	; DX_AX = cylinders * heads + head
	mov	cx, [heads]
	call	div32
	; ax = cylinder
	; bl = head
cpp_8:
	; 24/10/2020
	;mov	bh, 1  ; [si+ptBeginSector]
	cmp	ax, 1023  ; CHS limit
	jna	short cpp_18	
	; > CHS limit
	mov	ax, 1023 ; cylinder
	mov	bl, 0FEh ; 254 (head)
	;mov	bh, 3Fh ; 63 (sector)
	; 26/10/2020
	mov	byte [si+ptBeginSector], 3Fh
cpp_18:
	; BL = Head number
	mov	[si+ptBeginHead], bl
	; AX = Cylinder number
	;and	ax, 1023
	mov	[si+ptBeginCylinder], al
	shl	ah, 6
	; 24/10/2020
	;or	ah, bh	; bh = sector number
	; 26/10/2020
	or	[si+ptBeginSector], ah
	;mov	[si+ptBeginSector], ah

	; clear active partition flag (for now)
	;mov	byte [si+ptBootable], 0 ; not bootable/active partition
	mov	byte [si], 0 ; not bootable/active partition

	;mov	di, [c_fspc_offset]

	cmp	byte [cylinder_boundary], 'Y'
	jne	cpp_11

	mov	cl, [heads]
	mov	al, [sectors]
	mov	ch, al
	mul	cl	
	; ax = heads*sectors

	mov	bx, [di+free_space.end]	; end cylinder of the partition

	cmp	byte [wholedisk], 0 ; entire free space ?
	ja	short cpp_10

	mov	bx, ax
	mov	ax, [ppn_Sectors]
	mov	dx, [ppn_Sectors+2]
	add	ax, [pp_StartSector]
	adc	dx, [pp_StartSector+2]
	sub	ax, 1
	sbb	dx, 0
		; dx:ax = end sector
	div	bx
		; ax = cylinder number
	; 31/10/2020
	;and	dx, dx
	;jz	short cpp_9
	;inc	ax ; + 1 (because of remainder > 0)
cpp_9:	
	xchg	ax, bx
		; bx = end cylinder
		; ax = heads*sectors
	; free space limit check
	cmp	bx, [di+free_space.end]
	jna	short cpp_10 ; ok
	; end cylinder is out of free space
	dec	bx  ; decrease end cylinder number
cpp_10: 
	mul	bx
		 ; dx:ax = (end cylinder)*heads*sectors
	push	dx ; **
	push	ax ; *

	; 24/10/2020
	cmp	bx, 1023  ; CHS limit
	jna	short cpp_19	
	; > CHS limit
	mov	bx, 1023 ; cylinder
	;mov	cl, 0FFh ; 255 (head+1)
	;mov	ch, 3Fh ; 63 (sector)
	mov	cx, 3FFFh
cpp_19:
	dec	cl ; heads - 1 = end head

	mov	[si+ptEndHead], cl
	; 26/10/2020
	;mov	al, [sectors]
	; 31/10/2020
	mov	al, ch
	mov	[si+ptEndCylinder], bl
	;mov	bl, al
	;shl	bh, 6 ; shift high bytes (2 bits) of end cyl num
	;	      ; to 6 bits left	
	;or	bh, bl ; combine high bits of end cyl num and end sector
	shl	bh, 6
	or	bh, ch ; ch = end sector (= sectors)
	mov	[si+ptEndSector], bh

	; 24/10/2020
	;mov	bl, [sectors]
	;xor	bh, bh ; clear bh
	mov	bx, [sectors] ; 17 or 63

	dec	bl ; sectors - 1 ; 22/02/2019
	mul	cl ; sectors * [end head]
	pop	dx ; *
	; 22/02/2019
	xor	cx, cx
	add	ax, dx
	pop	dx ; **
	adc	dx, cx ; 0
	add	ax, bx
	adc	dx, cx ; 0
	; dx:ax = ((([end cylinder]*heads)+[end head])*sectors)+sectors-1

	; calculate aligned partition size in sectors
	mov	cx, ax
	mov	bx, dx
	add	cx, 1
	adc	bx, 0	
	sub	cx, [si+ptStartSector]
	sbb	bx, [si+ptStartSector+2]
		; bx:cx = partition size
	;mov	[ppn_Sectors], cx
	;mov	[ppn_Sectors+2], bx
	;jmp	cpp_16
	jmp	cpp_15 ; 06/03/2019
cpp_11:
	; 20/02/2019
	mov	cx, [ppn_Sectors]
	mov	bx, [ppn_Sectors+2]

	;;mov	ax, [di+free_space.startsector]
	;;mov	ax, [di+free_space.startsector+2]
	;mov	ax, [si+ptStartSector]
	;mov	dx, [si+ptStartSector+2]
	mov	ax, [pp_StartSector]
	mov	dx, [pp_StartSector+2]
cpp_12:	
	; 18/02/2019

	; [wholedisk] = 0

	; Fix partition size for MSDOS 3.3 (Retro DOS 3.0) compatibility.
	; (DOS partition size will be changed -down- to 65535 sectors,
	; if it is 65536 sectors.)

	call	fix_32mb_dos_psize
		; bx:cx = [ppn_sectors] = partition size
		; dx:ax = start sector's LBA

	add	ax, cx
	adc	dx, bx

	; Convert LBA sector address to CHS parameters
	sub	ax, 1 ; locate on to end sector of the partition
	sbb	dx, 0

	; 06/03/2019
	cmp	byte [cylinder_boundary],'Y'
	;jne	short cpp_15
	jne	short cpp_14

	mov	cx, ax
	mov	al, [heads]
	mul	byte [sectors]
	mov	di, ax ; [heads]*[sectors]
	xchg	ax, cx
		; cx = heads*spt

	call	div32
		; dx = 0
		; ax = end cylinder
		; bx = remainder	
	;and	bx, bx
	;jz	short cpp_13
	;inc	ax
;cpp_13:
	;cmp	ax, [cylinders]
	;jb	short cpp_14
	;dec	ax
;cpp_14:

	; 26/10/2020
	cmp	ax, 1023
	jna	short cpp_20

	mov	byte [si+ptEndHead], 0FEh
	mov	byte [si+ptEndSector], 0FFh
	mov	byte [si+ptEndCylinder], 0FFh
	jmp	short cpp_21
cpp_20:
	mov	bx, [heads]
	dec	bl
	mov	[si+ptEndHead], bl
	mov	cx, [sectors]
	mov	dl, ah
	shl	dl, 6
	or	dl, cl
	mov	[si+ptEndSector], dl
	mov	[si+ptEndCylinder], al	
	;mul	di ; [cylinders]*[heads]*[sectors]
	;;sub	di, cx ; ([heads] - 1) * [sectors]
	;add	ax, di
	;adc	dx, 0
	;;add	ax, cx
	;;adc	dx, 0
cpp_21:
	inc	ax
	mul	di  ; result = start LBA of next cylinder
	; dx:ax = end sector LBA + 1 (as cyl. boundary aligned)
	sub	ax, [pp_StartSector]
	sbb	dx, [pp_StartSector+2]

	mov	cx, ax
	mov	bx, dx
	;jmp	short cpp_16
	jmp	short cpp_15
;cpp_15:
cpp_14:
	mov	cx, [sectors]
	call	div32
	; BX = Sector number - 1
	inc	bl ; sector number (1 based)
	mov	[si+ptEndSector], bl	
	; DX_AX = cylinders * heads + head
	mov	cx, [heads]
	call	div32
	; BX = Head number
	mov	[si+ptEndHead], bl
	; AX = Cylinder number
	;and	ax, 1023

	; 26/10/2020
	cmp	ax, 1023
	jna	short cpp_22
		
	mov	byte [si+ptEndHead], 0FEh
	mov	byte [si+ptEndSector], 0FFh
	mov	byte [si+ptEndCylinder], 0FFh
	jmp	short cpp_23
cpp_22:
	mov	[si+ptEndCylinder], al
	; 18/02/2019
	shl	ah, 6
	or	[si+ptEndSector], ah
cpp_23:
	mov	cx, [ppn_Sectors]
	mov	bx, [ppn_Sectors+2]
;cpp_16:
cpp_15:
	mov	[si+ptSectors], cx
	mov	[si+ptSectors+2], bx

	; set partition ID after checking DOS FAT limits
	call	set_partition_id
	; al = partition ID

	mov	[pp_type], al

	mov	[si+ptFileSystemID], al

	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; decrease DOS partition size when it is (exact) 65536 sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 18/02/2019

fix_32mb_dos_psize:	; call this if [wholedisk] = 0

; Purpose: 
;	If a DOS partition's size is 65536 sectors
;	MSDOS 3.3 can not use it. (FAT 16 partition ID = 06h)
;	Decreasing partition size to 65535 sectors will ensure
;	MSDOS 3.3 compatibility (FAT 16 partition ID will be 04h).

	; INPUT:
	;    BX:CX = partition size in sectors
	;    [pp_type] = partition type dos, non-dos, user input
	;
	; OUTPUT:
	;    Partition size will be changed to 65535 sectors, if
	;    	- partition size is 65536 sectors and
	; 	- [pp_type] is 1 (DOS)
	;
	;    [ppn_Sectors] = 65535 (if it will be changed)
	;
	; (Modified registers: cx, bx) 

	;mov	cx, [ppn_Sectors]
	;mov	bx, [ppn_Sectors+2]

	cmp	byte [pp_type], 1 ;  DOS partition
	jne	short psfx_0  ; non-dos partition or user input
			      ; nothing to do ! 	
	or	cx, cx
	jnz	short psfx_0 ; <> 65536 sectors
	cmp	bx, 1
	jne	short psfx_0
	dec	bx
	;mov	[wholedisk], bx ; 0
	dec	cx  ; bx:cx = 65535
	mov	[ppn_Sectors], cx
	mov	[ppn_Sectors+2], bx
psfx_0:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; create extended dos partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 24/10/2020 (fdisk3.s)

; 16/02/2019 (hdimage.s) 
;	(This procedure must be called after 'find_part_free_space')

create_extended_partition:  
	; 15/02/2019

	; INPUT:
	;	none 
	;  (CHS parameters and free space calculation result will be used.) 
	;
	; OUTPUT:
	;	Partition table in MBR buffer will be updated.
	;
	; (Modified registers: ax, bx, cx, dx, si, di)

	; Create extended dos partition, make partition table entry
	; (Extended partition will be created on cylinder bounds.)

	; 16/02/2019
	call	get_first_free_pte
		 ; CX = First free PTE number, 0 to 3 
		 ;    (CX = 3 if there is not a free PTE, and CF = 1)
		 ;    ((But CF = 1 is not possible here because pcount < 4))
 	 	 ; SI = PTE addres/offset in MBR buffer

	;mov	si, MasterBootBuff+pTableOffset
	;shl	cl, 4 ; * 16
	;add	si, cx
	
	mov	bx, [c_fspc_offset]
	
	mov	ax, [bx+free_space.start]

	; 24/10/2020
	mov	di, ax
	;mov	cx, 1
	mov	cl, 1 ; head (ch) = 0, sector (cl) = 1

	cmp	ax, 1023
	jna	short cep_4

	; partition start  > CHS limit
	mov	ax, 1023
	;mov	ch, 254
	;mov	cl, 63
	mov	cx, 0FE3Fh
cep_4:	
	;mov	byte [si+ptBootable], 0 ; not bootable/active partition
	;mov	[si], ch ; 0 ; not bootable/active partition
	; 26/10/2020
	mov	byte [si], 0
	mov	[si+ptBeginHead], ch
	shl	ah, 6
	or	cl, ah
	mov	[si+ptBeginSector], cl ; Sector 1 
	mov	[si+ptBeginCylinder], al

	mov	al, [heads]
	mul	byte [sectors]
		; ax = heads*sectors
	mul	di  ; AX * cylinder count before start cylinder
		; DX:AX = LBA of cylinder DI, head 0, sector 1 

	mov	[si+ptStartSector], ax
	mov	[si+ptStartSector+2], dx

	; This is not needed for extended partition.
	; 16/02/2019
	;mov	[pp_StartSector], ax
	;mov	[pp_StartSector+2], dx

	; 16/02/2019
	cmp	byte [wholedisk], 0
	ja	short cep_2 ; all of free space will be used
			    ; ([bx+free_space.end] will be end cyl)	

	; calculate cylinder count (from partition size input)
	mov	al, [heads]
	mul	byte [sectors]
	mov	cx, ax
	mov	ax, [ppn_Sectors]
	mov	dx, [ppn_Sectors+2]
	call	div32
		; ax = cylinders
		; bx = remainder
	and	bx, bx
	jz	short cep_1

	inc	ax  ; round up
cep_1:
	; 16/02/2019
	; DI  = extended partition's start cylinder
	mov	dx, ax ; cylinder count
	add	dx, di ; result is end cyl + 1
	dec	dx ; decrease number for current end cylinder
	mov	bx, [c_fspc_offset]
	cmp	dx, [bx+free_space.end]
	jna	short cep_3
	; 24/10/2020
	;dec	ax ; decrease cylinder count down to the limit
	dec	dx ; decrease end cylinder down to the limit
	jmp	short cep_3 	
cep_2:
	mov	dx, [bx+free_space.end]
	;mov	ax, [bx+free_space.space]
cep_3:
	; 24/10/2020
	mov	ax, dx ; end cylinder
	mov	bl, 05h	; Extended Partition ID (CHS)
	cmp	ax, 1023
	jna	short cep_5

	; partition end > CHS limit
	mov	ax, 1023
	;mov	ch, 254
	;mov	cl, 63
	mov	cx, 0FE3Fh
	mov	bl, 0Fh	; Extended Partition ID (LBA)
	jmp	short cep_6
cep_5:
	mov	ch, [heads]
	dec	ch 
	mov	cl, [sectors]
cep_6:
	mov	[si+ptEndHead], ch
	; 24/10/2020
	; ax = (chs limit compatible) end cylinder (<= 1023)
	; 23/02/2019
	;mov	bl, dh
	;shl	bl, 6
	;or	bl, cl
	shl	ah, 6
	or	cl, ah
	;mov	[si+ptEndSector], bl
	mov	[si+ptEndSector], cl	
	;mov	[si+ptEndCylinder], dl
	mov	[si+ptEndCylinder], al
	; dx = (lba compatible) end cylinder (up to 65535)

	;mov	al, [heads]
	; 26/10/2020
	;;mul	cl
	;mul	byte [sectors]
	mov	ch, [heads]
	mov	al, [sectors]
	mul	ch	
	; ax = heads*sectors	
	mul	dx ; AX * cylinder count before end cylinder
		; DX:AX = LBA of cylinder DX, head 0, sector 1
	push	dx
	push	ax
	mov	al, ch
	dec	al ; 26/10/2020 ; [heads] - 1
	mov	cx, [sectors]  ; 63 or 17
	mul	cl
		; ax = (heads-1)*sectors
	pop	dx
	add	ax, dx
	pop	dx
	adc	dx, 0
		; dx:ax = ((end cylinder)*heads)+(heads-1)*sectors
	add	ax, cx
	adc	dx, 0
	; dx:ax = (((end cylinder)*heads)+(heads-1)*sectors) + sectors
	; dx:ax = LBA of the partition's end sector + 1
	sub	ax, [si+ptStartSector]
	sbb	dx, [si+ptStartSector+2] 

	mov	[si+ptSectors], ax
	mov	[si+ptSectors+2], dx

	;mov	[pp_type], al

	;mov	byte [si+ptFileSystemID], 5
	; 24/10/2020
	mov	 [si+ptFileSystemID], bl ; 05h or 0Fh
	
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set DOS (and non-dos) partition ID according to partition size
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 23/10/2020

set_partition_id: 
	; 23/10/2020 (fdisk3.s)
	; input:
	;   si = partition table offset
	;   bx:cx = partition size

	; 18/02/2019 (hdimage.s)
	;mov	cx, [ppn_Sectors]
	;mov	bx, [ppn_Sectors+2]

	mov	al, [pp_type]

	;cmp	byte [pp_type], 1
	cmp	al, 1 ; primary DOS (FAT12, FAT16, FAT32)
	jne	short spid_2 ; singlix, runix or user type 

	;mov	al, 1	; FAT12

	or	bx, bx
	jnz	short spid_1

	cmp	cx, 32680
	;jna	short spid_8	; FAT12 file system
	jna	short spid_19 ; 23/10/2020

	mov	al, 4	; FAT16 (< 32MB)

	jmp	short spid_8	; FAT16 (CHS) file system

spid_1:
	mov	al, 0Bh ; FAT32 (CHS)

	cmp	bx, 10h
	jnb	short spid_8	; FAT32 (CHS) file system

	mov	al, 6	; FAT16 (>= 32MB)

	jmp	short spid_8	; FAT16 big (CHS) file system

spid_2:
	;cmp	byte [pp_type], 2 ; Singlix FS
	cmp	al, 2
	jne	short spid_3
	mov	al, 0A1h
	;jmp	short spid_8
	retn	; 23/10/2020
spid_3:
	;cmp	byte [pp_type], 3 ; Retro Unix FS
	cmp	al, 3
	jne	short spid_4
	mov	al, 71h	
	;jmp	short spid_8
spid_19:
	retn

spid_4:
	; another partition type (user input)
	
	; [pp_type] = 4

	mov	al, [pp_type_user]

	; Check FAT12 fs size validation

	cmp	al, 1 ; FAT12
	ja	short spid_9

	and	bx, bx
	jnz	short spid_6

	cmp	cx, 32680
	;jna	short spid_8
	jna	short spid_19 ; 23/10/2020 
spid_5:
	mov	al, 4 ; FAT16 (<= 32MB) 
	jmp	short spid_8
spid_6:
	;;cmp	bx, 10h	; 512MB (16*32MB)
	; 15/03/2021
	cmp	bx, 40h ; 2GB (64*32MB)
	jnb	short spid_7
	
	mov	al, 6
	jmp	short spid_8
spid_7:
	mov	al, 0Bh ; FAT32 (CHS) partition
spid_8:
	; 23/10/2020
	; check the partition's end sector against CHS limit and
	; convert partition ID if the partition overs CHS limit
	;cmp	al, 1  ; FAT12 fs
	;je	short spid_19 ; nothing to do (for FAT12 fs)
	add	cx, [si+ptStartSector]
	adc	bx, [si+ptStartSector+2]
	cmp	bx, [chs_limit+2] ; 07/11/2020
	jb	short spid_19 ; partition's end sector is in CHS limit 
	ja	short spid_17 ; out of CHS limit
	cmp	cx, [chs_limit]
	jna	short spid_19 ; partition's end sector is in CHS limit
spid_17:
	; out of CHS limit, partition ID conversion is needed
	cmp	al, 0Bh	; FAT32 CHS
	jb	short spid_18
	mov	al, 0Ch	; FAT 32 LBA
	retn
spid_18:
	mov	al, 0Eh ; FAT16 LBA
	retn
spid_9:
	cmp	al, 4 ; FAT16 (< 32 MB)
	jne	short spid_10

	or	bx, bx
	jnz	short spid_6

	; 15/03/2021
	cmp	cx, 4150 ; 4085 + 32 + 32 + 1
	jb	short spid_12
	
	;retn	; FAT16 (< 32 MB) partition
	jmp	short spid_8  ; 23/10/2020

spid_10:
	cmp	al, 6 ; FAT 16 big partition
	jne	short spid_13 ; Extended partition or another type
	
	and	bx, bx
	jz	short spid_11

	;cmp	bx, 10h	; 512MB (16*32MB)
	; 15/03/2021
	cmp	bx, 40h ; 2GB (64*32MB)
	jnb	short spid_7

	;retn
	jmp	short spid_8 ; 23/10/2020

spid_11:
	;cmp	ax, 4150 ; 4085 + 32 + 32 + 1
	cmp	cx, 4150 ; 14/09/2020 (BugFix)
	jnb	short spid_5
spid_12:
	mov	al, 1 ;  FAT12 partition
spid_16:
	retn
spid_13:
	; 14/09/2020
	cmp	al, 0Bh ; FAT 32 (CHS) partition
	je	short spid_15 ; FAT 32 CHS
	cmp	al, 0Ch	 
	jne	short spid_14 
	; FAT 32 LBA
	and	bx, bx	; < 33 MB
	jz	short spid_11 ; force to FAT 16 CHS or FAT 12
	cmp	bx, 10h ; 512 MB
	;jnb	short spid_8  ; OK, FAT 32 LBA has been confirmed
	jnb	short spid_16 ; 23/10/2020
	mov	al, 0Eh ; force to FAT 16 LBA
	retn
spid_14:
	; 14/09/2020
	cmp	al, 0Eh
	;jne	short spid_8 ; non-dos or extended partition
	jne	short spid_16 ; 23/10/2020
	; FAT 16 LBA
	or	bx, bx
	jz	short spid_11
	; 23/10/2020
	cmp	bx, 20h ; 1 GB
	jb	short spid_16
	mov	al, 0Ch	; FAT 32 LBA
	retn
spid_15:
	; Minimum size of FAT 32 FS = 65525 + 512 + 512 + 32
 	; >= 66581 sectors (or >= 65525 data clusters)
	cmp	bx, 1
	jb	short spid_11  ; invalid! (< 33 MB)
	ja	short spid_8
	cmp	cx, 1045
	;jb	short spid_16  ; invalid! (< 33 MB)
	;retn
	;jmp	short spid_8 ; 23/10/2020
	jnb	short spid_8
;spid_16:
	mov	al, 6
	;retn
	jmp	short spid_8 ; 23/10/2020

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 32 bit division by using 16 bit registers
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
; 32 bit multiplication by using 16 bit registers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

mul32:
	; DX_AX*CX
	; Result: BX_DX_AX 
	push	cx
	mov	bx, dx
	mul	cx
 	xchg	ax, bx
	push	dx
	mul	cx 
	pop	cx 
	add	ax, cx 
	adc	dx, 0
	xchg	bx, ax
	xchg	dx, bx
	pop	cx
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; creeate new partition input menu
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

B_01:
	; 06/03/2019
	mov	byte [pSize_unit], 'M' ; default (for 'whole' disk/space)
	; 15/02/2019
	call	create_partition_input
B_02:		
	and	ax, ax
	;jz	_crlf_exit ; 0 = none or not a valid input
	jz	A_42 ; 25/02/2019

	;or	ah, ah
	;jz	short B_03

	; 23/02/2019
	cmp	ah, 4  ; user's partition type input ?
	jne	short B_03 ; no

	; 29/10/2020
	; (ah = 0)
	;or	al, al
	;jz	A_42  ; invalid partition type input
	;	      ; or ESC key has been pressed

	; user type input
	mov	[pp_type_user], al
	mov	al, ah ; mov al, 4
B_03:
	mov	[pp_type], al

	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	mov	al, [pp_type]
	cmp	al, 1
	ja	short B_04

	mov	si,  msg_create_dos_partition_h ; header
	jmp	short B_09
B_04:
	cmp	al, 5
	jb	short B_08
	;ja	short B_07
	ja	short B_58 ; 26/10/2020

	; 23/02/2019
	mov	si, msg_create_ext_partition_h
	call	print_msg

	; 24/02/2019
	cmp	byte [epnumber], 0
	jna	short B_05	

	mov	si, msg_ext_partition_exists
	call 	print_msg

	mov	si, msg_press_any_key
	call	print_msg

	xor	ah, ah
	int	16h

	jmp	A_42
B_05:
	cmp	byte [ppcount], 0  ; primary partition count
	ja	short B_10

	; 09/02/2019
	mov	si, msg_ext_partition_error
	call	print_msg
B_06:	
	mov	si, msg_press_any_key
	call	print_msg

	xor	ah, ah
	int	16h

	mov	byte [pp_type], 1

	call	cpi_2 ; 15/02/2019
	jmp	short B_02

B_58:
	; 26/10/2020
	cmp	byte [DrvNum], 80h ; hd0 ?
	ja	short B_59 ; Do not check primary dos partition count
	; The second hard disk may contain extended partition without
	; a primary dos partition
	cmp	byte [ppcount], 0  ; primary dos partition count
	jna	short B_07
B_59:
	cmp	byte [epnumber], 0
			; is there an extended partition ?
	jna	short B_07 ; no
	jmp	create_logical_drives
B_07:
	mov	si, msg_create_logical_drive_h
	call	print_msg

	mov	si, msg_logical_drive_error
	call	print_msg
	jmp	short B_06

;	mov	si, msg_use_entire_ep_space
;	jmp	short B_12
	
B_08:
	mov	si, msg_create_nondos_partition_h
B_09:
	call	print_msg
B_10:
	; 15/02/2019
	;cmp	byte [newdisk], 0
	;ja	short B_11

	cmp	byte [pcount], 0 ; (valid) partition count
	jna	short B_11

	mov	si, msg_use_all_space
	jmp	short B_12
B_11:
	mov	si, msg_use_whole_disk ; partition size: whole disk
B_12:
	call	print_msg
B_13:
	xor	ah, ah
	int	16h

	cmp	al, 27 ; ESCAPE key
	;;je	_crlf_exit
	je	A_42 ; 25/02/2019
	and	al, 0DFh
	cmp	al, 'N'
	je	short B_14  ;02/03/2019
	cmp	al, 'Y'
	jne	short B_13
	
	inc	byte [wholedisk]
	
	; 02/03/2019
	mov	si, _msg_YES
	;call	print_msg
	jmp	short B_15	
B_14:
	mov	si, _msg_NO
B_15:	; 06/03/2019
	call	print_msg

	; 15/02/2019
	sub	bx, bx
	;cmp	byte [newdisk], bl ; 0
	;ja	short B_16 ; 23/02/2019
	; 19/10/2020
	;cmp	byte [pcount], 0
	; 03/11/2020
	cmp	byte [pcount], bl ; 0
	jna	short B_16 ; [pcount] = 0 (newdisk, empty pt) 
	cmp	byte [wholedisk], bl ; 0
	ja	B_27 ; 23/02/2019
B_16:
	; 08/02/2019
	;sub	bx, bx
	mov	ax, [total_sectors]
	mov	dx, [total_sectors+2]
	sub	ax, [sectors]
	sbb	dx, bx ; sbb dx, 0

	mov	[pp_Sectors], ax   ; = [total_sectors] - [sectors]
	mov	[pp_Sectors+2], dx ; = [total_sectors+2] - carry bit
B_17:
	cmp	byte [wholedisk], bl ; 0
	ja	B_28 ; 09/02/2019
B_18:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	; 04/11/2020
	cmp	byte [pp_type], 1 ; primary dos partition ?
	je	short B_61
	; "Create Partition"
	mov	si, msg_create_partition_h
	jmp	short B_62
B_61:
	; "Create DOS partition"
	mov	si, msg_create_dos_partition_h ; header
B_62:
	call	print_msg
	mov	si, msg_create_trdos_partition_s ; size options
	call	print_msg	
B_19:	
	xor	ah, ah
	int	16h

	; 24/02/2019
	cmp	al, 27	; ESCAPE key 
	je	A_42	; Cancel

	cmp	al, 32	; SPACE key
	jne	short B_21

		; Entire space
	mov	ax, [pp_Sectors]
	mov	dx, [pp_Sectors+2]

	mov 	byte [wholedisk], 1 ; 09/02/2019
	jmp	short B_23

B_60:	; 31/10/2020
	jnz	short B_18  ; ESCape (return to options menu)
	; partition size input is ZERO !
B_20:
	; ZERO partition size message
	mov	si, msg_zero_partition_size
	call	print_msg
	
	xor	ah, ah
	int	16h
	cmp	al, 27 ; ESCAPE key
	jne	short B_18 ; Retry

	; 19/10/2020
	jmp	_crlf_exit ; exit

B_21:
	mov	byte [pSize_unit], '%'
	cmp	al, '%'
	je	short B_22
	mov	byte [pSize_unit], 'S'
	cmp	al, 13 ; 0Dh, Carriage Return key
	je	short B_22
	and	al, 0DFh
	cmp	al, 'S'
	je	short B_22
	mov	[pSize_unit], al
	cmp	al, 'K'
	je	short B_22
	cmp	al, 'M'
	je	short B_22
	cmp	al, 'G'
	je	short B_22
	cmp	al, 'C'
	jne	short B_19
B_22:
	mov	byte [msg_sectors_crlf_s], 's' ; " sectors"

	call	partition_size_input
	;jc	_crlf_exit ; exit if partition size input is 0
	;jc	short B_20
	; 29/10/2020
	;jnc	short B_60
	;jz	short B_20 ; partition size input is ZERO !
	;jmp	short B_18 ; ESCape (return to options menu)
	; 31/10/2020
	jc	short B_60 ; ESC or zero partition size
;B_60:
	and	bx, bx ; bx_dx_ax = partition size
	;jnz	short B_29
	; 23/02/2019 
	jnz	short B_24 ; invalid! (use maximum available sectors)

	; 08/02/2019
	or	dx, dx
	jnz	short B_23 ; proper size (for now)

	mov	bx, [min_sectors] ; minimum sectors
	cmp	bx, ax
	jna	short B_23 ; proper size (for now)	 

	; invalid! (use minimum sectors or sectors per cylinder)
	mov	ax, bx
B_23:
	; 09/02/2019
	; save dx,ax
	mov	[ppn_Sectors+2], dx
	mov	[ppn_Sectors], ax

	; write partition size
	;push	dx
	;push	ax
	;mov	si, msg_partition_sectors + 7 ; max. 8 digits
	; 06/11/2020
	mov	si, msg_partition_sectors + 10 ; max. 11 digits
	call	bin_to_decimal
	; ds:si = beginning of decimal number text
	call	print_msg
	mov	si, msg_sectors_crlf
	call	print_msg
	;pop	ax
	;pop	dx

	cmp	byte [wholedisk], 0
	ja	short B_29 ; 09/02/2019

	; restore ax,dx
	mov	ax, [ppn_Sectors]
	mov	dx, [ppn_Sectors+2]

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; select whole disk
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; select whole disk 
	;   if partition size > disk size
	cmp	dx, [pp_Sectors+2]
	ja	short B_24
	jb	short B_29
	cmp	ax, [pp_Sectors]
	jna	short B_29
B_24:
	;cmp	byte [newdisk], 0
	;jna	short B_30
	; 19/10/2020
	cmp	byte [pcount], 0
	ja	short B_30

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; "use whole disk or go to back" question
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, msg_partition_size_overs
B_25:
	call	print_msg
B_26:
	xor	ah, ah
	int	16h
	
	cmp	al, 27 ; ESCAPE key
	;je	B_01
	je	B_18 ; 09/02/2019
	cmp	al, 13 ; ENTER (Carriage Return) key
	jne	short B_26

	inc	byte [wholedisk]

	; 24/02/2019
	;cmp	byte [newdisk], 0
	;ja	short B_27
	cmp	byte [pcount], 0  ; (valid) partition count
	jna	short B_27 ; no

	mov	bx, [c_fspc_offset]
	mov	ax, [free_space.sectors_unused+bx]
	mov	dx, [free_space.sectors_unused+bx+2]
	mov	[pp_Sectors], ax
	mov	[pp_Sectors+2], dx
	jmp	short B_28 
B_27:
	; 09/02/2019
	mov	dx, [pp_Sectors+2]
	mov	ax, [pp_Sectors]
B_28:
	mov	[ppn_Sectors+2], dx
	mov	[ppn_Sectors], ax
B_29:
	cmp	byte [pp_type], 5
	jne	B_51

	cmp	byte [ppcount], 0  ; primary partition count
	ja	B_45

	mov	si, msg_ext_partition_error
	call	print_msg
	mov	si, msg_any_key_to_retry
	call	print_msg

	; 09/02/2019
	xor	ah, ah
	int	16h

	;jmp	B_01
	jmp	cpi_2 

B_30:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	mov	si, msg_create_dos_partition_h ; header
	call	print_msg

	mov	si, msg_partition_size_limit
	call	print_msg

	mov	bx, [c_fspc_offset]

	cmp	byte [pSize_unit], 'S'
	je	short B_31
	cmp	byte [pSize_unit], 'K'
	je	short B_31

	cmp	byte [pSize_unit], '%'
	je	short B_41

	cmp	byte [pSize_unit], 'C'
	je	B_44
B_31:
	; 'S', 'K'
	add	bx, free_space.sectors_unused	
	mov	ax, [bx]
	mov	dx, [bx+2]
		
	mov	cx, 10
	mov	bp, sp
B_32:
	call	div32
	push	bx
	or	dx, dx
	jnz	short B_32
	cmp	ax, 9
	ja	short B_32
B_33:
	mov	bx, 07h
	or	ax, ax
	jnz	short B_35
B_34:
	pop	ax
B_35:
	add	al, '0'
B_36:
	mov	ah, 0Eh
	;mov	bx, 07h
	int	10h	; write character (as tty)

	cmp	sp, bp
	jb	short B_34

	cmp	byte [pSize_unit], 'S'
	je	short B_38
	cmp	byte [pSize_unit], 'K'
	je	short B_38

	cmp	byte [pSize_unit], '%'
	jne	short B_37

	; 24/02/2019
	cmp	al, '%'
	je	short B_40
	mov	al, '%'
	jmp	short B_36 
B_37:
	cmp	byte [pSize_unit], 'C'
	jne	short B_38

	mov	si, msg_cylinders
	jmp	short B_39
B_38:
	mov	si, msg_sectors
B_39:
	call	print_msg
B_40:
	mov	si, msg_partition_size_limit_r
	;call	print_msg

	jmp	B_25

B_41:
	; '%'
	add	bx, free_space.percent_unused
	mov	ax, [bx]
B_42:	
	mov	bp, sp
	mov	cx, 10
B_43:	
	xor	dx, dx
	;mov	cx, 10
	div	cx
	push	dx
	cmp	ax, 9
	ja	short B_43	
	jmp	short B_33
B_44:
	; 'C'
	add	bx, free_space.space
	mov	ax, [bx]
	jmp	short B_42

B_45:
	; 23/02/2019
	mov	bx, [c_fspc_offset]
	mov	ax, [bx+free_space.start]

	; set free space start cylinder to 1 if it is 0
	or	ax, ax
	jnz	short B_46

	mov	al, 5	; Get free space for extended partition
	call	find_part_free_space

	mov	[c_fspc_offset], bx

	; 19/10/2020
	and	cx, cx
	jz	cp_3	; there is not free space on disk
B_46:
	; 24/02/2019
	call	partition_size_fixup_x
	jc	B_30

	; 16/02/2019
	call	create_extended_partition 
			; must be called after 'find_part_free_space'.
	; 24/02/2019
	call	show_selected_partition
	jmp	C_02

partition_size_fixup:
	; 24/02/2019
	mov	bx, [c_fspc_offset]
partition_size_fixup_x:
	; 19/10/2020

	;cmp	byte [newdisk], 0
	;ja	short B_50

	cmp	byte [pcount], 0 ; (valid) partition count
	jna	short B_50

	add	bx, free_space.sectors_unused
	mov	ax, [bx]
	mov	dx, [bx+2]

	cmp	dx, [ppn_Sectors+2]
	jb	short B_50 ; 24/02/2019
	ja	short B_47
	cmp	ax, [ppn_Sectors]
	jb	short B_50 ; 24/02/2019
	je	short B_49

B_47:
	; 19/02/2019
	mov	ax, [ppn_Sectors]
B_48:
	mov	dx, [ppn_Sectors+2]

	; 18/02/2019

	; check for best fit
	; (leave max. available space to next time if 
	;  another space/gap fits to partition size request.)

	call	find_enough_free_sectors
		; CX = Free space index (0 to 4)
		; DX:AX = First available space which fits to request
	;mov	bx, cx
	;shl	bl, 4 ; * 16  ; * Free space structure size
	;mov	[c_fspc_offset], bx
	
	; 22/02/2019
	shl	cl, 4
	mov	[c_fspc_offset], cx 

	;add	bx, free_space.sectors_unused
	;mov	ax, [bx]
	;mov	dx, [bx+2]
B_49:		
	; Save max. available space
	mov	[pp_Sectors], ax
	mov	[pp_Sectors+2], dx
B_50:
	retn

B_51:
	; 24/02/2019
	call	partition_size_fixup
	jc	B_30

	; 16/02/2019
	; Force cylinder boundary alignment except
	;   sectors ('S') and kilobytes ('K') as sizing unit.

	mov	byte [cylinder_boundary], 'Y' ; Default
				; sizing unit is one of
				; 'cylinders','megabytes','gigabytes'
				; and boundary alignment is yes for them.
	mov	al, [pSize_unit]

	;cmp	byte [pSize_unit], 'S' ; Sectors
	cmp	al, 'S'
	je	short B_52

	;cmp	byte [pSize_unit], 'K' ; Kilobytes
	cmp	al, 'K'
	jne	short B_56
B_52:
	; Sizing unit is one of 'sectors' and/or 'kilobytes'
	; and bound aligment will be applied if the answer will be yes.

	mov	si, msg_cylinder_boundary_set
	call	print_msg
B_53:
	xor	ah, ah
	int	16h
	; Cylinder boundary adjusting
	cmp	al, 13 ; ENTER key
	je	short B_52
	cmp	al, 27 ; ESCAPE key
	je	short B_55	; Align end of the partition only
				; if start of the partition is aligned
				; or it starts at cyl 0, head 1, sect 17 or 63.
				; (End of the partition will be extended to
				; end sector of it's last cylinder if the size
				; will not over [pp_Sectors] value.)
	and	al, 0DFh

	; 22/02/2019
	cmp	al, 'Y'
	jne	short B_54

	mov	si, _msg_YES
	call	print_msg

	;mov	al, 'Y'
	;jmp	short B_55
	jmp	short B_56
B_54:
	cmp	al, 'N'
	jne	short B_53

	mov	si, _msg_NO
	call	print_msg

	mov	al, 'N'
		; do not align partition to cylinder boundary 
B_55:
	mov	[cylinder_boundary], al
B_56:
	call	create_primary_partition
	
	; 18/02/2019
	;cmp	byte [newdisk], 0
	;jna	short B_57

	; 19/10/2020
	cmp	byte [pcount], 0
	ja	short B_57

	xor	ax, ax ; 0
	xor	dx, dx ; 0
	; DX_AX = Masterboot Sector = 0
	mov	bx, MasterBootBuff
	; ES:BX = Sector Buffer
	call	write_hd_sector
	jc	print_error_code ; 18/10/2020
B_57:
	; DS:SI = Partition Table Entry address
	; 25/02/2019
	mov	[pte_address], si  ; save PTE address
	call	show_selected_partition

	;jmp	C_01

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; format question
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C_01:
	;mov	si, CRLF
	;call	print_msg

	mov	byte [format_q], 1

	mov	al, [pp_type]
	cmp	al, 1		; FAT12 (CHS)
	je	short C_03
	cmp	al, 4		; FAT16 CHS
	je	short C_03
	cmp	al, 6		; FAT16 BIG CHS
	je	short C_03
	cmp	al, 0Bh		; FAT32 CHS
	je	short C_03
	cmp	al, 0A1h	; SINGLIX FS1
	je	short C_03

	;cmp	al, 71h
	;je	short C_03	; RETRO UNIX 386

	;dec	byte [format_q] ; 0
C_02:
	mov	byte [format_q], 0 ; 24/02/2019

	; NON-DOS partition!
	; Ask for editing PT or exit (continue without formatting)
	mov	si, msg_edit_or_exit
	call	print_msg
	jmp	short C_04
C_03:
	; Ask for formatting, editing PT or exit
	mov	si, msg_format_stage
	call	print_msg
C_04:
	mov	si, msg_partition_edit
	call	print_msg
C_05:
	sub	ah, ah
	int	16h

	cmp	al, 13
	je	short C_08

	;; 07/03/2019
	cmp	al, 20h ; SPACE
	;je	A_21 ; edit partition table option is selected
	jne	short C_06
	; 19/10/2020	
	jmp	A_21
C_06:
	cmp	al, 27 ; ESCAPE
	jne	short C_05
	
	; 19/010/2020
	jmp	_exit

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; save MBR then exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

save_mbr_exit:
	; 24/02/2019
	call	init_partition_table

	call	display_partition_table

	mov	si, CRLF
	call	print_msg

	mov	si, msg_writing_mbr
	call	print_msg

	xor	ax, ax ; 0
	xor	dx, dx ; 0
	; DX_AX = Masterboot Sector = 0
	mov	bx, MasterBootBuff
	; ES:BX = Sector Buffer
	call	write_hd_sector
	jnc	short C_07
	jmp	print_error_code ; 18/10/2020

C_07:
	mov	si, Msg_OK
	;call	print_msg
	; 19/10/2020
	;jmp	_exit
	jmp	_p_exit

C_08:
	cmp	byte [format_q], 1
	jb	short save_mbr_exit

	; 25/02/2019
	; Write MBR without writing message
	
	; NOTE: This may be second time of MBR writing...
	;	but, if partition table is modified,
	;	we need to write MBR to disk, here.
	;
	; (Otherwise.. the last created partition would not be recorded.)

	xor	ax, ax ; 0
	xor	dx, dx ; 0
	; DX_AX = Masterboot Sector = 0
	mov	bx, MasterBootBuff
	; ES:BX = Sector Buffer
	call	write_hd_sector
	jc	print_error_code ; 18/10/2020

	;; clear screen
	;mov	ax, 3 ; set video mode to 03h (80x25 text)
	;int	10h

	; 25/02/2019
	mov	si, [pte_address]

	; 18/02/2019
	call	show_selected_partition	

	mov	al, [partition_num_chr]
	mov	[partition_num_txt], al

	mov	si, msg_format_question
	call	print_msg
C_09:
	xor	ah, ah
	int	16h

	cmp	al, 1Bh ; ESCAPE
	je	short C_06

	and	al, 0DFh ; capitalization
	cmp	al, 'Y'
	je	short C_10
	cmp	al, 'N'
	jne	short C_09
	mov	si, _msg_NO 
	call	print_msg
	;jmp	short C_06
	; 24/02/2019
	jmp	short save_mbr_exit 
C_10:
	mov	si, _msg_YES
	call	print_msg

	; [pp_StartSector] = partition's start sector
	; [pp_Sectors] = partition size including start & end sector
	; [partition_num_chr] = partition number + '0'
	; [pp_type] = partition type (for TRDOS 386 boot sector)

	mov	[old_sp], sp
	
	mov	dl, [pp_type]
	; DL = partition type (file system ID)
	;dec	dl
	;jz	FAT12_hd_format
	cmp	dl, 1 ; 14/09/2020 (BugFix)
	jna	FAT12_hd_format
	;cmp	dl, 4
	;je	FAT16_hd_format
	;cmp	dl, 6
	;je	FAT16_hd_format
	cmp	dl, 0Bh 
	jb	FAT16_hd_format
	;je	short FAT32_hd_format
	;cmp	dl, 0Ch ; 14/09/2020
	ja	SINGLIX_hd_format

	;cmp	dl, 0A1h
	;je	SINGLIX_hd_format
	;jb	RUNIX386_hd_format ; dl = 071h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; FAT32 FORMATTING
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
	; 08/02/2019 (Modified for -only- FAT32 CHS partitions)
FAT32_hd_format:
	;mov	ax, 000Ch ; db 0Ch, 00h ; 'or al, 0'
	;cmp	dl, al ; 0Ch
	;je	short FAT32_lbAx_format
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
	mov	word [bp+5Ah], 0C00Bh ; 08/02/2019
	mov	ax, [sectors]
	mov	[bp+18h], ax	; [BPB_SecPerTrk]
	mov	ax, [heads]
	mov	[bp+1Ah], ax	; [BPB_NumHeads]
	mov	ax, [pp_StartSector]
	mov	[bp+1Ch], ax	; [BPB_HiddSec]
	mov	ax, [pp_StartSector+2]
	mov	[bp+1Eh], ax	; [BPB_HiddSec+2]
	;mov	ax, [pp_Sectors]
	mov	ax, [ppn_Sectors] ; 16/02/2019
	mov	[bp+20h], ax	; [BPB_TotSec32]
	;mov	dx, [pp_Sectors+2]
	mov	dx, [ppn_Sectors+2] ; 16/02/2019
	mov	[bp+22h], dx	; [BPB_TotSec32+2]
	
	; Sectors per cluster calculation
	; (According to MS FAT32 FS specification.)
	mov	cl, 8  ; 8 sectors per cluster
	cmp	dx, 8  ; >= 532480 sectors
	ja	short FAT32_f_2 ; 8 sectors per cluster
	jb	short FAT32_f_1 ; 1 sector per cluster
	cmp	ax, 2000h ; dx_ax = (8*65536)+8192
	jnb	short FAT32_f_2 ; 12/09/2020 (BugFix)
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
SINGLIX_fs1_f_12:
	; End of FAT format routine...
end_of_formatting:
	mov	al, 100
	call	write_format_percent_x
	;mov	si, CRLF
	;call	print_msg
	mov	si, Msg_OK
	;call	print_msg
	;jmp	_exit
	; 19/10/2020
	jmp	_p_exit

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
	;mov	ax, [pp_Sectors]
	;mov	dx, [pp_Sectors+2]
	; 16/02/2019
	mov	ax, [ppn_Sectors]
	mov	dx, [ppn_Sectors+2]

	; DX_AX = Total sectors for percentage
	;mov	cx, 100	
	mov	cx, 256*100 ; 16/03/2021 
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
	; 16/03/2021
	; AH = percentage value between 1 to 100
wpc_x:
	;cmp	al, [prev_percent]
	;je	short wpc_y
	;mov	[prev_percent], al
	; 16/03/2021
	cmp	ah, [prev_percent]
	je	short wpc_y
	mov	[prev_percent], ah
	mov	dx, [Cursor_Pos]
	mov	bx, 7
	mov	ah, 2
	int	10h  ; Set Cursor Position
	xor	dx, dx
	xor	ah, ah
	mov	al, [prev_percent] ; 16/03/2021
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

write_fs_format_percent:
	; DX_AX = Current sector (which has been written)

	push	ax
	push	dx
	push	bx
	push	cx
	push	si

	sub	ax, [bp+0Ch]	; [bsBeginSector]
	sbb	dx, [bp+0Eh]	; [bsBeginSector+2]
	jmp	short wpc_t
	
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
	;jmp	_exit
	; 19/10/2020
	jmp	_p_exit

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
	;call	print_msg
	;retn
	; 19/10/2020
	jmp	print_msg 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; FAT16 FORMATTING
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	; 08/02/2019 (Modified for -only- FAT16 CHS partitions)
FAT16_hd_format:
; 04/05/2024 (Retro DOS v5 modification)
; DL = Partition (FS) ID
;	mov	ax, 0706h ; db 06h, 07h ; 'push es, pop es'
;	cmp	dl, al ; 06h ; Big CHS partition (>= 32MB)
;	je	short FAT16_big_chs_format
;	;mov	ax, 070Eh ; db 0Eh, 07h	; 'push cs, pop es'
;	;cmp	dl, al ; 0Eh ; LBA partition
;	;je	short FAT16_lba_format
;FAT16_chs_format:  
;	; Partition Type: 04h, CHS (<32 MB) partition
;	mov	ax, 0004h ; db 04h, 00h ; 'add al, 0'
;FAT16_big_chs_format:
;;;
;;FAT16_lba_format:
	; Put TRDOS 386 FAT16 partition magic word 
	; at offset 3Eh, in TRDOS386 FAT16 boot sector.
	mov	bp, TRDOS_FAT16_hd_bs
	lea	di, [bp+3]
	mov	si, bs_oem_name
	mov	cx, 4
	rep	movsw 

	;mov	[bp+3Eh], ax	; [loc_3E]
	; 04/05/2024 (Retro DOS v5 modification)
	cmp	dl, 6
	je	short FAT16_f_x ; skip ; db 'RDv5 FAT16 06h', 0
	; dl = 04h or 0Eh
	mov	[bp+1CEh], dl	; Retro DOS v5 boot sect off 1CEh
				; (see: 'rd5hdbs.txt' for 1CEh)
FAT16_f_x:
	mov	ax, [sectors]
	mov	[bp+18h], ax	; [BPB_SecPerTrk]
	mov	ax, [heads]
	mov	[bp+1Ah], ax	; [BPB_NumHeads]
	mov	ax, [pp_StartSector]
	mov	[bp+1Ch], ax	; [BPB_HiddSec]
	mov	ax, [pp_StartSector+2]
	mov	[bp+1Eh], ax	; [BPB_HiddSec+2]
	;mov	ax, [pp_Sectors]
	mov	ax, [ppn_Sectors] ; 16/02/2019
	;mov	dx, [pp_Sectors+2]
	mov	dx, [ppn_Sectors+2] ; 16/02/2019
	and	dx, dx
	jnz	short FAT16_f_0
	mov	[bp+13h], ax	; [BPB_TotSec16]
	; CX = 0
	;mov	[bp+20h], cx	; [BPB_TotSec32] =  0
	;mov	[bp+22h], cx	; [BPB_TotSec32+2] = 0
	jmp	short FAT16_f_1
FAT16_f_0:
	mov	[bp+20h], ax	; [BPB_TotSec32]
	;;mov	dx, [pp_Sectors+2]
	;mov	dx, [ppn_Sectors+2] ; 16/02/2019 
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
	; 11/02/2019
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
	
	; 15/07/2024 (bugfix)
	mov	[bp+42h], cx	; bsRootDirStart
	mov	bx, [root_dir_secs]
	mov	[bp+44h], bx	; bsRootDirSects
	;mov	word [bp+46h], 16 ; bsDirEntsPerSec

	; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	;add	cx, [root_dir_secs] ; + RootDirsectors
	; 15/07/2024
	add	cx, bx
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

	; 15/07/2024 (bugfix)
	mov	 [bp+40h], cx	; bsDataStart

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
	; 11/02/2019
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

	; 11/02/2019
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
	; 11/02/2019
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

FAT12_hd_format:
	mov	bp, TRDOS_FAT12_hd_bs
	lea	di, [bp+3]
	mov	si, bs_oem_name
	mov	cx, 4
	rep	movsw 
	mov	ax, [sectors]
	mov	[bp+18h], ax	; [BPB_SecPerTrk]
	mov	ax, [heads]
	mov	[bp+1Ah], ax	; [BPB_NumHeads]
	mov	ax, [pp_StartSector]
	mov	[bp+1Ch], ax	; [BPB_HiddSec]
	mov	ax, [pp_StartSector+2]
	mov	[bp+1Eh], ax	; [BPB_HiddSec+2]
	;mov	ax, [pp_Sectors]
	mov	ax, [ppn_Sectors] ; 16/02/2019
	mov	[bp+13h], ax	; [BPB_TotSec16]

	; 11/02/2019
	xor	si, si ; reset (FAT size fix) flag
	mov	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
	mov	dx, [bp+11h]	; [BPB_RootEntCnt] = 512
	add	dx, 15	; (16-1) (512-1)
	shr	dx, 4	; /16  (*32/512)
	; AX = Root dir sectors
	; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	add	cx, dx ; + RootDirsectors ; + 32
	mov	[root_dir_secs], cx ; = 33

	; 11/02/2019
	;sub	ax, 33  ; 1 reserved sector, 32 root dir sectors
			; .. now AX has number of data sectors
			;	 		+ 2* (FAT sectors)
	sub	ax, cx	
FAT12_f_10:	; 11/02/2019
	; Sectors per cluster calculation
	; (According to MS FAT32 FS specification.)
	;mov	cx, 1  ; 1 sector per cluster
	mov	cl, 1  ; CH = 0
FAT12_f_0:
	cmp	ax, 4085 ; Max. cluster count for FAT12
	jb	short FAT12_f_1
	shl	cl, 1 ; *2
	shr	ax, 1 ; /2
	jmp	short FAT12_f_0
FAT12_f_1:
	mov	[bp+0Dh], cl	 ; [BPB_SecPerClus]
	;mov	byte [bp+10h], 2 ; [BPB_NumFATs] 
	;mov	word [bp+0Eh], 1 ; [BPB_RsvdSecCnt] 
	;mov	word [bp+11h], 512 ; [BPB_RootEntCnt]
	
	; Calculating FAT size in sectors
	; AX = partition (volume) size in sectors
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

	; 11/02/2019
	;mov	cx, 33
	mov	cx, [root_dir_secs]

	; 15/07/2024 (bugfix)
	; ax = 2 * FAT size (in sectors)
	add	ax, [bp+0Eh] ; total FAT sectors + reserved sectors
	mov	[bp+42h], ax	; bsRootDirStart
	mov	[bp+44h], cx	; bsRootDirSects
	;mov	word [bp+46h], 16 ; bsDirEntsPerSec

	; 15/07/2024
	;add	cx, [bp+0Eh]	; [BPB_RsvdSecCnt] ; 1
		; cx = root directory sectors + reserved sectors
	add	cx, ax
		; cx = root dir sects + rsvd sects + total FAT sects

	; CX = [BPB_RsvdSecCnt]+([BPB_NumFATs]*[BPB_FATSz16])
	;	  + RootDirSectors
	mov	ax, [bp+13h]	; [BPB_TotSec16]
	sub	ax, cx
		 ; AX = data sectors
		 ; cH = 0

	; 11/02/2019 - fix FAT size (better method)
	or	si, si
	jnz	short FAT12_f_9

	mov	si, ax  ; ax = data sectors
	jmp	short FAT12_f_10

FAT12_f_9:
	xor	dx, dx
	mov	[data_start], cx
	mov	[data_start+2], dx ; 0
	
	; 15/07/2024 (bugfix)
	mov	 [bp+40h], cx	; bsDataStart

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
	; 11/02/2019
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

	; 11/02/2019
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
	;inc	bx ; 11/02/2019
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
; SINGLIX FS FORMATTING
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 20/03/2021
	; 19/03/2021
SINGLIX_hd_format:
	; 06/01/2018
	; 05/01/2018
	; If Sectors/Track = 17, use CHS+LBA type boot sector
	mov	bx, [sectors]
	cmp	bx, 17
	ja	short SINGLIX_fs1_f_1
SINGLIX_fs1_f_0:
	mov	bp, TRDOS_TRFS1_chs_bs
	mov	[bp+45], bh ; 0 ; [bs_LBA_Ready] = 0
	mov	[bp+46], bl	; [bs_Disk_SecPerTrack]
	mov	al, [heads]
	mov	[bp+47], al	; [bs_Disk_Heads]
	jmp	short SINGLIX_fs1_f_3
SINGLIX_fs1_f_1:
	; Sectors per Track = 63
	; If disk capacity >= 63*16*1024 use LBA type boot sector
	mov	cx, [heads]
	mov	ax, [cylinders]
	mul	cx
	and	dx, dx
	jnz	short SINGLIX_fs1_f_2
	cmp	ax, 16384
	jb	short SINGLIX_fs1_f_0
SINGLIX_fs1_f_2:	
	mov	bp, TRDOS_TRFS1_lba_bs
	;mov	byte [bp+45], 1 ; [bs_LBA_Ready] = 1
SINGLIX_fs1_f_3:	
	;mov	ax, [pp_Sectors]
	;mov	dx, [pp_Sectors+2]
	; 16/02/2019
	mov	ax, [ppn_Sectors]
	mov	dx, [ppn_Sectors+2]
	mov	[bp+16], ax	; [bsVolumeSize]
	mov	[bp+18], dx	; [bsVolumeSize+2]
	mov	cx, [pp_StartSector]
	mov	bx, [pp_StartSector+2]
	mov	[bp+12], cx	; [bsBeginSector]
	mov	[bp+14], bx	; [bsBeginSector+2]
	mov	byte [bp+44], 80h ; [bsDriveNumber]  ; hd0

	; Prepare MAT
	mov	[MAT_VolumeSize], ax ; Total Sectors of the FS
	mov	[MAT_VolumeSize+2], dx
	mov	[MAT_BeginSector], cx ; Beginning Sector of the FS
	mov	[MAT_BeginSector+2], bx
	;mov	cx, [bp+24]	; [bsMATLocation]
	;mov	bx, [bp+26]	; [bsMATLocation+2]
	;add	cx, 1
	;adc	bx, 0
	; Note: (as Default) MAT Address = 1, DAT Address = 2
	mov	cx, 2
	;xor	bx, bx ; 0
	;mov	[bp+24], cx	; [bsMATLocation]
	;mov	[bp+26], bx	; [bsMATLocation+2]
	mov	[DAT_Address], cx
	;mov	[DAT_Address+2], bx
	; DX_AX = FS1 Volume Size
	add	ax, 7 ; 7 bits more (for round up)
	adc	dx, 0
	mov	cx, 8 ;  1 DAT byte == 8 sectors 
	call	div32
	; DX_AX = DAT bytes
	mov	cx, 511
	add	ax, cx
	adc	dx, 0
	inc	cx ; 512
	call	div32
	; DX_AX = DAT sectors (DX must be 0 for volume sizes < 128GB)
	mov	[DAT_SectorCount], ax
	; 19/03/2021
	mov	[DAT_SectorCount+2], dx

	add	ax, 2  ; BS + MAT
	adc	dx, 0  ; 19/03/2021
	mov	[bp+28], ax  ; [bsRootDirDT] ; RDT address (offset)
	mov	[bp+30], dx ; 19/03/2021

	; Free Sectors = Total Sectors - (BS+MAT+DATsects+RDT+4)
	add	ax, 5 ; DATsects + (BS+MAT+RDT+4)
	; 19/03/2021
	adc	dx, 0
	;mov	dx, ax ; ? ; 19/03/2021
	mov	cx, [MAT_VolumeSize]
	mov	bx, [MAT_VolumeSize+2]
	sub	cx, ax
	; 19/03/2021
	;sbb	bx, 0
	sbb	bx, dx	

	mov	[MAT_FreeSectors], cx
	mov	[MAT_FreeSectors+2], bx
	mov	[MAT_FirstFreeSector], ax
	;mov	word [MAT_FirstFreeSector+2], 0
	; 19/03/2021
	mov	[MAT_FirstFreeSector+2], dx
	
	mov	di, fs_volume_name
	call	write_fs_volume_name
	mov	si, fs_volume_serial
	call	write_volume_serial

	; Modify FS volume name
	; (Convert 20h tail bytes to 0)
	mov	cx, 64 
	mov	si, fs_volume_name
	xor	bx, bx
	mov	al, 0FFh
modify_fs_vname_1:	
	mov	ah, al
	lodsb
	cmp	al, 20h
	ja	short modify_fs_vname_2
	cmp	ah, 20h
	jna	short modify_fs_vname_2
	mov	bx, si
	dec	bx
modify_fs_vname_2:
	loop	modify_fs_vname_1
	or	bx, bx
	jz	short modify_fs_vname_3
	mov	di, bx
	mov	cx, fs_volume_name+64
	sub	cx, bx
	xor	al, al
	rep	stosb 

modify_fs_vname_3:
	call	write_formatting_msg
	mov	al, 0
	call	write_format_percent_x

	mov	ax, [bp+12]	; [bsBeginSector]
	mov	dx, [bp+14]	; [bsBeginSector+2]

	; DX_AX = TRFS1 Boot Sector address
	mov	bx, bp
	; ES:BX = Boot Sector Buffer
	call	write_hd_sector
	jc	formatting_error
	
	add	ax, 1
	adc	dx, 0

	; DX_AX = MAT (DAT header) sector address
	mov	bx, FS_MAT_Buffer
	; 16/02/2019
	mov	word [bx],'MA'
	mov	byte [bx+2],'T'
	call	write_hd_sector
	jc	formatting_error
	call	write_fs_format_percent

	; Calculate DAT bits 
	; NOTE: 4096 bits per DAT sector
	mov	ax, [MAT_FirstFreeSector]
	; 19/03/2021
	;xor	dx, dx
	mov	dx, [MAT_FirstFreeSector+2]
	push	dx
	push	ax
	mov	cx, 4096
	call	div32
	mov	[DAT_FFBit], bx
	mov	[DAT_FFSector], ax
	; 19/03/2021
	mov	[DAT_FFSector+2], dx
	pop	ax
	pop	dx
	sub	ax, 1
	sbb	dx, 0
	add	ax, [MAT_FreeSectors]
	adc	dx, [MAT_FreeSectors+2]
	call	div32
	mov	[DAT_LFBit], bx
	mov	[DAT_LFSector], ax
	; 19/03/2021
	mov	[DAT_LFSector+2], dx
	;
	xor	si, si ; 0
	; 19/03/2021
	mov	[tempword], si ; 0
SINGLIX_fs1_f_4:
	; calculate free bits for current DAT sector
	;			(to be written)
	mov	di, FS_DAT_Buffer

	; 19/03/2021
	mov	ax, [tempword]
	cmp	ax, [DAT_FFSector+2]
	jb	short SINGLIX_fs1_f_9
	ja	short SINGLIX_fs1_f_13

	cmp	si, [DAT_FFSector]
	je	short SINGLIX_fs1_f_7
	jb	short SINGLIX_fs1_f_9
SINGLIX_fs1_f_13:
	; 19/03/2021
	cmp	ax, [DAT_LFSector+2]
	jb	short SINGLIX_fs1_f_6
	ja	short SINGLIX_fs1_f_9

	cmp	si, [DAT_LFSector]
	jb	short SINGLIX_fs1_f_6
	ja	short SINGLIX_fs1_f_9

	;mov	bx, [DAT_LFBit]
	;;mov	al, 0FFh
	;mov	ah, bl
	;shr	bx, 3 ; bit count to byte count
	;jz	short SINGLIX_fs1_f_5
	; 20/03/2021
	mov	cx, [DAT_LFBit]
	mov	ah, cl
 	shr	cx, 3 ; bit count to byte count
	jz	short SINGLIX_fs1_f_5
	;Example:
	;last free sector = 47 -> byte 5, bit 7
	;last free sector = 48 -> byte 6, bit 0
	; (also previous sectors are free sectors)
	mov	al, 0FFh ; 20/03/2021
	;mov	cx, bx
	rep	stosb
	; 20/03/2021
	;mov	di, FS_DAT_Buffer
	;add	di, bx
SINGLIX_fs1_f_5:
	and	ah, 7
	;mov	cl, ah
	;shl	al, cl
	;not	al
	; 20/03/2021
	xor	al, al
SINGLIX_fs1_f_15:
	; 20/03/2021 
	; 0 -> 00000001b (last free bit is bit 0)
	; 1 -> 00000011b (last free bit is bit 1)
	; 7 -> 11111111b (last free bit is bit 7)
	or	al, 1
	or	ah, ah
	jz	short SINGLIX_fs1_f_16
	shl	al, 1
	dec	ah
	jmp	short SINGLIX_fs1_f_15
SINGLIX_fs1_f_16:
	stosb
	;mov	cx, 511
	;sub	cx, bx
	;jna	short SINGLIX_fs1_f_9
	;mov	al, 0 ; out of volume bits (=0)
	;rep	stosb
	jmp	short SINGLIX_fs1_f_9
SINGLIX_fs1_f_6:
	mov	cx, 512
	jmp	short SINGLIX_fs1_f_8
SINGLIX_fs1_f_7:
	; 20/03/2021
	;Example:
	;first free sector = 23 -> byte 2, bit 7
	;first free sector = 24 -> byte 3, bit 0
	; (previous sectors are allocated sectors)
	mov	bx, [DAT_FFBit]
	mov	cl, bl
	and	cl, 7
	shr	bx, 3 ; from bits to bytes
	add	di, bx
	mov	al, 0FFh
	shl	al, cl
	stosb
	mov	cx, 511
	sub	cx, bx
	jna	short SINGLIX_fs1_f_9
SINGLIX_fs1_f_8:
	mov	al, 0FFh ; Free sector bits (=1)
	rep	stosb
SINGLIX_fs1_f_9:
	mov	ax, [MAT_BeginSector]
	mov	dx, [MAT_BeginSector+2]
	;add	ax, [DAT_Address] ; = 2
	;adc	dx, [DAT_Address+2]
	add	ax, 2
	adc	dx, 0
	add	ax, si
	adc	dx, 0
	; Write DAT sector(s)
	; DX_AX = Disk Allocation Table sector address
	mov	bx, FS_DAT_Buffer
	call	write_hd_sector
	jc	formatting_error
	call	write_fs_format_percent
	; Clear DAT buffer again (for next stage)
	mov	cx, 256
	mov	di, FS_DAT_Buffer
	sub	ax, ax
	rep	stosw

	inc	si
	; 19/03/2021
	jnz	short SINGLIX_fs1_f_14

	; 19/03/2021
	inc	word [tempword]
SINGLIX_fs1_f_14:
	mov	ax, [tempword]
	cmp	ax, [DAT_SectorCount+2]
	jb	SINGLIX_fs1_f_4

	cmp	si, [DAT_SectorCount]
	;jna	SINGLIX_fs1_f_4
	; 19/03/2021 (bugfix)
	jb	SINGLIX_fs1_f_4

	; ;;;
	
	; DAT sectors has been written..
	; Now, Root Directory Description Table is in order

	mov	di, FS_RDT_Buffer
	mov	ax, 'DD' 
	stosw
	xor	ah, ah
	mov	al, 'T'
	stosw
	mov	ax, 512 ; Sector size (Bytes per sector)
	stosw
	xor	ax, ax ; RDT sequence number (= 0, section 1)
	stosw	
	; RDT address
	mov	ax, [DAT_SectorCount]
	mov	dx, [DAT_SectorCount+2]
	add	ax, 2 ; BS + MAT
	adc	dx, 0
	stosw
	mov	ax, dx
	stosw
	sub	ax, ax ; Next RDT number
	stosw
	stosw
	;mov	ax, 4 ; Sector count of this section	
	;	      ; (4*512)/4 = 512 root dir entries
	; 20/03/2021
	mov	al, 4
	stosw
	xor	al, al
	stosw
	; Volume beginning sector
	mov	ax, [bp+12]	; [bsBeginSector]
	mov	dx, [bp+14]	; [bsBeginSector+2]
	stosw
	mov	ax, dx
	stosw
	xor	ax, ax
	dec	ax ; 0FFFFh
	; Parent Dir Serial (= FFFFFFFFh for root dir)
	stosw
	stosw

set_fs_volume_serial_number:
	mov	si, fs_volume_serial
	movsw
	movsw

	sub	ax, ax
	;stosb	; sub directory level = 0
	;stosb	; 0, reserved
	stosw
	mov	al, 00000100b ;  (DOS) System attribute
	stosb	; (DOS) Basic attributes
	sub	al, al ; Extended attributes (0 for TRDOS 386)
	stosb
	; 20/03/2021
	;sub	ax, ax ; reserved (8) bytes for TR-MULTIX
	stosw
	stosw
	stosw
	stosw
	mov	ax, 'RT' ; TRFS Root directory signature
	stosw
	xor	ax, ax ; Country (language, date, text format)
		       ; (0 = Default, 1 = USA, 90 = Turkiye)
	;stosb
	;stosb	; Time Zone (0 = GMT = default ; -11 to +12)
	stosw

	mov	si, di
	; get the date (from RTC)
	mov	ah, 4
	int	1Ah
	; Creating Date (of root directory)
	xchg	ch, cl ; 07/01/2018
	mov	ax, cx ; cl = century (BCD), ch = year (BCD)
	stosw
	mov	al, dh  ; month (BCD)
	stosb
	mov	al, dl  ; day (BCD)
	stosb
	; get the time (from RTC)
	mov	ah, 2
	int	1Ah
	; Creating Time (of root directory)
	xchg	cl, ch ; ch = hour (BCD), cl = minute (BSD)
	mov	ax, cx ; al = hour, ah = minute
	stosw
	mov	al, dh ; seconds (BCD)
	stosb
	mov	al, dl ; daylight savings time option 
	stosb
	; Set Last Modification Date&Time
	mov	cx, 4
	rep	movsw ; copy creating date&time values to
		; last modification date time values
		; (last modif date&time = creating date&time)

set_fs_volume_name:
	mov	si, fs_volume_name
	mov	cl, 32 
	rep	movsw

	; Fill remain bytes (of this RDT) with zero
	mov	cx, (128+256)/2
	xor	ax, ax
	rep	stosw

	; RDT is ready here...

	mov	ax, [bp+28]	; [bsRootDirDT]
	mov	dx, [bp+30]	; [bsRootDirDT+2]
	; 20/03/2021
	;xor	dx, dx
	add	ax, [bp+12]	; [bsBeginSector]
	adc	dx, [bp+14]	; [bsBeginSector+2]

	; Write RDT sector
	; DX_AX = Root Directory Description Table address
	mov	bx, FS_RDT_Buffer
	call	write_hd_sector
	jc	formatting_error
	call	write_fs_format_percent

	add	ax, 1
	adc	dx, 0

	; write root directory data sectors
	mov	cx, 4

SINGLIX_fs1_f_10:
	push	cx
	; Write root directory sector(s)
	; DX_AX = Root Directory Sector address
	mov	bx, HDFORMAT_EMPTY_BUFF
	call	write_hd_sector
	jc	formatting_error
	call	write_fs_format_percent
	add	ax, 1
	adc	dx, 0
	pop	cx
	dec	cl
	jnz	short SINGLIX_fs1_f_10

	; Fill remain sectors with 'F6h' bytes
	mov	cx, [bp+16]	; [bsVolumeSize]
	mov	bx, [bp+18]	; [bsVolumeSize+2]
	add	cx, [bp+12]	; [bsBeginSector]
	adc	bx, [bp+14]	; [bsBeginSector+2]

	; write DATA sectors 
	; (after root directory sectors)
SINGLIX_fs1_f_11:
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
	cmp	dx, bx
	jb	short SINGLIX_fs1_f_11
	ja	SINGLIX_fs1_f_12
	cmp	ax, cx
	jb	short SINGLIX_fs1_f_11
	jmp	SINGLIX_fs1_f_12	

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; print messages
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
; reading a block (sector) on hard disk
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 17/10/2020

read_hd_sector:

	; INPUT -> DX_AX = Logical Block Address
	; 	   ES:BX = Sector Buffer
	; OUTPUT ->
	;  cf = 0 -> DX_AX = Logical Block Adress
	;	     ES:BX = Sector Buffer
	;  cf = 1 -> AH = Error Number

	cmp	dx, [chs_limit+2]
	ja	short read_lba_sector
	jb	short read_chs_sector
	cmp	ax, [chs_limit]
	ja	short read_lba_sector
	;jmp	short read_chs_sector

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; reading a block (sector) by using CHS read (ROMBIOS) function
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 17/10/2020

read_chs_sector:
	; hdformat.s (25/09/2020)
	mov	byte [rw], 2 ; read
	jmp	short chs_rw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; reading a block (sector) by using LBA read (ROMBIOS) function
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 17/10/2020

read_lba_sector:
	; hdformat.s (25/09/2020)
	mov	byte [rw], 42h
	jmp	short lba_rw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing a block (sector) by using LBA write (ROMBIOS) function
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 17/10/2020

	; 18/10/2020 
write_lba_sector:
	; hdformat.s (25/09/2020)
	mov	byte [rw], 43h
	;jmp	short lba_rw
lba_rw:
	;mov	di, 5
	; 18/10/2020
	mov	byte [rcnt], 5  ; Retry count
lba_rw_1:
	;pusha			; db 60h
	db	60h
	;push 	0		; db 6Ah, 00h
	db	6Ah, 0
	;push	0		; db 6Ah, 00h
	db	6Ah, 0
	push    dx
	push    ax
	push    es
	push    bx
	;push	1		; db 6Ah, 01h
	db	6Ah, 01h                     
	;push	10h		; db 6Ah, 10h
	db	6Ah, 10h

	mov     si, sp
	mov     dl, [DrvNum]
	xor	al, al	; verify off 
lba_rw_2:
	mov     ah, [rw] ; 42h = LBA read, 43h = LBA write
	;xor	al, al	; verify off 
	int     13h

	;mov	[error], ah
	jnc     short lba_rw_3

	;dec	di 
	; 18/10/2020
	dec	byte [rcnt]
	jz	short lba_rw_3 
        
	xor	ah, ah
	;mov	dl, [DrvNum]
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
; writing a block (sector) on hard disk
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 17/10/2020

write_hd_sector:

	; INPUT -> DX_AX = Logical Block Address
	; 	   ES:BX = Sector Buffer
	; OUTPUT ->
	;  cf = 0 -> DX_AX = Logical Block Adress
	;	     ES:BX = Sector Buffer
	;  cf = 1 -> AH = Error Number

	cmp	dx, [chs_limit+2]
	ja	short write_lba_sector
	jb	short write_chs_sector
	cmp	ax, [chs_limit]
	ja	short write_lba_sector
	;jmp	short write_chs_sector

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing a block (sector) by using CHS write (ROMBIOS) function
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 17/10/2020

	; 18/10/2020
write_chs_sector:
	; hdformat.s (25/09/2020)
	mov	byte [rw], 3 ; write
	;jmp	short chs_rw
chs_rw:
	push	si
        push	cx        
chs_rw_0:
	;mov	di, 5
	; 18/10/2020
	mov	byte [rcnt], 5  ; Retry count
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

	mov	dl, [DrvNum]
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
        
	jnc     short chs_rw_3
	;dec	di                 
	dec	byte [rcnt] ; 18/10/2020
	jz	short chs_rw_3 
        
	xor	ah, ah
	;mov	dl, [DrvNum]
	int	13h	; BIOS Service func (ah) = 0
			; Reset disk system
	jmp	short chs_rw_2

chs_rw_3:
	pop	ax
	pop	dx
	pop	cx
	pop	si	
	retn
	
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; convert byte to decimal number
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
; convert byte to hexadecimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

byte_to_hex: ; 04/02/2019
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
; read & write characters
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
	cmp     ah, [vname_length] ; 05/01/2018
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
; display CHS takle
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_chs_table:

	; 16/10/2020
	; 12/10/2020 (fdisk3)
	; 11/02/2019 (hdimage)

	push	es
	mov	di, 0B800h
	mov	es, di
	mov	di, 160 ; row 1
	mov	ah, 07h
	mov	al, 'D'
	stosw
	mov	al, 'i'
	stosw
	mov	al, 's'
	stosw
	mov	al, 'k'
	stosw
	mov	al, ':'
	stosw
	mov	al, ' '
	stosw
	mov	al, 'h'
	stosw
	mov	al, 'd'
	stosw
	mov	al, [DrvNum]
	sub	al, 80h-'0'
	stosw
	mov	di, 320 ; row 2
	mov	cx, 80		
	mov	ax, 0720h
	rep	stosw
	mov	cl, 80	; row 3 
	mov	al, "-"
	rep	stosw
	;mov	cl, 19
	mov	cl, 18 ; 16/10/2020 ; row 4
	mov	al, 20h
	rep	stosw
	mov	al, 'C'
	stosw
	mov	al, 'y'
	stosw
	mov	al, 'l'
	stosw
	mov	al, 'i'
 	stosw
	mov	al, 'n'
	stosw	
	mov	al, 'd'
	stosw
	mov	al, 'e'
	stosw
	mov	al, 'r'
 	stosw
	mov	al, 's'
 	stosw
	mov	al, ':'
	stosw
	mov	al, 20h
	stosw
	;mov	[cylnumpos], di
	mov	ax, [cylinders]
	mov	dx, [cylinders+2] ; 16/10/2020
	;mov	cl, 4
	;mov	ch, 07h ; color
	call	write_number
	
	;mov	ax, 0720h
	mov	al, 20h ; 16/10/2020
	stosw
	stosw
	mov	al, 'H'
	stosw
	mov	al, 'e'
	stosw
	mov	al, 'a'
	stosw
	mov	al, 'd'
 	stosw
	mov	al, 's'
 	stosw
	mov	al, ':'
	stosw
	mov	al, 20h
	stosw
	;mov	[hednumpos], di
	mov	ax, [heads]
	xor	dx, dx ; 16/10/2020
	;mov	cl, 2
	;mov	ch, 07h ; color
	call	write_number

	;mov	ax, 0720h
	mov	al, 20h ; 16/10/2020
	stosw
	stosw
	mov	al, 'S'
	stosw
	mov	al, 'e'
	stosw
	mov	al, 'c'
	stosw
	mov	al, 't'
 	stosw
	mov	al, 'o'
	stosw
	mov	al, 'r'
 	stosw
	mov	al, 's'
 	stosw
	mov	al, ':'
	stosw
	mov	al, 20h
	stosw
	;mov	[secnumpos], di
	mov	ax, [sectors]
	sub	dx, dx ; 16/10/2020
	;mov	cl, 2
	;mov	ch, 07h ; color
	call	write_number

	mov	cx, 800 ; 16/10/2020 ; row 5
	sub	cx, di
	shr	cl, 1
	;mov	cl, 22
	;mov	ax, 0720h
	mov	al, 20h ; 16/10/2020
	rep	stosw
	
	mov	cl, 80 	; row 6
	mov	al, "-"
	rep	stosw

	mov	cl, 80	 ; row 7
	;mov	cl, 160	 ; row 7, 8
	mov	al, 20h
	rep	stosw

	mov	dx, 0600h ; DH = row, DL = 0 column
	xor	bx, bx ; BH = video page (0)
	mov	ah, 02h ; set cursor position
	int	10h

	pop	es
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write decimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;write_number:
;	; 12/10/2020
;	mov	bx, 10
;	mov	si, cx
;wnum_1:
;	xor	dx, dx
;	div	bx
;	push	dx
;	dec	cl
;	jnz	short wnum_1
;	mov	cx, si
;	mov	ah, ch	; color (07h or 0Fh)
;	xor	ch, ch
;wnum_2:
;	pop	dx
;	mov	al, dl
;	add	al, '0'
;	stosw
;	loop	wnum_2
;	retn

write_number:
	; 16/10/2020
	; dx:ax = binary number
	; di = decimal number/string location
	; modified registers: ax, bx, cx, dx, bp, di
	mov	cx, 10
	mov	bp, sp
wnum_1:
	call	div32
	push	bx
	and	dx, dx
	jnz	short wnum_1
	or	ax, ax
	jnz	short wnum_1
wnum_2:	
	pop	ax
	add	al, '0'
	mov	ah, 07h ; color
	stosw
	cmp	bp, sp
	ja	short wnum_2
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; partition size input
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 12/03/2021
partition_size_input:
	; 23/10/2020 (fdisk3.s modification on hdimage.s source code)

	mov	word [pSize_multiplier+2], 0
	mov	byte [msg_psize_unit+1], 'B'
	mov	al, [pSize_unit]
	cmp	al, '%'
	jne	short psu_0
	; 08/02/2019
	; calculate sector count for max. available sectors percentage
	mov	dx, [pp_Sectors+2] ; max. available sectors
	mov	ax, [pp_Sectors]   ; for a new partition
	;mov	dx, [total_sectors+2]
	;mov	ax, [total_sectors]
	;mov	cx, 100
	; 13/03/2021
	mov	cx, 99
	add	ax, cx
	adc	dx, 0
	inc	cx ; cx = 100
	call	div32
	mov	[pSize_multiplier], ax
	; 29/10/2020
	mov	[pSize_multiplier+2], dx
	mov	ah, 0
	mov	[msg_psize_unit+1], ah
	;mov	byte [pSize_maxdigits], 2
	mov	byte [pSize_maxdigits], 3 ; 29/10/2020
	mov	al, '%'
	jmp	short psu_6
psu_0:
	cmp	al, 'C'
	jne	short psu_1
	mov	ax, [sectors]
	mul	word [heads]
	mov	[pSize_multiplier], ax
	;mov	byte [pSize_maxdigits], 4
	; <= 65535 cylinders
	mov	byte [pSize_maxdigits], 5 ; 23/10/2020
	;sub	dh, dh
	mov	[msg_psize_unit+1], dh
	mov	al, 'C'
	jmp	short psu_6
psu_1:
	cmp	al, 'G'
	jne	short psu_2
	mov	word [pSize_multiplier], 2*1024
	mov	word [pSize_multiplier+2], 1024
	;mov	byte [pSize_maxdigits], 1
	mov	byte [pSize_maxdigits], 3 ; <= 512 GB ; 23/10/2020
	jmp	short psu_6
psu_2:
	cmp	al, 'M'
	jne	short psu_3
	mov	word [pSize_multiplier], 2*1024
	;;mov	byte [pSize_maxdigits], 4
	;mov	byte [pSize_maxdigits], 7 ; 23/10/2020
	; 12/03/2021
	mov	byte [pSize_maxdigits], 6 ; <= 524288
	jmp	short psu_6
psu_3:
	cmp	al, 'K'
	jne	short psu_4
	mov	word [pSize_multiplier], 2
	;jmp	short psu_5
	; 12/03/2021
	mov	byte [pSize_maxdigits], 9 ; <= 536870912
	jmp	short psu_6 	
psu_4:
	; al = 'S'
	xor	ah, ah
	mov	[msg_psize_unit+1], ah
	mov	word [pSize_multiplier], 1
psu_5:
	;mov	byte [pSize_maxdigits], 7
	mov	byte [pSize_maxdigits], 10 ; 23/10/2020
psu_6:
	mov	[msg_partition_size_x], al
	mov	si, msg_partition_size	
	call	print_msg

	mov	bp, sp
	xor	bx, bx
	mov	[pSize_temp], bx ; 0
	;mov	[pSize_temp+2], bx ; 0
	mov	[pSize_digitpos], bl ; 0
	; bh = 0  ; video page
	mov     ah, 3	; get cursor pos
	int     10h
	mov	[Cursor_Pos], dx
	; 09/02/2019
	mov     bl, 7   ; page 0, color 7 (light gray)   
psu_7:
	xor	ah, ah
	int	16h

	cmp	al, '0'
	jb	short psu_8
	cmp	al, '9'
	ja	short psu_7
	mov	bl, [pSize_digitpos]
	cmp	bl, [pSize_maxdigits]
	jnb	short psu_7
	inc	byte [pSize_digitpos]
	sub	al, '0'
	xor	ah, ah
	push	ax
	add	al, '0'
	mov	ah, 0Eh	; write char as tty
	;mov	bx, 7   ; page 0, color 7 (light gray)
	int	10h
	jmp	short psu_7

psu_8esc:
	; 29/10/2020 (ESCape)
	mov	sp, bp ; clean stack

	or	al, al ; zf = 0
	stc
	retn	
psu_8:
	and	al, al
	jz	psu_15 ; check for left arrow key
	cmp	al, 27
	je	short psu_8esc ; ESCAPE key ; 29/10/2020
	ja	psu_14 ; check for left arrow key
	cmp	al, 13
	;je	short psu_9  ; ENTER key
	jb	short psu_11 ; check for backspace key
	;jmp	short psu_7
	ja	short psu_7
psu_9:
	xor	ax, ax
	mov	[pSize_temp+2], ax ; 0 ; 08/02/2019
	cmp	byte [pSize_digitpos], al ; 0
	jna	psu_16		
	;xor	bh, bh
	mov	bl, [pSize_digitpos]
	dec	bl
	shl	bl, 1
	add	bx, sp
	mov	ax, [bx]
	mov	[pSize_temp], ax
	;mov	word [pSize_temp+2], 0
	mov	cx, 10
psu_10:
	dec	byte [pSize_digitpos]
	jz	short psu_16
	
	mov	ax, [pSize_temp]
	mov	dx, [pSize_temp+2]
	;mov	cx, 10
	call	mul32
	;mov	[pSize_temp], ax
	;mov	[pSize_temp+2], dx
	;xor	bh, bh
	mov	bl, [pSize_digitpos]
	dec	bl
	shl	bl, 1
	add	bx, sp ; (*)
	;mov	ax, [bx]
	;add	[pSize_temp], ax
	;adc	word [pSize_temp+2], 0
	add	ax, [bx]
	adc	dx, 0
	mov	[pSize_temp], ax
	mov	[pSize_temp+2], dx
	jmp	short psu_10
	
	; Left arrow, backspace, DEL key checking
psu_11:
	cmp	al, 8 ; backspace key
	jne	psu_7
psu_12:
	;; bh = 0  ; video page
	;mov	ah, 3 ; get cursor pos
	;int	10h
	;cmp	dl, [Cursor_Pos]
	;jna	short psu_13
	;dec	dl
	;dec	byte [pSize_digitpos]

	; 08/02/2019
	mov	dx, [Cursor_Pos]
	mov	bl, [pSize_digitpos]
	and	bl, bl
	jz	short psu_13

	dec	bl 
	mov	[pSize_digitpos], bl
	
	add	dl, bl

	; bh = 0  ; video page
	mov	ah, 2 ; set cursor pos
	int	10h
	;mov	bl, dl
	;sub	bl, [Cursor_Pos] 
	mov	cx, 1
	mov	ah, 9 ; write char at current curs pos
	mov	al, 20h ; space (blank)
	mov	[si+bx], al
	; bh = 0 ; video page
	mov	bl, 7 ; attribute/color (light gray)
	int	10h

	; 09/02/2019
	pop	ax ; remove last digit on top of stack 
		   ; set sp to correct position for BX (*)
	jmp	psu_7
psu_13:
	mov	ah, 0Eh
	mov	al, 7
	;mov	bx, 7
	int	10h
	jmp	psu_7
psu_14:
	cmp	al, 0E0h
	jne	psu_7
psu_15:    
	cmp	ah, 4Bh ; left arrow
	je	short psu_12
	cmp	ah, 53h ; DEL key (backspace)
	je	short psu_12
	jmp	psu_7
psu_16:
	mov	sp, bp

	; 29/10/2020
	cmp	byte [pSize_unit], '%'
	jne	short psu_23

	mov	ax, 100
	cmp	word [pSize_temp], ax ; 100 ; % limit
	jna	short psu_17
	mov	word [pSize_temp], ax

	; (re)set asciiz number string to '100'

	sub	bh, bh ; 0 ; video page 0
	mov	dx, [Cursor_Pos]
	mov	ah, 2 ; set cursor pos
	int	10h

	mov	si, msg_100
	call	print_msg

	jmp	short psu_17
psu_23:
	cmp	byte [pSize_unit], 'S'
	jne	short psu_17 

	mov	si, msg_sectors_crlf
	call	print_msg
	;xor	bx, bx
	jmp	short psu_18
psu_17:
	mov	si, msg_psize_unit
	call	print_msg

	mov	si, CRLF
	call	print_msg

	; 29/10/2020
	cmp	byte [pSize_unit], '%'
	jne	short psu_18
	
	mov	cx, [pSize_temp]

	and	cx, cx
	jz	short psu_21 ; 0%, ZERO !

	cmp	cx, 100
	jna	short psu_24

	; show 100% for 1 second (for >100%)
	call	wait1second ; 29/10/2020

	mov	cx, 100
psu_24:
	mov	ax, [pSize_multiplier]
	mov	dx, [pSize_multiplier+2]

	;mov	cx, [pSize_temp]
	;and	cx, cx
	;jz	short psu_21 ; 0%, ZERO !

	jmp	mul32
psu_18:
	mov	ax, [pSize_temp]
	mov	dx, [pSize_temp+2]
	mov	cx, ax
	or	cx, dx
	;jz	short psu_20
	jz	short psu_21 ; 08/02/2019
	mov	cx, [pSize_multiplier]
	cmp	byte [pSize_unit], 'C'
	je	short psu_20 ; 09/02/2019
	;jne	short psu_19
	;call	mul32
	; 08/02/2019
	; dx:ax = requested partition size in sectors
	;retn	
;psu_19:
	;mov	cx, [pSize_multiplier]
	cmp	cx, 1
	;jna	short psu_20
	ja	short psu_19
	; 09/02/2019
	xor	bx, bx
	retn
psu_19:
	call	mul32
	;and	bx, bx
	;jnz	short psu_22 ; 09/02/2019
	mov	cx, [pSize_multiplier+2]
	or	cx, cx
	;jz	short psu_20
	jz	short psu_22 ; 09/02/2019
psu_20:
	;call	mul32
	;retn
	jmp	mul32 ; 23/10/2020
psu_21:
	; zf = 1 ; 29/10/2020
	stc
psu_22:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; wait for 1 second
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 29/10/2020
wait1second:
	mov	ah, 2
	int	1Ah ; get time of day
	jc	short w1s_2
w1s_1:
	push	dx
	mov	ah, 2
	int	1Ah ; get time of day
	pop	ax
	jc	short w1s_2
	cmp	dh, ah
	je	short w1s_1 ; same second
w1s_2:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; partition type input
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

partition_type_input:
	mov	si, msg_partition_type
	call	print_msg

	xor	bx, bx

	mov	[pType_pos], bl ; 0
	mov	[pType_num], bx ; 0

	; bh = 0  ; video page
	mov     ah, 3 ; get cursor pos
	int     10h
	mov	[Cursor_Pos], dx
ptu_0:
	xor	ah, ah
	int	16h

	cmp	byte [pType_pos], 1
	ja	short ptu_5	

	cmp	al, '0'
	jb	short ptu_5
	cmp	al, '9'
	ja	short ptu_1
	mov	ah, al
	sub	ah, '0'
	jmp	short ptu_2 
ptu_1:
	cmp     al, 0E0h
	je	short ptu_9

	and	al, 0DFh
	cmp	al, 'A'
	jb	short ptu_0
	cmp	al, 'F'
	ja	short ptu_0
	mov	ah, al
	sub	ah, 'A'-10
ptu_2:
	cmp	byte [pType_pos], 0
	jna	short ptu_3
	mov	[pType_num+1], ah
	jmp	short ptu_4 
ptu_3:
	mov	[pType_num], ah
ptu_4:
	mov	ah, 0Eh
	mov	bl, 7
	int	10h

	inc	byte [pType_pos]

	jmp	short ptu_0 
ptu_5:
	and	al, al
	jz	short ptu_9 ; check for left arrow key 
	cmp	al, 27
	je	short ptu_10 ; ESCAPE key
	ja	short ptu_0
	cmp	al, 13
	je	short ptu_11  ; ENTER key
	ja	short ptu_0
ptu_6:
	; Left arrow, backspace, DEL key checking

	cmp	al, 8 ; backspace key
	jne	short ptu_0
ptu_7:
	; bh = 0  ; video page
	mov	ah, 3 ; get cursor pos
	int	10h
	cmp	dl, [Cursor_Pos]
	jna	short ptu_8
	dec	dl
	dec	byte [pType_pos]
	; bh = 0  ; video page
	mov	ah, 2 ; set cursor pos
	int	10h
	mov	bl, dl
	sub	bl, [Cursor_Pos] 
	mov	cx, 1
	mov	ah, 9 ; write char at current curs pos
	mov	al, 20h ; space (blank)
	mov	[si+bx], al
	; bh = 0 ; video page
	mov	bl, 7 ; attribute/color (light gray)
	int	10h
	jmp	ptu_0
ptu_8:
	mov	ah, 0Eh
	mov	al, 7
	int	10h
	jmp	ptu_0
ptu_9:    
	cmp	ah, 4Bh ; left arrow
	je	short ptu_7
	cmp	ah, 53h ; DEL key (backspace)
	je	short ptu_7
	jmp	ptu_0

ptu_10: ; ESCAPE
	;mov	al, 0
	; 29/10/2020
	sub	al, al ; 0
	; partition type is 0 (none)
	jmp	short ptu_12
ptu_11: ; ENTER
	mov	al, [pType_num]
	cmp	byte [pType_pos], 1
	jna	short ptu_12
	mov	ah, 16
	mul	ah
	add	al, [pType_num+1]
ptu_12:
	push	ax
	call	bin_to_hex
	mov	[msg_ptype_num], ax
	; bh = 0  ; video page
	mov	ah, 2 ; set cursor pos
	mov	dx, [Cursor_Pos]
	int	10h
	mov	si, msg_ptype_num 
	call	print_msg
	pop	ax
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; show selected partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

show_selected_partition:
	; INPUT ->
	; 	DS:SI = Partition table row address
	
	; 2019 - 2020 (hdimage.s)
	;pt_s_offset	equ 7
	;pt_bh_offset	equ 11
	;pt_bs_offset	equ 15
	;pt_bc_offset	equ 19
	;pt_fs_offset	equ 23
	;pt_eh_offset	equ 27
	;pt_es_offset	equ 31
	;pt_ec_offset	equ 35
	;pt_rs_offset	equ 41
	;pt_ts_offset	equ 52
	;pt_fsn_offset	equ 63

	; 24/10/2020 (fdisk3.s)
	pt_s_offset	equ 6
	pt_bh_offset	equ 10
	pt_bs_offset	equ 14
	pt_bc_offset	equ 18
	pt_fs_offset	equ 22
	pt_eh_offset	equ 26
	pt_es_offset	equ 30
	pt_ec_offset	equ 34
	pt_rs_offset	equ 40
	pt_ts_offset	equ 51
	pt_fsn_offset	equ 63

	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h	

	mov	ax, si
	sub	ax, MasterBootBuff + pTableOffset ; + 446
	shr	al, 4 ; from offset to partition number
	add	al, '1'  ; 1 based partition number (chr)
	; Write partition number to the header location
	mov	[msg_selected_partition+43], al ; '1' to '4'
	
	; Partition number will be used at formatting stage
	mov	[partition_num_chr], al ; number + '0'

	mov	dl, 'h'
	mov	al, [si+ptBootable]
	call	bin_to_hex
	mov	[pt_row+pt_s_offset], ax
	mov	[pt_row+pt_s_offset+2], dl ; 'h'
	mov	al, [si+ptBeginHead]
	call	bin_to_hex
	mov	[pt_row+pt_bh_offset], ax
	mov	[pt_row+pt_bh_offset+2], dl ; 'h'
	mov	al, [si+ptBeginSector]
	call	bin_to_hex
	mov	[pt_row+pt_bs_offset], ax
	mov	[pt_row+pt_bs_offset+2], dl ; 'h'
	mov	al, [si+ptBeginCylinder]
	call	bin_to_hex
	mov	[pt_row+pt_bc_offset], ax
	mov	[pt_row+pt_bc_offset+2], dl ; 'h'
	mov	al, [si+ptFileSystemID]
 	; Partition type will be used at formatting stage
	mov	[pp_type_user], al
	call	bin_to_hex
	mov	[pt_row+pt_fs_offset], ax
	mov	[pt_row+pt_fs_offset+2], dl ; 'h'
	mov	al, [si+ptEndHead]
	call	bin_to_hex
	mov	[pt_row+pt_eh_offset], ax
	mov	[pt_row+pt_eh_offset+2], dl ; 'h'
	mov	al, [si+ptEndSector]
	call	bin_to_hex
	mov	[pt_row+pt_es_offset], ax
	mov	[pt_row+pt_es_offset+2], dl ; 'h'
	mov	al, [si+ptEndCylinder]
	call	bin_to_hex
	mov	[pt_row+pt_ec_offset], ax
	mov	[pt_row+pt_ec_offset+2], dl ; 'h'
	mov	al, [si+ptStartSector]
	call	bin_to_hex
	mov	[pt_row+pt_rs_offset+6], ax
	mov	[pt_row+pt_rs_offset+8], dl ; 'h'
	mov	al, [si+ptStartSector+1]
	call	bin_to_hex
	mov	[pt_row+pt_rs_offset+4], ax
	mov	al, [si+ptStartSector+2]
	call	bin_to_hex
	mov	[pt_row+pt_rs_offset+2], ax
	mov	al, [si+ptStartSector+3]
	call	bin_to_hex
	mov	[pt_row+pt_rs_offset], ax
	mov	al, [si+ptSectors]
	call	bin_to_hex
	mov	[pt_row+pt_ts_offset+6], ax
	mov	[pt_row+pt_ts_offset+8], dl ; 'h'
	mov	al, [si+ptSectors+1]
	call	bin_to_hex
	mov	[pt_row+pt_ts_offset+4], ax
	mov	al, [si+ptSectors+2]
	call	bin_to_hex
	mov	[pt_row+pt_ts_offset+2], ax
	mov	al, [si+ptSectors+3]
	call	bin_to_hex
	mov	[pt_row+pt_ts_offset], ax

	mov	al, [si+ptFileSystemID]
	mov	di, valid_partitions
	mov	cx, 19
	repnz	scasb
	jz	short ssp_1
	mov	ax, FS_OTHERS	 
	jmp	short ssp_2
ssp_1:
	sub	di, valid_partitions + 1
	mov	ax, 14
	mul	di
	add	ax, FileSys_Names
ssp_2:
	xchg	ax, si 
	mov	cl, 7
	mov	di, pt_row + pt_fsn_offset
	rep	movsw
	mov	di, ax ; partition table row address

	mov	si, msg_selected_partition
	call	print_msg

	mov	si, msg_boot_indicator
	call	print_msg
	mov	si, msg_YES
	test	byte [di+ptBootable], 80h
	jnz	short ssp_3
	mov	si, msg_NO
ssp_3:
	call	print_msg

	mov	si, msg_starting_head
	call	print_msg
	mov	al, [di+ptBeginHead]
	call	write_byte_decimal
	call	print_msg
	mov	si, msg_starting_sector
	call	print_msg
	mov	al, [di+ptBeginSector]
	mov	dl, al  ; bits 7&8 are bits 8&9 of cyl
	and	al, 3Fh ; sector number, 1 to 63
	call	write_byte_decimal
	call	print_msg	
	mov	si, msg_starting_cylinder
	call	print_msg
	mov	al, [di+ptBeginCylinder] ; bits 0to7 of cyl
	shr	dl, 6 ; bits 8&9 of cyl
	mov	ah, dl
	xor	dx, dx
	;mov	si, msg_partition_sectors + 7 ; max. 8 digits
	; 06/11/2020
	mov	si, msg_partition_sectors + 10 ; max. 11 digits
	; dx_ax: binary number
	call	bin_to_decimal
	; ds:si = decimal number text address
	call	print_msg
	mov	si, msg_system_id
	call	print_msg
	; Write file system name (again, copy)
	mov	si, pt_row + pt_fsn_offset
	;mov	cx, 14
	mov	cl, 14
	mov	ah, 0Eh ; write tty
	mov	bx, 7
ssp_4:	 
	lodsb
	int	10h
	loop	ssp_4

	mov	si, msg_ending_head
	call	print_msg
	mov	al, [di+ptEndHead]
	call	write_byte_decimal
	call	print_msg
	mov	si, msg_ending_sector
	call	print_msg
	mov	al, [di+ptEndSector]
	mov	dl, al  ; bits 7&8 are bits 8&9 of cyl
	and	al, 3Fh ; sector number, 1 to 63
	call	write_byte_decimal
	call	print_msg	
	mov	si, msg_ending_cylinder
	call	print_msg
	mov	al, [di+ptEndCylinder] ; bits 0to7 of cyl
	shr	dl, 6 ; bits 8&9 of cyl
	mov	ah, dl
	xor	dx, dx
	;mov	si, msg_partition_sectors + 7 ; max. 8 digits
	; 06/11/2020
	mov	si, msg_partition_sectors + 10 ; max. 11 digits 
	; dx_ax: binary number
	call	bin_to_decimal
	; ds:si = decimal number text address
	call	print_msg

	mov	si, msg_relative_sectors
	call	print_msg
	mov	ax, [di+ptStartSector]
	mov	dx, [di+ptStartSector+2]
	;;mov	si, msg_partition_sectors + 7 ; max. 8 digits
	;mov	si, reserved_bytes + 10 ; max. 11 digits
	; 06/11/2020
	mov	si, msg_partition_sectors + 10 ; max. 11 digits
	call	bin_to_decimal
	call	print_msg

	mov	si, msg_total_sectors
	call	print_msg
	mov	ax, [di+ptSectors]
	mov	dx, [di+ptSectors+2]
	;;mov	si, msg_partition_sectors + 7 ; max. 8 digits
	;mov	si, reserved_bytes + 10 ; max. 11 digits
	; 06/11/2020
	mov	si, msg_partition_sectors + 10 ; max. 11 digits
	call	bin_to_decimal	
	call	print_msg

	; 24/02/2019
	mov	si, CRLF
	call	print_msg

	mov	si, di  ; partition table row address

	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; write byte as descimal number
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

write_byte_decimal:
	; INPUT ->
	;	AL = binary number
	; OUTPUT ->
	;	DS:SI = decimal number text address
	;	        (ASCIIZ string)
	;
	; (SI, AX, CX will be modified)	

	;mov	si, msg_partition_sectors + 8 ; max. 8 digits
	; 06/11/2020
	mov	si, msg_partition_sectors + 11 ; max. 11 digits
	mov	cl, 10
wbd_loop:
	dec	si
	xor	ah, ah
	div	cl
	add	ah, '0'
	mov	[si], ah
	and	al, al
	jnz	short wbd_loop
	retn
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; init (MBR) partition table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 16/10/2020

	; 03/02/2019
init_partition_table:

	; INPUT -> none
	; OUTPUT -> none

	; 15/10/2020
	; (If a partition row contains invalid/wrong/defective parameters)
	; (it's active partition flag/byte will be 0FFh, an invalid value!)

	; 12/02/2019	
	;cmp	word [MBIDCode], 0AA55h
	;jne	ipt_stc ; invalid

	; 15/02/2019
	; clear primary partition table structure/list

	mov	di, part_table_boot_ind
	mov	cx, 36 ; 18*4 = 72 bytes
	xor	ax, ax ; 0
	rep	stosw

	mov	[pcount], ax ; reset (pcount & ppcount)
	mov	[apcount], ax ; reset (apcount & epnumber)

	mov	si, MasterBootBuff+446 ; Partition table offset
	;mov	cx, 4
	mov	cl, 4
	xor	di, di
ipt_0:
	mov	ah, [si+ptFileSystemID]

	and	ah, ah
	jz	ipt_8 ; empty

	inc	byte [pcount] ; partition count

	cmp	ah, 1	; FAT12
	je	short ipt_2
	cmp	ah, 4	; FAT16
	je	short ipt_2
	jb	short ipt_3	
	cmp	ah, 6	; FAT16 big
	je	short ipt_2
	ja	short ipt_1 ; EXTENDED

	;cmp	ah, 5 ; EXTENDED
	;jne	short ipt_3
ipt_17:
	cmp	byte [epnumber], 0
	;ja	short ipt_stc ; invalid  (more than 1 extended dos partition)
	jna	short ipt_15
	
	mov	byte [part_table_boot_ind+di], 0FFh 
				; Invalid/Defective partition flag
ipt_15:	
	mov	al, 5
	sub	al, cl ; partition number 1 to 4
	mov	byte [epnumber], al ; extended partition entry number (1 to 4)
	jmp	short ipt_3
ipt_1:
	; 26/10/2020
	cmp	ah, 07h ; NTFS (Windows FS)
	je	short ipt_2 ; accept NTFS as primary dos partition
			    ; (then, extended partition can be created)	
	; 24/10/2020
	cmp	ah, 0Ch ; FAT32 (LBA)
	je	short ipt_2
	ja	short ipt_18
	; 16/10/2020
	cmp	ah, 0Bh ; FAT32 (CHS)
	;jne	short ipt_3
	; 24/10/2020
	je	short ipt_2
	jb	short ipt_3 ; non-dos partition (NTFS or unknown fs type)
ipt_18:
	cmp	ah, 0Fh  ; EXTENDED (LBA)
	je	short ipt_17
	ja	short ipt_3 ; non-dos partition 
			    ; (xenix/unix/linux/singlix/runix or unknown)
	cmp	ah, 0Eh ; FAT16 (LBA)
	jne	short ipt_3 ; 0Dh
	; FAT16 LBA (0Eh)
ipt_2:
	inc	byte [ppcount] ; primary dos partition count
ipt_3:
	mov	[part_table_sys_id+di], ah  ; Partition Type (FS type)
	
	mov	al, [si+ptBootable]

	and	al, al
	jz	short ipt_4

	cmp	byte [apcount], 1 ; check (previous) active partition count
	;jnb	short ipt_stc  ; invalid (if it is not zero here)
	jnb	short ipt_16

	cmp	al, 80h  ; active partition sign/flag?
	;jne	short ipt_stc  ; invalid flag
	je	short ipt_11
ipt_16:
	; 15/10/2020
	mov	al, 0FFh ; Invalid/Defective partition flag
	jmp	short ipt_4
ipt_11:
	inc	byte [apcount]  ; active partition count = 1
ipt_4:
	mov	[part_table_boot_ind+di], al

	;mov	al, [heads]
;ipt_4_fix:
	;dec	al
	mov	ah, [si+ptBeginHead]
	;cmp	al, ah
	;;jb	short ipt_retn ; invalid
	;jb	ipt_heads_fix ; 10/02/2019
	mov	[part_table_start_head+di], ah
	mov	ah, [si+ptEndHead]
	;cmp	al, ah
	;;jb	short ipt_retn ; invalid
	;jb	short ipt_heads_fix ; 10/02/2019
	mov	[part_table_end_head+di], ah
	;mov	al, [sectors]
	mov	bh, [si+ptBeginSector]
	mov	ah, bh
	and	ah, 3Fh ; 63
	;cmp	al, ah
	;jb	short ipt_retn ; invalid
	mov	[part_table_start_sector+di], ah
	mov	dh, [si+ptEndSector]
	mov	ah, dh
	and	ah, 3Fh ; 63
	;cmp	al, ah
	;jb	short ipt_retn ; invalid
	mov	[part_table_end_sector+di], ah
	shr	bh, 6
	mov	bl, [si+ptBeginCylinder]
	;mov	ax, [cylinders]
	;dec	ax	
	;cmp	ax, bx
	;jb	short ipt_retn ; invalid
	mov	[part_table_start_cyl+di], bx
	shr	dh, 6
	mov	dl, [si+ptEndCylinder]
	;cmp	ax, dx
	;jb	short ipt_retn ; invalid
	mov	[part_table_end_cyl+di], dx

	mov	ax, [si+ptStartSector]
	mov	dx, [si+ptStartSector+2]

	mov	[part_table_rel_sec_lw+di], ax
	mov	[part_table_rel_sec_hw+di], dx

	or	ax, dx
	;jz	short ipt_stc ; invalid (start sector must not be zero)
	jnz	short ipt_12
	; 15/10/2020
	mov	byte [part_table_boot_ind+di], 0FFh 
				; Invalid/Defective partition flag
ipt_12:
	mov	ax, [si+ptSectors]
	mov	dx, [si+ptSectors+2]

	mov	bx, dx
	or	bx, ax
	;jz	short ipt_stc	; invalid (zero size of partition)
	jnz	short ipt_6 ; 10/02/2019

	; 15/10/2020
	mov	byte [part_table_boot_ind+di], 0FFh 
				; Invalid/Defective partition flag

	;cmp	dx, [total_sectors+2]
	;ja	short ipt_stc	; invalid (partition size > disk capacity)
	;jb	short ipt_6
	;;cmp	ax, [total_sectors] ; (ax + 1) <= total sectors
	;jb	short ipt_6
	
;ipt_stc:
;	; invalid MBR data
;	;stc
;ipt_retn:
;	retn

;ipt_heads_fix:
	; 10/02/2019
	; AL = [heads] - 1

	;test	byte [cylinders], 1
	;jnz	short ipt_stc ; odd cylinder count (can not be shifted)
	
	;inc	al ; = [heads]
	;cmp	al, 8
	;ja	short ipt_stc ; this fix is needed for <= 16 heads (& 17 spt)
	;shl	al, 1
	;mov	[heads], al ; heads = heads*2
	;shr	word [cylinders], 1 ; cylinders = cylinders/2
	;jmp	ipt_4_fix

ipt_6:
	mov	[part_table_num_sec_lw+di], ax
	mov	[part_table_num_sec_hw+di], dx

	add	ax, [part_table_rel_sec_lw+di]
	adc	dx, [part_table_rel_sec_hw+di]
	;jc	short ipt_retn  ; invalid
	; 27/10/2020
	jc	short ipt_13

	cmp	dx, [total_sectors+2]
	;ja	short ipt_stc	; invalid (partition end > disk capacity)
	jb	short ipt_7
	; 27/10/2020
	ja	short ipt_13
	cmp	ax, [total_sectors] ; ax <= total sectors
	;ja	short ipt_stc	; invalid (partition end > disk capacity)
	jna	short ipt_7
ipt_13:
	; 15/10/2020
	mov	byte [part_table_boot_ind+di], 0FFh 
				; Invalid/Defective partition flag 
ipt_7:
	; 27/10/2020
	mov	ax, 1022 ; CHS cylinder number limit
	cmp	[part_table_start_cyl+di], ax
	ja	short ipt_19 ; = 1023
	inc	ax ; 1023
	cmp	[part_table_end_cyl+di], ax
	jb	short ipt_14  ; < 1023
ipt_19:
	; 27/10/2020
	; set cylinder number if the partition's start or end sector is
	; at the beyond of chs limit 
	; ax = 1022 -> set start cylinder
	; ax = 1023 -> set end cylinder
	call	cylindernumberfix
ipt_14:
	add	si, 16
	loop	ipt_5
	
	; OK!

	;clc
	retn

ipt_5:
	add	di, 18
	jmp	ipt_0

ipt_8:
	; Empty partition table check (all of 16 bytes must be 0)
	push	cx
	push	si ; 16/10/2020
	mov	cl, 8
ipt_9:
	lodsw
	or	ax, ax
	jnz	short ipt_10
	loop	ipt_9
	pop	cx ; 16/10/2020  ; (discard si)
	pop	cx
	loop	ipt_5
	
	;clc
	retn
ipt_10:
	;pop	cx
	; invalid
	;stc
	; 27/10/2020
	; 15/10/2020
	;mov	byte [part_table_boot_ind+di], 0FFh
				; Invalid/Defective partition flag 
	;retn
	; 16/10/2020
	; 31/10/2020
	;mov	byte [part_table_sys_id+di], 0	; Empty partition
	pop	si
	pop	cx
	jmp	ipt_16

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; set cylinder number to partition's start or end sector 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 27/10/2020

cylindernumberfix:
	; ax = 1022 -> set start cylinder
	; ax = 1023 -> set end cylinder
	; di = partition table structure offset

	; modified registers: ax, dx, bx
	
	cmp	ax, 1023
	jnb	short cnf_4 ; set end cylinder
	
	cmp	byte [part_table_start_head+di], 0FEh ; 254
	jne	short cnf_4 ; no need to fix (start cyl) 
			; 30/10/2020
	; 28/10/2020
	cmp	byte [part_table_start_sector+di], 3Fh ; 63
	jne	short cnf_4 ; no need to fix (start cyl)
			; 30/10/2020

	mov	ax, [part_table_rel_sec_lw+di] ; start sector, lw
	mov	dx, [part_table_rel_sec_hw+di] ; start sector, hw
	push	cx
	mov	cx, [hs] ; [heads*spt]
	call	div32
		; dx:ax = cylinder number, dx = 0, quotient
		; bx = remainder
	pop	cx
	or 	dx, dx
	jnz	short cnf_2 ; invalid
	cmp	ax, [part_table_start_cyl+di]
	;je	short cnf_5
	je	short cnf_1
	jb	short cnf_2 ; invalid
	mov	[part_table_start_cyl+di], ax ; change it
	;jmp	short cnf_5 
cnf_1:
	cmp	byte [part_table_end_head+di], 0FEh ; 254
	jne	short cnf_2 ; no need to fix (also invalid)
	
	; 28/10/2020
	cmp	byte [part_table_end_sector+di], 3Fh ; 63
	je	short cnf_5 ; fix
	; no need to fix (also invalid)
cnf_2:
	mov	byte [part_table_boot_ind+di], 0FFh ; invalid PTE
cnf_3:
	retn
cnf_4:
	cmp	byte [part_table_end_head+di], 0FEh ; 254
	jne	short cnf_3 ; no need to fix (end cyl)
	
	; 28/10/2020
	cmp	byte [part_table_end_sector+di], 3Fh ; 63
	jne	short cnf_3 ; no need to fix (end cyl)
cnf_5:
	mov	ax, [part_table_rel_sec_lw+di] ; start sector, lw
	mov	dx, [part_table_rel_sec_hw+di] ; start sector, hw

	add	ax, [part_table_num_sec_lw+di] ; number of sectors, lw 
	adc	dx, [part_table_num_sec_hw+di] ; number of sectors, hw 
	;jc	short cnf_2

	sub	ax, 1
	sbb	dx, 0	
		; dx:ax = end sector
	push	cx
	mov	cx, [hs] ; [heads*spt]
	call	div32
		; dx:ax = cylinder number, dx = 0, quotient
		; bx = remainder
	pop	cx
	or 	dx, dx
	jnz	short cnf_2 ; invalid
	cmp	ax, [part_table_end_cyl+di]
	je	short cnf_3 ; same cylinder number
	jb	short cnf_2 ; invalid
	mov	[part_table_end_cyl+di], ax ; change it
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; sort partition table in (ending) cylinder number order
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 25/10/2020 (fdisk3.s)

	; 02/02/2019 (hdimage.s)
	
sort_partition_table:

	; INPUT -> none
	; OUTPUT -> none

	xor	bx, bx
	;jmp	short sortpt_2 ; 25/10/2020
sortpt_1:
	mov	[bx+sort], bl ; 0
	inc	bl
sortpt_2:
	cmp	bl, 4 ; number of partition table entries to sort
	jb	short sortpt_1

	; Do a bubble sort

	jmp	short sortpt_4
sortpt_3:
	; Sort until we don't do a swap

	or	cl, cl ; sort changed ?
	jz	short sortpt_8 ; no
sortpt_4:
	xor	cl, cl

	mov	bl, 1

	mov	dl, 18  ; partition table structure size
	jmp	short sortpt_6
sortpt_5:
	inc	bl

	cmp	bl, 4
	jnb	short sortpt_3

sortpt_6:
	mov	al, [bx+sort]

	mul	dl

	mov	si, ax

	mov	di, [part_table_end_cyl+si]

	mov	al, [bx+sort-1]

	mul	dl ; * 18

	xchg	di, ax

	cmp	[part_table_end_cyl+di], ax ; previous > current
	ja	short sortpt_7 ; swap order indicators
		; 31/10/2020
		; current end cyl >= previous end cyl
	; 31/10/2020
	jb	short sortpt_5  
		; current end cyl > previous end cyl
	
	and	ax, ax ; cylinder 0 ?
	jnz	short sortpt_5 ; no
		; current end cyl = previous end cyl, cyl > 0
	
	; current end cyl = previous end cyl = 0

	; If current entry is empty partition entry
	; and previous entry is not empty partition entry
	; swap them.
	;
	mov	ax, [part_table_num_sec_hw+di] ; previous entry
	or	ax, [part_table_num_sec_lw+di]
	jz	short sortpt_5

	mov	ax, [part_table_num_sec_hw+si] ; current entry
	or	ax, [part_table_num_sec_lw+si]
	jnz	short sortpt_5
sortpt_7:
	; Swap the order indicators

	mov	ax, [bx+sort-1]
	xchg	ah, al
	mov	[bx+sort-1], ax

	mov	cl, 1 ; sort changed
	jmp	short sortpt_5

sortpt_8:
	; 30/10/2020
	;mov 	byte [p_sorted], 1 ; 04/02/2019

	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; find free space before/after/between partitions
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 03/02/2019

find_part_free_space:  ; find/calculate free space for partitions
	; 15/02/2019

	; INPUT -> 
	;	AL = 0 -> calculate for primary/MBR partitions 
	;	AL = 5 -> calculate for extended partition
	; OUTPUT ->
	;	CX = the largest free space/cylinders (between partitions)
	;	AX = Free space index (gap number) - from 0 to 4 -
	;	BX = Free space structure offset (for the largest free space)
	;
	;	22/02/2019
	;	[freespace_count] = number of free spaces/gaps

	mov	[p_type], al

	; Sort the partition table

	call	sort_partition_table ; 16/02/2019

	; Initialize free space to zero

	; 15/02/2019
	mov	di, fspc ; free_space.space
	mov	cx, 5*8 ; (5*16/2)
	xor	ax, ax ; 0
	rep	stosw

	mov	[freespace_count], al ; 0
	; 22/02/2019
	;mov	[last_found_partition], al ; 0

	mov	[_i_], ax ; 0
	;jmp	short fpfs_2
	; 31/10/020
	jmp	short fpfs_3
fpfs_1:	
	inc	byte [_i_]
;;fpfs_2:
	cmp	byte [_i_], 4
	jb	short fpfs_3
	jmp	fpfs_13
fpfs_3:
	; Find space between start of disk and first partition

	;mov	ax, [_i_]
	mov	bx, [_i_] ; 31/10/2020
fpfs_2:
	;mov	bx, ax ; 31/10/2020
	;mov	al, [sort+bx]
	mov	cl, [sort+bx] ; 15/02/2019

	;mov	cl, 18 ; partition table structure size = 18 bytes
	mov	al, 18 ; 15/02/2019
	mul	cl
	mov	bx, ax

	cmp	byte [part_table_sys_id+bx], 0
	je	short fpfs_1

	mov	[last_found_partition], cl ; 15/02/2019

	;xor	cx, cx
	xor	cl, cl ; 0 	
	;mov	al, 1 ; LBA = 1 (after MBR) ; ah = 0
	; 03/11/2020
	mov	al, [sectors] ; 63 or 17 ; 03/11/2020

	cmp	byte [p_type], 5  ; EXTENDED
	jne	short fpfs_4

	; This is a special case - the extended partition can not start
	; on cylinder 0 due to its architecture. Protect against that here

	; 13/02/2019
	; LBA value of free space start
	;mov	al, [sectors] ; 03/11/2020
	mul	byte [heads]
		; ax = start sector (for Extended Partition)
	inc	cl ; 1  ; cx = start cylinder = 1
fpfs_4:
	; Found a partition, get the space

	; 15/02/2019
	mov	dx, [part_table_start_cyl+bx] 
		       ; Start cylinder of the 1st partition (as sorted)
	cmp	dx, cx ; (cx=0 for primary partition, cx=1 for extd partition)
	jna	fpfs_9 ; It is accepted as free (partition) space
		       ; if this space between masterboot sector and partition 1
		       ; has 1 cylinder (heads*spt) size at least.
	; 22/02/2019
	xor	di, di  ; 0 ; Reset Free Space Table offset

	inc	byte [freespace_count]	; mov byte [freespace_count], 1

	mov	[free_space.start], cx ; 0 (for PP) or 1 (for EP)
	
	; non-aligned address of start sector (for the 1st partition as sorted)
	; NOTE: later, start sector will be moved to (chs) head 1 and sector 1
	;	if new partition will be selected as a primary dos partition.
	;	(But, start sector -LBA- will not be changed 
	;	 if it is a non-dos or user input type partition.. unless
	;	 cylinder boundary alignment option is used.)
	 	
	mov	[free_space.startsector], ax ; = [sectors]*[heads] (for EP)
					     ; or 1 (for PP)
	;mov	[free_space.startsector+2], 0

	; free space ends before start of next valid partition
	mov	ax, dx	; start cylinder of partition 1 (as sorted)
	sub	dx, cx	; cylinder count of free space 1 (gap 1)
	dec	ax	; end cylinder of free space 1 (gap 1)
	mov	[free_space.end], ax 
	mov	[free_space.space], dx

	; save free space (1) as (non-aligned) sector count
	mov	ax, [part_table_rel_sec_lw+bx]
	mov	dx, [part_table_rel_sec_hw+bx]
	sub	ax, [free_space.startsector]
	sbb	dx, 0
	mov	[free_space.sectors_unused], ax
	mov	[free_space.sectors_unused+2], dx

	;mov	ax, [free_space.space]

	;call	cylinders_to_sectors

	;mov	[free_space.sectors_unused], ax
	;mov	[free_space.sectors_unused+2], dx

	mov	cx, [cylinders]		; Total (disk) cylinders -divisor-
	mov	bx, [free_space.space]	; Partition cylinders -dividend-

	call	cylinders_to_percent

	mov	[free_space.percent_unused], ax

	; 15/02/2019
	;mov	bl, [_i_]
	;xor	bh, bh
	;mov	al, [sort+bx]

	;mov	[last_found_partition], al

	; 31/10/2020
	jmp	fpfs_9

	; Look for space between the rest of the partitions

fpfs_7:
	; ah = 0
	;mov	al, [_i_]
	;;cbw
	;mov	bx, ax
	; 22/02/2019
	mov	bx, [_i_]

	; 15/02/2019
	;mov	al, [sort+bx]
	mov	cl, [sort+bx]
	;mov	bl, 18
	;mul	bl
	mov	al, 18
	mul	cl
	mov	si, ax
	
	cmp	byte [part_table_sys_id+si], 0
	je	short fpfs_9

	; 15/02/2019
	xchg	[last_found_partition], cl

	; Check to see if more than one partition on a cylinder
	; If so, leave the space at zero */

	; NOTE:
	; It is accepted as valid free (partition) space
	; if free space (gap) between partitions
	; has 1 cylinder (heads*spt) size at least.

	mov	al, 18 ; Partition tables/data structure size = 18 bytes
	mul	cl
	mov	bx, ax  ; ah = 0

	mov	dx, [part_table_end_cyl+bx]   ; end cyl. of previous partition 
	mov	ax, [part_table_start_cyl+si] ; start cyl. of current partition

	inc	dx  ; start cylinder of free space is after the end cylinder of
		    ; the previous partition (as sorted)

	cmp	ax, dx ; and it must be before than the next partition (as sorted)
	jna	short fpfs_9 ; je short fpfs_9
		
	; No, things are normal
	; Get space between the end of the last one and the start of the next one

	; 22/02/2019
	;add	di, 16
	mov	di, [freespace_count] 
	shl	di, 4	; * 16 ; next entry offset (in free space table)

	inc	byte [freespace_count]

	mov	cx, ax
	sub	cx, dx
	dec	ax
	mov	[free_space.start+di], dx
	mov	[free_space.end+di], ax
	mov	[free_space.space+di], cx

	;mov	ax, [free_space.space+di]
	;call	cylinders_to_sectors
	;mov	[free_space.sectors_unused+di], ax
	;mov	[free_space.sectors_unused+2+di], dx

	; calculate and save (non-aligned) free sector count between partitions

	mov	ax, [part_table_rel_sec_lw+bx]
	mov	dx, [part_table_rel_sec_hw+bx]
	add	ax, [part_table_num_sec_lw+bx]
	adc	dx, [part_table_num_sec_hw+bx]

	mov	cx, [part_table_rel_sec_lw+si]
	mov	bx, [part_table_rel_sec_hw+si]

	mov	[free_space.startsector+di], ax
	mov	[free_space.startsector+2+di], dx

	sub	cx, ax
	sbb	bx, dx

	mov	[free_space.sectors_unused+di], cx
	mov	[free_space.sectors_unused+2+di], bx

fpfs_8:
	; NOTE: Percentage is based on cylinder-boundary aligned partition size.
	; (total disk cylinders and partition's cylinder count are used for that)

	mov	cx, [cylinders] ; -divisor-
	mov	bx, [free_space.space+di] ; -dividend-
	call	cylinders_to_percent
	
	mov	[free_space.percent_unused+di], ax
	
	; ah = 0

	; 15/02/2019
	;mov	bl, [_i_]
	;xor	bh, bh
	;mov	al, [sort+bx]
	;	
	;mov	[last_found_partition], al

fpfs_9:
	inc	byte [_i_]
fpfs_10:
	cmp	byte [_i_], 4
	jnb	short fpfs_11

	jmp	fpfs_7

fpfs_11:
	; Find the space between the last partition and the end of the disk
	; Make sure that freespace cannot become negative

	mov	al, [last_found_partition]
	mov	cl, 18  ; Partition table structure size = 18 bytes
	mul	cl
	mov	bx, ax
	mov	cx, [cylinders]
	dec	cx ; 15/02/2019
		   ; (min. cylinder count = 1 for a valid/usable free space)
	mov	ax, [part_table_end_cyl+bx]
	;cmp	[part_table_end_cyl+bx], cx
	cmp	ax, cx	; cx = end cylinder of the disk
	jb	short fpfs_12
	jmp	fpfs_15
fpfs_12:
	; 22/02/2019
	mov	di, [freespace_count]
	shl	di, 4 ; * 16  ; next entry offset (in free space table)

	inc	byte [freespace_count]

	mov	[free_space.end+di], cx	; end cyl. of free space 5 (gap 5)
	;mov	ax, [part_table_end_cyl+bx]
	
	;mov	dx, cx
	;sub	dx, ax
	;mov	[free_space.space+di], dx ; di = 4*16
	; 12/03/2021
	; ax = end cylinder of the last partition
	; cx = end cylinder of the disk
	sub	cx, ax
	mov	[free_space.space+di], cx ; di = 4*16

	inc	ax
	mov	[free_space.start+di], ax
	
	;inc	cx ; [cylinders]
	;mov	ax, [free_space.space+si]
	;call	cylinders_to_sectors
	;mov	[free_space.sectors_unused+di], ax
	;mov	[free_space.sectors_unused+2+di], dx

	; calculate and save (non-aligned) free sector count

	; 22/02/2019
	mov	ax, [part_table_rel_sec_lw+bx]
	mov	dx, [part_table_rel_sec_hw+bx]
	add	ax, [part_table_num_sec_lw+bx]
	adc	dx, [part_table_num_sec_hw+bx]

	mov	[free_space.startsector+di], ax
	mov	[free_space.startsector+2+di], dx

	mov	cx, [total_sectors]
	mov	bx, [total_sectors+2]
	sub	cx, ax
	sbb	bx, dx
	mov	[free_space.sectors_unused+di], cx
	mov	[free_space.sectors_unused+2+di], bx

	mov	cx, [cylinders]
	mov	bx, [free_space.space+di]
	call	cylinders_to_percent
	mov	[free_space.percent_unused+di], ax
	jmp	short fpfs_15

fpfs_13:
	; No partitions found, show entire space as free

	; 22/02/2019
	inc	byte [freespace_count]	; mov byte [freespace_count], 1
	
	; 15/02/2019
	;sub	ax, ax
	; ah = 0 ; 31/10/2020
	sub	dx, dx

	;inc	al ; LBA = 1 (after MBR) ; ah = 0
	mov	al, 1 ; 25/02/2019

	;This is a special case - the extended partition can not start
	;on cylinder 0 due to its architecture. Protect against that here

	cmp	byte [p_type], 5 ; EXTENDED
	jne	short fpfs_14
		
	inc	dl

	; 15/02/2019
	; LBA value of free space start
	mov	al, [sectors]
	mul	byte [heads]
		; ax = start sector (for Extended Partition)
fpfs_14:
	mov	[free_space.start], dx ; 0 or 1

	; non-aligned address of start sector (for the 1st partition as sorted)
	; NOTE: later, start sector will be moved to (chs) head 1 and sector 1
	;	if new partition will be selected as a primary dos partition.
	 	
	mov	[free_space.startsector], ax ; = [sectors]*[heads] (for EP)
					     ; or 1 (for PP)
	;mov	[free_space.startsector+2], 0

	mov	ax, [cylinders] ; disk capacity
	mov	cx, ax
	dec	ax
	mov	[free_space.end], ax
	sub	ax, dx
	inc	ax
	mov	[free_space.space], ax

	mov	ax, [total_sectors]
	mov	dx, [total_sectors+2]
	sub	ax, [free_space.startsector]
	sbb	dx, 0
	mov	[free_space.sectors_unused], ax
	mov	[free_space.sectors_unused+2], dx

	;mov	cx, [cylinders] 
	mov	bx, [free_space.space]
	call	cylinders_to_percent
	mov	[free_space.percent_unused], ax

fpfs_15:
	; Find largest free space
	sub	cx, cx
	; 22/02/2019
	sub	ax, ax
	xor	bx, bx
	mov	dl, [freespace_count]
	or	dl, dl
	jz	short fpfs_19
	mov	[_i_], dl
fpfs_16:
	mov	dx, [free_space.space+bx]
	cmp	dx, cx
	jbe	short fpfs_17
	; 22/02/2019
	mov	cx, dx ; Largest free space
	mov	ax, bx
fpfs_17:
	dec	byte [_i_]
	jz	short fpfs_18

	add	bx, 16 ; Free space structure size
	jmp	short fpfs_16
fpfs_18:
	; 22/02/2019
	mov	bx, ax ; offset (from 0 to 64)
	shr	al, 4  ; index (from 0 to 4)
fpfs_19:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; find enough free space
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;	; 15/02/2019
;find_enough_free_space:
;	; Find enough free space
;	;
;	; INPUT:
;	;	AX = Requested space (in cylinders)
;	;	22/02/2019
;	;	[freespace_count] = number of free spaces/gaps
;	; OUTPUT:
;	;	AX = Available space
	;	DX = Requested space
;	;	If CF = 0 -> AX >= DX
;	;	If CF = 1 -> AX < DX
;	;	CX = Index of available space in free space structure
;	;	     (GAP number, 0 to 4) - if AX > 0 -
;
;	mov	dx, ax ; 22/02/2019
;	sub	ax, ax
;	xor	bx, bx
;	; 22/02/2019
;	mov	ch, [freespace_count]
;	mov	[_i_], ax ; 0
;	jmp	short fefs_1
;fefs_0:
;	;mov	al, 16
;	;mul	byte [_i_]
;	;mov	bx, ax
;	mov	bl, [_i_]
;	shl	bl, 4 ; * 16
;fefs_1:
;	mov	ax, [free_space.space+bx]
;	and	ax, ax
;	jz	short fefs_2 ; not a free space
;	mov	cl, [_i_] 
;	cmp	ax, dx
;	jnb	short fefs_3 ; enough space
;fefs_2:
;	; 22/02/2019
;	inc	byte [_i_]
;	cmp	byte [_i_], ch	
;	jb	short fefs_0
;	sub	ch, ch
;	stc
;	retn
;fefs_3:
;	xor	ch, ch
;	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; find enough free sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 15/02/2019
find_enough_free_sectors:
	; Find (first) enough free space in sectors
	;
	; INPUT:
	;	DX:AX = Requested space (as non-aligned free sectors)
	;	22/02/2019
	;	[freespace_count] = number of free spaces/gaps
	; OUTPUT:
	;	DX:AX = Available space
	;	If CF = 0 -> DX:AX >= Request
	;	If CF = 1 -> DX:AX < Request
	;	CX = Index of available space in free space structure
	;	     (GAP number, 0 to 4) - if DX:AX > 0 -

	xor	di, di
	xor	si, si
	xor	bx, bx
	; 22/02/2019
	mov	ch, [freespace_count]
	mov	[_i_], bx ; 0
	jmp	short fefss_1
fefss_0:
	;mov	al, 16
	;mul	byte [_i_]
	;mov	bx, ax
	mov	bx, [_i_]
	shl	bl, 4 ; * 16
fefss_1:
	add	bx, free_space.sectors_unused
	cmp	dx, [bx+2]
	jb	short fefss_7
	ja	short fefss_2
	cmp	ax, [bx]
	je	short fefss_8
	jb	short fefss_7 ; 18/02/2019
fefss_2:
	; 30/10/2020
	;cmp	word [bx], 0
	;jne	short fefss_3
	;cmp	word [bx+2], 0
	;je	short fefss_6	
fefss_3:
	cmp	di, [bx+2]
	ja	short fefss_6
	je	short fefss_4
	mov	di, [bx+2]
	jmp	short fefss_5
fefss_4:
	cmp	si, [bx]
	jnb	short fefss_6
fefss_5:
	mov	si, [bx]
	; 22/02/2019
	mov	cl, [_i_]
fefss_6:
	inc	byte [_i_]
	cmp	byte [_i_], ch ; [freespace_count]
	jb	short fefss_0
	
	mov	ax, si
	mov	dx, di
	xor	ch, ch
	stc
	retn
fefss_7:
	mov	ax, [bx]
	mov	dx, [bx+2]
fefss_8:
	mov	cx, [_i_]
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get first free partition table entry
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	; 16/02/2019
get_first_free_pte:
	; Find free partition table entry
	;
	; INPUT:
	;	none
	; OUTPUT:
	;	If CF = 0 -> CX = partition table entry number
	;	If CF = 1 -> there is not a free entry in partition table

	mov	si, MasterBootBuff+pTableOffset
	xor	cx, cx
gffp_1:
	mov	al, [si+ptFileSystemID] ; 23/02/2019

	and	al, al
	jz	short gffp_3 ; empty

	cmp	cl, 3
	jnb	short gffp_2

	inc	cl
	add	si, 16 ; Partition table entry size
	jmp	short gffp_1
gffp_2:
	; CL = 3
	stc
	retn
gffp_3:
	; CL = PTE number (0 to 3)
	retn 	

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; convert cylinder count to sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 03/02/2019

	; 15/03/2021
;cylinders_to_sectors:
	; INPUT:
	;	ax = Cylinders (Total or partition's cylinder count)
	; OUTPUT:
	;	dx:ax = Sectors

	;;mov	dx, [heads]
	;;mul	dx
	;; 30/10/2020
	;mul	word [heads]	
	;; dx:ax = Number of tracks (cylinders*heads)
	;mov	cx, [sectors]
	;call	mul32
	;	; dx:ax = Number of sectors 
	;retn
		
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; convert cylinder count to percent of disk size (total cylinders)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 03/02/2019

cylinders_to_percent:

	; INPUT:
	;	bx = Number of cylinders (of partition) -dividend-
	;	cx = Total cylinders -divisor-
	; OUTPUT:
	;	ax = Percentage

	;  if (cylinders_in == 0)
	;	 percentage_out = 0;
	;  else if (total_cylinders == 0)
	;           percentage_out = 0;
	;       else

	or	bx, bx
	jz	short ctpc_6 ; ax = 0 = percentage_out 
ctpc_1:
	or	cx, cx
	jz	short ctpc_6

	mov	ax, 100
	mul	bx ; [cylinders_in]

		; dx:ax = Dividend

	call	div32 ; 100*cylinders_in / total_cylinders
		; DX:AX = Quotient
		; BX = Remainder

	; ax = percentage_out

	; 12/03/2021
	shr	cx, 1

	cmp	bx, cx	; is remainder >= total_cylinders/2 ?
	jb	short ctpc_5 ; No. 
ctpc_4:
	inc	ax
ctpc_5:
	cmp	ax, 100
	jbe	short ctpc_6
	mov	ax, 100
ctpc_6:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display partition table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 04/02/2019

	; 12/03/2021
display_partition_table_x:
	mov	al, 5 ; display extended partition table
	jmp	short dpt_x

display_partition_table:

	; INPUT:
	;	al = 0 for MBR
	;	   = 5 for EBR (logical drives in extended partition)
	; OUTPUT:
	;	none

	; 12/03/2021
	xor	al, al ; display primary partition table
dpt_x:
;pt_positions:
n_pos	equ 30 ; 1 byte	

	mov	[_e_], al	

	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	cmp	byte [_e_], 5
	jne	short dpt_1
dpt_0:
	mov	bp, ext_table_boot_ind
	mov	al, 'E'
	jmp	short dpt_4
dpt_1:
	mov	bp, part_table_boot_ind
	mov	al, 'M'
dpt_4:
	mov	si, p_table_header
	mov	[si+n_pos], al
       	call 	print_msg
   
	xor	bx, bx
	mov	[_i_], bx
dpt_5:
	inc	byte [_i_]

	; BX = partition entry (0 to 3)
	; BP = partition table structure address

	call	display_pt_entry  ; display partition table row

	mov	bx, [_i_] 
	
	cmp	bl, 4
	jb	short dpt_5

	mov	si, p_table_footer
       	call 	print_msg

	; 27/02/2019
	mov	di, str_disk_sectors

	cmp	byte [_e_], 5
	je	short dpt_9 

	; display total disk sectors

	;mov	di, str_disk_sectors

	;cmp	byte [di], '0'
	;jnb	short dpt_8

        mov	ax, [total_sectors]
        mov	dx, [total_sectors+2]

	call	dpt_10
dpt_8:
	mov	si, msg_disk_sectors
dpt_11:
	call	print_msg

	mov	si, str_disk_sectors
	call	print_msg

	mov	si, CRLF
	call	print_msg

	retn

dpt_9:
	; display extended partition size

	mov	ax, [ep_Size]
        mov	dx, [ep_Size+2]

	call	dpt_10

	mov	si, msg_ep_size
	jmp	short dpt_11

dpt_10:
	mov	si, sp
	mov	cx, 10
dpt_6:
	call	div32
	
	add	bl, '0'
	push	bx
	and	ax, ax
	jnz	short dpt_6
	and	dx, dx
	jnz	short dpt_6

	sub	si, sp
	shr	si, 1
dpt_7:
	pop	ax
	stosb
	dec	si
	jnz	short dpt_7

	xor	al, al
	stosb

	; 27/02/2019
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display partition table entry/row
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 04/02/2019

struc ptbl

.boot_ind:	resb 1
.start_head:	resb 1
.start_sector:	resb 1
.start_cyl:	resw 1
.sys_id:	resb 1
.end_head:	resb 1
.end_sector:	resb 1
.end_cyl:	resw 1
.rel_sec_lw:	resw 1
.rel_sec_hw:	resw 1
.num_sec_lw:	resw 1
.num_sec_hw:	resw 1
.size:

endstruc

display_pt_entry:
	; 24/10/2020 (fdisk3.s)

	; INPUT:
	;	bl = partition entry
	;	bh = 0
	;	bp = partition table structure address
	; OUTPUT:
	;	none

; 24/10/2020 (fdisk3.s)
;pt_positions:
p_pos	equ 3  ; 1 byte
s_pos	equ 6  ; 2+1 byte
bh_pos	equ 10 ; 2+1 bytes
bs_pos	equ 14 ; 2+1 bytes
bc_pos	equ 18 ; 2+1 bytes
fs_pos  equ 22 ; 2+1 bytes
eh_pos  equ 26 ; 2+1 bytes
es_pos	equ 30 ; 2+1 bytes
ec_pos	equ 34 ; 2+1 bytes
rs_pos	equ 42 ; 10 bytes
ns_pos	equ 53 ; 10 bytes
fsx_pos equ 64 ; 14 bytes

; 2019-2020 (hdimage.s)
;;pt_positions:
;p_pos	equ 7  ; 1 byte
;s_pos	equ 9  ; 2+1 byte
;bh_pos	equ 13 ; 2+1 bytes
;bs_pos	equ 17 ; 2+1 bytes
;bc_pos	equ 21 ; 2+1 bytes
;fs_pos equ 25 ; 2+1 bytes
;eh_pos equ 29 ; 2+1 bytes
;es_pos	equ 33 ; 2+1 bytes
;ec_pos	equ 37 ; 2+1 bytes
;rs_pos	equ 42 ; 7 bytes
;ns_pos	equ 52 ; 7 bytes
;fsx_pos equ 61 ; 14 bytes

	push	 bp ; 23/02/2019

	or	bl, bl
	jz	short dpte_0

	;xor	bh, bh
	mov	al, ptbl.size ; 18
	mul	bl
	add	bp, ax
dpte_0:
	cmp	byte [bp+ptbl.sys_id], 0
	jna	dpte_9

	mov	bh, 'h'

	mov	si, pte_row

	; clear partition table display buffer/row
	mov	di, si
	mov	cx, 40
	mov	ax, 2020h
	rep	stosw

	mov	al, bl
	add	al, '1'
	mov	[si+p_pos], al  ; partition number '1' to '4'

	; partition status, type and CHS parameters
	; (as hexadecimal number)

	;mov	al, [bp+ptbl.boot_ind]
	mov	al, [bp]
	call	byte_to_hex

	mov	[si+s_pos], ax
	mov	[si+s_pos+2], bh ; 'h'

	mov	al, [bp+ptbl.start_head]
	call	byte_to_hex

	mov	[si+bh_pos], ax
	mov	[si+bh_pos+2], bh ; 'h'

	mov	cx, [bp+ptbl.start_cyl]
	; 28/10/2020
	cmp	cx, 1023
	jna	short dpte_10 ; cyl number is in CHS limit
	mov	cx, 1023 ; fix cylinder number (to it's PTE value)
			 ; to correct display PTE (CHS bytes)
dpte_10:
	shl	ch, 6
	mov	al, [bp+ptbl.start_sector]
	or	al, ch
	call	byte_to_hex

	mov	[si+bs_pos], ax
	mov	[si+bs_pos+2], bh ; 'h'

	;mov	al, [bp+ptbl.start_cyl]
	mov	al, cl
	call	byte_to_hex

	mov	[si+bc_pos], ax
	mov	[si+bc_pos+2], bh ; 'h'

	mov	al, [bp+ptbl.sys_id]
	call	byte_to_hex

	mov	[si+fs_pos], ax
	mov	[si+fs_pos+2], bh ; 'h'

	mov	al, [bp+ptbl.end_head]
	call	byte_to_hex

	mov	[si+eh_pos], ax
	mov	[si+eh_pos+2], bh ; 'h'

	mov	cx, [bp+ptbl.end_cyl]
	; 28/10/2020
	cmp	cx, 1023
	jna	short dpte_11 ; cyl number is in CHS limit
	mov	cx, 1023 ; fix cylinder number (to it's PTE value)
			 ; to correct display PTE (CHS bytes)
dpte_11:
	shl	ch, 6
	mov	al, [bp+ptbl.end_sector]
	or	al, ch
	call	byte_to_hex

	mov	[si+es_pos], ax
	mov	[si+es_pos+2], bh ; 'h'

	;mov	al, [bp+ptbl.end_cyl]
	mov	al, cl
	call	byte_to_hex

	mov	[si+ec_pos], ax
	mov	[si+ec_pos+2], bh ; 'h'

	; relative (start) sector address (lba)
	; (as decimal number)

        mov	ax, [bp+ptbl.rel_sec_lw]
        mov	dx, [bp+ptbl.rel_sec_hw]

	mov	di, sp
	mov	cx, 10
dpte_1:
	call	div32
	
	add	bl, '0'
	push	bx
	and	ax, ax
	jnz	short dpte_1
	and	dx, dx
	jnz	short dpte_1

	lea	bx, [si+rs_pos+7]

	sub	di, sp
	shr	di, 1
	sub	bx, di	
dpte_2:
	pop	ax
	mov	[bx], al
	dec	di
	jz	short dpte_3
	
	inc	bx
	jmp	short dpte_2

dpte_3:
	; number of sectors)
	; (as decimal number)

        mov	ax, [bp+ptbl.num_sec_lw]
        mov	dx, [bp+ptbl.num_sec_hw]

	mov	di, sp
	;mov	cx, 10
dpte_4:
	call	div32
	
	add	bl, '0'
	push	bx
	and	ax, ax
	jnz	short dpte_4
	and	dx, dx
	jnz	short dpte_4

	lea	bx, [si+ns_pos+7]

	sub	di, sp
	shr	di, 1
	sub	bx, di	
dpte_5:
	pop	ax
	mov	[bx], al
	dec	di
	jz	short dpte_6
	
	inc	bx
	jmp	short dpte_5
dpte_6:
	; set file system name

	mov	al, [bp+ptbl.sys_id]
	mov	di, valid_partitions
	mov	cx, 19
	repnz	scasb
	jz	short dpte_7
	mov	ax, FS_OTHERS
	jmp	short dpte_8
dpte_7:
	sub	di, valid_partitions + 1
	mov	ax, 14
	mul	di
	add	ax, FileSys_Names
dpte_8:
	lea	di, [si+fsx_pos]
	mov	si, ax
	mov	cl, 7
	rep	movsw

	mov	si, pte_row
	call	print_msg
dpte_9:
	pop	bp ; 23/02/2019
	
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; fix partition table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 24/02/2019

partition_table_fix:
	; DELETE or EXIT option for Invalid Partition Table Entry
	; INPUT: 
	;	DS:SI = PTE address in MBR buffer
	
	push	si  ; save PTE address

	mov	ax, si
	sub	ax, MasterBootBuff+446
	shr	al, 4 ; / 16 
	add	al, '1'
	mov	[inv_pte_num], al

	; DS:SI = PTE address in MBR buffer
	call	show_selected_partition

	;mov	si, CRLF
	;call	print_msg

	; Warning message...

	mov	si, msg_inv_pte
	call	print_msg

	pop	di  ; restore PTE address
ptf_0:	
	xor	ah, ah
	int	16h

	cmp	al, 13
	je	short ptf_1

	cmp	al, 27
	jne	short ptf_0

	stc
	retn
ptf_1:
	; Clear that (invalid) PTE
	xor	ax, ax	
	mov	cx, 8
	rep	stosw

	; Clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; display (& edit) EXTENDED (DOS) partition table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 28/02/2019

display_extended_pt:
	;mov	al, 5 ; extended partition (logical drives)
	;call	display_partition_table
	; 12/03/2021
	call	display_partition_table_x

	mov	si, ebr_editing_options
	call	print_msg

	mov	si, enter_opt_num_cancel_msg
	call	print_msg

	mov	si, CRLF
	call	print_msg
dept_1:
	xor	ah, ah
	int	16h

	cmp	al, '0'
	je	short dept_2

	cmp	al, '1'
	je	short edit_ext_table_create
	cmp	al, '2'
	je	short edit_ext_table_delete
		
	cmp	al, 27 ; ESCape
	jne	short dept_1
dept_2:
	jmp	A_42

edit_ext_table_delete:
	; 28/02/2019
	;mov	al, 5 ; extended partition (logical drives)
	;call	display_partition_table
	; 12/03/2021
	call	display_partition_table_x

	mov	si, msg_delete_ldd_q
	call	print_msg

	;mov	si, CRLF
	;call	print_msg
eetd_1:
	xor	ah, ah
	int	16h

	cmp	al, 27
	je	short eetd_2

	and	al, 0DFh
	cmp	al, 'Y'
	je	short eetd_yes
	cmp	al, 'N'
	jne	short eetd_1
eetd_no:
	mov	si, msg_NO ;  Write 'NO' (it may be shown for a moment)
	call	print_msg
eetd_2:	
	mov	si, CRLF  ; Next line
	call	print_msg
	jmp	short display_extended_pt
eetd_yes:
	mov	si, msg_YES ;  Write 'YES' (it may be shown for a moment)
	call	print_msg
	mov	si, CRLF  ; Next line
	call	print_msg
	call	delete_logical_drives
	cmp	byte [ldrives], 0
	ja	short display_extended_pt
	jmp	A_42

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; logical drive count limit error 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

eetc_2:
	mov	si, msg_create_ldd_max_error
eetc_10:
	call	print_msg
	mov	si, msg_press_any_key
	call	print_msg
	xor	ah, ah
	int	16h
	jmp	display_extended_pt

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; no free space in extended partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
	; 05/03/2019
eetc_9:
	mov	si, msg_c_ldd_nofspc_error
	jmp	short eetc_10

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; create logical dos drive in EXTENDED (DOS) partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 19/10/2020 (fdisk3.s) 

edit_ext_table_create:	; 01/03/2019 (hdimage.s)

; Create Method: ; 04/03/2019
;LDD 1 <- LDD_START0, ebr 1st entry <- MBR (extended partition) - EBR 0
;LDD 2 <- LDD_START1, ebr 2nd entry <- EBR 1
;LDD 3 <- LDD_START2, ebr 2nd entry <- EBR 2
;LDD 4 <- LDD_START3, ebr 2nd entry <- EBR 3

	; 01/03/2019
	cmp	byte [epnumber], 0 ; check extended partition
	jna	B_07		; cancel with error message

	; 05/03/2019
	call	check_ext_free_space
	jc	eetc_9	; error, no free space in extented partition

	; 30/10/2020
	mov	[ep_free_sectors], ax
	mov	[ep_free_sectors+2], dx

	cmp	byte [ldrives], 4
	jnb	eetc_2	; error, write program limit message

edit_ext_table_create_x: ; 02/03/2019

	; Get logical drive size/cylinders (request) from user
	call	get_ldd_size
	cmp	word [lcylinders], 1  ; at least 1 cylinder
	jb	display_extended_pt ; cancel

	; 03/03/2019
	mov	al, [ldrives]

	;cmp	byte [ldrives], 1
	;jnb	short eetc_3	; skip verify MBR extended partition

	and	al, al
	jnz	short eetc_3

	xor	bp, bp

	; Read MBR at EBR buffer
	xor	ah, ah ; 19/10/2020
	xor	dx, dx
	mov	bx, ebr_buffer
	call	read_hd_sector
	jc	short eetc_0

	mov	al, [epnumber]
	cbw
	shl	al, 4 ; * 16
	mov	si, ebr_buffer+446
	add	si, ax
	mov	di, MasterBootBuff+446
	add	di, ax
	mov	cx, 8
	repe	cmpsw	; repeat comparising while cx > 0 and zf = 1
	jcxz	eetc_1	; Extended partition entry is not changed 
	; Different PTE (means there is a new extended partition) 
eetc_0:
	; Write current MBR (with modified PT)
	mov	bx, MasterBootBuff
	xor	ax, ax
	;xor	dx, dx
	call	write_hd_sector
	jc	print_error_code ; ! display error msg and then exit !
eetc_1:
	; clear	EBR buffer
	mov	di, ebr_buffer
	xor	ax, ax
	mov	cx, 255
	rep	stosw
	mov	word [di], 0AA55h

	; Set logical dos drive parameters in it's EBR
	mov	di, ebr_buffer+446

	mov	cx, [sectors]
	sub	bx, bx ; 0
	mov	ax, [ep_StartSector]
	mov	dx, [ep_StartSector+2]

	; 03/03/2019
	; set partition start address & size

	mov	[ldd_start], ax
	mov	[ldd_start+2], dx

	; 01/11/2020
	cmp	word [lcylinders], 65535
			; sign for using all of available sectors
	jb	short eetc_11

	mov	ax, [ep_free_sectors]
	mov	dx, [ep_free_sectors+2]
	jmp	short eetc_12
eetc_11:
	mov	al, [heads]
	mul	byte [sectors]
	mul	word [lcylinders]
eetc_12:
	; 01/11/2020
	xor	bp, bp
	mov	[ldd_size], ax
	mov	[ldd_size+2], dx

	; 1st ldd start sector is always 63 or 17 (= spt)
	;sub	ax, cx ; cx = 63 or 17, [sectors]
	;sbb	dx, bx ; sbb dx, 0

	; 01/11/2020
	jmp	eetc_4

	;mov	[di+ptStartSector], cx
	;mov	[di+ptStartSector+2], bx
	;
	;; 01/11/2020
	;mov	[di+ptSectors], ax
	;mov	[di+ptSectors+2], dx
	;
	;mov	cx, ax
	;mov	bx, dx
	;; cx:bx = volume size of logical -dos- drive
	;
	;pop	ax ; ldd's LBA
	;pop	dx
	;; dx:ax = start sector address of ldd
	;
	;; 01/11/2020
	;add	cx, ax
	;adc	bx, dx
	;sub	cx, 1
	;sbb	bx, 0
	;; bx:cx = end sector address of ldd
	;push	cx
	;push	bx
	;
	;jmp	eetc_4

eetc_3:
	; [ldrives] >= 1
	; Read EBR
	;mov	al, [ldrives]
	xor	ah, ah ; 02/11/2020
	shl	al, 2
	mov	bp, ax

	; 03/03/2019
	mov	ax, [bp+ldd_start-4]
	mov	dx, [bp+ldd_start-2]
	mov	bx, ebr_buffer
	call	read_hd_sector
	jc	print_error_code ; ! display error msg and then exit !

	; 04/03/2019
	mov	si, ebr_buffer+446+16 ; 2nd entry (next EBR)
	
	; 03/03/2019
	mov	ax, [bp+ldd_start-4]
	mov	dx, [bp+ldd_start-2]
	add	ax, [bp+ldd_size-4]
	adc	dx, [bp+ldd_size-2]

	mov	cx, [ep_StartSector]
	mov	bx, [ep_StartSector+2]

	mov	[bp+ldd_start], ax
	mov	[bp+ldd_start+2], dx

	push	dx ; ldd's start sector LBA
	push	ax
	
	sub	ax, cx
	sbb	dx, bx
	; dx:ax = offset from start of the ep start sector

	mov	[si+ptStartSector], ax
	mov	[si+ptStartSector+2], dx

	; 01/11/2020
	cmp	word [lcylinders], 65535
			; sign for using all of available sectors
	jb	short eetc_13

	mov	ax, [ep_free_sectors]
	mov	dx, [ep_free_sectors+2]
	jmp	short eetc_14
eetc_13:
	mov	al, [heads]
	mul	byte [sectors]
	mul	word [lcylinders]
eetc_14:
	; 01/11/2020
	mov	[bp+ldd_size], ax
	mov	[bp+ldd_size+2], dx

 	mov	[si+ptSectors], ax
	mov	[si+ptSectors+2], dx

	; 01/11/2020
	mov	cx, ax
	mov	bx, dx
	; cx:bx = volume size of logical -dos- drive

	pop	ax ; ldd's start sector LBA
	pop	dx

	; 01/11/2020
	add	cx, ax
	adc	bx, dx
	sub	cx, 1
	sbb	bx, 0
	; bx:cx = end sector address of ldd
	push	cx
	push	bx

	; calculate CHS from LBA
	call	lba_to_chs
		; ax = cylinder
		; dl = sector
		; dh = head

	mov	byte [si+ptBootable], 0
	mov	[si+ptBeginHead], dh
	mov	[si+ptBeginCylinder], al
	shl	ah, 6
	or	dl, ah		
	mov	[si+ptBeginSector], dl

 	;;mov	ax, [si+ptSectors]
	;;mov	dx, [si+ptSectors+2]
 	;mov	ax, [bp+ldd_size]
	;mov	dx, [bp+ldd_size+2]
	;sub	ax, 1
	;sbb	dx, 0
	;add	ax, [bp+ldd_start]
	;adc	dx, [bp+ldd_start+2]
	;	; dx:ax = end sector
	
	; 01/11/2020
	pop	ax
	pop	dx
	; dx:ax = end sector address of ldd

	call	lba_to_chs
		; ax = cylinder
		; dl = sector
		; dh = head
		; 24/10/2020
		; bl = Extended partition ID
		
	;mov	byte [si+ptFileSystemID], 5 ; EXTENDED
	; 24/10/2020
	mov	[si+ptFileSystemID], bl ; EXTENDED ; 05h or 0Fh
	;
	mov	[si+ptEndHead], dh
	mov	[si+ptEndCylinder], al
	shl	ah, 6
	or	ah, dl	
	mov	[si+ptEndSector], ah

	; Write EBR
	mov	ax, [bp+ldd_start-4]
	mov	dx, [bp+ldd_start-2]
	mov	bx, ebr_buffer
	call	write_hd_sector
	jc	print_error_code ; ! display error msg and then exit !

	; clear	EBR buffer
	mov	di, ebr_buffer
	xor	ax, ax
	mov	cx, 255
	rep	stosw
	mov	word [di], 0AA55h

	mov	di, ebr_buffer+446 ; 1st entry points to LDD

	mov	cx, [sectors]
	sub	bx, bx ; 0
	; 01/11/2020
	mov	ax, [bp+ldd_size]
	mov	dx, [bp+ldd_size+2]
	;sub	ax, cx
	;sbb	dx, bx ; sbb dx, 0 
eetc_4:	
	mov	[di+ptStartSector], cx
	mov	[di+ptStartSector+2], bx

	; 01/11/2020
	; 1st ldd start sector is always 63 or 17 (= spt)
	sub	ax, cx ; cx = 63 or 17, [sectors]
	sbb	dx, bx ; sbb dx, 0

	; 01/11/2020
	mov	[di+ptSectors], ax
	mov	[di+ptSectors+2], dx

	mov	cx, [bp+ldd_size]
	mov	bx, [bp+ldd_size+2]
	; cx:bx = volume size of logical -dos- drive

	mov	ax, [bp+ldd_start]
	mov	dx, [bp+ldd_start+2]
	; dx:ax = start sector address of ldd (EBR addr)

	; 01/11/2020
	add	cx, ax
	adc	bx, dx
	sub	cx, 1
	sbb	bx, 0
	; bx:cx = end sector address of ldd
	push	bx
	push	cx
;eetc_4:
	; 01/11/2020
	; stack = end sector address of ldd
	; dx:ax = start sector address of ldd

	; calculate CHS from LBA

	call	lba_to_chs
		; ax = cylinder
		; dl = sector
		; dh = head

	;mov	byte [di+ptBootable], 0
	mov	[di+ptBeginHead], dh
	mov	[di+ptBeginCylinder], al
	;mov	bl, ah
	;shl	bl, 6
	;or	dl, bl
	; 01/11/2020
	shl	ah, 6
	or	dl, ah
	mov	[di+ptBeginSector], dl

	;add	ax, [lcylinders]
	;dec	ax ; end cylinder
	;mov	bx, ax

	; 01/11/2020
	pop	ax
	pop	dx
	; dx:ax = end sector address of ldd

	call	lba_to_chs
		; ax = cylinder number (0-1023)
		; dl = sector
		; dh = head
		; 01/11/2020
		; cx = cylinder number (0-65535)

	; 02/11/2020
	;mov	[endcyl], cx ; 01/11/2020
	xchg	[endcyl], cx 
		; cx = end cylinder of the ep
		; [endcyl] = end cylinder of the ldd

	;mov	[di+ptFileSystemID], 0
	;mov	cx, [heads]
	;dec	cl
	;mov	[di+ptEndHead], cl
	; 01/11/2020
	mov	[di+ptEndHead], dh

	mov	[di+ptEndCylinder], al
	;mov	dx, [sectors]
	shl	ah, 6
	or	ah, dl	
	mov	[di+ptEndSector], ah

	; 02/03/2019
	; Check unused space if it is 3rd LDD
	; (write message to add unused space.)

	; 02/11/2020
	;cmp	byte [ldrives], 3
	;jb	eetc_7

	; 01/11/2020
	cmp	word [lcylinders], 65535  
			; sign for using all of available sectors
	jnb	eetc_7	; no need to add unused sector 
			; (there are not any unused sectors)

	; cx = cylinder number (0 to 65535)

	; 02/11/2020
	cmp	byte [ldrives], 3 ; is this logical drive 4 ?
	jnb	short eetc_17  
			; force to show unused space message

	; 02/11/2020
	; (add unused space if cyl nums are same or diff is 1)
	dec	cx  ; tolerate cylinder numbers n and n-1 
	cmp	cx, [endcyl]
	ja	eetc_7  ; the end cyl number of the last ldd
			; is 2 or more cyls less than
			; the end cyl number of the ep
			; (a next ldd can be added to
			; extd partition with 2 or more cyls)
eetc_17:
	; 02/11/2020
	mov	al, [ldrives]
	;add	al, '0'
	add	al, '1'	; 03/11/2020
	mov	[char_lddn], al

	mov	al, [sectors]
	mov	bh, al
	mov	bl, [heads]
	dec	bl
	mul	bl ; [sectors] * [heads] - 1

	push	ax

	mov	al, bh  ; [sectors]
	;mul	byte [heads]
	inc	bl
	mul	bl
		; ax = heads * sectors
	;mul	cx ; * end cylinder
	mul	word [endcyl] ; 02/11/2020
		; dx:ax = end cylinder * heads * sectors
	pop	cx
	add	ax, cx ; + (sectors * (heads - 1))
	adc	dx, 0

	; 03/03/2019
	mov	cx, [sectors]
	dec	cl  ; [sectors] - 1
	add	ax, cx ; + (sectors - 1)
	adc	dx, 0
		; DX:AX = end LBA
	
	mov	cx, [ep_EndSector]
	mov	bx, [ep_EndSector+2]

  	cmp	dx, bx
	jne	short eetc_5
	cmp	ax, cx
	je	short eetc_7 ; 05/03/2019
eetc_5:
	push	bx
	mov	si, msg_c_ldd_unused_warning
	call	print_msg
	pop	bx
eetc_6:
	sub	ah, ah
	int	16h
	cmp	al, 13 ; ENTER
	je	short eetc_7 ; continue
	cmp	al, 27 ; ESC
	jne	short eetc_6

	; 03/03/2019
	; change end cylinder value
	mov	al, [epnumber]
	dec	al
	mov	ah, 18  ; primary partition table structure size
	mul	ah
	mov	si, ax
	mov	ax, [si+part_table_end_cyl]
	; 02/11/2020
	mov	[endcyl], ax ; end cylinder of the ep
eetc_19:
	; 03/11/2020
	cmp	ax, 1023 ; overs CHS limit ?
	jna	short eetc_20 ; no
	mov	ax, 1023 ; 03FFh ; fix end CHS values of the PTE
eetc_20:
	mov	[di+ptEndCylinder], al
	mov	al, [si+part_table_end_sector]
	shl	ah, 6
	or	ah, al	
	mov	[di+ptEndSector], ah
	mov	al, [si+part_table_end_head]
	mov	[di+ptEndHead], al

	mov	ax, cx
	mov	dx, bx
	add	ax, 1
	adc	dx, 0
		; dx:ax = end of extd partition + 1 

	; 02/11/2020
	sub	ax, [bp+ldd_start]
	sbb	dx, [bp+ldd_start+2]
	sub	ax, [di+ptStartSector]
	sbb	dx, [di+ptStartSector+2]
		; dx:ax = new sector count of the ldd

	mov	[di+ptSectors], ax
	mov	[di+ptSectors+2], dx
	jmp	short eetc_8
eetc_7:
	mov	ax, [di+ptSectors]
	mov	dx, [di+ptSectors+2]
eetc_8:
	; 01/11/2020
	cmp	word [endcyl], 1023
	jna	short eetc_15

	; 25/10/2020
	;mov	cx, [bp+ldd_start]
	;mov	bx, [bp+ldd_start+2]
	;; 01/11/2020
	;add	cx, [bp+ldd_size]
	;adc	bx, [bp+ldd_size+2]
	;sub	cx, 1 ; *
	;sbb	bx, 0 ; *
	;cmp	bx, [chs_limit+2]
	;jb	short eetc_15
	;ja	short eetc_19
	;cmp	cx, [chs_limit]
	;jna	short eetc_15
;eetc_19:
	call	size_to_fat_type_lba ; 25/10/2020
	jmp	short eetc_16
eetc_15:
	; Get proper FAT type in [ldd_type]
	; by using DX:AX - size -
	call	size_to_fat_type
eetc_16:
	mov	[di+ptFileSystemID], al

	; Write current EBR (with modified PT)
	mov	ax, [bp+ldd_start]
	mov	dx, [bp+ldd_start+2]

	; 01/11/2020
	; ! safety ! (to protect MBR and primary partition)
;eetc_16:
	;cmp	dx, [ep_StartSector+2]
	;jb	short eetc_18
	;ja	short eetc_17
	;cmp	ax, [ep_StartSector]
	;jna	short eetc_18
;eetc_17:
	;mov	ah, 0FFh
	;jmp	print_error_code
;eetc_18:

	mov	bx, ebr_buffer
	call	write_hd_sector
	jc	print_error_code ; ! display error msg and then exit !

	mov	al, [ldrives]
	mov	ah, 18 ; extended partition table structure size
	mul	ah

	mov	si, ax
	add	si, ext_table_boot_ind
	xchg	si, di

	; set partition (logical -dos- drive) data
	movsw	; boot indicator, beginning head
	lodsb
	mov	ah, al
	and	al, 3Fh
	stosb	; beginning sector
	shr	ah, 6
	lodsb	; beginning cylinder
	stosw	
	movsw	; partition id, end head
	lodsb
	mov	ah, al
	and	al, 3Fh
	stosb	; end sector
	shr	ah, 6
	lodsb	; end cylinder
	stosw

	movsw	; copy start sector (LBA) and sector count
	movsw
	movsw
	movsw

	inc	byte [ldrives]

	jmp	display_extended_pt

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; check free space in EXTENDED (DOS) partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 05/03/2019

check_ext_free_space:
	mov	ax, [ep_Size]
	mov	dx, [ep_Size+2]

	mov	bl, [ldrives]
	and	bl, bl
	jz	short cefs_1
	cmp	bl, 4
	jna	short cefs_0
	mov	bl, 4
cefs_0:
	dec	bl
	shl	bl, 2 ; * 4
	xor	bh, bh
	mov	si, bx

	mov	cx, [si+ldd_start]
	mov	bx, [si+ldd_start+2]
	add	cx, [si+ldd_size]
	adc	bx, [si+ldd_size+2]

	add	ax, [ep_StartSector]
	adc	dx, [ep_StartSector+2]

	sub	ax, cx
	sbb	dx, bx
	jnz	short cefs_1
	
	or	ax, ax
	jnz	short cefs_1

	; cf = 0 if the result not negative

	stc

	; cf = 1 if the result is negative or zero
	 
	; If cf = 0 -> There is free space as in DX:AX
	; NOTE: This calculation is unsafe if
	; logical dos drive count > 4 !	
	; (But still meaningful for creating a new ldd.
	;  Because a new ldd will be created only
	;  if ldd count < 4.)
cefs_1:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; convert LBA to CHS parameters (in registers)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 24/10/2020

	; 13/01/2021 (BugFix)
	; 01/03/2019 (hdimage.s)
lba_to_chs:
	; INPUT:
	;	DX:AX = LBA address
	; OUTPUT:
	;	AX = cylinder
	;	DL = sector
	;	DH = head
	; 24/10/2020
	;	BL = extended partition ID (05h, 0Fh)
	;
	; (modified registers: ax, bx, cx, dx)

	mov	cx, [sectors]
	call	div32
	inc	bl
	push	bx ; BL = sector
	mov	cx, [heads]
	call	div32
	pop	dx  ; DL = sector	
	mov	dh, bl ; BL = head
	
	; 24/10/2020
	; check cylinder number (CHS) limit
	;mov	bh, 05h ; ENTENTED (LBA)
	; 13/03/2021
	mov	bl, 05h ; EXTENDED (LBA)

	mov	cx, ax ; 01/11/2020

	cmp	ax, 1023
	jna	short lbatochs_ok

	mov	ax, 1023 ; BC/EC = 0FFh
	mov	dl, 63  ; BS/ES = 0FFh
	mov	dh, 254 ; BH/EH = 0FEh
	mov	bl, 0Fh ; EXTENDED (CHS)
lbatochs_ok:
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; specify FAT type by using partition/volume size (CHS)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 02/03/2019

size_to_fat_type:
	; INPUT:
	;	DX:AX = DOS Partition Size in sectors
	; OUTPUT:
	;	AL = Partition type/ID (FAT type)

	or	dx, dx
	jnz	short stft_2

	cmp	ax, 32680
	ja	short stft_1
stft_0:	
	mov	al, 1	; FAT12 file system
	retn
stft_1:
	mov	al, 4	; FAT16 (< 32MB)
	retn	
stft_2:
	cmp	dx, 10h
	jnb	short stft_3 ; FAT32 (CHS) file system

	mov	al, 6	; FAT16 (>= 32MB)
	retn
stft_3:
	mov	al, 0Bh ; FAT32 (CHS)
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; specify FAT type by using partition/volume size (LBA)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 25/10/2020

size_to_fat_type_lba:
	; INPUT:
	;	DX:AX = DOS Partition Size in sectors
	; OUTPUT:
	;	AL = Partition type/ID (FAT type)

	or	dx, dx
	jz	short stft_4

	cmp	dx, 10h
	jb	short stft_5 ; FAT16 (LBA) file system

	mov	al, 0Ch ; FAT32 file system (LBA)
	retn
stft_4:
	cmp	ax, 32680
	jna	short stft_0  ; FAT12 file system
stft_5:	
	mov	al, 0Eh	; FAT16 file system (LBA)
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get requested logical dos drive size (from user)
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 02/03/2019

get_ldd_size:
	; INPUT -> none
	;
	; OUTPUT -> 
	;	[lcylinders] = cylinder count
	;
	; (Modified registers: ax, bx, cx, dx, si, di) 

	mov	ax, 3 ; clear screen
	int	10h

	mov	si, msg_create_dos_partition_h
	call	print_msg

	; 03/03/2019
	mov	al, [ldrives]
	or	al, al		; cmp byte [ldrives], 0
	jz	short gldds_1	; jna short gldds_0

	;push	ax ; *	

	;dec	al
	;mov	ah, 18
	;mul	ah
	
	;mov	di, ext_table_rel_sec_lw
	;add	di, ax

	mov	si, msg_use_all_space
	; 01/11/2020
	;call	print_msg
	jmp	short gldds_2

	; Calculate available space

	;mov	ax, [ep_Size] ; ext part size in sectors
	;mov	dx, [ep_Size+2]
	
	; 04/03/2019
;gldds_0:
	;mov	cx, [di]    ; start sector
	;mov	bx, [di+2]
	;add	cx, [di+4]  ; sectors
	;adc	bx, [di+6]
	;	; bx:cx = start of next ldd
	;sub	ax, cx
	;sbb	dx, bx
	;	; dx:ax = free space in sectors
	;
	;pop	cx ; *
	;dec	cl
	;jz	short gldds_2
	;sub	di, 18
	;push	cx ; *
	;jmp	short gldds_0

	; 05/03/2019
	;call	check_ext_free_space
	;jc	short gldds_cancel
	
	; 01/11/2020
	; 30/10/2020
	;mov	ax, [ep_free_sectors]
	;mov	dx, [ep_free_sectors+2]
	
		; DX:AX = free sectors in extended partition
	;jmp	short gldds_2
gldds_1:
	mov	si, msg_use_entire_ep_space
	;call	print_msg
	
	; 01/11/2020
	; Calculate free space in extended partition
	;mov	ax, [ep_Size]
	;mov	dx, [ep_Size+2]
gldds_2:
	; 01/11/2020
	call	print_msg

	mov	al, [heads]
	mul	byte [sectors]
	mov	cx, ax
		
	mov	ax, [ep_free_sectors]
	mov	dx, [ep_free_sectors+2]

	; 01/11/2020
	; this is needed for partition size input
	; (maximum available sectors)
	;mov	[pp_Sectors], ax
	;mov	[pp_Sectors+2], dx

	; 01/11/2020
	; subtrack start offset (which always equals to spt value)
	;sub	ax, [sectors]
	;sbb	dx, 0

	;mov	cx, ax
	;mov	al, [heads]
	;mul	byte [sectors]
	;xchg	ax, cx
		; dx:ax = sectors
		; cx = heads*spt 
	;call	div32
		; ax = cylinders
		; bx = remainder
 		; dx = 0
	;and	bx, bx
	;jz	short gldds_3
	;inc	ax
	; 01/11/2020
	div	cx
	;and	dx, dx
	;jz	short gldds_3
	;inc	ax
gldds_3:
	mov	[lcylinders], ax ; <= 65535 (< 65535)
gldds_getchar:
	xor	ah, ah
	int	16h

	cmp	al, 27 ; ESCAPE key
	je	short gldds_cancel
	and	al, 0DFh
	cmp	al, 'N'
	je	short gldds_4
	cmp	al, 'Y'
	jne	short gldds_getchar

	; 01/11/2020
	mov	word [lcylinders], 65535 ; sign for all free sectors
	
	mov	si, _msg_YES
	;call	print_msg
	;retn
	jmp	print_msg

gldds_cancel:
	mov	word [lcylinders], 0
gldds_28:
	retn
gldds_4:	
	mov	si, _msg_NO
	call	print_msg
gldds_26:
	mov	ax, 3 ; clear screen
	int	10h

	mov	si, msg_create_dos_partition_h
	call	print_msg

	mov	si, msg_create_ldd_s
	call	print_msg
	mov	si, msg_press_esc_to_cancel
	call	print_msg
gldds_25:
	xor	ah, ah
	int	16h

	cmp	al, 27 ; ESCAPE key
	jne	short gldds_5

	jmp	short gldds_cancel
gldds_5:
	; 04/03/2019
	cmp	al, 32 ; SPACE key
	je	short gldds_28
	
	; 01/11/2020
	;mov	byte [pSize_unit], '%'
	cmp	al, '%'
	je	short gldds_6
	;mov	byte [pSize_unit], 'M'
	cmp	al, 13 ; 0Dh, Carriage Return key
	;je	short gldds_6
	jne	short gldds_29
	; 01/11/2020
	mov	al, 'M'
	jmp	short gldds_6
gldds_29:
	and	al, 0DFh
	cmp	al, 'M'
	je	short gldds_6
	;mov	[pSize_unit], al
	cmp	al, 'G'
	je	short gldds_6
	cmp	al, 'C'
	jne	short gldds_25
gldds_6:
	; 01/11/2020
	mov	[pSize_unit], al

	; Set maximum sector count (all of extended partition)
	;mov	al, [sectors]
	;mul	byte [heads]
	;mul	word [lcylinders]
	; 01/11/2020	
	mov	ax, [ep_free_sectors]
	mov	dx, [ep_free_sectors+2]
	mov	[pp_Sectors], ax
	mov	[pp_Sectors+2], dx
	
 	call	partition_size_input
	jc	short gldds_cancel
		; DX:AX = Partition size in sectors
	;or	bx, bx
	;jnz	short gldds_cancel

	; 01/11/2020
	mov	[ppn_Sectors], ax
	mov	[ppn_Sectors+2], dx
		
	;mov	cx, ax
	;mov	al, [heads]
	;mul	byte [sectors]
	;xchg	ax, cx
	; 13/03/2021
	mov	cx, [hs] ; heads*sectors
		; dx:ax = sectors
		; cx = heads*spt 
	call	div32
		; ax = cylinders
		; bx = remainder
 		; dx = 0
	; 02/11/2020
	;and	bx, bx
	;jz	short gldds_7
	;inc	ax
gldds_7:
	cmp	ax, [lcylinders]
	ja	short gldds_9
gldds_8:
	; OK
	mov	[lcylinders], ax
	retn

gldds_9:
	; Partition size limit message
	
	cmp	byte [pSize_unit], 'C'
	je	short gldds_10

	; Tolerate 1 cylinder if unit is not cylinder
	;dec	ax
	;cmp	ax, [lcylinders]
	;jna	short gldds_8

	; 01/11/2020
	; Tolerate sectors if sectorcount doesn't over ep limit
	mov	ax, [ppn_Sectors]
	mov	dx, [ppn_Sectors+2]
	cmp	dx, [ep_free_sectors+2]
	ja	short gldds_10
	jb	short gldds_30
	cmp	ax, [ep_free_sectors]
	ja	short gldds_10
gldds_30:
	mov	word [lcylinders], 65535 ; sign for all free sectors
	retn

gldds_10:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	mov	si, msg_create_dos_partition_h ; header
	call	print_msg

	mov	si, msg_partition_size_limit
	call	print_msg

	cmp	byte [pSize_unit], 'M'
	je	short gldds_11

	cmp	byte [pSize_unit], '%'
	je	short gldds_22

	cmp	byte [pSize_unit], 'C'
	jne	short gldds_11

	mov	ax, [lcylinders]
	jmp	short gldds_12
gldds_11:
	; 'M'
	mov	ax, [pp_Sectors]
	mov	dx, [pp_Sectors+2]
	mov	cx, 2*1024 ; sectors -> MB
	call	div32
		; DX = 0 
		; AX = Available space in Megabytes
	mov	di, ax
gldds_12:	
	mov	cx, 10
	mov	bp, sp
gldds_13:
	xor	dx, dx
	;mov	cx, 10
	div	cx
	push	dx
	cmp	ax, 9
	ja	short gldds_13
gldds_14:
	mov	bx, 07h
	or	ax, ax
	jnz	short gldds_16
gldds_15:
	pop	ax
gldds_16:
	add	al, '0'
gldds_17:
	mov	ah, 0Eh
	;mov	bx, 07h
	int	10h	; write character (as tty)

	cmp	sp, bp
	jb	short gldds_15

	cmp	byte [pSize_unit], '%'
	jne	short gldds_18

	cmp	al, '%'
	je	short gldds_21
	mov	al, '%'
	jmp	short gldds_17
gldds_18:
	cmp	byte [pSize_unit], 'C'
	jne	short gldds_19

	mov	si, msg_cylinders
	jmp	short gldds_20

gldds_22:
	; '%'
	; 01/11/2020
	mov	dx, [ep_Size+2]
	mov	ax, [ep_Size] ; *
	and	dx, dx
	jz	short gldds_33 ; dx = 0
	mov	cx, 1
gldds_31:
	shl	cx, 1
	call	div32
	or	dx, dx
	jz	short gldds_32 ; *
	mov	ax, [ep_Size]	
	mov	dx, [ep_Size+2]
	jmp	short gldds_31
gldds_32:
	mov	dx, [ep_free_sectors+2]
	; ep free sectors <= ep size
gldds_33:
	push	ax ; * ; dx = 0 (ep size weight)
	mov	ax, [ep_free_sectors]
	mov	cx, 100
	;and	dx, dx
	;jnz	short gldds_34
	;mul	cx
	;jmp	short gldds_35
;gldds_34:
	call	mul32
		; dx:ax = 100 * ep free sectors weight
;gldds_35:
	pop	cx ; *
	call	div32 ; 100 * ep free sectors / ep size
		; ax = % value (<= 100)
	or	ax, ax
	;jnz	short gldds_23
	jnz	short gldds_12 ; 13/01/2021
	inc	ax ; 0 -> 1
	; 13/03/2021
	jmp	short gldds_12

;gldds_23:	
	;mov	bp, sp
	;mov	cx, 10
;gldds_24:	
	;xor	dx, dx
	;div	cx
	;push	dx ; *
	;cmp	ax, 9
	;ja	short gldds_24
	;jmp	short gldds_14

gldds_19:
	mov	si, msg_megabytes
	mov	byte [msg_megabytes_s], 's'
	cmp	di, 1
	ja	short gldds_20
	mov	byte [msg_megabytes_s], 0
gldds_20:
	call	print_msg
gldds_21:
	mov	si, msg_partition_size_limit_r
	call	print_msg
gldds_27:
	xor	ah, ah
	int	16h
	
	cmp	al, 27 ; ESCAPE key
	je	gldds_26
	cmp	al, 13 ; ENTER (Carriage Return) key
	jne	short gldds_27

	;mov	ax, [lcylinders]
	retn
	
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; initialize extended (dos) partition table 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 18/10/2020 (fdisk3.s)
; 26/02/2019 (hdimage.s)

init_ext_partition_table:

	; INPUT -> 
	;	[epnumber] = extended partition number
	;
	; OUTPUT -> none

; Display Method: ; 04/03/2019
;LDD 1 <- LDD_START0, ebr 1st entry <- MBR (extended partition) - EBR 0
;LDD 2 <- LDD_START1, ebr 1st entry <- EBR 1
;LDD 3 <- LDD_START2, ebr 1st entry <- EBR 2
;LDD 4 <- LDD_START3, ebr 1st entry <- EBR 3
;LDD 5 <- LDD_START3, ebr 2nd entry (LDD 5 is not displayed)

	; 08/03/2019

	; clear extended partition table structure/list

	mov	di, ext_table_boot_ind
	mov	si, di ; 28/02/2019
	mov	cx, 36 ; 18*4 = 72 bytes
	xor	ax, ax ; 0
	rep	stosw
	mov	di, si ; 28/02/2019

	;mov	byte [ldrives], 0
	mov	[ldrives], al ; 0

	;cmp	byte [epnumber], 1
	;jb	short iept_0

	mov	al, [epnumber]
	dec	al
	mov	ah, 18  ; partition table structure size 
	mul	ah
	mov	si, part_table_rel_sec_lw
	add	si, ax
	; 02/11/2020
	;(save end cylinder for comparising later)
	mov	ax, [si-2] ; part_table_end_cyl
			; 16 bit cylinder number
	mov	[endcyl], ax
	;
	mov	ax, [si]
	mov	dx, [si+2]
	mov	[ep_StartSector], ax
	mov	[ep_StartSector+2], dx
	mov	cx, [si+4]
	mov	bx, [si+6]
	mov	[ep_Size], cx
	mov	[ep_Size+2], bx
	sub	cx, 1
	sbb	bx, 0
	add	cx, ax
	adc	bx, dx 
	mov	[ep_EndSector], cx
	mov	[ep_EndSector+2], bx

	; 27/02/2019
	mov	[ldd_start], ax
	mov	[ldd_start+2], dx

	; 03/03/2019
	;mov	word [ldd_size], 0
	;mov	word [ldd_size+2], 0
	
	mov	bx, ebr_buffer
	; dx:ax = Extended partition address
	; es:bx = Extended partition buffer
	call	read_hd_sector
	jc	short iept_0

	; Check EBR if it is a valid boot record or not
	cmp	word [ebr_buffer+510], 0AA55h
	je	short iept_1
iept_stc_retn:
	stc
iept_0:
	retn
iept_1:
	; Check PTE 1, if it is valid or not
	mov	al, [ebr_buffer+446+ptFileSystemID]
	and	al, al
	jz	short iept_stc_retn ; empty pte, error
	cmp	al, 5
	je	short iept_stc_retn ; extended partition, error
	; 24/10/2020
	cmp	al, 0Fh
	je	short iept_stc_retn ; extended partition, error

	;inc	byte [ldrives] ; logical (dos) drive count

	mov	si, ebr_buffer+446 ; Partition table offset
	mov	cx, 4

	; set partition (logical -dos- drive) data
	movsw	; boot indicator, beginning head
	lodsb
	mov	ah, al
	and	al, 3Fh
	stosb	; beginning sector
	shr	ah, 6
	lodsb	; beginning cylinder
	stosw	
	movsw	; partition id, end head
	lodsb
	mov	ah, al
	and	al, 3Fh
	stosb	; end sector
	shr	ah, 6
	lodsb	; end cylinder
	stosw

	; 04/03/2019
	mov	bl, [ldrives]
	;dec 	bl
	;jnz	short iept_2

	; 05/03/2019
	or	bl, bl
	jnz	short iept_2

	mov	ax, [si]   ; start sector of 1st LDD (offset)
	mov	dx, [si+2]
	add	ax, [si+4] ; size of 1st LDD (volume)
	adc	dx, [si+6]

	mov	[ldd_size], ax ; size of 1st LDD (partition)
	mov	[ldd_size+2], dx
	; 05/03/2019
	inc	byte [ldrives] ; logical (dos) drive count
	inc	bl
iept_2:
	rep	movsw ; copy start sector (LBA) and sector count

	; get next extended partition (logical -dos- drive) data
	mov	al, [si+ptFileSystemID]
	and	al, al ; EMPTY
	jz	short iept_0 ; end of logical -dos- drive chain

	cmp	al, 5 ; EXTENDED
	;jne	short iept_stc_retn  ; 2nd PTE must have
	;			     ; extended partition ID
	je	short iept_4
	; 24/10/2020
	cmp	al, 0Fh ; EXTENDED (LBA)
	jne	short iept_stc_retn  ; 2nd PTE must have
				     ; extended partition ID
iept_4:
	; 05/03/2019
	inc	byte [ldrives] ; logical (dos) drive count

	;cmp	byte [ldrives], al ; 5
	cmp	bl, 4 ; number of logical dos drives (till here)
	jnb	short iept_3

	add	si, 8

	lodsw	
	mov	dx, ax	; start sector
	lodsw
	xchg	dx, ax	; start sector + 2

	; 04/03/2019
	add	ax, [ep_StartSector]
	adc	dx, [ep_StartSector+2]
	
	xor	bh, bh
	shl	bl, 2 ; *4

	; 28/02/2019
	mov	[bx+ldd_start], ax ; start of (next) LDD (partition)
	mov	[bx+ldd_start+2], dx

	; 03/03/2019
	; set partition size for logical dos drive
	mov	cx, [si]
	mov	[bx+ldd_size], cx
	mov	cx, [si+2]
	mov	[bx+ldd_size+2], cx

	mov	bx, ebr_buffer
	; dx:ax = Extended partition address
	; es:bx = Extended partition buffer
	call	read_hd_sector
	jc	short iept_3 ; 03/03/2019

	; Check EBR if it is valid or not
	cmp	word [ebr_buffer+510], 0AA55h
	je	iept_1

	stc
iept_3:	
	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; delete logical -dos- drives in extended partition
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 18/10/2020 (fdisk3.s)

delete_logical_drives:
		; 27/02/2019 (hdimage.s)

; Delete Method: ; 04/03/2019
;LDD 5 <- LDD_START3, ebr 2nd entry
;LDD 4 <- LDD_START2, ebr 2nd entry
;LDD 3 <- LDD_START1, ebr 2nd entry
;LDD 2 <- LDD_START0, ebr 2nd entry
;LDD 1 <- LDD_START0, ebr 1st entry

	;cmp	byte [ldrives], 0
	;jna	short dld_stc

	;mov	al, 5 ; extended partition (logical drives)
	;call	display_partition_table
	; 12/03/2021
	call	display_partition_table_x

	mov	al, [ldrives]
	add	al, '0'
	mov	[lddp_num], al

	mov	si, msg_delete_ldd
	call	print_msg

	mov	si, msg_press_esc_to_cancel
	call	print_msg
dld_0:
	xor	ah, ah
	int	16h

	cmp	ah, 53h  ; DELETE or DEL key
	jne	short dld_1

	; Delete PTE 1 & PTE 2 on EBR 1
	xor	ah, ah
	mov	al, [ldrives]
	dec	al
	jnz	short dld_2

	mov	di, ebr_buffer+446
	mov	cx, 16
	rep	stosw

	; 04/03/2019
	; Clear ldd data in extended partition table/structure
	mov	di, ext_table_boot_ind
	mov	cl, 9
	rep	stosw
	xor	si, si
	jmp	short dld_3
dld_1:	
	cmp	al, 27 ; ESC key
	jne	short dld_0
dld_5:
	retn

;dld_stc:
;	stc
;	retn	

dld_2:
	; 04/03/2019
	; Delete 2nd PTE on EBR 2 to 4
	dec	al 
	shl	al, 2 ; * 4
	mov	si, ax

	mov	ax, [si+ldd_start]
	mov	dx, [si+ldd_start+2]
	mov	bx, ebr_buffer
	call	read_hd_sector
	jc	short dld_4

	mov	di, ebr_buffer+446+16
	mov	cx, 8
	sub	ax, ax
	rep	stosw

	; 04/03/2019
	;cmp	byte [ldrives], 5
	;jnb	short dld_3
	mov	ah, [ldrives]
	cmp	ah, 5
	jnb	short dld_3

	; Clear ldd data in extended partition table/structure
	;mov	ah, [ldrives]
	dec	ah
	mov	al, 18
	mul	ah
	mov	di, ext_table_boot_ind
	add	di, ax
	;mov	cx, 9
	mov	cl, 9
	;xor	ax, ax
	xor	al, al
	rep	stosw
dld_3:
	; Write changed EBR
	mov	ax, [si+ldd_start]
	mov	dx, [si+ldd_start+2]
	mov	bx, ebr_buffer
	call	write_hd_sector
	jc	short dld_4

	; 28/02/2019
	dec	byte [ldrives]
dld_4:
	cmp	byte [ldrives], 0
	ja	delete_logical_drives

	retn

;=============================================================================
;        	initialized data
;=============================================================================

; 12/10/2020
int13h_x:	db	0

TRDOS386_MASTERBOOT_SECTOR:
	incbin	'FS1_MBR.BIN' ; Singlix FS1 MBR

TRDOS_FAT_hd_bs:
	;incbin 'TRHDBS.BIN'
	; 04/05/2024
TRDOS_FAT32_hd_bs:
	;incbin	'FAT32_BS.BIN' ; 27/04/2024
	incbin	'RD5HDBS3.BIN' ; 29/04/2024
TRDOS_FAT16_hd_bs: 
	;incbin	'FAT16_BS.BIN' ; 26/12/2017
	incbin	'RD5HDBS2.BIN' ; 20/04/2024
TRDOS_FAT12_hd_bs: 
	;incbin	'FAT12_BS.BIN' ; 26/12/2017
	incbin	'RD5HDBS1.BIN' ; 20/04/2024

TRDOS_TRFS1_chs_bs:
	incbin	'TRFS1CHS.BIN' ; Singlix FS1 (CHS+LBA Disk) BS
TRDOS_TRFS1_lba_bs:
	incbin	'TRFS1LBA.BIN' ; Singlix FS1 (LBA Disk) BS

	db	0

hexchrs:
	db	'0123456789ABCDEF'

; 05/11/2020
	db	0

align 2

; (TR-DOS 386 compatible) Hard Disk (image) parameters

sectors: ; sectors per track (63)
	dw 63
heads:	 ; number of heads (16 or 32 or 64) 
	dw 16
cylinders: ; number of cylinders (16 to 1024)
	dw 1024
	dw 0 ; 16/10/2020 (double word)

; 05/11/2020

;random:
;	db 0  ; random write to file (0 = sequental)
;newdisk:
;	db 0

FileSys_Names: ; 2003-2017
; (Valid FileSystems for TRDOS 386, SINGLIX, RETRO UNIX OS projects in 2017)
FS_FAT12:     db "FAT12         "  ; 01h = FAT12
FS_XENIX:     db "XENIX         "  ; 02h , XENIX System V root
FS_XENIX_USR: db "XENIX usr     "  ; 03h , XENIX System V user
FS_FAT16:     db "FAT16 (04h)   "  ; 04h = FAT16 < 32MB
FS_EXT_CHS:   db "EXTENDED (CHS)"  ; 05h = Extended DOS Partition
FS_FAT16_BIG: db "FAT16 (06h)   "  ; 06h = FAT16 > 32MB, CHS mode
FS_NTFS:      db "NTFS          "  ; 07h , WINDOWS NTFS Partition
FS_FAT32_CHS: db "FAT32 (CHS)   "  ; 0Bh = FAT32, CHS mode
FS_FAT32_LBA: db "FAT32 (LBA)   "  ; 0Ch = FAT32, LBA mode
FS_FAT16_LBA: db "FAT16 (LBA)   "  ; 0Eh = FAT16, LBA mode
FS_EXT_LBA:   db "EXTENDED (LBA)"  ; 0Fh = Extented Partition, LBA mode
FS_UNIX_SYSV: db "UNIX SYSTEM V "  ; 63h , SCO UNIX, UNIXWARE, OPENSERVER
FS_RETROUNIX: db "RETRO UNIX    "  ; 71h , Retro UNIX 386 v2 Partition
FS_UNIX_V7:   db "UNIX V7       "  ; 72h , UNIX v7 x86 Partition  
FS_LINUXSWAP: db "LINUX SWAP    "  ; 82h , LINUX SWAP Partition
FS_LINUX:     db "LINUX         "  ; 83h , LINUX NATIVE (ext2) Partition
FS_LINUXEXT:  db "LINUX EXTENDED"  ; 85h , LINUX EXTENDED Partition
FS_TRDD:      db "RDD           "  ; A0h , (Random Data Disk) LBA
FS_TRFS:      db "SINGLIX FS1   "  ; A1h , (32 bit, 512 bytes per sector)
FS_OTHERS:    db "UNKNOWN FS    "  ; Another or Unknown File Systems
 
valid_partitions: ; (*)
	      db 01h, 02h, 03h, 04h, 05h, 06h, 07h
	      db 0Bh, 0Ch, 0Eh, 0Fh, 63h, 71h, 72h
	      db 82h, 83h, 85h, 0A0h, 0A1h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

CHS_msg: ; 80 bytes per row
db  "Press 'C' to set cylinders then press '+,-' keys to change number of cylinders. "
db  "Press 'H' to set heads then press '+,-' keys to change number of heads.         "
db  "Press 'S' to set sectors then press '+,-' keys to change numb of sectors/track. "
db  "                                                                                "
db  "                                                                                "
db  "Press ENTER to use current CHS values.                                          "
db  "                                                                                "
db  "Press ESC to cancel.                                                            "
db  "                                                                                "
db  0

	; 04/05/2024
TrDOS_Welcome:
	db	0Dh, 0Ah
	;db	'TR-DOS 386 Fixed Disk Partitioning Utility'
	db	'Retro DOS v5 Fixed Disk Partitioning Utility'
	db	0Dh, 0Ah
	;db	"v1.1.240504 (c) Erdogan TAN 2021-2024"
	db	"FDISK3 v2.0.240715 (c) Erdogan TAN 2021-2024"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	0
TrDOS_Usage:
	db	'Usage: fdisk3r5 <hard disk name> '
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Hard disk names: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	" hd0 ..for 1st hard disk "
	db	0Dh, 0Ah
	db	" hd1 ..for 2nd hard disk "
	db	0Dh, 0Ah
	db	" hd2 ..for 3rd hard disk "
	db	0Dh, 0Ah
	db	" hd3 ..for 4th hard disk "
	db	0Dh, 0Ah, 0

TrDOS_Options:
	db	"Select hard disk number (1 to "
TrDOS_dnmax:
	db	"4) or press ESC to exit ..."
	db	0Dh, 0Ah, 0
TrDOS_hdrow:
	db	0Dh, 0Ah 
	db	"("
TrDOS_hdrow_n:
	db	"0) hd"
TrDOS_hdrow_i:
	db	"0 - Capacity : "
	db	0
TrDOS_hdrow_unit:
	db	"GB "
	db 	0Dh, 0Ah, 0

TrDOS_hdrow_capacity:
	db	"0000 ", 0
	db	0

TrDOS_disksize:
	db	0Dh, 0Ah
	db	"Disk size : "
	db	0

msg_fdisk_capacity_err:
	db	0Dh, 0Ah
	db	"Huge disk !"
	db	0Dh, 0Ah
	db	"This FDISK version (v3) can work with disk sizes up to "
	db	0

msg_any_key_esc_exit:
	db	0Dh, 0Ah
	db	"Press ESC to exit or press another key to continue..."
	db	0

msg_create_partition_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                               Create Partition                                 "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0
msg_create_partition_m:
	db	"Select an option: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"  1) Create DOS partition ", 0Dh, 0Ah
	db	"  2) Create SINGLIX FS partition ", 0Dh, 0Ah
	db	"  3) Create RETRO UNIX partition ", 0Dh, 0Ah
	db	"  4) Create another type of partition ", 0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Press ESC or 0 to cancel .. "
 	db 	0Dh, 0Ah, 0

msg_create_primary_partition_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                          Create Primary DOS Partition                          "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0

msg_create_dos_partition_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                   Create DOS Partition or Logical DOS Drive                    "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0

msg_create_ext_partition_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                         Create Extended DOS Partition                          "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0

msg_create_logical_drive_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                            Create Logical DOS Drive                            "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0

msg_create_nondos_partition_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                            Create NON-DOS Partition                            "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0

msg_create_dos_partition_m:
	db	"Select an option: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"  1) Create Primary DOS partition ", 0Dh, 0Ah
	db	"  2) Create Extended DOS partition ", 0Dh, 0Ah
	db	"  3) Create Logical DOS drive(s) in Extended DOS partition ", 0Dh, 0Ah
	db	0Dh, 0Ah	
	db	"Press ESC or 0 to cancel .. "
 	db 	0Dh, 0Ah, 0

msg_create_trdos_partition_s:
	db	"Select an option to set partition size: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"  C) Cylinder count", 0Dh, 0Ah   
	db	"  %) Volume percentage (##%)", 0Dh, 0Ah
	db	"  S) Sectors (number of 512 bytes)", 0Dh, 0Ah
	db	"  K) Kilo bytes (KB, 2*K sectors)", 0Dh, 0Ah
	db	"  M) Mega bytes (MB, 2*1024*M sectors)", 0Dh, 0Ah
	db	"  G) Giga bytes (GB, 2*1024*1024*G sectors)", 0Dh, 0Ah
	db	0Dh, 0Ah	
	db	"Press SPACE to use whole disk or press ENTER to set sector count .. "
msg_press_esc_to_cancel:
 	db	0Dh, 0Ah
	db	"(Press ESC to CANCEL) "
	db 	0Dh, 0Ah, 0

msg_use_whole_disk:
	db	"Do you want to use WHOLE disk !? (Y/N)", 0

msg_use_all_space:
	db	"Do you want to use all of available space !? (Y/N)", 0

msg_use_entire_ep_space:
	db	"Do you want to use entire extended partition space !? (Y/N)", 0

msg_partition_size:
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Partition size ("
msg_partition_size_x:
	db	"?) : "
	db	0

msg_partition_type:
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Partition type : "
	db	0

msg_ptype_num:
	db	"00h", 0Dh, 0Ah, 0

msg_partition_type_error:
	db	"Partition size is not proper for selected partition type !"
	db	0Dh, 0Ah
msg_any_key_to_retry:
	db	"(Press any key to retry..)"
	db	0Dh, 0Ah, 0

msg_partition_size_overs:
	db	0Dh, 0Ah
	db	"Partition size overs the disk capacity !"
	db	0Dh, 0Ah
	db	"(Press ENTER to use WHOLE disk or press ESC key to retry..)" 
	db	0Dh, 0Ah, 0

msg_ext_partition_error:
	db	"Extended partition must be created after primary partition !"
	db	0Dh, 0Ah,0

msg_logical_drive_error:
	db	"Primary and extended partitions must be created before logical drive !"
	db	0Dh, 0Ah,0

msg_cylinder_boundary_set:
	db	0Dh, 0Ah
	db	"Do you want to adjust partition size to cylinder boundary ? (Y/N)"
	db	0

	; 2019 - 2020 (hdimage.s)
;msg_selected_partition:
;	db	"                                 PARTITION 0                                    "
;	db	"================================================================================"
;	db	"        S  BH  BS  BC  FS  EH  ES  EC    START SEC   SECTORS   FILE SYSTEM      "
;	db	"--------------------------------------------------------------------------------"
;pt_row: db	"                                                                                "
;	db	"================================================================================"
;	db	0

	; 24/10/2020 (fdisk3.s)
msg_selected_partition:
	db	"                                 PARTITION 0                                    "
	db	"================================================================================"
	db	"       S  BH  BS  BC  FS  EH  ES  EC   START SECT    SECTORS   FILE SYSTEM      "
	db	"--------------------------------------------------------------------------------"
pt_row:	db	"                                                                                "
	db	"================================================================================"
	db	0
msg_boot_indicator:
	db	0Dh, 0Ah
	db	"    Boot Indicator : ", 0 ; "YES", "NO"
msg_starting_head:
	db	0Dh, 0Ah
	db 	"     Starting Head : ", 0
msg_starting_sector:
	db	0Dh, 0Ah
	db	"   Starting Sector : ", 0
msg_starting_cylinder:
	db	0Dh, 0Ah
	db	" Starting Cylinder : ", 0
msg_system_id:
	db	0Dh, 0Ah
	db	"         System ID : ", 0
msg_ending_head:
	db	0Dh, 0Ah
	db 	"       Ending Head : ", 0
msg_ending_sector:
	db	0Dh, 0Ah
	db	"     Ending Sector : ", 0
msg_ending_cylinder:
	db	0Dh, 0Ah
	db	"   Ending Cylinder : ", 0
msg_relative_sectors:
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"  Relative Sectors : ", 0
msg_total_sectors:
	db	0Dh, 0Ah
	db	"     Total Sectors : ", 0

msg_format_stage:
	db	0Dh, 0Ah
	db	"Press ENTER to FORMAT disk partition "
partition_num_chr:
	db	"0"
	db	" or press ESC to EXIT.."
	db	0
msg_partition_edit:
	db	0Dh, 0Ah
	db	".. or press SPACE to EDIT partition table."
	db	0Dh, 0Ah, 0

msg_edit_or_exit:
	db 	0Dh, 0Ah
	db	"Press ENTER to continue or press ESC to exit.."
	db	0

msg_overwrite_question1:
	db	0Dh, 0Ah
	db	'Do you want to overwrite '
	db	27h
	db	0

msg_overwrite_question2: 
	db	27h
	db	' file '
	db	0

msg_format_question:
	db	0Dh, 0Ah
	db	"Do you want to format partition "
partition_num_txt:
	db	 "0 "
msg_yes_no:
	db	'(Yes/No)? ', 0

msg_writing_mbr:
	db	"Writing masterboot sector...", 0

msg_writing_disk_sectors:
	db	"Writing disk sector: ", 0

Msg_Writing_Boot_Sector:
	db	"Writing trdos boot sector...", 0

Msg_Writing_Root_Dir:
	db	"Writing root directory sectors...", 0

Msg_Writing_Data_Sectors:
	db	"Writing data sector: ", 0

Sector_Str:
	db	"0000000", 0
Cursor_Pos:
	dw	0

Msg_Writing_FAT_Sectors:
	db	"Writing FAT sectors...", 0

StrVolumeName:
	;times 	12 db  0
	times	65 db 0  ; 05/01/2018 (fs1 volume name)	

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
	db	"..."
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

msg_disk_sectors:
	db	"Total Disk Sectors : ", 0

str_disk_sectors:
	;times	8 db 0
	times	11 db 0 ; 27/10/2020

msg_ep_size:
	db	"Extended Partition Size in Sectors: ", 0

msg_press_any_key:
	db	0Dh, 0Ah
	db	"Press a key to continue..." 
	db	0Dh, 0Ah, 0

align 2

; Masterboot sector

MasterBootBuff:
MasterBootCode: 
	times	446 db 0
PartitionTable:
	times	64 db 0
MBIDCode:
	dw	0

PTable_Buffer:
	times	64 db 0
 
	db	'(c) Erdogan TAN 2019-2024'

; 05/11/2020
	db	0

	; 2019 - 2020 (hdimage.s)
;p_table_header:
;	db	"                              ?BR PARTITION TABLE                               "
;	db	"================================================================================"
;	db	"       P  S  BH  BS  BC  FS  EH  ES  EC  START SEC  SECTORS  FILE SYSTEM        "
;	db	"--------------------------------------------------------------------------------"
;	db	0
;p_table_footer:
;	db	"================================================================================"
;	db	0Dh, 0Ah, 0

	; 24/10/2020 (fdisk3.s)
p_table_header:
	db	"                              ?BR PARTITION TABLE                               "
	db	"================================================================================"
	db	"   P   S  BH  BS  BC  FS  EH  ES  EC   START SECT    SECTORS    FILE SYSTEM     "
	db	"--------------------------------------------------------------------------------"
	db	0
p_table_footer:
	db	"================================================================================"
	db	0Dh, 0Ah, 0

mbr_editing_options:
	db	0Dh, 0Ah, 0Dh, 0Ah 
	db	"MBR Partition Table Editing Options:"
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"       1. Create Partition", 0Dh, 0Ah
	db	"       2. Set Active Partition", 0Dh, 0Ah
	db	"       3. Delete Partition", 0Dh, 0Ah  
	db	"       4. Write Current Partition Table", 0Dh, 0Ah, 0

enter_option_number_msg:
	db	0Dh, 0Ah
	db	"Enter the option number or press ESC to exit ...", 0Dh, 0Ah
	;db	0Dh, 0Ah, 0
	db	0 

msg_zero_partition_size:  ; 19/02/2019
	db	0Dh, 0Ah
	db	"Partition size input must not be ZERO !", 0Dh, 0Ah
	db	"(Press ESC to exit or press another key to retry..)", 0Dh, 0Ah
	db	0 

msg_empty_pt:
	db	0Dh, 0Ah
	db	"Empty partition table !", 0Dh, 0Ah
	db	"(A valid partition must be created at first..)", 0Dh, 0Ah
	db	0

msg_full_pt:
	db	0Dh, 0Ah
	db	"There is not a free partition table entry ", 0
msg_to_create_new_p: 
	db	"to create a new partition !", 0Dh, 0Ah
	db	0
msg_no_free_space:
	db	0Dh, 0Ah
	db	"There is not (enough) free space ", 0

msg_enter_pn_to_del:
	db	0Dh, 0Ah
	db	"Enter partition number to delete: "
chr_del_pnum1:
	db	0       
	db	0Dh, 0Ah, 0

msg_delete_partition_q:
	db 	0Dh, 0Ah
	db 	"WARNING! All of data in the selected partition will be lost", 0Dh, 0Ah
	db	"         after you write changed partition table to disk !!", 0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Do you want to delete PARTITION "
chr_del_pnum2:
	db	'0'
	db	' ? (Y/N) ', 0

_msg_YES:
	db	 20h
msg_YES:
	db	'YES ', 0
_msg_NO:
	db	20h
msg_NO:
	db	'NO ', 0

; 11/02/2019
msg_write_masterboot_sector:
	db	0Dh, 0Ah 
	db	"Write Current Partition Table:" 
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	" 1. Write Partition Table only", 0Dh, 0Ah
	db	" 2. Write Partition Table and Singlix Master Boot Code", 0Dh, 0Ah
enter_opt_num_cancel_msg:
	db	0Dh, 0Ah
	db	"Enter the option number or press ESC to cancel ...", 0

msg_writing_ptable:
	db	0Dh, 0Ah, 0Dh, 0Ah  
	db	"Writing partition table on disk ... ", 0
_msg_OK:
	;db	7
	db	0Dh, 0Ah
	db	"OK !"
	db	0Dh, 0Ah, 0

option_input:
	db	' [ ]', 0

msg_enter_pn_to_act:
	db	0Dh, 0Ah
	db	"Enter partition number to set as active: ", 0 

; 04/05/2024
;trdos386_disk_chs_header:
;	db	0Dh, 0Ah
;	db	"                     TRDOS 386 (SINGLIX) HARD DISK IMAGE                        "
;	db 	0

;disk_chs_header:
;	db	0Dh, 0Ah
;	db	"                                HARD DISK IMAGE                                 "
;	db 	0

msg_partition_size_limit:
	db	0Dh, 0Ah
	db	"Partition size overs available free space !"
	db	0Dh, 0Ah
	db 	"Max. available free space is ", 0

msg_partition_size_limit_r:
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"(Press ENTER to use all of available space or press ESC key to retry..) " 
	db	0D, 0Ah, 0

msg_cylinders:
	db	" cylinders", 0
msg_sectors:
	db	" sectors", 0

; 02/03/2019
msg_megabytes:
	db	" megabyte"
msg_megabytes_s:
	db	0, 0

msg_inv_pte:
	db	0Dh, 0Ah
	db	"Invalid partition table entry ! (P"
inv_pte_num: db	"?)", 0Dh, 0Ah
	db	"(Press ENTER to DELETE or press ESC to EXIT..)"
	db	0Dh, 0Ah, 0

msg_ext_partition_exists:
	db	"Extended partition already exists !"
	db	0Dh, 0Ah, 0

msg_defective_pt:
	db	0Dh, 0Ah
	db	"Defective partition table !", 0

; 27/02/2019
msg_ext_part_del_error:
	db	0Dh, 0Ah
	db	"Extended partition must be deleted after logical drive(s) !"
	db	0Dh, 0Ah, 0

msg_cancel_continue:
	db	"(Press ESC to cancel or press another key to continue..)"
	db	0Dh, 0Ah, 0

msg_delete_ldd:
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Delete Logical (DOS) Drive:"
	db	0Dh, 0Ah
	db	"Press DELETE key to delete logical disk partition "
lddp_num: db	"?."
	db	0Dh, 0Ah, 0

msg_delete_ext_part:
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Delete Extended (DOS) Partition:"
	db	0Dh, 0Ah
	db	"Press ENTER to delete or press ESC to cancel.."
	db	0Dh, 0Ah, 0

str_display_ebr_pt:
	db	"(Press SPACE to edit EXTENDED Partition Table)", 0Dh, 0Ah, 0

ebr_editing_options:
	db	0Dh, 0Ah, 0Dh, 0Ah 
	db	"Extended Partition Table Editing Options:" 
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"       1. Create Logical DOS Drive", 0Dh, 0Ah
	db	"       2. Delete Logical (DOS) Drive(s)",0Dh, 0Ah, 0

msg_delete_ldd_q:
	db 	0Dh, 0Ah
	db 	"WARNING!", 0Dh, 0Ah 
	db	"All of data in logical (DOS) drive(s) will be lost !!", 0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Do you want to continue ? (Y/N) ", 0

msg_create_ldd_max_error:
	db	0Dh, 0Ah
	db	"Program limit for logical dos drive count is 4 !"
	db 	0Dh, 0Ah, 0

msg_c_ldd_unused_warning:
	db	0Dh, 0Ah
	db	"There is unused extended partition space after logical dos drive "
char_lddn:
	db	"4 !"
	db 	0Dh, 0Ah
	db	"(Press ESC to add unused space or press ENTER to continue.)"
	db 	0Dh, 0Ah, 0

msg_create_ldd_s:
	db	"Select an option to set logical dos drive size: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"  C) Cylinder count", 0Dh, 0Ah   
	db	"  %) Volume percentage (##%)", 0Dh, 0Ah
	db	"  M) Mega bytes (MB, 2*1024*M sectors)", 0Dh, 0Ah
	db	"  G) Giga bytes (GB, 2*1024*1024*G sectors)", 0Dh, 0Ah
	db	0Dh, 0Ah	
	db	"Press SPACE to use entire space or press ENTER to set Megabytes .. ", 0

msg_c_part_error:
	db 	0Dh, 0Ah
	;db	"No free space for a new primary partition", 0Dh, 0Ah, 0
	; 21/03/2021
	db	"No free space for a new primary partition !", 0Dh, 0Ah, 0
; 21/03/2021
;msg_c_ldd_error: 
;	db	"and logical dos drives over program limit (4) !", 0Dh, 0Ah, 0
msg_c_ldd_q:
	db	0Dh, 0Ah
	db	"Do you want to create logical dos drive in extended dos partition "
	db	'? (Y/N)', 0

msg_c_ldd_nofspc_error:
	db 	0Dh, 0Ah
	db	"No free space for a new logical dos drive !", 0Dh, 0Ah, 0

	; 12/10/2020
msg_drv_not_ready:
	db	0Dh, 0Ah
	db	"Disk drive not ready or read error ! "
	db	0Dh, 0Ah, 0

msg_not_any_disks:
	db	0Dh, 0Ah
	db	"There is not a hard disk (ready) ! "
	db	0Dh, 0Ah, 0

align 4

msg_sectors_crlf:
	db	" sector"
msg_sectors_crlf_s:
	db	"s"
	db	0Dh, 0Ah, 0

vname_length:
	db	0 ; 05/01/2018

bs_oem_name:
	;db	'TRDOS2.0', 0
	; 04/05/2024
	db	'RETRODOS', 0
align 2

no_name:
	db 	'NO NAME    ', 0

align 2

FDFORMAT_SECBUFFER:
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

; 29/10/2020
msg_100:
	db	'100', 0

SizeOfFile equ $-100

; 03/02/2019

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

; 12/10/2020
DrvNum:	resb 1
hdc:	resb 1
;hs:	resw 1 ; (bios/dos virtual) head*sectors ; 16/10/2020
rw:	resb 1 
rcnt:	resb 1 ; 18/10/2020
total_sectors:
disksize: resd 1
chs_limit: resd 1

; 16/10/2020
;lba_cyls: resd 1  ; cylinder count calculated from LBA sectors
lba_chs_remain: resw 1	 ; remain sectors after the last cylinder

alignb 2
old_sp:	resw 1

HDFORMAT_FATBUFFER:
HDFORMAT_EMPTY_BUFF:
	resb 512

data_start: resd 1
data_sectors: resd 1
cluster_count: resd 1
root_dir_secs: resw 1
format_percent: resw 1
prev_percent:	resb 1
rsvdbyte:	resb 1

alignb 4

; 05/01/2018
fs_volume_name: resb 64
fs_volume_serial: resd 1
DAT_FFBit:	resw 1
DAT_FFSector:	resw 1
		resw 1 ; 19/03/2021
DAT_LFBit:	resw 1
DAT_LFSector:	resw 1
		resw 1 ; 19/03/2021

;alignb 4

FS_MAT_Buffer: ; TRFS1 Master Allocation Table (05/01/2018)
MAT_Sign:		resb    3	; Offset 0  ; 'MAT' 
MAT_Version:		resb 	1	; 	 3  ; 0	
MAT_VolumeSize:		resd	1	;	 4  ; FS1 Volume Size
MAT_BeginSector:	resd	1	;	 8  ; FS1 Start Sector
DAT_Address:		resd	1	;	12  ; Offset (=2)
DAT_SectorCount:	resd	1	;	16  
MAT_FreeSectors:	resd 	1	; 	20
MAT_FirstFreeSector:	resd	1	;	24
;MAT_OS_Reserved:
;	 		resb	9
;MAT_Unused:
;			resb	112
FS_DAT_Buffer: ; TRFS1 Disk Allocation Table (05/01/2018)
FS_RDT_Buffer: ; TRFS1 Root Directory Description Table (05/01/2018)
			resb	512		
;alignb 4

; (TR-DOS 386 compatible) Hard Disk (image) parameters

;total_sectors: resd 1
pType:	       resb 4
file_size:     resd 1
pp_StartSector: resd 1
pp_Sectors:	resd 1
wholedisk:	resb 1
pp_type: resb 1 ; Primary partition type (for this program)
pp_type_user: resb 1
chs_focus: resb 1
pSize_temp: resd 1
pSize_multiplier: resd 1
pSize_maxdigits: resb 1
pSize_digitpos: resb 1
msg_psize_unit:
pSize_unit:	resw 1
		resb 1
; 06/11/2020		
;reserved_bytes: resb 3 ; for 11 bytes of numbers
msg_partition_sectors:
		;resb 8
		resb 11 ; 06/11/2020
		resb 1
format_q:	resb 1 ; 03/02/2018

;alignb 2
pType_pos:	resb 1
pType_num:	resb 1
		resb 1
;cylinder_boundary:
		resb 1
; 05/11/2020

alignb 2

GetChar:	resb 1 ; 11/02/2019
existingfile:	resb 1 ; 12/02/2019

hs: ; 17/10/2020 ; (bios/dos virtual) head*sectors
min_sectors:	resw 1 ; 08/02/2019
pp_StartCylinder: resw 1
pp_EndCylinder:	resw 1

ppn_Sectors:	resd 1 ; 09/02/2019

input_col:	resw 1
No:		resb 1
Yes:		resb 1

; 26/02/2019
ldrives:	resb 1	

;sort_1:	resb 1
		resb 1 ; 23/10/2020
sort:		resb 4

;max_sector:	resb 8
ebr_buffer:		; 26/02/2019
boot_record:	resb 512

valid_input:	resb 1
		resb 1

; 26/02/2019
ep_StartSector:	resd 1
ep_EndSector:	resd 1
ep_Size:	resd 1

; 10/02/2019
valid_ppnums:	resb 4	

_i_:	resw 1

freespace_count: resw 1
last_found_partition: resb 1
p_type: resb 1	

; 30/10/2020
;p_sorted:
;	resb 1
;ep_sorted:
;	resb 1

_e_:	resb 1

act_part_num:
cre_part_num:
del_part_num: resb 1 ; 10/02/2019

alignb 2

pte_row: resb 80

_zero_:	resb 1

;alignb 2

	resb 1

bss_clear_end: ; 15/02/2019

; PARTITION DATA STRUCTURE
; 4 partitions
; 18 byte partition data structure per partition
; 72 bytes (4*18)

part_table_boot_ind:	 resb 1
part_table_start_head:	 resb 1
part_table_start_sector: resb 1
part_table_start_cyl:	 resw 1
part_table_sys_id:	 resb 1
part_table_end_head:	 resb 1
part_table_end_sector:	 resb 1
part_table_end_cyl:	 resw 1
part_table_rel_sec_lw:	 resw 1
part_table_rel_sec_hw:	 resw 1
part_table_num_sec_lw:	 resw 1
part_table_num_sec_hw:	 resw 1

			resb 54 ; 3*18 bytes

pcount:		resb 1
ppcount:	resb 1
apcount:	resb 1
epnumber:	resb 1

; LOGICAL PARTITION DATA STRUCTURE
; (for logical partitions in extended partition)

; 1 extended partition
; 4 logical drives (18 bytes)
; 72 bytes (4*18)

ext_table_boot_ind:	resb 1
ext_table_start_head:	resb 1
ext_table_start_sector: resb 1
ext_table_start_cyl:	resw 1
ext_table_sys_id:	resb 1
ext_table_end_head:	resb 1
ext_table_end_sector:	resb 1
ext_table_end_cyl:	resw 1
ext_table_rel_sec_lw:	resw 1
ext_table_rel_sec_hw:	resw 1
ext_table_num_sec_lw:	resw 1
ext_table_num_sec_hw:	resw 1

			resb 54 ; 3*18 bytes

fspc:		; 5*8 words (for free space calculations)

; Space 1 - unused cylinders before partition 0
; Space 2 - unused cylinders between partition 0 & 1
; Space 3 - unused cylinders between partition 1 & 2
; Space 4 - unused cylinders between partition 2 & 3
; Space 5 - unused cylinders after partition 3

free_space.space:   	   resw 1
free_space.start:   	   resw 1
free_space.end:	   	   resw 1
free_space.percent_unused: resw 1
free_space.sectors_unused: resd 1
free_space.startsector:   resd 1 ; 13/02/2019
		;resw  4*6 
		resw   4*8 ; 4*16 bytes ; 13/02/2019

; 18/02/2019
c_cylinder:	resw 1
c_fspc_offset:	resw 1
cylinder_boundary: resb 1
;c_gap:		resb 1
		resb 1

; 27/02/2019
ldd_start:	resd 4
; 03/03/2019
ldd_size:	resd 4
; 30/10/2020
ep_free_sectors: resd 1

; 25/02/2019
pte_address:	resw 1
; 02/03/2019
lcylinders:	resw 1
; 01/11/2020
endcyl:		resw 1

;bss_clear_end: ; 18/02/2019

; 11/10/2020
;gdp_buffer:	resb 30 ; buffer for int 13h, ah = 48h
gdp_buffer:	resb 26 ; buffer for int 13h, ah = 48h
; 19/03/2021
tempword:	resw 1

bss_end:
