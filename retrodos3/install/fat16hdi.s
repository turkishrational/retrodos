; ****************************************************************************
; FAT16HDI.S (FAT16HDI.COM) - Retro DOS v3 FAT16 Disk Image Formatting Utility
; 					 	          (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 19/10/2018
; ----------------------------------------------------------------------------
; Beginning: 17/10/2018
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11
; ----------------------------------------------------------------------------
; Modified from 'rdhdimg.s'(RDHDIMG.COM) source code by Erdogan Tan
; (17/10/2018) - Retro DOS v2.0 FAT12 hard disk image formatting utility -
; ----------------------------------------------------------------------------
; Derived from 'hdimage.s'(HDIMAGE.COM) source code by Erdogan Tan
; (03/02/2018) - TRDOS 386 hard disk image formatting utility -
; ****************************************************************************
; nasm rdhdimg.s -l rdhdimg.lst -o RDHDIMG.COM

bsOemName	equ RETRODOS_FAT16_hd_bs + 3
bsBytesPerSec	equ RETRODOS_FAT16_hd_bs + 11
bsSecPerClust	equ RETRODOS_FAT16_hd_bs + 13
bsResSectors	equ RETRODOS_FAT16_hd_bs + 14
bsFATs		equ RETRODOS_FAT16_hd_bs + 16	
bsRootDirEnts	equ RETRODOS_FAT16_hd_bs + 17
bsSectors	equ RETRODOS_FAT16_hd_bs + 19
bsMedia		equ RETRODOS_FAT16_hd_bs + 21
bsFATsecs	equ RETRODOS_FAT16_hd_bs + 22
bsSecPerTrk	equ RETRODOS_FAT16_hd_bs + 24
bsHeads		equ RETRODOS_FAT16_hd_bs + 26
bsHidden1	equ RETRODOS_FAT16_hd_bs + 28
bsHidden2	equ RETRODOS_FAT16_hd_bs + 30	
bsHugeSectors	equ RETRODOS_FAT16_hd_bs + 32
bsDriveNumber   equ RETRODOS_FAT16_hd_bs + 36
bsReserved1	equ RETRODOS_FAT16_hd_bs + 37
bsBpbSignature	equ RETRODOS_FAT16_hd_bs + 38
bsVolumeID      equ RETRODOS_FAT16_hd_bs + 39
bsVolumeLabel   equ RETRODOS_FAT16_hd_bs + 43
bsFileSysType	equ RETRODOS_FAT16_hd_bs + 54
bsReserved2	equ RETRODOS_FAT16_hd_bs + 62
bsDataStart	equ RETRODOS_FAT16_hd_bs + 64	
bsRootDirStart	equ RETRODOS_FAT16_hd_bs + 66
bsRootDirSects	equ RETRODOS_FAT16_hd_bs + 68

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

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

pt_cylinders	equ RETRODOS_MASTERBOOT_SECTOR + 1B8h	
pt_heads	equ RETRODOS_MASTERBOOT_SECTOR + 1BAh	
pt_sectors	equ RETRODOS_MASTERBOOT_SECTOR + 1BCh
partition_table equ RETRODOS_MASTERBOOT_SECTOR + 1BEh

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

; Cursor Location
CCCpointer equ  0450h   ; BIOS data, current cursor column

; MINIMUM & MAXIMUM SECTORS (partition and disk size limits in sectors)
MINPARTSIZE equ 4096 ; 2GB
MAXPARTSIZE equ 4128768 - 1 ; 2GB - masterboot sector	
MINDISKSIZE equ 16384 ; 8MB
MAXDISKSIZE equ 4128768 ; 2GB

pTableOffset equ 1BEh ; 446		

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
	jz	R_15			; jump if zero
R_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short R_01 		
	jb	R_15
	
	; check hd image file name
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
	jnb	short R_10
	sub	di, bx
	sub	bx, img_file_name
	cmp	di, 3
	jna	short R_09
	and	bx, bx
	jnz	short R_10
	jmp	short R_11		
R_09:
	cmp	bx, 8
	jna	short R_11
R_10:
	mov	si, msg_inv_file_name
	call	print_msg
	jmp	R_14

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Find image file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
R_11:
	mov	dx, img_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS-DOS Function = Find First File
	int	21h
	jc	R_19

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Check image file features
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	si, DTA_Attrib
	mov	al, [si]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	jnz	short R_11e     
	mov	si, DTA_FileSize
	lodsw
	mov	dx, [si]

	cmp	dx, 1F8h
	jb	short R_11f
	je	short R_11d
	
	cmp	dx, 1F80h ; High word of 1F800000h (512MB)
	ja	short R_11e

	and	ax, ax
	jnz	short R_11e

	;mov	ax, 128 ; * 128 (65536/512 = 128)
	mov	al, 128
	mul	dx
	cmp	dx, 0Fh  ; dx:ax = 0FC000h (512MB disk)
	jne	short R_11a
	cmp	ax, 0C000h
	jne	short R_11e
	jmp	short R_12
R_11a:
	cmp	dx, 7
	ja	short R_11e
	jb	short R_11b
	cmp	ax, 0E000h  ; 7E000h (256MB disk)
	jne	short R_11e
	jmp	short R_12	
R_11b:
 	cmp	dx, 3
	ja	short R_11e
	jb	short R_11c
	cmp	ax, 0F000h  ; 3F000h (128MB disk)
	jne	short R_11e
	jmp	short R_12
R_11c:
	cmp	dx, 1	    ; 1F800h (64MB disk)
	jne	short R_11e
	cmp	ax, 0F800h
	je	short R_12
	jmp	short R_11e
R_11d:
	or	ax, ax
	jz	short R_12
R_11e:
	jmp	short R_17

R_11f:
	test	ax, 511
	jnz	short R_17

	shr	ax, 9   ; / 512
	shl	dx, 7 ; 65536/512 = 128 (128*DX sectors + AX sectors)
	add	ax, dx	

	cmp	ax, 64512 ; 64*16*63 sectors
	ja	short R_17
	je	short R_12
	cmp	ax, 41616 ; 306*8*17 sectors
	jb	short R_17
	je	short R_12
	cmp	ax, 62832 ; 462*8*17 sectors
	je	short R_12
	ja	short R_17
	cmp	ax, 32256 ; 32*16*63 sectors
	je	short R_12
	jb	short R_17
	cmp	ax, 40320 ; 40*16*63 sectors
	jne	short R_17		
R_12:

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
R_13:
	xor	ax, ax
	int	16h			; wait for keyboard command
	cmp	al, 'C'-40h
	je	short R_14 ; Exit                   
	cmp	al, 27
	je	short R_14 ; Exit
	and	al, 0DFh
	cmp	al, 'Y'			; Yes?
	je	short R_16		; write
	cmp	al, 'N'			; No?
	jne	short R_13      
					; no write (exit)  
	mov	si, Msg_NO
	call	print_msg 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Nextline & Exit
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

R_14:
	mov	si, CRLF
	call	print_msg
	mov	ax, 4C00h		; terminate
	int	21h

R_15:
	mov	si, RetroDOS_Welcome
	call	print_msg
	jmp	short R_14 ; Exit

R_16:
	mov	si, Msg_YES
	call	print_msg
	jmp	short R_18

R_17:
	mov	si, msg_inv_image_file
	call	print_msg
	jmp	short R_14

R_18:
	; Delete existing image file
	mov	ah, 41h ; Delete file
	mov	dx, img_file_name
	int	21h
	jnc	short R_19
	cmp	al, 2 ; file not found
	jne	short R_17 ; invalid image file

R_19:
	call	write_chs_table
R_20:
	xor	ah, ah
	int	16h

	cmp	al, '1'
	jb	short R_21
	cmp	al, '9'
	ja	short R_20
	sub	al, '0'
	jmp	short R_22
R_21:
	cmp	al, 27 ;  ESCape key
	jne	short R_20

	; Exit
	mov	ax, 4C00h
	int	21h

R_22:
	mov	[DiskType], al

	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	mov	bl, [DiskType]	; 1 to 9
	dec	bl 		; 0 to 8
	;xor	bh, bh
	mov	si, bx ; 1i
	shl	bx, 1
	add	bx, si ; bx = 3i	
	add	bx, DskTypeNumStr
	mov	al, [bx]
	mov	[msg_disk_size],al
	inc	bx
	mov	ax, [bx]
	mov	[msg_disk_size+1], ax
	shl	si, 1 ; 2i
	mov	bx, si
	shl	si, 2 ; 8i
	add	si, bx ; 8i+2i = 10i
	add	si, DskTypeParms
	lodsw
	mov	[pt_cylinders], ax
	lodsw
	mov	[pt_heads], ax
	lodsw
	mov	[pt_sectors], ax
	lodsw
	mov	[diskimage_size], ax	
	lodsw
	mov	[diskimage_size+2], ax	
	
	mov	si, msg_creating
	call	print_msg

	mov	si, msg_disk_size
	mov	al, [si]
	sub	al, '1'
	adc	si, 0
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Create a new hard disk image file
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		
	mov	dx, img_file_name
	mov	cx, 0 ; File Attributes
	mov	ah, 3Ch ; MS-DOS Function = Create File
	int	21h
	jc	short R_23

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Open image file for writing
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	mov	al, 2 ; open for reading and writing
	;mov	dx, img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jnc	short R_24

R_23:
	; AL = error code
	call	bin_to_hex
	mov 	[error_code], ax

	mov	si, CRLF
	call	print_msg

	mov	si, Msg_Error
	call	print_msg

	int	20h	; Exit

R_24:
	mov	[img_file_handle], ax
R_25:
	mov	dx, HDFORMAT_SECBUFFER  ; 0F6h buffer
	mov	cx, 512
	mov	bx, [img_file_handle]
	mov	ah, 40h ; write to file	
	int	21h
	jc	short R_26

	inc	dword [CWSector]
R_25a:
	mov	ax, [diskimage_size]
	mov	dx, [diskimage_size+2]

	mov	cx, 1

	cmp	dx, [CWSector+2]
	ja	short R_25b
	
	cmp	ax, [CWSector]
	jna	short R_27
	
R_25b:
	mov	ax, [CWSector]
	and	ax, 3
	mov	bx, RSymbols
	add	bx, ax
	mov	al, [bx]
	mov	bx, 7
	;mov	cx, 1
	mov	ah, 0Ah ; write character (on previous cpos)
	int	10h

	jmp	short R_25

R_26:
	; Error.. close file..
	push	ax
	mov	ah, 3Eh ; close file
	;mov	bx, [img_file_handle]
	int	21h
	pop	ax
	jmp	short R_23

R_27:
	mov	al, 20h ; space
	mov	bx, 7
	mov	ah, 0Ah ; write character (on previous cpos)
	;mov	cx, 1
	int	10h
	
	mov	si, msg_OK
	call	print_msg

	; Wait about 1 second (~16/18.2)
	; "before press any key to continue message"
	xor	ah, ah ; 0
	int	1Ah
	; CX:DX = tick count (18.2 ticks per second)
	and	dx, 0Fh
	mov	si, dx
	xor	bx, bx
R_28:
	mov	ax,0
	int	1Ah	
	inc	bx
	jz	short R_29 
	and	dx, 0Fh
	cmp	dx, si
	jne	short R_28
R_29:
	mov	si, msg_press_any_key
	call	print_msg

	xor	ah, ah
	int	16h

R_30:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h
	
	mov	si, msg_create_dos_partition_h ; header
	call	print_msg
	mov	si, msg_create_dos_partition_m ; menu
	call	print_msg
R_31:
	xor	ah, ah
	int	16h
	cmp	al, 27 ; ESC key	
	je	short R_32
	cmp	al, '0'
	je	short R_32
	jb	short R_31
	cmp	al, '9'
	ja	short R_31

	xor	ch, ch
	sub	al, '0'
	mov	cl, al
	dec	cl  ; 0 to 8
	mov	bl, [DiskType]
	dec	bl  ; 0 to 8
	mov	al, 9  ; partition size options = 9
	mul	bl	
	mov	bx, OptionValidatingTable
	add	ax, cx
	add	bx, ax
	mov	al, [bx]
	and	al, al
	jnz	short R_34

	mov	si, msg_option_not_proper
	call	print_msg
	mov	si, msg_press_any_key
	call	print_msg
	xor	ah, ah
	int	16h
	jmp	short R_30

R_32:	
	mov	si, CRLF
	call	print_msg

	xor	al, al	; return code = 0
	mov	ah, 4Ch ; EXIT - terminate with return code
	int	21h

R_33:
	jmp	short R_33

R_34:
	dec	al ; zero based type of partition selection
	;mov	ah, 20
	mov	ah, 14 ; 19/10/2018
	mul	ah
	mov	si, partition_parms
	add	si, ax
	mov	bx, partition_table
	lodsw
	mov	[bsSecPerClust], al  ; sectors per cluster
	;lodsw
	;;mov	cx, ax  	     ; number of clusters
	;lodsw
	;mov	[bsRootDirEnts], ax  ; root directory entries
	;mov	word [bsRootDirEnts], 512
	;add	ax, 15
	;shr	ax, 4
	;mov	[bsRootDirSects], ax ; root directory sectors
	;mov	byte [bsRootDirSects], 32	
	lodsw
	mov	[bsHidden1], ax	     ; hidden sectors
	mov	[bx+ptStartSector], ax ; start sector
	;mov	word [bx+ptStartSector+2], 0

	;mov	word [bsHidden2], 0
	;lodsw
	;;mov	[bsResSectors], ax   ; reserved sectors = 1
	lodsw
	mov	[bsFATsecs], ax      ; fat sectors
	lodsw
	mov	[bsRootDirStart], ax ; root directory address
	lodsw
	mov	[bsDataStart], ax    ; first data sector address
	lodsw
	mov	dx, [si]

	or	dx, dx
	jz	short R_34a  ; dx = 0
				     ; Big Partition (06h)
	mov	[bsHugeSectors], ax 
	mov	[bsHugeSectors+2], dx

	;mov	word [bsSectors], 0

	mov	cx, 06h	; FAT 16 BIG (>32MB) partition	
 	jmp	short R_34b

R_34a:	
	mov	[bsSectors], ax      ; partition/volume size in sectors
	;mov	[bsHugeSectors], ax 
	;;mov	[bsHugeSectors], 0 
	;mov	word [bsHugeSectors+2], 0
	mov	cx, 04h ; FAT 16 (<=32MB) partition	
R_34b:
	mov	[bx+ptSectors], ax
	mov	[bx+ptSectors+2], dx

	; DX = High word of partition size
	;mov	cx, ax 		     ; volume/partition size in sectors
	xchg	cx, ax

	mov	byte [bx], 80h       ; Active/Bootable partition
	mov	[bx+ptFileSystemID], al ; 04h or 06h
	mov	al,1
	mov	[bx+ptBeginHead], al ; beginning head = 1
	mov	[bx+ptBeginSector], al ; beginning sector = 1
	;xor	ah, ah	
	;mov	[bx+ptBeginCylinder], ah ; beginning cylinder = 0
		
	mov	al, [pt_sectors]
	mov	[bsSecPerTrk], al
	;mov	byte [bsSecPerTrk+1], 0
	mov	[bx+ptEndSector], al	
	add	cx, ax		     ; + sectors per track  
	;adc	dx, 0    		           

	mov	ah, [pt_heads]
	mov	[bsHeads], ah
	;mov	byte [bsHeads+1], 0
	dec	ah
	mov	[bx+ptEndHead], ah
	inc	ah
	mul	ah
	xchg	cx, ax
	div	cx
	dec	ax  ; the last cylinder of the partition (< 1024) 
	mov	[bx+ptEndCylinder], al
	
	; 19/10/2018
	and	ah, ah
	jz	short R_34c ; last cylinder number is less than 256

	shl	ah, 6 ; shift cylinder (AX) bits 8 & 9 to AH bits 6 & 7
	or	byte [bx+ptEndSector],ah ; .. and put it in EndSector bits 6 & 7

R_34c:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h	

	mov	si, msg_writing_mbr
	call	print_msg

	; Move file pointer to start of the file
	;xor	al, al
	mov	bx, [img_file_handle]
	sub	cx, cx
	;sub	dx, dx
	mov	ah, 42h ; seek (move file pointer)
	int	21h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing masterboot sector
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	;mov	bx, [img_file_handle]
	mov	dx, RETRODOS_MASTERBOOT_SECTOR ; Singlix FS1 MBR
	mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit

	mov	si, msg_OK
	call	print_msg

	mov	di, bsVolumeLabel
	call	write_volume_name
	mov	si, bsVolumeID
	call	write_volume_serial

	mov	si, msg_writing_boot_sector
	call	print_msg

	; Move file pointer to start of the dos partition
	mov	dx, [partition_table+ptStartSector]
	
	mov	ax, 512
	mul	dx
	mov	cx, dx
	mov	dx, ax

	sub	al, al
	mov	bx, [img_file_handle]
	mov	ah, 42h ; seek (move file pointer)
	int	21h
	;jc	R_26

	;mov	bx, [img_file_handle]
	mov	dx, RETRODOS_FAT16_hd_bs
	mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit

	mov	si, msg_OK
	call	print_msg

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; writing FAT sectors
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	;mov	si, CRLF
	;call	print_msg

	; Clear sector buffer (clear 0F6h bytes)
	mov	di, HDFORMAT_FATBUFFER
	xor	ax, ax
	mov	cx, 256
	rep	stosw 

	mov	si, msg_writing_FAT_sectors
	call	print_msg

	mov	bp, [bsFATsecs]
	dec	bp
	mov	di, bp

	mov	ax, 0FFF8h
	mov	si, HDFORMAT_FATBUFFER
	mov	[SI], ax
	mov	al, ah
	mov	[SI+2], ax ; FFFFh

	inc	al ; 0	
	;sub	al, al
	mov	bx, [img_file_handle]
	mov	dx, si ; [HDFORMAT_FATBUFFER]
	mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit
	;mov	bx, [img_file_handle]
	mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	xor	ax, ax
	mov	[SI], ax
	mov	[SI+2], ax
R_35:
	;mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit

	sub	al, al

	dec	bp
	jnz	short R_35

	mov	ax, 0FFF8h
	;mov	si, HDFORMAT_FATBUFFER
	mov	[SI], ax
	mov	al,ah
	mov	[SI+2], ax ; 0FFFFh

	;inc	al
	sub	al, al
	;mov	bx, [img_file_handle]
	mov	dx, si; [HDFORMAT_FATBUFFER]
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit
	;mov	bx, [img_file_handle]
	mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	sub	ax, ax
	mov	[SI], ax
	mov	[SI+2], ax
R_36:
	;mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit

	sub	al, al
	
	dec	di
	jnz	short R_36

	mov	si, msg_OK
	call	print_msg

	;mov	si, CRLF
	;call	print_msg

	mov	si, msg_writing_root_dir
	call	print_msg

	mov	di, [bsRootDirEnts]
	shr	di, 4 ; 240/16 = 15, 512/16 =  32 sectors
	mov	bx, [img_file_handle]
	mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
R_37:
	sub	al, al
	;mov	bx, [img_file_handle]
	;mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit
	
	dec	di
	jnz	short R_37

	mov	si, msg_OK
	call	print_msg

	mov	ah, 3Eh ; close file
	mov	bx, [img_file_handle]
	int	21h

	jmp	R_32


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
	;mov	di, [bp+47h) ; [BS_VolLab] ; FAT32
	;mov	di, [bp+2Bh) ; [BS_VolLab] ; FAT16 (&FAT12)
	mov	bx, di ; *	
	;ja	short svn_2
	jb	short svn_5
;svn_1:
;	mov	si, no_name
;	lodsb50
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

write_chs_table:
	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	mov	si, Dsk_Type_Select_msg
	call	print_msg

	retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Data
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DskTypeParms:
DskType1Parms:
cyl1:	dw 306
hed1:	dw 8
spt1:	dw 17
cap1:	dd 306*8*17
DskType2Parms:
cyl2:	dw 462
hed2:	dw 8
spt2:	dw 17
cap2:	dd 462*8*17
DskType3Parms:
cyl3:	dw 32
hed3:	dw 16
spt3:	dw 63
cap3:	dd 32*16*63
DskType4Parms:
cyl4:	dw 40
hed4:	dw 16
spt4:	dw 63
cap4:	dd 40*16*63
DskType5Parms:
cyl5:	dw 64
hed5:	dw 16
spt5:	dw 63
cap5:	dd 64*16*63
DskType6Parms:
cyl6:	dw 128
hed6:	dw 16
spt6:	dw 63
cap6:	dd 128*16*63
DskType7Parms:
cyl7:	dw 256
hed7:	dw 16
spt7:	dw 63
cap7:	dd 256*16*63
DskType8Parms:
cyl8:	dw 512
hed8:	dw 16
spt8:	dw 63
cap8:	dd 512*16*63
DskType9Parms:
cyl9:	dw 1024
hed9:	dw 16
spt9:	dw 63
cap9:	dd 1024*16*63

partition_parms:

; FAT16 File System (04h) ; <= 32MB

Partition_10MB_17: ; 1
secpc1a: dw 2
;clsts1a: dw 10338
;roote1a: dw 512
hidds1a: dw 17
;ressc1a: dw 1
fatsc1a: dw 41
rootd1a: dw 83
fdats1a: dw 115
parts1a: dd (153*8*17)-17 ; 20791 (0 sector free)

Partition_10MB_63: ; 2
secpc1b: dw 2
;clsts1b: dw 9992
;roote1b: dw 512
hidds1b: dw 63
;ressc1b: dw 1
fatsc1b: dw 40
rootd1b: dw 81
fdats1b: dw 113
parts1b: dd (20*16*63)-63 ; 20097 (0 sector free)

Partition_16MB_17: ; 3
secpc2a: dw 2
;clsts2a: dw 16231
;roote2a: dw 512
hidds2a: dw 17
;ressc2a: dw 1
fatsc2a: dw 64
rootd2a: dw 129
fdats2a: dw 161
parts2a: dd (240*8*17)-17 ; 32623 (0 sector0s free)

Partition_16MB_63: ; 4
secpc2b: dw 2
;clsts2b: dw 16017
;roote2b: dw 512
hidds2b: dw 63
;ressc2b: dw 1
fatsc2b: dw 63
rootd2b: dw 127
fdats2b: dw 159
parts2b: dd (32*16*63)-63 ; 32193 (0 sector free)					

Partition_20MB_17: ; 5
secpc3a: dw 4
;clsts3a: dw 10371
;roote3a: dw 512
hidds3a: dw 17
;ressc3a: dw 1
fatsc3a: dw 41
rootd3a: dw 83
fdats3a: dw 115
parts3a: dd (306*8*17)-17 ; 41599 (0 sector  free)

Partition_20MB_63: ; 6	
secpc3b: dw 4
;clsts3b: dw 10036
;roote3b: dw 512
hidds3b: dw 63
;ressc3b: dw 1
fatsc3b: dw 40
rootd3b: dw 81
fdats3b: dw 113
parts3b: dd (40*16*63)-63 ; 40257 (0 sector free)

Partition_31MB:    ; 7
secpc4a: dw 4
;clsts4a: dw 15664
;roote4a: dw 512
hidds4a: dw 17
;ressc4a: dw 1
fatsc4a: dw 62
rootd4a: dw 125
fdats4a: dw 157
parts4a: dd (462*8*17)-17 ; 62815 (2 sectors free)

Partition_32MB:	   ; 8
secpc4b: dw 4
;clsts4b: dw 16072
;roote4b: dw 512
hidds4b: dw 63
;ressc4b: dw 1
fatsc4b: dw 63
rootd4b: dw 127
fdats4b: dw 159
parts4b: dd (64*16*63)-63 ; 64449 (2 sectors free)

; FAT16 Big File System (06h) ; > 32MB

Partition_64MB:	   ; 9
secpc5: dw 4
;clsts5: dw 32169
;roote5: dw 512
hidds5: dw 63
;ressc5: dw 1
fatsc5: dw 126
rootd5: dw 253
fdats5: dw 285
parts5: dd (128*16*63)-63 ; 128961 (0 sector free)

Partition_128MB:   ; 10
secpc6: dw 4
;clsts6: dw 64362
;roote6: dw 512
hidds6: dw 63
;ressc6: dw 1
fatsc6: dw 252
rootd6: dw 505
fdats6: dw 537
parts6: dd (256*16*63)-63 ; 257985 (0 sector free)

Partition_256MB:   ; 11
secpc7: dw 8
;clsts7: dw 64437
;roote7: dw 512
hidds7: dw 63
;ressc7: dw 1
fatsc7: dw 252
rootd7: dw 505
fdats7: dw 537
parts7: dd (512*16*63)-63 ; 516033 (0 sector free)

Partition_512MB:   ; 12
secpc8: dw 16
;clsts8: dw 64474
;roote8: dw 512
hidds8: dw 63
;ressc8: dw 1
fatsc8: dw 252
rootd8: dw 505
fdats8: dw 537
parts8: dd (1024*16*63)-63 ; 1032129 (8 sectors free)

OptionValidatingTable:
; 	Option 10M,16M,20M,32M,64M,128M,256M,512M,WHOLE DISK	
	db	1,  3,  5,  0,  0,  0,   0,   0,    5	 ;  20MB, 17SPT
	db	1,  3,  5,  7,  0,  0,   0,   0,    7	 ;  31MB, 17SPT	 	
	db	2,  4,  0,  0,  0,  0,   0,   0,    4	 ;  16MB, 63SPT
	db	2,  4,  6,  0,  0,  0,   0,   0,    6	 ;  20MB, 63SPT
	db	2,  4,  6,  8,  0,  0,   0,   0,    8	 ;  32MB, 63SPT
	db	2,  4,  6,  8,  9,  0,   0,   0,    9	 ;  64MB, 63SPT
	db	2,  4,  6,  8,  9, 10,   0,   0,    10	 ; 128MB, 63SPT
	db	2,  4,  6,  8,  9, 10,  11,   0,    11	 ; 256MB, 63SPT
	db	2,  4,  6,  8,  9, 10,  11,  12,    12	 ; 512MB, 63SPT

	db	0

RETRODOS_MASTERBOOT_SECTOR:
	incbin	'FS1_MBR.BIN' ; Singlix FS1 MBR	

RETRODOS_FAT16_hd_bs: 
	incbin	'RD3HDBS.BIN'

	db	0

hexchrs:
	db	'0123456789ABCDEF'

align 2

img_file_handle:
	dw	0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  Messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Dsk_Type_Select_msg:
db  "RETRO DOS v3.0 Hard Disk Image Utility by Erdogan Tan (2018)                    "
db  "                                                                                "
db  "                                                                                "
db  "Disk Type     Cylinders     Heads     Sectors       Capacity                    "
db  "---------     ---------     -----     -------       --------                    "
db  "     1           306          8         17            20 MB                     "
db  "     2           462          8         17            31 MB                     "
db  "                                                                                "
db  "     3            32         16         63            16 MB                     "
db  "     4            40         16         63            20 MB                     "
db  "     5            64         16         63            32 MB                     "
db  "                                                                                "
db  "     6           128         16         63            64 MB                     "
db  "     7           256         16         63           128 MB                     "
db  "     8           512         16         63           256 MB                     "
db  "     9          1024         16         63           512 MB                     "
db  "                                                                                "
db  "                                                                                "
db  "Press disk number (1 to 9) to SELECT or                                         "
db  "                                                                                "
db  "Press ESC to cancel.                                                            "
db  "                                                                                "
db  0

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Retro DOS v3.0 Fixed Disk Image (FAT16 FS) Format Utility'
	db	0Dh, 0Ah
	db	"v1.0.191018  (c) Erdogan TAN 2018"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: fat16dhi <image file name> '
	db	0

msg_inv_file_name: 
	db	0Dh, 0Ah
	db	"Invalid file name !", 0Dh, 0Ah
	db	"(File name must fit to 8.3 DOS format) !"
	db	0Dh, 0Ah, 0

msg_inv_image_file:
	db	0Dh, 0Ah
	db	"Invalid fixed disk image file !", 0Dh, 0Ah
	db	"(File size is not compatible) !"
	db	0Dh, 0Ah, 0 

msg_option_not_proper:
	db	"Selected partion size is not proper for selected disk size !"
	db	0Dh, 0Ah
	db	"(You need to select another option.)"
	db	0Dh, 0Dh, 0 

msg_any_key_esc_exit:
	db	0Dh, 0Ah
	db	"Press ESC to exit or press another key to continue..."
	db	0

msg_create_dos_partition_h:
	db	0Dh, 0Ah
	db	"--------------------------------------------------------------------------------"
	db	"                        Create Primary DOS Partition                            "
	db	"--------------------------------------------------------------------------------"	
	db	0Dh, 0Ah, 0	
msg_create_dos_partition_m:
	db	"Select an option: "
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"  1) Use 10 MB disk space for FAT16 (04h) partition ", 0Dh, 0Ah
	db	"  2) Use 16 MB disk space for FAT16 (04h) partition ", 0Dh, 0Ah
	db	"  3) Use 20 MB disk space for FAT16 (04h) partition ", 0Dh, 0Ah
	db	"  4) Use 32 MB disk space for FAT16 (04h) partition ", 0Dh, 0Ah
	db	0Dh, 0Ah  
	db	"  5) Use  64 MB disk space for FAT16 (06h) partition ", 0Dh, 0Ah
	db	"  6) Use 128 MB disk space for FAT16 (06h) partition ", 0Dh, 0Ah
	db	"  7) Use 256 MB disk space for FAT16 (06h) partition ", 0Dh, 0Ah
	db	"  8) Use 512 MB disk space for FAT16 (06h) partition ", 0Dh, 0Ah
	db	0Dh, 0Ah  
	db	"  9) Use whole disk space for FAT16 partition ", 0Dh, 0Ah
	db	0Dh, 0Ah	
	db	"Press ESC or 0 to exit .. "
 	db 	0Dh, 0Ah, 0

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

msg_writing_mbr:
	db	"Writing masterboot sector...", 0

msg_writing_disk_sectors:
	db	"Writing disk sector: ", 0

msg_writing_boot_sector:
	db	"Writing volume boot sector...", 0

msg_writing_FAT_sectors:
	db	"Writing FAT sectors...", 0

msg_writing_root_dir:
	db	"Writing root directory sectors...", 0

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
Msg_YES:
	db	 20h
msg_yes_:
	db	'YES', 0
Msg_NO:
	db	20h
msg_no_:
	db	'NO', 0

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

msg_disk_type_selected:
	db	"Disk Type "
delected_disk_num:
	db	"  has been selected."
	db 0Dh, 0Ah, 0
msg_enter_cancel:
	db	0Dh, 0Ah
	db	"Press ENTER to write file or press ESC to cancel." 
	db	0Dh, 0Ah, 0

msg_press_any_key:
	db	0Dh, 0Ah
	db	"Press a key to continue..." 
	db	0Dh, 0Ah, 0

msg_creating:
	db	0Dh, 0Ah
	db	"Creating ",0
msg_disk_size:
	db	"000MB hard disk image... ",
	db	" "
	db	0

DskTypeNumStr:	
	db	"020031016020032064128256512"
	dw	0

RSymbols:
	db	"-\|/"

align 2

Cursor_Pos:
	dw	0

CWSector:
	dd	0

diskimage_size:
	dd	0

DiskType:
	db	0

img_file_name:  
	times	13 db 0

;no_name:
;	db 	'NO NAME    ', 0

align 2

HDFORMAT_SECBUFFER:
HDFORMAT_FATBUFFER:
HDFORMAT_ZERO_BUFF:
	times	512 db 0F6h

	db	0
	db	'(c) Erdogan TAN 2018'
	db	0

SizeOfFile equ $-100
