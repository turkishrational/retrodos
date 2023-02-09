; ****************************************************************************
; 'fat12hdi.s' - Retro DOS v2/v3 Hard Disk Image (FAT12 FS) Formatting Utility
; ----------------------------------------------------------------------------
; 'rdhdimg.s' (RDHDIMG.COM) - Retro DOS	v2 Hard Disk Image Formatting Utility
; 					 	          (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 19/10/2018
; ----------------------------------------------------------------------------
; Beginning: 07/05/2018
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11
; ----------------------------------------------------------------------------
; Derived from 'hdimage.s'(HDIMAGE.COM) source code by Erdogan Tan
; (03/02/2018) - TRDOS 386 hard disk image formatting utility -
; ****************************************************************************
; nasm fat12hdi.s -l fat12hdi.lst -o FAT12HDI.COM

bsOemName	equ RETRODOS_FAT12_hd_bs + 3
bsBytesPerSec	equ RETRODOS_FAT12_hd_bs + 11
bsSecPerClust	equ RETRODOS_FAT12_hd_bs + 13
bsResSectors	equ RETRODOS_FAT12_hd_bs + 14
bsFATs		equ RETRODOS_FAT12_hd_bs + 16	
bsRootDirEnts	equ RETRODOS_FAT12_hd_bs + 17
bsSectors	equ RETRODOS_FAT12_hd_bs + 19
bsMedia		equ RETRODOS_FAT12_hd_bs + 21
bsFATsecs	equ RETRODOS_FAT12_hd_bs + 22
bsSecPerTrk	equ RETRODOS_FAT12_hd_bs + 24
bsHeads		equ RETRODOS_FAT12_hd_bs + 26
bsHidden1	equ RETRODOS_FAT12_hd_bs + 28
bsHidden2	equ RETRODOS_FAT12_hd_bs + 30	
bsHugeSectors	equ RETRODOS_FAT12_hd_bs + 32
bsDriveNumber   equ RETRODOS_FAT12_hd_bs + 36
bsReserved1	equ RETRODOS_FAT12_hd_bs + 37
bsBpbSignature	equ RETRODOS_FAT12_hd_bs + 38
bsVolumeID      equ RETRODOS_FAT12_hd_bs + 39
bsVolumeLabel   equ RETRODOS_FAT12_hd_bs + 43
bsFileSysType	equ RETRODOS_FAT12_hd_bs + 54
bsReserved2	equ RETRODOS_FAT12_hd_bs + 62
bsDataStart	equ RETRODOS_FAT12_hd_bs + 64	
bsRootDirStart	equ RETRODOS_FAT12_hd_bs + 66
bsRootDirSects	equ RETRODOS_FAT12_hd_bs + 68

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
	jnz	R_17     
	mov	si, DTA_FileSize
	lodsw
	mov	dx, [si]
	cmp	dx, 1F8h
	ja	R_17
	test	ax, 511
	jnz	R_17
	shr	ax, 9
	shl	dx, 7
	add	ax, dx

	; max. size of Retro DOS v2 hard disk image < 32MB
	cmp	ax, 64512 ; 64*16*63 sectors
	ja	short R_17
	je	short R_12
	cmp	ax, 20160 ; 20*16*63 sectors
	je	short R_12
	jb	short R_17
	cmp	ax, 20808 ; 306*4*17 sectors
	jb	short R_17
	je	short R_12
	cmp	ax, 41616 ; 306*8*17 sectors
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
	cmp	al, '7'
	ja	short R_20
	; 19/10/2018
	sub	al, '0'
	jmp	short R_22
R_21:
	cmp	al, 27 ;  ESCape key
	jne	short R_20

	; Exit
	mov	ax, 4C00h
	int	21h

R_22:
	mov	[DiskType], al ; 19/10/2018

	; clear screen
	mov	ax, 3 ; set video mode to 03h (80x25 text)
	int	10h

	mov	bl, [DiskType]	; 1 to 7
	dec	bl 		; 0 to 6
	;xor	bh, bh
	mov	si, bx
	shl	bl, 1
	add	bx, DskTypeNumStr
	mov	ax, [bx]
	mov	[msg_disk_size], ax
	shl	si, 3 ; * 8
	add	si, DskTypeParms
	lodsw
	mov	[pt_cylinders], ax
	lodsw
	mov	[pt_heads], ax
	lodsw
	mov	[pt_sectors], ax
	lodsw	
	mov	[diskimage_size], ax
	
	mov	si, msg_creating
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
	mov	dx, HDFORMAT_SECBUFFER  ; 0F6h buffer
R_25:
	mov	cx, 512
	mov	bx, [img_file_handle]
	mov	ah, 40h ; write to file	
	int	21h
	jc	short R_26

	inc	word [CWSector]
	mov	ax, [diskimage_size]

	mov	cx, 1

	cmp	ax, [CWSector]
	jna	short R_27
	
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
	cmp	al, '6'
	ja	short R_31

	xor	ch, ch
	sub	al, '0'
	mov	cl, al
	dec	cl  ; 0 to 5
	mov	bl, [DiskType]
	dec	bl  ; 0 to 6
	mov	al, 6
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
	mov	ah, 18
	mul	ah
	mov	si, partition_parms
	add	si, ax
	mov	bx, partition_table
	lodsw
	mov	[bsSecPerClust], al  ; sectors per cluster
	lodsw
	;mov	cx, ax  	     ; number of clusters
	lodsw
	mov	[bsRootDirEnts], ax  ; root directory entries
	add	ax,15
	shr	ax, 4
	mov	[bsRootDirSects], ax ; root directory sectors	
	lodsw
	mov	[bsHidden1], ax	     ; hidden sectors
	mov	[bx+ptStartSector], ax ; start sector
	;mov	word [bx+ptStartSector+2], 0

	;mov	word [bsHidden2], 0
	lodsw
	;mov	[bsResSectors], ax   ; reserved sectors = 1
	lodsw
	mov	[bsFATsecs], ax      ; fat sectors
	lodsw
	mov	[bsRootDirStart], ax ; root directory address
	lodsw
	mov	[bsDataStart], ax    ; first data sector address
	lodsw
	mov	[bsSectors], ax      ; partition/volume size in sectors
	mov	[bsHugeSectors], ax 
	;mov	word [bsHugeSectors+2], 0
	mov	[bx+ptSectors], ax
	;mov	word [bx+ptSectors+2], 0

	mov	cx, ax 		     ; volume/partition size in sectors

	mov	byte [bx], 80h       ; Active/Bootable partition
	mov	al, 1
	mov	[bx+ptFileSystemID], al ; 1
	mov	[bx+ptBeginHead], al ; beginning head = 1
	mov	[bx+ptBeginSector], al ; beginning sector = 1
	xor	ah, ah	
	;mov	[bx+ptBeginCylinder], ah ; beginning cylinder = 0
		
	mov	al, [pt_sectors]
	mov	[bsSecPerTrk], al
	;mov	byte [bsSecPerTrk+1], 0
	mov	[bx+ptEndSector], al	
	add	cx, ax		     ; + sectors per track      		           

	mov	ah, [pt_heads]
	mov	[bsHeads], ah
	;mov	byte [bsHeads+1], 0
	dec	ah
	mov	[bx+ptEndHead], ah
	inc	ah
	mul	ah
	xchg	cx, ax
	xor	dx, dx
	div	cx
	dec	ax  ; the last cylinder of the partition (< 1024) 
	mov	[bx+ptEndCylinder], al

	; 19/10/2018
	and	ah, ah
	jz	short R_35  ; last cylinder number is less than 256

	shl	ah, 6 ; shift cylinder (AX) bits 8 & 9 to AH bits 6 & 7
	or	byte [bx+ptEndSector],ah ; .. and put it in EndSector bits 6 & 7

R_35:
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
	mov	dx, RETRODOS_FAT12_hd_bs
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
	mov	[SI+2], ah
	
	sub	al, al
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
	mov	[SI+2], ah
R_36:
	sub	al, al
	;mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit

	dec	bp
	jnz	short R_36

	mov	ax, 0FFF8h
	;mov	si, HDFORMAT_FATBUFFER
	mov	[SI], ax
	mov	[SI+2], ah

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
	mov	[SI+2], ah
R_37:
	sub	al, al
	;mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit
	
	dec	di
	jnz	short R_37

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
R_38:
	sub	al, al
	;mov	bx, [img_file_handle]
	;mov	dx, HDFORMAT_ZERO_BUFF
	;mov	cx, 512
	mov	ah, 40h ; write to file	
	int	21h
	jc	R_26 ; write error message then exit
	
	dec	di
	jnz	short R_38

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
;	lodsb
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
hed1:	dw 4
spt1:	dw 17
cap1:	dw 306*4*17
DskType2Parms:
cyl2:	dw 306
hed2:	dw 8
spt2:	dw 17
cap2:	dw 306*8*17
DskType3Parms:
cyl3:	dw 462
hed3:	dw 8
spt3:	dw 17
cap3:	dw 462*8*17
DskType4Parms:
cyl4:	dw 20
hed4:	dw 16
spt4:	dw 63
cap4:	dw 20*16*63
DskType5Parms:
cyl5:	dw 32
hed5:	dw 16
spt5:	dw 63
cap5:	dw 32*16*63
DskType6Parms:
cyl6:	dw 40
hed6:	dw 16
spt6:	dw 63
cap6:	dw 40*16*63
DskType7Parms:
cyl7:	dw 64
hed7:	dw 16
spt7:	dw 63
cap7:	dw 64*16*63

partition_parms:

Partition_4MB_17: ; 1
secpc0a: dw 4
clsts0a: dw 2028
roote0a: dw 256
hidds0a: dw 17
ressc0a: dw 1
fatsc0a: dw 6
rootd0a: dw 13
fdats0a: dw 29
parts0a: dw (60*8*17)-17 ; 8143 (2 sectors free)

Partition_4MB_63: ; 2
secpc0b: dw 4
clsts0b: dw 1993
roote0b: dw 256
hidds0b: dw 63
ressc0b: dw 1
fatsc0b: dw 6
rootd0b: dw 13
fdats0b: dw 29
parts0b: dw (8*16*63)-63 ; 8001 (0 sector free)

Partition_8MB_17: ; 3
secpc1a: dw 8
clsts1a: dw 2032
roote1a: dw 512
hidds1a: dw 17
ressc1a: dw 1
fatsc1a: dw 6
rootd1a: dw 13
fdats1a: dw 45
parts1a: dw (120*8*17)-17 ; 16303 (2 sectors free)

Partition_8MB_63: ; 4
secpc1b: dw 8
clsts1b: dw 2002
roote1b: dw 512
hidds1b: dw 63
ressc1b: dw 1
fatsc1b: dw 6
rootd1b: dw 13
fdats1b: dw 45
parts1b: dw (16*16*63)-63 ; 16065 (4 sectors free)

Partition_10MB_17: ; 5
secpc2a: dw 8
clsts2a: dw 2592
roote2a: dw 512
hidds2a: dw 17
ressc2a: dw 1
fatsc2a: dw 8
rootd2a: dw 17
fdats2a: dw 49
parts2a: dw (306*4*17)-17 ; 20791 (6 sectors free)

Partition_10MB_63: ; 6
secpc2b: dw 8
clsts2b: dw 2506
roote2b: dw 512
hidds2b: dw 63
ressc2b: dw 1
fatsc2b: dw 8
rootd2b: dw 17
fdats2b: dw 49
parts2b: dw (20*16*63)-63 ; 20097 (0 sector free)

Partition_16MB_17: ; 7
secpc3a: dw 8
clsts3a: dw 4070
roote3a: dw 512
hidds3a: dw 17
ressc3a: dw 1
fatsc3a: dw 12
rootd3a: dw 25
fdats3a: dw 57
parts3a: dw (240*8*17)-17 ; 32623 (6 sectors free)

Partition_16MB_63: ; 8
secpc3b: dw 8
clsts3b: dw 4017
roote3b: dw 512
hidds3b: dw 63
ressc3b: dw 1
fatsc3b: dw 12
rootd3b: dw 25
fdats3b: dw 57
parts3b: dw (32*16*63)-63 ; 32193 (0 sector free)					

Partition_20MB_17: ; 9
secpc4a: dw 16
clsts4a: dw 2594
roote4a: dw 1024
hidds4a: dw 17
ressc4a: dw 1
fatsc4a: dw 8
rootd4a: dw 17
fdats4a: dw 81
parts4a: dw (306*8*17)-17 ; 41599 (14 sectors free)

Partition_20MB_63: ; 10	
secpc4b: dw 16
clsts4b: dw 2511
roote4b: dw 1024
hidds4b: dw 63
ressc4b: dw 1
fatsc4b: dw 8
rootd4b: dw 17
fdats4b: dw 81
parts4b: dw (40*16*63)-63 ; 40257 (0 sector free)

Partition_31MB:    ; 11
secpc5a: dw 16
clsts5a: dw 3920
roote5a: dw 1024
hidds5a: dw 17
ressc5a: dw 1
fatsc5a: dw 12
rootd5a: dw 25
fdats5a: dw 89
parts5a: dw (462*8*17)-17 ; 62815 (6 sectors free)

Partition_32MB:	  ;  12
secpc5b: dw 16
clsts5b: dw 4022
roote5b: dw 1024
hidds5b: dw 63
ressc5b: dw 1
fatsc5b: dw 12
rootd5b: dw 25
fdats5b: dw 89
parts5b: dw (64*16*63)-63 ; 64449 (8 sectors free)


OptionValidatingTable:
; 	Option  4MB,8MB,10M,16M,20M,WHOLE DISK	
	db	 1,  0,  5,  0,  0,  5   ; 10MB, 17SPT	 	
	db	 1,  3,  5,  0,  9,  9	 ; 20MB, 17SPT
	db	 1,  3,  5,  7,  0, 11   ; 31MB, 17SPT	 	
	db	 2,  0,  6,  0,  0,  6   ; 10MB, 63SPT	 	
	db	 2,  4,  0,  8,  0,  8	 ; 16MB, 63SPT
	db	 2,  4,  6,  0, 10, 10	 ; 20MB, 63SPT
	db	 2,  4,  6,  8,  0, 12   ; 32MB, 63SPT

	db	0

RETRODOS_MASTERBOOT_SECTOR:
	incbin	'FS1_MBR.BIN' ; Singlix FS1 MBR	

RETRODOS_FAT12_hd_bs: 
	incbin	'RD2HDBS.BIN'

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
db  "RETRO DOS v2.0 Hard Disk Image Utility by Erdogan Tan (2018)                    "
db  "                                                                                "
db  "                                                                                "
db  "Disk Type     Cylinders     Heads     Sectors       Capacity                    "
db  "---------     ---------     -----     -------       --------                    "
db  "     1           306          4         17            10 MB                     "
db  "     2           306          8         17            20 MB                     "
db  "     3           462          8         17            31 MB                     " 	      	     	
db  "                                                                                "
db  "     4            20         16         63            10 MB                     "
db  "     5            32         16         63            16 MB                     "   	
db  "     6            40         16         63            20 MB                     "
db  "     7            64         16         63            32 MB                     " 	 
db  "                                                                                "
db  "                                                                                "
db  "Press disk number (1 to 7) to SELECT or                                         "
db  "                                                                                " 
db  "Press ESC to cancel.                                                            "
db  "                                                                                " 
db  0

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Retro DOS v3.0 Fixed Disk Image (FAT12 FS) Format Utility'
	db	0Dh, 0Ah
	db	"v1.0.191018  (c) Erdogan TAN 2018"
	db	0Dh,0Ah
	db	0Dh,0Ah
	db	'Usage: fat12hdi <image file name> '
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
	db	"  1) Use  4 MB disk space for FAT12 partition ", 0Dh, 0Ah
	db	"  2) Use  8 MB disk space for FAT12 partition ", 0Dh, 0Ah
	db	"  3) Use 10 MB disk space for FAT12 partition ", 0Dh, 0Ah
	db	"  4) Use 16 MB disk space for FAT12 partition ", 0Dh, 0Ah
	db	"  5) Use 20 MB disk space for FAT12 partition ", 0Dh, 0Ah
	db	"  6) Use whole disk space for FAT12 partition ", 0Dh, 0Ah
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
	db	"Creating "
msg_disk_size:
	db	"10MB hard disk image... ",
	db	" "
	db	0

DskTypeNumStr:	
	db	"10203110162032"
	dw	0

RSymbols:
	db	"-\|/"

align 2

Cursor_Pos:
	dw	0

CWSector:
	dw	0

diskimage_size:
	dw	0

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
