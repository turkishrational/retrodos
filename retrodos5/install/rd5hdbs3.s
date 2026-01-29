; ****************************************************************************
; RD5HDBS3.ASM - Retro DOS v5 FAT 32 (Hard Disk) FS BOOT SECTOR code
; ----------------------------------------------------------------------------
; Last Update: 29/01/2026
; ----------------------------------------------------------------------------
; Beginning: 27/04/2024
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
;	    ((nasm rd5hdbs3.s -l rd5hdbs3.txt -o RD5HDBS3.BIN))
; ----------------------------------------------------------------------------
; Modified from 'fat32_bs.s' TRDOS 386 v2 FAT32 hd boot sector source code
; by Erdogan Tan (31/01/2018).
; ----------------------------------------------------------------------------
; incbin "RD5HDBS3.BIN" (in 'rd5hdi3.s')

; fat32_bs.s
; ****************************************************************************
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; This boot sector code occupies 2 sectors.. as below:
; * Boot Sector 1:
; The 1st 512 bytes of this boot code will be on sector 0 (of the partition).
; * Boot Sector 2:
; Remain bytes of this boot code will be sector 2 (just after FSINFO sector)
; of the (FAT32) partition.
;
; NOTE:	This code has some tricks and TRDOS 386 specific modifications
; which are not a part of original Microsoft Windows (XP) FAT32 BS code.
; (Purpose of TRDOS 386 specific modifications and tricks is
; to load 'TRDOS386.SYS' kernel file as easy and as correct,
; without affecting FAT32 FS recognization for other operating systems.)
;
; Note: Modifications (on WINDOWS 98 FAT32 boot sector code)
;	are based on WINDOWS XP FAT32 boot sector(s) code (2 sectors),
;	which is disassembled by Erdogan Tan (12/12/2017)
;	by using BINFILEHEX (Erdogan Tan) & IDA PRO FREE (Hex-Rays SA)
;	programs.
;
; Derived from Microsoft WINDOWS 98 FAT 32 boot sector code
; 	  which is edited/disassembled by Erdogan Tan (04/10/2003).
;
; Derived from 'trfdbs.s' TRDOS 386 FAT12 (3.5") floppy disk boot sector
; source code by Erdogan Tan (06/07/2017).
; ****************************************************************************
; incbin "FAT32_BS.BIN" (in 'hdimage.s' & 'hdformat.s')

rts_segment     equ	1000h

[BITS 16]
[ORG 7C00h]
BS_jmpBoot:
                jmp     short loc_5A  ; jmp short start
BS_jmpBoot_nop:
                nop

; BootSector Identification (Data) Block
BS_OEMName:
		db	'RETRODOS'	; bp+3
BPB_BytesPerSec: dw	512		; bp+11
BPB_SecPerClus:	db	8		; bp+13
BPB_RsvdSecCnt:	dw	32		; bp+14
BPB_NumFATs:	db	2		; bp+16
BPB_RootEntCnt: dw	0		; bp+17
BPB_TotSec16:	dw	0		; bp+19
BPB_Media:	db	0F8h		; bp+21
BPB_FATSz16:	dw	0		; bp+22
BPB_SecPerTrk:	dw	63		; bp+24
BPB_NumHeads:	dw	255		; bp+26
BPB_HiddSec:	dd	1		; bp+28
BPB_TotSec32:	dd	0		; bp+32
BPB_FATSz32:	dd	0		; bp+36
BPB_ExtFlags:	dw	0		; bp+40
BPB_FSVer:	dw	0		; bp+42
BPB_RootClus:	dd	2		; bp+44
BPB_FSInfo:	dw	1		; bp+48
BPB_BkBootSec:	dw	6		; bp+50
BPB_Reserved:	times	12 db 0		; bp+52
BS_DrvNum:	db	80h		; bp+64
BS_Reserved1:	db	0		; bp+65
BS_BootSig:	db	29h		; bp+66
BS_VolID:	dd	0		; bp+67
BS_VolLab:	db	'NO NAME    '	; bp+71
BS_FilSysType:	db	'FAT32   '	; bp+82

start:
loc_5A:
		OR	AX, AX ; db 09h, C0h  (db 0Bh, C0h)
			 ; TRDOS 386 (FAT32 BS) LBA check trick!!

		mov	bp, 7C00h

		; ((WINDOWS XP FAT 32 boot sector code checks Masterboot
		; partition table for partition type, if it is 0Ch
		; -FAT32 LBA-, the boot code changes 90h at BS offset 2
		; to 0Eh. Then a 0Eh at this addr is used as identifier,
		; for reading disk sector by using INT 13h -LBA read-
		; extension.))

		cmp	ax, 417  ; If AX=417, the masterboot sector
				 ; has a SINGLIX FS (& TRDOS 386)
				 ; masterboot code; and...
				 ; DX=ES=SS=0, BP=7C00h
				 ; SP=7C00h ... masterboot sector has
				 ; been loaded at 0:600h, it has
				 ; CHS parameters at offset 600h+420.
				 ; (There is a 01A1h in offset 600h+417)

		je	short bs_01 ; no need to following assignments !

		xor	ax, ax
		mov	ds, ax
		mov	es, ax
		cli
		mov	ss, ax
		mov	sp, bp
		sti
		mov	dl, [bp+40h] ; [BS_DrvNum]
bs_01:
		mov	[bsReserved1], si ; Partition entry address..
		; 24/12/2017		  ; (from Singlix FS MBR)
					  ; (7BEh,7CEh,7DEh,7EEh)

		; Check Bytes/Sector value
		; It must be 512 !? (at least, for TRDOS386)
		cmp	word [bp+0Bh], 512 ; [BPB_BytesPerSec]
		jne	short invalid_system_disk

		; Following validation checks (*!*)
		; are done according to 
		; 	Microsoft Extensible Firmware Initiative
		;	FAT32 File System Specification, 
		;				Version 1.03 (2000).

                ; [BPB_FATSz16] must be 0 (!*!) ; [bp+16h]
		sub	cx, cx ; 0 ; * ; 27/04/2024
                ;cmp	[BPB_FATSz16], cx ; 0	; sectors/fat for FAT16
                cmp	[bp+16h], cx
		jne	short invalid_system_disk

		; [BPB_FSVer] must be 0 for current FAT32 version (!*!)
		;cmp	[BPB_FSVer], cx   ; 0 	; [bp+2Ah], FAT32 version
		cmp	[bp+2Ah], cx
		jne	short invalid_system_disk

		;mov	dh, [bp+5Ah]    ; 0Ch (FAT32 LBA) or 0Bh (FAT32 CHS)
			; NOTE: 09h at bp+5Ah (at loc_5A) will be overwritten
			; by TRDOS 386 disk format program.
			; (0BC0h -or AX,AX- is written for FAT32 CHS partition,
			; 0C00h -or AL,0- is written for FAT32 LBA partition.)

		; overwrite hd drive number !
                ;mov	[BS_DrvNum], dl		; drive number from INT 19h
		mov	[bp+40h], dl
		;mov	[bp+41h], dh	; [BS_Reserved1] = Partition ID !!!
		;mov	[bp+40h], dx

		; 27/04/2024
		; reset FAT32 FS reading pointers and set SP to 7BF4h
		;xor	cx, cx ; *
		;sub	cx, cx ; *
		push	cx
		push	cx	; [bp-4] = 0   ; CHS limit (8.4GB)
		push	cx
		push	cx	; [bp-8] = 0   ; Address of Cluster 2
		dec	cx 	; 0FFFFh
		push	cx
		push	cx	; [bp-12] = -1	; FAT sector in FAT buffer
		inc	cx	; 0

		; SP = 7BFAh ; 28/01/2026

		; check for ROMBIOS INT 13h extensions
		mov	ah, 41h
		mov	bx, 55AAh
		;mov	dl, [BS_DrvNum]
		;mov	dl, [bp+40h]
		int	13h
		jc	short bs_02
		cmp	bx, 0AA55h
		jne	short bs_02
		test	cl, 1
		jz	short bs_02

		; ROMBIOS INT 13h extentions are present...

		mov	ax, 4A42h
		;mov	[BS_jmpBoot+2], al ; 42h ; 'LBA mode is available'
		mov	[bp+2], al ; 42h
		;cmp	dh, 0Ch ; is this BS of a FAT32 LBA partition?
		cmp	byte [bp+5Ah], 0Ch
		jne	short bs_02 ; no..
		; yes, put sign for disk read subroutine (for LBA read)
		;mov	[loc_5A], ax
		mov	[bp+5Ah], ax ; INC DX (FAT32 LBA read), DEC DX
bs_02:
		; ..CHS limit setup..

		; Get drive parameters (CHS parameters)
		;mov	dl, [BS_DrvNum]
		;mov	dl, [bp+40h]
		mov	ah, 08h
		int	13h
		jc	short disk_io_error

		; CX = maximum value for cylinder and sector
		; DH = maximum value for head
		; DL = number of harddisks on first controller
		; ES:DI = address of hard disk parameters table
		; (Bits 6&7 of CL is high 2 bits of 10 bit clinder value
		; which is low 8 bits are in CH.)

		push	ds
		pop	es

		; 27/04/2024
		; convert CHS values to CHS limit (as LBA)
		xor	ax, ax
		mov	al, dh
		inc	ax
		mov	dl, cl
		;and	dl, 3Fh
		and	dx, 3Fh
		mul	dx
		; ax <= 16128
		;shr	cl, 6
		xchg	cl, ch
		and	ch, 0C0h
		rol	ch, 1
		rol	ch, 1
		inc	cx
		mul	cx
		mov	[bp-4], ax ; dword [7BFCh] ; CHS limit
		mov	[bp-2], dx

		; Load the second half (remain bytes) of this boot code
		; at 7E00h.

		; 27/04/2024
		mov	ax, [bp+1Ch] ; [BPB_HiddSec]
		mov	dx, [bp+1Eh]
		add	ax, 2 ; Second half of boot code is in BS 2
		adc	dx, 0
		mov	bx, 7E00h
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; 25/12/2017 (Read 1 sector)
		jc	short disk_io_error

		; Boot sector 2 validation check
		cmp	word [7FA1h], 417 ; The magic word !
		je	check_root_dir_entries
		; 27/04/2024
		;jne	short invalid_system_disk
		;jmp	check_root_dir_entries

invalid_system_disk:
		mov	si, Inv_disk_Msg
getchar_reboot:		; 27/04/2024
		call	print_msg
;getchar_reboot:
		; Wait for a keystroke just before reboot
		xor	ah, ah
		int	16h

		int	19h	; disk boot
				; causes reboot of disk system
disk_io_error:
		mov	si, Diskio_err_Msg
		call	print_msg
;replace_disk:
;		mov	si, Replace_Msg	
replace_disk:
		mov	si, Disk_err_replace_Msg
		; 27/04/2024
		;call	print_msg
		jmp	short getchar_reboot

print_msg:
		; DS:SI = Error message address (ASCIIZ string)
		mov	ah, 0Eh
		mov	bx, 7
bs_03:
		lodsb
		test	al, al
		jz	short bs_04
		int	10h
		jmp	short bs_03

		; 27/04/2024
;bs_04:
;		retn

read_sector:	; 25/12/2017 (Read 1 sector)
		mov	cx, 1
disk_read:
		;mov	byte [bp+retry_count-7C00h], 4
		mov	di, 4 ; retry count
disk_read_0:
		pusha
		;cmp	byte [loc_5A], 42h ; FAT32 LBA availability
		cmp	byte [bp+5Ah], 42h ; FAT32 LBA partition & LBA ready
		je	short lba_read
		; Jump to lba_read if sector addr overs CHS limit

		; 27/04/2024
		cmp	dx, [bp-2] ; hw of CHS limit
		ja	short disk_read_3
		jb	short chs_read
		cmp	ax, [bp-4] ; CHS limit ([7BFCh])
		jb	short chs_read
disk_read_3:
		; Disk I/O error if Int 13h LBA read func is not usable
		; byte [BS_jmpBoot+2] = 'LBA read function is ready' sign
		;cmp	byte [[BS_jmpBoot+2], 42h ; FAT32 LBA availability
		cmp	byte [bp+2], 42h ; is LBA mode ready ? 
		je	short lba_read ; LBA mode is usable/available
		stc ; cf = 1
bs_04:		;	 27/04/2024
		retn
lba_read:
		; 27/04/2024
		;pusha

		push	word 0
		push	word 0
		push	dx
		push	ax
		push	es
		push	bx
		push 	byte 1
		push	byte 16 ; 10h
		mov	ah, 42h
		;mov	dl, [BS_DrvNum]
		mov	dl, [bp+40h]
		mov	si, sp
		int	13h

		add	sp, 16
		jmp	short disk_read_1
chs_read:
		; 27/04/2024
		;pusha

		; Convert LBA to CHS
		push	ax ; lw of the LBA
		mov	ax, dx ; hw of the LBA
		xor	dx, dx
		mov	cx, [bp+18h]  ; [BPB_SecPerTrk] ; [bp+18h]
				; sectors per track (17 or 63)
		div	cx
		mov	si, ax	; hw of the quotient
		pop	ax
		div	cx
		; si:ax = (heads * cylinder) + head number
		; dl = sector (0 based)
		inc	dl	; sector number (1 based)
		mov	cl, dl
		mov	dx, si
		;div	word [BPB_NumHeads] ; [bp+1Ah]
				 ; number of heads (2 to 255)
		div	word [bp+1Ah]
		; AX = cylinder (0 to 1023)
		; DX = head number (in DL)
		mov	dh, dl	 ; head number in DH
		;mov	dl, [BS_DrvNum] ; [bp+40h] ; Drive number (80h)
		mov	dl, [bp+40h]
		mov	ch, al ; Low 8 bits of cylinder number (0 to 7)
		shl	ah, 6  ; High 2 bits of cylinder is in bit 7&8
		or	cl, ah ; High two bits of CL is cylinder bits 8&9
		mov	ax, 201h ; Read 1 sector
		int	13h
disk_read_1:
		; 27/04/2024
		popa
		jnc	short disk_read_2
		; cf = 1
		;dec	byte [retry_count]
		;dec	byte [bp+retry_count-7C00h]
		; 27/04/2024
		dec	di ; Retry count
		jnz	short disk_read_0 ; Retry
		; cf = 1
		retn
disk_read_2:
		;add	bx, [bp+0Bh] ; [BPB_BytesPerSec] ; 512
		;add	bx, 512
		add	bh, 2 ; **
		; 27/04/2024
		inc	ax
		jnz	short disk_read_4
		inc	dx
disk_read_4:
		dec	cx
		jnz	short disk_read
		;clc 	; ** (128 sectors/cluster!?)
		retn

		; Filler
		;db	07h
		;db	14h

Diskio_err_Msg:
		db	0Dh, 0Ah
		db	'Disk I/O error'
		;db	'!'
		db	0
Inv_disk_Msg:
		db	0Dh, 0Ah
		db	'Invalid system disk'
Disk_err_replace_Msg:
		db	'!'
Replace_Msg:
		db	0Dh, 0Ah
		db	'Replace the disk and press any key to reboot.'
		db	0Dh, 0Ah, 0

		; Boot sector code writing date (by Erdogan Tan)
		;;db	31
		;;db	01
		;;dw	2018
		;
		;db	27
		;db	04
		;dw	2024

		; TRDOS 386 FAT32 boot sector code version
		;db	'v1.0'
		; 27/04/2024
		; Retro DOS v5 FAT32 boot sector code
		;db	'v5.0'

		times	(508+rtsfilename-bsReserved1) - ($ - $$) db 0
rtsfilename:
		;db	'TRDOS386SYS'
		; 27/04/2024 - Retro DOS v5.0
		db	'PCDOS   SYS'
                db      0
bsReserved1:
		db	'TR'  ; 'Turkish Rational DOS' feature identifier.
bootsignature1:
		db	55h, 0AAh

bsReserved2:
		db	'RT'  ; 'Turkish Rational DOS' feature identifier

check_root_dir_entries:
		; load root directory and check directory entries

		; 28/04/2024
		; calculate total size of FAT area
                ;mov	cl, byte [BPB_NumFATs] ; [bp+10h] ; number of FATs
  		mov	cl, [bp+10h]
		;add	ax, [BPB_FATSz32] ; [bp+24h] ; sectors per FAT
                ;adc	dx, [BPB_FATSz32+2] ; [bp+26h]

		xor	ax, ax
		xor	dx, dx
crde_fat_secs:
		;add	ax, [BPB_FATSz32] ; [bp+24h] ; sectors per FAT
                ;adc	dx, [BPB_FATSz32+2] ; [bp+26h]
		add	ax, [bp+24h]
		adc	dx, [bp+26h]

		dec	cx
		jnz	short crde_fat_secs

		; add hidden sectors
		;add	ax, [BPB_HiddSec] ; [bp+1Ch]
		;adc	dx, [BPB_HiddSec+2] ; [bp+1Eh]
		add	ax, [bp+1Ch]
		adc	dx, [bp+1Eh]

		; add reserved sectors
                ;add	ax, [BPB_RsvdSecCnt] ; [bp+0Eh]
		add	ax, [bp+0Eh]
		;adc	dx, 0
		; 29/01/2026
		adc	dx, cx ; 0

		;mov	cl, [BPB_SecPerClus] ; [bp+13]
		mov	cl, [bp+0Dh]
		mov	[SecPerClus], cx

		; Save address of cluster 2 into 7BF8h 
		mov     [bp-8], ax	; DX:AX = Data Area (Cluster 2)
                                        ; (Data) Start Address in 7BF8h
		mov	[bp-6], dx

		; Reset FAT sector address pointer (7BF4h)
		; 	which points to FAT sectors in the FAT buffer
		;	(8000h)
		;
		;mov	dword [bp-12], 0FFFFFFFFh ; invalid address
						 ; (for now)

		; Check Root Directory Start Cluster is valid or not.
		;mov	ax, [BPB_RootClus] ; [bp+2Ch]
		;mov	dx, [BPB_RootClus+2] ; [bp+2Eh]
		mov	ax, [bp+2Ch]
		mov	dx, [bp+2Eh]

load_root_dir_sector:
		; 28/04/2024
		; DX:AX = Cluster Number

		; Cluster number must not be less than 2
		or	dx, dx
		jnz	short lrds_1
		cmp	ax, 2			; Is it Cluster 2?
		jb	invalid_system_disk	; error if less than 2
lrds_1:
               	; Is it End Of Cluster Chain marker or something above?
                ;cmp	dx, 0FFFh
		;jb	short lrds_2
		;cmp	ax, 0FFF8h	; clust 2 to 0FFFFFF7h is valid
                ;jnb	invalid_system_disk ; invalid cluster num
					      ; or end of cluster chain
		; 23/12/2017
		; Note: Bad Cluster number is 0FFFFFF7h !
		; (According to MS FAT32 Specification, 2000, page 18)
		; "It is not possible for the bad cluster mark to be
		; an allocatable cluster number on FAT12 and FAT16 volumes,
		; but it is feasible for 0x0FFFFFF7 to be an allocatable
		; cluster number on FAT32 volumes.
		; To avoid possible confusion by disk utilities,
		; no FAT32 volume should ever be configured such that
		; 0x0FFFFFF7 is an allocatable cluster number."

               	; Is it End Of Cluster Chain marker or something above?
                ; 28/04/2024
		cmp	dx, 0FFFh
		jb	short lrds_2
		cmp	ax, 0FFF7h	; clust 2 to 0FFFFFF6h is valid
                jnb	invalid_system_disk   ; invalid cluster num
					      ; or end of cluster chain
lrds_2:
		push	dx ; ++
		push	ax ; +
		
		; 29/01/2026
		;xor	bx, bx
		sub	ax, 2	; 0 based cluster number
		sbb	dx, 0
		;sbb	dx, bx
		push	dx ; hw
		;;mov	bl, [BPB_SecPerClus] ; [bp+13]
		;mov	bl, [bp+0Dh]
		;mov	si, bx	; save sector per cluster in SI
		; 29/01/2026
		mov	si, [SecPerClus]
		mul	si
		mov	bx, ax ; multiplication result lw
		pop	ax ; multiplicand hw
		push	dx ; carry
		mul	si
		pop	dx
		add	dx, ax	; multiplication result hw
		mov	ax, bx

		; DX:AX = relative sector of the 1st part of root dir
		add	ax, [bp-8]  ; add 'start address of data area'
				    ; (in 7BF8h) to relative address
		adc	dx, [bp-6]
bs_05:
		mov	bx, 8200h  ; Root directory buffer (1 sector)
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; 25/12/2017 (Read 1 sector)
		jc	disk_io_error

		; BX = 8400h

		; 29/04/2024
		mov	di, 8200h
search_startup_file: 
		; check/compare root dir entry for/with kernel file
		cmp	[di], ch ; 0
		je	invalid_system_disk ; kernel not found!
		mov	cl, 11 ; 0Bh
		; SI = count down value from 'sectors per cluster'
		push	si ; SPC to 1
		mov	si, rtsfilename ; Run Time System file name
					; or Kernel file name
					; (or Startup file name)
					; (or Standalone file name)
					; in MSDOS directory entry
					; format. ('TRDOS386SYS')
					; It is 'TRDOS386.SYS'
					; for TRDOS 386 OS.
		repe	cmpsb ; compare dir entry and kernel's name
		pop	si
		jz	load_startup_file ; kernel is there!
		add	di, cx
		add	di, 21 ; 15h (11+21=32)
		cmp	di, bx ; 8400h
		jb	short search_startup_file ; chk next entry
		; 28/04/2024
		; Sector Per Cluster countdown
		dec	si
		jnz	short bs_05 ; next sector in same cluster

		pop	ax ; +
		pop	dx ; ++

		call	get_next_cluster
		jc	disk_io_error

		;; DX:AX = 32 bit cluster number (32 bit FAT entry)
		;and	dx, 0FFFh ; 28 bit cluster number
		jmp	short load_root_dir_sector

get_next_cluster:	; get next (FAT32) directory/file cluster
		; EAX = current cluster number (28 bit, zero based)
		;shl	dx, 1
		;shl	ax, 1
		;adc	dx, 0
		;shl	dx, 1
		;shl	ax, 1 ; 32 bit FAT entry offset
		;adc	dx, 0
		; 27/01/2026
		shl	ax, 1
		rcl	dx, 1
		shl	ax, 1
		rcl	dx, 1

		call	get_fat32_entry
		jc	short bs_06 ; Disk read error!

		; 29/04/2024
		; ds = es = 0
		; si = cluster entry offset
		mov	ax, [si]	; 32 bit clust entry number
		mov	dx, [si+2]
		and	dx, 0FFFh	; 28 bit cluster number
bs_06:
		retn

get_fat32_entry:
		; DX:AX = 32 bit FAT entry (dword) offset
		;mov	cx, [BPB_BytesPerSec]
		;mov	cx, [bp+0Bh]
		mov	cx, 512
		push	ax  ; lw
		mov	ax, dx ; hw
		xor	dx, dx
		div	cx
		mov	bx, ax
		pop	ax  ; lw
		div	cx
		mov	si, dx	; offset in the buffer
		; BX:AX = FAT sector number (relative)
		; Check FAT sector number if it is already
		; in FAT buffer at 8000h.
		; Current FAT sector is in 7BF4h.
		; (Note: initial FAT sector value in 7BF4h is
		;	 0FFFFFFFFh which means the buff
		;	 is not loaded yet..)
		;cmp	bx, [bp-0Eh] ; [7BF6h]
		; 27/01/2026
		cmp	bx, [bp-0Ah] ; [7BF6h]
		jne	short g_fat32_e_1
		cmp	ax, [bp-0Ch] ; [7BF4h]
		je	short bs_08 ; same sector in FAT buffer
g_fat32_e_1:
		; 29/04/2024
		mov	[bp-0Ch], ax ; save FAT sector number
		;mov	[bp-0Eh], bx ; save FAT sector number
		; 27/01/2026
		mov	[bp-0Ah], bx ; save FAT sector number
		xor	dx, dx ; 0
		; Calculate absolute (LBA) address of FAT sector
		; by adding hidden (partition's start sector)
		; and reserved sectors (between BS and FAT).
		;add	ax, [BPB_HiddSec] ; [bp+1Ch]
		;adc	bx, [BPB_HiddSec+2]
		add	ax, [bp+1Ch]
		adc	bx, [bp+1Eh]
		;add	ax, [BPB_RsvdSecCnt] ; = 32 (typically)
		add	ax, [bp+0Eh]
		adc	dx, bx	; LBA of the FAT sector
		; Check FAT mirroring flag..
		;	bits 0 to 3 are used for active FAT number
		;	If bit 7 is 1, only one (active) FAT copy
		;	is updated (written without mirroring).
		;  (Default active FAT is 0 and default option
		;   is to mirror the active FAT -at runtime- into
		;	all FAT copies -generally 2 FATs-.
		;mov	bx, [BPB_ExtFlags]
		mov	bx, [bp+28h]
		and	bx, 0Fh
		jz	short bs_07	; FAT number 0 is active
					; (as default)
		; compare active FAT number with number of FATs
		;cmp	bl, [BPB_NumFATs] ; [bp+10h]
		cmp	bl, [bp+10h]
		jnb	short bs_09 ; invalid parameter!
		push	dx ; **
		push	ax ; *
		mov	cx, ax
		; multiply zero based FAT number with FAT size
		;mov	ax, [BPB_FATSz32] ; [bp+24h]
		mov	ax, [bp+24h]
		mul	bx
		mov	cx, ax ; multiplication result lw
		;mov	ax, [BPB_FATSz32+2] ; [bp+26h]
		mov	ax, [bp+26h]
		push	dx ; carry
		mul	bx
		pop	dx ; carry
		add	dx, ax ; multiplication result hw
		pop	ax ; *
		pop	bx ; **
		add	ax, cx   ; start of active FAT copy
		adc	dx, bx
bs_07:
		mov	bx, 8000h ; FAT (sector) buffer
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; 25/12/2017 (Read 1 sector)
		; If cf = 1 -> Disk I/O err, not invalid sys disk!
bs_08:
		add	si, 8000h ; add cluster offset to buffer addr
		retn
bs_09:
		; Invalid boot sector parameter/data
		;add	sp, 4
		pop	ax ; return to 'get_next_cluster'
		pop	ax ; return to 'search_startup_file'
		jmp	invalid_system_disk

load_startup_file:
		; DI = directory entry offset 9 (of 32 bytes)
		; 29/04/2024
		pop	ax ; +
		pop	ax ; ++ ; dx
		; High word of First Cluster
		mov	dx, [di+9] ; [di+DIR_FstClusHI-11]
		; Low word of First Cluster
		mov	ax, [di+0Fh] ; [di+DIR_FstClusLO-11]
		; Valid cluster number must not be less than 2
		; and it (a first cluster value in directory entry)
		; must not be greater than 0FFFFFF6h.
		and	dx, dx
		jnz	short lsf_1
		cmp	ax, 2
		jb	invalid_system_disk
lsf_1:		; 29/04/2024
		cmp	dx, 0FFFh
		jb	short lsf_2
		ja	invalid_system_disk
		cmp	ax, 0FFF7h ; 0FFFFFF7h ; 23/12/2017
		jnb	invalid_system_disk
lsf_2:
		push	ax ; save first cluster number (lw)

		; Load  RTS (Kernel) file
                mov	si, Loading_Msg
                call	print_msg

		pop	ax ; restore first cluster number (lw)

		;mov	bx, rts_segment ; 1000h
		;mov	[next_segment], bx
bs_10:
		; 29/04/2024
		; cx = 0 
		push	dx
		push	ax ; 28 bit cluster num, starts from 2
		sub	ax, 2 ; now, cluster num starts from 0
		sbb	dx, 0
		push	dx ; hw
		;;mov	cl, [BPB_SecPerClus] ; [bp+0Dh]
		;mov	cl, [bp+0Dh]
		; 29/01/2026
		mov	cx, [SecPerClus]
		mul	cx
		mov	bx, ax ; multiplication result lw
		pop	ax ; multiplicand hw
		push	dx ; carry
		mul	cx
		pop	dx
		add	dx, ax ; multiplication result hw
		mov	ax, bx
		; dx:ax = sector offset (from start of data area)
		add	ax, [bp-8] ; add data area (start) addr
		adc	dx, [bp-6]
		mov	bx, [next_segment]
		push	es
		mov	es, bx ; segment = 1000h +
		;
		; 28/01/2026
		;mov	bx, cx
		;shl	bx, 5 ; spc*32 ; number of paragraphs
		;add	[next_segment], bx
		;
		xor	bx, bx ; offset = 0
		; CX = num of sectors to read (= sectors/cluster)
		call	disk_read
		pop	es
		pop	ax
		pop	dx ; 28 bit cluster num, starts from 2
		;
		; 28/01/2026
		shr	bx, 4
		add	[next_segment], bx
		;
		call	get_next_cluster
		;jc	short diskio_error
		jc	short retrodos_loading_error

		; 29/04/2024
		or	dx, dx
		jz	short lsf_3
		cmp	dx, 0FFFh
		jb	short bs_10

		;cmp	ax, 0FFF8h
		cmp	ax, 0FFF7h ; 0FFFFFF7h ; 23/12/2017
		jnb	short bs_11 ; Startup file has been loaded.
lsf_3:
		cmp	ax, 2
		;jb	invalid_system_disk 
		;jmp	short bs_10 ; load next clust of the file
		jnb	short bs_10

retrodos_loading_error:
		mov	si, Load_err_Msg
		call	print_msg
		jmp	replace_disk

next_segment:
		dw	rts_segment

SecPerClus:	; 29/01/2026
loc_3A1:	dw	417

		;db	'TR-DOS' ; Filler
		; 29/04/2024
		db	'Retro DOS v5', 0

		; 29/04/2024
Loading_Msg:    db	0Dh, 0Ah
		;db	'Loading Kernel TRDOS386.SYS ...'
		db	'Loading Kernel PCDOS.SYS ...'
                db 	0Dh, 0Ah, 0
Load_err_Msg:
		db	0Dh, 0Ah
		;db	'TRDOS Loading Error'
		db	'PCDOS Kernel Loading Error' ; 29/04/2024
		;db	'!'
		db	0

bs_11:
; 29/04/2024
%if 0
		; Set TRDOS 386 kernel specific parameters (& signs)
		; and
		; Launch TRDOS 386 Kernel (Startup/RTS file)

		;mov	dl, [BS_DrvNum]
                mov	dx, [bp+40h] ; DL = Drive number, DH = 0
		inc	dh  ; TRDOS 386 FAT32 BS major version = 1

		mov	ax, [next_segment] ; 16 paragraphs after the
					  ; start of the last segment
					  ; of the kernel file loading
					  ; space.
					  ; So, (top of) stack will have
					  ; 256 bytes or more distance
					  ; from the last byte
					  ; of the kernel file.
					  ; (This will be enough for
					  ; TRDOS 386 kernel before
					  ; entering protected mode.)
		cli
		mov	ss, ax
		mov	sp, 0FFFEh
		sti

		mov     bx, rts_segment ; 1000h
                mov     ds, bx
                mov     es, bx
		;mov	fs, bx
		;mov	gs, bx

		;xor	ebx, ebx
		;xor	ecx, ecx
		;xor	edx, edx
		;xor	esi, esi
		;xor	edi, edi
		;xor	ebp, ebp

		; bp = 7C00h

		; NOTE: Offset 417 in boot sector 2 (the 2nd half of VBR)
		; is also FAT32 boot record validation check address and
		; boot sector 2 must have 417 here for boot sector 1 code
		; (the 1st half of volume boot record).
		; ((So, 'mov eax, 417' has double meaning here.))
loc_39F:
		mov	eax, 417 ; TRDOS boot sector sign for TRDOS386.SYS

		jmp	rts_segment:0

%else
		; 29/04/2024

		; 10/12/2023 - Retro DOS v5.0 Kernel INIT (retrodos5.s)
		; ------------------------------------------------------------
		; INPUT (registers from Retro DOS v4-v5 boot sector):
		;  DL = [bsDriveNumber]
		;  DH = [bsMedia]
		;  SS = 0
		;  BP = 7C00h (boot sector address)
		;
		; If the boot drive is a CD (CDROM) or DVD
		;    and CD boot option is enabled/requested:
		;    AX = 'CD'
		; If the boot drive is a FD or HD 
		;    or CD boot option is not enabled/requested:
		;    AX <> 'CD'

		; 29/04/2024
		;mov	dl, [BS_DrvNum]
                mov	dl, [bp+40h] ; DL = Drive number
		;mov	dh, [BPB_Media]
		mov	dh, [bp+15h] ; DH = 0F8h
		xor	ax, ax
		; ax = 0 (means 32 bit total sectors, big file system)
		;	((only requirement here is AX must not be 'CD'))
		; bp = 7C00h (but, not used by RETRODOS v5 kernel INIT)

		jmp	rts_segment:0
%endif

		times	1020 - ($ - $$) db 0
bsReserved3:
		db	'TR'	; 'Turkish Rational DOS' feature identifier
bootsignature2:
		db	55h, 0AAh