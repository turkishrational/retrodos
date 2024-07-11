; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.7 - fat32_bs.s - FAT32 BOOT SECTOR
; ----------------------------------------------------------------------------
; Last Update: 27/04/2024 (Previous: 31/01/2018)
; ----------------------------------------------------------------------------
; Beginning: 13/12/2017
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm fat32_bs.s -l fat32_bs.lst -o FAT32_BS.BIN)) 	
; ----------------------------------------------------------------------------
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
		db	'MSWIN4.1'	; bp+3
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
BS_VolLab:	db	'TRDOS386   '	; bp+71
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
		sub	ecx, ecx ; 0 ; *
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

		; reset FAT32 FS reading pointers and set SP to 7BF4h
		;xor	ecx, ecx ; *
		;sub	ecx, ecx ; *
		push	ecx 	; [bp-4] = 0   ; CHS limit (8.4GB)
		push	ecx	; [bp-8] = 0   ; Address of Cluster 2
		dec	ecx 	; 0FFFFFFFFh
		push	ecx	; [bp-12] = -1	; FAT sector in FAT buffer
		inc	ecx	; 0

		; SP = 7BF4h

		; check for ROMBIOS INT 13h extensions
		mov	ah, 41h
		;mov	ebx, 55AAh  
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

		; convert CHS values to CHS limit (as LBA)
		movzx	eax, dh
		inc	ax
		;movzx	edx, cl
		mov	dl, cl
		;and	dl, 3Fh
		and	dx, 3Fh
		mul	dx
		shr	cl, 6
		xchg	cl, ch
		inc	cx
		;movzx	ecx, cx ; *
		mul	ecx
		mov	[bp-4], eax ; dword [7BFCh] ; CHS limit

		; Load the second half (remain bytes) of this boot code
		; at 7E00h.

		mov	eax, [bp+1Ch] ; [BPB_HiddSec]
		add	eax, 2 ; Second half of boot code is in BS 2
		;mov	ebx, 7E00h
		mov	bx, 7E00h
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; 25/12/2017 (Read 1 sector)
		jc	short disk_io_error

		; Boot sector 2 validation check 
		cmp	word [7FA1h], 417 ; The magic word !
		je	check_root_dir_entries

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
		;call	print_msg
		;jmp	getchar_reboot
		; 27/04/2024
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
;bs_04:
		;retn

read_sector:	; 25/12/2017 (Read 1 sector)
		mov	cx, 1
disk_read:
		;mov	byte [bp+retry_count-7C00h], 4
		mov	dl, 4 ; retry count
disk_read_0:
		pushad
		;cmp	byte [loc_5A], 42h ; FAT32 LBA availability
		mov	dl, 42h
		cmp	[bp+5Ah], dl ; 42h ; FAT32 LBA partition & LBA ready
		je	short lba_read
		; Jump to lba_read if sector addr overs CHS limit
		cmp	eax, [bp-4] ; CHS limit ([7BFCh])
		jb	short chs_read
		; Disk I/O error if Int 13h LBA read func is not usable
		; byte [BS_jmpBoot+2] = 'LBA read function is ready' sign 
		;cmp	byte [[BS_jmpBoot+2], 42h ; FAT32 LBA availability
		;cmp	byte [bp+2], 42h
		cmp	[bp+2], dl ; 42h ; is LBA mode ready ? 
		je	short lba_read ; LBA mode is usable/available
		stc ; cf = 1
		; 27/04/2024
bs_04:
		retn

lba_read:
		;pushad

		;mov	di, sp
		
		push	dword 0
		push	eax
		push	es
		push	bx
		push 	byte 1
		push	byte 16 ; 10h
		;mov	ah, 42h
		mov	ah, dl ; 42h
		;mov	dl, [BS_DrvNum]
		mov	dl, [bp+40h]
		mov	si, sp
		int	13h

		;pop	eax
		;pop	eax
		;pop	eax
		;pop	eax
		;mov	sp, di
		
		popa
		jmp	short disk_read_1
chs_read:	
		;pushad

		; Convert LBA to CHS
		xor	edx, edx
		;movzx	ecx, word [BPB_SecPerTrk] ; [bp+18h]
				; sectors per track (17 or 63)
		movzx	ecx, word [bp+18h]
		div	ecx
		inc	dl	; sector number (1 based)
		mov	cl, dl
		mov	edx, eax ; (heads * cylinder) + head number
		shr	edx, 16	 ; high word in DX, low word in AX	
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
		popad
		jnc	short disk_read_2
		; cf = 1
		;dec	byte [retry_count]
		;dec	byte [bp+retry_count-7C00h]
		dec	dl ; Retry count
		jnz	short disk_read_0 ; Retry
		; cf = 1		
		retn
disk_read_2:
		;add	bx, [bp+0Bh] ; [BPB_BytesPerSec] ; 512
		;add	bx, 512
		add	bh, 2 ; **
		inc	eax
		dec	cx
		jnz	short disk_read
		;clc 	; ** (128 sectors/cluster!?)
		retn

		; 27/04/2024
		; Filler
		dd	0

		; Filler
		db	07h
		db	14h

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
		;db	31
		;db	01
		;dw	2018
		db	27
		db	04
		dw	2024

		; TRDOS 386 FAT32 boot sector code version
		;db	'v1.0'
		; 27/04/2024
		db	'v2.0'
		
		times	(508+rtsfilename-bsReserved1) - ($ - $$) db 0
rtsfilename:
                db      'TRDOS386SYS'
                db      0
bsReserved1:
		db	'TR'  ; 'Turkish Rational DOS' feature identifier.
bootsignature1:
		db	55h, 0AAh

bsReserved2:
		db	'RT'  ; 'Turkish Rational DOS' feature identifier

check_root_dir_entries: 
		; load root directory and check directory entries 
				
		; calculate total size of FAT area
                ;movzx	ecx, byte [BPB_NumFATs] ; [bp+10h] ; number of FATs
  		mov	cl, [bp+10h]
		;mov	eax, [BPB_FATSz32] ; [bp+24h] ; sectors per FAT
                mov	eax, [bp+24h]
		mul     ecx
                             
		; add hidden sectors
		;add	eax, [BPB_HiddSec] ; [bp+1Ch]
		add	eax, [bp+1Ch]

		; add reserved sectors
                ;movzx	edx, [BPB_RsvdSecCnt] ; [bp+0Eh]
                ;mov	dx, [BPB_RsvdSecCnt]
		mov	dx, [bp+0Eh]
		add	eax, edx

		; Save address of cluster 2 into 7BF8h 
		mov     [bp-8], eax	; EAX = Data Area (Cluster 2)
                                        ; (Data) Start Address in 7BF8h

		; Reset FAT sector address pointer (7BF4h) 
		; 	which points to FAT sectors in the FAT buffer
		;	(8000h) 
		;	
		;mov	dword [bp-12], 0FFFFFFFFh ; invalid address 
						 ; (for now)

		; Check Root Directory Start Cluster is valid or not.
		;mov	eax, [BPB_RootClus] ; [bp+2Ch]
		mov	eax, [bp+2Ch]

load_root_dir_sector:
		; EAX = Cluster Number

		; Cluster number must not be less than 2
		cmp	eax, 2			  ; Is it Cluster 2?
		jb	invalid_system_disk ; error if less than 2

               	; Is it End Of Cluster Chain marker or something above?
                ;cmp	eax, 0FFFFFF8h  ; clust 2 to 0FFFFFF7h is valid
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
                cmp	eax, 0FFFFFF7h  ; clust 2 to 0FFFFFF6h is valid
                jnb	invalid_system_disk   ; invalid cluster num
					      ; or end of cluster chain	
		push	eax
		sub	eax, 2	; 0 based cluster number
		;movzx	ebx, byte [BPB_SecPerClus] ; [bp+13]
		movzx	ebx, byte [bp+0Dh]
		mov	si, bx	; save sector per cluster in SI
		mul	ebx
		; EAX = relative sector of the 1st part of root dir 
		add	eax, [bp-8] ; add 'start address of data area'
				    ; (in 7BF8h) to relative address
bs_05:
		mov	bx, 8200h  ; Root directory buffer (1 sector)		
		mov	di, bx
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; 25/12/2017 (Read 1 sector)
		jc	disk_io_error

		; BX = 8400h

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
		; Sector Per Cluster countdown
		dec	si 
		jnz	short bs_05 ; next sector in same cluster
		pop	eax
		call	get_next_cluster
		jc	disk_io_error
		;; EAX = 32 bit cluster number (32 bit FAT entry)
		;and	eax, 0FFFFFFFh ; 28 bit cluster number
		jmp	short load_root_dir_sector

get_next_cluster:	; get next (FAT32) directory/file cluster
		; EAX = current cluster number (28 bit, zero based)
		shl	eax, 2 ; 32 bit FAT entry offset
		call	get_fat32_entry
		jc	short bs_06 ; Disk read error!
		mov	eax, [es:bx+di]	; 32 bit clust entry number
		and	eax, 0FFFFFFFh	; 28 bit cluster number
		;cmp	eax, 0FFFFFF8h
		;cmp	eax, 0FFFFFF7h  ; 23/12/2017
		;cmc
bs_06:
		retn

get_fat32_entry:
		; EAX = 32 bit FAT entry (dword) offset
		mov	di, 8000h ; FAT (sector) buffer
		;movzx	ecx, word [BPB_BytesPerSec] ; [bp+11]
		;mov	cx, [BPB_BytesPerSec]
		;mov	cx, [bp+0Bh]
		mov	cx, 512
		;xor	edx, edx
		xor	dx, dx
		div	ecx
		; EAX = FAT sector number (relative)
		; Check FAT sector number if it is already
		; in FAT buffer at 8000h.
		; Current FAT sector is in 7BF4h. 
		; (Note: initial FAT sector value in 7BF4h is
		;	 0FFFFFFFFh which means the buff 
		;	 is not loaded yet..)
		cmp	eax, [bp-0Ch] ; [7BF4h]
		je	short bs_08 ; same sector in FAT buffer
		mov	[bp-0Ch], eax ; save FAT sector number
		; Calculate absolute (LBA) address of FAT sector
		; by adding hidden (partition's start sector)
		; and reserved sectors (between BS and FAT).
		;add	eax, [BPB_HiddSec] ; [bp+1Ch]
		add	eax, [bp+1Ch]
		;movzx	ecx, word [BPB_RsvdSecCnt] ; [bp+0Eh]
		;mov	cx, [BPB_RsvdSecCnt] ; = 32 (typically)
		mov	cx, [bp+0Eh]
		add	eax, ecx ; LBA of the FAT sector
		; Check FAT mirroring flag..
		;	bits 0 to 3 are used for active FAT number 
		;	If bit 7 is 1, only one (active) FAT copy
		;	is updated (written without mirroring). 
		;  (Default active FAT is 0 and default option
		;   is to mirror the active FAT -at runtime- into
		;	all FAT copies -generally 2 FATs-. 	
		;movzx	ebx, word [BPB_ExtFlags] ; [bp+28h]
		;mov	bx, [BPB_ExtFlags]
		mov	bx, [bp+28h]
		and	bx, 0Fh
		jz	short bs_07	; FAT number 0 is active
					; (as default)
		; compare active FAT number with number of FATs	
		;cmp	bl, [BPB_NumFATs] ; [bp+10h]
		cmp	bl, [bp+10h]
		jnb	short bs_09 ; invalid parameter!
		push	dx
		mov	ecx, eax
		; multiply zero based FAT number with FAT size
		;mov	eax, [BPB_FATSz32] ; [bp+24h]
		mov	eax, [bp+24h]
		mul	ebx
		add	eax, ecx   ; start of active FAT copy
		pop	dx
bs_07:
		push	dx
		mov	bx, di
		;mov	cx, 1
		;call	disk_read
		call	read_sector ; 25/12/2017 (Read 1 sector)
		; If cf = 1 -> Disk I/O err, not invalid sys disk!
		pop	dx
bs_08:
		mov	bx, dx
		retn
bs_09:
		; Invalid boot sector parameter/data
		;add	sp, 4
		pop	ax ; return to 'get_next_cluster'
		pop	ax ; return to 'search_startup_file'
		jmp	invalid_system_disk

load_startup_file:
		; DI = directory entry offset 9 (of 32 bytes)
		add	sp, 4 ; pop eax
		; High word of First Cluster
		mov	ax, [di+9] ; [di+DIR_FstClusHI-11]
		; Low word of First Cluster
		mov	dx, [di+0Fh] ; [di+DIR_FstClusLO-11]
		shl	eax, 16
		mov	ax, dx
		; Valid cluster number must not be less than 2
		; and it (a first cluster value in directory entry)
		; must not be greater than 0FFFFFF6h. 
		cmp	eax, 2
		jb	invalid_system_disk
		;cmp	eax, 0FFFFFF8h
		cmp	eax, 0FFFFFF7h ; 23/12/2017
		jnb	invalid_system_disk

		mov	ecx, eax ; save first cluster number

		; Load  RTS (Kernel) file
                mov     si, Loading_Msg
                call    print_msg
                
		mov	eax, ecx ; restore first cluster number
		
		;mov	bx, rts_segment ; 1000h
		;mov	[next_segment], bx
bs_10:
		push	eax ; 28 bit cluster num, starts from 2
		sub	eax, 2 ; now, cluster num starts from 0 
		;movzx	ecx, byte [BPB_SecPerClus] ; [bp+0Dh]
		movzx	ecx, byte [bp+0Dh]
		mul	ecx
		; eax = sector offset (from start of data area)
		add	eax, [bp-8] ; add data area (start) addr
		mov	bx, [next_segment]
		push	es
		mov	es, bx ; segment = 1000h +
		xor	bx, bx ; offset = 0 
		; CX = num of sectors to read (= sectors/cluster)
		call	disk_read
		pop	es
		pop	eax
		shr	bx, 4 ; from byte count to paragraph count
		add	[next_segment], bx
		call	get_next_cluster
		;jc	short diskio_error
		jc	short trdos_loading_error
		;cmp	eax, 0FFFFFF8h
		cmp	eax, 0FFFFFF7h ; 23/12/2017
		jnb	short bs_11 ; Startup file has been loaded.

		cmp	eax, 2
		;jb	invalid_system_disk 
		;jmp	short bs_10 ; load next clust of the file
		jnb	short bs_10

trdos_loading_error:
		mov	si, Load_err_Msg
		call	print_msg
		jmp	replace_disk
next_segment:
		dw	rts_segment

		db	'TR-DOS' ; Filler
		
Loading_Msg:    db	0Dh, 0Ah
		db	'Loading Kernel TRDOS386.SYS ...'
                db 	0Dh, 0Ah, 0
Load_err_Msg:
		db	0Dh, 0Ah
		db	'TRDOS Loading Error'
		;db	'!'
		db	0

bs_11:
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
                mov     eax, 417 ; TRDOS boot sector sign for TRDOS386.SYS

		jmp	rts_segment:0

		times	1020 - ($ - $$) db 0
bsReserved3:
		db	'TR'	; 'Turkish Rational DOS' feature identifier
bootsignature2:
		db	55h, 0AAh