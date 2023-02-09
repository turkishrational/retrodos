; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.0 - fat16_bs.s - FAT16 BOOT SECTOR
; ----------------------------------------------------------------------------
; Last Update: 26/12/2017
; ----------------------------------------------------------------------------
; Beginning: 23/12/2017
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm fat16_bs.s -l fat16_bs.lst -o FAT16_BS.BIN)) 	
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; NOTE:	This code has some tricks and TRDOS 386 specific modifications
; which are not a part of original Microsoft Windows (XP) FAT16 BS code.
; (Purpose of TRDOS 386 specific modifications and tricks is to load
; 'TRDOS386.SYS' kernel file as easy and as correct, without affecting
; FAT16 FS recognization for other operating systems.) 
;
; Derived from Microsoft WINDOWS XP FAT (FAT16&FAT12) boot sector code
; which is edited/disassembled by Erdogan Tan (17/12/2017), 
; by using BINFILEHEX (Erdogan Tan) & IDA PRO FREE (Hex-Rays SA) programs.	
;
; Derived from 'fat32_bs.s' TRDOS 386 FAT32 boot sector source code
; by Erdogan Tan (23/12/2017).
; ****************************************************************************
; incbin "FAT16_BS.BIN" (in 'hdimage.s' & 'hdformat.s')

rts_segment     equ	1000h

[BITS 16]
[ORG 7C00h]
BS_jmpBoot:
                jmp     short loc_3E	; jmp short start
BS_jmpBoot_nop:
                nop

; BootSector Identification (Data) Block
BS_OEMName:
		db	'MSDOS5.0'	; bp+3
BPB_BytesPerSec: dw	512		; bp+11
BPB_SecPerClus:	db	16		; bp+13
BPB_RsvdSecCnt: dw	1		; bp+14
BPB_NumFATs:	db	2		; bp+16
BPB_RootEntCnt:	dw	512		; bp+17
BPB_TotSec16:	dw	0		; bp+19
BPB_Media:	db	0F8h		; bp+21
BPB_FATSz16:	dw	0		; bp+22
BPB_SecPerTrk:	dw	63		; bp+24
BPB_NumHeads:	dw	16		; bp+26
BPB_HiddSec:	dd	1		; bp+28
BPB_TotSec32:	dd	0		; bp+32
BS_DrvNum:	db	80h		; bp+36
BS_Reserved1:	db	0		; bp+37
BS_BootSig:	db	29h		; bp+38
BS_VolID:	dd	0		; bp+39
BS_VolLab:	db	'TRDOS386   '	; bp+43
BS_FilSysType:	db	'FAT16   '	; bp+54

start:
loc_3E:
		add	al, 0 ; db 04h, 00h -> FAT 16 CHS (<32MB)
			 ; TRDOS 386 (FAT16 BS) LBA check trick!!
			 ; db 0Eh, 07h -> FAT 16 LBA
			 ;	push	cs
			 ;	pop	es
			 ; db 06h, 07h -> FAT 16 BIG CHS (>=32MB)
			 ;	push	es
			 ;	pop	es

		mov	bp, 7C00h

		; ((WINDOWS XP FAT 16 boot sector code checks Masterboot
		; partition table for partition type, if it is 0Eh 
		; -FAT16 LBA-, the boot code changes 90h at BS offset 2
		; to 0Eh. Then a 0Eh at this addr is used as identifier. 
		; While reading a disk sector at the beyond of the CHS
		; limit, INT 13h -LBA read- extension will be used if 
		; this identifier is 0Eh.))  

		cmp	ax, 417  ; If AX=417, the masterboot sector
				 ; has a SINGLIX FS (& TRDOS 386)
				 ; masterboot code; and...  
				 ; DX=ES=SS=0, BP=7C00h
				 ; SP=7C00h ... masterboot sector has
				 ; been loaded at 0:600h, it has
				 ; CHS parameters at offset 600h+420.
				 ; (There is a 01A1h in offset 600h+417)
	
		je	short bs_01 ; no need to following assignments !

		;xor	ax, ax
		;mov	ds, ax
		;mov	es, ax
		push	cs
		pop	ds
		push	ds
		pop	es
		cli
		;mov	ss, ax
		;mov	sp, bp
		push	cs
		pop	ss
		mov	sp, bp
		sti
		mov	dl, [bp+24h] ; [BS_DrvNum]
bs_01:
		mov	[bsReserved1], si ; Partition entry address..
					  ; (from Singlix FS MBR)
					  ; (7BEh,7CEh,7DEh,7EEh) 

		; Check Bytes/Sector value
		; It must be 512 !? (at least, for TRDOS386) 
		;cmp	word [bp+0Bh], 512 ; [BPB_BytesPerSec]
		;cmp	byte [bp+0Ch], 2
		;jne	short invalid_system_disk

		; Check Extended Boot Signature (It must be 29h)
		;cmp	byte [bp+26h], 29h ; [BS_BootSig]
		;jne	short invalid_system_disk

		; overwrite hd drive number !
                ;mov	[BS_DrvNum], dl	; drive number from INT 19h
		mov	[bp+24h], dl
		;mov	[bp+25h], dh	; [BS_Reserved1] = Partition ID !!!
		;mov	[bp+24h], dx	

		; reset address pointers and set SP
		;xor	cx, cx ; 0
		;push	cx ; [bp-2] ; Cluster 2 address (high word)
		;push	cx ; [bp-4] ; Cluster 2 address (low word)
		;push	cx ; [bp-6] ; FAT (0) address (high word)
		;push	cx ; [bp-8] ; FAT (0) address (low word)
		sub	sp, 8
		;dec	cx ; 0FFFFh
		;push	cx ; [bp-10] ; FAT sector buffer address reset (-1)
		;inc	cx ; 0
		push	0FFFFh
		push	rts_segment ; Kernel loading segment in 7BF4h

		; SP = 7BF4h	

		; calculate cluster 2 address
		xor	cx, cx
		; 25/12/2017
		mov	si, [bp+1Ch]	; word [BPB_HiddSec]
		mov	di, [bp+1Eh]	; word [BPB_HiddSec+2]
		add	si, [bp+0Eh]	; [BPB_RsvdSecCnt]
		adc	di, cx ; 0

		mov	[bp-8], si	; FAT beginning sector (lw)		
		mov	[bp-6], di	; FAT beginning sector (hw)

		mov	al, [bp+10h]	; [BPB_NumFATs]
		cbw
		mul	word [bp+16h]	; [BPB_FATSz16]

		add	ax, si
		adc	dx, di

		mov	si, [bp+11h]	; [BPB_RootEntCnt]
		mov	bx, 512
		pusha
		mov	[bp-4],	ax	; [7BFCh] = Root Dir address
		mov	[bp-2],	dx
		mov	ax, 20h	; 32	; Directory Entry Size
		mul	si
		; DX = 0
		;mov	bx, [bp+0Bh]	; [BPB_BytesPerSec] =  512
		;mov	bx, 512
		add	ax, bx		
		dec	ax
		div	bx
		; AX = Root directory sectors
		add	[bp-4],	ax	; [7BFCh]  = Start of Data area 
		adc	[bp-2],	cx  	; adc [bp-2], 0
		;popa

		;; check for ROMBIOS INT 13h extensions
		;mov	ah, 41h
		;mov	bx, 55AAh
		;;mov	dl, [BS_DrvNum]
		;;mov	dl, [bp+40h]
		;int	13h
		;jc	short bs_02
		;cmp	bx, 0AA55h
		;jne	short bs_02
		;test	cl, 1
		;jz	short bs_02

		; ROMBIOS INT 13h extentions are present...

		;;mov	al, [loc_3E]
		;mov	al, [bp+62]
		;;mov	[BS_jmpBoot+2], al ; 'LBA mode is available'
		;mov	[bp+2], al ; 0Eh (LBA) or 06h (CHS) or 04h (CHS)
bs_02:
		popa
		jmp	short load_root_dir_sector

check_next_dir_entry:
		add	di, 32	; next dir entry
		cmp	di, bx	; end of root dir buffer
		jb	short search_startup_file

load_root_dir_sector:
		; load root directory and check directory entries 
		;mov	bx, 7E00h	; Root dir buffer (1 sector)
		mov	bh, 7Eh ; 25/12/2017
		mov	di, bx		; Root dir entry offset
		; DX_AX = root dir sector address (DX = 0)
		;mov	cl, 1
		call	Read1Sector ; Read 1 sector (CL=1)
		jc	short disk_io_error

		; BX = 8000h
search_startup_file: 
		; check/compare root dir entry for/with kernel file
		cmp	[di], ch ; 0
		je	invalid_system_disk ; kernel not found!
		pusha
		mov	cl, 11 ; 0Bh
		mov	si, rtsfilename ; Run Time System file name
					; or Kernel file name
					; (or Startup file name)
					; (or Standalone file name)
					; in MSDOS directory entry
					; format. ('TRDOS386SYS')
					; It is 'TRDOS386.SYS'
					; for TRDOS 386 OS.
		repe	cmpsb ; compare dir entry and kernel's name
		popa
		jz	short load_startup_file ; kernel is there!
		dec	si ; countdown from Root dir entry count
		jnz	short check_next_dir_entry

invalid_system_disk: 
		; 'Replace Disk & Press Any Key' message (24/12/2017)
		mov	si, Inv_disk_Msg
write_disk_io_err_msg:
		call	print_msg
getchar_reboot:
		; Wait for a keystroke just before reboot
		xor	ah, ah
		int	16h
		
		int	19h	; disk boot	
				; causes reboot of disk system
disk_io_error:
		mov	si, Diskio_err_Msg
		;call	print_msg
		jmp	short write_disk_io_err_msg ; 24/12/2017
;replace_disk:		
;		mov	si, Replace_Msg	
;replace_disk:	
;		mov	si, Disk_err_replace_Msg
;		call	print_msg
;		jmp	short getchar_reboot

load_startup_file:
		; DI = directory entry offset
		; Get the First Cluster (Dir entry offset 26)
		mov	ax, [di+1Ah] ; [di+DIR_FstClusLO]
ReadCluster:
		push	ax ; cluster number
		
		dec	ax
		dec	ax ; cluster number - 2
		
		;xor	ch, ch 
		mov	cl, [bp+0Dh]	; [BPB_SecPerClus]
		mul	cx

		add	ax, [bp-4] ; [7CFCh], Start of FAT FS data area
		adc	dx, [bp-2]	

		;mov	bx, [next_segment]
		mov	bx, [bp-12] ; Kernel loading segment (in 7BF4h)
		push	es
		mov	es, bx ; segment = 1000h +
		xor	bx, bx ; offset = 0 
		; CL = num of sectors to read (= sectors/cluster)
		call	ReadSector
		; CX = 0
		pop	es
		pop	ax ; cluster number	
		jc	short disk_io_error
		shr	bx, 4 ; from byte count to paragraph count
		;add	[next_segment], bx
		add	[bp-12], bx ; Next segment (in 7BF4h)

		;call	get_next_cluster
		;jc	short disk_io_error

get_next_cluster:	; get next (FAT16) file cluster
		; AX = current cluster number
		; 25/12/2017
		xor	dx, dx
		shl	ax, 1 ; 16 bit FAT entry offset
		adc	dl, ch ; 0

		;call	get_fat16_entry
		;jc	short disk_io_error

get_fat16_entry:
		; AX = 16 bit FAT entry (word) offset
		mov	di, 8000h ; FAT (sector) buffer
		; 25/12/2017
		;mov	bx, [BPB_BytesPerSec]  ; [bp+11]
		;mov	bx, [bp+0Bh]
		mov	bx, 512
		div	bx

		push	dx  ; 0 to 510 (cluster entry offset)
	
		; AX = FAT sector number (relative)
		; Check FAT sector number if it is already
		; in FAT buffer at 8000h.
		; Current FAT sector is in 7BF6h. 
		; (Note: initial FAT sector value in 7BF6h is
		; 0FFFFh which means the buff is not loaded yet.)
		cmp	ax, [bp-10] ; [7BF6h]
		je	short bs_03 ; same sector in FAT buffer
		mov	[bp-10], ax ; save FAT sector number

		; Calculate absolute (LBA) address of FAT sector
		; by adding hidden (partition's start sector)
		; and reserved sectors (between BS and FAT).
		; (Note: FAT copy 0 address is stored in 7BF8h)
		xor	dx, dx
		add	ax, [bp-8]  ; FAT (0) sector addr (lw)
		adc	dx, [bp-6]  ; FAT (0) sector addr (hw)
		
		mov	bx, di ; FAT (sector) buffer
		;mov	cx, 1
		;mov	cl, 1 ; Sector count in CL
		call	Read1Sector ; Read 1 sector (25/12/2017)
		; If cf = 1 -> Disk I/O err, not invalid sys disk!
		pop	bx
		jc	short disk_io_error ; 24/12/2017
		push	bx
bs_03:
		pop	bx
;		retn

end_of_get_fat16_entry:
		mov	ax, [di+bx] ; 16 bit cluster number

		;;cmp	ax, 0FFF7h
		;;cmc
;;		retn

end_of_get_next_cluster:
		; 24/12/2017
		cmp	ax, 2
		jb	short invalid_system_disk 

		cmp	ax, 0FFF7h
		;jnb	short bs_04 ; Startup file has been loaded.
		jb	short ReadCluster ; load/read next cluster

end_of_ReadCluster:
bs_04:
		; Set TRDOS 386 kernel specific parameters (& signs)
		; and
		; Launch TRDOS 386 Kernel (Startup/RTS file)

		;mov	dl, [BS_DrvNum]
                mov	dx, [bp+24h] ; DL = Drive number, DH = 0 
		inc	dh  ; TRDOS 386 FAT16 BS major version = 1

		;mov	ax, [next_segment] ; 16 paragraphs after the
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

		mov	ax, [bp-12] ; [7BF4h] ; the last segment		
					      ; which is the kernel file
					      ; has been loaded in.	
		cli
		mov	ss, ax
		mov	sp, 0FFFEh			
		sti

		mov     bx, rts_segment ; 1000h
                mov     ds, bx
                mov     es, bx

		; bp = 7C00h
                
                mov     ax, 417 ; TRDOS boot sector sign for TRDOS386.SYS
		
		;jmp	rts_segment:0
		
		push	es
		push	0
		retf

print_msg:
		; DS:SI = Error message address (ASCIIZ string)	
		mov	ah, 0Eh
		mov	bx, 7
bs_05:
		lodsb
		test	al, al
		;jz	short bs_06
		jz	short bs_09
		int	10h
		jmp	short bs_05
bs_06:
;		retn

Read1Sector:	; 25/12/2017
		mov	cl, 1
ReadSector:
		mov	ch, 4 ; retry count
ReadSector_retry:
		pusha
		push	dword 0
		push	dx
		push	ax
		push	es
		push	bx
		push	1
		push	10h
		xchg	ax, cx
		mov	ax, [bp+18h] ; [BPB_SecPerTrk]
		xchg	ax, si
		xchg	ax, dx
		xor	dx, dx
		div	si
		xchg	ax, cx
		div	si
		inc	dx
		xchg	cx, dx
		div	word [bp+1Ah] ; [BPB_NumHeads]
		mov	dh, dl
		mov	ch, al
		ror	ah, 2
		or	cl, ah
		mov	ax, 201h
		;cmp	byte [bp+2], 0Eh ; (*) ; [BS_jmpBoot+2]
		cmp	byte [bp+62], 0Eh ; 24/12/217
		jne	short bs_07	; Not a LBA disk/partition
		mov	ah, 42h	; 'B'
		mov	si, sp		; Disk address packet
bs_07:
		mov	dl, [bp+24h]	; [BS_DrvNum]
		int	13h		; DISK - read
		popa
		popa
		jc	short bs_10	; return with error
		inc	ax
		jnz	short bs_08
		inc	dx
bs_08:
		;add	bx, [bp+0Bh]	; [BPB_BytesPerSec] = 512
		;add	bx, 512
		add	bh, 2
		;dec	cx
		dec	cl
		jnz	short ReadSector
		;clc
		xor	ch, ch
bs_09:
		retn
bs_10:
		dec	ch
		jnz	short ReadSector_retry
		retn

;next_segment:
;		dw	rts_segment

		db	07h	; Filler

Diskio_err_Msg:
                db	0Dh, 0Ah
		db	'Disk error!'
		;db	0
Inv_disk_Msg:   
;		db	0Dh, 0Ah
;		db	'Invalid system disk'
;Disk_err_replace_Msg:
;		db	'!'
Replace_Msg:    
		db	0Dh, 0Ah
		db	'Replace the disk and press any key to reboot.'
		db	0Dh, 0Ah, 0

		;times	(508+rtsfilename-bsReserved1) - ($ - $$) db 0
rtsfilename:
                db      'TRDOS386SYS'
                db      0
next_segment:
		;org	7C00h+1FCh ; BS offset 508 (bp+508)
bsReserved1:
		db	'TR'  ; 'Turkish Rational DOS' feature identifier.
bootsignature1:
		db	55h, 0AAh