; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.0 - trfs1chs.s - TRFS1 BOOT SECTOR
; ----------------------------------------------------------------------------
; Turkish Rational SINGLIX File System 1 Hard Disk (CHS) Boot Sector Code
; ----------------------------------------------------------------------------
; Last Update: 02/02/2018
; ----------------------------------------------------------------------------
; Beginning: 03/01/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm trfs1chs.s -l trfs1chs.lst -o TRFS1CHS.BIN)) 	
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'fat16_bs.s' TRDOS 386 (v2) FAT16 boot sector source code
; by Erdogan Tan (26/12/2017).
;
; Derived from 'FS1_HDBS.ASM' TRDOS 8086 (v1) Singlix FS1 boot sector
; source code by Erdogan Tan (21/02/2010).
; ****************************************************************************
; incbin "TRFS1CHS.BIN" (in 'hdimage.s' & 'hdformat.s')

rts_segment     equ	1000h

; FDT equalities
; 03-01-2018 [ TRFS Issue 2, Revision 14 ]

FDT_Sign	equ 0	; 'FDT'
FDT_SectorSize	equ 4	; 512
FDT_Number	equ 6	; FDT sequence number
FDT_FileNumber	equ 8	; 1st FDT address (offset)	
FDT_NextFDTNum	equ 12  ; Next FDT address
FDT_SectorCount	equ 16	; Sector count in section
FDT_ParentDir	equ 20	; Parent directory address
FDT_PDirSerial	equ 24	; Parent directory serial number
FDT_FileSize	equ 28	; File Size lower 4 bytes
FDT_FileSizeHW	equ 32	; File Size higher 2 bytes

[BITS 16]
[ORG 7C00h]
bsjmpBoot:
                jmp     short loc_30  ; jmp short start
bsjmpBoot_nop:
                nop

; TR-SINGLIX FS1 BootSector Identification (Data) Block
; 03-01-2018 FS1 HD Partition Boot Sector
; 02-01-2018 [ TRFS Issue 1, Revision 14 ]

bsFSystemID:    db 'FS'			; bp+3
                db 0   
bsBytesPerSec:  dw 512			; bp+6
bsMediaAttrib:  db 1			; bp+8
bsPartitionID:  db 0A1h			; bp+9
bsFSVersionMaj: db 01h			; bp+10
bsFSVersionMin: db 0			; bp+11
bsBeginSector:	dd 0			; bp+12 
bsVolumeSize:   dd 0			; bp+16
bsStartupFDT:	dd 0			; bp+20
bsMATLocation:  dd 1			; bp+24
bsRootDirDT:    dd 0			; bp+28
bsSysConfFDT:	dd 0			; bp+32
bsSwapFD:       dd 0			; bp+36
bsUndelDirDT:	dd 0			; bp+40
bsDriveNumber:  db 0			; bp+44
bs_LBA_Ready:	db 01h			; bp+45
bsMagicWord:	
bs_Disk_SecPerTrack:
		db 0A1h			; bp+46
bs_Disk_Heads: 
                db 01h			; bp+47
start:
loc_30:
		mov	bp, 7C00h

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
		mov	dl, [bp+44] ; [bsDriveNumber]
bs_01:
		mov	[bsReserved1], si ; Partition entry address..
		; 24/12/2017		  ; (from Singlix FS MBR)
					  ; (7BEh,7CEh,7DEh,7EEh) 
check_startup_file_address: 
		mov	eax, [bp+20] ; [bsStartupFDT]
		and	eax, eax
		jz	short invalid_system_disk

		cmp	eax, [bp+16] ; [bsVolumeSize]
		jnb	short invalid_system_disk

		sub	sp, 8  ; 13/01/2018
		; SP = 7BF8h

		mov	edi, eax
		add	eax, [bp+12]
		mov	[bp-4], eax

		; overwrite hd drive number !
                ;mov	[bsDriveNumber], dl ; drive number from INT 19h
		mov	[bp+44], dl

		mov	bx, 7E00h    ; FDT Buffer address
		;mov	si, bx

		call	read_sector
		jc	short disk_io_error

check_startup_file_fdt:
 		; 29/01/2018
		; Check File Number (the 1st FDT address)
		cmp	edi, [7E00h+FDT_FileNumber]
		jne	short invalid_system_disk
		call	FDT_validation
		mov	edi, eax
		; EDI = Next FDT (Section) Address
		; [BP-4] = Current FDT (Section) Address
		; ECX = Sector Count
		mov	eax, [7E00h+FDT_FileSize]
		or	eax, eax
		jnz	short bs_02
invalid_system_disk:
		mov	si, Inv_disk_Msg
		call	print_msg
getchar_reboot:
		; Wait for a keystroke just before reboot
		xor	ah, ah
		int	16h
		
		int	19h	; disk boot	
				; causes reboot of disk system
disk_io_error:
		mov	si, Diskio_err_Msg
		call	print_msg
		jmp	short getchar_reboot

FDT_validation:
		; 29/01/2018
		; Check FDT signature
		mov	si, 7E00h
		lodsw
		cmp	ax, 'FD' 
		jne	short invalid_FDT ; not a valid FDT!
		lodsb
		cmp	al, 'T'
		jne	short invalid_FDT ; not a valid FDT!
		add	si, 9
		lodsd	; FDT_NextFDTNum
		mov	ecx, [esi] ; FDT_SectorCount
		or	cx, cx  ; sector count (in section) > 0		
		jz	short invalid_FDT
		add	eax, [bp+12] ; [bsBeginSector]
		; EAX = Next FDT address (physical)
		; ECX = Sector count (in section)
		retn
invalid_FDT:
		pop	ax ; return address
		jmp	short invalid_system_disk

bs_02:	
		xor	edx, edx
		mov	ebx, 512
		add	eax, ebx
		dec	eax
		div	ebx
		mov	bx, 1151
		cmp	eax, ebx	; Maximum 1151 sectors
					; (Segment 1000h to 9FE0h)
					; ((512 bytes must be reserved for
					; stack just before segment A000h))	
		jna	short bs_03
		mov	eax, ebx
bs_03:
		; ECX = sector count (in section)
load_startup_file:
		cmp	eax, ecx
		jnb	short bs_04
		mov	ecx, eax	; sector count (in section)
					; must not be greater
					; remain sectors (for file) to read
bs_04:
		sub	eax, ecx
		mov	[bp-8], eax	; Remain sector count (for next read)	
		mov	ax, [next_segment]
		push	es
		mov	es, ax ; segment = 1000h +
		mov	eax, edi ; Next FDT Address
		xchg	eax, [bp-4] ; eax = Current FDT Address
		inc	eax	 ; +1 (section data)
		; CX = Sector count (<= 1151) in section
		xor	bx, bx ; offset = 0 
		; CX = num of sectors to read (= sectors/cluster)
		call	disk_read
		pop	es
		jc	short disk_io_error
		shr	bx, 4 ; from byte count to paragraph count
		add	[next_segment], bx

		mov	edi, [bp-8]	; Remain sector count
		;or	edi, edi
		or	di, di
		jz	bs_06		; none
		
		mov	eax, [bp-4]	; Next FDT address
		mov	bx, 7E00h	; FDT Buffer address
		;mov	si, bx

		call	read_sector
		jc	disk_io_error
		
		call	FDT_validation
		
		xchg	eax, edi
		
		; EDI = Next FDT address (physical)
		; EAX = Remain sector count
		; ECX = Sector count (in section)
		
		jmp	short load_startup_file

read_sector:	; 25/12/2017 (Read 1 sector)
		mov	cx, 1
disk_read:
		;mov	byte [bp+retry_count-7C00h], 4
		mov	dl, 4 ; retry count
disk_read_0:
		pushad
		cmp	byte [bp+45], 0 ; TRFS1 LBA partition & LBA ready
		jna	short chs_read
lba_read:
		;pushad

		;mov	di, sp
				
		push	dword 0
		push	eax
		push	es
		push	bx
		push 	byte 1
		push	byte 16 ; 10h
		mov	ah, 42h
		;mov	dl, [bsDriveNumber]
		mov	dl, [bp+44]
		mov	si, sp
		int	13h

		;pop	eax
		;pop	eax
		;pop	eax
		;pop	eax
		;mov	sp, di
		
		; 30/01/2018
		popa
		jmp	short disk_read_1
chs_read:	
		;pushad

		; Convert LBA to CHS
		xor	edx, edx
		;movzx	ecx, byte [bs_Disk_SecPerTrack] ; [bp+46]
				; sectors per track (17 or 63)
		movzx	ecx, byte [bp+46]
		div	ecx
		inc	dl	; sector number (1 based)
		push	dx
		mov	edx, eax ; (heads * cylinder) + head number
		shr	edx, 16	 ; high word in DX, low word in AX
		;mov	cl, [bs_Disk_Heads] ; [bp+47]
		mov	cl, [bp+47] ; number of heads (2 to 255)	
		div	cx 	
		; AX = cylinder (0 to 1023)
		; DX = head number (in DL)
		mov	dh, dl	 ; head number in DH
		;mov	dl, [bsDriveNumber] ; [bp+2Ch] ; Drive number (80h)
		mov	dl, [bp+44]
		pop	cx
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
		;add	bx, [bp+6] ; [bsBytesPerSec] ; 512
		add	bx, 512
		;add	bh, 2 ; **
		jnc	short disk_read_3
		mov	bx, es
		add	bx, 1000h	 
		mov	[next_segment], bx
		mov	es, bx
		xor	bx, bx
disk_read_3:
		inc	eax
		dec	cx
		jnz	short disk_read
		;clc 	; ** (128 sectors/cluster!?)
bs_05:
		retn

		; Filler
		db	'417'
		db	0
bs_06:
		; Set TRDOS 386 kernel specific parameters (& signs)
		; and
		; Launch TRDOS 386 Kernel (Startup/RTS file)

		mov	eax, 417

		mov	bx, [next_segment] ; 16 paragraphs after the
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
		mov	ss, bx
		mov	sp, 0FFFEh			
		sti

		mov     bx, rts_segment ; 1000h

		mov	ds, bx
		mov	es, bx
		;mov	fs, bx
		;mov	gs, bx

		;xor	ebx, ebx
		;xor	ecx, ecx
		;xor	edx, edx
		;xor	esi, esi
		;xor	edi, edi
		;xor	ebp, ebp

		; bp = 7C00h

		;mov	dl, [bsDriveNumber]
                mov	dx, [bp+44] ; DL = Drive number, DH = 0 (CHS)
				    ;			 DH = 1 (LBA)

		;mov	eax, 417 ; TRDOS boot sector sign for TRDOS386.SYS

		jmp	rts_segment:0

print_msg:
		; DS:SI = Error message address (ASCIIZ string)	
		mov	ah, 0Eh
		mov	bx, 7
bs_07:
		lodsb
		test	al, al
		jz	short bs_05
		int	10h
		jmp	short bs_07

next_segment:
		dw	rts_segment

Diskio_err_Msg:
		db	0Dh, 0Ah
		db	'Disk error!'
		db	0Dh, 0Ah, 0
Inv_disk_Msg:   
		db	0Dh, 0Ah
		db	'Invalid system disk!'
		db	0Dh, 0Ah, 0

		; Filler (Boot Code Date)
		db	02h
		db	02h
		db	20h
		db	18h

		times	508 - ($ - $$) db 0
bsReserved1:
		db	'TR'  ; 'Turkish Rational DOS' feature identifier.
bootsignature1:
		db	55h, 0AAh