; ****************************************************************************
; RD5HDBS2.ASM - Retro DOS v5 FAT 16 (Hard Disk) BOOT SECTOR code
; ----------------------------------------------------------------------------
; Last Update: 20/04/2024
; ----------------------------------------------------------------------------
; Beginning: 18/10/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
;	    ((nasm rd5hdbs2.s -l rd5hdbs2.txt -o RD5HDBS2.BIN)) 	
; ----------------------------------------------------------------------------
; Derived from 'rd4hdbs.s' Retro DOS v4 FAT16 hd boot sector source code
; by Erdogan Tan (24/10/2023).
; ****************************************************************************
; incbin "RD5HDBS2.BIN" (in 'rd5hdi2.s')

notvalidfmask   equ	0018h

root_dir_buff   equ	7E00h

rts_segment	equ	1000h

FAT_Buffer	equ	7E00h

; 24/10/2023
root_FAT_buffer equ	7E00h 

[BITS 16]
[ORG 7C00h]
		jmp	short BS_01
		nop

; BootSector Identification (Data) Block

bsOemName:	db 'RETRODOS'	    
bsBytesPerSec:  dw 512
bsSecPerClust:  db 4
bsResSectors:   dw 1
bsFATs:	   db 2
bsRootDirEnts:  dw 512
bsSectors:	dw 0
bsMedia:	  db 0F8h
bsFATsecs:	dw 63
bsSecPerTrack:  dw 63
bsHeads:	  dw 16
bsHidden1:	dw 63
bsHidden2:	dw 0
bsHugeSectors:  dd 0
bsDriveNumber:  db 80h
bsReserved1:    db 0
bsBpbSignature: db 29h		 
bsVolumeID:	dd 0
bsVolumeLabel:  db 'NO NAME    '
bsFileSysType:  db 'FAT16   '
;;bsReserved2:	dw 'v2'	; Retro DOS 2.0 ; 27/03/2018
;bsReserved2:	dw 'v4'	; Retro DOS 4.0 ; 22/10/2023
bsReserved2:	dw 'v5'	; Retro DOS 5.0 ; 20/04/2024
; Retro DOS 1.0 Extensions
;bsReserved2:	dw 'v1'
bsDataStart:    dw 159
bsRootDirStart:	dw 127
bsRootDirSects:	dw 32
; 22/10/2023
bsDirEntsPerSec: dw 16

		; 20/04/2024 - Retro DOS v5 (PCDOS.SYS) modification
		; Erdogan Tan - 21/10/2023  
		; Retro DOS v4.0 v4.1 v4.2 (FAT16 FS) combined kernel loader
BS_01:
		mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax
		mov	sp, 0FFFEh
		sti

		; 24/10/2023
		; 21/10/2023
		;mov	bp, sp

		; overwrite hd drive number !
		mov	[bsDriveNumber], dl 	; drive number from INT 19h
						; (DL = 80h)
		; 24/10/2023
		; Retro DOS v3.0 - 19/10/2018
		;mov	dx, [bsSectors]
		;and	dx, dx
		;jz	short BS_02
		;
		;mov	[bsHugeSectors], dx
		;xor	dx, dx
		;;mov	[bsHugeSectors+2], dx
;BS_02:
		mov	ax, [bsRootDirStart]	; Root Dir Location
		; 24/10/2023
		mov	bp, [bsRootDirSects]
BS_02:		; 24/10/2023
		; 22/10/2023
		;;mov	cl, [bsRootDirSects]	; Root Directory Sectors
		;mov	cl, 1			; read 1 root dir sector
		;mov	bx, root_dir_buff	; Destination offset = 7E00h
		;mov	si, bx
		; 24/10/2023	  
		call	hd_read_1
		;call	hd_read
		jc	short BS_05		; Disk read error message

		; 24/10/2023
		; ax = next root directory sector
	
		; 22/10/2023
		;mov	bx, [bsRootDirEnts]	; Number of root dir entries
		mov	bx, [bsDirEntsPerSec]
		; 24/10/2023
		mov	si, root_dir_buff
BS_03:	 
		; 24/10/2023
		cmp	[si], dl ; dx = 0
		;cmp	byte [si], 0		; Is it null entry?
		je	short BS_04_ERR		; Jump if zero ( = )

		mov	cx, 11			; Size of file/directory name
		push    si
		mov	di, rtsfilename   
		repe	cmpsb			; Repeat if ZF = 1, CX > 0
						; Cmp byte DS:SI with ES:DI
		pop	si
		je	short BS_07		; If the file name found
		dec	bx		    
		jz	short BS_04		; Jump if no next entry
		add	si, 32			; To next directory entry
		jmp	short BS_03		; Jump for next sector

BS_04:
		; 22/10/2023
		;dec	word [bsRootDirSects]
		;jz	short BS_04_ERR
		;inc	word [bsRootDirStart]
		;jmp	short BS_02
		; 24/10/2023
		dec	bp  	
		jnz	short BS_02
BS_04_ERR:
		mov	si, Replace_Msg
		jmp	short BS_06
BS_05:	
		mov	si, Error_Msg
BS_06:	    
		call	print_msg

		xor	ax, ax
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						; AH-scan code AL-char code
		int	19h			; Reboot

BS_07:
		;mov	al, [si+0Bh]		; Move attributes byte to BL
		;and	al, notvalidfmask	 ; Is it a file, really?
		; 24/10/2023
		test	byte [si+0Bh], notvalidfmask
		jnz	short BS_04_ERR		; Jump if not
		mov	ax, [si+1Ah]		; First cluster of the file
		cmp	ax, 2			; Start cluster
		jb	short BS_05

  		;mov	[bsReserved2], ax	; Save the first cluster
		; 21/10/2023
		push	ax 	

		; Load  RTS (Kernel) file
		mov	si, Loading_Msg
		call	print_msg

		; 21/10/2023
		; Load FAT
		;;mov	ax, 1
		;mov	ax, [bsResSectors]	; FAT Location
		; 21/10/2023
		; loading entire FAT is not proper for big FAT16 fs
		; also FAT buffer (at 7E00h) will overlap stack (at 0:7FFEh)
		; if fat sectors > 64.
		;mov	cl, [bsFATsecs]		; FAT Sectors
		;mov	bx, FAT_Buffer
		;call	hd_read
		;jc	short BS_05

		; 21/10/2023
		pop	ax	
		;mov	ax, [bsReserved2]	; The First cluster
		mov	bx, rts_segment
		mov	es, bx
		xor	bx, bx
		call	load_file
		jc	short BS_05

		; Launch RTS (Kernel)
   		;mov	ax, es
		;cli
		;mov	ss, ax
		;mov	sp, 0FFFEh
		;sti
		;mov	dl, [bsDriveNumber]
		;mov	ax, rts_segment ; 1000h ; 60h
		;mov	ds, ax
		;mov	es, ax
		 
		mov	bp, 7C00h

		; Retro DOS 3.0 ; 19/10/2018
		mov	dl, [bsDriveNumber]
		; Retro DOS 2.0 ; 27/03/2018
		mov	dh, [bsMedia]
		;mov	ax, [bsSectors]
		; 20/04/2024 - Retro DOS 5.0
		mov	ax, 0

		jmp	rts_segment:0

		;db	0EAh
		;dw	0
		;dw	rts_segment

print_msg:
BS_08:
		lodsb			; Load byte at DS:SI to AL
		and	al, al		
		jz	short BS_10	; If AL = 00h then stop

		mov	ah, 0Eh
		mov	bx, 07h		 
		int	10h		; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
		jmp	short BS_08

BS_09:	
		dec	ch  ; dec byte [RetryCount]
		jz	short BS_10 ; cf = 1
		
		; 24/10/2023
		push	ax
		xor	ax, ax
		; ah = 0 ; INT 13h reset function
		int	13h
		pop	ax
		jnc	short BS_11
BS_10:
		retn

hd_read_1:
		; 24/10/2023
		mov	cl, 1	; read 1 sector (only)
		mov	bx, root_FAT_buffer ; *-* ; 7E00h
		xor	dx, dx  ; 0
hd_read:
		; Only for FAT16 file systems !
		
		; 19/10/2018
		
		; DX:AX = LBA address (as partition offset)
		; CL = sector count
		; ES:BX = buffer address
		
		mov	ch, 4 ; mov byte [RetryCount], 4
BS_11:
		pusha	; !*

		add	ax, [bsHidden1]
		adc	dx, [bsHidden2]

		; DX:AX = LBA address (as physical address)

		xchg	cx, ax
		mov	si, [bsSecPerTrack] ; Sectors per track
		xchg	ax, dx
		xor	dx, dx
		div	si
		xchg	ax, cx
		div	si
		inc	dx
		xchg	cx, dx
		div	word [bsHeads]	; Heads
		mov	dh, dl
		mov	ch, al
		ror	ah, 2
		or	cl, ah
		mov	ax, 201h
		mov	dl, [bsDriveNumber] ; Drive number (80h)
		int	13h		; BIOS Service func ( ah ) = 2
					; Read disk sectors
					; AL-sec num CH-track CL-sec
					; DH-head DL-drive ES:BX-buffer
					; CF-flag AH-stat AL-sec read
		popa	; !*
		jc	short BS_09

		inc	ax

		;add	bx, 512
		add	bh, 2
		jnc	short BS_12
		push	bx
		mov	bx, es
		;add	bx, 1000h
		add	bh, 10h
		mov	es, bx
		pop	bx
BS_12:
		;dec	cx
		dec	cl
		jnz	short hd_read
		retn

load_file:
		 	; ES:BX = File Buffer
		  	; AX = First Cluster Number
		  	; Only for FAT16 File Systems	; 19/10/2018

		; 21/10/2023
		; bp = 0FFFEh (sp)
		; 24/10/2023
		mov	bp, sp ; 0FFFEh
BS_13:
		;mov	[File_Cluster], ax
		; 21/10/2023
		mov	di, ax ; file (current) cluster

		dec	ax			; First cluster is cluster 2
		dec	ax
	
		; 18/05/2018
		xor	ch, ch
		mov	cl, [bsSecPerClust]
		mul	cx
BS_14:
		add	ax, [bsDataStart]	; Beginning sector of Data
		adc	dx, 0 ; 19/10/2018
		;mov	dl, [bsDriveNumber]	; Disk number (= 80h)
	
		call    hd_read
		jc	short BS_16
get_next_cluster:
		; 21/10/2023
		mov	cx, [bsBytesPerSec]
		shr	cx, 1
		; cx = clusters per FAT sector 
		xor	dx, dx
		;mov	ax, [File_Cluster]
		mov	ax, di ; file (current) cluster
		div	cx	
		; ax = sector
		shl	dx, 1 ; convert index to offset
		; dx = offset
		mov	si, FAT_Buffer
		add	si, dx

		cmp	ax, bp	; previous FAT sector in the FAT buffer
		je	short BS_15 ; no need to read FAT sector again
		mov	bp, ax

		push	es ; ** ; kernel file buffer segment
		push	bx ; * ; save kernel file buffer address

		push	ds
		pop	es
		
		; 24/10/2023
		;mov	bx, FAT_Buffer	

		add	ax, [bsResSectors]	; FAT sector LBA
		; 24/10/2023
		; 22/10/2023
		;sub	dx, dx ; 0
			; hw of the FAT sec is always zero for FAT16 fs
 
		; 24/10/2023
		;mov	cl, 1			; Read 1 FAT Sector
		;;mov	bx, FAT_Buffer
		;call	hd_read
		call	hd_read_1

		pop	bx ; * ; restore kernel file buffer address
		pop	es ; ** ; kernel file buffer segment
		jc	short BS_16
BS_15:
		; 21/10/2023
		; 19/10/2018
		;push	bx
		;mov	bx, [File_Cluster]
		;shl	bx, 1 ; *2
		; bx = FAT_Buffer = 7E00h

		; BX = Buffer Byte Offset
		; AX = Current Cluster
		;mov	ax, [FAT_Buffer+bx]
		; 21/10/2023
		lodsw		

		;pop	bx
		cmp	ax, 0FFF7h	; 0FFF7h = Bad cluster (stop!)
					; >= 0FFF8h = End of file
		jb	short BS_13

		; EOCC (kernel file has been loaded successfully)
BS_16:
		retn

		; 24/10/2023 ; Filler
		db	0
		db	20
		db	4
		dw	2024	; 20/04/2024
		db	0
Error_Msg:
		db	0Dh, 0Ah
		;;db	'DOS Kernel Loading Error!'
		; 21/10/2023
		;db	'Disk read error!'
		; 22/10/2023
		db	'Error!'
Replace_Msg:
		db 	0Dh, 0Ah
		db	'Replace the disk and press any key to reboot.'
		; 21/10/2023
		;db	'Replace the disk and press a key to reboot.'
		db	0Dh, 0Ah,0

		; 21/10/2023
;Reserved3:	db	20h
;RetryCount:	db	18h

		; 24/10/2023 ; Filler
		;db	'RDv4 FAT16 06h'
		db	'RDv5 FAT16 06h' ; 20/04/2024
		db	0

Loading_Msg:    db	0Dh, 0Ah
		;db	"Loading Kernel MSDOS.SYS ..."
		db	"Loading Kernel PCDOS.SYS ..." ; 20/04/2024
		db	0Dh, 0Ah, 0

		; Filler
		;db	'RETRODOS.SYS'
		;db	0

;File_Cluster:	; 22/10/2023
		dw	417
		; 21/10/2023
		;dw	2023

rtsfilename:
		;db	'MSDOS   SYS'
		db	'PCDOS   SYS' ; 20/04/2024
		db	0

		times	510 - ($ - $$) db 0

bootsignature:  db	55h, 0AAh