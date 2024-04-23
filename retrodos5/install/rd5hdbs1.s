; ****************************************************************************
; RD5HDBS1.ASM - Retro DOS v5 FAT 12 (Hard Disk) BOOT SECTOR code
; ----------------------------------------------------------------------------
; Last Update: 20/04/2024
; ----------------------------------------------------------------------------
; Beginning: 14/05/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
;	    ((nasm rd5hdbs1.s -l rd5hdbs1.txt -o RD5HDBS1.BIN)) 	
; ----------------------------------------------------------------------------
; Modified from 'rd2hdbs.s' Retro DOS v4 FAT12 boot sector source code
; by Erdogan Tan (25/10/2023).
; ****************************************************************************
; incbin "RD5HDBS1.BIN" (in 'rd5hdi1.s')

notvalidfmask   equ	0018h

root_dir_buff   equ	7E00h

rts_segment     equ	1000h

FAT_Buffer      equ	7E00h

; 22/10/2023
root_FAT_buffer equ	7E00h 

[BITS 16]
[ORG 7C00h]
		jmp	short BS_01
		nop

; BootSector Identification (Data) Block

bsOemName:      db 'RETRODOS'
bsBytesPerSec:  dw 512
bsSecPerClust:  db 16
bsResSectors:   dw 1
bsFATs:         db 2
bsRootDirEnts:  dw 512
bsSectors:      dw 0
bsMedia:        db 0F8h
bsFATsecs:      dw 12
bsSecPerTrack:  dw 63
bsHeads:        dw 16
bsHidden1:      dw 1
bsHidden2:      dw 0
bsHugeSectors:  dd 0
bsDriveNumber:  db 80h
bsReserved1:    db 0
bsBpbSignature: db 29h
bsVolumeID:     dd 0
bsVolumeLabel:  db 'NO NAME    '
bsFileSysType:  db 'FAT12   '
;;bsReserved2:	dw 'v2'	; Retro DOS 2.0 ; 27/03/2018
;bsReserved2:	dw 'v4'	; Retro DOS 4.0 ; 22/10/2023
bsReserved2:	dw 'v5'	; Retro DOS 5.0 ; 20/04/2024
; Retro DOS 1.0 Extensions
;bsReserved2:	dw 'v1'
bsDataStart:    dw 57
bsRootDirStart:	dw 25
bsRootDirSects:	dw 32
; 22/10/2023
bsDirEntsPerSec: dw 16

		; 20/04/2024 - Retro DOS v5 (PCDOS.SYS) modification
		; Erdogan Tan - 22/10/2023  
		; Retro DOS v4.0-v4.1-v4.2 (FAT12 FS) combined kernel loader
		; ((also compatible with Retro DOS v2.0-v3.2))
BS_01:
		mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax ; 0
		mov     sp, 0FFFEh
		sti

		; 22/10/2023
		mov	bp, sp

		; overwrite hd drive number !
		mov	[bsDriveNumber], dl 	; drive number from INT 19h
						; (DL = 80h)
;BS_02:
		mov	ax, [bsRootDirStart]	; Root Dir Location
		; 24/10/2023
		mov	dx, [bsRootDirSects]
BS_02:		; 24/10/2023
		; 22/10/2023
		;mov	cl, [bsRootDirSects]	; Root Directory Sectors
		;xor	dx, dx ; 0

		; 22/10/2023 ; *-*
		;mov	bx, root_dir_buff       ; Destination offset = 7E00h
		
		;mov	cl, 1	; read 1 root dir sector
		;call	hd_read
		; 22/10/2023
		call	hd_read_1
		jc	short BS_05		; Disk read error message
	
		; 24/10/2023
		; ax = next root directory sector

		; 22/10/2023
		;mov	bx, [bsRootDirEnts]	; Number of root dir entries
		mov	bx, [bsDirEntsPerSec]
		mov	si, root_dir_buff
BS_03:          
		; 24/10/2023
		cmp	[si], bh ; 0
		;cmp	byte [si], 0		; Is it null entry?
		je	short BS_04_ERR		; Jump if zero ( = )
		mov	cx, 11			; Size of file/directory name
		push	si
		mov	di, rtsfilename
		repe	cmpsb			; Repeat if ZF = 1, CX > 0
						; Cmp byte DS:SI with ES:DI
		pop	si
		je	short BS_07		; If the file name found
		dec	bx		    
		jz	short BS_04		; Jump if no next entry
		add	si, 32			; To next directory entry
		jmp	short BS_03             ; Jump for next sector
BS_04:
		; 22/10/2023
		;dec	word [bsRootDirSects]
		;jz	short BS_04_ERR
		;inc	word [bsRootDirStart]
		;jmp	short BS_02
		; 24/10/2023
		dec	dx
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
		;and	al, notvalidfmask       ; Is it a file, really?
		; 24/10/2023
		test	byte [si+0Bh], notvalidfmask
		jnz	short BS_04_ERR		; Jump if not
		; 24/10/2023
		;mov	ax, [si+1Ah]		; First cluster of the file
		mov	dx, [si+1Ah]
		; 22/10/2023
		;cmp	ax, 2			; Start cluster
		;jb	short BS_05

  		;mov	[bsReserved2], ax	; Save the first cluster
		; 22/10/2023
		; 24/10/2023
		;push	ax 	

		; Load  RTS (Kernel) file
		mov	si, Loading_Msg
		call	print_msg

		; 22/10/2023
		;; Load FAT
		;;mov	ax, 1
		;mov	ax, [bsResSectors]	; FAT Location
		;mov	cl, [bsFATsecs]		; FAT Sectors
		;mov	bx, FAT_Buffer
		;call	hd_read
		;jc	short BS_05

		; 22/10/2023
		;; Load RTS (Kernel) file
		;mov	si, Loading_Msg
		;call	print_msg
		
		; 24/10/2023
		;pop	ax
		xchg	ax, dx ; mov ax, dx
		;;mov	ax, [bsReserved2]	; The First cluster
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

		; 22/10/2023
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
		; 22/10/2023
		mov	cl, 1	; read 1 sector (only)
		mov	bx, root_FAT_buffer ; *-* ; 7E00h
hd_read:
		; Only for FAT12 file systems !
		
		; 18/05/2018
		; 22/10/2023
		; AX = LBA address (as partition offset)
		; CL = sector count
		;; DL = disk drive number (80h)
		; ES:BX = buffer address
		
		mov	ch, 4 ; mov byte [RetryCount], 4
BS_11:
		pusha	; !*

		; 22/10/2023
		sub	dx, dx ; 0
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
              	; Only for FAT12 File Systems

		; 22/10/2023
		; bp = 0FFFEh (sp)
BS_13:	
        	;mov	[File_Cluster], ax
		; 22/10/2023
		mov	di, ax ; file (current) cluster

		dec	ax			; First cluster is cluster 2
		dec	ax
	
		; 18/05/2018
		xor	ch, ch ; 0
		mov	cl, [bsSecPerClust]
		mul	cx
		; 22/10/2023
		;sub	ch, dl	; dx must be 0
		;;cmp	ch, dl
		;jb	short BS_16 ; > 32MB (It must not be a FAT12 file system)
				    ; ((It is possible but nonsense!)) 	
BS_14:
		add	ax, [bsDataStart]	; Beginning sector of Data
		; 22/10/2023
		;jc	short BS_16

		;mov	dl, [bsDriveNumber]	; Disk number (= 80h)
	
		call	hd_read
		jc	short BS_16
get_next_cluster:
		; 22/10/2023
		;push	bx
		;;push	dx
		;mov	ax, [File_Cluster]
		mov	ax, di ; file (current) cluster
		;mov	bx, ax
		;; Multiply by 3
		;;mov	dx, 3
		;;mul	dx
               	;add	ax, ax
		;add	ax, bx
		; 22/10/2023
		shr	ax, 1  ; Divide by 2
		;xchg	bx, ax
		add	ax, di ; AX * 1.5 = AX + AX/2
		; dx = 0
		; ax = byte offset in the FAT
		div	word [bsBytesPerSec]
		; ax = FAT sector (index) number
		; dx = offset in FAT sector buffer
		
		; 22/10/2023
		mov	si, dx
		; si = offset in FAT sector buffer
		cmp	ax, bp ; previous FAT sector in the FAT buffer
		je	short SplitChk ; no need to read it again
		call	NextFATSec1 ; 25/10/2023
        	jc	short BS_16
SplitChk:
		mov	cx, FAT_Buffer-1
		inc	si
		cmp	si, [bsBytesPerSec] ; 512 ?
		jne	short ClusterOk ; jb short ClusterOk

		; 24/10/2023
		; ax = next FAT sector (at 'hd_read' return)

		; mov	byte [FAT_Buffer+511] to [FatBuffer-1] (as AL)
		
		; 24/10/2023
		add	si, cx
		;lodsb
		mov	dl, [si]
		mov	si, cx
		;mov	[si], al ; mov [7DFFh], al
		mov	[si], dl

		; read next sector and then
		; use first byte of the next sector (as AH)

		; 24/10/2023		
		;;mov	ax, bp	; 2 byte opcode
		;xchg	ax, bp  ; 1 byte opcode
		;inc	ax ; next FAT sector

		; 25/10/2023
		inc	bp

		call	NextFATSec2 ; 25/10/2023
		jnc	short EvenOdd  ; si = 7DFFh
		retn
ClusterOk:
		; si = 1 to 511
		; cx = FAT_Buffer-1
		add	si, cx
EvenOdd:
		lodsw
		
		; di = current cluster
		shr	di, 1
		jnc	short BS_17
		mov	cl, 4
               	shr	ax, cl
BS_15:
		; 22/10/2023
		cmp	ax, 0FF7h
		jb	short BS_13
		; EOCC (kernel file has been loaded successfully)
BS_16:
		retn
BS_17:
		and	ah, 0Fh
		jmp	short BS_15

		; 25/10/2023
		; 22/10/2023
NextFATSec1:
		mov	bp, ax
		; 25/10/2023
		add	ax, [bsResSectors]	; FAT sector LBA
NextFATSec2:
		push	es ; +  ; kernel file buffer segment
		push	bx ; ++ ; save kernel file buffer address

		push	ds
		pop	es

		; 22/10/2023 ; *-*
		;mov	bx, FAT_Buffer

		; 25/10/2023
		;add	ax, [bsResSectors]	; FAT sector LBA
		;sub	dx, dx ; 0
		;mov	cl, 1			; Read 1 FAT Sector
		;call	hd_read
		; 22/10/2023
		call	hd_read_1
 		pop	bx ; ++ ; restore kernel file buffer address
		pop	es ; +  ; kernel file buffer segment

		retn

; ---------------------------------------------------------------------
	
		; 22/10/2023
Error_Msg:
		db	0Dh, 0Ah
		;db	'DOS Kernel Loading Error!'
		; 22/10/2023
		db	'Error!'

Replace_Msg:    db	0Dh, 0Ah
		;db	'Replace the disk and press a key to reboot.'
		; 22/10/2023
		;db	'Replace disk and hit a key to reboot.'
		; 25/10/2023
		db	'Replace disk & press a key to reboot.'
		db	0Dh, 0Ah,0

		; 25/10/2023 (filler)
		db	0

Loading_Msg:    db	0Dh, 0Ah
		;db	"Loading Kernel MSDOS.SYS ..."
		db	"Loading Kernel PCDOS.SYS ..." ; 20/04/2024
		db	0Dh, 0Ah, 0

		; 24/10/2023
;File_Cluster:
		dw	417
		;dw	2023
rtsfilename:
		;db	'MSDOS   SYS'
		db	'PCDOS   SYS' ; 20/04/2024
		db	0

		times	510 - ($ - $$) db 0

bootsignature:  db	55h, 0AAh