; ****************************************************************************
; FDBS5A0H.ASM - Retro DOS v5.0 - FAT12 FD BOOT SECTOR (3 1/2", 1.44 MB)
; ----------------------------------------------------------------------------
; Last Update: 20/04/2024
; ----------------------------------------------------------------------------
; Beginning: 20/04/2024
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (fdbs5a0h.s)
; ----------------------------------------------------------------------------
; Retro DOS Operating System Project by ERDOGAN TAN (Beginning: 04/02/2018)
; ----------------------------------------------------------------------------
; Modified from Retro DOS v2.0-v4.2 boot sector code, 'fdbs1440.s', 25/10/2023
; ****************************************************************************

; fdbs1440.s - 25/10/2023

notvalidfmask   equ	0018h

root_dir_buff   equ	7E00h

rts_segment     equ	1000h  ; Retro DOS 2.0 ; 27/03/2018

;rts_segment	equ	0060h  ; Retro DOS 1.0

FAT_Buffer      equ	7E00h

; 24/10/2023
root_FAT_buffer equ	7E00h

[BITS 16]
[ORG 7C00h]
		jmp	short BS_01
		nop

; BootSector Identification (Data) Block

bsOemName:      db 'RETRODOS'          
bsBytesPerSec:  dw 512
bsSecPerClust:  db 1
bsResSectors:   dw 1
bsFATs:         db 2
bsRootDirEnts:  dw 224
bsSectors:      dw 2880
bsMedia:        db 0F0h
bsFATsecs:      dw 9
bsSecPerTrack:  dw 18
bsHeads:        dw 2
bsHidden1:      dw 0
bsHidden2:      dw 0
bsHugeSectors:  dd 2880
bsDriveNumber:  db 0
bsReserved1:    db 0
bsBpbSignature: db 29h		 
bsVolumeID:     dd 0
bsVolumeLabel:  db 'NO NAME    '
bsFileSysType:  db 'FAT12   '
;;bsReserved2:	dw 'v2'	; Retro DOS 2.0 ; 27/03/2018
;bsReserved2:	dw 'v4'	; Retro DOS 4.0 ; 24/10/2023
bsReserved2:	dw 'v5'	; Retro DOS 5.0 ; 20/04/2024
; Retro DOS 1.0 Extensions
;bsReserved2:	dw 'v1'
bsDataStart:    dw 33
bsRootDirStart:	dw 19
bsRootDirSects:	dw 14 

; 24/10/2023
bsDirEntsPerSec: dw 16         

BS_01:       
		mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax ; 0
		mov	sp, 0FFFEh
		sti

		; 24/10/2023
		;mov	bp, sp

		; 25/10/2023
		; (DL = boot drive number -from INT 19h-
		; DL value is not changed in this boot sector code.
		; No need to save boot drive number.)
		;
		; overwrite fd drive number !
		;mov	[bsDriveNumber], dl 	; drive number from INT 19h
;BS_02:
		mov	ax, [bsRootDirStart]	; Root Dir Location
		; 24/10/2023
		mov	bp, [bsRootDirSects]
BS_02:
		call	fd_read_1
		jc	short BS_05		; Disk read error message

		; 24/10/2023
		;;mov	ax, 19			; Root Dir Location
		;mov	ax, [bsRootDirStart]
		;;mov	cx, 14			; Root Directory Sectors
		;mov	cx, [bsRootDirSects]
		;mov	bx, root_dir_buff       ; Destination offset = 7E00h
		;mov	si, bx
		;call	fd_read
		;jc	short BS_04		; Disk read error message

		; 24/10/2023
		; ax = next root directory sector

		; 24/10/2023
		;mov	bx, 224
		;mov	bx, [bsRootDirEnts]	; Number of root dir entries
		mov	bx, [bsDirEntsPerSec]
		mov	si, root_dir_buff
BS_03:          
		; 24/10/2023
		cmp	byte [si], bh ; 0
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
		; 24/10/2023
		;dec	word [bsRootDirSects]
		;jz	short BS_04_ERR
		;inc	word [bsRootDirStart]
		;jmp	short BS_02
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
		;and	al, notvalidfmask       ; Is it a file, really?
		; 24/10/2023
		test	byte [si+0Bh], notvalidfmask
		jnz	short BS_04_ERR		; Jump if not
		mov	ax, [si+1Ah]		; First cluster of the file
		cmp	ax, 2			; Start cluster
		jb	short BS_05

		;mov	[bsReserved2], ax	; Save the first cluster
		; 24/10/2023
		push	ax

		; Load  RTS (Kernel) file
		mov	si, Loading_Msg
		call	print_msg
	     	
		; 24/10/2023
		;; Load FAT
		;;mov	ax, 1			; FAT Location
		;mov	ax, [bsResSectors]
		;;mov	cx, 9			; FAT Sectors
		;mov	cx, [bsFATsecs]
		;mov	bx, FAT_Buffer    
		;call	fd_read
		;jc	short BS_05

		; 24/10/2023
		;; Load RTS (Kernel) file
		;mov	si, Loading_Msg
		;call	print_msg

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
		;mov	ax, rts_segment ; 60h
		;mov	ds, ax
		;mov	es, ax
             
		mov	bp, 7C00h

		; Retro DOS 3.0 ; 19/10/2018
		;mov	dl, [bsDriveNumber]
		; 24/10/2023
		; dl = [bsDriveNumber]
		
		; Retro DOS 2.0 ; 27/03/2018
		mov	dh, [bsMedia]
		mov	ax, [bsSectors] ; 720 sectors, 360 KB

		jmp	rts_segment:0

		;db	0EAh
		;dw	0
		;dw	rts_segment

print_msg:
BS_08:
		lodsb				; Load byte at DS:SI to AL
		and	al, al            
		jz	short BS_10		; If AL = 00h then stop

		mov	ah, 0Eh
		mov	bx, 07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						; AL-char BH-page BL-color
		jmp	short BS_08

BS_09:	
		; 24/10/2023   
		dec	ch
		;dec	byte [RetryCount]
		jz	short BS_10 ; cf = 1

		; 24/10/2023
		;; Reset disk system
		;push	ax
		;xor	ah, ah
		;;mov	dl, [bsDriveNumber]
		;int	13h
		;pop	ax
		;jnc	short BS_12

		; 24/10/2023
		push	ax
		xor	ax, ax 
		; ah = 0 ; INT 13h reset function
		int	13h
		pop	ax
		jnc	short BS_11
BS_10:
		retn
 
fd_read_1:
		; 24/10/2023
		mov	cl, 1	; read 1 sector (only)
		mov	bx, root_FAT_buffer ; *-* ; 7E00h
fd_read_2:
;fd_read:
		; Only for FAT12 Floppy Disks
		
		; 24/10/2023
		mov	ch, 4
		;mov	byte [RetryCount], 4
BS_11:
		push	cx
		push	ax			; PHYSICAL ADRESS CALCULATION
		;mov	cl, 18			; Sectors per track
		mov	cl, [bsSecPerTrack]
		div	cl		      
		mov	cl, ah			; Sector (zero based)
		inc	cl			; To make it 1 based
		;xor	ah, ah
		;mov	dl, 2			; Heads 
		;div	dl
						; AL=cyl, AH=head, CL=sector
		;mov	dh, ah
		sub	dh, dh
		shr	al, 1
		adc	dh, 0
		mov	ch, al            

		;mov	dl, [bsDriveNumber]	; (!DL has not been changed!)
		; 24/10/2023
		; dl = [bsDriveNumber]

		mov	ax, 0201h
		
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						; AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						; CF-flag AH-stat AL-sec read
		pop	ax			
		pop	cx
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
		jnz	short fd_read_2	; 24/10/2023
		retn

load_file:
		; ES:BX = File Buffer
		; AX = First Cluster Number
		; Only for FAT12 Floppy Disks

		; 24/10/2023
		; bp = 0FFFEh (sp)
		; 24/10/2023
		mov	bp, sp ; 0FFFEh
BS_13:
		;mov	[File_Cluster], ax
		; 24/10/2023
		mov	di, ax ; file (current) cluster

		dec	ax			; First cluster is cluster 2
		dec	ax
		               
		;mov	cx, 1			; Sector count
		
		mov	cl, [bsSecPerClust]
		; ch = 0
		test	cl, 2
		jz	short BS_14
		shl	ax, 1

		;add	ax, 12			; Beginning sector of Data
BS_14:
		add	ax, [bsDataStart]
		;call	fd_read
		; 24/10/2023
		call	fd_read_2
		jc	short BS_16
get_next_cluster:
		; 24/10/2023
		;push	bx
		;mov	ax, [File_Cluster]
		;mov	bx, ax
		;; Multiply by 3
		;;mov	dx, 3
		;;mul	dx
               	;add	ax, ax
		;add	ax, bx

		; 24/10/2023
		push	dx
		xor	dx, dx ; 0	
		mov	ax, di ; file (current) cluster
		shr	ax, 1  ; Divide by 2
		add	ax, di ; AX * 1.5 = AX + AX/2
		; dx = 0
		; ax = byte offset in the FAT
		div	word [bsBytesPerSec]
		; ax = FAT sector (index) number
		; dx = offset in FAT sector buffer
		mov	si, dx
		pop	dx		

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
		; ax = next FAT sector (at 'fd_read' return)
		
		; 24/10/2023
		add	si, cx
		;lodsb
		mov	dh, [si]
		mov	si, cx
		;mov	[si], al ; mov [7DFFh], al
		mov	[si], dh

		; read next sector and then
		; use first byte of the next sector (as AH)
		
		; 24/10/2023
		;;mov	ax, bp	; 2 byte opcode
		;xchg	ax, bp  ; 1 byte opcode
		
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

		; 24/10/2023
		;shr	ax, 1 ; Divide by 2
		;xchg	bx, ax
		;; BX = Buffer Byte Offset
		;; AX = Current Cluster
		;mov	ax, [FAT_Buffer+bx]
		;jnc	short BS_17
		;shr	ax, 4
BS_15:
		; 24/10/2023
		;pop	bx
		
		cmp	ax, 0FF7h
		jb	short BS_13	; 25/10/2023
		; EOCC (kernel file has been loaded successfully)
BS_16:
		retn
BS_17:
		and	ah, 0Fh
		jmp	short BS_15

		; 24/10/2023
NextFATSec1:
		mov	bp, ax
		; 25/10/2023
		add	ax, [bsResSectors]	; FAT sector LBA
NextFATSec2:
		push	es ; +  ; kernel file buffer segment
		push	bx ; ++ ; save kernel file buffer address

		push	ds
		pop	es

		; 24/10/2023 ; *-*
		;mov	bx, FAT_Buffer	

		; 25/10/2023
		;add	ax, [bsResSectors]	; FAT sector LBA
		;mov	cl, 1			; Read 1 FAT Sector
		call	fd_read_1
 		pop	bx ; ++ ; restore kernel file buffer address
		pop	es ; +  ; kernel file buffer segment

		retn

		; 24/10/2023 ; Filler
		dw	0
		dw	20
		dw	4
		dw	2024	; 20/04/2024
		dw	0
Error_Msg:
		db	0Dh, 0Ah
		;db	'DOS Kernel Loading Error!'
		; 24/10/2023
		db	'Error!'
Replace_Msg:    
		db	0Dh, 0Ah
		db	'Replace the disk and press any key to reboot.'
		db	0Dh, 0Ah,0

Loading_Msg:    
		db	0Dh, 0Ah
		;db	"Loading Kernel MSDOS.SYS ..."
		db	"Loading Kernel PCDOS.SYS ..." ; 20/04/2024
		db	0Dh, 0Ah, 0

;File_Cluster: 
		dw	417	; (filler)

rtsfilename:
		;db	'MSDOS   SYS'
		db	"PCDOS   SYS"	; 20/04/2024
		db	0

		times	510 - ($ - $$) db 0

bootsignature:  db	55h, 0AAh