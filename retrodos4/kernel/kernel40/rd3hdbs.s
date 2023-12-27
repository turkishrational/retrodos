; ****************************************************************************
; RD3HDBS.ASM - Retro DOS v3.0 FAT 16 (Hard Disk) BOOT SECTOR code
; ----------------------------------------------------------------------------
; Last Update: 19/10/2018
; ----------------------------------------------------------------------------
; Beginning: 18/10/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm rd3hdbs.s -l rd3hdbs.lst -o RD3HDBS.BIN)) 	
; ----------------------------------------------------------------------------
; Derived from 'rd2hdbs.s' Retro DOS v2.0 FAT 12 hd boot sector source code
; by Erdogan Tan (19/05/2018).
; ****************************************************************************
; incbin "RD3HDBS.BIN" (in 'fat16hdi.s')

notvalidfmask   equ	0018h

root_dir_buff   equ	7E00h

rts_segment     equ	1000h

FAT_Buffer      equ	7E00h

[BITS 16]
[ORG 7C00h]
                jmp     short BS_01
                nop

; BootSector Identification (Data) Block

bsOemName:      db 'RETRODOS'          
bsBytesPerSec:  dw 512
bsSecPerClust:  db 4
bsResSectors:   dw 1
bsFATs:         db 2
bsRootDirEnts:  dw 512
bsSectors:      dw 0
bsMedia:        db 0F8h
bsFATsecs:      dw 63
bsSecPerTrack:  dw 63
bsHeads:        dw 16
bsHidden1:      dw 63
bsHidden2:      dw 0
bsHugeSectors:  dd 0
bsDriveNumber:  db 80h
bsReserved1:    db 0
bsBpbSignature: db 29h                 
bsVolumeID:     dd 0
bsVolumeLabel:  db 'NO NAME    '
bsFileSysType:  db 'FAT16   '
;bsReserved2:	dw 'v2'	; Retro DOS 2.0 ; 27/03/2018 
bsReserved2:	dw 'v3'	; Retro DOS 3.0 ; 18/10/2018 
; Retro DOS 1.0 Extensions
;bsReserved2:	dw 'v1'
bsDataStart:    dw 159
bsRootDirStart:	dw 127
bsRootDirSects:	dw 32  

BS_01:
                mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax
		mov     sp, 0FFFEh
		sti

		; overwrite hd drive number !
                mov	[bsDriveNumber], dl 	; drive number from INT 19h
						; (DL = 80h)
		; Retro DOS v3.0 - 19/10/2018
		mov	dx, [bsSectors]
		and	dx, dx
		jz	short BS_02

		mov	[bsHugeSectors], dx
		xor	dx, dx
		;mov	[bsHugeSectors+2], dx

BS_02:
		mov	ax, [bsRootDirStart]	; Root Dir Location
                mov	cl, [bsRootDirSects]	; Root Directory Sectors

		mov     bx, root_dir_buff       ; Destination offset = 7E00h
                mov     si, bx
                call    hd_read
		jc      short BS_05		; Disk read error message
     
		mov	bx, [bsRootDirEnts]	; Number of root dir entries
BS_03:          
		cmp     byte [si], 0		; Is it null entry?
                je      short BS_04		; Jump if zero ( = )
                mov     cx, 11			; Size of file/directory name
                push    si
                mov     di, rtsfilename   
                repe    cmpsb                   ; Repeat if ZF = 1, CX > 0
						; Cmp byte DS:SI with ES:DI
                pop	si
                je      short BS_07		; If the file name found
                dec     bx                    
                jz      short BS_04		; Jump if no next entry
                add     si, 32                  ; To next directory entry
                jmp     short BS_03             ; Jump for next sector

BS_04:
                mov     si, Replace_Msg
                jmp     short BS_06
BS_05:      
                mov     si, Error_Msg
BS_06:          
                call    print_msg

                xor	ax, ax
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						; AH-scan code AL-char code
		int	19h			; Reboot

BS_07:
                mov     al, [si+0Bh]		; Move attributes byte to BL
                and     al, notvalidfmask       ; Is it a file, really?
                jnz     short BS_04		; Jump if not
                mov     ax, [si+1Ah]		; First cluster of the file
                cmp     ax, 2                   ; Start cluster
                jb      short BS_05

                mov	[bsReserved2], ax	; Save the first cluster

		; Load FAT
                ;mov	ax, 1
                mov	ax, [bsResSectors]	; FAT Location
                mov	cl, [bsFATsecs]		; FAT Sectors
		mov     bx, FAT_Buffer    
                call    hd_read
                jc      short BS_05

		; Load  RTS (Kernel) file
                mov     si, Loading_Msg
                call    print_msg
                mov     ax, [bsReserved2]	; The First cluster
                mov     bx, rts_segment
                mov     es, bx
                xor     bx, bx
                call    load_file
                jc      short BS_05

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
             
		mov 	bp, 7C00h

		; Retro DOS 3.0 ; 19/10/2018
		mov	dl, [bsDriveNumber]
		; Retro DOS 2.0 ; 27/03/2018
		mov	dh, [bsMedia]
		mov	ax, [bsSectors]

		jmp	rts_segment:0

                ;db	0EAh
                ;dw	0
                ;dw	rts_segment

print_msg:
BS_08:
		lodsb			; Load byte at DS:SI to AL
                and     al, al            
                jz	short BS_10	; If AL = 00h then stop

                mov     ah, 0Eh
                mov     bx, 07h             
		int	10h		; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					; AL-char BH-page BL-color
                jmp     short BS_08

BS_09:     
                dec	ch  ;  dec byte [RetryCount]
		jz	short BS_10 ; cf = 1

		; Reset disk system
		push	ax
		xor	ah, ah
                ;mov	dl, [bsDriveNumber]
		int     13h
                pop	ax
		jnc	short BS_11
BS_10:
		retn
 
hd_read:
                ; Only for FAT16 file systems !
		
		; 19/10/2018
		
		; DX:AX = LBA address (as partition offset)
		; CL = sector count
		; ES:BX = buffer address
                
		mov	ch, 4 ; mov byte [RetryCount], 4
BS_11:
		pusha

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
		mov	dl, [bsDriveNumber] ; Drive number (80h)
		mov	ax, 201h
		int	13h		; BIOS Service func ( ah ) = 2
					; Read disk sectors
					; AL-sec num CH-track CL-sec
					; DH-head DL-drive ES:BX-buffer
					; CF-flag AH-stat AL-sec read
		popa
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
BS_13:
                mov     [File_Cluster], ax
                dec     ax                      ; First cluster is cluster 2
                dec     ax
	
		; 18/05/2018
		xor	ch, ch
		mov	cl, [bsSecPerClust]
		mul	cx

BS_14:
                add	ax, [bsDataStart]	; Beginning sector of Data
		adc	dx, 0 ; 19/10/2018
		;mov	dl, [bsDriveNumber]	; Disk number (= 80h)
	
		call    hd_read
                jc      short BS_15
get_next_cluster:
		; 19/10/2018
		push	bx
                mov     bx, [File_Cluster]
		shl     bx, 1 ; *2
                ; BX = Buffer Byte Offset
                ; AX = Current Cluster
                mov     ax, [FAT_Buffer+bx]
		pop	bx
                cmp     ax, 0FFF7h
		jb	short BS_13
		; EOCC (kernel file has been loaded successfully)
BS_15:
                retn

rtsfilename:
		db	'MSDOS   SYS'
		db      0

		;db	07h  ; Filler
		db	'RD3'
		db	0
Error_Msg:
                db      0Dh, 0Ah
                db      'DOS Kernel Loading Error!'

Replace_Msg:    db      0Dh, 0Ah
                db      'Replace the disk and press any key to reboot.'
                db      0Dh, 0Ah,0

Reserved3:	db	20h
RetryCount:     db      18h

Loading_Msg:    db      0Dh, 0Ah
                db      "Loading Kernel MSDOS.SYS ..."
                db      0Dh, 0Ah, 0

		; Filler
		;db	'RETRODOS.SYS'
		;db	0

File_Cluster:  	dw	417

		times	510 - ($ - $$) db 0

bootsignature:  db      55h, 0AAh