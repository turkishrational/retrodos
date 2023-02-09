; ****************************************************************************
; FDBS360.ASM - Retro DOS v2.0 - FAT12 FD BOOT SECTOR (5 1/4", 360K diskette)
; ----------------------------------------------------------------------------
; Last Update: 27/03/2018 (Retro DOS 2.0) -different boot sector code-
; Last Update: 23/02/2018
; ----------------------------------------------------------------------------
; Beginning: 12/02/2018
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 (fdbs360.s)
; ----------------------------------------------------------------------------
; Retro DOS Operating System Project by ERDOGAN TAN (Beginning: 04/02/2018)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v2.0 (386) boot sector source code 
; by Erdogan Tan. 'trfdbs.s' (12/02/2018)
; ****************************************************************************

notvalidfmask   equ	0018h

root_dir_buff   equ	7E00h

rts_segment     equ	1000h  ; Retro DOS 2.0 ; 27/03/2018

;rts_segment	equ	0060h  ; Retro DOS 1.0

FAT_Buffer      equ	7E00h

[BITS 16]
[ORG 7C00h]

                jmp     short BS_01
                nop

; BootSector Identification (Data) Block

bsOemName:      db 'RETRODOS'          
bsBytesPerSec:  dw 512
bsSecPerClust:  db 2
bsResSectors:   dw 1
bsFATs:         db 2
bsRootDirEnts:  dw 112
bsSectors:      dw 720
bsMedia:        db 0FDh
bsFATsecs:      dw 2
bsSecPerTrack:  dw 9
bsHeads:        dw 2
bsHidden1:      dw 0
bsHidden2:      dw 0
bsHugeSectors:  dd 720
bsDriveNumber:  db 0
bsReserved1:    db 0
bsBpbSignature: db 29h                 
bsVolumeID:     dd 0
bsVolumeLabel:  db 'NO NAME    '
bsFileSysType:  db 'FAT12   '
bsReserved2:	dw 'v2'	; Retro DOS 2.0 ; 27/03/2018 
; Retro DOS 1.0 Extensions
;bsReserved2:	dw 'v1'
bsDataStart:    dw 12
bsRootDirStart:	dw 5
bsRootDirSects:	dw 7         

BS_01:          
                mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax
		mov     sp, 0FFFEh
		sti

		; overwrite fd drive number !
                mov	[bsDriveNumber], dl 	; drive number from INT 19h
						; (DL = 0)

                ;mov	ax, 5                  ; Root Dir Location
		mov	ax, [bsRootDirStart]
		;mov	cx, 7			; Root Directory Sectors
                mov	cx, [bsRootDirSects]
		mov     bx, root_dir_buff       ; Destination offset = 700h
                mov     si, bx
                call    fd_read
		jc      short BS_04		; Disk read error message

                ;mov	bx, 112                 ; Number of root dir entries
		mov	bx, [bsRootDirEnts]
BS_02:          
		cmp     byte [si], 0		; Is it null entry?
                je      short BS_03		; Jump if zero ( = )
                mov     cx, 11			; Size of file/directory name
                push    si
                mov     di, rtsfilename   
                repe    cmpsb                   ; Repeat if ZF = 1, CX > 0
						; Cmp byte DS:SI with ES:DI
                pop	si
                je      short BS_06		; If the file name found
                dec     bx                    
                jz      short BS_03		; Jump if no next entry
                add     si, 32                  ; To next directory entry
                jmp     short BS_02             ; Jump for next sector
BS_03:
                mov     si, Replace_Msg
                jmp     short BS_05
BS_04:      
                mov     si, Error_Msg
BS_05:          
                call    print_msg

                xor	ax, ax
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						; AH-scan code AL-char code
		int	19h			; Reboot

BS_06:
                mov     al, [si+0Bh]		; Move attributes byte to BL
                and     al, notvalidfmask       ; Is it a file, really?
                jnz     short BS_03		; Jump if not
                mov     ax, [si+1Ah]		; First cluster of the file
                cmp     ax, 2                   ; Start cluster
                jb      short BS_04

                mov	[bsReserved2], ax	; Save the first cluster
    
		; Load FAT
                ;mov	ax, 1                   ; FAT Location
                mov	ax, [bsResSectors]
		;mov	cx, 2                   ; FAT Sectors
                mov	cx, [bsFATsecs]
		mov     bx, FAT_Buffer    
                call    fd_read
                jc      short BS_04

		; Load  RTS (Kernel) file
                mov     si, Loading_Msg
                call    print_msg
                mov     ax, [bsReserved2]	; The First cluster
                mov     bx, rts_segment
                mov     es, bx
                xor     bx, bx
                call    load_file
                jc      short BS_04

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
             
		mov 	bp, 7C00h

		; Retro DOS 2.0 ; 27/03/2018
		mov	dh, [bsMedia]
		mov	ax, [bsSectors] ; 720 sectors, 360 KB

		jmp	rts_segment:0

                ;db	0EAh
                ;dw	0
                ;dw	rts_segment

print_msg:
BS_07:
		lodsb				; Load byte at DS:SI to AL
                and     al, al            
                jz	short BS_09		; If AL = 00h then stop

                mov     ah, 0Eh
                mov     bx, 07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						; AL-char BH-page BL-color
                jmp     short BS_07

BS_08:         
                dec	byte [RetryCount]
		jz	short BS_09 ; cf = 1

		; Reset disk system
		push	ax
		xor	ah, ah
                ;mov	dl, [bsDriveNumber]
		int     13h
                pop	ax
		jnc	short BS_10
BS_09:
		retn
 
fd_read:
                ; Only for FAT12 Floppy Disks
                
                mov     byte [RetryCount], 4
BS_10:
                push    cx
                push    ax                      ; PHYSICAL ADRESS CALCULATION
                ;mov	cl, 9			; Sectors per track
                mov	cl, [bsSecPerTrack]
		div     cl                      
                mov     cl, ah                  ; Sector (zero based)
                inc     cl                      ; To make it 1 based
                ;xor	ah, ah
                ;mov	dl, 2			; Heads 
                ;div	dl
                                                ; AL=cyl, AH=head, CL=sector
                ;mov	dh, ah
		sub	dh, dh
		shr	al, 1
		adc	dh, 0
		;mov	dl, [bsDriveNumber]	; (!DL has not been changed!)
                mov     ch, al            

                mov     ax, 0201h
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						; AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						; CF-flag AH-stat AL-sec read
		pop	ax			
                pop     cx
                jc      short BS_08
                inc     ax
	                
                ;add	bx, 512
	        add	bh, 2
		jnc	short BS_11
		push	bx
		mov	bx, es
		;add	bx, 1000h
		add	bh, 10h
		mov	es, bx
		pop	bx
BS_11:
                ;dec	cx
                dec	cl
		jnz	short fd_read
		retn

load_file:
              ; ES:BX = File Buffer
              ; AX = First Cluster Number
              ; Only for FAT12 Floppy Disks
BS_12:
                mov     [File_Cluster], ax
                dec     ax                      ; First cluster is cluster 2
                dec     ax
		               
                ;mov	cx, 2                   ; Sector count
                
		mov	cl, [bsSecPerClust]
		; ch = 0
		test	cl, 2
		jz	short BS_13
		shl	ax, 1

		;add	ax, 12                  ; Beginning sector of Data
BS_13:
                add	ax, [bsDataStart]
		call    fd_read
                jc      short BS_15
get_next_cluster:
		push	bx
                mov     ax, [File_Cluster]
                mov     bx, ax
		; Multiply by 3
		;mov	dx, 3
		;mul	dx
               	add	ax, ax
		add	ax, bx
                shr     ax, 1 ; Divide by 2
                xchg    bx, ax
                ; BX = Buffer Byte Offset
                ; AX = Current Cluster
                mov     ax, [FAT_Buffer+bx]
                jnc     short BS_16
                shr     ax, 4
BS_14:
		pop	bx
                cmp     ax, 0FF7h
		jb	short BS_12
		; EOCC (kernel file has been loaded successfully)
BS_15:
                retn
BS_16:
                and     ah, 0Fh
                jmp     short BS_14

Reserved3:	db	20h
RetryCount:     db      18h

rtsfilename:
                db      'MSDOS   SYS'
                db      0

		db	07h  ; Filler

Error_Msg:
                db      0Dh, 0Ah
                db      'DOS Kernel Loading Error!'

Replace_Msg:    db      0Dh, 0Ah
                db      'Replace the disk and press any key to reboot.'
                db      0Dh, 0Ah,0

Loading_Msg:    db      0Dh, 0Ah
                db      "Loading Kernel MSDOS.SYS ..."
                db      0Dh, 0Ah, 0

		; Filler
		db	'RETRODOS.SYS'
		db	0

File_Cluster:  	dw	417

		times	510 - ($ - $$) db 0

bootsignature:  db      55h, 0AAh