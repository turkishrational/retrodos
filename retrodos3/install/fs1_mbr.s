; ****************************************************************************
; FS1_MBR.ASM (FS1_MBR.BIN) - SINGLIX FS1 MASTERBOOT SECTOR for TRDOS 386 OS
; ****************************************************************************
; Last Update: 22/12/2017 (2nd version) -TRDOS 386 FAT & SINGLIX FS-
; ----------------------------------------------------------------------------
; Beginning: 26/04/2009 (1st version) -WINDOWS XP MBR & TRDOS 8086 SINGLIX FS-
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
; ****************************************************************************
; nasm fs1_mbr.s -l fs1_mbr.lst -o FS1_MBR.BIN
; ----------------------------------------------------------------------------
;
;#############################################################################
;
; SINGLIX FS1 MASTER BOOT RECORD  (c) Erdogan Tan [26/04/2009]
;
; Derived from WINDOWS XP MASTER BOOT RECORD
;
; Modified for SINGLIX Operating System v1.0b and SINGLIX FS1 File System v1.0
; ! WINDOWS XP compatible MBR code !
;
; Original code by (c) Microsoft Corporation 2000 [ Windows XP SP2 2004 ]
; DMADMIN.EXE (4 August 2004) offset 216616 (512 byte) (XP_MASTERBOOT.BIN)
; WINDOWS XP/SP2 Turkish OS Master Boot code (masterboot.bin)
; Singlix Code Grabber v2.0 [binfilehex.exe] XP_MASTERBOOT.BIN, masterboot.bin

; Microsoft Macro Assembler 
; compatible assembly source code 
; for SINGLIX FS1 MASTER BOOT CODE (c) Erdogan Tan 2009
;
; Disassembled by Erdogan Tan [26/04/2009] as Microsoft MASM compatible code
; by using DASM.EXE [24/3/1993] and PV.EXE [22/1/1993]
;
; Some explanations from 
;"Win2k Master Boot Record (MBR) Revealed!" html page by (c) Daniel B. Sedory
; http://www.geocities.com/thestarman3/asm/mbr/Win2kmbr.htm 
; http://mirror.href.com/thestarman/asm/mbr/Win2kmbr.htm

;#############################################################################

[BITS 16]	; 16 bit (x86, real mode) code

;#############################################################################
;#
;#		ENTRY POINT
;#
;#############################################################################

		[ORG 600h]                     ; Final memory location 0:600h

;#############################################################################
;#
;#		PROCEDURE proc_start
;#
;#############################################################################

start:		
		xor	ax, ax			; db 33h, 0C0h
		mov	ss, ax			
		mov	sp, 7C00h		
		sti				; Enable interrupts
		push	ax			
		pop	es			
		push	ax			
		pop	ds			
		cld				; Clear direction flag
		mov	si, 7C1Bh		; offset 01Bh (label loc_01Bh)
		mov	di, 61Bh			
		push	ax			
		push	di			
		mov	cx, 1E5h			
		repz	movsb			; Repeat if ZF = 1, CX > 0
						; Move byte from DS:SI to ES:DI
		retf				; Return FAR

loc_01Bh:
		mov	bp, 7BEh                 ; db 0BDh, 0BEh, 07h 
		mov	cl, 4                    ; db 0B1h, 04h
loc_020h:	
		cmp	[bp+0], ch ; 0	
		jl	short loc_02Eh		; Jump if less ( < )
		jne	short loc_03Ah		; Jump if not equal ( != )
		add	bp, 10h			
		loop	loc_020h		; Loop if CX > 0

                ; Beginning of SINGLIX MBR code extension
		; SINGLIX MBR feature : Check Sector 1 for a valid boot record
                mov	bx, 7C00h
                mov	cx, 2
                mov	dx, 80h
loc_read_bootsector_location:
                mov	ax, 201h			
		
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;IN: AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;OUT: CF-flag AH-stat AL-sec read
		jc	short loc_check_bootsector_number
                
loc_check_bootsector_sign:
                ;cmp	word [7DFEh], 0AA55h	; Check for the Magic Word/Number
                cmp	word [data_7FEh+7600h], 0AA55h
                je	short loc_094h
loc_check_bootsector_number:		
                cmp	cx, 2
                jne	short loc_int18h
                ; SINGLIX MBR feature : Check Sector 63 for a valid boot record
                inc	dh      ; mov dh, 1  ; Head 1 	                
                mov     cl, dh  ; mov cl, 1
                jmp     short loc_read_bootsector_location
		; End of SINGLIX MBR code extension

loc_int18h:                
		int	18h			; Switch to BASIC (if there is!?)
loc_02Eh:		
		mov	si, bp			
loc_030h:		
		add	si, 10h			
		dec	cx			
		jz	short loc_04Fh		; Jump if zero (Jump if equal)
		cmp	[si], ch ; 0	
		je	short loc_030h		
loc_03Ah:		
; SINGLIX MBR MODIFICATION
                mov     si, msg_ipt		; Invalid partition table!

print_msg:	; Print Error Message 
; SINGLIX MBR MODIFICATION
loc_041h:		
		lodsb				; Load byte at DS:SI to AL
loc_042h:		
		cmp	al, 0			
		je	short loc_042h		
		mov	bx, 7			
		mov	ah, 0Eh			
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;IN: AL-char BH-page BL-color
		jmp	short loc_041h		
loc_04Fh:		
		mov	[bp+10h], cl ; 0	
		call	load_bootsector		; offset 09Bh		
		jnc	short loc_081h		
loc_057h:		
		inc	byte [bp+10h]  ; 1	
		cmp	byte [bp+4], 0Bh	; FAT 32 CHS partition	
		je	short loc_06Bh		
		cmp	byte [bp+4], 0Ch	; FAT 32 LBA partition	
		je	short loc_06Bh		
loc_066h:
; SINGLIX MBR MODIFICATION
                mov	si, msg_elos		; Error loading operating system!
                jmp	short print_msg		; Jump if not equal ( != )
loc_06Bh:
		add	byte [bp+2], 6		; Beginning sector (in track)
		add	word [bp+8], 6		; Start sector (LBA, low word)
		adc	word [bp+0Ah], 0	; ADD with carry (LBA, high word)
		call	load_bootsector		; Load FAT 32 backup boot record 		
		jc	short loc_066h			
loc_081h:		
		;cmp	word [7DFEh], 0AA55h	; Check for the Magic Word/Number
                cmp	word [data_7FEh+7600h], 0AA55h
                je	short loc_094h		
		cmp	byte [bp+10h], 0
		je	short loc_057h

; SINGLIX MBR MODIFICATION		
		mov	si, msg_mos		; Missing operating system!
                jmp	short print_msg      
loc_094h:		
                ; Beginning of SINGLIX MBR code extension
                ; SINGLIX MBR Feature/Sign 
                mov	ax, [singlix_mbr_sign]	; 417 
		; End of SINGLIX MBR code extension
                mov	di, sp			
		push	ds			
		push	di			
		mov	si, bp			
		retf				; Return FAR
                                                ; jump to the Boot Sector code

;#############################################################################
;#
;#		PROCEDURE proc_load_bootsector
;#
;#############################################################################

; Original WINDOWS XP MBR Boot Sector Reading/Loading PROCEDURE
; by Microsoft Corporation

load_bootsector:

loc_09Bh:
		mov	di, 5  			; Retry count			
		mov	dl, [bp+0] ; 80h	
		mov	ah, 8			
		int	13h			; BIOS Service func ( ah ) = 8
						; Get current drive parameters
						; OUT: DL-drvs DH-heads CL-sects
						; CH-tracks CF-flag AH-status
		jc	short loc_0CAh		
		mov	al, cl
		and	al, 3Fh			; Number of sectors		
		cbw				; Convert byte to word
		mov	bl, dh			; Number of heads - 1	
		mov	bh, ah ; 0			
		inc	bx			; Number of heads			
		mul	bx			; AX = sectors*heads
		mov	dx, cx			
		xchg	dl, dh			
		mov	cl, 6			
		shr	dh, cl			; Shift cyl bits 8&9 to bit 0&1
		inc	dx			; Number of cylinders			
		mul	dx			; DX_AX = cylinders*(sectors*heads)
		cmp	[bp+0Ah], dx		; Start sector < CHS total sects?	
		jnbe	short loc_0E6h		; no, let's use int 13h extensions		
		jb	short loc_0CAh		; yes, lets use CHS parameters 	
		cmp	[bp+8], ax		; Start sector < CHS total sects?	
		jnb	short loc_0E6h		; no, let's use int 13h extensions			
loc_0CAh:		
		mov	ax, 0201h			
		mov	bx, 7C00h				
		mov	cx, [bp+2]	
		mov	dx, [bp+0]	
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;IN: AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;OUT: CF-flag AH-stat AL-sec read
		jnc	short loc_12Bh		
		dec	di			
		jz	short loc_12Bh		
		
                xor	ah, ah			
		mov	dl, [bp+0]	
		int	13h			; BIOS Service func ( ah ) = 0
						; Reset disk system
		jmp	short loc_0CAh
		
loc_check_int13h_extensions:
loc_0E6h:		
		mov	dl, [bp+0]
		pusha				; db 60h
		mov	bx, 55AAh               ; db 0BBh, 0AAh, 55h
		mov	ah, 41h                 ; db 0B4h, 41h
		int	13h                     ; db 0CDh, 13h  
                jc	short loc_129h   
		
                cmp	bx, 0AA55h		; Extensions present
                jne	short loc_129h
                test	cl, 1			; Fixed disk access subset check
                jz	short loc_129h                   
		
               	popa				; db 61h
loc_0FFh:
		pusha                           ; db 60h

		push	0			; db 6Ah, 00h
		push	0			; db 6Ah, 00h
                push	word [bp+0Ah]		; db 0FFh, 76h, 0Ah
                push	word [BP+08h]		; db 0FFh, 76h, 08h
		push    0			; db 6Ah, 00h 
		push    7C00h			; db 68h, 00h, 7Ch
		push    1			; db 6Ah, 01h
		push    10h			; db 6Ah, 10h

		mov	ah, 42h
		mov	si, sp
                int	13h

		popa				; db 61h
		popa				; db 61h

                jnc	short loc_12Bh
                
		dec	di 

		jz	short loc_12Bh
                
                xor	ah, ah
                mov	dl, [bp+0]
                int	13h
                
		jmp	short loc_0FFh                  
 
loc_129h:
		popa				; db 61h

		stc				; db 0F9h
loc_12Bh:
		retn				; db 0C3h
			
              
; SINGLIX MBR MODIFICATION

; Error messages
msg_ipt: 
		db 	'Invalid partition table !'
		db	0
msg_elos:
                db	'Error loading operating system !'
	        db	0
msg_mos:
	        db 	'Missing operating system !' 
          	db	0

		dw	2017  ; SINGLIX FS1 MBR version

		times	1A1h-($-$$) db 0

singlix_mbr_sign:		; MBR offset 417
                dw     01A1h	; SINGLIX MBR sign (=417)

		db 	0

; SINGLIX MBR MODIFICATION 
; (for disk images, TRDOS 386, 2nd version)
; CHS parameters

pt_cylinders:			; C	
  		dw	0
pt_heads:			; H	
		dw	0
pt_sectors:			; S	
		dw	0	

		;org	7BEh
		;times	1BEh-($-$$) db 0
		times	1BCh-($-$$) db 0
		dw	7BEh
partition_table:
		times	64 db 0

		;org	7FEh
		;org 	7DFEh
		; 22/12/2017
		;times	1FEh-($-$$) db 0
bs_sign:
data_7FEh:	db	55h
                db	0AAh