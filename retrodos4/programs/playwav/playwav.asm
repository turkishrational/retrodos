; ****************************************************************************
; PLAYWAV.ASM - ICH AC97 .wav player for DOS.			   PLAYWAV.COM
; ----------------------------------------------------------------------------
; Last Update: 17/02/2017
; ----------------------------------------------------------------------------
; Beginning: 17/02/2017
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11
;	     nasm playwav.asm -l playwav.lst -o PLAYWAV.COM	
; ----------------------------------------------------------------------------
; Derived from '.wav file player for DOS' Jeff Leyda, Sep 02, 2002 
; ****************************************************************************
; Modidified from 'PLAYER.COM' for VIA VT8233 wav player source code by
; (PLAYER.ASM) by Erdogan Tan (07/11/2016 - 08/12/2016)

[BITS 16]

[ORG 100h] 

	%include 'ac97.inc' ; 17/02/2017

_STARTUP:

; memory allocation

        call    setFree				; deallocate unused DOS mem

	; 17/02/2017
	; Clear BSS (uninitialized data) area
	xor	ax, ax ; 0
	mov	cx, (EOF - bss_start)/2
	mov	di, bss_start
	rep	stosw

; allocate 256 bytes of data for DCM_OUT Buffer Descriptor List. (BDL)

        mov     ax, BDL_SIZE / 16
        call    memAlloc
        mov     [BDL_BUFFER], ax		; segment 

; allocate 2 buffers, 64k each for now.

        mov     ax, BUFFERSIZE / 16		; 64k for .WAV file
        call    memAlloc
        mov     [WAV_BUFFER1], ax		; segment

	mov	ax, BUFFERSIZE / 16
	call	memAlloc
	mov	[WAV_BUFFER2], ax

; Detect/reset AC97 
;
        call    pciFindDevice
        jnc     short _1

; couldn't find the audio device!

	push	cs
	pop	ds
        mov     dx, noDevMsg
        mov     ah, 9
        int     21h
        jmp     exit

; 17/02/2017
noDevMsg db "Error: Unable to find intel ICH based audio device!",CR,LF,"$"

_1:
	; eax = BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; edx = DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	mov	[bus_dev_fn], eax
	mov	[dev_vendor], edx

	; get ICH base address regs for mixer and bus master

        mov     al, NAMBAR_REG
        call    pciRegRead16			; read PCI registers 10-11
        and     dx, IO_ADDR_MASK 		; mask off BIT0

        mov     [NAMBAR], dx			; save audio mixer base addy

	mov     al, NABMBAR_REG
        call    pciRegRead16
        and     dx, IO_ADDR_MASK

        mov     [NABMBAR], dx			; save bus master base addy

	; init controller
	; 17/02/2017
	mov	al, PCI_CMD_REG ; command register (04h)
	call	pciRegRead16 ; pciRegRead8

	; eax = BUS/DEV/FN/REG
	;  dx = PCI Command Register Content ; 17/02/2017
	; 	00000000CCCCCCCC
	mov	[stats_cmd], dx

	mov	al, PCI_IO_BASE ; IO base address register (10h)
	call	pciRegRead32

	and     dx, 0FFC0h	; IO_ADDR_MASK (0FFFE) ?
        mov     [ac97_io_base], dx

	mov	al, AC97_INT_LINE ; Interrupt line register (3Ch)
	call	pciRegRead8 ; 17/02/2017

	mov     [ac97_int_ln_reg], dl

	; 28/11/2016
	mov	bx, 1
	xor	dh, dh 	 ; 17/02/2017
	mov	cx, dx
	shl	bx, cl

	;not	bx
	in	al, 0A1h ; irq 8-15
        mov	ah, al
        in	al, 21h  ; irq 0-7 
	;and	ax, bx   ; unmask
 	btr	ax, dx	 ; unmask
	out	21h, al  ; enable interrupt (if irq <= 7)
	mov	al, ah
	out	0A1h, al ; enable interrupt (if irq > 7)
	;not	bx

	mov	dx, 4D1h			;8259 ELCR1
        in	al, dx
	mov	ah, al
	mov	dx, 4D0h 
        in	al, dx
	;or	ax, bx        
	bts	ax, cx
	mov	dx, 4D0h
	out	dx, al                          ;set level-triggered mode
	mov	al, ah
	mov	dx, 4D1h
	out	dx, al                          ;set level-triggered mode

	; 24/11/2016 - Erdogan Tan
	mov	bx, cx
	;mov	bx, dx
	mov	bl, [bx+irq_int]
	shl	bx, 2 ; * 4

	; set up interrupt vector
	; 30/11/2016
	push	es
	xor	ax, ax
	mov	es, ax
	mov	word [es:bx], ac97_int_handler
	mov	ax, cs
	mov	[es:bx+2], ax
	pop	es
		
	call	write_ac97_dev_info 

; check the command line for a file to play

        ;push    ds
        call    processCmdline			; get the filename

; open the file
        mov     al, OPEN                        ; open existing file
        call    openFile                        ; no error? ok.
        ;pop     ds
        jnc     short _gsr

; file not found!

        ;push   cs
        ;pop    ds
        mov	dx, noFileErrMsg
        mov     ah, 9
        int     21h
        jmp     exit

noFileErrMsg  db "Error: file not found.",CR,LF,"$"

_gsr:
        call    getSampleRate                   ; read the sample rate
                                                ; pass it onto codec.
	jc	short exit ; 19/11/2016 - nothing to do

	mov	[sample_rate], ax
	; 19/11/2016
	mov	[stmo], cl
	mov	[bps], dl

	; 17/02/2017
	mov	byte [fbs_shift], 0 ; 0 = stereo and 16 bit 
	dec	cl
	jnz	short _gsr_1 ; stereo
	inc	byte [fbs_shift] ; 1 = mono or 8 bit		
_gsr_1:	
	cmp	dl, 8 
	ja	short _gsr_2 ; 16 bit samples
	inc	byte [fbs_shift] ; 2 = mono and 8 bit
_gsr_2:	
	call	write_sample_rate

	; 17/02/2017
	mov	dx, [stats_cmd]
        or      dl, IO_ENA+BM_ENA               ; enable IO and bus master
        call    pciRegWrite16 ; pciRegWrite8

; setup the Codec (actually mixer registers) 
        call    codecConfig                     ; unmute codec, set rates.

;
; position file pointer to start in actual wav data
; MUCH improvement should really be done here to check if sample size is
; supported, make sure there are 2 channels, etc.  
;
        mov     ah, 42h
        mov     al, 0                           ; from start of file
        mov     bx, [filehandle]
        xor     cx, cx
        mov     dx, 44                          ; jump past .wav/riff header
        int     21h

; play the .wav file.  Most of the good stuff is in here.

        call    playWav

; close the .wav file and exit.

        call    closeFile

exit:
        mov     ax, 4c00h
	int 	21h

here:
	jmp	short here

; MEMALLOC.ASM
;-- SETFREE: Release memory not used  ----------------
;-- Input    : ES = address of PSP
;-- Output   : none
;-- Register : AX, BX, CL and FLAGS are changed 
;-- Info     : Since the stack-segment is always the last segment in an 
;              EXE-file, ES:0000 points to the beginning and SS:SP
;              to the end of the program in memory. Through this the
;              length of the program can be calculated 
; call this routine once at the beginning of the program to free up memory
; assigned to it by DOS.

setFree:
	  mov	bx, 65536/16	; 4K paragraphs ; 17/02/2017 (Erdogan Tan)		

          mov	ah, 4ah		;pass new length to DOS
          int	21h

          retn			; back to caller 
				; new size (allocated memory) = 64KB

memAlloc:
; input: AX = # of paragraphs required
; output: AX = segment of block to use

	push	bx
	mov	bx, ax
	mov	ah, 48h
	int	21h
	pop	bx
	retn

; CMDLINE.ASM
; parse the command line
; entry: none
; exit: DS:DX to the 1st supplied item on the command line 

processCmdline:
        push    bx
        push    si

        ;mov    ah, 51h
        ;int    21h
        ;mov    ds, bx

        mov     si, 80h
        movzx   bx, byte [si]
        add     si, bx
        inc     si

        mov     byte [si], NULL         ; zero terminate

        mov     si, 81h

cmdlineloop:
        lodsb

        cmp     al, NULL                ; found end of line?
        je      short exitpc
        cmp     al, ' '                 ; found a space?
        je      short cmdlineloop

        ; must be the filename here.
exitpc:
        dec     si                      ; point to start of filename
        mov     dx, si
        pop     si
        pop     bx
	retn

; FILE.ASM
;open or create file
;
;input: ds:dx-->filename (asciiz)
;       al=file Mode (create or open)
;output: none  cs:[filehandle] filled
;
openFile:
	push	ax
	push	cx
	mov	ah, 3bh			; start with a mode
	add	ah, al			; add in create or open mode
	xor	cx, cx
	int	21h
	jc	short _of1
	;mov	[cs:filehandle], ax
	mov	[filehandle], ax
_of1:
	pop	cx
	pop	ax
	retn

; close the currently open file
; input: none, uses cs:[filehandle]
closeFile:
	push	ax
	push	bx
	cmp	word [filehandle], -1
	jz	short _cf1
	mov     bx, [filehandle]  
	mov     ax,3e00h
        int     21h              ;close file
_cf1:
	pop	bx
	pop	ax
	retn

getSampleRate:
	; 08/12/2016
; reads the sample rate from the .wav file.
; entry: none - assumes file is already open
	; 19/11/2016 - Erdogan Tan
; exit: ax = sample rate (11025, 22050, 44100, 48000)
;	cx = number of channels (mono=1, stereo=2)
;	dx = bits per sample (8, 16)

	push    bx

        mov     ah, 42h
        mov     al, 0				; from start of file
        mov     bx, [filehandle]
        xor     cx, cx
        mov     dx, 08h				; "WAVE"
        int     21h

        mov     dx, smpRBuff
        mov     cx, 28				; 28 bytes
	mov	ah, 3fh
        int     21h

	cmp	word [smpRBuff], 'WA'
	jne	short gsr_stc

	cmp	word [smpRBuff+2], 'VE'
	jne	short gsr_stc

	cmp	word [smpRBuff+12], 1	; Offset 20, must be 1 (= PCM)
	jne	short gsr_stc


	mov	cx, [smpRBuff+14]	; return num of channels in CX
        mov     ax, [smpRBuff+16]	; return sample rate in AX
	mov	dx, [smpRBuff+26]	; return bits per sample value in DX
gsr_retn:
        pop     bx
        retn

gsr_stc:
	stc
	jmp	short gsr_retn

%include 'ac97.asm' ; 29/11/2016 (AC97 codec configuration)
%include 'ich_wav.asm' ; 17/02/2017 (ICH AC97 wav playing functions)

; UTILS.ASM
;----------------------------------------------------------------------------
;       delay1_4ms - Delay for 1/4 millisecond.
;		    1mS = 1000us
;       Entry:
;         None
;       Exit:
;	  None
;
;       Modified:
;         None
;
PORTB			EQU	061h
  REFRESH_STATUS	EQU	010h		; Refresh signal status

delay1_4ms:
        push    ax 
        push    cx
        mov     cx, 16			; close enough.
	in	al,PORTB
	and	al,REFRESH_STATUS
	mov	ah,al			; Start toggle state
	or	cx, cx
	jz	short _d4ms1
	inc	cx			; Throwaway first toggle
_d4ms1:	
	in	al,PORTB		; Read system control port
	and	al,REFRESH_STATUS	; Refresh toggles 15.085 microseconds
	cmp	ah,al
	je	short _d4ms1		; Wait for state change

	mov	ah,al			; Update with new state
	dec	cx
	jnz	short _d4ms1

        pop     cx
        pop     ax
        retn

	; 13/11/2016 - Erdogan Tan
write_ac97_dev_info:
	; BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	xor	bh, bh
	mov	esi, [dev_vendor]
	mov	ax, si
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgVendorId+3], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgVendorId+2], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgVendorId+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgVendorId], al
	shr	esi, 16
	mov	ax, si
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgDevId+3], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgDevId+2], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgDevId+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgDevId], al

	mov	esi, [bus_dev_fn]
	shr	esi, 8
	mov	ax, si
	mov	bl, al
	mov	dl, bl
	and	bl, 7 ; bit 0,1,2
	mov	al, [bx+hex_chars]
	mov	[msgFncNo+1], al
	mov	bl, dl
	shr	bl, 3
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgDevNo+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgDevNo], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgBusNo+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgBusNo], al

	mov	ax, [ac97_io_base]
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr+3], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr+2], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr], al

	; 24/11/2016
	xor	ah, ah
	mov	al, [ac97_int_ln_reg]
	mov	cl, 10
	div	cl
	add	[msgIRQ], ax
	and	al, al
	jnz	short _pmi
	mov	al, [msgIRQ+1]
	mov	ah, ' '
	mov	[msgIRQ], ax
_pmi:
        mov	dx, msgAC97Info
        mov     ah, 9
        int     21h
        retn

write_sample_rate:
	; ax = sample rate (hertz)

	xor	dx, dx
	mov	cx, 10
	div	cx
	add	[msgHertz+4], dl
	sub	dx, dx
	div	cx
	add	[msgHertz+3], dl
	sub	dx, dx
	div	cx
	add	[msgHertz+2], dl
	sub	dx, dx
	div	cx
	add	[msgHertz+1], dl
	add	[msgHertz], al
	
        mov     dx, msgSampleRate
        mov     ah, 9
        int     21h

	; 19/11/2016
	mov	dx, msg16Bits
	cmp	byte [bps], 16
	je	short wsr_1
	mov	dx, msg8Bits
wsr_1:
        mov     ah, 9
        int     21h

	mov	dx, msgMono
	cmp	byte [stmo], 1
	je	short wsr_2
	mov	dx, msgStereo		
wsr_2:
        mov     ah, 9
        int     21h

        retn

;detect_codec:
;	; 13/11/2016 - Erdogan Tan (Ref: KolibriOS, codec.inc)
;	mov	eax, 7Ch
;	call	codec_read
;       shl     eax, 16
;       mov     [codec_id], eax
;
;	mov	eax, 7Eh
;       call	codec_read
;       or      eax, [codec_id]
;       mov     [codec_chip_id], eax
;       and     eax, 0FFFFFF00h
;
;       mov     edi, codecs
;_dcb:
;       mov     ebx, [di]
;       test    ebx, ebx
;       jz      short _dco_unknown
;
;       cmp     eax, ebx
;       jne     short _dco_next
;       mov     ax, [di+4]
;       mov     [codec_vendor_ids], ax
;       movzx   esi, ax
;       call	print_msg
;       
;	mov	ax, [di+6]
;	call	detect_chip
;       retn
;
;_dco_next:
;       add     di, 8
;       jmp     short _dcb
;
;_dco_unknown:
;       mov    word [codec_vendor_ids], ac_unknown
;       mov    word [codec_chip_ids], chip_unknown
;       mov     esi, chip_unknown
;	call	print_msg
;       mov     eax, [codec_chip_id]
;       call    dword2str
;       call	print_msg
;       retn

;detect_chip:
;	; 13/11/2016 - Erdogan Tan (Ref: KolibriOS, codec.inc)
;	mov	di, ax ; chip_tab
;       mov     eax, [codec_chip_id]
;       and     ax, 0FFh
;_dch1:
;       mov     bx, [di]
;       cmp     bx, 0FFh
;       je      short _dch_unknown
;
;       cmp     ax, bx
;       jne     short _dch_next
;       mov     ax, [di+2]
;       mov     [codec_chip_ids], ax
;       mov     si, ax
;       call	print_msg
;       retn

;_dch_next:
;       add     di, 4
;       jmp     short _dch1
;
;_dch_unknown:
;       mov    word [codec_chip_ids], chip_unknown
;       mov     si, chip_unknown
;       call	print_msg
;       mov     eax, [codec_chip_id]
;       call    dword2str
;       call	print_msg
;       retn

ac97_int_handler:
	; 17/02/2016
	push	ax
	push	dx
	push	cx
	push	bx
	push	si
	push	di

	cmp	byte [inside], 1
	jnb	short _busy

	mov	byte [inside], 1

        mov     dx, [NABMBAR]
        add     dx, PO_SR_REG	; set pointer to Status reg
	in	al, dx
	mov	[irq_status], al
	test	al, BCIS ; Buffer Completion Interrupt Status (Bit 3)
	jz	short _ih_3

	; 28/11/2016 - Erdogan Tan
	call	tuneLoop
_ih0:
	mov	byte [inside], 0
_busy:
	mov	al, 20h
	test	byte [ac97_int_ln_reg], 8
	jz	short _ih_1
	out 	0A0h, al ; 20h ; EOI
_ih_1:
	out	20h, al  ; 20h ; EOI
_ih_2:
	pop	di
	pop	si
	pop	bx
	pop	cx
	pop	dx
	pop	ax
	iret
_ih_3:
	; 17/02/2017
	out	dx, al ; clear interrupt event (by writing 1 to same bits)
	jmp	short _ih0

print_msg:
	; 13/11/2016 - Erdogan Tan 
	; esi = ASCIIZ text address
	;
	mov	bx, 7h
	mov	ah, 0Eh 
pm_next_char:
	lodsb
	and	al, al
	jz	short pm_retn
	int	10h
	jmp	short pm_next_char
pm_retn:
	retn

dword2str:
	; 13/11/2016 - Erdogan Tan 
	; eax = dword value
	;
	call	dwordtohex
	mov	[dword_str], edx
	mov	[dword_str+4], eax
	mov	si, dword_str
	retn

	; trdos386.s (unix386.s) - 10/05/2015
	; Convert binary number to hexadecimal string

bytetohex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	push	bx
	xor	bh, bh
	mov	bl, al
	shr	bl, 4
	mov	bl, [bx+hex_chars] 	 	
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [bx+hex_chars] 
	pop	bx	
	retn

wordtohex:
	; INPUT ->
	; 	AX = word (binary number)
	; OUTPUT ->
	;	EAX = hexadecimal string
	;
	push	bx
	xor	bh, bh
	xchg	ah, al
	push	ax
	mov	bl, ah
	shr	bl, 4
	mov	al, [bx+hex_chars] 	 	
	mov	bl, ah
	and	bl, 0Fh
	mov	ah, [bx+hex_chars]
	shl	eax, 16
	pop	ax
	pop	bx
	jmp	short bytetohex

dwordtohex:
	; INPUT ->
	; 	EAX = dword (binary number)
	; OUTPUT ->
	;	EDX:EAX = hexadecimal string
	;
	push	eax
	shr	eax, 16
	call	wordtohex
	mov	edx, eax
	pop	eax
	call	wordtohex
	retn

_DATA:

; 24/11/2016
;	       IRQ  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
irq_int		db 08h,09h,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh,70h,71h,72h,73h,74h,75h,76h,77h

; 17/02/2017
; Valid ICH device IDs

valid_ids:
dd	(ICH_DID << 16) + INTEL_VID  ; 8086h:2415h 
dd	(ICH0_DID << 16) + INTEL_VID ; 8086h:2425h 
dd	(ICH2_DID << 16) + INTEL_VID ; 8086h:2445h 
dd	(ICH3_DID << 16) + INTEL_VID ; 8086h:2485h 
dd	(ICH4_DID << 16) + INTEL_VID ; 8086h:24C5h
dd	(ICH5_DID << 16) + INTEL_VID ; 8086h:24D5h
dd	(ICH6_DID << 16) + INTEL_VID ; 8086h:266Eh
dd	(ESB6300_DID << 16) + INTEL_VID ; 8086h:25A6h
dd	(ESB631X_DID << 16) + INTEL_VID ; 8086h:2698h
dd	(ICH7_DID << 16) + INTEL_VID ; 8086h:27DEh
valid_id_count:	equ $ - valid_ids 

; 13/11/2016
hex_chars	db "0123456789ABCDEF", 0
msgAC97Info	db "AC97 Audio Controller & Codec Info", 0Dh, 0Ah 
		db "Vendor ID: "
msgVendorId	db "0000h Device ID: "
msgDevId	db "0000h", 0Dh, 0Ah
		db "Bus: "
msgBusNo	db "00h Device: "
msgDevNo	db "00h Function: "
msgFncNo	db "00h"
		db 0Dh, 0Ah
		db "I/O Base Address: "
msgIOBaseAddr	db "0000h IRQ: "
msgIRQ		dw 3030h
		db 0Dh, 0Ah, "$"
msgSampleRate	db "Sample Rate: "
msgHertz	db "00000 Hz ", "$" 
msg8Bits	db "8 bits ", "$" 
msgMono		db "Mono", 0Dh, 0Ah, "$"
msg16Bits	db "16 bits ", "$" 
msgStereo	db "Stereo", 0Dh, 0Ah, "$"

;; 13/11/2016 - Erdogan Tan (Ref: KolibriOS, codec.inc)
;codec_id	dd 0
;codec_chip_id	dd 0
;codec_vendor_ids dw 0
;codec_chip_ids	dw 0

dword_str	 dd 30303030h, 30303030h
		 db 'h', 0Dh, 0Ah, 0

;ac_unknown     db 'unknown manufacturer',13,10,0
;ac_Realtek     db 'Realtek Semiconductor',13,10,0
;ac_Analog      db 'Analog Devices',13,10,0
;ac_CMedia      db 'C-Media Electronics',13,10,0
;ac_Cirrus      db 'Cirrus Logic',13,10,0
;ac_Wolfson     db 'Wolfson Microelectronics',13,10,0
;ac_VIA         db 'VIA Technologies',13,10,0
;ac_SigmaTel    db 'SigmaTel',13,10,0
;ac_eMicro      db 'eMicro',13,10,0
;
;chip_unknown   db 'unknown codec id ', 0

;CHIP_REALTEK   equ 414C4700h
;CHIP_CMEDIA    equ 434D4900h
;CHIP_VIA       equ 56494100h

;codecs        dd CHIP_CMEDIA
;	       dw ac_CMedia, chips_CMedia
;              dd CHIP_REALTEK
;	       dw ac_Realtek, chips_Realtek
;              dd CHIP_VIA
;	       dw ac_VIA, chips_VIA
;              dd 0

;chips_Realtek dw 10h, chip_ALC201a
;              dw 20h, chip_ALC650
;              dw 21h, chip_ALC650D
;              dw 22h, chip_ALC650E
;              dw 23h, chip_ALC650F
;              dw 60h, chip_ALC655
;              dw 80h, chip_ALC658
;              dw 81h, chip_ALC658D
;              dw 90h, chip_ALC850
;              dw 0FFh

;chips_CMedia  dw 41h, chip_CM9738
;              dw 61h, chip_CM9739
;              dw 69h, chip_CM9780
;              dw 78h, chip_CM9761
;              dw 82h, chip_CM9761
;              dw 83h, chip_CM9761
;              dw 0FFh

;chips_VIA     dw 61h, chip_VIA1612A
;              dw 0FFh

;Realtek
;chip_ALC201a    db 'ALC201a',0dh,0ah,00h
;chip_ALC650     db 'ALC650 ',0dh,0ah,00h
;chip_ALC650D    db 'ALC650D',0dh,0ah,00h
;chip_ALC650E    db 'ALC650E',0dh,0ah,00h
;chip_ALC650F    db 'ALC650F',0dh,0ah,00h
;chip_ALC655     db 'ALC655 ',0dh,0ah,00h
;chip_ALC658     db 'ALC658 ',0dh,0ah,00h
;chip_ALC658D    db 'ALC658D',0dh,0ah,00h
;chip_ALC850     db 'ALC850 ',0dh,0ah,00h

;CMedia
;chip_CM9738     db 'CMI9738', 0dh,0ah,0
;chip_CM9739     db 'CMI9739', 0dh,0ah,0
;chip_CM9780     db 'CMI9780', 0dh,0ah,0
;chip_CM9761     db 'CMI9761', 0dh,0ah,0

;VIA
;chip_VIA1612A   db 'VIA1612A',13,10,0

; 17/02/2017
bss_start:

ABSOLUTE bss_start

alignb 2

; 28/11/2016

smpRBuff:	resw 14 ; 19/11/2016 - Erdogan Tan

filehandle:	resw 1

flags:		resb 1
irq_status:	resb 1

inside:		resb 1
tLoop:		resb 1	

; 17/02/2017
; NAMBAR:  Native Audio Mixer Base Address Register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 10h-13h
; NABMBAR: Native Audio Bus Mastering Base Address register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 14h-17h
NAMBAR:		resw 1			; BAR for mixer
NABMBAR         resw 1			; BAR for bus master regs

; 256 byte buffer for descriptor list
BDL_BUFFER:	resw 1			; segment of our 256byte BDL buffer
WAV_BUFFER1:	resw 1			; segment of our WAV storage
; 64k buffers for wav file storage
WAV_BUFFER2:	resw 1			; segment of 2nd wav buffer

tBuff:		resb 1

ac97_int_ln_reg: resb 1

; 12/11/2016 - Erdogan Tan

bus_dev_fn:	resd 1
dev_vendor:	resd 1
stats_cmd:	resw 1 ; 17/02/2017
ac97_io_base:	resw 1
sample_rate:	resw 1
; 19/11/2016
stmo:		resw 1 
bps:		resw 1

fbs_shift:	resb 1
fbs_seg:	resw 1
fbs_off:	resw 1

; 32 kilo bytes for temporay buffer
; (for stereo-mono, 8bit/16bit corrections)
temp_buffer:	resb 32768
alignb 16
EOF: