; ****************************************************************************
; cgaplay2.asm - Retro DOS (MSDOS/PCDOS) WAV PLAYER - Video Mode 13h
; ----------------------------------------------------------------------------
; CGAPLAY2.COM ! Sound Blaster 16 .WAV PLAYER program by Erdogan TAN
;
; 02/01/2025				- play music from multiple wav files -
;
; [ Last Modification: 23/01/2025 ]
;
; Modified from CGAPLAY.COM .wav player program by Erdogan Tan, 01/01/2025
;	        SB16PLAY.COM, 20/12/2024
;
; ****************************************************************************
; nasm cgaplay2.asm -l cgaplay2.lst -o CGAPLAY2.COM -Z error.txt

; 01/01/2025
; cgaplay.asm  : Video Mode 13h (320*200, 256 colors) -NASM-
; 20/12/2024
; sb16play.asm : Video Mode 03h (80x25 text mode) -FASM-

; vgaplay.s (26/12/2024) - play music from multiple wav files -
; dplayvga.s (25/12/2024) - play music from single wav file -
; ac97play.s (18/12/2024) - play music from multiple wav files -

; 07/12/2024 - playwav9.s - interrupt (srb) + tuneloop version
; ------------------------------------------------------------
; INTERRUPT (SRB) + TUNELOOP version ; 24/11/2024 (PLAYWAV9.ASM)
;	(running in DOSBOX, VIRTUALBOX, QEMU is ok)
; Signal Response Byte = message/signal to user about an event/interrupt
;	    as requested (TuneLoop procedure continuously checks this SRB)
; (TRDOS 386 v2 feature is used here as very simple interrupt handler output)

; ------------------------------------------------------------

; 01/01/2025
%macro	sys_msg	2
	mov	si, %1	; message
	mov	bl, %2	; text color
	xor	bh, bh	; video page 0
	mov	ah, 0Eh
	call	p_msg	
%endmacro

; 02/01/2025
%macro SbOut 1
%%wait:
	in	al, dx
	or	al, al
	js	short %%wait
	mov	al, %1	; command
	out	dx, al
%endmacro

; ------------------------------------------------------------

; player internal variables and other equates.
; -------------------------
; 02/01/2025 - cgaplay2.asm
; -------------------------
; 20/12/2024 - sb16play.asm
;--------------------------
; 17/11/2024
;BUFFERSIZE	equ 65520
; 24/11/2024
;dma_buffer_size equ 32768
;LOADSIZE	equ 16384
ENDOFFILE	equ 1		; flag for knowing end of file
; 27/11/2024
dma_buffer_size equ 44100
LOADSIZE	equ 22050

; ------------------------------------------------------------

[BITS 16] ; 16-bit intructions

[ORG 100h]

	; 02/01/2025
START_CODE:
	; 30/05/2024
	; Prints the Credits Text.
	sys_msg Credits, 0Bh

	; 01/01/2025
	; (setFree is required before memAlloc)
	; 30/05/2024
        call    setFree		; deallocate unused DOS mem

	; 17/02/2017
	; Clear BSS (uninitialized data) area
	xor	ax, ax ; 0
	mov	cx, (bss_end - bss_start)/2
	mov	di, bss_start
	rep	stosw

; -------------------------------------------------------------

	; 02/01/2025
	; 24/11/2024
	; Detect (& Reset) Sound Blaster 16 Audio Device
	call	DetectSB16
	;jnc	short GetFileName
	; 02/01/2025
	jnc	short set_video_mode_13h

	; 30/11/2024
	; 30/05/2024
_dev_not_ready:
	; couldn't find the audio device!
	sys_msg noDevMsg, 0Fh
        jmp     Exit

; -------------------------------------------------------------

	; 02/01/2025
set_video_mode_13h:
	; 01/01/2025
	; set VGA/CGA mode (320*200 pixels, 256 colors)
	mov	ax, 13h
	int	10h

; -------------------------------------------------------------

mode_13h_set_ok:
	; 01/01/2025
	;mov	word [graphstart], (11*8*320)+(4*320)

; -------------------------------------------------------------

	; 01/01/2025
Player_ParseParameters:
	; 28/11/2024
	mov	si, 81h
	mov	[PSP_CurrentOffset], si
	cmp	byte [si], 0Dh		; "CR": No command line parameters
	ja	short Player_ParseNextParameter ; 01/01/2025
	jmp	pmsg_usage

	; 30/12/2024
	; 29/11/2024
check_p_command:
  	cmp	byte [command], 'P'
	je	short Player_ParsePreviousParameter
    
	mov	si, [PSP_CurrentOffset]
	cmp	byte [si], 0Dh
	ja	short Player_ParseNextParameter
jmp_Player_Quit:
	jmp	Player_Quit

Player_ParsePreviousParameter:
	; 29/11/2024
	;mov	byte [command], 0

	mov	si, [PSP_CurrentOffset]

	cmp	si, 81h
	je	short Player_ParseNextParameter

	;; Search for previous space character
	dec	si
	mov	cx, 2
PSPParsePrev_Search:
	dec	si
	mov	al, [si]
	cmp	al, 20h
	jne	short PSPParsePrev_Search

	cmp	si, 81h
	jna	PSPParsePrev_Copy
	loop	PSPParsePrev_Search

PSPParsePrev_Copy:
	mov	[PSP_CurrentOffset], si
	
Player_ParseNextParameter:
	; 29/11/2024
	call	GetFileName
	jcxz	jmp_Player_Quit

	; 01/01/2025
	; 28/11/2024
	mov	dx, wav_file_name

	; 30/12/2024
        ; open existing file
        call	openFile ; no error? ok.
        jnc	getwavparms	; 14/11/2024

	; 29/11/2024
	cmp	byte [filecount], 0
	ja	short check_p_command

	; 25/12/2024
	; 21/12/2024
	call	set_text_mode
	; file not found!
	; 01/01/2025
	; 30/11/2024
	sys_msg	noFileErrMsg, 0Ch
        jmp     Exit

_exit_:
	jmp	terminate

; -------------------------------------------------------------

	; 29/11/2024
	; 30/05/2024
GetFileName:
	mov	di, wav_file_name 
	mov	si, [PSP_CurrentOffset]
	xor	cx, cx ; 0
ScanName:
	lodsb
	;test	al, al
	;jz	short a_4
	; 29/11/2024
	cmp	al, 0Dh
	jna	short a_4
	cmp	al, 20h
	je	short ScanName	; scan start of name.
	stosb
	mov	ah, 0FFh
	;;;
	; 14/11/2024
	; (max. path length = 64 bytes for MSDOS ?) (*)
	;xor	cx, cx ; 0
	;;;
a_0:	
	inc	ah
a_1:
	;;;
	; 14/11/2024
	inc	cx
	;;;
	lodsb
	stosb
	cmp	al, '.'
	je	short a_0
	; 29/11/2024
	cmp	al, 20h
	;and	al, al
	;jnz	short a_1
	;;;
	; 14/11/2024
	jna	short a_3
	and	ah, ah
	jz	short a_2
	cmp	al, '\'
	jne	short a_2
	mov	ah, 0
a_2:
	cmp	cl, 75	; 64+8+'.'+3 -> offset 75 is the last chr
	jb	short a_1
	; 29/11/2024
	sub	cx, cx
	jmp	short a_4
a_3:
	; 29/11/2024
	dec	di
	;;;
	or	ah, ah		; if period NOT found,
	jnz	short a_4 	; then add a .WAV extension.
SetExt:
	; 29/11/2024
	;dec	di
	mov	dword [di], '.WAV' ; ! 64+12 is DOS limit
				   ;   but writing +4 must not
				   ;   destroy the following data	 
	;mov	byte [di+4], 0	   ; so, 80 bytes path + 0 is possible here
	; 29/11/2024
	add	cx, 4
	add	di, 4
a_4:	
	mov	byte [di], 0
	dec	si
	mov	[PSP_CurrentOffset], si
	retn

; -------------------------------------------------------------

getwavparms:
	; 14/11/2024
       	call    getWAVParameters
	jc	short _exit_		; nothing to do

	; 17/11/2024
	mov	bl, 4
	sub	bl, byte [WAVE_BlockAlign]
			; = 0 for 16 bit stereo
			; = 2 for 8 bit stereo or 16 bit mono
			; = 3 for 8 bit mono	

	shr	bl, 1	;  0 -->  0,  2 -->  1,  3 -->  1
	; 15/11/2024
	adc	bl, 0	; 3 --> 1 --> 2
	mov	byte [fbs_shift], bl	; = 2 mono and 8 bit
					; = 0 stereo and 16 bit
					; = 1 mono or 8 bit
	; 02/01/2025

; -------------------------------------------------------------

	; 02/01/2025
StartPlay:
	; 30/12/2024
	mov	byte [wpoints], 1

	; 02/01/2025
	mov	ax, [WAVE_SampleRate]
	mov	cx, 10
	mul	cx
	mov	cl, 182
	div	cx
	; ax = samples per 1/18.2 second
	mov	cl, byte [WAVE_BlockAlign]
	mul	cx
_w:
	mov	[wpoints_dif], ax ; buffer read differential (distance)
				; for wave volume leds update
				; (byte stream per 1/18.2 second)

; -------------------------------------------------------------

	; 02/01/2025 (cgaplay2.asm)
	;;;
	; 23/11/2024 (sb16play.asm, [turn_on_leds])
	cmp	byte [WAVE_NumChannels], 1
	ja	short stolp_s
stolp_m:
	cmp	byte [WAVE_BitsPerSample], 8
	ja	short stolp_m16
stolp_m8:
	mov	word [UpdateWavePoints], UpdateWavePoints_8m
	jmp	short stolp_ok
stolp_m16:
	mov	word [UpdateWavePoints], UpdateWavePoints_16m
	jmp	short stolp_ok
stolp_s:
	cmp	byte [WAVE_BitsPerSample], 8
	ja	short stolp_s16
stolp_s8:
	mov	word [UpdateWavePoints], UpdateWavePoints_8s
	jmp	short stolp_ok
stolp_s16:
	mov	word [UpdateWavePoints], UpdateWavePoints_16s
	jmp	short stolp_ok
stolp_ok:
	;;;

	; 25/12/2024 (vgaplay.s)
	inc	byte [filecount]
	mov	byte [command], 0
	; 30/12/2024 (cgaplay.s)
	mov	byte [pbprev], -1

; -------------------------------------------------------------

	; 02/01/2025
	; 01/01/2025 (cgaplay.asm)
	; 30/12/2024
Player_Template:
	; 21/12/2024
	call	clearscreen
	call	drawplayingscreen
	; 14/11/2024
	call	SetTotalTime
	call	UpdateFileInfo

; -------------------------------------------------------------

	; 02/01/2025 (cgaplay2.asm) -SB16-
	; 01/01/2025 (cgaplay.asm) -AC97-
	; 30/12/2024 (cgaplay.s) -AC97- 	
	; 29/12/2024 (vgaplay3.s) -AC97-
	; 20/12/2024 (sb16play.asm) 
	; 18/12/2024 (ac97play.s)
PlayNow:
	; 01/12/2024 (32bit)
	; 14/11/2024
	;;mov	al, 3	; 0 = max, 31 = min
	; 24/11/2024
	;mov	al, 5	; 15 = max, 0 = min
	; 27/11/2024
	;mov	[volume], al
	; 14/12/2024
	mov	al, [volume]
	;call	SetPCMOutVolume@
	; 02/01/2025
	call	SetMasterVolume@
	; 15/11/2024
	;call	SetMasterVolume
	;;call	SetPCMOutVolume

	;;;
	; 14/11/2024
	call	UpdateProgressBar
	;;;

 	; 30/05/2024
	; playwav4.asm
_2:	
	call	check4keyboardstop	; flush keyboard buffer
	jc	short _2		; 07/11/2023

	; 01/01/2025 (cgaplay.asm)

; play the .wav file. Most of the good stuff is in here.

	call    PlayWav

	; 30/12/2024
	; 29/12/2024 (vgaplay3.s)
	; 27/12/2024 (vgaplay.s)
_3:

; close the .wav file and exit.

	; 25/12/2024
	call	closeFile

	; 01/01/2025 (16bit modifications)
	; 25/12/2024
	;;;
	; reset file loading and EOF parameters
	; 18/12/2024
	mov	word [count], 0
	;mov	dword [LoadedDataBytes], 0
	mov	word [LoadedDataBytes], 0
	mov	word [LoadedDataBytes+2], 0
	mov	byte [flags], 0
	mov	byte [stopped], 0
	; 29/12/2024
	mov	word [pbuf_s], 0
	;;;

	cmp	byte [command], 'Q'
	;je	short terminate
	; 02/01/2025
	je	short terminate@
	jmp	check_p_command

	; 02/01/2025
terminate@:
	; 27/11/2024
	; 24/11/2024
	; restore old interrupt vector
	mov	al, [IRQnum]
	xor	ah, ah ; reset
	call	set_hardware_int_vector

terminate:
	call	set_text_mode
	
Exit:	; 01/01/2025
	mov	ax, 4C00h	; bye !
	int	21h
halt:
	jmp	short halt

; -------------------------------------------------------------

	; 30/05/2024
pmsg_usage:
	; 21/12/2024
	call	set_text_mode
	; 01/01/2025
	; 01/12/2024
	sys_msg msg_usage, 0Fh
	jmp	short Exit

; -------------------------------------------------------------

	; 30/05/2024
init_err:
	; 21/12/2024
	call	set_text_mode
	; 01/01/2025
	; 01/12/2024
	sys_msg msg_init_err, 0Fh
	jmp	short Exit

; -------------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
	; 21/12/2024
	; 14/11/2024
PrintString:
	mov	ah, 0Eh
	mov	bx, 0Fh ; char attribute & color (white)
p_msg:
	; ah = 0Eh
	; bh = 0
	; bl = color
	; ds:si = msg address
p_next_chr:
	lodsb
	or	al, al
	jz	short p_retn ; retn
	int	10h
	jmp	short p_next_chr
p_retn:
	retn

; -------------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
clearscreen:
	; fast clear
	; 320*200, 256 colors
	push	es
	mov	di, 0A000h
	mov	es, di
	xor	di, di
	mov	cx, 320*200*1/2
	xor	ax, ax
	rep	stosw
	pop	es
	retn

; -------------------------------------------------------------

	; 01/01/2025v (cgaplay.asm)
drawplayingscreen:
	;push	es
	;push	bp
	;push	ds
	;pop	es
	; es = ds
	mov	bp, PlayingScreen
	mov	ax, 1300h ; write character string
	mov	bx, 0Fh
	mov	cx, 24*40
	xor	dx, dx ; row 0, columns 0
	int	10h
	;pop	bp
	;pop	es
	retn

; -------------------------------------------------------------

	; 01/01/2025
set_text_mode:
	mov	ax, 03h
	int	10h
	;retn

	sys_msg Credits, 0Bh
	;call	write_audio_dev_info
	;retn
	jmp	write_audio_dev_info

; -------------------------------------------------------------

	; 01/01/2025 (16bit pop)
	; 02/12/2024
Player_Quit@:
	pop	ax ; return addr (call PlayWav@)
	
	; 29/11/2024
Player_Quit:
	;jmp	terminate
	; 02/01/2025
	jmp	terminate@  ; reset hardware interrupt

; -------------------------------------------------------------

	; 02/01/2025 (cgaplay2.asm) -SB16-
	; 01/01/2025 (cgaplay.asm) -AC97-
	; 20/12/2024 (sb16play.asm)
PlayWav:
	; 24/11/2024
	mov     ax, wav_buffer1
	call	loadFromFile
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	mov     ax, wav_buffer2
	call	loadFromFile
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	; 25/11/2024
	call	SB16Init_play	; initialize SB16 card
				; set sample rate, start to play
	jc	init_err

	; 02/01/2025
	; 27/11/2024
	; set audio interrupt vector (to user's handler)
	mov	al, [IRQnum]
	mov	ah, 1 ; set
	mov	dx, IRQ_service
	call	set_hardware_int_vector

	; 26/11/2024
	call	check4keyboardstop
	jc	_exitt_

	; 27/11/2024
	mov	byte [IRQnum], 0

	; 02/01/2025

; -------------------------------------------

	; 02/01/2025 (cgaplay2.asm) -SB16-
	; 01/01/2025 (cgaplay.asm) -AC97-
	; 20/12/2024 (sb16play.asm)
	; ----------
	; 29/11/2024
	; 27/11/2024
	; 24/11/2024
TuneLoop: 
	; 30/05/2024
	; 18/11/2023 (ich_wav4.asm)
	; 08/11/2023
	; 06/11/2023
tLWait:
	; 18/11/2024
	cmp	byte [stopped], 0
	; 24/11/2024
	jna	short tL1
tLWait@:	; 21/11/2024
	call	checkUpdateEvents
	jc	_exitt_
	;;;
	; 29/11/2024
	cmp	byte [command], 'N'
	je	_exitt_
	cmp	byte [command], 'P'
	je	_exitt_
	;;;
	cmp	byte [tLO], '0'
	je	short tLWait
	call	tLZ
	mov	byte [tLO], '0'
	jmp	short tLWait
tL1:
	; 27/11/2024
	; Check SB 16 interrupt status
	cmp	byte [IRQnum], 0
	ja	short tL3
tL2:
	call	checkUpdateEvents
	jc	_exitt_
	jmp	short tLWait
tL3:
	xor	byte [half_buffer], 1

	mov	byte [IRQnum], 0

	; load buffer 1
	;mov	ax, wav_buffer1
	mov     ax, dma_buffer  ; wav_buffer1
	cmp	byte [half_buffer], 0
	jna	short tL4

	; load buffer 2
	;mov	ax, wav_buffer2
	add     ax, LOADSIZE ; dma_buffer_size/2
tL4:
	call	loadFromFile
	jc	short _exitt_	; end of file

	; 26/11/2024
	mov	al, [half_buffer]
	add	al, '1'
	; 19/11/2024
	mov	[tLO], al
	call	tL0
	; 24/11/2024
	; 14/11/2024
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	; 27/11/2024
	jmp	short tL2

_exitt_:
	; 24/11/2024
	call	sb16_stop

	;;;
	; 14/11/2024
	call	UpdateProgressBar
	;;;

	; 18/11/2024
tLZ:
	; 30/05/2024
	mov	al, '0'
tL0:
	; 01/01/2025 (cgaplay.asm)
	; 31/12/2024
	push	ax
	xor	dx, dx ; row 0, column 0
	call	setCursorPosition
	pop	ax
	;
	mov	ah, 0Eh
	mov	bx, 0Eh
	int	10h
	
	retn

; -------------------------------------------

	; 02/01/2025
SetMasterVolume:
	mov	[volume], al  ; max = 15, min = 0
	; 02/01/2025
	; 24/11/2024
SetMasterVolume@:
	; al = sound volume (15 = max, 0 = min)
	push	ax
	; Tell the SB 16 card which register to write
	mov	dx, [audio_io_base]
	;add	dx, 4 ; Mixer chip address port
	add	dl, 4
	mov	al, 22h
	out	dx, al
	pop	ax
	;and	al, 0Fh
	; Set the volume for both L and R
	mov	bl, 11h
	mul	bl
	; Set new volume
	mov	dx, [audio_io_base]
	;add	dx, 5
	add	dl, 5
	out	dx, al
	retn

; -------------------------------------------
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      DetectSB procedure (06/08/2022, v2.0.5)	 
DetectSB16:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 24/04/2017
ScanPort:
	mov	bx, 0210h	; start scanning ports
				; 210h, 220h, .. 260h
ResetDSP:       
	; 26/11/2024
	mov	dx, bx		; try to reset the DSP.
	add	dl, 06h

	mov	al, 1
	out	dx, al

	in	al, dx
	in	al, dx
	in	al, dx
	in	al, dx

	xor     al, al
	out	dx, al

	;add	dx, 08h
	add	dl, 08h
	mov	cx, 100
WaitID:
	in	al, dx
	or      al, al
	js      short GetID
	loop    WaitID
	jmp     short NextPort
GetID:          
	;sub	dx, 04h
	sub	dl, 04h
	in	al, dx
	cmp     al, 0AAh
	je      short Found
	;add	dx, 04h
	add	dl, 04h
	loop    WaitID
NextPort:
	;add	bx, 10h		; if not response,
	add	bl, 10h
	;cmp	bx, 260h	; try the next port.
	cmp	bl, 60h
	jbe     short ResetDSP
	stc
	retn
Found:
	mov     [audio_io_base], bx ; SB Port Address Found!
ScanIRQ:
SetIrqs:
	sub 	al, al ; 0
	mov 	[IRQnum], al ; reset
	; 27/11/2024
	;mov	[audio_intr], al

	; 25/11/2024
	; save IRQ status
	in      al, 21h		; save the IMR.
	mov	[IRQstatus], al

	; ah > 0 -> set IRQ vector
	; al = IRQ number
	mov	ax, 105h ; IRQ 5
	; 26/11/2024
	mov	dx, IRQ5_service
	call	set_hardware_int_vector
	mov	ax, 107h ; IRQ 7
	; 26/11/2024
	mov	dx, IRQ7_service
	call	set_hardware_int_vector

	mov     dx, [audio_io_base] ; tells to the SB to
	;add	dx, 0Ch		    ; generate a IRQ!
	add	dl, 0Ch
WaitSb:
	in	al, dx
	or      al, al
	js      short WaitSb
	mov     al, 0F2h
	out	dx, al
	; 24/11/2024
	xor     cx, cx	; wait until IRQ level
WaitIRQ: 
	mov	al, [IRQnum]
	cmp     al, 0 ; is changed or timeout.
	ja	short IrqOk
	dec	cx
	jnz	short WaitIRQ
	jmp	short RestoreIrqs
IrqOk:
	;;;
	; 27/11/2024
	mov	[audio_intr], al
	mov 	dx, [audio_io_base]
	;add	dx, 0Eh
	add	dl, 0Eh ; 8bit DMA-mode int ack
	in	al, dx	; SB acknowledge.
	inc	dx ; 0Fh ; 16bit DMA-mode int ack
	in	al, dx	; SB 16 acknowledge.
	;;;
	mov	al, 20h
	out	20h, al	; Hardware acknowledge.
RestoreIrqs:
	; ah = 0 -> reset IRQ vector
	; al = IRQ number
	mov	ax, 5 ; IRQ 5
	call	set_hardware_int_vector
	mov	ax, 7 ; IRQ 7
	call	set_hardware_int_vector

	cmp     byte [IRQnum], 1 ; IRQ level was changed?
	
	retn

; ----------------------------------

	; 24/11/2024
set_hardware_int_vector:
	or	ah, ah
	jnz	short shintv_1 ; set user's audio interrupt handler

rhintv_1:		
	; reset the interrupt vector to the old interrupt handler
	push	ds
	cmp	al, 5
	jne	short rhintv_2

	; 25/11/2024
	; restore IRQ 5 status
	mov	ah, [IRQstatus]
	in	al, 21h
	and	ah, 00100000b ; 20h
	or	al, ah
	out     21h, al

	mov	dx, [old_irq5v_o]
	mov	ds, [old_irq5v_s]
shintv_3:
	mov	al, 0Dh
	mov	ah, 25h
	int	21h
	pop	ds
	retn

rhintv_2:
	; 25/11/2024
	; restore IRQ 7 status
	mov	ah, [IRQstatus]
	in	al, 21h
	and	ah, 10000000b ; 80h
	or	al, ah
	out     21h, al

	mov	dx, [old_irq7v_o]
	mov	ds, [old_irq7v_s]
shintv_4:
	mov	al, 0Fh
	mov	ah, 25h
	int	21h
	pop	ds
	retn

shintv_1:
	push	es

	cmp	al, 5
	jne	short shintv_2

	; INT 0Dh = IRQ 5 (default) interrupt number

	; 25/11/2024
	; enable IRQ 5
	; 26/11/2024
	in	al, 21h
	and	al, 11011111b
	out     21h, al

	mov	al, 0Fh
	mov	ah, 35h	; Get Interrupt Vector 
	int	21h
	mov	[old_irq5v_s], es
	mov	[old_irq5v_o], bx
	pop	es
	push	ds
	; 27/11/2024
	;mov	dx, IRQ5_service
	jmp	short shintv_3

shintv_2:	
	; al = 7
	; INT 0Fh = IRQ 7 (default) interrupt number

	; 25/11/2024
	; enable IRQ 7
	; 26/11/2024
	in	al, 21h
	and	al, 01111111b
	out     21h, al
	
	mov	al, 0Fh
	mov	ah, 35h	; Get Interrupt Vector 
	int	21h
	mov	[old_irq7v_s], es
	mov	[old_irq7v_o], bx
	pop	es
	push	ds
	; 27/11/2024
	;mov	dx, IRQ7_service
	jmp	short shintv_4

IRQ5_service:
	mov	byte [cs:IRQnum], 5
	iret

IRQ7_service:
	mov	byte [cs:IRQnum], 7
	iret

	; 27/11/2024
IRQ_service:
	push	ds
	push	dx
	push	ax
	;
	push	cs
	pop	ds
	mov	byte [IRQnum], 5
	mov     dx, [audio_io_base]
	;add	dx, 0Eh
	add	dl, 0Eh ; 8bit DMA-mode int ack
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short irq_ack_@
	inc	dl ; 0Fh ; 16bit DMA-mode int ack
irq_ack_@:
	in	al, dx	; SB acknowledge.
	;
	mov	al, 20h
	out	20h, al	; Hardware acknowledge
	;
	pop	ax
	pop	dx
	pop	ds
	iret

; ----------------------------------

	; 02/01/2025
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_stop procedure (06/08/2022, v2.0.5)	
sb16_stop:
	; 02/01/2025
	mov	byte [stopped], 2
	;
	mov	dx, [audio_io_base]
	;add	dx, 0Ch
	add	dl, 0Ch

	mov	bl, 0D9h ; exit auto-initialize 16 bit transfer
	; stop  autoinitialized DMA transfer mode 
	cmp	byte [WAVE_BitsPerSample], 16 ; 16 bit samples
	je	short sb16_stop_1
	;mov	bl, 0DAh ; exit auto-initialize 8 bit transfer
	inc	bl
sb16_stop_1:
	SbOut	bl ; exit auto-initialize transfer command

	xor     al, al ; stops all DMA processes on selected channel

	cmp	byte [WAVE_BitsPerSample], 16 ; 16 bit samples
	je	short sb16_stop_2
	out	0Ch, al ; clear selected channel register
	jmp	short sb16_stop_3

sb16_stop_2:
	out	0D8h, al ; clear selected channel register

sb16_stop_3:
	; 24/11/2024
	mov	byte [stopped], 2 ; stop !
SbDone:
	;mov	dx, [audio_io_base]
	;add	dx, 0Ch
	SbOut   0D0h
	SbOut   0D3h
sb16_stop_4:
	retn

; ----------------------------------

	; 02/01/2025
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_pause procedure (06/08/2022, v2.0.5)	
sb16_pause:
	; 02/01/2025
	mov	byte [stopped], 1 ; paused
	;
	mov	dx, [audio_io_base]
	;add	dx, 0Ch ; Command & Data Port
	add	dl, 0Ch
	cmp	byte [WAVE_BitsPerSample], 16 ; 16 bit samples
	je	short sb_pause_1
	; 8 bit samples
	mov	bl, 0D0h ; 8 bit DMA mode
	jmp	short sb_pause_2
sb_pause_1:
	; 16 bit samples
	mov	bl, 0D5h ; 16 bit DMA mode
sb_pause_2:
	SbOut   bl ; bCommand
sb_pause_3:
	retn

; ----------------------------------

	; 02/01/2025
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_continue procedure (06/08/2022, v2.0.5)
sb16_play:
sb16_continue:
	; 02/01/2025
	; continue to play (after pause)
	mov	byte [stopped], 0
	;
	mov	dx, [audio_io_base]
	;add	dx, 0Ch ; Command & Data Port
	add	dl, 0Ch
	cmp	byte [WAVE_BitsPerSample], 16 ; 16 bit samples
	je	short sb_cont_1
	; 8 bit samples
	mov	bl, 0D4h ; 8 bit DMA mode
	jmp	short sb_cont_2
sb_cont_1:
	; 16 bit samples
	mov	bl, 0D6h ; 16 bit DMA mode
sb_cont_2:     
	SbOut   bl ; bCommand
sb_cont_3:
	retn

; ----------------------------------

	; 14/11/2024
	; INPUT: ds:dx = file name address
	; OUTPUT: [filehandle] = ; -1 = not open
openFile:
	mov	ax, 3D00h	; open File for read
	int	21h
	jnc	short _of1
	mov	ax, -1
	; cf = 1 -> not found or access error
_of1:
	mov	[filehandle], ax
	retn

; ----------------------------------

; close the currently open file

	; 14/11/2024
	; INPUT: [filehandle] ; -1 = not open
	; OUTPUT: none
closeFile:
	cmp	word [filehandle], -1
	jz	short _cf1
	mov     bx, [filehandle]
	mov     ax, 3E00h
        int     21h              ; close file
_cf1:
	retn

; ----------------------------------

	; 14/11/2024 - Erdogan Tan
getWAVParameters:
; reads WAV file header(s) (44 bytes) from the .wav file.
; entry: none - assumes file is already open
; exit: ax = sample rate (11025, 22050, 44100, 48000)
;	cx = number of channels (mono=1, stereo=2)
;	dx = bits per sample (8, 16)
;	bx = number of bytes per sample (1 to 4)

        mov     dx, WAVFILEHEADERbuff
	mov	bx, [filehandle]
        mov     cx, 44			; 44 bytes
	mov	ah, 3Fh
        int     21h
	jc	short gwavp_retn

	cmp	ax, 44
	jb	short gwavp_retn

	cmp	dword [RIFF_Format], 'WAVE'
	jne	short gwavp_stc_retn

	cmp	word [WAVE_AudioFormat], 1 ; Offset 20, must be 1 (= PCM)
	;jne	short gwavp_stc_retn
	je	short gwavp_retn ; 15/11/2024

	; 15/11/2024
	;mov	cx, [WAVE_NumChannels]	; return num of channels in CX
        ;mov    ax, [WAVE_SampleRate]	; return sample rate in AX
	;mov	dx, [WAVE_BitsPerSample] 
					; return bits per sample value in DX
	;mov	bx, [WAVE_BlockAlign]	; return bytes per sample in BX
;gwavp_retn:
        ;retn

gwavp_stc_retn:
	stc
gwavp_retn:
	retn

; --------------------------------------------------------
; ----	30/05/2024 (playwav4.asm, 19/05/2024)
; --------------------------------------------------------

; MEMALLOC.ASM
;-- SETFREE: Release memory not used  --------------------
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

	mov	ah, 4Ah		; pass new length to DOS
	int	21h

	retn			; back to caller 
				; new size (allocated memory) = 64KB

; --------------------------------------------------------

memAlloc:
; input: AX = # of paragraphs required
; output: AX = segment of block to use

	push	bx
	mov	bx, ax
	mov	ah, 48h
	int	21h
	pop	bx
	retn

; --------------------------------------------------------
; 02/01/2025
; --------------------------------------------------------

; /////
	; 24/11/2024 (SB16 version of playwav8.asm -> playwav9.asm)
	; 30/05/2024 (ich_wav4.asm, 19/05/2024)
loadFromFile:
	; 18/12/2024
	mov	word [count], 0

	; 07/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff_0		; no
	stc
	retn

lff_0:
	; 24/11/2024
	; 08/11/2023
	mov	di, ax ; save buffer address
	; 17/11/2024
	mov	bx, [filehandle]

	; 24/11/2024
	mov	cx, LOADSIZE
	mov	dx, ax ; buffer address

	; 24/11/2024
	; load/read file
	; bx = file handle
	; ds = cs
	; ds:dx = buffer
	; cx = read count
       	mov	ah, 3Fh
	int	21h
	jc	short lff_4 ; error !

	; 14/11/2024
	mov	[count], ax

	cmp	ax, cx
	je	short endLFF
	; 24/11/2024
	; di = buffer address
	add	di, ax
lff_3:
	call    padfill			; blank pad the remainder
        ;clc				; don't exit with CY yet.
        or	byte [flags], ENDOFFILE	; end of file flag
endLFF:
        retn
lff_4:
	; 08/11/2023
	mov	al, '!'  ; error
	call	tL0

	xor	ax, ax
	jmp	short lff_3

; entry ds:ax points to last byte in file
; cx = target size
; note: must do byte size fill
; destroys bx, cx
;
padfill:
	; 24/11/2024
	;   di = offset (to be filled with ZEROs)
	;   es = ds = cs
	;   ax = di = number of bytes loaded
	;   cx = buffer size (> loaded bytes)
	sub	cx, ax
	xor	ax, ax
	cmp	byte [WAVE_BitsPerSample], 8
	ja	short padfill@
	mov	al, 80h
padfill@:
	rep	stosb
	retn
; /////
	
write_audio_dev_info:
	; 30/05/2024
     	sys_msg msgAudioCardInfo, 0Fh
	retn

	; 02/01/2025
write_sb16_dev_info:
	; 24/11/2024
	mov	ax, [audio_io_base]
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgBasePort+2], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgBasePort+1], al
	mov	bl, ah
	; 20/12/2024
	;mov	dl, bl
	;and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgBasePort], al

	xor	ax, ax
	;mov	al, [IRQnum]
	; 27/11/2024
	mov	al, [audio_intr]
	;mov	cl, 10
	;div	cl
	;add	ah, 30h
	;mov	[msgIRQ], ah
	; 25/11/2024
	add	al, 30h
	mov	[msgIRQ], al

	call 	clear_window
	;mov	dh, 13
	; 02/01/2025
	mov	dh, 12
	mov	dl, 0
	call	setCursorPosition

	sys_msg msgSB16Info, 07h

	retn

; --------------------------------------------------------
; 24/11/2024 - Sound Blaster 16 initialization
; --------------------------------------------------------

	; 25/11/2024
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9, audio.s (06/06/2024)
	;      SbInit_play procedure (06/08/2022, v2.0.5)	 
SB16Init_play:
	mov	ax, ds
	mov	dx, ax
	shr	dx, 12
	shl	ax, 4
	add	ax, dma_buffer
	adc	dx, 0
	mov	bx, ax	; linear address
	; dx = page number

	mov     cx, dma_buffer_size

	cmp	byte [WAVE_BitsPerSample], 16
	jne	short sbInit_0 ; set 8 bit DMA buffer

	; 26/11/2024
	mov	ax, dx	; page number
	
	; convert byte count to word count
	; 26/11/2024
	;dec	cx
	shr	cx, 1
	dec	cx	; word count - 1

	; convert byte offset to word offset
	shr	ax, 1
	rcr	bx, 1
	; 26/11/2024
	;shr	bx, 1

	; 16 bit DMA buffer setting (DMA channel 5)
	mov	al, 05h  ; set mask bit for channel 5 (4+1)
	out	0D4h, al
	
	xor	al, al   ; stops all DMA processes on selected channel
	out	0D8h, al ; clear selected channel register

	mov	al, bl	 ; byte 0 of DMA buffer offset in words (physical)
	out	0C4h, al ; DMA channel 5 port number

	mov	al, bh   ; byte 1 of DMA buffer offset in words (physical)
	out	0C4h, al
	
	; 26/11/2024
	and	dl, 0FEh ; clear bit 0 (not necessary, it will be ignored)

	mov	al, dl   ; byte 2 of DMA buffer address (physical)
	out	8Bh, al  ; page register port addr for channel 5

	mov	al, cl   ; low byte of DMA count - 1
	out	0C6h, al ; count register port addr for channel 5

	mov	al, ch   ; high byte of DMA count - 1
	out	0C6h, al

	; channel 5, read, autoinitialized, single mode
	mov	al, 59h
	out	0D6h, al ; DMA mode register port address

	mov	al, 01h  ; clear mask bit for channel 5
	out	0D4h, al ; DMA mask register port address

	jmp	short ResetDsp

sbInit_0:    
	dec	cx	; byte count - 1

	; 8 bit DMA buffer setting (DMA channel 1)
	mov	al, 05h ; set mask bit for channel 1 (4+1)
	out	0Ah, al ; DMA mask register

	xor	al, al  ; stops all DMA processes on selected channel
	out	0Ch, al ; clear selected channel register

	mov	al, bl	; byte 0 of DMA buffer address (physical)
	out	02h, al ; DMA channel 1 port number

	mov	al, bh  ; byte 1 of DMA buffer address (physical)
	out	02h, al

	mov	al, dl  ; byte 2 of DMA buffer address (physical)
	out	83h, al ; page register port addr for channel 1

	mov	al, cl  ; low byte of DMA count - 1
	out	03h, al ; count register port addr for channel 1

	mov	al, ch  ; high byte of DMA count - 1
	out	03h, al

	; channel 1, read, autoinitialized, single mode
	mov	al, 59h
	out	0Bh, al ; DMA mode register port address

	mov	al, 01h ; clear mask bit for channel 1
	out	0Ah, al ; DMA mask register port address

ResetDsp:
	mov	dx, [audio_io_base]
	;add	dx, 06h
	add	dl, 06h
	mov	al, 1
	out	dx, al

	in	al, dx
	in	al, dx
	in	al, dx
	in	al, dx

	xor	ax, ax
	out	dx, al

	mov	cx, 100
WaitId:         
	mov	dx, [audio_io_base]
	add	dl, 0Eh
	in	al, dx
	or	al, al
	;js	short sb_GetId
	; 26/11/2024
	jns	short sb_next
	;loop	WaitId
	;jmp	sb_Exit

sb_GetId:
	mov	dx, [audio_io_base]
	;add	dx, 0Ah
	add	dl, 0Ah
	in	al, dx
	cmp	al, 0AAh
	je	short SbOk
sb_next:
	loop	WaitId
	stc
	retn
SbOk:
	mov	dx, [audio_io_base]
	;add	dx, 0Ch
	add	dl, 0Ch
	SbOut	0D1h	; Turn on speaker
	SbOut	41h	; 8 bit or 16 bit transfer
	mov	bx, [WAVE_SampleRate] ; sampling rate (Hz)
	SbOut	bh	; sampling rate high byte
	SbOut	bl	; sampling rate low byte

	; 25/11/2024

StartDMA: 
	; autoinitialized mode
	cmp	byte [WAVE_BitsPerSample], 16 ; 16 bit samples
	je	short sb_play_1
	; 8 bit samples
	mov	bx, 0C6h ; 8 bit output (0C6h)
	cmp	byte [WAVE_NumChannels], 2 ; 1 = mono, 2 = stereo
	jb	short sb_play_2
	mov	bh, 20h	; 8 bit stereo (20h)
	jmp	short sb_play_2
sb_play_1:
	; 16 bit samples
	mov	bx, 10B6h ; 16 bit output (0B6h)
	cmp	byte [WAVE_NumChannels], 2 ; 1 = mono, 2 = stereo
	jb	short sb_play_2
	add	bh, 20h	; 16 bit stereo (30h)
sb_play_2:     
	; PCM output (8/16 bit mono autoinitialized transfer)
	SbOut   bl	; bCommand
	SbOut	bh	; bMode
	; 25/11/2024
	mov	bx, dma_buffer_size/2
			; half buffer size
	cmp	byte [WAVE_BitsPerSample], 16 ; 16 bit DMA
	jne	short sb_play_3
	shr	bx, 1	; byte count to word count (samples)
sb_play_3: 
	dec	bx ; wBlkSize is one less than the actual size
	SbOut   bl
	SbOut   bh

	; 24/11/2024
	;mov	byte [stopped], 0 ; playing !
sb_Exit:
	retn

; --------------------------------------------------------
; 14/11/2024 - Erdogan Tan
; --------------------------------------------------------

	; 02/01/2025 (cgaplay2.asm, SB16)
	; 01/01/2025 (cgaplay.asm, 16bit registers)
	; 07/12/2024
	; 01/12/2024 (32bit registers)
	; 29/11/2024
checkUpdateEvents:
	call	check4keyboardstop
	jc	short c4ue_ok

	; 01/01/2025
	; 18/11/2024
	push	ax ; *
	or	ax, ax
	jz	c4ue_cpt

	; 18/11/2024
	cmp	al, 20h ; SPACE (spacebar) ; pause/play
	jne	short c4ue_chk_s
	cmp	byte [stopped], 0
	ja	short c4ue_chk_ps
	; pause
	;call	ac97_pause
	; 02/01/2025
	call	sb16_pause	; 24/11/2024
	; 02/01/2025
	; 21/11/2024
	;mov	al, [tLO]
	;mov	byte [tLP], al
	jmp	c4ue_cpt
c4ue_chk_ps:
	cmp	byte [stopped], 1
	ja	short c4ue_replay
	; continue to play (after a pause)
	;call	ac97_play
	; 02/01/2025
	call	sb16_play	; 24/11/2024
	jmp	c4ue_cpt
c4ue_replay:
	; 19/11/2024
	pop	ax ; *
	pop	ax ; return address
	; 07/02/2024
	;mov	al, [volume]
	;call	SetmasterVolume
	mov	byte [stopped], 0
	; 02/01/2024
	; 24/11/2024
	mov	byte [half_buffer], 1
	call	move_to_beginning
	; 02/01/2025 (cgaplay2.asm, SB16)
	jmp	PlayWav
	; 07/12/2024 (cgaplay.asm, AC97)
	;jmp	RePlayWav

c4ue_chk_s:
	cmp	al, 'S'	; stop
	jne	short c4ue_chk_fb
	cmp	byte [stopped], 0
	ja	c4ue_cpt ; Already stopped/paused
	;call	ac97_stop
	; 02/01/2025
	call	sb16_stop	; 24/11/2024
	; 19/11/2024
	mov	byte [tLO], 0
	; 02/01/2025
	; 21/11/2024
	;mov	byte [tLP], '0'
	jmp	c4ue_cpt

	; 01/12/2024
	; 18/11/2024
c4ue_ok:
	retn

c4ue_chk_fb:
	; 17/11/2024
	cmp	al, 'F'
	jne	short c4ue_chk_b
	call 	Player_ProcessKey_Forwards
	jmp	c4ue_cpt

c4ue_chk_b:
	cmp	al, 'B'
	;;jne	short c4ue_cpt
	; 19/11/2024
	;jne	short c4ue_chk_h
	; 25/12/2024
	; 29/11/2024
	jne	short c4ue_chk_n
	call 	Player_ProcessKey_Backwards
	jmp	short c4ue_cpt

	;;;
	; 25/12/2024
	; 29/11/2024
c4ue_chk_n:
	cmp	al, 'N'
	je	short c4ue_nps
c4ue_chk_p:
	cmp	al, 'P'
	jne	short c4ue_chk_h
c4ue_nps:
	mov	byte [stopped], 3
	jmp	short c4ue_cpt
	;;;

c4ue_chk_h:
	; 19/11/2024
	cmp	al, 'H'
	jne	short c4ue_chk_cr
	mov	byte [wpoints], 0
	;call 	write_ac97_pci_dev_info
	; 02/01/2025
	call 	write_sb16_dev_info
	; 30/12/2024
	jmp	short c4ue_cpt
c4ue_chk_cr:
	;;;
	; 24/12/2024 (wave lighting points option)
	;mov	ah, [wpoints]
	; 01/01/2025
	xor	bx, bx
	mov	bl, [wpoints]
	cmp	al, 'G'
	je	short c4ue_g
	; 19/11/2024
	cmp	al, 0Dh ; ENTER/CR key
	jne	short c4ue_cpt
	; 23/11/2024
	;xor	bx, bx
	; 30/12/2024
	;mov	bl, ah ; 24/12/2024
	inc	bl
c4ue_g:	; 30/12/2024
	and	bl, 07h
	jnz	short c4ue_sc
	; 01/01/2025
	inc	bx
c4ue_sc:
	mov	[wpoints], bl
	; 01/01/2025
	mov	al, [bx+colors-1] ; 1 to 7
	; 24/12/2024
	mov	[ccolor], al
	; 30/12/2024
	call	clear_window
	;;;
c4ue_cpt:
	push	ds
	mov	bx, 40h
	mov	ds, bx
	mov	bx, 6Ch  ; counter (INT 08h, 18.2 ticks per sec)
	;cli
	mov	ax, [bx]
	mov	dx, [bx+2]
	;sti
	pop	ds
	; 18/11/2024
	pop	cx ; *
	cmp	dx, [timerticks+2]
	jne	short c4ue_utt
	cmp	ax, [timerticks]
	;je	short c4ue_ok
	; 18/11/2024
	je	short c4ue_skip_utt
c4ue_utt:
	; 01/01/2025
	mov	[timerticks], ax
	mov	[timerticks+2], dx
	jmp	short c4ue_cpt_@

	; 30/12/2024
c4ue_vb_ok:
	retn

c4ue_skip_utt:
	; 01/01/2025
	; 18/11/2024
	and	cx, cx
	jz	short c4ue_vb_ok
c4ue_cpt_@:
	; 18/11/2024
	cmp	byte [stopped], 0
	ja	short c4ue_vb_ok
	
	call	CalcProgressTime

	cmp	ax, [ProgressTime]
	;je	short c4ue_vb_ok
			; same second, no need to update
	; 23/11/2024
	je	short c4ue_uvb

	;call	UpdateProgressTime
	;call	UpdateProgressBar@
	call	UpdateProgressBar

	; 23/11/2024
c4ue_uvb:
	cmp	byte [wpoints], 0
	jna	short c4ue_vb_ok

	; 30/12/2024
	;call	UpdateWavePoints
	;retn

	; 02/01/2025 (cgaplay2.asm, SB16)
	call	word [UpdateWavePoints]
	retn

; --------------------------------------------------------
; 27/12/2024 - Erdogan Tan
; --------------------------------------------------------

	; 23/01/2025
	; 02/01/2025 (cgaplay2.asm, SB16)
	; 01/01/2025 (cgaplay.asm, 16bit registers)
	; 30/12/2024 (cgaplay.s)
	;  * 320*200 pixels, 256 colors
	;  * 64 volume levels
	; 29/12/2024
	; 27/12/2024 (DMA Buffer Tracking)
	; 26/12/2024
	; 24/12/2024
;UpdateWavePoints:
	; 02/01/2025
UpdateWavePoints_16s:
	; 01/01/2025
	push	es ; **
	mov	bx, 0A000h
	mov	es, bx
	;
	mov	si, prev_points
	cmp	word [si], 0
	jz	short lights_off_ok

	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
light_off:
	lodsw
	; ax = wave point (lighting point) address
	;mov	byte [ax], 0 ; black point (light off)
	; 01/01/2025 (16bit mode modification)
	mov	bx, ax
	mov	byte [es:bx], 0
	loop	light_off

lights_off_ok:
	; 29/12/2024
	cmp	byte [tLO],'2'
	jne	short lights_on_buff_1
lights_on_buff_2:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER2]
	; 02/01/2025
	mov	dx, wav_buffer2
	jmp	short lights_on
lights_on_buff_1:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER1]
	; 02/01/2025
	mov	dx, wav_buffer1
lights_on:
	cmp	[pbuf_s], dx
	jne	short lights_on_2
	mov	bx, [wpoints_dif]
	mov	si, [pbuf_o]
	;mov	cx, [buffersize] ; samples
	;; 01/01/2025
	;shl	cx, 1  ; bytes
	; 02/01/2025
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wpoints_dif]
	add	si, bx
	jc	short lights_on_1
	cmp	si, cx
	jna	short lights_on_3
lights_on_1:
	mov	si, cx
	jmp	short lights_on_3

lights_on_2:
	; 29/12/2024
	mov	[pbuf_s], dx
	xor	si, si ; 0
lights_on_3:
	mov	[pbuf_o], si
	; 02/01/2025
	; 01/01/2025
	;push	ds ; **
	;
	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
	mov	bp, cx
	; 26/12/2024
	mov	di, prev_points
	;mov	bx, [graphstart] ; start (top) line
	; 01/01/2025
	;mov	ds, dx
	; 02/01/2025
	add	si, dx
	mov	bx, (11*8*320)+(4*320)
lights_on_4:
	; 23/01/2025
	;; 01/01/2025
	;xor	eax, eax ; 0
	;lodsw	; left
	;add	ah, 80h
	;mov	edx, eax
	;lodsw	; right
	;;add	ax, dx
	;add	ah, 80h
	;;;shr	eax, 9	; 128 volume levels
	;add	eax, edx
	;;;shr	eax, 10	; (L+R/2) & 128 volume levels
	;;shr	eax, 9	; (L+R/2) & 256 volume levels
	;; 30/12/2024
	;shr	eax, 11	; (L+R/2) & 64 volume levels
	; 23/01/2025
	lodsw	; left
	add	ah, 80h
	mov	dx, ax	
	lodsw	; right
	add	ah, 80h
	add	ax, dx	; (L+R)	
	rcr	ax, 1	; (L+R)/2
	shr	ax, 10	; 64 volume levels

	; * 320 row  ; 30/12/2024
	mul	bp	; * 640 (row) 
	add	ax, bx ; + column
	; 02/01/2025
	mov	dl, [ccolor]
	; 01/01/2025
	;mov	dl, [cs:ccolor]
	xchg	ax, bx
	mov	[es:bx], dl ; pixel (light on) color
	xchg	ax, bx
	;mov	[cs:di], ax ; save light on addr in prev_points
	; 02/01/2025
	mov	[di], ax
	inc	di
	inc	di
	inc	bx
	loop	lights_on_4
	; 02/01/2025
	;pop	ds ; **
	pop	es ; *
	retn

; -------------------------

	; 02/01/2025 (cgaplay2.asm, SB16, RetroDOS)
	; 01/01/2025 (cgaplay.asm, AC97, RetroDOS)
	; 30/12/2024 (cgaplay.s, AC97, TRDOS386)
	;  * 320*200 pixels, 256 colors
	;  * 64 volume levels
UpdateWavePoints_16m:
	; 01/01/2025
	push	es ; **
	mov	bx, 0A000h
	mov	es, bx
	;
	mov	si, prev_points
	cmp	word [si], 0
	jz	short lights_off_16m_ok

	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
light_16m_off:
	lodsw
	; ax = wave point (lighting point) address
	;mov	byte [ax], 0 ; black point (light off)
	; 01/01/2025 (16bit mode modification)
	mov	bx, ax
	mov	byte [es:bx], 0
	loop	light_16m_off

lights_off_16m_ok:
	; 29/12/2024
	cmp	byte [tLO],'2'
	jne	short lights_16m_on_buff_1
lights_16m_on_buff_2:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER2]
	; 02/01/2025
	mov	dx, wav_buffer2
	jmp	short lights_16m_on
lights_16m_on_buff_1:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER1]
	; 02/01/2025
	mov	dx, wav_buffer1
lights_16m_on:
	cmp	[pbuf_s], dx
	jne	short lights_16m_on_2
	mov	bx, [wpoints_dif]
	mov	si, [pbuf_o]
	;mov	cx, [buffersize] ; samples
	;; 01/01/2025
	;shl	cx, 1  ; bytes
	; 02/01/2025
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wpoints_dif]
	add	si, bx
	jc	short lights_16m_on_1
	cmp	si, cx
	jna	short lights_16m_on_3
lights_16m_on_1:
	mov	si, cx
	jmp	short lights_16m_on_3

lights_16m_on_2:
	; 29/12/2024
	mov	[pbuf_s], dx
	xor	si, si ; 0
lights_16m_on_3:
	mov	[pbuf_o], si
	; 02/01/2025
	; 01/01/2025
	;push	ds ; **
	;
	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
	mov	bp, cx
	; 26/12/2024
	mov	di, prev_points
	;mov	bx, [graphstart] ; start (top) line
	; 01/01/2025
	;mov	ds, dx
	; 02/01/2025
	add	si, dx
	mov	bx, (11*8*320)+(4*320)
lights_16m_on_4:
	; 02/01/2025 (16bit mono play modifications)
	; 23/01/2025
	;xor	ax, ax ; 0
	lodsw	; left
	add	ah, 80h
	shr	ax, 10	; 64 volume levels
	; 30/12/2024
	; * 320 row
	mul	bp ; * 640 (row)
	add	ax, bx ; + column
	; 02/01/2025
	mov	dl, [ccolor]
	; 01/01/2025
	;mov	dl, [cs:ccolor]
	xchg	ax, bx
	mov	[es:bx], dl ; pixel (light on) color
	xchg	ax, bx
	;mov	[cs:di], ax ; save light on addr in prev_points
	; 02/01/2025
	mov	[di], ax
	inc	di
	inc	di
	inc	bx
	loop	lights_16m_on_4
	; 02/01/2025
	;pop	ds ; **
	pop	es ; *
	retn

; -------------------------

	; 02/01/2025
UpdateWavePoints_8s:
	; 01/01/2025
	push	es ; **
	mov	bx, 0A000h
	mov	es, bx
	;
	mov	si, prev_points
	cmp	word [si], 0
	jz	short lights_off_8s_ok

	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
light_8s_off:
	lodsw
	; ax = wave point (lighting point) address
	;mov	byte [ax], 0 ; black point (light off)
	; 01/01/2025 (16bit mode modification)
	mov	bx, ax
	mov	byte [es:bx], 0
	loop	light_8s_off

lights_off_8s_ok:
	; 29/12/2024
	cmp	byte [tLO],'2'
	jne	short lights_8s_on_buff_1
lights_8s_on_buff_2:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER2]
	; 02/01/2025
	mov	dx, wav_buffer2
	jmp	short lights_8s_on
lights_8s_on_buff_1:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER1]
	; 02/01/2025
	mov	dx, wav_buffer1
lights_8s_on:
	cmp	[pbuf_s], dx
	jne	short lights_8s_on_2
	mov	bx, [wpoints_dif]
	mov	si, [pbuf_o]
	;mov	cx, [buffersize] ; samples
	;; 01/01/2025
	;shl	cx, 1  ; bytes
	; 02/01/2025
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wpoints_dif]
	add	si, bx
	jc	short lights_8s_on_1
	cmp	si, cx
	jna	short lights_8s_on_3
lights_8s_on_1:
	mov	si, cx
	jmp	short lights_8s_on_3

lights_8s_on_2:
	; 29/12/2024
	mov	[pbuf_s], dx
	xor	si, si ; 0
lights_8s_on_3:
	mov	[pbuf_o], si
	; 02/01/2025
	; 01/01/2025
	;push	ds ; **
	;
	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
	mov	bp, cx
	; 26/12/2024
	mov	di, prev_points
	;mov	bx, [graphstart] ; start (top) line
	; 01/01/2025
	;mov	ds, dx
	; 02/01/2025
	add	si, dx
	mov	bx, (11*8*320)+(4*320)
lights_8s_on_4:
	; 02/01/2025 (8bit stereo play modifications)
	xor	ax, ax ; 0
	lodsb	; left
	mov	dx, ax
	lodsb	; right
	add	ax, dx
	shr	ax, 1 ; (L+R/2)
	sub	al, 255	; max. value will be shown on top
	shr	ax, 2 ; 64 volume levels
	; * 320 row  ; 30/12/2024
	mul	bp	; * 640 (row) 
	add	ax, bx ; + column
	; 02/01/2025
	mov	dl, [ccolor]
	; 01/01/2025
	;mov	dl, [cs:ccolor]
	xchg	ax, bx
	mov	[es:bx], dl ; pixel (light on) color
	xchg	ax, bx
	;mov	[cs:di], ax ; save light on addr in prev_points
	; 02/01/2025
	mov	[di], ax
	inc	di
	inc	di
	inc	bx
	loop	lights_8s_on_4
	; 02/01/2025
	;pop	ds ; **
	pop	es ; *
	retn

; -------------------------

	; 02/01/2025
UpdateWavePoints_8m:
	; 01/01/2025
	push	es ; **
	mov	bx, 0A000h
	mov	es, bx
	;
	mov	si, prev_points
	cmp	word [si], 0
	jz	short lights_off_8m_ok

	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
light_8m_off:
	lodsw
	; ax = wave point (lighting point) address
	;mov	byte [ax], 0 ; black point (light off)
	; 01/01/2025 (16bit mode modification)
	mov	bx, ax
	mov	byte [es:bx], 0
	loop	light_8m_off

lights_off_8m_ok:
	; 29/12/2024
	cmp	byte [tLO],'2'
	jne	short lights_8m_on_buff_1
lights_8m_on_buff_2:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER2]
	; 02/01/2025
	mov	dx, wav_buffer2
	jmp	short lights_8m_on
lights_8m_on_buff_1:
	; 01/01/2025
	;mov	dx, [WAV_BUFFER1]
	; 02/01/2025
	mov	dx, wav_buffer1
lights_8m_on:
	cmp	[pbuf_s], dx
	jne	short lights_8m_on_2
	mov	bx, [wpoints_dif]
	mov	si, [pbuf_o]
	;mov	cx, [buffersize] ; samples
	;; 01/01/2025
	;shl	cx, 1  ; bytes
	; 02/01/2025
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wpoints_dif]
	add	si, bx
	jc	short lights_8m_on_1
	cmp	si, cx
	jna	short lights_8m_on_3
lights_8m_on_1:
	mov	si, cx
	jmp	short lights_8m_on_3

lights_8m_on_2:
	; 29/12/2024
	mov	[pbuf_s], dx
	xor	si, si ; 0
lights_8m_on_3:
	mov	[pbuf_o], si
	; 02/01/2025
	; 01/01/2025
	;push	ds ; **
	;
	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
	mov	bp, cx
	; 26/12/2024
	mov	di, prev_points
	;mov	bx, [graphstart] ; start (top) line
	; 01/01/2025
	;mov	ds, dx
	; 02/01/2025
	add	si, dx
	mov	bx, (11*8*320)+(4*320)
lights_8m_on_4:
	; 02/01/2025 (8bit mono play modifications)
	xor	ax, ax ; 0
	lodsb
	sub	al, 255	; max. value will be shown on top
	shr	ax, 2  ; 64 volume levels
	; 30/12/2024
	; * 320 row
	mul	bp ; * 640 (row)
	add	ax, bx ; + column
	; 02/01/2025
	mov	dl, [ccolor]
	; 01/01/2025
	;mov	dl, [cs:ccolor]
	xchg	ax, bx
	mov	[es:bx], dl ; pixel (light on) color
	xchg	ax, bx
	;mov	[cs:di], ax ; save light on addr in prev_points
	; 02/01/2025
	mov	[di], ax
	inc	di
	inc	di
	inc	bx
	loop	lights_8m_on_4
	; 02/01/2025
	;pop	ds ; **
	pop	es ; *
	retn

; --------------------------------------------------------
; 19/05/2024 - (playwav4.asm) ich_wav4.asm
; --------------------------------------------------------

	; 02/01/2025 (cgaplay2.asm)
	; 29/11/2024
check4keyboardstop:
	; 19/05/2024
	; 08/11/2023
	; 04/11/2023
	mov	ah, 1
	int	16h
	;clc
	jz	short _cksr

	xor	ah, ah
	int	16h

	; 29/11/2024
	mov	[command], al

	;;;
	; 19/05/2024 (change PCM out volume)
	cmp	al, '+'
	jne	short p_1
	
	mov	al, [volume]
	;cmp	al, 0
	;jna	short p_3
	;dec	al
	;jmp	short p_2
	; 02/01/2025
	cmp	al, 15
	jnb	short p_3
	inc	al
	jmp	short p_2
p_1:
	cmp	al, '-'
	jne	short p_4

	mov	al, [volume]
	;cmp	al, 31
	;jnb	short p_3
	;inc	al
	; 02/01/2025
	cmp	al, 0
	jna	short p_3
	dec	al
p_2:
	; 02/01/2025
	;mov	[volume], al
	; 14/11/2024
	;call	SetPCMOutVolume
	; 02/01/2025
	; 15/11/2024 (QEMU)
	call	SetMasterVolume
	;call	UpdateVolume
	;;clc
	;retn
	jmp	UpdateVolume

_cksr:		; 19/05/2024
	; 18/11/2024
	xor	ax, ax
	;clc
p_3:
	retn
p_4:
	; 17/11/2024
	cmp	ah, 01h  ; ESC
    	je	short p_q
	cmp	al, 03h  ; CTRL+C
	je	short p_q

	; 18/11/2024
	cmp	al, 20h
	je	short p_r

	; 19/11/2024
	cmp	al, 0Dh ; CR/ENTER
	je	short p_r

	and	al, 0DFh

	; 29/11/2024
	mov	[command], al

	;cmp	al, 'B'
	;je	short p_r
	;cmp	al, 'F'
	;je	short p_r

	; 29/11/2024
	;cmp	al, 'N'
	;je	short p_r
	;cmp	al, 'P'
	;je	short p_r

	cmp	al, 'Q'
	;je	short p_q
	je	short p_quit ; 29/11/2024

	clc
	retn

	;;;
;_cskr:	
p_q:
	; 29/11/2024
	mov	byte [command], 'Q'
p_quit:
	stc
p_r:
	retn

; 29/05/2024
; 19/05/2024
volume: 
	;db	02h
; 26/12/2024
	;db	03h
; 02/01/2025 (SB16)
	db	10 ; max = 15, min = 0

; --------------------------------------------------------

	; 01/01/2025 /cgaplay.asm)
setCursorPosition:
	; dh = Row
	; dl = Column

	; 31/12/2024
	mov	bh, 0
	mov	ah, 02h
	int	10h
	retn
	
; --------------------------------------------------------
; 14/11/2024
; (Ref: player.asm, out_cs.asm, Matan Alfasi, 2017)

;; NAME:	SetTotalTime
;; DESCRIPTION: Calculates the total time in seconds in file
;; INPUT:	DATA_SubchunkSize, WAVE_SampleRate, WAVE_BlockAlign
;; OUTPUT:	CurrentTotalTime=Total time in seconds in file,
;; 		Output on the screen of the total time in seconds

	; 01/01/2025
SetTotalTime:
	;; Calculate total seconds in file
	mov	ax, [DATA_SubchunkSize]
	mov	dx, [DATA_SubchunkSize+2]
	mov	bx, [WAVE_SampleRate]
	div	bx
	xor	dx, dx

	mov	bx, [WAVE_BlockAlign]

	div	bx

	mov	[TotalTime], ax

	mov	bl, 60
	div	bl

	;; al = minutes, ah = seconds
	push	ax ; **
	push	ax ; *

	;mov	dh, 24
	;mov	dl, 42
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 23
	mov	dl, 22
	call	setCursorPosition

	pop	ax ; *
	xor	ah, ah
	mov	bp, 2
	call	PrintNumber
	
	;mov	dh, 24
	;mov	dl, 45
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 23
	mov	dl, 25
	call	setCursorPosition

	pop	ax ; **
	mov	al, ah
	xor	ah, ah
	mov	bp, 2
	;jmp	short PrintNumber

; --------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
PrintNumber:
	; bp = digits
	; ax = binary number
	mov	bx, 10
	xor	cx, cx
printNumber_CutNumber:
	inc	cx
	xor	dx, dx
	div	bx
	push	dx
	cmp	cx, bp
	je	short printNumber_printloop
	jmp	printNumber_CutNumber

printNumber_printloop:
	pop	ax
	; bp = count of digits
	; ax <= 9

	add	al, '0'
	
	; al = character
	call	write_character_white

	dec	bp
 	jz	short printNumber_ok
	jmp	short printNumber_printloop
printNumber_ok:
	retn

; --------------------------------------------------------

	; 01/01/2025
	; 14/11/2024 - Erdogan Tan
SetProgressTime:
	;; Calculate playing/progress seconds in file
	call	CalcProgressTime

UpdateProgressTime:
	; ax = (new) progress time 

	mov	[ProgressTime], ax

	mov	bl, 60
	div	bl

	;; al = minutes, ah = seconds
	push	ax ; **
	push	ax ; *

	;mov	dh, 24
	;mov	dl, 33
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 23
	mov	dl, 13
	call	setCursorPosition

	pop	ax ; *
	xor	ah, ah
	mov	bp, 2
	call	PrintNumber
	
	;mov	dh, 24
	;mov	dl, 36
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 23
	mov	dl, 16
	call	setCursorPosition

	pop	ax ; **
	mov	al, ah
	xor	ah, ah
	; 21/12/2024
	mov	bp, 2
	jmp	short PrintNumber

; --------------------------------------------------------

	; 17/11/2024
	; 14/11/2024
CalcProgressTime:
	mov	ax, [LoadedDataBytes]
	mov	dx, [LoadedDataBytes+2]
	mov	bx, ax
	or	bx, dx
	jz	short cpt_ok

	mov	bx, [WAVE_SampleRate]
	div	bx
	xor	dx, dx
	mov	bx, [WAVE_BlockAlign]
	div	bx
cpt_ok:
	; ax = (new) progress time
	retn

; --------------------------------------------------------
; 14/11/2024
; (Ref: player.asm, out_cs.asm, Matan Alfasi, 2017)

;; DESCRIPTION: Update file information on template
;; PARAMS:	WAVE parameters and other variables
;; REGS:	AX(RW)
;; VARS:	CurrentFileName, WAVE_SampleRate, 
;; RETURNS:	On-screen file info is updated.

	; 01/01/2025 (cgaplay.asm)
UpdateFileInfo:
	;; Print File Name
	;mov	dh, 9
	;mov	dl, 23
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 7
	mov	dl, 8
	call	setCursorPosition
	
	mov	si, wav_file_name
	
	;;;
	; 14/11/2024
	; skip directory separators
	; (note: asciiz string, max. 79 bytes except zero tail)
	mov	bx, si
chk4_nxt_sep:
	lodsb
	cmp	al, '\'
	je	short chg_fpos
	and	al, al
	jz	short chg_fpos_ok
	jmp	short chk4_nxt_sep
chg_fpos:
	mov	bx, si
	jmp	short chk4_nxt_sep
chg_fpos_ok:
	mov	si, bx	; file name (without its path/directory)
	;;;
_fnl_chk:
	; 01/01/2025 (cplay.asm)
	; 30/12/2024 (cgaplay.s)
	; ????????.wav
	; 26/12/2024 (file name length limit -display-)
	mov	bx, 12
	;mov	bx, 17	; ????????.wav?????
	push	si
_fnl_chk_loop:
	lodsb
	and	al, al
	jz	short _fnl_ok
 	dec	bx
	jnz	short _fnl_chk_loop
	mov	byte [si], 0
_fnl_ok:
	pop	si
	;;;

	call	PrintString
	
	;; Print Frequency
	;mov	dh, 10
	;mov	dl, 23
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 8
	mov	dl, 8
	call	setCursorPosition
	 
	mov	ax, [WAVE_SampleRate]
	mov	bp, 5
	call	PrintNumber

	;; Print BitRate
	;mov	dh, 9
	;mov	dl, 57
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 7
	mov	dl, 31
	call	setCursorPosition

	mov	ax, [WAVE_BitsPerSample]
	mov	bp, 2
	call	PrintNumber

	;; Print Channel Number
	;mov	dh, 10
	;mov	dl, 57
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 8
	mov	dl, 31
	call	setCursorPosition

	mov	ax, [WAVE_NumChannels]
	mov	bp, 1
	call	PrintNumber

	;call	UpdateVolume
	;retn

; --------------------------------------------------------

	; 02/02/2025 (cgaplay2.asm) -SB16-
	; 01/01/2025 (cgaplay.asm) -AC97-
	; 14/11/2024
UpdateVolume:
	;; Print Volume
	;mov	dh, 24
	;mov	dl, 75
	; 01/01/2025 (cgaplay.asm, video mode 13h)
	mov	dh, 23
	mov	dl, 35
	call	setCursorPosition
	
	mov	al, [volume]

	mov	bl, 100
	mul	bl

	;mov	bl, 31
	;div	bl
	; 02/01/2025 (SB16)
	mov	bl, 15
	div	bl

	;neg	ax
	;add	ax, 100

	xor	ah, ah
	mov	bp, 3
	;call	PrintNumber
	;retn
	jmp	PrintNumber	

; --------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
write_character_white:
	;mov	cx, 0Fh
	mov	bl, 0Fh
write_character:
	; 01/12/2024
	xor	bh, bh
	; bl = foreground color
	; al = character (ASCII code)
	mov	ah, 0Eh
	int	10h
	retn

; --------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
	; 30/12/2024
	; write characters in video mode 13h
	; (320*200 pixels, 256 colors)
	; 22/12/2024
	; 21/12/2024
	; (write chars in VESA VBE graphics mode)
	; 14/11/2024
	; (Ref: player.asm, Matan Alfasi, 2017)
	; (Modification: Erdogan Tan, 14/11/2024)

	;PROGRESSBAR_ROW equ 23
	; 21/12/2024 (640*480)
	;PROGRESSBAR_ROW equ 31
	; 30/12/2024 (320*200)
	PROGRESSBAR_ROW equ 22

UpdateProgressBar:
	call	SetProgressTime	; 14/11/2024

	; 01/01/2025
	mov	ax, [ProgressTime]
UpdateProgressBar@:
	;mov	dx, 80
	; 30/12/2024
	mov	dx, 40 ; 320*200 pixels, 40 columns 
	mul	dx
	mov	bx, [TotalTime]
	div	bx

	; 22/12/2024
	; check progress bar indicator position if it is same 
	cmp	al, [pbprev]
	je	short UpdateProgressBar_ok
	mov	[pbprev], al

	; 01/01/2025
UpdateProgressBar@@:
	;; Push for the 'Clean' part
	push	ax ; **
	push	ax ; *

	;; Set cursor position
	mov	dh, PROGRESSBAR_ROW
	mov	dl, 0
	call	setCursorPosition

	pop	ax ; *
	or	ax, ax
	jz	short UpdateProgressBar_Clean

	; 01/01/2025
UpdateProgressBar_DrawProgress:
	; 31/12/2024 (int 31h)
	; 22/12/2024
	; 21/12/2024
	; (write progress bar chars in graphics mode)
	;;;;
	mov	bp, ax
	push	ax ; ***
UpdateProgressBar_DrawProgress_@:
	; 01/01/2025
	mov	al, 223

	; al = character
	;call	write_character
	; 22/12/2024
	call	write_character_white

	dec	bp
	jz	short UpdateProgressBar_DrawCursor
	jmp	short UpdateProgressBar_DrawProgress_@

UpdateProgressBar_ok:
	retn

	; 01/01/2025
UpdateProgressBar_DrawCursor:
	; 22/12/2024
	pop	dx ; ***
	mov	dh, PROGRESSBAR_ROW
	; 31/12/2024
	dec	dl ; last written position again
	call	setCursorPosition

	; 01/01/2025	
	mov	al, 223
	;mov	cl, 0Ch ; red
	; 01/01/2025
	mov	bl, 0Ch
	call	write_character

	; 01/01/2025
UpdateProgressBar_Clean:
	;pop	ax  ; **
	; 22/12/2024
	pop	dx  ; **
	; 30/12/2024
	; 21/12/2024
	;mov	bp, 80
	; 30/12/2024
	mov	bp, 40 ; 40 columns (320*200 pixels)
	;sub	bp, ax
	sub	bp, dx ; 22/12/2024
	;jz	short UpdateProgressBar_ok
	; 31/12/2024
	jna	short UpdateProgressBar_ok

	mov	dh, PROGRESSBAR_ROW
	;mov	dl, al ; 22/12/2024
	call	setCursorPosition

	; 21/12/2024
	; (write progress bar chars in graphics mode)
UpdateProgressBar_Clean_@:
	; 01/01/2025	
	mov	al, 223
	;mov	cl, 08h ; gray (dark)
	mov	bl, 08h
	call	write_character
	
	dec	bp
	jz	short UpdateProgressBar_ok
	jmp	short UpdateProgressBar_Clean_@

; --------------------------------------------------------
; 17/11/2024

Player_ProcessKey_Backwards:
	;; In order to go backwards 5 seconds:
	;; Update file pointer to the beginning, skip headers
	mov	cl, 'B'
	jmp	short Player_ProcessKey_B_or_F

Player_ProcessKey_Forwards:
	;; In order to fast-forward 5 seconds, set the file pointer
	;; to CUR_SEEK + 5 * Freq

	mov	cl, 'F'
	;jmp	short Player_ProcessKey_B_or_F

	; 01/01/2025 (cgaplay.asm, 16bit registers)
	; 01/12/2024 (32bit registers)
Player_ProcessKey_B_or_F:
	; 17/11/2024
	; 04/11/2024
	; (Ref: player.asm, Matan Alfasi, 2017)
  
	; 04/11/2024
	mov	ax, 5
	mov	bx, [WAVE_BlockAlign]
	mul	bx
	mov	bx, [WAVE_SampleRate]
	mul	bx
	; dx:ax = transfer byte count for 5 seconds
	
	; 17/11/2024
	cmp	cl, 'B'
	mov	bx, [LoadedDataBytes]
	mov	cx, [LoadedDataBytes+2]
	jne	short move_forward ; cl = 'F'
move_backward:
	sub	bx, ax
	sbb	cx, dx
	jnc	short move_file_pointer
move_to_beginning:
	xor	cx, cx ; 0
	xor	bx, bx ; 0
	jmp	short move_file_pointer
move_forward: 
	add	bx, ax
	adc	cx, dx
	jc	short move_to_end
	cmp	cx, [DATA_SubchunkSize+2]
	ja	short move_to_end
	jb	short move_file_pointer
	cmp	bx, [DATA_SubchunkSize]
	jna	short move_file_pointer
move_to_end:
	mov	bx, [DATA_SubchunkSize]
	mov	cx, [DATA_SubchunkSize+2]
move_file_pointer:
	mov	dx, bx    
	mov	[LoadedDataBytes], dx
	mov	[LoadedDataBytes+2], cx
	add	dx, 44 ; + header
	adc	cx, 0

	; seek
	mov	bx, [filehandle]
	mov	ax, 4200h
	int	21h
	
	retn

; --------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
	; (video mode 13h, 320*200, 256 colors)
clear_window:
	push	es
	mov	di, 0A000h
	mov	es, di
	mov	di, (11*8*320)+(2*320) ; AC97 info start 
	sub	ax, ax
	mov	cx, (10*8*320)/2
	rep	stosw
	mov	[prev_points], ax ; 0
	pop	es
	retn

; -------------------------------------------------------------
; DATA (INFO)
; -------------------------------------------------------------

; 02/01/2025

Credits:
	db 'VGA WAV Player for Retro DOS by Erdogan Tan. '
	db 'January 2025.',10,13,0
	db '02/01/2025', 10,13,0
	db '23/01/2025', 10,13,0

msgAudioCardInfo:
	db  'for Sound Blaster 16 audio device.', 10,13,0

	; 02/01/2025
msg_usage:
	db 'usage: CGAPLAY2 <FileName1> <FileName2> <...>',10,13,0

	; 24/11/2024
noDevMsg:
	db 'Error: Unable to find Sound Blaster 16 audio device!'
	db 10,13,0

noFileErrMsg:
	db 'Error: file not found.',10,13,0

msg_error: ; 30/05/2024

; 24/11/2024
msg_init_err:
	db 0Dh, 0Ah
	db "Sound Blaster 16 hardware initialization error !"
	db 0Dh, 0Ah, 0

; 19/11/2024
; 03/06/2017
hex_chars:
	db "0123456789ABCDEF", 0

; 24/11/2024
msgSB16Info:
	db 0Dh, 0Ah
	db " Audio Hardware: Sound Blaster 16", 0Dh, 0Ah 
	db "      Base Port: "
msgBasePort:
	db "000h", 0Dh, 0Ah 
	db "            IRQ: "
msgIRQ:
	db 30h
	db 0Dh, 0Ah, 0

align 4

; -------------------------------------------------------------

	; 30/12/2024
PlayingScreen:
	db  14 dup(219), " DOS Player ", 14 dup(219)
	db  201, 38 dup(205), 187
	db  186, " <Space> Play/Pause <N>/<P> Next/Prev ", 186
	db  186, " <S>     Stop       <Enter> Color     ", 186
	db  186, " <F>     Forwards   <+>/<-> Volume    ", 186
	db  186, " <B>     Backwards  <Q>     Quit Prg  ", 186
	db  204, 38 dup(205), 185
	db  186, " File:              Bits:     0       ", 186
	db  186, " Freq: 0     Hz     Channels: 0       ", 186
	db  200, 38 dup(205), 188
	db  40 dup(32)
improper_samplerate_txt:
read_error_txt:
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(32)
	db  40 dup(205)
	db  40 dup(32)
	db  13 dup(32), "00:00 ", 174, 175, " 00:00", 4 dup(32), "VOL 000%"
	;db  40 dup(32) ; not necessary
	db 0

; -------------------------------------------------------------

; 31/12/2024
	; 30/12/2024
;fillblock:
;	times 8 db 0FFh
;	dw 0

; -------------------------------------------------------------

; 30/12/2024
; 23/11/2024
colors:
	db 0Fh, 0Bh, 0Ah, 0Ch, 0Eh, 09h, 0Dh
	; white, cyan, green, red, yellow, blue, magenta
ccolor:	db 0Bh	; cyan

; -------------------------------------------------------------

; 02/05/2024
; 24/11/2024
half_buffer:
	db 1	; dma half buffer 1 or 2 (0 or 1)
		; (initial value = 1 -> after xor in TuneLoop -> 0)
EOF: 

; -------------------------------------------------------------

bss:

ABSOLUTE bss

; 02/01/2025 (SB16 modifications) -cgaplay2.asm-
; 01/01/2025 (16bit modifications) -AC97, cgaplay.asm-

alignb 2

; 24/12/2024
wpoints_dif:	; wave lighting points factor (differential) 
	resw 1	; required bytes for 1/18 second wave lighting
; 01/01/2025
;graphstart:
;	resw 1	; start (top) line/row for wave lighting points 	 

columns:
	resb 1
pbprev:	resb 1 ; previous progress bar indicator position

;alignb 2

bss_start:

; 30/12/2024
prev_points:
	resw 320 ; previous wave points (which are lighting)

; 02/01/2025
; 24/11/2024
old_irq5v_o:
	resw 1
old_irq5v_s:
	resw 1
old_irq7v_o:
	resw 1
old_irq7v_s:
	resw 1
;
IRQnum:	resb 1
; 27/11/2024
audio_intr:
	resb 1
; 25/11/2024
IRQstatus:
	resb 1	

; 18/11/2024
stopped:
	resb 1
tLO:	resb 1
; 21/11/2024
;tLP:	resb 1
; 30/12/2024
wpoints:
	resb 1
pbuf_o:	resw 1
; 29/12/2024
pbuf_s:	resw 1

; 25/12/2024
; 29/11/2024
command:
	resb 1
filecount:
	resb 1

; 30/11/2024
alignb 4

;;;;;;;;;;;;;;
; 14/11/2024
; (Ref: player.asm, Matan Alfasi, 2017)  
WAVFILEHEADERbuff:
RIFF_ChunkID:
	resd 1	; Must be equal to "RIFF" - big-endian
		; 0x52494646
RIFF_ChunkSize:
	resd 1	; Represents total file size, not 
        	; including the first 2 fields 
		; (Total_File_Size - 8), little-endian
RIFF_Format:
	resd 1	; Must be equal to "WAVE" - big-endian
		; 0x57415645

;; WAVE header parameters ("Sub-chunk")
WAVE_SubchunkID:
	resd 1	; Must be equal to "fmt " - big-endian
		; 0x666d7420
WAVE_SubchunkSize:
	resd 1	; Represents total chunk size
WAVE_AudioFormat:
	resw 1	; PCM (Raw) - is 1, other - is a form 
		; of compression, not supported.
WAVE_NumChannels:
	resw 1	; Number of channels, Mono-1, Stereo-2
WAVE_SampleRate:
	resd 1	; Frequency rate, in Hz (8000, 44100 ...)
WAVE_ByteRate:
	resd 1	; SampleRate * NumChannels * BytesPerSample
WAVE_BlockAlign:
	resw 1	; NumChannels * BytesPerSample
		; Number of bytes for one sample.
WAVE_BitsPerSample:
	resw 1	; 8 = 8 bits, 16 = 16 bits, etc.

;; DATA header parameters
DATA_SubchunkID:
	resd 1	; Must be equal to "data" - big-endian
        	; 0x64617461
DATA_SubchunkSize:
	resd 1	; NumSamples * NumChannels * BytesPerSample
        	; Number of bytes in the data.
;;;;;;;;;;;;;;

; 01/01/2025

flags:	resb 1
; 06/11/2023
ac97_int_ln_reg:
	resb 1
filehandle:
	resw 1

; 01/01/2025
; 28/11/2024
PSP_CurrentOffset:
	resw 1

; 30/05/2024
wav_file_name:
	resb 80	; wave file, path name (<= 80 bytes)
	resw 1	; 30/11/2024

; 08/11/2023
; 07/11/2023
fbs_shift:
	resb 1
; 07/12/2024
SRB:	resb 1

; 02/01/2025
audio_io_base:
	resw 1	; Sound Blaster 16 base port address (220h)

; 02/01/2025
UpdateWavePoints:
	resw 1	; wave lighting procedure pointer (8m,16m,8s,16s)
		
; 01/01/2025
; 14/11/2024
TotalTime:
	resw 1	; Total (WAV File) Playing Time in seconds
ProgressTime:
	resw 1
count:	resw 1	; byte count of one (wav file) read
LoadedDataBytes:
	resd 1	; total read/load count

timerticks:
	resd 1	; (to eliminate excessive lookup of events in tuneloop)
		; (in order to get the emulator/qemu to run correctly)
alignb 16

; 02/01/2025
; 24/11/2024
dma_buffer:	; 32768 bytes	
wav_buffer1:
	resb dma_buffer_size/2 ; 16384
wav_buffer2:
	resb dma_buffer_size/2 ; 16384

; 14/11/2024
bss_end:
