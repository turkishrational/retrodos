; ****************************************************************************
; playwav9.asm (for Retro DOS)
; ----------------------------------------------------------------------------
; PLAYWAV9.COM ! Sound Blaster 16 (DOS) .WAV PLAYER program by Erdogan TAN
;
; 24/11/2024
;
; [ Last Modification: 18/12/2024 ]
;
; Modified from PLAYWAV8.COM .wav player program by Erdogan Tan, 23/11/2024
;
; Assembler: FASM 1.73
;	     fasm playwav9.asm PLAYWAV9.COM	
; ----------------------------------------------------------------------------
; In the visualization part of the code, the source code of Matan Alfasi's
; (Ami-Asaf) player.exe program was partially used.
; ----------------------------------------------------------------------------
; Previous versions of this Wav Player were based in part on .wav file player
; (for DOS) source code written by Jeff Leyla in 2002.

; playwav8.asm (23/11/2024)

; TUNELOOP version ; 27/11/2024
;	(running in DOSBOX, VIRTUALBOX is ok, QEMU fails)
;	((checking SB16 interrupt status via mixer register 82h))

; CODE

	; 13/11/2024
macro sys_msg op1,op2
{   	; 30/05/2024
	mov	si, op1	; message
  	mov	bl, op2	; text color
	xor	bh, bh	; video page 0
	mov	ah, 0Eh
	call	p_msg
}

	; 24/11/2024
macro SbOut op1
{
local .wait
.wait:
	in	al, dx
	or	al, al
	js	short .wait
	mov	al, op1	; command
	out	dx, al
}

; player internal variables and other equates.
; 17/11/2024
;BUFFERSIZE	equ 65520
; 24/11/2024
;dma_buffer_size equ 32768
;LOADSIZE	equ 16384
ENDOFFILE	equ 1		; flag for knowing end of file
; 27/11/2024
dma_buffer_size equ 44100
LOADSIZE	equ 22050

use16

org 100h

_STARTUP:
	; 30/05/2024
	; Prints the Credits Text.
	sys_msg Credits, 0Bh

	; 30/05/2024
        call    setFree		; deallocate unused DOS mem

	; 17/02/2017
	; Clear BSS (uninitialized data) area
	xor	ax, ax ; 0
	mov	cx, (bss_end - bss_start)/2
	mov	di, bss_start
	rep	stosw

	; 24/11/2024
	; Detect (& Reset) Sound Blaster 16 Audio Device
	call	DetectSB16
	jnc	short GetFileName

	; 30/05/2024
_dev_not_ready:
	; couldn't find the audio device!
	sys_msg noDevMsg, 0Fh
        jmp     Exit

	; 30/05/2024
GetFileName:
	mov	di, wav_file_name 
	mov	si, 80h
	mov	bl, [si]
	xor	bh, bh
	inc	bx
	mov	byte [si+bx], 0	; make AsciiZ filename.
	inc	si
ScanName:
	lodsb
	test	al, al
	jz	pmsg_usage
	cmp	al, 20h
	je	short ScanName	; scan start of name.
	stosb
	mov	ah, 0FFh
	;;;
	; 14/11/2024
	; (max. path length = 64 bytes for MSDOS ?) (*)
	xor	cx, cx ; 0
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
	and	al, al
	;jnz	short a_1
	;;;
	; 14/11/2024
	jz	short a_3
	and	ah, ah
	jz	short a_2
	cmp	al, '\'
	jne	short a_2
	mov	ah, 0
a_2:
	cmp	cl, 75	; 64+8+'.'+3 -> offset 75 is the last chr
	jb	short a_1
a_3:
	;;;
	or	ah, ah		; if period NOT found,
	jnz	short _1 	; then add a .WAV extension.
SetExt:
	dec	di
	mov	dword [di], '.WAV' ; ! 64+12 is DOS limit
				   ;   but writing +4 must not
				   ;   destroy the following data	 
	mov	byte [di+4], 0	   ; so, 80 bytes path + 0 is possible here
_1:
	call	write_audio_dev_info

; open the file
        ; open existing file
	; 14/11/2024
	;mov    al, OPEN	; open existing file
	mov	dx, wav_file_name
        call    openFile ; no error? ok.
        jnc     short getwavparms ; 14/11/2024

; file not found!
	sys_msg noFileErrMsg, 0Ch
_exit_:
        jmp     Exit

getwavparms:
	; 14/11/2024
       	call    getWAVParameters
	jc	short _exit_	; nothing to do

	; 15/11/2024
	;; Set video mode to 03h (not necessary)
	mov	ax, 03h
	int	10h

	; 15/11/2024
	;; Get the cursor type
	mov	ah, 03h
	int	10h
	mov	[cursortype], cx ; save

	; 15/11/2024
	;; Set the cursor to invisible
	mov	ah, 01h
	mov	cx, 2607h
	int	10h

	;;; 14/11/2024
Player_SplashScreen:
	; 15/11/2024
	;xor	dx, dx
	;call	setCursorPosition
    
	;; Print the splash screen in white
	mov	ax, 1300h
	mov	bx, 000Fh
	mov	cx, 1999
	mov	dx, 0
    
	mov	bp, SplashScreen
	int	10h
	;;;

	;;;
	; 22/11/2024
	; set wave volume led addresses
	mov	bx, 13*80*2
	mov	bp, 80
	mov	di, wleds_addr
wleds_sa_1:
	mov	cx, 7
wleds_sa_2:
	mov	ax, 80*2
	mul	cx
	add	ax, bx
	stosw
	loop	wleds_sa_2
	mov	ax, bx
	stosw
	inc	bx
	inc	bx	
	dec	bp
	jnz	short wleds_sa_1
	;;;

	;;;
	; 23/11/2024
	cmp	byte [WAVE_NumChannels], 1
	ja	short stolp_s
stolp_m:
	cmp	byte [WAVE_BitsPerSample], 8
	ja	short stolp_m16
stolp_m8:
	mov	word [turn_on_leds], turn_on_leds_mono_8bit
	jmp	short stolp_ok
stolp_m16:
	mov	word [turn_on_leds], turn_on_leds_mono_16bit
	jmp	short stolp_ok
stolp_s:
	cmp	byte [WAVE_BitsPerSample], 8
	ja	short stolp_s16
stolp_s8:
	mov	word [turn_on_leds], turn_on_leds_stereo_8bit
	jmp	short stolp_ok
stolp_s16:
	mov	word [turn_on_leds], turn_on_leds_stereo_16bit
	jmp	short stolp_ok
stolp_ok:
	;;;

	;;; wait for 3 seconds
	;mov	cx, 002Dh
	;mov	dx, 0C6C0h
	;mov	ah, 86h
	;int	15h
	;;;
	; 26/11/2024
	mov	cx, 3*18
getticks:
	; 26/11/2024
	call	GetTimerTicks

	cmp	ax, [timerticks]
	jne	short chkws
	cmp	dx, [timerticks+2]
	je	short getticks
chkws:
	mov	[timerticks], ax
	mov	[timerticks+2], dx
	loop	getticks
	;;;	
	
	;;;
Player_Template:
	xor	dx, dx
	call	setCursorPosition

	;; Print the splash screen in white
	mov	ax, 1300h
	mov	bx, 000Fh
	mov	cx, 1999
	mov	dx, 0
    
	mov	bp, Template
	int	10h
	;;;
	
	; 14/11/2024
	call	SetTotalTime
	call	UpdateFileInfo

PlayNow:
	; 24/11/2024
	mov	al, 5	; 15 = max, 0 = min
	; 27/11/2024
	mov	[volume], al
	; 15/11/2024
	call	SetMasterVolume
	call	UpdateVolume
	;;;

	;;;
	;
	; 14/11/2024
	call	UpdateProgressBar
	;;;

 	; 30/05/2024
	; playwav4.asm
_2:	
	call	check4keyboardstop	; flush keyboard buffer
	jc	short _2		; 07/11/2023

; play the .wav file. Most of the good stuff is in here.

	call    PlayWav

Exit@@:
	; 27/11/2024
	; 24/11/2024
	; restore old interrupt vector
	;mov	al, [IRQnum]
	;xor	ah, ah ; reset
	;call	set_hardware_int_vector

; close the .wav file and exit.

Exit:
	; 15/11/2024
	;; Restore Cursor Type
	mov	cx, [cursortype]	
	cmp	cx, 0
	jz	short Exit@
	mov	ah, 01h
	int	10h
Exit@:
        call    closeFile
         
	mov	ax, 4C00h	; bye !
	int	21h
here:
	jmp	short here	; do not come here !

	; 30/05/2024
pmsg_usage:
	sys_msg msg_usage, 0Fh	; 14/11/2024 
	jmp	short Exit

	; 30/05/2024
init_err:
	sys_msg msg_init_err, 0Fh
	jmp	short Exit

	; --------------------------------------------
	
	; 24/11/2024
PlayWav:
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

	; 19/11/2024
	mov	byte [wleds], 1

	mov	ax, [WAVE_SampleRate]
	mov	cx, 10
	mul	cx
	mov	cl, 182
	div	cx
	; ax = samples per 1/18.2 second
	mov	cl, byte [WAVE_BlockAlign] 
	mul	cx
	mov	[wleds_dif], ax ; buffer read differential (distance)
				; for wave volume leds update
				; (byte stream per 1/18.2 second)
	; 27/11/2024
	; set audio interrupt vector (to user's handler)
	;mov	al, [IRQnum]
	;mov	ah, 1 ; set
	;mov	dx, IRQ_service
	;call	set_hardware_int_vector

	; 26/11/2024
	call	check4keyboardstop
	jc	_exitt_

	;mov	byte [IRQnum], 0

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
	cmp	byte [tLO], '0'
	je	short tLWait
	call	tLZ
	mov	byte [tLO], '0'
	jmp	short tLWait
tL1:
	; 27/11/2024
	; Check SB 16 interrupt status
    	mov	al, 82h
	mov	dx, [audio_io_base]
	add	dl, 4	; 2x4h
	out	dx, al
	inc	dx	; 2x5h		
	in	al, dx
	
	test	al, 00000011b
	jnz	short tL3
tL2:
	call	checkUpdateEvents
	jc	_exitt_
	jmp	short tLWait
tL3:
	; 27/11/2024
	;mov	dx, [audio_io_base]
	;;add	dx, 0Eh
	;add	dl, 0Eh ; 8bit DMA-mode int ack
	add	dl, 0Eh-05h
	in	al, dx	; SB acknowledge.
	inc	dx ; 0Fh ; 16bit DMA-mode int ack
	in	al, dx	; SB 16 acknowledge.

	xor	byte [half_buffer], 1

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

	; 06/11/2023
tL0:
	; 08/11/2023
	; 05/11/2023
	; 17/02/2017 - Buffer switch test (temporary)
	; 06/11/2023
	; al = buffer indicator ('1', '2' or '0' -stop- )

	push	ds
	;push	bx 
	mov	bx, 0B800h ; video display page segment
	mov	ds, bx
	sub	bx, bx ; 0
	mov	ah, 4Eh
	mov	[bx], ax ; show current play buffer (1, 2)
	;pop	bx
	pop	ds

	retn

; -------------------------------------------

; 27/11/2024	
;	; 24/11/2024
;IRQ_ack:
;	; 26/11/2024
;	push	dx
;	push	ax
;	mov     dx, [audio_io_base]
;	;add	dx, 0Eh
;	add	dl, 0Eh ; 8bit DMA-mode int ack
;	; 25/11/2024
;	cmp	byte [WAVE_BitsPerSample], 8
;	jna	short irq_ack_@
;	inc	dl ; 0Fh ; 16bit DMA-mode int ack
;irq_ack_@:
;	in	al, dx	; SB acknowledge.
;	; 27/11/2024
;	;mov	al, 20h
;	;out	20h, al	; Hardware acknowledge.
;irq_ack_ok:
;	; 26/11/2024
;	pop	ax
;	pop	dx
;	retn

; -------------------------------------------

	; 24/11/2024
SetMasterVolume:
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

;	; 27/11/2024
;IRQ_service:
;	push	ds
;	push	dx
;	push	ax
;	;
;	push	cs
;	pop	ds
;	mov	byte [IRQnum], 5
;	mov     dx, [audio_io_base]
;	;add	dx, 0Eh
;	add	dl, 0Eh ; 8bit DMA-mode int ack
;	cmp	byte [WAVE_BitsPerSample], 8
;	jna	short irq_ack_@
;	inc	dl ; 0Fh ; 16bit DMA-mode int ack
;irq_ack_@:
;	in	al, dx	; SB acknowledge.
;	;
;	mov	al, 20h
;	out	20h, al	; Hardware acknowledge
;	;
;	pop	ax
;	pop	dx
;	pop	ds
;	iret

; ----------------------------------

	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_stop procedure (06/08/2022, v2.0.5)	
sb16_stop:
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

	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_pause procedure (06/08/2022, v2.0.5)	
sb16_pause:
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

	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_continue procedure (06/08/2022, v2.0.5)
sb16_play:
sb16_continue:
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

	; 27/11/2024
	; frequency limit (for SB16) = 44100 kHz
	;cmp	word [WAVE_SampleRate], 44101 ; 48000
	;cmc
	;retn
 
gwavp_stc_retn:
	stc
gwavp_retn:
	retn

; ----	30/05/2024 (playwav4.asm, 19/05/2024)

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

	mov	ah, 4Ah		; pass new length to DOS
	int	21h

	retn			; back to caller 
				; new size (allocated memory) = 64KB

	; 27/11/2024
;memAlloc:
;; input: AX = # of paragraphs required
;; output: AX = segment of block to use
;
;	push	bx
;	mov	bx, ax
;	mov	ah, 48h
;	int	21h
;	pop	bx
;	retn

; ----

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
	mov	dl, bl
	;and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgBasePort], al

	xor	ax, ax
	mov	al, [IRQnum]
	;mov	cl, 10
	;div	cl
	;add	ah, 30h
	;mov	[msgIRQ], ah
	; 25/11/2024
	add	al, 30h
	mov	[msgIRQ], al

	call 	clear_window
	mov	dh, 13
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
	;      SbInit_play procedure (06/08/2024, v2.0.5)	 
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

	; 24/11/2024 (SB16 version)
checkUpdateEvents:
	call	check4keyboardstop
	jc	short c4ue_ok

	; 18/11/2024
	push	ax ; *
	or	ax, ax
	jz	c4ue_cpt

	; 18/11/2024
	cmp	al, 20h ; SPACE (spacebar) ; pause/play
	jne	short ch4ue_chk_s
	cmp	byte [stopped], 0
	ja	short ch4ue_chk_ps
	; pause
	call	sb16_pause	; 24/11/2024
	; 27/11/2024
	mov	byte [stopped], 1
	jmp	c4ue_cpt
ch4ue_chk_ps:
	cmp	byte [stopped], 1
	ja	short ch4ue_replay
	; continue to play (after a pause)
	call	sb16_play	; 24/11/2024
	; 27/11/2024
	mov	byte [stopped], 0
	jmp	short c4ue_cpt
ch4ue_replay:
	; 19/11/2024
	pop	ax ; *
	pop	ax ; return address
	; 24/11/2024
	; initialize (again)
	; and start playing (after stop)
	call	SB16Init_play
	mov	al, [volume]
	call	SetMasterVolume	; 24/11/2024
	mov	byte [stopped], 0
	; 24/11/2024
	mov	byte [half_buffer], 1
	call	move_to_beginning
	jmp	PlayWav	

ch4ue_chk_s:
	cmp	al, 'S'	; stop
	jne	short ch4ue_chk_fb
	cmp	byte [stopped], 0
	ja	short c4ue_cpt ; Already stopped/paused
	call	sb16_stop	; 24/11/2024	
	; 19/11/2024
	mov	byte [tLO], 0
	jmp	short c4ue_cpt	

ch4ue_chk_fb:
	; 17/11/2024
	cmp	al, 'F'
	jne	short c4ue_chk_b
	call 	Player_ProcessKey_Forwards
	jmp	short c4ue_cpt
	
	; 18/11/2024
c4ue_ok:
	retn

c4ue_chk_b:
	cmp	al, 'B'
	; 19/11/2024
	jne	short c4ue_chk_h
	call 	Player_ProcessKey_Backwards
	jmp	short c4ue_cpt
c4ue_chk_h:
	; 19/11/2024
	cmp	al, 'H'
	jne	short c4ue_chk_cr
	mov	byte [wleds], 0
	call 	write_sb16_dev_info
	mov	dh, 24
	mov	dl, 79
	call	setCursorPosition
c4ue_chk_cr:
	; 19/11/2024
	cmp	al, 0Dh ; ENTER/CR key
	jne	short c4ue_cpt
	; 23/11/2024
	xor	bx, bx
	mov	bl, [wleds]
	inc	bl
	and	bl, 0Fh
	jnz	short c4ue_sc
	inc	bx
c4ue_sc:
	mov	[wleds], bl	
	shr	bl, 1
	mov	al, [bx+colors]
	jnc	short c4ue_sc_@
	or	al, 10h ; blue (dark) background
c4ue_sc_@:
	mov	[ccolor], al
	;;;
c4ue_cpt:
	;push	ds
	;mov	bx, 40h
	;mov	ds, bx
	;mov	bx, 6Ch  ; counter (INT 08h, 18.2 ticks per sec)
	;;cli
	;mov	ax, [bx]
	;mov	dx, [bx+2]
	;;sti
	;pop	ds
	; 26/11/2024
	call	GetTimerTicks

	; 18/11/2024
	pop	cx ; *
	cmp	dx, [timerticks+2]
	jne	short c4ue_utt
	cmp	ax, [timerticks]
	; 18/11/2024
	je	short c4ue_skip_utt
c4ue_utt:	
	mov	[timerticks], ax
	mov	[timerticks+2], dx
	jmp	short c4ue_cpt_@
c4ue_skip_utt:
	; 18/11/2024
	and	cx, cx
	jz	short c4ue_ok
c4ue_cpt_@:
	; 18/11/2024
	cmp	byte [stopped], 0
	ja	short c4ue_ok
	
	call	CalcProgressTime

	cmp	ax, [ProgressTime]
	; 23/11/2024
	je	short c4ue_uvb
			; same second, no need to update

	call	UpdateProgressBar

	; 23/11/2024
c4ue_uvb:
	cmp	byte [wleds], 0
	jna	short c4ue_vb_ok

	call	UpdateWaveLeds

c4ue_vb_ok:
	retn

	; 26/11/2024
GetTimerTicks:
	push	ds
	mov	bx, 40h
	mov	ds, bx
	mov	bx, 6Ch  ; counter (INT 08h, 18.2 ticks per sec)
	;cli
	mov	ax, [bx]
	mov	dx, [bx+2]
	;sti
	pop	ds
	retn

; --------------------------------------------------------
; 19/05/2024 - (playwav4.asm) ich_wav4.asm
; --------------------------------------------------------

	; 24/11/2024 (SB16 version)
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

	;;;
	; 19/05/2024 (change PCM out volume)
	cmp	al, '+'
	jne	short p_1
	
	mov	al, [volume]
	; 24/11/2024
	cmp	al, 15
	jnb	short p_3
	inc	al
	jmp	short p_2
p_1:
	cmp	al, '-'
	jne	short p_4

	mov	al, [volume]
	; 24/11/2024
	cmp	al, 0
	jna	short p_3
	dec	al
p_2:
	mov	[volume], al
	; 24/11/2024
	call	SetMasterVolume
	;call	UpdateVolume
	;;clc
	;retn
	jmp	UpdateVolume
_cksr:
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
	cmp	al, 'B'
	je	short p_r
	cmp	al, 'F'
	je	short p_r
	cmp	al, 'Q'
	je	short p_q
	
	clc
	retn

	;;;
;_cskr:	
p_q:
	stc
p_r:
	retn

; --------------------------------------------------------

	; 14/11/2024
setCursorPosition:
	; dh = Row
	; dl = Column
	mov	ax, 0500h
	int	10h
	mov	ah, 02h
	mov	bh, 00h
	;mov	dh, setCursorPosition_Row
	;mov	dl, setCursorPosition_Column
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

SetTotalTime:
	;; Calculate total seconds in file
	mov	ax, [DATA_SubchunkSize]
	mov	dx, [DATA_SubchunkSize + 2]
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

	mov	dh, 24
	mov	dl, 42
	call	setCursorPosition

	pop	ax ; *
	xor	ah, ah
	mov	bp, 2
	call	PrintNumber
	
	mov	dh, 24
	mov	dl, 45
	call	setCursorPosition

	pop	ax ; **
	mov	al, ah
	xor	ah, ah
	;mov	bp, 2
	;jmp	short PrintNumber

; --------------------------------------------------------

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
	mov	dl, '0'
	add	dl, al
	mov	ah, 02h
	int	21h
	loop	printNumber_printloop
	
	retn

; --------------------------------------------------------

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

	mov	dh, 24
	mov	dl, 33
	call	setCursorPosition

	pop	ax ; *
	xor	ah, ah
	mov	bp, 2
	call	PrintNumber
	
	mov	dh, 24
	mov	dl, 36
	call	setCursorPosition

	pop	ax ; **
	mov	al, ah
	xor	ah, ah
	;mov	bp, 2
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

UpdateFileInfo:
	;; Print File Name
	mov	dh, 9
	mov	dl, 23
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

	call	PrintString
	
	;; Print Frequency
	mov	dh, 10
	mov	dl, 23
	call	setCursorPosition
	mov	ax, [WAVE_SampleRate]
	mov	bp, 5
	call	PrintNumber

	;; Print BitRate
	mov	dh, 9
	mov	dl, 57
	call	setCursorPosition
	mov	ax, [WAVE_BitsPerSample]
	mov	bp, 2
	call	PrintNumber

	;; Print Channel Number
	mov	dh, 10
	mov	dl, 57
	call	setCursorPosition
	mov	ax, [WAVE_NumChannels]
	mov	bp, 1
	call	PrintNumber

	;call	UpdateVolume
	;retn

; --------------------------------------------------------

	; 24/11/2024
	; 14/11/2024
UpdateVolume:
	;; Print Volume
	mov	dh, 24
	mov	dl, 75
	call	setCursorPosition

	mov	al, [volume]

	mov	bl, 100
	mul	bl

	mov	bl, 15 ; 24/11/2024
	div	bl

	xor	ah, ah
	mov	bp, 3
	;call	PrintNumber
	;retn
	jmp	PrintNumber

; 24/11/2024
; 29/05/2024
; 19/05/2024
volume: db	12

; --------------------------------------------------------

	; 14/11/2024
PrintString:
	; si = string address 
	mov	bx, 0Fh	 ; white
	mov	ah, 0Eh	 ; write as tty 	
printstr_loop:
	lodsb
	or	al, al
	jz	short printstr_ok
	int	10h
	jmp	short printstr_loop
printstr_ok:
	retn

; --------------------------------------------------------

	; 14/11/2024
	; (Ref: player.asm , Matan Alfasi, 2017)
	; (Modification: Erdogan Tan, 14/11/2024)

	PROGRESSBAR_ROW equ 23

UpdateProgressBar:
	call	SetProgressTime	; 14/11/2024

	mov	ax, [ProgressTime]
UpdateProgressBar@:
	mov	dx, 80
	mul	dx
	mov	bx, [TotalTime]
	div	bx

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

UpdateProgressBar_DrawProgress:
	mov	cx, ax
	mov	ah, 09h
	mov	al, 223
	mov	bx, 0Fh
	int	10h

UpdateProgressBar_DrawCursor:
	;mov	ax, cx
	mov	dh, PROGRESSBAR_ROW
	;mov	dl, al
	dec	cx
	mov	dl, cl
	call	setCursorPosition

	mov	ah, 09h
	mov	al, 223
	mov	bx, 0Ch
	mov	cx, 1
	int	10h

UpdateProgressBar_Clean:
	pop	ax  ; **
	mov	cx, ax
	mov	dh, PROGRESSBAR_ROW
	mov	dl, al
	call	setCursorPosition

	neg	cx
	add	cx, 80 ; cf = 1 ; +
	;; CX = No. of times to print a clean character
	;mov	cx, 80
	;sub	cx, ax
	;; 09h = Write character multiple times
	mov	ah, 09h
	;; 32 = Space ASCII code
	;mov	al, 32
	;mov	bx, 0
	; 15/11/2024
	mov	al, 223
	mov	bx, 8
	int	10h
	; 14/11/2024
	clc	; +

	retn

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

	; 19/11/2024
UpdateWaveLeds:
	; 23/11/2024
	call	reset_wave_leds
	;call	word [turn_on_leds]
	;retn
	jmp	word [turn_on_leds]

; --------------------------------------------------------

	; 23/11/2024
	; 19/11/2024
clear_window:
	xor	ax, ax
	jmp	short clear_window_@

reset_wave_leds:
	; 23/11/2024
	;mov	al, 254
	;mov	ah, 8 ; gray (dark)
	mov	ax, 08FEh
clear_window_@:
	push	es
	mov	di, 0B800h
	mov	es, di
	mov	di, 2080 ; 13*80*2
	mov	cx, 8*80 ; 8 rows
	rep	stosw
	pop	es
	retn

; --------------------------------------------------------

	; 24/11/2024
	; 19/11/2024
turn_on_leds_stereo_16bit:
	; 25/11/2024
	push	es

	cmp	byte [tLO],'2'
	jne	short tol_buffer_1

tol_buffer_2:
	; 21/11/2024
	mov	si, wav_buffer2 ; 24/11/2024
	jmp	short tol_@

tol_buffer_1:
	cmp	byte [tLO],'1'
	;jne	short tol_retn
	; 23/11/2024
	jne	short tol_clc_retn

	mov	si, wav_buffer1 ; 24/11/2024
tol_@:
	; calculate differential
	cmp	[pbuf_s], si
	jne	short tol_ns_buf
	mov	bx, [wleds_dif]
	mov	si, [pbuf_o]
	; 24/11/2024
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wleds_dif]
	add	si, bx
	jc	short tol_o_@
	cmp	si, cx
	jna	short tol_s_buf
tol_o_@:
	mov	si, cx
	jmp	short tol_s_buf

tol_clc_retn:
	; 23/11/2024
	clc
tol_retn:
	; 25/11/2024
	pop	es
	retn

tol_ns_buf:
	mov	[pbuf_s], si
	xor	si, si	; 0
tol_s_buf:
	mov	[pbuf_o], si

tol_buf_@:
	; 25/11/2024
	mov	di, 0B800h
	mov	es, di
	;mov	di, (20*80*2)-2
	
	; 23/11/2024
	mov	cx, 80

	; 22/11/2024
	mov	bx, wleds_addr

	; 27/11/2024
	add	si, [pbuf_s]
tol_fill_c:
	; 22/11/2024
	;inc	di
	;inc	di
	;push	di
	lodsw	; left
	;shr	ax, 8
	mov	dx, ax
	lodsw	; right
	;shr	ax, 8
	;;;
	; 23/11/2024
	add	ax, dx
	shr	ax, 8
	;shr	ax, 9
	add	al, 80h
	shr	ax, 5
	;;;
	;shr	ax, 6

	push	bx
	shl	ax, 1
	add	bx, ax
	; 25/11/2024
	mov	di, [bx]
	; 23/11/2024
	mov	ah, [ccolor]
	mov	al, 254
	mov	[es:di], ax
	pop	bx
	add	bx, 16
	loop	tol_fill_c

	jmp	short tol_retn


	; 24/11/2024
	; 23/11/2024
turn_on_leds_mono_16bit:
	; 25/11/2024
	push	es

	cmp	byte [tLO],'2'
	jne	short tol2_buffer_1

tol2_buffer_2:
	mov	si, wav_buffer2 ; 24/11/2024
	jmp	short tol2_@

tol2_buffer_1:
	cmp	byte [tLO],'1'
	jne	short tol_clc_retn

	mov	si, wav_buffer1 ; 24/11/2024
tol2_@:
	; calculate differential
	cmp	[pbuf_s], si
	jne	short tol2_ns_buf
	mov	bx, [wleds_dif]
	mov	si, [pbuf_o]
	; 24/11/2024
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wleds_dif]
	add	si, bx
	jc	short tol2_o_@
	cmp	si, cx
	jna	short tol2_s_buf
tol2_o_@:
	mov	si, cx
	jmp	short tol2_s_buf

;tol2_clc_retn:
;	clc
;tol2_retn:
;	; 25/11/2024
;	pop	es
;	retn

tol2_ns_buf:
	mov	[pbuf_s], si
	xor	si, si	; 0
tol2_s_buf:
	mov	[pbuf_o], si

tol2_buf_@:
	; 25/11/2024
	mov	di, 0B800h
	mov	es, di
	
	mov	cx, 80

	mov	bx, wleds_addr

	; 27/11/2024
	add	si, [pbuf_s]
tol2_fill_c:
	lodsw
	shr	ax, 8
	add	al, 80h
	shr	ax, 5
	push	bx
	shl	ax, 1
	add	bx, ax
	; 25/11/2024
	mov	di, [bx]
	mov	ah, [ccolor]
	mov	al, 254
	mov	[es:di], ax
	pop	bx
	add	bx, 16
	loop	tol2_fill_c

	jmp	tol_retn

	; 24/11/2024
turn_on_leds_stereo_8bit:
	; 25/11/2024
	push	es

	cmp	byte [tLO],'2'
	jne	short tol3_buffer_1

tol3_buffer_2:
	mov	si, wav_buffer2 ; 24/11/2024
	jmp	short tol3_@

tol3_buffer_1:
	cmp	byte [tLO],'1'
	jne	short tol3_clc_retn

	mov	si, wav_buffer1 ; 24/11/2024
tol3_@:
	; calculate differential
	cmp	[pbuf_s], si
	jne	short tol3_ns_buf
	mov	bx, [wleds_dif]
	mov	si, [pbuf_o]
	; 24/11/2024
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wleds_dif]
	add	si, bx
	jc	short tol3_o_@
	cmp	si, cx
	jna	short tol3_s_buf
tol3_o_@:
	mov	si, cx
	jmp	short tol3_s_buf

tol3_clc_retn:
	clc
tol3_retn:
	; 25/11/2024
	pop	es
	retn

tol3_ns_buf:
	mov	[pbuf_s], si
	xor	si, si	; 0
tol3_s_buf:
	mov	[pbuf_o], si

tol3_buf_@:
	; 25/11/2024
	mov	di, 0B800h
	mov	es, di
	
	mov	cx, 80

	mov	bx, wleds_addr

	; 27/11/2024
	add	si, [pbuf_s]
tol3_fill_c:
	lodsw	; left (al), right (ah)
	add	al, ah
	add	al, 80h
	xor	ah, ah
	;shr	ax, 6
	shr	ax, 5
	push	bx
	shl	ax, 1
	add	bx, ax
	; 25/11/2024
	mov	di, [bx]
	mov	ah, [ccolor]
	mov	al, 254
	mov	[es:di], ax
	pop	bx
	add	bx, 16
	loop	tol3_fill_c

	jmp	short tol3_retn

	; 24/11/2024
	; 23/11/2024
turn_on_leds_mono_8bit:
	; 25/11/2024
	push	es

	cmp	byte [tLO],'2'
	jne	short tol4_buffer_1

tol4_buffer_2:
	mov	si, wav_buffer2 ; 24/11/2024
	jmp	short tol4_@

tol4_buffer_1:
	cmp	byte [tLO],'1'
	jne	short tol3_clc_retn

	mov	si, wav_buffer1 ; 24/11/2024
tol4_@:
	; calculate differential
	cmp	[pbuf_s], si
	jne	short tol4_ns_buf
	mov	bx, [wleds_dif]
	mov	si, [pbuf_o]
	; 24/11/2024
	mov	cx, LOADSIZE
	sub	cx, bx ; sub cx, [wleds_dif]
	add	si, bx
	jc	short tol4_o_@
	cmp	si, cx
	jna	short tol4_s_buf
tol4_o_@:
	mov	si, cx
	jmp	short tol4_s_buf

;tol4_clc_retn:
;	clc
;tol4_retn:
;	; 25/11/2024
;	pop	es
;	retn

tol4_ns_buf:
	mov	[pbuf_s], si
	xor	si, si	; 0
tol4_s_buf:
	mov	[pbuf_o], si

tol4_buf_@:
	; 25/11/2024
	mov	di, 0B800h
	mov	es, di
	
	mov	cx, 80

	mov	bx, wleds_addr

	; 27/11/2024
	add	si, [pbuf_s]
tol4_fill_c:
	lodsb
	; 27/11/2024
	add	ax, ax
	add	al, 80h
	xor	ah, ah
	shr	ax, 5
	push	bx
	shl	ax, 1
	add	bx, ax
	; 25/11/2024
	mov	di, [bx]
	mov	ah, [ccolor]
	mov	al, 254
	mov	[es:di], ax
	pop	bx
	add	bx, 16
	loop	tol4_fill_c

	jmp	tol3_retn

; --------------------------------------------------------

; 30/05/2024
print_msg:
	mov	bx, 07h
p_msg:
	push	es
	push	bp
	push	cx
	push	dx

	push	ds
	pop	es
	mov	bp, si
	mov	ah, 03h ; Return cursor position (in DX)
	; bh = video page number 	
	int	10h
	xor	cx, cx
p_msg_0:	
	lodsb
	or	al, al
	jz	short p_msg_1
	inc	cx
	jmp	short p_msg_0
p_msg_1:
	or	cx, cx
	jz	short p_msg_x
	; cx = number of chars
	; dx = screen (cursor) position
	; bl = color/attribute
	; bh = video page number
	; es:bp = string buffer
	;mov	al, 1 ; attribute in BL, update cursor pos
	;mov	ah, 13h ; write character string
 	mov	ax, 1301h
	int	10h
p_msg_x:
	pop	dx
	pop	cx
	pop	bp
	pop	es
	retn

; --------------------------------------------------------
; --------------------------------------------------------

; DATA

Credits:
	db	'Tiny WAV Player for Retro DOS by Erdogan Tan. '
	db	'December 2024.',10,13,0
	db	'18/12/2024', 10,13,0

msgAudioCardInfo:
	db 	'for Sound Blaster 16 audio device.', 10,13,0

msg_usage:
	db	'usage: playwav9 filename.wav',10,13,0 ; 24/11/2024

	; 24/11/2024
noDevMsg:
	db	'Error: Unable to find Sound Blaster 16 audio device!'
	db	10,13,0

noFileErrMsg:
	db	'Error: file not found.',10,13,0

msg_error:	; 30/05/2024

; 24/11/2024
msg_init_err:
	db	0Dh, 0Ah
	db	"Sound Blaster 16 hardware initialization error !"
	db	0Dh, 0Ah, 0 ; 07/12/2024

; 19/11/2024
; 03/06/2017
hex_chars:	db "0123456789ABCDEF", 0

; 24/11/2024
msgSB16Info:	db 0Dh, 0Ah
		db " Audio Hardware: Sound Blaster 16", 0Dh, 0Ah 
		db "      Base Port: "
msgBasePort:	db "000h", 0Dh, 0Ah 
		db "            IRQ: "
msgIRQ:		db 30h
		db 0Dh, 0Ah, 0

; --------------------------------------------------------
; 14/11/2024 (Ref: player.asm, Matan Alfasi, 2017)

SplashScreen:
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                     _______   ______        _______.                     ", 221, 219, 222
		db  221, 219, 222, "                    |       \ /  __  \      /       |                     ", 221, 219, 222
		db  221, 219, 222, "                    |  .--.  |  |  |  |    |   (----`                     ", 221, 219, 222
		db  221, 219, 222, "                    |  |  |  |  |  |  |     \   \                         ", 221, 219, 222
		db  221, 219, 222, "                    |  '--'  |  `--'  | .----)   |                        ", 221, 219, 222
		db  221, 219, 222, "                    |_______/ \______/  |_______/                         ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "     .______    __          ___   ____    ____  _______ .______           ", 221, 219, 222
		db  221, 219, 222, "     |   _  \  |  |        /   \  \   \  /   / |   ____||   _  \          ", 221, 219, 222
		db  221, 219, 222, "     |  |_)  | |  |       /  ^  \  \   \/   /  |  |__   |  |_)  |         ", 221, 219, 222
		db  221, 219, 222, "     |   ___/  |  |      /  /_\  \  \_    _/   |   __|  |      /          ", 221, 219, 222
		db  221, 219, 222, "     |  |      |  `----./  _____  \   |  |     |  |____ |  |\  \----.     ", 221, 219, 222
		db  221, 219, 222, "     | _|      |_______/__/     \__\  |__|     |_______|| _| `._____|     ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                WELCOME TO                                ", 221, 219, 222
		db  221, 219, 222, "                                DOS PLAYER                                ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  221, 219, 222, "                                                                          ", 221, 219, 222
		db  "                                                                                         "
Template:
		db  201, 78 dup(205), 187
		db  186, 33 dup(219), " DOS Player ", 33 dup(219), 186
		db  204, 78 dup(205), 185
		db  186, 33 dup(32), " User Guide ", 33 dup(32), 186
		db  186, 6  dup(32), "<Space>         Play/Pause    ", 4 dup(32), "<H>             Hardware Info", 9 dup(32), 186
		db  186, 6  dup(32), "<S>             Stop          ", 4 dup(32), "<Enter>         Wave Lighting", 9 dup(32), 186
		db  186, 6  dup(32), "<F>             Forwards      ", 4 dup(32), "<+>/<->         Inc/Dec Volume", 8 dup(32), 186
		db  186, 6  dup(32), "<B>             Backwards     ", 4 dup(32), "<Q>             Quit Program ", 9 dup(32), 186
		db  204, 78 dup(205), 185
		db  186, 6  dup(32), "File Name :                   ", 4 dup(32), "Bit-Rate  :     0  Bits      ", 9 dup(32), 186
		db  186, 6  dup(32), "Frequency :     0     Hz      ", 4 dup(32), "#-Channels:     0            ", 9 dup(32), 186
		db  200, 78 dup(205), 188
		db  80 dup(32)
improper_samplerate_txt:			; 03/11/2024
read_error_txt:
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(32)
		db  80 dup(205)
		db  80 dup(32)
		db  33 dup(32), "00:00 ", 174, 175, " 00:00", 24 dup(32), "VOL 000%"

; 23/11/2024
colors:		db 0Fh, 0Bh, 0Ah, 0Ch, 0Eh, 09h, 0Dh, 0Fh
			; white, cyan, green, red, yellow, blue, magenta
ccolor:		db 0Bh	; cyan

; 24/11/2024
half_buffer:	db 1	; dma half buffer 1 or 2 (0 or 1)
			; (initial value = 1 -> after xor in TuneLoop -> 0)
EOF: 

; BSS

align 2

; 24/11/2024
; 22/11/2024
; wave volume leds address array
wleds_addr:	rw 80*8 ; rb 2*80*8

; 24/11/2024 (SB16 version of playwav8.com -> playwav9.com)
; 14/11/2024
; 17/02/2017
bss_start:

; 13/11/2024
; ('resb','resw','resd' to 'rb','rw','rd' conversions for FASM)

; 24/11/2024
old_irq5v_o:	rw 1
old_irq5v_s:	rw 1
old_irq7v_o:	rw 1
old_irq7v_s:	rw 1
;
IRQnum:		rb 1
; 25/11/2024
IRQstatus:	rb 1
		rb 1
; 18/11/2024
stopped:	rb 1
tLO:		rb 1
; 19/11/2024
wleds:		rb 1
wleds_dif:	rw 1
pbuf_s:		rw 1
pbuf_o:		rw 1

; 25/11/2024
align 4

;;;;;;;;;;;;;;
; 14/11/2024
; (Ref: player.asm, Matan Alfasi, 2017)  
WAVFILEHEADERbuff:
RIFF_ChunkID:	rd 1	; Must be equal to "RIFF" - big-endian
			; 0x52494646
RIFF_ChunkSize:
		rd 1	; Represents total file size, not 
                        ; including the first 2 fields 
			; (Total_File_Size - 8), little-endian
RIFF_Format:
		rd 1	; Must be equal to "WAVE" - big-endian
			; 0x57415645

;; WAVE header parameters ("Sub-chunk")
WAVE_SubchunkID:
		rd 1	; Must be equal to "fmt " - big-endian
			; 0x666d7420
WAVE_SubchunkSize:
		rd 1	; Represents total chunk size
WAVE_AudioFormat:
		rw 1	; PCM (Raw) - is 1, other - is a form 
			; of compression, not supported.
WAVE_NumChannels:
		rw 1	; Number of channels, Mono-1, Stereo-2
WAVE_SampleRate:
		rd 1	; Frequency rate, in Hz (8000, 44100 ...)
WAVE_ByteRate:	rd 1	; SampleRate * NumChannels * BytesPerSample
WAVE_BlockAlign:
		rw 1	; NumChannels * BytesPerSample
			; Number of bytes for one sample.
WAVE_BitsPerSample:
		rw 1	; 8 = 8 bits, 16 = 16 bits, etc.

;; DATA header parameters
DATA_SubchunkID:
		rd 1	; Must be equal to "data" - big-endian
                        ; 0x64617461
DATA_SubchunkSize:
		rd 1	; NumSamples * NumChannels * BytesPerSample
                        ; Number of bytes in the data.
;;;;;;;;;;;;;;

; 15/11/2024
cursortype:	rw 1

filehandle:	rw 1

flags:		rb 1	; (END_OF_FILE flag)
		rb 1

audio_io_base:	rw 1	; Sound Blaster 16 base port address (220h)

; 30/05/2024
wav_file_name:
		rb 80	; wave file, path name (<= 80 bytes)

		rw 1
; 24/11/2024
align 4

; 23/11/2024
turn_on_leds:	rw 1	; turn_on_leds procedure pointer (m8,m16,s8,s16)

; 14/11/2024
TotalTime:	rw 1	; Total (WAV File) Playing Time in seconds
ProgressTime:	rw 1
count:		rw 1	; byte count of one (wav file) read
LoadedDataBytes:
		rd 1	; total read/load count

timerticks:	rd 1	; (to eliminate excessive lookup of events in TuneLoop)
			; (in order to get the emulator/qemu to run correctly)
align 16

; 24/11/2024
dma_buffer:	; 32768 bytes	
wav_buffer1:	rb dma_buffer_size/2 ; 16384
wav_buffer2:	rb dma_buffer_size/2 ; 16384

bss_end:
