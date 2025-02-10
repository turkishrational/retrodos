; ****************************************************************************
; cgaplay.asm - Retro DOS (MSDOS/PCDOS) WAV PLAYER - Video Mode 13h
; ----------------------------------------------------------------------------
; CGAPLAY.COM ! AC'97 (ICH) .WAV PLAYER program by Erdogan TAN
;
; 01/01/2025				- play music from multiple wav files -
;
; [ Last Modification: 04/02/2025 ]
;
; Modified from CGAPLAY1.PRG .wav player program by Erdogan Tan, 31/12/2024
;	        AC97PLAY.COM, 18/12/2024
;
; ****************************************************************************
; nasm cgaplay.asm -l cgaplay.lst -o CGAPLAY.COM -Z error.txt

; 31/12/2024
; cgaplay1.s (int 31h modifications on cgaplay.s) -TRDOS386-PRG-
; 18/12/2024
; ac97play.asm : TUNELOOP version (playing without AC97 interrupt) -DOS-COM-

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

; ------------------------------------------------------------
; player internal variables and other equates.
;BUFFERSIZE	equ 64 * 1024	; 64k file buffer size.
; 17/11/2024
BUFFERSIZE	equ 65520
ENDOFFILE	equ 1		; flag for knowing end of file
; ------------------------------------------------------------

[BITS 16] ; 16-bit intructions

[ORG 100h]

	; 01/01/2025
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

	; 21/12/2024
	; Detect (& Enable) AC'97 Audio Device
	call	DetectAC97
	jnc	short ac97_hardware_ready

	; 30/11/2024
	; 30/05/2024
_dev_not_ready:
	; couldn't find the audio device!
	sys_msg noDevMsg, 0Fh
        jmp     Exit

ac97_hardware_ready:
	call	write_audio_dev_info

; -------------------------------------------------------------
	; 30/05/2024
allocate_memory:

; allocate 256 bytes of data for DCM_OUT Buffer Descriptor List. (BDL)

        mov     ax, BDL_SIZE / 16
        call    memAlloc
        mov     [BDL_BUFFER], ax	; segment 

; allocate 2 buffers, 64k each for now.

        mov     ax, BUFFERSIZE / 16	; 64k for .WAV file
        call    memAlloc
        mov     [WAV_BUFFER1], ax	; segment

	mov	ax, BUFFERSIZE / 16
	call	memAlloc
	mov	[WAV_BUFFER2], ax

; -------------------------------------------------------------

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
	; 29/12/2024
	; 30/05/2024
	call	codecConfig		; unmute codec, set rates.
	jc	init_err

; -------------------------------------------------------------

	; 01/01/2025
StartPlay:
	; 30/12/2024
	mov	byte [wpoints], 1

	; 01/01/2025 (RetroDOS, 16bit)
	; 09/12/2024 (TRDOS386, 32bit)
	mov	ax, 10548 ; (48000*10/182)*4
	cmp	byte [VRA], 0
	jna	short _w ; 48kHZ (interpolation)
	;
	mov	ax, [WAVE_SampleRate]
	mov	cx, 10
	mul	cx
	mov	cl, 182
	div	cx
	; ax = samples per 1/18.2 second
	;mov	cl, byte [WAVE_BlockAlign]
	; 09/12/2024 
	;mov	cl, 4 ; 16 bit, stereo
	;mul	cx
	shl	ax, 2 ; * 4
_w:
	mov	[wpoints_dif], ax ; buffer read differential (distance)
				; for wave volume leds update
				; (byte stream per 1/18.2 second)

; -------------------------------------------------------------

	; 25/12/2024
	inc	byte [filecount]
	mov	byte [command], 0
	; 30/12/2024
	mov	byte [pbprev], -1

; -------------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
	; 28/11/2024 (ac97play.asm)
	; 25/11/2023
	; 18/11/2023 (ich_wav4.asm)
	; 13/11/2023 (ich_wav3.asm)

	cmp	byte [VRA], 1
	jb	short chk_sample_rate

playwav_48_khz:
	mov	word [loadfromwavfile], loadFromFile
	;mov	word [loadsize], 0 ; 65536
	;;;
	; 17/11/2024
	;mov	word [buffersize], 32768
	mov	ax, BUFFERSIZE/2 ; 32760
	mov	[buffersize], ax	; 16 bit samples
	shl	ax, 1			; bytes
	mov	cl, [fbs_shift]
	shr	ax, cl
	mov	[loadsize], ax ; 16380 or 32760 or 65520
	;;;
	;jmp	PlayNow ; 30/05/2024
	; 01/01/2025
	jmp	Player_Template

chk_sample_rate:
	; set conversion parameters
	; (for 8, 11.025, 16, 22.050, 24, 32 kHZ)
	mov	ax, [WAVE_SampleRate]
	cmp	ax, 48000
	je	short playwav_48_khz
chk_22khz:
	cmp	ax, 22050
	jne	short chk_11khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_22khz_1
	mov	bx, load_22khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_22khz_2
	mov	bx, load_22khz_mono_16_bit
	jmp	short chk_22khz_2
chk_22khz_1:
	mov	bx, load_22khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_22khz_2
	mov	bx, load_22khz_mono_8_bit
chk_22khz_2:
	mov	ax, 7514  ; (442*17)
	mov	dx, 37
	mov	cx, 17
	jmp	set_sizes
chk_11khz:
	cmp	ax, 11025
	jne	short chk_44khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_11khz_1
	mov	bx, load_11khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_11khz_2
	mov	bx, load_11khz_mono_16_bit
	jmp	short chk_11khz_2
chk_11khz_1:
	mov	bx, load_11khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_11khz_2
	mov	bx, load_11khz_mono_8_bit
chk_11khz_2:
	mov	ax, 3757  ; (221*17)
	mov	dx, 74
	mov	cx, 17
	jmp	set_sizes
chk_44khz:
	cmp	ax, 44100
	jne	short chk_16khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_44khz_1
	mov	bx, load_44khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_44khz_2
	mov	bx, load_44khz_mono_16_bit
	jmp	short chk_44khz_2
chk_44khz_1:
	mov	bx, load_44khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_44khz_2
	mov	bx, load_44khz_mono_8_bit
chk_44khz_2:
	;mov	ax, 15065 ; (655*23)
	; 18/11/2023 ((file size + bss + stack) <= 64KB)
	;mov	ax, 14076 ; (612*23)
	; 17/11/2024
	mov	ax, 12650 ; (550*23)
	mov	dx, 25
	mov	cx, 23
	jmp	set_sizes
chk_16khz:
	cmp	ax, 16000
	jne	short chk_8khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_16khz_1
	mov	bx, load_16khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_16khz_2
	mov	bx, load_16khz_mono_16_bit
	jmp	short chk_16khz_2
chk_16khz_1:
	mov	bx, load_16khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_16khz_2
	mov	bx, load_16khz_mono_8_bit
chk_16khz_2:
	;mov	ax, 5461
	; 17/11/2024
	mov	ax, 5460
	mov	dx, 3
	mov	cx, 1
	jmp	set_sizes
chk_8khz:
	cmp	ax, 8000
	jne	short chk_24khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_8khz_1
	mov	bx, load_8khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_8khz_2
	mov	bx, load_8khz_mono_16_bit
	jmp	short chk_8khz_2
chk_8khz_1:
	mov	bx, load_8khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_8khz_2
	mov	bx, load_8khz_mono_8_bit
chk_8khz_2:
	mov	ax, 2730
	mov	dx, 6
	mov	cx, 1
	jmp	set_sizes  ; 02/02/2025
chk_24khz:
	cmp	ax, 24000
	jne	short chk_32khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_24khz_1
	mov	bx, load_24khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_24khz_2
	mov	bx, load_24khz_mono_16_bit
	jmp	short chk_24khz_2
chk_24khz_1:
	mov	bx, load_24khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_24khz_2
	mov	bx, load_24khz_mono_8_bit
chk_24khz_2:
	;mov	ax, 8192
	; 17/11/2024
	mov	ax, 8190
	mov	dx, 2
	mov	cx, 1
	jmp	short set_sizes

chk_32khz:
	cmp	ax, 32000
	;jne	short vra_needed
	; 02/02/2025
	jne	short chk_12khz
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_32khz_1
	mov	bx, load_32khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_32khz_2
	mov	bx, load_32khz_mono_16_bit
	jmp	short chk_32khz_2
chk_32khz_1:
	mov	bx, load_32khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_32khz_2
	mov	bx, load_32khz_mono_8_bit
chk_32khz_2:
	;mov	ax, 10922
	; 17/11/2024
	mov	ax, 10920
	mov	dx, 3
	mov	cx, 2
	; 02/02/2025
	jmp	short set_sizes

	; 01/01/2025 (cgaplay.asm)
vra_needed:
	; 13/11/2023
	pop	ax ; discard return address to the caller
	; 30/05/2024
vra_err:
	sys_msg msg_no_vra, 0Fh
	jmp	Exit

	;;;;
	; 02/02/2025
chk_12khz:
	cmp	ax, 12000
	jne	short vra_needed
	cmp	byte [WAVE_BitsPerSample], 8
	jna	short chk_12khz_1
	mov	bx, load_12khz_stereo_16_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_12khz_2
	mov	bx, load_12khz_mono_16_bit
	jmp	short chk_12khz_2
chk_12khz_1:
	mov	bx, load_12khz_stereo_8_bit
	cmp	byte [WAVE_NumChannels], 1
	jne	short chk_12khz_2
	mov	bx, load_12khz_mono_8_bit
chk_12khz_2:
	;mov	ax, 4096
	mov	ax, 4095 ; playwav8.asm
	mov	dx, 4
	mov	cx, 1
	; 02/02/2025
	;jmp	short set_sizes
	;;;;

set_sizes:
	;;;
	; 17/11/2024
	push	cx
	mov	cl, 2
	sub	cl, [fbs_shift]
		; = 2 for 16 bit stereo
		; = 1 for 16 bit mono or 8 bit stereo
		; = 0 for 8 bit mono
	shl	ax, cl
	pop	cx
	mov	[loadsize], ax	; (one) read count in bytes
	;;;
	mul	dx
	cmp	cx, 1
	je	short s_2
s_1:
	div	cx
s_2:	
	;;;
	; ax = byte count of (to be) converted samples 
	
	; 17/11/2024
	;;;
	mov	cl, [fbs_shift]

	shl	ax, cl
		; *1 for 16 bit stereo
		; *2 for 16 bit mono or 8 bit stereo
		; *4 for for 8 bit mono
	;;;

	; ax = 16 bit stereo byte count (target buffer size)
	
	shr	ax, 1	; buffer size is 16 bit sample count
	mov	[buffersize], ax 
	mov	[loadfromwavfile], bx
	
	; 01/01/2025 (cgaplay.asm)
; -------------------------------------------------------------
	; 30/12/2024
Player_Template:
	; 21/12/2024
	call	clearscreen
	call	drawplayingscreen
	; 14/11/2024
	call	SetTotalTime
	call	UpdateFileInfo

; -------------------------------------------------------------

	; 30/12/2024 (cgaplay.s)
	; 29/12/2024 (vgaplay3.s)
	; 18/12/2024 (ac97play.s)
PlayNow:
	; 01/12/2024 (32bit)
	; 14/11/2024
	;mov	al, 3	; 0 = max, 31 = min
	; 14/12/2024
	mov	al, [volume]
	call	SetPCMOutVolume@
	; 15/11/2024
	;;call	SetMasterVolume
	;call	SetPCMOutVolume

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
	je	short terminate
	jmp	check_p_command

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
	jmp	 terminate

; -------------------------------------------------------------

	; 01/01/2025 (cgaplay.asm)
	; 18/12/2024 (ac97play.asm)
	; 30/12/2024 (cgaplay.s)
	; 29/12/2024 (vgaplay3.s)
	; 02/12/2024 (ac97play.s)
PlayWav:
	; 29/05/2024 (TRDOS 386, playwav7.s)
	; ((Modified from playwav4.asm, ich_wav4.asm))
	; ------------------

	; create Buffer Descriptor List

	;  Generic Form of Buffer Descriptor
	;  ---------------------------------
	;  63   62    61-48    47-32   31-0
	;  ---  ---  --------  ------- -----
	;  IOC  BUP -reserved- Buffer  Buffer
	;		      Length   Pointer
	;		      [15:0]   [31:0]

        push    es
        mov     ax, [BDL_BUFFER]		; get segment # for BDL
        mov     es, ax

        mov     cx, 32 / 2                      ; make 32 entries in BDL
        xor     di, di
_pw0:
        movzx   eax, word [WAV_BUFFER1]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd		       ; store pointer to wavbuffer1

	;mov	eax, BUFFERSIZE / 2 ; size of buffer (32K) in (16bit) words
	; 13/11/2023 (ich_wav3.asm) - 18/11/2023 (ich_wav4.asm)
	mov	eax, [buffersize]

	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

        movzx   eax, word [WAV_BUFFER2]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd		       ; store pointer to wavbuffer2

	;mov	eax, BUFFERSIZE / 2 ; size of half buffer (32K)
	; 13/11/2023 (ich_wav3.asm) - 18/11/2023 (ich_wav4.asm)
	mov	eax, [buffersize]

	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

        loop    _pw0
        pop     es

	; 18/12/2024
	;mov	word [count], cx ; 0
	; 14/11/2024
	;mov	dword [LoadedDataBytes], 0

	; 19/11/2024
RePlayWav:
	; load 64k into buffer 1
        mov     ax, [WAV_BUFFER1]
        ;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]
	; 14/11/2024
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	; and 64k into buffer 2
	mov     ax, [WAV_BUFFER2]
       	;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]
	; 14/11/2024
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	; write NABMBAR+10h with offset of buffer descriptor list

        movzx   eax, word [BDL_BUFFER]
	shl     eax, 4                          ; convert seg:off to 0:off
        mov     dx, [NABMBAR]
        add     dx, PO_BDBAR_REG                ; set pointer to BDL
        out     dx, eax                         ; write to AC97 controller

	; 19/05/2024
	call	delay1_4ms

        mov	al, 31
	call	setLastValidIndex

	; 19/05/2024
	;call	delay1_4ms

	; 17/02/2017
        mov	dx, [NABMBAR]
        add	dx, PO_CR_REG		; PCM out Control Register
        ;mov	al, IOCE + RPBM	; Enable 'Interrupt On Completion' + run
	;			; (LVBI interrupt will not be enabled)
	; 06/11/2023 (TUNELOOP version, without interrupt)
	mov	al, RPBM
	out	dx, al			; Start bus master operation.

	; 19/05/2024
	; 06/11/2023
	call	delay1_4ms	; 30/05/2024
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms

	; 30/12/2024

; -------------------------------------------

	; 01/01/2025 (cgaplay.asm)
	; 18/12/2024 (ac97play.asm)
	; 30/12/2024 (cgaplay.s)
	; 29/12/2024 (vgaplay3.s)
	; 18/12/2024 (ac97play.s)
	; 01/12/2024 (32bit)
	; 29/11/2024
tuneLoop:
	; 30/05/2024
	; 18/11/2023 (ich_wav4.asm)
	; 08/11/2023
	; 06/11/2023
tLWait:
	; 18/11/2024
	cmp	byte [stopped], 0
	;jna	short tL@
	; 21/11/2024
	ja	short tLWait@
	mov	al, [tLP]
	cmp	al, '1'
	je	short tL1@
	ja	short tL2@
	mov	al, '1'
	mov	[tLP], al
	jmp	short tL1@ 
tLWait@:	; 21/11/2024
	;;;
	; 09/12/2024
	cmp	byte [stopped], 3
	jnb	_exitt_
	;;;
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

;tLO:	db 0
	
tL1@:
	;mov	al, '1'
	; 19/11/2024
	mov	[tLO], al
	call	tL0
tL1:
	call	updateLVI	; /set LVI != CIV/
	jz	_exitt_		; 08/11/2023
	;;;
	;call	check4keyboardstop
	; 14/11/2024
	call	checkUpdateEvents
	jc	_exitt_
	; 18/11/2024
	cmp	byte [stopped], 0
	ja	short tLWait@	; 21/11/2024
	;;;
	call	getCurrentIndex
	test	al, BIT0
	jz	short tL1	; loop if buffer 2 is not playing

	; load buffer 1
	mov     ax, [WAV_BUFFER1]
	;call	loadFromFile
	; 18/11/2023
	call	word [loadfromwavfile]
	jc	short _exitt_	; end of file

	; 14/11/2024
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	mov	al, '2'
	; 21/11/2024
	mov	[tLP], al
tL2@:
	; 19/11/2024
	mov	[tLO], al
	call	tL0
tL2:
	call    updateLVI
	jz	short _exitt_	; 08/11/2023
	;;;
	;call	check4keyboardstop
	; 14/11/2024
	call	checkUpdateEvents
	jc	short _exitt_
	; 18/11/2024
	cmp	byte [stopped], 0
	ja	tLWait@		; 21/11/2024 
	;;;
	call    getCurrentIndex
	test	al, BIT0
	jnz	short tL2	; loop if buffer 1 is not playing

	; load buffer 2
	mov     ax, [WAV_BUFFER2]
	;call	loadFromFile
	; 18/11/2023
	call	word [loadfromwavfile]
	;jnc	short tuneLoop
	jc	short _exitt_

	; 14/11/2024
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	; 21/11/2024
	mov	byte [tLP], '1'
	jmp	tuneLoop

	; 29/12/2024 (vgaplay3.s)
_exitt_:
	; 07/12/2024
	; Stop Playing
	call	ac97_stop

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

	; 14/11/2024
;SetMasterVolume:
	; 15/11/2024
SetPCMOutVolume:
	;cmp	al, 31
	;ja	short setvolume_ok
	mov	[volume], al  ; max = 0, min = 31
SetPCMOutVolume@:	; 19/11/2024
	mov	ah, al
	mov	dx, [NAMBAR]
	; 15/11/2024 (QEMU)
  	;add	dx, CODEC_MASTER_VOL_REG
	add	dx, CODEC_PCM_OUT_REG
	out	dx, ax
;setvolume_ok:
	retn

; -------------------------------------------

	; 30/05/2024
DetectAC97:
DetectICH:
	; 22/11/2023
	; 19/11/2023
	; 01/11/2023 - TRDOS 386 Kernel v2.0.7
	;; 10/06/2017
	;; 05/06/2017
	;; 29/05/2017
	;; 28/05/2017

	; 19/11/2023
	mov	si, valid_ids	; address of Valid ICH (AC97) Device IDs
	mov	cx, valid_id_count
pfd_1:
	lodsd
	call	pciFindDevice
	jnc	short d_ac97_1
	loop	pfd_1

	;stc
	retn

d_ac97_1:
	; eax = BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; edx = DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	; playwav4.asm - 19/05/2024

	mov	[bus_dev_fn], eax
	mov	[dev_vendor], edx

	; get ICH base address regs for mixer and bus master

        mov     al, NAMBAR_REG
        call    pciRegRead16			; read PCI registers 10-11
        ;and    dx, IO_ADDR_MASK 		; mask off BIT0
	; 19/05/2024
	and	dl, 0FEh

        mov     [NAMBAR], dx			; save audio mixer base addr

	mov     al, NABMBAR_REG
        call    pciRegRead16
        ;and    dx, IO_ADDR_MASK
	; 19/05/2024
	and	dl, 0C0h

        mov     [NABMBAR], dx			; save bus master base addr

	mov	al, AC97_INT_LINE ; Interrupt line register (3Ch)
	call	pciRegRead8 ; 17/02/2017

	mov	[ac97_int_ln_reg], dl

	;clc

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

	; 04/02/2025
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
	; 04/02/2025
	jne	short gwavp_stc_retn
	;je	short gwavp_retn ; 15/11/2024

	; 04/02/2025
	; (Open MPT creates wav files with a new type header,
	;  this program can not use the new type
	;  because of 'data' offset is not at DATA_SubchunkID.)
	; ((GoldWave creates common type wav file.))
	cmp	dword [DATA_SubchunkID], 'data'
	je	short gwavp_retn

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

; 29/12/2024 (vgaplay3.s)
; 18/12/2024 (ac97play.s)
; --------------------------------------------------------
; 27/05/2024 - (TRDOS 386 Kernel) audio.s
; --------------------------------------------------------

NOT_PCI32_PCI16	EQU 03FFFFFFFh ; NOT BIT31+BIT30 ; 19/03/2017
NOT_BIT31 EQU 7FFFFFFFh

pciFindDevice:
	; 19/11/2023
	; 03/04/2017 ('pci.asm', 20/03/2017)
	;
	; scan through PCI space looking for a device+vendor ID
	;
	; Entry: EAX=Device+Vendor ID
	;
	; Exit: EAX=PCI address if device found
	;	EDX=Device+Vendor ID
	;       CY clear if found, set if not found. EAX invalid if CY set.
	;
	; Destroys: ebx, edi ; 19/11/2023

        ; 19/11/2023
	mov	ebx, eax
	mov	edi, 80000000h
nextPCIdevice:
	mov 	eax, edi		; read PCI registers
	call	pciRegRead32
	; 19/11/2023
	cmp	edx, ebx
	je	short PCIScanExit	; found
	; 19/11/2023
	cmp	edi, 80FFF800h
	jnb	short pfd_nf		; not found
	add	edi, 100h
	jmp	short nextPCIdevice
pfd_nf:
	stc
	retn
PCIScanExit:
	;pushf
	mov	eax, NOT_BIT31 	; 19/03/2017
	and	eax, edi	; return only bus/dev/fn #
	retn

pciRegRead:
	; 30/05/2024
	; 03/04/2017 ('pci.asm', 20/03/2017)
	;
	; 8/16/32bit PCI reader
	;
	; Entry: EAX=PCI Bus/Device/fn/register number
	;           BIT30 set if 32 bit access requested
	;           BIT29 set if 16 bit access requested
	;           otherwise defaults to 8 bit read
	;
	; Exit:  DL,DX,EDX register data depending on requested read size
	;
	; Note1: this routine is meant to be called via pciRegRead8,
	;	 pciRegread16 or pciRegRead32, listed below.
	;
	; Note2: don't attempt to read 32 bits of data from a non dword
	;	 aligned reg number. Likewise, don't do 16 bit reads from
	;	 non word aligned reg #
	
	push	ebx
	push	cx
        mov     ebx, eax		; save eax, dh
        mov     cl, dh

        and     eax, NOT_PCI32_PCI16	; clear out data size request
        or      eax, BIT31		; make a PCI access request
	; 01/01/2025 (NASM)
        and     al, ~3 ; NOT 3		; force index to be dword

        mov     dx, PCI_INDEX_PORT
	out	dx, eax			; write PCI selector
		
        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3			; figure out which port to
        add     dl, al			; read to

	test    ebx, PCI32+PCI16
        jnz     short _pregr0

	in	al, dx			; return 8 bits of data

	mov	dl, al
	mov     dh, cl			; restore dh for 8 bit read
	jmp	short _pregr2
_pregr0:	
	test    ebx, PCI32
        jnz	short _pregr1

	in	ax, dx

	mov     dx, ax			; return 16 bits of data
	jmp	short _pregr2
_pregr1:
	in	eax, dx			; return 32 bits of data

	mov	edx, eax
_pregr2:
	mov     eax, ebx		; restore eax
        and     eax, NOT_PCI32_PCI16	; clear out data size request
	pop	cx
	pop	ebx
	retn

pciRegRead8:
        and     eax, NOT_PCI32_PCI16	; set up 8 bit read size
        jmp     short pciRegRead	; call generic PCI access

pciRegRead16:
        and     eax, NOT_PCI32_PCI16	; set up 16 bit read size
        or      eax, PCI16		; call generic PCI access
        jmp     short pciRegRead

pciRegRead32:
        and     eax, NOT_PCI32_PCI16	; set up 32 bit read size
        or      eax, PCI32		; call generic PCI access
        jmp     pciRegRead

pciRegWrite:
	; 30/05/2024
	; 03/04/2017 ('pci.asm', 29/11/2016)
	;
	; 8/16/32bit PCI writer
	;
	; Entry: EAX=PCI Bus/Device/fn/register number
	;           BIT31 set if 32 bit access requested
	;           BIT30 set if 16 bit access requested
	;           otherwise defaults to 8bit read
	;        DL/DX/EDX data to write depending on size
	;
	; Note1: this routine is meant to be called via pciRegWrite8,
	;	 pciRegWrite16 or pciRegWrite32 as detailed below.
	;
	; Note2: don't attempt to write 32bits of data from a non dword
	;	 aligned reg number. Likewise, don't do 16 bit writes from
	;	 non word aligned reg #

	push	ebx
	push	ecx
        mov     ebx, eax		; save eax, edx
        mov     ecx, edx
	and     eax, NOT_PCI32_PCI16	; clear out data size request
        or      eax, BIT31		; make a PCI access request
	; 01/01/2025 (NASM)
        and     al, ~3 ; NOT 3		; force index to be dword

        mov     dx, PCI_INDEX_PORT
	out	dx, eax			; write PCI selector
		
        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3			; figure out which port to
        add     dl, al			; write to

	test    ebx, PCI32+PCI16
        jnz     short _pregw0
	
	mov	al, cl 			; put data into al
	out	dx, al
	
	jmp	short _pregw2
_pregw0:
	test    ebx, PCI32
        jnz     short _pregw1
	
	mov	ax, cx			; put data into ax
	out	dx, ax

	jmp	short _pregw2
_pregw1:
	mov	eax, ecx		; put data into eax
	out	dx, eax
_pregw2:
        mov     eax, ebx		; restore eax
        and     eax, NOT_PCI32_PCI16	; clear out data size request
        mov     edx, ecx		; restore dx
	pop	ecx
	pop	ebx
	retn

pciRegWrite8:
        and     eax, NOT_PCI32_PCI16	; set up 8 bit write size
        jmp	short pciRegWrite	; call generic PCI access

pciRegWrite16:
        and     eax, NOT_PCI32_PCI16	; set up 16 bit write size
        or      eax, PCI16		; call generic PCI access
        jmp	short pciRegWrite

pciRegWrite32:
        and     eax, NOT_PCI32_PCI16	; set up 32 bit write size
        or      eax, PCI32		; call generic PCI access
        jmp	pciRegWrite

; --------------------------------------------------------
; 19/05/2024 - (playwav4.asm) ac97_vra.asm
; --------------------------------------------------------

	; 13/11/2023

;VRA:	db 1

codecConfig:
	; 30/05/2024
	; 19/05/2024
	; 19/11/2023
	; 15/11/2023
	; 04/11/2023
	; 17/02/2017 
	; 07/11/2016 (Erdogan Tan)

	;AC97_EA_VRA equ 1
	AC97_EA_VRA equ BIT0

	; 04/11/2023
init_ac97_controller:
	mov	eax, [bus_dev_fn]
	mov	al, PCI_CMD_REG
	call	pciRegRead16		; read PCI command register
	or      dl, IO_ENA+BM_ENA	; enable IO and bus master
	call	pciRegWrite16

	;call	delay_100ms

	; 19/05/2024
	; ('PLAYMOD3.ASM', Erdogan Tan, 18/05/2024)

init_ac97_codec:
	; 18/11/2023
	mov	bp, 40
	; 29/05/2024
	;mov	bp, 1000
_initc_1:
	; 30/05/2024
	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	; 19/05/2024
	call	delay1_4ms

	cmp	eax, 0FFFFFFFFh ; -1
	jne	short _initc_3
_initc_2:
	dec	bp
	jz	short _ac97_codec_ready

	call	delay_100ms
	jmp	short _initc_1
_initc_3:
	test	eax, CTRL_ST_CREADY
	jnz	short _ac97_codec_ready

	; 30/05/2024
	cmp	byte [reset], 1
	jnb	short _initc_2

	call	reset_ac97_codec
	; 30/05/2024
	mov	byte [reset], 1
	; 19/05/2024
	jmp	short _initc_2

_ac97_codec_ready:
	mov	dx, [NAMBAR]
	;add	dx, 0 ; ac_reg_0 ; reset register
	out	dx, ax
	
	call	delay_100ms

	; 19/11/2023
	or	bp, bp
	jnz	short _ac97_codec_init_ok

	xor	ax, ax ; 0
	mov	dx, [NAMBAR]
	add	dx, CODEC_REG_POWERDOWN
	out	dx, ax

	; 30/05/2024
	call	delay1_4ms

	; 19/11/2023
	; wait for 1 second
	; 19/05/2024
	mov	cx, 1000 ; 1000*4*0.25ms = 1s
	;;mov	cx, 10
	; 30/05/2024
	;mov	cx, 40
_ac97_codec_rloop:
	;call	delay_100ms

	; 30/05/2024
	mov	dx, [NAMBAR]
	add	dx, CODEC_REG_POWERDOWN
	in	ax, dx

	call	delay1_4ms
	
	and	ax, 0Fh
	cmp	al, 0Fh
	je	short _ac97_codec_init_ok
	loop	_ac97_codec_rloop 

init_ac97_codec_err1:
	;stc	; cf = 1 ; 19/05/2024
init_ac97_codec_err2:
	retn

_ac97_codec_init_ok:
	call 	reset_ac97_controller

	; 30/05/2024
	; 19/05/2024
	;call	delay_100ms

	; 30/05/2024
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

setup_ac97_codec:
	; 12/11/2023
	cmp	word [WAVE_SampleRate], 48000
	je	short skip_rate
	
	; 30/05/2024
	; 19/05/2024
	;call	delay1_4ms

	; 30/05/2024
	;cmp	byte [VRA], 0
	;jna	short skip_rate

	; 11/11/2023
	mov	dx, [NAMBAR]
	add	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah
	in	ax, dx

	; 30/05/2024
	; 19/05/2024
	call	delay1_4ms
	
	; 13/11/2024
	;and	al, NOT BIT1 ; Clear DRA
	;;;
	; 30/05/2024
	;and	al, NOT (BIT1+BIT0) ; Clear DRA+VRA
	; 01/01/2025 (NASM)
	and	al, ~(BIT1+BIT0)
	out	dx, ax

	call	check_vra

	cmp	byte [VRA], 0
	jna	short skip_rate

	mov	dx, [NAMBAR]
	add	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah
	in	ax, dx
	;and	al, ~BIT1 ; Clear DRA
	;;;

	or	al, AC97_EA_VRA ; 1 ; 04/11/2023
	; 30/05/2024
	mov	dx, [NAMBAR]
	add	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah

	out	dx, ax			; Enable variable rate audio

	mov	cx, 10
check_vra_loop:
	call	delay_100ms
	; 30/05/2024
	;call	delay1_4ms

	; 30/05/2024
	mov	dx, [NAMBAR]
	add	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah
	; 11/11/2023
	in	ax, dx
	
	test	al, AC97_EA_VRA ; 1
	jnz	short set_rate

	; 11/11/2023
	loop	check_vra_loop

;vra_not_supported:	; 19/05/2024
	mov	byte [VRA], 0
	jmp	short skip_rate

set_rate:
	mov	ax, [WAVE_SampleRate] ; 17/02/2017 (Erdogan Tan)

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch
	out	dx, ax 			; PCM Front/Center Output Sample Rate

	;call	delay_100ms
	; 30/05/2024
	call	delay1_4ms

	; 12/11/2023
skip_rate:
	mov	ax, 0202h
  	mov	dx, [NAMBAR]
  	add	dx, CODEC_MASTER_VOL_REG	; 02h
	out	dx, ax

	; 11/11/2023
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

	mov	ax, 0202h
  	mov	dx, [NAMBAR]
  	add	dx, CODEC_PCM_OUT_REG		; 18h
  	out	dx, ax
	
	; 11/11/2023
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

	; 30/05/2024
	; 19/05/2024
	;clc

        retn

reset_ac97_controller:
	; 19/05/2024
	; 11/11/2023
	; 10/06/2017
	; 29/05/2017
	; 28/05/2017
	; reset AC97 audio controller registers
	xor     ax, ax
        mov	dx, PI_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 19/05/2024
	call	delay1_4ms

        mov     dx, PO_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 19/05/2024
	call	delay1_4ms

        mov     dx, MC_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 19/05/2024
	call	delay1_4ms

        mov     al, RR
        mov     dx, PI_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 19/05/2024
	call	delay1_4ms

        mov     dx, PO_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 19/05/2024
	call	delay1_4ms

        mov     dx, MC_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 19/05/2024
	call	delay1_4ms

	retn

reset_ac97_codec:
	; 11/11/2023
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, intelac97.asm)
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	in	eax, dx

	;test	eax, 2
	; 06/08/2022
	test	al, 2
	jz	short _r_ac97codec_cold

	call	warm_ac97codec_reset
	jnc	short _r_ac97codec_ok
_r_ac97codec_cold:
        call    cold_ac97codec_reset
        jnc     short _r_ac97codec_ok
	
	; 16/04/2017
        ;xor	eax, eax	; timeout error
       	;stc
	retn

_r_ac97codec_ok:
        xor     eax, eax
        ;mov	al, VIA_ACLINK_C00_READY ; 1
        inc	al
	retn

warm_ac97codec_reset:
	; 11/11/2023
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, intelac97.asm)
	mov	eax, 6
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	out	dx, eax

	mov	cx, 10	; total 1s
_warm_ac97c_rst_wait:
	call	delay_100ms

	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	test	eax, CTRL_ST_CREADY
	jnz	short _warm_ac97c_rst_ok

        dec     cx
        jnz     short _warm_ac97c_rst_wait

_warm_ac97c_rst_fail:
        stc
_warm_ac97c_rst_ok:
	retn

cold_ac97codec_reset:
	; 11/11/2023
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, intelac97.asm)
        mov	eax, 2
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	out	dx, eax

	call	delay_100ms 	; wait 100 ms
	call	delay_100ms 	; wait 100 ms
	call	delay_100ms 	; wait 100 ms
	call	delay_100ms 	; wait 100 ms

	mov	cx, 16	; total 20*100 ms = 2s

_cold_ac97c_rst_wait:
	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	test	eax, CTRL_ST_CREADY
	jnz	short _cold_ac97c_rst_ok

	call	delay_100ms

        dec     cx
        jnz     short _cold_ac97c_rst_wait

_cold_ac97c_rst_fail:
        stc
_cold_ac97c_rst_ok:
	retn

; 13/11/2024
; 30/05/2024
%if 1
check_vra:
	; 30/05/2024
	mov	byte [VRA], 1

	; 29/05/2024 - audio.s (TRDOS 386 Kernel) - 27/05/2024
	; 24/05/2024
	; 23/05/2024
	mov	dx, [NAMBAR]
	add	dx, CODEC_EXT_AUDIO_REG	; 28h
	in	ax, dx

	; 30/05/2024
	; 23/05/2024
	call	delay1_4ms

	; 30/05/2024
	test	al, BIT0
	;test	al, 1 ; BIT0 ; Variable Rate Audio bit
	jnz	short check_vra_ok

vra_not_supported:
	; 13/11/2023
	mov	byte [VRA], 0
check_vra_ok:
	retn
%endif

; --------------------------------------------------------
; --------------------------------------------------------

; 01/01/2025 (cgaplay.asm)
; 29/12/2024 (vgaplay3.s)
; 18/12/2024 (ac97play.s)
;
; 18/11/2024
; Ref: TRDOS 386 v2.0.9, audio.s, Erdogan Tan, 06/06/2024

ac97_stop:
	; 18/11/2024
	mov	byte [stopped], 2

ac97_po_cmd@:
	xor	al, al ; 0
ac97_po_cmd:
	mov     dx, [NABMBAR]
        add     dx, PO_CR_REG	; PCM out control register
	out	dx, al
	retn

ac97_pause:
	mov	byte [stopped], 1 ; paused
	;mov	al, 0
	;jmp	short ac97_po_cmd
	jmp	short ac97_po_cmd@

ac97_play: ; continue to play (after pause)
	mov	byte [stopped], 0
	mov	al, RPBM
	jmp	short ac97_po_cmd

; --------------------------------------------------------

PORTB		EQU 061h
REFRESH_STATUS	EQU 010h	; Refresh signal status

delay_100ms:
	; 11/11/2023
	; 29/05/2017
	; 24/03/2017 ('codec.asm')
	; wait 100 ms
	push	cx
	mov	cx, 400  ; 400*0.25ms
_delay_x_ms:
	call	delay1_4ms
        loop	_delay_x_ms
	pop	cx
	retn

delay1_4ms:
        push    ax
        push    cx
        mov     cx, 16			; close enough.
	in	al, PORTB
	and	al, REFRESH_STATUS
	mov	ah, al			; Start toggle state
	or	cx, cx
	jz	short _d4ms1
	inc	cx			; Throwaway first toggle
_d4ms1:
	in	al, PORTB		; Read system control port
	and	al, REFRESH_STATUS	; Refresh toggles 15.085 microseconds
	cmp	ah, al
	je	short _d4ms1		; Wait for state change

	mov	ah, al			; Update with new state
	dec	cx
	jnz	short _d4ms1

	; 30/05/2024
	clc

        pop     cx
        pop     ax
        retn

; --------------------------------------------------------

; returns AL = current index value
getCurrentIndex:
	; 08/11/2023
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_CIV_REG
	in	al, dx
	;pop	dx
uLVI2:	;	06/11/2023
	retn

; --------------------------------------------------------

updateLVI:
	; 06/11/2023
	mov	dx, [NABMBAR]
	add	dx, PO_CIV_REG
	; (Current Index Value and Last Valid Index value)
	in	ax, dx

	cmp	al, ah ; is current index = last index ?
	jne	short uLVI2

	; 08/11/2023
	call	getCurrentIndex
 
	test	byte [flags], ENDOFFILE
	;jnz	short uLVI1
	jz	short uLVI0  ; 08/11/2023

	; 08/11/2023
	push	ax
	mov	dx, [NABMBAR]
	add	dx, PO_SR_REG  ; PCM out status register
	in	ax, dx

	test	al, 3 ; bit 1 = Current Equals Last Valid (CELV)
		      ; (has been processed)
		      ; bit 0 = 1 -> DMA Controller Halted (DCH)
	pop	ax
	jz	short uLVI1
uLVI3:
	xor	ax, ax
	; zf = 1
	retn
uLVI0:
        ; not at the end of the file yet.
	dec	al
	and	al, 1Fh
uLVI1:
	;call	setLastValidIndex
;uLVI2:
	;retn

;input AL = index # to stop on
setLastValidIndex:
	; 08/11/2023
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	;pop	dx
	retn

; --------------------------------------------------------
; 07/12/2024
; --------------------------------------------------------

; /////
	; 30/05/2024 (ich_wav4.asm, 19/05/2024)
loadFromFile:
	; 07/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff_0		; no
	stc
	retn

lff_0:
	; 08/11/2023
	mov	bp, ax ; save buffer segment

	; 17/11/2024
	mov	bx, [filehandle]

	; 17/11/2024
	mov	cx, [loadsize]
	xor	di, di ; 0

	;mov	cl, [fbs_shift]
	;and	cl, cl
	;jz	short lff_1 ; stereo, 16 bit
	; 17/11/2024
	cmp	byte [fbs_shift], 0
	jna	short lff_1 ; stereo, 16 bit

	;mov	di, BUFFERSIZE - 1 ; 65535

	;; fbs_shift =
	;;	2 for mono and 8 bit sample (multiplier = 4)
	;;	1 for mono or 8 bit sample (multiplier = 2)
	;shr	di, cl
	;inc	di ; 16384 for 8 bit and mono
	;	   ; 32768 for 8 bit or mono
	
	; 17/11/2024
	;mov	cx, [loadsize] ; 16380 or 32760

	;mov	ax, cs
	mov	dx, temp_buffer ; temporary buffer for wav data

	; 17/02/2017 (stereo/mono, 8bit/16bit corrections)
	; load file into memory
	;mov	cx, di ; 17/11/2024
	;mov	bx, [filehandle] ; 17/11/2024
	;mov    ds, ax
       	mov	ah, 3Fh
	int	21h

	;mov	bx, cs
	;mov	ds, bx
	; 17/11/2024
	;push	cs
	;pop	ds

	jc	lff_4 ; error !

	; 14/11/2024
	mov	[count], ax

	; 17/11/2024
	; 08/11/2023
	;xor	dx, dx ; 0

	and	ax, ax
	jz	short lff_3

	mov	bl, [fbs_shift]

	push	es
	;mov	di, dx ; 0 ; [fbs_off]
	; 17/11/2024
	; di = 0
	;mov	bp, [fbs_seg] ; buffer segment
	mov	es, bp
	mov	si, temp_buffer ; temporary buffer address
	mov	cx, ax ; byte count
	cmp	byte [WAVE_BitsPerSample], 8 ; bits per sample (8 or 16)
	jne	short lff_7 ; 16 bit samples
	; 8 bit samples
	dec	bl  ; shift count, 1 = stereo, 2 = mono
	jz	short lff_6 ; 8 bit, stereo
lff_5:
	; mono & 8 bit
	lodsb
	sub	al, 80h ; 08/11/2023
	shl	ax, 8 ; convert 8 bit sample to 16 bit sample
	stosw	; left channel
	stosw	; right channel
	loop	lff_5
	jmp	short lff_9
lff_6:
	; stereo & 8 bit
	lodsb
	sub	al, 80h ; 08/11/2023
	shl	ax, 8 ; convert 8 bit sample to 16 bit sample
	stosw
	loop	lff_6
	jmp	short lff_9
lff_7:
	shr	cx, 1 ; word count
lff_8:
	lodsw
	stosw	; left channel
	stosw	; right channel
	loop	lff_8
lff_9:
	pop	es
	
	;or	di, di
	;jz	short endLFF ; 64KB ok 
	;mov	ax, di ; [fbs_off]
	;dec	ax
	; 17/11/2024
	mov	ax, di 
	;cmp	ax, BUFFERSIZE ; 65520
	;jnb	short endLFF

	;mov	cx, BUFFERSIZE - 1 ; 65535
	; 17/11/2024
	mov	cx, BUFFERSIZE
	; 17/11/2024
	; ax = di
	cmp	ax, cx
	;jnb	short endLFF
	;jmp	short lff_3
	jb	short lff_3
	retn
	
lff_1:  
	;mov	bp, ax ; save buffer segment
	xor	dx, dx
	; load file into memory
        ;mov	cx, (BUFFERSIZE / 2)	; 32k chunk
	
	; 17/11/2024
	;mov	cx, [buffersize] ; BUFFERSIZE / 2
	; 17/11/2024 (*)
	; cx = [loadsize] = 2*[buffersize]

	;mov	bx, [filehandle] ; 17/11/2024
	mov     ds, ax ; mov ds, bp
       	mov	ah, 3Fh
	int	21h

	;mov	di, cs
	;mov	ds, di
	; 17/11/2024
	push	cs
	pop	ds

	; 07/11/2023
	jc	short lff_4 ; error !

	; 14/11/2024
	mov	[count], ax
	; 17/11/2024
	; di = 0

; 17/11/2024 (*)
%if 0	
	cmp	ax, cx
	jne	short lff_3
lff_2:
	; 08/11/2023
	add	dx, ax
	;;mov	cx, (BUFFERSIZE / 2)	; 32k chunk
	;mov	cx, [buffersize] ; BUFFERSIZE / 2
	;mov	bx, [filehandle]
	mov     ds, bp
       	mov	ah, 3Fh
	int	21h

	;;mov	di, cs
	;mov	ds, di
	; 17/11/2024
	push	cs
	pop	ds

	jc	short lff_4 ; error !

	; 17/11/2024
	; 14/11/2024
	add	[count], ax
%endif

	cmp	ax, cx
	je	short endLFF
	; 17/11/2024
	; di = 0
	mov	di, ax
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

padfill:
	; 14/12/2024
	; 17/11/2024
	;   di = offset (to be filled with ZEROs)
	;   bp = buffer segment
	;   ax = di = number of bytes loaded
	;   cx = buffer size (> loaded bytes)
	; 07/11/2023
	; 06/11/2023
	; 17/02/2017
	push	es
        ;push	di
	;mov	di, [fbs_seg]
	;mov	es, di
        mov	es, bp
	sub	cx, ax
	; 08/11/2023
	;mov	di, ax ; (wrong)
	; 17/11/2024
	;mov	di, dx ; buffer offset
	;add	di, ax
	; 07/11/2023
	;add	di, [fbs_off]
 	; 25/11/2024
	xor	ax, ax
	; 14/12/2024
	rep	stosb
	;mov	[fbs_off], di
	;pop	di
        pop	es
	retn

; /////

; --------------------------------------------------------
; --------------------------------------------------------
	
write_audio_dev_info:
	; 30/05/2024
     	sys_msg msgAudioCardInfo, 0Fh
	retn

; --------------------------------------------------------

write_ac97_pci_dev_info:
	; 01/01/2025 (cgaplay.asm)
	; 19/11/2024
	; 30/05/2024
	; 06/06/2017
	; 03/06/2017
	; BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	mov	eax, [dev_vendor]
	xor	bh, bh
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
	shr	eax, 16
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

	mov	eax, [bus_dev_fn]
	shr	eax, 8
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

	;mov	ax, [ac97_NamBar]
	mov	ax, [NAMBAR]
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgNamBar+3], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgNamBar+2], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgNamBar+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgNamBar], al

	;mov	ax, [ac97_NabmBar]
	mov	ax, [NABMBAR]
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgNabmBar+3], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgNabmBar+2], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgNabmBar+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgNabmBar], al

	xor	eax, eax
	mov	al, [ac97_int_ln_reg]
	mov	cl, 10
	div	cl
	; 23/11/2024
	;add	[msgIRQ], ax
	add	ax, 3030h
	mov	[msgIRQ], ax
	;and	al, al
	cmp	al, 30h
	jnz	short _w_ac97imsg_
	mov	al, byte [msgIRQ+1]
	mov	ah, ' '
	mov	[msgIRQ], ax
_w_ac97imsg_:
	; 19/11/2024
	call 	clear_window
	;mov	dh, 13
	; 01/01/2025 (video mode 13h)
	mov	dh, 11
	mov	dl, 0
	call	setCursorPosition
	;;;
	; 30/05/2024
	sys_msg msgAC97Info, 07h

	; 19/11/2024
        ;retn

	; 30/05/2024
write_VRA_info:
	sys_msg	msgVRAheader, 07h
	cmp	byte [VRA], 0
	jna	short _w_VRAi_no
_w_VRAi_yes:
	sys_msg msgVRAyes, 07h
	retn
_w_VRAi_no:
	sys_msg msgVRAno, 07h
	retn

; --------------------------------------------------------
; 02/02/2025 - playwav8.asm - ac97play.asm - cgaplay.asm 
; 01/01/2025 - cgaplay.asm
; 18/12/2024 - ac97play.asm
; ----------
; 30/05/2024 - playwav6.asm
; 18/11/2023 - ich_wav3.asm & ich_wav4.asm
; 15/11/2023 - PLAYWAV5.COM, ich_wav5.asm
; 14/11/2023
; 13/11/2023 - Erdogan Tan - (VRA, sample rate conversion)
; --------------------------------------------------------

;;Note:	At the end of every buffer load,
;;	during buffer switch/swap, there will be discontinuity
;;	between the last converted sample and the 1st sample
;;	of the next buffer.
;;	(like as a dot noises vaguely between normal sound samples)
;;	-To avoid this defect, the 1st sample of
;;	the next buffer may be read from the wav file but
;;	the file pointer would need to be set to 1 sample back
;;	again via seek system call. Time comsumption problem! -
;;
;;	Erdogan Tan - 15/11/2023
;;
;;	((If entire wav data would be loaded at once.. conversion
;;	defect/noise would disappear.. but for DOS, to keep
;;	64KB buffer limit is important also it is important
;;	for running under 1MB barrier without HIMEM.SYS or DPMI.
;;	I have tested this program by using 2-30MB wav files.))
;;
;;	Test Computer:	ASUS desktop/mainboard, M2N4-SLI, 2010.
;;			AMD Athlon 64 X2 2200 MHZ CPU.
;;		       	NFORCE4 (CK804) AC97 audio hardware.
;;			Realtek ALC850 codec.
;;		       	Retro DOS v4.2 (MSDOS 6.22) operating system.

load_8khz_mono_8_bit:
	; 02/02/2025
	; 15/11/2023
	; 14/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff8m_0		; no
	stc
	retn

lff8m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	;jc	short lff8m_5 ; error !
	; 14/11/2023
	jnc	short lff8m_6
	jmp	lff8m_5

lff8m_6:
	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	and	ax, ax
	;jz	short lff8m_3
	; 15/11/2023
	jz	short lff8_eof

	mov	cx, ax	; byte count
lff8m_1:
	lodsb
	mov	[previous_val], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	;xor	ax, ax
	; 02/02/2025
	mov	al, [si]
	dec	cx
	jnz	short lff8m_2
	; 14/11/2023
	mov	al, 80h
lff8m_2:
	;mov	[next_val], ax
	mov	bh, al	; [next_val]
	mov	ah, [previous_val]
	add	al, ah	; [previous_val]
	rcr	al, 1
	mov	dl, al	; this is interpolated middle (3th) sample
	add	al, ah	; [previous_val]
	rcr	al, 1	
	mov	bl, al 	; this is temporary interpolation value
	add	al, ah	; [previous_val]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 1st interpolated sample (L)
	stosw		; this is 1st interpolated sample (R)
	mov	al, bl
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 2nd interpolated sample (L)
	stosw		; this is 2nd interpolated sample (R)
	mov	al, dl
	sub	al, 80h
	shl	ax, 8
	stosw		; this is middle (3th) interpolated sample (L)
	stosw		; this is middle (3th) interpolated sample (R)
	;mov	al, [next_val]
	mov	al, bh
	add	al, dl
	rcr	al, 1
	mov	bl, al	; this is temporary interpolation value
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 4th interpolated sample (L)
	stosw		; this is 4th interpolated sample (R)
	;mov	al, [next_val]
	mov	al, bh
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 5th interpolated sample (L)
	stosw		; this is 5th interpolated sample (R)
	; 8 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff8m_1

	; --------------

lff8s_3:
lff8m_3:
lff8s2_3:
lff8m2_3:
lff16s_3:
lff16m_3:
lff16s2_3:
lff16m2_3:
lff24_3:
lff32_3:
lff44_3:
lff22_3:
lff11_3:
lff12_3:	; 02/02/2025
	mov	cx, [buffersize] ; 16 bit (48 kHZ, stereo) sample size
	shl	cx, 1	; byte count
	sub	cx, di
	jna	short lff8m_4
	;inc	cx
	shr	cx, 1
	xor	ax, ax	; fill (remain part of) buffer with zeros
	rep	stosw
lff8m_4:
	push	cs
	pop	es
	retn

lff8_eof:
lff16_eof:
lff24_eof:
lff32_eof:
lff44_eof:
lff22_eof:
lff11_eof:
lff12_eof:	; 02/02/2025
	; 15/11/2023
	mov	byte [flags], ENDOFFILE
	jmp	short lff8m_3

lff8s_5:
lff8m_5:
lff8s2_5:
lff8m2_5:
lff16s_5:
lff16m_5:
lff16s2_5:
lff16m2_5:
lff24_5:
lff32_5:
lff44_5:
lff22_5:
lff11_5:
lff12_5:	; 02/02/2025
	mov	al, '!'  ; error
	call	tL0
	
	;jmp	short lff8m_3
	; 15/11/2023
	jmp	lff8_eof

	; --------------

load_8khz_stereo_8_bit:
	; 02/02/2025
	; 15/11/2023
	; 14/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff8s_0		; no
	stc
	retn

lff8s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff8s_5 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;;and	ax, ax
	;jz	short lff8s_3
	; 15/11/2023
	jz	short lff8_eof

	mov	cx, ax	; word count
lff8s_1:
	lodsb
	mov	[previous_val_l], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	lodsb
	mov	[previous_val_r], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)

	;xor	ax, ax
	; 02/02/2025
	mov	ax, [si]
	dec	cx
	jnz	short lff8s_2
		; convert 8 bit sample to 16 bit sample
	; 14/11/2023
	mov	ax, 8080h
lff8s_2:
	mov	[next_val_l], al
	mov	[next_val_r], ah
	mov	ah, [previous_val_l]
	add	al, ah
	rcr	al, 1
	mov	dl, al	; this is interpolated middle (3th) sample (L)
	add	al, ah
	rcr	al, 1	
	mov	bl, al	; this is temporary interpolation value (L)
	add	al, ah
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 1st interpolated sample (L)
	mov	al, [next_val_r]
	mov	ah, [previous_val_r]
	add	al, ah
	rcr	al, 1
	mov	dh, al	; this is interpolated middle (3th) sample (R)
	add	al, ah
	rcr	al, 1
	mov	bh, al	; this is temporary interpolation value (R)
	add	al, ah
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 1st interpolated sample (R)
	mov	al, bl
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 2nd interpolated sample (L)
	mov	al, bh
	add	al, dh
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw 		; this is 2nd interpolated sample (R)
	mov	al, dl
	sub	al, 80h
	shl	ax, 8
	stosw		; this is middle (3th) interpolated sample (L)
	mov	al, dh
	sub	al, 80h
	shl	ax, 8
	stosw		; this is middle (3th) interpolated sample (R)
	mov	al, [next_val_l]
	add	al, dl
	rcr	al, 1
	mov	bl, al	; this is temporary interpolation value (L)
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 4th interpolated sample (L)
	mov	al, [next_val_r]
	add	al, dh
	rcr	al, 1
	mov	bh, al	; this is temporary interpolation value (R)
	add	al, dh
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 4th interpolated sample (R)
	mov	al, [next_val_l]
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 5th interpolated sample (L)
	mov	al, [next_val_r]
	add	al, bh
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 5th interpolated sample (R)
	; 8 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	jcxz	lff8s_6
	jmp	lff8s_1
lff8s_6:
	jmp	lff8s_3

load_8khz_mono_16_bit:
	; 02/02/2025
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff8m2_0		; no
	stc
	retn

lff8m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff8m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff8m2_8
	;jmp	lff8m2_3
	; 15/11/2023
	jmp	lff8_eof

lff8m2_8:
	mov	cx, ax	; word count
lff8m2_1:
	lodsw
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	add	ah, 80h	; convert sound level to 0-65535 format
	mov	[previous_val], ax
	; 02/02/2025
	mov	ax, [si]
	dec	cx
	jnz	short lff8m2_2
	xor	ax, ax
lff8m2_2:
	add	ah, 80h ; convert sound level to 0-65535 format
	mov	bp, ax	; [next_val]
	add	ax, [previous_val]
	rcr	ax, 1
	mov	dx, ax	; this is interpolated middle (3th) sample
	add	ax, [previous_val]
	rcr	ax, 1	; this is temporary interpolation value
	mov	bx, ax 		
	add	ax, [previous_val]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 1st interpolated sample (L)
	stosw		; this is 1st interpolated sample (R)
	mov	ax, bx
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h
	stosw		; this is 2nd interpolated sample (L)
	stosw		; this is 2nd interpolated sample (R)
	mov	ax, dx
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is middle (3th) interpolated sample (L)
	stosw		; this is middle (3th) interpolated sample (R)
	mov	ax, bp
	add	ax, dx
	rcr	ax, 1
	mov	bx, ax	; this is temporary interpolation value
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h
	stosw		; this is 4th interpolated sample (L)
	stosw		; this is 4th interpolated sample (R)
	mov	ax, bp
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 5th interpolated sample (L)
	stosw		; this is 5th interpolated sample (R)
	; 8 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff8m2_1
	jmp	lff8m2_3

lff8m2_7:
lff8s2_7:
	jmp	lff8m2_5  ; error

load_8khz_stereo_16_bit:
	; 02/02/2025
	; 16/11/2023
	; 15/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff8s2_0		; no
	stc
	retn

lff8s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff8s2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; 16/11/2023
	;and	ax, ax
	jnz	short lff8s2_8
	;jmp	lff8s2_3
	; 15/11/2023
	jmp	lff8_eof

lff8s2_8:
	mov	cx, ax	; dword count
lff8s2_1:
	lodsw
	stosw		; original sample (L)
	; 15/11/2023
	add	ah, 80h	; convert sound level to 0-65535 format
	mov	[previous_val_l], ax
	lodsw
	stosw		; original sample (R)
	add	ah, 80h	; convert sound level to 0-65535 format
	mov	[previous_val_r], ax
	; 02/02/2025
	mov	ax, [si]
	mov	dx, [si+2]
	; 16/11/2023
	dec	cx
	jnz	short lff8s2_2
	xor	dx, dx
	xor	ax, ax
lff8s2_2:
	add	ah, 80h	; convert sound level to 0-65535 format
	mov	[next_val_l], ax
	add	dh, 80h	; convert sound level to 0-65535 format
	mov	[next_val_r], dx
	add	ax, [previous_val_l]
	rcr	ax, 1
	mov	dx, ax	; this is interpolated middle (3th) sample (L)
	add	ax, [previous_val_l]
	rcr	ax, 1	
	mov	bx, ax 	; this is temporary interpolation value (L)
	add	ax, [previous_val_l]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 1st interpolated sample (L)
	mov	ax, [next_val_r]
	add	ax, [previous_val_r]
	rcr	ax, 1
	mov	bp, ax	; this is interpolated middle (3th) sample (R)
	add	ax, [previous_val_r]
	rcr	ax, 1
	push	ax ; *	; this is temporary interpolation value (R)
	add	ax, [previous_val_r]
	rcr	ax, 1
	sub	ah, 80h
	stosw		; this is 1st interpolated sample (R)
	mov	ax, bx
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 2nd interpolated sample (L)
	pop	ax ; *
	add	ax, bp
	rcr	ax, 1
	sub	ah, 80h
	stosw 		; this is 2nd interpolated sample (R)
	mov	ax, dx
	sub	ah, 80h
	stosw		; this is middle (3th) interpolated sample (L)
	mov	ax, bp
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is middle (3th) interpolated sample (R)
	mov	ax, [next_val_l]
	add	ax, dx
	rcr	ax, 1
	mov	bx, ax	; this is temporary interpolation value (L)
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h
	stosw		; this is 4th interpolated sample (L)
	mov	ax, [next_val_r]
	add	ax, bp
	rcr	ax, 1
	push	ax ; ** ; this is temporary interpolation value (R)
	add	ax, bp
	rcr	ax, 1
	sub	ah, 80h
	stosw		; this is 4th interpolated sample (R)
	mov	ax, [next_val_l]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 5th interpolated sample (L)
	pop	ax ; **
	add	ax, [next_val_r]
	rcr	ax, 1
	sub	ah, 80h
	stosw		; this is 5th interpolated sample (R)
	; 8 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	jcxz	lff8_s2_9
	jmp	lff8s2_1
lff8_s2_9:
	jmp	lff8s2_3

; .....................

load_16khz_mono_8_bit:
	; 02/02/2025
	; 14/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff16m_0		; no
	stc
	retn

lff16m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff16m_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff16m_8
	;jmp	lff16m_3
	; 15/11/2023
	jmp	lff16_eof

lff16m_8:
	mov	cx, ax	; byte count
lff16m_1:
	lodsb
	;mov	[previous_val], al
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	;xor	ax, ax
	; 02/02/2025
	mov	al, [si]
	dec	cx
	jnz	short lff16m_2
	; 14/11/2023
	mov	al, 80h
lff16m_2:
	;mov	[next_val], al
	mov	bh, al
	;add	al, [previous_val]
	add	al, bl
	rcr	al, 1
	mov	dl, al	; this is interpolated middle (temp) sample
	;add	al, [previous_val]
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 1st interpolated sample (L)
	stosw		; this is 1st interpolated sample (R)
	;mov	al, [next_val]
	mov	al, bh
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 2nd interpolated sample (L)
	stosw		; this is 2nd interpolated sample (R)
	
	; 16 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff16m_1
	jmp	lff16m_3

lff16m_7:
lff16s_7:
	jmp	lff16m_5  ; error

load_16khz_stereo_8_bit:
	; 02/02/2025
	; 14/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff16s_0		; no
	stc
	retn

lff16s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff16s_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff16s_8
	;jmp	lff16s_3
	; 15/11/2023
	jmp	lff16_eof

lff16s_8:
	mov	cx, ax	; word count
lff16s_1:
	lodsb
	mov	[previous_val_l], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	lodsb
	mov	[previous_val_r], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)

	;xor	ax, ax
	; 02/02/2025
	mov	ax, [si]
	dec	cx
	jnz	short lff16s_2
		; convert 8 bit sample to 16 bit sample
	; 14/11/2023
	mov	ax, 8080h
lff16s_2:
	;mov	[next_val_l], al
	;mov	[next_val_r], ah
	mov	bx, ax
	add	al, [previous_val_l]
	rcr	al, 1
	mov	dl, al	; this is temporary interpolation value (L)
	add	al, [previous_val_l]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 1st interpolated sample (L)
	mov	al, bh	; [next_val_r]
	add	al, [previous_val_r]
	rcr	al, 1
	mov	dh, al	; this is temporary interpolation value (R)
	add	al, [previous_val_r]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 1st interpolated sample (R)
	mov	al, dl
	add	al, bl	; [next_val_l]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is 2nd interpolated sample (L)
	mov	al, dh
	add	al, bh	; [next_val_r]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw 		; this is 2nd interpolated sample (R)
	
	; 16 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff16s_1
	jmp	lff16s_3

load_16khz_mono_16_bit:
	; 02/02/2025
	; 15/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff16m2_0		; no
	stc
	retn

lff16m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff16m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff16m2_8
	;jmp	lff16m2_3
	; 15/11/2023
	jmp	lff16_eof

lff16m2_8:
	mov	cx, ax	; word count
lff16m2_1:
	lodsw
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	add	ah, 80h ; convert sound level 0 to 65535 format
	;mov	[previous_val], ax
	mov	bx, ax	
	; 02/02/2025
	mov	ax, [si]
	dec	cx
	jnz	short lff16m2_2
	xor	ax, ax
lff16m2_2:
	add	ah, 80h ; convert sound level 0 to 65535 format
	mov	bp, ax	; [next_val]
	;add	ax, [previous_val]
	add	ax, bx
	rcr	ax, 1
	mov	dx, ax	; this is temporary interpolation value
	;add	ax, [previous_val]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 1st interpolated sample (L)
	stosw		; this is 1st interpolated sample (R)
	mov	ax, bp 
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 2nd interpolated sample (L)
	stosw		; this is 2nd interpolated sample (R)
	; 16 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff16m2_1
	jmp	lff16m2_3

lff16m2_7:
lff16s2_7:
	jmp	lff16m2_5  ; error

load_16khz_stereo_16_bit:
	; 02/02/2025
	; 16/11/2023
	; 15/11/2023
	; 13/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff16s2_0		; no
	stc
	retn

lff16s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff16s2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	;shr	ax, 1
	shr	ax, 2	; 16/11/2023
	;and	ax, ax
	jnz	short lff16s2_8
	;jmp	lff16s2_3
	; 15/11/2023
	jmp	lff16_eof

lff16s2_8:
	mov	cx, ax	; dword count
lff16s2_1:
	lodsw
	stosw		; original sample (L)
	add	ah, 80h	; convert sound level 0 to 65535 format
	mov	[previous_val_l], ax
	lodsw
	stosw		; original sample (R)
	add	ah, 80h	; convert sound level 0 to 65535 format
	mov	[previous_val_r], ax
	; 02/02/2025
	mov	ax, [si]
	mov	dx, [si+2]
	; 16/11/2023
	dec	cx
	jnz	short lff16s2_2
	xor	dx, dx
	xor	ax, ax
lff16s2_2:
	add	ah, 80h	; convert sound level 0 to 65535 format
	;mov	[next_val_l], ax
	mov	bp, ax
	add	dh, 80h	; convert sound level 0 to 65535 format
	mov	[next_val_r], dx
	add	ax, [previous_val_l]
	rcr	ax, 1
	mov	dx, ax	; this is temporary interpolation value (L)
	add	ax, [previous_val_l]
	rcr	ax, 1
	sub	ah, 80h ; -32768 to +32767 format again
	stosw		; this is 1st interpolated sample (L)
	mov	ax, [next_val_r]
	add	ax, [previous_val_r]
	rcr	ax, 1
	mov	bx, ax	; this is temporary interpolation value (R)
	add	ax, [previous_val_r]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 1st interpolated sample (R)
	;mov	ax, [next_val_l]
	mov	ax, bp
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is 2nd interpolated sample (L)
	mov	ax, [next_val_r]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; this is 2nd interpolated sample (R)
	
	; 16 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff16s2_1
	jmp	lff16s2_3

; .....................

load_24khz_mono_8_bit:
	; 02/02/2025
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff24m_0		; no
	stc
	retn

lff24m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff24m_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff24m_8
	jmp	lff24_eof

lff24m_8:
	mov	cx, ax	; byte count
lff24m_1:
	lodsb
	;mov	[previous_val], al
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	;xor	ax, ax
	; 02/02/2025
	mov	al, [si]
	dec	cx
	jnz	short lff24m_2
	mov	al, 80h
lff24m_2:
	;;mov	[next_val], al
	;mov	bh, al
	;add	al, [previous_val]
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is interpolated sample (L)
	stosw		; this is interpolated sample (R)
	
	; 24 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff24m_1
	jmp	lff24_3

lff24m_7:
lff24s_7:
	jmp	lff24_5  ; error

load_24khz_stereo_8_bit:
	; 02/02/2025
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff24s_0		; no
	stc
	retn

lff24s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff24s_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff24s_8
	jmp	lff24_eof

lff24s_8:
	mov	cx, ax	; word count
lff24s_1:
	lodsb
	mov	[previous_val_l], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	lodsb
	mov	[previous_val_r], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)
	;xor	ax, ax
	; 02/02/2025
	mov	ax, [si]
	dec	cx
	jnz	short lff24s_2
		; convert 8 bit sample to 16 bit sample
	mov	ax, 8080h
lff24s_2:
	;;mov	[next_val_l], al
	;;mov	[next_val_r], ah
	;mov	bx, ax
	mov	bh, ah
	add	al, [previous_val_l]
	rcr	al, 1
	;mov	dl, al
	sub	al, 80h
	shl	ax, 8
	stosw		; this is interpolated sample (L)
	mov	al, bh	; [next_val_r]
	add	al, [previous_val_r]
	rcr	al, 1
	;mov	dh, al
	sub	al, 80h
	shl	ax, 8
	stosw		; this is interpolated sample (R)
		
	; 24 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff24s_1
	jmp	lff24_3

load_24khz_mono_16_bit:
	; 02/02/2025
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff24m2_0		; no
	stc
	retn

lff24m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff24m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff24m2_8
	jmp	lff24_eof

lff24m2_8:
	mov	cx, ax	; word count
lff24m2_1:
	lodsw
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	add	ah, 80h ; convert sound level 0 to 65535 format
	;mov	[previous_val], ax
	;mov	bx, ax	
	;mov	ax, [si
	; 02/02/2025
	mov	bx, [si]
	dec	cx
	jnz	short lff24m2_2
	;xor	ax, ax
	xor	bx, bx
lff24m2_2:
	; 02/02/2025
	add	bh, 80h ; convert sound level 0 to 65535 format
	;add	ah, 80h
	;mov	bp, ax	; [next_val]
	;add	ax, [previous_val]
	; ax = [previous_val]
	; bx = [next_val]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is interpolated sample (L)
	stosw		; this is interpolated sample (R)
	; 24 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff24m2_1
	jmp	lff24_3

lff24m2_7:
lff24s2_7:
	jmp	lff24_5  ; error

load_24khz_stereo_16_bit:
	; 02/02/2025
	; 16/11/2023
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff24s2_0		; no
	stc
	retn

lff24s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff24s2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	;shr	ax, 1
	shr	ax, 2	; 16/11/2023
	;and	ax, ax
	jnz	short lff24s2_8
	jmp	lff24_eof

lff24s2_8:
	mov	cx, ax	; dword count
lff24s2_1:
	lodsw
	stosw		; original sample (L)
	add	ah, 80h	; convert sound level 0 to 65535 format 
	mov	[previous_val_l], ax
	lodsw
	stosw		; original sample (R)
	add	ah, 80h	; convert sound level 0 to 65535 format 
	;mov	[previous_val_r], ax
	mov	bx, ax
	; 02/02/2025
	mov	ax, [si]
	mov	dx, [si+2]
	; 16/11/2023
	dec	cx
	jnz	short lff24s2_2
	xor	dx, dx
	xor	ax, ax
lff24s2_2:
	add	ah, 80h	; convert sound level 0 to 65535 format 
	;;mov	[next_val_l], ax
	;mov	bp, ax
	add	dh, 80h	; convert sound level 0 to 65535 format 
	;mov	[next_val_r], dx
	add	ax, [previous_val_l]
	rcr	ax, 1
	sub	ah, 80h ; -32768 to +32767 format again
	stosw		; this is interpolated sample (L)
	;mov	ax, [next_val_r]
	mov	ax, dx
	;add	ax, [previous_val_r]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is interpolated sample (R)
	
	; 24 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	or	cx, cx
	jnz	short lff24s2_1
	jmp	lff24_3

; .....................

load_32khz_mono_8_bit:
	; 02/02/2025
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff32m_0		; no
	stc
	retn

lff32m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff32m_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff32m_8
	jmp	lff32_eof

lff32m_8:
	mov	cx, ax	; byte count
lff32m_1:
	lodsb
	;mov	[previous_val], al
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	;xor	ax, ax
	; 02/02/2025
	mov	al, [si]
	dec	cx
	jnz	short lff32m_2
	mov	al, 80h
lff32m_2:
	;;mov	[next_val], al
	;mov	bh, al
	;add	al, [previous_val]
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8
	stosw		; this is interpolated sample (L)
	stosw		; this is interpolated sample (R)
	
	; different than 8-16-24 kHZ !
	; 'original-interpolated-original' trio samples
	jcxz	lff32m_3

	lodsb
	sub	al, 80h
	shl	ax, 8
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)

	; 32 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	dec	cx
	jnz	short lff32m_1
lff32m_3:
	jmp	lff32_3

lff32m_7:
lff32s_7:
	jmp	lff32_5  ; error

load_32khz_stereo_8_bit:
	; 02/02/2025
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff32s_0		; no
	stc
	retn

lff32s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff32s_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff32s_8
	jmp	lff32_eof

lff32s_8:
	mov	cx, ax	; word count
lff32s_1:
	lodsb
	mov	[previous_val_l], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	lodsb
	mov	[previous_val_r], al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)
	; 02/02/2025
	mov	ax, [si]
	dec	cx
	jnz	short lff32s_2
		; convert 8 bit sample to 16 bit sample
	;xor	ax, ax
	mov	ax, 8080h
lff32s_2:
	;;mov	[next_val_l], al
	;;mov	[next_val_r], ah
	;mov	bx, ax
	mov	bh, ah
	add	al, [previous_val_l]
	rcr	al, 1
	;mov	dl, al
	sub	al, 80h
	shl	ax, 8
	stosw		; this is interpolated sample (L)
	mov	al, bh	; [next_val_r]
	add	al, [previous_val_r]
	rcr	al, 1
	;mov	dh, al
	sub	al, 80h
	shl	ax, 8
	stosw		; this is interpolated sample (R)

	; different than 8-16-24 kHZ !
	; 'original-interpolated-original' trio samples
	jcxz	lff32s_3

	lodsb
	sub	al, 80h
	shl	ax, 8
	stosw		; original sample (left channel)

	lodsb
	sub	al, 80h
	shl	ax, 8
	stosw		; original sample (right channel)
		
	; 32 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	dec	cx
	jnz	short lff32s_1
lff32s_3:
	jmp	lff32_3

load_32khz_mono_16_bit:
	; 02/02/2025
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff32m2_0		; no
	stc
	retn

lff32m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff32m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff32m2_8
	jmp	lff32_eof

lff32m2_8:
	mov	cx, ax	; word count
lff32m2_1:
	lodsw
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)
	add	ah, 80h ; convert sound level 0 to 65535 format
	;mov	[previous_val], ax
	;mov	bx, ax	
	; 02/02/2025
	;mov	ax, [si]
	mov	bx, [si]
	dec	cx
	jnz	short lff32m2_2
	;xor	ax, ax
	xor	bx, bx
lff32m2_2:
	; 02/02/2025
	add	bh, 80h ; convert sound level 0 to 65535 format
	;add	ah, 80h
	;mov	bp, ax	; [next_val]
	;add	ax, [previous_val]
	; ax = [previous_val]
	; bx = [next_val]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is interpolated sample (L)
	stosw		; this is interpolated sample (R)

	; different than 8-16-24 kHZ !
	; 'original-interpolated-original' trio samples
	jcxz	lff32m2_3

	lodsw
	stosw		; original sample (left channel)
	stosw		; original sample (right channel)

	; 32 kHZ mono to 48 kHZ stereo conversion of the sample is OK
	dec	cx
	jnz	short lff32m2_1
lff32m2_3:
	jmp	lff32_3

lff32m2_7:
lff32s2_7:
	jmp	lff32_5  ; error

load_32khz_stereo_16_bit:
	; 02/02/2025
	; 16/11/2023
	; 15/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff32s2_0		; no
	stc
	retn

lff32s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff32s2_7 ; error !

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; 16/11/2023 (word left ch + word right ch)
	;and	ax, ax
	jnz	short lff32s2_8
	jmp	lff32_eof

lff32s2_8:
	mov	cx, ax	; dword count
lff32s2_1:
	lodsw
	stosw		; original sample (L)
	add	ah, 80h	; convert sound level 0 to 65535 format
	mov	[previous_val_l], ax
	lodsw
	stosw		; original sample (R)
	add	ah, 80h	; convert sound level 0 to 65535 format
	;mov	[previous_val_r], ax
	mov	bx, ax
	; 02/02/2025
	mov	ax, [si]
	mov	dx, [si+2]
	; 16/11/2023
	dec	cx
	jnz	short lff32s2_2
	xor	dx, dx
	xor	ax, ax
lff32s2_2:
	add	ah, 80h	; convert sound level 0 to 65535 format
	;;mov	[next_val_l], ax
	;mov	bp, ax
	add	dh, 80h	; convert sound level 0 to 65535 format
	;mov	[next_val_r], dx
	add	ax, [previous_val_l]
	rcr	ax, 1
	sub	ah, 80h ; -32768 to +32767 format again
	stosw		; this is interpolated sample (L)
	;mov	ax, [next_val_r]
	mov	ax, dx
	;add	ax, [previous_val_r]
	add	ax, bx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; this is interpolated sample (R)

	; different than 8-16-24 kHZ !
	; 'original-interpolated-original' trio samples
	jcxz	lff32s2_3

	lodsw
	stosw	; original sample (L)
	lodsw
	stosw	; original sample (R)
	
	; 32 kHZ stereo to 48 kHZ stereo conversion of the sample is OK
	dec	cx
	jnz	short lff32s2_1
lff32s2_3:
	jmp	lff32_3

; .....................

load_22khz_mono_8_bit:
	; 02/02/2025
	; 16/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff22m_0		; no
	stc
	retn

lff22m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff22m_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff22m_8
	jmp	lff22_eof

lff22m_8:
	mov	cx, ax	; byte count
lff22m_9:
	mov	bp, 5	; interpolation (one step) loop count
	mov	byte [faz], 3  ; 3 steps/phases
lff22m_1:
	; 3:2:2:2:2:2::3:2:2:2:2::3:2:2:2:2:2  ; 37/17
	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff22m_2_1
	mov	dl, 80h
lff22m_2_1:	
	; al = [previous_val]
	; dl = [next_val]
	call	interpolating_3_8bit_mono ; 1 of 17
	jcxz	lff22m_3
lff22m_2_2:
	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff22m_2_3
	mov	dl, 80h
lff22m_2_3:
 	call	interpolating_2_8bit_mono ; 2 of 17 .. 6 of 17
	jcxz	lff22m_3
	dec	bp
	jnz	short lff22m_2_2

	mov	al, [faz]
	dec	al
	jz	short lff22m_9
	dec	byte [faz]
	mov	bp, 4
	dec	al
	jnz	short lff22m_1 ; 3:2:2:2:2 ; 7-11 of 17
	inc	bp ; 5
	jmp	short lff22m_1 ; 3:2:2:2:2:2 ; 12-17 of 17

lff22m_3:
lff22s_3:
	jmp	lff22_3	; padfill
		; (put zeros in the remain words of the buffer)
lff22m_7:
lff22s_7:
	jmp	lff22_5  ; error

load_22khz_stereo_8_bit:
	; 02/02/2025
	; 16/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff22s_0		; no
	stc
	retn

lff22s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff22s_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff22s_8
	jmp	lff22_eof

lff22s_8:
	mov	cx, ax	; word count
lff22s_9:
	mov	bp, 5	; interpolation (one step) loop count
	mov	byte [faz], 3  ; 3 steps/phase
lff22s_1:
	; 3:2:2:2:2:2::3:2:2:2:2::3:2:2:2:2:2  ; 37/17
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff22s_2_1
	mov	dx, 8080h
lff22s_2_1:	
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dl = [next_val_r]	
	call	interpolating_3_8bit_stereo ; 1 of 17
	jcxz	lff22s_3
lff22s_2_2:
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff22s_2_3
	mov	dx, 8080h
lff22s_2_3:
 	call	interpolating_2_8bit_stereo ; 2 of 17 .. 6 of 17
	jcxz	lff22s_3
	dec	bp
	jnz	short lff22s_2_2

	mov	al, [faz]
	dec	al
	jz	short lff22s_9
	dec	byte [faz]
	mov	bp, 4
	dec	al
	jnz	short lff22s_1 ; 3:2:2:2:2 ; 7-11 of 17
	inc	bp ; 5
	jmp	short lff22s_1 ; 3:2:2:2:2:2 ; 12-17 of 17

load_22khz_mono_16_bit:
	; 02/02/2025
	; 16/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff22m2_0		; no
	stc
	retn

lff22m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff22m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff22m2_8
	jmp	lff22_eof

lff22m2_8:
	mov	cx, ax	; word count
lff22m2_9:
	mov	bp, 5	; interpolation (one step) loop count
	mov	byte [faz], 3  ; 3 steps/phases
lff22m2_1:
	; 3:2:2:2:2:2::3:2:2:2:2::3:2:2:2:2:2  ; 37/17
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff22m2_2_1
	xor	dx, dx
lff22m2_2_1:	
	; ax = [previous_val]
	; dx = [next_val]
	call	interpolating_3_16bit_mono ; 1 of 17
	jcxz	lff22m2_3
lff22m2_2_2:
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff22m2_2_3
	xor	dx, dx
lff22m2_2_3:
 	call	interpolating_2_16bit_mono ; 2 of 17 .. 6 of 17
	jcxz	lff22m2_3
	dec	bp
	jnz	short lff22m2_2_2

	mov	al, [faz]
	dec	al
	jz	short lff22m2_9
	dec	byte [faz]
	mov	bp, 4
	dec	al
	jnz	short lff22m2_1 ; 3:2:2:2:2 ; 7-11 of 17
	inc	bp ; 5
	jmp	short lff22m2_1 ; 3:2:2:2:2:2 ; 12-17 of 17

lff22m2_3:
lff22s2_3:
	jmp	lff22_3	; padfill
		; (put zeros in the remain words of the buffer)
lff22m2_7:
lff22s2_7:
	jmp	lff22_5  ; error

load_22khz_stereo_16_bit:
	; 16/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff22s2_0		; no
	stc
	retn

lff22s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff22s2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; dword (left chan word + right chan word)
	;and	ax, ax
	jnz	short lff22s2_8
	jmp	lff22_eof

lff22s2_8:
	mov	cx, ax	; dword count
lff22s2_9:
	mov	bp, 5	; interpolation (one step) loop count
	mov	byte [faz], 3  ; 3 steps/phase
lff22s2_1:
	; 3:2:2:2:2:2::3:2:2:2:2::3:2:2:2:2:2  ; 37/17
	lodsw
	mov	bx, ax
	lodsw
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	dec	cx
	jnz	short lff22s2_2_1
	xor	dx, dx ; 0
	mov	[next_val_l], dx
lff22s2_2_1:
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; dx = [next_val_r]
	call	interpolating_3_16bit_stereo ; 1 of 17 
	jcxz	lff22s2_3
lff22s2_2_2:
	lodsw
	mov	bx, ax
	lodsw
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	dec	cx
	jnz	short lff22s2_2_3
	xor	dx, dx ; 0
	mov	[next_val_l], dx
lff22s2_2_3:
 	call	interpolating_2_16bit_stereo ; 2 of 17 .. 6 of 17
	jcxz	lff22s2_3
	dec	bp
	jnz	short lff22s2_2_2

	mov	al, [faz]
	dec	al
	jz	short lff22s2_9
	dec	byte [faz]
	mov	bp, 4
	dec	al
	jnz	short lff22s2_1 ; 3:2:2:2:2 ; 7-11 of 17
	inc	bp ; 5
	jmp	short lff22s2_1 ; 3:2:2:2:2:2 ; 12-17 of 17

; .....................

load_11khz_mono_8_bit:
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff11m_0		; no
	stc
	retn

lff11m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff11m_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff11m_8
	jmp	lff11_eof

lff11m_8:
	mov	cx, ax	; byte count
lff11m_9:
	mov	bp, 6	; interpolation (one step) loop count
lff11m_1:
	; 5:4:4::5:4:4::5:4:4::5:4:4::5:4:4::5:4  ; 74/17
	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff11m_2_1
	mov	dl, 80h
lff11m_2_1:	
	; al = [previous_val]
	; dl = [next_val]
	call	interpolating_5_8bit_mono
	jcxz	lff11m_3
lff11m_2_2:
	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff11m_2_3
	mov	dl, 80h
lff11m_2_3:
 	call	interpolating_4_8bit_mono
	jcxz	lff11m_3

	dec	bp
	jz	short lff11m_9

	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff11m_2_4
	mov	dl, 80h
lff11m_2_4:
	call	interpolating_4_8bit_mono
	jcxz	lff11m_3
	jmp	short lff11m_1

lff11m_3:
lff11s_3:
	jmp	lff11_3	; padfill
		; (put zeros in the remain words of the buffer)
lff11m_7:
lff11s_7:
	jmp	lff11_5  ; error

load_11khz_stereo_8_bit:
	; 02/02/2025
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff11s_0		; no
	stc
	retn

lff11s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff11s_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff11s_8
	jmp	lff11_eof

lff11s_8:
	mov	cx, ax	; word count
lff11s_9:
	mov	bp, 6	; interpolation (one step) loop count
lff11s_1:
	; 5:4:4::5:4:4::5:4:4::5:4:4::5:4:4::5:4  ; 74/17
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff11s_2_1
	mov	dx, 8080h
lff11s_2_1:	
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dl = [next_val_r]	
	call	interpolating_5_8bit_stereo
	jcxz	lff11s_3
lff11s_2_2:
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff11s_2_3
	mov	dx, 8080h
lff11s_2_3:
 	call	interpolating_4_8bit_stereo
	jcxz	lff11s_3
	
	dec	bp
	jz	short lff11s_9

	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff11s_2_4
	mov	dx, 8080h
lff11s_2_4:
	call	interpolating_4_8bit_stereo
	jcxz	lff11s_3
	jmp	short lff11s_1

load_11khz_mono_16_bit:
	; 02/02/2025
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff11m2_0		; no
	stc
	retn

lff11m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff11m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff11m2_8
	jmp	lff11_eof

lff11m2_8:
	mov	cx, ax	; word count
lff11m2_9:
	mov	bp, 6	; interpolation (one step) loop count
lff11m2_1:
	; 5:4:4::5:4:4::5:4:4::5:4:4::5:4:4::5:4  ; 74/17
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff11m2_2_1
	xor	dx, dx
lff11m2_2_1:	
	; ax = [previous_val]
	; dx = [next_val]
	call	interpolating_5_16bit_mono
	jcxz	lff11m2_3
lff11m2_2_2:
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff11m2_2_3
	xor	dx, dx
lff11m2_2_3:
 	call	interpolating_4_16bit_mono
	jcxz	lff11m2_3

	dec	bp
	jz	short lff11m2_9

	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff11m2_2_4
	xor	dx, dx
lff11m2_2_4:
 	call	interpolating_4_16bit_mono
	jcxz	lff11m2_3
	jmp	short lff11m2_1

lff11m2_7:
lff11s2_7:
	jmp	lff11_5  ; error

load_11khz_stereo_16_bit:
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff11s2_0		; no
	stc
	retn

lff11s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff11s2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; dword (left chan word + right chan word)
	;and	ax, ax
	jnz	short lff11s2_8
	jmp	lff11_eof

lff11m2_3:
lff11s2_3:
	jmp	lff11_3	; padfill
		; (put zeros in the remain words of the buffer)

lff11s2_8:
	mov	cx, ax	; dword count
lff11s2_9:
	mov	bp, 6	; interpolation (one step) loop count
lff11s2_1:
	; 5:4:4::5:4:4::5:4:4::5:4:4::5:4:4::5:4  ; 74/17
	lodsw
	mov	bx, ax
	lodsw
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	mov	[next_val_r], dx
	dec	cx
	jnz	short lff11s2_2_1
	xor	dx, dx ; 0
	mov	[next_val_l], dx
	mov	[next_val_r], dx
lff11s2_2_1:
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; dx = [next_val_r]
	call	interpolating_5_16bit_stereo
	jcxz	lff11s2_3
lff11s2_2_2:
	lodsw
	mov	bx, ax
	lodsw
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	mov	[next_val_r], dx
	dec	cx
	jnz	short lff11s2_2_3
	xor	dx, dx ; 0
	mov	[next_val_l], dx
	mov	[next_val_r], dx
lff11s2_2_3:
 	call	interpolating_4_16bit_stereo
	jcxz	lff11s2_3
	
	dec	bp
	jz	short lff11s2_9

	lodsw
	mov	bx, ax
	lodsw
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	mov	[next_val_r], dx
	dec	cx
	jnz	short lff11s2_2_4
	xor	dx, dx ; 0
	mov	[next_val_l], dx
	mov	[next_val_r], dx
lff11s2_2_4:
 	call	interpolating_4_16bit_stereo
	jcxz	lff11s2_3
	jmp	short lff11s2_1

; .....................

load_44khz_mono_8_bit:
	; 02/02/2025
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff44m_0		; no
	stc
	retn

lff44m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff44m_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff44m_8
	jmp	lff44_eof

lff44m_8:
	mov	cx, ax	; byte count
lff44m_9:
	mov	bp, 10	; interpolation (one step) loop count
	mov	byte [faz], 2  ; 2 steps/phases
lff44m_1:
	; 2:1:1:1:1:1:1:1:1:1:1::	; 25/23
	; 2:1:1:1:1:1:1:1:1:1:1:1
	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff44m_2_1
	mov	dl, 80h
lff44m_2_1:	
	; al = [previous_val]
	; dl = [next_val]
	call	interpolating_2_8bit_mono
	jcxz	lff44m_3
lff44m_2_2:
	lodsb
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; (L)
	stosw		; (R)	

	dec	cx
	jz	short lff44m_3	
	dec	bp
	jnz	short lff44m_2_2
	
	dec	byte [faz]
	jz	short lff44m_9 
	mov	bp, 11
	jmp	short lff44m_1

lff44m_3:
lff44s_3:
	jmp	lff44_3	; padfill
		; (put zeros in the remain words of the buffer)
lff44m_7:
lff44s_7:
	jmp	lff44_5  ; error

load_44khz_stereo_8_bit:
	; 02/02/2025
	; 16/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff44s_0		; no
	stc
	retn

lff44s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff44s_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff44s_8
	jmp	lff44_eof

lff44s_8:
	mov	cx, ax	; word count
lff44s_9:
	mov	bp, 10	; interpolation (one step) loop count
	mov	byte [faz], 2  ; 2 steps/phase
lff44s_1:
	; 2:1:1:1:1:1:1:1:1:1:1::	; 25/23
	; 2:1:1:1:1:1:1:1:1:1:1:1
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff44s_2_1
	mov	dx, 8080h
lff44s_2_1:	
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dl = [next_val_r]	
	call	interpolating_2_8bit_stereo
	jcxz	lff44s_3
lff44s_2_2:
	lodsb
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; (L)
	lodsb
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; (R)

	dec	cx
	jz	short lff44s_3	
	dec	bp
	jnz	short lff44s_2_2
	
	dec	byte [faz]
	jz	short lff44s_9 
	mov	bp, 11
	jmp	short lff44s_1

load_44khz_mono_16_bit:
	; 02/02/2025
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff44m2_0		; no
	stc
	retn

lff44m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff44m2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff44m2_8
	jmp	lff44_eof

lff44m2_8:
	mov	cx, ax	; word count
lff44m2_9:
	mov	bp, 10	; interpolation (one step) loop count
	mov	byte [faz], 2  ; 2 steps/phases
lff44m2_1:
	; 2:1:1:1:1:1:1:1:1:1:1::	; 25/23
	; 2:1:1:1:1:1:1:1:1:1:1:1
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff44m2_2_1
	xor	dx, dx
lff44m2_2_1:	
	; ax = [previous_val]
	; dx = [next_val]
	call	interpolating_2_16bit_mono
	jcxz	lff44m2_3
lff44m2_2_2:
	lodsw
	stosw		; (L)eft Channel
	stosw		; (R)ight Channel

	dec	cx
	jz	short lff44m2_3	
	dec	bp
	jnz	short lff44m2_2_2
	
	dec	byte [faz]
	jz	short lff44m2_9 
	mov	bp, 11
	jmp	short lff44m2_1

lff44m2_3:
lff44s2_3:
	jmp	lff44_3	; padfill
		; (put zeros in the remain words of the buffer)
lff44m2_7:
lff44s2_7:
	jmp	lff44_5  ; error

load_44khz_stereo_16_bit:
	; 18/11/2023
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff44s2_0		; no
	stc
	retn

lff44s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff44s2_7 ; error !

	; 14/11/2024
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; dword (left chan word + right chan word)
	;and	ax, ax
	jnz	short lff44s2_8
	jmp	lff44_eof

lff44s2_8:
	mov	cx, ax	; dword count
lff44s2_9:
	mov	bp, 10	; interpolation (one step) loop count
	mov	byte [faz], 2  ; 2 steps/phase
lff44s2_1:
	; 2:1:1:1:1:1:1:1:1:1:1::	; 25/23
	; 2:1:1:1:1:1:1:1:1:1:1:1
	lodsw
	mov	bx, ax
	lodsw
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	dec	cx
	jnz	short lff44s2_2_1
	xor	dx, dx ; 0
	mov	[next_val_l], dx
lff44s2_2_1:
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; dx = [next_val_r]
	call	interpolating_2_16bit_stereo
	jcxz	lff44s2_3
lff44s2_2_2:
	;lodsw
	;stosw		; (L)
	;lodsw
	;stosw		; (R)
	movsw		; (L)eft Channel
	movsw		; (R)ight Channel

	dec	cx
	jz	short lff44s2_3	
	dec	bp
	jnz	short lff44s2_2_2
	
	dec	byte [faz]
	jz	short lff44s2_9 
	mov	bp, 11
	jmp	short lff44s2_1

; .....................

	; 02/02/2025
load_12khz_mono_8_bit:
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff12m_0		; no
	stc
	retn

lff12m_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff12m_7 ; error !

	; 02/02/2025
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address

	and	ax, ax
	jnz	short lff12m_8
	jmp	lff12_eof

lff12m_8:
	mov	cx, ax	; byte count
lff12m_1:
	; original-interpolated-interpolated-interpolated
	lodsb
	; 02/02/2025
	mov	dl, [si]
	dec	cx
	jnz	short lff12m_2
	mov	dl, 80h
lff12m_2:	
	; al = [previous_val]
	; dl = [next_val]
 	call	interpolating_4_8bit_mono
	jcxz	lff12m_3
	jmp	short lff12m_1

	; 02/02/2025
load_12khz_stereo_8_bit:
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff12s_0		; no
	stc
	retn

lff12s_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff12s_7 ; error !

	; 02/02/2025
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff12s_8
	jmp	lff12_eof

lff12m_7:
lff12s_7:
	jmp	lff12_5  ; error

lff12s_8:
	mov	cx, ax	; word count
lff12s_1:
	; original-interpolated-interpolated-interpolated
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff12s_2
	mov	dx, 8080h
lff12s_2:	
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dh = [next_val_r]
	call	interpolating_4_8bit_stereo
	jecxz	lff12s_3
	jmp	short lff12s_1

lff12m_3:
lff12s_3:
	jmp	lff12_3	; padfill
		; (put zeros in the remain words of the buffer)

	; 02/02/2025
load_12khz_mono_16_bit:
	test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff12m2_0		; no
	stc
	retn

lff12m2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff12m2_7 ; error !

	; 02/02/2025
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff12m2_8
	jmp	lff12_eof

lff12m2_8:
	mov	cx, ax	; word count
lff12m2_1:
	; original-interpolated-interpolated-interpolated
	lodsw
	; 02/02/2025
	mov	dx, [si]
	dec	cx
	jnz	short lff12m2_2
	xor	dx, dx
lff12m2_2:	
	; ax = [previous_val]
	; dx = [next_val]
 	call	interpolating_4_16bit_mono
	jecxz	lff12m_3
	jmp	short lff12m2_1

lff12m2_7:
lff12s2_7:
	jmp	lff12_5  ; error

	; 02/02/2025
load_12khz_stereo_16_bit:
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short lff12s2_0		; no
	stc
	retn

lff12s2_0:
	mov	es, ax ; buffer segment
	xor	di, di
	mov	dx, temp_buffer ; temporary buffer for wav data
	; ds = cs

	; load file into memory
        mov	cx, [loadsize]
	mov	bx, [filehandle]
       	mov	ah, 3Fh
	int	21h
	jc	short lff12s2_7 ; error !

	; 02/02/2025
	mov	[count], ax

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; dword (left chan word + right chan word)
	;and	ax, ax
	jnz	short lff12s2_8
	jmp	lff12_eof

lff12m2_3:
lff12s2_3:
	jmp	lff12_3	; padfill
		; (put zeros in the remain words of the buffer)

lff12s2_8:
	mov	cx, ax	; dword count
lff12s2_1:
	; original-interpolated-interpolated-interpolated
	lodsw
	mov	bx, ax
	lodsw
	; 02/02/2025
	mov	dx, [si]
	mov	[next_val_l], dx
	mov	dx, [si+2]
	mov	[next_val_r], dx
	dec	cx
	jnz	short lff12s2_2
	xor	dx, dx ; 0
	mov	[next_val_l], dx
	mov	[next_val_r], dx
lff12s2_2:
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; [next_val_r]
	call	interpolating_4_16bit_stereo
	jecxz	lff12s2_3
	jmp	short lff12s2_1

; .....................

interpolating_3_8bit_mono:
	; 02/02/2025
	; 16/11/2023
	; al = [previous_val]
	; dl = [next_val]
	; original-interpolated-interpolated
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	stosw		; original sample (R)
	mov	al, bl
	add	al, dl
	rcr	al, 1
	mov	bh, al	; interpolated middle (temporary)
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (L)
	stosw		; interpolated sample 1 (R)
	mov	al, bh
	add	al, dl	; [next_val]
	rcr	al, 1
	; 02/02/2025
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)
	stosw		; interpolated sample 2 (R)
	retn

interpolating_3_8bit_stereo:
	; 02/02/2025
	; 16/11/2023
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dh = [next_val_r]
	; original-interpolated-interpolated
	mov	bx, ax
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	mov	al, bh
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)
	mov	al, bl
	add	al, dl	; [next_val_l]
	rcr	al, 1
	push	ax ; *	; al = interpolated middle (L) (temporary)
	add	al, bl	; [previous_val_l]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (L)
	mov	al, bh
	add	al, dh	; [next_val_r]
	rcr	al, 1
	push	ax ; ** ; al = interpolated middle (R) (temporary)
	add	al, bh	; [previous_val_r]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (R)
	pop	bx ; **
	pop	ax ; *
	add	al, dl	; [next_val_l]
	rcr	al, 1
	; 02/02/2025
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)
	mov	al, bl
	add	al, dh	; [next_val_r]
	rcr	al, 1
	; 02/02/2025
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (R)
	retn

interpolating_2_8bit_mono:
	; 16/11/2023
	; al = [previous_val]
	; dl = [next_val]
	; original-interpolated
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	stosw		; original sample (R)
	mov	al, bl
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample (L)
	stosw		; interpolated sample (R)
	retn

interpolating_2_8bit_stereo:
	; 16/11/2023
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dh = [next_val_r]
	; original-interpolated
	mov	bx, ax
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	mov	al, bh
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)
	mov	al, bl	; [previous_val_l]
	add	al, dl	; [next_val_l]	
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample (L)
	mov	al, bh
	add	al, dh	; [next_val_r]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample (R)
	retn

interpolating_3_16bit_mono:
	; 16/11/2023
	; ax = [previous_val]
	; dx = [next_val]
	; original-interpolated-interpolated

	stosw		; original sample (L)
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	push	ax ; *	; [previous_val]
	add	dh, 80h
	add	ax, dx
	rcr	ax, 1
	pop	bx ; *
	xchg	bx, ax	; bx  = interpolated middle (temporary)
	add	ax, bx	; [previous_val] + interpolated middle
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (L)
	stosw		; interpolated sample 1 (R)
	mov	ax, bx
	add	ax, dx	 ;interpolated middle + [next_val]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; interpolated sample 2 (L)
	stosw		; interpolated sample 2 (R)
	retn

interpolating_3_16bit_stereo:
	; 16/11/2023
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; dx = [next_val_r]
	; original-interpolated-interpolated

	xchg	ax, bx
	stosw		; original sample (L)
	xchg	ax, bx
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	push	ax ; *	; [previous_val_r]
	add	bh, 80h
	add	byte [next_val_l+1], 80h
	mov	ax, [next_val_l]
	add	ax, bx	; [previous_val_l]
	rcr	ax, 1
	xchg	ax, bx	; ax = [previous_val_l]	
	add	ax, bx	; bx = interpolated middle (L)
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (L)
	pop	ax  ; *
	add	dh, 80h ; convert sound level 0 to 65535 format
	push	dx  ; * ; [next_val_r]
	xchg	ax, dx
	add	ax, dx	; [next_val_r] + [previous_val_r]
	rcr	ax, 1	; / 2
	push	ax ; ** ; interpolated middle (R)
	add	ax, dx	; + [previous_val_r]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (R)
	mov	ax, [next_val_l]
	add	ax, bx	; + interpolated middle (L)
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (L)
	pop	ax ; **
	pop	dx ; *
	add	ax, dx	; interpolated middle + [next_val_r]
	rcr	ax, 1	; / 2
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (L)
	retn


interpolating_2_16bit_mono:
	; 16/11/2023
	; ax = [previous_val]
	; dx = [next_val]
	; original-interpolated

	stosw		; original sample (L)
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	add	dh, 80h
	add	ax, dx
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; interpolated sample (L)
	stosw		; interpolated sample (R)
	retn

interpolating_2_16bit_stereo:
	; 16/11/2023
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; dx = [next_val_r]
	; original-interpolated

	xchg	ax, bx
	stosw		; original sample (L)
	xchg	ax, bx
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	add	dh, 80h
	add	ax, dx	; [previous_val_r] + [next_val_r]
	rcr	ax, 1	; / 2
	push	ax ; *	; interpolated sample (R)
	mov	ax, [next_val_l]
	add	ah, 80h
	add	bh, 80h
	add	ax, bx	; [next_val_l] + [previous_val_l]
	rcr	ax, 1	; / 2		
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample (L)
	pop	ax ; *	
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample (R)
	retn

interpolating_5_8bit_mono:
	; 17/11/2023
	; al = [previous_val]
	; dl = [next_val]
	; original-interpltd-interpltd-interpltd-interpltd
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	stosw		; original sample (R)
	mov	al, bl
	add	al, dl
	rcr	al, 1
	mov	bh, al	; interpolated middle (temporary)
	add	al, bl  ; [previous_val]
	rcr	al, 1 	
	mov	dh, al	; interpolated 1st quarter (temporary)
	add	al, bl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (L)
	stosw		; interpolated sample 1 (R)
	mov	al, bh
	add	al, dh
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)
	stosw		; interpolated sample 2 (R)
	mov	al, bh
	add	al, dl	; [next_val]
	rcr	al, 1
	mov	dh, al	; interpolated 3rd quarter (temporary)
	add	al, bh
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 3 (L)
	stosw		; interpolated sample 3 (R)
	mov	al, dh
	add	al, dl
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 4 (L)
	stosw		; interpolated sample 4 (R)
	retn

interpolating_5_8bit_stereo:
	; 17/11/2023
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dh = [next_val_r]
	; original-interpltd-interpltd-interpltd-interpltd
	mov	bx, ax
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	mov	al, bh
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)
	push	dx ; *
	mov	al, bl
	add	al, dl	; [next_val_l]
	rcr	al, 1
	push	ax ; **	; al = interpolated middle (L) (temporary)
	add	al, bl	; [previous_val_l]
	rcr	al, 1
	xchg	al, bl
	add	al, bl	; bl = interpolated 1st quarter (L) (temp)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (L)
	mov	al, bh
	add	al, dh	; [next_val_r]
	rcr	al, 1
	push	ax ; *** ; al = interpolated middle (R) (temporary)
	add	al, bh	; [previous_val_r]
	rcr	al, 1
	xchg	al, bh
	add	al, bh	; bh = interpolated 1st quarter (R) (temp)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (R)
	pop	dx ; ***
	pop	ax ; **	; al = interpolated middle (L) (temporary)
	xchg	al, bl	; al = interpolated 1st quarter (L) (temp)
	add	al, bl	; bl = interpolated middle (L) (temporary)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)	
	mov	al, dl 	; interpolated middle (R) (temporary)
	xchg	al, bh	; al = interpolated 1st quarter (R) (temp)
	add	al, bh	; bh = interpolated middle (R) (temporary)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (R)
	pop	dx ; *
	mov	al, bl	; interpolated middle (L) (temporary)
	add	al, dl	; [next_val_l]
	rcr	al, 1
	xchg	al, bl	; al = interpolated middle (R) (temporary)
	add	al, bl	; bl = interpolated 3rd quarter (L) (temp)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 3 (L)
	mov	al, bh	
	add	al, dh	; interpolated middle (R) + [next_val_r]
	rcr	al, 1
	xchg	al, bh	; al = interpolated middle (R)
	add	al, bh	; bh = interpolated 3rd quarter (R) (temp)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 3 (R)
	mov	al, bl
	add	al, dl	; [next_val_l]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 4 (L)
	mov	al, bh
	add	al, dh	; [next_val_r]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 4 (R)
	retn

interpolating_4_8bit_mono:
	; 17/11/2023
	; al = [previous_val]
	; dl = [next_val]
	; original-interpolated-interpolated-interpolated
	mov	bl, al
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	stosw		; original sample (R)
	mov	al, bl
	add	al, dl
	rcr	al, 1
	xchg	al, bl  ; al = [previous_val]
	add	al, bl	; bl = interpolated middle (sample 2)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (L)
	stosw		; interpolated sample 1 (R)
	mov	al, bl	; interpolated middle (sample 2)
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)
	stosw		; interpolated sample 2 (R)
	mov	al, bl
	add	al, dl	; [next_val]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 3 (L)
	stosw		; interpolated sample 3 (R)
	retn

interpolating_4_8bit_stereo:
	; 17/11/2023
	; al = [previous_val_l]
	; ah = [previous_val_r]
	; dl = [next_val_l]
	; dh = [next_val_r]
	; original-interpolated-interpolated-interpolated
	mov	bx, ax
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (L)
	mov	al, bh
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; original sample (R)
	mov	al, bl
	add	al, dl	; [next_val_l]
	rcr	al, 1
	xchg	al, bl	; al = [previous_val_l]
	add	al, bl	; bl = interpolated middle (L) (sample 2)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (L)
	mov	al, bh
	add	al, dh	; [next_val_r]
	rcr	al, 1
	xchg	al, bh	; al = [previous_val_h]
	add	al, bh	; bh = interpolated middle (R) (sample 2)
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 1 (R)
	mov	al, bl	; interpolated middle (L) (sample 2)
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)
	mov	al, bh	; interpolated middle (L) (sample 2)
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 2 (L)
	mov	al, bl
	add	al, dl	; [next_val_l]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 3 (L)
	mov	al, bh
	add	al, dh	; [next_val_r]
	rcr	al, 1
	sub	al, 80h
	shl	ax, 8	; convert 8 bit sample to 16 bit sample
	stosw		; interpolated sample 3 (R)
	retn

interpolating_5_16bit_mono:
	; 18/11/2023
	; ax = [previous_val]
	; dx = [next_val]
	; original-interpltd-interpltd-interpltd-interpltd
	stosw		; original sample (L)
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	mov	bx, ax	; [previous_val]
	add	dh, 80h
	add	ax, dx
	rcr	ax, 1
	push	ax ; *	; interpolated middle (temporary)
	add	ax, bx	; interpolated middle + [previous_val]
	rcr	ax, 1
	push	ax ; **	; interpolated 1st quarter (temporary)
	add	ax, bx	; 1st quarter + [previous_val]
	rcr	ax, 1	
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (L)
	stosw		; interpolated sample 1 (R)
	pop	ax ; **	
	pop	bx ; *
	add	ax, bx	; 1st quarter + middle
	rcr	ax, 1	; / 2
	sub	ah, 80h	; -32768 to +32767 format again	
	stosw		; interpolated sample 2 (L)
	stosw		; interpolated sample 2 (R)
	mov	ax, bx
	add	ax, dx	; interpolated middle + [next_val]
	rcr	ax, 1
	push	ax ; *	; interpolated 3rd quarter (temporary)
	add	ax, bx	; + interpolated middle
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; interpolated sample 3 (L)
	stosw		; interpolated sample 3 (R)
	pop	ax ; *	
	add	ax, dx	; 3rd quarter + [next_val]
	rcr	ax, 1	; / 2
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; interpolated sample 4 (L)
	stosw		; interpolated sample 4 (R)
	retn

interpolating_5_16bit_stereo:
	; 18/11/2023
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; [next_val_r]
	; original-interpltd-interpltd-interpltd-interpltd
	push	cx ; !
	xchg	ax, bx
	stosw		; original sample (L)
	xchg	ax, bx
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	push	ax ; *	; [previous_val_r]
	add	bh, 80h
	add	byte [next_val_l+1], 80h
	mov	ax, [next_val_l]
	add	ax, bx	; [previous_val_l]
	rcr	ax, 1
	mov	cx, ax	; interpolated middle (L)
	add	ax, bx	
	rcr	ax, 1
	mov	dx, ax	; interpolated 1st quarter (L)
	add	ax, bx	; [previous_val_l]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (L)
	mov	ax, cx
	add	ax, dx	; middle (L) + 1st quarter (L)
	rcr	ax, 1	; / 2
	mov	bx, ax  ; interpolated sample 2 (L)
	pop	dx ; *	; [previous_val_r]
	mov	ax, dx
	add	byte [next_val_r+1], 80h
	add	ax, [next_val_r]
	rcr	ax, 1
	push	ax ; *	; interpolated middle (R)
	add	ax, dx
	rcr	ax, 1
	push	ax ; **	; interpolated 1st quarter (R)
	add	ax, dx	; [previous_val_r]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (R)
	mov	ax, bx
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (L)
	pop	ax ; **
	pop	dx ; *
	add	ax, dx	; 1st quarter (R) + middle (R)
	rcr	ax, 1	; / 2
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (R)
	mov	ax, cx
	add	ax, [next_val_l]
	rcr	ax, 1
	push	ax ; * 	; interpolated 3rd quarter (L)
	add	ax, cx	; interpolated middle (L)
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 3 (L)
	mov	ax, dx
	add	ax, [next_val_r]
	rcr	ax, 1
	push	ax ; ** ; interpolated 3rd quarter (R)
	add	ax, dx	; interpolated middle (R)
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 3 (R)
	pop	bx ; **
	pop	ax ; *
	add	ax, [next_val_l]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 4 (L)
	mov	ax, bx	
	add	ax, [next_val_r]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 4 (R)
	pop	cx ; !
	retn

interpolating_4_16bit_mono:
	; 18/11/2023
	; ax = [previous_val]
	; dx = [next_val]
	; 02/02/2025
	; original-interpolated-interpolated-interpolated

	stosw		; original sample (L)
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	mov	bx, ax	; [previous_val]
	add	dh, 80h
	add	ax, dx	; [previous_val] + [next_val]
	rcr	ax, 1
	xchg	ax, bx	
	add	ax, bx	; [previous_val] + interpolated middle
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (L)
	stosw		; interpolated sample 1 (R)
	mov	ax, bx	; interpolated middle
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (L)
	stosw		; interpolated sample 2 (R)
	mov	ax, bx
	add	ax, dx	; interpolated middle + [next_val]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw		; interpolated sample 3 (L)
	stosw		; interpolated sample 3 (R)
	retn

interpolating_4_16bit_stereo:
	; 18/11/2023
	; bx = [previous_val_l]
	; ax = [previous_val_r]
	; [next_val_l]
	; [next_val_r]
	; original-interpolated-interpolated-interpolated
	xchg	ax, bx
	stosw		; original sample (L)
	xchg	ax, bx
	stosw		; original sample (R)
	add	ah, 80h ; convert sound level 0 to 65535 format
	mov	dx, ax	; [previous_val_r]
	add	bh, 80h
	add	byte [next_val_l+1], 80h
	mov	ax, [next_val_l]
	add	ax, bx	; [previous_val_l]
	rcr	ax, 1
	xchg	ax, bx	
	add	ax, bx	; bx = interpolated middle (L)
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (L)
	add	byte [next_val_r+1], 80h
	mov	ax, dx	; [previous_val_r]
	add	ax, [next_val_r]
	rcr	ax, 1
	xchg	ax, dx	
	add	ax, dx	; dx = interpolated middle (R)
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 1 (R)
	mov	ax, bx
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (L)
	mov	ax, dx
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 2 (R)
	mov	ax, bx
	add	ax, [next_val_l]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 3 (L)
	mov	ax, dx
	add	ax, [next_val_r]
	rcr	ax, 1
	sub	ah, 80h	; -32768 to +32767 format again
	stosw 		; interpolated sample 3 (R)
	retn

; 13/11/2023
previous_val:
previous_val_l: dw 0
previous_val_r: dw 0
next_val:
next_val_l: dw 0
next_val_r: dw 0

; 16/11/2023
faz:	db 0

; --------------------------------------------------------
; 14/11/2024 - Erdogan Tan
; --------------------------------------------------------

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
	call	ac97_pause
	; 21/11/2024
	mov	al, [tLO]
	mov	byte [tLP], al
	jmp	c4ue_cpt
c4ue_chk_ps:
	cmp	byte [stopped], 1
	ja	short c4ue_replay
	; continue to play (after a pause)
	call	ac97_play 
	jmp	c4ue_cpt
c4ue_replay:
	; 19/11/2024
	pop	ax ; *
	pop	ax ; return address
	; 07/02/2024
	;mov	al, [volume]
	;call	SetmasterVolume
	mov	byte [stopped], 0
	call	move_to_beginning
	;jmp	PlayWav
	; 07/12/2024
	jmp	RePlayWav

c4ue_chk_s:
	cmp	al, 'S'	; stop
	jne	short c4ue_chk_fb
	cmp	byte [stopped], 0
	ja	c4ue_cpt ; Already stopped/paused
	call	ac97_stop
	; 19/11/2024
	mov	byte [tLO], 0
	; 21/11/2024
	mov	byte [tLP], '0'
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
	call 	write_ac97_pci_dev_info
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

; --------------------------------------------------------
; 27/12/2024 - Erdogan Tan
; --------------------------------------------------------

	; 01/01/2025 (cgaplay.asm, 16bit registers)
	; 30/12/2024 (cgaplay.s)
	;  * 320*200 pixels, 256 colors
	;  * 64 volume levels
	; 29/12/2024
	; 27/12/2024 (DMA Buffer Tracking)
	; 26/12/2024
	; 24/12/2024
UpdateWavePoints:
	; 01/01/2024
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
	mov	dx, [WAV_BUFFER2]
	jmp	short lights_on
lights_on_buff_1:
	; 01/01/2025
	mov	dx, [WAV_BUFFER1]
lights_on:
	cmp	[pbuf_s], dx
	jne	short lights_on_2
	mov	bx, [wpoints_dif]
	mov	si, [pbuf_o]
	mov	cx, [buffersize] ; samples
	; 01/01/2025
	shl	cx, 1  ; bytes
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
	; 01/01/2025
	push	ds ; **
	;
	;mov	cx, 640
	; 30/12/2024
	mov	cx, 320
	mov	bp, cx
	; 26/12/2024
	mov	di, prev_points
	;mov	bx, [graphstart] ; start (top) line
	; 01/01/2025
	mov	ds, dx
	mov	bx, (11*8*320)+(4*320)
lights_on_4:
	; 01/01/2025
	xor	eax, eax ; 0
	lodsw	; left
	add	ah, 80h
	mov	edx, eax
	lodsw	; right
	;add	ax, dx
	add	ah, 80h
	;;shr	eax, 9	; 128 volume levels
	add	eax, edx
	;;shr	eax, 10	; (L+R/2) & 128 volume levels
	;shr	eax, 9	; (L+R/2) & 256 volume levels
	; 30/12/2024
	shr	eax, 11	; (L+R/2) & 64 volume levels
	; * 320 row  ; 30/12/2024
	mul	bp	; * 640 (row) 
	add	ax, bx ; + column
	;mov	dl, [ccolor]
	; 01/01/2025
	mov	dl, [cs:ccolor]
	xchg	ax, bx
	mov	[es:bx], dl ; pixel (light on) color
	xchg	ax, bx
	mov	[cs:di], ax ; save light on addr in prev_points
	inc	di
	inc	di
	inc	bx
	loop	lights_on_4
	pop	ds ; **
	pop	es ; *
	retn

; --------------------------------------------------------
; 19/05/2024 - (playwav4.asm) ich_wav4.asm
; --------------------------------------------------------

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
	cmp	al, 0
	jna	short p_3
	dec	al
	jmp	short p_2
p_1:
	cmp	al, '-'
	jne	short p_4

	mov	al, [volume]
	cmp	al, 31
	jnb	short p_3
	inc	al
p_2:
	mov	[volume], al
	; 14/11/2024
	call	SetPCMOutVolume
	; 15/11/2024 (QEMU)
	;call	SetMasterVolume
	;call	UpdateVolume
	;;clc
	;retn
	jmp	UpdateVolume
	;mov	ah, al
	;mov    dx, [NAMBAR]
  	;;add   dx, CODEC_MASTER_VOL_REG
	;add	dx, CODEC_PCM_OUT_REG
	;out    dx, ax
	;
	;call   delay1_4ms
        ;call   delay1_4ms
        ;call   delay1_4ms
        ;call   delay1_4ms
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
	db	03h

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

	; 01/01/2025
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

	mov	bl, 31
	div	bl

	neg	ax
	add	ax, 100

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
; ac97.inc (11/11/2023)
; -------------------------------------------------------------

; special characters
LF      EQU 10
CR      EQU 13

; PCI stuff

BIT0  EQU 1
BIT1  EQU 2
BIT2  EQU 4
BIT8  EQU 100h
BIT9  EQU 200h
BIT28 EQU 10000000h
BIT30 EQU 40000000h
BIT31 EQU 80000000h

BUP		equ	BIT30		; Buffer Underrun Policy.
					; if this buffer is the last buffer
					; in a playback, fill the remaining
					; samples with 0 (silence) or not.
					; It's a good idea to set this to 1
					; for the last buffer in playback,
					; otherwise you're likely to get a lot
					; of noise at the end of the sound.

RR		equ	BIT1		; reset registers. Nukes all regs
                                        ; except bits 4:2 of this register.
                                        ; Only set this bit if BIT 0 is 0
RPBM		equ	BIT0		; Run/Pause
					; set this bit to start the codec!
IO_ENA		EQU	BIT0		; i/o decode enable
BM_ENA		EQU	BIT2		; bus master enable

PCI_INDEX_PORT  EQU     0CF8h
PCI_DATA_PORT   EQU     0CFCh
PCI32           EQU     BIT31           ; bitflag to signal 32bit access
PCI16           EQU     BIT30           ; bitflag for 16bit access

AC97_INT_LINE	equ	3Ch		; AC97 Interrupt Line register offset

; Intel ICH2 equates. It is assumed that ICH0 and plain ole ICH are compatible.

INTEL_VID       equ     8086h           ; Intel's PCI vendor ID
; 03/11/2023 - Erdogan Tan (Ref: MenuetOS AC97 WAV Player source code, 2004)
SIS_VID		equ	1039h
NVIDIA_VID	equ	10DEh	 ; Ref: MPXPLAY/SBEMU/KOLIBRIOS AC97 source c.
AMD_VID		equ	1022h

ICH_DID         equ     2415h           ; ICH device ID
ICH0_DID        equ     2425h           ; ICH0
ICH2_DID        equ     2445h           ; ICH2 I think there are more ICHes.
                                        ; they all should be compatible.

; 17/02/2017 (Erdogan Tan, ref: ALSA Device IDs, ALSA project)
ICH3_DID	equ     2485h           ; ICH3
ICH4_DID        equ     24C5h           ; ICH4
ICH5_DID	equ     24D5h           ; ICH5
ICH6_DID	equ     266Eh           ; ICH6
ESB6300_DID	equ     25A6h           ; 6300ESB
ESB631X_DID	equ     2698h           ; 631XESB
ICH7_DID	equ	27DEh		; ICH7
; 03/11/2023 - Erdogan Tan (Ref: MenuetOS AC97 WAV Player source code, 2004)
MX82440_DID	equ	7195h
SI7012_DID	equ	7012h
NFORCE_DID	equ	01B1h
NFORCE2_DID	equ	006Ah
AMD8111_DID	equ	746Dh
AMD768_DID	equ	7445h
; 03/11/2023 - Erdogan Tan - Ref: MPXPLAY/SBEMU/KOLIBRIOS AC97 source code
CK804_DID	equ	0059h
MCP04_DID	equ	003Ah
CK8_DID		equ	008Ah
NFORCE3_DID	equ	00DAh
CK8S_DID	equ	00EAh

NAMBAR_REG	equ	10h		; native audio mixer BAR
NABMBAR_REG	equ	14h		; native audio bus mastering BAR

CODEC_MASTER_VOL_REG	equ	02h	; master volume
CODEC_MASTER_TONE_REG	equ	08h	; master tone (R+L)
CODEC_PCM_OUT_REG 	equ	18h     ; PCM output volume
CODEC_EXT_AUDIO_REG	equ	28h	; extended audio
CODEC_EXT_AUDIO_CTRL_REG equ	2Ah	; extended audio control
CODEC_PCM_FRONT_DACRATE_REG equ	2Ch	; PCM out sample rate

; ICH supports 3 different types of register sets for three types of things
; it can do, thus:
;
; PCM in (for recording) aka PI
; PCM out (for playback) aka PO
; MIC in (for recording) aka MC

PI_BDBAR_REG	equ	0		; PCM in buffer descriptor BAR
PO_BDBAR_REG	equ	10h		; PCM out buffer descriptor BAR

GLOB_CNT_REG	equ	2Ch		; Global control register
GLOB_STS_REG 	equ	30h		; Global Status register (RO)

PI_CR_REG 	equ	0Bh		; PCM in Control Register
PO_CR_REG	equ	1Bh		; PCM out Control Register
MC_CR_REG	equ	2Bh		; MIC in Control Register

PCI_CMD_REG	EQU	04h		; reg 04h, command register

CTRL_ST_CREADY		equ	BIT8+BIT9+BIT28 ; Primary Codec Ready
CODEC_REG_POWERDOWN	equ	26h

PO_CIV_REG	equ	14h		; PCM out current Index value (RO)
PO_LVI_REG	equ	15h		; PCM out Last Valid Index
PO_SR_REG	equ	16h		; PCM out Status register

BDL_SIZE	equ	32*8		; Buffer Descriptor List size

; -------------------------------------------------------------

; 22/12/2024
align 4

; 13/11/2024
; ('<<' to 'shl' conversion for FASM)
;
; 29/05/2024 (TRDOS 386)
; 17/02/2017
; Valid ICH device IDs

valid_ids:
	;dd (ICH_DID shl 16) + INTEL_VID	; 8086h:2415h
	dd (ICH_DID << 16) + INTEL_VID		; 8086h:2415h
	dd (ICH0_DID << 16) + INTEL_VID		; 8086h:2425h
	dd (ICH2_DID << 16) + INTEL_VID		; 8086h:2445h
	dd (ICH3_DID << 16) + INTEL_VID		; 8086h:2485h
	dd (ICH4_DID << 16) + INTEL_VID		; 8086h:24C5h
	dd (ICH5_DID << 16) + INTEL_VID		; 8086h:24D5h
	dd (ICH6_DID << 16) + INTEL_VID		; 8086h:266Eh
	dd (ESB6300_DID << 16) + INTEL_VID	; 8086h:25A6h
	dd (ESB631X_DID << 16) + INTEL_VID	; 8086h:2698h
	dd (ICH7_DID << 16) + INTEL_VID		; 8086h:27DEh
	; 03/11/2023 - Erdogan Tan
	dd (MX82440_DID << 16) + INTEL_VID	; 8086h:7195h
	dd (SI7012_DID << 16)  + SIS_VID	; 1039h:7012h
	dd (NFORCE_DID << 16)  + NVIDIA_VID	; 10DEh:01B1h
	dd (NFORCE2_DID << 16) + NVIDIA_VID	; 10DEh:006Ah
	dd (AMD8111_DID << 16) + AMD_VID	; 1022h:746Dh
	dd (AMD768_DID << 16)  + AMD_VID	; 1022h:7445h
	dd (CK804_DID << 16) + NVIDIA_VID	; 10DEh:0059h
	dd (MCP04_DID << 16) + NVIDIA_VID	; 10DEh:003Ah
	dd (CK8_DID << 16) + NVIDIA_VID		; 1022h:008Ah
	dd (NFORCE3_DID << 16) + NVIDIA_VID	; 10DEh:00DAh
	dd (CK8S_DID << 16) + NVIDIA_VID	; 10DEh:00EAh

valid_id_count equ (($ - valid_ids)>>2)	; 05/11/2023
; 13/11/2024
;valid_id_count = ($ - valid_ids) shr 2	; 05/11/2023

	dd 0

; 01/01/2025

Credits:
	db 'VGA WAV Player for Retro DOS by Erdogan Tan. '
	;db 'January 2025.',10,13,0
	;db '01/01/2025', 10,13
	db 'February 2025.',10,13,0
	db '04/02/2025', 10,13
; 15/11/2024
reset:
	db 0

msgAudioCardInfo:
	db 'for Intel AC97 (ICH) Audio Controller.', 10,13,0

	; 01/01/2025
msg_usage:
	db 'usage: CGAPLAY <FileName1> <FileName2> <...>',10,13,0

noDevMsg:
	db 'Error: Unable to find AC97 audio device!'
	db 10,13,0

noFileErrMsg:
	db 'Error: file not found.',10,13,0

; 29/05/2024
; 11/11/2023
msg_init_err:
	db CR, LF
	db 'AC97 Controller/Codec initialization error !'
	db CR, LF, 0 ; 07/12/2024

; 25/11/2023
msg_no_vra:
	db 10,13
	db 'No VRA support ! Only 48 kHZ sample rate supported !'
	db 10,13,0

; 19/11/2024
; 03/06/2017
hex_chars:
	db '0123456789ABCDEF', 0
msgAC97Info:
	db 0Dh, 0Ah
	db ' AC97 Audio Controller & Codec Info', 0Dh, 0Ah 
	db ' Vendor ID: '
msgVendorId:
	db '0000h Device ID: '
msgDevId:
	db '0000h', 0Dh, 0Ah
	db ' Bus: '
msgBusNo:
	db '00h Device: '
msgDevNo:
	db '00h Function: '
msgFncNo:
	db '00h'
	db 0Dh, 0Ah
	db ' NAMBAR: '
msgNamBar:
	db '0000h  '
	db 'NABMBAR: '
msgNabmBar:
	db '0000h  IRQ: '
msgIRQ:
	dw 3030h
	db 0Dh, 0Ah, 0
; 25/11/2023
msgVRAheader:
	db ' VRA support: '
	db 0	
msgVRAyes:
	db 'YES', 0Dh, 0Ah, 0
msgVRAno:
	db 'NO ', 0Dh, 0Ah
	;db ' (Interpolated sample rate playing method)'
	; 30/12/2024
	db ' (Interpolated samplerate play method)'
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

EOF: 

; -------------------------------------------------------------

bss:

ABSOLUTE bss

; 01/01/2025 (16bit modifications)

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

; 18/11/2024
stopped:
	resb 1
tLO:	resb 1
; 21/11/2024
tLP:	resb 1
; 30/12/2024
wpoints:
	resb 1
pbuf_o:	resw 1
; 29/12/2024
pbuf_s:	resw 1

; 30/05/2024
VRA:	resb 1	; Variable Rate Audio Support Status

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

; 12/11/2016 - Erdogan Tan
bus_dev_fn:
	resd 1
dev_vendor:
	resd 1

; 17/02/2017
; NAMBAR:  Native Audio Mixer Base Address Register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 10h-13h
; NABMBAR: Native Audio Bus Mastering Base Address register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 14h-17h
NAMBAR:	resw 1	; BAR for mixer
NABMBAR:
	resw 1	; BAR for bus master regs

; 01/01/2024
; 256 byte buffer for descriptor list
BDL_BUFFER:
	resw 1	; segment of our 256byte BDL buffer
WAV_BUFFER1:
	resw 1	; segment of our WAV storage
; 64k buffers for wav file storage
WAV_BUFFER2:
	resw 1	; segment of 2nd wav buffer

; 01/01/2025
; 15/11/2024
loadfromwavfile:
	resw 1	; 'loadfromfile' or load+conversion proc address
loadsize:
	resw 1	; (.wav file) read count (bytes) per one time
buffersize:
	resd 1	; 16 bit samples (not bytes)
		
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
; 14/11/2024
bss_end:

; 01/01/2025
; 17/11/2024
temp_buffer:
	resb 50600  ; (44.1 kHZ stereo 12650 samples)
