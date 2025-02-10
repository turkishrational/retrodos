; ****************************************************************************
; twavplay.asm (for Retro DOS)
; ----------------------------------------------------------------------------
; TWAVPLAY.COM ! AC'97 (ICH) WAV PLAYER & VGA DEMO program by Erdogan TAN
;
; 06/02/2025
;
; [ Last Modification: 10/02/2025 ]
;
; Assembler: NASM 2.15
; ----------------------------------------------------------------------------
;	   nasm  twavplay.asm -l twavplay.lst -o TWAVPLAY.COM
; ****************************************************************************

%macro	sys_msg	2
	mov	si, %1	; message
	mov	bl, %2	; text color
	xor	bh, bh	; video page 0
	mov	ah, 0Eh
	call	p_msg	
%endmacro

%macro SbOut 1
%%wait:
	in	al, dx
	or	al, al
	js	short %%wait
	mov	al, %1	; command
	out	dx, al
%endmacro

; ----------------------------------------------------------------------------

;BUFFERSIZE equ 65520 ; AC97
; 07/02/2025
;BUFFERSIZE equ 33680 ; AC97
; 08/02/2025	
;BUFFERSIZE equ 10548 ; AC97 ; 48kHZ 16bit stereo audio block (18.2 block/s) 

ENDOFFILE equ 1	; flag for knowing end of file

;LOADSIZE equ 16384 ; SB16
;dma_buffer_size equ 32768  ; SB16
; 08/02/2025
;LOADSIZE equ 10560 ; SB16 ; 48kHZ 16bit stereo audio block (18.2 block/s)

; ----------------------------------------------------------------------------
; Reference:
; ----------
; Tiny Player v2.11 by Carlos Hasan.
;	June, 1994.

;=============================================================================
;	code
;=============================================================================

[BITS 16] ; 16-bit intructions

[ORG 100h]

	; 06/02/2025
Start:
	; Prints the Credits Text.
	sys_msg Credits, 0Bh

	; deallocate unused DOS memory
	call    setFree

	; Clear BSS (uninitialized data) area
	xor	ax, ax ; 0
	mov	cx, (bss_end - bss_start)/2
	mov	di, bss_start
	rep	stosw

	; Detect (& Enable) AC'97 or SB16 Audio Device
	call	detect_audio_device
	jnc     short GetFileName

_dev_not_ready:
	; couldn't find the audio device!
	sys_msg noDevMsg, 0Fh
        jmp     Exit

; ----------------------------------------------------------------------------

GetFileName:
	mov	si, 81h
	lodsb
	cmp	al, 0Dh	; CR : No command line parameters
	ja	short _x
	jmp	pmsg_usage

_x:
	mov	di, wav_file_name
	xor	cx, cx ; 0
	jmp	short _y

ScanName:       
	lodsb

	cmp	al, 0Dh	; CR
	jna	short a_4
_y:
	cmp	al, 20h
	je	short ScanName	; scan start of name.
	stosb
	mov	ah, 0FFh
a_0:	
	inc	ah
a_1:
	inc	cx
	lodsb
	stosb
	cmp	al, '.'
	je	short a_0
	cmp	al, 20h
	jna	short a_3
	and	ah, ah
	jz	short a_2

	cmp	al, '\'
	jne	short a_2
	mov	ah, 0
a_2:
	cmp	cl, 75	; 64+8+'.'+3 -> offset 75 is the last chr
	jb	short a_1
	jmp	short a_4
a_3:
	dec	di
	or	ah, ah		; if period NOT found,
	jnz	short a_4 	; then add a .WAV extension.
SetExt:
	mov	dword [di], '.WAV' ; ! 64+12 is DOS limit
				   ;   but writing +4 must not
				   ;   destroy the following data
				; so, 80 bytes path + 0 is possible here
	add	di, 4
a_4:	
	mov	byte [di], 0
	
	cmp	byte [wav_file_name], 20h
	ja	short open_wav_file

; ----------------------------------------------------------------------------

pmsg_usage: 
	sys_msg msg_usage, 0Fh

Exit:
	mov	ax, 4C00h	; bye !
	int	21h
halt:
	jmp	short halt

; ----------------------------------------------------------------------------

open_wav_file:
        ; open existing file
	mov	dx, wav_file_name
        call    openFile ; no error? ok.
        jnc     short _z

	; file not found!
	sys_msg	noFileErrMsg, 0Fh

	jmp	short Exit

not_valid_wav:
	; not a proper/valid wav file !
	sys_msg	not_valid_wavf, 0Fh

	jmp	short Exit

_z:
       	call    getWAVParameters
	jc	short not_valid_wav

	mov	bl, 4
	mov	cl, [WAVE_BlockAlign]
	sub	bl, cl	; = 0 for 16 bit stereo
			; = 2 for 8 bit stereo or 16 bit mono
			; = 3 for 8 bit mono

	shr	bl, 1	; 0 --> 0, 2 --> 1, 3 --> 1
	adc	bl, 0	; 3 --> 1 --> 2
	mov	[fbs_shift], bl	; = 2 mono and 8 bit
				; = 0 stereo and 16 bit
				; = 1 mono or 8 bit

	; 07/02/2025
	cmp	byte [audio_hardware], 1 ; SB16 ?
	;jne	short write_info
	; 08/02/2025
	;jne	short audio_hardware_init
	jne	short _r
				; no, skip [g_samples] calculation

	; count of audio samples for graphics data
	mov	ax, 256
	shr	cl, 1
	; 0 = 8 bit mono, 1 = 16 bit mono or 8 bit stereo
	; 2 = 16 bit stereo  
	shl	ax, cl
	mov	[g_samples], ax ; 256 .. 1024

	; 08/02/2025
_r:
	; calculate 18.2 block/s buffer size for proper wave scope
	mov	ax, [WAVE_SampleRate]
	mov	dx, 4*10
	mul	dx
	mov	cx, 182
	div	cx
	mov	cl, bl	; 0 = stereo & 16bit
			; 1 = mono 16bit or stereo 8bit
			; 2 = mono & 8bit
	and	al, ~3 ; NOT 3
	shr	ax, cl
		; AX = 
		; 10548 bytes for 48kHZ 16bit stereo
		; 9692 bytes for 44kHZ 16bit stereo
		; 7032 bytes for 32kHZ 16bit stereo
		; 5272 bytes for 24kHz 16bit stereo
		; 4844 bytes for 22kHZ 16bit stereo 
		; 3516 bytes for 16kHZ 16bit stereo
		; 2636 bytes for 12kHZ 16bit stereo
		; 2420 bytes for 11kHZ 16bit stereo
		; 1756 bytes for 8kHZ 16bit stereo

	mov	[loadsize], ax
	
	cmp	byte [audio_hardware], 1 ; SB16 ?
	je	short _t		; yes	

	; AC97 codec plays 16 bit stereo PCM data only
	shl	ax, cl
	; count of 16 bit samples
	shr	ax, 1
_t:
	mov	[buffersize], ax ; (if audio hardware supports vra)	
		 
; ----------------------------------------------------------------------------

	; 08/02/2025

audio_hardware_init:

	call	audio_system_init
	jc	short Exit

write_info:

	call	write_audio_dev_info

	call	write_wav_file_info

	; 07/02/2025

	sys_msg	msgPressAKey, 07h

	xor	ah, ah
	int	16h

	cmp	al, 1Bh ; ESC
	;jne	short _continue
	;jmp	short Exit
	; 08/02/2025
	;je	short Exit
	; 08/02/2025 (*)
	jne	short _continue
	jmp	Exit

_continue:
	;call	audio_system_init
	;jc	short Exit

; ----------------------------------------------------------------------------

PlayNow: 
	mov	cx, 256
	xor	bx, bx
	mov	di, RowOfs
MakeOfs:
	mov	ax, bx
	shl	ax, 7 ; * 128
	mov	al, 80
	mul	ah
	stosw
	inc	bx
	loop	MakeOfs

; ----------------------------------------------------------------------------

	;;;;
setgraphmode:
	; set VGA 640x480x16 graphics mode
        mov     ax, 12h
        int     10h
        mov     dx, 3C0h
        xor     al, al
setgraphmodel0:
        out     dx, al
        out     dx, al
        inc     al
        cmp     al, 10h
        jb	short setgraphmodel0
        mov     al, 20h
        out     dx, al
	;;;;

; ----------------------------------------------------------------------------
	
	;mov	si, LOGO_ADDRESS
	call	putlbm
;	; 07/02/2025
;	jnc	short loadlbm_ok
;
;loadlbm_err:
;	call	settextmode
;	sys_msg	LOGO_ERROR_MSG, 0Ch
;	jmp	Exit
;
;LOGO_ERROR_MSG:
;	db "Error loading the IFF/ILBM logo picture !", 0Dh, 0Ah, 0
;
;loadlbm_ok:

; ----------------------------------------------------------------------------
	
	; 08/02/2025
	cmp	byte [audio_hardware], 1
	jne	short skip_sdc
	
	; 07/02/2025
	; parepare g_buffer wave graphics parameters

	mov	bx, sdc_16bit_stereo

	mov	al, [WAVE_BlockAlign]
	cmp	al, 4
	je	short set_sdc_p_ok
	mov	bx, sdc_8bit_mono
	cmp	al, 1
	je	short set_sdc_p_ok
	mov	bx, sdc_8bit_stereo
	cmp	byte [WAVE_BitsPerSample], 8
	je	short set_sdc_p_ok
	mov	bx, sdc_16bit_mono
set_sdc_p_ok:
	mov	[sound_data_copy], bx

skip_sdc:

; ----------------------------------------------------------------------------

	; play the .wav file.

	call	PlayWav

; ----------------------------------------------------------------------------

	; close the .wav file and exit.
	call	closeFile

; ----------------------------------------------------------------------------

	; 07/02/2025
	cmp	byte [audio_hardware], 1
	jne	short terminate

	; restore old interrupt vector
	mov	al, [audio_intr] ; 5 or 7
	xor	ah, ah ; reset
	call	set_hardware_int_vector

; ----------------------------------------------------------------------------

terminate:
	call	settextmode
	
	jmp	Exit

; ----------------------------------------------------------------------------

	; 06/02/2025
	; INPUT: ds:dx = file name address
	; OUTPUT: [FileHandle]
openFile:
	mov	ax, 3D00h	; open File for read
	int	21h
	jnc	short _of1
	; cf = 1 -> not found or access error
	retn
_of1:
	mov	[FileHandle], ax
	retn

; ----------------------------------------------------------------------------

	; 06/02/2025
	; INPUT: [FileHandle]
	; OUTPUT: none
closeFile:
	mov	bx, [FileHandle]
	mov	ax, 3E00h
	int	21h	; close file
	retn

; ----------------------------------------------------------------------------

	; 06/02/2025
getWAVParameters:
        mov     dx, WAVFILEHEADERbuff
	mov	bx, [FileHandle]
        mov     cx, 44	; 44 bytes
	mov	ah, 3Fh
        int     21h
	jc	short gwavp_retn

	cmp	ax, 44
	jb	short gwavp_retn

	cmp	dword [RIFF_Format], 'WAVE'
	jne	short gwavp_stc_retn

	cmp	word [WAVE_AudioFormat], 1 ; Offset 20, must be 1 (= PCM)
	jne	short gwavp_stc_retn

	; (OpenMPT creates wav files with a new type header,
	;  this program can not use the new type
	;  because of 'data' offset is not at DATA_SubchunkID.)
	; ((GoldWave creates common type wav file.))

	cmp	dword [DATA_SubchunkID], 'data'
	je	short gwavp_retn

gwavp_stc_retn:
	stc
gwavp_retn:
	retn

;=============================================================================
;
;=============================================================================

	; 08/02/2025 (*) (Wave scope/graphics synchronization)
	; 06/02/2025
PlayWav:
	cmp	byte [audio_hardware], 1
	ja	short playwav_ac97

playwav_sb16:
	cmp	byte [stopped], 1
	; 07/02/2025
	jb	short playwav_sb16_@

	; replay
	mov	byte [stopped], 0
	mov	byte [half_buffer], 1

	jmp	short playwav_sb16_@@

playwav_sb16_@:
	; set audio interrupt vector (to user's handler)
	mov	al, [IRQnum]
	mov	ah, 1 ; set
	mov	dx, IRQ_service
	call	set_hardware_int_vector

playwav_sb16_@@:
	;mov	ax, wav_buffer1
	; 08/02/2025 (*)
	mov	ax, dma_buffer
	call	SB16_LoadFromFile

	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	;mov	ax, wav_buffer2
	; 08/02/2025 (*)
	mov	ax, dma_buffer
	add	ax, [loadsize] ; = add ax, [buffersize]
	call	SB16_LoadFromFile

	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	call	sb16_init_play

	mov	byte [IRQnum], 0
	jmp	SB16_TuneLoop

playwav_ac97:
	; 08/02/2025
	cmp	byte [stopped], 1
	jb	short playwav_ac97_@

	mov	byte [stopped], 0

	call	ac97_RePlayWav

	jmp	short AC97_TuneLoop

playwav_ac97_@:
	call	ac97_play_setup

	call	ac97_init_play

	;jmp	short AC97_TuneLoop

; ----------------------------------------------------------------------------

	; 08/02/2025
	; 06/02/2025
AC97_TuneLoop:
	; 04/02/2025 (cgaplay.asm)

;tuneLoop:
tLWait:
	cmp	byte [stopped], 0
	jna	short tL1 
tLWait@:
	cmp	byte [stopped], 3
	jnb	short tL0

	call	checkUpdateEvents
	jnc	short tLWait
tL0:
	jmp	_exitt_
tL1:
	call	updateLVI	; /set LVI != CIV/
	jz	short tL0

	call	checkUpdateEvents
	jc	short tL0

	cmp	byte [stopped], 0
	ja	short tLWait@

	call	getCurrentIndex
	test	al, BIT0
	jz	short tL1	; loop if buffer 2 is not playing

	; load buffer 1
	mov     ax, [WAV_BUFFER_1]
	call	word [loadfromwavfile]
	jnc	short tL2

	; end of file
_exitt_:
	; Stop Playing
	call	ac97_stop

	retn

tL2:
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0
tL3:
	call    updateLVI
	jz	short _exitt_

	call	checkUpdateEvents
	jc	short _exitt_

	cmp	byte [stopped], 0
	ja	short tLWait@

	call    getCurrentIndex
	test	al, BIT0
	;jnz	short tL2	; loop if buffer 1 is not playing
	; 08/02/2025
	jnz	short tL3

	; load buffer 2
	mov     ax, [WAV_BUFFER_2]
	call	word [loadfromwavfile]
	jc	short _exitt_

	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	jmp	short tLWait

; ----------------------------------------------------------------------------

	; 08/02/2025 (*)
	; 06/02/2025
SB16_TuneLoop:
	; 04/02/2025 (cgaplay2.asm)
;TuneLoop:
.tLWait:
	cmp	byte [stopped], 0
	jna	short .tL2
.tL1:
	call	checkUpdateEvents
	jnc	short .tLWait
._exit_:
	call	sb16_stop

	retn
.tL2:
	; Check SB 16 interrupt status
	cmp	byte [IRQnum], 0
	jna	short .tL1

	xor	byte [half_buffer], 1

	mov	byte [IRQnum], 0

	; load buffer 1
	mov     ax, dma_buffer  ; wav_buffer1
	cmp	byte [half_buffer], 0
	jna	short .tL3

	; load buffer 2
	;add	ax, LOADSIZE	; ax = wav_buffer2
	; 08/02/2025 (*)
	add	ax, [loadsize]
.tL3:
	call	SB16_LoadFromFile
	jc	short ._exit_	; end of file

	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	jmp	short .tL1

;=============================================================================
;
;=============================================================================

c4ue_ok:
	retn

	; 06/02/2025
checkUpdateEvents:
	call	check4keyboardstop
	jc	short c4ue_ok

	push	ax ; *
	or	ax, ax
	jz	short c4ue_cpt

	cmp	al, 20h ; SPACE (spacebar) ; pause/play
	jne	short c4ue_chk_s
	cmp	byte [stopped], 0
	ja	short c4ue_chk_ps

	call	audio_pause

	jmp	short c4ue_cpt

c4ue_chk_ps:
	cmp	byte [stopped], 1
	ja	short c4ue_replay

	; continue to play (after a pause)
	call	audio_play

	jmp	short c4ue_cpt

c4ue_replay:
	pop	ax ; *
	pop	ax ; return address

	call	move_to_beginning

	;mov	byte [stopped], 0

	jmp	PlayWav

c4ue_chk_s:
	cmp	al, 'S'	; stop
	jne	short c4ue_chk_fb
	cmp	byte [stopped], 0
	ja	c4ue_cpt ; Already stopped/paused

	call	audio_stop

	jmp	short c4ue_cpt

c4ue_chk_fb:
	cmp	al, 'F'
	jne	short c4ue_chk_b
	call 	move_forward
	jmp	short c4ue_cpt

c4ue_chk_b:
	cmp	al, 'B'
	jne	short c4ue_cpt

	call 	move_backward

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
	pop	cx ; *
	cmp	dx, [timerticks+2]
	jne	short c4ue_utt
	cmp	ax, [timerticks]
	je	short c4ue_skip_utt
c4ue_utt:
	mov	[timerticks], ax
	mov	[timerticks+2], dx
	jmp	short c4ue_cpt_@

c4ue_skip_utt:
	and	cx, cx
	jz	short c4ue_cpt_@
c4ue_vb_ok:
	retn

c4ue_cpt_@:
	cmp	byte [stopped], 0
	ja	short c4ue_vb_ok

	jmp	drawscopes

;=============================================================================
;
;=============================================================================

	; 06/02/2025
check4keyboardstop:
	mov	ah, 1
	int	16h
	;clc
	jz	short _cksr

	xor	ah, ah
	int	16h

	; (change PCM out volume)
	cmp	al, '+'
	jne	short p_1

	inc	byte [volume]
	jmp	short p_2
p_1:
	cmp	al, '-'
	jne	short p_4

	dec	byte [volume]
p_2:
	call	SetPCMOutVolume
_cksr:
	xor	ax, ax
p_3:
	retn
p_4:
	cmp	ah, 01h  ; ESC
    	je	short p_quit
	cmp	al, 03h  ; CTRL+C
	je	short p_quit

	cmp	al, 20h
	je	short p_3

	cmp	al, 0Dh ; CR/ENTER
	je	short p_3

	and	al, 0DFh

	cmp	al, 'Q'
	je	short p_quit

	clc
	retn

p_quit:
	stc
	retn

;-----------------------------------------------------------------------------
;
;-----------------------------------------------------------------------------

	; 06/02/2025
SetPCMOutVolume:
	cmp	byte [audio_hardware], 1
	je	short sb16_set_volume

;-----------------------------------------------------------------------------

ac97_set_volume:
	mov	al, [volume]
	mov	ah, 31
	cmp	al, ah ; 31
	jna	short _ac97sv_@
	mov	al, ah
	mov	[volume], al ; max = 31, min = 0
_ac97sv_@:
	; max = 0, min = 31
	sub	ah, al
	mov	al, ah
	mov	dx, [NAMBAR]
  	;add	dx, CODEC_MASTER_VOL_REG
	add	dx, CODEC_PCM_OUT_REG
	out	dx, ax
	retn

;-----------------------------------------------------------------------------

sb16_set_volume:
	mov	al, [volume]
	mov	ah, 15
	cmp	al, ah ; 15
	jna	short _sb16sv_@
	mov	al, ah
	mov	[volume], al ; max = 15, min = 0
_sb16sv_@:
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

;=============================================================================
; 07/02/2025 - change song (wave file) play position
;=============================================================================

move_backward:
	; 07/02/2025
move_forward:
	;; In order to go backwards 5 seconds:
	;; Update file pointer to the beginning, skip headers
	mov	cl, al ; 'B' or 'F'

	; 04/02/2025 - cgaplay.asm
	; 01/01/2025 (cgaplay.asm, 16bit registers)
	; 01/12/2024 (32bit registers)
move_backward_or_forward:
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
	jne	short move_fw ; cl = 'F'
move_bw:
	sub	bx, ax
	sbb	cx, dx
	jnc	short move_file_pointer
move_to_beginning:
	xor	cx, cx ; 0
	xor	bx, bx ; 0
	jmp	short move_file_pointer
move_fw: 
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
	mov	bx, [FileHandle]
	mov	ax, 4200h
	int	21h

	retn

;=============================================================================
; Wave Data Loading procedure for Sound Blaster 16 (there is not a conversion)
;=============================================================================

	; 08/02/2025 (*)
	; 07/02/2025
SB16_LoadFromFile:
        test    byte [flags], ENDOFFILE	; have we already read the
					; last of the file?
	jz	short sblff_0		; no
	stc
	retn

sblff_0:
	mov	di, ax ; save buffer address

	mov	bx, [FileHandle]

	;mov	cx, LOADSIZE
	; 08/02/2025 (*)
	mov	cx, [loadsize]
	mov	dx, ax ; buffer address

	; load/read file
	; bx = file handle
	; ds = cs
	; ds:dx = buffer
	; cx = read count

       	mov	ah, 3Fh
	int	21h
	jc	short sblff_2 ; error !

	mov	[count], ax

	cmp	ax, cx
	je	short _endLFF

	; di = buffer address
	add	di, ax
sblff_1:
	call    sb_padfill		; blank pad the remainder
        ;clc				; don't exit with CY yet.
        or	byte [flags], ENDOFFILE	; end of file flag
	; 07/02/2025
	;cmp	word [count], 1
_endLFF:
        retn

sblff_2:
	xor	ax, ax
	jmp	short sblff_1

;-----------------------------------------------------------------------------

; entry ds:ax points to last byte in file
; cx = target size
; note: must do byte size fill
; destroys bx, cx

sb_padfill:
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

;=============================================================================
; AC97 procedures - load and convert sound data
;=============================================================================

	; 08/02/2025 (*) (Wave scope/graphics synchronization)
	; 07/02/2025
	; 04/02/2025 - cgaplay.asm

;-----------------------------------------------------------------------------
; 07/12/2024
;-----------------------------------------------------------------------------

; /////
	; 08/02/2025 (*)
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
	mov	bx, [FileHandle]

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
	;mov	bx, [FileHandle] ; 17/11/2024
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

	;;mov	cx, BUFFERSIZE - 1 ; 65535
	; 17/11/2024
	;mov	cx, BUFFERSIZE
	; 08/02/2025 (*)
	mov	cx, [buffersize] ; words
	shl	cx, 1 ; bytes
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

	;mov	bx, [FileHandle] ; 17/11/2024
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
	xor	ax, ax
	jmp	short lff_3

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------
; interpolation procedures
;-----------------------------------------------------------------------------

; 07/02/2025
; 04/02/2025 - cgaplay.asm
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------

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
	mov	bx, [FileHandle]
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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------


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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;=============================================================================
;	Write AC'97 Hadrware Information
;=============================================================================
	
	; 08/02/2025
	; 07/02/2025
	; 04/02/2025 - cgaplay.asm

write_audio_dev_info:
	; 08/02/2025
	xor	bh, bh
	cmp	byte [audio_hardware], 1
	jne	short write_ac97_pci_dev_info

;-----------------------------------------------------------------------------
	
	; 08/02/2025
	; 07/02/2025
	; 04/02/2025 - cgaplay2.asm	
write_sb16_dev_info:
	;xor	bh, bh ; 08/02/2025
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

	; 07/02/2025
	;xor	ax, ax
	;mov	al, [IRQnum]
	; 27/11/2024
	mov	al, [audio_intr]
	;mov	cl, 10
	;div	cl
	;add	ah, 30h
	;mov	[msgIRQ], ah
	; 25/11/2024
	add	al, 30h
	; 07/02/2025
	mov	[msgSB16IRQ], al

	sys_msg msgSB16Info, 07h

	retn

;-----------------------------------------------------------------------------

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
	;xor	bh, bh
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

	;mov	ax, [NABMBAR]
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
	; 07/02/2025
	;and	al, al
	cmp	al, 30h
	jnz	short _w_ac97imsg_
	mov	al, [msgIRQ+1]
	mov	ah, ' '
	mov	[msgIRQ], ax
_w_ac97imsg_:
	sys_msg msgAC97Info, 07h

	; 19/11/2024
        ;retn

;-----------------------------------------------------------------------------

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

;=============================================================================
;	Write WAV File Information
;=============================================================================

	; 07/02/2025

write_wav_file_info:
	sys_msg msgWavFileName, 0Fh
	sys_msg wav_file_name, 0Fh

write_sample_rate:
	mov	ax, [WAVE_SampleRate]
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
	
	sys_msg	msgSampleRate, 0Fh

	mov	bp, msg16Bits
	cmp	byte [WAVE_BitsPerSample], 16
	je	short wsr_1
	mov	bp, msg8Bits
wsr_1:
	sys_msg	bp, 0Fh

	mov	bp, msgMono
	cmp	byte [WAVE_NumChannels], 1
	je	short wsr_2
	mov	bp, msgStereo
wsr_2:
	sys_msg	bp, 0Fh
        retn

;=============================================================================
;	Audio System Initialization
;=============================================================================
	
	; 08/02/2025
	; 06/02/2025
audio_system_init:
	cmp	byte [audio_hardware], 1
	je	short sb16_init

	call	ac97_init
	jnc	short init_ok

	; 08/02/2025
	mov	bp, ac97_init_err_msg

init_error:
	;sys_msg init_err_msg, 0Fh
	sys_msg	bp, 0Fh

	stc
init_ok:
	retn

;=============================================================================
;	Sound Blaster 16 Initialization
;=============================================================================

	; 08/02/2025 (*) (wave graphics/scope synchronization)
	; 08/02/2025
	; 06/02/2025
sb16_init:
	; 04/02/2025 - cgaplay2.asm
	; 25/11/2024
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9, audio.s (06/06/2024)
	;      SbInit_play procedure (06/08/2022, v2.0.5)

	mov	ax, ds
	mov	dx, ax
	shr	dx, 12
	shl	ax, 4
	add	ax, dma_buffer
	adc	dx, 0
	mov	bx, ax	; linear address
	; dx = page number

	;mov	cx, dma_buffer_size ; 32768
	; 08/02/2025 (*)
	mov	cx, [buffersize] ; = [loadsize] for SB16
	shl	cx, 1	; 2*[buffersize] = dma buffer size

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
	;stc

	; 08/02/2025
	mov	bp, sb16_init_err_msg
	jmp	init_error

SbOk:
	retn

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

	; 08/02/2025 (*) (Wave scope/graphics synchronization)
	; 07/02/2025
	; 06/02/2025

sb16_init_play:
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
	;mov	bx, dma_buffer_size/2
	; 08/02/2025 (*)
	mov	bx, [buffersize] ; = [loadsize] for SB16
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

	; 06/02/2025
	mov	byte [volume], 13 ; max = 15, min = 0

	; 07/02/2025
	call	SetPCMOutVolume

	retn

;-----------------------------------------------------------------------------

	; 09/02/2025
	; 06/02/2025
	; 02/01/2025
	; 24/11/2024
	; Ref: TRDOS 386 Kernel v2.0.9 audio.s (06/06/2024)
	;      sb16_stop procedure (06/08/2022, v2.0.5)	
sb16_stop:
	; 09/02/2025
	; 02/01/2025
	;mov	byte [stopped], 2
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

;-----------------------------------------------------------------------------

	; 06/02/2025
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

;-----------------------------------------------------------------------------

	; 06/02/2025
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

;=============================================================================
;	AC'97 Audio System Initialization
;============================================================================

	; 06/02/2025
ac97_init:
	; 04/02/2025 - cgaplay.asm
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

	; 06/02/2025
	mov	byte [volume], 29 ; max = 31, min = 0

        retn

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

	; 08/02/2025 (*)
	; !!!! 18.2 block/second buffer sizing for proper wave scopes !!!!
	; (wave graphics synchronization) 

	; 08/02/2025
ac97_play_setup:

	; 07/02/2025
	; 06/02/2025

;ac97_init_play:
	;;;
	; 04/02/2025
	; 01/01/2025 (cgaplay.asm)
	; 28/11/2024 (ac97play.asm)
	; 25/11/2023
	; 18/11/2023 (ich_wav4.asm)
	; 13/11/2023 (ich_wav3.asm)

	cmp	byte [VRA], 1
	jb	short chk_sample_rate

playwav_48_khz:
	mov	word [loadfromwavfile], loadFromFile

; 08/02/2025
%if 0
	;mov	word [loadsize], 0 ; 65536
	;;;
	; 17/11/2024
	;mov	word [buffersize], 32768
	mov	ax, BUFFERSIZE/2 ; 32760
	; 07/02/2025
	; ax = 16840
	mov	[buffersize], ax	; 16 bit samples
	shl	ax, 1			; bytes
	mov	cl, [fbs_shift]
	shr	ax, cl
	mov	[loadsize], ax ; 16380 or 32760 or 65520
	;;;
	; 07/02/2025
	;jmp	_PlayNow
%endif
	; 08/02/2025
	;retn
	; 09/02/2025
	jmp	allocate_memory

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
	;;mov	ax, 7514  ; (442*17)
	; 07/02/2025
	;mov	ax, 3842  ; (226*17)	
	; 08/02/2025 (*)
	mov	ax, 1207  ; (71*17)		
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
	;;mov	ax, 3757  ; (221*17)
	; 07/02/2025
	;mov	ax, 1921  ; (113*17)
	; 08/02/2025 (*)
	mov	ax, 612  ; (36*17)	
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
	;;;;mov	ax, 15065 ; (655*23)
	; 18/11/2023 ((file size + bss + stack) <= 64KB)
	;;;mov	ax, 14076 ; (612*23)
	; 17/11/2024
	;;mov	ax, 12650 ; (550*23)
	; 07/02/2025
	;mov	ax, 8418  ; (366*23)
	; 08/02/2025 (*)
	mov	ax, 2438  ; (106*23)
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
	;;;mov	ax, 5461
	; 17/11/2024
	;;mov	ax, 5460
	; 07/02/2025
	;mov	ax, 2806
	; 08/02/2025 (*)
	mov	ax, 879
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
	;;mov	ax, 2730
	; 07/02/2025
	;mov	ax, 1403
	; 08/02/2025 (*)
	mov	ax, 440
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
	;;;mov	ax, 8192
	; 17/11/2024
	;;mov	ax, 8190
	; 07/02/2025
	;mov	ax, 4210
	; 08/02/2025 (*)
	mov	ax, 1318
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
	;;;mov	ax, 10922
	; 17/11/2024
	;;mov	ax, 10920
	; 07/02/2025
	;mov	ax, 5613
	; 08/02/2025 (*)
	mov	ax, 1758
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
	;;;mov	ax, 4096
	;;mov	ax, 4095 ; playwav8.asm
	; 07/02/2025
	;mov	ax, 2105
	; 08/02/2025 (*)
	mov	ax, 659
	mov	dx, 4
	mov	cx, 1
	; 02/02/2025
	;jmp	short set_sizes
	;;;;

;-----------------------------------------------------------------------------

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
	mov	[buffersize], ax  ; ** ; 09/02/2025
	mov	[loadfromwavfile], bx

	; 08/02/2025
	;retn

; ----------------------------------------------------------------------------

	; 09/02/2025
	; 08/02/2025

allocate_memory:
	; allocate 256 bytes of data for DCM_OUT Buffer Descriptor List. (BDL)

	;mov	ax, BDL_SIZE / 16
	; 09/02/2025
	mov	bx, BDL_SIZE / 16
	call	memAlloc
	mov	[BDL_BUFFER], ax	; segment

	; allocate 2 buffers, 64k each for now.

	;;mov	ax, BUFFERSIZE / 16	; 64k for .WAV file
	;mov	bx, [buffersize]	; word count
	; 09/02/2025
	mov	bx, ax ; **
	add	bx, 7
	shr	bx, 3	; / 8		; paragraph count
	push	bx
	call	memAlloc
	mov	[WAV_BUFFER_1], ax	; segment

	;;mov	ax, BUFFERSIZE / 16
	;mov	bx, [buffersize]	; word count
	;add	bx, 7
	;shr	bx, 3	; / 8		; paragraph count
	pop	bx
	call	memAlloc
	mov	[WAV_BUFFER_2], ax	; segment

	retn

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

	; 08/02/2025
ac97_init_play:

	; 06/02/2025
_PlayNow:
	; 01/01/2025 (cgaplay.asm)
	; 18/12/2024 (ac97play.asm)
	; 30/12/2024 (cgaplay.s)
	; 29/12/2024 (vgaplay3.s)
	; 02/12/2024 (ac97play.s)

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

	; 09/02/2025
	; 08/02/2025
	push	es
	mov	ax, [BDL_BUFFER]	; get segment # for BDL
	mov	es, ax

	mov	cx, 32 / 2		; make 32 entries in BDL
	xor	di, di
	;mov	di, bdl_buffer
_pw0:
	movzx	eax, word [WAV_BUFFER_1]
	shl	eax, 4			; convert seg:off ->0:offset
	stosd				; store pointer to wavbuffer1

	;mov	eax, BUFFERSIZE / 2	; size of buffer (32K) in (16bit) words
	; 13/11/2023 (ich_wav3.asm) - 18/11/2023 (ich_wav4.asm)
	mov	eax, [buffersize]

	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

	movzx	eax, word [WAV_BUFFER_2]
	shl	eax, 4			; convert seg:off ->0:offset
	stosd				; store pointer to wavbuffer2

	;mov	eax, BUFFERSIZE / 2 ; size of half buffer (32K)
	; 13/11/2023 (ich_wav3.asm) - 18/11/2023 (ich_wav4.asm)
	mov	eax, [buffersize]

	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

	loop	_pw0
	pop	es

	; 18/12/2024
	;mov	word [count], cx ; 0
	; 14/11/2024
	;mov	dword [LoadedDataBytes], 0

	; 08/02/2025
	; 19/11/2024
ac97_RePlayWav:
	; load 64k into buffer 1
        mov     ax, [WAV_BUFFER_1]
        ;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]
	; 14/11/2024
	mov	ax, [count]
	add	[LoadedDataBytes], ax
	adc	word [LoadedDataBytes+2], 0

	; and 64k into buffer 2
	mov     ax, [WAV_BUFFER_2]
       	;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]
	; 14/11/2024
	; 08/02/2025
	xor	eax, eax
	mov	ax, [count]
	;add	[LoadedDataBytes], ax
	;adc	word [LoadedDataBytes+2], 0
	add	[LoadedDataBytes], eax

	; write NABMBAR+10h with offset of buffer descriptor list

	; 09/02/2025
	movzx	eax, word [BDL_BUFFER]
	shl	eax, 4			; convert seg:off to 0:off
	; 08/02/2025	
	;mov	ax, cs
	;shl	eax, 4
	;add	eax, bdl_buffer
	;
	mov	dx, [NABMBAR]
	add	dx, PO_BDBAR_REG	; set pointer to BDL
	out	dx, eax			; write to AC97 controller

	; 19/05/2024
	call	delay1_4ms

	mov	al, 31
	call	setLastValidIndex

	; 19/05/2024
	;call	delay1_4ms

	; 07/02/2025
	;mov	al, [volume]
	call	SetPCMOutVolume

	; 17/02/2017
	mov	dx, [NABMBAR]
        add	dx, PO_CR_REG		; PCM out Control Register
	;mov	al, IOCE + RPBM		; Enable 'Interrupt On Completion' + run
	;				; (LVBI interrupt will not be enabled)
	; 06/11/2023 (TUNELOOP version, without interrupt)
	mov	al, RPBM
	out	dx, al			; Start bus master operation.

	; 19/05/2024
	; 06/11/2023
	;call	delay1_4ms	; 30/05/2024

	retn

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

; 01/01/2025 (cgaplay.asm)
; 29/12/2024 (vgaplay3.s)
; 18/12/2024 (ac97play.s)
;
; 18/11/2024
; Ref: TRDOS 386 v2.0.9, audio.s, Erdogan Tan, 06/06/2024


	; 06/02/2025
audio_stop:
	cmp	byte [audio_hardware], 1
	ja	short ac97_stop
	jmp	sb16_stop

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

	; 06/02/2025
audio_pause:
	cmp	byte [audio_hardware], 1
	ja	short ac97_pause
	jmp	sb16_pause

;-----------------------------------------------------------------------------

ac97_pause:
	mov	byte [stopped], 1 ; paused
	;mov	al, 0
	;jmp	short ac97_po_cmd
	jmp	short ac97_po_cmd@

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

	; 06/02/2025
audio_play:
	cmp	byte [audio_hardware], 1
	ja	short ac97_play
	jmp	sb16_play

;-----------------------------------------------------------------------------

ac97_play: ; continue to play (after pause)
	mov	byte [stopped], 0
	mov	al, RPBM
	jmp	short ac97_po_cmd

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

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

;-----------------------------------------------------------------------------

;input AL = index # to stop on
setLastValidIndex:
	; 08/11/2023
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	;pop	dx
	retn

;=============================================================================
;	Detect (& Enable) Audio Device
;=============================================================================

	; 06/02/2025

detect_audio_device:

	; check for SB16 at first
	call	DetectSB16
	jnc	short detected
	
	call	Get_CPU_Type
	jc	short not_detected
	
	call	DetectAC97
	jc	short not_detected

	inc	byte [audio_hardware] ; 2 = AC'97
detected:
	inc	byte [audio_hardware] ; 1 = SB16
	
not_detected:			      ; 0 = none
	retn

;=============================================================================
;	Detect AC'97 Hardware
;=============================================================================

	; 06/02/2025
	; 04/02/2025 - cgaplay.asm
	; 30/05/2024
DetectAC97:
DetectICH:
	; 22/11/2023
	; 19/11/2023
	; 01/11/2023 - TRDOS 386 Kernel v2.0.7

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

;-----------------------------------------------------------------------------

; 06/02/2025
; 04/02/2025 (cgaplay.asm)
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

; --------------------------------------------------------

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

; --------------------------------------------------------

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

; -------------------------------------------------------------
; ac97.inc (11/11/2023)
; -------------------------------------------------------------

; 06/02/2025
; 04/02/2025 - cgaplay.asm

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

; 07/02/2025
PO_PICB_REG	equ 18h	; PCM Out Position In Current Buffer Register

; -------------------------------------------------------------

; 06/05/2025

; 22/12/2024
align 4

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

	dd 0

;=============================================================================
;	Detect Sound Blaster 16 sound card (or compatible hardware)
;=============================================================================

	; 06/02/2025
	; 04/02/2025 - cgaplay2.asm
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
	;
	; 10/02/2025
	; 16bit DMA mode intr ack is
	; not necessary for initial IRQ scan
	;
	;inc	dx ; 0Fh ; 16bit DMA-mode int ack
	;in	al, dx	; SB 16 acknowledge.
	;
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

;-----------------------------------------------------------------------------

	; 06/02/2025
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
	;inc	dl ; 0Fh ; 16bit DMA-mode int ack
	; 10/02/2025
	inc	dx
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

;-----------------------------------------------------------------------------
; get CPU type - AC'97 audio system will not be checked if CPU is not 80386+
;-----------------------------------------------------------------------------
; Ref: ibmdos7.s - 10/07/2024 (Retro DOS v5.0, PCDOS 7.1 kernel)

	; 07/02/2025
	; 06/02/2025
Get_CPU_Type:
	pushf
	xor	ax, ax			; 0000 into AX
	push	ax			; put it on the stack...
	popf				; ...then shove it into the flags
	pushf				; get it back out of the flags...
	pop	ax			; ...and into ax
	and	ax, 0F000h		; mask off high four bits
	cmp	ax, 0F000h		; was it all 1's?
	je	short cpu_8086		; yes; it's an 8086 or 8088

	mov	ax, 0F000h		; now try to set the high four bits..
	push	ax
	popf
	pushf
	pop	ax			; ...and see what happens
	and	ax, 0F000h		; any high bits set ?
	jz	short cpu_286		; no, it's an 80286
cpu_386:				; yes, it's an 80386
	popf
	clc
	retn
cpu_8086:
cpu_286:
	popf
	stc
	retn

;-----------------------------------------------------------------------------
; settextmode - restore the VGA 80x25x16 text mode
;-----------------------------------------------------------------------------
	
	; 07/02/2025
settextmode:
	mov	ax, 0003h
	int	10h
	retn
	
;=============================================================================
; drawscopes - draw wave/voice sample scopes
;=============================================================================
	
	; 07/02/2025
drawscopes:
	call	get_current_sound_data
	mov	si, g_buffer

	xor     cx, cx
	xor     dx, dx
	xor	di, di
drawscope0:
	lodsw
	xor	ah, 80h
	xor	bh, bh
	mov	bl, ah	; Left Channel
	shl	bx, 1
	mov	ax, [RowOfs+bx]
	mov	[NewScope_L+di], ax
	xor	bh, bh
	lodsw
	xor	ah, 80h
	mov	bl, ah	; Right Channel
	shl	bx, 1
	mov	ax, [RowOfs+bx]
	mov	[NewScope_R+di], ax
	add	di, 2
	inc	cl
	jnz	short drawscope0

	mov	dx, 3C4h
	mov	ax, 0802h
	out	dx, ax

	;mov	dx, 3CEh
	mov	dl, 0CEh
	mov	al, 08h
	out	dx, al

	inc	dx

	xor	si, si
	push	es  ; ***
	mov	bx, 0A000h
	mov	es, bx
	mov	bx, 0645h
	; 0A0645h
drawscopel4:
	mov     al, 80h
drawscopel2:
	push	ax ; **
	push	dx ; *
	out	dx, al

	;mov	cx, 32
	mov	cl, 32
	mov	ax, 0FF00h
drawscopel3:
        mov	di, [OldScope_L+si]
	mov	dx, [NewScope_L+si]
        cmp	di, dx
        je	short drawscopef3
	add	di, bx
	mov	[es:di], al ; L
        mov     di, dx
	add	di, bx
	mov	[es:di], ah ; L
        mov     [OldScope_L+si], dx
drawscopef3:
	mov	di, [OldScope_R+si]
	mov	dx, [NewScope_R+si]
	cmp	di, dx
	je	short drawscopef4
	add	di, bx
	mov	[es:di+38], al ; R
	mov     di, dx
	add	di, bx
	mov	[es:di+38], ah ; R
	mov     [OldScope_R+si], dx
drawscopef4:
	add	si, 2*8
	inc	bx
	loop    drawscopel3

        pop     dx ; *
        pop     ax ; **
	sub	si, 2*256-2
	sub	bx, 32
        shr     al, 1
        jnz	short drawscopel2
	; 07/02/2025
	clc
	pop	es ; ***
        retn

;=============================================================================
; Get Current Sound Data
;=============================================================================
; Reference: TRDOS 386 v2.0.9 Kernel - audio.s file (28/01/2025)

	; 08/02/2025 (*)
	; !!! 18.2 block/second buffer sizing for proper wave scopes !!!
	; (wave graphics synchronization) 

	; 07/02/2025
get_current_sound_data:
	; get current sound (PCM out) data for graphics

	mov	di, g_buffer

	cmp	byte [audio_hardware], 1
	ja	short ac97_current_sound_data

;-----------------------------------------------------------------------------
	
	; 08/02/2025 (*)		
sb16_current_sound_data:
	mov	cx, [g_samples]
	
	cmp	byte [WAVE_BitsPerSample], 16
	jne	short sb16_gcd_1 ; 8 bit DMA channel
	in	al, 0C6h ; DMA channel 5 count register
	mov	ah, al
	in	al, 0C6h
	xchg	ah, al
	shl	ax, 1 ; word count -> byte count
	jmp	short sb16_gcd_2
sb16_gcd_1:
	in	al, 03h ; DMA channel 1 count register
	mov	ah, al
	in	al, 03h
	xchg	ah, al

sb16_gcd_2:
	;cmp	ax, cx
	;jnb	short sb16_gcd_3
	;; remain count < graphics bytes
	;mov	ax, cx ; fix remain count to data size
	; 08/02/2025 (*)
	mov	bx, [buffersize] ; half buffer size
	cmp	ax, bx
	jna	short sb16_gcd_3 ; 2nd half
	xor	bx, bx ; 1st half
sb16_gcd_3:
	;mov	si, dma_buffer+dma_buffer_size ; 32768
	;sub	si, ax
	; 08/02/2025 (*)
	lea	si, [dma_buffer+bx] ; start of 1st half or 2nd half

	; si = dma buffer offset
	; cx = load (source) count
	; di = g_buffer

	jmp	word [sound_data_copy]

;-----------------------------------------------------------------------------

	; 10/02/2025
	; 09/02/2025
	; 08/02/2025 (*)
	; 08/02/2025
	; 07/02/2025
ac97_current_sound_data:
	;mov	cx, 1024 ; always 16bit stereo
	; 08/02/2025
	mov	cx, 512

	mov	dx, PO_CIV_REG ; Position In Current Buff Reg
	add	dx, [NABMBAR]
	in	al, dx	; current index value
	mov	bx, [WAV_BUFFER_1]
	test	al, 1
	jz	short ac97_gcd_1
	mov	bx, [WAV_BUFFER_2]
ac97_gcd_1:
	; 10/02/2025
	;mov	dx, PO_PICB_REG ; Position In Current Buff Reg
	;add	dx, [NABMBAR]
	;in	ax, dx	; remain words
	; 08/02/2025
	;shl	ax, 1	; remain bytes
ac97_gcd_2:
	;cmp	ax, cx
	;jnb	short ac97_gcd_3
	;; remain count < graphics bytes
	;mov	ax, cx ; fix remain count to data size
	; 09/02/2025
	; 08/02/2025 (*)
	xor	si, si ; 1st half
ac97_gcd_3:
	push	ds

	; 08/02/2025
	;mov	si, [buffersize] ; 16 bit sample count
	;sub	si, ax
	;shl	si, 1 ; byte offset

	; 09/02/2025
	; si = 0, start of the wave/pcm data (dma) buffer
	; (buffer size is adjusted for playing in 1/18.2 second) 

	mov	ds, bx

	; ds:si = dma buffer offset
	; cx = load (source) count
	; es:di = g_buffer

	; AC97 dma buffer contains 16bit stereo samples (only)
	; copy samples to g_buffer

	;shr	cx, 1
	rep	movsw
	
	pop	ds
	retn

;-----------------------------------------------------------------------------

	; 07/02/2025
sdc_16bit_stereo:
	; si = dma buffer offset
	; cx = load (source) count = 1024
	; di = g_buffer
	shr	cx, 1
	rep	movsw
	retn

;-----------------------------------------------------------------------------

	; 07/02/2025
sdc_16bit_mono:
	; si = dma buffer offset
	; cx = load (source) count = 512
	; di = g_buffer
	shr	cx, 1
sdc_16bm_loop:
	lodsw
	stosw
	stosw
	loop	sdc_16bm_loop
	retn

;-----------------------------------------------------------------------------

	; 07/02/2025
sdc_8bit_stereo:
	; si = dma buffer offset
	; cx = load (source) count = 512
	; di = g_buffer

	; convert to 16 bit sample
sdc_8bs_loop:
	lodsb
	sub	al, 80h ; middle = 0, min = -128, max = 127
	shl	ax, 8
	stosw
	loop	sdc_8bs_loop
	retn

;-----------------------------------------------------------------------------

	; 07/02/2025
sdc_8bit_mono:
	; si = dma buffer offset
	; cx = load (source) count = 256
	; di = g_buffer

	; convert to 16 bit sample
sdc_8bm_loop:
	lodsb
	sub	al, 80h ; middle = 0, min = -128, max = 127
	shl	ax, 8
	stosw	; L
	; convert to stereo
	stosw	; R
	loop	sdc_8bm_loop
	retn

;-----------------------------------------------------------------------------
; Memory Allocation
;-----------------------------------------------------------------------------

	; 07/02/2025
	; Release memory not used
setFree:
	mov	bx, 65536/16	; 4K paragraphs ; 17/02/2017 (Erdogan Tan)

	mov	ah, 4Ah		; pass new length to DOS
	int	21h

	retn			; back to caller 
				; new size (allocated memory) = 64KB

;-----------------------------------------------------------------------------

	; 08/02/2025
	; bx = number of paragraphs

	; 07/02/2025
memAlloc:
	; input: AX = # of paragraphs required
	; output: AX = segment of block to use

	; 08/02/2025
	;mov	bx, ax
	mov	ah, 48h
	int	21h
	retn

;-----------------------------------------------------------------------------
; Print/Display Text
;-----------------------------------------------------------------------------

	; 07/02/2025
	; 04/02/2025 - cgaplay.asm
	; 01/01/2025 
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

;-----------------------------------------------------------------------------

;=============================================================================
;	Load IFF/ILBM files for VGA 640x480x16 graphics mode
;=============================================================================

; EX1B.ASM (21/6/1994, Carlos Hasan; MSDOS, 'RUNME.EXE', 'TNYPL211')

; 21/10/2017 (TRDOS 386, 'tmodplay.s', Erdogan Tan, NASM syntax)

;-----------------------------------------------------------------------------
; EQUATES AND STRUCTURES
;-----------------------------------------------------------------------------

ID_FORM equ 4D524F46h		; IFF/ILBM chunk IDs
ID_ILBM equ 4D424C49h
ID_BMHD equ 44484D42h
ID_CMAP equ 50414D43h
ID_BODY equ 59444F42h

struc Form			; IFF/ILBM header file format
  .ID:		resd 1
  .Length:	resd 1
  .Type:	resd 1
  .size:
endstruc

struc Chunk			; IFF/ILBM header chunk format
  .ID:		resd 1
  .Length:	resd 1
  .size:
endstruc

struc BMHD			; IFF/ILBM BMHD chunk format
  .Width: 	resw 1
  .Height:	resw 1
  .PosX:	resw 1
  .PosY:	resw 1
  .Planes:	resb 1
  .Masking:	resb 1
  .Compression:	resb 1
  .Pad:		resb 1
  .Transparent:	resw 1
  .AspectX	resb 1
  .AspectY:	resb 1
  .PageWidth:	resw 1
  .PageHeight:	resw 1
  .size:
endstruc

struc CMAP			; IFF/ILBM CMAP chunk format
  .Colors:	resb 768
  .size:
endstruc

;------------------------------------------------------------------------------
; bswap - macro to reverse the byte order of a 32-bit register, converting
;         a value in little/big endian form to big/little endian form.
;------------------------------------------------------------------------------

;%macro	bswap   1
;       xchg    al, ah
;       rol     eax, 16
;       xchg    al, ah
;%endmacro

;------------------------------------------------------------------------------
; putlbm - draw the IFF/ILBM picture on VGA 640x480x16 graphics mode
; In:
;  ESI = IFF/ILBM image file address
;------------------------------------------------------------------------------

	; 07/02/2025
putlbm:
	; 06/02/2025
	mov	si, LOGO_ADDRESS
	
	;pushad

; check if this is a valid IFF/ILBM Deluxe Paint file

;	cmp	dword [si+Form.ID], ID_FORM
;	jne	short putlbmd0
;	cmp	dword [si+Form.Type], ID_ILBM
;	jne	short putlbmd0

; get the IFF/ILBM file length in bytes

	;mov	eax, [esi+Form.Length]
        ;bswap	eax
	;mov	ecx, eax

	mov	cx, [si+Form.Length+2]
	xchg	cl, ch

; decrease the file length and updates the file pointer

	;sub	ecx, 4
	sub	cx, 4	

	add	si, Form.size

; IFF/ILBM main parser body loop

putlbml0:
	;test	ecx, ecx
	test	cx, cx
        jle	short putlbmd1

; get the next chunk ID and length in bytes

	;mov	ebx, [si+Chunk.ID]
	;mov	eax, [si+Chunk.Length]
	;bswap	eax
	;xchg	ebx, eax
	mov	ax, [si+Chunk.ID]
	mov	bx, [si+Chunk.Length+2]
	xchg	bh, bl	

        add     si, Chunk.size

; word align the chunk length and decrease the file length counter

	;inc	ebx
	inc	bx
	and	bl, 0FEh ; ~1
	;sub	ecx, Chunk.size
	sub	cx, Chunk.size
        
	;sub	ecx, ebx
	sub	cx, bx

; check for the BMHD/CMAP/BODY chunk headers

	;cmp	eax, ID_BMHD
	cmp	ax, 'BM'
	je	short putlbmf0
	;cmp	eax, ID_CMAP
	cmp	ax, 'CM'
	je	short putlbmf1
	;cmp	eax, ID_BODY
	cmp	ax, 'BO'
        je      short putlbmf2

; advance to the next IFF/ILBM chunk structure

putlbmc0:
	;add	esi, ebx
	add	si, bx
	jmp     short putlbml0

putlbmd0:
	stc
	;popad
	retn

; process the BMHD bitmap header chunk

putlbmf0:
	cmp	byte [si+BMHD.Planes], 4
	jne	short putlbmd0
	cmp	byte [si+BMHD.Compression], 1
	jne	short putlbmd0
	cmp	byte [si+BMHD.Pad], 0
	jne	short putlbmd0
	;movzx	eax, word [esi+BMHD.Width]
	mov	ax, [si+BMHD.Width]
	xchg	al, ah
	;add	eax, 7
	;shr	eax, 3
	add	ax, 7
	shr	ax, 3
	;mov	[picture.width], eax
	mov	[picture.width], ax
	;movzx	eax, word [esi+BMHD.Height]
	mov	ax, [si+BMHD.Height]
	xchg	al, ah
	;mov	[picture.height], eax
	mov	[picture.height], ax
	jmp	short putlbmc0

putlbmd1:
	clc
	;popad
	retn

; process the CMAP colormap chunk

putlbmf1:
	mov	dx, 3C8h
	xor	al, al
	out	dx, al

	inc	dx
putlbml1:
	mov	al, [si]
	shr	al, 2
	out	dx, al

	inc	si
	dec	bx
	jg	short putlbml1
	jmp	putlbml0

; process the BODY bitmap body chunk

putlbmf2:
	;pushad
	pusha
        ;mov	edi, 0A0000h
        ;cld
	push	es
	mov	di, 0A000h
	mov	es, di
	xor	di, di
	mov	dx, 3CEh
	mov	ax, 0FF08h
	out	dx, ax
	;mov	dx, 3C4h
	mov	dl, 0C4h
	mov     al, 02h
        out	dx, al
	inc	dx
	;mov	ecx, [picture.height]
	mov	cx, [picture.height]
putlbml2:
	;push	ecx
	push	cx
	mov	al, 11h
putlbml3:
	;push	eax
	;push	edi
	push	ax
	push	di
        out	dx, al
	;mov	ebx, [picture.width]
	mov	bx, [picture.width]
putlbml4:
        lodsb
	xor	ch, ch
        test	al, al
        jl	short putlbmf3
	;movzx	ecx, al
	mov	cl, al 
	;inc	ecx
	inc	cx
	;sub	ebx, ecx
	sub	bx, cx
	rep	movsb
	jmp	short putlbmc4
putlbmf3:
	neg	al
	;movzx	ecx, al
	mov	cl, al ; ch = 0
	;inc	ecx
	;sub	ebx, ecx
	inc	cx
	sub	bx, cx
	lodsb
	rep	stosb
putlbmc4:
	;test	ebx, ebx
	test	bx, bx
	jg	short putlbml4
	;pop	edi
	;pop	eax
	pop	di
	pop	ax
	add	al, al
	jnc	short putlbml3
	;add	edi, 80
	add	di, 80
	;pop	ecx
	pop	cx
	loop	putlbml2
	pop	es
	;popad
	popa
        jmp	putlbmc0

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

align 2

; 22/10/2017
LOGO_ADDRESS:
; 27/10/2017
incbin "TINYPLAY.LBM"

;=============================================================================
;		preinitialized data
;=============================================================================
	
		db 0
FileHandle:	dd -1
		db 0

Credits:	db 'Tiny WAV Player for Retro DOS by Erdogan Tan. '
		db 'February 2025.',10,13,0
		db '10/02/2025',10,13
reset:		db 0

msg_usage:	db 10,13
		db 'usage: twavplay filename.wav',10,13,0

noDevMsg:	db 10,13
		db 'Error: Unable to find a proper audio device !'
		db 10,13,0

noFileErrMsg:	db 10,13
		db 'Error: file not found.',10,13,0

not_valid_wavf:	db 10,13
		db 'Not a proper/valid WAV file !',10,13,0

		; 08/02/2025
sb16_init_err_msg:
		db 10,13
		db 'Sound Blaster 16 initialization error !',10,13,0
ac97_init_err_msg:
		db 10,13
		db 'AC97 hardware initialization error !',10,13,0
		
;init_err_msg:	;db 10,13
		;db 'Audio system initialization error !',10,13,0

msg_no_vra:	db 10,13
		db "No VRA support ! Only 48 kHZ sample rate supported !"
		db 10,13,0

hex_chars:	db "0123456789ABCDEF", 0

msgAC97Info:	db 0Dh, 0Ah
		db "AC97 Audio Controller & Codec Info", 0Dh, 0Ah
		db "Vendor ID: "
msgVendorId:	db "0000h Device ID: "
msgDevId:	db "0000h", 0Dh, 0Ah
		db "Bus: "
msgBusNo:	db "00h Device: "
msgDevNo:	db "00h Function: "
msgFncNo:	db "00h"
		db 0Dh, 0Ah

		db "NAMBAR: "
msgNamBar:	db "0000h  "
		db "NABMBAR: "
msgNabmBar:	db "0000h  IRQ: "
msgIRQ:		dw 3030h
		db 0Dh, 0Ah, 0

msgWavFileName:	db 0Dh, 0Ah, "WAV File Name: ",0
msgSampleRate:	db 0Dh, 0Ah, "Sample Rate: "
msgHertz:	db "00000 Hz, ", 0 
msg8Bits:	db "8 bits, ", 0 
msgMono:	db "Mono", 0Dh, 0Ah, 0
msg16Bits:	db "16 bits, ", 0
msgStereo:	db "Stereo"
nextline:	db 0Dh, 0Ah, 0

msgVRAheader:	db "VRA support: "
		db 0	
msgVRAyes:	db "YES", 0Dh, 0Ah, 0
msgVRAno:	db "NO ", 0Dh, 0Ah
		db "(Interpolated sample rate playing method)"
		db 0Dh, 0Ah, 0

msgSB16Info:	db 0Dh, 0Ah
		db " Audio Hardware: Sound Blaster 16", 0Dh, 0Ah
		db "      Base Port: "
msgBasePort:
		db "000h", 0Dh, 0Ah
		db "            IRQ: "
msgSB16IRQ:
		db 30h
		db 0Dh, 0Ah, 0

; 07/02/2025
msgPressAKey:	db 0Dh, 0Ah
		db ' ... press a key to continue ... '
		db 0Dh, 0Ah, 0

; 07/02/2025
half_buffer:	db 1

vra_ok:		db '.. VRA OK ..', 0Dh, 0Ah,0

;=============================================================================
;		uninitialized data
;=============================================================================

; 06/02/2025

; BSS

bss_start:

ABSOLUTE bss_start

alignb 4

;------------------------------------------------------------------------------
; IFF/ILBM DATA
;------------------------------------------------------------------------------

; 07/02/2025
picture.width:	resw 1 	; current picture width and height
picture.height:	resw 1

;------------------------------------------------------------------------------

; 07/02/2025 (SB16)
old_irq5v_o:	resw 1
old_irq5v_s:	resw 1
old_irq7v_o:	resw 1
old_irq7v_s:	resw 1

;------------------------------------------------------------------------------

;;;;;;;
WAVFILEHEADERbuff:
RIFF_ChunkID:	resd 1	; Must be equal to "RIFF" - big-endian
			; 0x52494646
RIFF_ChunkSize: resd 1	; Represents total file size, not
        		; including the first 2 fields
			; (Total_File_Size - 8), little-endian
RIFF_Format:	resd 1	; Must be equal to "WAVE" - big-endian
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
WAVE_ByteRate:	resd 1	; SampleRate * NumChannels * BytesPerSample
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
;;;;;;;

;------------------------------------------------------------------------------

wav_file_name:	resb 80 ; wave file, path name (<= 80 bytes)

		resw 1

;------------------------------------------------------------------------------

ac97_int_ln_reg:
audio_intr:	resb 1
VRA:		resb 1	; Variable Rate Audio Support Status
fbs_shift:	resb 1
flags:		resb 1

dev_vendor:	resd 1
bus_dev_fn:	resd 1
audio_io_base:		; Sound Blaster 16 base port address (220h)
NAMBAR:		resw 1
NABMBAR:	resw 1

audio_hardware:	resb 1
IRQnum:		resb 1
IRQstatus:	resb 1
volume:		resb 1
stopped:	resb 1

;------------------------------------------------------------------------------

alignb 16

RowOfs:		resw 256

NewScope_L:	resw 256
NewScope_R:	resw 256
OldScope_L:	resw 256
OldScope_R:	resw 256

;------------------------------------------------------------------------------

; 08/02/2025
; 256 byte buffer for descriptor list
BDL_BUFFER:	resw 1	; segment of 256 byte BDL buffer
; DMA buffers
WAV_BUFFER_1:	resw 1	; segment of 1st wav/pcm data buffer
WAV_BUFFER_2:	resw 1	; segment of 2nd wav/pcm data buffer

;------------------------------------------------------------------------------

loadfromwavfile:
		resw 1	; 'loadfromfile' or load+conversion proc address
loadsize:	resw 1	; (.wav file) read count (bytes) per one time
buffersize:	resd 1	; 16 bit samples (not bytes)

count:		resw 1	; byte count of one (wav file) read
LoadedDataBytes:
		resd 1	; total read/load count
		
timerticks:	resd 1	; (to eliminate excessive lookup of events in tuneloop)
			; (in order to get the emulator/qemu to run correctly)
bss_end:

;------------------------------------------------------------------------------

; 07/02/2025
sound_data_copy:	; address pointer for g_buffer fast (conversion) copy
		resw 1
g_samples:	resw 1	; count of samples for g_buffer copy/transfer

alignb 16
; 08/02/2025
;bdl_buffer:	resb 256
g_buffer:	resb 1024 ; 16 bit stereo samples for wave graphics display

;dma_buffer:
;temp_buffer:	;resb 50600 ; (44.1kHZ stereo 12650 samples)
;		; 07/02/2025
;		resb 33680  ; (44.1kHZ stereo 8418 samples)

; 08/02/2025
temp_buffer:	
		; max. 10656 bytes (no-VRA AC97)
		; 10600: 44.1 kHZ stereo 2438 samples, 2650 (48kHZ) samples
		; 10656: 11.025 kHZ stereo 612 samples, 2664 (48kHZ) samples
		; 10508: 22.050 kHZ stereo 1207 samples, 2627 (48kHZ) samples
		; 10544: 24 kHZ stereo 1318 samples, 2636 (48kHZ) samples
		; 10548: 32 kHZ stereo 1758 samples, 2637 (48kHZ) samples
		; 10548: 16 kHZ stereo 879 samples, 2637 (48kHZ) samples
		; 10544: 12 kHZ stereo 659 samples, 2636 (48kHZ) samples
		; 10560: 8 kHZ stereo 440 samples, 2640 (48kHZ) samples
		
dma_buffer:	; max. 21120 bytes (SB16)
		resb 10560
		resb 10560

;------------------------------------------------------------------------------

;wav_buffer1 equ dma_buffer
;wav_buffer2 equ wav_buffer1 + LOADSIZE ; + 16384

;------------------------------------------------------------------------------
