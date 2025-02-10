; ****************************************************************************
; playwav6.asm (for Retro DOS)
; ----------------------------------------------------------------------------
; PLAYWAV6.COM ! AC'97 (ICH) .WAV PLAYER program by Erdogan TAN
;
; 30/05/2024
;
; [ Last Modification: 04/02/2025 ]
;
; Modified from PLAYWAV6.PRG .wav player program by Erdogan Tan, 27/11/2023
; Modified from PLAYWAV4.COM .wav player program by Erdogan Tan, 19/05/2024
;
; Assembler: NASM version 2.15
;	     nasm playwav6.asm -l playwav6.txt -o PLAYWAV6.COM	
; ----------------------------------------------------------------------------
; Derived from '.wav file player for DOS' Jeff Leyda, Sep 02, 2002

; previous version: playwav4.asm (19/05/2024)

; TUNELOOP version (playing without AC97 interrupt) - 06/11/2023 - Erdogan Tan
; sample rate conversion version - 18/11/2023 - Erdogan Tan

; CODE

%macro sys_msg 2
   	; 30/05/2024
	mov	si, %1	; message
  	mov	bl, %2	; text color
	xor	bh, bh	; video page 0
	mov	ah, 0Eh
	call	p_msg
%endmacro

; player internal variables and other equates.
BUFFERSIZE      equ     64 * 1024       ; 64k file buffer size.
ENDOFFILE       equ     BIT0            ; flag for knowing end of file

[BITS 16]

[ORG 100h] 

	%include 'ac97.inc' ; 17/02/2017

_STARTUP:
	; 30/05/2024
	; Prints the Credits Text.
	sys_msg Credits, 0Bh

	; 30/05/2024
        call    setFree			; deallocate unused DOS mem

	; 17/02/2017
	; Clear BSS (uninitialized data) area
	xor	ax, ax ; 0
	mov	cx, (bss_end - bss_start)/2
	mov	di, bss_start
	rep	stosw

	; Detect (& Enable) AC'97 Audio Device
	call	DetectAC97
	;jnc	short GetFileName
	; 30/05/2024
	jnc	short allocate_memory

	; 30/05/2024
_dev_not_ready:
	; couldn't find the audio device!
	sys_msg noDevMsg, 0Fh
        jmp     Exit

	; 30/05/2024
allocate_memory:

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

	; 30/05/2024
GetFileName:
	mov	di, wav_file_name 
	mov	si, 80h
	mov	bl, [si]
	xor	bh, bh
	inc	bx
	mov	byte [si+bx], 0		; make AsciiZ filename.
	inc	si
ScanName:
	lodsb
	test	al, al
	jz	pmsg_usage
	cmp	al, 20h
	je	short ScanName	; scan start of name.
	stosb
	mov	ah, 0FFh
a_0:	
	inc	ah
a_1:
	lodsb
	stosb
	cmp	al, '.'
	je	short a_0	
	and	al, al
	jnz	short a_1

	or	ah, ah		; if period NOT found,
	jnz	short _1 	; then add a .WAV extension.
SetExt:
	dec	di
	mov	dword [di], '.WAV'
	mov	byte [di+4], 0

_1:
	call	write_audio_dev_info 

; open the file
        ; open existing file
	mov     al, OPEN		; open existing file
	mov	dx, wav_file_name
        call    openFile ; no error? ok.
        jnc     short _gsr

; file not found!
	sys_msg noFileErrMsg, 0Fh
_exit_:
        jmp     Exit

_gsr:  
       	call    getSampleRate		; read the sample rate
                                        ; pass it onto codec.
	; 25/11/2023
	jc	short _exit_		; nothing to do

	mov	[sample_rate], ax
	mov	[stmo], cl
	mov	[bps], dl

	; 26/11/2023
	mov	byte [fbs_shift], 0 ; 0 = stereo and 16 bit 
	dec	cl
	jnz	short _gsr_1 ; stereo
	inc	byte [fbs_shift] ; 1 = mono or 8 bit		
_gsr_1:
	cmp	dl, 8 
	ja	short _gsr_2 ; 16 bit samples
	inc	byte [fbs_shift] ; 2 = mono and 8 bit
_gsr_2:
	; 29/05/2024
	call	write_ac97_pci_dev_info

	; 30/05/2024
	; 29/05/2024
	;call	check_vra

	; 30/05/2024
	call	codecConfig		; unmute codec, set rates.
	jc	init_err

	; 25/11/2023
	call	write_VRA_info

	; 01/05/2017
	call	write_wav_file_info

	; 25/11/2023
	; ------------------------------------------

	; 18/11/2023 (ich_wav4.asm)
	; 13/11/2023 (ich_wav3.asm)

	cmp	byte [VRA], 1
	jb	short chk_sample_rate
playwav_48_khz:	
	mov	word [loadfromwavfile], loadFromFile
	;mov	word [loadsize], 0 ; 65536
	mov	word [buffersize], 32768 ; samples
	jmp	PlayNow ; 30/05/2024

	; 04/02/2025
	; 02/02/2025
chk_sample_rate:
	; set conversion parameters
	; (for 8, 11.025, 16, 22.050, 24, 32 kHZ)
	mov	ax, [sample_rate]
	cmp	ax, 48000
	je	short playwav_48_khz
chk_22khz:
	cmp	ax, 22050
	jne	short chk_11khz
	cmp	byte [bps], 8
	jna	short chk_22khz_1
	mov	bx, load_22khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_22khz_2
	mov	bx, load_22khz_mono_16_bit
	jmp	short chk_22khz_2
chk_22khz_1:
	mov	bx, load_22khz_stereo_8_bit
	cmp	byte [stmo], 1 
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
	cmp	byte [bps], 8
	jna	short chk_11khz_1
	mov	bx, load_11khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_11khz_2
	mov	bx, load_11khz_mono_16_bit
	jmp	short chk_11khz_2
chk_11khz_1:
	mov	bx, load_11khz_stereo_8_bit
	cmp	byte [stmo], 1 
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
	cmp	byte [bps], 8
	jna	short chk_44khz_1
	mov	bx, load_44khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_44khz_2
	mov	bx, load_44khz_mono_16_bit
	jmp	short chk_44khz_2
chk_44khz_1:
	mov	bx, load_44khz_stereo_8_bit
	cmp	byte [stmo], 1 
	jne	short chk_44khz_2
	mov	bx, load_44khz_mono_8_bit
chk_44khz_2:
	;mov	ax, 15065 ; (655*23)
	; 18/11/2023 ((file size + bss + stack) <= 64KB)
	mov	ax, 14076 ; (612 *23)
	mov	dx, 25
	mov	cx, 23
	jmp	set_sizes 
chk_16khz:
	cmp	ax, 16000
	jne	short chk_8khz
	cmp	byte [bps], 8
	jna	short chk_16khz_1
	mov	bx, load_16khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_16khz_2
	mov	bx, load_16khz_mono_16_bit
	jmp	short chk_16khz_2
chk_16khz_1:
	mov	bx, load_16khz_stereo_8_bit
	cmp	byte [stmo], 1 
	jne	short chk_16khz_2
	mov	bx, load_16khz_mono_8_bit
chk_16khz_2:
	mov	ax, 5461
	mov	dx, 3
	mov	cx, 1
	jmp	set_sizes 
chk_8khz:
	cmp	ax, 8000
	jne	short chk_24khz
	cmp	byte [bps], 8
	jna	short chk_8khz_1
	mov	bx, load_8khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_8khz_2
	mov	bx, load_8khz_mono_16_bit
	jmp	short chk_8khz_2
chk_8khz_1:
	mov	bx, load_8khz_stereo_8_bit
	cmp	byte [stmo], 1 
	jne	short chk_8khz_2
	mov	bx, load_8khz_mono_8_bit
chk_8khz_2:
	mov	ax, 2730
	mov	dx, 6
	mov	cx, 1
	jmp	short set_sizes 
chk_24khz:
	cmp	ax, 24000
	jne	short chk_32khz
	cmp	byte [bps], 8
	jna	short chk_24khz_1
	mov	bx, load_24khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_24khz_2
	mov	bx, load_24khz_mono_16_bit
	jmp	short chk_24khz_2
chk_24khz_1:
	mov	bx, load_24khz_stereo_8_bit
	cmp	byte [stmo], 1 
	jne	short chk_24khz_2
	mov	bx, load_24khz_mono_8_bit
chk_24khz_2:
	;mov	ax, 8192
	; 04/02/2025
	mov	ax, 8190
	mov	dx, 2
	mov	cx, 1
	jmp	short set_sizes 
chk_32khz:
	cmp	ax, 32000
	;jne	short vra_needed
	; 02/02/2025
	jne	short chk_12khz
	cmp	byte [bps], 8
	jna	short chk_32khz_1
	mov	bx, load_32khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_32khz_2
	mov	bx, load_32khz_mono_16_bit
	jmp	short chk_32khz_2
chk_32khz_1:
	mov	bx, load_32khz_stereo_8_bit
	cmp	byte [stmo], 1 
	jne	short chk_32khz_2
	mov	bx, load_32khz_mono_8_bit
chk_32khz_2:
	mov	ax, 10922
	mov	dx, 3
	mov	cx, 2
	;jmp	short set_sizes 
set_sizes:
	cmp	byte [stmo], 1
	je	short ss_1
	shl	ax, 1
ss_1:
	cmp	byte [bps], 8
	jna	short ss_2
	; 16 bit samples
	shl	ax, 1
ss_2:
	mov	[loadsize], ax
	mul	dx

	; 04/02/2025
	cmp	cx, 1
	je	short ss_3

	div	cx
ss_3:
	mov	cl, [fbs_shift]
	shl	ax, cl

	; 03/02/2025
	shr	ax, 1	; buffer size is 16 bit sample count
	; 65536 -> 32768
	mov	[buffersize], ax
	mov	[loadfromwavfile], bx
	jmp	short PlayNow

	;;;;
	; 02/02/2025
chk_12khz:
	cmp	ax, 12000
	jne	short vra_needed
	cmp	byte [bps], 8
	jna	short chk_12khz_1
	mov	bx, load_12khz_stereo_16_bit
	cmp	byte [stmo], 1 
	jne	short chk_12khz_2
	mov	bx, load_12khz_mono_16_bit
	jmp	short chk_12khz_2
chk_12khz_1:
	mov	bx, load_12khz_stereo_8_bit
	cmp	byte [stmo], 1 
	jne	short chk_12khz_2
	mov	bx, load_12khz_mono_8_bit
chk_12khz_2:
	;mov	ax, 4096
	; 04/02/2025
	mov	ax, 4095
	mov	dx, 4
	mov	cx, 1
	jmp	set_sizes 
	;;;;

vra_needed:
	; 13/11/2023
	pop	ax ; discard return address to the caller
	; 30/05/2024
vra_err:
	sys_msg msg_no_vra, 0Fh 
	jmp	Exit

	; 30/05/2024
	; 13/11/2023  (ich_wav4.asm)
loadfromwavfile:
	dw	loadFromFile
loadsize:	; read from wav file
	dw	0
buffersize:	; write to DMA buffer
	dd	32768 ; 16 bit samples (not bytes)

PlayNow: 
	; 30/05/2024
	; playwav4.asm
_2:
	call	check4keyboardstop	; flush keyboard buffer
	jc	short _2		; 07/11/2023

 	;call	codecConfig		; unmute codec, set rates.
	; 11/11/2023
	;jc	short init_err

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

	; 30/05/2024
	sys_msg nextline, 07h

; play the .wav file. Most of the good stuff is in here.

	call    PlayWav

; close the .wav file and exit.

Exit:  
        call    closeFile
         
	mov	ax, 4c00h	; bye !
	int	21h
here:
	jmp	short here	; do not come here !

	; 30/05/2024
pmsg_usage:
	sys_msg msg_usage, 0Bh
	jmp	short Exit

	; 30/05/2024
init_err:
	sys_msg msg_init_err, 0Fh
	jmp	short Exit

	; 30/05/2024
error_exit:
	sys_msg msg_error, 0Eh
	jmp	short Exit

	; --------------------------------------------
	
	; 29/05/2024 (TYRDOS 386, playwav7.s)
	; ((Modified from playwav4.asm, ich_wav4.asm))
	; ------------------
;playwav_vra:
PlayWav:
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
_0:
        movzx   eax, word [WAV_BUFFER1]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer1

	;mov	eax, BUFFERSIZE / 2 ; size of buffer (32K) in (16bit) words
	; 13/11/2023 (ich_wav3.asm) - 18/11/2023 (ich_wav4.asm)
	mov	eax, [buffersize]

	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

        movzx   eax, word [WAV_BUFFER2]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer2

	;mov	eax, BUFFERSIZE / 2 ; size of half buffer (32K)
	; 13/11/2023 (ich_wav3.asm) - 18/11/2023 (ich_wav4.asm)
	mov	eax, [buffersize]

	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

        loop    _0
        pop     es

	; load 64k into buffer 1
        mov     ax, [WAV_BUFFER1]
        ;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]

	; and 64k into buffer 2
	mov     ax, [WAV_BUFFER2]
       	;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]
	
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

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k. Likewise when it's playing buffer 2, refresh buffer 1 and repeat.

; 18/11/2023
; 08/11/2023
; 07/11/2023
	
tuneLoop:
	; 30/05/2024
	; 18/11/2023 (ich_wav4.asm)
	; 08/11/2023
	; 06/11/2023
	mov	al, '1'
	call	tL0
tL1:
	call	updateLVI	; /set LVI != CIV/
	jz	short _exitt_	; 08/11/2023
	call	check4keyboardstop
	jc	short _exitt_
	call	getCurrentIndex
	test	al, BIT0
	jz	short tL1	; loop if buffer 2 is not playing

	; load buffer 1
	mov     ax, [WAV_BUFFER1]
	;call	loadFromFile
	; 18/11/2023
	call	word [loadfromwavfile]
	jc	short _exitt_	; end of file

	mov	al, '2'
	call	tL0
tL2:
	call    updateLVI
	jz	short _exitt_	; 08/11/2023
	call    check4keyboardstop
	jc	short _exitt_
	call    getCurrentIndex
	test	al, BIT0
	jnz	short tL2	; loop if buffer 1 is not playing

	; load buffer 2
	mov     ax, [WAV_BUFFER2]
	;call	loadFromFile
	; 18/11/2023
	call	word [loadfromwavfile]
	jnc	short tuneLoop
_exitt_:
	mov	dx, [NABMBAR]		
	add	dx, PO_CR_REG	; PCM out Control Register
	mov	al, 0
	out	dx, al		; stop player

	; 30/05/2024
	mov	al, '0'

	;add	al, '0'
	;call	tL0
	;
	;retn
	; 06/11/2023
	;jmp	short tL0
	;retn

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

	; ------------------

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

	mov     [ac97_int_ln_reg], dl

	;clc

	retn

; FILE.ASM
;open or create file
;
;input: ds:dx-->filename (asciiz)
;       al=file Mode (create or open)
;output: none  cs:[filehandle] filled
;
openFile:	; (playwav4.asm)
	push	ax
	push	cx
	mov	ah, 3Bh			; start with a mode
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
closeFile:	; (playwav4.asm)
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

getSampleRate:	; (playwav4.asm)
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

memAlloc:
; input: AX = # of paragraphs required
; output: AX = segment of block to use

	push	bx
	mov	bx, ax
	mov	ah, 48h
	int	21h
	pop	bx
	retn

; ----

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

	mov	cl, [fbs_shift]   
	and	cl, cl
	jz	short lff_1 ; stereo, 16 bit	

	mov	di, BUFFERSIZE - 1 ; 65535

	; fbs_shift =
	;	2 for mono and 8 bit sample (multiplier = 4)
	;	1 for mono or 8 bit sample (multiplier = 2)
	shr	di, cl
	inc	di ; 16384 for 8 bit and mono	
		   ; 32768 for 8 bit or mono	

	mov	ax, cs
	mov	dx, temp_buffer ; temporary buffer for wav data

	; 17/02/2017 (stereo/mono, 8bit/16bit corrections)
	; load file into memory
        mov	cx, di                       
	mov	bx, [filehandle]
	mov     ds, ax
       	mov	ah, 3Fh
	int	21h

	mov	bx, cs
	mov	ds, bx

	jc	short lff_4 ; error !

	; 08/11/2023
	xor	dx, dx ; 0

	and	ax, ax
	jz	short lff_3

	mov	bl, [fbs_shift]

	push	es
	mov	di, dx ; 0 ; [fbs_off]
	;mov	bp, [fbs_seg] ; buffer segment
	mov	es, bp
	mov	si, temp_buffer ; temporary buffer address
	mov	cx, ax ; byte count
	cmp	byte [bps], 8 ; bits per sample (8 or 16)
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
	or	di, di
	jz	short endLFF ; 64KB ok 
	
	mov	ax, di ; [fbs_off]
	dec	ax
	mov	cx, BUFFERSIZE - 1 ; 65535
	jmp	short lff_3
	
lff_1:  
	;mov	bp, ax ; save buffer segment
	xor	dx, dx
	; load file into memory
        mov	cx, (BUFFERSIZE / 2)	; 32k chunk
	mov	bx, [filehandle]
	mov     ds, ax
       	mov	ah, 3Fh
	int	21h

	mov	di, cs
	mov	ds, di

	; 07/11/2023
	jc	short lff_4 ; error !
	
	cmp	ax, cx
	jne	short lff_3
lff_2:
	; 08/11/2023
	add	dx, ax
	;mov	cx, (BUFFERSIZE / 2)	; 32k chunk
	;mov	bx, [filehandle]
	mov     ds, bp
       	mov	ah, 3Fh
	int	21h

	;mov	di, cs
	mov	ds, di

	jc	short lff_4 ; error !

	cmp	ax, cx
	je	short endLFF
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
; cx=target size
; note: must do byte size fill
; destroys bx, cx
;
padfill:
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
	mov	di, dx ; buffer offset
	add	di, ax       	
	; 07/11/2023
	;add	di, [fbs_off]
        xor	al, al
	rep	stosb
	;mov	[fbs_off], di
	;pop	di
        pop	es
	retn
; /////
	
write_audio_dev_info:
	; 30/05/2024
     	sys_msg msgAudioCardInfo, 0Fh
	retn

write_wav_file_info:
	; 30/05/2024
	; 01/05/2017
	sys_msg msgWavFileName, 0Fh
	sys_msg wav_file_name, 0Fh

write_sample_rate:
	; 30/05/2024
	; 01/05/2017
	mov	ax, [sample_rate]
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
	
	; 30/05/2024
	sys_msg msgSampleRate, 0Fh

	; 19/11/2016
	mov	dx, msg16Bits
	cmp	byte [bps], 16
	je	short wsr_1
	mov	dx, msg8Bits
wsr_1:
	; 30/05/2024
	sys_msg dx, 0Fh

	mov	dx, msgMono
	cmp	byte [stmo], 1
	je	short wsr_2
	mov	dx, msgStereo
wsr_2:
	; 30/05/2024
	sys_msg dx, 0Fh
	; 30/05/2024
	sys_msg	nextline, 07h
        retn

write_ac97_pci_dev_info:
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
	add	[msgIRQ], ax
	and	al, al
	jnz	short _w_ac97imsg_
	mov	al, [msgIRQ+1]
	mov	ah, ' '
	mov	[msgIRQ], ax
_w_ac97imsg_:
	; 30/05/2024
	sys_msg msgAC97Info, 07h

        retn

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

; 02/02/2025
; 30/05/2024 (playwav6.asm)
; 18/11/2023 (ich_wav3.asm & ich_wav4.asm)
; 15/11/2023 (ich_wav3.asm)	
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; 16/11/2023
	;and	ax, ax
	jnz	short lff8s2_8
	;jmp	lff8s2_3
	; 15/11/2023
	jmp	lff8_eof

lff8s2_8:
	mov	cx, ax ; dword count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff16m_8
	;jmp	lff16m_3
	; 15/11/2023
	jmp	lff16_eof

lff16m_8:
	mov	cx, ax		; byte count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff24m_8
	jmp	lff24_eof

lff24m_8:
	mov	cx, ax		; byte count
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
	;mov	ax, [si
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff22m_8
	jmp	lff22_eof

lff22m_8:
	mov	cx, ax	; byte count
lff22m_9:
	mov	bp, 5 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address

	shr	ax, 1
	;and	ax, ax
	jnz	short lff22s_8
	jmp	lff22_eof

lff22s_8:
	mov	cx, ax	; word count
lff22s_9:
	mov	bp, 5 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff22m2_8
	jmp	lff22_eof

lff22m2_8:
	mov	cx, ax	; word count
lff22m2_9:
	mov	bp, 5 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; dword (left chan word + right chan word)
	;and	ax, ax
	jnz	short lff22s2_8
	jmp	lff22_eof

lff22s2_8:
	mov	cx, ax	; dword count
lff22s2_9:
	mov	bp, 5 ; interpolation (one step) loop count
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
	; 02/02/2025
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	and	ax, ax
	jnz	short lff11m_8
	jmp	lff11_eof

lff11m_8:
	mov	cx, ax	; byte count
lff11m_9:
	mov	bp, 6 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address

	shr	ax, 1
	;and	ax, ax
	jnz	short lff11s_8
	jmp	lff11_eof

lff11s_8:
	mov	cx, ax	; word count
lff11s_9:
	mov	bp, 6 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff11m2_8
	jmp	lff11_eof

lff11m2_8:
	mov	cx, ax	; word count
lff11m2_9:
	mov	bp, 6 ; interpolation (one step) loop count
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
	mov	bp, 6 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address

	and	ax, ax
	jnz	short lff44m_8
	jmp	lff44_eof

lff44m_8:
	mov	cx, ax	; byte count
lff44m_9:
	mov	bp, 10 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address

	shr	ax, 1
	;and	ax, ax
	jnz	short lff44s_8
	jmp	lff44_eof

lff44s_8:
	mov	cx, ax	; word count
lff44s_9:
	mov	bp, 10 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 1
	;and	ax, ax
	jnz	short lff44m2_8
	jmp	lff44_eof

lff44m2_8:
	mov	cx, ax	; word count
lff44m2_9:
	mov	bp, 10 ; interpolation (one step) loop count
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

	mov	si, dx ; temp_buffer ; temporary buffer address
	
	shr	ax, 2	; dword (left chan word + right chan word)
	;and	ax, ax
	jnz	short lff44s2_8
	jmp	lff44_eof

lff44s2_8:
	mov	cx, ax	; dword count
lff44s2_9:
	mov	bp, 10 ; interpolation (one step) loop count
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
	jc	short lff12m_7 ; error ! !

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
	cmp	word [sample_rate], 48000
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
	
	;and	al, ~BIT1 ; Clear DRA
	;;;
	; 30/05/2024
	and	al, ~(BIT1+BIT0) ; Clear DRA+VRA
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
	mov	ax, [sample_rate] ; 17/02/2017 (Erdogan Tan)

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
  	add	dx, CODEC_MASTER_VOL_REG	;02h 
	out	dx, ax

	; 11/11/2023
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

	mov	ax, 0202h
  	mov	dx, [NAMBAR]
  	add	dx, CODEC_PCM_OUT_REG		;18h 
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

	; 30/05/2024
	clc

        pop     cx
        pop     ax
        retn

; --------------------------------------------------------
; 19/05/2024 - (playwav4.asm) ich_wav4.asm
; --------------------------------------------------------

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
	mov	ah, al
	mov     dx, [NAMBAR]
  	;add    dx, CODEC_MASTER_VOL_REG
	add	dx, CODEC_PCM_OUT_REG
	out     dx, ax

	call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
_cksr:		; 19/05/2024
	clc
p_3:
	retn
p_4:
	;;;
;_cskr:	
	stc
	retn

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

; 29/05/2024
; 19/05/2024
volume: db	02h
	
; --------------------------------------------------------

; DATA

FileHandle:	
	dd	-1

; 30/05/2024
reset:	db	0

Credits:
	db	'Tiny WAV Player for Retro DOS by Erdogan Tan. '
	;;db	'May 2024.',10,13,0
	;;db	'30/05/2024', 10,13,0
	;db	'December 2024.',10,13,0
	;db	'14/12/2024', 10,13,0
	db	'February 2025.',10,13,0
	db	'04/02/2025', 10,13,0

msgAudioCardInfo:
	db 	'for Intel AC97 (ICH) Audio Controller.', 10,13,0

msg_usage:
	db	'usage: playwav6 filename.wav',10,13,0 ; 30/05/2024

noDevMsg:
	db	'Error: Unable to find AC97 audio device!'
	db	10,13,0

noFileErrMsg:
	db	'Error: file not found.',10,13,0

msg_error:	; 30/05/2024

; 29/05/2024
; 11/11/2023
msg_init_err:
	db	CR, LF
	db	"AC97 Controller/Codec initialization error !"
	;db	CR, LF, "$"
	db	CR, LF, 0 ; 14/12/2024

; 25/11/2023
msg_no_vra:
	db	10,13
	db	"No VRA support ! Only 48 kHZ sample rate supported !"
	db	10,13,0

; 29/05/2024 (TRDOS 386)
; 17/02/2017
; Valid ICH device IDs

valid_ids:
dd	(ICH_DID << 16) + INTEL_VID  	 ; 8086h:2415h
dd	(ICH0_DID << 16) + INTEL_VID 	 ; 8086h:2425h
dd	(ICH2_DID << 16) + INTEL_VID 	 ; 8086h:2445h
dd	(ICH3_DID << 16) + INTEL_VID 	 ; 8086h:2485h
dd	(ICH4_DID << 16) + INTEL_VID 	 ; 8086h:24C5h
dd	(ICH5_DID << 16) + INTEL_VID 	 ; 8086h:24D5h
dd	(ICH6_DID << 16) + INTEL_VID 	 ; 8086h:266Eh
dd	(ESB6300_DID << 16) + INTEL_VID  ; 8086h:25A6h
dd	(ESB631X_DID << 16) + INTEL_VID  ; 8086h:2698h
dd	(ICH7_DID << 16) + INTEL_VID 	 ; 8086h:27DEh
; 03/11/2023 - Erdogan Tan
dd	(MX82440_DID << 16) + INTEL_VID  ; 8086h:7195h
dd	(SI7012_DID << 16)  + SIS_VID	 ; 1039h:7012h
dd 	(NFORCE_DID << 16)  + NVIDIA_VID ; 10DEh:01B1h
dd 	(NFORCE2_DID << 16) + NVIDIA_VID ; 10DEh:006Ah
dd 	(AMD8111_DID << 16) + AMD_VID 	 ; 1022h:746Dh
dd 	(AMD768_DID << 16)  + AMD_VID 	 ; 1022h:7445h
dd 	(CK804_DID << 16) + NVIDIA_VID	 ; 10DEh:0059h
dd 	(MCP04_DID << 16) + NVIDIA_VID	 ; 10DEh:003Ah
dd 	(CK8_DID << 16) + NVIDIA_VID	 ; 1022h:008Ah
dd 	(NFORCE3_DID << 16) + NVIDIA_VID ; 10DEh:00DAh
dd 	(CK8S_DID << 16) + NVIDIA_VID	 ; 10DEh:00EAh

valid_id_count:	equ ($ - valid_ids)>>2 ; 05/11/2023

msgWavFileName:	db 0Dh, 0Ah, "WAV File Name: ",0
msgSampleRate:	db 0Dh, 0Ah, "Sample Rate: "
msgHertz:	db "00000 Hz, ", 0 
msg8Bits:	db "8 bits, ", 0 
;msgMono:	db "Mono", 0Dh, 0Ah, 0
msgMono:	db "Mono", 0
msg16Bits:	db "16 bits, ", 0 
;msgStereo:	db "Stereo"
msgStereo:	db "Stereo",0
nextline:	db 0Dh, 0Ah, 0

; 03/06/2017
hex_chars	db "0123456789ABCDEF", 0
msgAC97Info	db 0Dh, 0Ah
		db "AC97 Audio Controller & Codec Info", 0Dh, 0Ah 
		db "Vendor ID: "
msgVendorId	db "0000h Device ID: "
msgDevId	db "0000h", 0Dh, 0Ah
		db "Bus: "
msgBusNo	db "00h Device: "
msgDevNo	db "00h Function: "
msgFncNo	db "00h"
		db 0Dh, 0Ah
		db "NAMBAR: "
msgNamBar	db "0000h  "
		db "NABMBAR: "
msgNabmBar	db "0000h  IRQ: "
msgIRQ		dw 3030h
		db 0Dh, 0Ah, 0
; 25/11/2023
msgVRAheader:	db "VRA support: "
		db 0	
msgVRAyes:	db "YES", 0Dh, 0Ah, 0
msgVRAno:	db "NO ", 0Dh, 0Ah
		db "(Interpolated sample rate playing method)"
		db 0Dh, 0Ah, 0

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
	mov	bp,si
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

EOF: 

; BSS

; 17/02/2017
bss_start:

ABSOLUTE bss_start

alignb 2

; 30/05/2024
VRA:		resb 1	; Variable Rate Audio Support Status
		resb 1
; 28/11/2016

smpRBuff:	resw 14 ; 19/11/2016 - Erdogan Tan

filehandle:	resw 1

flags:		resb 1
; 06/11/2023
ac97_int_ln_reg: resb 1

; 30/05/2024
wav_file_name:
		resb 80 ; wave file, path name (<= 80 bytes)

		resw 1	

; 17/02/2017
; NAMBAR:  Native Audio Mixer Base Address Register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 10h-13h
; NABMBAR: Native Audio Bus Mastering Base Address register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 14h-17h
NAMBAR:		resw 1			; BAR for mixer
NABMBAR:	resw 1			; BAR for bus master regs

; 256 byte buffer for descriptor list
BDL_BUFFER:	resw 1			; segment of our 256byte BDL buffer
WAV_BUFFER1:	resw 1			; segment of our WAV storage
; 64k buffers for wav file storage
WAV_BUFFER2:	resw 1			; segment of 2nd wav buffer

; 12/11/2016 - Erdogan Tan

bus_dev_fn:	resd 1
dev_vendor:	resd 1

; 06/11/2023
sample_rate:	resw 1

; 19/11/2016
stmo:		resw 1 
bps:		resw 1

; 08/11/2023
; 07/11/2023
fbs_shift:	resb 1
		resb 1 ; 08/11/2023

;alignb 2

; 32 kilo bytes for temporay buffer
; (for stereo-mono, 8bit/16bit corrections)
;temp_buffer:	resb 32768
; 18/11/2023
temp_buffer:	resb 56304  ; (44.1 kHZ stereo 14076 samples)	

;alignb 16
bss_end: