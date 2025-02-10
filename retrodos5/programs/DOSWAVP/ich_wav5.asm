; 04/02/2025
; 03/02/2025
; 02/02/2025
; 19/05/2024
; 19/11/2023
; 17/11/2023 - 18/11/2023
; 15/11/2023 - 16/11/2023
; 13/11/2023 - 14/11/2023
; 03/11/2023 - 11/11/2023
; DOS based .WAV player using AC'97 and codec interface.
; ---------------------------------------------------------------
; NASM version: Erdogan Tan (29/11/2016)
; Last Update: 17/02/2017 (by Erdogan Tan)

; AC97 interrupt version - 09/11/2023 - Erdogan Tan
; sample rate conversion version - 13/11/2023 - Erdogan Tan
; LVI interrupt version (instead of IOC) - 18/11/2023 - Erdogan Tan

; ICHWAV.ASM

; player internal variables and other equates.
BUFFERSIZE      equ     64 * 1024       ; 64k file buffer size.
ENDOFFILE       equ     BIT0            ; flag for knowing end of file

;===========================================================================
; entry: none. File is already open and [filehandle] filled.
; exit:  not until the song is finished or the user aborts.
;
playWav:
	; 13/11/2023

	cmp	byte [VRA], 1
	jb	short chk_sample_rate
playwav_48_khz:	
	mov	word [loadfromwavfile], loadFromFile
	;mov	word [loadsize], 0 ; 65536
	mov	word [buffersize], 32768 ; samples
	jmp	playWav_vra

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
	jmp	short playWav_vra

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
	mov	dx, msg_no_vra
	jmp	vra_err

	; 13/11/2023
loadfromwavfile:
	dw	loadFromFile
loadsize:	; read from wav file
	dw	0
buffersize:	; write to DMA buffer
	dd	32768 ; 16 bit samples (not bytes)

playWav_vra:
	; 07/11/2023
	; clear buffer 2
        ;push	es
	;mov	ax, [WAV_BUFFER2]
	;mov	es, ax
	;sub	ax, ax
	;mov	di, ax ; 17/02/2017
	;mov	cx, (BUFFERSIZE/2)
	;rep	stosw
	;pop	es	     
	
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

; register reset the DMA engine. This may cause a pop noise on the output
; lines when the device is reset. Prolly a better idea to mute output, then
; reset.

	; 11/11/2023
        ;mov	dx, [NABMBAR]
        ;add	dx, PO_CR_REG		; set pointer to Ctrl reg
        ;mov	al, RR			; set reset
	;out	dx, al			; self clearing bit

; write last valid index to 31 to start with.
; The Last Valid Index register tells the DMA engine when to stop playing.
; 
; As we progress through the song we change the last valid index to always be
; something other than the index we're currently playing.  
;
	; 08/11/2023
	;; 07/11/2023
	;mov	al, 1
        ;;mov	al, 31
	;call	setLastValidIndex

; create Buffer Descriptor List
;
; A buffer descriptor list is a list of pointers and control bits that the
; DMA engine uses to know where to get the .wav data and how to play it.
;
; I set it up to use only 2 buffers of .wav data, and whenever 1 buffer is
; playing, I refresh the other one with good data.
;
;
; For the control bits, you can specify that the DMA engine fire an interrupt
; after a buffer has been processed, but I poll the current index register
; to know when it's safe to update the other buffer.
;
; I set the BUP bit, which tells the DMA engine to just play 0's (silence)
; if it ever runs out of data to play. Good for safety.
;
        push    es
        mov     ax, [BDL_BUFFER]		; get segment # for BDL
        mov     es, ax

        mov     cx, 32 / 2                      ; make 32 entries in BDL
        xor     di, di                          
_0:

; set buffer descriptor 0 to start of data file in memory
        movzx   eax, word [WAV_BUFFER1]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer1

;
; set length to 32k samples. 1 sample is 16bits or 2bytes.
; Set control (bits 31:16) to BUP, bits 15:0=number of samples.
; 

; 17/02/2017 (Erdogan Tan)
; Intel® 82801AA (ICH) & Intel® 82801AB (ICH0) I/O Controller Hub AC’97
; Programmer’s Reference Manual

; 2.2.1 Buffer Descriptor List  (on Page 13)
	;
	;  Generic Form of Buffer Descriptor
	;  ---------------------------------
	;  63   62    61-48    47-32   31-0
	;  ---  ---  --------  ------- -----
	;  IOC  BUP -reserved- Buffer  Buffer
	;		      Length   Pointer
	;		      [15:0]   [31:0]
	;
	;  IOC:	Interrupt On Completion. 
	;	1 = Enabled. 
	;	    When this is set, it means that the controller should
	;	    issue an interrupt upon completion of this buffer.
	;	    It should also set the IOC bit in the status register
	;	0 = Disabled	
	;
	;  BUP: Buffer Underrun Policy.
	;       0 = When this buffer is complete,
	;	    if the next buffer is not yet ready 
	;	    (i.e., the last valid buffer has been processed),
	;	    then continue to transmit the last valid sample.
	;	1 = When this buffer is complete,
	;     	    if this is the last valid buffer, transmit zeros after
	;	    this buffer has been processed completely.
	;	    This bit typically is set only if this is the last 
	;	    buffer in the current stream.
	;
	; [31:0]: Buffer pointer. This field points to the location of
	;	  the data buffer. Since samples can be as wide as one
	;	  word, the buffer must be aligned with word boundaries,
	;	  to prevent samples from straddling DWord boundaries.
	;
	; [15:0]: Buffer Length: This is the length of the data buffer,
	;	  in number of samples. The controller uses this data
	;	  to determine the length of the buffer, in bytes.
	;	  "0" indicates no sample to process.

; ICH2AC97.INC

;	IOC	equ     BIT31   ; Fire an interrupt whenever this
				; buffer is complete.

;	BUP	equ     BIT30   ; Buffer Underrun Policy.
				; if this buffer is the last buffer
				; in a playback, fill the remaining
				; samples with 0 (silence) or not.
				; It's a good idea to set this to 1
				; for the last buffer in playback,
				; otherwise you're likely to get a lot
				; of noise at the end of the sound.
;
; Bits 15:0 contain the length of the buffer, in number of samples, which
; are 16 bits each, coupled in left and right pairs, or 32bits each.
; Luckily for us, that's the same format as .wav files.
;
; A value of FFFF is 65536 samples. Running at 44.1Khz, that's just about
; 1.5 seconds of sample time. FFFF * 32bits is 1FFFFh bytes or 128k of data.
;
; A value of 0 in these bits means play no samples.
;

; ICHWAV.ASM
				    ; 19/05/2024
	;mov	eax, BUFFERSIZE / 2 ; size of buffer (32K) in (16bit) words
	; 13/11/2023
	mov	eax, [buffersize]

	; 18/11/2023
	; 09/11/2023
	;or	eax, IOC + BUP
	; 06/11/2023
	or	eax, BUP
	stosd

; 2nd buffer:

        movzx   eax, word [WAV_BUFFER2]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer2

; set length to 64k (32k of two 16 bit samples)
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	;mov	eax, BUFFERSIZE / 2
	; 13/11/2023
	mov	eax, [buffersize]

	; 18/11/2023
	; 09/11/2023
	;or	eax, IOC + BUP
	; 06/11/2023
	or	eax, BUP
	stosd

        loop    _0
        pop     es

;
; tell the DMA engine where to find our list of Buffer Descriptors.
; this 32bit value is a flat mode memory offset (ie no segment:offset)
;
; write NABMBAR+10h with offset of buffer descriptor list
;
        movzx   eax, word [BDL_BUFFER]
	shl     eax, 4                          ; convert seg:off to 0:off
        mov     dx, [NABMBAR]
        add     dx, PO_BDBAR_REG                ; set pointer to BDL
        out     dx, eax                         ; write to AC97 controller

	; 19/05/2024
	call	delay1_4ms

;
; All set. Let's play some music.
;

	; 18/11/2023
	; 10/11/2023
	; 08/11/2023
	; 07/11/2023
	; 08/12/2016
	; 07/10/2016
	mov	al, 1	; 18/11/2023
	;mov	al, 31
	mov	[LVI], al ; 10/11/2023
	call	setLastValidIndex

	; 06/11/2023 (not neccessary)
	;; 05/11/2023
	;; reset current index
	;mov	al, 0
	;call	setCurrentIndex

	; 06/11/2023
	;mov	byte [tLoop], 1 ; 30/11/2016

	; 19/05/2024
	call	delay1_4ms

	; 17/02/2017
        mov     dx, [NABMBAR]
        add     dx, PO_CR_REG                   ; PCM out Control Register
        ; 18/11/2023
	mov	al, LVBIE + RPBM ; Enable LVBI interrupt and run bus master
	; 09/11/2023
	;mov	al, IOCE + RPBM	; Enable 'Interrupt On Completion' + run
	;			; (LVBI interrupt will not be enabled)
	; 06/11/2023 (TUNELOOP version, without interrupt)
	;mov	al, RPBM
	out     dx, al                          ; Start bus master operation.

	; 09/11/2023
	mov	byte [b_indicator], '?'	

	; 19/05/2024
	; 06/11/2023
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

	; 10/11/2023
	;mov	byte [tloop], 1

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k. Likewise when it's playing buffer 2, refresh buffer 1 and repeat.

; 04/02/2025
; 09/11/2023   
; 06/11/2023
%if 1
	; 08/12/2016
	; 28/11/2016
p_loop:
	call    check4keyboardstop ; keyboard halt?
        jnc	short r_loop 	   ; no ; 09/11/2023

	; 09/11/2023
	;or	byte [flags], ENDOFFILE
	;mov	byte [b_indicator], '0'
q_loop:
	; 04/02/2025
	mov	al, '0'
	call	tL0

	; 04/11/2023
	; finished with song, stop everything
	call	ac97_stop

	; 11/11/2023
irq_restore:	
	; restore previous interrupt vector and interrupt_status
	cli
	in	al, 0A1h ; irq 8-15
	mov	al, [IRQ_status+1]
	out	0A1h, al 
	in	al, 021h ; irq 0-7
	mov	al, [IRQ_status]
	out	21h, al 
	; ...
	push	es
	xor	ax, ax
	mov	es, ax
	mov	ax, [IRQ_vector]
	mov	[es:bx], ax
	mov	ax, [IRQ_vector+2]
	mov	[es:bx+2], ax
	pop	es
	sti
	retn

r_loop:
	; 19/05/2024
	; 10/11/2023
	nop
	nop
	nop

	; 04/02/2025
	; 11/11/2023
	;mov	al, '0'
	cmp	byte [tloop], 0
	je	short p_loop
	jl	short q_loop

	dec	byte [tloop] ; 0

	cmp	byte [b_indicator], '1' ; will be played
	jne	short s_loop
	
	; 19/11/2023
	; load buffer 1
	mov	ax, [WAV_BUFFER1] ; will be played
u_loop:
	;call	loadFromFile
	; 13/11/2023
	call	word [loadfromwavfile]
	;jnc	short v_loop
	; 04/02/2025
	jc	short q_loop
	
	; 19/11/2023
	;cmp	byte [tloop], -1
	;jne	short v_loop
	
	; 04/02/2025
	;mov	al, '0'
	;jmp	short q_loop
v_loop:
	; 09/11/2023 - Erdogan Tan
	; (print buffer number on top left of the screen)
	mov	al, [b_indicator] ; going to be played
	call	tL0
	jmp	short p_loop
s_loop:
	; load buffer 2
	mov	ax, [WAV_BUFFER2] ; will be played
	jmp	short u_loop

%endif

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k. Likewise when it's playing buffer 2, refresh buffer 1 and repeat.

; 18/11/2023
%if 1

tuneLoop:
	; 04/02/2025
	; 19/11/2023
	; 18/11/2023
	test	byte [flags], ENDOFFILE
	jnz	short tL3

	; 19/11/2023
	mov	byte [tloop], 1
	
	; set LVI to the next
	; (and then load audio data in that buffer) 
	mov	al, [LVI]
	inc	ax
	and	al, 1Fh
	call	setLastValidIndex
	;mov	[LVI], al
tL1:
	; al = next buffer index (not for the buffer being played)
	test	al, BIT0
	jnz	short tL2

	; 19/11/2023
	; load buffer 1
	;mov	ax, [WAV_BUFFER1] ; will be played
	;call	word [loadfromwavfile]
	;jc	short tL3

	mov	byte [b_indicator], '1'
	retn
tL2:
	; 19/11/2023
	; load buffer 2
	;mov	ax, [WAV_BUFFER2] ; will be played
	;call	word [loadfromwavfile]
	;jc	short tL3

	mov	byte [b_indicator], '2'
	retn
tL3:
	mov	byte [tloop], -1
	stc
	retn

	; 18/11/2023
tL0:
	; al = buffer indicator ('1', '2' or '0' -stop- )
	push	ds
	; 04/02/2025
	push	bx 
	mov	bx, 0B800h ; video display page segment
	mov	ds, bx
	sub	bx, bx ; 0
	mov	ah, 4Eh
	mov	[bx], ax ; show current play buffer (1, 2)
	pop	bx
	pop	ds
	retn

%endif

; 11/11/2023
; 10/11/2023
; 09/11/2023
; 08/11/2023
; 07/11/2023
%if 0	; 18/11/2023

tuneLoop:
	; 11/11/2023
	; 10/11/2023
	; 09/11/2023
	; 08/11/2023
	; 06/11/2023
	;mov	al, '1'
	;call	tL0
tL1:
	; 10/11/2023
	;mov	byte [tloop], 1

	; 11/11/2023
	;call    updateLVI	; set LVI != CIV
	;jz	short tL4	

	; 11/11/2023
	mov	byte [tloop], 1

	call    getCurrentIndex

	; 10/11/2023
	cmp	al, [LVI]
	jne	short tL2

	test	byte [flags], ENDOFFILE
	;jnz	short tL2
	jnz	short tL4

	dec	ax
	and	al, 1Fh 
	call	setLastValidIndex	
	xchg	al, [LVI]
tL2:
	test	al, BIT0
	jz	short tL3

	mov	byte [b_indicator], '1'
	retn
tL3:	
	mov	byte [b_indicator], '2'
	retn
tL4:
	; 11/11/2023
	mov	byte [tloop], -1
	stc
	retn

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

%endif

; 08/11/2023
; 07/11/2023
%if 1

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

lff_4:
	; 08/11/2023
	mov	al, '!'  ; error
	call	tL0

	xor	ax, ax
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

%endif

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

; examines the CIV and the LVI. When they're the same, we set LVI <> CIV
; that way, we never run out of buffers to play

; 08/11/2023
; 07/11/2023
%if 0
updateLVI:
	; 06/11/2023
	mov	dx, [NABMBAR]      		
	add	dx, PO_CIV_REG
	; (Current Index Value and Last Valid Index value)
	in	ax, dx

	cmp	al, ah ; is current index = last index ?
	jne	short uLVI2

	; 10/11/2023
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

	;test	al, 2
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

%endif
	
;input AL = index # to stop on
setLastValidIndex:
	; 08/11/2023
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	; 18/11/2023
	mov	[LVI], al
	;pop	dx
	retn

; checks if either shift key has been pressed. Exits with CY if so.
; 
check4keyboardstop:

; 06/11/2023
%if 1
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

; 19/05/2024
volume:	db 02h

%endif

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
	jc	short lff12m_7 ; error !

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