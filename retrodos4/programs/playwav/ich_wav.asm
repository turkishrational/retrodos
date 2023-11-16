; 11/11/2023
; 08/11/2023
; 06/11/2023
; 05/11/2023
; 04/11/2023
; 03/11/2023
; DOS based .WAV player using AC'97 and codec interface.
; ---------------------------------------------------------------
; NASM version: Erdogan Tan (29/11/2016)
; Last Update: 17/02/2017 (by Erdogan Tan)

; TUNELOOP version (playing without interrupt) - 06/11/2023 - Erdogan Tan

; ICHWAV.ASM

; player internal variables and other equates.
BUFFERSIZE      equ     64 * 1024       ; 64k file buffer size.
ENDOFFILE       equ     BIT0            ; flag for knowing end of file

;===========================================================================
; entry: none. File is already open and [filehandle] filled.
; exit:  not until the song is finished or the user aborts.
;
playWav:
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
        call    loadFromFile

	; and 64k into buffer 2
	mov     ax, [WAV_BUFFER2]
       	call    loadFromFile

; register reset the DMA engine. This may cause a pop noise on the output
; lines when the device is reset. Prolly a better idea to mute output, then
; reset.
;
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

	mov	eax, BUFFERSIZE / 2 ; size of half buffer (32K)
	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
	or	eax, BUP
	stosd

; 2nd buffer:

        movzx   eax, word [WAV_BUFFER2]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer2

; set length to 64k (32k of two 16 bit samples)
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	mov	eax, BUFFERSIZE / 2
	;or	eax, IOC + BUP
	; 06/11/2023 (TUNELOOP version, without interrupt)
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

;
; All set. Let's play some music.
;

	; 08/11/2023
	; 07/11/2023
	; 08/12/2016
	; 07/10/2016
        ;mov	al, 1
        mov	al, 31
	call	setLastValidIndex

	; 06/11/2023 (not neccessary)
	;; 05/11/2023
	;; reset current index
	;mov	al, 0
	;call	setCurrentIndex

	; 06/11/2023
	;mov	byte [tLoop], 1 ; 30/11/2016

	; 17/02/2017
        mov     dx, [NABMBAR]
        add     dx, PO_CR_REG                   ; PCM out Control Register
        ;mov	al, IOCE + RPBM	; Enable 'Interrupt On Completion' + run
	;			; (LVBI interrupt will not be enabled)
	; 06/11/2023 (TUNELOOP version, without interrupt)
	mov	al, RPBM
	out     dx, al                          ; Start bus master operation.

	; 06/11/2023
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k. Likewise when it's playing buffer 2, refresh buffer 1 and repeat.
   
; 06/11/2023 (TUNELOOP version, without interrupt)
%if 0
	; 08/12/2016
	; 28/11/2016
p_loop:
	call    check4keyboardstop      ; keyboard halt?
        jc	short r_loop 	; no ; 05/11/2023

	;mov	byte [tLoop], 0
	; 04/11/2023
	;mov	byte [audio_play_cmd], 0
	;call	ac97_stop
	;retn
_exit_:
	; 04/11/2023
	; finished with song, stop everything
	call	ac97_stop
	
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
	; 07/12/2016 - Erdogan Tan
	nop
	nop
	nop
	; 17/02/2017
	mov	al, [tBuff]
	dec	al ; 1-32 -> 0-31 or 0 -> 0FFh
	js	short  p_loop
	mov	byte [tBuff], 0 ; reset
	and	al, 1
	jnz	short q_loop
        mov     ax, [WAV_BUFFER2] ; [tBuff]=2 (from tuneLoop)
s_loop:
        call    loadFromFile
	;jc	short _exit_
	; 05/11/2023
	jc	short exit_loop
	jmp	short r_loop
q_loop:
     	mov     ax, [WAV_BUFFER1] ; [tBuff]=1 (from tuneLoop)
        ;call	loadFromFile
	;jc	short _exit_
	;jmp	short r_loop
	; 05/11/2023
	jmp	short s_loop

exit_loop: 
	mov	byte [tLoop], 0
	retn
		
tuneLoop:
	; 05/11/2023
	; 08/12/2016
	; 28/11/2016 - Erdogan Tan
	
	cmp	byte [tLoop], 1
	jb	short _exit_ ; 05/11/2023
_tlp_1:	
	; 17/02/2017
	call	getCurrentIndex
	inc	al ; 0-31 -> 1-32
	mov	[tBuff], al

	; 05/11/2023
	dec	ax
	dec	ax
	and	al, 1Fh
	call	setLastValidIndex

	; 05/11/2023
	; 17/02/2017 - Buffer switch test (temporary)
	push	ds
	push	si 
	mov	si, 0B800h ; video display page segment
	mov	ds, si
	sub	si, si ; 0
	mov	ah, 4Eh
	and	al, 1
	add	al, '1'
	mov	[si], ax ; show current play buffer (1, 2)
	pop	si
	pop	ds

	; 17/02/2017

	;test	byte [irq_status], LVBCI ; last buff completion intr.
	;jz	short _tlp2 ; BCIS ; Buffer completion interrupt

	; Last Valid Buffer Completion Interrupt (LVBCI).
	;   1 = Last valid buffer has been processed. 
	;	It remains active until cleared by software. 
	;	This bit indicates the occurrence of the event 
	;	signified by the last valid buffer being processed.
	;	Thus, this is an event status bit that can be cleared
	;	by software once this event has been recognized.
	;	This event causes an interrupt if the enable bit
	;	in the Control Register is set. The interrupt is
	;	cleared when the software clears this bit.
	;	In the case of Transmits (PCM out, Modem out) this bit
	;	is set, after the last valid buffer has been
	;	fetched (not after transmitting it).
	;	While in the case of Receives, this bit is set after
	;	the data for the last buffer has been written to memory.
	;   0 = Cleared by writing a "1" to this bit position.

	; Note: We are not using LVBCI 
	;     Last Buffer Completion Interrupt !!!

	; 17/02/2017
        ;mov     dx, [NABMBAR]
        ;add     dx, PO_SR_REG	; PCM out Status register
        ;mov     al, [irq_status]
        ;out     dx, al		; Clear (LVBCI) interrupt status
	;retn

;_tlp2:
	; Buffer Completion Interrupt Status (BCIS).
	;   1 =	Set by the hardware after the last sample 
	; 	of a buffer has been processed, AND if the Interrupt
	; 	on Completion (IOC) bit is set in the command byte of 
	; 	the buffer descriptor. It remains active
	; 	until cleared by software.
	;   0 =	Cleared by writing a "1" to this bit position.

	; 17/02/2017
        mov     dx, [NABMBAR]
        add     dx, PO_SR_REG	; PCM out Status register
        mov     al, [pcm_irq_status] ; 05/11/2023
        out     dx, al		; Clear (BCI) interrupt status
	retn

;_exit_:
;	mov	byte [tLoop], 0
;_exit:
;       ; finished with song, stop everything
;	mov     dx, [NABMBAR]		
;       add     dx, PO_CR_REG           ; PCM out Control Register
;       mov     al, 0
;       out     dx, al                  ; stop player
;_return:
;	retn

%endif

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k. Likewise when it's playing buffer 2, refresh buffer 1 and repeat.

; 08/11/2023
; 07/11/2023
%if 1

tuneLoop:
	; 08/11/2023
	; 06/11/2023
	mov	al, '1'
	call	tL0
tL1:
	call    updateLVI	; /set LVI != CIV/
	jz	short _exit_	; 08/11/2023
	call    check4keyboardstop
	jc	short _exit_
	call    getCurrentIndex
	test	al, BIT0
	jz	short tL1	; loop if buffer 2 is not playing

	; load buffer 1
	mov     ax, [WAV_BUFFER1]
	call	loadFromFile
	jc	short _exit_	; end of file

	mov	al, '2'
	call	tL0
tL2:
	call    updateLVI
	jz	short _exit_	; 08/11/2023
	call    check4keyboardstop
	jc	short _exit_
	call    getCurrentIndex
	test	al, BIT0
	jnz	short tL2	; loop if buffer 1 is not playing

	; load buffer 2
	mov     ax, [WAV_BUFFER2]
	call	loadFromFile
	jnc	short tuneLoop
_exit_:
	mov	dx, [NABMBAR]		
	add	dx, PO_CR_REG	; PCM out Control Register
	mov	al, 0
	out	dx, al		; stop player

	add	al, '0'
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

%endif

; 08/11/2023
; 07/11/2023
%if 0

tuneLoop:
	; 07/11/2023
	;mov	dx, NABMBAR
	;add	dx, PO_CIV_REG ; 14h
tL1:
	call    updateLVI	; set LVI != CIV
	call    check4keyboardstop
	jc	short _exit_
	call    getCurrentIndex
	test	al, BIT0
	jz	short tL1	; loop if buffer 2 is not playing

	; load buffer 1
	mov     ax, [WAV_BUFFER1]
	call	loadFromFile
	jc	short _exit_	; end of file
tL2:
	call    updateLVI
	call    check4keyboardstop
	jc	short _exit_
	call    getCurrentIndex
	test	al, BIT0
	jnz	short tL2	; loop if buffer 1 is not playing

	; load buffer 2
	mov     ax, [WAV_BUFFER2]
	call	loadFromFile
	jnc	short tuneLoop
_exit_:
	mov	dx, [NABMBAR]		
	add	dx, PO_CR_REG	; PCM out Control Register
	mov	al, 0
	out	dx, al		; stop player
	retn

%endif

; load data from file in 32k chunks. Would be nice to load 1 64k chunk,
; but in DOS you can only load FFFF bytes at a time.
;
; entry: ax = segment to load data to
; exit: CY set if end of file reached.
; note: last file buffer is padded with 0's to avoid pops at the end of song.
; assumes file is already open. uses [filehandle]

; 07/11/2023
%if 0

loadFromFile:
	; 07/11/2023
	; 06/11/2023
	; 17/02/2017
	;push	ax
	;push	cx
	;push	dx
	;push	bx

	;push	es
	;push	ds
	
        test    byte [flags], ENDOFFILE	; have we already read the
	;stc				; last of the file?
	; 05/11/2023
	;jnz	endLFF
	jz	short lff_12	; 06/11/2023
	; 06/11/2023
	;;mov	byte [tLoop], 0
	;jmp	endLFF
	stc
	retn

lff_12:	; 06/11/2023    
	mov	[fbs_seg], ax ; save buffer segment
	xor	dx, dx
lff_0:
	mov	[fbs_off], dx ; buffer offset

	mov     bx, (BUFFERSIZE / 2)	; 32k chunk
	mov	cl, [fbs_shift]   
	and	cl, cl
	jz	short lff_1 ; stereo, 16 bit	

	; fbs_shift =
	;	2 for mono and 8 bit sample (multiplier = 4)
	;	1 for mono or 8 bit sample (multiplier = 2)
	shr	bx, cl ; 32K / multiplier

	mov	ax, cs
	mov	dx, temp_buffer ; temporary buffer for wav data
lff_1:
	; 17/02/2017 (stereo/mono, 8bit/16bit corrections)
	; load file into memory
        mov	cx, bx                         
	mov	bx, [filehandle]
	mov     ds, ax
       	mov	ah, 3fh
	int	21h

	mov	bx, cs
	mov	ds, bx

	jc	short lff_9 ; error !

	and	ax, ax
	jz	short lff_10
	
	mov	bl, [fbs_shift] ; shift count  
	or	bl, bl
	jz	short lff_7 ; 16 bit stereo samples

	push	es
	; 06/11/2023
	;push	di
	;push	si
	mov	di, [fbs_off]
	mov	si, [fbs_seg] ; buffer segment
	mov	es, si
	mov	si, temp_buffer ; temporary buffer address
	mov	cl, [bps] ; bits per sample (8 or 16)
	cmp	cl, 8
	jne	short lff_4 ; 16 bit samples
	; 8 bit samples
	mov	cx, ax ; byte count
	dec	bl  ; shift count, 1 = stereo, 2 = mono
	jz	short lff_3 ; 8 bit, stereo
lff_2:
	; mono & 8 bit
	lodsb
	shl	ax, 8 ; convert 8 bit sample to 16 bit sample
	stosw	; left channel
	stosw	; right channel
	loop	lff_2
	jmp	short lff_6	
lff_3:
	; stereo & 8 bit
	lodsb
	shl	ax, 8 ; convert 8 bit sample to 16 bit sample
	stosw
	loop	lff_3			
	jmp	short lff_6
lff_4:
	; 16 bit mono samples
	mov	cx, ax ; word count
lff_5:	
	lodsw
	stosw	; left channel
	stosw	; right channel
	loop	lff_5
lff_6:
	mov	ax, di ; save next buffer offset/position
	; 06/11/2023
	;pop	si
	;pop	di
	pop	es
;lff_7:        
	; 06/11/2023
	and	ax, ax
	jz	short endLFF ; end of 2nd half
	mov	cx, (BUFFERSIZE / 2)
lff_7:
	cmp	ax, cx
	je	short endLFF	 ; 06/11/2023
	;jb	short lff_11
	;xor	cx, cx
	;sub	cx, ax
	;neg	cx
	jmp	short lff_11
lff_8:
	; 07/11/2023
	cmp	word [fbs_off], (BUFFERSIZE / 2) ; 32768
	jnb	short endLFF
	
	mov	dx, ax ; buffer offset
	mov	ax, [fbs_seg] ; buffer segment
	jmp	lff_0
lff_9:  
	; 07/11/2023
	;; 06/11/2023 (temporary)
	;mov	al, '!'  ; error
	;call	tL0

	xor	ax, ax
lff_10:
	; 07/11/2023
	;mov	cx, (BUFFERSIZE / 2)  
lff_11:
	call    padfill				; blank pad the remainder
        ;clc					; don't exit with CY yet.
        or	byte [flags], ENDOFFILE		; end of file flag
endLFF:
        ;pop	ds
	;pop	es
	; 06/11/2023
	;pop	bx
	;pop	dx
        ;pop	cx
        ;pop	ax
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
%if 0
	; 07/11/2023
updateLVI:
	push	ax
	push	dx
	; 06/11/2023
	mov	dx, [NABMBAR]
	add	dx, PO_CIV_REG
	; (Current Index Value and Last Valid Index value)
	in	ax, dx

	cmp	al, ah ; is current index = last index ?
	jne	short uLVI1

	call	setNewIndex
uLVI1:
	pop	dx
	pop	ax
	retn

setNewIndex:
	; 07/11/2023
	push	ax
        call    getCurrentIndex                 ; get CIV
        test    byte [flags], ENDOFFILE
        jnz     short sni_1
        ; not at the end of the file yet.
        dec     al                              ; make new index <> current
        and     al, INDEX_MASK                  ; make sure new value is 0-31
sni_1:
        call    setLastValidIndex               ; write new value
        clc
        pop	ax
	retn

%endif	

; 08/11/2023
; 07/11/2023
;%if 1
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

;%endif
	
;input AL = index # to stop on
setLastValidIndex:
	; 08/11/2023
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	;pop	dx
	retn

; 06/11/2023
;	; 05/11/2023
;setCurrentIndex:
;	;push	dx
;	mov	dx, [NABMBAR]
;	add	dx, PO_CIV_REG
;	out	dx, al
;	;pop	dx
;	retn

; checks if either shift key has been pressed. Exits with CY if so.
; 
check4keyboardstop:

; 06/11/2023
%if 1
	; 08/11/2023
	; 04/11/2023
	mov	ah, 1
	int	16h
	jz	short _cksr
	xor	ah, ah
	int	16h
	stc
_cksr:
	retn
%endif

; 06/11/2023
; 04/11/2023
%if 0	
        push    ds
        push    0
        pop     ds		       ; examine BDA for keyboard flags
        test    byte [417h], (BIT0 | BIT1)
        pop     ds
        stc
        jnz	short _cksr ; 07/11/2023
	;jz	short _cksr ; 06/11/2023
        clc
_cksr:
        retn
%endif