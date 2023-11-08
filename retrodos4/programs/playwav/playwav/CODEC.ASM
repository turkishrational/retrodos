; codec configuration code.  Not much here really.
; NASM version: Erdogan Tan (29/11/2016)

; 20/03/2017
; enable codec, unmute stuff, set output rate to 44.1
; entry: ax = desired sample rate
;
codecConfig:
	; 15/11/2016
	; 14/11/2016
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, 'setup_codec', codec.inc)

	; 14/11/2016 - Erdogan Tan	
	; (Ref: Mpxplay, PDSoft, Attila Padar, SC_VIA82.C)
;;;	call	channel_reset

	mov	byte [err_num], 0

	mov     eax, 0202h
	mov	edx, CODEC_MASTER_VOL_REG ; 02h ; Line Out
	call	codec_write
	;jc	cconfig_error

	mov	byte [err_num], 1

	mov     eax, 0202h
	mov	edx, CODEC_PCM_OUT_REG ; 18h ; Wave Output (Stereo)
	call	codec_write
	;jc	cconfig_error
      
	mov	byte [err_num], 2

 	;xor    eax, eax
	mov	eax, 0202h
	mov	edx, CODEC_AUX_VOL ; 04h ; CODEC_HP_VOL_REG ; HeadPhone
	call	codec_write
	;jc	cconfig_error

	mov	byte [err_num], 3

        mov     ax,  08h
        mov	edx, 0Ch  ; AC97_PHONE_VOL ; TAD Input (Mono)
	call	codec_write
	;jc	short cconfig_error

	mov	byte [err_num], 4

        mov     ax,  0808h
        mov	edx, CODEC_LINE_IN_VOL_REG ; 10h ; Line Input (Stereo)	
	call	codec_write
	;jc	short cconfig_error

	mov	byte [err_num], 5

	mov     ax,  0808h
        mov	edx, CODEC_CD_VOL_REG ; 12h ; CR Input (Stereo)
	call	codec_write
	;jc	short cconfig_error

	mov	byte [err_num], 6

	mov     ax,  0808h
        mov	edx, CODEC_AUX_VOL_REG ; 16h ; Aux Input (Stereo)
	call	codec_write
	;jc	short cconfig_error

	mov	byte [err_num], 7

	; Extended Audio Status (2Ah)
	mov	eax, CODEC_EXT_AUDIO_CTRL_REG ; 2Ah 
	call	codec_read
        and     eax, 0FFFFh - 2			; clear DRA (BIT1)
        ;or     eax, 1				; set VRA (BIT0)
	or	eax, 5  	; VRA (BIT0) & S/PDIF (BIT2) ; 14/11/2016
	mov	edx, CODEC_EXT_AUDIO_CTRL_REG
	call	codec_write
	;jc	short cconfig_error

	mov	byte [err_num], 8
set_sample_rate:
        xor	eax, eax
	mov	ax, [sample_rate]
	mov	edx, CODEC_PCM_FRONT_DACRATE_REG ; 2Ch ; PCM Front DAC Rate
        ;call	codec_write
        ;retn
	jmp	codec_write
	
cconfig_error:
	mov	al, [err_num]
        retn

reset_codec:
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
	mov	eax, [bus_dev_fn]
 	mov	al, VIA_ACLINK_CTRL
       	mov	dl, VIA_ACLINK_CTRL_ENABLE + VIA_ACLINK_CTRL_RESET + VIA_ACLINK_CTRL_SYNC

	call	pciRegWrite8

	call	delay_100ms 	; wait 100 ms
_rc_cold:
        call    cold_reset
        jnc     short _reset_codec_ok

        xor     eax, eax         ; timeout error
        retn

_reset_codec_ok:
        xor     eax, eax
        ;mov	al, VIA_ACLINK_C00_READY ; 1
        inc	al
	retn

cold_reset:
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
	;mov	eax, [bus_dev_fn]
	;mov	al, VIA_ACLINK_CTRL
	xor	dl, dl ; 0
	call	pciRegWrite8

	call	delay_100ms 	; wait 100 ms

	;; ACLink on, deassert ACLink reset, VSR, SGD data out
        ;; note - FM data out has trouble with non VRA codecs !!
        
	;mov	eax, [bus_dev_fn]
	;mov	al, VIA_ACLINK_CTRL

	mov	dl, VIA_ACLINK_CTRL_INIT

	call	pciRegWrite8

	mov	ecx, 16	; total 2s

_crst_wait:
	push	ecx

	;mov	eax, [bus_dev_fn]
	mov	al, VIA_ACLINK_STAT
	call	pciRegRead8	

        test    dl, VIA_ACLINK_C00_READY
        jnz     short _crst_ok

	call	delay_100ms

	pop	ecx

        dec     ecx
        jnz     short _crst_wait

_crst_fail:
        stc
        retn

_crst_ok:
	pop	ecx

	; are these necessary ? - 14/11/2016 - Erdogan Tan

	;mov	eax, [bus_dev_fn]
	;mov	al, VIA_ACLINK_CTRL
	call	pciRegRead8

	;mov	eax, [bus_dev_fn]
	;mov	al, VIA_ACLINK_STAT
	call	pciRegRead8

	movzx	eax, dl
	and     al, VIA_ACLINK_C00_READY ; 1
	jz	short _crst_fail

	retn

delay_100ms:
	; 20/03/2017
	; wait 100 ms
	mov	ecx, 25
_delay_x_ms:
	;push	ecx
	call	delay1_4ms
	;pop	ecx
        loop	_delay_x_ms
	retn

codec_io_w16: ;w32
        mov	dx, [ac97_io_base]
        add     dx, VIA_REG_AC97
        out     dx, eax
        retn

codec_io_r16: ;r32
        mov     dx, [ac97_io_base]
        add     dx, VIA_REG_AC97
        in      eax, dx
        retn

ctrl_io_w8:
        add     dx, [ac97_io_base]
        out     dx, al
        retn

ctrl_io_r8:
        add     dx, [ac97_io_base]
        in      al, dx
        retn

ctrl_io_w32:
        add     dx, [ac97_io_base]
        out     dx, eax
        retn

ctrl_io_r32:
        add	dx, [ac97_io_base]
        in	eax, dx
        retn

codec_read:
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
        ; Use only primary codec.
        ; eax = register
        shl     eax, VIA_REG_AC97_CMD_SHIFT
        or      eax, VIA_REG_AC97_PRIMARY_VALID + VIA_REG_AC97_READ

	call    codec_io_w16

      	; codec_valid
	call	codec_check_ready
        jnc	short _cr_ok

	retn

_cr_ok:
	; wait 25 ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

        call    codec_io_r16
        and     eax, 0FFFFh
        retn

codec_write:
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
        ; Use only primary codec.
        
	; eax = data (volume)
	; edx = register (mixer register)
	
	shl     edx, VIA_REG_AC97_CMD_SHIFT

        shl     eax, VIA_REG_AC97_DATA_SHIFT ; shl eax, 0
        or      edx, eax

        mov     eax, VIA_REG_AC97_CODEC_ID_PRIMARY
        shl     eax, VIA_REG_AC97_CODEC_ID_SHIFT
        or      eax, edx

        call    codec_io_w16
        ;mov    [codec.regs+esi], ax

        ;call	codec_check_ready
       	;retn
	;jmp	short _codec_check_ready	

codec_check_ready:
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)

_codec_check_ready:
	mov	ecx, 20	; total 2s
_ccr_wait:
	push	ecx

        call    codec_io_r16
        test    eax, VIA_REG_AC97_BUSY
        jz      short _ccr_ok

	call	delay_100ms

	pop	ecx

	dec     ecx
        jnz     short _ccr_wait

        stc
        retn

_ccr_ok:
	pop	ecx
	and     eax, 0FFFFh
        retn

channel_reset:
	; 14/11/2016 - Erdogan Tan
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
        mov	edx, VIA_REG_OFFSET_CONTROL
        mov     eax, VIA_REG_CTRL_PAUSE + VIA_REG_CTRL_TERMINATE + VIA_REG_CTRL_RESET
        call    ctrl_io_w8

        ;mov	edx, VIA_REG_OFFSET_CONTROL
        ;call   ctrl_io_r8

	mov	ecx, 12 ; 50 ms	
_ch_rst_wait:
	push	ecx
	call	delay1_4ms
	pop	ecx
	dec	ecx
	jnz	short _ch_rst_wait     

        ; disable interrupts
        mov	edx, VIA_REG_OFFSET_CONTROL
        xor     eax, eax
        call    ctrl_io_w8

        ; clear interrupts
        mov	edx, VIA_REG_OFFSET_STATUS
	;mov	eax, 3
        mov     eax, 0FFh ; 14/11/2016 - SC_VIA82.C (Attila Padar)
        call	ctrl_io_w8

	; 14/11/2016 (Ref: Attila Padar, Mpxplay, SC_VIA82.C)
	mov	edx, VIA_REG_OFFSET_CURR_PTR
	xor	eax, eax
	call	ctrl_io_w32

        retn