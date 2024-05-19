; 19/05/2024
; 12/11/2023
; 11/11/2023
; 06/11/2023
; 05/11/2023
; 04/11/2023
; 03/11/2023
; PCI and AC97 codec functions for wav player
; Erdogan Tan (17/02/2017)

; ----------------------------------------------------------------------------
; PCI.ASM
; ----------------------------------------------------------------------------

; PCI device register reader/writers.
; NASM version: Erdogan Tan (29/11/2016)
; 		Last Update: 17/02/2017

;===============================================================
; 8/16/32bit PCI reader
;
; Entry: EAX=PCI Bus/Device/fn/register number
;           BIT30 set if 32 bit access requested
;           BIT29 set if 16 bit access requested
;           otherwise defaults to 8 bit read
;
; Exit:  DL,DX,EDX register data depending on requested read size
;
; Note: this routine is meant to be called via pciRegRead8, pciRegread16,
;	or pciRegRead32, listed below.
;
; Note2: don't attempt to read 32bits of data from a non dword aligned reg
;	 number. Likewise, don't do 16bit reads from non word aligned reg #
; 
pciRegRead:
	push	ebx
	push	cx
        mov     ebx, eax                        ; save eax, dh
        mov     cl, dh
        and     eax, (~PCI32)+PCI16             ; clear out data size request
        or      eax, BIT31                      ; make a PCI access request
        and     al, ~3 ; NOT 3                  ; force index to be dword

        mov     dx, PCI_INDEX_PORT
        out     dx, eax                         ; write PCI selector

        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3                           ; figure out which port to
        add     dl, al                          ; read to

	in      eax, dx                         ; do 32bit read
        test    ebx, PCI32
        jz      short _pregr1

        mov     edx, eax                        ; return 32bits of data
_pregr1:
	mov     dx, ax                          ; return 16bits of data
        test    ebx, PCI32+PCI16
        jnz     short _pregr2
        mov     dh, cl                          ; restore dh for 8 bit read
_pregr2:
        mov     eax, ebx                        ; restore eax
        and     eax, (~PCI32)+PCI16             ; clear out data size request
	pop	cx
	pop	ebx
	retn

pciRegRead8:
        and     eax, (~PCI16)+PCI32             ; set up 8 bit read size
        jmp     short pciRegRead		; call generic PCI access

pciRegRead16:
        and     eax, (~PCI16)+PCI32		; set up 16 bit read size
        or      eax, PCI16			; call generic PCI access
        jmp     short pciRegRead

pciRegRead32:
        and     eax, (~PCI16)+PCI32		; set up 32 bit read size
        or      eax, PCI32			; call generic PCI access
        jmp     short pciRegRead

;===============================================================
; 8/16/32bit PCI writer
;
; Entry: EAX=PCI Bus/Device/fn/register number
;           BIT31 set if 32 bit access requested
;           BIT30 set if 16 bit access requested
;           otherwise defaults to 8bit read
;        DL/DX/EDX data to write depending on size
;
;
; note: this routine is meant to be called via pciRegWrite8, pciRegWrite16,
; 	or pciRegWrite32 as detailed below.
;
; Note2: don't attempt to write 32bits of data from a non dword aligned reg
;	 number. Likewise, don't do 16bit writes from non word aligned reg #
;
pciRegWrite:
	push	ebx
	push	cx
        mov     ebx, eax                        ; save eax, dx
        mov     cx, dx
        or      eax, BIT31                      ; make a PCI access request
        and     eax, ~PCI16 ; NOT PCI16         ; clear out data size request
        and     al, ~3 ; NOT 3                  ; force index to be dword

        mov     dx, PCI_INDEX_PORT
        out     dx, eax                         ; write PCI selector

        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3                           ; figure out which port to
        add     dl, al                          ; write to

        mov     eax, edx                        ; put data into eax
        mov     ax, cx

        out     dx, al
        test    ebx, PCI16+PCI32                ; only 8bit access? bail
        jz      short _pregw1

        out     dx, ax                          ; write 16 bit value
        test    ebx, PCI16                      ; 16bit requested?  bail
        jnz     short _pregw1

        out     dx, eax                         ; write full 32bit
_pregw1:
        mov     eax, ebx                        ; restore eax
        and     eax, (~PCI32)+PCI16             ; clear out data size request
        mov     dx, cx                          ; restore dx
	pop	cx
	pop	ebx
	ret

pciRegWrite8:
        and     eax, (~PCI16)+PCI32		; set up 8 bit write size
        jmp     short pciRegWrite		; call generic PCI access

pciRegWrite16:
        and     eax, (~PCI16)+PCI32		; set up 16 bit write size
        or      eax, PCI16			; call generic PCI access
        jmp     short pciRegWrite

pciRegWrite32:
        and     eax, (~PCI16)+PCI32		; set up 32 bit write size
        or      eax, PCI32			; call generic PCI access
        jmp     short pciRegWrite

; 17/02/2017 (Modifed by Erdogan Tan for various ICH device IDs)
;===============================================================
; PCIFindDevice: scan through PCI space looking for a device+vendor ID
;
;  ENTRY: none
;; Entry: EAX=Device+Vendor ID
;
;  Exit: EAX=PCI address if device found
;	 EDX=Device+Vendor ID
;        CY clear if found, set if not found. EAX invalid if CY set.
;
; [old stackless] Destroys: ebx, esi, edi, cl
;
pciFindDevice:
	;push	cx
	;push	eax ; *
	;push	esi
	;push	edi

 	;mov     esi, eax                ; save off vend+device ID

	; 17/02/2017
	mov	si, valid_ids	; address of Valid ICH (AC97) Device IDs
	mov	cx, valid_id_count
pfd_0:
       	mov     edi, (80000000h - 100h) ; start with bus 0, dev 0 func 0
nextPCIdevice:
        add     edi, 100h
        cmp     edi, 80FFF800h		; scanned all devices?
        ;stc
        ;je 	short PCIScanExit       ; not found
	jb	short pfd_1
	mov     edi, 80000000h
	add	si, 4 ; scan for next device ID
	loop	pfd_1	 
	stc	
	;jmp 	short PCIScanExit
	retn
pfd_1:
        mov     eax, edi                ; read PCI registers
        call    pciRegRead32
        ;cmp    edx, esi                ; found device?
        cmp	edx, dword [si]
	jne     short nextPCIdevice
        ;clc
PCIScanExit:
	;pushf
	mov	eax, BIT31
	not	eax
	and	eax, edi		; return only bus/dev/fn #
	;popf

	;pop	edi
	;pop	esi
	;pop	edx ; *
	;pop	cx
	retn

; ----------------------------------------------------------------------------
; CODEC.ASM
; ----------------------------------------------------------------------------

; codec configuration code. Not much here really.
; NASM version: Erdogan Tan (29/11/2016)

; enable codec, unmute stuff, set output rate to 44.1
; entry: ax = desired sample rate

; 19/05/2024
; 11/11/2023
; 06/11/2023
%if 1

codecConfig:
	; 19/05/2024
	; 04/11/2023
	; 17/02/2017 
	; 07/11/2016 (Erdogan Tan)
	;PORT_NABM_GLB_CTRL_STAT equ 60h

	; 03/11/2023 (MPXPLAY, 'SC_ICH.C', ac97_init)
 	; 'AC97_DEF.H'
	;AC97_EXTENDED_STATUS equ 002Ah
	AC97_EA_SPDIF	equ 0002h
	AC97_EA_VRA	equ 0001h
	; 04/11/2023
	ICH_PO_CR_RESET equ 0002h  ; reset codec
	ICH_PCM_20BIT	equ 400000h ; 20-bit samples (ICH4)
	ICH_PCM_246_MASK equ 300000h ; 6 channels

	; 04/11/2023
init_ac97_controller:
	mov	eax, [bus_dev_fn]
	mov	al, PCI_CMD_REG
	call	pciRegRead16		; read PCI command register
	or      dl, IO_ENA+BM_ENA	; enable IO and bus master
	call	pciRegWrite16

	call	delay_100ms

	; 19/05/2024
	; ('PLAYMOD3.ASM', Erdogan Tan, 18/05/2024)

init_ac97_codec:
	; 19/05/2024
	mov	bp, 40
_initc_1:
	; 11/11/2023
	; (TRDOS 386 v2.0.5, 'audio.s')
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	in	eax, dx

	; 19/05/2024
	call	delay1_4ms

	; ?
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
	; 19/05/2024
	test	eax, CTRL_ST_CREADY
	jnz	short _ac97_codec_ready

	call	reset_ac97_codec
	jmp	short _initc_2

_ac97_codec_ready:
	mov	dx, [NAMBAR]
	;add	dx, 0 ; ac_reg_0 ; reset register
	out	dx, ax

	call	delay_100ms

	; 19/05/2024
	or	bp, bp
	jnz	short _ac97_codec_init_ok

	xor	eax, eax ; 0
	mov	dx, [NAMBAR]
	add	dx, CODEC_REG_POWERDOWN
	out	dx, ax

	; wait for 1 second
	mov	ecx, 1000 ; 1000*0.25ms = 1s
_ac97_codec_rloop:
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms
	call	delay_100ms

	;mov	dx, [NAMBAR]
	;add	dx, CODEC_REG_POWERDOWN
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
        ; 11/11/2023
	;mov	al, 2 ; force set 16-bit 2-channel PCM
	;mov	dx, GLOB_CNT_REG ; 2Ch
	;add	dx, [NABMBAR]
	;out	dx, eax

	;;call	delay1_4ms

	call 	reset_ac97_controller

	; 11/11/2023
	;call	delay1_4ms
	; 19/05/2024
	call	delay_100ms

	;call 	setup_ac97_codec

setup_ac97_codec:
	; 12/11/2023
	cmp	word [sample_rate], 48000
	je	short skip_rate

; 11/11/2023
; 05/11/2023
;%if 1
	AC97_EA_VRA equ BIT0 ; 11/11/2023

	; 19/05/2024
	call	delay1_4ms

	;; 19/05/2024
	;;mov	dx, [NAMBAR]
	;;add	dx, CODEC_EXT_AUDIO_REG	; 28h
	;;in	ax, dx
	;
	;; 19/05/2024
	;;test	al, 1 ; BIT0 ; Variable Rate Audio bit
	;;jz	short vra_not_supported
	;
	;; 19/05/2024
	;;call	delay1_4ms

	; 11/11/2023
	mov    	dx, [NAMBAR]
	add    	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah
	in     	ax, dx

	; 19/05/2024
	call	delay1_4ms
	
	and	al, ~BIT1 ; Clear DRA
	or	al, AC97_EA_VRA ; 1 ; 04/11/2023
	out	dx, ax			; Enable variable rate audio
	
	mov	cx, 10
check_vra:
	call	delay_100ms

	; 11/11/2023
	in	ax, dx
	test	al, AC97_EA_VRA ; 1
	jnz	short set_rate

	; 11/11/2023
	loop	check_vra

	; 19/05/2024
vra_not_supported:
	; 12/11/2023
	pop	ax ; discard return address to the caller
	mov	dx, msg_no_vra
	jmp	vra_err

set_rate:
	mov	ax, [sample_rate] ; 17/02/2017 (Erdogan Tan)

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch  	  
	out	dx, ax 			; PCM Front/Center Output Sample Rate

	call	delay_100ms

	; 12/11/2023
skip_rate:
	; 11/11/2023 (temporary)

	;mov   	dx, [NAMBAR]               	
	;add   	dx, CODEC_PCM_SURND_DACRATE_REG	; 2Eh  	  
	;out	dx, ax 			; PCM Surround Output Sample Rate

	;call	delay_100ms

	;mov   	dx, [NAMBAR]               	
	;add   	dx, CODEC_PCM_LFE_DACRATE_REG	; 30h  	  
	;out	dx, ax 			; PCM LFE Output Sample Rate

	;call	delay_100ms

	; 05/11/2023 (temporary)
	;mov	dx, [NAMBAR]               	
	;add	dx, CODEC_LR_ADCRATE_REG 	; 32h  	  
	;out	dx, ax 			; PCM Input Sample Rate
	;
	;call	delay_100ms

	mov	ax, 0202h
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_VOL_REG	;02h 
	out     dx, ax

	; 11/11/2023
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
 	;
  	;mov	dx, [NAMBAR]
  	;add	dx, CODEC_MASTER_MONO_VOL_REG	;06h 
  	;out	dx, ax

	; 11/11/2023
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
	;
	;mov	ax, 02h
  	;mov	dx, [NAMBAR]
  	;add	dx, CODEC_PCBEEP_VOL_REG	;0Ah 
  	;out	dx, ax

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

	;mov	ax, 0202h
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCM_OUT_REG		;18h 
  	out     dx, ax

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

	; 11/11/2023
	;mov	ax, 8008h ; Mute
  	;mov	dx, [NAMBAR]
	;add	dx, CODEC_PHONE_VOL_REG		;0Ch
	;			 ; AC97_PHONE_VOL ; TAD Input (Mono)
  	;out	dx, ax
	;
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
	;call	delay1_4ms

	;mov	ax, 0808h
	;mov	dx, [NAMBAR]
	;add	dx, CODEC_LINE_IN_VOL_REG ;10h ; Line Input (Stereo)
	;out	dx, ax
	;
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
	;call	delay1_4ms

  	;mov	dx, [NAMBAR]
        ;add	dx, CODEC_CD_VOL_REG ;12h ; CD Input (Stereo)
  	;out	dx, ax
	;
  	;mov	dx, [NAMBAR]
        ;add	dx, CODEC_AUX_VOL_REG ;16h ; Aux Input (Stereo)
  	;out	dx, ax
	;
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
	;call	delay1_4ms

	; 19/05/2024
	clc

;detect_ac97_codec:
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

%endif

; 11/11/2023
%if 0

codecConfig:
	; 06/11/2023
	; TUNELOOP version (playing without interrupt)
	; 04/11/2023
	; 17/02/2017 
	; 07/11/2016 (Erdogan Tan)

	mov	ax, [sample_rate] ; 17/02/2017 (Erdogan Tan)

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch  	  
	out	dx, ax 				; out sample rate
		
	; 05/11/2023 temp
	;mov	dx, [NAMBAR]               	
	;add	dx, CODEC_LR_ADCRATE_REG 	; 32h  	  
	;out	dx, ax 

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

	mov	eax, [dev_vendor]
        cmp	eax, (SI7012_DID<<16)+SIS_VID
        jne	short cConfig1

	; Unmute quirk specifically for the SiS7012

	CUSTOM_SIS_7012_REG equ 4Ch ; SiS7012-specific register
	
        mov     dx, [NABMBAR]
        add     dx, CUSTOM_SIS_7012_REG
        in      ax, dx
        or	al, 1
        out     dx, ax

cConfig1:
	; 03/11/2023 (MPXPLAY, 'SC_ICH.C', ac97_init)
	; initial ac97 volumes (and clear mute flag)
		
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_VOL_REG        ;02h ; STEREO
  	;xor	ax, ax ; volume attenuation = 0 (max. volume)
  	; 03/11/2023
	mov	ax, 0202h
	out     dx, ax

        call    delay1_4ms	; delays because codecs are slow
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
 
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCM_OUT_REG		;18h
  	;;xor	ax, ax
	;mov	ax, 0202h
  	out     dx, ax

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
 
	retn

%endif
