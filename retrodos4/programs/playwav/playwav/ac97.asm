; 06/11/2023
; 05/11/2023
; 04/11/2023
; 03/11/2023
; PCI and AC97 codec functions for wav player
; Erdogan Tan (17/02/2017)

; TUNELOOP version (playing without interrupt) - 06/11/2023 - Erdogan Tan
 
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
;           otherwise defaults to 8bit read
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

; 06/11/2023
%if 0

codecConfig:
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

delay_100ms:
	; 29/05/2017
	; 24/03/2017 ('codec.asm')
	; wait 100 ms
	mov	ecx, 400  ; 400*0.25ms
_delay_x_ms:
	call	delay1_4ms
        loop	_delay_x_ms

init_ac97_codec:
	; 04/11/2023
	; reset codec
	mov     dx, [NABMBAR]
	add	dx, PO_CR_REG
	in	al, dx
	or	al, ICH_PO_CR_RESET
	out	dx, al

	call	delay1_4ms

	; set channels (2) and bits (16/32)
	mov     dx, [NABMBAR]
	add	dx, GLOB_CNT_REG
	in	eax, dx
	and	eax, ~(ICH_PCM_246_MASK | ICH_PCM_20BIT)
	out	dx, eax

	; 05/11/2023
_ssr:

; 05/11/2023
;%if 1
	AC97_EA_VRA equ 0001h ; 04/11/2023

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah  	  
	in     	ax, dx
	or	al, AC97_EA_VRA ; 1 ; 04/11/2023
	out	dx, ax 				; Enable variable rate audio

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

	mov	ax, [sample_rate] ; 17/02/2017 (Erdogan Tan)

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch  	  
	out	dx, ax 				; out sample rate
		
	; 05/11/2023 temp
	mov    dx, [NAMBAR]               	
	add    dx, CODEC_LR_ADCRATE_REG 	; 32h  	  
	out	dx, ax 

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

;%endif

; 05/11/2023
;%if 0
	mov     dx, [NAMBAR]			; mixer base address
        add     dx, CODEC_RESET_REG  		; reset register
        mov	ax, 42
	out     dx, ax                          ; reset

	mov     dx, [NABMBAR]			; bus master base address
        add     dx, PORT_NABM_GLB_CTRL_STAT
        mov	ax, 2
	out     dx, ax                         

	mov	cx, 7
_100ms:
	push	cx
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
	pop	cx
	loop	_100ms
;%endif

	mov	dx, [NAMBAR]
	;add	dx, 0 ; ac_reg_0 ; reset register
	out	dx, ax

	xor	eax, eax ; 0
	mov	dx, [NAMBAR]
	add	dx, CODEC_POWER_CTRL_REG ; CODEC_REG_POWERDOWN
	out	dx, ax

	; wait for (max.) 1 second
	mov	cx, 1000 ; 100*0.25ms = 1s
_ac97_codec_rloop:
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	;mov	dx, [NAMBAR]
	;add	dx, CODEC_POWER_CTRL_REG ; CODEC_REG_POWERDOWN
	in	ax, dx	
	and	ax, 0Fh
	cmp	al, 0Fh
	je	short _ac97_codec_init_ok
	loop	_ac97_codec_rloop 

_ac97_codec_init_ok:
reset_ac97_controller:
	; 04/11/2023
	xor     ax, ax
        mov	dx, PI_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

        mov     dx, PO_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

        mov     dx, MC_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

        mov     al, RR
        mov     dx, PI_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

        mov     dx, PO_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

        mov     dx, MC_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

setup_ac97_codec:
	; 03/11/2023 (MPXPLAY, 'SC_ICH.C', ac97_init)
	; initial ac97 volumes (and clear mute flag)
		
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_VOL_REG        ;02h ; STEREO
  	;xor	ax, ax ; volume attenuation = 0 (max. volume)
  	; 03/11/2023
	mov	ax, 0202h
	out     dx, ax
 
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_MONO_VOL_REG   ;06h 
  	;;xor	ax, ax
	;mov	ax, 0202h
  	out     dx, ax

  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCBEEP_VOL_REG        ;0Ah 
  	;;xor	ax, ax
  	;mov	ax, 0202h
	out     dx, ax

  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCM_OUT_REG		;18h
  	;;xor	ax, ax
	;mov	ax, 0202h
  	out     dx, ax

	; 05/11/2023 temp
	mov     dx, [NAMBAR]
  	add     dx, 36h				;36h 
					; Center + LFE Master Volume
  	;;xor	ax, ax
	;mov	ax, 0202h
  	out     dx, ax

	;mov	ax, 8008h ; Mute
  	;mov	dx, [NAMBAR]
	;add	dx, CODEC_PHONE_VOL_REG		;0Ch
	;			 ; AC97_PHONE_VOL ; TAD Input (Mono)
  	;out	dx, ax
	;
        ;mov	ax, 0808h
  	;mov	dx, [NAMBAR]
        ;add	dx, CODEC_LINE_IN_VOL_REG ;10h ; Line Input (Stereo)
  	;out	dx, ax
	;
	;;mov	ax, 0808h
  	;mov	dx, [NAMBAR]
        ;add	dx, CODEC_CD_VOL_REG ;12h ; CR Input (Stereo)
  	;out	dx, ax
	;
	;;mov	ax, 0808h
  	;mov	dx, [NAMBAR]
        ;add	dx, CODEC_AUX_VOL_REG ;16h ; Aux Input (Stereo)
  	;out	dx, ax

	; 03/11/2023
        ;call	delay1_4ms
        ;call	delay1_4ms
        ;call	delay1_4ms
	;call	delay1_4ms

	retn

%endif

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
