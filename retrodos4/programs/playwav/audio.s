; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.7 - audio.s
; ----------------------------------------------------------------------------
; Last Update: 04/11/2023  (Previous: 06/08/2022 - Kernel v2.0.5)
; ----------------------------------------------------------------------------
; Beginning: 03/04/2017
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ****************************************************************************

; AUDIO CONTROLLER & CODEC DEFINITIONS & CODE FOR TRDOS 386

;=============================================================================
;               EQUATES
;=============================================================================

; PCI EQUATES

BIT0  EQU 1
BIT1  EQU 2
BIT2  EQU 4
BIT3  EQU 8
BIT4  EQU 10h
BIT5  EQU 20h
BIT6  EQU 40h
BIT7  EQU 80h
BIT8  EQU 100h
BIT9  EQU 200h
BIT10 EQU 400h
BIT11 EQU 800h
BIT12 EQU 1000h
BIT13 EQU 2000h
BIT14 EQU 4000h
BIT15 EQU 8000h
BIT16 EQU 10000h
BIT17 EQU 20000h
BIT18 EQU 40000h
BIT19 EQU 80000h
BIT20 EQU 100000h
BIT21 EQU 200000h
BIT22 EQU 400000h
BIT23 EQU 800000h
BIT24 EQU 1000000h
BIT25 EQU 2000000h
BIT26 EQU 4000000h
BIT27 EQU 8000000h
BIT28 EQU 10000000h
BIT29 EQU 20000000h
BIT30 EQU 40000000h
BIT31 EQU 80000000h
NOT_BIT31 EQU 7FFFFFFFh

; PCI equates
; PCI function address (PFA)
; bit 31 = 1
; bit 23:16 = bus number     (0-255)
; bit 15:11 = device number  (0-31)
; bit 10:8 = function number (0-7)
; bit 7:0 = register number  (0-255)

IO_ADDR_MASK    EQU     0FFFEh	; mask off bit 0 for reading BARs
PCI_INDEX_PORT  EQU     0CF8h
PCI_DATA_PORT   EQU     0CFCh
PCI32           EQU     BIT31	; bitflag to signal 32bit access
PCI16           EQU     BIT30	; bitflag for 16bit access
NOT_PCI32_PCI16	EQU	03FFFFFFFh ; NOT BIT31+BIT30 ; 19/03/2017

PCI_FN0         EQU     0 << 8
PCI_FN1         EQU     1 << 8
PCI_FN2         EQU     2 << 8
PCI_FN3         EQU     3 << 8
PCI_FN4         EQU     4 << 8
PCI_FN5         EQU     5 << 8
PCI_FN6         EQU     6 << 8
PCI_FN7         EQU     7 << 8

PCI_CMD_REG	EQU	04h	; reg 04, command reg
IO_ENA		EQU	BIT0	; i/o decode enable
MEM_ENA		EQU	BIT1	; memory decode enable
BM_ENA		EQU     BIT2	; bus master enable

; VIA VT8233 EQUATES

VIA_VID		equ 1106h	; VIA's PCI vendor ID
VT8233_DID      equ 3059h	; VT8233 (VT8235) device ID
		
PCI_IO_BASE          equ 10h
AC97_INT_LINE        equ 3Ch
VIA_ACLINK_CTRL      equ 41h
VIA_ACLINK_STAT      equ 40h
VIA_ACLINK_C00_READY equ 01h ; primary codec ready
	
VIA_REG_AC97	     equ 80h ; dword

VIA_ACLINK_CTRL_ENABLE	equ   80h ; 0: disable, 1: enable
VIA_ACLINK_CTRL_RESET	equ   40h ; 0: assert, 1: de-assert
VIA_ACLINK_CTRL_SYNC	equ   20h ; 0: release SYNC, 1: force SYNC hi
VIA_ACLINK_CTRL_VRA	equ   08h ; 0: disable VRA, 1: enable VRA
VIA_ACLINK_CTRL_PCM	equ   04h ; 0: disable PCM, 1: enable PCM
					; 3D Audio Channel slots 3/4
VIA_ACLINK_CTRL_INIT	equ  (VIA_ACLINK_CTRL_ENABLE + \
                              VIA_ACLINK_CTRL_RESET + \
                              VIA_ACLINK_CTRL_PCM + \
                              VIA_ACLINK_CTRL_VRA)

CODEC_AUX_VOL		equ   04h
VIA_REG_AC97_BUSY	equ   01000000h ;(1<<24) 
VIA_REG_AC97_CMD_SHIFT	equ   10h ; 16
VIA_REG_AC97_PRIMARY_VALID equ 02000000h ;(1<<25)
VIA_REG_AC97_READ	equ   00800000h ;(1<<23)
VIA_REG_AC97_CODEC_ID_SHIFT   equ  1Eh ; 30
VIA_REG_AC97_CODEC_ID_PRIMARY equ  0
VIA_REG_AC97_DATA_SHIFT equ   0
VIADEV_PLAYBACK         equ   0
VIA_REG_OFFSET_STATUS   equ   0    ;; byte - channel status
VIA_REG_OFFSET_CONTROL  equ   01h  ;; byte - channel control
VIA_REG_CTRL_START	equ   80h  ;; WO
VIA_REG_CTRL_TERMINATE  equ   40h  ;; WO
VIA_REG_CTRL_PAUSE      equ   08h  ;; RW
VIA_REG_CTRL_RESET      equ   01h  ;; RW - probably reset? undocumented
VIA_REG_OFFSET_STOP_IDX equ   08h  ;; dword - stop index, channel type, sample rate
VIA8233_REG_TYPE_16BIT  equ   200000h ;; RW
VIA8233_REG_TYPE_STEREO equ   100000h ;; RW
VIA_REG_OFFSET_CURR_INDEX equ 0Fh ;; byte - channel current index (for via8233 only)
VIA_REG_OFFSET_TABLE_PTR equ  04h  ;; dword - channel table pointer
VIA_REG_OFFSET_CURR_PTR equ   04h  ;; dword - channel current pointer
VIA_REG_OFS_PLAYBACK_VOLUME_L equ  02h ;; byte
VIA_REG_OFS_PLAYBACK_VOLUME_R equ  03h ;; byte
VIA_REG_CTRL_AUTOSTART	equ   20h
VIA_REG_CTRL_INT_EOL	equ   02h
VIA_REG_CTRL_INT_FLAG	equ   01h
VIA_REG_CTRL_INT	equ  (VIA_REG_CTRL_INT_FLAG + \
                              VIA_REG_CTRL_INT_EOL + \
                              VIA_REG_CTRL_AUTOSTART)

VIA_REG_STAT_STOP_IDX	equ   10h    ;; RO ; 27/07/2020
				     ; current index = stop index
VIA_REG_STAT_STOPPED	equ   04h    ;; RWC
VIA_REG_STAT_EOL	equ   02h    ;; RWC
VIA_REG_STAT_FLAG	equ   01h    ;; RWC
VIA_REG_STAT_ACTIVE	equ   80h    ;; RO
; 28/11/2016
VIA_REG_STAT_LAST	equ   40h    ;; RO
VIA_REG_STAT_TRIGGER_QUEUED equ 08h  ;; RO
VIA_REG_CTRL_INT_STOP	equ   04h  ; Interrupt on Current Index = Stop Index
		   		   ; and End of Block

VIA_REG_OFFSET_CURR_COUNT equ 0Ch ;; dword - channel current count, index

PORTB		EQU	061h
REFRESH_STATUS	EQU	010h	; Refresh signal status

; AC97 Codec registers.

; 22/07/2020
; REALTEK ALC655 and ADI SOUNDMAX AD1980 CODEC MIXER REGISTERS

; each codec/mixer register is 16bits

CODEC_RESET_REG                 equ     00h	; reset codec
CODEC_MASTER_VOL_REG            equ     02h	; master volume
CODEC_HP_VOL_REG                equ     04h	; headphone volume ; AD1980
CODEC_MASTER_MONO_VOL_REG       equ     06h	; master mono volume (mono-out)
;CODEC_MASTER_TONE_REG          equ     08h	; master tone (R+L) ; (not used)
CODEC_PCBEEP_VOL_REG            equ     0Ah	; PC beep volume ; ALC655
CODEC_PHONE_VOL_REG             equ     0Ch	; phone volume
CODEC_MIC_VOL_REG               equ     0Eh	; mic volume
CODEC_LINE_IN_VOL_REG           equ     10h	; line in volume
CODEC_CD_VOL_REG                equ     12h	; CD volume
;CODEC_VID_VOL_REG              equ     14h	; video volume ; (not used)
CODEC_AUX_VOL_REG               equ     16h	; aux volume
CODEC_PCM_OUT_REG               equ     18h	; PCM out volume
CODEC_RECORD_SELECT_REG         equ     1Ah	; record select
CODEC_RECORD_VOL_REG            equ     1Ch	; record volume (record gain)
;CODEC_RECORD_MIC_VOL_REG       equ     1Eh	; record mic volume ; (not used)
CODEC_GP_REG                    equ     20h	; general purpose
;CODEC_3D_CONTROL_REG           equ     22h	; 3D control
;;CODEC_AUDIO_INT_PAGING_REG    equ	24h	; audio int & paging ; (not used) 
CODEC_POWER_CTRL_REG            equ     26h	; power down control
CODEC_EXT_AUDIO_REG             equ     28h	; extended audio ID
CODEC_EXT_AUDIO_CTRL_REG        equ     2Ah	; extended audio status/control
CODEC_PCM_FRONT_DACRATE_REG     equ     2Ch	; PCM front sample rate
CODEC_PCM_SURND_DACRATE_REG     equ     2Eh	; PCM surround sample rate
CODEC_PCM_LFE_DACRATE_REG       equ     30h	; PCM Center/LFE sample rate
CODEC_LR_ADCRATE_REG            equ     32h	; PCM input sample rate
CODEC_MIC_ADCRATE_REG           equ     34h	; mic in sample rate  ; AD1980
CODEC_PCM_LFE_VOL_REG           equ     36h	; PCM Center/LFE volume
CODEC_PCM_SURND_VOL_REG         equ     38h	; PCM surround volume
;CODEC_SPDIF_CTRL_REG           equ     3Ah	; S/PDIF control
; 22/07/2020
CODEC_MISC_CRTL_BITS_REG	equ	76h	; misc control bits ; AD1980
;	
CODEC_VENDOR_ID1		equ	7Ch	; REALTEK: 414Ch, ADI: 4144h	
CODEC_VENDOR_ID2		equ	7Eh	; REALTEK: 4760h, ADI: 5370h	

; VT8233 SGD bits (21/04/2017)
FLAG	EQU BIT30
EOL	EQU BIT31

; INTEL ICH EQUATES
; 28/05/2017
INTEL_VID	equ	8086h	; Intel's PCI vendor ID
ICH_DID		equ	2415h	; ICH (82801AA) device ID
; 01/11/2023
NFORCE_VID	equ	10DEh	; Nvidia NFORCE PCI vendor ID
NFORCE_DID	equ	0059h	; Nvidia NFORCE (CK804) device ID

NAMBAR_REG      equ	10h	; native audio mixer Base Address Register
NABMBAR_REG     equ	14h	; native audio bus mastering Base Addr Reg

PI_CR_REG       equ     0Bh     ; PCM in Control Register
PO_CR_REG	equ     1Bh     ; PCM out Control Register
MC_CR_REG	equ     2Bh     ; MIC in Control Register

PI_SR_REG	equ     6       ; PCM in Status register
PO_SR_REG	equ     16h     ; PCM out Status register
MC_SR_REG	equ     26h     ; MIC in Status register

IOCE 		equ     BIT4    ; interrupt on complete enable.
FEIFE		equ     BIT3    ; set if you want an interrupt to fire
LVBIE		equ     BIT2    ; last valid buffer interrupt enable.
RR 		equ     BIT1    ; reset registers. Nukes all regs
                                ; except bits 4:2 of this register.
                                ; Only set this bit if BIT 0 is 0
RPBM		equ     BIT0    ; Run/Pause
				; set this bit to start the codec!

PI_BDBAR_REG	equ     0       ; PCM in buffer descriptor BAR
PO_BDBAR_REG	equ     10h     ; PCM out buffer descriptor BAR
MC_BDBAR_REG	equ     20h     ; MIC in buffer descriptor BAR

PI_CIV_REG	equ     4       ; PCM in current Index value (RO)
PO_CIV_REG	equ     14h     ; PCM out current Index value (RO)
MC_CIV_REG 	equ     24h     ; MIC in current Index value (RO)

PI_LVI_REG	equ     5       ; PCM in Last Valid Index
PO_LVI_REG	equ     15h     ; PCM out Last Valid Index
MC_LVI_REG	equ     25h     ; MIC in Last Valid Index

IOC		equ     BIT31	; Fire an interrupt whenever this
                		; buffer is complete.
BUP		equ     BIT30	; Buffer Underrun Policy.

GLOB_CNT_REG	equ     2Ch     ; Global Control Register
GLOB_STS_REG	equ     30h     ; Global Status register (RO)

CTRL_ST_CREADY	equ   BIT8+BIT9+BIT28 ; Primary Codec Ready

CODEC_REG_POWERDOWN   equ 26h
CODEC_REG_ST          equ 26h

; 22/06/2017
PO_PICB_REG	equ 18h	; PCM Out Position In Current Buffer Register

;=============================================================================
;               CODE
;=============================================================================

; CODE for INTEL ICH AC'97 AUDIO CONTROLLER

DetectICH:
	; 10/06/2017
	; 05/06/2017
	; 29/05/2017
	; 28/05/2017
	mov     eax, (ICH_DID << 16) + INTEL_VID
        call    pciFindDevice
        jnc     short d_ac97_1

	; 01/11/2023
	mov     eax, (NFORCE_DID << 16) + NFORCE_VID
        call    pciFindDevice
        jnc     short d_ac97_1

d_ac97_0:
; couldn't find the audio device!
	retn

; CODE for VIA VT8233 AUDIO CONTROLLER

DetectVT8233:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 10/06/2017
	; 05/06/2017
	; 29/05/2017
	; 03/04/2017
	mov     eax, (VT8233_DID << 16) + VIA_VID
        call    pciFindDevice
;       jnc     short d_vt8233_0
; couldn't find the audio device!
;	retn
	jc	short d_ac97_0  ; 28/05/2017
d_vt8233_0:
	; 24/03/2017 ('player.asm')
	; 12/11/2016 
	; Erdogan Tan - 8/11/2016
	; References: Kolibrios - vt823x.asm (2016)
	;	      VIA VT8235 V-Link South Bridge (VT8235-VIA.PDF)(2002)
	;	      lowlevel.eu - AC97 (2016)
	;	      .wav player for DOS by Jeff Leyda (2002) -this file-
	;	      Linux kernel - via82xx.c (2016)
d_ac97_1:
	; eax = BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; edx = DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	mov	[audio_dev_id], eax
	mov	[audio_vendor], edx

	; init controller
	mov	al, PCI_CMD_REG ; command register (04h)
	call	pciRegRead32

	; eax = BUS/DEV/FN/REG
	; edx = STATUS/COMMAND
	; 	SSSSSSSSSSSSSSSSCCCCCCCCCCCCCCCC
	mov	[audio_stats_cmd], edx

	mov	al, PCI_IO_BASE ; IO base address register (10h)
	;mov	al, NAMBAR_REG	; Native Audio Mixer BAR (10h)
	call	pciRegRead32

	;cmp	word [audio_vendor], INTEL_VID ; 8086h ; AC'97 ?
	;;jne	short d_vt8233_1
	;; 01/11/2023
	;je	short d_ac97_3
	;cmp	word [audio_vendor], NFORCE_VID ; 10DEh ; AC'97
	;jne	short d_vt8233_1	
	; 02/11/2023
	cmp	word [audio_vendor], VIA_VID	; 1106h ; VT8233-VT8237R 
	je	short d_vt8233_1
d_ac97_3:
	;and	dx, 0FFFEh ; Audio Codec IO_ADDR_MASK
	; 06/08/2022
	and	dl, 0FEh
	mov	[NAMBAR], dx

	mov	al, NABMBAR_REG ; Native Audio Bus Mastering BAR (14h)
	call	pciRegRead32	

	;;and	dx, 0FFC0h ; Audio Controller IO_ADDR_MASK
	;; 06/08/2022
	;and	dl, 0C0h
	;mov	[NABMBAR], dx
        ;;mov	[audio_io_base], dx
	;
	;jmp	short d_ac97_2
	; 02/11/2023
	; NABMBAR = audio_io_base

d_vt8233_1:
	;and	dx, 0FFC0h ; Audio Controller IO_ADDR_MASK 
	; 06/08/2022
	and	dl, 0C0h
        mov     [audio_io_base], dx

d_ac97_2:
	; 10/06/2017
	mov	al, AC97_INT_LINE ; Interrupt Line Register (3Ch)
	;call	pciRegRead32
	call	pciRegRead8

	;;and 	edx, 0FFh
	; 06/08/2022
	;and	dx, 0FFh

  	mov     [audio_intr], dl

	retn

	;; (Note: Interrupts are already enabled by TRDOS 386 kernel!)
	;mov	cx, dx
	
	;in	al, 0A1h ; irq 8-15
	;mov	ah, al
	;in	al, 21h  ; irq 0-7 
	;btr	ax, dx	 ; unmask ; 17/03/2017
	;;bts	ax, dx   ; MASK interrupt ; 10/06/2017 
	;out	21h, al  ; irq <= 7
	;mov	al, ah
	;out	0A1h, al ; irq > 7
	;
	
	; 10/06/2017
	; === Intel ICH I/O Controller Hub Datasheet, Section 8.1.16 ===
	; PRQ[n]_ROUT Register (61h, PRQB) Bit 7:
	; Interrupt Routing Enable (IRQEN).
	; 0 = The corresponding PIRQ is routed to one of the ISA-compatible
	;     interrupts specified in bits[3:0].
	; 1 = The PIRQ is not routed to the 8259.
	; Note: If the PIRQ is intended to cause an interrupt to the ICH’s 
	;	integrated I/O APIC, then this bit should be set to 0 and 
	;	the APIC_EN bit should be set to 1. 
	;	The IRQEN must be set to 0 and the PIRQ routed to 
	;	an 8259 interrupt via the IRQ Routing filed (bits[3:0).
	;	The corresponding 8259 interrupt must be masked via the 
	;	appropriated bit in the 8259’s OCW1 (Interrupt Mask)
	;	register. The IOAPIC must then be enabled by setting 
	;	the APIC_EN bit in the GEN_CNTL register.

	;mov	eax, 0F861h  ; D31:F0
		;AL=61h : PIRQ[B] Routing Control Reg, LPC interface
	;;mov	dl, [audio_intr]
	;call	pciRegWrite8
	;;mov	al, 0D0h ; General Control Register (GEN_CTL)
	;;call	pciRegRead32
	;;or	edx, 100h ; Bit 8, APIC_EN (Enable I/O APIC) 
	;;;call	pciRegWrite32
	;;and	edx, ~100h
	;;call	pciRegWrite32 ; ; Bit 8, APIC_EN (Disable I/O APIC) 
	;

	;mov	dx, 4D1h	; 8259 ELCR2
    	;in	al, dx
	;mov	ah, al
	;;mov	dx, 4D0h 	; 8259 ELCR1
	;dec	dl
	;in	al, dx
	;bts	ax, cx
	;;mov	dx, 4D0h
	;out	dx, al		; set level-triggered mode
	;mov	al, ah ; 29/05/2017
	;;mov	dx, 4D1h
	;inc	dl
	;out	dx, al		; set level-triggered mode

	;xor	eax, eax ; 0

	;retn

; CODE for PCI

pciFindDevice:
	; 03/04/2017 ('pci.asm', 20/03/2017)
	;
	; scan through PCI space looking for a device+vendor ID
	;
	; Entry: EAX=Device+Vendor ID
	;
	; Exit: EAX=PCI address if device found
	;	 EDX=Device+Vendor ID
	;        CY clear if found, set if not found. EAX invalid if CY set.
	;
	; Destroys: ebx, esi, edi, cl
	;

	;push	ecx
	push	eax
	;push	esi
	;push	edi

        mov     esi, eax                ; save off vend+device ID
        mov     edi, (80000000h - 100h) ; start with bus 0, dev 0 func 0

nextPCIdevice:
        add     edi, 100h
        cmp     edi, 80FFF800h		; scanned all devices?
        stc
        je      short PCIScanExit       ; not found

        mov     eax, edi                ; read PCI registers
        call    pciRegRead32
        cmp     edx, esi                ; found device?
        jne     short nextPCIdevice
        clc

PCIScanExit:
	pushf
	mov	eax, NOT_BIT31 	; 19/03/2017
	and	eax, edi	; return only bus/dev/fn #
	popf

	;pop	edi
	;pop	esi
	pop	edx
	;pop	ecx
	retn

pciRegRead:
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
	push	ecx
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
	pop	ecx
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

init_codec:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 05/06/2017
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
	;
	mov	eax, [audio_dev_id]	
	mov	al, VIA_ACLINK_CTRL
	call	pciRegRead8
	; ?
	mov	al, VIA_ACLINK_STAT
	call	pciRegRead8
	test	dl, VIA_ACLINK_C00_READY
        jnz     short _codec_ready_1
	call	reset_codec
	jnc	short _codec_ready_2 ; eax = 1
	retn
_codec_ready_1:
	;mov	eax, 1
	; 06/08/2022
	sub	eax, eax
	inc	al
	; eax = 1
_codec_ready_2:
	call	codec_io_w16
detect_codec:
	retn

reset_codec:
	; 16/04/2017
	; 23/03/2017 
	; ('codec.asm')
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
	mov	eax, [audio_dev_id]
 	mov	al, VIA_ACLINK_CTRL
       	mov	dl, VIA_ACLINK_CTRL_ENABLE + VIA_ACLINK_CTRL_RESET + VIA_ACLINK_CTRL_SYNC
	call	pciRegWrite8

	call	delay_100ms 	; wait 100 ms
_rc_cold:
        call    cold_reset
        jnc     short _reset_codec_ok
	
	; 16/04/2017
        ;xor	eax, eax	; timeout error
       	;stc
	retn

_reset_codec_ok:
	; 01/09/2020
	; 15/08/2020
	; 27/07/2020
	; also reset codec by using index control register 0 of AD1980 or ALC655
	; (to fix line out -2 channels audio playing- problem on AD1980 codec)  

	sub	eax, eax
	; 02/11/2023
	;mov	edx, CODEC_RESET_REG ; 00h ; Reset register
	xor	edx, edx ; 00h ; Reset register
	call	codec_write

	;sub	eax, eax
	; 01/09/2020
	; 15/08/2020
	; AD1980 BugFix
	; (set HPSEL -headphone amp to be driven from mixer- and
	;      CLDIS -center and LFE disable- bits)	
	;mov	eax, 0C00h ; HPSEL = bit 10, CLDIS = bit 11 ; 01/09/2020
 	;mov	edx, CODEC_MISC_CRTL_BITS_REG ; 76h ; Misc Ctrl Bits ; AD1980
	;call	codec_write

        xor     eax, eax
        ;mov	al, VIA_ACLINK_C00_READY ; 1
        inc	al
	retn

cold_reset:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 16/04/2017
	; 23/03/2017
	; ('codec.asm')
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
	;mov	eax, [audio_dev_id]
	;mov	al, VIA_ACLINK_CTRL
	xor	dl, dl ; 0
	call	pciRegWrite8

	call	delay_100ms	; wait 100 ms

	;; ACLink on, deassert ACLink reset, VSR, SGD data out
        ;; note - FM data out has trouble with non VRA codecs !!
        
	;mov	eax, [audio_dev_id]
	;mov	al, VIA_ACLINK_CTRL
	mov	dl, VIA_ACLINK_CTRL_INIT
	call	pciRegWrite8

	;mov	ecx, 16	; total 2s
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 16
_crst_wait:
	;mov	eax, [audio_dev_id]
	mov	al, VIA_ACLINK_STAT
	call	pciRegRead8	

        test    dl, VIA_ACLINK_C00_READY
        jnz     short _crst_ok

	push	ecx
	call	delay_100ms
	pop	ecx

        dec     ecx
        jnz     short _crst_wait

_crst_fail:
        stc
_crst_ok:
	retn

delay_100ms:
	; 29/05/2017
	; 24/03/2017 ('codec.asm')
	; wait 100 ms
	mov	ecx, 400  ; 400*0.25ms
_delay_x_ms:
	call	delay1_4ms
        loop	_delay_x_ms
	retn

;       delay1_4ms - Delay for 1/4 millisecond.
;	    1ms = 1000us
;       Entry:
;         None
;       Exit:
;	  None
;
;       Modified:
;         None

	; 29/05/2017
	; 23/04/2017
	; 05/03/2017 (TRDOS 386)
	; ('UTILS.ASM')
delay1_4ms:
        push    eax 
        push    ecx
        mov	cl, 16		; close enough.

	in	al, PORTB ; 61h
		
	and	al, REFRESH_STATUS ; 10h
	mov	ch, al		; Start toggle state
_d4ms1:	
	in	al, PORTB	; Read system control port
	
	and	al, REFRESH_STATUS ; Refresh toggles 15.085 microseconds
	cmp	ch, al
	je	short _d4ms1	; Wait for state change

	mov	ch, al		; Update with new state
	dec	cl
	jnz	short _d4ms1

	clc	; 29/05/2017

        pop     ecx
        pop     eax
        retn

; 10/04/2017 (TRDOS 386)
; 12/11/2016

codec_io_w16: ;w32
	; ('codec.asm')
        mov	dx, [audio_io_base]
        add     dx, VIA_REG_AC97
	out	dx, eax
        retn

codec_io_r16: ;r32
	; ('codec.asm')
        mov     dx, [audio_io_base]
        add     dx, VIA_REG_AC97
        in	eax, dx
        retn

ctrl_io_w8:
	; ('codec.asm')
        add     dx, [audio_io_base]
        out	dx, al
        retn

ctrl_io_r8:
	; ('codec.asm')
        add     dx, [audio_io_base]
        in	al, dx
        retn

ctrl_io_w32:
	; ('codec.asm')
        add     dx, [audio_io_base]
        out	dx, eax
        retn

ctrl_io_r32:
	; ('codec.asm')
        add	dx, [audio_io_base]
	in	eax, dx
_cr_not_rdy:	; 06/08/2022
        retn

codec_read:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
        ; Use only primary codec.
        ; eax = register
        shl     eax, VIA_REG_AC97_CMD_SHIFT
        or      eax, VIA_REG_AC97_PRIMARY_VALID + VIA_REG_AC97_READ

	call    codec_io_w16

      	; codec_valid
	call	codec_check_ready
	;jnc	short _cr_ok
	;retn
	; 06/08/2022
	jc	short _cr_not_rdy
	; ecx <= 20
_cr_ok:
	; wait 25 ms
	;mov	ecx, 80 ; (100*0.25 ms)
	; 06/08/2022
	;xor	ecx, ecx
	mov	cl, 80
	; ecx = 80
_cr_wloop:
	call	delay1_4ms
	loop	_cr_wloop

        call    codec_io_r16
	; 06/08/2022
	;and	eax, 0FFFFh
        retn

codec_write:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
        ; Use only primary codec.
        
	; eax = data (volume)
	; edx = register (mixer register)
	
	shl     edx, VIA_REG_AC97_CMD_SHIFT

	; 02/11/2023 (shl eax, 0)
	;shl	eax, VIA_REG_AC97_DATA_SHIFT ; shl eax, 0
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
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)

_codec_check_ready:
	;mov	ecx, 20	; total 2s
	; 06/08/2022
	sub	ecx, ecx
	;mov	cl, 20
	; 02/11/2023
	mov	cl, 10	; wait 1s
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

codec_config:
	; 02/11/2023 - TRDOS 386 Kernel v2.0.7
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 10/06/2017
	; 29/05/2017
	; 24/04/2017
	; 21/04/2017
	; 16/04/2017 (TRDOS 386 Kernel) 
	; 15/11/2016 ('codec.asm', 'player.com')
	; 14/11/2016
	; 12/11/2016 - Erdogan Tan
	;	     (Ref: KolibriOS, 'setup_codec', codec.inc)

	mov     eax, 0202h
	mov	[audio_master_volume], ax

	;mov	ax, 1F1Fh ; 31,31
	; 02/11/2023
	mov	ax, 0B0Bh
	;mov	edx, CODEC_MASTER_VOL_REG ; 02h ; Line Out
	; 06/08/2022
	sub	edx, edx
	mov	dl, CODEC_MASTER_VOL_REG ; 02h ; Line Out
	call	codec_write
	;jc	short cconfig_error

 	;mov    eax, 0202h
	mov     ax, 0202h
	;mov	edx, CODEC_PCM_OUT_REG ; 18h ; Wave Output (Stereo)
	; 06/08/2022
	sub	edx, edx
	mov	dl, CODEC_PCM_OUT_REG ; 18h ; Wave Output (Stereo)
	call	codec_write
	;jc	short cconfig_error
      
 	;mov    eax, 0202h
	mov	ax, 0202h
	;mov	edx, CODEC_AUX_VOL ; 04h ; CODEC_HP_VOL_REG ; HeadPhone
	; 06/08/2022
	sub	edx, edx
	mov	dl, CODEC_AUX_VOL ; 04h ; CODEC_HP_VOL_REG ; HeadPhone
	call	codec_write
	;jc	short cconfig_error

 	;mov    eax, 08h
        ;mov    ax, 08h
	mov	ax, 8008h ; Mute
	;mov	edx, 0Ch  ; AC97_PHONE_VOL ; TAD Input (Mono)
	; 06/08/2022
	sub	edx, edx
	mov	dl, 0Ch	  ; AC97_PHONE_VOL ; TAD Input (Mono)
	call	codec_write
	;jc	short cconfig_error

 	;mov    eax, 0808h
	mov	ax, 0808h
	;mov	edx, CODEC_LINE_IN_VOL_REG ; 10h ; Line Input (Stereo)	
	; 06/08/2022
	sub	edx, edx
	mov	dl, CODEC_LINE_IN_VOL_REG ; 10h ; Line Input (Stereo)	
	call	codec_write
	;jc	short cconfig_error

 	;mov    eax, 0808h
	mov	ax, 0808h
	;mov	edx, CODEC_CD_VOL_REG ; 12h ; CR Input (Stereo)
	; 06/08/2022
	sub	edx, edx ; 02/11/2023
	mov	dl, CODEC_CD_VOL_REG ; 12h ; CR Input (Stereo)
	call	codec_write
	;jc	short cconfig_error

 	;mov    eax, 0808h
	mov     ax, 0808h
        ;mov	edx, CODEC_AUX_VOL_REG ; 16h ; Aux Input (Stereo)
	; 06/08/2022
	sub	edx, edx
	mov	dl, CODEC_AUX_VOL_REG ; 16h ; Aux Input (Stereo)
	;call	codec_write
	;;jc	short cconfig_error
	jmp	codec_write ; 10/06/2017

;	; Extended Audio Status (2Ah)
;	mov	eax, CODEC_EXT_AUDIO_CTRL_REG ; 2Ah 
;	call	codec_read
;	and     eax, 0FFFFh - 2		; clear DRA (BIT1)
;	;or     eax, 1			; set VRA (BIT0)
;	or	eax, 5  	; VRA (BIT0) & S/PDIF (BIT2) ; 14/11/2016
;	mov	edx, CODEC_EXT_AUDIO_CTRL_REG
;	call	codec_write
;	;jc	short cconfig_error
;
;set_sample_rate:
;	;movzx	eax, word [audio_freq]
;	mov	ax, [audio_freq]
;	mov	edx, CODEC_PCM_FRONT_DACRATE_REG ; 2Ch ; PCM Front DAC Rate
;	;call	codec_write
;	;retn
;	jmp	codec_write
	
;cconfig_error:
;	retn

vt8233_int_handler:
	; 27/07/2020
	; 22/07/2020
	; Interrupt Handler for VIA VT8237R Audio Controller
	; Note: called by 'dev_IRQ_service'
	; 14/10/2017 
	; 09/10/2017, 10/10/2017, 12/10/2017
	; 13/06/2017
	; 21/04/2017 (TRDOS 386 kernel, 'audio.s')
	; 24/03/2017 - 'PLAYER.COM' ('player.asm') 

	;push	eax ; * must be saved !
	;push	edx
	;push	ecx
	;push	ebx ; * must be saved !
	;push	esi
	;push	edi

	;cmp	byte [audio_busy], 1
	;jnb	short _ih0 ; 09/10/2017

	;mov	byte [audio_flag_eol], 0

        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_r8

	test    al, VIA_REG_STAT_ACTIVE
        jz      short _ih0 ; 09/10/2017

        and     al, VIA_REG_STAT_EOL + VIA_REG_STAT_FLAG + VIA_REG_STAT_STOPPED
	mov	[audio_flag_eol], al
        jz	short _ih0 ; 09/10/2017

	; 09/10/2017
	;mov	byte [audio_busy], 1

	cmp	byte [audio_play_cmd], 1
	jnb	short _ih1 ; 10/10/2017

	call	channel_reset
_ih0:
	; 09/10/2017
        mov     al, [audio_flag_eol]   ;; ack ;;
        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_w8
	jmp	short _ih4
_ih1:
vt8233_tuneLoop:
        mov     al, [audio_flag_eol]   ;; ack ;;
        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_w8

	; 22/07/2020
	;; 12/10/2017
	;mov	byte [audio_flag], 0 ; Reset	

	; 10/10/2017
	; 09/10/2017
	;test	byte [audio_flag_eol], VIA_REG_STAT_FLAG
	;jz	short _ih2 ; EOL

	; 22/07/2020
	; 14/10/2017
	;test	byte [audio_flag_eol], VIA_REG_STAT_EOL
	;jnz	short _ih2 ; EOL
	;		   ; (Half Buffer 2 has been completed 
	;		   ; and Half Buffer 1 will be played.)
	
	; FLAG  
	; (Half Buffer 1 has been completed 
	;  and Half Buffer 2 will be played.)

	; 14/10/2017
	;; (Continue to play.)
	;mov	al, VIA_REG_CTRL_INT
       	;or	al, VIA_REG_CTRL_START
       	;mov	dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        ;call	ctrl_io_w8
	; 12/10/2017
	;mov	byte [audio_flag], 1 

	; 22/07/2020
	;inc	byte [audio_flag] ; = 1
_ih2: 
	; 10/10/2017
	mov	edi, [audio_dma_buff]
	mov	ecx, [audio_dmabuff_size]
	shr	ecx, 1 ; dma buff size / 2 = half buffer size
	
	; 22/07/2020
	; 12/10/2017
	;cmp	byte [audio_flag], 0
	;ja	short _ih3 ; Playing Half Buffer 2 (Current: FLAG)
	
	; 27/07/2020
	; 22/07/2020
	test	byte [audio_flag], 1  ; Current flag value
	jz	short _ih3 ; Half Buffer 1 must be filled

	; Half Buffer 2 must be filled
	add	edi, ecx
_ih3:
	; Update half buffer 2 while playing half buffer 1
	; Update half buffer 1 while playing half buffer 2

	mov	esi, [audio_p_buffer] ; phy addr of audio buff
	shr	ecx, 2 ; half buff size / 4
	rep	movsd

	; switch flag value ;
	xor	byte [audio_flag], 1
	; 12/10/2017
	; [audio_flag] = 0 : Playing dma half buffer 2
			   ; Next buffer (to update) is dma half buff 1
	; 	       = 1 : Playing dma half buffer 1
			   ; Next buffer (to update) is dma half buff 2
_ih4:	
	; 28/05/2017
	;mov	byte [audio_busy], 0 ; 09/10/2017
	;
	;pop	edi
	;pop	esi
	;pop	ebx ; * must be restored !
	;pop	ecx
	;pop	edx
	;pop	eax ; * must be restored !

	retn

channel_reset:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 24/06/2017
	; 29/05/2017
	; 23/03/2017
	; 14/11/2016 - Erdogan Tan
	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
        ;mov	edx, VIA_REG_OFFSET_CONTROL
	; 06/08/2022
	sub	edx, edx
	mov	dl, VIA_REG_OFFSET_CONTROL 
	;mov	eax, VIA_REG_CTRL_PAUSE + VIA_REG_CTRL_TERMINATE + VIA_REG_CTRL_RESET
        mov	eax, VIA_REG_CTRL_PAUSE + VIA_REG_CTRL_TERMINATE ; 24/06/2017       
	call    ctrl_io_w8

        ;mov	edx, VIA_REG_OFFSET_CONTROL
        ;call   ctrl_io_r8

	; wait for 50 ms
	;mov	ecx, 160 ; (200*0.25 ms) ; 29/05/2017	
	; 06/08/2022
	xor	ecx, ecx
	mov	cl, 160
_ch_rst_wait:
	call	delay1_4ms
	dec	ecx
	jnz	short _ch_rst_wait     

        ; disable interrupts
	;mov	edx, VIA_REG_OFFSET_CONTROL
        ; 06/08/2022
	sub	edx, edx
	mov	dl, VIA_REG_OFFSET_CONTROL
 	xor     eax, eax
        call    ctrl_io_w8

        ; clear interrupts
        ;mov	edx, VIA_REG_OFFSET_STATUS
	; 06/08/2022
	sub	edx, edx
	;mov	dl, VIA_REG_OFFSET_STATUS ; 0
	; edx = 0
	;mov	eax, 3
        ; 06/08/2022
	sub	eax, eax
	mov	al, 3
	; eax = 3
	call	ctrl_io_w8

	;mov	edx, VIA_REG_OFFSET_CURR_PTR
	;xor	eax, eax
	;call	ctrl_io_w32

        retn	

vt8233_stop: ; 22/04/2017
	mov	byte [audio_play_cmd], 0 ; stop !
_tlp2:
	; 24/06/2017
        ; finished with song, stop everything
	;mov	al, VIA_REG_CTRL_INT
        ;or	al, VIA_REG_CTRL_TERMINATE
	;mov	dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        ;call	ctrl_io_w8

        ;call	channel_reset
	;retn

	jmp	short channel_reset

set_vt8233_bdl: ; Set VT8237R Buffer Descriptor List
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 22/07/2020 - TRDOS 386 v2.0.2
	; 28/05/2017
	; 21/04/2017 (TRDOS 386 kernel, 'audio.s')
	; 24/03/2017 - 'PLAYER.COM' ('via_wav.asm' - 29/11/2016) 
	
	; eax = dma buffer address = [audio_DMA_buff]
	; ecx = dma buffer buffer size = [audio_dmabuff_size]

	shr	ecx, 1 ; dma half buffer size
	mov	esi, ecx

        mov     edi, audio_bdl_buff	; get BDL address
	;mov	ecx, 32 / 2		; make 32 entries in BDL
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 16

	jmp	short s_vt8233_bdl1 

s_vt8233_bdl0:
	; set buffer descriptor 0 to start of data file in memory

 	mov	eax, [audio_dma_buff]	; Physical address of DMA buffer
 
s_vt8233_bdl1:
	stosd				; store dmabuffer1 address

	mov	edx, eax

; VIA VT8235.PDF: (Page 110) (Erdogan Tan, 29/11/2016)
	;
	; 	Audio SGD Table Format
	;	-------------------------------
	;	63   62    61-56    55-32  31-0
	;	--   --   --------  -----  ----
	;	EOL FLAG -reserved- Base   Base
	;		    	    Count  Address
	;		            [23:0] [31:0]
	;	EOL: End Of Link. 
	;	     1 indicates this block is the last of the link.
	;	     If the channel “Interrupt on EOL” bit is set, then
	;	     an interrupt is generated at the end of the transfer.
	;
	;	FLAG: Block Flag. If set, transfer pauses at the end of this
	;	      block. If the channel “Interrupt on FLAG” bit is set,
	;	      then an interrupt is generated at the end of this block.

	mov	eax, esi ; DMA half buffer size
	add	edx, eax
	or	eax, FLAG
	;or	eax, EOL
	stosd

; 2nd buffer:

        mov	eax, edx ; Physical address of the 2nd half of DMA buffer	
	stosd		 ; store dmabuffer2 address

; set length to [audio_dmabuff_size]/2
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	mov	eax, esi ; DMA half buffer size
	; 22/07/2020
	;or	eax, EOL
	or	eax, FLAG
	stosd

	loop    s_vt8233_bdl0

	; 22/07/2020
	or	dword [edi-4], EOL
	
	retn

vt8233_start_play:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 01/09/2020 
	; 22/07/2020 
	; start to play audio data via VT8233 audio controller
	; 13/06/2017
	; 10/06/2017
	; 24/04/2017 
	; 21/04/2017 (TRDOS 386 kernel, 'audio.s')
	; 24/03/2017 - 'PLAYER.COM' ('via_wav.asm' - 29/11/2016) 
	; write buffer descriptor list address

	; Extended Audio Status (2Ah)
	mov	eax, CODEC_EXT_AUDIO_CTRL_REG ; 2Ah 
	call	codec_read
	and     eax, 0FFFFh - 2		; clear DRA (BIT1)
	;or     eax, 1			; set VRA (BIT0)
	;or	eax, 5  	; VRA (BIT0) & S/PDIF (BIT2) ; 14/11/2016
	or	al, 5
	; 01/09/2020	
	;or	eax, 3805h ; AD1980 (PRK, PRJ, PRI = 1 .. only front DAC)
	; 01/09/2020
	;mov	edx, CODEC_EXT_AUDIO_CTRL_REG
	;cmp	word [audio_freq], 0BB80h ; 48 kHz
	;jne	short set_extd_audio_status_1
	;and	al, 0FEh ; disable VRA bit (set sample rate to 48000 Hz)
	;jmp	short set_extd_audio_status_2
;set_extd_audio_status_1:
	mov	edx, CODEC_EXT_AUDIO_CTRL_REG
	call	codec_write
	;jc	short cconfig_error

set_sample_rate:
	;movzx	eax, word [audio_freq]
	mov	ax, [audio_freq]
	mov	edx, CODEC_PCM_FRONT_DACRATE_REG ; 2Ch ; PCM Front DAC Rate
;set_extd_audio_status_2:
	call	codec_write

	; 01/09/2020
	; set AD1980 MCB register (Index 76h) to 0C00h
	; (CLDIS, HPSEL)
	;mov	ax, 0C00h
	;mov	edx, CODEC_MISC_CRTL_BITS_REG ; 76h 
	;			; Miscellaneous Control Bit Register
	;call	codec_write
	;

        mov	eax, audio_bdl_buff
  
	; 12/11/2016 - Erdogan Tan 
	; (Ref: KolibriOS, vt823x.asm, 'create_primary_buff')
	;mov	edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_TABLE_PTR
        ; 06/08/2022
	sub	edx, edx
	;mov	dl, VIADEV_PLAYBACK + VIA_REG_OFFSET_TABLE_PTR
	; edx = 0
	call	ctrl_io_w32

	;call	codec_check_ready

  	mov	dx, VIADEV_PLAYBACK + VIA_REG_OFS_PLAYBACK_VOLUME_L
        ;mov	eax, 2	; 31
	mov	al, 31
        sub	al, [audio_master_volume_l]
	call	ctrl_io_w8

	;call	codec_check_ready

        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFS_PLAYBACK_VOLUME_R
        ;mov	ax, 2	; 31
	mov	al, 31
        sub	al, [audio_master_volume_r]
	call    ctrl_io_w8

	;call	codec_check_ready
;
;
; All set. Let's play some music.
;
;
       	;mov    dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
        ;mov    ax, VIA8233_REG_TYPE_16BIT or VIA8233_REG_TYPE_STEREO or 0xfffff or 0xff000000
        ;call   ctrl_io_w32

	;call	codec_check_ready

	; 08/12/2016
	; 07/10/2016
        ;;mov    al, 1
        ;mov	al, 31
	; 22/07/2020
	mov	al, 0FFh
	call    set_VT8233_LastValidIndex

	mov	byte [audio_play_cmd], 1 ; play command (do not stop) !

	; 22/07/2020
	;mov	byte [audio_flag], 0  ; clear half buffer flag

vt8233_play: ; continue to play
	; 22/04/2017
	;mov	al, VIA_REG_CTRL_INT
       	;or	al, VIA_REG_CTRL_START
        ;;mov	al, VIA_REG_CTRL_AUTOSTART + VIA_REG_CTRL_START
	; 22/07/2020	
	mov	al, VIA_REG_CTRL_AUTOSTART + VIA_REG_CTRL_START + VIA_REG_CTRL_INT_FLAG

	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8
	;call	codec_check_ready
	;retn
	;jmp	codec_check_ready
	retn

;input AL = index # to stop on
set_VT8233_LastValidIndex:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5
	; 23/07/2020
	; 10/06/2017
	; 21/04/2017 (TRDOS 386 kernel, 'audio.s')
	; 24/03/2017 - 'PLAYER.COM' ('via_wav.asm' - 29/11/2016) 
	; 19/11/2016
	; 14/11/2016 - Erdogan Tan (Ref: VIA VT8235.PDF, Page 110)
	; 12/11/2016 - Erdogan Tan
	; (Ref: KolibriOS, vt823x.asm, 'create_primary_buff')
	;push	edx
	;push	ax
	push	eax ; 23/07/2020
	;push	ecx
	movzx	eax, word [audio_freq] ; Hertz
	mov	edx, 100000h ; 2^20 = 1048576
	mul	edx
	mov	ecx, 48000	
	div	ecx
	;and	eax, 0FFFFFh
	;pop	ecx
	;pop	dx 
	pop	edx ; 23/07/2020
	shl	edx, 24  ; STOP Index Setting: Bit 24 to 31
	or	eax, edx
	; 19/11/2016
	cmp	byte [audio_bps], 16
	jne	short sLVI_1
	or	eax, VIA8233_REG_TYPE_16BIT
sLVI_1:
	cmp	byte [audio_stmo], 2
	jne	short sLVI_2
	or	eax, VIA8233_REG_TYPE_STEREO
sLVI_2:
	;mov	edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
	; 06/08/2022
	sub	edx, edx
	mov	dl, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
	call    ctrl_io_w32
	;call	codec_check_ready
	;pop	edx
	retn

vt8233_pause: ; pause
	; 10/06/2017
	; 22/04/2017
	;mov	al, VIA_REG_CTRL_INT
        ;or	al, VIA_REG_CTRL_PAUSE
	; 23/07/2020
	mov	al, VIA_REG_CTRL_PAUSE+VIA_REG_CTRL_INT_FLAG+VIA_REG_CTRL_AUTOSTART
	
	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8
	;call	codec_check_ready
	;retn
	;jmp	codec_check_ready
	retn

vt8233_reset: 
	; 22/04/2017
	; reset VT8237R (vt8233) Audio Controller
	;cmp	byte [audio_play_cmd], 1
	;jna	short vt8233_rst_0
	mov	byte [audio_play_cmd], 0 ; stop !
vt8233_rst_0:
	call	reset_codec
	jc	short vt8233_rst_1 ; codec error !
	; eax = 1
	call	codec_io_w16 ; w32
	call	channel_reset
vt8233_rst_1:
vt8233_vol_1:	; 06/08/2022
	retn

vt8233_volume:
	; set VT8237R (vt8233) sound volume level
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 24/04/2017
	; 22/04/2017
	; bl = component (0 = master/playback/lineout volume)
	; cl = left channel volume level (0 to 31)
	; ch = right channel volume level (0 to 31)

	or	bl, bl
	jnz	short vt8233_vol_1 ; temporary !
	mov	ax, 1F1Fh ; 31,31
	cmp	cl, al
	ja	short vt8233_vol_1 ; temporary !
	cmp	ch, ah
	ja	short vt8233_vol_1 ; temporary !
	mov	[audio_master_volume], cx
	sub	ax, cx
	;mov	edx, CODEC_MASTER_VOL_REG ; 02h ; Line Out
	; 06/08/2022
	sub	edx, edx
	mov	dl, CODEC_MASTER_VOL_REG ; 02h ; Line Out
	; 06/08/2022
	jmp	codec_write
	;call	codec_write
;vt8233_vol_1:
	;retn

; CODE for SOUND BLASTER 16

DetectSB:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 24/04/2017
	;pushad
ScanPort:
	; 06/08/2022
	mov	bx, 0210h	; start scanning ports
				; 210h, 220h, .. 260h
	; 06/08/2022
	xor	ecx, ecx
	mov	dh, bh
ResetDSP:       
	;mov	dx, bx		; try to reset the DSP.
	;add	dx, 06h
	; 06/08/2022
	mov	dl, bl
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
	; 06/08/2022
	add	dl, 08h
	;mov	cx, 100
	mov	cl, 100
WaitID:
	in	al, dx
	or      al, al
	js      short GetID
	loop    WaitID
	jmp     short NextPort
GetID:          
	;sub	dx, 04h
	; 06/08/2022
	sub	dl, 04h
	in	al, dx
	cmp     al, 0AAh
	je      short Found
	;add	dx, 04h
	; 06/08/2022
	add	dl, 04h
	loop    WaitID
NextPort:
	;add	bx, 10h		; if not response,
	; 06/08/2022
	add	bl, 10h
	;cmp	bx, 260h	; try the next port.
	cmp	bl, 60h
	jbe     short ResetDSP
	stc
	retn
Found:
	mov     [audio_io_base], bx	; SB Port Address Found!
ScanIRQ:
SetIrqs:
	sub 	al, al ; 0
	mov 	[IRQnum], al ; reset
	mov	[audio_intr], al ; reset

	; ah > 0 -> set IRQ vector
	; al = IRQ number
	;mov	ax, 103h ; IRQ 3
	;call	set_hardware_int_vector
	;mov	ax, 104h ; IRQ 4
	;call	set_hardware_int_vector
	mov	ax, 105h ; IRQ 5
	call	set_hardware_int_vector
	mov	ax, 107h ; IRQ 7
	call	set_hardware_int_vector

	mov     dx, [audio_io_base] ; tells to the SB to
	;add	dx, 0Ch		    ; generate a IRQ!
	; 06/08/2022
	add	dl, 0Ch
WaitSb:
	in	al, dx
	or      al, al
	js      short WaitSb
	mov     al, 0F2h
	out	dx, al

	xor     ecx, ecx	; wait until IRQ level
WaitIRQ: 
	mov	al, [IRQnum]
	cmp     al, 0 ; is changed or timeout.
	ja	short IrqOk
	dec	cx
	jnz	short WaitIRQ
	jmp	short RestoreIrqs
IrqOk:
	mov	[audio_intr], al ; set   
	mov     dx, [audio_io_base]
	;add	dx, 0Eh
	; 06/08/2022
	add	dl, 0Eh
	in	al, dx	; SB acknowledge.
	mov	al, 20h
	out	20h, al	; Hardware acknowledge.

RestoreIrqs:
	; ah = 0 -> reset IRQ vector
	; al = IRQ number
	;mov	ax, 3 ; IRQ 3
	;call	set_hardware_int_vector
	;mov	ax, 4 ; IRQ 4
	;call	set_hardware_int_vector
	mov	ax, 5 ; IRQ 5
	call	set_hardware_int_vector
	mov	ax, 7 ; IRQ 7
	call	set_hardware_int_vector

	xor	edx, edx
	mov	[audio_dev_id], edx ; 0
	mov	[audio_vendor], edx ; 0
	mov	[audio_stats_cmd], edx ; 0

	;popad

	cmp     byte [audio_intr], 1 ; IRQ level was changed?
	
	retn

%macro	SbOut	1
%%Wait:
	in	al, dx
	or	al, al
	js	short %%Wait
	mov	al, %1
	out	dx, al
%endmacro

SbInit_play:
	; 06/08/2022 - TRDOS 386 Kernel v2.0.5 
	; 22/10/2017
	; 20/10/2017
	; 06/10/2017
	; 13/07/2017 - 09/08/2017
	; 24/04/2017 - 15/05/2017 - 24/06/2017
	;pushad
SetBuffer:
	;mov	byte [DmaFlag], 0

	mov	ebx, [audio_dma_buff] ; physical addr of DMA buff
	mov	edi, ebx
	mov     ecx, [audio_dmabuff_size]

	cmp	byte [audio_bps], 16
	jne	short sbInit_0 ; set 8 bit DMA buffer
	
	; 09/08/2017
	; convert byte count to word count
	shr	ecx, 1
	dec	ecx ; word count - 1
	; convert byte offset to word offset
	shr	ebx, 1

	; 16 bit DMA buffer setting (DMA channel 5)
	mov     al, 05h  ; set mask bit for channel 5  (4+1)
	out	0D4h, al
	
	xor     al, al   ; stops all DMA processes on selected channel
	out	0D8h, al ; clear selected channel register

	mov     al, bl	 ; byte 0 of DMA buffer offset in words (physical) 
	out	0C4h, al ; DMA channel 5 port number

	mov     al, bh   ; byte 1 of DMA buffer offset in words (physical)   
	out	0C4h, al
	
	; 09/08/2017
	shr	ebx, 15	 ; complete 16 bit shift
	and	bl, 0FEh ; clear bit 0 (not necessary, it will be ignored)

	mov     al, bl   ; byte 2 of DMA buffer address (physical) 
	out	8Bh, al  ; page register port addr for channel 5 ; 13/07/2017

	mov     al, cl   ; low byte of DMA count - 1
	out	0C6h, al ; count register port addr for channel 1

	mov     al, ch   ; high byte of DMA count - 1
	out	0C6h, al

	; channel 5, read, autoinitialized, single mode
	;mov	al, 49h
	mov	al, 59h  ; 06/10/2017 
	out	0D6h, al ; DMA mode register port address

	mov     al, 01h  ; clear mask bit for channel 1
	out	0D4h, al ; DMA mask register port address

	jmp	short ClearBuffer

sbInit_0:    
	dec     ecx	; 09/08/2017

	; 8 bit DMA buffer setting (DMA channel 1)
	mov     al, 05h ; set mask bit for channel 1  (4+1)
	out	0Ah, al ; DMA mask register

	xor     al, al  ; stops all DMA processes on selected channel
	out	0Ch, al ; clear selected channel register

	mov     al, bl	; byte 0 of DMA buffer address (physical)   
	out	02h, al ; DMA channel 1 port number

	mov     al, bh  ; byte 1 of DMA buffer address (physical)   
	out	02h, al

	shr	ebx, 16

	mov     al, bl  ; byte 2 of DMA buffer address (physical)   
	out	83h, al ; page register port addr for channel 1

	mov     al, cl  ; low byte of DMA count - 1
	out	03h, al ; count register port addr for channel 1

	mov     al, ch  ; high byte of DMA count - 1
	out	03h, al

	; channel 1, read, autoinitialized, single mode
	;mov	al, 49h
	mov	al, 59h ; 06/10/2017 
	out	0Bh, al ; DMA mode register port address

	mov     al, 01h ; clear mask bit for channel 1
	out	0Ah, al ; DMA mask register port address

ClearBuffer:
	;;mov	edi, [audio_dma_buff]
	;;mov	ecx, [audio_dmabuff_size]
	;inc	ecx
	;mov	al, 80h
	;;cld
	;rep	stosb
SetIrq:
	;mov	ebx, SbIrqhandler
	;mov	al, [audio_intr] ; IRQ number	
	;call	set_dev_IRQ_service
	;; SETUP (audio) INTERRUPT CALLBACK SERVICE
	;mov	bl, [audio_intr] ; IRQ number
	;mov	bh, [audio_cb_mode]
	;inc	bh  ; 1 = Signal Response Byte method (fixed value)
	;	    ; 2 = Callback service method
	;	    ; 3 = Auto Increment S.R.B. method 	
	;mov	cl, [audio_srb]
	;mov	edx, [audio_cb_addr]
	;mov	al, [audio_user]
 	;call	set_irq_callback_service
ResetDsp:
	mov     dx, [audio_io_base]
	;add	dx, 06h
	; 06/08/2022
	add	dl, 06h
	mov     al, 1
	out	dx, al

	in	al, dx
	in	al, dx
	in	al, dx
	in	al, dx

	xor     al, al
	out	dx, al

	;mov	cx, 100
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 100
	sub	ah, ah ; 0
WaitId:         
	mov     dx, [audio_io_base]
	;add	dx, 0Eh
	; 06/08/2022
	add	dl, 0Eh
	in	al, dx
	or      al, al
	js      short sb_GetId
	loop    WaitId
	jmp     sb_Exit
sb_GetId:
	mov     dx, [audio_io_base]
	;add	dx, 0Ah
	; 06/08/2022
	add	dl, 0Ah
	in	al, dx
	cmp     al, 0AAh
	je      short SbOk
	loop    WaitId
	jmp	sb_Exit
SbOk:
	mov     dx, [audio_io_base]
	;add	dx, 0Ch
	; 06/08/2022
	add	dl, 0Ch
	SbOut   0D1h ; Turn on speaker
	SbOut   41h ; 8h bit or 16 bit transfer
	mov	bx, [audio_freq] ; sampling rate (Hz)
	SbOut	bh ; sampling rate high byte
	SbOut	bl ; sampling rate low byte

	; 22/05/2017
	call	sb16_volume_initial ; 15/05/2017
	; 20/05/2017
	;call	sb16_volume

StartDma: 
	; autoinitialized mode
	cmp	byte [audio_bps], 16 ; 16 bit samples
	je	short sb_play_1
	; 8 bit samples
	mov	bx, 0C6h ; 8 bit output (0C6h)
	cmp	byte [audio_stmo], 2 ; 1 = mono, 2 = stereo
	jb	short sb_play_2
	mov	bh, 20h	; 8 bit stereo (20h)
	jmp	short sb_play_2
sb_play_1:
	; 16 bit samples
	mov	bx, 10B6h ; 16 bit output (0B6h)
	cmp	byte [audio_stmo], 2 ; 1 = mono, 2 = stereo
	jb	short sb_play_2
	add	bh, 20h	; 16 bit stereo (30h)
sb_play_2:     
	; PCM output (8/16 bit mono autoinitialized transfer)
	SbOut   bl ; bCommand
	SbOut	bh ; bMode
	mov	ebx, [audio_dmabuff_size]  ; 15/05/2017
	shr	ebx, 1 ; half buffer size
	; 20/10/2017	
	cmp	byte [audio_bps], 16 ; 16 bit DMA
	jne	short sb_play_3
	shr	ebx, 1 ; byte count to word count
sb_play_3: 
	;dec	bx  ; wBlkSize is one less than the actual size 
	; 06/08/2022
	dec	ebx
	SbOut   bl
	SbOut   bh

	mov	byte [audio_play_cmd], 1 ; playing !

	;; Set Voice and master volumes
	;mov	dx, [audio_io_base]
	;add	dl, 4 ; Mixer chip Register Address Port
	;SbOut	30h   ; select Master Volume Register (L)
	;inc	dl    ; Mixer chip Register Data Port
	;SbOut	0F8h  ; Max. volume value is 31 (31*8)
	;dec	dl
	;SbOut	31h   ; select Master Volume Register (R)
	;inc	dl
	;SbOut	0F8h  ; Max. volume value is 31 (31*8)
	;dec	dl
	;SbOut	32h   ; select Voice Volume Register (L)
	;inc	dl
	;SbOut	0F8h  ; Max. volume value is 31 (31*8)
	;dec	dl
	;SbOut	33h   ; select Voice Volume Register (R)
	;inc	dl
	;SbOut	0F8h  ; Max. volume value is 31 (31*8)	
	;;
	;dec	dl
	;SbOut	44h   ; select Treble Register (L)
	;inc	dl
	;SbOut	0F0h  ; Max. Treble value is 15 (15*16)
	;dec	dl
	;SbOut	45h   ; select Treble Register (R)
	;inc	dl
	;SbOut	0F0h  ; Max. Treble value is 15 (15*16)
	;dec	dl
	;SbOut	46h   ; select Bass Register (L)
	;inc	dl
	;SbOut	0F0h  ; Max. Bass value is 15 (15*16)
	;dec	dl
	;SbOut	47h   ; select Bass Register (R)
	;inc	dl
	;SbOut	0F0h  ; Max. Bass value is 15 (15*16)	

sb_Exit:           
	;popad
	retn

sb16_int_handler:
	; Interrupt Handler for Sound Blaster 16 Audio Card
	; Note: called by 'dev_IRQ_service'
	; 20/10/2017
	; 12/10/2017
	; 10/10/2017 
	; 12/05/2017, 09/10/2017
	; 24/04/2017 (TRDOS 386 kernel, 'audio.s')
	; 10/03/2017 - 'PLAYWAV.PRG' ('playwav.s') 

	;push	eax ; * must be saved !
	;push	ebx ; * must be saved !
	;push	ecx
	;push	edx
	;push	esi
	;push	edi

	mov     dx, [audio_io_base]
	; 20/10/2017
	add     dl, 0Fh ; 2xFh (DSP 16 bit intr ack)
	cmp	byte [audio_bps], 16
	je	short sb_irq_16bit_ack
sb_irq_8bit_ack:
	dec	dl  ; 2xEh (DSP 8 bit intr ack)
sb_irq_16bit_ack:
	in	al, dx

	;cmp	byte [audio_busy], 0
	;ja	short sb_irq_h3

	;mov	byte [audio_busy], 1

	cmp	byte [audio_play_cmd], 1
	jnb	short sb_irq_h1
sb_irq_h0:
	call	sb16_stop
	jmp	short sb_irq_h3
sb_irq_h1:
	;call	sb16_tuneloop
	; 09/10/2017
sb16_tuneloop:
	mov	edi, [audio_dma_buff]
	mov	ecx, [audio_dmabuff_size]
	shr	ecx, 1 ; dma buff size / 2 = half buffer size

	; 22/05/2017
	test	byte [audio_flag], 1  ; Current flag value
	jz	short sb_tlp1 ; EOL (Half Buffer 1 must be filled)
	; FLAG (Half Buffer 2 must be filled)
	add	edi, ecx
	; 15/05/2017
sb_tlp1: 
	mov	esi, [audio_p_buffer] ; phy addr of audio buff
	;rep	movsb
	shr	ecx, 2 ; half buff size / 4
	rep	movsd 
	;retn

	; 10/10/2017
	; switch flag value
	xor	byte [audio_flag], 1

	; 12/10/2017
	; [audio_flag] = 0 : Playing dma half buffer 2 (odd intr count)
			   ; Next buffer (to update) is dma half buff 1
	; 	       = 1 : Playing dma half buffer 1 (even intr count)
			   ; Next buffer (to update) is dma half buff 2

sb_irq_h3:
	;mov	byte [audio_busy], 0

	;pop	edi
	;pop	esi
	;pop	edx
	;pop	ecx
	;pop	ebx ; * must be restored !
	;pop	eax ; * must be restored !
	
	retn

sb16_volume:
	; 06/08/2022 (TRDOS 386 v2.0.5)
	; 22/10/2017
	; mov [audio_master_volume_l], cl 
	; mov [audio_master_volume_h], ch 
	mov	[audio_master_volume], cx
sb16_volume_initial:
	;push	dx ; DX (port address) must be saved
	; 06/08/2022
	push	edx
	mov	dx, [audio_io_base]
	;add	dx, 4 ; Mixer chip address port
	; 06/08/2022
	add	dl, 4
	mov	al, 22h ; master volume
	out	dx, al
	;inc	dx
	; 06/08/2022
	inc	edx
	mov	ah, [audio_master_volume_l]
	shr	ah, 2 ; 32 -> 8 level
	shl	ah, 5 ; bit 5 to 7
	mov	al, [audio_master_volume_r]	
	shr	al, 2 ; 32 -> 8 level
	;and	al, 0Fh
	shl	al, 1 ; bit 1 to 3
	or	al, ah
	out	dx, al
	;pop	dx ; DX (port address) must be restored
	; 06/08/2022
	pop	edx
	retn

sb16_pause:
	; 06/08/2022 (TRDOS 386 v2.0.5)
	mov	dx, [audio_io_base]
	;add	dx, 0Ch ; Command & Data Port
	; 06/08/2022
	add	dl, 0Ch
	cmp	byte [audio_bps], 16 ; 16 bit samples
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

sb16_continue:
	; 06/08/2022 (TRDOS 386 v2.0.5)
	mov	dx, [audio_io_base]
	;add	dx, 0Ch ; Command & Data Port
	; 06/08/2022
	add	dl, 0Ch
	cmp	byte [audio_bps], 16 ; 16 bit samples
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

sb16_stop:
	; 06/08/2022 (TRDOS 386 v2.0.5)
	; 24/04/2017
	cmp	byte [audio_play_cmd], 0
	jna	short sb16_stop_4

	; 22/05/2017
	mov	dx, [audio_io_base]
	;add	dx, 0Ch
	; 06/08/2022
	add	dl, 0Ch

	mov	bl, 0D9h ; exit auto-initialize 16 bit transfer
	; stop  autoinitialized DMA transfer mode 
	cmp	byte [audio_bps], 16 ; 16 bit samples
	je	short sb16_stop_1
	;mov	bl, 0DAh ; exit auto-initialize 8 bit transfer
	inc	bl
sb16_stop_1:
	SbOut	bl ; exit auto-initialize transfer command

	xor     al, al ; stops all DMA processes on selected channel

	cmp	byte [audio_bps], 16 ; 16 bit samples
	je	short sb16_stop_2
	out	0Ch, al ; clear selected channel register
	jmp	short sb16_stop_3

sb16_stop_2:
	out	0D8h, al ; clear selected channel register

sb16_stop_3:
	mov	byte [audio_play_cmd], 0 ; stop !
SbDone:
	;mov	dx, [audio_io_base]
	;add	dx, 0Ch
	SbOut   0D0h
	SbOut   0D3h
sb16_stop_4:
	retn

sb16_reset:
	; 06/08/2022 (TRDOS 386 v2.0.5)
	; 24/04/2017
	mov     dx, [audio_io_base] ; try to reset the DSP.
	;add	dx, 06h
	; 06/08/2022
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
	; 06/08/2022
	add	dl, 08h
	;mov	cx, 100
	sub	ecx, ecx
	mov	cl, 100
sbrstWaitID:
	in	al, dx
	or      al, al
	js      short sbrstGetID
	loop    sbrstWaitID
	stc
	retn
sbrstGetID:          
	;sub	dx, 04h
	; 06/08/2022
	sub	dl, 04h
	in	al, dx
	cmp     al, 0AAh
	je      short sb_rst_retn
	;add	dx, 04h
	; 06/08/2022
	add	dl, 04h
	loop    sbrstWaitID
sb_rst_retn:
	retn

ac97_codec_config:
	; 10/06/2017
	; 05/06/2017
	; 29/05/2017
	; 28/05/2017 (TRDOS 386, 'audio.s')
	; 07/11/2016 (Erdogan Tan)
	; Derived from 'codecConfig' procedure in 'CODEC.ASM'
	; .wav player for DOS by Jeff Leyda (02/09/2002)

	;; 'PLAYER.ASM'
	;; get ICH base address regs for mixer and bus master

init_ac97_controller: ; 10/06/2017
	mov	eax, [audio_dev_id]
        ;mov	al, NAMBAR_REG
        ;;call  pciRegRead16			; read PCI registers 10-11
        ;call	pciRegRead32
	;and	dx, IO_ADDR_MASK 		; mask off BIT0
	;;and	edx, IO_ADDR_MASK 

        ;mov	[NAMBAR], dx			; save audio mixer base addr

        ;mov    al, NABMBAR_REG
        ;;call	pciRegRead16
        ;call	pciRegRead32
	;and	dx, 0FFC0h ; IO_ADDR_MASK
	;;and	edx, 0FFC0h

        ;mov    [NABMBAR], dx			; save bus master base addr

	;mov	eax, [audio_dev_id]
        mov     al, PCI_CMD_REG
        ;call	pciRegRead8			; read PCI command register
        call	pciRegRead16
	or      dl, IO_ENA+BM_ENA               ; enable IO and bus master
        ;call 	pciRegWrite8
	call	pciRegWrite16

	; 'CODEC.ASM'

	; enable codec, unmute stuff, set output rate
;	; entry: [audio_freq] = desired sample rate
		
;	mov    	dx, [NAMBAR]               	
;	add    	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah  	  
;	in     	ax, dx
;	or	ax, 1
;	out	dx, ax 				; Enable variable rate audio

;       ;call    delay1_4ms
;       ;call    delay1_4ms
;       ;call    delay1_4ms
;       ;call    delay1_4ms

;	mov	ax, [audio_freq]		; sample rate

;	mov    	dx, [NAMBAR]               	
;	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch  	  
;	out	dx, ax 				; out sample rate
		
;       ;call	delay1_4ms
;       ;call	delay1_4ms
;       ;call	delay1_4ms
;       ;call	delay1_4ms

	;mov	dx, [NAMBAR]			; mixer base address
        ;add	dx, CODEC_RESET_REG  		; reset register
        ;mov	ax, 42
	;out	dx, ax                          ; reset

	;mov    dx, [NABMBAR]			; bus master base address
        ;add	dx, GLOB_STS_REG
        ;mov	ax, 2
	;out	dx, ax                         

        call    delay_100ms ; 29/05/2017

init_ac97_codec:
	; 10/06/2017
	; 29/05/2017
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, intelac97.asm)
	;	
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	in	eax, dx
	; ?
	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	cmp	eax, 0FFFFFFFFh ; -1
	je	short init_ac97_codec_err1

	test	eax, CTRL_ST_CREADY
	jnz	short _ac97_codec_ready

	call	reset_ac97_codec
	jc	short init_ac97_codec_err2

_ac97_codec_ready:
	mov	dx, [NAMBAR]
	;add	dx, 0 ; ac_reg_0 ; reset register
	out	dx, ax

	xor	eax, eax ; 0
	mov	dx, [NAMBAR]
	add	dx, CODEC_REG_POWERDOWN
	out	dx, ax

	; 10/06/2017
	; 29/05/2017
	; wait for 1 second
	mov	ecx, 1000 ; 1000*0.25ms = 1s
_ac97_codec_rloop:
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	;mov	dx, [NAMBAR]
	;add	dx, CODEC_REG_POWERDOWN
	in	ax, dx	
	and	ax, 0Fh
	cmp	al, 0Fh
	je	short _ac97_codec_init_ok
	loop	_ac97_codec_rloop 

init_ac97_codec_err1:
	stc
init_ac97_codec_err2:
	retn

_ac97_codec_init_ok:
        mov     al, 2 ; force set 16-bit 2-channel PCM
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	out	dx, eax

	;call	delay1_4ms

	; 10/06/2017
	call 	reset_ac97_controller

;	call 	setup_ac97_codec
;
;detect_ac97_codec:
;	retn

setup_ac97_codec:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 22/07/2020
	; 10/06/2017
	; 29/05/2017
	mov     eax, 0202h
	mov	[audio_master_volume], ax
	mov	ax, 1F1Fh ; 31, 31
	
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_VOL_REG	;02h 
	;xor	ax, ax 	; volume attenuation = 0 (max. volume)
	; 06/08/2022
	xor	eax, eax
	out     dx, ax
 
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_MONO_VOL_REG	;06h 
	;xor	ax, ax
  	out     dx, ax

  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCBEEP_VOL_REG	;0Ah 
  	;xor    ax, ax
  	out     dx, ax

  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCM_OUT_REG		;18h 
  	;xor    ax, ax
  	out     dx, ax

	mov     ax, 8008h ; Mute
  	mov     dx, [NAMBAR]
	; 22/07/2020
	add	dx, CODEC_PHONE_VOL_REG		;0Ch
				 ; AC97_PHONE_VOL ; TAD Input (Mono)
  	out     dx, ax

        mov	ax, 0808h
  	mov     dx, [NAMBAR]
        add	dx, CODEC_LINE_IN_VOL_REG ;10h ; Line Input (Stereo)
  	out     dx, ax

	;mov	ax, 0808h
  	mov     dx, [NAMBAR]
        add	dx, CODEC_CD_VOL_REG ;12h ; CR Input (Stereo)
  	out     dx, ax

	;mov	ax, 0808h
  	mov     dx, [NAMBAR]
        add	dx, CODEC_AUX_VOL_REG ;16h ; Aux Input (Stereo)
  	out     dx, ax

        ;call    delay1_4ms
        ;call    delay1_4ms
        ;call    delay1_4ms
        ;call    delay1_4ms

detect_ac97_codec:
        retn

set_ac97_bdl: ; Set AC97 (ICH) Buffer Descriptor List
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 17/06/2017
	; 11/06/2017
	; 28/05/2017
	; eax = dma buffer address = [audio_DMA_buff]
	; ecx = dma buffer buffer size = [audio_dmabuff_size]

	shr	ecx, 1 ; dma half buffer size
	mov	esi, ecx

        mov     edi, audio_bdl_buff	; get BDL address
	;mov	ecx, 32 / 2		; make 32 entries in BDL
	; 06/08/2022
	sub	ecx, ecx
	mov	cl, 16

	jmp	short s_ac97_bdl1 

s_ac97_bdl0:
	; set buffer descriptor 0 to start of data file in memory

 	mov	eax, [audio_dma_buff]	; Physical address of DMA buffer
 
s_ac97_bdl1:
	stosd				; store dmabuffer1 address

	mov	edx, eax

;
; Buffer Descriptors List
; As stated earlier, each buffer descriptor list is a set of (up to) 32 
; descriptors, each 8 bytes in length. Bytes 0-3 of a descriptor entry point
; to a chunk of memory to either play from or record to. Bytes 4-7 of an
; entry describe various control things detailed below.
; 
; Buffer pointers must always be aligned on a Dword boundry.
;
;

;IOC                     equ     BIT31	; Fire an interrupt whenever this
                                        ; buffer is complete.

;BUP                     equ     BIT30  ; Buffer Underrun Policy.
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

	mov	eax, esi ; DMA half buffer size
	add	edx, eax
	shr	eax, 1 ; count of 16 bit samples
	;or	eax, IOC+BUP
	or	eax, IOC ; 11/06/2017
	stosd

; 2nd buffer:

        mov	eax, edx ; Physical address of the 2nd half of DMA buffer	
	stosd		 ; store dmabuffer2 address

; set length to [audio_dmabuff_size]/2
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	mov	eax, esi ; DMA half buffer size
	shr	eax, 1 ; count of 16 bit samples
	;or	eax, IOC+BUP
	or	eax, IOC ; 11/06/2017
	stosd

	loop    s_ac97_bdl0
	
	retn

ac97_start_play:  
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 28/05/2017
	; Derived from 'playWav' procedure in 'ICHWAV.ASM'
	; .wav player for DOS by Jeff Leyda (02/09/2002)

	; set output rate
	; entry: [audio_freq] = desired sample rate

	AC97_EA_VRA equ 0001h ; 04/11/2023
		
	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah
	in     	ax, dx
	;or	ax, 1
	; 06/08/2022
	or	al, AC97_EA_VRA ; 1 ; 04/11/2023
	out	dx, ax				; Enable variable rate audio

       ;call    delay1_4ms
       ;call    delay1_4ms
       ;call    delay1_4ms
       ;call    delay1_4ms

	mov	ax, [audio_freq]		; sample rate

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch  	  
	out	dx, ax 				; out sample rate
		
       ;call    delay1_4ms
       ;call    delay1_4ms
       ;call    delay1_4ms
       ;call    delay1_4ms

;
; register reset the DMA engine. This may cause a pop noise on the output
; lines when the device is reset. Prolly a better idea to mute output, then
; reset.
;
        mov     dx, [NABMBAR]
        add     dx, PO_CR_REG                  ; set pointer to Cntl reg
        mov     al, RR                         ; set reset
        out     dx, al                         ; self clearing bit
;
;	mov	edi, audio_bdl_buff
;	mov	edx, [audio_dmabuff_size]
;	shr	edx, 1
;	mov	ecx, 32/2
;ac97_set_bdl_buffer:
;	; 1st half of DMA buffer
;	mov	eax, [audio_dma_buff]
;	push	eax
;	stosd
;	mov	eax, edx ; dma buffer size / 2
;	or	eax, IOC+BUP
;	stosd
;	pop	eax
;	; 2nd half of DMA buffer
;	add	eax, edx
;	stosd
;	mov	eax, edx ; dma buffer size / 2
;	or	eax, IOC+BUP
;	stosd
;	loop	ac97_set_bdl_buffer
 	
; tell the DMA engine where to find our list of Buffer Descriptors.
; this 32bit value is a flat mode memory offset (ie no segment:offset)
;
; write NABMBAR+10h with offset of buffer descriptor list
;
        mov	eax, audio_bdl_buff
	mov	dx, [NABMBAR]
	add	dx, PO_BDBAR_REG
	out	dx, eax
;
; All set. Let's play some music.
;
;
	;mov	eax, 31
	; 06/08/2022
	sub	eax, eax
	mov	al, 31
	call    set_ac97_LastValidIndex

	mov	byte [audio_play_cmd], 1 ; play command (do not stop) !

ac97_play: ; continue to play (after pause)
	; 11/06/2017
	; 29/05/2017
	; 28/05/2017
        mov     dx, [NABMBAR]
        add     dx, PO_CR_REG		; PCM out control register
        mov	al, IOCE+RPBM ; 29/05/2017
        ;mov	al, 1Dh ; (Ref: KolibriOS, intelac97.asm, 'play:')
	out     dx, al			; set start!

	;mov	byte [audio_play_cmd], 1 ; play command (do not stop) !

	retn

;input AL = index # to stop on
set_ac97_LastValidIndex:
	; 28/05/2017
	; Derived from 'setLastValidIndex' procedure in 'ICHWAV.ASM'
	; .wav player for DOS by Jeff Leyda (02/09/2002)
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	;mov	[audio_lvi], al ; for ac97_int_handler
	retn

ac97_volume:
	; 28/05/2017
	; bl = component (0 = master/playback/lineout volume)
	; cl = left channel volume level (0 to 31)
	; ch = right channel volume level (0 to 31)

	or	bl, bl
	jnz	short ac97_vol_1 ; temporary !
	mov	ax, 1F1Fh ; 31,31
	cmp	cl, al
	ja	short ac97_vol_1 ; temporary !
	cmp	ch, ah
	ja	short ac97_vol_1 ; temporary !
	mov	[audio_master_volume], cx
	sub	ax, cx
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_VOL_REG  ; 02h ; Line Out 
  	out     dx, ax
ac97_vol_1:
_ac97_ih5:	; 06/08/2022
	retn

ac97_int_handler:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 12/10/2017
	; 10/10/2017
	; 09/10/2017
	; 13/06/2017, 13/06/2017
	; 10/06/2017, 11/06/2017
	; Interrupt Handler for AC97 (ICH) Audio Controller
	; Note: called by 'dev_IRQ_service' 
	; 28/05/2017

	;push	eax ; * must be saved !
	;push	edx
	;push	ecx
	;push	ebx ; * must be saved !
	;push	esi
	;push	edi

	;cmp	byte [audio_busy], 1
	;jnb	_ac97_ih2 ; busy !

        mov     dx, GLOB_STS_REG
        add	dx, [NABMBAR]
	in	eax, dx

        cmp     eax, 0FFFFFFFFh ; -1
	;je	_ac97_ih3 ; exit
	; 06/08/2022
	je	short _ac97_ih5

        ;test	eax, 40h ; PCM Out Interrupt
	; 06/08/2022
	test	al, 40h
	jnz     short _ac97_ih0

	test	eax, eax
	;jz	_ac97_ih3 ; exit
	; 06/08/2022
	jz	short _ac97_ih5

 	;mov	dx, GLOB_STS_REG
        ;add	dx, [NABMBAR]
	out	dx, eax

	;jmp	_ac97_ih3 ; exit
	; 06/08/2022
	retn

_ac97_ih0:
	push	eax
	; 09/10/2017
	cmp	byte [audio_play_cmd], 1
	jb	short _ac97_ih4 ; stop command !

	;mov	byte [audio_busy], 1

	;mov	al, 10h
	;mov	dx, PO_CR_REG
        ;add	dx, [NABMBAR]
	;out	dx, al

	mov	ax, 1Ch ; FIFOE(=16)+BCIS(=8)+LVBCI(=4)
	mov	dx, PO_SR_REG
        add	dx, [NABMBAR]
	out	dx, ax

	mov	dx, PO_CIV_REG
        add	dx, [NABMBAR]
	in	al, dx

	;cmp	al, [audio_civ] ; [audio_flag]
	;je	short _ac97_ih2

	mov	[audio_civ], al
	dec	al
	;inc	al ; 11/06/2017
	and	al, 1Fh

        mov     dx, PO_LVI_REG
        add	dx, [NABMBAR]
        out	dx, al

	; 12/10/2017
	mov	al, [audio_civ]
	inc	al
	and	al, 1
	mov	[audio_flag], al 
	;; [audio_flag] : 0 = Buffer 1, 1 = Buffer 2
	;
	pop	eax
	;
	and     eax, 40h
        mov	dx, [NABMBAR]
	add	dx, GLOB_STS_REG
	out	dx, eax

	;; 13/06/2017
	;mov	al, 11h ; IOCE + RPBM
	;mov	dx, PO_CR_REG
        ;add	dx, [NABMBAR]
	;out	dx, al

ac97_tuneloop:
	; 09/10/2017
	mov	edi, [audio_dma_buff]
	mov	ecx, [audio_dmabuff_size]
	shr	ecx, 1 ; dma buff size / 2 = half buffer size

	; 12/10/2017
	cmp 	byte [audio_flag], 0
	ja	short _ac97_ih1  ; Playing Half Buffer 2 (Current: FLAG)
	; Playing Half Buffer 1 (Current: EOL)	
	add	edi, ecx
_ac97_ih1: 
	; Update half buffer 2 while playing half buffer 1 (next: FLAG)
	; Update half buffer 1 while playing half buffer 2 (next: EOL)

	mov	esi, [audio_p_buffer] ; phy addr of audio buff
	shr	ecx, 2 ; half buff size / 4
	rep	movsd 

	; 10/10/2017
	; switch flag value
	xor	byte [audio_flag], 1
	; 12/10/2017
	; [audio_flag] = 0 : Playing dma half buffer 2 (even index value)
			   ; Next buffer (to update) is dma half buff 1
	; 	       = 1 : Playing dma half buffer 1 (odd index value)
			   ; Next buffer (to update) is dma half buff 2
_ac97_ih2:
	;mov	byte [audio_busy], 0
_ac97_ih3:
	;pop	edi
	;pop	esi
	;pop	ebx ; * must be restored !
	;pop	ecx
	;pop	edx
	;pop	eax ; * must be restored !

	retn

_ac97_ih4:
	; 09/10/2017
	call	_ac97_stop
	;
	pop	eax
	;
	and     eax, 40h
        mov	dx, [NABMBAR]
	add	dx, GLOB_STS_REG
	out	dx, eax

	;; 13/06/2017
	;mov	al, 11h ; IOCE + RPBM
	;mov	dx, PO_CR_REG
        ;add	dx, [NABMBAR]
	;out	dx, al

	; 10/10/2017
	;jmp	short _ac97_ih3  ; exit
	retn

ac97_stop: 
	; 28/05/2017
	mov	byte [audio_play_cmd], 0 ; stop !
_ac97_stop: ; 09/10/2017
	; 29/05/2017
	;mov	dx, [NABMBAR]
	;add	dx, PO_CR_REG
	;mov	al, 0
	;out	dx, al

	; 11/06/2017
	xor	al, al ; 0
	call	ac97_po_cmd

	; (Ref: KolibriOS, intelac97.asm, 'stop:')
	; Clear FIFOE, BCIS, LVBCI (Ref: Intel ICH hub manual)
	mov     ax, 1Ch
	mov     dx, [NABMBAR]
	add     dx, PO_SR_REG
	out     dx, ax

	;retn

	; 11/06/2017
	mov     al, RR
ac97_po_cmd:
	 ;11/06/2017
	; 29/05/2017
	mov     dx, [NABMBAR]
        add     dx, PO_CR_REG		; PCM out control register
	out	dx, al
	retn

ac97_pause:
	; 11/06/2017
	; 29/05/2017
	mov 	al, IOCE
	jmp	short ac97_po_cmd

reset_ac97_controller:
	; 10/06/2017
	; 29/05/2017
	; 28/05/2017
	; reset AC97 audio controller registers
	xor     eax, eax
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

	retn

ac97_reset:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 10/06/2017
	; 29/05/2017
	; 28/05/2017
	call	reset_ac97_controller
	; 29/05/2017
	;jmp	reset_ac97_codec
reset_ac97_codec:
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
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, intelac97.asm)
        ;mov	eax, 6
	; 06/08/2022
	sub	eax, eax
	mov	al, 6
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	out	dx, eax

	;mov	ecx, 10	; total 1s
	; 06/08/2022
	xor	ecx, ecx
	mov	cl, 10
_warm_ac97c_rst_wait:
	push	ecx
	call	delay_100ms
	pop	ecx

	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	test	eax, CTRL_ST_CREADY
	jnz	short _warm_ac97c_rst_ok

        dec     ecx
        jnz     short _warm_ac97c_rst_wait

_warm_ac97c_rst_fail:
        stc
_warm_ac97c_rst_ok:
	retn

cold_ac97codec_reset:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 28/05/2017 - Erdogan Tan (Ref: KolibriOS, intelac97.asm)
        ;mov	eax, 2
	; 06/08/2022
	xor	eax, eax
	mov	al, 2
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	out	dx, eax

	call	delay_100ms 	; wait 100 ms
	call	delay_100ms 	; wait 100 ms
	call	delay_100ms 	; wait 100 ms
	call	delay_100ms 	; wait 100 ms

	;mov	ecx, 16	; total 20*100 ms = 2s
	; 06/08/2022
	xor	ecx, ecx
	mov	cl, 16
_cold_ac97c_rst_wait:
	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	test	eax, CTRL_ST_CREADY
	jnz	short _cold_ac97c_rst_ok

	push	ecx
	call	delay_100ms
	pop	ecx

        dec     ecx
        jnz     short _cold_ac97c_rst_wait

_cold_ac97c_rst_fail:
        stc
_cold_ac97c_rst_ok:
	retn

sb16_current_sound_data:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 20/08/2017
	; 24/06/2017
	; 22/06/2017
	; get current sound (PCM out) data for graphics
	; (for Sound Blaster 16)
	; ebx = Physical address (on page boundary)
	; ecx = Byte count
	; [audio_buff_size]

	;;mov	edi, [audio_buff_size]
	;mov	edi, [audio_dmabuff_size]
	;mov	esi, [audio_dma_buff]
	cmp	edi, ecx
	jnb	short sb16_gcd_0
	mov	ecx, edi
sb16_gcd_0:
	; 06/08/2022
	xor	eax, eax
	; 20/08/2017
	cmp	byte [audio_bps], 16
	jne	short sb16_gcd_1 ; 8 bit DMA channel
	in	al, 0C6h ; DMA channel 5 count register
	;mov	dl, al
	; 06/08/2022
	mov	ah, al
	in	al, 0C6h
	;mov	dh, al
	;movzx	eax, dx
	; 06/08/2022
	xchg	ah, al
	shl	eax, 1 ; word count -> byte count
	jmp	short sb16_gcd_2	
sb16_gcd_1:
	in	al, 03h ; DMA channel 1 count register
	;mov	dl, al
	; 06/08/2022
	mov	ah, al
	in	al, 03h
	;mov	dh, al
	;movzx	eax, dx
	; 06/08/2022
	xchg	ah, al
	jmp	short sb16_gcd_2
;sb16_gcd_2:	
;	cmp	eax, ecx
;	jnb	short sb16_gcd_3 
;	; remain count < graphics bytes
;	mov	eax, ecx ; fix remain count to data size
;sb16_gcd_3:	
;	sub	edi, eax
;	jna	short sb16_gcd_4
;	add	esi, edi ; dma buffer offset
;sb16_gcd_4:
;	mov	edi, ebx ; buffer address (for graphics) 
;	mov	[u.r0], ecx
;	rep	movsb
;	retn

get_current_sound_data:
	; 24/06/2017
	; 22/06/2017
	; get current sound (PCM out) data for graphics
	;
	; ebx = Physical address (on page boundary)
	; ecx = Byte count
	; [audio_buff_size]

	;mov	edi, [audio_buff_size]
	mov	edi, [audio_dmabuff_size]
	mov	esi, [audio_dma_buff]
	cmp	byte [audio_device], 2
	jb	short sb16_current_sound_data ; = 1
	shr	edi, 1
	cmp	edi, ecx
	jnb	short gcd_0
	mov	ecx, edi
gcd_0:
	cmp	byte [audio_device], 3
	jb	short ac97_current_sound_data ; = 2
	; = 3
vt8233_current_sound_data:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 22/06/2017
	; 21/06/2017
	; get current sound (PCM out) data for graphics
	; (for VT 8233, VT 8237R)
	; ebx = Physical address (on page boundary)
	; ecx = Byte count
	; [audio_buff_size]
	
	;;mov	edi, [audio_buff_size]
	;mov	edi, [audio_dmabuff_size]
	;mov	esi, [audio_dma_buff]
	;shr	edi, 1
	;cmp	edi, ecx
	;jnb	short vt8233_gcd_1
	;mov	ecx, edi
vt8233_gcd_1:
	;mov	edx, VIA_REG_OFFSET_CURR_COUNT
	; 06/08/2022
	xor	edx, edx
	mov	dl, VIA_REG_OFFSET_CURR_COUNT
	call	ctrl_io_r32
	mov	edx, eax ; remain count (bits 23-0),
			 ; SGD index (bits 31-24) 
	and	edx, 1000000h ; SGD index (0 = 1st half)
	jz	short vt8233_gcd_2
	; the second half of DMA buffer
	add	esi, edi 
vt8233_gcd_2:	
	and	eax, 0FFFFFFh ; bits 23-0
ac97_gcd_2:
sb16_gcd_2:
	cmp	eax, ecx
	jnb	short vt8233_gcd_3 
	; remain count < graphics bytes
	mov	eax, ecx ; fix remain count to data size
vt8233_gcd_3:
	sub	edi, eax
	jna	short vt8233_gcd_4
	add	esi, edi ; dma buffer offset
vt8233_gcd_4:
	mov	edi, ebx ; buffer address (for graphics) 
	mov	[u.r0], ecx
	rep	movsb
vt8233_gcd_5:
	retn

ac97_current_sound_data:
	; 23/06/2017
	; 22/06/2017
	; get current sound (PCM out) data for graphics
	; (for AC'97, ICH)
	; ebx = Physical address (on page boundary)
	; ecx = Byte count
	; [audio_buff_size]
	
	;;mov	edi, [audio_buff_size]
	;mov	edi, [audio_dmabuff_size]
	;mov	esi, [audio_dma_buff]
	;shr	edi, 1
	;cmp	edi, ecx
	;jnb	short ac97_gcd_0
	;mov	ecx, edi
ac97_gcd_0:
	mov	dx, PO_CIV_REG ; Position In Current Buff Reg
	add	dx, [NABMBAR]
	in	al, dx ; current index value
	test	al, 1
	jz	short ac97_gcd_1
	add	esi, edi
ac97_gcd_1:
	xor	eax, eax
	mov	dx, PO_PICB_REG ; Position In Current Buff Reg
	add	dx, [NABMBAR]
	in	ax, dx ; remain dwords
	shl	eax, 2 ; remain bytes ; 23/06/2017 
	jmp	short ac97_gcd_2
;	cmp	eax, ecx
;	jnb	short ac97_gcd_2 
;	; remain count < graphics bytes
;	mov	eax, ecx ; fix remain count to data size
;ac97_gcd_2:	
;	sub	edi, eax
;	jna	short ac97_gcd_3
;	add	esi, edi ; dma buffer offset
;ac97_gcd_3:
;	mov	edi, ebx ; buffer address (for graphics) 
;	mov	[u.r0], ecx
;	rep	movsb
;	retn

sb16_get_dma_buff_off:
	; 28/10/2017
	; 24/06/2017
	; 22/06/2017
	; get current (PCM OUT DMA buffer) pointer
	; (for Sound Blaster 16)

	;mov	ecx, [audio_dmabuff_size]
	;xor	ebx, ebx
	;shr	ecx, 1
sb16_gdmabo_0:
	; 28/10/2017
	cmp	byte [audio_bps], 16
	jne	short sb16_gdmabo_1 ; 8 bit DMA channel
	; 16 bit DMA channel
	in	al, 0C6h ; DMA channel 5 count register
	mov	dl, al	
	in	al, 0C6h
	mov     dh, al
	movzx	eax, dx
	shl	eax, 1 ; word count -> byte count
	jmp	short sb16_gdmabo_2	
sb16_gdmabo_1:
	in	al, 03h ; DMA channel 1 count register
	mov	dl, al	
	in	al, 03h
	mov     dh, al
	movzx	eax, dx
	jmp	short sb16_gdmabo_2

get_dma_buffer_offset:
	; 24/06/2017
	; 22/06/2017
	; get current sound (PCM out) data for graphics
	;
	; ebx = Physical address (on page boundary)
	; ecx = Byte count
	; [audio_buff_size]

	mov	ecx, [audio_dmabuff_size]
	xor	ebx, ebx
gdmabo_0:
	cmp	byte [audio_device], 2
	jb	short sb16_get_dma_buff_off
	je	short ac97_get_dma_buff_off

vt8233_get_dma_buff_off:
	; 06/08/2022 - TRDOS 386 v2.0.5
	; 24/06/2017
	; 22/06/2017
	; get current (PCM OUT DMA buffer) pointer
	; (for VT 8233, VT 8237R)
	
	;mov	ecx, [audio_dmabuff_size]
	;xor	ebx, ebx
	shr	ecx, 1
vt8233_gdmabo_0:
	;mov	edx, VIA_REG_OFFSET_CURR_COUNT
	; 06/08/2022
	xor	edx, edx
	mov	dl, VIA_REG_OFFSET_CURR_COUNT
	call	ctrl_io_r32
	mov	edx, eax ; remain count (bits 23-0),
			 ; SGD index (bits 31-24) 
	and	edx, 1000000h ; SGD index (0 = 1st half)
	jz	short vt8233_gdmabo_1
	; the second half of DMA buffer
	mov	ebx, ecx 
vt8233_gdmabo_1:	
	and	eax, 0FFFFFFh ; bits 23-0
sb16_gdmabo_2:
ac97_gdmabo_2:
	sub	ecx, eax
	jna	short vt8233_gdmabo_2
	add	ebx, ecx ; dma buffer offset
vt8233_gdmabo_2:
	mov	[u.r0], ebx
	retn

ac97_get_dma_buff_off:
	; 24/06/2017
	; 22/06/2017
	; get current (PCM OUT DMA buffer) pointer
	; (for AC'97, ICH)
	; ebx = Physical address (on page boundary)
	; ecx = Byte count
	; [audio_buff_size]
	
	;mov	ecx, [audio_dmabuff_size]
	;xor	ebx, ebx
	shr	ecx, 1
ac97_gdmabo_0:
	mov	dx, PO_CIV_REG ; Position In Current Buff Reg
	add	dx, [NABMBAR]
	in	al, dx ; current index value
	test	al, 1
	jz	short ac97_gdmabo_1
	mov	ebx, ecx
ac97_gdmabo_1:
	xor	eax, eax
	mov	dx, PO_PICB_REG ; Position In Current Buff Reg
	add	dx, [NABMBAR]
	in	ax, dx ; remain dwords
	jmp	short ac97_gdmabo_2
