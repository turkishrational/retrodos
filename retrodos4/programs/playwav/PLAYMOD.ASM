; ****************************************************************************
; playmod.asm (for MSDOS)
; ----------------------------------------------------------------------------
; PLAYMOD.COM ! VIA VT8237 MOD PLAYER & VGA DEMO program by Erdogan TAN
;
; 13/02/2017
;
; [ Last Modification: 15/02/2017 ]
;
; Derived from source code of 'PLAY.EXE' (TINYPLAY) by Carlos Hasan (1993)
;          PLAY.EXE: PLAY.ASM, MODLOAD.ASM, MODPLAY.ASM, SB.ASM
;
; Modified from 'TINYPLAY.COM' ('tinyplay.asm') source code by Erdogan Tan
;					                      (13/02/2017)
;
; Derived from source code of 'PLAYER.COM' ('PLAYER.ASM') by Erdogan Tan
;							      (08/12/2016) 
; Assembler: NASM 2.11
; ----------------------------------------------------------------------------
;	   nasm  playmod.asm -l playmod.lst -o PLAYMOD.COM	
; ****************************************************************************

; Tiny MOD Player v0.1b by Carlos Hasan.
;		July 14th, 1993.

;=============================================================================
;               PLAYER.ASM
;=============================================================================
; Audio controller, codec & PCI functions are derived from '.wav file player 
; for DOS' source code by Jeff Leyda (PLAYER.EXE), Sep 02, 2002 .

[BITS 16]
[org 100h]

Start:
		call    DetectVT8233		; Detect the VT8233 Audio Device
GetFileName:    				; Parse  the Command line...
		mov	si, 80h
		mov	bl, [si]
		xor	bh, bh
		inc	bx
		mov	byte [si+bx], 0		; make AsciiZ filename.
		inc	si
ScanName:       
		lodsb
		test	al, al
		je	pmsg_2017
		cmp	al, 20h
		je	short ScanName		; scan start of name.
		mov	di, si
		dec	di
ScanPeriod:
		lodsb
		cmp	al, '.'			; if period NOT found,
		je	short PrintMesg		; then add a .MOD extension.
		test	al, al
		jnz	short ScanPeriod
		dec	si
SetExt:
		;mov	byte [si+0], '.'
		;mov	byte [si+1], 'M'
		;mov	byte [si+2], 'O'
		;mov	byte [si+3], 'D'
		mov	dword [si], '.MOD'
		mov	byte [si+4], 0

PrintMesg:      mov     ax, 0900h		; Prints the Credits Text.
		;lea    dx, [Credits]
		mov	dx, Credits
		int     21h
LoadMod:  
		; es:di = Filename address
		push	es
		push	di
		call    LoadModule		; Load the MODule...

		cmp     word [ErrorInfo], 0	; any error loading?
		je      short init_codec

		mov     ax, 0900h		; yes, print error and Exit.
		;lea    dx, [ErrorMesg]
		mov	dx, ErrorMesg
		int     21h
		jmp     Exit

init_codec:
	; init AC97 codec
_a1:
	mov	al, VIA_ACLINK_CTRL  ; AC link interface control (41h)
	call	pciRegRead8

	mov	al, VIA_ACLINK_STAT  ; AC Link interface status (40h)
	call	pciRegRead8
	
	;movzx	eax, dl
	;test	al, VIA_ACLINK_C00_READY  ; 1 ; primary codec ready ?
	;jnz	short _a2

	call	reset_codec
	jnc	short _a2 ; EAX = 1

	;test	al, VIA_ACLINK_C00_READY 	
        ;jnz     short _a2

_codec_err:
	push	cs
	pop	ds
        mov	dx, CodecErrMsg
        mov     ah, 9
        int     21h
        jmp     Exit

CodecErrMsg db "Codec Error #"
ErrNo	db	"0 !", CR,LF,"$"

_a2:
	; eax = 1
	call	codec_io_w16 ; w32
	
	;call	detect_codec

	call	channel_reset

	call	write_ac97_dev_info 

; setup the Codec (actually mixer registers) 
        call    codecConfig                     ; unmute codec, set rates.
	jnc	short PlayNow

	add	al, '0'
	mov	[ErrNo], al
	jmp	short _codec_err

PlayNow:  
	mov	ax, BdlBuffer
	mov	[BDL_BUFFER], ax
    
	mov	ax, DmaBuffer		; DmaBuffer (4096 bytes) buff addr
	mov	[DMA_BUFFER1], ax	; 2048 byte half buffer 1 

	add	ax, BUFFERSIZE		; code/current segment
	mov	[DMA_BUFFER2], ax	; 2048 byte half buffer 2
  
	mov     word [MixSpeed], 22050	; Mixing at 22.050 kHz

	call    StartPlaying

	mov     ax, 0013h		; Set Mode 320x200x256
	int     10h

	mov     cx, 128			; Make a lookup table
	xor     bx, bx			; for fastest pixel
	mov     dx, 320*(100-64)	; addressing.
MakeOfs:        
	mov     [RowOfs+bx], dx
	mov     [RowOfs+bx+2], dx
	add     dx, 320
	add     bx, 4
	loop    MakeOfs

; Note: Normally IRQ 0 calls the ModPlay Polling at 18.2Hz thru
;       the software interrupt 1Ch. If the IRQ 0 is disabled, then
;       the INT 1Ch MUST BE CALLED at least MixSpeed/1024 times per
;       second, or the module will sound "looped".
;       Because we need better sync with the ModPlayer to draw the scope,
;       the polling is called from my routine, and then the irq 0 must be
;       disabled. The [DmaBuffer] points to the current buffer of 8-bit
;       samples played by the Sound Blaster. Note that some samples are
;       discarded in the next code, just for fun!

	;in     al, 21h			; disable irq 0!
	;or     al, 00000001b
	;out    21h, al
		
	call	ModPlay ; 13/02/2017

	;in	al, 21h			; enable irq 0!
	;and	al, 11111110b
	;out	21h, al

	mov     ax, 0003h		; Set Text Mode 80x25x16
	int     10h

	call	StopPlaying		; STOP!
Exit:           
	;call    FreeModule             ; Free MODule core.
error_exit:
	mov     ax, 4C00h		; Bye!
	int     21h
here:
	jmp	short here

pmsg_2017:
	mov     ax, 0900h		; Prints the Credits Text.
	;lea    dx, [msg_2017]
	mov	dx, msg_2017
	int     21h
	jmp	short Exit

DetectVT8233:
	; 13/02/2017
        mov     eax, (VT8233_DID << 16) + VIA_VID
        call    pciFindDevice
        jnc     short _1

; couldn't find the audio device!

	push	cs
	pop	ds
        mov     dx, noDevMsg
        mov     ah, 9
        int     21h
        jmp     short error_exit

noDevMsg db "Error: Unable to find VIA VT8233 based audio device!",CR,LF,"$"

_1:
	; 12/11/2016
	; Erdogan Tan - 8/11/2016
	; References: Kolibrios - vt823x.asm (2016)
	;	      VIA VT8235 V-Link South Bridge (VT8235-VIA.PDF)(2002)
	;	      lowlevel.eu - AC97 (2016)
	;	      .wav player for DOS by Jeff Leyda (2002) -this file-
	;	      Linux kernel - via82xx.c (2016)

	; eax = BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; edx = DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	mov	[bus_dev_fn], eax
	mov	[dev_vendor], edx

	; init controller
	mov	al, PCI_CMD_REG ; command register (04h)
	call	pciRegRead32

	; eax = BUS/DEV/FN/REG
	; edx = STATUS/COMMAND
	; 	SSSSSSSSSSSSSSSSCCCCCCCCCCCCCCCC
	mov	[stats_cmd], edx

	mov	al, PCI_IO_BASE ; IO base address register (10h)
	call	pciRegRead32

	and     dx, 0FFC0h	; IO_ADDR_MASK (0FFFE) ?
        mov     [ac97_io_base], dx

	mov	al, AC97_INT_LINE ; Interrupt line register (3Ch)
	call	pciRegRead32

	and 	edx, 0FFh
  	mov     [ac97_int_ln_reg], dl

	; 28/11/2016
	mov	bx, 1
	mov	cx, dx
	shl	bx, cl

	;not	bx
	in	al, 0A1h ; irq 8-15
        mov	ah, al
        in	al, 21h  ; irq 0-7 
	;and	ax, bx   ; unmask
 	btr	ax, dx	 ; unmask
	out	21h, al  ; enable interrupt (if irq <= 7)
	mov	al, ah
	out	0A1h, al ; enable interrupt (if irq > 7)
	;not	bx

	mov	dx, 4D1h			;8259 ELCR1
        in	al, dx
	mov	ah, al
	mov	dx, 4D0h 
        in	al, dx
	;or	ax, bx        
	bts	ax, cx
	mov	dx, 4D0h
	out	dx, al                          ;set level-triggered mode
	mov	al, ah
	mov	dx, 4D1h
	out	dx, al                          ;set level-triggered mode

	; 24/11/2016 - Erdogan Tan
	mov	bx, cx
	;mov	bx, dx
	mov	bl, [bx+irq_int]
	shl	bx, 2 ; * 4

	; set up interrupt vector
	; 30/11/2016
	push	es
	xor	ax, ax
	mov	es, ax
	mov	word [es:bx], ac97_int_handler
	mov	ax, cs
	mov	[es:bx+2], ax
	pop	es
		
	retn

ac97_int_handler:
	push	ax
	push	dx
	push	cx
	push	bx
	push	si
	push	di

	; 15/02/2017
	push	ds
	mov	dx, cs
	mov	ds, dx	

	; 28/11/2016
	cmp	byte [uLVI], 1
	jnb	short _busy

	mov	byte [uLVI], 1

	mov	byte [irq_status], 0

        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_r8

	test    al, VIA_REG_STAT_ACTIVE
        jz      short _ih0

        and     al, VIA_REG_STAT_EOL + VIA_REG_STAT_FLAG + VIA_REG_STAT_STOPPED
	mov	[irq_status], al
        jz	short _ih0

	; 28/11/2016 - Erdogan Tan
	call	tuneLoop
_ih0:
	mov	byte [uLVI], 0
_p_i_retn:
	mov	al, 20h
	test	byte [ac97_int_ln_reg], 8
	jz	short _ih_1
	out 	0A0h, al ; 20h ; EOI
_ih_1:
	out	20h, al  ; 20h ; EOI
_ih_2:
	pop	ds ; 15/02/2017

	pop	di
	pop	si
	pop	bx
	pop	cx
	pop	dx
	pop	ax
	iret

_busy:
	; 28/11/2016 - Erdogan Tan
        mov     al, [irq_status]   ;; ack ;;
        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_w8

	jmp	short _ih0


;=============================================================================
;               PCI.ASM
;=============================================================================

; EQUATES

;constants of stuff that seem hard to remember at times.

TRUE  EQU 1
FALSE EQU 0

ENABLED  EQU 1
DISABLED EQU 0

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

;special characters
NUL     EQU 0
NULL    EQU 0
BELL    EQU 07
BS      EQU 08
TAB     EQU 09
LF      EQU 10
CR      EQU 13
ESCAPE  EQU 27           ;ESC is a reserved word....


;file stuff
READONLY  EQU   BIT0
HIDDEN    EQU   BIT1
SYSTEM    EQU   BIT2
VOLUME    EQU   BIT3         ;ignored for file access
DIRECTORY EQU   BIT4         ;must be 0 for file access
ARCHIVE   EQU   BIT5
SHAREABLE EQU   BIT7         ;for novell networks
OPEN	EQU	2		; open existing file
CREATE	EQU	1		; create new file


; PCI equates
; PCI function address (PFA)
; bit 31 = 1
; bit 23:16 = bus number     (0-255)
; bit 15:11 = device number  (0-31)
; bit 10:8 = function number (0-7)
; bit 7:0 = register number  (0-255)

IO_ADDR_MASK    EQU     0FFFEh          ; mask off bit 0 for reading BARs
PCI_INDEX_PORT  EQU     0CF8h
PCI_DATA_PORT   EQU     0CFCh
PCI32           EQU     BIT31           ; bitflag to signal 32bit access
PCI16           EQU     BIT30           ; bitflag for 16bit access

PCI_FN0         EQU     0 << 8
PCI_FN1         EQU     1 << 8
PCI_FN2         EQU     2 << 8
PCI_FN3         EQU     3 << 8
PCI_FN4         EQU     4 << 8
PCI_FN5         EQU     5 << 8
PCI_FN6         EQU     6 << 8
PCI_FN7         EQU     7 << 8

PCI_CMD_REG		EQU	04h		; reg 04, command reg
 IO_ENA			EQU	BIT0		; i/o decode enable
 MEM_ENA		EQU	BIT1		; memory decode enable
 BM_ENA                 EQU     BIT2		; bus master enable

; CODE

; PCI device register reader/writers.
; NASM version: Erdogan Tan (29/11/2016)

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
;	 number.  Likewise, don't do 16bit reads from non word aligned reg #
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
;	 number.  Likewise, don't do 16bit writes from non word aligned reg #
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

;===============================================================
; PCIFindDevice: scan through PCI space looking for a device+vendor ID
;
; Entry: EAX=Device+Vendor ID
;
;  Exit: EAX=PCI address if device found
;	 EDX=Device+Vendor ID
;        CY clear if found, set if not found. EAX invalid if CY set.
;
; [old stackless] Destroys: ebx, esi, edi, cl
;
pciFindDevice:
	;push	cx
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
	mov	eax, BIT31
	not	eax
	and	eax, edi		; return only bus/dev/fn #
	popf

	;pop	edi
	;pop	esi
	pop	edx
	;pop	cx
	retn

;=============================================================================
;               CODEC.ASM
;=============================================================================

; EQUATES

;Codec registers.
;
;Not all codecs are created equal. Refer to the spec for your specific codec.
;
;All registers are 16bits wide.  Access to codec registers over the AC97 link
;is defined by the OEM.  
;
;Secondary codec's are accessed by ORing in BIT7 of all register accesses.
;

; each codec/mixer register is 16bits

CODEC_RESET_REG                 equ     00      ; reset codec
CODEC_MASTER_VOL_REG            equ     02      ; master volume
CODEC_HP_VOL_REG                equ     04      ; headphone volume
CODEC_MASTER_MONO_VOL_REG       equ     06      ; master mono volume
CODEC_MASTER_TONE_REG           equ     08      ; master tone (R+L)
CODEC_PCBEEP_VOL_REG            equ     0ah     ; PC beep volume
CODEC_PHONE_VOL_REG             equ     0bh     ; phone volume
CODEC_MIC_VOL_REG               equ     0eh     ; MIC volume
CODEC_LINE_IN_VOL_REG           equ     10h     ; line input volume
CODEC_CD_VOL_REG                equ     12h     ; CD volume
CODEC_VID_VOL_REG               equ     14h     ; video volume
CODEC_AUX_VOL_REG               equ     16h     ; aux volume
CODEC_PCM_OUT_REG               equ     18h     ; PCM output volume
CODEC_RECORD_SELECT_REG         equ     1ah     ; record select input
CODEC_RECORD_VOL_REG            equ     1ch     ; record volume
CODEC_RECORD_MIC_VOL_REG        equ     1eh     ; record mic volume
CODEC_GP_REG                    equ     20h     ; general purpose
CODEC_3D_CONTROL_REG            equ     22h     ; 3D control
; 24h is reserved
CODEC_POWER_CTRL_REG            equ     26h     ; powerdown control
CODEC_EXT_AUDIO_REG             equ     28h     ; extended audio
CODEC_EXT_AUDIO_CTRL_REG        equ     2ah     ; extended audio control
CODEC_PCM_FRONT_DACRATE_REG     equ     2ch     ; PCM out sample rate
CODEC_PCM_SURND_DACRATE_REG     equ     2eh     ; surround sound sample rate
CODEC_PCM_LFE_DACRATE_REG       equ     30h     ; LFE sample rate
CODEC_LR_ADCRATE_REG            equ     32h     ; PCM in sample rate
CODEC_MIC_ADCRATE_REG           equ     34h     ; mic in sample rate

; registers 36-7a are reserved on the ICH

CODEC_VENDORID1_REG             equ     7ch     ; codec vendor ID 1
CODEC_VENDORID2_REG             equ     7eh     ; codec vendor ID 2

; Mixer registers 0 through 51h reside in the ICH and are not forwarded over
; the AC97 link to the codec, which I think is a little weird.  Looks like
; the ICH makes it so you don't need a fully functional codec to play audio?
;
; whenever 2 codecs are present in the system, use BIT7 to access the 2nd
; set of registers, ie 80h-feh

PRIMARY_CODEC                   equ     0       ; 0-7F for primary codec
SECONDARY_CODEC                 equ     BIT7    ; 80-8f registers for 2ndary

SAMPLE_RATE_441khz	equ     44100   ; 44.1Khz (cd quality) rate

; each buffer descriptor BAR holds a pointer which has entries to the buffer
; contents of the .WAV file we're going to play.  Each entry is 8 bytes long
; (more on that later) and can contain 32 entries total, so each BAR is
; 256 bytes in length, thus:

BDL_SIZE                equ     32*8    ; Buffer Descriptor List size
INDEX_MASK              equ     31      ; indexes must be 0-31

;
; Buffer Descriptors List
; As stated earlier, each buffer descriptor list is a set of (up to) 32 
; descriptors, each 8 bytes in length.  Bytes 0-3 of a descriptor entry point
; to a chunk of memory to either play from or record to.  Bytes 4-7 of an
; entry describe various control things detailed below.
; 
; Buffer pointers must always be aligned on a Dword boundry.
;
;

IOC                     equ     BIT31   ; Fire an interrupt whenever this
                                        ; buffer is complete.

BUP                     equ     BIT30   ; Buffer Underrun Policy.
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
; A value of FFFF is 65536 samples.  Running at 44.1Khz, that's just about
; 1.5 seconds of sample time.  FFFF * 32bits is 1FFFFh bytes or 128k of data.
;
; A value of 0 in these bits means play no samples.
;

;VIA VT8233 (VT8235) AC97 Codec equates 
;(edited by Erdogan Tan, 7/11/2016)

; PCI stuff

VIA_VID		equ     1106h           ; VIA's PCI vendor ID
VT8233_DID      equ     3059h           ; VT8233 (VT8235) device ID
			
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
; 24/11/2016
VIA_REG_STAT_STOPPED	equ   04h    ;; RWC
VIA_REG_STAT_EOL	equ   02h    ;; RWC
VIA_REG_STAT_FLAG	equ   01h    ;; RWC
VIA_REG_STAT_ACTIVE	equ   80h    ;; RO
; 28/11/2016
VIA_REG_STAT_LAST	equ   40h    ;; RO
VIA_REG_STAT_TRIGGER_QUEUED equ 08h  ;; RO
VIA_REF_CTRL_INT_STOP	equ   04h  ; Interrupt on Current Index = Stop Index
				   ; and End of Block	

; CODE

; codec configuration code.  Not much here really.
; NASM version: Erdogan Tan (29/11/2016)

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
	; wait 100 ms
	mov	ecx, 25
_delay_x_ms:
	push	ecx
	call	delay1_4ms
	pop	ecx
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

;=============================================================================
;               VIA_WAV.ASM
;=============================================================================

; DOS based .WAV player using AC'97 and codec interface.
; ---------------------------------------------------------------
; VIA VT8233 Modification & NASM version: Erdogan Tan (29/11/2016)
; Last Update: 08/12/2016 (by Erdogan Tan)

; player internal variables and other equates.
BUFFERSIZE      equ     2048		; 2K half buffer size. ; 13/02/2017
ENDOFFILE       equ     BIT0            ; flag for knowing end of file

;===========================================================================
; entry: none.  File is already open and [filehandle] filled.
; exit:  not until the song is finished or the user aborts.
;
ModPlay: ; 13/02/2017  ; ModPlay Polling!
	;cld
	; clear (half) buffer 2
       	mov     di, [DMA_BUFFER2]
	sub	ax, ax
	mov	cx, (BUFFERSIZE/2)
	rep	stosw
	
       ; load 2048 bytes into buffer 1

        mov     si, [DMA_BUFFER1]
	mov	cx, BUFFERSIZE

	push	ds ; segment
	push	si ; offset
	push	cx ; count
	call    GetSamples

       ; load 2048 bytes into buffer 1

	;mov	si, [DMA_BUFFER2]
	;mov	cx, BUFFERSIZE
	;push	ds ; segment
	;push	si ; offset
	;push	cx ; count
	;call	GetSamples

; write last valid index to 31 to start with.
; The Last Valid Index register tells the DMA engine when to stop playing.
; 
; As we progress through the song we change the last valid index to always be
; something other than the index we're currently playing.  
;
        ;;mov   al, 1
        ;mov	al, 31
	;call   setLastValidIndex

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
; if it ever runs out of data to play.  Good for safety.
;
	; 14/02/2017
        mov     di, [BDL_BUFFER]		; get BDL address
        mov     cx, 32 / 2                      ; make 32 entries in BDL
	xor	edx, edx
	mov	dx, ds
	shl	edx, 4 ; segment*16 (linear/physical address of the segment)
_0:

; set buffer descriptor 0 to start of data file in memory
	; 14/02/2017
        movzx   eax, word [DMA_BUFFER1]
	add	eax, edx ; linear/physical address of the buffer (seg*16+off)
        stosd					; store dmabuffer1 address

;
; set length to 32k samples.  1 sample is 16bits or 2bytes.
; Set control (bits 31:16) to BUP, bits 15:0=number of samples.
; 

; VIA VT8235.PDF: (Page 110) (Erdogan Tan, 29/11/2016)
	;
	; 	Audio SGD Table Format
	;	-------------------------------
	;	63   62    61-56    55-32  31-0
	;	--   --   --------  -----  ----
	;	EOL FLAG -reserved- Base   Base
	;			    Count  Address
	;			    [23:0] [31:0]
	;	EOL: End Of Link. 
	;	     1 indicates this block is the last of the link.
	;	     If the channel “Interrupt on EOL” bit is set, then
	;	     an interrupt is generated at the end of the transfer.
	;
	;	FLAG: Block Flag. If set, transfer pauses at the end of this
	;	      block. If the channel “Interrupt on FLAG” bit is set,
	;	      then an interrupt is generated at the end of this block.

	FLAG	EQU BIT30
	EOL	EQU BIT31

	; 08/12/2016 - Erdogan Tan
	mov	eax, BUFFERSIZE
	or	eax, FLAG
	;or	eax, EOL
	stosd

; 2nd buffer:
	; 14/02/2017
        movzx   eax, word [DMA_BUFFER2]
	add	eax, edx ; linear/physical address of the buffer (seg*16+off)
        stosd					; store dmabuffer2 address

; set length to 64k (32k of two 16 bit samples)
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	; 08/12/2016 - Erdogan Tan
	mov	eax, BUFFERSIZE
	or	eax, EOL
	;or	eax, FLAG
	stosd

        loop    _0

;
; tell the DMA engine where to find our list of Buffer Descriptors.
; this 32bit value is a flat mode memory offset (ie no segment:offset)
;
; write buffer descriptor list address
;
        movzx   eax, word [BDL_BUFFER]
	; 14/02/2017
	add	eax, edx ; linear/physical address of the BDL
  
	; 12/11/2016 - Erdogan Tan 
	; (Ref: KolibriOS, vt823x.asm, 'create_primary_buff')
	mov     edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_TABLE_PTR
        call    ctrl_io_w32

	call	codec_check_ready

  	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFS_PLAYBACK_VOLUME_L
        mov     eax, 2   ;31
        call    ctrl_io_w8

	call	codec_check_ready

        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFS_PLAYBACK_VOLUME_R
        mov     ax, 2   ;31
        call    ctrl_io_w8

	call	codec_check_ready
;
;
; All set.  Let's play some music.
;
;
       	;mov    dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
        ;mov    ax, VIA8233_REG_TYPE_16BIT or VIA8233_REG_TYPE_STEREO or 0xfffff or 0xff000000
        ;call   ctrl_io_w32

	;call	codec_check_ready

	; 08/12/2016
	; 07/10/2016
        ;mov    al, 1
        mov	al, 31
	call    setLastValidIndex

	mov	byte [tLoop], 1 ; 30/11/2016

        mov	ax, VIA_REG_CTRL_INT
       	or	eax, VIA_REG_CTRL_START
        ;mov	ax, VIA_REG_CTRL_AUTOSTART + VIA_REG_CTRL_START
	; 28/11/2016
	;mov	ax, VIA_REG_CTRL_AUTOSTART + VIA_REG_CTRL_START + VIA_REG_CTRL_INT_FLAG
	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8

	call	codec_check_ready

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k.  Likewise when it's playing buffer 2, refresh buffer 1 and repeat.
   
	; 14/02/2017
	; 13/02/2017
	; 08/12/2016
	; 28/11/2016

	push	es
	mov	ax, 0A000h
	mov	es, ax
	mov	bp, DmaBuffer ; 14/02/2017
p_loop:
	mov     ah, 1			; any key pressed?
	int     16h			; no, Loop.
	jz	short q_loop

	mov     ah, 0			; flush key buffer...
	int     16h
p_return:
	mov	byte [tLoop], 0	; 13/02/2017
	pop	es
	retn
q_loop:
	xor	ax, ax
	xchg	al, [tBuff] ; AL = [tBuff], [tBuff] = 0
	cmp	al, 1
	ja	short r_loop
	jb	short ScopeLoop
	;
     	mov	si, [DMA_BUFFER1] ; [tBuff]=1 (from tuneLoop)
       	jmp	short s_loop 
r_loop:
	; 13/02/2017
        mov     si, [DMA_BUFFER2] ; [tBuff]=2 (from tuneLoop)
s_loop:
	; 14/02/2017
	mov	bp, si ; save current buffer addres in bp register
	mov	cx, BUFFERSIZE ; 2048 byte
	push	ds ; segment
	push	si ; offset
	push	cx ; count
	call    GetSamples
	;
ScopeLoop:
	mov     si, bp			; get current samples
	xor     cx, cx			; to be drawed ...
	xor     dx, dx
DrawLoop:       
	mov     bx, dx			; (save Index)
	mov     di, [Scope+bx]		; get old SCOPE pixel address
	mov     byte [es:di], 0		; erase it!
	lodsb				; get a sample (8-bit)
	mov     bl, al			; calc new pixel address...
	xor     bh, bh
	shl     bx, 1
	mov     di, [RowOfs+bx]
	add     di, cx
	mov     bx, dx			; (restore Index)
	mov     [Scope+bx], di		; save new address...
	mov     byte [es:di], 10	; and DRAW.
	add     dx, 2			; the next pixel...
	inc     cx
	cmp     cx, 320			; 320 pixels drawed?
	jb      short DrawLoop
	jmp	short p_loop

tuneLoop:
	; 08/12/2016
	; 28/11/2016 - Erdogan Tan
	
	cmp	byte [tLoop], 1
	jb	short _exit

	mov	byte [tBuff], 1 ; Buffer 1

	test	byte [irq_status], VIA_REG_STAT_EOL 
	jz	short _tlp1 ; FLAG
	
	; EOL
	inc	byte [tBuff] ; Buffer 2
	inc	al
_tlp1: 
	test	byte [irq_status], VIA_REG_STAT_FLAG 
	jz	short _tlp2

	mov	ax, VIA_REG_CTRL_INT
       	or	ax, VIA_REG_CTRL_START
       	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8

	call	codec_check_ready
_tlp2:	
        mov     al, [irq_status]   ;; ack ;;
        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_w8

	retn

_exit_:
	mov	byte [tLoop], 0
_exit:
        ; finished with song, stop everything
	mov     ax, VIA_REG_CTRL_INT
        or      ax, VIA_REG_CTRL_TERMINATE
	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8

        call	channel_reset
_return:
	retn

;input AL = index # to stop on
setLastValidIndex:
	; 19/11/2016
	; 14/11/2016 - Erdogan Tan (Ref: VIA VT8235.PDF, Page 110)
	; 12/11/2016 - Erdogan Tan
	; (Ref: KolibriOS, vt823x.asm, 'create_primary_buff')
	;push	edx
	push	ax
	;push	ecx
	movzx	eax, word [sample_rate] ; Hertz
	mov	edx, 100000h ; 2^20 = 1048576
	mul	edx
	mov	ecx, 48000	
	div	ecx
	;and	eax, 0FFFFFh
	;pop	ecx
	pop	dx 
	shl	edx, 24  ; STOP Index Setting: Bit 24 to 31
	or	eax, edx
	; 19/11/2016
	cmp	byte [bps], 16
	jne	short sLVI_1
	or	eax, VIA8233_REG_TYPE_16BIT
sLVI_1:
	cmp	byte [stmo], 2
	jne	short sLVI_2
	or	eax, VIA8233_REG_TYPE_STEREO
sLVI_2:
	mov     edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
        call    ctrl_io_w32
	call	codec_check_ready
	;pop	edx
	retn

;=============================================================================
;               MODLOAD.ASM
;=============================================================================

; Amiga Module Loader v0.1b by Carlos Hasan.
;		July 10th, 1993.

; STRUCTURES

struc ModSample
.msName:	resb 22
.msLength:	resw 1
.msFinetune:	resb 1
.msVolume:	resb 1
.msRepeat:	resw 1
.msRepLen:	resw 1
.size:
endstruc

struc ModHeader
.mhName:	resb 20
.mhSamples:	resb ModSample.size*31
.mhOrderLen:	resb 1
.mhReStart:	resb 1
.mhOrder:	resb 128
.mhSign:	resw 2
.size:	
endstruc

struc ModInfoRec
.OrderLen:	resb 1
.ReStart:	resb 1
.Order:		resb 128
.Patterns:	resd 1
.SampOfs:	resw 31
.SampSeg:	resw 31
.SampLen:	resw 31
.SampRep:	resw 31
.SampRepLen:	resw 31
.SampVol:	resw 31
.size:	
endstruc

; CODE

LoadModule:
		;es:di = filename

		;[sp+4] = es
		;[sp+2] = di

		FileName equ 4		

		push    bp
		mov     bp, sp
		pusha
		push    ds
		push    es

		mov	word [ErrorInfo], 1

		call    ClearModInfo
OpenFile:       
		push    ds
		mov     ax, 3D00h
		lds     dx, [bp+FileName]
		int     21h
		pop     ds
		jc      Failed
		mov     [FileHandle], ax

ReadHeader:     
		mov     ax, 3F00h
		mov     bx, [FileHandle]
		mov     cx, ModHeader.size
		;lea    dx, [Header]
		mov	dx, Header
		int     21h
		jc      CloseFile
CheckMK:        
		cmp     word [Header+ModHeader.mhSign], 'M.'
		jne     short CheckFLT4
		cmp     word [Header+ModHeader.mhSign+2], 'K.'
		je      short IsModFile
CheckFLT4:
		cmp     word [Header+ModHeader.mhSign], 'FL'
		jne     short Is15Inst
		cmp     word [Header+ModHeader.mhSign+2], 'T4'
		je      short IsModFile
Is15Inst:
		mov     si, (Header+ModHeader.mhSamples) + (15*ModSample.size)
		mov     di, Header+ModHeader.mhOrderLen
		mov     ax, ds
		mov     es, ax
		cld
		mov     cx, 130
		rep     movsb
		mov     di, Header+ModHeader.mhSamples + (15*ModSample.size)
		xor     ax, ax
		mov     cx, 16*ModSample.size
		rep     stosb
SeekPatterns:   
		mov     ax, 4200h
		mov     bx, [FileHandle]
		mov     cx, 0
		mov     dx, 600
		int     21h
IsModFile:  
		mov     al, [Header+ModHeader.mhOrderLen]
		mov     [ModInfo.OrderLen], al

		mov     al, [Header+ModHeader.mhReStart]
		cmp     al, [Header+ModHeader.mhOrderLen]
		jb      short SetReStart
		mov     al, 7Fh
SetReStart:
		mov     [ModInfo.ReStart], al

		mov     cx, 128
		xor     ax, ax
		xor     bx, bx
CopyOrder:
		mov     ah, [Header+ModHeader.mhOrder+bx]
		mov     [ModInfo.Order+bx], ah
		cmp     ah, al
		jb      short NextOrder
		mov     al, ah
NextOrder:
		inc     bx
		loop    CopyOrder
AllocPatterns:  
		; Erdogan Tan (13/02/2017)
		xor	ah, ah
		inc	al
		; al = count of 1024 bytes
		mov	bx, ax
		; count of paragraphs = al*64 
		shl	ax, 6 ; *64
		mov	bp, ax
		mov	dx, cs ; current (code) segment
		add	dx, 1000h ; next 64K (4096*16)
		;
		mov	word [ModInfo.Patterns], 0
		mov	[ModInfo.Patterns+2], dx
		;
		add	bp, dx ; next segment for samples
ReadPatterns:   
		push    ds
		mov     ax, 3F00h
		mov     cx, bx ; count of 1024 bytes
		shl     cx, 10 ; byte count (cx*1024)
		mov     bx, [FileHandle]
		;lds    dx, [ModInfo.Patterns]
		mov	ds, dx
		xor	dx, dx
		int     21h
		pop     ds
		jc      CloseFile

		;lea	si, [Header+ModHeader.mhSamples]
		mov	si, Header+ModHeader.mhSamples
		xor     di, di
CopySamples:
		mov     ax, [si+ModSample.msLength]
		xchg    al, ah
		shl     ax, 1
		mov     [ModInfo.SampLen+di], ax
		mov     al, [si+ModSample.msVolume]
		xor     ah, ah
		mov     [ModInfo.SampVol+di], ax
		mov     ax, [si+ModSample.msRepeat]
		xchg    al, ah
		shl     ax, 1
		mov     [ModInfo.SampRep+di], ax
		mov     ax, [si+ModSample.msRepLen]
		xchg    al, ah
		shl     ax, 1
		mov     [ModInfo.SampRepLen+di], ax
		add     si, ModSample.size
		add     di, 2
		cmp     di, 2*31
		jb      short CopySamples

		xor     si, si
AllocSamples:
		; Erdogan Tan (13/02/2017)
		;mov	bx, [ModInfo.SampLen+si]
		mov	cx, [ModInfo.SampLen+si]
		mov	bx, cx
		shr     bx, 4 ; byte count / 16
		jz      short NextSample
		inc	bx ; number of paragraphs
		mov	word [ModInfo.SampOfs+si], 0
		mov     [ModInfo.SampSeg+si], bp
		mov	dx, bp
		add	bp, bx ; next segment for sample 
ReadSample:
		push    ds
		mov     ax, 3F00h
		mov     bx, [FileHandle]
		;mov    cx, [ModInfo.SampLen+si]
		;mov    dx, [ModInfo.SampOfs+si]
		;mov    ds, [ModInfo.SampSeg+si]
		mov	ds, dx
		xor	dx, dx	
		int     21h
		pop     ds
		jc      short CloseFile
NextSample:
		add     si, 2
		cmp     si, 2*31
		jb      short AllocSamples

		mov     word [ErrorInfo], 0
CloseFile:      
		mov     ax, 3E00h
		mov     bx, [FileHandle]
		int     21h
Failed:         
		pop     es
		pop     ds
		popa
		pop     bp
		ret	4

FreeModule:
		; Erdogan Tan (13/02/2017)
		; nothing to do here for memory de-allocation
ClearModInfo:
		pusha
		push    es
		mov     ax, ds
		mov     es, ax
		;lea    di, [ModInfo]
		mov	di, ModInfo
		mov     cx, ModInfoRec.size
		cld
		xor     ax, ax
		rep     stosb
		pop     es
		popa
		retn

;=============================================================================
;               MODPLAY.ASM
;=============================================================================

; Amiga Module Loader v0.3b by Carlos Hasan.
;		July 23th, 1993.

; EQUATES

NumTracks       equ 4
DefTempo        equ 6
DefBpm          equ 125
MidCRate        equ 8448
MixBufSize      equ 4096

; STRUCTURES

struc TrackInfo
.Samples:	resd 1
.Position:	resw 1
.Len:		resw 1
.Repeat:	resw 1
.RepLen:	resw 1
.Volume: 	resb 1
.Error:		resb 1
.Period:	resw 1
.Pitch:		resw 1
.Effect:	resw 1
.PortTo:	resw 1
.PortParm:	resb 1
.VibPos:	resb 1
.VibParm:	resb 1
.OldSampOfs:	resb 1
.Arp:		resw 3
.ArpIndex:	resw 1
.size:
endstruc

; CODE

;--------------------------------------------------------------------------
; BeatTrack:  Process the next beat in one track.
;  In:
;    ds:di -  Track info Address.
;--------------------------------------------------------------------------

BeatTrack:
		mov     dx, [di+TrackInfo.Effect]
		test    dx, dx
		je      short None
		cmp     dh, 00h
		je      short Arpeggio
		cmp     dh, 01h
		je      short PortUp
		cmp     dh, 02h
		je      short PortDown
		cmp     dh, 03h
		je      short TonePort
		cmp     dh, 04h
		je      Vibrato
		cmp     dh, 05h
		je      PortSlide
		cmp     dh, 06h
		je      VibSlide
		cmp     dh, 0Ah
		je      VolSlide
None:           
		retn
Arpeggio:
		mov     bx, [di+TrackInfo.ArpIndex]
		mov     ax, [di+TrackInfo.Arp+bx]
		mov     [di+TrackInfo.Pitch], ax
		add     bx, 2
		cmp     bx, 6
		jb      short SetArpIndex
		xor     bx,bx
SetArpIndex:
		mov     [di+TrackInfo.ArpIndex], bx
		retn
PortUp:
		xor     dh, dh
		mov     bx, [di+TrackInfo.Period]
		sub     bx, dx
		cmp     bx, 113
		jge     short NotSmall
		mov     bx, 113
NotSmall:
		mov     [di+TrackInfo.Period], bx
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Pitch], ax
		retn
PortDown:
		xor     dh, dh
		mov     bx, [di+TrackInfo.Period]
		add     bx, dx
		cmp     bx, 856
		jle     short NotBig
		mov     bx, 856
NotBig:         mov     [di+TrackInfo.Period], bx
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Pitch], ax
		retn
TonePort:
		xor     dh, dh
		mov     ax, [di+TrackInfo.PortTo]
		mov     bx, [di+TrackInfo.Period]
		cmp     bx, ax
		je      short NoPort
		jg      short PortToUp
PortToDown:     
		add     bx, dx
		cmp     bx, ax
		jle     short SetPort
FixPort:        
		mov     bx, ax
		jmp     short SetPort
PortToUp:
		sub     bx, dx
		cmp     bx, ax
		jl      short FixPort
SetPort:        
		mov     [di+TrackInfo.Period], bx
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Pitch], ax
NoPort:         
		retn
Vibrato:
		mov     dh, dl
		and     dl, 0Fh
		shr     dh, 4
		shl     dh, 2
		add     [di+TrackInfo.VibPos], dh
		mov     dh, [di+TrackInfo.VibPos]
		mov     bl, dh
		shr     bl, 2
		and     bx, 1Fh
		mov     al, [SinTable+bx]
		mul     dl
		rol     ax, 1
		xchg    al, ah
		and     ah, 1
		test    dh, dh
		jns     short VibUp
		neg     ax
VibUp:          
		add     ax, [di+TrackInfo.Period]
		mov     bx, ax
		cmp     bx, 113
		jge     short NoLoVib
		mov     bx, 113
NoLoVib:        
		cmp     bx, 856
		jle     short NoHiVib
		mov     bx, 856
NoHiVib:        
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Pitch], ax
		retn
PortSlide:
		call    VolSlide
		mov     dl, [di+TrackInfo.PortParm]
		jmp     short TonePort
VibSlide:
		call    VolSlide
		mov     dl, [di+TrackInfo.VibParm]
		jmp     short Vibrato
VolSlide:
		mov     dh, dl
		and     dl, 0Fh
		shr     dh, 4
		mov     al, [di+TrackInfo.Volume]
		sub     al, dl
		jge     short NoLoVol
		xor     al, al
NoLoVol:        
		add     al, dh
		cmp     al, 64
		jbe     short NoHiVol
		mov     al, 64
NoHiVol:        
		mov     [di+TrackInfo.Volume], al
		retn

;--------------------------------------------------------------------------
; GetTrack:   Get the next Note from a pattern.
;  In:
;    ds:di -  Track info Address.
;    es:si -  Pattern Note Address.
; Out:
;    es:si -  The Next Pattern Note address.
;--------------------------------------------------------------------------

GetTrack:
		es lodsw
		xchg    al, ah
		mov     bl, ah
		and     ah, 0Fh
		mov     cx, ax
		es lodsw
		xchg    al, ah
		mov     bh, ah
		and     ah, 0Fh
		mov     dx, ax
		mov     [di+TrackInfo.Effect], dx
		and     bl, 0F0h
		shr     bh, 4
		or      bl, bh
		je      short SetPeriod
SetSample:
		xor     bh, bh
		dec     bx
		add     bx, bx
		mov     ax, [ModInfo.SampVol+bx]
		mov     [di+TrackInfo.Volume], al
		mov     ax, [ModInfo.SampOfs+bx]
		mov     [di+TrackInfo.Samples], ax
		mov     ax, [ModInfo.SampSeg+bx]
		mov     [di+TrackInfo.Samples+2], ax
		mov     ax, [ModInfo.SampLen+bx]
		mov     [di+TrackInfo.Len], ax
		mov     ax, [ModInfo.SampRep+bx]
		mov     [di+TrackInfo.Repeat], ax
		mov     ax, [ModInfo.SampRepLen+bx]
		mov     [di+TrackInfo.RepLen], ax
SetPeriod:      
		test    cx, cx
		je      short SetEffect

		mov     [di+TrackInfo.PortTo], cx
		cmp     dh, 03h
		je      short SetEffect

		mov     [di+TrackInfo.Period], cx
		mov     bx, cx
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Pitch], ax
		mov     word [di+TrackInfo.Position], 0
SetEffect:
		test    dx, dx
		je      short InitNone
		cmp     dh, 00h
		je      InitArpeggio
		cmp     dh, 03h
		je      short InitTonePort
		cmp     dh, 04h
		je      short InitVibrato
		cmp     dh, 09h
		je      short SampleOfs
		cmp     dh, 0Bh
		je      short PosJump
		cmp     dh, 0Ch
		je      short SetVolume
		cmp     dh, 0Dh
		je      short Break
		cmp     dh, 0Fh
		je      short SetSpeed
InitNone:
		retn
InitTonePort:
		test    dl, dl
		jne     short SetPortParm
		mov     dl, [di+TrackInfo.PortParm]
SetPortParm:    
		mov     [di+TrackInfo.PortParm], dl
		mov     [di+TrackInfo.Effect], dx
		retn
InitVibrato:
		mov     al, [di+TrackInfo.VibParm]
		mov     ah, al
		and     al, 0Fh
		and     ah, 0F0h
		test    dl, 0Fh
		jne     short OkDepth
		or      dl, al
OkDepth:        
		test    dl, 0F0h
		jne     short OkRate
		or      dl, ah
OkRate:         
		mov     [di+TrackInfo.VibParm], dl
		mov     [di+TrackInfo.Effect], dx
		test    cx, cx
		je      short OkPos
		mov     byte [di+TrackInfo.VibPos], 0
OkPos:          
		retn
SampleOfs:      
		test    dl, dl
		jne     short SetSampleOfs
		mov     dl, [di+TrackInfo.OldSampOfs]
SetSampleOfs:
		mov     [di+TrackInfo.OldSampOfs], dl
		mov     dh, dl
		xor     dl, dl
		mov     [di+TrackInfo.Position], dx
		retn
PosJump:
		mov     [OrderPos], dl
		mov     byte [Row], 64
		retn
SetVolume:
		cmp     dl, 64
		jbe     short OkVol
		mov     dl, 64
OkVol:
		mov     [di+TrackInfo.Volume], dl
		retn
Break:
		mov     dh, dl
		and     dl, 0Fh
		shr     dh, 4
		add     dh, dh
		add     dl, dh
		shl     dh, 2
		add     dl, dh
		mov     [BreakRow], dl
		mov     byte [Row], 64
		retn
SetSpeed:
		test    dl,dl
		je      Skip
		cmp     dl,31
		ja      short SetBpm
SetTempo:       
		mov     [Tempo], dl
		mov     [TempoWait], dl
		retn
SetBpm:
		mov     [Bpm], dl
		mov     al, 103
		mul     dl
		mov     bl, ah
		xor     bh, bh
		mov     ax, [MixSpeed]
		xor     dx, dx
		div     bx
		mov     [BpmSamples], ax
Skip:           
		retn
InitArpeggio:
		mov     dh, dl
		and     dl, 0Fh
		shr     dh, 4
		mov     cx, 36
		xor     bx, bx
		mov     ax, [di+TrackInfo.Period]
gt_ScanPeriod:
		cmp     ax, [PeriodTable+bx]
		jae     short SetArp
		add     bx, 2
		loop    gt_ScanPeriod
SetArp:         
		add     dx, dx
		add     dh, bl
		add     dl, bl
		mov     bx, [PeriodTable+bx]
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Arp], ax
		mov     bl, dh
		xor     bh, bh
		mov     bx, [PeriodTable+bx]
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Arp+2], ax
		mov     bl, dl
		xor     bh, bh
		mov     bx, [PeriodTable+bx]
		add     bx, bx
		mov     ax, [PitchTable+bx]
		mov     [di+TrackInfo.Arp+4], ax
		mov     word [di+TrackInfo.ArpIndex], 0
		retn

;--------------------------------------------------------------------------
; UpdateTracks:  Main code to process the next tick to be played.
;--------------------------------------------------------------------------

UpdateTracks:
		dec     byte [TempoWait]
		jz      short GetTracks

		mov	cx, NumTracks
		mov	di, Tracks
BeatTracks:
		call	BeatTrack	
		add	di, TrackInfo.size
		loop	BeatTracks
		retn
GetTracks:
		mov     al, [Tempo]
		mov     [TempoWait], al

		les     si, [Note]
		cmp     byte [Row], 64
		jb      short NoPattWrap

		les     si, [ModInfo.Patterns]
		mov     bl, [OrderPos]
		cmp     bl, [ModInfo.OrderLen]
		jb      short NoOrderWrap
		mov     bl, [ModInfo.ReStart]
		mov     [OrderPos], bl
		cmp     bl, [ModInfo.OrderLen]
		jae     short NoUpdate
NoOrderWrap:    
		xor     bh, bh
		mov     bl, [ModInfo.Order+bx]
		shl     bx, 10
		add     si, bx
		mov     bl, [BreakRow]
		mov     [Row], bl
		xor     bh, bh
		mov     [BreakRow], bh
		shl     bx, 4
		add     si, bx
		mov     [Note], si
		mov     [Note+2], es
		inc     byte [OrderPos]
NoPattWrap:     
		inc     byte [Row]

		cld
		mov	cx, NumTracks
		mov	di, Tracks
GetTracks_next:
		push	cx		
		call	GetTrack
		pop	cx
		add	di, TrackInfo.size
		loop	GetTracks_next

		mov     [Note], si
NoUpdate:
		retn

;--------------------------------------------------------------------------
; MixTrack:  Mixes one track into a CLEAN buffer.
;  In:
;   ds:si -  Track Info Address.
;   ds:di -  Buffer Address.
;    cx   -  Buffer Size.
;--------------------------------------------------------------------------

MixTrack:
		cmp     word [si+TrackInfo.RepLen], 2
		ja      short MixLooped
MixNonLooped:   
		les     dx, [si+TrackInfo.Samples]
		mov     bx, [si+TrackInfo.Position]
		mov     bp, [si+TrackInfo.Len]
		push    dx
		push    si
		add     bx, dx
		add     bp, dx
		mov     dx, [si+TrackInfo.Pitch]
		mov     al, [si+TrackInfo.Volume]
		mov     ah, [si+TrackInfo.Error]
		mov     si, bx
		mov     bh, al
		mov     al, dl
		mov     dl, dh
		xor     dh, dh
nlMixSamp:      
		cmp     si, bp
		jae     short nlMixBye
		mov     bl, [es:si]
		mov     bl, [VolTable+bx]
		add     [di], bl
		inc     di
		add     ah, al
		adc     si, dx
		loop    nlMixSamp
nlMixBye:       
		mov     bx, si
		pop     si
		pop     dx
		sub     bx, dx
		mov     [si+TrackInfo.Position], bx
		mov     [si+TrackInfo.Error], ah
		retn
MixLooped:
		les     dx, [si+TrackInfo.Samples]
		mov     bx, [si+TrackInfo.Position]
		mov     bp, [si+TrackInfo.RepLen]
		mov     [BufRep], bp
		add     bp, [si+TrackInfo.Repeat]
		push    dx
		push    si
		add     bx, dx
		add     bp, dx
		mov     dx, [si+TrackInfo.Pitch]
		mov     al, [si+TrackInfo.Volume]
		mov     ah, [si+TrackInfo.Error]
		mov     si, bx
		mov     bh, al
		mov     al, dl
		mov     dl, dh
		xor     dh, dh
lpMixSamp:      
		cmp     si, bp
		jb      short lpMixNow
		sub     si, [BufRep]
lpMixNow:       
		mov     bl, [es:si]
		mov     bl, [VolTable+bx]
		add     [di], bl
		inc     di
		add     ah, al
		adc     si, dx
		loop    lpMixSamp
lpMixBye:       
		mov     bx, si
		pop     si
		pop     dx
		sub     bx, dx
		mov     [si+TrackInfo.Position], bx
		mov     [si+TrackInfo.Error], ah
		retn

;--------------------------------------------------------------------------
; GetSamples:  Returns the next chunk of samples to be played.
;  In:
;    Buffer  - Buffer Address.
;    Count   - Buffer Size.
;--------------------------------------------------------------------------

GetSamples:
		;ds:si = buffer address
		;cx = count

		;[sp+6] = ds
		;[sp+4] = si
		;[sp+2] = count

		Count	equ 4
		Buffer	equ 6 

		push	bp
		mov	bp, sp

		pusha
		push    ds
		push    es
		cld

		les     di, [bp+Buffer]
		mov     bx, [bp+Count]

NextChunk:      cmp     word [BufLen], 0
		jne     short CopyChunk

		push    bx
		push    di
		push    es
MixChunk:       
		;lea     di, [MixBuffer]
		mov	di, MixBuffer
		mov     cx, [BpmSamples]
		mov     [BufPtr], di
		mov     [BufLen], cx

		mov     ax, ds
		mov     es, ax
		mov     al, 80h
		rep     stosb

		mov	cx, NumTracks
		mov	si, Tracks - TrackInfo.size
GetSamples_next:
		push	cx
		add	si, TrackInfo.size
		mov	cx, [BufLen]
		mov	di, [BufPtr]
		call	MixTrack
		pop	cx
		loop	GetSamples_next		

		call    UpdateTracks

		pop     es
		pop     di
		pop     bx
CopyChunk:      
		mov     cx, [BufLen]
		cmp     cx, bx
		jbe     short MoveChunk
		mov     cx, bx
MoveChunk:
		mov     si, [BufPtr]
		add     [BufPtr], cx
		sub     [BufLen], cx
		sub     bx, cx
		rep     movsb
		test    bx, bx
		jne     short NextChunk

		pop     es
		pop     ds
		popa
		pop	bp
		ret	6

;--------------------------------------------------------------------------
; StartPlaying: Initializes the Sound System.
;  In:
;   Module Information Resources.
;--------------------------------------------------------------------------

StartPlaying:
		pusha
		push    ds
		push    es
SetModParms:    
		mov     byte [OrderPos], 0
		mov     byte [Tempo], DefTempo
		mov     byte [TempoWait], DefTempo
		mov     byte [Bpm], DefBpm
		mov     byte [Row], 64
		mov     byte [BreakRow], 0
		mov     ax, [MixSpeed]
		xor     dx, dx
		mov     bx, 24*DefBpm/60
		div     bx
		mov     [BpmSamples], ax
ClearTracks:    
		mov     di, Tracks
		mov     ax, ds
		mov     es, ax
		mov     cx, NumTracks*TrackInfo.size
		xor     ax, ax
		cld
		rep     stosb

		mov     [BufPtr], ax
		mov     [BufLen], ax
MakePitch:
		mov     ax, MidCRate
		mov     bx, 428
		mul     bx
		div     word [MixSpeed]
		xor     dh, dh
		mov     dl, ah
		mov     ah, al
		xor     al, al
		mov     cx, 857
		xor     bx, bx
		mov     di, PitchTable
PitchLoop:      
		push    ax
		push    dx
		cmp     dx, bx
		jae     short NoDiv
		div     bx
NoDiv:          
		stosw
		pop     dx
		pop     ax
		inc     bx
		loop    PitchLoop
MakeVolume:     
		mov     cx, 16640
		mov     bx, cx
VolLoop:
		dec     bx
		mov     al, bl
		imul    bh
		mov     [VolTable+bx], ah
		loop    VolLoop

		pop     es
		pop     ds
		popa
		;retn

;--------------------------------------------------------------------------
; StopPlaying: ShutDown the Sound System.
;--------------------------------------------------------------------------

StopPlaying:
		retn

;=============================================================================
;               PLAYER.ASM
;=============================================================================

; UTILS.ASM
;----------------------------------------------------------------------------
;       delay1_4ms - Delay for 1/4 millisecond.
;		    1mS = 1000us
;       Entry:
;         None
;       Exit:
;	  None
;
;       Modified:
;         None
;
PORTB			EQU	061h
  REFRESH_STATUS	EQU	010h		; Refresh signal status

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

        pop     cx
        pop     ax
        retn

print_msg:
	; 13/11/2016 - Erdogan Tan 
	; esi = ASCIIZ text address
	;
	mov	bx, 7h
	mov	ah, 0Eh 
pm_next_char:
	lodsb
	and	al, al
	jz	short pm_retn
	int	10h
	jmp	short pm_next_char
pm_retn:
	retn

dword2str:
	; 13/11/2016 - Erdogan Tan 
	; eax = dword value
	;
	call	dwordtohex
	mov	[dword_str], edx
	mov	[dword_str+4], eax
	mov	si, dword_str
	retn

	; trdos386.s (unix386.s) - 10/05/2015
	; Convert binary number to hexadecimal string

bytetohex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	push	bx
	xor	bh, bh
	mov	bl, al
	shr	bl, 4
	mov	bl, [bx+hex_chars] 	 	
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [bx+hex_chars] 
	pop	bx	
	retn

wordtohex:
	; INPUT ->
	; 	AX = word (binary number)
	; OUTPUT ->
	;	EAX = hexadecimal string
	;
	push	bx
	xor	bh, bh
	xchg	ah, al
	push	ax
	mov	bl, ah
	shr	bl, 4
	mov	al, [bx+hex_chars] 	 	
	mov	bl, ah
	and	bl, 0Fh
	mov	ah, [bx+hex_chars]
	shl	eax, 16
	pop	ax
	pop	bx
	jmp	short bytetohex

dwordtohex:
	; INPUT ->
	; 	EAX = dword (binary number)
	; OUTPUT ->
	;	EDX:EAX = hexadecimal string
	;
	push	eax
	shr	eax, 16
	call	wordtohex
	mov	edx, eax
	pop	eax
	call	wordtohex
	retn

	; 13/11/2016 - Erdogan Tan
write_ac97_dev_info:
	; BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	xor	bh, bh
	mov	esi, [dev_vendor]
	mov	ax, si
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
	shr	esi, 16
	mov	ax, si
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

	mov	esi, [bus_dev_fn]
	shr	esi, 8
	mov	ax, si
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

	mov	ax, [ac97_io_base]
	mov	bl, al
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr+3], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr+2], al
	mov	bl, ah
	mov	dl, bl
	and	bl, 0Fh
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr+1], al
	mov	bl, dl
	shr	bl, 4
	mov	al, [bx+hex_chars]
	mov	[msgIOBaseAddr], al

	; 24/11/2016
	xor	ah, ah
	mov	al, [ac97_int_ln_reg]
	mov	cl, 10
	div	cl
	add	[msgIRQ], ax
	and	al, al
	jnz	short _pmi
	mov	al, [msgIRQ+1]
	mov	ah, ' '
	mov	[msgIRQ], ax
_pmi:
        mov	dx, msgAC97Info
        mov     ah, 9
        int     21h
        retn

;=============================================================================
;               preinitialized data
;=============================================================================

;=============================================================================
;               PLAY.ASM - DATA
;=============================================================================

msg_2017:
		db	'Tiny MOD Player by Erdogan Tan. February 2017.',10,13
		db	'usage: playmod filename.mod', 10, 13, '$'
		db	'15/02/2017'

Credits:	db	'Tiny MOD Player v0.1b by Carlos Hasan. July 1993.'
		db	10,13,'$'
ErrorMesg:	db	'Error loading Module file.',10,13,'$'
MsgNotFound:	db	'Sound Blaster not found or IRQ error.',10,13,'$'
MsgFound:	db	'Sound Blaster found at Address 2'
PortText:	db	'x0h, IRQ '
IrqText:	db	'x.',10,13,'$'

;=============================================================================
;               MODPLAY.ASM - DATA
;=============================================================================

;Credits:	db	'Amiga Module Player v0.3b by Carlos Hasan.'

SinTable:	db	0,25,50,74,98,120,142,162,180,197,212,225
		db	236,244,250,254,255,254,250,244,236,225
		db	212,197,180,162,142,120,98,74,50,25

PeriodTable:	dw	856,808,762,720,678,640,604,570,538,508,480,453
		dw	428,404,381,360,339,320,302,285,269,254,240,226
		dw	214,202,190,180,170,160,151,143,135,127,120,113

;=============================================================================
;               PLAYER.ASM - DATA
;=============================================================================

tBuff	db 2	; 08/12/2016

; 24/11/2016
;	       IRQ  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
irq_int		db 08h,09h,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh,70h,71h,72h,73h,74h,75h,76h,77h

; 13/11/2016
hex_chars	db "0123456789ABCDEF", 0
msgAC97Info	db "AC97 Audio Controller & Codec Info", 0Dh, 0Ah 
		db "Vendor ID: "
msgVendorId	db "0000h Device ID: "
msgDevId	db "0000h", 0Dh, 0Ah
		db "Bus: "
msgBusNo	db "00h Device: "
msgDevNo	db "00h Function: "
msgFncNo	db "00h"
		db 0Dh, 0Ah
		db "I/O Base Address: "
msgIOBaseAddr	db "0000h IRQ: "
msgIRQ		dw 3030h
		db 0Dh, 0Ah, "$"
msgSampleRate	db "Sample Rate: "
msgHertz	db "00000 Hz ", "$" 
msg8Bits	db "8 bits ", "$" 
msgMono		db "Mono", 0Dh, 0Ah, "$"
msg16Bits	db "16 bits ", "$" 
msgStereo	db "Stereo", 0Dh, 0Ah, "$"

;; 13/11/2016 - Erdogan Tan (Ref: KolibriOS, codec.inc)
;codec_id	dd 0
;codec_chip_id	dd 0
;codec_vendor_ids dw 0
;codec_chip_ids	dw 0

dword_str	 dd 30303030h, 30303030h
		 db 'h', 0Dh, 0Ah, 0

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

alignb 4

stmo		resb	1 ; stereo or mono  
bps		resb	1 ; bits per sample (16)

smpRBuff:	resw	14	; 19/11/2016

flags:		resb	1
irq_status:	resb 	1

uLVI:		resb	1 
tLoop:		resb 	1

; 256 byte buffer for descriptor list
BDL_BUFFER:	resw	1		; segment of our 256byte BDL buffer
DMA_BUFFER1:	resw 	1		; Pointer to 1st half of DMA Buffer
DMA_BUFFER2:	resw	1		; Pointer to 1st half of DMA Buffer

; 12/11/2016 - Erdogan Tan

ac97_int_ln_reg: resb 1 
err_num:	resb 1

bus_dev_fn:	resd 1
dev_vendor:	resd 1
stats_cmd:	resd 1
ac97_io_base:	resw 1

alignb 4

BdlBuffer:	resb	BDL_SIZE ; 13/02/2017
DmaBuffer:	resb	2*BUFFERSIZE ; 13/02/2017

; MODLOAD.ASM
FileHandle:	resw	1
ErrorInfo:	resw	1
Header:		resb	ModHeader.size

sample_rate: ; PLAYER.ASM (22050Hz)
; MODPLAY.ASM
MixSpeed:	    resw 1

ModInfo:
ModInfo.OrderLen:   resb 1
ModInfo.ReStart:    resb 1
ModInfo.Order:	    resb 128
ModInfo.Patterns:   resd 1

ModInfo.SampOfs:    resw 31
ModInfo.SampSeg:    resw 31
ModInfo.SampLen:    resw 31
ModInfo.SampRep:    resw 31
ModInfo.SampRepLen: resw 31
ModInfo.SampVol:    resw 31

; MODPLAY.ASM
PitchTable:	resw	857
VolTable:	resb	16640
MixBuffer       resb	MixBufSize

; MODPLAY.ASM
OrderPos:	resb 1
Tempo:		resb 1
TempoWait:	resb 1
Bpm:		resb 1
Row:		resb 1
BreakRow:	resb 1
BpmSamples:	resw 1
BufPtr:		resw 1
BufLen:		resw 1
BufRep:		resw 1
Note:		resd 1
Tracks:		resb TrackInfo.size*NumTracks

alignb 16

; PLAY.ASM
Scope:		resw	320
RowOfs:		resw	256

EOF: