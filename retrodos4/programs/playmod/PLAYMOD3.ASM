; ****************************************************************************
; playmod3.asm (for MSDOS)
; ----------------------------------------------------------------------------
; PLAYMOD3.COM ! ICH AC97 MOD PLAYER & VGA DEMO program by Erdogan TAN
;
; 18/02/2017
;
; [ Last Modification: 15/05/2024 ]
;
; Derived from source code of 'PLAYWAV.COM' ('PLAYWAV.ASM') by Erdogan Tan
;							      (17/02/2017)
; Modified from 'PLAYMOD2.COM' ('playmod2.asm') source code 
;		 AC97 MOD PLAYER & VGA DEMO program (tuneloop version)
;						by Erdogan TAN (12/05/2024)	
;
; Derived from source code of 'PLAY.EXE' (TINYPLAY) by Carlos Hasan (1993)
;          PLAY.EXE: PLAY.ASM, MODLOAD.ASM, MODPLAY.ASM, SB.ASM
;
; Assembler: NASM 2.15
; ----------------------------------------------------------------------------
;	   nasm  playmod3.asm -l playmod3.lst -o PLAYMOD3.COM	
; ****************************************************************************

; Tiny MOD Player v0.1b by Carlos Hasan.
;		July 14th, 1993.

;=============================================================================
;               PLAYWAV.ASM / PLAYER.ASM / TINYPLAY.ASM
;=============================================================================
; Audio controller, codec & PCI functions are derived from '.wav file player 
; for DOS' source code by Jeff Leyda (PLAYER.EXE), Sep 02, 2002.

; AC97 Interrupt version ; 12/05/2024

[BITS 16]
[org 100h]

Start:
	; 15/05/2024
	; 13/05/2024
	; Clear BSS (uninitialized data) area
	;xor	ax, ax ; 0
	;mov	cx, (EOF - bss_start)/2
	;mov	di, bss_start
	;rep	stosw

	call    DetectICH		; Detect AC97 Audio Device
GetFileName:    			; Parse  the Command line...
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

PrintMesg:
	mov	ax, 0900h		; Prints the Credits Text.
	;lea	dx, [Credits]
	mov	dx, Credits
	int	21h

	; 13/05/2024
	call	write_ac97_dev_info
	mov	ax, 0900h		; Prints the Credits Text.
	mov	dx, CRLF
	int	21h
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
	; 13/05/2024
	;call	write_ac97_dev_info

	; 08/05/2024
	; 17/02/2017
	;mov	dx, [stats_cmd]
        ;or	dl, IO_ENA+BM_ENA		; enable IO and bus master
        ;call	pciRegWrite16 ; pciRegWrite8

	; 18/02/2017
	;mov	word [sample_rate], 22050	; Mixing at 22.050 kHz
	; 15/05/2024
	mov	word [sample_rate], 24000

	; 08/05/2024
	; (48 kHZ mixing is necessary 
	;  if the AC97 hardware/codec has not got VRA feature)
	; ((or frequency converting code would be needed))
	;mov	word [sample_rate], 48000	; Mixing at 48 kHz

; setup the Codec (actually mixer registers) 
        call    codecConfig			; unmute codec, set rates.
	jnc	short PlayNow

_codec_err:
	push	cs
	pop	ds
        mov	dx, CodecErrMsg
        mov     ah, 9
        int     21h
        jmp     Exit

CodecErrMsg db "Codec Error!"
	db	CR,LF,"$"
PlayNow:  
	mov	ax, BdlBuffer
	mov	[BDL_BUFFER], ax
    
	mov	ax, DmaBuffer		; DmaBuffer (4096 bytes) buff addr
	mov	[DMA_BUFFER1], ax	; 2048 byte half buffer 1 

	add	ax, BUFFERSIZE		; code/current segment
	mov	[DMA_BUFFER2], ax	; 2048 byte half buffer 2
  
	;mov    word [MixSpeed], 22050	; Mixing at 22.050 kHz

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
	;call   FreeModule		; Free MODule core.
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

DetectICH:
	; 18/02/2017
	; Detech Intel ICH based AC97 Audio Device 
	call    pciFindDevice 	; AC97.ASM (PLAYWAV.COM)
        jnc     short _1

; couldn't find the audio device!
	;push	cs
	;pop	ds
        mov     dx, noDevMsg
        mov     ah, 9
        int     21h
        jmp     short error_exit

noDevMsg db "Error: Unable to find Intel ICH based audio device!",CR,LF,"$"

_1:
	; 18/02/2017
	; eax = BUS/DEV/FN
	;	00000000BBBBBBBBDDDDDFFF00000000
	; edx = DEV/VENDOR
	;	DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV

	mov	[bus_dev_fn], eax
	mov	[dev_vendor], edx

	; get ICH base address regs for mixer and bus master

        mov     al, NAMBAR_REG
        call    pciRegRead16			; read PCI registers 10-11
        ;and    dx, IO_ADDR_MASK 		; mask off BIT0
	; 14/05/2024
	and	dl, 0FEh

        mov     [NAMBAR], dx			; save audio mixer base addr

	mov     al, NABMBAR_REG
        call    pciRegRead16
        ;and    dx, IO_ADDR_MASK
	; 14/05/2024
	and	dl, 0C0h

        mov     [NABMBAR], dx			; save bus master base addr

	; 08/05/2024
	; 06/11/2023
	;; init controller
	;; 17/02/2017
	;mov	al, PCI_CMD_REG ; command register (04h)
	;call	pciRegRead16 ; pciRegRead8
	;
	;; eax = BUS/DEV/FN/REG
	;;  dx = PCI Command Register Content ; 17/02/2017
	;; 	00000000CCCCCCCC
	;mov	[stats_cmd], dx
	;
	; 06/11/2023
	;mov	al, PCI_IO_BASE ; IO base address register (10h)
	;call	pciRegRead32
	;
	;and	dx, 0FFC0h	; IO_ADDR_MASK (0FFFE) ?
        ;mov	[ac97_io_base], dx

	mov	al, AC97_INT_LINE ; Interrupt line register (3Ch)
	call	pciRegRead8 ; 17/02/2017

	mov     [ac97_int_ln_reg], dl

	; 28/11/2016
	;mov	bx, 1	; 08/05/2024
	xor	dh, dh	; 17/02/2017
	; 10/11/2023
	;mov	cx, dx
	;shl	bx, cl

	; 04/11/2023
	cli

	;not	bx
	in	al, 0A1h ; irq 8-15
        mov	ah, al
        in	al, 21h  ; irq 0-7 

	; 04/11/2023
	; save IRQ status
	mov	[IRQ_status], ax

	; 12/05/2024 (enable AC97 IRQ)
	;mov	cl, dl
	;mov	bx, 1
	;shl	bx, cl
	;not	bx
	;and	ax, bx
	;out	21h, al
	;mov	al, ah
	;out	0A1h, al

	; 15/05/2024
	;and	ax, bx   ; unmask
 	btr	ax, dx	 ; unmask
	out	21h, al  ; enable interrupt (if irq <= 7)
	mov	al, ah
	out	0A1h, al ; enable interrupt (if irq > 7)
	;not	bx

	; 08/05/2024
	;mov	dx, 4D1h			;8259 ELCR1
        ;in	al, dx
	;mov	ah, al
	;mov	dx, 4D0h
        ;in	al, dx
	;;or	ax, bx
	;bts	ax, cx
	;mov	dx, 4D0h
	;out	dx, al                          ;set level-triggered mode
	;mov	al, ah
	;mov	dx, 4D1h
	;out	dx, al                          ;set level-triggered mode

	; 24/11/2016 - Erdogan Tan
	;mov	bx, cx
	; 10/11/2023
	mov	bx, dx
	mov	bl, [bx+irq_int]
	shl	bx, 2 ; * 4

	; set up interrupt vector
	; 30/11/2016
	push	es
	xor	ax, ax
	mov	es, ax
	; 04/11/2023
	; save interrupt vector
	;mov	ax, [es:bx]
	; 13/05/2024
	mov	ax, ac97_int_handler
	xchg	ax, [es:bx]
	mov	[IRQ_vector], ax
	;mov	ax, [es:bx+2]
	; 13/05/2024
	mov	ax, cs
	xchg	ax, [es:bx+2]
	mov	[IRQ_vector+2], ax

	; 13/05/2024
	;mov	word [es:bx], ac97_int_handler
	;mov	ax, cs
	;mov	[es:bx+2], ax
	
	pop	es

	; 04/11/2023
	sti

	retn

	; 15/05/2024
ac97_int_handler:
	; 13/05/2024
	; 12/05/2024
	; 11/05/2024
	; 11/11/2023
	; 10/11/2023
	; 17/02/2016
	push	eax	; 11/11/2023
	push	dx
	; 05/11/2023
	;push	cx
	;push	bx
	;push	si
	;push	di

	; 10/11/2023
	; EOI at first
	mov	al, 20h
	test	byte [ac97_int_ln_reg], 8
	jz	short _ih_0
	out 	0A0h, al ; 20h	; EOI
_ih_0:
	out	20h, al  ; 20h	; EOI

	; 11/11/2023
	; 09/11/2023
	mov	dx, GLOB_STS_REG
        add	dx, [NABMBAR]
	in	eax, dx

	; 12/05/2024
	; 09/11/2023
	;cmp	eax, 0FFFFFFFFh ; -1
	;je	short _ih_3
	; 15/05/2024
	inc	eax	; 0FFFFFFFFh
	jz	short _ih_3
	dec	eax	; 0
	jz	short _ih_3

	; 15/05/2024
	cmp	byte [tLoop], 1
	jb	short _ih_2	

	; 12/05/2024
	;test	al, 40h		; PCM Out Interrupt
	; 15/05/2024
	test	al, PCM_OUT_IRQ+BCIS
	jnz	short _ih_1

	; 15/05/2024
	;test	eax, eax
	;jz	short _ih_3

	; 12/05/2024
	;test	ax, PCM_OUT_IRQ+BCIS
	;jnz	short _ih_1

 	;mov	dx, GLOB_STS_REG
        ;add	dx, [NABMBAR]
	; 12/05/2024
	;out	dx, eax
	jmp	short _ih_2
	;jmp	short _ih_3

	; .....
	;mov	al, 1
	; 10/11/2023
	;mov	[tLoop], al  ; 1

	;cmp	[inside], al ; 1
	;jnb	short _ih_2	; busy

	;mov	[inside], al ; 1
	;
	;; 09/11/2023
        ;mov	dx, [NABMBAR]
        ;add	dx, PO_SR_REG	; set pointer to Status reg
	;in	al, dx
	;; 10/11/2023
	;;;out	dx, eax
	;;out	dx, al		; clear interrupt event
	;			; (by writing 1 to same bits)
	;
	;;mov	[pcm_irq_status], al ; 05/11/2023
	;test	al, BCIS ; Buffer Completion Interrupt Status (Bit 3)
	;jz	short _ih_3
	; .....

_ih_1:
	; 11/11/2023
	;push	eax
	; 15/05/2024
	push	ax

	; 13/05/2024
	mov	ax, 1Ch ; FIFOE(=16)+BCIS(=8)+LVBCI(=4)
	mov	dx, PO_SR_REG
        add	dx, [NABMBAR]
	out	dx, ax

	; 13/05/2024
	mov	dx, PO_CIV_REG
        add	dx, [NABMBAR]
	in	al, dx
	mov	ah, al
	dec	al
	and	al, 1Fh
        mov     dx, PO_LVI_REG
        add	dx, [NABMBAR]
        out	dx, al
	and	ah, 1
	; 13/05/2024
	inc	ah
	mov	[tBuff], ah ; 1 = Buffer 1, 2 = Buffer 2

	; 13/05/2024
	; 10/11/2023
	; 28/11/2016 - Erdogan Tan
	;call	tuneLoop

	; 11/11/2023
	;pop	eax
	; 15/05/2024
	pop	ax	

	mov	dx, GLOB_STS_REG
        add	dx, [NABMBAR]
_ih_2:			; 12/05/2024
	out	dx, eax

	; 12/05/2024
_ih_3:
	; 11/11/2023
	;mov	dx, [NABMBAR]
	;add	dx, PO_SR_REG	; set pointer to Status reg
	;mov	ax, 1Ch
	;out	dx, ax

;	; 10/11/2023
;	mov	al, 20h
;	test	byte [ac97_int_ln_reg], 8
;	jz	short _ih_3
;	out 	0A0h, al ; 20h	; EOI
;_ih_3:
;	out	20h, al  ; 20h	; EOI
;_ih_4:
	;mov	byte [inside], 0
	;pop	di
	;pop	si
	;pop	bx
	;pop	cx
	pop	dx
	pop	eax ; 11/11/2023
	iret

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

; AC97.ASM
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

; AC97.ASM (PLAYWAV.COM)
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

; ----------------------------------------------------------------------------
; 17/02/2017
PCI_IO_BASE	equ 10h			; = NAMBAR register offset
AC97_INT_LINE   equ 3Ch			; AC97 Interrupt Line register offset

; ----------------------------------------------------------------------------
; ICH2AC97.INC
; ----------------------------------------------------------------------------

; PCI stuff

; Intel ICH2 equates. It is assumed that ICH0 and plain ole ICH are compatible.

INTEL_VID       equ     8086h           ; Intel's PCI vendor ID

; 08/05/2024
; 03/11/2023 - Erdogan Tan (Ref: MenuetOS AC97 WAV Player source code, 2004)
SIS_VID		equ	1039h
NVIDIA_VID	equ	10DEh	 ; Ref: MPXPLAY/SBEMU/KOLIBRIOS AC97 source c.
AMD_VID		equ	1022h

ICH_DID         equ     2415h           ; ICH device ID
ICH0_DID        equ     2425h           ; ICH0
ICH2_DID        equ     2445h           ; ICH2 I think there are more ICHes.
                                        ; they all should be compatible.
; 08/05/2024
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

NAMBAR_REG      equ     10h             ; native audio mixer BAR
 NAM_SIZE       equ     256             ; 256 bytes required.

NABMBAR_REG     equ     14h             ; native audio bus mastering BAR
 NABM_SIZE      equ     64              ; 64 bytes

; BUS master registers, accessed via NABMBAR+offset

; ICH supports 3 different types of register sets for three types of things
; it can do, thus:
;
; PCM in (for recording) aka PI
; PCM out (for playback) aka PO
; MIC in (for recording) aka MC

PI_BDBAR_REG            equ     0       ; PCM in buffer descriptor BAR
PO_BDBAR_REG            equ     10h     ; PCM out buffer descriptor BAR
MC_BDBAR_REG            equ     20h     ; MIC in buffer descriptor BAR

; each buffer descriptor BAR holds a pointer which has entries to the buffer
; contents of the .WAV file we're going to play.  Each entry is 8 bytes long
; (more on that later) and can contain 32 entries total, so each BAR is
; 256 bytes in length, thus:

BDL_SIZE                equ     32*8    ; Buffer Descriptor List size
INDEX_MASK              equ     31      ; indexes must be 0-31



PI_CIV_REG              equ     4       ; PCM in current Index value (RO)
PO_CIV_REG              equ     14h     ; PCM out current Index value (RO)
MC_CIV_REG              equ     24h     ; MIC in current Index value (RO)
;8bit read only
; each current index value is simply a pointer showing us which buffer
; (0-31) the codec is currently processing.  Once this counter hits 31, it
; wraps back to 0.
; this can be handy to know, as once it hits 31, we're almost out of data to
; play back or room to record!


PI_LVI_REG              equ     5       ; PCM in Last Valid Index
PO_LVI_REG              equ     15h     ; PCM out Last Valid Index
MC_LVI_REG              equ     25h     ; MIC in Last Valid Index
;8bit read/write
; The Last Valid Index is a number (0-31) to let the codec know what buffer
; number to stop on after processing.  It could be very nasty to play audio
; from buffers that aren't filled with the audio we want to play.


PI_SR_REG               equ     6       ; PCM in Status register
PO_SR_REG               equ     16h     ; PCM out Status register
MC_SR_REG               equ     26h     ; MIC in Status register
;16bit read/write
; status registers.  Bitfields follow:

FIFO_ERR                equ     BIT4    ; FIFO Over/Underrun W1TC.

BCIS                    equ     BIT3    ; buffer completion interrupt status.
                                        ; Set whenever the last sample in ANY
                                        ; buffer is finished.  Bit is only
                                        ; set when the Interrupt on Complete
                                        ; (BIT4 of control reg) is set.

LVBCI                   equ     BIT2    ; Set whenever the codec has processed
                                        ; the last buffer in the buffer list.
                                        ; Will fire an interrupt if IOC bit is
                                        ; set. Probably set after the last
                                        ; sample in the last buffer is
                                        ; processed.  W1TC

                                        ; 
CELV                    equ     BIT1    ; Current buffer == last valid.
                                        ; Bit is RO and remains set until LVI is
                                        ; cleared.  Probably set up the start
                                        ; of processing for the last buffer.


DCH                     equ     BIT0    ; DMA controller halted.
                                        ; set whenever audio stream is stopped
                                        ; or something else goes wrong.

PI_PICB_REG             equ     8       ; PCM in position in current buffer(RO)
PO_PICB_REG             equ     18h     ; PCM out position in current buffer(RO)
MC_PICB_REG             equ     28h     ; MIC in position in current buffer (RO)
;16bit read only
; position in current buffer regs show the number of dwords left to be
; processed in the current buffer.
; 

PI_PIV_REG              equ     0ah     ; PCM in Prefected index value
PO_PIV_REG              equ     1ah     ; PCM out Prefected index value
MC_PIV_REG              equ     2ah     ; MIC in Prefected index value
;8bit, read only
; Prefetched index value register.
; tells which buffer number (0-31) has be prefetched.  I'd imagine this
; value follows the current index value fairly closely. (CIV+1)
;

PI_CR_REG               equ     0bh     ; PCM in Control Register
PO_CR_REG               equ     1bh     ; PCM out Control Register
MC_CR_REG               equ     2bh     ; MIC in Control Register
; 8bit
; Control register *MUST* only be accessed as an 8bit value.
; Control register.  See bitfields below.
;

IOCE                    equ     BIT4    ; interrupt on complete enable.
                                        ; set this bit if you want an intrtpt
                                        ; to fire whenever LVBCI is set.
FEIFE                   equ     BIT3    ; set if you want an interrupt to fire
                                        ; whenever there is a FIFO (over or
                                        ; under) error.
LVBIE                   equ     BIT2    ; last valid buffer interrupt enable.
                                        ; set if you want an interrupt to fire
                                        ; whenever the completion of the last
                                        ; valid buffer.
RR                      equ     BIT1    ; reset registers.  Nukes all regs
                                        ; except bits 4:2 of this register.
                                        ; Only set this bit if BIT 0 is 0
RPBM                    equ     BIT0    ; Run/Pause
                                        ; set this bit to start the codec!


GLOB_CNT_REG            equ     2ch     ; Global control register
SEC_RES_EN              equ     BIT5    ; secondary codec resume event 
                                        ; interrupt enable.  Not used here.
PRI_RES_EN              equ     BIT4    ; ditto for primary. Not used here.
ACLINK_OFF              equ     BIT3    ; Turn off the AC97 link
ACWARM_RESET            equ     BIT2    ; Awaken the AC97 link from sleep.
                                        ; registers preserved, bit self clears
ACCOLD_RESET            equ     BIT1    ; Reset everything in the AC97 and
                                        ; reset all registers.  Not self clearing

GPIIE                   equ     BIT0    ; GPI Interrupt enable.
                                        ; set if you want an interrupt to
                                        ; fire upon ANY of the bits in the
                                        ; GPI (general pursose inputs?) not used.

GLOB_STS_REG            equ     30h     ; Global Status register (RO)

MD3                     equ     BIT17   ; modem powerdown status (yawn)
AD3                     equ     BIT16   ; Audio powerdown status (yawn)
RD_COMPLETE_STS         equ     BIT15   ; Codec read timed out. 0=normal
BIT3SLOT12              equ     BIT14   ; shadowed status of bit 3 in slot 12
BIT2SLOT12              equ     BIT13   ; shadowed status of bit 2 in slot 12
BIT1SLOT12              equ     BIT12   ; shadowed status of bit 1 in slot 12
SEC_RESUME_STS          equ     BIT11   ; secondary codec has resumed (and irqed)
PRI_RESUME_STS          equ     BIT10   ; primary codec has resumed (and irqed)
SEC_CODEC_RDY           equ     BIT9    ; secondary codec is ready for action
PRI_CODEC_RDY           equ     BIT8    ; Primary codec is ready for action
                                        ; software must check these bits before
                                        ; starting the codec!
MIC_IN_IRQ              equ     BIT7    ; MIC in caused an interrupt
PCM_OUT_IRQ             equ     BIT6    ; One of the PCM out channels IRQed
PCM_IN_IRQ              equ     BIT5    ; One of the PCM in channels IRQed
MODEM_OUT_IRQ           equ     BIT2    ; modem out channel IRQed
MODEM_IN_IRQ            equ     BIT1    ; modem in channel IRQed
GPI_STS_CHANGE          equ     BIT0    ; set whenever GPI's have changed.
                                        ; BIT0 of slot 12 also reflects this.


ACC_SEMA_REG            equ     34h     ; Codec write semiphore register
CODEC_BUSY              equ     BIT0    ; codec register I/O is happening
                                        ; self clearing
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

; CODE

; AC97.ASM

; codec configuration code.  Not much here really.
; NASM version: Erdogan Tan (29/11/2016)

; enable codec, unmute stuff, set output rate to 44.1
; entry: ax = desired sample rate
;
	
	; 15/05/2024
	; 12/05/2024
	; 08/05/2024
	; (ac97_vra.asm, 19/11/2023, Erdogan Tan, playwav3.asm) 
codecConfig:
	; 02/12/2023
	; 26/11/2023
	; 21/11/2023
	; 20/11/2023
	; 19/11/2023 (TRDOS 386 v2.0.7)
	; 15/11/2023
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

	; 08/05/2024
	; 11/11/2023
	CTRL_ST_CREADY	equ	BIT8+BIT9+BIT28 ; Primary Codec Ready
	CODEC_REG_POWERDOWN equ	26h

	; 04/11/2023
init_ac97_controller:
	mov	eax, [bus_dev_fn]
	mov	al, PCI_CMD_REG
	call	pciRegRead16		; read PCI command register
	or      dl, IO_ENA+BM_ENA	; enable IO and bus master
	call	pciRegWrite16

	; 14/05/2024
	; 02/12/2023
	call	delay_100ms ; 29/05/2017

	; 02/12/2023
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms

init_ac97_codec:
	; 18/11/2023
	mov	bp, 40
	; 14/05/2024
	;mov	bp, 100
_initc_1:
	; 11/11/2023
	; (TRDOS 386 v2.0.5, 'audio.s')
	mov	dx, GLOB_CNT_REG ; 2Ch
	add	dx, [NABMBAR]
	in	eax, dx

	; 02/12/2023
	call	delay1_4ms

	; ?
	;; 15/11/2023
	;;mov	cx, 40
	;mov	bp, 40 ; 18/11/2023
;_initc_1:
	mov	dx, GLOB_STS_REG ; 30h
	add	dx, [NABMBAR]
	in	eax, dx

	; 02/12/2023
	call	delay1_4ms

	cmp	eax, 0FFFFFFFFh ; -1
	;je	short init_ac97_codec_err1
	; 15/11/2023
	jne	short _initc_3
_initc_2:
	;dec	cx
	dec	bp	; 18/11/2023
	;jz	short init_ac97_codec_err1
	; 19/11/2023
	jz	short _ac97_codec_ready

	call	delay_100ms
	jmp	short _initc_1
_initc_3:
	test	eax, CTRL_ST_CREADY
	jnz	short _ac97_codec_ready

	call	reset_ac97_codec
	; 11/11/2023
	;jc	short init_ac97_codec_err2
	; 15/11/2023
	;jc	short _initc_2
	; 26/11/2023
	jmp	short _initc_2

_ac97_codec_ready:
	mov	dx, [NAMBAR]
	;add	dx, 0 ; ac_reg_0 ; reset register
	out	dx, ax

	call	delay_100ms

	; 19/11/2023
	or	bp, bp
	jnz	short _ac97_codec_init_ok

	xor	eax, eax ; 0
	mov	dx, [NAMBAR]
	add	dx, CODEC_REG_POWERDOWN
	out	dx, ax
	
	; 19/11/2023
	; wait for 1 second
	; 15/05/2024
	mov	ecx, 1000 ; 1000*4*0.25ms = 1s
	;mov	cx, 10
_ac97_codec_rloop:
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms
	;call	delay1_4ms
	call	delay_100ms

	;mov	dx, [NAMBAR]
	;add	dx, CODEC_REG_POWERDOWN
	in	ax, dx

	; 14/05/2024
	; 02/12/2023
	call	delay1_4ms
	
	and	ax, 0Fh
	cmp	al, 0Fh
	je	short _ac97_codec_init_ok
	loop	_ac97_codec_rloop 

	; 12/05/2024
	; cf = 1
init_ac97_codec_err1:
	;stc	; 12/05/2024
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
	; 21/11/2023 - temporary
	call	delay_100ms

	;call 	setup_ac97_codec

setup_ac97_codec:
	; 08/05/2028
	; [sample_rate] = 22050
	; 12/11/2023
	;cmp	word [sample_rate], 48000
	;je	short skip_rate

; 11/11/2023
; 05/11/2023
;%if 1
	AC97_EA_VRA equ BIT0 ; 11/11/2023

	; 11/11/2023
	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah  	  
	in     	ax, dx

	; 02/12/2023
	call	delay1_4ms

	; 15/05/2024
	test	al, 1 ; BIT0 ; Variable Rate Audio bit
	jz	short vra_not_supported

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

	; 13/11/2023
	;mov	byte [VRA], 0
vra_not_supported:
	; 08/05/2024
	mov	word [sample_rate], 48000
	jmp	short skip_rate	

	; 12/11/2023
	;pop	ax ; discard return address to the caller
	;mov	dx, msg_no_vra
	;jmp	vra_err

set_rate:
	mov	ax, [sample_rate] ; 17/02/2017 (Erdogan Tan)
	; 08/05/2024
	; ax = 22050 (Hz)

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
	mov	[volume], al ; 29
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

	; 14/05/2024
	; 20/11/2023
        call	delay1_4ms
        call	delay1_4ms
        call	delay1_4ms
        call	delay1_4ms

	; 21/11/2023 temporary
	;call	delay_100ms

	;mov	ax, 0202h
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCM_OUT_REG		;18h 
  	out     dx, ax

	; 14/05/2024
	; 20/11/2023
        call	delay1_4ms
        call	delay1_4ms
        call	delay1_4ms
        call	delay1_4ms

	; 14/05/2024
	; 21/11/2023 - temporary
	;call	delay_100ms

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

	; 14/05/2024
	;call	delay_100ms

	; 14/05/2024
	clc

;detect_ac97_codec:
        retn

reset_ac97_controller:
	; 11/11/2023
	; 10/06/2017
	; 29/05/2017
	; 28/05/2017
	; reset AC97 audio controller registers
	xor     ax, ax
        mov	dx, PI_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 14/05/2024
	call	delay1_4ms

        mov     dx, PO_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 14/05/2024
	call	delay1_4ms

        mov     dx, MC_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 14/05/2024
	call	delay1_4ms

        mov     al, RR
        mov     dx, PI_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 14/05/2024
	call	delay1_4ms

        mov     dx, PO_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 14/05/2024
	call	delay1_4ms

        mov     dx, MC_CR_REG
	add	dx, [NABMBAR]
	out     dx, al

	; 14/05/2024
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

;=============================================================================
;               ICH_WAV.ASM
;=============================================================================

; DOS based .WAV player using AC'97 and codec interface.
; ---------------------------------------------------------------
; NASM version: Erdogan Tan (29/11/2016)
; Last Update: 17/02/2017 (by Erdogan Tan)

; ICHWAV.ASM
; PLAYMOD.ASM
; player internal variables and other equates.
;BUFFERSIZE	equ	2048*4		; 8K half buffer size. 18/02/2017
; 15/05/2024
BUFFERSIZE	equ	2560*4
;ENDOFFILE	equ	BIT0		; flag for knowing end of file

;===========================================================================
; entry: none.  File is already open and [filehandle] filled.
; exit:  not until the song is finished or the user aborts.
;
; 18/02/2017
ModPlay: ; 13/02/2017  ; ModPlay Polling!
	;cld
	; clear (half) buffer 2
       	;mov	di, [DMA_BUFFER2]
	;sub	ax, ax
	;mov	cx, (BUFFERSIZE/2)
	;rep	stosw

	; 15/05/2024
	push	es
	push	ds
	pop	es
	mov	cx, 320
	mov	di, MOD_BUFFER
	mov	ax, 8080h
	rep	stosw
	mov	cx, 0A000h
	mov	es, cx
	call	ScopeLoop
	pop	es
	
        ; load 2048 bytes into buffer 1
	mov	si, [DMA_BUFFER1]
	; 11/05/2024
	;mov	cx, BUFFERSIZE
	;push	ds ; segment
	;push	si ; offset
	;push	cx ; count
	call    GetSamples_ICH ; 18/02/2017

        ; load 2048 bytes into buffer  2
	mov	si, [DMA_BUFFER2]
	; 11/05/2024
	;mov	cx, BUFFERSIZE
	;push	ds ; segment
	;push	si ; offset
	;push	cx ; count
	call	GetSamples_ICH ; 18/02/2017

; register reset the DMA engine. This may cause a pop noise on the output
; lines when the device is reset. Prolly a better idea to mute output, then
; reset.

	; 08/05/2024
        ;mov    dx, [NABMBAR]
        ;add    dx, PO_CR_REG		; set pointer to Ctrl reg
        ;mov    al, RR			; set reset
	;out    dx, al			; self clearing bit

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
; if it ever runs out of data to play. Good for safety.
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
; set length to 32k samples. 1 sample is 16bits or 2bytes.
; Set control (bits 31:16) to BUP, bits 15:0=number of samples.
; 

; 17/02/2017 (Erdogan Tan)
; Intel® 82801AA (ICH) & Intel® 82801AB (ICH0) I/O Controller Hub AC ’97
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
	; 08/12/2016 - Erdogan Tan
	;mov	eax, BUFFERSIZE
	; 12/05/2024
	mov	eax, BUFFERSIZE/2
	or	eax, IOC + BUP
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
	;mov	eax, BUFFERSIZE
	; 12/05/2024
	mov	eax, BUFFERSIZE/2
	or	eax, IOC + BUP
	stosd
        loop    _0
;
; tell the DMA engine where to find our list of Buffer Descriptors.
; this 32bit value is a flat mode memory offset (ie no segment:offset)
;
; write NABMBAR+10h with offset of buffer descriptor list
;
        movzx   eax, word [BDL_BUFFER]
	; 14/02/2017
	add	eax, edx ; linear/physical address of the BDL
  	; 18/02/2017
        mov     dx, [NABMBAR]
        add     dx, PO_BDBAR_REG                ; set pointer to BDL
        out     dx, eax                         ; write to AC97 controller

	; 15/05/2024
	call	delay1_4ms
;
; All set. Let's play some music.
;
	; 08/12/2016
	; 07/10/2016
        ;mov    al, 1
        mov	al, 31

	; 13/05/2024
	; 08/05/2024
	;mov	[LVI], al ; 10/11/2023
	;call   setLastValidIndex

	;input AL = index # to stop on
setLastValidIndex:
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al

	mov	byte [tLoop], 1 ; 30/11/2016

	; 15/05/2024
	call	delay1_4ms

	; 12/05/2024	
	; 17/02/2017
        mov	dx, [NABMBAR]
        add	dx, PO_CR_REG			; PCM out Control Register
        mov	al, IOCE + RPBM	; Enable 'Interrupt On Completion' + run
				; (LVBI interrupt will not be enabled)
        out	dx, al				; Start bus master operation.

	; 15/05/2024
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms
	call	delay1_4ms

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k.  Likewise when it's playing buffer 2, refresh buffer 1 and repeat.
   
	; 18/02/2017
	; 14/02/2017
	; 13/02/2017
	; 08/12/2016
	; 28/11/2016

	push	es
	mov	ax, 0A000h
	mov	es, ax
	;mov	bp, DmaBuffer ; 14/02/2017
p_loop:
	mov     ah, 1			; any key pressed?
	;mov	ah, 11h
	int     16h			; no, Loop.
	jz	short q_loop

	mov	ah, 0
	;mov    ah, 10h			; flush key buffer...
	int     16h

	; 12/05/2024 (change PCM out volume)
	cmp	al, '+'
	jne	short p_1
	
	mov	al, [volume]
	cmp	al, 0
	jna	short q_loop
	dec	al
	jmp	short p_2
p_1:
	cmp	al, '-'
	jne	short p_return

	mov	al, [volume]
	cmp	al, 31
	jnb	short q_loop
	inc	al
p_2:
	mov	[volume], al
	mov	ah, al
	mov     dx, [NAMBAR]
  	;add    dx, CODEC_MASTER_VOL_REG
	add	dx, CODEC_PCM_OUT_REG
	out     dx, ax

	; 12/05/2024
	call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

	jmp	short q_loop

p_return:
	mov	byte [tLoop], 0	; 13/02/2017
	pop	es
	retn

q_loop:
	; 10/05/2024
	xor	ax, ax
	xchg	al, [tBuff] ; AL = [tBuff], [tBuff] = 0
	cmp	al, 1
	ja	short r_loop
	;jb	short ScopeLoop
	; 15/05/2024
	jb	short x_loop
	;
     	mov	si, [DMA_BUFFER1] ; [tBuff]=1 (from tuneLoop)
       	jmp	short s_loop 
r_loop:
	; 13/02/2017
        mov     si, [DMA_BUFFER2] ; [tBuff]=2 (from tuneLoop)
s_loop:
	; 14/02/2017
	;mov	bp, si ; save current buffer addres in bp register
	; 11/05/2024
	;mov	cx, BUFFERSIZE ; 2048*4 byte
	;push	ds ; segment
	;push	si ; offset
	;push	cx ; count
	call    GetSamples_ICH ; 18/02/2017

	; 15/05/2024
x_loop:
	call	ScopeLoop
	jmp	short p_loop

ScopeLoop:
	;mov    si, bp			; get current samples
	mov	si, MOD_BUFFER ; 18/02/2017
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
	mov     byte [es:di], 10 ; 0Ah	; and DRAW.
	add     dx, 2			; the next pixel...
	inc     cx
	cmp     cx, 320			; 320 pixels drawed?
	jb      short DrawLoop
	; 15/05/2024
	;jmp	p_loop
	retn

; 13/05/2024
%if 0

tuneLoop:
	; 11/05/2024
	; 11/11/2023
	; 10/11/2023
	; 18/02/2017
	; 08/12/2016
	; 28/11/2016 - Erdogan Tan

	; 11/05/2024
	cmp	byte [tLoop], 1
	jb	short tL3
	
	call    getCurrentIndex

	; 10/11/2023
	cmp	al, [LVI]
	jne	short tL1

	dec	ax
	and	al, 1Fh 
	call	setLastValidIndex
	xchg	al, [LVI]
tL1:
	test	al, BIT0
	jz	short tL2

	; 11/05/2024
	mov	byte [tBuff], 1
	retn
tL2:	
	mov	byte [tBuff], 2
tL3:
	retn

; returns AL = current index value
getCurrentIndex:
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_CIV_REG
	in	al, dx
	;pop	dx
	retn

;input AL = index # to stop on
setLastValidIndex:
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	;pop	dx
	retn

%endif

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

; Amiga Module Player v0.3b by Carlos Hasan.
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

GetSamples_ICH:
		; 11/05/2024
		; 18/02/2017
		; 8 bit mono samples 
		; must be converted to 16 bit, stereo samples (for ICH) !
		; (ICH AC97 Modification by Erdogan Tan)
		mov	[_si_], si ; DMA Buff Addr, [DMA_BUFFER1] or [DMA_BUFFER2]
		; 11/05/2024
		;mov	si, MOD_BUFFER
		;shr	cx, 2 ; mod buffer size = dma (half) buffer size / 4 (*)

;--------------------------------------------------------------------------
; GetSamples:  Returns the next chunk of samples to be played.
;  In:
;    Buffer  - Buffer Address.
;    Count   - Buffer Size.
;--------------------------------------------------------------------------

GetSamples:
		;ds:si = buffer address
		;cx = count

		; 11/05/2024
		;
		;;[sp+6] = ds
		;;[sp+4] = si
		;;[sp+2] = count
		;
		;Count	equ 4
		;Buffer	equ 6 
		;
		;push	bp
		;mov	bp, sp
		;
		;push    es
		;;;push  ds
		;;;pusha
		;
		;cld
		;
		;les	di, [bp+Buffer]
		;mov    bx, [bp+Count]

		; 11/05/2024
		; ds = cs
		push	es ; +

		push	ds
		pop	es

		; 11/05/2024
		mov	di, MOD_BUFFER
		mov	bx, (BUFFERSIZE/4) ; (*) ; 2048
NextChunk:
		cmp     word [BufLen], 0
		jne     short CopyChunk

		; 11/05/2024
		push	es
		push	bx
		push	di
MixChunk:       
		;lea    di, [MixBuffer]
		mov	di, MixBuffer
		mov     cx, [BpmSamples]
		mov     [BufPtr], di
		mov     [BufLen], cx

		; 12/05/2024
		; es = ds
		;mov    ax, ds
		;mov    es, ax

		mov    al, 80h
		rep    stosb

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

		pop	di
		pop	bx
		pop	es ; es = ds ; 12/05/2024
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
		jnz     short NextChunk

		;;popa
		;;pop	ds
		;pop	es
		;pop	bp
		;ret	6

; GetSamples_ICH_convert_samples:
		; 11/05/2024
		; es = ds = cs
		; 18/02/2017
		;mov	di, ds
		;mov	es, di
		mov	di, [_si_] ; DMA Buffer Address (16 bit, stereo)
		mov	si, MOD_BUFFER - 1 ; buffer for 8 bit mono samples
		mov	cx, (BUFFERSIZE/4) ; (*) ; 2048 ; 11/04/2024
		xor	al, al
gscs_loop:		
		inc	si
		mov	ah, [si] ; convert 8 bit sample to 16 bit sample
		; 11/05/2024
		sub	ah, 80h

		stosw	; left channel
		stosw	; right channel
		loop	gscs_loop

		; 11/05/2024
		pop	es ; +
		;pop	bp
		;ret	6
		retn

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
		retn	; 12/05/2024

;--------------------------------------------------------------------------
; StopPlaying: ShutDown the Sound System.
;--------------------------------------------------------------------------

StopPlaying:
	; 08/05/2024
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

	;mov	ax, [ac97_io_base]
	; 08/05/2024
	mov	ax, [NABMBAR]
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

	; 08/05/2024
ac97_stop:
	; 11/11/2023
	; 09/11/2023
	; 05/11/2023
	; 04/11/2023 
	; 28/05/2017 (TRDOS 386 v2, 'audio.s')
	;mov	byte [tLoop], 0 ; stop ! ; 05/11/2023
;_ac97_stop:

	; 11/11/2023
	mov	dx, [NAMBAR]
	;add	dx, 0 ; ac_reg_0 ; reset register
	out	dx, ax

	; 04/11/2023
	; 09/10/2017 (TRDOS 386 v2, 'audio.s')
	; 11/06/2017
	xor	al, al ; 0
	call	ac97_po_cmd

	; (Ref: KolibriOS, intelac97.asm, 'stop:')
	; Clear FIFOE, BCIS, LVBCI (Ref: Intel ICH hub manual)
	mov     ax, 1Ch
	mov     dx, [NABMBAR]
	add     dx, PO_SR_REG
	out     dx, ax

	; 11/06/2017
	mov     al, RR
ac97_po_cmd:
	; 11/06/2017
	; 29/05/2017
	mov     dx, [NABMBAR]
        add     dx, PO_CR_REG		; PCM out control register
	out	dx, al
	retn

;=============================================================================
;               preinitialized data
;=============================================================================

;=============================================================================
;               PLAY.ASM - DATA
;=============================================================================

msg_2017:
		db	'Tiny MOD Player by Erdogan Tan. May 2024.',10,13
		db	'usage: playmod2 filename.mod', 10, 13, '$'
		db	'18/02/2017',0
		db	'15/05/2024',0

Credits:	db	'Tiny MOD Player v0.1b by Carlos Hasan. July 1993.'
CRLF:		; 13/05/2024
		db	10,13,'$'
ErrorMesg:	db	'Error loading Module file.',10,13,'$'

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
;               PLAYWAV.ASM / PLAYER.ASM - DATA
;=============================================================================

; 24/11/2016
;	       IRQ  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
irq_int:	db 08h,09h,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh,70h,71h,72h,73h,74h,75h,76h,77h

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

;msgSampleRate	db "Sample Rate: "
;msgHertz	db "00000 Hz ", "$" 
;msg8Bits	db "8 bits ", "$" 
;msgMono	db "Mono", 0Dh, 0Ah, "$"
;msg16Bits	db "16 bits ", "$" 
;msgStereo	db "Stereo", 0Dh, 0Ah, "$"

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

; 17/02/2017
; NAMBAR:  Native Audio Mixer Base Address Register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 10h-13h
; NABMBAR: Native Audio Bus Mastering Base Address register
;    (ICH, Audio D31:F5, PCI Config Space) Address offset: 14h-17h
NAMBAR:		resw 1			; BAR for mixer
NABMBAR         resw 1			; BAR for bus master regs

tBuff:		resb	1
;irq_status:	resb 	1
pcm_irq_status:	resb	1 ; 08/05/2024

inside:		resb	1 
tLoop:		resb 	1

; 08/05/2024
IRQ_status:	resw 1	; IRQ status before enabling audio interrupt
IRQ_vector:	resd 1  ; Previous interrupt handler address	

; 256 byte buffer for descriptor list
BDL_BUFFER:	resw	1		; segment of our 256byte BDL buffer
DMA_BUFFER1:	resw 	1		; Pointer to 1st half of DMA Buffer
DMA_BUFFER2:	resw	1		; Pointer to 1st half of DMA Buffer

; 12/11/2016 - Erdogan Tan

ac97_int_ln_reg: resb 1 
err_num:	resb 1

bus_dev_fn:	resd 1
dev_vendor:	resd 1
; 08/05/2024
;stats_cmd:	resd 1
;ac97_io_base:	resw 1
LVI:		resb 1
; 12/05/2024
volume:		resb 1

;alignb 4
; 13/05/2024
alignb 16

BdlBuffer:	resb	BDL_SIZE ; 13/02/2017
DmaBuffer:	resb	2*BUFFERSIZE ; 13/02/2017

; MODLOAD.ASM
FileHandle:	resw	1
ErrorInfo:	resw	1
Header:		resb	ModHeader.size

sample_rate: ; PLAYER.ASM (22050Hz)
; MODPLAY.ASM
MixSpeed:	resw 1

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

; 18/02/2017
_si_:		resw 1
MOD_BUFFER:	resb BUFFERSIZE/4 ; 2048 ; 11/05/2024
EOF: