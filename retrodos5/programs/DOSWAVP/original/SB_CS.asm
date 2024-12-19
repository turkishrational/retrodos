;;; ***************************************************************************
;;; *** SB16 controling, writing, reading and handling                      ***
;;; *** procedures start here.                                              ***
;;; ***************************************************************************

;;; *** Base port ***
SB16_PORT_Base			equ	220h

;;; *** DSP_PORTS ***
SB16_PORT_DSP_Reset		equ	SB16_PORT_Base + 06h

SB16_PORT_DSP_ReadStat	equ	SB16_PORT_Base + 0Eh
SB16_PORT_DSP_Read		equ	SB16_PORT_Base + 0Ah

SB16_PORT_DSP_Write		equ	SB16_PORT_Base + 0Ch

SB16_PORT_DSP_IntAck	equ	SB16_PORT_Base + 0Fh

;;; *** DSP/DMA Channels ***
SB16_8BIT_CHANNEL		equ 01h
SB16_16BIT_CHANNEL		equ 05h


;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure resets the DSP (Digital Signal Processing)
;;; *** PARAMS:			None.
;;; *** REGS:			AX(RW), CX(RW), DX(W)
;;; *** VARS:			None.
;;;	*** CALLS:			None.
;;; ***	RETURNS:		CF==1: failed, CF==0: success, DSP is ready.

proc ResetDSP  
	push ax
	push cx
	push dx

	;;; Write '1' to the DSP_Reset port
	mov al, 01h
	mov dx, SB16_PORT_DSP_Reset
	out dx, al

	;;; Must wait at least 3 micro seconds
	mov cx, 128

ResetDSP_waitLoop:
	nop
	loop ResetDSP_waitLoop

	;;; Write '1' to the DSP_Reset port
	mov al, 00h
	mov dx, SB16_PORT_DSP_Reset
	out dx, al

	;;; Check if data is ready through the ReadStat port
	mov dx, SB16_PORT_DSP_ReadStat

ResetDSP_ReadStat:
	in  al, dx
	and al, 80h
	jnz ResetDSP_readData
	loop ResetDSP_ReadStat

	;;; Since the 7th bit is still off, an error occured
	jmp ResetDSP_Error

	;;; Continue until "0xAA" is received

ResetDSP_readData:
	mov  dx, SB16_PORT_DSP_Read
	in   al, dx
	cmp  al, 0AAh
	je  ResetDSP_Return

	;;; Turn CF on
ResetDSP_Error:
	stc

ResetDSP_Return:
	pop dx
	pop cx
	pop ax

	ret
endp ResetDSP   

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure writes to the DSP write port.
;;; *** PARAMS:			AL=Byte to write
;;; *** REGS:			AX(R)m DX
;;; *** VARS:			None.
;;;	*** CALLS:			None.
;;; ***	RETURNS:		AL is written to the DSP port.

proc WriteDSP  
	push dx
	push ax

	mov dx, SB16_PORT_DSP_Write

WriteDSP_ReadyLoop:
	in  al, dx
	and al, 80h
	jnz WriteDSP_ReadyLoop	;; If the 7th bit is set, try again

WriteDSP_WriteData:			;; Write the AL byte to the DSP write port
	pop ax
	out dx, al

WriteDSP_Return:
	pop dx
	ret
endp WriteDSP  

;;; ***************************************************************************
;;; *** DESCRIPTION:	Program the DMA controller
;;; *** PARAMS:			Bits per sample, Transfer Length, Buffer offset
;;; *** REGS:			AX, BX, DX, BP
;;; *** VARS:			LinearAddr
;;;	*** CALLS:			None.
;;; ***	RETURNS:		The DMA starts background transfers.

ProgramDMA_BPS			equ	[word ptr bp+8]
ProgramDMA_TransferLen	equ	[word ptr bp+6]
ProgramDMA_AddrOffset	equ	[word ptr bp+4]

ProgramDMA_LinearAddr	equ	[bp-2]
ProgramDMA_PageNumber	equ 0

proc ProgramDMA
	push bp
	mov bp, sp
	sub sp, 2

	push ax
	push bx
	push dx

	;; Calculate absolute linear address
	mov ax, ds
	shl ax, 04h
	add ax, ProgramDMA_AddrOffset
	mov ProgramDMA_LinearAddr, ax

	;; Check whether BPS=8/16 bits
	;; BPS==08: DMAC1
	;; BPS==16: DMAC2
	mov ax, ProgramDMA_BPS
	cmp ax, 16
	je ProgramDMA_DMAC2

ProgramDMA_DMAC1:
	;; Set channel mask bit (enable/disable the appropriate channels)
	xor ah, ah
	mov al, 05h
	mov dx, 0Ah
	out dx, al
	
	;; Clear the flip-flop
	xor al, al
	mov dx, 0Ch
	out dx, al

	;; Single Auto-init mode
	mov al, (58h + SB16_8BIT_CHANNEL)
	mov dx, 0Bh
	out dx, al

	;; Send the absolute address of the buffer
	mov ax, ProgramDMA_LinearAddr

	mov dx, 02h
	out dx, al	;; Lo. byte
	xchg al,ah
	out dx, al	;; Hi. byte

	;; Send the transfer length of the buffer
	mov ax, ProgramDMA_TransferLen
	dec ax
	mov dx, 03h
	out dx, al
	xchg al, ah
	out dx, al

	;; Send the page number (always 0)
	mov ax, ProgramDMA_PageNumber
	mov dx, 083h
	out dx, al

	;; Send the channel selection
	mov ax, 1
	mov dx, 0Ah
	out dx, al

	jmp ProgramDMA_Return

ProgramDMA_DMAC2:
;	;; Set the appropriate mask bit to disable the
;	;; DMA sound card channels
;	xor ah, ah
;	mov al, 05h
;	mov dx, 0D4h
;	out dx, al
;	
;	;; Clear the flip-flop
;	xor al, al
;	mov dx, 0D8h
;	out dx, al
;
;	;; Tell the DMAC to do all of the transfers in
;	;; Auto-Init mode
;	mov al, 01011001b
;	mov dx, 0D6h
;	out dx, al
;
;	;; Send the absolute address of the buffer
;	;; Because of the 16 bit mode, the absolute
;	;; address is passed by number of words
;	;; from the beggining of a 128kb page
;	;; so it must divided by two first.
;	mov ax, ProgramDMA_LinearAddr
;	shr ax, 1
;
;	mov dx, 0C4h
;	out dx, al	;; Lo. byte
;	xchg al, ah
;	out dx, al	;; Hi. byte
;
;	;; Send the transfer length of the buffer
;	mov ax, ProgramDMA_TransferLen
;	dec ax
;
;	;; Divide by two
;	shr ax, 1
;
;	mov dx, 0C6h
;	out dx, al
;	xchg al, ah
;	out dx, al
;
;	;; Send the page number (always 0)
;	mov ax, ProgramDMA_PageNumber
;	mov dx, 08Bh
;	out dx, al
;
;	;; Send the channel selection
;	;; (5 % 4 = 1)
;	mov ax, 01h
;	mov dx, 0D4h
;	out dx, al

ProgramDMA_Return:
	pop dx
	pop bx
	pop ax

	add sp, 2
	pop bp

	ret 6
endp ProgramDMA

;;; ***************************************************************************
;;; *** DESCRIPTION:	Program the DSP (Sound Processor)
;;; *** PARAMS:			Transfer Length, Bits per sample, Frequency, Mono/Stereo
;;; *** REGS:			AX, BP
;;; *** VARS:			None.
;;;	*** CALLS:			SetSampleRate, WriteDSP
;;; ***	RETURNS:		The DSP starts playing.

ProgramDSP_TransferLen	equ	[word ptr bp+10]
ProgramDSP_BPS			equ	[word ptr bp+8]
ProgramDSP_SampleRate	equ	[word ptr bp+6]
ProgramDSP_NChannels	equ	[word ptr bp+4]

proc ProgramDSP
	push bp
	mov bp, sp

	push ax

	;; Set the sample rate (DSP4)
	push ProgramDSP_SampleRate
	call SetSampleRate

	mov ax, ProgramDSP_BPS
	cmp ax, 16					;; Handle 16 bit
	je ProgramDSP_16Bit

ProgramDSP_8Bit:
	mov al, 0C6h
	call WriteDSP

	mov ax, ProgramDSP_NChannels
	cmp ax, 2
	je ProgramDSP_8Bit_Stereo
	
ProgramDSP_8Bit_Mono:
	mov al, 00h
	jmp ProgramDSP_SetMode

ProgramDSP_8Bit_Stereo:
	mov al, 20h
	jmp ProgramDSP_SetMode
	
ProgramDSP_16Bit:
	mov ax, 0C6h
	call WriteDSP

	mov ax, ProgramDSP_NChannels
	cmp ax, 2
	je ProgramDSP_16Bit_Stereo

ProgramDSP_16Bit_Mono:
	mov al, 10h
	jmp ProgramDSP_SetMode
	
ProgramDSP_16Bit_Stereo:
	mov al, 30h

ProgramDSP_SetMode:
	call WriteDSP

ProgramDSP_StartTransfer:
	mov ax, ProgramDSP_BPS
	cmp ax, 16
	je ProgramDSP_Tansfer16Bit

ProgramDSP_Tansfer8Bit:
	mov ax, ProgramDSP_TransferLen
	jmp ProgramDSP_Tansfer

ProgramDSP_Tansfer16Bit:
	mov ax, ProgramDSP_TransferLen
	shr ax, 1

ProgramDSP_Tansfer:
	call WriteDSP
	xchg al, ah
	call WriteDSP

ProgramDSP_Return:
	pop ax

	pop bp

	ret 8
endp ProgramDSP

;;; ***************************************************************************
;;; *** Sound card command procedures ***

;;; ***************************************************************************
;;; *** DESCRIPTION:	Because SB16 is used, the sample rate is set through
;;;						a port, and not using a time constant.
;;; *** PARAMS:			SAMPLE_RATE (WORD)
;;; *** REGS:			AX
;;; *** VARS:			None.
;;;	*** CALLS:			WriteDSP
;;; ***	RETURNS:		A new sample rate is set.

SetSampleRate_SAMPLE_RATE equ [bp+4]

proc SetSampleRate
	push bp
	mov bp, sp
	push ax

	;; Send "Set-Sample" command to DSP
	mov al, 041h
	call WriteDSP
	
	;; Send the actual sample, Hi.Byte first
	mov ax, SetSampleRate_SAMPLE_RATE
	xchg al, ah
	call WriteDSP
	xchg al, ah
	call WriteDSP

SetSampleRate_Return:
	pop ax
	pop bp

	ret 2
endp SetSampleRate

;;; ***************************************************************************
;;; *** DESCRIPTION:	Sends int-ack to DSP
;;; *** PARAMS:			BPS
;;; *** REGS:			AX, DX
;;; *** VARS:			None.
;;;	*** CALLS:			None.
;;; ***	RETURNS:		Interrupt acknowledgment is sent.

SendIntACK_BPS			 equ [bp+4]

proc SendIntACK
	push bp
	mov bp, sp

	push ax
	push dx

	mov ax, SendIntACK_BPS
	cmp ax, 16
	je SendIntACK_16Bit

SendIntACK_8Bit:
	mov dx, 22Eh
	jmp SendIntACK_Read

SendIntACK_16Bit:
	mov dx, 22Fh

SendIntACK_Read:
	in al, dx

SendIntACK_Return:
	pop dx
	pop ax

	pop bp

	ret 2
endp SendIntACK

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure stops to play the audio
;;; *** PARAMS:			BPS
;;; *** REGS:			AX
;;; *** VARS:			None.
;;;	*** CALLS:			WriteDSP.
;;; ***	RETURNS:		Audio stops playing

PauseAudio_BPS equ [bp+4]

proc PauseAudio
	push bp
	mov bp, sp

	push ax

	mov ax, PauseAudio_BPS
	cmp ax, 16
	je PauseAudio_16Bit

PauseAudio_8Bit:	
	mov al, 0D0h
	call WriteDSP
	jmp PauseAudio_Return

PauseAudio_16Bit:	
	mov al, 0D5h
	call WriteDSP

PauseAudio_Return:
	pop ax

	pop bp
	
	ret 2
endp PauseAudio

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure continues to play audio after it has been stopped
;;; *** PARAMS:			BPS
;;; *** REGS:			AX
;;; *** VARS:			None.
;;;	*** CALLS:			WriteDSP.
;;; ***	RETURNS:		Audio keeps on playing

ContAudio_BPS equ [bp+4]

proc ContAudio
	push bp
	mov bp, sp

	push ax

	mov ax, ContAudio_BPS
	cmp ax, 16
	je ContAudio_16Bit
	
ContAudio_8Bit:	
	mov al, 0D4h
	call WriteDSP
	jmp ContAudio_Return

ContAudio_16Bit:	
	mov al, 0D6h
	call WriteDSP

ContAudio_Return:
	pop ax

	pop bp

	ret 2
endp ContAudio

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure sets a new volume
;;; *** PARAMS:			New volume
;;; *** REGS:			AX, BX, DX
;;; *** VARS:			None.
;;;	*** CALLS:			None.
;;; ***	RETURNS:		A new volume is set in the DSP

SetMasterVol_Volume equ [word ptr bp+4]

proc SetMasterVol
	push bp
	mov bp, sp

	push ax 
	push bx
	push dx

	;; Tell the SB card which register to write
	mov al, 22h
	mov dx, 224h
	out dx, al

	;; Set the volume for both L and R
	mov ax, SetMasterVol_Volume
	and ax, 0000000000001111b
	mov bx, 11h
	mul bl
	
	;; Set new volume
	mov dx, 225h
	out dx, al

SetMasterVol_Return:
	pop dx
	pop bx
	pop ax

	pop bp

	ret 2
endp SetMasterVol


;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure exits Auto initalize mode
;;; *** PARAMS:			BPS.
;;; *** REGS:			AX.
;;; *** VARS:			None.
;;;	*** CALLS:			WriteDSP
;;; ***	RETURNS:		DMA stops, no more background transfers.


ExitAutoInit_BPS	equ	[bp+4]

proc ExitAutoInit 
	push bp
	mov bp, sp

	push ax 

	mov ax, ExitAutoInit_BPS
	cmp ax, 16
	je ExitAutoInit_16Bit
	
ExitAutoInit_8Bit:
	mov ax, 00DAh
	jmp ExitAutoInit_Write

ExitAutoInit_16Bit:
	mov ax, 00DAh

ExitAutoInit_Write:
	call WriteDSP

ExitAutoInit_Return:
	pop ax

	pop bp

	ret 2
endp ExitAutoInit

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure stops playing a file completly
;;; *** PARAMS:			None.
;;; *** REGS:			None.
;;; *** VARS:			WAVE_BitsPerSample
;;;	*** CALLS:			PauseAudio, ExitAutoInit
;;; ***	RETURNS:		Audio is stopped.

proc StopAudio
	push [word ptr WAVE_BitsPerSample]
	call PauseAudio

	push [word ptr WAVE_BitsPerSample]
	call ExitAutoInit

StopAudio_Return:
	ret
endp StopAudio
