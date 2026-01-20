; ****************************************************************************
; tinyplay.asm (for MSDOS)
; ----------------------------------------------------------------------------
; TINYPLAY.COM ! MOD PLAYER & VGA DEMO program ! NASM version by Erdogan TAN
;
; 12/02/2017
;
; [ Last Modification: 04/03/2017 ]
;
; Derived from source code of 'PLAY.EXE' (TINYPLAY) by Carlos Hasan (1993)
;          PLAY.EXE: PLAY.ASM, MODLOAD.ASM, MODPLAY.ASM, SB.ASM
;
; Assembler: NASM 2.11
; ----------------------------------------------------------------------------
;	   nasm  tinyplay.asm -l tinyplay.lst -o TINYPLAY.COM	
; ****************************************************************************

; Tiny MOD Player v0.1b by Carlos Hasan.
;		July 14th, 1993.

;=============================================================================
;               PLAY.ASM
;=============================================================================

[BITS 16]
[org 100h]

Start:
GetFileName:    				; Parse  the Command line...
		mov	si, 80h
		mov	bl, [si]
		;xor	bh, bh
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

		call    DetectSb		; Detect the SB Addr, Irq.
LoadMod:  
		; es:di = Filename address
		push	es
		push	di
		call    LoadModule		; Load the MODule...

		cmp     word [ErrorInfo], 0	; any error loading?
		je      short PlayNow

		mov     ax, 0900h		; yes, print error and Exit.
		;lea    dx, [ErrorMesg]
		mov	dx, ErrorMesg
		int     21h
		jmp     short Exit

PlayNow:        
		mov     word [MixSpeed], 22000	; Mixing at 22 kHz

;		call    StartPlaying            ; PLAY!

		call    SbInit

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

		mov     ax, 0A000h 		; ES points to VideoMem
		mov     es, ax			; segment area.

		cld

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
ScopeLoop:
		;int	1Ch			; ModPlay Polling!
		call    SbPoll

		mov     si, [DmaBuffer]		; get current samples
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
CheckAnyKey:
		mov     ah, 1			; any key pressed?
		int     16h			; no, Loop.
		jz	short ScopeLoop

		mov     ah, 0			; flush key buffer...
		int     16h

		;in	al, 21h			; enable irq 0!
		;and	al, 11111110b
		;out	21h, al

		mov     ax, 0003h		; Set Text Mode 80x25x16
		int     10h

		;call	StopPlaying		; STOP!
		call	SbDone
Exit:           
		call    FreeModule              ; Free MODule core.

		mov     ax, 4C00h		; Bye!
		int     21h

pmsg_2017:
		mov     ax, 0900h		; Prints the Credits Text.
		;lea    dx, [msg_2017]
		mov	dx, msg_2017
		int     21h
		jmp	short Exit

DetectSb:
		pusha
		push    es
ScanPort:
		mov     bx, 210h		; start scanning ports
						; 210h, 220h, .. 260h
ResetDSP:       
		mov     dx, bx			; try to reset the DSP.
		add     dx, 06h
		mov     al, 1
		out     dx, al
		in      al, dx
		in      al, dx
		in      al, dx
		in      al, dx
		xor     al, al
		out     dx, al
		add     dx, 08h
		mov     cx, 100
WaitID:
		in      al, dx
		or      al, al
		js      short GetID
		loop    WaitID
		jmp     short NextPort
GetID:          
		sub     dx, 04h
		in      al, dx
		cmp     al, 0AAh
		je      short Found
		add     dx, 04h
		loop    WaitID
NextPort:
		add     bx, 10h			; if not response,
		cmp     bx, 260h		; try the next port.
		jbe     short ResetDSP
		jmp     Fail
Found:
		mov     [SbAddr], bx		; SB Port Address Found!
ScanIRQ:
		cli

		in      al, 21h			; save the IMR.
		mov     bl, al

		mov     al, 11111111b		; disable all the IRQs.
		out     21h, al

		xor     ax, ax			; trap the IRQs 2,3,5,7.
		mov     es, ax
SaveIrqs:
		mov     ax, [es:28h]		; irq2
		mov     dx, [es:2Ah]
		push    ax
		push    dx

		mov     ax, [es:2Ch]		; irq3
		mov     dx, [es:2Eh]
		push    ax
		push    dx

		mov     ax, [es:34h]		; irq5
		mov     dx, [es:36h]
		push    ax
		push    dx

		mov     ax, [es:3Ch]		; irq7
		mov     dx, [es:3Eh]
		push    ax
		push    dx
SetIrqs:        
		mov     ax, TrapIrq2		; irq2
		mov     [es:28h], ax
		mov     [es:2Ah], cs

		mov     ax, TrapIrq3		; irq3
		mov     [es:2Ch], ax
		mov     [es:2Eh], cs

		mov     ax, TrapIrq5		; irq5
		mov     [es:34h], ax
		mov     [es:36h], cs

		mov     ax, TrapIrq7		; irq7
		mov     [es:3Ch], ax
		mov     [es:3Eh], cs

EnableIrqs:     mov     al, bl			; enable IRQs 2,3,5,7.
		and     al, 01010011b
		out     21h, al

		sti

		mov     byte [SbIrq], 0		; clear the IRQ level.

		mov     dx, [SbAddr]		; tells to the SB to
		add     dx, 0Ch			; generate a IRQ!
WaitSb:         in      al, dx
		or      al, al
		js      short WaitSb
		mov     al, 0F2h
		out     dx, al

		xor     cx, cx			; wait until IRQ level
WaitIRQ:        
		cmp     word [SbIrq], 0		; is changed or timeout.
		jne     short IrqOk
		loop    WaitIRQ
IrqOk:
		mov     al, bl			; restore IMR.
		out     21h, al
RestoreIrqs:    
		cli				; restore IRQ vectors.

		xor     ax, ax
		mov     es, ax

		pop     dx		      ; irq7
		pop     ax
		mov     [es:3Ch], ax
		mov     [es:3Eh], dx

		pop     dx		      ; irq5
		pop     ax
		mov     [es:34h], ax
		mov     [es:36h], dx

		pop     dx		      ; irq3
		pop     ax
		mov     [es:2Ch], ax
		mov     [es:2Eh], dx

		pop     dx		      ; irq2
		pop     ax
		mov     [es:28h], ax
		mov     [es:2Ah], dx

		;cli

		cmp     word [SbIrq], 0		; IRQ level was changed?
		je      short Fail		; no, fail.
Success:        
		mov     dx, [SbAddr]		; Print Sucessful message.
		mov     cx, [SbIrq]
		shr     dl, 4
		add     dl, '0'
		mov     [PortText], dl
		add     cl, '0'
		mov     [IrqText], cl
		mov     ah, 9
		mov     dx, MsgFound
		int     21h
		pop     es
		popa				; Return to caller.
		retn

Fail:           mov     ah, 9			; Print Failed Message,
		mov     dx, MsgNotFound		; and exit to DOS.
		int     21h
		mov     ax, 4C00h
		int     21h

here:
		jmp	short here

TrapIrq2:	
		push	ax
		mov	ax, 2
		jmp     TrapIrq
TrapIrq3:
		push	ax
		mov	ax, 3
		jmp     TrapIrq
TrapIrq5:
		push	ax
		mov	ax, 5
		jmp     TrapIrq
TrapIrq7:
		push	ax
		mov	ax, 7
TrapIrq:
		push	dx			; General IRQ trapper				
		push    ds			; used for IRQ autodetect.
		mov     dx, cs
		mov     ds, dx
		mov     [SbIrq], ax		; save IRQ level.
		mov     dx, [SbAddr]
		add     dx, 0Eh
		in      al, dx			; SB acknowledge.
		mov     al, 20h
		out     20h, al			; Hardware acknowledge.
		pop     ds
		pop     dx
		pop     ax
		iret				; bye!


;=============================================================================
;               SB.ASM
;=============================================================================

;  Sound Blaster DMA Driver v0.1b by Carlos Hasan.
;		July 24th, 1993.

; EQUATES

DmaBufSize 	equ	2048

; MACROS

%macro	SbOut	1
%%Wait:
	in	al, dx
	or	al, al
	js	short %%Wait
	mov	al, %1
	out	dx, al
%endmacro

; CODE

;--------------------------------------------------------------------------
; SbIrqHandler:  Sound Blaster IRQ handler.
;--------------------------------------------------------------------------

SbIrqHandler:
		push    ax
		push    dx
		push    ds

		mov     ax, cs
		mov     ds, ax

		mov     dx, [SbAddr]
		add     dx, 0Eh
		in      al, dx

		sub     dx, 02h
		SbOut	14h
		SbOut	0FFh
		SbOut	0FFh

		mov     al, 20h
		out     20h, al

		pop     ds
		pop     dx
		pop     ax
		iret

;--------------------------------------------------------------------------
; SbPoll:  Sound Blaster Polling.
;--------------------------------------------------------------------------

SbPoll:
		pusha
		push    ds
		push    es

		mov     ax, cs
		mov     ds, ax
GetDmaCount:
		in      al, 03h
		mov     cl, al
		in      al, 03h
		mov     ch, al

		mov     ax, [DmaFlag]
		test    ax, ax
		jnz     short SecondHalf
FirstHalf:
		cmp     cx, DmaBufSize/2
		jae     short Bye
		mov     si, [DmaBuffer]
		mov     cx, DmaBufSize/2
		push	ds ; segment
		push	si ; offset
		push	cx ; count
		call    GetSamples
		inc     word [DmaFlag]
		jmp     short Bye
SecondHalf:     
		cmp     cx, DmaBufSize/2
		jb      short Bye
		mov     si, [DmaBuffer]
		mov     cx, DmaBufSize/2
		add     si, cx
		push	ds
		push	si
		push	cx
		call    GetSamples
		dec     word [DmaFlag]
Bye:
		pop     es
		pop     ds
		popa
		retn     ;<==== IRET

;--------------------------------------------------------------------------
; SbInit: Initializes the Sound Blaster Driver.
;--------------------------------------------------------------------------

SbInit:
		pusha
		push    ds
		push    es

		call    StartPlaying

		cli

		in      al, 21h
		push    ax

		mov     al, 11111111b
		out     21h, al
SetBuffer:
		mov     word [DmaFlag], 0
		mov     ax, DoubleBuffer
		mov     [DmaBuffer], ax
		mov     dx, ds
		mov     bx, dx
		shr     dh, 4
		shl     bx, 4
		add     ax, bx
		adc     dh, 0
		mov     cx, ax
		neg     cx
		cmp     cx, DmaBufSize
		jae     short SetDma
		add     [DmaBuffer], cx
		add     ax, cx
		adc     dh, 0
SetDma:
		mov     bx, ax
		mov     cx, DmaBufSize
		dec     cx

		mov     al, 05h
		out     0Ah, al

		xor     al, al
		out     0Ch, al

		mov     al, bl
		out     02h, al
		mov     al, bh
		out     02h, al
		mov     al, dh
		out     83h, al

		mov     al, cl
		out     03h, al
		mov     al, ch
		out     03h, al

		mov     al, 59h
		out     0Bh, al

		mov     al, 01h
		out     0Ah, al
ClearBuffer:
		mov     di, [DmaBuffer]
		mov     cx, DmaBufSize
		mov     ax, ds
		mov     es, ax
		mov     al, 80h
		cld
		rep     stosb
SetIrq:
		xor     ax, ax
		mov     es, ax

		mov     bx, [SbIrq]
		add     bx, 08h
		shl     bx, 2

		mov     ax, [es:bx]
		mov     dx, [es:bx+2]
		mov     [DmaHandler], ax
		mov     [DmaHandler+2], dx

		mov     ax, SbIrqHandler
		mov     [es:bx], ax
		mov     [es:bx+2], cs
SetTimer:
		mov     ax, [es:70h]
		mov     dx, [es:72h]
		mov     [TimerHandler], ax
		mov     [TimerHandler+2], dx

		;;;
		;mov     ax, SbPoll
		;mov     [es:70h], ax
		;mov     [es:72h], cs
		;;;
ResetDsp:
		mov     dx, [SbAddr]
		add     dx, 06h
		mov     al, 1
		out     dx, al
		in      al, dx
		in      al, dx
		in      al, dx
		in      al, dx
		xor     al, al
		out     dx, al
		mov     cx, 100
WaitId:         
		mov     dx, [SbAddr]
		add     dx, 0Eh
		in      al, dx
		or      al, al
		js      short sb_GetId
		loop    WaitId
		jmp     short sb_Exit
sb_GetId:          
		mov     dx, [SbAddr]
		add     dx, 0Ah
		in      al, dx
		cmp     al, 0AAh
		je      short SbOk
		loop    WaitId
		jmp     short sb_Exit
SbOk:
		mov     dx, [SbAddr]
		add     dx, 0Ch
		SbOut   0D1h
		mov     ax, 1000
		mul     ax
		div     word [MixSpeed]
		neg     al
		mov     ah, al
		mov     dx, [SbAddr]
		add     dx, 0Ch
		SbOut   40h
		SbOut   ah
StartDma:       
		SbOut   14h
		SbOut   0FFh
		SbOut   0FFh
sb_Exit:           
		pop     ax
		mov     cx, [SbIrq]
		mov     ah, 1
		shl     ah, cl
		not     ah
		and     al, ah
		out     21h, al

		sti

		pop     es
		pop     ds
		popa
		retn

;--------------------------------------------------------------------------
; SbDone:  Shut Down the Sound Blaster Driver.
;--------------------------------------------------------------------------

SbDone:
		pusha
		push    ds
		push    es

		cli

		in      al, 21h
		push    ax

		mov     al, 11111111b
		out     21h, al

		xor     ax, ax
		mov     es, ax

		mov     bx, [SbIrq]
		add     bx, 08h
		shl     bx, 2

		mov     ax, [DmaHandler]
		mov     dx, [DmaHandler+2]
		mov     [es:bx], ax
		mov     [es:bx+2], dx

		mov     ax, [TimerHandler]
		mov     dx, [TimerHandler+2]
		mov     [es:70h], ax
		mov     [es:72h], dx

		mov     dx, [SbAddr]
		add     dx, 0Ch
		SbOut   0D0h
		SbOut   0D3h

		pop     ax
		mov     cx, [SbIrq]
		mov     ah, 1
		shl     ah, cl
		or      al, ah
		out     21h, al

		sti

		call    StopPlaying

		pop     es
		pop     ds
		popa
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
;               preinitialized data
;=============================================================================

;=============================================================================
;               SB.ASM - DATA
;=============================================================================

SbAddr:		dw      220h
SbIrq:		dw      7

;=============================================================================
;               PLAY.ASM - DATA
;=============================================================================

msg_2017:
		db	'Tiny MOD Player by Erdogan Tan. March 2017.',10,13
		db	'usage: tinyplay filename.mod', 10, 13, '$'
		db	'04/03/2017'

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

bss_start:

ABSOLUTE bss_start

alignb 2

;=============================================================================
;        	uninitialized data
;=============================================================================

; SB.ASM
DmaFlag:	resw 1
DmaBuffer:	resw 1
DmaHandler:	resd 1
TimerHandler:	resd 1

DoubleBuffer:	resb DmaBufSize ; 04/03/2017

; MODLOAD.ASM
FileHandle:	resw	1
ErrorInfo:	resw	1
Header:		resb	ModHeader.size

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