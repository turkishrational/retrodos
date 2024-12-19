;;; ***************************************************************************
;;; *** Print strings and numbers to STDOUT                                 ***
;;; ***************************************************************************

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure prints a string by given string offset.
;;; *** PARAMS:			(WORD) stringPointer
;;; *** REGS:			AH(W), DX(W)
;;; *** VARS:			None.
;;;	*** CALLS:			DOS interrupt (0x21)
;;; ***	RETURNS:		String is printed on screen.

printString_stringPointer equ [bp+4]

proc PrintString
	push bp
	mov bp, sp

	push ax
	push dx

	mov ah, 09h
	mov dx, printString_stringPointer
	int 21h
	
PrintString_Return:
	pop dx
	pop	ax
	pop bp

	ret 2
endp PrintString

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure prints a number (word) in ASCII form.
;;; *** PARAMS:			(WORD) Number offset
;;; *** REGS:			AX(RW), BX(RW), BP(RW), SP(R)
;;; *** VARS:			None.
;;;	*** CALLS:			DOS interrupt (0x21)
;;; ***	RETURNS:		The number is printed on screen.

printNumber_Number equ [bp+6]
printNumber_Digits equ [bp+4]

proc printNumber
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, printNumber_Number
	mov bx, 10

	xor cx, cx
printNumber_CutNumber:
	inc cx
	xor dx, dx
	div bx
	push dx
	cmp cx, printNumber_Digits
	je printNumber_printloop
	jmp printNumber_CutNumber

printNumber_printloop:
	pop ax
	mov dl, '0'
	add dl, al
	mov ah, 2h
	int 21h
	loop printNumber_printloop

printNumber_return:
	pop dx
	pop cx
	pop bx
	pop	ax

	pop bp

	ret 4
endp printNumber

;;; ***************************************************************************
;;; *** DESCRIPTION:	Update volume value on template
;;; *** PARAMS:			None
;;; *** REGS:			AX(RW)
;;; *** VARS:			CurrentVolume
;;;	*** CALLS:			setCursorPosition, PrintNumber
;;; ***	RETURNS:		On-screen volume is updated.

proc UpdateVolume
	push ax

	;; Print Volume
	push 24
	push 75
	call setCursorPosition

	mov al, [byte ptr CurrentVolume]
	inc al

	mov bl, 100
	mul bl

	mov bl, 16
	div bl

	xor ah, ah
	push ax
	push 3
	call PrintNumber

UpdateVolume_Return:
	pop	ax

	ret
endp UpdateVolume

;;; ***************************************************************************
;;; *** DESCRIPTION:	Update file information on template
;;; *** PARAMS:			WAVE parameters and other variables from DS
;;; *** REGS:			AX(RW)
;;; *** VARS:			CurrentFileName, WAVE_SampleRate, 
;;;	*** CALLS:			setCursorPosition, PrintString, PrintNumber, UpdateVolume
;;; ***	RETURNS:		On-screen file info is updated.

proc UpdateFileInfo
	push ax
	;; Print File Name
	push 9
	push 23
	call setCursorPosition
	push offset CurrentFileName
	call PrintString
	
	;; Print Frequency
	push 10
	push 23
	call setCursorPosition
	push [word ptr WAVE_SampleRate]
	push 5
	call PrintNumber

	;; Print BitRate
	push 9
	push 57
	call setCursorPosition
	push [word ptr WAVE_BitsPerSample]
	push 2
	call PrintNumber

	;; Print Channel Number
	push 10
	push 57
	call setCursorPosition
	push [word ptr WAVE_NumChannels]
	push 1
	call PrintNumber

	call UpdateVolume

UpdateFileInfo_Return:
	pop	ax

	ret
endp UpdateFileInfo

;;; ***************************************************************************
;;; *** DESCRIPTION:	All time related calculations are here
;;; ***************************************************************************

;;;***************************************************************************
;;; NAME:         	SetTotalTime
;;; DESCRIPTION:    Caclulates the total time in seconds in file
;;; INPUT:          DATA_SubchunkSize, WAVE_SampleRate, WAVE_BlockAlign
;;; OUTPUT:        	CurrentTotalTime=Total time in seconds in file, Output on the
;;; 				screen of the total time in seconds

proc SetTotalTime
	push ax
	push bx
	push dx

	;; Calculate total seconds in file
	mov ax, [word ptr DATA_SubchunkSize]
	mov dx, [word ptr DATA_SubchunkSize + 2]
	mov bx, [word ptr WAVE_SampleRate]
	div bx
	xor dx, dx

	;push ax

	;mov ax, [word ptr WAVE_BitsPerSample]
	;mov bx, 8
	;div bx
	;xor dx, dx
    ;
	;mov bx, [word ptr WAVE_NumChannels]
	;mul bx
	;xor dx, dx
	;
	;mov bx, ax
    ;
	;pop ax
	
	mov bx, [word ptr WAVE_BlockAlign]

	div bx

	mov [word ptr CurrentTotalTime], ax

	mov bl, 60
	div bl

	push 24
	push 42
	call setCursorPosition

	mov bh, ah
	xor ah, ah
	push ax
	push 2
	call PrintNumber
	
	push 24
	push 45
	call setCursorPosition

	mov al, bh
	xor ah, ah
	push ax
	push 2
	call PrintNumber

SetTotalTime_Return:
	pop dx
	pop bx
	pop	ax
	ret
endp SetTotalTime


;;; ***************************************************************************
;;; *** DESCRIPTION:	Updates the time and the on-screen timer
;;; *** PARAMS:			None.
;;; *** REGS:			AX, BX, DX
;;; *** VARS:			TotalTransfers, WAVE_BlockAlign
;;;	*** CALLS:			setCursorPosition, printNubmer
;;; ***	RETURNS:		

proc UpdateTime
	push ax
	push bx
	push dx
	
	;; ax = (total_transfers) / (bytesinsample) = number of seconds
	mov ax, [word ptr TotalTransfers]
	mov bx, [word ptr WAVE_BlockAlign]
	xor dx, dx
	div bx

	;; because of double-buffering
	mov bx, 2
	xor dx, dx
	div bx

	;; al = minutes, ah = seconds
	mov bl, 60
	div bl
	
	;; print minutes
	push 24
	push 33
	call setCursorPosition

	push ax
	xor ah, ah
	push ax
	push 2
	call PrintNumber

	;; print seconds
	push 24
	push 36
	call setCursorPosition

	pop ax
	mov al, ah
	xor ah, ah
	push ax
	push 2
	call PrintNumber

UpdateTime_Return:
	pop dx
	pop bx
	pop	ax

	ret
endp UpdateTime

;;; ***************************************************************************
;;; *** DESCRIPTION:	Updates the progress bar on-screen
;;; *** PARAMS:			None.
;;; *** REGS:			AX, BX, CX, DX
;;; *** VARS:			DATA_SubchunkSize, TotalTransfers, SubBufferLength
;;;	*** CALLS:			setCursorPosition, DOS Interrupt(21h)
;;; ***	RETURNS:		TransferLength = new transfer length

;; Which row to draw the progress bar on
PROGRESSBAR_ROW			equ	23

proc UpdateProgressBar
	push ax
	push cx
	push bx
	push dx

	;; AX = Total seconds played
	xor dx, dx
	mov ax, [word ptr TotalTransfers]

	mov bx, [word ptr WAVE_BlockAlign]
	div bx

	;; AX = Total seconds played * 80
	mov bx, 80
	mul bx

	;; AX = Total seconds in file
	mov bx, [word ptr CurrentTotalTime]
	div bx

	;; Divide by two because of double-buffer method
	xor dx, dx
	mov bx, 2
	div bx

	;; Push for the 'Clean' part
	push ax

	;; Set cursor position
	push PROGRESSBAR_ROW
	push 0
	call setCursorPosition

	;; In order to avoid mistakes
	;; IF AX==0: Jump to clean loop
	or ax, ax
	jz UpdateProgressBar_Clean

UpdateProgressBar_DrawProgress:
	mov cx, ax
	dec cx
	mov ah, 09h
	mov al, 223
	mov bh, 0
	mov bl, 0Fh
	int 10h

UpdateProgressBar_DrawCursor:
	mov ax, cx
	push PROGRESSBAR_ROW
	push ax
	call setCursorPosition

	mov ah, 09h
	mov al, 223
	mov bh, 0
	mov bl, 0Ch
	mov cx, 01h
	int 10h

UpdateProgressBar_Clean:
	pop ax
	push PROGRESSBAR_ROW
	push ax
	call setCursorPosition

	;; CX = No. of times to print a clean character
	mov cx, 80
	sub cx, ax
	;; 09h = Write character multiple times
	mov ah, 09h
	;; 32 = Space ASCII code
	mov al, 32
	mov bx, 0000h
	int 10h

UpdateProgressBar_Return:
	pop dx
	pop cx
	pop bx
	pop	ax

	ret
endp UpdateProgressBar

;;; ***************************************************************************
;;; *** DESCRIPTION:	Updates the sound visualization on screen
;;; *** PARAMS:			None.
;;; *** REGS:			AX, BX, CX, DX
;;; *** VARS:			Visualization_SoundWave, Visualization_CurrentLoc.
;;;	*** CALLS:			setCursorPosition, DOS Interrupt(21h)
;;; ***	RETURNS:		On-screen visualization characers are printed

UpdateSoundVisualization_CurrentColumn		equ		[byte ptr bp-1]

proc UpdateSoundVisualization
	push bp
	mov bp, sp
	dec sp

	push ax
	push cx
	push bx
	push dx

UpdateSoundVisualization_Clear:
	push 12
	push 0
	call setCursorPosition
	mov ah, 09h
	mov al, 219
	mov bl, 00h
	;; 10 ROWS * # Of columns
	mov cx, 800
	int 10h

	lea si, [byte ptr Visualization_SoundWave]

	xor ah, ah
	mov al, [byte ptr Visualization_CurrentLoc]

	add si, ax

	mov cx, 40	
UpdateSoundVisualization_ColumnsLoop:
	push cx
	;; UpdateSoundVisualization_CurrentColumn=
	mov UpdateSoundVisualization_CurrentColumn, cl
	mov cl, 40
	sub cl, UpdateSoundVisualization_CurrentColumn
	mov UpdateSoundVisualization_CurrentColumn, cl

	;; CX=N. of rows to print
	xor ch, ch
	mov cl, [byte ptr si]
UpdateSoundVisualization_RowsLoop:
	push cx

	mov ax, 22
	sub ax, cx
	push ax

	xor ah, ah
	mov ax, 2
	mov bl, UpdateSoundVisualization_CurrentColumn
	mul bl

	mov bl, [byte ptr Visualization_CurrentLoc]
	inc ax

UpdateSoundVisualization_RowsLoop_PushRow:
	push ax

	call setCursorPosition

	mov ah, 09h
	mov al, 220
	mov cx, 1

UpdateSoundVisualization_RowsLoop_SetColor:
	;; Switch the colors of the columns for consistancy
	mov bl, [byte ptr Visualization_CurrentLoc]
	test bl, 00000001b
	jz UpdateSoundVisualization_RowsLoop_SetColor_Second

	mov bl, UpdateSoundVisualization_CurrentColumn
	test bl, 00000001b
	jz UpdateSoundVisualization_RowsLoop_Blue
	jmp UpdateSoundVisualization_RowsLoop_Red

UpdateSoundVisualization_RowsLoop_SetColor_Second:
	mov bl, UpdateSoundVisualization_CurrentColumn
	test bl, 00000001b
	jz UpdateSoundVisualization_RowsLoop_Red

	
	;; Set to BLUE
UpdateSoundVisualization_RowsLoop_Blue:
	mov bx, 0009h
	jmp UpdateSoundVisualization_RowsLoop_Print

	;; Set to RED
UpdateSoundVisualization_RowsLoop_Red:
	mov bx, 0004h

	;; Print
UpdateSoundVisualization_RowsLoop_Print:
	int 10h
	
	pop cx
	loop UpdateSoundVisualization_RowsLoop

	pop cx

	inc si
	cmp si, (offset Visualization_SoundWave + 40)
	jne UpdateSoundVisualization_RowsLoop_End

	lea si, [byte ptr Visualization_SoundWave]


UpdateSoundVisualization_RowsLoop_End:
	loop UpdateSoundVisualization_ColumnsLoop

	inc [byte ptr Visualization_CurrentLoc]
	mov al, [byte ptr Visualization_CurrentLoc]
	cmp al, 40
	jl	UpdateSoundVisualization_Return

	sub al, 40
	mov [byte ptr Visualization_CurrentLoc], al

UpdateSoundVisualization_Return:
	pop dx
	pop cx
	pop bx
	pop	ax

	inc sp
	pop bp

	ret
endp UpdateSoundVisualization
