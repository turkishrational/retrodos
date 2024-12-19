;;; ***************************************************************************
;;; *** Graphic/Text modes related proc s.                                  ***
;;; ***************************************************************************


;;; ***************************************************************************
;;; *** DESCRIPTION:	The proc starts "Text Mode" and sets cursor to 0,0
;;; *** PARAMS:			None.
;;; *** REGS:			AX(W)
;;; *** VARS:			None.
;;;	*** CALLS:			BIOS Interrupt(0d16 / 0x10)
;;; ***	RETURNS:		A new cursor position is set and the video mode is set to Text.

proc initTextMode
	push ax
	push bx
	push cx

	;; Start text mode
	mov ax, 002h
	int 10h

	;; Set the cursor to invisible
	mov ah, 01h
	mov cx, 2607h
	int 10h

	;; Set cursor to pos 0,0
	push 0
	push 0
	call setCursorPosition

initTextMode_Return:
	pop cx
	pop bx
	pop ax
	ret
endp initTextMode


;;; ***************************************************************************
;;; *** DESCRIPTION:	The proc sets a new cursor position on screen
;;; *** PARAMS:			Row, Column.
;;; *** REGS:			AX, BX, DX
;;; *** VARS:			None.
;;;	*** CALLS:			BIOS Interrupt(0d16 / 0x10)
;;; ***	RETURNS:		A new cursor position is set.

setCursorPosition_Row		equ	[bp+6]
setCursorPosition_Column	equ [bp+4]

proc setCursorPosition
	push bp
	mov bp, sp
	push ax
	push bx
	push dx

	mov ax, 0500h
	int 10h
	
	mov ah, 02h
	mov bh, 00h
	mov dh, setCursorPosition_Row
	mov dl, setCursorPosition_Column
	int 10h

setCursorPosition_Return:
	pop dx
	pop bx
	pop ax
	pop bp
	ret 4
endp setCursorPosition

;;; ***************************************************************************

;;; *** DESCRIPTION:	The proc clears the screen
;;; *** PARAMS:			None.
;;; *** REGS:			AX(W)
;;; *** VARS:			None.
;;;	*** CALLS:			BIOS Interrupt(0d16 / 0x10)
;;; ***	RETURNS:		Screen is cleared.

proc ClearScreen
	push ax

	mov ax, 0002h
	int 10h
	
ClearScreen_Return:
	pop ax

	ret
endp ClearScreen
