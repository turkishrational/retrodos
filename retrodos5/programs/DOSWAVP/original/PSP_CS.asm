;;; ***************************************************************************
;;; *** PSP related proc s                                                  ***
;;; ***************************************************************************

;;; Offset location of the first letter
PSP_StartOffset	equ	82h

;;; ***************************************************************************
;;; *** DESCRIPTION:	The proc initiates the PSP reading proc (getting the 
;;;	                    PSP segment, first command line argument, etc)
;;; *** PARAMS:			None.
;;; *** REGS:			AX(W), BX(RW), DS
;;; *** VARS:			PSP_Segment
;;;	*** CALLS:			DOS Interrupt
;;; ***	RETURNS:		CF==1: No command line arguments, CF==0: At least
;;;						one command line argument

proc InitPSP
	push ax
	push bx
	
	push ds

	;; Clear CF
	clc

	;; Set correct offset
	mov [word ptr PSP_CurrentOffset], PSP_StartOffset

	;; Get current PSP segment, write it to variable
	mov ah, 62h
	int 21h	
	
	mov [word ptr PSP_Segment], bx

	mov ds, bx
	mov bx, 81h
	cmp [byte ptr bx], 0Dh		;; [81h]=="CR": No command line parameters
	jnz InitPSP_Return
	
	;; Set carry flag
	stc
	
InitPSP_Return:
	pop ds

	pop bx
	pop ax

	ret 
endp InitPSP

;;; ***************************************************************************
;;; *** DESCRIPTION:	The proc reads the next command line argument
;;; *** PARAMS:			(W) PSP_Segment
;;; *** REGS:			AX, CX, DS
;;; *** VARS:			CurrentFileName (W)
;;;	*** CALLS:			not-set
;;; ***	RETURNS:		CF==1: Error, end of arguments CF==0: Success (copied to
;;;						CurrentFileName)

PSPParseNext_PSPSegment		equ	[word ptr bp+6]
PSPParseNext_CurrentOffset	equ	[word ptr bp+4]

proc PSPParseNext
	push bp
	mov bp, sp
	
	push ax
	push bx
	push cx
	
	push si
	push di

	push ds
	push es
	
	;; Set current file name to 0's
	push ds
	pop es

	xor al, al
	mov di, offset CurrentFileName
	mov cx, 15
	rep stosb

	mov di, offset CurrentFileName

	;; Set the correct PSP segment and offset
	push PSPParseNext_PSPSegment
	pop es
	mov si, PSPParseNext_CurrentOffset

	;; Check for end of arguments
	mov al, [byte ptr es:si]
	cmp al, 0Dh
	je PSPParseNext_Error

	xor cx, cx
PSPParseNext_CopyLoop:
	mov al, [byte ptr es:si]

	cmp cx, 15
	jae PSPParseNext_Error

	cmp al, 20h
	je PSPParseNext_CopyDone_incSI
	cmp al, 0Dh
	je PSPParseNext_CopyDone
	
	mov [byte ptr ds:di], al
	
	inc si
	inc di
	inc cx
	jmp PSPParseNext_CopyLoop
	
PSPParseNext_CopyDone_incSI:
	inc si
PSPParseNext_CopyDone:
	mov [word ptr PSP_CurrentOffset], si
	jmp PSPParseNext_Return

	;; No more arguments, error, turn on CF
PSPParseNext_Error:
	stc

PSPParseNext_Return:
	pop es
	pop ds

	pop di
	pop si

	pop cx
	pop bx
	pop ax

	pop bp
	ret 4
endp PSPParseNext

;;; ***************************************************************************
;;; *** DESCRIPTION:	The proc reads the previous command line argument
;;; *** PARAMS:			(W) PSP_Segment, (W) CurrentOffset
;;; *** REGS:			AX, BX, CX, SI, DI, DS, ES
;;; *** VARS:			CurrentFileName (W)
;;;	*** CALLS:			None.
;;; ***	RETURNS:		CF==1: Error, end of arguments CF==0: Success (copied to
;;;						CurrentFileName)

PSPParsePrev_PSPSegment		equ	[bp+6]
PSPParsePrev_CurrentOffset	equ	[bp+4]

proc PSPParsePrev
	push bp
	mov bp, sp
	
	push ax
	push bx
	push cx
	
	push si
	push di

	push ds
	push es
	
	push ds
	pop es

	;; Set current file name to 0's
	xor al, al
	mov di, offset CurrentFileName
	mov cx, 15
	rep stosb

	mov di, offset CurrentFileName

	;; Set the correct PSP segment and offset
	mov ax, PSPParsePrev_PSPSegment
	mov es, ax
	mov si, PSPParsePrev_CurrentOffset

	;; Check for start of arguments (not likely to happen, at all)
	cmp si, 81h
	je PSPParsePrev_Copy

	;; Search for previous space character
	dec si
	mov cx, 2
PSPParsePrev_Search:
	dec si
	mov al, [byte ptr es:si]
	cmp al, 20h
	jne PSPParsePrev_Search

	cmp si, 81h
	je PSPParsePrev_Copy

	loop PSPParsePrev_Search


PSPParsePrev_Copy:
	inc si
	xor cx, cx
PSPParsePrev_CopyLoop:
	mov al, [byte ptr es:si]

	;; Make sure the parameter isn't too long to handle 
	;; and prevent buffer overflow (8 + 1 + 3)
	cmp cx, 15
	je PSPParsePrev_Error

	cmp al, 20h
	je PSPParsePrev_CopyDone_incSI
	cmp si, 81h
	je PSPParsePrev_CopyDone
	
	mov [byte ptr ds:di], al
	
	inc si
	inc di
	inc cx
	jmp PSPParsePrev_CopyLoop
	
PSPParsePrev_CopyDone_incSI:
	inc si
PSPParsePrev_CopyDone:
	mov [word ptr PSP_CurrentOffset], si

	jmp PSPParsePrev_Return

	;; Turn on CF for error
PSPParsePrev_Error:
	stc

PSPParsePrev_Return:
	pop es
	pop ds

	pop di
	pop si

	pop cx
	pop bx
	pop ax

	pop bp
	ret 4
endp PSPParsePrev
