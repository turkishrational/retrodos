;;; ***************************************************************************
;;; *** General file handling procedures start here.                        ***
;;; ***************************************************************************

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure takes CurrentFileName's value,
;;;						and returns a newly created file-handle.
;;; *** PARAMS:			None.
;;; *** REGS:			AX, DX
;;; *** VARS:			CurrentFileName(R), CurrentFileHandle(W)
;;;	*** CALLS:			None.
;;; ***	RETURNS:		CF==1: ERR, CF==0: AX is inserted to CurrentFileHandle

proc getFileHandleByPath
	push ax
	push dx

	xor al, al
	mov ah, 3Dh
	lea	dx, [CurrentFileName]
	int 21h
	
	mov [word ptr CurrentFileHandle], ax

getFileHandleByPath_Return:
	pop dx
	pop ax
	ret
endp getFileHandleByPath

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure closes the file handle
;;; *** PARAMS:			(WORD) fileHandle
;;; *** REGS:			AX, BX
;;; *** VARS:			None.
;;;	*** CALLS:			DOS interrupt (0x21)
;;; ***	RETURNS:		None.

closeFileHandle_FileHandle	equ	[bp+4]

proc closeFileHandle
	push bp
	mov bp, sp

	push ax
	push bx
	
	mov ax, 3E00h
	mov bx, [word ptr CurrentFileHandle]

	int 21h
	
closeFileHandle_Return:
	pop bx
	pop ax

	pop bp

	ret 2
endp closeFileHandle

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure loads file data - given size, file handle
;;;						and memory pointer to load to.
;;; *** PARAMS:			(WORD) fileHandle, (WORD) loadSize, (WORD) loadPtr
;;; *** REGS:			AH(W), BX(W), CX(W), DX(W), BP(RW), SP(R)
;;; *** VARS:			None.
;;;	*** CALLS:			DOS interrupt (0x21)
;;; ***	RETURNS:		AX = Number of bytes read.

loadData_fileHandle 	equ	[bp+8]
loadData_loadSize 		equ	[bp+6]
loadData_loadPtr 		equ	[bp+4]

proc loadData
	push bp
	mov bp, sp
	
	push bx
	push cx
	push dx
	
	mov ah, 3Fh						;; AH=3Fh
	mov bx, loadData_fileHandle		;; BX=File handle
	mov cx, loadData_loadSize		;; CX=Amount of data to read
	mov dx, loadData_loadPtr		;; DX=Buffer read pointer
	int 21h							;; DOS interrupt
	
loadData_return:
	pop dx
	pop cx
	pop bx
	
	pop bp
	
	ret 6
endp loadData

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure loads file data, using the "double buffer" method
;;; *** PARAMS:			None.
;;; *** REGS:			-
;;; *** VARS:			CurrentFileHandle, TransferLength, CurrentLoadBuffer
;;;	*** CALLS:			loadData
;;; ***	RETURNS:		AX = Number of bytes read.

proc loadBufferData
	mov al, [byte ptr CurrentLoadBuffer]
	cmp al, 2
	je loadBufferData_Second

loadBufferData_First:
	mov ax, offset DATA
	mov [byte ptr CurrentLoadBuffer], 2
	jmp loadBufferData_Load
	
loadBufferData_Second:
	mov ax, offset DATA
	add ax, [word ptr SubBufferLength]
	mov [byte ptr CurrentLoadBuffer], 1
	
loadBufferData_Load:
	push [word ptr CurrentFileHandle]
	push [word ptr SubBufferLength]
	push ax
	call loadData

	or ax, ax
loadBufferData_Return:
	ret
endp loadBufferData
