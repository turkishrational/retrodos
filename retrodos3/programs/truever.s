; Erdogan Tan - Retro DOS 2.0 True Version Check
; 20/04/2018

	[org 100h]

	mov	ax, 0FFFFh
	int	21h

	; Retro DOS version will be displayed if
	; MSDOS.SYS file is a Retro DOS kernel 	

terminate:
	int	20h