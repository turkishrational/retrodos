;;; ***************************************************************************
;;; *** RIFF/WAVE file format related procedures start here                 ***
;;; ***************************************************************************

;;; ***************************************************************************
;;; *** DESCRIPTION:	The procedure uses other procedures to load a WAV
;;;						file's headers into the memory.
;;; *** PARAMS:			(WORD) fileNamePointer
;;; *** REGS:			BP(RW)
;;; *** VARS:			not-set
;;;	*** CALLS:			not-set
;;; ***	RETURNS:		ZF==0: Error, ZF==1: OK

loadWAVEHeaders_RIFFHeaderSize	equ 0Ch
loadWAVEHeaders_WAVHeaderSize	equ 18h

proc loadWAVEHeaders
	push bp
	mov bp, sp

		;; Load - what is to be assumed - the RIFF header from the file.

loadWAVEHeaders_loadRIFF:
	push [word ptr CurrentFileHandle]
	push loadWAVEHeaders_RIFFHeaderSize
	push offset RIFF_ChunkID
	call loadData

loadWAVEHeaders_checkRIFF:
	call checkRIFFHeader
	jz loadWAVEHeaders_loadWAV 		;; If ZF is set, the header is valid.

	push offset string_riffheader_invalid
	call printString
	jmp loadWAVEHeaders_return

		;; Load - what is to be assumed - the WAV header from the file.

loadWAVEHeaders_loadWAV:
	push [word ptr CurrentFileHandle]
	push loadWAVEHeaders_WAVHeaderSize
	push offset WAVE_SubchunkID
	call loadData
	jz loadWAVEHeaders_return

	push offset string_waveheader_invalid
	call printString
	jmp loadWAVEHeaders_return
	
loadWAVEHeaders_return:
	pop bp
	
	ret
endp loadWAVEHeaders

;;; ***************************************************************************
;;; *** DESCRIPTION:	This procedure checks whether a given file handle
;;;						contains the correct & supported RIFF-WAV format.
;;; *** PARAMS:			None.
;;; *** REGS:			None.
;;; *** VARS:			(DWORD) RIFF_ChunkID, (DWORD) RIFF_Format
;;;	*** CALLS:			None.
;;; ***	RETURNS:		ZF==1: Not a RIFF file, ZF==0: A correct RIFF file.

proc checkRIFFHeader
		;; Check for "RIFF" RIFF_chunkID
	cmp [byte ptr RIFF_ChunkID + 0], 'R'
	jnz checkRIFFHeader_return
	
	cmp [byte ptr RIFF_ChunkID + 1], 'I'
	jnz checkRIFFHeader_return
	
	cmp [byte ptr RIFF_ChunkID + 2], 'F'
	jnz checkRIFFHeader_return
	
	cmp [byte ptr RIFF_ChunkID + 3], 'F'
	jnz checkRIFFHeader_return
	
		;; Check for "WAVE" RIFF_format
	cmp [byte ptr RIFF_Format + 0], 'W'
	jnz checkRIFFHeader_return
	
	cmp [byte ptr RIFF_Format + 1], 'A'
	jnz checkRIFFHeader_return
	
	cmp [byte ptr RIFF_Format + 2], 'V'
	jnz checkRIFFHeader_return
	
	cmp [byte ptr RIFF_Format + 3], 'E'
	jnz checkRIFFHeader_return
	
checkRIFFHeader_return:
	ret
endp checkRIFFHeader

;;; ***************************************************************************
;;; *** DESCRIPTION:	This procedure checks whether a given file handle
;;;						contains the correct & supported WAV subchunk format.
;;; *** PARAMS:			None.
;;; *** REGS:			None
;;; *** VARS:			(QWORD) WAVE_SubchunkID
;;;	*** CALLS:			None.
;;; ***	RETURNS:		ZF==1: Invalid WAV file, ZF==0: A valid RIFF/WAV file

proc checkWAVEHeader
	cmp [byte ptr WAVE_SubchunkID + 0], 'f'
	jnz checkWAVEHeader_return
	
	cmp [byte ptr WAVE_SubchunkID + 1], 'm'
	jnz checkWAVEHeader_return

	cmp [byte ptr WAVE_SubchunkID + 2], 't'
	jnz checkWAVEHeader_return

	cmp [byte ptr WAVE_SubchunkID + 3], ' '
	jnz checkWAVEHeader_return

checkWAVEHeader_return:
	ret
endp checkWAVEHeader
