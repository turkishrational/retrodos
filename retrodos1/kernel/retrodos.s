; ****************************************************************************
; RETRODOS.SYS (MSDOS 1.25 Kernel) - RETRO DOS v1.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 20/04/2018
; ----------------------------------------------------------------------------
; Beginning: 04/02/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm retrodos.s -l retrodos.lst -o MSDOS.SYS)) 	
; ----------------------------------------------------------------------------
; This assembly file has two parts: (MSDOS BIOS+KERNEL in one kernel file)
; 1) 'ibmbio.s' (IBMBIO.COM) part from beginning to '%incbin MSDOS.BIN'
; 2) MSDOS.BIN (MSDOS 1.25 Kernel) as included binary file.
;
; (Note: RETRO DOS boot sector code will load 'MSDOS.SYS' at 0060h:0000h)

;=============================================================================
; ibmbio.s
;=============================================================================
; Derived from (PCDOS 1.0) 'ibmbio.s' file which is disassembled from
; 'IBMBIO.COM' by Michael Steil (06/01/2015). (IBM PCDOS v1.0) source code
; ****************************************************************************
; Modified by using disassembly of 'IBMBIO.COM' file of PCDOS 1.1 disk image
;(Disassembler: Hex-Rays IDA Pro Free)
; 05/02/2018, Erdogan Tan.
; Modified by using 'Programming Guide to the AMIBIOS', 1993.
; 10/02/2018, Erdogan Tan.
; ----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; DOS 1.0 IBMBIO.COM (disk image MD5 73c919cecadf002a7124b7e8bfe3b5ba)
;   http://www.pagetable.com/
;-----------------------------------------------------------------------------

SECTOR_SIZE     equ     0200h		; size of a sector
PAUSE_KEY       equ     7200h		; scancode + charcode of PAUSE key
KEYBUF_NEXT     equ     041Ah		; next character in keyboard buffer
KEYBUF_FREE     equ     041Ch		; next free slot in keyboard buffer
KEYBUF          equ     041Eh		; keyboard buffer data
LOGICAL_DRIVE   equ     0504h		; linear address of logical drive byte
;DOS_SEGMENT	equ     00BFh ; v1.1	; segment in which DOS will run
DOS_SEGMENT	equ     00C4h		; Retro DOS v1.0 - 13/02/2018
BIO_SEGMENT     equ     0060h		; segment in which BIO is running

;-----------------------------------------------------------------------------
; Start of code
;-----------------------------------------------------------------------------

        	[ORG 0]			; segment 0x0060

                jmp     INIT		; 0x0060:0x0000 entry point
                jmp     STATUS		; 0x0060:0x0003 check for keypress
                jmp     INP		; 0x0060:0x0006 get key from keyboard
                jmp     OUTP		; 0x0060:0x0009 send character to screen
                jmp     PRINT		; 0x0060:0x000C send character to printer
                jmp     AUXIN		; 0x0060:0x000F get character from serial
                jmp     AUXOUT		; 0x0060:0x0012 send character to serial
                jmp     READ		; 0x0060:0x0015 read sector(s) from disk (INT 0x25)
                jmp     WRITE		; 0x0060:0x0018 write sector(s) to disk  (INT 0x26)
                jmp     DSKCHG		; 0x0060:0x001B check for disk change
		; PCDOS v1.1
		jmp	SETDATE
		jmp	SETTIME
		jmp	GETTIME
		jmp	FLUSH
		jmp	MAPDEV

;-----------------------------------------------------------------------------
; print zero-terminated string at DS:DX
;-----------------------------------------------------------------------------
print_string:
		xchg    si, dx
prints1:
		cs	lodsb
                and     al, 7Fh		; clear bit 7 (XXX why?)
                jz      short prints2	; zero-terminated
                call    BIO_SEGMENT:OUTP ; print character
                jmp     short prints1	; loop
prints2:
		xchg    si, dx
                retn
	
;-----------------------------------------------------------------------------

TXT_INSERTDISK: db 13,10,'Insert diskette for drive '
TXT_DRIVE:      db 'A: and strike', 13,10
                db 'any key when ready',13,10,10,0

;-----------------------------------------------------------------------------

ERR_PAPER:	db 13,10,'Out of paper',13,10,0
ERR_PRINTER:	db 13,10,'Printer fault',13,10,0
ERR_AUX:	db 13,10,'Aux I/O error',13,10,0

;-----------------------------------------------------------------------------

conv_status:    db 80h,40h,20h,10h,09h,08h,04h,03h,02h,01h ; BIOS error codes
                db 02h,06h,0Ch,04h,0Ch,04h,08h,00h,0Ch,0Ch ; IBMBIO err codes

;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; check for keypress
;  AL = character
;  ZF = set if no character
;  all other registers preserved
;-----------------------------------------------------------------------------
STATUS:		mov     al, [cs:next_char]; check for waiting character
                or      al, al
                jnz     short char_avail ; yes, return it
                push    dx
                xchg    ax, dx
                mov     ah, 1
                int     16h		; otherwise get key (don't clear)
                jz      short status_exit ; no key
                cmp     ax, PAUSE_KEY   ; PAUSE key?
                jnz     short status_exit
                mov     al, 10h		; convert into Ctrl+P
                or      al, al
status_exit:
		mov     ah, dh          ; restore original AH
                pop     dx
char_avail:
		retf

;-----------------------------------------------------------------------------
; Interrupt 0x1B handler: Control+Break handler
;-----------------------------------------------------------------------------
int_1B:		
		mov     byte [cs:next_char], 3	; put code for Ctrl+C
iret1:
                iret                    	; into keyboard queue


;-----------------------------------------------------------------------------
; Interrupt 0x00 handler: Division by Zero
;-----------------------------------------------------------------------------
;int_00:
;		sti
;               push    ax
;               push    dx
;               mov     dx, ERR_DIVIDE
;               call    print_string
;               pop     dx
;               pop     ax
;               int     23h		; exit program through Ctrl+C path

;-----------------------------------------------------------------------------
; Interrupt 0x00 handler: Single Step
; Interrupt 0x03 handler: Breakpoint
; Interrupt 0x04 handler: Overflow
;-----------------------------------------------------------------------------
;iret1:
;		iret                    ; empty interrupt handler


;-----------------------------------------------------------------------------


;ERR_DIVIDE:	db 13,10,'Divide overflow',13,10,0


;-----------------------------------------------------------------------------
; get key from keyboard
;  AL = character
;  all other registers preserved
;-----------------------------------------------------------------------------
again:		
		xchg    ax, dx
                pop     dx
INP:
		mov     al, 0
                xchg    al, [cs:next_char]; get and clear waiting character
                or      al, al
                jnz     short inp_exit	; there is no character waiting
                push    dx
                xchg    ax, dx
                mov     ah, 0
                int	16h		; then read character from keyboard
                or      ax, ax
                jz      short again
                cmp     ax, PAUSE_KEY
                jnz     short not_pause
                mov     al, 10h		; Ctrl+P
not_pause:
		cmp     al, 0
                jnz     short skip1	; key with ASCII representation
                mov     [cs:next_char], ah; return scancode next time
skip1:
		mov     ah, dh          ; restore AH
                pop     dx
inp_exit:
		retf


;-----------------------------------------------------------------------------
; send character to screen
;  AL = character
;  all registers preserved
;-----------------------------------------------------------------------------
OUTP:
		push    bp
                push    ax
                push    bx
                push    si
                push    di
                mov     ah, 0Eh
                mov     bx, 7
                int	10h            ; print character
                pop     di
                pop     si
                pop     bx
                pop     ax
                pop     bp
                retf

;-----------------------------------------------------------------------------
; send character to printer
;  AL = character
;  all registers preserved
;-----------------------------------------------------------------------------
PRINT:
		push    ax
                push    dx
                mov     byte [cs:printer_retry], 0
printer_again:
		mov     dx, 0           ; printer port #0
                mov     ah, 0
                int     17h		; send character to printer
                mov     dx, ERR_PAPER
                test    ah, 20h
                jnz	short printer_error ; out of paper error
                mov     dx, ERR_PRINTER
                test    ah, 5
                jz	short pop_dx_ax_retf  ; no timeout error, return
                xor     byte [cs:printer_retry], 1
                jnz     short printer_again   ; on a timeout, try twice
printer_error:
		call    print_string
pop_dx_ax_retf:
		pop     dx
                pop     ax
                retf

;-----------------------------------------------------------------------------
; get character from serial port
;  AL = character
;  all other registers preserved
;-----------------------------------------------------------------------------
AUXIN:
		push    dx
                push    ax
                mov     dx, 0           ; serial port #0
                mov     ah, 2
                int     14h		; get character from serial port
                mov     dx, ERR_AUX
                test    ah, 0Eh		; framing, parity or overrun?
                jz      short aux_noerr ; no error
                call    print_string
aux_noerr:
		pop     dx
                mov     ah, dh          ; restore AH
                pop     dx
                retf

;-----------------------------------------------------------------------------
; send character to serial
;  AL = character
;  all registers preserved
;-----------------------------------------------------------------------------
AUXOUT:
		push    ax
                push    dx
                mov     ah, 1
                mov     dx, 0
                int     14h		; send character to serial port
                test    ah, 80h		; timeout error?
                jz      short pop_dx_ax_retf  ; no all fine
                mov     dx, ERR_AUX
                jmp     short printer_error

;-----------------------------------------------------------------------------
; check for disk change
;  AH = flag (1=changed)
;-----------------------------------------------------------------------------
DSKCHG:
		;mov	ah, 0		; the IBM PC can't detect disk change
                shl	al, 1		; PCDOS 1.1
		retf

; ----------------------------------------------------------------------------
; Set date of the day
; ----------------------------------------------------------------------------
SETDATE:	
		mov	[cs:day_count], ax
		xor	ax, ax
		int	1Ah		; CLOCK	- GET TIME OF DAY
					; Return: CX:DX	= clock	count
					; AL = 00h if clock was	read or	written	
					; (via AH=0,1) since the previous
					; midnight 
					; Otherwise, AL	> 0
		retf

; ----------------------------------------------------------------------------
; Set time of the day
; ----------------------------------------------------------------------------
SETTIME:	; CH = Hour
		; CL = Minute
		; DH = Second
		; DL = 10 milli second (1/100 second)

		mov	al, 60 ; 3Ch 
		mul	ch	
		mov	ch, 0
		add	ax, cx	
		mov	cx, 6000 ; 1770h	
		mov	bx, dx
		mul	cx
		mov	cx, ax	
		mov	al, 100 ; 64h
		mul	bh	
		add	cx, ax	
		adc	dx, 0	
		mov	bh, 0
		add	cx, bx	
		adc	dx, 0	
		xchg	ax, dx
		xchg	ax, cx	
		mov	bx, 59659 ; 0E90Bh
		mul	bx
		xchg	dx, cx	
		xchg	ax, dx	
		mul	bx
		add	ax, cx
		adc	dx, 0
		xchg	ax, dx	
		mov	bx, 5
		div	bl
		mov	cl, al
		mov	ch, 0
		mov	al, ah
		cbw
		xchg	ax, dx
		div	bx
		mov	dx, ax
		mov	ah, 1
		int	1Ah		; CLOCK	- SET TIME OF DAY
					; CX:DX	= clock	count
					; Return: time of day set
		retf

; ----------------------------------------------------------------------------
; Get time of the day
; ----------------------------------------------------------------------------
GETTIME:
		push	bx
		mov	ax, 0
		int	1Ah		; CLOCK	- GET TIME OF DAY
					; Return: CX:DX	= clock	count
					; AL = 00h if clock was	read or	written
					; (via AH=0,1) since the previous
					; midnight
					; Otherwise, AL	> 0
		add	[cs:day_count], ax
		mov	ax, cx
		mov	bx, dx
		shl	dx, 1
		rcl	cx, 1
		shl	dx, 1
		rcl	cx, 1
		add	dx, bx
		adc	ax, cx
		xchg	ax, dx
		mov	cx, 59659 ; 0E90Bh
		div	cx
		mov	bx, ax
		xor	ax, ax
		div	cx
		mov	dx, bx
		mov	cx, 200 ; 0C8h
		div	cx
		cmp	dl, 100 ; 64h
		jb	short gettime1
		sub	dl, 100 ; 64h
gettime1:
		cmc
		mov	bl, dl
		rcl	ax, 1
		mov	dl, 0
		rcl	dx, 1
		mov	cx, 60 ; 3Ch
		div	cx
		mov	bh, dl
		div	cl
		xchg	al, ah
		mov	dx, bx
		xchg	ax, cx
		mov	ax, [cs:day_count]
		pop	bx
		retf

; ----------------------------------------------------------------------------
; Clear keyboard buffer
; ----------------------------------------------------------------------------
FLUSH:
		mov	byte [cs:next_char], 0 
		push	ds
		xor	bp,bp
		mov	ds,bp
		mov	word [KEYBUF_NEXT], KEYBUF & 0xFF ; 1Eh
		mov	word [KEYBUF_FREE], KEYBUF & 0xFF ; 1Eh
		pop	ds
		retf

; ----------------------------------------------------------------------------
;
; ----------------------------------------------------------------------------
MAPDEV:	
		and	ah, 1
		or	al, ah
		retf

;-----------------------------------------------------------------------------
; READ  - read sector(s) from disk
; WRITE - write sector(s) to disk
;  al     drive number (0-3)
;  ds:bx  buffer
;  cx     count
;  dx     logical block number
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
INT25h_vector:
		shl	al, 1 ; 19/02/2018
;-----------------------------------------------------------------------------
READ:
		; AL = Drive number (0,1)
		; DS:BX = Buffer address
		; CX = Sector count
		; DX = (Absolute) Sector number (LBA)

		mov     ah, 2           ; BIOS code "read"
                jmp     short read_write

;-----------------------------------------------------------------------------
rw_return:
		clc
rw_err_return:
		pop	ds ; *
		mov	al, 8
		retf

;-----------------------------------------------------------------------------
INT26h_vector:
		shl	al, 1 ; 19/02/2018
;-----------------------------------------------------------------------------
WRITE:
		; AL = Drive number (0-3)
		; AH = Verify flag
		; DS:BX = Buffer address
		; CX = Sector count
		; DX = (Absolute) Sector number (LBA)

		mov	[cs:int_13h_cmd+1], ah
		mov     ah, 3           ; BIOS code "write"
read_write:
		; 19/02/2018
		; (2 disk drives -> 4 disks, MSDOS 1.25 feature!)
		
		; Retro DOS v1.0 - 11/02/2018
		cmp	al, 3 ; 19/02/2018 (1->3)
		jna	short valid_drv_num
		; invalid drive number
		mov	ax, 8002h ; Drive not ready !
		stc	
		retf	; return with error code	
valid_drv_num:
		push	ds ; *				
		push	cs
		pop	ds
		cmp	al, [drv_num]
		je	short rw2
		push	ax
		mov	[drv_num], al
		; 19/02/2018
		;and	al, al
		;jz	short rw0
		; 20/02/2018
		cmp     byte [single_floppy], 1
		je	short rw0
		; 19/02/2018	
		cmp	al, 1  ; Disks 0,1
		jna	short rw0 
		mov	si, fd1_dos_parameters
		jmp	short rw1		
rw0:		
		mov	si, fd0_dos_parameters
rw1:	
		mov	ax, [si]		  ; [si+0]
		mov	[BytesPerSec], ax
		mov	ax, [si+bsSectors-11]	  ; [si+8]
		mov	[TotalSectors], ax
		mov	ax, [si+bsSecPerTrack-11] ; [si+13]
		mov	[SecPerTrack], ax		
		mov	ax, [si+bsHeads-11]	  ; [si+15]
		mov	[Heads], ax
		pop	ax
rw2:
		shr	al, 1 ; 19/02/2018 (3->1,2->1,1->0,0->0)
		jcxz	rw_return
		mov	si, dx
		add	si, cx
		cmp	si, [TotalSectors] ; 720, 2400, 1440, 2880, 5760	
		cmc
		jc	short rw_err_return
		pop	si ; * 
		push    es
		push	si ; ds
		mov	es, si
                mov     [temp_sp], sp   ; save sp for function abort
                mov     [int_13h_cmd], ah; save whether it was read or write
; logic to emulate a "logical" drive B: by prompting the user to change disk
; when the currently used drive is changed
                cmp     byte [single_floppy], 1
                jne	short multi_drive ; more than one drive
                push    ds
                xor     si, si
                mov     ds, si          ; DS := 0x0000
                mov     ah, al
                xchg    ah, [LOGICAL_DRIVE]; current logical drive
                pop     ds
                cmp     al, ah
                jz      short drive_unchanged
                push    dx              ; save sector number
                add     al, 'A'
                mov     [TXT_DRIVE], al
                mov     dx, TXT_INSERTDISK
                call    print_string    ; prompt for disk change
               	call	BIO_SEGMENT:FLUSH ; Clear keyboard buffer
                mov     ah, 0
                int     16h		; wait for any key
                pop     dx              ; block number
drive_unchanged:
		mov     al, 0           ; for both logical A: or B: use drive A:
multi_drive:
		; Retro DOS v1.0 - 10/02/2018
		xchg    ax, dx		; dl = drive number (0 or 1)
		; AX = Sector number (LBA)
                div	byte [SecPerTrack] ; convert LBA to CHS
		; AL = track*heads
		; AH = sector
		; 18/02/2018
                inc     ah              ; ah = sector (starts at 1)
		xor	dh, dh ; 0
		;test	byte [Heads], 2
		;jz	short skip2
		shr	al, 1 		; Track
		rcl	dh, 1		; Head
skip2:
		xchg	al, ah 		; ah = track, al = sector, dh = head
                xchg    ax, cx          ; ch = track, cl = sector, ax = count
                mov     [num_sectors], ax ; count
; work around DMA hardware bug in case I/O spans a 64 KB boundary
; by using a temporary buffer
                mov     di, es          ; destination segment
                shl     di, 1
                shl     di, 1           ; make es:bx a linear address
                shl     di, 1           ; (discard upper bits)
                shl     di, 1
                add     di, bx
                ;add	di, SECTOR_SIZE-1; last byte of sector (linear)
		mov	si, [BytesPerSec]
		dec	si ; 512-1                
		add	di, si
		jc      short across_64k ; sector overflows it
                xchg    bx, di          ; bx = last byte, di = buffer
                shr     bh, 1           ; sector index in memory
		mov     ah, 128		; 0x80 sectors fit into 64 KB
                sub     ah, bh          ; sectors until 64 KB boundary
		mov     bx, di          ; bx = buffer
		cmp     ah, al		; compare to number of sectors
                jbe     short skip3	; they fit into 64 KB, cap num
                mov     ah, al          ; don't cap number of sectors
skip3:
		push    ax
		mov	al, ah
		call    rw_tracks
                pop	ax
                sub     al, ah		; requested = done?
                jz      short rw_done	; yes, exit
across_64k:
		dec     al		; one sector less
                push    ax
                cld
                push    bx
                push    es              ; save data pointer
                cmp     byte [int_13h_cmd], 2
                je      short across_64k_read ; write case follows
                mov     si, bx
                push    cx
                ;mov	cx, SECTOR_SIZE/2 ; copy first sector
                mov	cx, [BytesPerSec]
		shr	cx, 1
		push    es
                pop     ds
                push    cs
                pop     es
                mov     di, temp_sector
                mov     bx, di
                rep	movsw		; copy into IBMBIO local data
                pop     cx
                push    cs
                pop     ds
                call    rw_one_sector   ; write last sector
                pop     es
                pop     bx
                jmp     short across_64k_end
across_64k_read:
		mov     bx, temp_sector
                push    cs
                pop     es
                call    rw_one_sector   ; read last sector into temp buffer
                mov     si, bx
                pop     es
                pop     bx
                mov     di, bx
                push    cx
                ;mov	cx, SECTOR_SIZE/2
		mov	cx, [BytesPerSec]
		shr	cx, 1
                rep	movsw		; copy out
                pop     cx
across_64k_end:
		add     bh, 2           ; continue 0x0200 after that
                pop     ax
		call	rw_tracks
rw_done:
	        pop     ds
                pop     es
                ;clc			; success
                retf

;-----------------------------------------------------------------------------
; read/write an arbitrary number of sectors
;-----------------------------------------------------------------------------
rw_tracks:
		; 18/02/2018
		or      al, al
                jz      short ret3	; nothing to read
		; Retro DOS v1.0 - 11/02/2018		
                mov     ah, [SecPerTrack]
                inc	ah
		sub     ah, cl
                cmp     ah, al          ; more sectors than left in track?
                jbe     short skip4	; no
                mov     ah, al          ; otherwise, read up to end of track
skip4:
		push    ax
                mov     al, ah
                call    rw_sectors	; r/w up to [SecPerTrack] sectors
                pop     ax
                sub     al, ah          ; decrease sectors to read
                shl     ah, 1
                add     bh, ah          ; advance pointer by sectors * 0x0200
		jmp     short rw_tracks ; continue
ret2:
		clc
ret3:
		retn

;-----------------------------------------------------------------------------

int_13h_err:
		xchg    ax, di
                mov     ah, 0
                int	13h		; disk reset
                dec     si
                jz      short translate	; retries exhausted
                mov     ax, di
                cmp     ah, 80h		; in the "timeout (not ready)" case,
                jz      short translate ; we don't retry 
					; (this would take forever)
                pop     ax
                jmp     short retry
translate:				; Translate rombios err codes for DOS
		push    cs
                pop     es
                mov     ax, di
                mov     al, ah          ; status
                mov     cx, 10
                mov     di, conv_status
                repne	scasb
                mov     al, [di+9]
                mov     cx, [num_sectors]
                mov     sp, [temp_sp]   ; clean up stack
                pop     ds
                pop     es
                stc                     ; error
                retf

;-----------------------------------------------------------------------------
rw_one_sector:
		mov     al, 1
;-----------------------------------------------------------------------------
; reads/writes one or more sectors that are on the same track

rw_sectors:	
		mov     si, 5           ; number of retries
                mov     ah, [int_13h_cmd]
retry:
		; Retro DOS v1.0 - 11/02/2018
		push    ax
		int     13h		; perform the read/write
                jc      short int_13h_err
                pop     ax
		push	ax
		cmp	word [int_13h_cmd], 103h ; write and (then) verify ?
		jnz	short skip5
		mov	ah, 4
		int	13h		; DISK - VERIFY	SECTORS
					; AL = number of sectors to verify
					; CH = track, CL = sector
					; DH = head, DL	= drive
					; Return: CF set on error, AH =	status
					; AL = number of sectors verified
		jb	short int_13h_err
skip5:
		pop	ax
		;mov	ah, 0
                xor	ah, ah
		sub     [num_sectors], ax
                add     cl, al          ; calculate next sector number
                cmp     cl, [SecPerTrack] ; exceeds track?
                jbe     short ret2      ; no
                mov     cl, 1           ; sector 1
		;test	byte [Heads], 2
		;jz	short skip6
		xor	dh, 1
		jnz	short ret4
skip6:
                inc     ch              ; next track
ret4:
		retn

align 2

printer_retry:	db 0		; count for printer retries
next_char:	db 0		; extra character in keyboard queue
int_13h_cmd:	dw 2
single_floppy:	db 1		; true if we emulate a 2nd logical floppy
drv_num:	db 0FFh ; 14/02/2018
num_sectors:	dw 0
BytesPerSec:	dw 0
SecPerTrack:	dw 0
Heads:		dw 0
TotalSectors:	dw 0
day_count:	dw 0
temp_sp:	dw 0

;-----------------------------------------------------------------------------

align 2

fd0_dos_parameters:
		times	17 db 0
fd1_dos_parameters:
		times	17 db 0

;-----------------------------------------------------------------------------

align 16

temp_sector: ; 13/02/2018

fd0_drv_parameters:
		times	11 db 0
fd1_drv_parameters:
		times	11 db 0

; this is passed to IBMDOS.COM
num_floppies:	
		db 4                    ; if there's 1 physical drive, this says 2
		db 0			; physical drive number	
floppy_list:    
		dw fd0_dos_parameters	; point to params for every floppy installed
		db 0						; 0-terminated
                dw fd0_dos_parameters
		db 1
                dw fd1_dos_parameters
		db 1
		dw fd1_dos_parameters
		db 0,0,0

fd_dos_parameters:
		; This is the IBM Personal Computer
	 	; disk format. (86-DOS IO.ASM, 8-02-82)
		; Default Floppy drive Type is 1
		;	INT 13h (INT 40h) Function 08h
		;		1 - 360KB, 40 track, 5 1/4"
		;		2 - 1.2MB, 80 track, 5 1/4"
		;		3 - 720KB, 80 track, 3 1/2"
		;		4 - 1.44MB, 80 track, 3 1/2"
		;		5 - 2.88MB, 80 track, 3 1/2"	
		;		(AMIBIOS 1993)

		; Retro DOS v1.0 - 10/02/2018
		; 17 bytes of DOS disk parameters
_360K_parameters:
		; 360KB
		dw 512		; Sector size in bytes.
		db 2		; Sector per allocation unit.
		dw 1		; Reserved sectors.
		db 2		; Number of allocation tables.
		dw 112		; Number of directory entrys.
		dw 720		; Number of sectors on the disk.
		; Retro Dos v1.0 - 10/02/2018
		db 0FDh		; Media descriptor
		dw 2		; FAT size in sectors
		dw 9		; Sectors per track
		dw 2		; Number of heads	
_1200K_parameters:
		; 1.2MB
		dw 512
                db 1
                dw 1	
                db 2
                dw 224
		dw 2400
		; Retro DOS v1.0 - 10/02/2018
		db 0F9h		; Media Descriptor
		dw 7		; FAT size in sectors
		dw 15		; Sectors per track
		dw 2		; Number of heads
_720K_paramaters:
		; 720KB
		dw 512		; Sector size in bytes.
		db 2		; Sector per allocation unit.
		dw 1		; Reserved sectors.
		db 2		; Number of allocation tables.
		dw 112		; Number of directory entrys.
		dw 1440		; Number of sectors on the disk.
		; Retro DOS v1.0 - 10/02/2018
		db 0F9h		; Media descriptor
		dw 3		; FAT size in sectors
		dw 9		; Sectors per track
		dw 2		; Number of heads
_1440K_parameters:
		; 1.44MB
		dw 512
                db 1
                dw 1	
                db 2
                dw 224
		dw 2880
		; Retro DOS v1.0 - 10/02/2018
		db 0F0h		; Media descriptor
		dw 9		; FAT size in sectors
		dw 18		; Sectors per track
		dw 2		; Number of heads
_2880K_parameters:
		; 2.88MB
		dw 512
                db 2
                dw 1	
                db 2
                dw 240
		dw 5760
		; Retro DOS v1.0 - 10/02/2018
		db 0F0h		; Media descriptor
		dw 9		; FAT size in sectors
		dw 36		; Sectors per track
		dw 2		; Number of heads	

fd_drv_parameters:
		; Floppy drive parameters (AMIBIOS 1993)
		; (11 bytes)

_360K_drv_parms: ; 360KB, 5 1/4" drive
		db 0E2h	; Head unload time & step rate
		db 0	; Head load time
		db 25h	; Motor wait timer
		db 02h	; Number of bytes per sector
		db 09h	; Number of sectors per track
		db 2Ah	; Gap length
		db 0FFh	; Data length (always FFh)
		db 50h	; Gap length for format 
		db 0F6h	; Fill byte for formatting (always F6h)
		db 0Fh	; Head settle time
		db 08h	; Motor start time
		
_1200K_drv_parms: ; 1.2MB, 5 1/4" drive
		db 60h	; Head unload time & step rate
		db 0	; Head load time
		db 25h	; Motor wait timer
		db 02h	; Number of bytes per sector
		db 0Fh	; Number of sectors per track
		db 1Bh	; Gap length
		db 0FFh	; Data length (always FFh)
		db 51h	; Gap length for format 
		db 0F6h	; Fill byte for formatting (always F6h)
		db 0Fh	; Head settle time
		db 08h	; Motor start time
			
_720K_drv_parms: ; 720KB, 3 1/2" drive
		db 0E2h	; Head unload time & step rate
		db 0	; Head load time
		db 25h	; Motor wait timer
		db 02h	; Number of bytes per sector
		db 09h	; Number of sectors per track
		db 2Ah	; Gap length
		db 0FFh	; Data length (always FFh)
		db 50h	; Gap length for format 
		db 0F6h	; Fill byte for formatting (always F6h)
		db 0Fh	; Head settle time
		db 08h	; Motor start time
		
_1440K_drv_parms: ; 1.44MB, 3 1/2" drive
		db 62h	; Head unload time & step rate
		db 0	; Head load time
		db 25h	; Motor wait timer
		db 02h	; Number of bytes per sector
		db 12h	; Number of sectors per track
		db 1Bh	; Gap length
		db 0FFh	; Data length (always FFh)
		db 6Bh	; Gap length for format 
		db 0F6h	; Fill byte for formatting (always F6h)
		db 0Fh	; Head settle time
		db 08h	; Motor start time

_2880K_drv_parms: ; 2.88MB, 3 1/2" drive
		db 30h	; Head unload time & step rate
		db 0	; Head load time
		db 25h	; Motor wait timer
		db 02h	; Number of bytes per sector
		db 12h	; Number of sectors per track
		db 1Bh	; Gap length
		db 0FFh	; Data length (always FFh)
		db 50h	; Gap length for format 
		db 0F6h	; Fill byte for formatting (always F6h)
		db 0Fh	; Head settle time
		db 08h	; Motor start time

;-----------------------------------------------------------------------------
; initialization - stage 2
;-----------------------------------------------------------------------------
INIT2:
		; Retro DOS 1.0 - 11/02/2018
		mov	ax, 0E07h ; Beep
		mov	bx, 7
		int	10h

		xor	dx, dx
		xor	ax, ax
		;mov	ss, ax
		mov	sp, BIO_SEGMENT*16 ; 600h
		int     13h		; reset disk system (DL = drive)
                mov     al, 0A3h	; 2400 8N1
                int     14h		; initialize serial port
		mov     ah, 1
                int     17h		; initialize printer
		mov	es, dx ; 0
		
		cld

		; Set CRTRL+BREAK handler
		mov	di, 6Ch
		mov	ax, int_1B 
		stosw
		mov	ax, BIO_SEGMENT	; 60h, IBMBIO.COM segment
		stosw
		;; Set Division by ZERO int handler
		;xor	di, di
		;mov	bx, int_00
		; Set int 00h to int 04h handlers
		; (Except NMI interrupt, int 02h)
		;xchg	ax, bx
		;stosw			; INT 00h offset
                ;xchg	ax, bx
		;stosw			; INT 00h segment
		mov	di, 4 ; 12/08/2018 
		mov	bx, iret1
		xchg	ax, bx
		stosw			; INT 01h offset
		xchg	ax, bx
		stosw			; INT 01h segment
		add	di, 4
		xchg	ax, bx
		stosw			; INT 03h offset
		xchg	ax, bx
		stosw			; INT 03h segment
		xchg	ax, bx
		stosw			; INT 04h offset
		xchg	ax, bx
		stosw			; INT 04h segment
		;
		mov	[ES:500h], dx ; 0

		; Retro DOS v1.0 (11/02/2018)
		; Move Diskette Parameters Table to 0:0570h and set
		; INT 1Eh vector.
		
		mov	cx, 11
		;mov	cl, 11
		mov	si, fd0_drv_parameters 
		mov	ax, 570h
		mov	di, ax
		rep	movsb
		cmp	byte [num_floppies], 2 ; 19/02/2018
		jna	short init3
		;mov	si, fd1_drv_parameters 		
		mov	cl, 11
		rep	movsb		 
init3:
		mov	di, 78h
		stosw
		;mov	ax, es
		xor	ax, ax
		stosw	

		; Retro DOS v1.0 (10/02/2018)
		; Move MSDOS.SYS (IBMDOS.COM) to DOS_SEGMENT
		mov	si, msdos_bin_offset
		mov	cx, msdos_bin_size+1
		shr	cx, 1
		;cld
		;mov	ax, DOS_SEGMENT ; 0C8h
		;mov	es, ax
		;xor	di, di
		push	ds
		pop	es
		mov	di, DOS_RUN_ADDRESS ; 13/02/2018
		rep	movsw
		; restore ES to DS (CS)
		;push	ds
		;pop	es
		; Get memory size
		int	12h	
		; Return: AX = number of contiguous 1K blocks of memory
		mov	cl, 6
		shl	ax, cl
 		; init DOS (returns DS = memory for COMMAND.COM)
		xchg	ax, dx
                ; DX = memory size in paragraphs
		mov     si, num_floppies ; pointer to fd parameters structure
                call    DOS_SEGMENT:0 
		; DS = segment for COMMAND.COM)
		sti
		; Set INT 25h (disk READ) and INT 26h (disk WRITE) vectors
		xor	ax, ax
		mov	es, ax
		mov	di, 94h
		mov	ax, INT25h_vector ; 406h
		stosw
		mov	ax, cs
		stosw
		mov	ax, INT26h_vector ; 410h
		stosw
		mov	[es:di], cs
		; Set DTA for COMMAND.COM
		mov	dx, 100h	; offset in COMMAND.COM segment	
		mov	ah, 1Ah
		int	21h
		; DS:DX	-> disk	transfer buffer
		mov     cx, [06h]	; remaining memory size
                sub     cx, 100h	; PSP = bytes to read
                mov     bx, ds
                mov     ax, cs
                mov     ds, ax
		; DOS - Open File: COMMAND.COM 
		mov	dx, FCB_command_com
		mov	ah, 0Fh
		int	21h
		; AL = 00h file	found, FFh file	not found
		or	al, al
		jnz	short error_command
		; DOS - Random Block Read 
		; DS:DX	-> FCB
		; CX = number of records to be read
		mov	word [FCB_command_com+14], 1
		mov	ah, 27h
		int	21h
		jcxz	error_command
		cmp	al, 1
		jnz	short error_command
		mov	ds, bx
		mov	es, bx
		mov	ss, bx
		mov	sp, 5Ch
		xor	ax, ax
		push	ax
		; DOS - Set Disk Transfer Area address
		mov	dx, 80h
		; DS:DX	-> disk	transfer buffer
		mov	ah, 1Ah
		int	21h
		; Run COMMAND.COM
		push	bx
		mov	ax, 100h
		push	ax
		retf	

;-----------------------------------------------------------------------------

error_command:  mov     dx, ERR_COMMANDCOM ; "Bad or missing Command Interpreter"
                call    print_string
halt:
		jmp	short halt

;-----------------------------------------------------------------------------

FCB_command_com:
		db 1, 'COMMAND COM', 0
                times 24 db 0

;-----------------------------------------------------------------------------

ERR_COMMANDCOM:
		db 13,10,'Bad or missing Command Interpreter',13,10,0

;-----------------------------------------------------------------------------

		
		times 512-($-temp_sector) db 0

temp_sector_end:

;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------

align 16

DOS_RUN_ADDRESS	equ $ ; 13/02/2018

;-----------------------------------------------------------------------------
; entry point from boot sector
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; initialization - stage 1
;-----------------------------------------------------------------------------
INIT:
		; Retro DOS v1.0 (10/02/2018)
		; (Register values from Retro DOS v1.0 Boot Sector)
		; CS = 60h  ; BIO_SEGMENT
		; DS = 0
		; ES = 0
		; SS = 0
		; SP = 0FFFEh
 		; BP = 7C00h
		; DL = Physical drive number = 0
		push	cs
		pop	ds

		; Retro DOS v1.0 boot sector (fd0) dos parameters
		; (17 bytes)
		bsBytesPerSec	equ 11
		bsSecPerClust	equ 13
		bsResSectors	equ 14	
		bsFATs		equ 16
		bsRootDirEnts	equ 17
		bsSectors	equ 19	
		bsMedia		equ 21
		bsFATsecs	equ 22
		bsSecPerTrack	equ 24
		bsHeads		equ 26

		push	ds
		pop	es

		; 14/02/2018
		; Set video mode to 3 (Clear screen)
		mov	ax, 3
		int	10h

		mov	si, 7C00h+bsBytesPerSec-(BIO_SEGMENT*16)
		; si = 30219 (60h:760Bh=0:7C0Bh)
		mov	cx, 17
		mov	di, fd0_dos_parameters
		rep	movsb

		;  Return Floppy Disk Parameters (for Drive 0)
		mov	ah, 08h
		;mov	dl, 0
		int	13h
		jc	short very_old_rombios
		cmp	bl, 1
		jb	short very_old_rombios		
		cmp	bl, 5
		ja	short very_old_rombios
		shl	dl, 1		; 19/02/2018
		mov	[num_floppies], dl

		; ES:DI = Address of Disk Parameters Table
		push	es
		pop	ds
		push	cs
		pop	es
		mov	si, di
		mov	di, fd0_drv_parameters
		mov	cx, 11
		rep	movsb
		push	cs
		pop	ds
		cmp	dl, 1
		jna	short init1
		mov	byte [single_floppy], 0 ; 2 floppy drives
		; Return Floppy Disk Parameters (for Drive 1)
		mov	ah, 08h
		mov	dl, 1
		int	13h
		jc	short very_old_rombios	
		cmp	bl, 1
		jb	short very_old_rombios
		cmp	bl, 5
		ja	short very_old_rombios
		push	es
		pop	ds
		push	cs
		pop	es
		mov	si, di
		mov	di, fd1_drv_parameters
		mov	cx, 11
		rep	movsb
		push	es
		pop	ds
		dec	bl ; Floppy disk drive type number 1 to 4
			   ; 0 to 3	
		mov	al, 17
		mul	bl
		mov	si, ax
		add	si, fd_dos_parameters
		mov	di, fd1_dos_parameters
		mov	cl, 17
		rep	movsb
init1:
		mov	ax, [bp+bsSecPerTrack]
		mov	[SecPerTrack], ax
		mov	ax, [bp+bsHeads]
		mov	[Heads], ax
		mov	ax, [bp+bsSectors]
		mov	[TotalSectors], ax

		jmp	INIT2
			
very_old_rombios:
		; Very old or abnormal or unknown type ROMBIOS
		; or CMOS RAM data is invalid or not present
		mov	byte [num_floppies], 2 ; 19/02/2018
		jmp	short init1


;-----------------------------------------------------------------------------

align 16

msdos_bin_offset:
incbin		'MSDOS.BIN'
msdos_bin_size	equ $ - msdos_bin_offset