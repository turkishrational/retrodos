; ****************************************************************************
; RETRODOS.SYS (MSDOS 2.0 Kernel) - RETRO DOS v2.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 18/07/2018
; ----------------------------------------------------------------------------
; Beginning: 04/02/2018 (Retro DOS 1.0), 24/02/2018 (Retro DOS 2.0)
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm retrodos.s -l retrodos.lst -o MSDOS.SYS)) 	
; ----------------------------------------------------------------------------
; This assembly file has three parts: (BIOS+SYSINIT+KERNEL in one kernel file)
; 1a) IBMBIO.COM (IO.SYS) part from beginning to '%incbin MSDOS.BIN'
; 1b) SYSINIT part (at the end of IBMBIO.COM/IO.SYS) as '%incbin SYSINIT.BIN'		
; 2) MSDOS.SYS (MSDOS 2.0 Kernel) as included binary file (MSDOS.BIN).
; source : 'retrodos.s' (main), 'sysinit.s' (included), 'msdos.s' (included)	
;
; (Note: RETRO DOS 2.0 boot sector code will load 'MSDOS.SYS' at 1000h:0000h)
; (Original MSDOS 2.0 IBMBIO.COM loads/runs its own init code at 0070h:0000h)

;=============================================================================
; Modified from 'retrodos.s', Retro DOS v1.0 Kernel ("IBMBIO.COM") Source code
; by Erdogan Tan, 20/02/2018
;=============================================================================

SECTOR_SIZE     equ     0200h		; size of a sector
PAUSE_KEY       equ     7200h		; scancode + charcode of PAUSE key
KEYBUF_NEXT     equ     041Ah		; next character in keyboard buffer
KEYBUF_FREE     equ     041Ch		; next free slot in keyboard buffer
KEYBUF          equ     041Eh		; keyboard buffer data
LOGICAL_DRIVE   equ     0504h		; linear address of logical drive byte
;DOS_SEGMENT	equ     00BFh ; v1.1	; segment in which DOS will run
DOS_SEGMENT	equ     00C4h		; Retro DOS v1.0 - 13/02/2018
BIO_SEGMENT     equ     0060h		; segment in which BIO is running

; 24/02/2018 (Retro DOS 2.0 - MSDOS 3.3 "DISKPRM.INC" - 24/07/1987)
; The following structure defines the disk parameter table
; pointed to by Interrupt vector 1EH (location 0:78H)

struc	DISK_PARMS
.DISK_SPECIFY_1:  resb	1
.DISK_SPECIFY_2:  resb	1
.DISK_MOTOR_WAIT: resb  1	; Wait till motor off
.DISK_SECTOR_SIZ: resb 	1	; Bytes/Sector (2 = 512)
.DISK_EOT:	  resb  1	; Sectors per track (MAX)
.DISK_RW_GAP:	  resb  1	; Read Write Gap
.DISK_DTL:	  resb	1
.DISK_FORMT_GAP:  resb  1	; Format Gap Length
.DISK_FILL:	  resb  1	; Format Fill Byte
.DISK_HEAD_STTL:  resb  1	; Head Settle Time (MSec)
.DISK_MOTOR_STRT: resb  1	; Motor start delay
.size:
endstruc

ROMStatus	equ	1
ROMRead 	equ	2
ROMWrite	equ	3
ROMVerify	equ	4
ROMFormat	equ	5  

; 24/02/2018 (Retro DOS 2.0 - MSDOS 3.3 "MSBDS.INC" - 24/07/1987)
; -------------------------------------------------------------------------
;  BDS is the Bios Data Structure.
;
;  There is one BDS for each logical drive in the system.  All the BDS's
;  are linked together in a list with the pointer to the first BDS being
;  found in START_BDS.	The BDS hold various values important to the disk
;  drive.  For example there is a field for last time accesses.  As actions
;  take place in the system the BDS are update to reflect the actions.
;  For example is there is a read to a disk the last access field for the
;  BDS for that drive is update to the current time.
;
; Values for various flags in BDS.Flags.
;

fNon_Removable	    equ     01H 	;For non-removable media
fChangeline	    equ     02H 	;If changeline supported on drive
RETURN_FAKE_BPB     equ     04H 	; When set, don't do a build BPB
					; just return the fake one
GOOD_TRACKLAYOUT    equ     08H 	; The track layout has no funny sectors
fI_Am_Mult	    equ     10H 	;If more than one logical for this physical
fI_Own_Physical     equ     20H 	;Signify logical owner of this physical
fChanged	    equ     40H 	;Indicates media changed
SET_DASD_true	    equ     80H 	; Set DASD before next format
fChanged_By_Format  equ    100h

;
; Various form factors to describe media
;

ff48tpi 	    equ     0
ff96tpi 	    equ     1
ffSmall 	    equ     2
ffHardFile	    equ     5
ffOther 	    equ     7
; MSDOS 6.0 ("MSBDS.INC", 1991)
ff288		    equ     9	; 2.88 MB drive
; Retro DOS v2.0 fetaure only !
ff144		    equ	   10	; 1.44 MB drive			

struc	BDS	; BDS_Type
.Link:		resd 1		; Link to next BDS
.DriveNum:	resb 1		; Physical drive number
.DriveLet:	resb 1		; DOS drive number
.BytePerSec:	resw 1		; number of bytes/sec
.SecPerClus:	resb 1		; sec per allocation unit
.RESSEC:	resw 1		; number of reserved sectors
.cFAT:		resb 1		; number of fats
.cDir:		resw 1		; number of directory entries
.DRVLIM:	resw 1		; number of sectors on medium
.Mediad:	resb 1		; media descriptor byte
.cSecFat: 	resw 1		; number of sectors/fat
.SECLIM:	resw 1		; sectors per track
.HDLIM:		resw 1		; max number of heads
.HIDSEC:	resw 1		; number of hidden sectors
.FatSiz:	resb 1		; flags...
.Opcnt:		resw 1		; Open ref. count
.Volid:		resb 12		; volume ID of medium
.FormFactor:	resb 1		; form factor index
.Flags:		resw 1		; various flags
.cCyln:		resw 1		; max number of cylinders
.RBytePerSec:	resw 1		; Recommended BPB
.RSecPerClus:	resb 1
.RRESSEC: 	resw 1
.RcFAT:		resb 1
.RcDir:		resw 1
.RDRVLIM: 	resw 1
.Rmediad: 	resb 1
.RcSecFat:	resw 1
.RSECLIM: 	resw 1
.RHDLIM:	resw 1
.RHIDSEC: 	resw 1
.RHHIDSEC:	resw 1
.RLOGSEC: 	resd 1
.Reserve: 	resb 6		; Reserved for future
.Track:		resb 1		; last track accessed on drive
.TIM_LO:	resw 1		; Time of last access. Keep
.TIM_HI:	resw 1		; these contiguous.
endstruc

BPBSize	equ	BDS.Track - BDS.RBytePerSec 
				; size in bytes of RecBPB area in the BDS

; 23/03/2018

;STATIC REQUEST HEADER (DEVSYM.INC, MSDOS 6.0, 1991)
STRUC SRHEAD
.REQLEN:	resb 1		;LENGTH IN BYTES OF REQUEST BLOCK
.REQUNIT:	resb 1		;DEVICE UNIT NUMBER
.REQFUNC:	resb 1		;TYPE OF REQUEST
.REQSTAT:	resw 1		;STATUS WORD
	       	resb 8		;RESERVED FOR QUEUE LINKS
.size:
endstruc

; GENERIC IOCTL REQUEST STRUCTURE (DEVSYM.INC, MSDOS 6.0, 1991)
;	SEE THE DOS 4.0 DEVICE DRIVER SPEC FOR FURTHER ELABORATION.
;
struc IOCTL_REQ
	       ;DB    (SIZE SRHEAD) DUP(?)
		resb SRHEAD.size	
			    	; GENERIC IOCTL ADDITION.
.MAJORFUNCTION:	resb 1		;FUNCTION CODE
.MINORFUNCTION:	resb 1		;FUNCTION CATEGORY
.REG_SI:	resw 1
.REG_DI:	resw 1
.GENERICIOCTL_PACKET: resd 1	; POINTER TO DATA BUFFER
endstruc

; GENERIC IOCTL CATEGORY CODES  (IOCTL.INC, MSDOS 6.0, 1991)
IOC_OTHER	EQU	0	; Other device control J.K. 4/29/86
IOC_SE		EQU	1	; SERIAL DEVICE CONTROL
IOC_TC		EQU	2	; TERMINAL CONTROL
IOC_SC		EQU	3	; SCREEN CONTROL
IOC_KC		EQU	4	; KEYBOARD CONTROL
IOC_PC		EQU	5	; PRINTER CONTROL
IOC_DC		EQU	8	; DISK CONTROL (SAME AS RAWIO)

; DEFINITIONS FOR IOCTL_REQ.MINORFUNCTION
GEN_IOCTL_WRT_TRK   EQU   40H
GEN_IOCTL_RD_TRK    EQU   60H
GEN_IOCTL_FN_TST    EQU   20H	; USED TO DIFF. BET READS AND WRTS

;struc A_RETRYCOUNT  ; (IOCTL.INC, MSDOS 6.0, 1991)
;.RC_COUNT:	resw 	1
;endstruc

; 27/03/2018 - DEVSYM.INC - MSDOS 3.3 - 24/07/1987

;
; ATTRIBUTE BIT MASKS
;
; CHARACTER DEVICES:
;
; BIT 15 -> MUST BE 1
;     14 -> 1 IF THE DEVICE UNDERSTANDS IOCTL CONTROL STRINGS
;     13 -> 1 IF THE DEVICE SUPPORTS OUTPUT-UNTIL-BUSY
;     12 -> UNUSED
;     11 -> 1 IF THE DEVICE UNDERSTANDS OPEN/CLOSE
;     10 -> MUST BE 0
;      9 -> MUST BE 0
;      8 -> UNUSED
;      7 -> UNUSED
;      6 -> UNUSED
;      5 -> UNUSED
;      4 -> 1 IF DEVICE IS RECIPIENT OF INT 29H
;      3 -> 1 IF DEVICE IS CLOCK DEVICE
;      2 -> 1 IF DEVICE IS NULL DEVICE
;      1 -> 1 IF DEVICE IS CONSOLE OUTPUT
;      0 -> 1 IF DEVICE IS CONSOLE INPUT
;
; BLOCK DEVICES:
;
; BIT 15 -> MUST BE 0
;     14 -> 1 IF THE DEVICE UNDERSTANDS IOCTL CONTROL STRINGS
;     13 -> 1 IF THE DEVICE DETERMINES MEDIA BY EXAMINING THE FAT ID BYTE.
;	    THIS REQUIRES THE FIRST SECTOR OF THE FAT TO *ALWAYS* RESIDE IN
;	    THE SAME PLACE.
;     12 -> UNUSED
;     11 -> 1 IF THE DEVICE UNDERSTANDS OPEN/CLOSE/REMOVABLE MEDIA
;     10 -> MUST BE 0
;      9 -> MUST BE 0
;      8 -> UNUSED
;      7 -> UNUSED
;      6 -> IF DEVICE HAS SUPPORT FOR GETMAP/SETMAP OF LOGICAL DRIVES.
;	    IF THE DEVICE UNDERSTANDS GENERIC IOCTL FUNCTION CALLS.
;      5 -> UNUSED
;      4 -> UNUSED
;      3 -> UNUSED
;      2 -> UNUSED
;      1 -> UNUSED
;      0 -> UNUSED
;

DEVTYP	       EQU   8000H	    ; BIT 15 - 1  IF CHAR, 0 IF BLOCK
CHARDEV        EQU   8000H
DEVIOCTL       EQU   4000H	    ; BIT 14 - CONTROL MODE BIT
ISFATBYDEV     EQU   2000H	    ; BIT 13 - DEVICE USES FAT ID BYTES,
				    ;  COMP MEDIA.
OUTTILBUSY     EQU   2000H	    ; OUTPUT UNTIL BUSY IS ENABLED
ISNET	       EQU   1000H	    ; BIT 12 - 1 IF A NET DEVICE, 0 IF
				    ;  NOT.  CURRENTLY BLOCK ONLY.
DEVOPCL        EQU   0800H	    ; BIT 11 - 1 IF THIS DEVICE HAS
				    ;  OPEN,CLOSE AND REMOVABLE MEDIA
				    ;  ENTRY POINTS, 0 IF NOT

EXTENTBIT      EQU   0400H	    ; BIT 10 - CURRENTLY 0 ON ALL DEVS
				    ;  THIS BIT IS RESERVED FOR FUTURE USE
				    ;  TO EXTEND THE DEVICE HEADER BEYOND
				    ;  ITS CURRENT FORM.

; NOTE BIT 9 IS CURRENTLY USED ON IBM SYSTEMS TO INDICATE "DRIVE IS SHARED".
;    SEE IOCTL FUNCTION 9. THIS USE IS NOT DOCUMENTED, IT IS USED BY SOME
;    OF THE UTILITIES WHICH ARE SUPPOSED TO FAIL ON SHARED DRIVES ON SERVER
;    MACHINES (FORMAT,CHKDSK,RECOVER,..).

DEV320	       EQU   0040H	    ;BIT 6 - FOR BLOCK DEVICES, THIS
				    ;DEVICE SUPPORTS SET/GET MAP OF
				    ;LOGICAL DRIVES, AND SUPPORTS
				    ;GENERIC IOCTL CALLS.
				    ;FOR CHARACTER DEVICES, THIS
				    ;DEVICE SUPPORTS GENERIC IOCTL.
				    ;THIS IS A DOS 3.2 DEVICE DRIVER.
ISSPEC	       EQU   0010H	    ;BIT 4 - THIS DEVICE IS SPECIAL
ISCLOCK        EQU   0008H	    ;BIT 3 - THIS DEVICE IS THE CLOCK DEVICE.
ISNULL	       EQU   0004H	    ;BIT 2 - THIS DEVICE IS THE NULL DEVICE.
ISCOUT	       EQU   0002H	    ;BIT 1 - THIS DEVICE IS THE CONSOLE OUTPUT.
ISCIN	       EQU   0001H	    ;BIT 0 - THIS DEVICE IS THE CONSOLE INPUT.


; 19/03/2018

;*********************************************************************
;	BDS structure for mini disk
;*********************************************************************	  
									  
struc	BDSM	; BDSM_type							  
.mLink:		resw	1 ; DW -1		;Link to next structure   
		resw	1 ; DW ?						  
.mDriveNum:	resb	1 ; DB 80h		;Int 13 Drive Number	  
.mDriveLet:	resb	1 ; DB 3		;Logical Drive Number	  
.mBytePerSec:	resw 	1 ; DW 512						  
.mSecPerClus:	resb	1 ; DB 1		;Sectors/allocation unit  
.mRESSEC:	resw	1 ; DW 1		;Reserved sectors for DOS 
.mcFAT:		resb	1 ; DB 2		;No. of allocation tables 
.mcDIR:		resw	1 ; DW 16		;Number of directory entries
.mDRVLIM:	resw 	1 ; DW 0		;Num of sectors (at 512 bytes each)
.mMediad:	resb	1 ; DB 11111000b	;Media descriptor	  
.mcSecFat:	resw 	1 ; DW 1		;Number of FAT sectors	  
.mSECLIM:	resw 	1 ; DW 0		;Sector limit		  
.mHDLIM:	resw	1 ; DW 0		;Head limit		  
.mHIDSEC:	resw 	1 ; DW 0		;Hidden sector count	  
.mFatSiz:	resb 	1 ; DB 0		;TRUE => bigfat 	  
.mOPCNT:	resw	1 ; DW 0		;Open Ref. Count	  
.mVOLID:	resb   11 ; DB "NO NAME    "    ;Volume ID for this disk  
		resb 	1 ; DB 0		;ASCIZII for "NO NAME    "
.mFormFactor:	resb 	1 ; DB 3		;Form Factor		  
.mFLAGS:	resw	1 ; DW 0020H		;Various Flags		  
.mcCyln:	resw	1 ; DW 40		;max number of cylinders  
.mRecBPB:	resb   31 ; DB 31 DUP (0) 	;Recommended BPB for drive
.mTrack:	resb	1 ; DB -1						  
.isMini:	resw	1 ; DW 1		;Overlapping TIM_LOH	  
.Hidden_Trks:	resw 	1 ; DW 0		;Overlapping TIM_HIH	  
									  
;.TIM_LOH:	resw	1 ; DW -1		;Keep these two contiguous (?)   
;.TIM_HIH:	resw	1 ; DW -1
.size:							  

endstruc
							  
;******************************************************************************
Max_mini_dsk_num equ	23		; Max # of mini disk bios can support


KERNEL_SEGMENT	equ 0070h  ; Retro DOS v2.0 - 17/03/2018

;-----------------------------------------------------------------------------
; Start of code
;-----------------------------------------------------------------------------

        	[ORG 0]			; segment 0x0060

;-----------------------------------------------------------------------------
; MSDATA.INC - MSDOS 3.3 BIOS - 24/07/1987
;-----------------------------------------------------------------------------
; 21/03/2018 - Retro DOS v2.0

START$:
		JMP	INIT		; START$ patch by init to point to
					; hdrive BPB

SYS_INIT_START EQU SYSINIT_START - START$
KERNEL_BYTES EQU END_OF_KERNEL - START$
MSDOS_BIN_ADDRESS EQU MSDOS_BIN_OFFSET - START$

%define SYSINITSEG SYS_INIT_START >> 4  ; 26/03/2018
%define MSDOS_BIN_SEGMENT MSDOS_BIN_ADDRESS >> 4 ; 26/03/2018 
%define KERNEL_SIZE KERNEL_BYTES >> 1 ; Retro DOS v2.0 - 26/03/2018


;----------------------------------------------------------------------------
;
;		Command Jump Tables
;
;     These tables hold the entry points for the various service routines
; for the different drivers.  The index in the table is the command code for
; that funcion plus two.  For example the command code for Read (input) is 4,
; The 6th (4 plus 2) entry in the table DSKTBL is DSK$READ - the command to
; read a disk.	Commands which do not exist for a device are filled with
; exit (e.g. MediaCheck for CONTBL).  The first entry in the table is the
; largest command code implemented for that device.  This value is used
; for error checking.  If new command codes are added then the first entry
; in the table must be incremented.
;
;	BEWARE - These tables overlap somewhat! -c.p.
;


align 2

		; 08/04/2018
		; Retro DOS v2.0 (IBMBIO.COM, IBMDOS 2.1)
		; ((Disassembler: IDA Pro Free))

;
; Disk:
;

DSKTBL:
		DW	DSK$INIT	; Code	0: INIT
		DW	MEDIA$CHK	; code	1: Media Check
		DW	GET$BPB 	; code	2: BUILD BPB
		DW	CMDERR		; code	3: IOCTL input
		DW	DSK$READ	; code	4: INPUT
		DW	BUS$EXIT	; code	5: NONDESTRUCITVE INPUT, NO WAIT
		DW	EXIT		; code	6: INPUT STATUS
		DW	EXIT		; code	7: INPUT FLUSH
		DW	DSK$WRIT	; code	8: OUTPUT
		DW	DSK$WRITV	; code	9: OUTPUT with verify

;
; Console:
;

CONTBL:
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	CMDERR
		DW	CON$READ
		DW	CON$RDND
		DW	EXIT
		DW	CON$FLSH
		DW	CON$WRIT
		DW	CON$WRIT

;
; Auxilary:
;

AUXTBL:
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	CMDERR
		DW	AUX$READ
		DW	AUX$RDND
		DW	EXIT
		DW	AUX$FLSH
		DW	AUX$WRIT
		DW	AUX$WRIT
		DW	AUX$WRST

;
; Clock:
;

TIMTBL:
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	CMDERR
		DW	TIM$READ
		DW	BUS$EXIT
		DW	EXIT
		DW	EXIT
		DW	TIM$WRIT
		DW	TIM$WRIT

;
; Printer:
;

PRNTBL:
		DW	EXIT			;INIT
		DW	EXIT
		DW	EXIT
		DW	CMDERR
		DW	EXIT$ZER		;INDICATE ZERO CHARS READ
		DW	BUS$EXIT
		DW	EXIT
		DW	EXIT
		DW	PRN$WRIT
		DW	PRN$WRIT
		DW	PRN$STAT
		DW	EXIT

;
; PTRSAV - pointer save
;
;   This variable holds the pointer to the Request Header passed by a
; program wishing to use a device driver.  When the strategy routine is
; called it puts the address of the Request header in this variable and
; returns.
;

	;EVENB
PTRSAV:
		DD	0

AUXBUF:
		;DB	0,0,0,0	; SET OF 1 BYTE BUFFERS FOR COM 1,2,3, AND 4
		DB	0, 0  ; 08/04/2018 (IBMDOS 2.1)

;
; AUXNUM holds the number of the printer or AUX device requested.  For
; example if printer 2 was called (PRN2$IN) AUXNUM is set to be one; with
; line printer 3 AUXNUM is set to 2.  With this set the printer device driver
; can tell which printer to command applies to.
;
; WARNING!!!  These are addressed together in GETDX
;

	;EVENB

AUXNUM:
		DB	0
		DB	0

;
; Device Header for the CON Device Driver
;

	;EVENB

;align 2

CONHeader:
		;DD	AUXDEV2
		DW	AUXDEV2
		DW	KERNEL_SEGMENT ; 27/03/2018
		DW	1000000000010011B	; Con in and con out + special
		DW	STRATEGY		; Strategy entry point
		DW	CON$IN			; interrupt entry point
		DB	'CON     '              ; device name


;
; Device Header for device "AUX"
;

	;EVENB
;align 2

AUXDEV2:					;HEADER FOR DEVICE "AUX"
		;DD	PRNDEV2
		DW	PRNDEV2
		DW	KERNEL_SEGMENT ; 27/03/2018
		DW	1000000000000000B	; attribute word, character device
		DW	STRATEGY		; device strategy routine
		DW	AUX0$IN 		; device interrupt routine
		DB	'AUX     '              ; device name


;
; Device Header for device PRN
;

	;EVENB
;align 2

PRNDEV2:					 ;HEADER FOR DEVICE "PRN"
		;DD	TIMDEV
		DW	TIMDEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		;DW	CHARDEV + OUTTILBUSY + DEV320
		DW	1000000000000000B	; 08/04/2018 (IBMDOS 2.1)
		DW	STRATEGY
		DW	PRN0$IN
		DB	'PRN     '


;
; Device Header for device CLOCK$
;

	;EVENB
;align 2

TIMDEV:
		;DD	DSKDEV
		DW	DSKDEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		DW	1000000000001000B
		DW	STRATEGY
		DW	TIM$IN
		DB	'CLOCK$  '

;
; Device Header for disk devices
;
;	Device attribute bits:
;		Bit 6 - DOS 3.2 Bit
;

	;EVENB
;align 2

DSKDEV:
		;DD	COM1DEV
		DW	COM1DEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		;DW	0000100001000000B	; DOS 3.2
		DW	0		; 08/04/2018 (IBMDOS 2.1)
		DW	STRATEGY		; strategy routine
		DW	DSK$IN			; Interrupt entry point

;
; maximum number of drives
;

DRVMAX:
		DB	4

;
; Last drive accessed
;

		; 09/04/2018
;MEDIACHK_DRV:  ;; 13/04/2018
;		DB	0
		
TIM_LO:
		Dw	0FFFFh
TIM_HI:
		Dw	0FFFFh
WRTVERIFY:
RFLAG:
		DB	ROMRead		;2 for read, 3 for write
VERIFY:
		DB	0		;1 if verify after write
Single:
		DB	0
CURSECBUF:
		DB	0
SECCNT:
		DW	0
HARDNUM:
		DB	99		;logical drive number of first hardfile	
DRVNUM:
		DB	0
CURHD:
		DB	0
CURSEC:
		DB	0
CURTRK:
		DW	0
SPSAV:
		DW	0

;
; Device Header for device "COM1"
;

	;EVENB

align 2

COM1DEV:
		;DD	LPT1DEV
		DW	LPT1DEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		DW	1000000000000000B ; attribute word, character device
		DW	STRATEGY	; device strategy routine
		DW	AUX0$IN 	; device interrupt routine
		DB	'COM1    '	; device name


;
; Device Header for device LPT1
;

	;EVENB
;align 2

LPT1DEV:
		;DD	LPT2DEV
		DW	LPT2DEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		;DW	CHARDEV + OUTTILBUSY + DEV320
		DW	1000000000000000B ; 08/04/2018 (IBMDOS 2.1)
		DW	STRATEGY
		DW	PRN0$IN
		DB	'LPT1    '


;
; Device Header for device LPT2
;

	;EVENB
;align 2
LPT2DEV:
		;DD	LPT3DEV
		DW	LPT3DEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		;DW	CHARDEV + OUTTILBUSY + DEV320
		DW	1000000000000000B ; 08/04/2018 (IBMDOS 2.1)
		DW	STRATEGY
		DW	PRN1$IN
		DB	'LPT2    '

;
; Device Header for device LPT3
;

	;EVENB
;align 2

LPT3DEV:
		;DD	COM2DEV
		DW	COM2DEV
		DW	KERNEL_SEGMENT ; 27/03/2018
		;DW	CHARDEV + OUTTILBUSY + DEV320
		DW	1000000000000000B ; 08/04/2018 (IBMDOS 2.1)
		DW	STRATEGY
		DW	PRN2$IN
		DB	'LPT3    '


;
; Device Header for device "COM2"
;

	;EVENB

;align 2

COM2DEV:
		Dw	-1 ; 08/04/2018 (IBMDOS 2.1)
		DW	KERNEL_SEGMENT
		DW	1000000000000000B	; attribute word, character device
		DW	STRATEGY		; device strategy routine
		DW	AUX1$IN 		; device interrupt routine
		DB	'COM2    '              ; device name


; Some floppy drives do not have changeline support.  The result is a
; large amount of inefficiency in the code.  A media-check always returns
; "I don`t know".  This cause DOS to reread the FAT on every access and
; always discard any cached data.
;    We get around this inefficiency by implementing a "Logical Door Latch".
; The following three items are used to do this.  The logical door latch is
; based on the premise that it is not physically possible to change floppy
; disks in a drive in under two seconds (most people take about 10).  The
; logical door latch is implemented by saving the time of the last successful
; disk operation (in the value TIM_DRV).  When a new request is made the
; current time is compared to the saved time.  If less than two seconds have
; passed then the value "No Change" is returned.  If more than two seconds
; have passed the value "Don't Know" is returned.
;    There is one complecation to this algorithm.  Some programs change the
; value of the timer.  In this unfortunate case we have an invalid timer.
; This possiblity is detected by counting the number of disk operations
; which occur without any time passing.  If this count exceeds the value of
; "AccessMax" we assume the counter is invalid and always return "Don't
; Know".  The variable "AccessCount" is used to keep track of the number
; of disk operation which occur without the time changing.
;

AccessCount:
		DB	0		; number of times media check called
TIM_DRV:
		DB	-1		; time when last disk I/O was performed
;
;FlagBits:
;		DW	0		; Bits to set in flag field when doing
;					; a Set_Changed_DL
;
;MedByt:
;		DB	0		; hold media byte from floppy
;
;		;EVENB
;align 2
;WRTVERIFY:
;RFLAG:
;		DB	ROMRead		;2 for read, 3 for write
;VERIFY:
;		DB	0		;1 if verify after write
;
;SECCNT:
;		DW	0
;
;
;HARDNUM:
;		DB	99		;logical drive number of first hardfile

;
; Some of the older versions of the IBM rom-bios always assumed a seek would
; have to be made to read the diskette.  Consequently a large head settle
; time was always used in the I/O operations.  To get around this problem
; we need to continually adjust the head settle time.  The following
; algorithm is used:
;
;   Get the current head settle value.
;   If it is 1, then
;	set slow = 15
;   else
;	set slow = value
;   ...
;   if we are seeking and writing then
;	use slow
;   else
;	use fast
;   ...
;   restore current head settle value
;

;MotorStartup:
;		DB	0			; value from table
;SettleCurrent:
;		DB	0			; value from table
;SettleSlow:
;		DB	0			; slow settle value
;
;NextSpeed:
; 		DB	0			; value of speed to be used
;
;Save_head_sttl:
;		DB	0			;used by READ_SECTOR routine
;

EOT:
		DB	9

;
; pointer to Disk Parameter Table
;
;
;		;EVENB
;align 2
;DPT:
;		dd	0

; 23/03/2018

;
; The following two sets of variables are used to hold values for
; disk I/O operations
; Keep the next two items contiguous - see IOCTL_Block for reason

;
;CURSEC:	
;		DB	0			; current sector
;CURHD:
;		DB	0			; current head
;CURTRK:
;		DW	0			; current track
;SPSAV:
;		DW	0			; save the stack pointer

;
; The following are used for IOCTL function calls
;

;FORMT_EOT:
;		DB	8			; EOT used for format
;HDNUM:
;		DB	0			; Head number
;TRKNUM:
;		DW	0			; Track being manipulated
;GAP_PATCH:
;		DB	50h			; Format gap patched into DPT

;
; Disk errors returned from the IBM rom
;

ERRIN:
		DB	80H			; no response
		DB	40H			; seek failure
		DB	10H			; bad CRC
		DB	8			; DMA overrun
		DB	6			; media change
		DB	4			; sector not found
		DB	3			; write attempt to write-protect disk
LSTERR:
		DB	0			; all other errors

;
; returned error code corresponding to above errors
;

ERROUT:
		DB	2			; no response
		DB	6			; seek failure
		DB	4			; bad CRC
		DB	4			; DMA overrun
		DB	15			; invalid media change
		DB	8			; sector not found
		DB	0			; write attempt on write-protect disk
		DB	12			; general error

NUMERR	EQU	ERROUT-ERRIN


align 2

;-------------------------------------------------------------
;
; DiskSector is a 512 byte sector into which the boot sector
; is read.  It is also used as read sector for DMA check for
; hard disk.

DiskSector:
		;db	11 dup(?)	   ; take care of 3 jump bytes plus OEM name.
		times	11 db 0
BPB_In_Sector:
		dw	0
SECPERCLUSINSECTOR:
		db	0
		dw	0
		db	0
		dw	0
		dw	0
mediabyte:
		db	0
		dw	0
		dw	0
		dw	0
		dw	0
		db	0
		;db	512-($-DiskSector) dup (?)
		times	512-($-DiskSector) db 0

	
;
; ALTAH is a single character buffer used to handle special keys.
;

ALTAH:
		DB	0			;Special key handling

;
; The following variable can be modified via IOCTL sub-function 16. In this
; way, the wait can be set to suit the speed of the particular printer being
; used. One for each printer device.
;

PRINTDEV:
		DB	0		; Index into following array

;EVENB
align 2

WAIT_COUNT:
		;DW	4 dup (50h)	; Array of Retry counts for printer
		times	4 dw 50h	 	

;
; DAYCNT is the number of days since 1-1-80.
; Each time the clock is read it is necessary to check if another day has
; passed.  The ROM only returns the day rollover once so if it is missed
; the time will be off by a day.
;

;EVENB
;align 2

DAYCNT:
		DW	0

;-----------------------------------------------------------------------------
; MSBIO1.ASM  - MSDOS 3.3 - 24/07/1987
;-----------------------------------------------------------------------------
; 23/03/2018 - Retro DOS v2.0

;
; The next nine equ's describe the offset into the request header for
; different information.  For example STATUS is in byte 3 of the request
; header (starting count at zero).
;

CMDLEN	equ	0			; length of this command
UNIT	equ	1			; sub unit specifier
CMD	equ	2			; command code
STATUS	equ	3			; status
MEDIA	equ	13			; media descriptor
TRANS	equ	14			; transfer address
COUNT	equ	18			; count of blocks or characters
START	equ	20			; first block to transfer
EXTRA	equ	22			; Usually pointer to Vol Id for error 15

;
; Strategy is the strategy entry point for all default bio device drivers.
; All that is done is to save the pointer to the request header in the
; variable PtrSav.
;

STRATEGY:
		mov	[CS:PTRSAV],BX
		mov	[CS:PTRSAV+2],ES
		retf

;------------------------------------------------------------------------------
;
;			Device entry point
;
; The following ten pieces of code are the interrupt entry points for the
; default device drivers.  These small pieces of code have two jobs.
;
;	1) Make SI point to the beginning of the proper command jump  table.
;	   SI must first be pushed to preserve original contents.
;	2) If the call is an AUX or a printer save the number of the
;	   request in AL.  AL is moved to AUXNUM below.
;

;
; Con device:
;
CON$IN:
		PUSH	SI
		MOV	SI,CONTBL
		JMP	SHORT ENTRY
AUX0$IN:
		PUSH	SI
		PUSH	AX
		XOR	AL,AL
		JMP	SHORT AUXENT
AUX1$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,1
		JMP	short AUXENT

AUXENT:
		MOV	SI,AUXTBL
		JMP	SHORT ENTRY1

PRN0$IN:
		PUSH	SI
		PUSH	AX
		XOR	AL,AL
		JMP	SHORT PRNENT
PRN1$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,1
		JMP	SHORT PRNENT
PRN2$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,2
PRNENT:
		MOV	SI,PRNTBL
		JMP	SHORT ENTRY1

TIM$IN:
		PUSH	SI
		MOV	SI,TIMTBL
		JMP	SHORT ENTRY

DSK$IN:
		PUSH	SI
		mov	SI,DSKTBL

;
;  This section is the prolog to all default device drivers.  All registers
; are saved, the registers are filled with information from the request header,
; and the routine from the jump table is called. Error checking is done
; to assure command code is valid. Before calling the routine in the
; jump table the register are:
;
;	AH = Media Descriptor
;	AL = Unit Code
;	BX = offset to PTRSAV (request header is therefore at DS:BX)
;	CX = count from request header
;	DX = start sector
;	ES:DI = transfer address
;	SI = points to jump table
;	DS = points to this segment
;
; Once the routine finishes its job it jumps back to one of the eight
; pieces of code below labeled Exit Points.
;

ENTRY:
		push	AX
ENTRY1:
		push	CX			; save all registers
		push	DX
		push	DI
		push	BP
		push	DS
		push	ES
		push	BX

		mov	[CS:AUXNUM],AL		; save choice of AUX/PRN device

		lds	BX,[CS:PTRSAV]		; get pointer to I/O packet
	
		mov	AL,[BX+UNIT]		;AL = UNIT CODE
		mov	AH,[BX+MEDIA]		;AH = MEDIA DESCRIP
		mov	CX,[BX+COUNT]		;CX = COUNT
		mov	DX,[BX+START]		;DX = START SECTOR

		xchg	DI,AX
		mov	AL,[BX+CMD]
		; 11/04/2018
		;cmp	AL,[CS:SI]		; is command code a valid number?
		;ja	SHORT CMDERR		; no, jump to handle error

		;XOR	AH,AH
		;CBW				; note that AL <= 15 means OK
		;shl	AX,1
		;add	SI,AX			; get SI to point to address of routine
		
		; 10/04/2018
		CMP	AL,11
		JA	SHORT CMDERR
		XOR	AH,AH
		ADD	SI,AX
		ADD	SI,AX

		xchg	AX,DI			; put proper value back into AX
		les	DI,[BX+TRANS]		; get ES:DI to point to transfer address
		push	CS			; get DS equal to CS
		pop	DS

		cld				; clear the direction flag
		jmp	WORD [SI]		; go to the command


;=====================================================
;=
;=	SUBROUTINES SHARED BY MULTIPLE DEVICES
;=
;=====================================================

;----------------------------------------------------------
;
;		Exit  Points
;
; All device driver call return through one of these eight
; pieces of code.  The code set error and status conditions
; and then restores the registers.
;
		
BUS$EXIT:			; device busy exit

		mov	AH,00000011b		; set error code
		jmp	SHORT ERR1
CMDERR:
		mov	AL,3			; unknown command error
ERR$CNT:
		;lds	BX,[CS:PTRSAV]
		lds	BX,[PTRSAV]	; 11/04/2018
		sub	[BX+COUNT],CX		;# of successful I/O's
ERR$EXIT:
		mov	AH,10000001b		; mark error and return
		jmp	SHORT ERR1

EXIT$ZER:
		;lds	BX,[CS:PTRSAV]
		lds	BX,[PTRSAV]		; IBMDOS 3.3 (IBMBIO.COM)		
		xor	AX,AX
		mov	[BX+COUNT],AX		; indicate no character read

EXIT:
		mov	AH,00000001b
ERR1:
		;lds	BX,[CS:PTRSAV]
		lds	BX,[PTRSAV]	; 11/04/2018
		mov	[BX+STATUS],AX		; mark operation complete

		pop	BX			; restore register and return
		pop	ES
		pop	DS
		pop	BP
		pop	DI
		pop	DX
		pop	CX
		pop	AX
		pop	SI
		retf


;-------------------------------------------------------------
;
; Chrout - write out character in AL using current attribute
;
;   called via int 29h
;

CHROUT	equ	29H

OUTCHR:
		; 08/04/2018 (IBMDOS 2.1, IBMBIO.COM) ; *
		sti ; *
		push	AX			; preserve affect registers
		;push	SI
		;push	DI
		;push	BP
		push	BX			;
		mov	AH,0Eh 			; set command to write a character
		;mov	BH,0			;
		;mov	BL,7			; set foreground color
		mov	BX,7
		int	10h			; call rom-bios
		pop	BX			;
		;pop	BP			; restore registers
		;pop	DI
		;pop	SI
		pop	AX
		iret

;----------------------------------------------
;
; Fill DX register with value in AUXNUM
;

GETDX:
		; IBMDOS 2.1
		;mov	dl, [AUXNUM]
		;xor	dh,dh

		; MSDOS 3.3
		mov	DX,[CS:AUXNUM]
		retn


;-----------------------------------------------------------------------------
; MSCON.ASM - MSDOS 3.3 - 24/07/1987
;-----------------------------------------------------------------------------
; 23/03/2018 - Retro DOS v2.0

;----------------------------------------------------------------
;								:
;	      C O N - CONSOLE DEVICE DRIVER			:
;								:
;								:
;   This file contains the Console Device Driver.  The		:
; console device driver sends characters to the moniter and	:
; gets characters from the keyboard.				:
;								:
;----------------------------------------------------------------


;----------------------------------------------------------------
;								:
;		    Console read routine			:
;								:
;----------------------------------------------------------------
;

CON$READ:					; if CX is zero, no characters
		jcxz	CON$EXIT		;   to be read -- just exit
CON$LOOP:
		call	CHRIN			; get char in AL
		stosb				; store char at ES:DI, specified buffer
		loop	CON$LOOP		; if CX is non-zero more char to read
CON$EXIT:
		jmp	short EXIT		; all done, successful return


;----------------------------------------------------------------
;								:
;	    Input single character into AL			:
;								:
;----------------------------------------------------------------
CHRIN:
						; set command to read character
		xor	AX, AX			;  and clear AL
		xchg	AL,[ALTAH]		; get character & zero ALTAH
		or	AL, AL			; see if buffer has a character
		jnz	short KEYRET		; if so - return this character
						; if not - read single character
		int	16h			; call ROM-Bios keyboard routine
ALT10:
		or	AX,AX			; Check for non-key after BREAK
		jz	short CHRIN
		cmp	AX,7200h		; Check for CTRL-PRTSC
		jnz	short ALT15
		mov	AL,16			; indicate prtsc
ALT15:
		or	AL,AL			; special case?
		jnz	short KEYRET		; no, return with character
		mov	[ALTAH], AH		; yes, store special key
KEYRET:
		RETN

;----------------------------------------------------------------
;								:
;	   Keyboard non destructive read, no wait		:
;								:
; If bit 10 is set by the DOS in the status word of the request :
; packet, and there is no character in the input buffer, the	:
; driver issues a system WAIT request to the ROM. On return	:
; from the ROM, it returns a 'char-not-found' to the DOS.       :
;								:
;----------------------------------------------------------------

;CONBUSJ:
;		JMP	CONBUS

CON$RDND:
		mov	AL,[ALTAH]		; first see if there is a
		or	AL,AL			; character in the buffer?
		;jz	short RD1		; with debugging code it is
		;jmp	short RDEXIT		; too far for conditional jump
		jnz	short RDEXIT

RD1:				     		; set command to 'see if
		mov	AH, 1	     		; character available'
		int	16h	     		; call ROM-BIOS keyboard routine
		;jz	short nochr		; with debugging code it is
		;jz	short CONBUSJ
		;jz	short CONBUS
		jz	short BUS$EXIT	
		;jmp	short gotchr		; to far for conditional jump
;nochr:
		;cmp	short [fHaveK09], 0
		;jz	short CONBUSJ
		;lds	bx,[PTRSAV]		; get pointer to request header
		;test	word [bx+STATUS],0400h	; System WAIT enabled?
		;jz	short CONBUSJ 		; no, get out

		;mov	AX, 4100h     		; set command for Wait on External
				      		; event and condition type to
				      		; any external event
		;xor	BL,BL			; no timeout value
		;int	15h	      		; call rom-bios sleep function
		;jmp	short CONBUS		; after wait exit to con busy

gotchr:
		or	AX, AX			; check for null after break
		JNZ	short NOTBRK		; no, skip down
; note: AH is already zero, no need to set command
		int	16h ;SB 		; yes, read the null
		Jmp	short CON$RDND		; and get a real status

NOTBRK:
		cmp	AX, 7200H		; check for ctrl-prtsc
		jnz	short RDEXIT		; no
		mov	AL, 16			; yes, indicate ctrl-prtsc

RDEXIT:
		lds	BX, [PTRSAV]		; get pointer to request header
		mov	[BX+MEDIA], AL		; move character into req. header
EXVEC:
		jmp	short EXIT		; all done -- successful return
;CONBUS:
;		Jmp	short BUS$EXIT		; done -- con device is busy


;----------------------------------------------------------------
;								:
;		Keyboard flush routine				:
;								:
;----------------------------------------------------------------

CON$FLSH:
		call	FLUSH
		jmp	EXIT

FLUSH:
		mov	byte [ALTAH], 0	; clear out holding buffer

FlLoop:
				 	; Is there a char there?
		mov	AH, 1	 	; command code for check status
		int	16h	 	; call rom-bios keyboard routine
					; if z flag is set then no character
		jz	short FlDone	; is ready, buffer is empty -- get out
		xor	AH, AH	 	; if zf is nof set, get character
		int	16h	 	; call rom-bios to get character
		jmp	short FlLoop	; repeat until buffer is empty
FlDone:
		retn


;----------------------------------------------------------------
;								:
;	       Console Write Routine				:
;								:
;----------------------------------------------------------------
	
CON$WRIT:
		jcxz	EXVEC			; if CX is zero, get out
CON$LP:
		mov	AL,[ES:DI]		; get character
		inc	DI			; point to next character
		;push	cx
		;push	di
		int	CHROUT			; Output character
		loop	CON$LP			; repeat until all through
		;pop	di
		;pop	cx
		jmp	EXIT



;-----------------------------------------------
;
;	BREAK KEY HANDLING
;

CBREAK:
		mov	byte [CS:ALTAH], 3	; indicate break key set
		;nop
INTRET:
		IRET

;-----------------------------------------------------------------------------
; MSAUX.ASM - MSDOS 3.3 - 24/07/1987
;-----------------------------------------------------------------------------
; 23/03/2018 - Retro DOS v2.0

;----------------------------------------------------------------
;								:
;	     A U X - AUXILARY DEVICE DRIVER			:
;								:
;								:
;   This file contains the Auxilary Device Driver.  The 	:
; auxilary driver handles calls to and from the RS-232 port.	:
; Three devices uses this code: AUX, COM1, and COM2.  AUX and	:
; COM1 talk to the zero RS-232 card and COM2 talks to the	:
; 'one' RS-232 card.  The beginning of the interrupt entry      :
; point for these devices sets the variable AUXNUM in the	:
; msbio.asm module.  If the value is 0 the routines in this	:
; file will talk to the the 'zero' card.  If the value in       :
; AUXNUM is 1 the routines will talk to the 'one' card.         :
; The procedure GETDX is called to put the value 0 or 1 in	:
; the DX register depending on the value in AUXBUF.		:
;								:
;   The routines in this files are:				:
;								:
;	routine 		function			:
;	------- 		--------			:
;	AUX$READ		Read characters from the	:
;				  specified device.		:
;	AUX$RDND		Non-desrucrtive read with	:
;				  no waiting.			:
;	AUX$FLSH		Flush specified device input	:
;				  buffer.			:
;	AUX$WRIT		Write characters to the 	:
;				  specified device.		:
;	AUX$WRST		Get status of specified 	:
;				  device			:
;								:
;  These routines are not called directly.  Call are made via	:
; the strategy and interrupt entry point (see Device Header).	:
;								:
;  Data structure:						:
;    The Aux Device has a two byte buffer called AUXBUF.  The	:
;  first byte is for the zero card, the second byte is for the	:
;  one card.  A zero value in the byte indicates the buffer is	:
;  empty.  The routines use GETBX to get the address of the	:
;  buffer.							:
;								:
;----------------------------------------------------------------

;		VALUES IN AH, REQUESTING FUNCTION OF INT 14H IN ROM BIOS
AUXFUNC_SEND	 EQU	1	;TRANSMIT
AUXFUNC_RECEIVE  EQU	2	;READ
AUXFUNC_STATUS	 EQU	3	;REQUEST STATUS

;		ERROR FLAGS, REPORTED BY INT 14H

;	 THESE FLAGS REPORTED IN AH:
FLAG_DATA_READY  EQU	01H	;DATA READY
FLAG_OVERRUN	 EQU	02H	;OVERRUN ERROR
FLAG_PARITY	 EQU	04H	;PARITY ERROR
FLAG_FRAME	 EQU	08H	;FRAMING ERROR
FLAG_BREAK	 EQU	10H	;BREAK DETECT
FLAG_TRANHOL_EMP EQU	20H	;TRANSMIT HOLDING REGISTER EMPTY
FLAG_TRANSHF_EMP EQU	40H	;TRANSMIT SHIFT REGISTER EMPTY
FLAG_TIMEOUT	 EQU	80H	;TIMEOUT

;	THESE FLAGS REPORTED IN AL:
FLAG_DELTA_CTS	 EQU	01H	;DELTA CLEAR TO SEND
FLAG_DELTA_DSR	 EQU	02H	;DELTA DATA SET READY
FLAG_TRAIL_RING  EQU	04H	;TRAILING EDGE RING INDICATOR
FLAG_DELTA_SIG	 EQU	08H	;DELTA RECEIVE LINE SIGNAL DETECT
FLAG_CTS	 EQU	10H	;CLEAR TO SEND
FLAG_DSR	 EQU	20H	;DATA SET READY
FLAG_RING	 EQU	40H	;RING INDICATOR
FLAG_REC_SIG	 EQU	80H	;RECEIVE LINE SIGNAL DETECT


;----------------------------------------------------------------
;								  :
;	Read zero or more characters from Auxilary Device	  :
;								  :
;	input:es:[di] points to area to receive aux data	  :
;	      cx has number of bytes to be read 		  :
;	      "auxnum" first byte has number of aux device (rel 0):
;								  :
;----------------------------------------------------------------

AUX$READ:
		jcxz	EXVEC2		; if no characters, get out
		call	GETBX		; put address of AUXBUF in BX
		xor	AX,AX		; clear AX register
		xchg	AL,[BX] 	; Get character , if any, from
					;   buffer and clear buffer
		or	AL,AL		; if AL is nonzero there was a
					;   character in the buffer
		jnz	short AUX2	; if so skip AUXIN call
AUX1:					;
		call	AUXIN		; get character from port
AUX2:					;
		stosb			; store character
		loop	AUX1		; if more character, go around again
EXVEC2: 				;
		jmp	EXIT		; all done, successful exit

;
; AUXIN: make a call on ROM BIOS to read character from
;	 the auxilary device, then do some error checking.
;	 If an error occurs then AUXIN jumps to ERR$CNT and
;	 does NOT return to where it was called from.
;

AUXIN:
		mov	ah,AUXFUNC_RECEIVE
		call	AUXOP
		 			;check for Frame, Parity, or Overrun errors
		 			;WARNING: these error bits are unpredictable 
		 			;         if timeout (bit 7) is set
		test	ah,FLAG_FRAME | FLAG_PARITY | FLAG_OVERRUN
		jz	short AROK	;No error if all bits are clear

		;Error getting character
		;add	sp,+2		;Remove rtn address (near call)
		pop	ax
		xor	al,al
		or	al,FLAG_REC_SIG | FLAG_DSR | FLAG_CTS

		jmp	ERR$CNT
AROK:
		RETN			;CHAR JUST READ IS IN AL, STATUS IS IN AH

;----------------------------------------------------------------
;								:
;	Aux non-destructive read with no waiting		:
;								:
;	input: es:[di] points to area to receive aux data	:
;								:
;----------------------------------------------------------------
;

AUX$RDND:
		call	GETBX		; have BX point to AUXBUF
		mov	AL,[BX] 	; copy contents of buffer to AL
		or	AL,AL		; if AL is non-zero (char in buffer)
		jnz	short AUXRDX	;   then return character
		call	AUXSTAT 	;   if not, get status of AUX device
		TEST	AH,FLAG_DATA_READY ;TEST DATA READY
		jz	short AUXBUS	;   then device is busy (not ready)

		TEST	AL,FLAG_DSR	;TEST DATA SET READY
		jz	short AUXBUS	;   then device is busy (not ready)
		call	AUXIN		;   else aux is ready, get character
		call	GETBX		; have bx point to AUXBUF
		mov	[BX],AL 	; save character in buffer
AUXRDX: 				;
		jmp	short RDEXIT	; return character

AUXBUS: 				;
		Jmp	BUS$EXIT	; jump to device busy exit

;----------------------------------------------------------------
;								:
;		Aux Output Status				:
;								:
;----------------------------------------------------------------

AUX$WRST:
		call	AUXSTAT 	; get status of AUX in AX
					; now test to see if device is busy
					; if this bit is not set,
		TEST	AL,FLAG_DSR	;TEST DATA SET READY
		jz	short AUXBUS	;   then device is busy (not ready)
		TEST	AH,FLAG_TRANHOL_EMP ;TEST TRANSMIT HOLD REG EMPTY
		jz	short AUXBUS	;   then device is busy (not ready)
		jmp	EXIT

;
; AUXSTAT makes a call on the ROM-BIOS to determine the status
;	  of the auxilary device
;	  Outputs:
;		AX is filled with status of port.
;		DX is changes to specify which card - either 0, 1 (, 2, 3) ;ba
;		NO other registers are modified
;

AUXSTAT:
		mov	ah,AUXFUNC_STATUS
		;call	AUXOP
		;retn
AUXOP:					;AH=FUNCTION CODE
					;0=INIT, 1=SEND, 2=RECEIVE, 3=STATUS
		call	GETDX		; have DX point to proper card
		int	14h		; call rom-bios for status
		retn

;----------------------------------------------------------------
;								:
;  Flush AUX Input buffer - set contents of AUXBUF to zero	:
;								:
;----------------------------------------------------------------

AUX$FLSH:
		call	GETBX		; get BX to point to AUXBUF
		mov	BYTE [BX],0	; zero out buffer
		jmp	EXIT		; all done, successful return
	
;----------------------------------------------------------------
;								:
;		Write to Auxilary Device			:
;								:
;----------------------------------------------------------------

AUX$WRIT:
		jcxz	EXVEC2		; if CX is zero, no characters
				;   to be written, jump to exit
AUX$LOOP:
		mov	AL,[ES:DI]	; get character to be written
		inc	DI		; move DI pointer to next character
		MOV	AH,AUXFUNC_SEND ;VALUE=1, INDICATES A WRITE
		CALL	AUXOP		;SEND CHARACTER OVER AUX PORT

		TEST	AH,FLAG_TIMEOUT ;CHECK FOR ERROR
		jz	short AWOK	;   then no error
		mov	AL,10		;   else indicate write fault
		jmp	ERR$CNT 	; call error routines

					; if CX is non-zero, still more
AWOK:
		loop	AUX$LOOP	; more characrter to print
		jmp	EXIT		; all done, successful return


;
;  GETBX puts the address of AUXBUF (the Auxilary Device buffer)
;	 in BX.  After calling GETBX, a routine can get to AUXBUF
;	 with [BX].
;
;  NOTE: The getdx routine is in msbio1 and looks like:
;	mov	dx,word ptr cs:[auxnum]
;

GETBX:
		call	GETDX
		mov	BX,DX
		add	BX,AUXBUF
		retn

;-----------------------------------------------------------------------------
; MSLPT.ASM - MSDOS 3.3 - 24/07/1987
;-----------------------------------------------------------------------------
; 23/03/2018 - Retro DOS v2.0

;----------------------------------------------------------------
;								:
;	P R N - PRINTER DEVICE					:
;								:
;								:
;   This file contains the Printer Device Driver.  The		:
; printer driver handles calls to the printers.  Four devices	:
; use this code: PRN, LPT1, LPT2, and LPT3.  The beginning	:
; of the interrupt entry point for these device sets the	:
; variable AUXNUM in the msbio.asm module.  The number is	:
; in AUXNUM dictates which device will to written to: 0 for	:
; PRN and LPT1, 1 for LPT2, and 2 for LPT3.			:
;								:
;   The routines in this files are:				:
;								:
;	routine 		function			:
;	------- 		--------			:
;	PRN$WRIT		Write to printer device 	:
;	PRN$STAT		Printer status routine		:
;	PRN$TilBusy		Print spooler routine		:
;	Prn$GenIOCTL		Generic IOCTL routine		:
;								:
;  These routines are not called directly.  Call are made via	:
; the strategy and interrupt entry point (see Device Header).	:
;								:
;----------------------------------------------------------------

; IBM ROM STATUS BITS (I DON'T TRUST THEM, NEITHER SHOULD YOU)             
									   
NOTBUSYSTATUS	equ 10000000b		; NOT BUSY			   
ACKSTATUS	equ 01000000b		; ACKNOWLEDGE (FOR WHAT?)	   
NOPAPERSTATUS	equ 00100000b		; NO MORE PAPER 		   
SELECTEDSTATUS	equ 00010000b		; THE PRINTER SAID IT WAS SELECTED 
IOERRSTATUS	equ 00001000b		; SOME KINDA ERROR		   
RESERVED	equ 00000110b		; NOPS				   
TIMEOUTSTATUS	equ 00000001b		; TIME OUT.			   
									   
									   
; WARNING!!!  THE IBM ROM DOES NOT RETURN JUST ONE BIT.  IT RETURNS A	   
; WHOLE SLEW OF BITS, ONLY ONE OF WHICH IS CORRECT.			   
									   
;----------------------------------------------------------------
;								:
;		WRITE TO PRINTER DEVICE 			:
;								:
;   CX has count of bytes to be printed 			:
;   ES:DI point to source buffer contains characters		:
;   AuxNum (in msbio.asm) has printer number			:
;								:
;----------------------------------------------------------------
						   
PRN$WRIT:
		jcxz	EXVEC3			; no chars to output, Get out
PRN$LOOP:
		mov	BX,2			; Initialize retry flag
PRN$out:
		mov	AL,[ES:DI]		; Get a character into AL
		inc	DI			; Point to next character
		XOR	AH,AH			; AH=0 => OUTPUT CHAR IN DL	   
		call	PRNOP			; print character
		jnz	short PrRetry 		; if error, try to print again
		loop	PRN$LOOP		; if more character, keep printing
EXVEC3:
		jmp	EXIT

PrRetry:
		dec	DI			; undo the inc above...
		dec	BX			; Decrement retry count
		jnz	short PRN$out 		; See if done with retrys
PMESSG:
		JMP	ERR$CNT 		; if so return with the error

;----------------------------------------------------------------
;								:
;		PRINTER STATUS ROUTINE				:
;								:
;----------------------------------------------------------------
;
						   
PRN$STAT:
		call	PRNSTAT 		; get the status
		jnz	short PMESSG		; if error jump to error routine
		MOV	AL,9			; AGAIN, ASSUME OUT OF PAPER...    
		TEST	AH,NOPAPERSTATUS					   
		JNZ	short PMESSG							   
		TEST	AH,NOTBUSYSTATUS					   
		jnz	short EXVEC3		; if not busy return via EXVEC3
		JMP	BUS$EXIT		; else busy, return to busy exit   

;
;   PRNSTAT	get printer status
;   PRNOP	print a character
;
; PRNSTAT and PRNOP are two routines which call on the ROM-BIOS
; printer routines.  The routines share code which calls on the bios and
; then determines which, if any, error occured. PRNSTAT and PRNOP differ
; only by the value put into AH before the ROM-BIOS call.
;
;   INPUT	if PRNOP then character in AL
;
;   OUTPUT	- AL holds error code
;		- AH status byte from printer
;		- flag NZ if error

PRNSTAT:						   
		mov	AH,2			; set command for get status   *
PRNOP:
		call	GETDX			; determine which printer      *
		int	17h			; call ROM-BIOS printer routine  *

		TEST	AH,IOERRSTATUS		; I/O ERROR?			   
		JZ	CHECKNOTREADY		; NO, TRY NOT READY		   
									   
; AT THIS POINT, WE KNOW WE HAVE AN ERROR. THE CONVERSE IS NOT TRUE.	   
									   
		MOV	AL,9			; FIRST, ASSUME OUT OF PAPER	   
		TEST	AH,NOPAPERSTATUS	; OUT OF PAPER SET?		   
		JNZ	short RET1			; YES, ERROR IS SET		   
		INC	AL			; INDICATE I/O ERROR		   
RET1:									   
									   
; WE HAVE TRIAGED NOW FOR OUT OF PAPER AND IO ERR (IGNORING TIME-OUT)	   
									   
		RETN				; RETURN WITH ERROR		   
									   
; THE BITS SAID NO ERROR.  UNFORTUNATELY, THERE MAY BE OTHER THINGS AT WOR  K
; HERE. 								   
									   
CHECKNOTREADY:								   
		MOV	AL,2			; ASSUME NOT-READY		   
		TEST	AH,TIMEOUTSTATUS	; IS TIME-OUT SET?		   
						; IF NZ THEN ERROR, ELSE OK???	   
PRNOP2: 								   
		RETN								   

;-----------------------------------------------------------------------------
; MSCLOCK.ASM - MSDOS 3.3 - 24/07/1987
;-----------------------------------------------------------------------------
; 23/03/2018 - Retro DOS v2.0

;----------------------------------------				  
;	CMOS EQUATES FOR THIS SYSTEM	:  ; 26/03/2018 - CMOSEQU.INC, 1987				  
;-----------------------------------------------------------------------------
CMOS_PORT	EQU	070H		; I/O ADDRESS OF CMOS ADDRESS PORT	 
CMOS_DATA	EQU	071H		; I/O ADDRESS OF CMOS DATA PORT 	 
NMI		EQU	10000000B	; DISABLE NMI INTERRUPTS MASK - 	 
					;  HIGH BIT OF CMOS LOCATION ADDRESS
;---------- CMOS TABLE LOCATION ADDRESS'S ## --------------------------------- 
CMOS_SECONDS	EQU	000H		; SECONDS				 
CMOS_SEC_ALARM	EQU	001H		; SECONDS ALARM  ## NOTE:  ALL LOCATIONS 
CMOS_MINUTES	EQU	002H		; MINUTES	      | IN THE CMOS AREA 
CMOS_MIN_ALARM	EQU	003H		; MINUTES ALARM       | ARE IBM USE ONLY 
CMOS_HOURS	EQU	004H		; HOURS 	      | AND  SUBJECT  TO 
CMOS_HR_ALARM	EQU	005H		; HOURS ALARM	      | CHANGE. ONLY THE 
CMOS_DAY_WEEK	EQU	006H		; DAY OF THE WEEK     | POST & BIOS CODE 
CMOS_DAY_MONTH	EQU	007H		; DAY OF THE MONTH    | SHOULD	DIRECTLY 
CMOS_MONTH	EQU	008H		; MONTH 	      | ACCESS LOCATIONS 
CMOS_YEAR	EQU	009H		; YEAR (TWO DIGITS)   | IN CMOS STORAGE. 
CMOS_REG_A	EQU	00AH		; STATUS REGISTER A   '----------------- 
CMOS_REG_B	EQU	00BH		; STATUS REGISTER B  ALARM		 
CMOS_REG_C	EQU	00CH		; STATUS REGISTER C  FLAGS		 
CMOS_REG_D	EQU	00DH		; STATUS REGISTER D  BATTERY		 
CMOS_DIAG	EQU	00EH		; POST DIAGNOSTIC STATUS RESULTS BYTE	 
CMOS_SHUT_DOWN	EQU	00FH		; SHUTDOWN STATUS COMMAND BYTE		 
CMOS_DISKETTE	EQU	010H		; DISKETTE DRIVE TYPE BYTE	      ;  
;		EQU	011H		; - RESERVED			      ;C 
CMOS_DISK	EQU	012H		; FIXED DISK TYPE BYTE		      ;H 
;		EQU	013H		; - RESERVED			      ;E 
CMOS_EQUIP	EQU	014H		; EQUIPMENT WORD LOW BYTE	      ;C 
CMOS_B_M_S_LO	EQU	015H		; BASE MEMORY SIZE - LOW BYTE (X1024) ;K 
CMOS_B_M_S_HI	EQU	016H		; BASE MEMORY SIZE - HIGH BYTE	      ;S 
CMOS_E_M_S_LO	EQU	017H		; EXPANSION MEMORY SIZE - LOW BYTE    ;U 
CMOS_E_M_S_HI	EQU	018H		; EXPANSION MEMORY SIZE - HIGH BYTE   ;M 
CMOS_DISK_1	EQU	019H		; FIXED DISK TYPE - DRIVE C EXTENSION ;E 
CMOS_DISK_2	EQU	01AH		; FIXED DISK TYPE - DRIVE D EXTENSION ;D 
;		EQU	01BH		; - 1BH THROUGH 2DH - RESERVED	      ;  
CMOS_CKSUM_HI	EQU	02EH		; CMOS CHECKSUM - HIGH BYTE	      ;* 
CMOS_CKSUM_LO	EQU	02FH		; CMOS CHECKSUM - LOW BYTE	      ;* 
CMOS_U_M_S_LO	EQU	030H		; USABLE MEMORY ABOVE 1 MEG - LOW BYTE	 
CMOS_U_M_S_HI	EQU	031H		; USABLE MEMORY ABOVE 1 MEG - HIGH BYTE  
CMOS_CENTURY	EQU	032H		; DATE CENTURY BYTE (BCD)		 
CMOS_INFO128	EQU	033H		; 128KB INFORMATION STATUS FLAG BYTE	 
;		EQU	034H		; - 34H THROUGH 3FH - RESERVED		 	

;----------------------------------------------------------------
;								:
;		    CLOCK DEVICE DRIVER 			:
;								:
;								:
;   This file contains the Clock Device Driver. 		:
;								:
;   The routines in this files are:				:
;								:
;	routine 		function			:
;	------- 		--------			:
;	TIM$WRIT		Set the current time		:
;	TIM$READ		Read the current time		:
;	Time_To_Ticks		Convert time to corresponding	:
;				  number of clock ticks 	:
;								:
; The clock ticks at the rate of:				:
;								:
;	1193180/65536 ticks/second (about 18.2 ticks per second):
; See each routine for information on the use.			:
;								:
;----------------------------------------------------------------

;
; DAYCNT is the number of days since 1-1-80.
; Each time the clock is read it is necessary to check if another day has
; passed.  The ROM only returns the day rollover once so if it is missed
; the time will be off by a day.
;

;; variables for real time clock setting
;HaveCMOSClock:
;		db	0	;set by MSINIT.
;base_century:
;		db	19
;base_year:
;		db	80
;month_tab:
;		db	31,28,31,30,31,30,31,31,30,31,30,31

; The following are indirect intra-segment call addresses. The
;procedures are defined in MSINIT for relocation.  MSINIT will set these
;address when the relocation is done.
	
;BinToBCD:
;		dw	0	;should point to Bin_To_BCD proc in MSINIT
;
;DaycntToDay:
;		dw	0	;should point to Daycnt_to_day in MSINIT

;********************************************************************
; Indirect call address of TIME_TO_TICKS procedure.
;This will be used by the relocatable portable suspend/resume code.

;TimeToTicks:
;		dw	TIME_TO_TICKS

;--------------------------------------------------------------------
;
; Settime sets the current time
;
; On entry ES:[DI] has the current time:
;
;	number of days since 1-1-80	(WORD)
;	minutes (0-59)			(BYTE)
;	hours (0-23)			(BYTE)
;	hundredths of seconds (0-99)	(BYTE)
;	seconds (0-59)			(BYTE)
;
; Each number has been checked for the correct range.
;

TIM$WRIT:
		mov	AX,[ES:DI]
		push	AX		;DAYCNT. We need to set this at the very
					;  end to avoid tick windows.
		;;;Rev 3.30 Modification
		;cmp	byte [HaveCMOSClock], 0
		;je	short No_CMOS_1
		;mov	al,[es:di+3]		;get binary hours
		;call	BinToBCD		;convert to BCD
		;mov	ch,al			;CH = BCD hours
		;mov	al,[es:di+2]		;get binary minutes
		;call	BinToBCD		;convert to BCD
		;mov	cl,al			;CL = BCD minutes
		;mov	al,[es:di+5]		;get binary seconds
		;call	BinToBCD		;convert to BCD
		;mov	dh,al			;DH = BCD seconds
		;mov	dl,0			;DL = 0 (ST) or 1 (DST)
		;cli				;turn off timer
		;mov	ah,03h			;set RTC time
		;int	1Ah			;call rom bios clock routine
		;sti
		;;;End of Modification
No_CMOS_1:
		mov	CX,[ES:DI+2]
		mov	DX,[ES:DI+4]
		;;Rev 3.30 Modification
		call	TIME_TO_TICKS		; convert time to ticks
						;CX:DX now has time in ticks
		cli				; Turn off timer
		mov	AH, 1			; command is set time in clock
		int	1Ah			; call rom-bios clock routines
		pop	word [DAYCNT]
		sti
		;;CMOS clock -------------------------------------
		;cmp	byte [HaveCMOSClock], 0
		;je	short No_CMOS_2
		;call	DaycntToDay		; convert to BCD format
		;cli				; Turn off timer
		;mov	AH,05h			; set RTC date
		;int	1Ah			; call rom-bios clock routines
		;sti
		;------------------------------------------------
No_CMOS_2:
		jmp	EXIT
		;;End of Modification

;
; convert time to ticks
; input : time in CX and DX
; ticks returned in CX:DX
;

TIME_TO_TICKS:
		; first convert from Hour,min,sec,hund. to
		; total number of 100th of seconds
		mov	AL,60
		mul	CH		;Hours to minutes
		mov	CH,0
		add	AX,CX		;Total minutes
		mov	CX,6000 	;60*100
		mov	BX,DX		;Get out of the way of the multiply
		mul	CX		;Convert to 1/100 sec
		mov	CX,AX
		mov	AL,100
		mul	BH		;Convert seconds to 1/100 sec
		add	CX,AX		;Combine seconds with hours and min.
		adc	DX,0		;Ripple carry
		mov	BH,0
		add	CX,BX		;Combine 1/100 sec
		adc	DX,0

		;;Rev 3.30 Modification
		;DX:CX IS TIME IN 1/100 SEC
		XCHG	AX,DX
		XCHG	AX,CX		;NOW TIME IS IN CX:AX
		MOV	BX,59659
		MUL	BX		;MULTIPLY LOW HALF
		XCHG	DX,CX
		XCHG	AX,DX		;CX->AX, AX->DX, DX->CX
		MUL	BX		;MULTIPLY HIGH HALF
		ADD	AX,CX		;COMBINE OVERLAPPING PRODUCTS
		ADC	DX,0
		XCHG	AX,DX		;AX:DX=TIME*59659
		MOV	BX,5
		DIV	BL		;DIVIDE HIGH HALF BY 5
		MOV	CL,AL
		MOV	CH,0
		MOV	AL,AH		;REMAINDER OF DIVIDE-BY-5
		CBW
		XCHG	AX,DX		;USE IT TO EXTEND LOW HALF
		DIV	BX		;DIVIDE LOW HALF BY 5
		MOV	DX,AX
			; CX:DX is now number of ticks in time
		retn

;
; Gettime reads date and time
; and returns the following information:
;
;	ES:[DI]  =count of days since 1-1-80
;	ES:[DI+2]=hours
;	ES:[DI+3]=minutes
;	ES:[DI+4]=seconds
;	ES:[DI+5]=hundredths of seconds
;

TIM$READ:				; read the clock
		xor	AH, AH		; set command to read clock
		int	1Ah		; call rom-bios to get time

		or	al,al		; check for a new day
		jz	short noroll1 	; if al=0 then don't reset day count
		INC	word [DAYCNT]	; CATCH ROLLOVE
noroll1:
		MOV	SI,[DAYCNT]

;
; we now need to convert the time in tick to the time in 100th of
; seconds.  The relation between tick and seconds is:
;
;		 65536 seconds
;	       ----------------
;		1,193,180 tick
;
; To get to 100th of second we need to multiply by 100. The equation is:
;
;	Ticks from clock  * 65536 * 100
;      ---------------------------------  = time in 100th of seconds
;		1,193,180
;
; Fortunately this formula simplifies to:
;
;	Ticks from clock * 5 * 65,536
;      --------------------------------- = time in 100th of seconds
;		59,659
;
; The calculation is done by first multipling tick by 5. Next we divide by
; 59,659.  In this division we multiply by 65,536 by shifting the dividend
; my 16 bits to the left.
;
; start with ticks in CX:DX
; multiply by 5
		MOV	AX,CX
		MOV	BX,DX
		SHL	DX,1
		RCL	CX,1		;TIMES 2
		SHL	DX,1
		RCL	CX,1		;TIMES 4
		ADD	DX,BX
		ADC	AX,CX		;TIMES 5
		XCHG	AX,DX		
	

; now have ticks * 5 in DX:AX
; we now need to multiply by 65,536 and divide by 59659 d.

		mov	CX,59659	; get divisor
		div	CX
					; DX now has remainder
					; AX has high word of final quotient
		mov	BX,AX		; put high work if safe place
		xor	AX,AX		; this is the multiply by 65536
		div	CX		; BX:AX now has time in 100th of seconds

;
;Rounding based on the remainder may be added here
;The result in BX:AX is time in 1/100 second.
		mov	DX,BX
		mov	CX,200		;Extract 1/100's
;Division by 200 is necessary to ensure no overflow--max result
;is number of seconds in a day/2 = 43200.
		div	CX
		cmp	DL,100		;Remainder over 100?
		jb	short NOADJ
		sub	DL,100		;Keep 1/100's less than 100
NOADJ:
		cmc			;If we subtracted 100, carry is now set
		mov	BL,DL		;Save 1/100's
;To compensate for dividing by 200 instead of 100, we now multiply
;by two, shifting a one in if the remainder had exceeded 100.
		rcl	AX,1
		mov	DL,0
		rcl	DX,1
		mov	CX,60		;Divide out seconds
		div	CX
		mov	BH,DL		;Save the seconds
		div	CL		;Break into hours and minutes
		xchg	AL,AH

;Time is now in AX:BX (hours, minutes, seconds, 1/100 sec)

		push	AX
		MOV	AX,SI		; DAYCNT
		stosw
		pop	AX
		stosw
		mov	AX,BX
		stosw
		jmp	EXIT

;-----------------------------------------------------------------------------
; MSDISK.ASM (1) - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 23/03/2018 - Retro DOS v2.0

;------------------------------------------------------------------------
;									:
;	       DISK INTERFACE ROUTINES					:
;									:
;									:
;   This file contains the Disk Device Driver.				:
;									:
;   The routines in this files are:					:
;									:
;	routine 		function				:
;	------- 		--------				:
;									:
;	MEDIA$CHK		Determine if media in drive has changed :
;									:
;	GET$BPB 		Build a valid BPB for drive		:
;									:
;	DSK$REM 		Determine if disk has removable media	:
;									:
;	DSK$WRTV		Disk write with verify			:
;									:
;	DSK$WRT 		Disk write				:
;									:
;	DSK$READ		Read disk				:
;									:
;									:
;  These routines are not called directly.  Call are made via		:
; the strategy and interrupt entry point (see Device Header).		:
;									:
;  Data structures:							:
;	There are two main types of data structures associated with	:
;  the disk drives.  The first is the BDS.  BDS is the Bios Data	:
;  structure.  There is one BDS for each logical drive in the system.	:
;  All the BDS's are linked together in a list with the pointer to the  :
;  first BDS being found in START_BDS.	The BDS hold various values	:
;  important to the disk drive.  For example there is a field for last	:
;  time accesses.  As actions take place in the system the BDS are	:
;  update to reflect the actions.  For example if there is a read to	:
;  a disk the last access field for the BDS for that drive is updated	:
;  to the current time. 						:
;	 The second data structure associated with disk drives is the	:
;  BPB.  A BPB is a Bios Parameter Block.  The BPB contains information :
;  about the media inside a disk drive.  Some on the fields in the BPB	:
;  are Sectors per track, number of FATs, and number of tracks.  This	:
;  information is used to tell where sectors are on the disk.  For	:
;  example,  if we need to read logical sector 52:			:
;									:
;	Diskette			Track	Sector	Side		:
;    single density							:
;    eight sectors per track		  6	   5	  0		:
;									:
;    double density							:
;    nine sectors per track		  2	   7	  1		:
;									:
;  The BPB for the media in the drive is stored in the BDS for the	:
;  drive.  If the user changes the floppy in the drive a call is	:
;  made to GET$BPB to build a new BPB in the BDS.  See this routine	:
;  for the algorithm.							:
;									:
;									:
;------------------------------------------------------------------------

;
; Maximum number of retries in case of error
;

MAXERR	EQU	5
LSTDRV	EQU	0504h

;
; Some floppy drives do not have changeline support.  The result is a
; large amount of inefficiency in the code.  A media-check always returns
; "I don`t know".  This cause DOS to reread the FAT on every access and
; always discard any cached data.
;    We get around this inefficiency by implementing a "Logical Door Latch".
; The following three items are used to do this.  The logical door latch is
; based on the premise that it is not physically possible to change floppy
; disks in a drive in under two seconds (most people take about 10).  The
; logical door latch is implemented by saving the time of the last successful
; disk operation (in the value TIM_DRV).  When a new request is made the
; current time is compared to the saved time.  If less than two seconds have
; passed then the value "No Change" is returned.  If more than two seconds
; have passed the value "Don't Know" is returned.
;    There is one complecation to this algorithm.  Some programs change the
; value of the timer.  In this unfortunate case we have an invalid timer.
; This possiblity is detected by counting the number of disk operations
; which occur without any time passing.  If this count exceeds the value of
; "AccessMax" we assume the counter is invalid and always return "Don't
; Know".  The variable "AccessCount" is used to keep track of the number
; of disk operation which occur without the time changing.
;

AccessMax	equ	5

;
; Some of the older versions of the IBM rom-bios always assumed a seek would
; have to be made to read the diskette.  Consequently a large head settle
; time was always used in the I/O operations.  To get around this problem
; we need to continually adjust the head settle time.  The following
; algorithm is used:
;
;   Get the current head settle value.
;   If it is 1, then
;	set slow = 15
;   else
;	set slow = value
;   ...
;   if we are seeking and writing then
;	use slow
;   else
;	use fast
;   ...
;   restore current head settle value
;
;
; flags for size of FAT
;

;fTOOBIG 	EQU	80h
;fBIG		EQU	40h

error_unknown_media	equ	7	; for use in BUILD BPB call

struc BPB_TYPE
.SECSIZE:	resw 1
.SECALL:	resb 1
.RESNUM:	resw 1
.FATNUM:	resb 1
.DIRNUM:	resw 1
.SECNUM:	resw 1
.FATID:		resb 1
.FATSIZE:	resw 1
.SLIM:		resw 1
.HLIM:		resw 1
.HIDDEN:	resw 1
.size:
endstruc

;------------------------------------------------------------------------
;									:
; The next 100 or so lines of code do the Media Check.	Media Check	:
; determines if the diskette (media) in the drive has been changed.	:
;									:
;	SI is used to hold media check code:				:
;			-1	media changed				:
;			 0	Don't know                              :
;			 1	media has not been changed		:
;									:
; The algorithm used is a follows:					:
;	if (hard disk)							:
;	    if (changed by format)					:
;		   return (not changed) 				:
;	    if	not (changed by format) 				:
;		   return (changed)					:
;	else we have a floppy						:
;	    if floppy has change line support go ask the floppy 	:
;	    if floppy does not have change line do the following	:
;		read the time						:
;		if more than two second have passed return don't know   :
;		if no time has passed then might be unreliable		:
;		  counter (some program fool with the counter when	:
;		  they should not).  See note below for procedure with	:
;		  unreliable counter					:
;		if sometime has passed but not two second return	:
;		  media has not changed.  This is based on the		:
;		  assumption that it is not physically possible to	:
;		  change a disk in less the two seconds (most people	:
;		  take about 10 seconds).				:
;									:
;------------------------------------------------------------------------

MEDIA$CHK:
		; 08/04/2018
		; Retro DOS v2.0
		; (Media check code here is mix of MSDOS 2.0, MSDOS 3.3
		; and Retro DOS v2.0 special media -disk change- check code..)
		; (..it is not compatible with MSDOS 3.3 mediacheck)
		; (Excluded methods: Volume Serial Number Check,
		; "fChanged_By_Format" check -via int 13h hook-)

		xor	si, si  ; Presume "I don't know"

		;cmp	AL, 1	; Retro DOS v2.0 method
				; We will not check disk change status
				; if disk/unit number > 1.
				; (hard disk or diskette 3 ! or diskette 4 !)
		;ja	short Media$Done

		cmp	al, [HARDNUM]
		jnb	short Media$Done ; fixed/hard disk !

;
; If this code is reached disk is a diskette drive
;

WeHaveAFloppy:
		;mov	[CS:MEDIACHK_DRV], al ; Retro DOS v2.0 method 
		; 13/04/2018
		;mov	[MEDIACHK_DRV], al ; 11/04/2018

;
; If drive is a floppy with changeline support the rom is called to
; determine if the media has changed. It is not necessary to do the 2
; second check on these drives.
;
		CALL	MediaCheck

		; DL = drive number (0..3) ; 13/04/2018

		and	si, si
		jnz	short Media$Done

		; SI = 0, "I don't know" (if media has been changed or not!?)		

;
; If this code is reached the drive is a floppy with no changeline support
;
		MOV	SI,1			; PRESUME NO CHANGE
		;mov	al,[CS:TIM_DRV] 	; last drive accessed
						;is drive of last access the same?
		mov	al, [TIM_DRV]
		;CMP	AL,[CS:MEDIACHK_DRV]
		;cmp	al, [MEDIACHK_DRV] ; 11/04/2018
		CMP	AL, DL ; 13/04/2018
		JNE	short Media$Unk		; no, then return don't know
;
; CHECK TO SEE IF THIS DRIVE HAS BEEN ACCESSED IN THE LAST 2 SECONDS.
;
		call	CHECK_TIME_OF_ACCESS
		jmp	short Media$Done

Media$Unk:
		DEC	SI			; RETURN "I DON'T KNOW"

;
; SI now contains the correct value for media change. Clean up the left overs
;
Media$Done:
		;les	bx,[CS:PTRSAV]		; get original packet
		les	bx,[PTRSAV] ; 11/04/2018
		mov	[ES:BX+TRANS],SI
		or	SI,SI
		jns	EXIT

		; make sure we ask ROM for media check
		;mov	byte [CS:TIM_DRV],-1
		mov	byte [TIM_DRV],-1 ; 11/04/2018
		jmp	EXIT

;
; PERFORM A CHECK ON THE TIME PASSED SINCE THE LAST ACCESS FOR THIS
; PHYSICAL DRIVE.
; WE ARE ACCESSING THE SAME DRIVE.  IF THE TIME OF LAST SUCCESSFUL ACCESS
; WAS LESS THAN 2 SECONDS AGO, THEN WE MAY PRESUME THAT THE DISK WAS NOT
; CHANGED
; RETURNS IN SI:
;	0 - IF TIME OF LAST ACCESS WAS >= 2 SECONDS
;	1 - IF TIME WAS < 2 SECONDS (I.E NO MEDIA CHANGE ASSUMED)
; REGISTERS AFFECTED AX,CX,DX, FLAGS.
;

CHECK_TIME_OF_ACCESS:

		mov	si,1			; Presume no change
		;;Rev 3.30 Modification
		xor	AH, AH			; set command to read time
		int	1Ah			; call rom-bios clock routine

;
; Once time is read, must make sure the date wrap is not lost.	The ROM will
; return the value only once, it must check for day wrap on each call.
;
		SHR	AL,1
		;ADC	word [CS:DAYCNT],0	; ADD IT TO OUR SAVED DAY COUNT
		ADC	word [DAYCNT], 0 ; 11/04/2018
;
; Compute elapsed time1
;
		; 08/04/2018
		; Retro DOS v2.0
		;MOV	AX,[CS:TIM_LO]		; GET STORED TIME
		; 11/04/2018
		MOV	AX,[TIM_LO]
		SUB	DX,AX
		;MOV	AX,[CS:TIM_HI]
		MOV	AX,[TIM_HI]
		SBB	CX,AX
;
; CX:DX is the elapsed time
;
		JNZ	short TimeCheck_Unk	; CX <> 0 => > 1 hour
		OR	DX,DX			; did some time pass?
		JNZ	short TimePassed	; yes, examine max value
;
; No noticeable time has passed. There are two possibilities. First there
; could be two driver calls with in one clock tick (55 milliseconds).  The
; second possibility is the program has reprogramed the counter -- this is
; the unreliable counter case. To distinguish between the case a count is
; kept of the number of calls that happen without a clock tick (the variable
; is AccessCount).  If this count exceeds a set limit (MaxAccess) it is
; assumed the counter is unreliable and the value don't know is returned.
; If AccessCount is less than MaxAccess we assume the time is valid and
; therefore the media has not changed.
;
		;inc	byte [cs:AccessCount]
		; 11/04/2018
		inc	byte [AccessCount]
						; Exceeded threshold for count?
		;cmp	byte [cs:AccessCount],AccessMax
		jb	short TimeCheck_Ret	; no, return media unchanged
		;dec	byte [cs:AccessCount]	; don't let the count wrap
		dec	byte [AccessCount]
		jmp	short TimeCheck_Unk	; "I don't know" if media changed

;
; If this code is reached some time has passed.  Need to determine if
; 2 seconds have passed.  Note: 18.2 ticks per second.
;
TimePassed:
		CMP	DX,18 * 2		; IF ( Time_passed <= 2secs )
		JBE	short TimeCheck_Ret	;      presume no change


; Everything indicates that we do not know what has happened.
;
TimeCheck_Unk:
		DEC	SI			; Presume I don't know
TimeCheck_Ret:
		RETN

ERR$EXITJ: 
		JMP 	ERR$EXIT


;------------------------------------------------------------------------
;									:
;		Get Bios Parameter Block				:
;									:
; GET$BPB is called to build a valid BPB for the media in the disk	:
; drive.  A BPB (Bios Parameter Block) contains information about	:
; the media which is currently in the drive.  The values stored is	:
; information like number of fat sectors, size of drive, 8 or 9 sectors,:
; etc.									:
;									:
;	This routine is called by the device drive code.		:
;									:
;	On entry AL contains the logical drive number which needs	:
;	  the BPB built.						:
;	ES:[di] points to a buffer; the first byte of the buffer is a	:
;	   media decriptor byte.					:
;									:
;------------------------------------------------------------------------
;
; Build a valid BPB for the disk in the drive.
;

GET$BPB:
		mov	AH,[ES:DI]		; get FAT ID byte read by DOS
		call	SETDRIVE		; get the correct BDS for the drv
		; 08/04/2018
		;;;Rev 3.30 Modification
		;TEST	word [DI+BDS.Flags],fNon_Removable
		;JNZ	short ALREADY_GOTBPB	; NO NEED TO BUILD FOR FIXED DISKS
		;;End of Modification
		;call	GETBP			; build a BPB if necessary.
		jc	short ERR$EXITJ		; if error exit
;SET_PATCH:
;		CALL	Set_Volume_ID
;ALREADY_GOTBPB:
;		add	di,BDS.BytePerSec	; return the BPB that is in the current BDS

SetPTRSAV:					; return point for DSK$INIT
		;les	BX,[CS:PTRSAV]
		les	BX,[PTRSAV] ; 11/04/2018
		mov	[ES:BX+MEDIA],AH
		mov	[ES:BX+COUNT],DI
		mov	[ES:BX+COUNT+2],DS
		jmp	EXIT

;
; SetDrive scans through the data structure of BDSs and returns a
; pointer to the BDS that belongs to the drive specified in AL.
; Carry is set if no BDS has a logical drive number which matches the
; value in AL.
;	Input:
;	  AL contains the logical drive number
;	Output:
;	  DS:DI points to correct BDS if Carry is clear.
;
;	 All register execpt DS and DI are preserved
;

SETDRIVE:
		; 08/04/2018
		; Retro DOS v2.0 (IBMBIO.COM, IBMDOS 2.1)
		push	ax
		cmp	al, [DRVMAX]
		jb	short CHECKHARDDISK
		;
		mov	al,error_unknown_media
		stc
		retn
CHECKHARDDISK:
		and	ah, 0F8h
		cmp	ah, 0F8h
		je	short SETHARDDISK
SETFLOPPYDISK:
		xor	ah, ah
		mov	di, DSKDRVS
		shl	al, 1
		add	di, ax
		mov	di, [DI] 
SETDRIVE_OK:
		pop	ax
		retn
SETHARDDISK:
		mov	di, HD0_PARAMETERS
		cmp	al, [HARDNUM]
		je	short SETDRIVE_OK
		jb	short SETFLOPPYDISK
		mov	di, HD1_PARAMETERS
		jmp	short SETDRIVE_OK



;------------------------------------------------------------------------
;									:
;		DISK I/O ROUTINES					:
;									:
;  On entry the register contain the following values:			:
;									:
;	AH - Media Descriptor byte					:
;	AL - logical drive number					:
;	CX - count of sectors to be read or written			:
;	DX - start sector						:
;	DI - offset of destination buffer				:
;									:
;------------------------------------------------------------------------


;------------------------------------------------------------------------
;									:
;		Disk Write with Verify					:
;									:
;  Input								:
;	See about header for register contents on entry.		:
;									:
;------------------------------------------------------------------------


DSK$WRITV:
		MOV	WORD [CS:WRTVERIFY],103H
		JMP	SHORT DSK$CL

;------------------------------------------------------------------------
;									:
;		       Disk Write					:
;									:
;  Input								:
;	See about header for register contents on entry.		:
;									:
;------------------------------------------------------------------------

DSK$WRIT:
		MOV	WORD [CS:WRTVERIFY],ROMWrite

DSK$CL:
		CALL	DISKIO
DSK$IO:
		JC	short DSKBad
		JMP	EXIT
DSKBad:
		JMP	ERR$CNT

;------------------------------------------------------------------------
;									:
;			Disk Read					:
;									:
;  Input								:
;	See about header for register contents on entry.		:
;									:
;------------------------------------------------------------------------

DSK$READ:
		CALL	DISKRD
		JMP	short DSK$IO

;
; Miscellaneous odd jump routines.  Moved out of mainline for speed.
;


;
; CheckSingle determines if the drive specified is a virtual drive (more
; than one logical drive associated with one physical drive). If this
; is the case we need to prompt the user to place the correct disk in
; the drive.
;
;	Input:
;	   DS:DI pints to the BDS for the drive being checked.
;
;	If there is a error the carry flag is set on return
;
;  All registers are preserved.
;

CHECKSINGLE:
		; 13/04/2018
		;push	ax
		;mov	[MEDIACHK_DRV], al
		;cmp	byte [cs:Single],1	; single drive system?
		cmp	byte [Single], 1
		jb	short SingleRet		; no, jump down
		;mov	al,[CS:MEDIACHK_DRV]
		;mov	al,[MEDIACHK_DRV]
		push	ax
		push	ds
		push	di
		xor	di,di	
		mov	ds,di
		mov	ah, al
		xchg	ah,[LSTDRV] ; 10/04/2018
		pop	di
		pop	ds
		cmp	ah,al
		;je	short SingleRet
		je	short SingleRet_pop ; 13/04/2018

		call	SWPDSK			; ask user for correct disk
		;mov	al, [MEDIACHK_DRV] ; 13/04/2018
;SingleRet:
SingleRet_pop:
		pop	ax
SingleRet:
		retn				; return

;
; BadDrive is called when sector specified is greater than last
; sector on disk.
; or when BDS is not found for drive
;

BadDrive:
		mov	AL,8			; error code 'sector not found'
		stc				; indicate error
IORET:	
		retn				; return

;------------------------------------------------------------
;
;	DISK I/O HANDLER
;
;   On entry:
;	AL = Drive Number (0-6)
;	AH = Media Descriptor
;	CX = sector count
;	DX = first sector
;	DS = CS
;	ES:DI = transfer address
;	[RFLAG] = operation (2 for read, 3 for write)
;	[VERIFY] = 1 for verity after write
;
;   On exit:
;	if successful carry flag = 0
;	  else CF=1 and AL contains error code
;

DISKRD:
		;mov	byte [CS:RFLAG],ROMRead	; set command to read
		mov	byte [RFLAG],ROMRead	 ; 11/04/2018

DISKIO:
		; 13/04/2018
		; 08/04/2018
		; Retro DOS v2.0 (IBMDOS 2.1, IBMBIO.COM)

		clc
		jcxz	IORET
		
		;mov	[CS:SPSAV],SP		; save the sp value
		mov	[SPSAV],sp

		mov	BX,DI			; ES:BX is transfer address
		call	SETDRIVE		; map logical and physical
		jc	short BadDrive		; carry means BDS not found
		;mov	al,[DI+BDS.Mediad]
		;mov	[CS:MedByt],al		; Preserve media byte for drive for use
						; in determining media change.
		; DX = Sector address (offset)
		; BX = Buffer address (offset)
		
; Ensure that we are trying to access valid sectors on the drive
;
		mov	SI,DX			; start with first sector
		add	SI,CX			; add in sector count
		add	DX,[DI+17]		; add in the hidden sectors
		cmp	SI,[DI+8]		; compare against max (volume size)
		ja	short BadDrive		; if greater than max, error
		;mov	[CS:SECCNT],CX		; save sector count
		mov	[SECCNT],CX

; For hard drives do not do media check or set DPT.
;
		; 13/04/2018	
		;mov	[MEDIACHK_DRV],al

		cmp	al, [HARDNUM]
		jnb	short SKIP1

		CALL	CHECKSINGLE

		; AL = Drive number (0,1,2,3) 

		; DX = Sector address (LBA/physical)
		; BX = Buffer address (offset)

		cmp	byte [Single], 0
		jna	short SKIP2
		xor	al, al ;  Logical B: -> Physical drive 0 (A:)
		JMP	SHORT SKIP2
SKIP1:
		ADD	AL, 7Eh
SKIP2:
		mov	[DRVNUM], AL	

		mov	AX,DX			; setup logical sector for divide
		xor	DX,DX
		div	word [DI+13]		; divide by sectors per track
		inc	DL
		;mov	[CS:CURSEC],DL		; save current sector
		mov	[CURSEC],DL
		mov	CX,[DI+15]		; get number of heads
		xor	DX,DX			; divide tracks by heads per cylinder
		div	CX
		;mov	[CS:CURHD],DL		; save current head
		mov	[CURHD],DL
		;mov	[CS:CURTRK],AX		; save current track
		mov	[CURTRK],AX

;
; We are now set up for the I/O. Normally, we consider the DMA boundary
; violations here.  

		;mov	AX,[CS:SECCNT]

		; 08/04/2018
		; RetRo DOS v2.0 (IBMDOS 2.1, IBMBIO.COM)

		mov	ax, [SECCNT]
		mov	si, es
		shl	si, 1
		shl	si, 1
		shl	si, 1
		shl	si, 1
		add	si, bx
		add	si, 511
		jc	short ACROSS_64K
		xchg	bx, si
		shr	bh, 1
		mov	ah, 128
		sub	ah, bh
		xchg	bx, si
		cmp	ah, al
		jbe	short SKIP3
		mov	ah, al
SKIP3:
		push	ax
		mov	al, ah
		call	RW_TRACKS
		pop	ax
		sub	al, ah
		jz	short SET_TIM
ACROSS_64K:
		push	ax
		push	es
		push	bx
		call	MOVE_BUFFER_CONTENT
		add	bh, 2
		call	RW_ONE_SECTOR
		pop	bx
		pop	es
		pop	ax
		call	MOVE_BUFFER_CONTENT
		dec	al
		add	bh, 2
		call	RW_TRACKS
SET_TIM:
		 ; 13/04/2018
		;MOV	AL,[MEDIACHK_DRV]
		;MOV	AL, [DRVNUM]

		;cmp	al,[HARDNUM]
		CMP	DL, [HARDNUM]
		jnb	short SKIP4

		;MOV	[CS:TIM_DRV],AL 	; SAVE DRIVE LETTER
		;MOV	[TIM_DRV],AL 		; save drive number
		MOV	[TIM_DRV],DL

		xor	ah, ah
		int	1Ah		; CLOCK	- GET TIME OF DAY
					; Return: CX:DX	= clock	count
					; AL = 00h if clock was	read or	written
					; (via AH=0,1) since the previous
					; midnight; Otherwise, AL > 0
		or	al, al
		jz	short NOROLL3
		inc	word [DAYCNT]
NOROLL3:
		mov	[TIM_LO], dx
		mov	[TIM_HI], cx
		clc
SKIP4:
		retn

;----------------------------------------------------------------------------

MOVE_BUFFER_CONTENT:
		; 08/04/2018 (IBMDOS 2.1, IBMBIO.COM)
		; Retro DOS v2.0
		; save/restore existing data before/after buffer offset change
		; (this is done for solving dma boundary error problem)

		push	di
		push	bx
		push	ax
		mov	di, bx
		add	bh, 2
		mov	si, bx
		cld
		mov	cx, 256
MOVEBUFFER_LOOP:
		mov	bx, [es:di]
		mov	ax, [es:si]
		mov	[es:si], bx
		stosw
		inc	si
		inc	si
		loop	MOVEBUFFER_LOOP
		xor	byte [CURSECBUF], 1
		pop	ax
		pop	bx
		pop	di
RET3:
		retn

;----------------------------------------------------------------------------

RW_TRACKS:
		or	al, al
		jz	short RET3
		mov	ah, [di+0Dh]	; bsSecPerTrack
		inc	ah
		sub	ah, [CURSEC]	; sector number on track
		cmp	ah, al
		jbe	short SKIP5	; sector count overs track limit
		mov	ah, al		; fit to sector count
SKIP5:
		push	ax
		mov	al, ah		; read (remain) sectors on track
		call	RW_SECTORS
		pop	ax
		sub	al, ah
		shl	ah, 1		; 256 --> 512 (bx)
		add	bh, ah		; +1X sector = +2X*256 bytes
		jmp	short RW_TRACKS

;----------------------------------------------------------------------------

RW_ONE_SECTOR:
		mov	al, 1

RW_SECTORS:
		; 09/04/2018
		; Retro DOS v2.0

		;mov	si, 5
		MOV	SI, MAXERR
		mov	ah, [RFLAG]
RETRY:
		push	ax
		mov	dx, [CURTRK]
		mov	cl, 6
		shl	dh, cl
		or	dh, [CURSEC]
		mov	cx, dx
		xchg	ch, cl
		mov	dx, [DRVNUM]
		int	13h
		jc	short DSKERR
		pop	ax
		push	ax
NO_SET:
		cmp	word [RFLAG], 103h
		jne	short NoVerify
		mov	ah, 4
		int	13h
		jc	short DSKERR
NoVerify:
		pop	ax
		and	cl, 3Fh
		xor	ah, ah
		sub	[SECCNT], ax
		add	cl, al
		mov	[CURSEC], cl
		cmp	cl, [di+0Dh]  ; bsSecPerTrack	
		jbe	short Disk_Ret
		mov	byte [CURSEC], 1
		mov	dh, [CURHD]
		inc	dh
		cmp	dh, [di+0Fh]  ; bsHeads	
		jb	short NOXOR
		xor	dh, dh
		inc	word [CURTRK]
NOXOR:
		mov	[CURHD], dh
Disk_Ret:
		retn

DSKERR:
		; 09/04/2018
		; Retro DOS v2.0

		; reset the disk and decrement retry cnt
		push	ax
		mov	ah, 0
		int	13h
		pop	ax		; Restore sector count
		dec	si
		jz	short HARDERR
		cmp	ah, 80h		; timeout?
		;jz	short HARDERR 	; yes, jump to hard error
		;jmp	short RETRY	; and try again
		jne	short RETRY	
HARDERR:
		cmp	byte [CURSECBUF], 0
		jna	short HARDERR1
		pop	bx 	; ax, disk sector address 
		pop	bx 	; bx
		pop	es 	; es 	
		call	MOVE_BUFFER_CONTENT
HARDERR1:		
;		CALL	MAPERROR
;HARDERR2:			; for routines that call MapError themselves
;		;mov	byte [CS:TIM_DRV],-1 	;Force a media check through ROM
;		mov	byte [TIM_DRV], -1
;		;mov	CX,[CS:SECCNT]		;Get count of sectors to go
;		mov	CX,[SECCNT]
;		;mov	SP,[CS:SPSAV]		;Recover entry stack pointer
;		mov	SP,[SPSAV]
;		;stc
;		retn			;and return


;----------------------------------------------------------------------------

;
; Map error returned by ROM into corresponding code to be returned to
; DOS in AL.
;
MAPERROR:
		;push	CX			; save cx
		push	CS
		pop	ES			; make ES the local segment
		mov	AL,AH			; move error code into AL
		;mov	[CS:LSTERR],AL		; terminate list with error code
		mov	[LSTERR],AL
		mov	CX,NUMERR		; number of possible error conditions
		mov	DI,ERRIN	 	; point to error conditions
		repne	SCASB
		;mov	AL,[CS:DI + NUMERR - 1]	; get translation
		mov	AL,[DI+NUMERR+1]
		;pop	cx			; restore cx
;		stc				; flag error condition
;		retn

HARDERR2:			; for routines that call MapError themselves
		;mov	byte [CS:TIM_DRV],-1 	;Force a media check through ROM
		mov	byte [TIM_DRV], -1
		;mov	CX,[CS:SECCNT]		;Get count of sectors to go
		mov	CX,[SECCNT]
		;mov	SP,[CS:SPSAV]		;Recover entry stack pointer
		mov	SP,[SPSAV]
		stc
		retn			;and return


; include msioctl.inc

;-----------------------------------------------------------------------------
; include msioctl.inc - MSDOS 3.3 - MSDISK.ASM - 02/02/1988
;-----------------------------------------------------------------------------
; 24/03/2018 - Retro DOS v2.0

		; include ioctl.inc

; IOCTL.INC - MSDOS 6.0 - 1991
; ............................................................................

;*** J.K.
;General Guide -
;Category Code:
; 0... .... DOS Defined
; 1... .... User defined
; .xxx xxxx Code

;Function Code:
; 0... .... Return error if unsupported
; 1... .... Ignore if unsupported
; .0.. .... Intercepted by DOS
; .1.. .... Passed to driver
; ..0. .... Sends data/commands to device
; ..1. .... Quries data/info from device
; ...x .... Subfunction
;
; Note that "Sends/queries" data bit is intended only to regularize the
; function set.  It plays no critical role; some functions may contain both
; command and query elements. The convention is that such commands are
; defined as "sends data".

;*****************************;*
; BLOCK DRIVERS 	      ;*
;*****************************;*

; IOCTL SUB-FUNCTIONS
IOCTL_GET_DEVICE_INFO	EQU	0
IOCTL_SET_DEVICE_INFO	EQU	1
IOCTL_READ_HANDLE	EQU	2
IOCTL_WRITE_HANDLE	EQU	3
IOCTL_READ_DRIVE	EQU	4
IOCTL_WRITE_DRIVE	EQU	5
IOCTL_GET_INPUT_STATUS	EQU	6
IOCTL_GET_OUTPUT_STATUS EQU	7
IOCTL_CHANGEABLE?	EQU	8
IOCTL_DeviceLocOrRem?	EQU	9
IOCTL_HandleLocOrRem?	EQU	0Ah   ;10
IOCTL_SHARING_RETRY	EQU	0Bh   ;11
GENERIC_IOCTL_HANDLE	EQU	0Ch   ;12
GENERIC_IOCTL		EQU	0Dh   ;13
IOCTL_GET_DRIVE_MAP 	EQU	0Eh   ;14
IOCTL_SET_DRIVE_MAP	EQU	0Fh   ;15
IOCTL_QUERY_HANDLE	EQU	10h   ;16
IOCTL_QUERY_BLOCK	EQU	11h   ;17

; GENERIC IOCTL SUB-FUNCTIONS
RAWIO			EQU	8

; RAWIO SUB-FUNCTIONS
GET_DEVICE_PARAMETERS	EQU	60H
SET_DEVICE_PARAMETERS	EQU	40H
READ_TRACK		EQU	61H
WRITE_TRACK		EQU	41H
VERIFY_TRACK		EQU	62H
FORMAT_TRACK		EQU	42H
GET_MEDIA_ID		EQU	66h	;AN000;AN003;changed from 63h
SET_MEDIA_ID		EQU	46h	;AN000;AN003;changed from 43h
GET_ACCESS_FLAG 	EQU	67h	;AN002;AN003;Unpublished function.Changed from 64h
SET_ACCESS_FLAG 	EQU	47h	;AN002;AN003;Unpublished function.Changed from 44h
SENSE_MEDIA_TYPE	EQU	68H	;Added for 5.00


; SPECIAL FUNCTION FOR GET DEVICE PARAMETERS
BUILD_DEVICE_BPB	EQU	000000001B

; SPECIAL FUNCTIONS FOR SET DEVICE PARAMETERS
INSTALL_FAKE_BPB	EQU	000000001B
ONLY_SET_TRACKLAYOUT	EQU	000000010B
TRACKLAYOUT_IS_GOOD	EQU	000000100B

; SPECIAL FUNCTION FOR FORMAT TRACK
STATUS_FOR_FORMAT	EQU	000000001B
DO_FAST_FORMAT			equ	000000010B	;AN001;
; CODES RETURNED FROM FORMAT STATUS CALL
FORMAT_NO_ROM_SUPPORT		EQU	000000001B
FORMAT_COMB_NOT_SUPPORTED	EQU	000000010B

; DEVICETYPE VALUES
MAX_SECTORS_IN_TRACK	EQU	63	; MAXIMUM SECTORS ON A DISK.(Was 40 in DOS 3.2)
DEV_5INCH		EQU	0
DEV_5INCH96TPI		EQU	1
DEV_3INCH720KB		EQU	2
DEV_8INCHSS		EQU	3
DEV_8INCHDS		EQU	4
DEV_HARDDISK		EQU	5
DEV_OTHER		EQU	7
;DEV_3INCH1440KB	EQU	7
DEV_3INCH2880KB		EQU	9
; Retro DOS v2.0 - 26/03/2018
;;DEV_TAPE		EQU	6
;;DEV_ERIMO		EQU	8
;DEV_3INCH2880KB	EQU	9
DEV_3INCH1440KB		EQU	10

;MAX_DEV_TYPE		EQU	9	; MAXIMUM DEVICE TYPE THAT WE
					; CURRENTLY SUPPORT.
MAX_DEV_TYPE		EQU	10

struc A_SECTORTABLE
.ST_SECTORNUMBER:	resw	1
.ST_SECTORSIZE:		resw	1
.size:
endstruc

; MSDOS 6.0 - BPB.INC - 1991
; ####
;**	BIOS PARAMETER BLOCK DEFINITION
;
;	The BPB contains information about the disk structure. It dates
;	back to the earliest FAT systems and so FAT information is
;	intermingled with physical driver information.
;
;	A boot sector contains a BPB for its device; for other disks
;	the driver creates a BPB. DOS keeps copies of some of this
;	information in the DPB.
;
;	The BDS structure contains a BPB within it.
;

struc A_BPB
.BPB_BYTESPERSECTOR:	resw	1
.BPB_SECTORSPERCLUSTER:	resb	1
.BPB_RESERVEDSECTORS:	resw	1
.BPB_NUMBEROFFATS:	resb	1
.BPB_ROOTENTRIES: 	resw	1
.BPB_TOTALSECTORS:	resw	1
.BPB_MEDIADESCRIPTOR:	resb	1
.BPB_SECTORSPERFAT:	resw	1
.BPB_SECTORSPERTRACK:	resw	1
.BPB_HEADS:		resw	1
.BPB_HIDDENSECTORS:	resw	1
			resw	1
.BPB_BIGTOTALSECTORS:	resw	1
			resw	1
			resb	6	; NOTE:  many times these
;					; 	 6 bytes are omitted
;					;	 when BPB manipulations
;					;	 are performed!
.size:
endstruc
; ####

struc A_DEVICEPARAMETERS
.DP_SPECIALFUNCTIONS:	resb	1
.DP_DEVICETYPE:		resb	1
.DP_DEVICEATTRIBUTES:	resw	1
.DP_CYLINDERS:		resw	1
.DP_MEDIATYPE:		resb	1
.DP_BPB:		resb	A_BPB.size
.DP_TRACKTABLEENTRIES:	resw	1
.DP_SECTORTABLE:	resb	MAX_SECTORS_IN_TRACK * A_SECTORTABLE.size
endstruc

struc A_TRACKREADWRITEPACKET
.TRWP_SPECIALFUNCTIONS:	resb	1
.TRWP_HEAD:		resw	1
.TRWP_CYLINDER:		resw	1
.TRWP_FIRSTSECTOR:	resw	1
.TRWP_SECTORSTOREADWRITE: resw	1
.TRWP_TRANSFERADDRESS:	resd	1
endstruc

;AN001; - FP_TRACKCOUNT is only meaningful when FP_SPECIALFUNCTIONS bit 1 = 1.
struc A_FORMATPACKET
.FP_SPECIALFUNCTIONS:	resb	1  ; db ?
.FP_HEAD: 		resw	1  ; dw ? 
.FP_CYLINDER:		resw	1  ; dw ?
.FP_TRACKCOUNT:		resw	1  ; dw 1 ; !
endstruc

struc A_VERIFYPACKET
.VP_SPECIALFUNCTIONS:	resb	1
.VP_HEAD: 		resw	1
.VP_CYLINDER:		resw	1
endstruc

struc A_MEDIA_ID_INFO
.MI_LEVEL:		resw	1  ; dw 0 ; !		;J.K. 87 Info. level
.MI_SERIAL:		resd	1  ; dd ?		;J.K. 87 Serial #
.MI_LABEL:		resb	11 ; db 11 DUP (' ') ;!	;J.K. 87 volume label
.MI_SYSTEM:		resb 	8  ; db 8 DUP (' ')  ;!	;J.K. 87 File system type
endstruc

struc A_DISKACCESS_CONTROL	   ;AN002; Unpublished function. Only for Hard file.
.DAC_SPECIALFUNCTIONS:	resb 	1  ; db 0 ; ! ;AN002; Always 0
.DAC_ACCESS_FLAG: 	resb 	1  ; db 0 ; ! 
				   ; Non Zero - allow disk I/O to unformatted hard file
endstruc			   ; 0 - Disallow disk I/O to unformatted hard file


struc A_MEDIA_SENSE			; Media sense structure added 5.00
.MS_ISDEFAULT:		resb	1	; If 1 type returned is drv default
.MS_DEVICETYPE:		resb	1	; Drive type 
.MS_RESERVED1:		resb	1	; RESERVED
.MS_RESERVED2:		resb 	1	; RESERVED 
endstruc

;********************************;*
; CHARACTER DEVICES (PRINTERS)	 ;*
;********************************;*

;RAWIO SUB-FUNCTIONS
GET_RETRY_COUNT 	EQU	65H
SET_RETRY_COUNT 	EQU	45H

struc A_RETRYCOUNT
.RC_COUNT:		resw 1
endstruc

;********************************;*		;J.K. 4/29/86
; CHARACTER DEVICES (SCREEN)	 ;*
;********************************;*		;J.K. 4/29/86
;
;SC_MODE_INFO	 struc
;SC_INFO_LENGTH 	 DW	 9
;SC_MODE		 DB	 0
;SC_COLORS		 DW	 0
;SC_WIDTH		 DW	 0
;SC_LENGTH		 DW	 0
;SC_MODE_INFO	 ends
;
;SC_INFO_PACKET_LENGTH	 EQU	 9		 ;LENGTH OF THE INFO PACKET.

;SUBFUNCTIONS FOR CON$GENIOCTL
;GET_SC_MODE		 EQU	 60h
;SET_SC_MODE		 EQU	 40h
;The following subfunctions are reserved for installable CODE PAGE switch
;console devices. - J.K. 4/29/86
;Get_active_codepage	 equ	 6Ah
;Invoke_active_codepage  equ	 4Ah
;Start_designate_codepage equ	 4Ch
;End_designate_codepage  equ	 4Dh
;Get_list_of_designated_codepage equ 6Bh
;J.K. 4/29/86 *** End of Con$genioctl equates & structures

; ............................................................................

;-----------------------------------------------------------------------------
; MSDISK.ASM (2) - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 24/03/2018 - Retro DOS v2.0 

;-----------------------------------------------------------------------------
; MSBIO2.ASM - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 25/03/2018 - Retro DOS v2.0 

		;EVENB
align 2

DSKDRVS:	; 07/04/2018
		DW	_360K_parameters
		DW	_360K_parameters
		DW	_360K_parameters
		DW	_360K_parameters
HDSKTAB:
		DW	HD0_PARAMETERS
		DW	HD1_PARAMETERS


;*****************************************************************************

DSK$INIT:
		;PUSH	CS
		;POP	DS

		MOV	AH,[DRVMAX]
		MOV	DI,DSKDRVS
		JMP	SetPTRSAV

;-------------------------------------------------
;
;  Ask to swap the disk in drive A:
; Using a different drive in a one drive system so
; request the user to change disks
;

SWPDSK:
		; 13/04/2018
		; 09/04/2018
		; Retro DOS v2.0 (IBMDOS 2.1, IBMBIO.COM)

		;push	ds
		push	si ; 13/04/2018
		;xor	si,si
		;mov	ds,si
		;mov	ah,al
		;xchg	ah, [LSTDRV]
		;pop	ds
		;cmp	ah,al
		;je	short SWPDSK_ret

		add	al,"A"
		;mov	[cs:DRVLET],AL
		mov	[DRVLET],AL		
		;push	ds			; preserve segment register
		;push	cs
		;pop	ds
		mov	SI,SNGMSG		; ds:si -> message
		;push	BX
		call	WRMSG			;Print disk change message
		call	FLUSH
						; wait for a keyboard character
		xor	AH, AH			; set command to read character
		int	16h			; call rom-bios
		;POP	BX
		;pop	ds			; restore segment register
;SWPDSK_ret:
		;xor	al, al
		pop	si ; 13/04/2018
WRMRET:
		retn

;----------------------------------------------
;
;  WrMsg writes out message pointed to by [SI]
;

WRMSG:
		lodsb				; get the next character of the message
		or	AL,AL			; see if end of message
		jz	short WRMRET
		pushf
		push	CS
		call	OUTCHR
		jmp	SHORT WRMSG


;	INCLUDE BIOMES.INC

; BIOMES.INC - 24/07/1987
;----------------------------------------------------------------------------
;
; Single drive message for msbio.com. Nul terminated.	;;End of Modification
;

SNGMSG:	DB	0Dh,0Ah,"Insert diskette for drive "
DRVLET:	DB	"A: and strike",0Dh,0Ah,"any key when ready",0Dh,0Ah,0Ah,0

;
; End of code for virtual floppy drives
;

EndSwap:

;HNUM:		
;		DB	0		; number of hardfile (hard drives)
;HARDDRV:
;		DB	80h		;Physical drive number of first hardfile


; include ms96tpi.inc

; MS96TPI.INC - 24/07/1987
;----------------------------------------------------------------------------
; 25/03/2018 - Retro DOS v2.0

;------------------------------------------------------------------------
;									:
; File: ms96tpi.asm							:
;									:
; This file contains code to support the 96 tpi drives.  The code	:
; is included in the bio if the machine has at least one drive with	:
; changeline support.  If the machine has no changeline drives then	:
; the code is not kept in the bio at system initialization time.	:
;									:
;------------------------------------------------------------------------

;
; At media check time, we need to really get down and check what the change is.
; This is GUARANTEED to be expensive.
;
; On entry AL contains logical drive number
;

MediaCheck:
		; 13/04/2018
		; 08/04/2018
		; Retro DOS v2.0

		; SI = 0
		call	CHECKSINGLE	; make sure correct disk is in place
		;xor	si,si
		; 10/04/2018
		;mov	DL, [CS:MEDIACHK_DRV]
		; 13/04/2018
		;mov	dl, [MEDIACHK_DRV]
		mov	dl, al	
		call	HasChange
		jz	short MediaRet ; SI = 0 ("I don't know")

					; see if changeline has been triggered
;;Rev 3.30 Modification
		;mov	DL, [DI+BDS.DriveNum] ; set logical drive number	  
		;mov	DL, [CS:MEDIACHK_DRV] ; 08/04/2018 - Retro DOS v2.0
		mov	AH, 16h 	    ; get changeline status	  
		int	13h		    ; call rom diskette routine
			; (Programmer's Guide To AMIBIOS - 1993)
			; AH = 00h No floppy disk (media) change
			;      01h Invalid floppy disk parameter
			;      06h Floppy disk was changed since last access
			;      80h Floppy disk drive not ready		
			;      = Any other value is an error code

		jc	short MediaRet ; SI = 0 ; ("I don't know")
		
		inc	si ; 1 = "MEDIA IS NOT CHANGED"
		and	ah, ah
		jz	short MediaRet
		dec	si ; 0 = ("I don't know")
		cmp	ah, 6
		jne	short MediaRet
		dec	si ; -1 = "MEDIA HAS BEEN CHANGED"
MediaRet:
		; DL = drive number (0 to 3)
		retn				
		
;
; HasChange - see if drive can supply change line
;
;   Inputs:	DS:DI points to current BDS
;   Outputs:	Zero set - no change line available
;		Zero reset - change line available
;   Registers modified: none

HasChange:
		; 08/04/2018
		; Retro DOS v2.0 (Programmer's Guide To AMIBIOS, 1993)
	
		; DL = Drive number (0,1)
		mov	ah, 15h  ; Return Drive Type
		Int	13h
			; AH = 00h No drive present
			;      01h Drive does not have change line support
			;      02h Drive dos have changle line support
			; CF = 0 No error
			;      1 Error
		mov	al, 0
		jc	short HasNotChange
		cmp	ah, 2
		jne	short HasNotChange
		inc	al ; 1 = (Disk drive has change line)	
HasNotChange:			
		or	al, al
		retn


;-----------------------------------------------------------------------------
; MSINIT.ASM - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 19/03/2018 - Retro DOS v2.0

;-----------------------------------------------------------------------------
; BOOT DISK PARAMETERS
;-----------------------------------------------------------------------------
; 07/04/2018 - RETRO DOS V2.0

BOOT_DRV_PARMS:
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
		dw 0 ; Hidden sectors
		
; Note: These (fd&hd) parameters table sizes are 19 bytes for MSDOS 2.11

;-----------------------------------------------------------------------------
; FLOPPY DISK PARAMETERS
;-----------------------------------------------------------------------------
; 07/04/2018 - RETRO DOS V2.0

		; Retro DOS v2.0 - 19 bytes disk parameters (07/04/2018)
_FD_parameters:
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
		dw 0	
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
		dw 0
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
		dw 0
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
		dw 0
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
		dw 0

END_OF_FDP_TBLS:

;-----------------------------------------------------------------------------
; HARD DISK PARAMETERS
;-----------------------------------------------------------------------------
; 07/04/2018 - RETRO DOS V2.0

align 2

HNUM:		db 0	
HD_PARAMETERS:	
		db 80h	
HD0_PARAMETERS:	
		dw 200h		
		db 1
		dw 1
		db 2
		dw 10h
		dw 0
		db 0F8h
		dw 1
		dw 0
		dw 0
		dw 0

END_OF_HD1P_TBL:

HD1_PARAMETERS:
		dw 200h	
		db 0
		dw 1
		db 2
		dw 0
		dw 0
		db 0F8h
		dw 0
		dw 0
		dw 0
		dw 0

END_OF_HD2P_TBL:

;-----------------------------------------------------------------------------
DRVFAT		dw 0
BIOS$		dw 0
DOSCNT		dw 0

; 28/03/2018
; MSDOS 6.0 - MSINIT.ASM, 1991
init_bootseg:	dw	0	; seg addr of buffer for reading boot record

BootBias equ 200h ; 19/03/2018

;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------

align 16

;-----------------------------------------------------------------------------
; entry point from boot sector
;-----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; SYSINIT PARAMETER ADDRESS EQUALITIES - 25/02/2018 (MSDOS 2.0 'SYSINIT.ASM')
; ----------------------------------------------------------------------------
; ('sysinit.s', 'SYSINIT.BIN')

; IN SYSINIT SEGMENT (SYSINITSEG):

CURRENT_DOS_LOCATION EQU 6   ; dw (segment)
FINAL_DOS_LOCATION   EQU 10  ; dw (segment)
DEVICE_LIST	     EQU 12  ; dd (segment:offset)	
DOSSIZE		     EQU 16  ; dw ; Retro DOS 2.0 - 'MSDOS.BIN' size in words
MEMORY_SIZE	     EQU 18  ; dw (paragraphs)	
DEFAULT_DRIVE	     EQU 20  ; db

;-----------------------------------------------------------------------------
; initialization - stage 1
;-----------------------------------------------------------------------------
INIT:
		; 07/04/2018
		; Retro DOS v2.0 - registers from FD Boot Sector 
                ; DL = [bsDriveNumber]
		; DH = [bsMedia]
		; AX = [bsSectors] ; Total sectors
		; DS = 0, SS = 0
		; BP = 7C00h

		; Move Retro DOS v2.0 boot sector parameters to 0060h:0
		mov	bx, 60h
		mov	es, bx
		mov	si, bp
		sub	di, di
		mov	cx, 35 ; 70 bytes, 35 words
		;mov	cl, 35
		rep	movsw

		push	cs
		pop	ds

		; 27/03/2018
		mov	cx, KERNEL_SIZE	; words !
		; 07/04/2018
		;mov	bx, KERNEL_SEGMENT ; 0070h
		mov	bl, KERNEL_SEGMENT
		mov	es, bx
		xor	di, di
		mov	si, di
		
		; Move KERNEL file from 1000h:0 to 0070h:0
		; (Retro DOS v2 BS loads 'MSDOS.SYS' at 1000h:0000h)
		rep	movsw 
		mov	ds, bx
INIT0:
		push	es
		push	INIT1 ; 07/04/2018
		retf	; jump to 0070h:INIT1

;INIT:
INIT1:
		; 19/03/2018
		; Retro DOS v2.0 (24/02/2018)
		; [REF: MSDOS 3.3, MSBIO, "MSINIT.ASM"  (24/07/1987)]

;------------------------------------------------------------------------
;									:
;	System initialization						:
;									:
;	The entry conditions are established by the bootstrap		:
;	loader and are considered unknown.  The following jobs		:
;	will be performed by this module:				:
;									:
;	1.	All device initialization is performed			:
;	2.	A local stack is set up and DS:SI are set		:
;		to point to an initialization table. Then		:
;		an inter-segment call is made to the first		:
;		byte of the dos 					:
;	3.	Once the dos returns from this call the ds		:
;		register has been set up to point to the start		:
;		of free memory. The initialization will then		:
;		load the command program into this area 		:
;		beginning at 100 hex and transfer control to		:
;		this program.						:
;									:
;------------------------------------------------------------------------

		; MSDOS 3.3 - "MSEQU.INC" (24/07/1987)
		EOI	EQU	20H
		AKPORT	EQU	20H
		INITSPOT EQU	534H	; IBM wants 4 zeros here
		BRKADR	EQU	1BH * 4	; 6CH, 1BH break vector address
		TIMADR	EQU	1CH * 4	; 70H, 1CH timer interrupt
		DSKADR	EQU	1EH * 4	; address of ptr to disk parameters
		SEC9	EQU	522H	; address of disk parameters
		;CHROUT	EQU	29H
		;LSTDRV	EQU     504H
		;;SYSIZE	EQU 200H ;NUMBER OF PARAGRAPHS IN SYSINIT MODULE
		 ; 26/03/2018
		RSINIT	EQU	0A3H
		NORMSETTLE EQU  15

		; Retro DOS v1.0 (10/02/2018)
		; (Register values from Retro DOS v1.0 Boot Sector)
		; CS = 60h  ; BIO_SEGMENT
		; DS = 0
		; ES = 0
		; SS = 0
		; SP = 0FFFEh
 		; BP = 7C00h
		; DL = Physical drive number = 0
		;push	cs
		;pop	ds

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

		; Retro DOS 1.0 extensions (to boot sector parameters) 
		bsDataStart	equ 64

		;push	ds
		;pop	es

		; 14/02/2018
		; Set video mode to 3 (Clear screen)
		mov	ax, 3
		int	10h

		; 06/04/2018 - Retro DOS v2.0
		; (IBMDOS 2.1, IBMBIO.COM, 20/10/1983)
		; (by IDA Pro Free Dissassembler)
		; 19/03/2018
		; (Source Code Ref: MSDOS 3.3, MSINIT.ASM, 1991)
		
		; "MSINIT.ASM"
		;---------------------------------------------------------------
		; At this point the registers contain these values:
		;   AX = 0-based drive we booted from
		;   BX = the logical number of the first data sector on the disk
		;   CX = number of floppies including logical one

		;
		; set up local stack
		;

		XOR	BX,BX
		CLI		; turn interrupts off while manupulating stack
		MOV	SS,BX	; set stack segment register
		MOV	SP,700h	; set stack pointer
		STI		; turn interrupts on

		; 07/04/2018
		; Retro DOS v2.0
		INT	11h		; rom-bios equipment determination
		; 10/04/2018
		TEST	AL, 1
		JNZ	SHORT CHECKSINGLEFD
		XOR	AX, AX
		JMP	SHORT NOFD
CHECKSINGLEFD:
		MOV	CL, 6
		SHR	AL, CL
		JNZ	short NOTSINGLE	;ZERO MEANS SINGLE DRIVE SYSTEM
		INC	byte [Single]	;REMEMBER THIS
NOTSINGLE:
		XOR	AH, AH
		INC	AL ; 10/04/2018
NOFD:	
		PUSH	AX ; (***)	; save number of floppies (in AL)
		PUSH	DX ; (**)	; DL = Boot drive number (0 = A, 2 = C) 
					; DH = Media ID

		INT	12h		; call rom-bios for memory size
		SHL	AX, CL		; change from K to 16 byte blocks
		PUSH	AX ; (*)	; save memory size		

Turn_Timer_On:								
		mov	AL,EOI
		out	AKPORT,AL	; turn on the timer

		mov	SI,COM2DEV
		call	AUX_INIT		;INIT COM2
		mov	SI,COM1DEV
		call	AUX_INIT		;INIT COM1

		mov	SI,LPT3DEV
		call	PRINT_INIT		;INIT LPT3
		mov	SI,LPT2DEV
		call	PRINT_INIT		;INIT LPT2
		mov	SI,LPT1DEV
		call	PRINT_INIT		;INIT LPT1

		xor	DX,DX
		mov	DS,DX			;TO INITIALIZE PRINT SCREEN VECTOR
		mov	ES,DX

		mov	AX,CS			;FETCH SEGMENT

		mov	word [BRKADR],CBREAK	;BREAK ENTRY POINT
		mov	[BRKADR+2],AX		;VECTOR FOR BREAK

		mov	word [CHROUT*4],OUTCHR
		mov	[CHROUT*4+2],AX

		; IBMDOS 2.1
		;MOV	WORD [DSKADR], SEC9 ; 522h
		;MOV	[DSKADR+2], ES ; 0
 
		; MSDOS 3.3
		mov	SI,[DSKADR]
		mov	DS,[DSKADR+2]		; DS:SI -> current table

		mov	DI,SEC9 		; ES:DI -> New Table
		mov	CX,DISK_PARMS.size ; 11
		rep	MOVSB			; Copy Table
		push	ES			;
		pop	DS			; DS = 0

		mov	DI,4
		mov	BX,INTRET		;WILL INITIALIZE REST OF INTERRUPTS
		xchg	AX,BX
		stosw				;Location 4
		xchg	AX,BX
		stosw				;INT 1		;Location 6
		add	DI,4
		xchg	AX,BX
		stosw				;Location 12
		xchg	AX,BX
		stosw				;INT 3		;Location 14
		xchg	AX,BX
		stosw				;Location 16
		xchg	AX,BX
		stosw				;INT 4		;Location 18

		ADD	DI,40			
		xchg	AX,BX
		stosw				;Location 60
		xchg	AX,BX
		stosw				;INT 0Fh	;Location 62 

		mov	[500h],DX		;SET PRINT SCREEN & BREAK = 0
		mov	[LSTDRV],DX		;clean out last drive spec

		; IBMDOS 2.1 (180KB, 5 1/4" diskette parameters)
		;mov	DI,SEC9 ; 522h 		; ES:DI -> New Table
		;mov	ax, 2DFh
		;stosw
		;mov	ax, 225h
		;stosw
		;mov	ax, 2A09h
		;stosw
		;mov	ax, 50FFh
		;stosw
		;mov	ax, 0FF6h
		;stosw
		;mov	al, 2
		;stosb
			
NO_DIDDLE:
		; 24/02/2018 (Retro DOS 2.0) ; *
		push	cs
		pop	ds

		; 07/04/2018
		;int	12h			; call rom-bios for memory size
		;mov	CL, 6			; get ready for shift
		;shl	AX, CL			; change from K to 16 byte blocks
		
		POP	AX ; (*)		; Memory size in paragraphs			

		; 19/03/2018
		pop	CX ; (**)		; CL = boot drive, CH = media byte
		; 11/04/2018
		cmp	cl, 80h
		jb	short NO_HDBOOT
		sub	cl, 7Eh ; 80h = 2, 81h = 3
NO_HDBOOT:
		; 30/03/2018
		mov	[DRVFAT], CX ; (**)
		
		; 06/04/2018
		MOV	[SYSINIT_START+MEMORY_SIZE],AX ; Top of memory (in paragraphs)
		INC	CL
		MOV	[SYSINIT_START+DEFAULT_DRIVE],CL ; SAVE DEFAULT DRIVE SPEC
		
		; 08/04/2018
		; 28/03/2018
		; MSDOS 6.0 - MSINIT.ASM, 1991
		sub	ax,64		;room for boot record buffer segment (1 kb)
		mov	[init_bootseg],ax
		
		mov	ax, MSDOS_BIN_SEGMENT  ; MSDOS_BIN_OFFSET >> 4
		add	ax, KERNEL_SEGMENT ; 26/03/2018
		MOV	[SYSINIT_START+CURRENT_DOS_LOCATION], AX

		; 07/04/2018
		MOV	AX,END_OF_HD2P_TBL
		; 06/04/2018
		;MOV	AX,INIT
		ADD	AX,15
		SHR	AX,1
		SHR	AX,1
		SHR	AX,1
		SHR	AX,1
		ADD	AX,KERNEL_SEGMENT
		MOV	[SYSINIT_START+FINAL_DOS_LOCATION],AX

		MOV	word [SYSINIT_START+DEVICE_LIST],CONHeader
		MOV	[SYSINIT_START+DEVICE_LIST+2],CS
		;PUSH	CS
		;PUSH	CS
		;POP	DS
		;POP	ES

		;XOR	SI,SI
		;MOV	word [SI],HD_PARAMETERS

		; 07/04/2018
		; Retro  DOS v2.0 
		; DISK DRIVE PARAMETERS TABLE SETUP METHOD (by Erdogan Tan)
		
		;; 24/02/2018 (Retro DOS 2.0, MSDOS 3.3 "MSINIT.ASM")
		;int	11h		; rom-bios equipment determination
		;AND	AL, 0C0h
		;JNZ	short NOTSINGLE	;ZERO MEANS SINGLE DRIVE SYSTEM
		;INC	byte [Single]	;REMEMBER THIS
;NOTSINGLE:
		POP	AX ; (***)	; Number of floppies
		MOV	[HARDNUM],AL 	; Remember which drive is hard disk
		MOV	[DRVMAX],AL	; And set initial number of drives
		;SHL	AX,1		; Two bytes per address
		;MOV	DI,DSKDRVS
		;ADD	DI,AX		;Point to hardfile location
		;MOV	SI,HDSKTAB
		;MOVSW			;Two addresses to move
		;MOVSW

		AND	AL, AL
		JZ	SHORT SET_BOOT_DRV_PARMS  ; No floppy disks !?
		
		MOV	SI, DSKDRVS
		XOR	DL, DL
		; 09/04/2018
		push	si
SET_FD_PARMS:
		PUSH	DX
		mov	AH, 8	     	; set command to get drive parameterS
		int	13h	     	; call ROM-BIOS to get number of drives
		POP	DX
		jc	short ENDFLOPPIES ; old, rom therefore no hard disks

		; This is the IBM Personal Computer
	 	; disk format. (Retro DOS v1.0 & v2.0)
		; Default Floppy drive Type is 1
		;	INT 13h (INT 40h) Function 08h
		;	   BL =	1 - 360KB, 40 track, 5 1/4"
		;		2 - 1.2MB, 80 track, 5 1/4"
		;		3 - 720KB, 80 track, 3 1/2"
		;		4 - 1.44MB, 80 track, 3 1/2"
		;		5 - 2.88MB, 80 track, 3 1/2"	
		; ES:DI = ROM BIOS DPT address (for floppy drives)
		;		(AMIBIOS 1993)

		; 10/04/2018
		OR	AH, AH
		JNZ	SHORT ENDFLOPPIES

		AND	BL, BL
		JZ	SHORT ENDFLOPPIES  ; < 1 is invalid
		CMP	BL, 5
		JA	SHORT ENDFLOPPIES  ; > 5 is invalid	

		DEC	BL ; 0 based type number for table offset calculation
		XOR	BH,BH
		; 09/04/2018
		;push	dx
		;MOV	AX,19		; each parameters table size = 19 bytes
		;MUL	BX
		;pop	dx
		; 10/04/2018
		MOV	AL,19
		MUL	BL
		MOV	BX,_FD_parameters ; FD PARAMETERS FOR DRIVE TYPE 1 TO 5
		ADD	BX,AX		  ; add table offset for this type	
		MOV	[SI],BX
		INC	SI
		INC	SI
		INC	DL
		CMP	DL, [DRVMAX]
		JB	SHORT SET_FD_PARMS
ENDFLOPPIES:
		pop	si
		CMP	byte [Single], 1
		jne	short SET_BOOT_DRV_PARMS
		;MOV	SI, DSKDRVS
		LODSW
		MOV	[SI], AX

SET_BOOT_DRV_PARMS:
		; 09/04/2018
		push	cs
		pop	es

		MOV	BL, [DRVFAT]	; Boot drive number (0 = A, 2 = C)
		XOR	BH, BH
		SHL	BL, 1
		ADD	BX, DSKDRVS
		MOV	DI, BOOT_DRV_PARMS ; Boot drv params from its boot sector!
		MOV	[BX], DI ; Set drv par. tbl. to boot sector parameters.	
		PUSH	DS
		MOV	BX, 60h
		MOV	DS, BX
		;MOV	CX, 17
		MOV	CX, 19 ; 09/04/2018
		MOV	SI, 11
		REP	MOVSB
		POP	DS

SET_HD_PARMS:
		; 09/04/2018
		;push	es
		; 07/04/2018
		mov	DL, 80h      	; tell rom bios to look at hard drives
		mov	AH, 08h	     	; set command to get drive parameter
		int	13h	     	; call ROM-BIOS to get number of drives
		; 09/04/2018
		;pop	es
		jc	short INIT2	; old, rom therefore no hard disks
		;mov	[HNUM], DL	; save number of hard drives in HNUM

;
; Set up all the hard drives in the system
;
		
DoHard:
		;CMP	byte [HNUM],0	; IF (No_Hard_files)
		;JLE	short INIT2	;    THEN EXIT TO CONFIGURE

		; 09/04/2018
		or	dl, dl
		jz	short INIT2

		mov	[HNUM], DL	; save number of hard drives in HNUM

		MOV	DL,80h
		MOV	DI,HD0_PARAMETERS
		call	SETHARD

		INC	byte [DRVMAX]
		
		DEC	byte [HNUM]
		JZ	SHORT SET_DSKDRVS_TBL

		;push	es
		mov	DL, 81h
		mov	AH, 08h
		int	13h
		;pop	es
		jc	short SET_DSKDRVS_TBL

		MOV	DL,81h
		MOV	DI,HD1_PARAMETERS
		call	SETHARD

		INC	byte [DRVMAX]


; End of physical drive initialization.	

SET_DSKDRVS_TBL:
		; 11/04/2018
		push	cs
		pop	es
		; 07/04/2018
		MOV	DI,DSKDRVS
		MOV	SI,HDSKTAB
		
		mov	al, [HARDNUM]  ; number of floppies or
		cmp	al, 2	       ; drv number of 1st hard drive
		jnb	short NO_SET_DEF

		;mov	ax, _360K_PARAMETERS ; Default for Retro DOS v2.0	
		;stosw
		;stosw		

		; 10/04/2018
		mov	ah, [DRVMAX] ; number of disks
		sub	ah, al	     ; = number of hard disks 	
		
		mov	al, 2
		add	ah, al	      ; number of disks with B: diskette	

		mov	[HARDNUM], al
		mov	[DRVMAX], ah ; 10/04/2018

		;JMP	SHORT MOV_HD_TBL_ADDR

NO_SET_DEF:
		xor	ah, ah
		shl	al, 1
		add	di, ax
;MOV_HD_TBL_ADDR:
		movsw
		movsw

INIT2:
		CMP	BYTE [DRVMAX], 3
		JB	SHORT NO_HARDDISKS
		JA	SHORT GOSYSINIT

		MOV	AX,END_OF_HD1P_TBL
		JMP	SHORT CHANGE_FINALDOS_ADDR
NO_HARDDISKS:
		MOV	AX,END_OF_FDP_TBLS		
CHANGE_FINALDOS_ADDR:
		ADD	AX,15
		SHR	AX,1
		SHR	AX,1
		SHR	AX,1
		SHR	AX,1
		ADD	AX,KERNEL_SEGMENT
		MOV	[SYSINIT_START+FINAL_DOS_LOCATION],AX
GOSYSINIT:
		; 19/03/2018
		mov	cx, msdos_bin_size+1
		shr	cx, 1
		mov	[SYSINIT_START+DOSSIZE], cx ; DOS size in words

		; 07/04/2018
		; Retro DOS v2.0

; *** SYSINIT1.ASM ***
; ----------------------------------------------------------------------------
; START OF MSDOS 3.3 SYSINIT CODE - SYSINIT1.ASM - 24/07/1987
; ----------------------------------------------------------------------------
; 25/03/2018 - Retro DOS v2.0

SYSINIT:
		;JMP	GOINIT

; ..SYSINIT DATA .............................................................

GOINIT:

Move_Myself:
		; 03/05/2018
		; 25/02/2018 - Retro DOS 2.0 - MSDOS 2.0 "SYSINIT.ASM"
		; (Modified for Retro DOS 2.0, for NASM 'incbin' method)

		SYSINITSIZE	EQU  sysinit_code_end - sysinit_code_start

		; 28/03/2018
        	;CLD
        	MOV     SI,SYSINIT_START
        	XOR     DI,DI
		; 19/03/2018
	       	mov	CX,[SYSINIT_START+MEMORY_SIZE]

		MOV	AX,SYSINITSIZE + 15 ; 03/05/2018
		SHR     AX,1                    ; Divide by 16 for paras
		SHR     AX,1
		SHR     AX,1
		SHR     AX,1
		SUB     CX,AX
		MOV     ES,CX ; SYSINITSEG = [MEMORY_SIZE] - (SYSIZE+15)/16
		MOV     CX,SYSINITSIZE + 1
		SHR     CX,1                    ; Divide by 2 to get words
		REP     MOVSW                   ; RELOCATE SYSINIT

		PUSH    ES
		XOR	AX,AX ; 0
		PUSH    AX

		RETF	; far jump to final location of SYSINIT code

; ----------------------------------------------------------------------------
; MSINIT.ASM (MSDOS 3.3)
; ----------------------------------------------------------------------------
; 19/03/2018

;	READ A BOOT RECORD INTO 7C0h:BootBias
;  GetBoot reads the boot record into 7C0h:BootBias
;  On Entry:
;	DL contains ROM drive number (80h or 81h)
;  On Exit:
;	if carry set error
;	if carry clear:
;	   ES:BX piont to boot sector
;	AX and CX are not preserved
;	BX and ES are used to return values
;
GETBOOT:
		; 08/04/2018
		; Retro DOS v2.0 (IBMBIO.COM, IBMDOS 2.1)

		;mov	AX, 07C0h     ; prepare to load ES
		; 28/03/2018 - MSDOS 6.0 - MSINIT.ASM, 1991	
		;mov	ax, [cs:init_bootseg]
		mov	ax, [init_bootseg]
		mov	ES, AX	      ; load ES segment register
		mov	BX, BootBias  ; load BX,  ES:BX is where sector goes
		mov	AX, 0201h     ; command to read & num sec. to 1
		xor	DH, DH	      ; head number zero
		mov	CX, 0001h     ; cylinder zero and sector one
		int	13h	      ; call rom bios
		jc	short ERRET
		cmp	WORD [ES:BootBias+1FEH],0AA55H	; DAVE LITTON MAGIC BYTE?
		je	short Norm_Ret
		stc
ERRET:
Norm_Ret:
		RETN

;
;   SetHard - generate BPB for a variable sized hard file.  IBM has a
;   partitioned hard file; we must read physical sector 0 to determine where
;   our own logical sectors start.  We also read in our boot sector to
;   determine version number
;
;   Inputs:	DL is ROM drive number (80 OR 81)
;		DS:DI points to BDS
;   Outputs:	Carry clear -> BPB is filled in
;		Carry set   -> BPB is left uninitialized due to error
;

SETHARD:
		; 13/04/2018
		; 08/04/2018
		; Retro DOS v2.0 (IBMBIO.COM, IBMDOS 2.1)
		
		;push	dx
		;mov	ah,8
            	;int	13h	; GET CURRENT DRIVE PARAMETERS
				; Input:
				;   DL = drive number
				; Return:
				;   CF set on error
				;   AH = status code, BL = drive type
				;   DL = number of consecutive drives
				;   DH = maximum value for head number
				;   ES:DI -> drive parameters
		; 09/04/2018
		;push	cs
		;pop	es
		
		inc	dh 		; Number of heads
		mov	[di+0Fh],dh 	; bsHeads
		;pop	dx
		;jc	short SETRET
		and	cl, 3Fh		; Sectors per track
		mov	[di+0Dh],cl 	; bsSecPerTrack
		call	GETBOOT
		jc	short SETRET
		; 13/04/2018
		;mov	bx, 1C2h
		mov	bx, BootBias+1C2h ; Partition Table offset
SET1:
		cmp	byte [ES:BX], 1 ; Primary DOS Partition, FAT12 file system
		je	short OKDrive
		add	bx, 16		; Next partition
		;cmp	bx, 202h	; End of masterboot sector / partition table	
		cmp	bx,  BootBias+202h ; 13/04/2018
		jb	short SET1		
		stc
SETRET:
		retn

OKDrive:
		mov	ax, [ES:BX+4]	; Start sector (LBA)
		mov	[di+11h], ax	; Hidden sectors	      ; bsHidden1
		mov	ax, [ES:BX+8]	; Partition size in sectors
		cmp	ax, 64
		jb	short SETRET
		mov	[di+8], ax	; Total sectors (Volume Size) ; bsSectors
		mov	cx, 256
		mov	dx, 64
		cmp	ax, 512
		jbe	short GotParm
		add	ch, ch			; 512
		inc	cl 		     ; 1
		mov	dx, 112		; 112
		cmp	ax, 2048
		jbe	short GotParm
		add	ch, ch 			; 1024
		inc	cl		     ; 2
		mov	dx, 256		; 256
		cmp     ax, 8192  	; 19/05/2018
		jbe	short GotParm
		add	ch, ch			; 2048
		inc	cl 		     ; 3
		add	dx, dx 		; 512
		cmp	ax, 32680
		jbe	short GotParm	; 19/05/2018
		add	ch, ch			; 4096
		inc	cl		     ; 4
		add	dx, dx 		; 1024
	  
GotParm:
		mov     [di+6], dx  	; Root directory entries ; bsRootDirEnts 
		mov     [di+2], ch	; Sectors per cluster
		;xor	bx, bx
		xor	bh, bh
		mov     bl, ch
		dec     bx		
		add     bx, ax
		shr     bx, cl		; Cluster (Sector) shift value
		inc     bx
		and     bl, 0FEh
		mov     si, bx
		shr     bx, 1
		add     bx, si
		add     bx, 511
		shr     bh, 1
		mov     [di+0Bh], bh	; FAT sectors
		clc
		retn
							
; ----------------------------------------------------------------------------

; SI POINTS TO DEVICE HEADER
;
;  4/22/86 - print_init, aux_init is modified to eliminate the
;  self-modifying code.

PRINT_INIT:
		call	GET_DEVICE_NUMBER
		mov	ah,1		;initalize printer port
		int	17h		;call ROM-Bios routine
		retn

AUX_INIT:
		call	GET_DEVICE_NUMBER
		mov	al,RSINIT	;2400,N,1,8 (MSEQU.INC)
		mov	ah,0		;initalize AUX port
		int	14h		;call ROM-Bios routine
		retn

GET_DEVICE_NUMBER:
;SI -> device header
		;MOV	AL,[CS:SI+13]	;GET DEVICE NUMBER FROM THE NAME
		MOV	AL,[SI+13]
		SUB	AL,'1'
		CBW
		MOV	DX,AX
		RETN

align 16

; ----------------------------------------------------------------------------
; MSDOS 2.0 -IBMBIO.COM- SYSINIT CODE ('SYSINIT.ASM')  	-will be relocated-
; ----------------------------------------------------------------------------

SYSINIT_START equ $

sysinit_code_start: ; 10/06/2018 (sysinit.s)
		incbin	'SYSINIT.BIN' ; Retro DOS 2.0 - MSDOS 2.0 'SYSINIT'
sysinit_code_end:
		db 90h  ; 09/04/2018	

align 16 ; Paragraph alignment is necessary here for MSDOS kernel relocation

; ----------------------------------------------------------------------------
; START OF MSDOS 2.0 -IBMDOS.COM- KERNEL CODE (MSDOS.SYS) -will be relocated-
; ----------------------------------------------------------------------------

MSDOS_BIN_OFFSET:  ; this offset must be paragraph aligned
incbin		'MSDOS2.BIN'  ; 18/07/2018
msdos_bin_size	equ $ - MSDOS_BIN_OFFSET

align 2
END_OF_KERNEL equ $