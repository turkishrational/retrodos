; ****************************************************************************
; RETRODOS.SYS (MSDOS 3.3 Kernel) - RETRO DOS v3.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 16/01/2024 (BugFix) - (Previous: 28/08/2023)
; ----------------------------------------------------------------------------
; Beginning: 24/02/2018 (Retro DOS 2.0), 25/05/2018 (Retro DOS 3.0)
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm retrodos.s -l retrodos.lst -o MSDOS.SYS)) 	
; ----------------------------------------------------------------------------
; This assembly file has three parts: (BIOS+SYSINIT+KERNEL in one kernel file)
; 1a) IBMBIO.COM (IO.SYS) part from beginning to '%incbin MSDOS.BIN'
; 1b) SYSINIT part (at the end of IBMBIO.COM/IO.SYS) as '%incbin SYSINIT.BIN'
; 2) MSDOS.SYS (MSDOS 3.3 Kernel) as included binary file (MSDOS.BIN).
; source : 'retrodos.s' (main), 'sysinit.s' (included), 'msdos.s' (included)	
;
; (Note: RETRO DOS 3.0 boot sector code will load 'MSDOS.SYS' at 1000h:0000h)
; (Original MSDOS 3.3 IBMBIO.COM loads/runs its own init code at 0070h:0000h)

;=============================================================================
; Modified from 'retrodos.s', Retro DOS v1.0 Kernel ("IBMBIO.COM") Source code
; by Erdogan Tan, 20/02/2018
;=============================================================================

; MSBIO (IO.SYS 3.3) source files:
; 	MSBIO1.ASM,MSCON.ASM,MSAUX.ASM,MSLPT.ASM,MSCLOCK.ASM,MSDISK.ASM
;	MSBIO2.ASM,MSHARD.ASM,MSINIT.ASM 
;	SYINIT1.ASM,SYSCONF.ASM,SYSINIT2.ASM,SYSIMES.ASM

;=============================================================================
; MSBIO
;=============================================================================
;msbio1+mscon+msaux+mslpt+msclock+msdisk+
;msbio2+mshard+msinit+sysinit1+sysconf+sysinit2+sysimes,
;msbio

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
; Retro DOS v2.0 feature only !
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

; 27/05/2018 - Retro DOS v3.0 
; [MSDOS 3.3, MSDISK.ASM]

struc INT13FRAME
.oldbp:	resw 1
.oldax:	resw 1
.oldbx:	resw 1
.oldcx:	resw 1
.olddx:	resw 1
.olddd:	resd 1
.oldf:	resw 1
.size:
endstruc

; 02/06/2018 - Retro DOS v3.0
; [MSDOS 3.3, BIOSTRUC.INC]

struc ROMBIOS_DESC		; BIOS_SYSTEM_DESCRIPTOR						  
.bios_SD_leng:		resw 1				  
.bios_SD_modelbyte:	resb 1					  
.bios_SD_scnd_modelbyte: 
			resb 1					  
			resb 1					  
.bios_SD_featurebyte1:	resb 1					  
			resb 4					  
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

;align 2

		; 08/04/2018
		; Retro DOS v2.0 (IBMBIO.COM, IBMDOS 2.1)
		; ((Disassembler: IDA Pro Free))

;
; Disk:
;

		;nop
DSKTBL:
		DB	24		; This is the size of the table YUK!!!!
		DW	DSK$INIT	; code	0: INIT
		DW	MEDIA$CHK	; code	1: Media Check
		DW	GET$BPB 	; code	2: BUILD BPB
		DW	CMDERR		; code	3: IOCTL input
		DW	DSK$READ	; code	4: INPUT
		DW	BUS$EXIT	; code	5: NONDESTRUCITVE INPUT, NO WAIT
		DW	EXIT		; code	6: INPUT STATUS
		DW	EXIT		; code	7: INPUT FLUSH
		DW	DSK$WRIT	; code	8: OUTPUT
		DW	DSK$WRITV	; code	9: OUTPUT with verify
		DW	EXIT		; code 10: OUTPUT STATUS
		DW	EXIT		; code 11: OUTPUT FLUSH
		DW	CMDERR		; code 12: IOCTL output
TABLE_PATCH:
		DW	DSK$OPEN	; code 13: DEVICE OPEN
		DW	DSK$CLOSE	; code 14: DEVICE CLOSE
		DW	DSK$REM 	; code 15: REMOVABLE MEDIA
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	GENERIC$IOCTL
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	IOCTL$GETOWN
		DW	IOCTL$SETOWN


;
; Console:
;
		;nop
		db	0
CONTBL:
		DB	10
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
		DW	EXIT

;
; Auxilary:
;
		;nop
		db	0
AUXTBL:
		DB	10
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
		;nop
		db	0
TIMTBL:
		DB	9
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
		;nop
		db	0
PRNTBL:
		DB	24
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
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	PRN$TILBUSY
		DW	EXIT
		DW	EXIT
		DW	PRN$GENIOCTL
		DW	EXIT
		DW	EXIT
		DW	EXIT
		DW	CMDERR
		DW	CMDERR

; 11/06/2018 - Retro DOS v3.0 (MSDOS 3.3, MSDATA.INC, 24/07/1987) 

	;EVENB
align 2
OLD13:
		db	'5986'		;Code for 3.30
ORIG13:
		db	'21',0,0	;Code for 3.30

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
		DB	0,0,0,0	; SET OF 1 BYTE BUFFERS FOR COM 1,2,3, AND 4


; 12/06/2018 - Retro DOS v3.0 (MSDOS 3.3, MSDATA.INC, 24/07/1987)

	; EVENB
align 2

PrevOper:
		DW	0	; Holds ROM DISK INT request (i.e. Register AX).
Number_Of_Sec:
		DB	0	; Holds number of sectors to read on an ECC error

;;Rev 3.30 Modification
;	IF	($-CODE) GT 100H
;	    %OUT VDISK BUFFER NOT CORRECTLY LOCATED
;	ELSE
;	    ORG 100H
;	ENDIF
;	PUBLIC	VDISK_AREA
;VDISK_AREA DB	108 DUP(0)	;FOR USE BY VDISK
;;End of Modification

align 256 ; 13/06/2018

VDISK_AREA:
	times	108 db 	0	;FOR USE BY VDISK	

;align 2

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
		DW	CHARDEV + OUTTILBUSY + DEV320
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
		DW	0000100001000000B	; DOS 3.2
		DW	STRATEGY		; strategy routine
		DW	DSK$IN			; Interrupt entry point

;
; maximum number of drives
;

DRVMAX:
		DB	4

; 25/05/2018 - Retro DOS 3.0
STEP_DRV:	DB	-2
PHYS_DRV:	DB	0

; 11/06/2018 - Retro DOS 3.0
fHave96:	db	0
fHaveK09:	db	0
Single:		db	0 ; 13/06/2018
fSetOwner:	db	0
NEW_ROM:	db	0
Secrete_Code:	dw	'kj'		;Code for 3.30.

;
; Last drive accessed
;

		; 09/04/2018
;MEDIACHK_DRV:  ;; 13/04/2018
;		DB	0  ; 25/05/2018 
		
;TIM_LO:
;		Dw	0FFFFh
;TIM_HI:
;		Dw	0FFFFh
;WRTVERIFY:
;RFLAG:
;		DB	ROMRead		;2 for read, 3 for write
;VERIFY:
;		DB	0		;1 if verify after write
;Single:
;		DB	0
;CURSECBUF:
;		DB	0
;SECCNT:
;		DW	0
;HARDNUM:
;		DB	99		;logical drive number of first hardfile	
;DRVNUM:
;		DB	0
;CURHD:
;		DB	0
;CURSEC:
;		DB	0
;CURTRK:
;		DW	0
;SPSAV:
;		DW	0

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
		DW	CHARDEV + OUTTILBUSY + DEV320
		DW	STRATEGY
		DW	PRN1$IN ; 03/08/2019
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
		DW	CHARDEV + OUTTILBUSY + DEV320
		DW	STRATEGY
		DW	PRN2$IN ; 03/08/2019
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
		DW	CHARDEV + OUTTILBUSY + DEV320
		DW	STRATEGY
		DW	PRN3$IN ; 03/08/2019
		DB	'LPT3    '

		; 25/05/2018

;
; Device Header for device "COM2"
;

	;EVENB

;align 2

COM2DEV:
		;DD	COM3DEV
		DW	COM3DEV
		DW	KERNEL_SEGMENT
		DW	1000000000000000B	; attribute word, character device
		DW	STRATEGY		; device strategy routine
		DW	AUX1$IN 		; device interrupt routine
		DB	'COM2    '              ; device name

;;Rev 3.30 Modification
;
; Device header for device "COM3"
;

	;EVENB

;align 2 

COM3DEV:
		;DD	COM4DEV
		DW	COM4DEV
		DW	KERNEL_SEGMENT
		DW	1000000000000000b	; character device attribute
		DW	STRATEGY
		DW	AUX2$IN			; com3 == aux2
		DB	'COM3    '

;
; Device header for device "COM4"
;

	;EVENB

;align 2 

COM4DEV:
		DW	-1
		DW	KERNEL_SEGMENT
		DW	1000000000000000b	; character device attribute
		DW	STRATEGY
		DW	AUX3$IN			; com4 == aux3
		DB	'COM4    '


; Hard-wire the link to the next Int2f handler.
;;Rev 3.30 Modification
		
		;EVENB
;align 2

NEXT2F_13:
		; 13/06/2018
		DW	INT2F_DISK		; MSBIO2.ASM
		DW	KERNEL_SEGMENT

;
; Start of linked list of BDS's
;

		;EVENB
;align 2

START_BDS:
		; 13/06/2018
       		Dw      BDS1			;START OF BDS LINKED LIST.
		DW	KERNEL_SEGMENT

;;End of Modification


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
; This possibility is detected by counting the number of disk operations
; which occur without any time passing.  If this count exceeds the value of
; "AccessMax" we assume the counter is invalid and always return "Don't
; Know".  The variable "AccessCount" is used to keep track of the number
; of disk operation which occur without the time changing.
;

AccessCount:
		DB	0		; number of times media check called
TIM_DRV:
		DB	-1		; time when last disk I/O was performed
FlagBits:
		DW	0		; Bits to set in flag field when doing
					; a Set_Changed_DL
MedByt:
		DB	0		; hold media byte from floppy

;		;EVENB
align 2
		; 13/06/2018
WRTVERIFY:
RFLAG:
		DB	ROMRead		;2 for read, 3 for write
VERIFY:
		DB	0		;1 if verify after write
SECCNT:
		DW	0
HARDNUM:
		DB	99		;logical drive number of first hardfile	

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

MotorStartup:
		DB	0			; value from table
SettleCurrent:
		DB	0			; value from table
SettleSlow:
		DB	0			; slow settle value
NextSpeed:
 		DB	0			; value of speed to be used
Save_head_sttl:
		DB	0			;used by READ_SECTOR routine
EOT:
		DB	9

; 11/06/2018

;
; pointer to Disk Parameter Table
;
;
;		;EVENB
align 2
DPT:
		dd	0

; 13/06/2018
; 23/03/2018
;
; The following two sets of variables are used to hold values for
; disk I/O operations
; Keep the next two items contiguous - see IOCTL_Block for reason

CURSEC:	
		DB	0			; current sector
CURHD:
		DB	0			; current head
CURTRK:
		DW	0			; current track
SPSAV:
		DW	0			; save the stack pointer

;
; The following are used for IOCTL function calls
;

; 11/06/2018 - Retro DOS v3.0
FORMT_EOT:
		DB	8			; EOT used for format
HDNUM:
		DB	0			; Head number
TRKNUM:
		DW	0			; Track being manipulated
GAP_PATCH:
		DB	50h			; Format gap patched into DPT

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
MediaByte:
		db	0
		dw	0
		dw	0
		dw	0
		dw	0
		db	0
		;db	512-($-DiskSector) dup (?)
		times	512-($-DiskSector) db 0


; 25/05/2018 (04/04/2018)

;---------------------------------------------------------------------
;
;	The "BDS"'s contain information for each drive in the system.
;  There is one BDS for each logical drvie in the system.  The BDS's
;  are all linked together in a chain.	The BDS contain various values
;  important to the disk drive. Various values are updated whenever actions
;  are performed.  For example if a drive is read from the last time
;  accessed fields are updated to the current time.
;	Initial values:
;    *	Sectors/Alloc. unit in BPB initially set to -1 to signify that
;	the BPB has not been filled.
;    *	Link is set to -1 to signify end of list.
;    *	number of cylinders in MaxParms initialized to -1 to indicate
;	that the parameters have not been set.
;
;  START_BDS contains a pointer to the first BDS.  It is through this
;  pointer that routines find particular BDS (see SetDrive to see how
;  this is done).
;

	;EVENB
;align 2

BDS1:
		;DD	BDS2			;LINK TO NEXT STRUCTURE
		DW	BDS2
		DW	KERNEL_SEGMENT  ; 28/03/2018
		DB	0			;ROM DISK INT Drive Number
		DB	0			;Logical Drive Letter
FDRIVE1:
		DW	512			;Physical sector size in bytes
		DB	-1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	64			;Number directory entries
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	00000000b		;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	1			;Head limit
		DW	0			;Hidden sector count
		DB	0			; TRUE => Large fats
OPCNT1:
		DW	0			;Open Ref. Count
VOLID1:
		DB	"NO NAME    ",0         ;Volume ID for this disk
		DB	3			;Form Factor
FLAGS1:
		DW	0020H			;Various Flags
	;	DB	9 dup (0)		;Reserved for future use
		dw	40			; number of cylinders
RecBPB1:
		DW	512			;Physical sector size in bytes
		DB	1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	0E0H			;NUMBER DIRECTORY ENTRIES
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	0F0h			;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	2			;HEAD LIMIT
		DW	0			;Hidden sector count
		;DB	12 DUP (?)
		times	12 db 0
TRACK1:
		DB	-1			;Last track accessed on this drive
TIM_LO1:
		DW	-1			;Keep these two contiguous (?)
TIM_HI1:
		DW	-1

	;EVENB
align 2
BDS2:
		;DD	BDS3			;LINK TO NEXT STRUCTURE
		DW	BDS3
		DW	KERNEL_SEGMENT ; 28/03/2018
		DB	0			;INT 13 DRIVE NUMBER
		DB	0			;Logical Drive Letter
FDRIVE2:
		DW	512			;Physical sector size in bytes
		DB	-1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	64			;Number directory entries
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	00000000b		;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	1			;Head limit
		DW	0			;Hidden sector count
		DB	0			; TRUE => Large fats
OPCNT2:
		DW	0			;Open Ref. Count
VOLID2:
		DB	"NO NAME    ",0         ;Volume ID for this disk
		DB	3			;Form Factor
FLAGS2:
		DW	0020H			;Various Flags
	;	DB	9 dup (0)		;Reserved for future use
		dw	40			; number of cylinders
RecBPB2:
		DW	512			;Physical sector size in bytes
		DB	1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	0E0H			;NUMBER DIRECTORY ENTRIES
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	0F0h			;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	2			;HEAD LIMIT
		DW	0			;Hidden sector count
		;DB	12 DUP (?)
		times	12 db 0
TRACK2:
		DB	-1			;Last track accessed on this drive
TIM_LO2:
		DW	-1			;Keep these two contiguous (?)
TIM_HI2:
		DW	-1

	;EVENB
align 2
BDS3:
		;DD	BDS4			;LINK TO NEXT STRUCTURE
		DW	BDS4
		DW	KERNEL_SEGMENT ; 28/03/2018
		DB	0			;INT 13 DRIVE NUMBER
		DB	0			;Logical Drive Letter
FDRIVE3:
		DW	512			;Physical sector size in bytes
		DB	-1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	64			;Number directory entries
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	00000000b		;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	1			;Head limit
		DW	0			;Hidden sector count
		DB	0			; TRUE => Large fats
OPCNT3:
		DW	0			;Open Ref. Count
VOLID3:
		DB	"NO NAME    ",0         ;Volume ID for this disk
		DB	3			;Form Factor
FLAGS3:
		DW	0020H			;Various Flags
	;	DB	9 dup (0)		;Reserved for future use
		dw	40			; number of cylinders
RecBPB3:
		DW	512			;Physical sector size in bytes
		DB	1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	0E0H			;NUMBER DIRECTORY ENTRIES
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	0F0h			;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	2			;HEAD LIMIT
		DW	0			;Hidden sector count
		;DB	12 DUP (?)
		times	12 db 0	
TRACK3:
		DB	-1			;Last track accessed on this drive
TIM_LO3:
		DW	-1			;Keep these two contiguous (?)
TIM_HI3:
		DW	-1

	;EVENB
align 2
BDS4:
		DW	-1			;Link to next structure
		;DW	Code
		DW	KERNEL_SEGMENT
		DB	0			;INT 13 DRIVE NUMBER
		DB	0			;Logical Drive Letter
FDRIVE4:
		DW	512			;Physical sector size in bytes
		DB	-1			;Sectors/allocation unit
		DW	1			;Reserved sectors for DOS
		DB	2			;No. allocation tables
		DW	64			;Number directory entries
		DW	9*40			;Number sectors (at 512 bytes ea.)
		DB	00000000b		;Media descriptor, initially 00H.
		DW	2			;Number of FAT sectors
		DW	9			;Sector limit
		DW	1			;Head limit
		DW	0			;Hidden sector count
		DB	0			; TRUE => Large fats
OPCNT4:
		DW	0			;Open Ref. Count
VOLID4:
		DB	"NO NAME    ",0         ;Volume ID for this disk
		DB	3			;Form Factor
FLAGS4:
		DW	0020H			;Various Flags
	;	DB	9 dup (0)		;Reserved for future use
		dw	40			; number of cylinders
;;Rev 3.30 Modification
RECBPB4:
		DW	512			;BYTES PER SECTOR
		DB	1			;SECTORS/ALLOCATION UNIT
		DW	1			;RESERVED SECTORS FOR DOS
		DB	2			;NO. ALLOCATION TABLES
		DW	0E0H			;NUMBER DIRECTORY ENTRIES
		DW	9*40			;NUMBER SECTORS (AT 512 BYTES EA.)
		DB	0F0H			;MEDIA DESCRIPTOR, INITIALLY F0H.
		DW	2			;NUMBER OF FAT SECTORS
		DW	9			;SECTOR LIMIT
		DW	2			;HEAD LIMIT
		DW	0			;HIDDEN SECTOR COUNT
		;DB	12 DUP (?)
		times	12 db 0
;;End of Modification
TRACK4:
		DB	-1			;Last track accessed on this drive
TIM_LO4:
		DW	-1			;Keep these two contiguous (?)
TIM_HI4:
		DW	-1

struc bpbType
.spf:	resb 	1
.spt:	resb 	1
.cdire:	resb 	1
.csec:	resw 	1
.spa:	resb	1
.chead:	resb	1
.size:
endstruc

SM92:   ; bpbType
		db	3	; .spf
		db	9	; .spt
		db	70h	; .cdire
		dw	2*9*80	; .csec
		db	2	; .spa	
		db	2	; .chead

	
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

		; 25/05/2018
AUX2$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,2
		JMP	short AUXENT
AUX3$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,3
		;JMP	short AUXENT
AUXENT:
		MOV	SI,AUXTBL
		JMP	SHORT ENTRY1

PRN0$IN:
		PUSH	SI
		PUSH	AX
		XOR	AX,AX
		JMP	SHORT PRNENT
PRN1$IN:
		PUSH	SI
		PUSH	AX
		XOR	AL,AL
		MOV	AH,1
		JMP	SHORT PRNENT
PRN2$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,1
		MOV	AH,2
		JMP	SHORT PRNENT
PRN3$IN:
		PUSH	SI
		PUSH	AX
		MOV	AL,2
		MOV	AH,3
PRNENT:
		MOV	SI,PRNTBL
		MOV	[CS:PRINTDEV],AH
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
		; 11/04/2018 (25/05/2018)
		cmp	AL,[CS:SI]		; is command code a valid number?
		ja	SHORT CMDERR		; no, jump to handle error

		;XOR	AH,AH
		CBW				; note that AL <= 15 means OK
		shl	AX,1
		add	SI,AX			; get SI to point to address of routine
		
		xchg	AX,DI			; put proper value back into AX
		les	DI,[BX+TRANS]		; get ES:DI to point to transfer address
		push	CS			; get DS equal to CS
		pop	DS

		cld				; clear the direction flag
		; 25/05/2018
		jmp	WORD [SI+1]		; go to the command


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
		
BUS$EXIT:				; device busy exit

		mov	AH,00000011b		; set error code
		jmp	SHORT ERR1
CMDERR:
		mov	AL,3			; unknown command error
ERR$CNT:
		lds	BX,[CS:PTRSAV]
		;lds	BX,[PTRSAV]	; 11/04/2018
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
		lds	BX,[CS:PTRSAV]
		;lds	BX,[PTRSAV]	; 11/04/2018
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

CHROUT	equ 29H

OUTCHR:
		; 13/06/2018 (IBMDOS 3.3, IBMBIO.COM) ; **
		; 08/04/2018 (IBMDOS 2.1, IBMBIO.COM) ; *
		;sti ; *
		push	AX			; preserve affect registers
		push	SI ; **
		push	DI ; **
		push	BP ; **
		;push	BX			;
		mov	AH,0Eh 			; set command to write a character
		;mov	BH,0			;
		;mov	BL,7			; set foreground color
		mov	BX,7
		int	10h			; call rom-bios
		;pop	BX			;
		pop	BP ; **			; restore registers
		pop	DI ; **
		pop	SI ; **
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

		; 13/06/2018 (Retro DOS v3.0, IBMDOS 3.0, IBMBIO.COM) 
;CONBUSJ:
;		JMP	short CONBUS

CON$RDND:
		mov	AL,[ALTAH]		; first see if there is a
		or	AL,AL			; character in the buffer?
		;jz	short RD1		; with debugging code it is
		;jmp	short RDEXIT		; too far for conditional jump
		jnz	short RDEXIT

RD1:				     		; set command to 'see if
		mov	AH, 1	     		; character available'
		int	16h	     		; call ROM-BIOS keyboard routine
		jz	short nochr		; with debugging code it is
		jmp	short gotchr		; to far for conditional jump
nochr:
		cmp	byte [fHaveK09], 0
		;jz	short CONBUSJ
		jz	short CONBUS
		lds	bx,[PTRSAV]		; get pointer to request header
		test	word [bx+STATUS],0400h	; System WAIT enabled?
		;jz	short CONBUSJ 		; no, get out
		jz	short CONBUS

		mov	AX, 4100h     		; set command for Wait on External
				      		; event and condition type to
				      		; any external event
		xor	BL,BL			; no timeout value
		int	15h	      		; call rom-bios sleep function
		jmp	short CONBUS		; after wait exit to con busy

gotchr:
		or	AX, AX			; check for null after break
		JNZ	short NOTBRK		; no, skip down
		;mov	ah, 0
; note: AH is already zero, no need to set command
		int	16h			; yes, read the null
		Jmp	short CON$RDND		; and get a real status

NOTBRK:
		cmp	AX, 7200H		; check for ctrl-prtsc
		jnz	short RDEXIT		; no
		mov	AL, 16			; yes, indicate ctrl-prtsc

RDEXIT:
		lds	BX, [PTRSAV]		; get pointer to request header
		mov	[BX+MEDIA], AL		; move character into req. header
EXVEC:
		jmp	EXIT			; all done -- successful return
CONBUS:
		Jmp	BUS$EXIT		; done -- con device is busy

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
		; 13/06/2018 - Retro DOS v3.0 (IBMDOS 3.3, IBMBIO.COM)
		mov	AL,[ES:DI]		; get character
		inc	DI			; point to next character
		int	CHROUT			; Output character
		loop	CON$LP			; repeat until all through
		jmp	EXIT

;-----------------------------------------------
;
;	BREAK KEY HANDLING
;

CBREAK:
		mov	byte [CS:ALTAH], 3	; indicate break key set

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

;------------------------------------------------------------------
;								  :
;	Read zero or more characters from Auxilary Device	  :
;								  :
;	input:es:[di] points to area to receive aux data	  :
;	      cx has number of bytes to be read 		  :
;	      "auxnum" first byte has number of aux device (rel 0):
;								  :
;------------------------------------------------------------------

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
		test	ah,FLAG_FRAME|FLAG_PARITY|FLAG_OVERRUN
		jz	short AROK	;No error if all bits are clear

		; 26/05/2018
		;Error getting character
		;add	sp,2		;Remove rtn address (near call)
		;xor	al,al
		;or	al,FLAG_REC_SIG | FLAG_DSR | FLAG_CTS

		; 13/06/2018 (IBMDOS 3.3, IBMBIO.COM)
		pop	ax
		mov	al,FLAG_REC_SIG+FLAG_DSR+FLAG_CTS 

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
		jmp	RDEXIT		; return character

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
		mov	byte [BX],0	; zero out buffer
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
									   
									   
; WARNING!!! THE IBM ROM DOES NOT RETURN JUST ONE BIT. IT RETURNS A	   
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
		JZ	short CHECKNOTREADY	; NO, TRY NOT READY		   
									   
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

;26/05/2018

;----------------------------------------------------------------
;								:
;		Output until Busy				:
;								:
; Output until busy.  This entry point is used EXCLUSIVELY by	:
; the print spoolers.  Under no curcumstances should the device :
; driver block waiting for the device to become ready.		:
;								:
;   Inputs:	CX has count of bytes to output.		:
;		ES:DI points to source buffer			:
;   Outputs:	Set the number of bytes transferred		:
;		  appropriately.				:
;								:
;----------------------------------------------------------------

PRN$TILBUSY:
	push	DS			; save DS
	push	ES			; copy ES to DS
	pop	DS
	mov	SI,DI			; everything is set for LODSB
PRN$TilBLoop:
	push	CX
	push	BX
	xor	BX,BX
	mov	BL,[CS:PRINTDEV]
	shl	BX,1
	mov	CX,[CS:BX+WAIT_COUNT]	; wait COUNT times to come ready
	pop	BX
PRN$GetStat:
	call	PRNSTAT 		; get status
	jnz	short PRN$BPERR		; if error jump to error routine
	TEST	AH,10000000B		; READY YET?
	loopz	PRN$GetStat		; if busy keep trying
	pop	CX			; get original count
	jz	short PRN$BErr		; still not ready => done
	lodsb
	XOR	AH,AH
	call	PRNOP			; print the character
	jnz	short PRN$BErr		; error
	loop	PRN$TilBLoop		; go for more
PRN$B:
	pop	DS			; recover DS
	lds	BX,[CS:PTRSAV]		; get pointer to header

	sub	[BX+COUNT],CX		; Determine number of succ. I/O's
	jmp	EXIT			; all done, successful return

PRN$BPERR:
	pop	CX			; recover number of char left
PRN$BErr:
	pop	DS			; get pointer to header
	lds	BX,[CS:PTRSAV]
	sub	[BX+COUNT],CX		; Determine number of succ. I/O's
	jmp	ERR$EXIT		; jump to error exit

;
; Prn$GenIOCTL:
;
; Manipulates the value in WAIT_COUNT depending on the value passed in the
; Generic IOCTL packet.
; It either sets or returns the current value for the retry count for the
; device.
;

PRN$GENIOCTL:

	les	di,[PTRSAV]
	cmp	byte [es:di+IOCTL_REQ.MAJORFUNCTION],IOC_PC
	je	short PrnFunc_OK
PrnFuncErr:
	jmp	CMDERR

PrnFunc_OK:
	mov	al,[es:di+IOCTL_REQ.MINORFUNCTION]
	les	di,[es:di+IOCTL_REQ.GENERICIOCTL_PACKET]
	xor	bx,bx
	mov	bl,[PRINTDEV]		; get index into retry counts
	shl	bx,1
	mov	CX,[BX+WAIT_COUNT]	; pull out retry count for device
	cmp	al,GET_RETRY_COUNT
	jz	short PrnGetCount
	cmp	al,SET_RETRY_COUNT
	jnz	short PrnFuncErr
	mov	cx,[es:di+A_RETRYCOUNT.RC_COUNT]  ; A_RETRYCOUNT.RC_COUNT = 0 
PrnGetCount:
	mov	[BX+WAIT_COUNT],CX	; place "new" retry count
	mov	[es:di+A_RETRYCOUNT.RC_COUNT],cx ; return current retry count
	jmp	EXIT

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

;26/05/2018

; variables for real time clock setting
HaveCMOSClock:
		db	0	;set by MSINIT.
base_century:
		db	19
base_year:
		db	80
month_tab:
		db	31,28,31,30,31,30,31,31,30,31,30,31

; The following are indirect intra-segment call addresses. The
;procedures are defined in MSINIT for relocation.  MSINIT will set these
;address when the relocation is done.
	
BinToBCD:
		dw	0	;should point to Bin_To_BCD proc in MSINIT

DaycntToDay:
		dw	0	;should point to Daycnt_to_day in MSINIT

;********************************************************************
; Indirect call address of TIME_TO_TICKS procedure.
;This will be used by the relocatable portable suspend/resume code.

TimeToTicks:
		dw	TIME_TO_TICKS

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
		
		;11/06/2018
		;26/05/2018

		;;Rev 3.30 Modification
		cmp	byte [HaveCMOSClock], 0
		je	short No_CMOS_1
		mov	al,[es:di+3]		;get binary hours
		call	word [BinToBCD]		;convert to BCD
		mov	ch,al			;CH = BCD hours
		mov	al,[es:di+2]		;get binary minutes
		call	word [BinToBCD]		;convert to BCD
		mov	cl,al			;CL = BCD minutes
		mov	al,[es:di+5]		;get binary seconds
		call	word [BinToBCD]		;convert to BCD
		mov	dh,al			;DH = BCD seconds
		mov	dl,0			;DL = 0 (ST) or 1 (DST)
		cli				;turn off timer
		mov	ah,03h			;set RTC time
		int	1Ah			;call rom bios clock routine
		sti
		;;End of Modification
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
		;CMOS clock -------------------------------------
		cmp	byte [HaveCMOSClock], 0
		je	short No_CMOS_2
		; 13/06/2018
		call	word [DaycntToDay]	; convert to BCD format
		cli				; Turn off timer
		mov	AH,05h			; set RTC date
		int	1Ah			; call rom-bios clock routines
		sti
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
; 26/05/2018 - Retro DOS v3.0
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

MAXERR	EQU 5
LSTDRV	EQU 0504h

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

AccessMax EQU 5

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

fTOOBIG	EQU 80h
fBIG	EQU 40h

error_unknown_media equ	7	; for use in BUILD BPB call

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
		; 13/06/2018
		; 26/05/2018 - Retro DOS v3.0
		; (Volume Serial Number Check)

		; 08/04/2018
		; Retro DOS v2.0
		; (Media check code here is mix of MSDOS 2.0, MSDOS 3.3
		; and Retro DOS v2.0 special media -disk change- check code..)
		; (..it is not compatible with MSDOS 3.3 mediacheck)
		; (Excluded methods: Volume Serial Number Check,
		; "fChanged_By_Format" check -via int 13h hook-)

		; 26/05/2018

		call	SETDRIVE ; point DS:DI to BDS for specified drive	

		cmp	word [CS:Secrete_Code], 'kj' ; Secrete code for
		jne	short Media$Done	; DOS 3.3 MSBIO.
		
		;cmp	AL, 1	; Retro DOS v2.0 method
				; We will not check disk change status
				; if disk/unit number > 1.
				; (hard disk or diskette 3 ! or diskette 4 !)
		;ja	short Media$Done

;
; For non-removable disks only return changed if changed by format,
; otherwise return 'not changed'.
;
		mov	si,1		; assume no change
		test	word [DI+BDS.Flags],fChanged_By_Format
		jz	short WeAreNotFakingIt
					; reset flag
		and	word [DI+BDS.Flags],~fChanged_By_Format

;
; If media has been changed by format, must use the ROM.
; Cannot rely on the 2 second time check.
;
		mov	byte [CS:TIM_DRV],-1  ; Ensure that we ask the ROM if
					      ; media has changed
		test	word [DI+BDS.Flags],fNon_Removable
		jz	short WeHaveAFloppy
		mov	SI,-1			; Indicate media changed
		jmp	short Media$Done
;
; return 'not changed' if disk is a hard file.
;

WeAreNotFakingIt:
		test	word [DI+BDS.Flags],fNon_Removable
		jnz	short Media$Done

		;
		; return 'not changed' if disk is a hard file.
		;

		;mov	si,1    ; not changed

		;cmp	al, [CS:HARDNUM]
		;jnb	short Media$Done ; fixed/hard disk !

;
; If this code is reached disk is a diskette drive
;

WeHaveAFloppy:
		xor	si,si	; Presume "I don't know"

		; 11/04/2018
		;mov	[CS:MEDIACHK_DRV], al ; Retro DOS v2.0 method 
		; 13/04/2018
		;mov	[MEDIACHK_DRV], al

;
; If drive is a floppy with changeline support the rom is called to
; determine if the media has changed. It is not necessary to do the 2
; second check on these drives.
;
		;CALL	MediaCheck

		; DL = drive number (0..3) ; 13/04/2018

		;and	si, si
		;jnz	short Media$Done

		; SI = 0, "I don't know" (if media has been changed or not!?)

		;inc	si	; 1 = no change
		 
		; 26/05/2018 - Retro DOS v3.0
		;----------------------------------------|
		; Warning: Do not change the following. ;|
		;	   It gets patched in MSINIT	;|
							;|
Media_Patch:						;|
		CALL	MediaCheck			;|
		jc	short ERR$EXITJ			;|
		call	HasChange			;|
		jnz	short Media$Done		;|
		;----------------------------------------|

;
; If this code is reached the drive is a floppy with no changeline support
;

		MOV	SI,1			; PRESUME NO CHANGE
		mov	al,[CS:TIM_DRV] 	; last drive accessed
						;is drive of last access the same?
		;mov	al,[TIM_DRV]
		;;CMP	AL,[CS:MEDIACHK_DRV]
		;;cmp	al,[MEDIACHK_DRV]  ; 11/04/2018
		;CMP	AL,DL ; 13/04/2018

		cmp	al,[DI+BDS.DriveNum] ; 26/05/2018
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
		les	bx,[CS:PTRSAV]		; get original packet
		;les	bx,[PTRSAV] ; 11/04/2018
		mov	[ES:BX+TRANS],SI
		or	SI,SI
		;jns	EXIT
		js	short INIT_PATCH
		jmp	EXIT

		;; make sure we ask ROM for media check
		;mov	byte [CS:TIM_DRV],-1
		;;mov	byte [TIM_DRV],-1 ; 11/04/2018
		;jmp	EXIT

		; 26/05/2018 - Retro DOS v3.0
		;----------------------------------------|
		; Warning: Do not change the following. ;|
		;	   It gets patched in msinit	;|
							;|
INIT_PATCH:						;|
		CALL	MEDIA_SET_VID			;|
		;----------------------------------------|
		mov	byte [cs:TIM_DRV],-1 ; make sure we ask ROM for media check
VOLIDOK:
		jmp	EXIT

ERR$EXITJ:
		CALL	MAPERROR
		JMP	ERR$EXIT

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
		; 27/05/2018 - Retro DOS v3.0
		mov	si,1			; Presume no change
		;;Rev 3.30 Modification
		xor	AH, AH			; set command to read time
		int	1Ah			; call rom-bios clock routine

;
; Once time is read, must make sure the date wrap is not lost.	The ROM will
; return the value only once, it must check for day wrap on each call.
;
		SHR	AL,1
		ADC	word [CS:DAYCNT],0	; ADD IT TO OUR SAVED DAY COUNT
		;ADC	word [DAYCNT], 0 ; 11/04/2018
;
; Compute elapsed time1
;
		; 27/05/2018
		; Retro DOS v3.0
		MOV	AX,[DI+BDS.TIM_LO]	; GET STORED TIME
		SUB	DX,AX
		MOV	AX,[DI+BDS.TIM_HI]
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
		inc	byte [cs:AccessCount]
						; Exceeded threshold for count?
		cmp	byte [cs:AccessCount],AccessMax
		jb	short TimeCheck_Ret	; no, return media unchanged
		dec	byte [cs:AccessCount]	; don't let the count wrap
		jmp	short TimeCheck_Unk	; "I don't know" if media changed

;
; If this code is reached some time has passed.  Need to determine if
; 2 seconds have passed.  Note: 18.2 ticks per second.
;
TimePassed:
		CMP	DX,18*2			; IF ( Time_passed <= 2secs )
		JBE	short TimeCheck_Ret	;      presume no change


; Everything indicates that we do not know what has happened.
;
TimeCheck_Unk:
		DEC	SI			; Presume I don't know
TimeCheck_Ret:
		RETN

ERR$EXITJ2:
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
;	ES:[DI] points to a buffer; the first byte of the buffer is a	:
;	   media decriptor byte.					:
;									:
;------------------------------------------------------------------------
;
; Build a valid BPB for the disk in the drive.
;

GET$BPB:
		mov	AH,[ES:DI]		; get FAT ID byte read by DOS
		call	SETDRIVE		; get the correct BDS for the drv
		; 27/05/2018
		;;Rev 3.30 Modification
		TEST	word [DI+BDS.Flags],fNon_Removable
		JNZ	short ALREADY_GOTBPB	; NO NEED TO BUILD FOR FIXED DISKS
		;End of Modification
		call	GETBP			; build a BPB if necessary.
		jc	short ERR$EXITJ2	; if error exit
SET_PATCH:
		CALL	Set_Volume_ID
ALREADY_GOTBPB:
		add	di,BDS.BytePerSec	; return the BPB that is in the current BDS

SetPTRSAV:					; return point for DSK$INIT
		les	BX,[CS:PTRSAV]
		mov	[ES:BX+MEDIA],AH
		mov	[ES:BX+COUNT],DI
		mov	[ES:BX+COUNT+2],DS
		jmp	EXIT

; 13/06/2018
; 27/05/2018 - Retro DOS v3.0

;
;      GETBP fills the BDS with the BPB for the media currently in the drive.
; The following steps are followed:
;	If the Return_Fake_BPB flag is set then the GETBP just returns.
;	If the BDS is for a hard disk (non-removable) then GETBP returns since
;   the BPB cannot change on a hard disk drive.
;	For all other cases GETBP reads the boot sector and looks for a BPB
;   in the boot sector. (All DOS 2.X and about disks should have a valid
;   BPB in the boot sector.)
;	If no valid BPB is found (DOS 1.X disk) then GETBP reads the FAT
;   sector and gets the FAT ID byte.  With this byte a valid BPB is build.
;
;	Inputs:
;		DS:DI points to correct BDS
;
;	Outputs:
;		Fills in BPB in current BDS if valid BPB or FAT ID on disk.
;		Carry set, and AL=7 if invalid disk.
;		Carry set and error code in AL if other error.
;

GETBP:
					; if non-removable or returning
					; fake BPB then return BPB as is.
		TEST	WORD [DI+BDS.Flags],RETURN_FAKE_BPB|fNon_Removable
		jz	short GETBP1
		JMP	GETRET_Exit

GETBP1:
		push	cx
		push	dx
		push	es
		push	bx
;
; Attempt to read in boot sector and determine BPB.
; We assume that the 2.x and greater DOS disks all have a valid boot sector.
;
Rdboot:
		call	READBOOTSEC
		jc	short GetBP_Err_Ret
		;jnc	short NoRdErr
		;jmp	GetBP_Err_Ret	; Carry set if there was error.
NoRdErr:
		cmp	bx,0		; BX is 0 if boot sector is valid.
		;or	bx, bx
		jnz	short DoFatBPB	; if not go read FAT

		call	MOVBPB		; Move BPB into registers.
		jmp	short HAS1
	
;
; At this point the drive contains a 1.X diskette.  We read the FAT byte
; and fill in the BPB from there.
;

DoFatBPB:
        	call    READFAT		; puts media descriptor byte in AH
        	jc	short GetBP_Err_Ret ; if carry set, there was error, get out

		;----------------------------------------|
		; Warning: Do not change the following. ;|
		;          It gets patched in msinit    ;|
							;|
GETBP1_PATCH:   		                        ;|
		call    hidensity               	;|
		;----------------------------------------|
		
		; Test for a valid 3.5" medium
        	cmp     byte [DI+BDS.FormFactor],ffSmall
        	jnz	short Is_Floppy
        
		cmp     ah,0F9H		; is it a valid fat ID byte for 3.5" ?
		jnz     short Got_Unknown_Medium
        	mov     bx,SM92		; pointer to correct BPB
        	push    cs
        	pop     es
;--------------------------------------------------------------bug330a08
		;mov	al,[es:bx+bpbType.spf]
		mov	al,[es:bx]  ; 05/07/2019
        	mov     cx,[es:bx+bpbType.csec]
        	mov     dx,[es:bx+bpbType.spa]
        	mov     bx,[es:bx+bpbType.spt]
;--------------------------------------------------------------bug330a08
		jmp	short HAS1_res	; Need to load reserved sectors

		; 13/06/2018
GetBP_Err_Ret:
		CALL	MAPERROR
		JMP	SHORT GETRET

;
; must be a 5.25" floppy if we come here
;
Is_Floppy:
		mov     CL,AH		; save media
		and     CL,0F8H		; normalize
		cmp     CL,0F8H		; compare with good media byte
        	jnz     short Got_Unknown_Medium

GOODID: 	mov     AL,1		; set number of FAT sectors
        	mov     BX,64*256+8	; set dir entries and sector max
        	mov     CX,40*8		; set size of drive
        	mov     DX,01*256+1	; set head limit and sec/all unit
       		test    AH,00000010B	; test for 8 or 9 sectors
        	jnz	short HAS8	; NZ = has 8 sectors
        	inc     AL		; inc number of FAT sectors
        	inc     BL		; inc sector max
        	add     CX,40		; increase size
HAS8:   	test    AH,00000001B	; test for 1 or 2 heads
		jz	short HAS1_res	; Z = 1 head
		add     CX,CX		; double size of disk
		mov     BH,112		; increase number of directory entries
		inc     DH		; inc sec/all unit
		inc     DL		; inc head limit
HAS1_res:
		mov	si,[DI+BDS.RESSEC]
					; save values in BDS
HAS1:		mov     [DI+BDS.SecPerClus],DH
        	mov     [DI+BDS.cDir],BH
        	mov     [DI+BDS.DRVLIM],CX
        	mov     [DI+BDS.Mediad],AH
        	mov     [DI+BDS.cSecFat],AL
        	mov     [DI+BDS.SECLIM],BL
        	mov     [DI+BDS.HDLIM],DL
        	mov     [DI+BDS.RESSEC],SI
GETRET: 
		pop     bx
		pop	es
		pop	dx
		pop	cx	
GETRET_Exit:
		retn

;
; We have a 3.5" diskette for which we cannot build a BPB. We do not assume any
; type of BPB for this medium.
;
Got_Unknown_Medium:
		mov	al,error_unknown_media
		stc
		jmp	short GETRET

;
; Read in the boot sector. Set carry if error in reading sector.
; BX is set to 1 if the boot sector is invalid, otherwise it is 0.
;
READBOOTSEC:
		mov	CX,0001h	; set track and sector number
		xor     DH,DH		; set head number for read_sector
		call	READ_SECTOR
		jc	short Err_ret	; error - get out
		xor	bx,bx		; assume valid boot sector.

				; at this point the boot sector has been
				; read in from the disk.  We now need to
				; determine if the boot sector contains
				; a valid BPB.	Currently there are only
				; a few simple checks.	Expanding the
				; number or types of checks would not be
				; a bad idea.

;*******************************************************************************
; Put a sanity check for the boot sector in here to detect boot sectors that
; do not have valid BPBs.
; We examine the first two bytes - they must contain a long jump or a short
; jump followed by a NOP.
; If this test is passed, we further check by examining the signature at
; the end of the boot sector for the word AA55H.
; If the signature is not present, we examine the media descriptor byte to
; see if it is valid.
;******************************************************************************
		cmp	byte [cs:DiskSector],069H    ; Is it a direct jump?
		JE	short Check_bpb_MediaByte    ; DON'T NEED TO FIND A NOP
		cmp	byte [cs:DiskSector],0E9H    ; DOS 2.0 jump?
		JE	short Check_bpb_MediaByte    ; NO NEED FOR NOP
		cmp	byte [cs:DiskSector],0EBH    ; How about a short jump.
		JNE	short INVALIDBOOTSEC
		cmp	byte [cs:DiskSector+2],090H  ; Is next one a NOP?
		JNE	short INVALIDBOOTSEC

; Don't have to perform the following signature check since
; we need to check the media byte even with the good signatured diskette.
;CHECK_SIGNATURE:
;		CMP	word [cs:DiskSector+1FEh],0AA55h ; SEE IF NON-IBM
;							 ; DISK OR 1.X MEDIA.
;		JZ	short CHECKSINGLESIDED ; GO SEE IF SINGLE SIDED MEDIUM.
;					       ; MAY NEED SOME SPECIAL HANDLING
;
; CHECK FOR NON-IBM DISKS WHICH DO NOT HAVE THE SIGNATURE AA55 AT THE
; END OF THE BOOT SECTOR, BUT STILL HAVE A VALID BOOT SECTOR. THIS IS DONE
; BY EXAMINING THE MEDIA DESCRIPTOR IN THE BOOT SECTOR.
;

;;Rev 3.30 Modification
Check_bpb_MediaByte:

		MOV	AL,[CS:MediaByte]
		AND	AL,0F0H
		CMP	AL,0F0H		; ALLOW FOR STRANGE MEDIA
		JNZ	short INVALIDBOOTSEC 
;
; THERE WERE SOME (APPARENTLY A BUNCH) DISKETTES THAT HAD BEEN FORMATTED
; UNDER DOS 3.1 AND EARLIER VERSIONS WHICH HAVE INVALID BPBS IN THEIR BOOT
; SECTORS. THESE ARE SPECIFICALLY DISKETTES THAT WERE FORMATTED IN DRIVES
; WITH ONE HEAD, OR WHOSE SIDE 0 WAS BAD. THESE CONTAIN BPBS IN THE BOOT
; SECT THAT HAVE THE SEC/CLUS FIELD SET TO 2 INSTEAD OF 1, AS IS STANDARD
; IN DOS. TO SUPPORT THEM, WE HAVE TO INTRODUCE A "HACK" THAT WILL
; HELP OUR BUILD BPB ROUTINE TO RECOGNISE THESE SPECIFIC CASES, AND TO
; SET UP OUT COPY OF THE BPB ACCORDINGLY.
; WE DO THIS BY CHECKING TO SEE IF THE BOOT SECTOR IS OFF A DISKETTE THAT
; IS SINGLE-SIDED AND IS A PRE-DOS 3.20 DISKETTE. IF IT IS, WE SET THE
; SEC/CLUS FIELD TO 1. IF NOT, WE CARRY ON AS NORMAL.
CHECKSINGLESIDED:
		MOV	AL,[CS:MediaByte]
		TEST	AL,0001H ; IS LOW BIT SET? - INDICATES DOUBLE SIDED
		JNZ	short GoodDsk
		CMP	word [CS:DiskSector+8],"3."
		JNZ	short MUSTBEEARLIER
		CMP	byte [CS:DiskSector+10],"2"
		JAE	short GoodDsk

; WE MUST HAVE A PRE-3.20 DISKETTE. SET THE SEC/CLUS FIELD TO 1
MUSTBEEARLIER:
		MOV	BYTE [CS:SECPERCLUSINSECTOR],1
		JMP	short GoodDsk

INVALIDBOOTSEC:
		INC	BX		; SET THAT BOOT SECTOR INVALID
;;End of Modification

GoodDsk:				; carry already reset
		clc
Err_ret:
		retn

;Err_Ret:				; carry is already set on entry here
;		retn

;
; MovBPB moves the BPB read from the Boot sector into registers for use by
; GETBP routine at Has1
;
MOVBPB:
		push	ds
		push	di
		push	cs
		pop	ds
		mov	di,BPB_In_Sector
		mov	dh,[DI+BPB_TYPE.SECALL]	;sectors per unit
		mov	bh,[DI+BPB_TYPE.DIRNUM]	;number of directory entries
		mov	cx,[DI+BPB_TYPE.SECNUM]	;size of drive
		mov	ah,[DI+BPB_TYPE.FATID]	;media descriptor
		mov	al,[DI+BPB_TYPE.FATSIZE] ;number of FAT sectors
		mov	bl,[DI+BPB_TYPE.SLIM]	;sectors per track
		mov	dl,[DI+BPB_TYPE.HLIM]	;number of heads
		mov	si,[DI+BPB_TYPE.RESNUM]	;reserved sectors
		pop	di
		pop	ds
		retn

;
; Read in the FAT sector and get the Media Byte from it.
; Input : AL contains logical drive.
; Output:
;	  Carry set if an error occurs, AX contains error code.
;	  Otherwise, AH contains media byte on exit. AL is preserved.

READFAT:
		push	ax		   ; preserve logical drive in AL
		MOV	DH,0		   ; HEAD 0
		mov	CX,2		   ; set track and sector number
		call	READ_SECTOR	   ; CS:BX points to fat sector
		jc	short Bad_FAT_Ret  ; error, get out
		pop	ax		   ; reset logical drive
		mov	ah,[CS:BX]	   ; media byte
		retn

Bad_FAT_Ret:				   ; carry set on entry
		pop	cx	           ; clear stack
		retn

;
; Read_sector reads a single sector into the tempory buffer 'DiskSector'.
; Up to three retries are done in case of error.
;
;    Inputs:
;	DS:DI	points to BDS for drive
;	CH - track number
;	CL - sector number
;	DH - head number
;
;    Outputs:
;	If carry is clear -- successful read
;	   CS:BX points to buffer holding sector
;	   AX, BX are not preserved, CX, DX, BP, and ES are preserved
;
;	If carry is set -- error on read
;	   AX, BX, and DX are not preserved; CX, BP, and ES are preserved
;
;

READ_SECTOR:
		push	BP		; preserve BP register
		mov	BP,3		; BP is retry count, set to 3
		push	ES		; preserve ES also
		mov	DL,[DI+BDS.DriveNum]
		mov	BX,DiskSector	; Get ES:BX to point to buffer
		push	CS		;    get the segment right
		pop	ES		; now ES:BX is correct

RD_RET:
					; set command to read (AH=2) and
		mov	AX,0201h	; number of sectors to 1 (AL=1)
		int	13h		; call rom-bios disk routines
		jnc	short OKRET2	; if no carry then no error - done
Rd_rty:
		call	AGAIN		; reset disk and decrement BP
		jz	short ERR_RD_RET
		test	word [DI+BDS.Flags],fNon_Removable
		JNZ	short RD_RET

;;Rev 3.30 Modification -----------------------------------------
		push	ds		; For retry, set head settle
		push	ax		; time to 0Fh.
		lds	si,[CS:DPT]
		mov	al,[SI+DISK_PARMS.DISK_HEAD_STTL]
		mov	[CS:Save_head_sttl],al
		mov	byte [SI+DISK_PARMS.DISK_HEAD_STTL],NORMSETTLE
		pop	ax
		pop	ds
					; SET CMD TO READ (AH=2) AND
		MOV	AX,0201h	; NUM OF SECTORS TO 1 (AL=1)
		INT	13h		; CALL ROM-BIOS DISK ROUTINES
		push	ds
		push	ax
		lds	si,[CS:DPT]
		mov	al,[CS:Save_head_sttl]
		mov	[SI+DISK_PARMS.DISK_HEAD_STTL],al
		pop	ax
		pop	ds
		jnc	short OKRET2
		jmp	short Rd_rty
ERR_RD_RET:
		MOV	DL,-1	; MAKE SURE WE ASK ROM IF MEDIA  CHANGED
		STC		; RETURN ERROR
;;End of Modification -----------------------------------------

			; Update information pertaining to last drive
			; accessed, time of access, last track accessed
			; in that drive.
OKRET2:
				; set up for head settle logic in DISK
		mov	[CS:STEP_DRV],DL ; save last drive accessed
		mov	[CS:TIM_DRV],DL	; save the values
		mov	[DI+BDS.Track],CH ;
		pushf			; save the flags
		call	SET_TIM
		popf			; restore flags
		pop	ES		; restore registers
		pop	BP
		retn

;27/05/2018 - Retro DOS v3.0

;------------------------------------------------------------------------
;									:
;		Disk Removable Routine					:
;									:
;  This routine determines if a particular logical drive has		:
;  removable media.							:
;									:
;  Input								:
;     AL contains the logical drive number which the check is being	:
;  done.								:
;------------------------------------------------------------------------

DSK$REM:				;ARR 2.41
		call	SETDRIVE	; get BDS for this drive
		test	word [DI+BDS.Flags],fNon_Removable
		jnz	short NON_REM
		jmp	EXIT

NON_REM:				; if non removable set busy bit
		jmp	BUS$EXIT

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
		; 27/05/2018 - Retro DOS v3.0
		; (MSDOS v3.3, 'MSDISK.ASM')
		
		push	bx
		push	cs
		pop	ds

		mov	di,[START_BDS] 
Scan_Loop:
;;Rev 3.30 Modification -----------------------------------------
		CMP	BYTE [CS:PHYS_DRV],1 ; DOES AL HAVE PHYS DRV?
		JB	short USE_LOGICAL_DRV
		CMP	BYTE [DI+BDS.DriveNum],AL
		JE	short SetDrv
		JMP	SHORT GET_NXT_BDS
USE_LOGICAL_DRV:
		CMP	[DI+BDS.DriveLet],AL
		JE	short SetDrv
GET_NXT_BDS:
		MOV	BX,[DI+BDS.Link+2] ; GO TO NEXT BDS
		;MOV	DI,[DI+BDS.Link]
		mov	di,[di] ; 05/07/2019
		mov	ds,bx
;;End of Modification -----------------------------------------

		cmp	di,-1		; at end of list?
		jnz	short Scan_Loop	; no, keep looking
		stc			; yes, indicate error set carry
SetDrv:
		pop	bx		; restore bx
		retn

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
		; 27/05/2018 - Retro DOS v3.0
		push	AX		; save affected registers
		push	BX

		mov	BX,[DI+BDS.Flags]
					;Can't change disk
		TEST	BL,fNon_Removable | fI_Own_Physical
		jnz	short SingleRet	; on hard drive so return
					; is there a drive sharing this
		TEST	BL,fI_Am_Mult	;   physical drive?
		jz	short SingleRet	; if not, then return

			; At this point there is more than one
			; logical drive mapped to this physical drive.
			; But the drive being accessed is not the
			; owner of the physical drive.	What needs to
			; be done is find the current owner BDS and
			; turn off the owner flag and then make current
			; BDS the owner of the drive.  Then prompt the
			; user to change disks.
	
		mov	al,[DI+BDS.DriveNum] ; get physical drive number
		push	ds		; preserve pointer to current BDS
		push	di
		push	cs
		pop	ds		; Point to start of BDS linked list

		mov	di,START_BDS
Scan_List:
		mov	bx,[DI+BDS.Link+2] ; go to next BDS
		;mov	di,[DI+BDS.Link]
		mov	di,[di] ; 05/07/2019
		mov	ds,bx
		
		cmp	di,-1		; end of list?
		jz	short Single_Err_Ret ; if so there must be an error
					; same physical drive?
		cmp	[DI+BDS.DriveNum],al
		jnz	short Scan_List	; no, keep looking

Check_Own:				; yes, check to see if owner
		mov	bx,[DI+BDS.Flags]
		test	bl,fI_Own_Physical
		jz	short Scan_List	; not owner, keep looking
		xor	bl,fI_Own_Physical ; yes owner, reset ownership flag
		mov	[DI+BDS.Flags],bx
		pop	di		; Restore pointer to current BDS
		pop	ds
		xor	bx,bx
		or	bl,fI_Own_Physical ; establish current BDS as owner
		or	[DI+BDS.Flags],bx

			;
			; We examine the fSetOwner flag. If it is
			; set, then we are using the code in
			; CheckSingle to just set the owner of
			; a drive. We must not issue the prompt
			; in this case.
			;

		cmp	byte [cs:fSetOwner],1
		jz	short SingleRet
	
			;
			; To support "backward" compatibility with
			; IBM's "single drive status byte" we now
			; check to see if we are in a single drive
			; system and the Application has "cleverly"
			; diddled the SDSB (Single Drive Status Byte)
			;

		cmp	byte [cs:Single],2	   ; single drive system?
		jne	short Ignore_SDSB  ; no, jump down

		push	ds		   ; yes...		
		push	di
		push	ax

		mov	al,[DI+BDS.DriveLet] ; IF (Curr_drv == Req_drv)
		mov	ah,al
		xor	di,di
		mov	ds,di
		xchg	al,[LSTDRV]	    ; THEN swap(Curr_drv,Req_drv)
		cmp	ah,al		    ; ELSE
		pop	ax		    ;     swap(Curr_drv,Req_drv)	
		pop	di		    ;	  Issue Swap_dsk_msg	
		pop	ds
		je	short SingleRet	

Ignore_SDSB:
		call	SWPDSK		; ask user for correct disk

SingleRet:
		pop	BX		; restore registers
		pop	AX
		retn			; return

Single_Err_Ret:
		stc			; set carry flage to indicate error
		pop	di		; restore current BDS
		pop	ds
		jmp	short SingleRet

;
; BadDrive is called when sector specified is greater than last
; sector on disk.
; or when BDS is not found for drive
;

BadDrive:
		mov	AL,8		; error code 'sector not found'
		stc			; indicate error
IORET:	
		retn			; return

BogusSettle:
		MOV	AL,NORMSETTLE	; someone has diddled the settle
		JMP	GotSlowSettle

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

DISKRD:		; 27/05/2018 - Retro DOS v3.0
		;mov	byte [CS:RFLAG],ROMRead	; set command to read
		mov	byte [RFLAG],ROMRead	 ; 11/04/2018
DISKIO:
		; 13/04/2018
		; 08/04/2018
		; Retro DOS v2.0 (IBMDOS 2.1, IBMBIO.COM)

		;clc
		;jcxz	IORET
		
		;;mov	[CS:SPSAV],SP		; save the sp value
		;mov	[SPSAV],sp

		mov	BX,DI			; ES:BX is transfer address
		call	SETDRIVE		; map logical and physical
		jc	short BadDrive		; carry means BDS not found
		mov	al,[DI+BDS.Mediad]
		mov	[CS:MedByt],al		; Preserve media byte for drive for use
						; in determining media change.
		jcxz	IORET
		mov	[CS:SPSAV],SP		; save the sp value

		; DX = Sector address (offset)
		; BX = Buffer address (offset)
		
; Ensure that we are trying to access valid sectors on the drive
;
		mov	SI,DX			; start with first sector
		add	SI,CX			; add in sector count
		add	DX,[DI+BDS.HIDSEC]	; add in the hidden sectors
		cmp	SI,[DI+BDS.DRVLIM]	; compare against max (volume size)
		ja	short BadDrive		; if greater than max, error
		mov	[CS:SECCNT],CX		; save sector count

;;Rev 3.30 Modification -----------------------------------------
; SET UP POINTER TO DISK BASE TABLE IN [DPT]. WE CANNOT ASSUME THAT IOSETUP
; WILL DO IT BECAUSE WE WILL SKIP THE SET UP STUFF WITH HARD DISKS.
		PUSH	DS
		XOR	AX,AX
		MOV	DS,AX
		LDS	SI,[DSKADR]		; CURRENT DISK PARM TABLE
		MOV	[CS:DPT],SI
		MOV	[CS:DPT+2],DS
		POP	DS
;;End of Modification -----------------------------------------


; For hard drives do not do media check or set DPT.
;

		test	word [DI+BDS.Flags],fNon_Removable
		jnz	short Skip_Setup
	
		CALL	CHECKSINGLE

;
; Check to see if we have previously noted a change line.  The routine
; returns if everything is OK.	Otherwise, it pops off the stack and returns
; the proper error code.
;
		;----------------------------------------|
		; Warning: Do not change the following. ;|
		;	   It gets patched in msinit	;|
							;|
DiskIO_PATCH:						;|
		CALL	CheckLatchIO			;|
		;----------------------------------------|
;
; Set up tables and variables for I/O
		call	IOSETUP
;
; Now the settle values are correct for the following code
;
Skip_Setup:
		mov	AX,DX		; setup locical sector for divide
		xor	DX,DX
		div	word [DI+BDS.SECLIM] ; divide by sectors per track
		inc	DL
		mov	[CS:CURSEC],DL	; save current sector
		mov	CX,[DI+BDS.HDLIM]  ; get number of heads
		xor	DX,DX		; divide tracks by heads per cylinder
		div	CX
		mov	[CS:CURHD],DL	; save current head
		mov	[CS:CURTRK],AX	; save current track
;
; We are now set up for the I/O.  Normally, we consider the DMA boundary
; violations here.  Not true.  We perform the operation as if everything is
; symmetric; let the DISK INT handler worry about the DMA violations.
;
		mov	AX,[CS:SECCNT]
		call	BLOCK
		call	DONE
		retn

; 27/05/2018 - Retro DOS v3.0

; IOSetUp:
;
; IOSetUp does the following functions:
;   *	Set the drive-last-accessed flag (for diskette only).  No need to
;	update these flags for hard disks becuase we know a hard disk will
;	not be removed.
;   *	Set the proper last sector number in the Disk Parameter Table (DPT)
;   *	Set the proper motor start up time in DPT
;   *	Set the proper head settle time in the DPT
;
; Input:
;	DS:DI -> current BDS.
; Output:
;	AX,CX,SI are destroyed.
;

IOSETUP:
		MOV	AL,[DI+BDS.DriveNum]
		MOV	[CS:TIM_DRV],AL	; SAVE DRIVE LETTER
;
; determine proper head settle values
;
		mov	CX,DS
		LDS	SI,[CS:DPT]	; GET POINTER TO DISK BASE TABLE
		MOV	AL,[CS:EOT]
		mov	[SI+DISK_PARMS.DISK_EOT],AL	; bump for us
		mov	AL,[SI+DISK_PARMS.DISK_MOTOR_STRT] 
					; preserve old motor start time
		mov	[CS:MotorStartup],AL
;
; For 3.5" drives, both external as well as on the K09, we need to set the
; Motor Start Time to 4. This checking for every I/O is going to affect
; performance across the board, but is necessary!!
;
		push	es
		mov	es,cx		; ES:DI -> to current BDS
		cmp	byte [es:di+BDS.FormFactor],ffSmall
		jnz	short Motor_Start_OK
		mov	AL,4
		xchg	AL,[SI+DISK_PARMS.DISK_MOTOR_STRT]
Motor_Start_OK:
		pop	ES
;
; DS:SI now points to disk parameter table.  Get current settle and set fast
; settle
;
		XOR	AL,AL
		INC	AL		; IBM WANTS FAST SETTLE = 1 - RS
		xchg	AL,[SI+DISK_PARMS.DISK_HEAD_STTL]
					; get settle and set up for fast
		mov	[CS:SettleCurrent],AL
		MOV	AL,NORMSETTLE	; SOMEONE HAS DIDDLED THE SETTLE
GotSlowSettle:
		mov	DS,CX
		mov	[CS:SettleSlow],AL
		retn

;
; Set time of last access, and reset default values in the DPT.
;
DONE:
		test	word [DI+BDS.Flags],fNon_Removable
		jnz	short RETZ	; Do not set for non-removable Media
		call	SET_TIM		; set time of last access for drive
;
; Restore head settle and EOT values
;
DiddleBack:
		push	ax			; preserve AX
		mov	DX,DS			; save DS in DX
		mov	AL,[cs:SettleCurrent]	; get value in registers
		mov	AH,[cs:MotorStartup]
		lds	SI,[CS:DPT]		; get pointer to DPT
		mov	byte [SI+DISK_PARMS.DISK_EOT],9 ; save values in DPT
		mov	[SI+DISK_PARMS.DISK_HEAD_STTL],AL
		mov	byte [SI+DISK_PARMS.DISK_SECTOR_SIZ],2
		mov	[SI+DISK_PARMS.DISK_MOTOR_STRT],AH
		mov	DS,DX			; restore DS
		pop	ax			; restore AX
RETZ:
		retn

;
; Block reads or writes the number of sectors specified in AX
; handling track boundaries.  For example, on an 8 sector per track
; disk there might be a request to read 6 sectors starting at the 5th
; sector.  Block breaks this request into a read of sectors 5-8 on
; the first track and a read of sectors 1-2 on the next track.	Disk is
; called to do the actual read.
;
;   Inputs:
;	AX - number of sectors to be read
;	DS:DI points to BDS for disk drive
;	cs:CurSec - sector on track where read should start
;	cs:CurTrk - track where read should start
;	cs:CurHd - head for read
;	ES:BX - transfer address
; AX, CX, and BL are not preserved
;

BLOCK:
		or	AX,AX		; see if any sectors to read
		jz	short RETZ	; if not, return
;;Rev 3.30 Modification -----------------------------------------
; Fixed disk will not be restricted to the trk-by-trk basis.
		test	word [DI+BDS.Flags], fNon_Removable
		jz	short BLOCK_FLOPPY
		call	DISK
		xor	ax,ax
		RETN
BLOCK_FLOPPY:
;;End of Modification -----------------------------------------
;
; READ AT MOST 1 TRACK WORTH.  PERFORM MINIMIZATION AT SECTOR / TRACK
;
		mov	CL,[DI+BDS.SECLIM] ; get sectors per track
		inc	CL
		sub	CL,[CS:CURSEC]	; set CX to number of sector after current
		xor	CH,CH		;    sector on the current track
		cmp	AX,CX		; is all of request on current track?
		jae	short GotMin	; no, jump down
		mov	CX,AX	; yes, set number of sector on this track to AX
GotMin:
				; now
				; AX is the requested number of sectors to read
				; CX is the number that we can do on this track
		push	AX
		push	CX
		mov	AX,CX	; AL is number of sectors to read
		call	DISK
		pop	CX
		pop	AX

				; CX is the number of sectors just transferred
		sub	AX,CX	; reduce sectors-remaining by last I/O
		shl	CL,1
		add	BH,CL	; adjust transfer address
		jmp	short BLOCK ; jump to do any remaining sectors


; 27/05/2018 - Retro DOS v3.0
;
;  DISK:
; Disk is called to read or write one or more sectors on a track.
; Retries are make if an error occurs.
;
;    Input:
;	AL - number of sector to be read/written (they must all be on one track)
;	DS:DI points to BDS for the drive
;	ES:BX is transfer address (must not cross 64k physical boundry)
;	[RFLAG] is 2 for read and 3 for write
;	[VERIFY] is 0 for normal, 1 for verify after write
;	[CurTrk] is track (cylinder) to be read/written.
;	[CurHd] is head to be used in operation.
;	[CurSec] is sector to start read on.
;
; The following are overwritten: BP,
;   Output:
;	[SECCNT] is decrement by the number of sectors read or written

DISK:
		mov	BP,MAXERR	; set up retry count
		MOV	AH,[CS:RFLAG]	;GET READ/WRITE INDICATOR

RETRY:
				; AX is overwritten in int 13 call, so
				; to do a retry we need to save the
				; value by pushing on the stack
		push	AX
				; the next five lines of code put the
				; sector number in bit 5-0 of CL and the
				; cylinder number in CH and bits 7-6 of
				; CL.  The register must be set up in this
				; way for the bios.
		mov	DX,[CS:CURTRK]	;Load current cylinder
;;Rev 3.30 Modification -----------------------------------------
		test	word [DI+BDS.Flags],fNon_Removable ;Fixed disk
		jz	short DISK_NOT_MINI 	;no, skip this.
		;cmp	byte [DI+BDSM.isMini], 1
		cmp	word [DI+BDSM.isMini], 1 ;Is this a mini disk?
		jnz	short DISK_NOT_MINI	;No. continue to next.
		add	dx, [DI+BDSM.Hidden_Trks] ;else add hidden trks.
DISK_NOT_MINI:
;;End of Modification -----------------------------------------
		ror	DH,1 ; get high two bits of cylinder in correct place
		ror	DH,1

		or	DH,[cs:CURSEC]	; get sector value
		mov	CX,DX ; put cylinder/sector values in correct register
				; get head value
		xchg	CH,CL	; put bytes in correct place
		mov	DH,[CS:CURHD]
				; get drive number
		mov	DL,[DI+BDS.DriveNum]

		CMP	BYTE [DI+BDS.FormFactor],ffHardFile
		JZ	short DO_Fast ; HARD FILES USE FAST SPEED
;
; The registers are now all set up for call on rom-bios.
; The next dozen or so line determines whether we call Do_Fast or Do_Norm
; for the actual I/O read.  Do_Fast calls FastSpeed for the actual I/O.
; Do_Norm calls NormSpeed.  NormSpeed changes the value for the head settle
; time in the disk parameter table to a larger value and then calls FastSpeed
; to do the I/O.  So Do_Fast just has a shorter head settle time.
;
		CMP	byte [CS:STEP_DRV],-1
		;jz	short Do_Writej
		jz	short DO_WRITE ; 14/06/2018

		cmp	AH,ROMRead	; For read...
		je	short DO_Fast	;   ... alway use fast

		cmp	AH, ROMVerify	; For verify...
		je	short DO_Fast	;   ... alway use fast

Do_Writej:
		jmp	short DO_WRITE	; Jump down for write...

DO_Fast:
		CALL	FASTSPEED	; do I/O carry set if error
TestErr:
		jc	short DSKERR	; error -- get out
; SET DRIVE AND TRACK OF LAST ACCESS
		mov	[CS:STEP_DRV],DL ; save the last drive accessed
		mov	[DI+BDS.Track],CH ; save in BDS

NO_SET:
		cmp	word [CS:WRTVERIFY],103H ; Check for write and verify
		jz	short DOVERIFY	; yes -- go do verify
NOVERIFY:
		pop	AX		; pop command and num sec. from stack
		and	CL,03FH		; Eliminate cylinder bits from sector
		xor	AH,AH
		sub	[CS:SECCNT],AX	; Reduce count of sectors to go
		add	CL,AL		; Next sector
		mov	[CS:CURSEC],CL
					 ; See if sector/track limit reached
		cmp	CL,[DI+BDS.SECLIM]
		jbe	short Disk_Ret	; yes, return
NextTrack:
		mov	byte [CS:CURSEC],1 ; Start with first sector of next track
		mov	DH,[CS:CURHD]
		inc	DH		; go to next head
		cmp	DH,[DI+BDS.HDLIM] ; at head limit?
		jb	short NOXOR	; no, jump down
		xor	DH,DH		; at head limit, reset to head zero ...
		inc	word [CS:CURTRK] ; 	and go to next head
NOXOR:
		mov	[CS:CURHD],DH	; save new head number
Disk_Ret:
		clc			; successful return so clear error flag
		retn			; all done

;
; The request is for write.  Determine if we are talking about the same
; track and drive.  If so, use the fast speed.
;
DO_WRITE:
		cmp	DL,[CS:STEP_DRV] ; same drive?
		jnz	short DO_Norm 	 ; no, do normal speed
		cmp	CH,[DI+BDS.Track] ; same track on drive
		jz	short DO_Fast	; yes, do fast speed
DO_Norm:
		call	NORMSPEED	; use larger head settle time
		jmp	SHORT TestErr	; test for error

;
; we have a verify request also.  Get state info and go verify
;

DOVERIFY:
		pop	AX		; get number of sectors from stack
		push	AX		;    in non-detructive fashion
		MOV	AH,ROMVerify	; REQUEST VERIFY
		CALL	FASTSPEED	; MZ  2.21 change settle mode
		JNC	short NOVERIFY

;
; Need to special case the change-line error AH=06h.  If we get this, we
; need to return it.
;
	;----------------------------------------|
	; Warning: Do not change the following. ;|
	;	   It gets patched in msinit	;|
						;|
DSKERR:						;|
		CALL	CheckIO 		;|
	;---------------------------------------;|

		Call	AGAIN	; reset the disk and decrement retry cnt
		jz	short HARDERR ; if z flag set, did all retries-give up
		cmp	AH,80H		; timeout?
		jz	short HARDERR ; yes, jump to hard error
DSKERR1:
		pop	AX		; Restore sector count
		jmp	RETRY		;     and try again

HARDERR:
		CALL	MAPERROR

HARDERR2:			; for routines that call MapError themselves
		mov	byte [CS:TIM_DRV],-1 ;Force a media check through ROM
		mov	CX,[CS:SECCNT]	;Get count of sectors to go
		mov	SP,[CS:SPSAV]	;Recover entry stack pointer
;
; Since we are performing a non-local goto, restore the disk parameters
;
MedByt_OK:
		call	DiddleBack
		retn			;and return

;
; change settle value from SettleCurrent to whatever is appropriate
;
NORMSPEED:
		push	DS		; save two registers
		push	AX
				; change value in current disk parm tbl
		mov	AL,[CS:SettleSlow] 
		lds	SI,[CS:DPT]	; current disk parm table
		mov	[SI+DISK_PARMS.DISK_HEAD_STTL],AL
		pop	AX		; restore command and sector count
		pop	DS
		call	FASTSPEED	; do I/0
		push	DS	; restore the value in disk parm table
		lds	SI,[CS:DPT]
		mov	byte [SI+DISK_PARMS.DISK_HEAD_STTL],1 ; 1 is fast settle
		pop	DS
		retn

FASTSPEED:
;
; If the drive has been marked as too big (i.e. starting sector of the
; partition is > 16 bits, then ALWAYS return drive not ready.
;
		TEST	BYTE [DI+BDS.FatSiz],fTOOBIG
		JNZ	short NotReady

		int	13h		; call rom-bios disk routines
Death:
		retn
NotReady:
		stc			; set carry to indicate error
		mov	AH,80h		; put error code in AH
		jmp	short Death	; jump to ret

;
; Map error returned by ROM into corresponding code to be returned to
; DOS in AL.
;
MAPERROR:
		push	CX		; save cx
		push	CS
		pop	ES		; make ES the local segment
		mov	AL,AH		; move error code into AL
		mov	[CS:LSTERR],AL	; terminate list with error code
		mov	CX,NUMERR	; number of possible error conditions
		mov	DI,ERRIN	; point to error conditions
		repne	SCASB
		mov	AL,[CS:DI+NUMERR-1] ; get translation
		pop	cx		; restore cx
		stc			; flag error condition
		retn

;
; Set the time of last access for this drive. This is done only for removable
; media.
;
SET_TIM:
		push	ax
		xor	AH, AH		; set command to get time
		int	1Ah		; call rom-bios timer function
		or	AL,AL		; is there 24 hour rollover?
		jz	short NOROLL3 	; no, skip down
		inc	word [CS:DAYCNT] ; yes, then increment DayCnt
NOROLL3:
; We have the new time. If we see that the time has passed, then we reset
; the threshold counter...
		cmp	DX,[DI+BDS.TIM_LO] ; Did any time pass?
		jnz	short SetAccess	; yes, update access time
		cmp	CX,[DI+BDS.TIM_HI] ; now look at the high bits
		jz	short Done_Set	; if equal then no time passed
SetAccess:				; we get here if some time has passed
					; zero AccessCount to show time passage
		mov	byte [CS:AccessCount],0
		MOV	[DI+BDS.TIM_LO],DX ; save low time bits
		MOV	[DI+BDS.TIM_HI],CX ; save high time bit
Done_Set:
		clc			; indicate no error
		pop	ax		; restore AX register
		retn

; 28/05/2018 - Retro DOS v3.0

;
; This is the true DISK INT handler.  We parse the request to see if there is
; a DMA violation.  If so, depending on the function, we:
;   READ/WRITE	Break the request into three pieces and move the middle one
;	into our internal buffer.
;   FORMAT	Copy the format table into the buffer
;   VERIFY	Point the transfer address into the buffer
;
; This is the biggest bogosity of all.	The IBM controller does NOT handle
; operations that cross physical 64K boundaries.  In these cases, we copy
; the offending sector into the buffer below and do the I/O from there.
;

;;Rev 3.30 Modification -----------------------------------------
;To handle the INT 13h, AH = 8 Problem.
;Save Registers here.
Save_AX:   DW  0
Save_BX:   DW  0
Save_CX:   DW  0
Save_DX:   DW  0
Save_DI:   DW  0
Save_SI:   DW  0
Save_BP:   DW  0
Save_DS:   DW  0
Save_ES:   DW  0
Prev_DX:   DW  0
Save_Flag: DW  0
;;End of Modification -----------------------------------------

;
; Block13:
;
;   Entry conditions:
;	AH = function
;	AL = number of sectors
;	ES:BX = DMA address
;	CX = packed track and sector
;	DX = head and drive
;   Output conditions:
;	NO DMA violation.
;

Block13:
;
; Let the operation proceed.  If there is a DMA violation, then we do things.
;
		mov	 [cs:PrevOper],AX	; save request
		pushf				; preserve the flags
		cmp	AH,ROMFormat		; format request?
		jnz	short Not_Format	;    no, skip down

	; Set changed by format bit for all logical drives using this physical drive
		;---------------------------------------------------------|
		; Warning: Do Not Change the following. 		  |
		; It gets patched in at INIT time			  |
Changed_Patch:
		mov	word [cs:FlagBits],fChanged_By_Format+fChanged
		call	Set_Changed_DL	; Indicate that media changed by format
		;							  |
		;---------------------------------------------------------|

Not_Format:
;;Rev 3.30 Modification -----------------------------------------
		cmp	ah, 8			; Read Driver Parm ?
		je	short Bus_Problem
		cmp	ah, 15h
		je	short Bus_Problem
		CALL	far [CS:ORIG13]		; SIMULATE INT 13
		JC	short GOTERR13_br	; ERROR?
		;14/06/2018
		RETF	2			; NO, RETURN AND CLEAR FLAGS

GOTERR13_br: 
		jmp	GotErr13

;Some machines have a problem with Int 13h function=8
;This function does not reset the common buses after the execution.
;To solve this problem, when we detect AH=8h, then we will save the result
;and will issue AH=1 (Read Status) call to reset the buses.

Bus_Problem:
		mov	[cs:Prev_DX], DX	;save orignal drive number
		call	far [CS:ORIG13]		;Do "Read drive parm"

		mov	[cs:Save_AX], AX	;Save registers,flag
		mov	[cs:Save_BX], BX
		mov	[cs:Save_CX], CX
		mov	[cs:Save_DX], DX
		mov	[cs:Save_DI], DI
		mov	[cs:Save_SI], SI
		mov	[cs:Save_BP], BP
		mov	[cs:Save_DS], DS
		mov	[cs:Save_ES], ES
		pushf
		pop	word [cs:Save_Flag]

		mov	dx, [cs:Prev_DX]	;restore orignal drive
		pushf
		mov	ah, 1			;Read Status.
		call	far [CS:ORIG13]		;Reset the bus as a side effect

		mov	AX, [cs:Save_AX]	;restore registers,flag
		mov	BX, [cs:Save_BX]
		mov	CX, [cs:Save_CX]
		mov	DX, [cs:Save_DX]
		mov	DI, [cs:Save_DI]
		mov	SI, [cs:Save_SI]
		mov	BP, [cs:Save_BP]
		mov	DS, [cs:Save_DS]
		mov	ES, [cs:Save_ES]
		push	word [cs:Save_Flag]
		popf
		jc	short GotErr13		;AH=8 had been an error?
		;14/06/2018
		retf	2
;
; Some kind of error occurred.	See if it is DMA violation
;
GotErr13:
		pushf
		cmp	AH, 09h			; is error DMA error code?
		JNZ	short CHECK_ECC
		JMP	short GotDMAErr
CHECK_ECC:
		CMP	AH,11H
		JZ	short OK11
		POPF
		;14/06/2018
		RETF	2
;
; We have an error status 11h.	This indicates an ECC-corrected error.	Note
; that this indicates that the data is PROBABLY correct but not CERTAINLY
; correct. The ROMs on PC-1s and PC_XTs have a 'bug' in that if an ECC error
; occurs for a multi-sector read, only the sectors up to the one where the
; error occurred are read in. We have no way of knowing how many were read in
; this case, so we redo the operation, reading one sector at a time. If we
; get an ECC error on reading one sector, we ignore the error because the
; sector has been read in.
;

OK11:
;		popf			; restore flags
;;Rev 3.30 Modification -----------------------------------------
; Here, it is better reset the system. So, we are going to
; call Orig13 again
		mov	ah,0
		call	far [CS:ORIG13]	;reset. Don't care about result
;;End of Modification -----------------------------------------

		mov	ax,[cs:PrevOper] ; Retrieve request
;
; This will provide a termination point.
;
		cmp	AL,1		; If request for one sector, assume OK
		jnz	short ECC_Err_Handle ; more than one sector -- jump down
		xor	AH,AH		; clear carry too!
		;14/06/2018
		retf	2

ECC_Err_Handle:
		push	bx
		push	cx
		push	dx
		mov	[cs:Number_Of_Sec],AL
Loop_ECC:
		mov	AX,[CS:PrevOper] ; set command to previos command
		mov	AL,1		 ;     but request only one sector
;
; we do reads one sector at a time. this ensures that we will eventually
; finish the request since ecc errors on 1 sector do read in that sector.
;
; we need  some "intelligence" in the ecc handler to handle reads
; that attempt to read more sectors than are available on a particular
; track.
; we call check_wrap to set up the sector #, head # and cylinder # for
; this request.
; at this point, all registers are set up for the call to orig13, except
; that there maybe a starting sector number that is bigger than the number
; of sectors on a track.
;
		CALL	Check_Wrap	; see if wrapping around cylinder

		pushf			; save flags
		call	far [CS:ORIG13]	; call original rom-bios code
;;Rev 3.30 Modification ------------------------------------------------------
		JNC	short OK11_Op
		CMP	AH,11H		; ONLY ALLOW ECC ERRORS
		JNZ	short OK11_EXIT_err ; Other error?
		mov	ah,0		; ECC error. Reset it again.
		pushf
		call	far [CS:ORIG13]
OK11_Op:
					; adjust number of sectors for one read
		dec	byte [CS:Number_Of_Sec]	
		jz	short OK11_Exit	; all done?
		inc	CL		; advance sector number
		inc	BH		; add 200H to address
		inc	BH
		jmp	short Loop_ECC	; and around for reading another sector

OK11_EXIT_err:
		stc			; Set carry bit again.
;;End of Modification ------------------------------------------------------

OK11_Exit:
		pop	dx
		pop	cx
		pop	bx
		;14/06/2018
		retf	2

;
; we truly have a DMA violation.  Restore register AX and retry the
; operation as best we can.
;
GotDMAErr:
		pop	AX		; clean up stack
		mov	AX,[cs:PrevOper] ; restore command
		sti			; restore interrupts
		cmp	AH,ROMRead	; determine the command
		jb	short IntDone
		cmp	AH,ROMVerify
		jz	short IntVerify
		cmp	AH,ROMFormat
		jz	short IntFormat
		ja	short IntDone
;
; We are doing a read/write call.  Check for DMA problems
;
		push	dx		; save registers we overwrite
		push	cx
		push	bx
		push	ax

		push	BP
		mov	BP,SP
		mov	DX,ES		; Check for 64k boundary error

		shl	DX,1
		shl	DX,1
		shl	DX,1
		shl	DX,1		; Segment converted to absolute address

		add	DX,BX		; Combine with offset
		add	DX,511		; simulate a transfer
;
; If carry is set, then we are within 512 bytes of the end of the segment.
; We skip the first transfer and perform the remaining buffering and transfer
;
		JNC	short NO_SKIP_FIRST
		mov	DH,[bp+INT13FRAME.olddx+1] ; set correct head number
		jmp	Buffer

NO_SKIP_FIRST:
;
; DX is the physical 16 bits of start of transfer.  Compute remaining
; sectors in segment.
;
		shr	DH,1		; DH = number of sectors before address
		mov	AH,128		; AH = max number of sectors in segment
		sub	AH,DH
;
; AH is now the number of sectors that we can successfully write in this
; segment.  If this number is above or equal to the requested number, then we
; continue the operation as normal.  Otherwise, we break it into pieces.
;
		cmp	AH,AL		; can we fit it in?
		jb	short DoBlock	; no, perform blocking.
;
; Yes, the request fits.  Let it happen
;
		MOV	DH,[BP+INT13FRAME.olddx+1] ; SET UP HEAD NUMBER
		call	DoINT
		jmp	Bad13
;
; Verify the given sectors.  Place the buffer pointer into our space.
;
IntVerify:
		push	es
		push	bx
		push	CS
		pop	ES
DoSimple:
		mov	BX,DiskSector
		pushf
		call	far [CS:ORIG13]
		pop	bx
		pop	es
		;14/06/2018
		retf	2

;
; Format operation.  Copy the parameter table into memory
;
IntFormat:
		push	es
		push	bx
		push	si
		push	di
		push	ds
		push	ES
		push	CS
		pop	ES
		pop	DS
		mov	SI,BX
		mov	DI,DiskSector
		call	Move
		pop	ds
		pop	di
		pop	si
		jmp	short DoSimple
;
; Inline continuation of operation
;
IntDone:
		jmp	far [CS:ORIG13]
;
; We can't fit the request into the entire block.  Perform the operation on
; the first block.
;
;
; DoBlock is modified to correctly handle multi-sector disk I/O. 
; Old DoBlock had added the number of sectors I/Oed (AH in Old DoBlock) after
; the DoInt call to CL.  Observing only the lower 6 bits of CL(=max. 64) can
; represent a starting sector, if AH was big, then CL would be clobbered.
; By the way, we still are going to use CL for this purpose since Checkwrap
; routine will use it as an input.  To prevent CL from being clobbered, a 
; safe number of sectors should be calculated like "63 - # of sectors/track".
; DoBlock will handle the first block of requested sectors within the
; boundary of this safe value.

;Try to get the # of sectors/track from BDS via Rom drive number.
;For any mini disks installed, here we have to pray that they have the
;same # of sector/track as the main DOS partition disk drive.

DoBlock:
;;Rev 3.30 Modification ------------------------------------------------------
		;mov	dx,[bp+INT13FRAME] 	;set head #
		mov	dx,[bp+INT13FRAME.olddx] ;set head # ; 17/07/2019
		push	di					
		push	ds					
		push	ax		;AH=# of sectors before DMA err
					;AL - User requested # of sectors
		mov	byte [CS:PHYS_DRV],1				
		mov	al, dl							
		call	SETDRIVE	;get BDS pointer for this DISK. 
		pop	ax							
		mov	byte [CS:PHYS_DRV],0				
		test	word [DI+BDS.Flags],fNon_Removable ;don't have to worry
		jnz	short DoBlockHard ;about floppies. They are track by
					;track operations	
		mov	al, ah		;set al = ah for floppies
		jmp	short DoBlockCont			
DoBlockHard:					
		push	cx				
		xor	cx, cx				
		mov	cx, [DI+BDS.SECLIM] ;# of sectors/track
		mov	ch, 63						
		sub	ch, cl						
		mov	al, ch						
		xchg	ah, al		;now ah - safe # of sectors
					;al - # of sectors before DMA err
		pop	cx							
DoBlockCont:								
		pop	ds							
		pop	di							
DoBlockContinue:							
		cmp	ah, al		;if safe_# >= #_of_sectors_to_go_before DMA,
		jae	short DoBlocklast ;then #_of_sectors_to_go as it is for DoInt.
		push	ax		;save AH, AL				
		mov	al, ah		;Otherwise, set al to ah to operate.
			;DoInt will set AH to a proper function in [BP.Oldax]	
		jmp	short DoBlockDoInt 
DoBlocklast:						
		mov	ah, al					
		push	ax		;save AH	
DoBlockDoInt:				;let AH=AL=# of sectors this shot
		CALL	DoINT							 
		JC	short Bad13	;something happened, bye!	 
		pop	ax
					;decrement by the successful operation							 
		SUB	BYTE [BP+INT13FRAME.oldax], AH 	
		ADD	CL,AH		;advance sector number. Safety guaranteed.	
		ADD	BH,AH		;advance DMA address			
		ADD	BH,AH		;twice for 512 byte sectors.	
		cmp	ah, al		;check the previous value	
		je	short Buffer	;if #_of_sectors_to_go < safe_#, 
					; then we are done already. 
		sub	al, ah		;otherwise, 
					; #_sector_to_go = #_of_sector_to_go - safe_#  
		call	Check_Wrap	;get new CX, DH for the next operation. 		  
		jmp	short DoBlockContinue ;handles next sectors left.			
;;End of Modification ------------------------------------------------------
;
Buffer:
		push	BX
		mov	AH,[BP+INT13FRAME.oldax+1]
		cmp	AH,ROMWrite
		jnz	short DoRead
;
; Copy the offending sector into local buffer
;
		push	ds
		push	es
		push	si
		push	di
		push	CS		; exchange segment registers
		push	ES
		pop	DS
		pop	ES
		mov	DI,DiskSector	; where to move
		push	DI		; save it
		mov	SI,BX		; source
		call	Move
		pop	BX		; new transfer address
		pop	di
		pop	si
		mov	AL,1
		mov	DL,[BP+INT13FRAME.olddx] ; set drive number
		call	Check_Wrap	; check for head or cylinder wrap
;
;   AH is function
;   AL is 1 for single sector transfer
;   ES:BX is local transfer addres
;   CX is track/sector number
;   DX is head/drive number
;   SI,DI unchanged
;
		CALL	DoINT
		pop	es
		pop	ds
		jc	short Bad13	; go clean up
		jmp	SHORT DoTail
;
; Reading a sector.  Do INT first, then move things around
;
DoRead:
		push	es
		push	bx
		push	CS
		pop	ES
		mov	BX,DiskSector
		mov	AL,1
		mov	DL,[BP+INT13FRAME.olddx] ; set drive number
		call	Check_Wrap	; check for head or cylinder wrap
;
;   AH = function
;   AL = 1 for single sector
;   ES:BX points to local buffer
;   CX, DX are track/sector, head/drive
;
		CALL	DoINT
		pop	bx
		pop	es
		jc	short Bad13	; error => clean up
		push	ds
		push	si
		push	di
		push	CS
		pop	DS
		mov	DI,BX
		mov	SI,DiskSector
		call	Move
		pop	di
		pop	si
		pop	ds
;
; Note the fact that we've done 1 more sector
;
DoTail:
		pop	BX		; retrieve new DMA area
		add	BH,2		; advance over sector
		inc	CX
		mov	AL,[BP+INT13FRAME.oldax]
		clc
		dec	AL
		jz	short Bad13	; no more I/O
		mov	DL,[BP+INT13FRAME.olddx] ; set drive number
		call	Check_Wrap	; check for head or cylinder wrap
		call	DoINT
;
; We are done.	AX has the final code; we throw away what we got before
;
Bad13:
		mov	SP,BP
		pop	bp
		pop	bx
		pop	bx
		pop	cx
		pop	dx
		; 06/07/2018
		retf	2

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
DO_FAST_FORMAT		EQU	000000010B ;AN001;
; CODES RETURNED FROM FORMAT STATUS CALL
FORMAT_NO_ROM_SUPPORT	EQU	000000001B
FORMAT_COMB_NOT_SUPPORTED EQU	000000010B

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

; 28/05/2018 - Retro DOS v3.0

;
; Generic IOCTL dispatch tables
;
IOReadJumpTable: 
		db	2
		dw	GetDeviceParameters
		dw	ReadTrack
		dw	VerifyTrack

IOWriteJumpTable:
		db	2
		dw	SetDeviceParameters
		dw	WriteTrack
		dw	FormatTrack

MAX_SECTORS_CURR_SUP	EQU	63	; CURRENT MAXIMUM SEC/TRK THAT	  ;3.30
					; WE SUPPORT (Was 40 in DOS 3.2)  ;3.30
;
; TrackTable is an area for saving information passwd by the set device
; parameter function for laster use my Read/Write/Format/Verify.
;
; Entries are 4-Tuples (C,H,R,N) where:
;   C = Cylinder, H = Head, R = Sector, N = Bytes/Sector
;
; fixed for bug0016 - initialised table with values - sp
TrackTable:
		db	0,0,1,2
		db	0,0,2,2
		db	0,0,3,2
		db	0,0,4,2
		db	0,0,5,2
		db	0,0,6,2
		db	0,0,7,2
		db	0,0,8,2
		db	0,0,9,2
		db	0,0,10,2
		db	0,0,11,2
		db	0,0,12,2
		db	0,0,13,2
		db	0,0,14,2
		db	0,0,15,2
		db	0,0,16,2
		db	0,0,17,2
		db	0,0,18,2
                times (MAX_SECTORS_CURR_SUP*A_SECTORTABLE.size)-($-TrackTable) db 0

SectorsPerTrack:
		dw      15

; This is a real ugly place to put this
; it should really go in the BDS
mediaType:	db	0

Media_Set_For_Format:
		db	0		; 1 if we have done an Int 13 Set Media
					; Type for Format call
; Rev 3.30 *****************************************************************
Had_Format_Error:
		db	0		; 1 if the previous format operation
					; failed.
Dsk_time_out_Err	equ	80h	; Time out error (No media present).
Dsk_change_line_Err	equ	6h	; Change line error
Dsk_illegal_combination equ	0Ch	; Return code of ah=18h function.
; Rev 3.30 *****************************************************************

;
; TempDPT is a temporary place to hold a pointer to the original
; Disk Parameter Table while DPT is made to point to a table returned
; by a BIOS call.  A value of -1 indicateds no value has been saved.
;

TempDPT:
		DD	-1

;
; Generic$IOCTL:
;    Perform Generic IOCTL request
;    Input:
;	al	- unit number
;    Output:
;	if carry set then al contains error code
;

GENERIC$IOCTL:
		les	bx,[cs:PTRSAV]		; es:bx points to request header.
		call	SETDRIVE		; ds:di points to BDS for drive.
;
; At this point:
;    es:bx - points to the Request Header
;    ds:di points to the BDS for the drive
;
		cmp	byte [es:bx+IOCTL_REQ.MAJORFUNCTION],RAWIO
		jne	IOCTL_Func_Err
		mov	al,[es:bx+IOCTL_REQ.MINORFUNCTION]
		mov	si,IOReadJumpTable
		test	al,GEN_IOCTL_FN_TST	; Test of req. function
		jnz	short NotGenericIoctlWrite	;   function is a Read.
		mov	si,IOWriteJumpTable
NotGenericIoctlWrite:
		and	al,0FH
		cmp	al,[cs:si]
		ja	short IOCTL_Func_Err
		cbw
		shl	ax, 1
		inc	si
		add	si,ax
		les	bx,[es:bx+IOCTL_REQ.GENERICIOCTL_PACKET]
		call	[cs:si]
		jc	short FailGeneric$IOCTL
		jmp	EXIT

FailGeneric$IOCTL:
		jmp	ERR$EXIT

IOCTL_Func_Err:
		jmp	CMDERR

;
; GetDeviceParameters:
;
; Input: DS:DI points to BDS for drive
;	 ES:BX points to device parameter packet
;

GetDeviceParameters:
; Copy info from BDS to the device parameters packet
		mov	al,[DI+BDS.FormFactor]
		mov	[es:bx+A_DEVICEPARAMETERS.DP_DEVICETYPE], al
		mov	ax,[DI+BDS.Flags]
		and	ax,fNon_Removable+fChangeline	; mask off other bits
		mov	[es:bx+A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES], ax
		mov	ax,[DI+BDS.cCyln]
		mov	[es:bx+A_DEVICEPARAMETERS.DP_CYLINDERS], ax

; Set media type to default
		xor	al, al
		mov	[es:bx+A_DEVICEPARAMETERS.DP_MEDIATYPE],al

; Copy recommended BPB
		lea	si, [DI+BDS.RBytePerSec]
		;test	byte [es:bx+A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS],BUILD_DEVICE_BPB
		test	byte [es:bx],BUILD_DEVICE_BPB ; 05/07/2019
		jz	short use_BPB_present
; Get the correct disk in the drive
		call	CHECKSINGLE
; Build the BPB from scratch
		call	GETBP
		jc	short Get_Parm_Ret
		lea	si,[DI+BDS.BytePerSec]
use_BPB_present:
		lea	di,[bx+A_DEVICEPARAMETERS.DP_BPB]
		mov	cx,BPB_TYPE.size	; for now use 'small' BPB
		rep	movsb
		clc
Get_Parm_Ret:
		retn

;
; SetDeviceParameters:
;
; Input: DS:DI points to BDS for drive
;	 ES:BX points to device parameter packet
;

SetDeviceParameters:

; Make sure the fChanged_By_Format flag gets set to kick DOS into looking at
; the BPB
		or	word [DI+BDS.Flags],fChanged_By_Format | fChanged
		;test	byte [es:bx+A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS],ONLY_SET_TRACKLAYOUT
		test	byte [es:bx],ONLY_SET_TRACKLAYOUT ; 05/07/2019
		;jz	short SetDevParm_1
		;jmp	short SetTrackTable		; Originally TrackLayout
		jnz	short SetTrackTable ; 05/07/2019 
SetDevParm_1:
; Copy info from the device parameters packet to BDS
		mov	al,[es:bx+A_DEVICEPARAMETERS.DP_DEVICETYPE]
		mov	[DI+BDS.FormFactor], al

		mov	ax,[es:bx+A_DEVICEPARAMETERS.DP_CYLINDERS]
		mov	[DI+BDS.cCyln],ax

; If change line is not loaded then ignore changeling flag
		mov	ax,[es:bx+A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES]
		cmp	byte [cs:fHave96],0
		jnz	short Have_Change
		and	ax,~fChangeline
Have_Change:
; ignore all bits except Non_removable and Changeline
		and	ax,fNon_Removable | fChangeline
		mov	cx,[DI+BDS.Flags]
		and	cx,~(fNon_Removable | fChangeline | GOOD_TRACKLAYOUT)
		or	ax,cx
		mov	[DI+BDS.Flags],ax

; Set media type
		mov	al,[es:bx+A_DEVICEPARAMETERS.DP_MEDIATYPE]
		mov	[cs:mediaType],al
; the media changed (maybe) so we will have to do a SetDASD the next time
; we format a track
		or	word [DI+BDS.Flags],SET_DASD_true

		push	ds
		push	di
		push	es
		push	bx
; Figure out what we are supposed to do with the BPB

; Were we asked to install a fake BPB?
		;test	byte [es:bx+A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS],INSTALL_FAKE_BPB
		test	byte [es:bx],INSTALL_FAKE_BPB ; 05/07/2019
		jnz	short InstallFakeBPB

; Were we returning a fake BPB when asked to build a BPB?
		test	word [DI+BDS.Flags],RETURN_FAKE_BPB
		jz	short InstallRecommendedBPB

; We were returning a fake BPB but we can stop now
		and	word[DI+BDS.Flags],~RETURN_FAKE_BPB
		jmp	short DoneWithBPBstuff

InstallRecommendedBPB:
		mov	cx,A_BPB.size
		lea	di,[DI+BDS.RBytePerSec]
		jmp	short CopyTheBPB

InstallFakeBPB:
		mov	cx,BPB_TYPE.size    ; move 'smaller' BPB
		lea	di,[DI+BDS.BytePerSec]
CopyTheBPB:
		lea	si,[bx+A_DEVICEPARAMETERS.DP_BPB]
; exchange es and ds
		push	es
		push	ds
		pop	es
		pop	ds

		rep	movsb

DoneWithBPBstuff:
		call	RestoreOldDPT
		pop	bx
		pop	es
		pop	di
		pop	ds

; Set up track table (if neccessary)
SetTrackTable:
		mov	cx,[es:bx+A_DEVICEPARAMETERS.DP_TRACKTABLEENTRIES]
		mov	[cs:SectorsPerTrack],cx
		and	word [DI+BDS.Flags],~GOOD_TRACKLAYOUT
		;test	byte [es:bx+A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS],TRACKLAYOUT_IS_GOOD
		test	byte [es:bx],TRACKLAYOUT_IS_GOOD ; 05/07/2019
		jz	short UglyTrackLayout
		or	word [DI+BDS.Flags],GOOD_TRACKLAYOUT

UglyTrackLayout:
		cmp	cx,MAX_SECTORS_IN_TRACK
		ja	short TooManySectorsPerTrack
		jcxz	SectorInfoSaved		; if no value don't copy table
						; save information in the track table

		push    BX			; get ES:BX to point to sector
		add     BX,A_DEVICEPARAMETERS.DP_SECTORTABLE ;  table in Device param. struct
	
		push    DI
		mov     DI,TrackTable + 2	; CS:DI now points to sector id
                                                ; of the first track table entry
		push    AX			; preserve AX value

                                                ; For MAX_SECTORS_IN_TRACK
TrackLoop:                                      ;   DO:
		mov     AX,[ES:BX]		;   get sector number
		mov	[CS:DI],AL		;   save in track table

	        mov     AX,[ES:BX+2]		;   get sector size
	        call    SectorSizeToSectorIndex ;   convert size to index number
		mov     [CS:DI+1],AL		;   save size in track table

		add     BX,A_SECTORTABLE.size	;   advance pointers to next
		add     DI,A_SECTORTABLE.size	;   entries
		loopnz  TrackLoop		; End FOR

		pop     AX 			; restore the saved values
		pop     DI
		pop     BX

SectorInfoSaved:
		clc
		retn

TooManySectorsPerTrack:
		mov	al, 0Ch
		stc
		retn

;
; FormatTrack:
; If SpecialFunction byte is 1, then this is a status call to see if there is
; ROM support for the combination of sec/trk and # of cyln, and if the
; combination is legal. If SpecialFunction byte is 0, then format the track.
;
; Input: DS:DI points to BDS for drive
;        ES:BX points to format packet
;
; Output:
;       For status call:
;       SpecialFunction byte set to:
;               0 - ROM support + legal combination
;               1 - No ROM support
;               2 - Illegal Combination
;		3 - no media present			;Rev 3.30
;       Carry cleared.
;
;       For format track:
;               Carry set if error
;
;
;  Flags also may be altered. All other registers preserved.
;  If the call to ROM returns no error, then the current DPT is "replaced" by
;  the one returned by the ROM. This is done by changing the pointer in [DPT]
;  to the one returned. The original pointer to the disk base table is stored
;  in TempDPT, until it is restored.
;
;  This proc was changed to force a status for format call if we are on the
;  new ROM.
;
;
FormatTrack:
		;test	byte [es:bx+A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS],STATUS_FOR_FORMAT
		test	byte [es:bx],STATUS_FOR_FORMAT ; 05/07/2019
		jz	short SkipStatusOnly

Do_Status_Only:
		call	FormatStatus
		;mov	[es:bx+A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS],al
		mov	[es:bx],al ; 05/07/2019
		;clc
		clc	; 05/07/2019 (CF=1 is possible from 'FormatStatus')
		retn
	
SkipStatusOnly:                       ; for a hard disk only do the verify
        	cmp     byte [DI+BDS.FormFactor],DEV_HARDDISK
		jnz	short SkipVerify
		jmp	DoVerifyTrack
SkipVerify:
		push	ds			; Format a Track
		push	di
		push	es
		push	bx
		call	FormatStatus		; SetDASD checks media_set_for_format
		;;14/06/2018 (IBMDOS 3.3,IBMBIO.COM)
		;cmp	al,1
		;je	short NeedToSetDasd
		cmp	al,3			; Check for time out
		;jne	short NoSetDasd
		je	short Format_Failed	; Fail if time out
		;jmp	short Format_Failed
NeedToSetDasd:
        	call    SetDASD
;
; Store Cylinder,Head in track table
;                       ***** ASSUMPTION *******
;       Since format requests on Fixed Media are converted to Verifies, we
;       assume that we are formatting a floppy and hence have 255 or less
;       tracks and heads. We therefore must change the Cylinder, Head data
;       from the Request Packet Size to that of the TrackTable (see Int 13
;       interface in IBM's Tech Ref.).

; Check to ensure correct disk is in drive
NoSetDasd:
		call    CHECKSINGLE

		mov     ax,[es:bx+A_FORMATPACKET.FP_CYLINDER]
		mov     [cs:TRKNUM],ax
		mov     cx,[es:bx+A_FORMATPACKET.FP_HEAD]
		mov     [cs:HDNUM],cl
		mov     ah,cl
                        ; this next piece of code copies the correct head
                        ; and cylinder numbers to the tracktable
		push    di			; preserve DI
		mov     di,TrackTable
		mov     CX,[cs:SectorsPerTrack] ; get number of sectors
        	jcxz    EndSetUpTrackTable	; if nothing to do skip down
SetUpLoop:
		mov     [cs:di],AX		; set head and track value
		add     di,4			; move to next entry
		loopnz  SetUpLoop		; loop if not done yet
EndSetUpTrackTable:
        	pop     di			; restore DI (BDS pointer)
        	mov     cx,MAXERR		; Set up retry count
FormatRetry:
		push    cx
                                		; set up registers for format call to TO_ROM
		mov     AX,[CS:SectorsPerTrack]	; set number of sectors
		mov     AH,ROMFormat
		push    cs			; set ES:BX to point to
		pop     es			;    the track table
		mov     BX,TrackTable
                                                ; don't need to set CL on format
		call    To_ROM
		pop	cx
		jnc	short FormatOk
		;pop     cx
		mov	byte [cs:Had_Format_Error],1	; Mark the error
		push	ax		
		push	cx			
		push	dx
		call    ResetDisk
		call	FormatStatus
		cmp	al,1
		jnz	short While_Err
		call	SetDASD 
While_Err:
		pop	dx
		pop	cx
		pop	ax
		loop    FormatRetry

; Format failed
Format_Failed:
		mov	byte [cs:Had_Format_Error],1	; Indicate a format error
		cmp	ah,Dsk_change_line_Err		; Convert change line to
		jne	short Map_Err			;   to time out.
		mov	ah,Dsk_time_out_Err
Map_Err:
        	call    MAPERROR
		pop	bx
		pop	es
		pop	di
		pop	ds
		retn

FormatOk:
		mov	byte [cs:Had_Format_Error],0 ; Reset format error flag
		;pop     cx			; clean up stack after bailing out
						; of FormatRetry loop early
		pop	bx
		pop	es
		pop	di
		pop	ds
DoVerifyTrack:
	        call    VerifyTrack             ; Will reset DPT entries.
		retn

;
; FormatStatus:
; If SpecialFunction byte is 1, then this routine is called to see if there is
; ROM support for the combination of sec/trk and # of cyln, and if the
; combination is legal.
;
; Input: DS:DI points to BDS for drive
;	 ES:BX points to format packet
;
; Output:
;	SpecialFunction byte set to:
;		0 - ROM support + legal combination
;		1 - No ROM support
;		2 - Illegal Combination
;		3 - No media present, ROM support exists but can't determine
;		    media
;	Carry cleared.
;
;	For format track:
;		Carry set if error
;
;
;  Flags also may be altered. All other registers preserved.
;  If the call to ROM returns no error, then the current DPT is "replaced" by
;  the one returned by the ROM. This is done by changing the pointer in [DPT]
;  to the one returned. The original pointer to the disk base table is stored
;  in TempDPT, until it is restored.
;
;
FormatStatus:
		push	cx
		push	dx
			; Are we here because of a format err
		cmp	byte [cs:Had_Format_Error],1
		je	short Fstat01
		cmp	byte [cs:Media_Set_For_Format],1
		jnz	short FStat03
		jmp	Stat_Ret
FStat03:
		mov	byte [cs:Media_Set_For_Format],0
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			modification - sp001
;	
;	remove check for new rom from here. we shall just assume the
;	prescence of the new rom and go ahead and issue the int13 call
;	anyway. later on if there is an error we shall check this to
;	see if it is there because of lack of rom support, in which
;	case the appropriate error will be indicated by setting al to 1
;
;	I would ideally like to see the new rom testing code shifted to
;	msinit and this code reintroduced. however for this version we
;	are aiming to stick close to the IBM variety. 
;
;	More changes to support this commenting out will follow. All
;	will be marked as modification sp001
;
;	mov	al,1			; No ROM support available error code
;	test	byte ptr cs:[NEW_ROM],1
;	jnz	short FStat01
;	jmp	Stat_Ret
Fstat01:
		push	ds
		push	si

		xor	ax,ax
		mov	ds,ax
		lds	si,[DSKADR]		; DS:SI := pDPT

		mov	word [cs:DPT],si	; cs:[DPT] := pDPT
		mov	word [cs:DPT+2],ds

		pop	si
		pop	ds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			modification sp001
;	
;	the following instruction introduced for the new rom modification
;
		mov	byte [cs:NEW_ROM],1		; assume new rom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		mov	ax,[DI+BDS.cCyln]
		mov	cx,[DI+BDS.SECLIM]
					; set up registers for format status call
		and	AH,03h		; 'and' out unneeded track bits
		ror	AH,1		; get track and sector values correct
		ror	AH,1
		or	AH,CL		; set sector number
		xchg	AH,AL
		mov	CX,AX
		dec	CH
		mov	DL,[DI+BDS.DriveNum] ; get drive number
		mov	AH,18h	; set command to "sec/trk supported?"

		push	es
		push	di
		push	ds
		push	si

		int	13h		; call rom bios to see if supported
		jc	short Format_Stat_Err ; if carry, combination is not supported

				; ES:DI points to new Disk Base Table
				; combination for this drive replace
				; current (DskAdr) pointer with new one,
				; saving the old one in TempDPT.

		; Are we here because of a format err
		cmp	byte [cs:Had_Format_Error],1
		jnz	short Fstat02	; Then skip the disk base setup
	
		xor	al,al			; Supported and OK
		mov	[cs:Had_Format_Error],al ; Clear format error
		jmp	short Pop_Stat_Ret	; Back to work
Fstat02:
		xor	ax,ax
		mov	ds,ax
		lds	si,[DSKADR]	; DS:SI := pDPT

		mov	[cs:TempDPT],si
		mov	[cs:TempDPT+2],ds ; Save pDPT

		mov	[DSKADR],DI 	; Setup New DPT returned by
		mov	[DSKADR+2],ES	; ROM

		mov	byte [cs:Media_Set_For_Format],1 ; set flag
		xor	al,al		; Legal combination + ROM support code
		jmp	short Pop_Stat_Ret

Format_Stat_Err:
		mov	al,3			; Assume a time out
		cmp	ah,Dsk_time_out_Err	; Was it a time out???
		jz	short Pop_Stat_Ret	;  Yes - then done
		dec	al			; Assume an illegal comb.
		cmp	ah,Dsk_illegal_combination ; Was it an illegal comb???
		jz	short Pop_Stat_Ret	; Yes - then done
		dec	al			; Assume No ROM Support	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			modification sp001
;
;	the following instruction was introduced for the new_rom modification
;
		mov	byte [cs:NEW_ROM],0	; the old rom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Return result of status call
Pop_Stat_Ret:
		pop	si
		pop	ds
		pop	di
		pop	es
Stat_Ret:
		clc
		pop	dx
		pop	cx
		retn

;
; VerifyTrack:
;
; Input: DS:DI points to BDS for drive
;	 ES:BX points to verify packet
;

VerifyTrack:
		mov	byte [cs:RFLAG],ROMVerify
		mov	ax,[es:bx+A_VERIFYPACKET.VP_CYLINDER]
		mov	[cs:CURTRK], ax
		mov	ax,[es:bx+A_VERIFYPACKET.VP_HEAD]

;	****** ASSUMPTION ******
;	we assume that we have less than 256 heads, and that the Request
;	Header Data Structure is unneccessarily big
		mov	[cs:CURHD],al
		xor	ax,ax
		mov	cx,[cs:SectorsPerTrack]
; Use 0:0 as the transfer address for verify
		xor	bx, bx
		mov	es, bx
		call	TrackIO
		retn

;
; ReadTrack:
;
; Input: DS:DI points to BDS for drive
;	 ES:BX points to read packet
;

ReadTrack:
		mov	byte [cs:RFLAG],ROMRead
		jmp	short ReadWriteTrack

;
; WriteTrack:
;
; Input: DS:DI points to BDS for drive
;	 ES:BX points to write packet
;

WriteTrack:
		mov	byte [cs:RFLAG],ROMWrite
		jmp	short ReadWriteTrack
;
; ReadWriteTrack:
;
; Input:
;    DS:DI points to BDS for drive
;    ES:BX points to write packet
;    RFLAG - 2 for read, 3 for write
;

ReadWriteTrack:
		mov	ax,[es:bx+A_TRACKREADWRITEPACKET.TRWP_CYLINDER]
		mov	[cs:CURTRK],ax
		mov	ax,[es:bx+A_TRACKREADWRITEPACKET.TRWP_HEAD]

;	****** ASSUMPTION ******
;	we assume that we have less than 256 heads, and that the Request
;	Header Data Structure is unneccessarily big
		mov	[cs:CURHD],al
		mov	ax,[es:bx+A_TRACKREADWRITEPACKET.TRWP_FIRSTSECTOR]
		mov	cx,[es:bx+A_TRACKREADWRITEPACKET.TRWP_SECTORSTOREADWRITE]
		les	bx,[es:bx+A_TRACKREADWRITEPACKET.TRWP_TRANSFERADDRESS]
		call	TrackIO
		retn
;
; TrackIO:
;    Performs Track Read/Write/Verify
;
;   Input:
;      RFLAG	- 2 = Read
;		  3 = Write
;		  4 = Verify
;      ax	- Index into track table of first sector to IO
;      cx	- number of sectors to IO
;      es:bx	- Transfer address
;      ds:di	- pointer to BDS
;      curtrk	- current cylinder
;      curhd	- current head
;

TrackIO:
; procedure `disk' will pop stack to SPsav and return if error
		mov	[cs:SPSAV], sp
; Ensure correct disk is in drive
		call	CHECKSINGLE
;
; Set up tables and variables for I/O
;
		cmp	byte [cs:Media_Set_For_Format],1
		jz	short DPTAlreadySet
;
; SET UP TABLES AND VARIABLES FOR I/O
;
		push	ax
		push	cx
		call	IOSETUP
		pop	cx
		pop	ax
;
; point si at the table entry of the first sector to be IO'd
;
DPTAlreadySet:
		mov	si,TrackTable
		shl	ax,1
		shl	ax,1
		add	si,ax
;
; we want:
;    cx to be the number of times we have to loop
;    dx to be the number of sectors we read on each iteration
		mov	dx,1
		test	word [DI+BDS.Flags],GOOD_TRACKLAYOUT
		jz	short IOnextSector

; Hey! we can read all the sectors in one blow
		xchg	dx, cx

IOnextSector:
		push	cx
		push	dx
; skip over the cylinder and head in the track table
		inc	si
		inc	si

; Get sector id from track table
		mov     AL,[cs:si]	; get current sector value
		mov     [cs:CURSEC], AL	; save cursec value
        
;*** For a Fixed disk multi-track disk I/O -  4/14/86
;Assumptions: 1). In the input CX (# of sectors to go) to TRACKIO, only CL is
;valid.  2). Sector size should be set to 512 bytes. 3). GOODTRACKLAYOUT.
;
		test	word [DI+BDS.Flags],fNon_Removable ;Fixed disk? - J.K
		jz	short IOREMOVABLE		;no -
		mov	[cs:SECCNT], dx 		;# of sectors to I/O -
		mov	ax, dx				;		 
		call	DISK				;		
		pop	dx				;		 
		pop	cx				;		
		clc					;		
		retn					;		

IOREMOVABLE:		 
        	mov     AL,[cs:si+1]	; get sector size index

		; The next eight lines put sector size index in DPT
		push    ES                      ; save value while getting pointer
		push    SI                      ;    to DPT
		push	AX

		les     SI,[cs:DPT]		; ES:SI points to DPT
                                                ; put size in DPT
		mov     [ES:SI+DISK_PARMS.DISK_SECTOR_SIZ], AL
		mov     AX,[DI+BDS.SECLIM]        ; get number of sector/track
		mov     [ES:SI+DISK_PARMS.DISK_EOT],AL ; patch in DPT

		pop	AX			; restore register values
		pop     SI     
		pop     ES
                                        ; convert index to byte value
		call    SectorSizeIndexToSectorSize
		push    AX                      ; save number of bytes in sector
		mov     AX, DX                  ; get number of sector for I/0

DoTheIO:
		mov	[cs:SECCNT],ax	; set up the count of sectors to I/O
		call	DISK		
					; advance buffer pointer by adding
					;   sector size
		pop	ax
		add	bx, ax
		pop	dx
		pop	cx
		loop	IOnextSector
		call	DONE		; Set time of last access, and reset
		clc			; entries in DPT.
		retn

;
; The sector size in bytes needs to be converted to an index value for the IBM
; ROM. (0=>128, 1=>256,2=>512,3=>1024). It is assumed that only these values
; are permissible.
; On Input   AX contains sector size in bytes
; On Output  AL contains index
;

SectorSizeToSectorIndex:
		and     AH, 07h 	; very simple error correction
		mov     AL, AH          ; shift left 8 bits
		cmp     AL, 4           ; size 1024?
		jnz	short SecToIndexRet ; no, then we are done
		sub     AL, 1           ; if 1024, adjust index to 3
SecToIndexRet:
		retn

SectorSizeIndexToSectorSize:
; value in AH on entry is not important
		push    CX              ; save CX value
		mov     CL, AL          ; use index number as shift size
		mov     AX, 0080h       ; set AX to 128
		shl     AX, CL          ; shift by index to get proper value
		pop     CX              ; restore CX value
		retn

;
; Set up the ROM for formatting.
; we have to tell the ROM BIOS what type of disk is in the drive.
; On Input   - DS:DI - points to BDS
;
SetDASD:
; See if we have new ROM and have issues Set Media Type For Format call
        	test    byte [cs:Media_Set_For_Format],1
		jnz	short DasdHasBeenSet
; See if we have previously set DASD type
		cmp	byte [cs:Had_Format_Error],1
		je	short DoSetDasd
		test    word [DI+BDS.Flags],SET_DASD_true
		jz	short DasdHasBeenSet
		and     word [DI+BDS.Flags],~SET_DASD_true
                ; the next nine lines determine and put the DASD type in AL
DoSetDasd:
		mov	byte [cs:Had_Format_Error],0
        	mov	byte [cs:GAP_PATCH],50h	; assume 48tpi or 3.5" drive
        	cmp     byte [DI+BDS.FormFactor],ffSmall ; is 3.5" drive?
        	jnz	short Not35Drive	; no, skip down
        	mov     AL,04h			; yes set proper DASD value
		jmp     short Do_Set            ; jump down

Not35Drive:
		mov     AL,01h			;
		cmp     byte [DI+BDS.FormFactor],ff96tpi; 96tpi disk drive?
		jnz	short Do_Set		; no skip down to rom call
		inc     AL                      ; reflect 96tpi drive in DASD type
		cmp     byte [DI+BDS.SECLIM],15 ; 96tpi media in drive?
		jnz	short Do_Set		; no, skip down to rom call
		inc     AL                      ; reflect 96tpi media in DASD type
		mov	byte [cs:GAP_PATCH],54h ;  and in the GAP_PATCH
Do_Set:
		mov     AH,17h			; set command to Set DASD type
		mov     DL,[DI+BDS.DriveNum]	; set drive number
		int     13h                     ; call rom-bios
DasdHasBeenSet:
		mov     ah,[DI+BDS.SECLIM]
		mov	[cs:FORMT_EOT],ah
		retn

;
; This routine is called if an error occurs while formatting or verifying.
; It resets the drive, and decrements the retry count.
; On Entry - DS:DI - points to BDS for the drive
;	     BP    - contains retry count
; On Exit    Flags indicate result of decrementing retry count
;
;
;  There are some drives that "lose" the changeline indication if another
;  floppy drive is accessed before the changeline is recorded by the device
;  driver. In this situation, it is possible for the ROM to also not detect
;  that the medium has changed. So, the end result is that we could have a
;  diskette in the drive for which we can not even read the boot sector.
;  We "fix" this by setting the byte at location DISK_STATE_MACHINE_DRV_0 (hex)
;  for physical drive 0 (or DISK_STATE_MACHINE_DRV_1  for drive 1) to 0 (See
;  IBM PC/AT "blessed" addresses Document for explanation) . This tells the ROM
;  that the medium is 'unknown'.  The ROM actually uses these locations for
;  itself.  Note that we do this only for internal drives; we do not do this for
;  fixed disks or for physical drives > 1. We may end up corrupting some
;  other bytes in memory that may be used for something else.
;  NOTE: We do not stuff this byte if the last operation was a FORMAT because
;  the ROM loses track of what it is trying to format!!
;
;  This routine was changed to only stuff 61H when the drive indicated it
;  supported changeline. The Phoenix ROM was taking a very long time
;  to figure out what the media was which caused disk time outs to take
;  forever
;
;  We assume that DS:DI points to the current BDS for this drive.
;	no registers should be touched
;

AGAIN:
		call	ResetDisk
		dec	bp		; decrement retry count
		RETN

ResetDisk:
		push	ax
		xor	AH, AH			; set command to reset disk
		int	13h			; call the rom-bios
		pop	ax
		mov	byte [cs:STEP_DRV],-1	; zap up the speed
		retn

;
; This routine sets up the Drive Parameter Table with the values needed for
; Format, does an Int 13. Values in DPT are restored after a VERIFY is done.
;
; On Entry  -	DS:DI - points to BDS for the drive
;		ES:BX - points to TRKBUF
;		AL    - number of sectors
;		AH    - Int 13 function code
;		CL    - Sector number for verify
; On Exit   -	DS,DI,ES,BX remain unchanged.
;		ax and flags are the results of the int 13
;

To_ROM:
		push	ds
		push	di
		push	es
		push	bx
		push	si

; The below line was replaced because saving the DPT is predicated upon
; whether the functionality of the new ROM was used, not if it exists.
;		test	byte ptr cs:[NEW_ROM],1

		test	byte [cs:Media_Set_For_Format],1
		jnz	short Got_Valid_DPT

; Set up values in the DPT
; Set up motor start correctly for 3.5" drives.
		push	ax
		push	ds

		xor	ax,ax
		mov	ds,ax
		lds	si,[DSKADR]	; DS:SI := pDPT

		mov	[cs:DPT],si
		mov	[cs:DPT+2],ds	; Save pDPT

		pop     ds
		push    ES		; save value in ES
		LES     SI,[CS:DPT]

		mov     DX,[DI+BDS.SECLIM] ; set the sector per track in
		mov     [es:si+DISK_PARMS.DISK_EOT],DL	; the Disk Parameter Table
		cmp     DX, 15                  ; 96tip media?
		jz	short To_ROM1		; yes, skip down
                                ; no - set Format Gap to 320/360 media value
		mov     CL,[cs:GAP_PATCH]
		mov     [ES:SI+DISK_PARMS.DISK_FORMT_GAP],CL
To_ROM1:                                ; 3.5" floppy drive?
		cmp     byte [DI+BDS.FormFactor],ffSmall
		jnz	short To_ROM2	; no, skip down
                                ; yes - reset disk motor start value
		mov     byte [ES:SI+DISK_PARMS.DISK_MOTOR_STRT],4
To_ROM2:
		pop     ES		; restore ES value
		pop     ax

Got_Valid_DPT:
 					; now set up the registers
        	mov     DL,[DI+BDS.DriveNum] ; set drive number
        	mov     DH,[CS:HDNUM]	; set head number
		mov     CX,[CS:TRKNUM]	; set track number
		ror     CH,1
		ror     CH,1
		xchg    CH, CL
		int     13h		; call the rom-bios disk routines
		pop	si
		pop	bx
		pop	es
		pop	di
		pop	ds
		retn

;
; Get the owner of the physical drive represented by the logical drive in BL.
; The assumption is that we **ALWAYS** keep track of the owner of a drive!!
; If this is not the case, the system may hang, just following the linked list.
;

IOCTL$GETOWN:
		call	SETDRIVE
		mov	al,[DI+BDS.DriveNum]	; Get physical drive number
		push	cs
		pop	ds
		mov	di,START_BDS
Own_Loop:
		cmp	[DI+BDS.DriveNum],al
		jne	short GetNextBDS
		test	word [DI+BDS.Flags],fI_Own_Physical
		jnz	short Done_GetOwn
GetNextBDS:
		mov	bx,[DI+BDS.Link+2]
		;mov	di,[DI+BDS.Link]
		mov	di,[di] ; 05/07/2019
		mov	ds,bx
		jmp	short Own_Loop
Done_GetOwn:
		JMP	SHORT EXIT_OWN

;
; Set the ownership of the physical drive represented by the logical drive in
; BL.
;

IOCTL$SETOWN:
		call	SETDRIVE
		mov	byte [cs:fSetOwner],1	; set flag for CheckSingle to
						; look at.
		call	CHECKSINGLE		; Set ownership of drive
		mov	byte [cs:fSetOwner],0	; reset flag
		xor	bx,bx
		mov	es,bx
		mov	cl,-1
		mov	byte [es:LSTDRV],cl 	; Set up SDSB as well

EXIT_OWN:
; If there is only one logical drive assigned to this physical drive, return
; 0 to user to indicate this.
		xor	cl,cl
		test	word [DI+BDS.Flags],fI_Am_Mult
		jz	short EXIT_NO_MULT
		mov	cl,[DI+BDS.DriveLet]	; Get logical drive number
		inc	cl			; get it 1-based
EXIT_NO_MULT:
		LDS	BX,[CS:PTRSAV]
		mov	[BX+UNIT],CL
		jmp	EXIT

;
; Moves the old DPT that had been saved in TempDPT back to DPT. This is done
; only if the first byte of TempDPT is not -1.
; All registers (including flags) are preserved.
;

RestoreOldDPT:
; If we have already restored the disk base table earlier, do not do it
; again.
		push	ax
		xor	al,al
; Reset flag and get current flag setting
		mov	[cs:Had_Format_Error],al
		xchg	[cs:Media_Set_For_Format],al
		or	al,al
		jz	short DontRestore
		push	si
		push	ds
		push	es
		LDS	SI,[CS:TempDPT]
		xor	ax,ax
		mov	es,ax			; have ES -> segment 0
		MOV	[ES:DSKADR],SI
		MOV	[ES:DSKADR+2],DS
GotCurrentDPT:
		pop	es
		pop	ds
		pop	si
DontRestore:
		pop	ax
		clc				; clear carry
		retn				; (7/31/86)

;end of file msioctl.asm

; ............................................................................

;-----------------------------------------------------------------------------
; MSDISK.ASM (2) - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 28/05/2018 - Retro DOS v3.0
; 24/03/2018 - Retro DOS v2.0 

;
; Check_Wrap is a routine that adjusts the starting sector, starting head
; and starting cylinder for an Int 13 request that requests I/O of a lot
; of sectors. It only does this for fixed disks. It is used in the sections
; of code that handle ECC errors and DMA errors. It is necessary, because
; ordinarily the ROM would take care of wraps around heads and cylinders,
; but we break down a request when we get an ECC or DMA error into several
; I/O of one or more sectors. In this case, we may already be beyond the
; number of sectors on a track on the medium, and the request would fail.
;
; Input conditions:
;	ALL registers set up for an Int 13 request.
;
; Output:
;	 - contains starting head number for request
;	 - contains starting sector and cylinder numbers
;	(The above may or may not have been changed, and are 0-based)
;	All other registers preserved.
;

Check_Wrap:
		push	ax
		push	bx
		push	ds
		push	di
		mov	byte [cs:PHYS_DRV],1;Use phys. drive in AL to get BDS
		mov	al,dl		; put drive number in AL for get BDS
		call	SETDRIVE	; Get pointer to BDS for drive.
		mov	byte [cs:PHYS_DRV],0; Restore flag to use Logical Drive
		jc	short No_Wrap	; Do nothing if wrong phys. drive

		test	word [DI+BDS.Flags],fNon_Removable
		jz	short No_Wrap	; No wrapping for removable media
		MOV	BX,[DI+BDS.SECLIM]
		MOV	AX,CX
		AND	AX,003FH	; EXTRACT SECTOR NUMBER
		cmp	ax,bx		; If Wrap
		jbe	short No_Wrap

		div	bl		; AH=new sector#, AL = # of head wraps

; We need to be careful here.  If the new sector number == 0, then we are
; on the last sector on that track
		or	ah,ah
		jnz	short Not_on_Bound

		mov	ah,bl		; set sector = SECLIM if on Bndry
		dec	al		; and decrement Num. head wraps

Not_on_Bound:
		and	CL,0C0H		; zero out sector #

		or	CL,AH		; OR in new sector #
		xor	ah,ah		; AX = # of head wraps
		inc	ax
		add	al,DH		; add in starting head #
		adc	ah,0		; catch any carry
		CMP	AX,[DI+BDS.HDLIM] ; are we going to wrap around a head?
		jbe	short No_Wrap_Head ; Do not lose new head number!!

		push	DX		; preserve drive number and head number
		xor	dx,dx
		mov	bx,[DI+BDS.HDLIM]
		div	bx		; DX=new head #, AX=# of cylinder wraps
; Careful here! If new head # is 0, then we are on the last head.
		or	dx,dx
		jnz	short No_Head_Bound

		mov	dx,bx		; On boundary. Set to HDLIM
; If we had some cylinder wraps, we need to reduce them by one!!
		or	ax,ax
		jz	short No_Head_Bound

		dec	ax		; Reduce number of cylinder wraps
No_Head_Bound:
		mov	bh,dl		; bh has new head number
		POP	DX		; restore drive number and head number

		dec	bh		; get it 0-based
		mov	DH,bh		; set up new head number in DH

		mov	bh,CL
		and	bh,3FH		; preserve sector number
		mov	bl,6
		xchg	cl,bl
		shr	bl,cl		; get ms cylinder bits to ls end

		ADD	CH,AL		; ADD IN CYLINDER WRAP
		adc	bl,ah		; add in high byte
		shl	bl,cl		; move up to ms end
		xchg	bl,cl		; restore cylinder bits into CL
		or	CL,bh		; OR in sector number
No_Wrap:
		clc			; reset carry
		pop	di
		pop	ds
		pop	bx
		pop	ax
		retn

No_Wrap_Head:
		mov	DH,al		; Do not lose new head number
		dec	DH		; get it 0-based
		jmp	short No_Wrap

; INT_2F_13:
;		This code is chained into the INT_2F interrupt during bios
;	initialization.  It allows the user to change the ORIG13 int_13 vector
;	after booting.	This allows testing and implementation of custom int_13
;	handlers, without giving up MS-DOS error recovery
;
;	Entry Conditions
;		AH	== RESET_Int_13  (13h)
;		DS:DX	== Address of New INT_13 Handler
;		ES:BX	== Address of New INT_13 vector used by WARM BOOT
;								(INT 19)
;
;	Exit Conditions
;		Orig13	== Address of new Int_13 Handler
;		DS:DX	== Old ORIG13 value
;		ES:BX	== Old OLD13  value

INT_2F_13:
		cmp	AH,13h			; IF (interrupt_value != Reset_Int_13)
		je	short Chg_Orig13
		jmp	far [CS:NEXT2F_13]	;    THEN Continue on Int_2F chain

Chg_Orig13:					;    ELSE
		push	word [CS:ORIG13]	;	 Save Old value of OLD13 and
		push	word [CS:ORIG13+2]	;	 ORIG13 so that we can

		push	word [CS:OLD13]		;	 Return them to caller
		push	word [CS:OLD13+2]

		mov	word [CS:ORIG13],DX	;	 Orig13 := Addr. Of New INT_13
						;	             		Vector
		mov	word [CS:ORIG13+2],DS

		mov	word [CS:OLD13],BX	;	 Old13 := Addr. Of New
						;			  Boot_13 vector
		mov	word [CS:OLD13+2],ES

		pop	ES			;	 ES:BX := Old OLD13 vector
		pop	BX

		pop	DS			;	 DS:DX := Old ORIG13 vector
		pop	DX

		iret				;    END else

Move:
		push	CX
		mov	CX,512/2
		cld
		rep	MOVSW
		pop	CX
		retn

DoINT:
		mov	DL,[BP+INT13FRAME.olddx]
		xor	AH,AH
		or	AL,AL
		jz	short DoIntDone
		mov	AH,[BP+INT13FRAME.oldax+1]
		push	word [BP+INT13FRAME.oldf]
		call	far [CS:ORIG13]
		pushf
		pop	word [BP+INT13FRAME.oldf]
DoIntDone:
		retn

;-----------------------------------------------------------------------------
; MSBIO2.ASM - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 29/05/2018 - Retro DOS v3.0
; 25/03/2018 - Retro DOS v2.0 

;;Rev 3.30 modification ----------------------------

INT19SEM:
		DB	0		; INDICATE THAT ALL INT 19
					; INITIALIZATION IS COMPLETE

		;EVENB
align 4

ORIG19:
		dd	0

	;IRP	AA,<02,08,09,0A,0B,0C,0D,0E,70,72,73,74,76,77>
	;public	Int19OLD&AA
;Int19OLD&AA	dd	-1		;Orignal hw int. vec for INT 19h.
	;ENDM

INT19OLD02:	dd	-1
INT19OLD08:	dd	-1
INT19OLD09:	dd	-1
INT19OLD0A:	dd	-1
INT19OLD0B:	dd	-1
INT19OLD0C:	dd	-1
INT19OLD0D:	dd	-1
INT19OLD0E:	dd	-1
INT19OLD70:	dd	-1	
INT19OLD72:	dd	-1
INT19OLD73:	dd	-1
INT19OLD74:	dd	-1
INT19OLD76:	dd	-1
INT19OLD77:	dd	-1	

		;EVENB
;align 2

DSKDRVS:	; 29/05/2018
		DW	FDRIVE1
		DW	FDRIVE2
		DW	FDRIVE3
		DW	FDRIVE4
HDSKTAB:
		DW	HDRIVE
		DW	DRIVEX

;* Next area is reserved for mini disk BPB pointers *** 4/7/86
;* Don't change this pos. Should be add. from DskDrvs *** 4/7/86
MINI_DISK_BPB_PTRS:	
		times 40 db 0 ;4/7/86 - mem res for Mini disk.

	;EVENB
;align 2

INT_2F_NEXT:
		dd	0
RET_ADDR:
		dd	0

;;End of modification ----------------------------


;  INT19
;
;	We "hook" the INT_REBOOT vector, because contrary to IBM documentation,
;  it does NOT "bootstrap" the machine.  It leaves memory almost untouched.
;  Since the BIOS_INIT code assumes that certain Interrupt Vectors point to
;  the ROM_BIOS  we must "unhook" them before issuing the actual INT_REBOOT.
;  Currently the following vectors need to be unhooked:
;		  02,08,09,0A,0B,0C,0D,0E,70,72,73,74,75,76,77
;

INT19:
		xor	AX,AX		; get data segment to
		mov	DS,AX		;   point to the vector table
		les	DI,[CS:OLD13]	; get ES to point to this segment
		mov	[13h*4],DI	; restore old int13 value
		mov	[13h*4+2],ES

		cmp	byte [CS:INT19SEM], 0
		jnz	short int19vecs
		jmp	doint19

;Stacks code has changed these hardware interrupt vectors
;STKINIT in SYSINIT1 will initialize Int19hOLDxx values.

int19vecs:

;
; we now need to unhook all the vector replace to prevent stack overflow
;

;;Rev 3.30 modification ----------------------------

		XOR	AX,AX
		MOV	DS,AX

		;IRP	AA,<02,08,09,0A,0B,0C,0D,0E,70,72,73,74,76,77>

		;LES	DI,Int19OLD&AA

		LES	DI,[CS:INT19OLD02] ; 15/06/2018

		mov	ax,es		; Put segment where we can compare it
		cmp	ax,-1		; OPT 0ffffh is not likely
		;je	skip_int&AA	; OPT could get away without checking
		je	short skip_int02
		cmp	di,-1		; OPT offset here.
		;je	skip_int&AA
		je	short skip_int02

		;MOV	DS:[AA&H*4],DI
		mov	[02H*4],DI
		;MOV	DS:[AA&H*4+2],ES
		mov	[02H*4+2],ES
	
;skip_int&AA:
skip_int02:
		;ENDM

		LES	DI,[CS:INT19OLD08]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int08
		cmp	di,-1
		je	short skip_int08

		mov	[08H*4],DI
		mov	[08H*4+2],ES
	
skip_int08:
		LES	DI,[CS:INT19OLD09]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int09
		cmp	di,-1
		je	short skip_int09

		mov	[09H*4],DI
		mov	[09H*4+2],ES
	
skip_int09:
		LES	DI,[CS:INT19OLD0A]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int0A
		cmp	di,-1
		je	short skip_int0A

		mov	[0AH*4],DI
		mov	[0AH*4+2],ES
	
skip_int0A:
		LES	DI,[CS:INT19OLD0B]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int0B
		cmp	di,-1
		je	short skip_int0B

		mov	[0BH*4],DI
		mov	[0BH*4+2],ES
	
skip_int0B:
		LES	DI,[CS:INT19OLD0C]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int0C
		cmp	di,-1
		je	short skip_int0C

		mov	[0CH*4],DI
		mov	[0CH*4+2],ES
	
skip_int0C:
		LES	DI,[CS:INT19OLD0D]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int0D
		cmp	di,-1
		je	short skip_int0D

		mov	[0DH*4],DI
		mov	[0DH*4+2],ES
	
skip_int0D:
		LES	DI,[CS:INT19OLD0E]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int0E
		cmp	di,-1
		je	short skip_int0E

		mov	[0EH*4],DI
		mov	[0EH*4+2],ES
	
skip_int0E:
		LES	DI,[CS:INT19OLD70]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int70
		cmp	di,-1
		je	short skip_int70

		mov	[70H*4],DI
		mov	[70H*4+2],ES
	
skip_int70:
		LES	DI,[CS:INT19OLD72]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int72
		cmp	di,-1
		je	short skip_int72

		mov	[72H*4],DI
		mov	[72H*4+2],ES
	
skip_int72:
		LES	DI,[CS:INT19OLD73]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int73
		cmp	di,-1
		je	short skip_int73

		mov	[73H*4],DI
		mov	[73H*4+2],ES
	
skip_int73:
		LES	DI,[CS:INT19OLD74]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int74
		cmp	di,-1
		je	short skip_int74

		mov	[74H*4],DI
		mov	[74H*4+2],ES
	
skip_int74:
		LES	DI,[CS:INT19OLD76]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int76
		cmp	di,-1
		je	short skip_int76

		mov	[76H*4],DI
		mov	[76H*4+2],ES
	
skip_int76:
		LES	DI,[CS:INT19OLD77]

		mov	ax,es
		cmp	ax,-1
		je	short skip_int77
		cmp	di,-1
		je	short skip_int77

		mov	[77H*4],DI
		mov	[77H*4+2],ES
	
skip_int77:

;;End of modification ----------------------------

doint19:
		LES	DI,[CS:ORIG19]
		MOV	[19h*4],DI
		MOV	[19h*4+2],ES

		INT	19h


;*****************************************************************************

DSK$INIT:	; 29/05/2018 - Retro DOS v3.0
		PUSH	CS
		POP	DS

		MOV	AH,[DRVMAX]
		MOV	DI,DSKDRVS
		JMP	SetPTRSAV

;
; Int 2f handler for external block drivers to communicate with the internal
; block driver in msdisk. The multiplex number chosen is 8. The handler
; sets up the pointer to the request packet in [PTRSAV] and then jumps to
; DSK$IN, the entry point for all disk requests.
; On exit from this driver (at EXIT), we will return to the external driver
; that issued this Int 2F, and can then remove the flags from the stack.
; This scheme allows us to have a small external device driver, and makes
; the maintainance of the various drivers (DRIVER and msBIO) much easier,
; since we only need to make changes in one place (most of the time).
;
; AL contains the Int2F function:
;   0 - Check for installed handler - RESERVED
;   1 - Install the BDS into the linked list
;   2 - DOS request
;

MYNUM		EQU	8

INT2F_DISK:
		cmp	ah,MYNUM
		je	short Mine
		jmp	far [cs:INT_2F_NEXT]	; chain to next Int 2F handler
Mine:
		cmp	al,0F8H 		; IRET on reserved functions
		jb	short Do_Func
		IRET
Do_Func:
		or	al,al			; A GET INSTALLED STATE request?
		jne	short Disp_Func
		mov	al,0FFH
		IRET
Disp_Func:
		cmp	al,1			; Request for installing BDS?
		jne	short Do_DOS_Req
		call	INSTALL_BDS
		IRET
Do_DOS_Req:
; Set up pointer to request packet
		MOV	[CS:PTRSAV],BX
		MOV	[CS:PTRSAV+2],ES
		jmp	DSK$IN

;
; Install_BDS installs a BDS a location DS:DI into the current linked list of
; BDS maintained by this device driver. It places the BDS at the END of the
; list.

INSTALL_BDS:
; ds:di point to BDS to be installed
		les	si,[cs:START_BDS]	; Start at beginning of list
		push	es			; Save pointer to current BDS
		push	si
; es:si now point to BDS in linked list
Loop_Next_BDS:
		cmp	si,-1			; got to end of linked list?
		jz	short Install_Ret
; If we have several logical drives using the same physical drive, we must
; set the I_Am_Mult flag in each of the appropriate BDSs.
		mov	al,[DI+BDS.DriveNum]
		cmp	[ES:SI+BDS.DriveNum],al
		jnz	short Next_BDS
		xor	bx,bx
		mov	bl,fI_Am_Mult
		or	[DI+BDS.Flags],bx	; set flags in both BDSs concerned
		or	[ES:SI+BDS.Flags],bx
		mov	bl,fI_Own_Physical
		xor	bx,-1
		and	[DI+BDS.Flags],bx	; reset that flag for 'new' BDS
; We must also set the fChangeline bit correctly.
		mov	bx,[ES:SI+BDS.Flags]	; determine if changeline available
		and	bl,fChangeline
		xor	bh,bh
		or	[DI+BDS.Flags],bx

Next_BDS:
; Before moving to next BDS, preserve pointer to current one. This is needed at
; the end when the new BDS is linked into the list.
		pop	bx			; discard previous pointer to BDS
		pop	bx
		push	es
		push	si
		mov	bx,[ES:SI+BDS.Link+2]
		;mov	si,[ES:SI+BDS.Link]
		mov	si,[es:si] ; 05/07/2019
		mov	es,bx
		jmp	short Loop_Next_BDS

Install_Ret:
		pop	si			; Retrieve pointer to last BDS
		pop	es			; in linked list.
		mov	ax,ds
		mov	[ES:SI+BDS.Link+2],ax	; install BDS
		;mov	[ES:SI+BDS.Link],di
		mov	[es:si],di ; 05/07/2019
		;mov	word [DI+BDS.Link],-1	; set NEXT pointer to NULL
		;mov	dword [DI+BDS.Link],-1  ; Retro DOS v3.0
		mov	word [DI],-1 ; 05/07/2019
		retn

;
; RE_INIT installs the Int 2F vector that will handle communication between
; external block drivers and the internal driver. It also installs the
; Reset_Int_13 interface. It is called by SYSYINIT
;

RE_INIT:
		PUSH	AX
		PUSH	DS
		PUSH	DI
		XOR	DI,DI
		MOV	DS,DI
		MOV	DI,2FH*4		; point it to Int 2F Vector
		MOV	AX,[DI]
		MOV	[CS:INT_2F_NEXT],AX
		MOV	AX,[DI+2]		; preserve old Int 2F vector
		MOV	[CS:INT_2F_NEXT+2],AX

; INSTALL the Reset_Int_13
; interface
		CLI
		MOV	word [DI],INT_2F_13	; install new vectors
		MOV	[DI+2],CS
		STI
		POP	DI
		POP	DS
		POP	AX
		RETF


;-------------------------------------------------
;
;  Ask to swap the disk in drive A:
; Using a different drive in a one drive system so
; request the user to change disks
;

SWPDSK:		; 29/05/2018 - Retro DOS v3.0

		; 13/04/2018
		; 09/04/2018
		; Retro DOS v2.0 (IBMDOS 2.1, IBMBIO.COM)

		mov	al,[DI+BDS.DriveLet]	; get the drive letter
		add	al,"A"
		mov	[cs:DRVLET],AL

		push	ds			; preserve segment register
		push	cs
		pop	ds
		mov	SI,SNGMSG		; ds:si -> message
		push	BX

		call	WRMSG			;Print disk change message
		call	FLUSH
						; wait for a keyboard character
		xor	AH, AH			; set command to read character
		int	16h			; call rom-bios
		POP	BX
		pop	ds			; restore segment register
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
; Single drive message for msbio.com. Nul terminated.
;

SNGMSG:	DB	0Dh,0Ah,"Insert diskette for drive "
DRVLET:	DB	"A: and strike",0Dh,0Ah,"any key when ready",0Dh,0Ah,0Ah,0

;----------------------------------------------------------------------------

; 01/06/2018 - Retro DOS v3.0

;
; End of support for multiple floppies with no logical drives
; This is not 'special' any more because we now have the capability of
; defining logical drives in CONFIG.SYS. We therefore keep the code for
; swapping resident ALL the time.
;

;;Rev 3.30 modification ----------------------------
;Variables for Dynamic Relocatable modules
;These should be stay resident.

INT6C_RET_ADDR:	DD	0	; ret add from INT 6C for P12 mach

;
;   DATA STRUCTURES FOR REAL-TIME DATE AND TIME
;

BIN_DATE_TIME:
	DB	0		; CENTURY (19 OR 20) OR HOURS (0-23)
	DB	0		; YEAR IN CENTURY (0-99) OR MINUTES (0-59)
	DB	0		; MONTH IN YEAR (1-12) OR SECONDS (0-59)
	DB	0		; DAY IN MONTH (1-31)
MONTH_TABLE:
	DW	0		;MJB002 JANUARY
	DW	31		;MJB002 FEBRUARY
	DW	59		;MJB002
	DW	90		;MJB002
	DW	120		;MJB002
	DW	151		;MJB002
	DW	181		;MJB002
	DW	212		;MJB002
	DW	243		;MJB002
	DW	273		;MJB002
	DW	304		;MJB002
	DW	334		;MJB002
DAYCNT2:
	DW	0		;MJB002 TEMP FOR CNT OF DAYS SINCE 1-1-80
FEB29:
	DB	0		;MJB002 FEBRUARY 29 IN A LEAP YEAR FLAG

;;End of modification ------------------------------

EndFloppy:

;
; End of code for virtual floppy drives
;

EndSwap:

HNUM:	
	DB	0			; number of hardfile (hard drives)
HARDDRV:
	DB	80H			;Physical drive number of first hardfile


;
;  "HDRIVE" is a hard disk with 512 byte sectors
;

	;EVENB

align 2

BDSH:
	DW	-1			; Link to next structure
	DW	KERNEL_SEGMENT
	DB	80h			; physical drive number
	DB	"C"                     ; Logical Drive Letter
HDRIVE:
	DW	512
	DB	1			; Sectors/allocation unit
	DW	1			; Reserved sectors for DOS
	DB	2			; No. of allocation tables
	DW	16			; Number of directory entries
	DW	0000			; Number of sectors (at 512 bytes each)
	DB	11111000B		; Media descriptor
	DW	1			; Number of FAT sectors
	DW	00			; Sector limit
	DW	00			; Head limit
	DW	00			; Hidden sector count
	DB	0			; TRUE => bigfat
OPCNTH:	DW	0			; Open Ref. Count
VOLIDH:	DB	"NO NAME    ",0         ; Volume ID for this disk
	DB	3			; Form Factor
FLAGSH:	DW	0020H			; Various Flags
	DW	40			; number of cylinders
RecBPBH:
	times	31 db 0			; Recommended BPB for drive
TRACKH:	
	DB	-1			; Last track accessed on this drive
TIM_LOH:
	DW	-1			; Keep these two contiguous (?)
TIM_HIH:
	DW	-1

;
; End of single hard disk section
;

EndOneHard:

;
;"DRIVEX" is an extra type of drive usually reserved for an
; additional hard file
;

	;EVENB

align 2

BDSX:
	DW	-1			; Link to next structure
	DW	KERNEL_SEGMENT
	DB	81h			; physical drive number
	DB	"D"                     ; Logical Drive Letter
DRIVEX:
	DW	512
	DB	00			; Sectors/allocation unit
	DW	1			; Reserved sectors for DOS
	DB	2			; No. of allocation tables
	DW	0000			; Number of directory entries
	DW	0000			; Number of sectors (at 512 bytes each)
	DB	11111000B		; Media descriptor
	DW	0000			; Number of FAT sectors
	DW	00			; Sector limit
	DW	00			; Head limit
	DW	00			; Hidden sector count
	DB	0			; TRUE => bigfat
OPCNTD:	DW	0			; Open Ref. Count
VOLIDD:	DB	"NO NAME    ",0         ; Volume ID for this disk
	DB	3			; Form Factor
FLAGSD:	DW	0020H			; Various Flags
	DW	40			; number of cylinders
RecBPBD:
	times	31 db 0			; Recommended BPB for drive
TRACKD:
	DB	-1			; Last track accessed on this drive
TIM_LOD:
	DW	-1			; Keep these two contiguous
TIM_HID:
	DW	-1

;
; End of section for two hard disks

EndTwoHard:

TWOHARD:

;
; End of code for virtual floppy drives
;

; include ms96tpi.inc

; MS96TPI.INC - 24/07/1987
;----------------------------------------------------------------------------
; 01/06/2018 - Retro DOS v3.0
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

;------------------------------------------------------------------------
;									:
;		DISK OPEN/CLOSE ROUTINES				:
;									:
;------------------------------------------------------------------------

DSK$OPEN:					; AL is logical drive
		call	SETDRIVE		; Get BDS for drive
		inc	WORD [DI+BDS.Opcnt]
		jmp	EXIT

DSK$CLOSE:					; AL is logical drive
		call	SETDRIVE		; Get BDS for drive
		cmp	WORD [DI+BDS.Opcnt],0
		jz	short EXITJX		; Watch out for wrap
		dec	WORD [DI+BDS.Opcnt]
EXITJX:
		jmp	EXIT

;
; ChkOpCnt checks the number of open files on drive.
;
; Input : DS:DI points to current BDS for drive.
;
; Return : zero set if no open files
;	   zero reset if open files
;

ChkOpCnt:
		cmp	WORD [DI+BDS.Opcnt],0
		retn

;
; At media check time, we need to really get down and check what the change is.
; This is GUARANTEED to be expensive.
;
; On entry AL contains logical drive number
;

MediaCheck:
		; 01/06/2018 - Retro DOS v3.0

		; 13/04/2018
		; 08/04/2018
		; Retro DOS v2.0

		call	CHECKSINGLE	; make sure correct disk is in place
		xor	SI,SI
		call	HasChange
		jz	short MediaRet
		call	CheckROMChange
		jnz	short MediaDoVolId
		push	AX
		push	DX
					; see if changeline has been triggered
;;Rev 3.30 Modification
		mov	DL, [DI+BDS.DriveNum] ; set logical drive number	  
		mov	AH, 16h		; get changeline status	  
		int	13h		; call rom diskette routine	  
;;End of Modification
		pop	DX
		pop	AX
		jc	short MediaDoVolId ; if changeline was triggered jmp
		mov	SI,1		; else signal no change

; There are some drives with changeline that "lose" the changeline indication
; if a different drive is accessed after the current one. In order to avoid
; missing a media change, we return an "I don't know" to DOS if the changeline
; is not active AND we are accessing a different drive from the last one.
; If we are accessing the same drive, then we can safely rely on the changeline
; status.

LOSECHNG:
		mov	bl,[cs:TIM_DRV] ; get last drive accessed
		cmp	byte [DI+BDS.DriveNum],bl
		jz	short MediaRet

; Do the 2 second twiddle. If time >= 2 seconds, do a volid check.
; Otherwise return "I don't know" (Strictly speaking, we should return a
; "Not Changed" here since the 2 second test said no change.) - RS.

		push	ax
		push	cx
		push	dx
		call	CHECK_TIME_OF_ACCESS
		pop	dx
		pop	cx
		pop	ax
		or	si,si
		jz	short MediaDoVolId ; Check_Time says ">= 2 secs passed"
		xor	si,si		; return "I don't know"
MediaRet:
		retn

; 01/06/2018 - Retro DOS v3.0


;
; MediaDoVolid:  if this is called somehow the media was changed.  Look at
; VID to see. We do not look at FAT because this may be different since we
; only set MedByt when doing a READ or WRITE.
;

MediaDoVolId:
		call	GETBP		; build a new BPB in current BDS
		jc	short MediaRet
		call	Check_VID
		jnc	short MediaRet
		call	MAPERROR	; fix up AL for return to DOS
		retn

;
; Checklatchio:
;
; Simple, quick check of latched change.  If no indication, then return
; otherwise do expensive check.  If the expensive test fails, POP off the
; return and set AL = 15 (for invalid media change) which will be returned to
; DOS.
;

CheckLatchIO:
; If returning fake BPB then assume the disk has not changed
;		test	word [DI+BDS.Flags],RETURN_FAKE_BPB
;		jnz	short CheckRet
;;Rev 3.30 Modification
		call	HasChange		;change line supported?
		jz	short CheckRet		;No. Just return
;;End of Modification
		call	ChkOpCnt
		jnz	short CheckROM
CheckRet:
		retn
;
; Check for past ROM indications.  If no ROM change indicated, then return OK.
;

CheckROM:
		call	CheckROMChange
		jz	short CheckRet		; no change
;
; We now see that a change line has been seen in the past.  Let's do the
; expensive verification.
;
		call	GETBP			; build BPB in current BDS
		jc	short Ret_No_Error_Map	; GETBP has already called MapError
		call	Check_VID
		jc	short CheckLatchRet	; disk error trying to read in.
		or	SI,SI			; Is changed for sure?
		jns	short CheckRet
		call	ReturnVID
CheckLatchRet:
		call	MAPERROR		; fix up AL for return to DOS
Ret_No_Error_Map:
		stc				; indicate an error
		pop	si			; pop off return address
		retn

;
;  CheckFatVID:
;
; Check the FAT and the VID.  Return in DI -1 or 0.  Return with carry set
; ONLY if there was a disk error.  Return that error code in AX.
;

CheckFATVID:
		call	FAT_CHECK
		or	SI,SI
		js	short Changed_Drv
;
; The fat was the same.  How about the volume ID?
;
Check_VID:
		call	Read_Volume_ID
		jc	short CheckFatRet
		call	Check_Volume_ID

		or	SI,SI
		jnz	short Changed_Drv
		call	ResetChanged
CheckFatRet:
		retn
Changed_Drv:
		mov	byte [cs:TIM_DRV],-1 	; Ensure that we ask ROM for media
		retn				; check next time round

;
; CheckIO:  At I/O time the rom-bios returned an error.  We need to
; determine if the error is due to a media change.  If error code is not
; change-line error (06h) we just return.  We pop off the call and jmp to
; harderr if we see an error.
;
;   On entry:  AH contains error code returned from rom-bios.
;

CheckIO:
		cmp	AH,6			; change line error?
		jnz	short CheckFatRet	; no - just return
		call	ChkOpCnt
		jz	short CheckFatRet	; no open files
; If returning fake BPB then ignore disk changes
;		test	word [DI+BDS.Flags],RETURN_FAKE_BPB
;		jnz	short IgnoreChange
		call	GETBP			; build up a new BPB in current BDS
		jc	short No_Error_Map	; GETBP has already called MapError
		call	CheckFATVID
		jc	short CheckIORet	; disk error trying to read in.
		or	SI,SI			; Is changed for sure?
		js	short CheckIOErr	; yes changed
IgnoreChange:
		inc	BP			; allow a retry
		retn
CheckIOErr:
		call	ReturnVID
CheckIORet:
		stc				; make sure carry gets passed through
		jmp	HARDERR

No_Error_Map:
		jmp	HARDERR2

;
; Return VID sets up the VID for a return to DOS.
;

ReturnVID:
		push	DS			; save pointer to current BDS
		push	di
		push	cx
		call	init_vid_loop		; Sets ES:DI -> vid
		lds	BX,[cs:PTRSAV]
		mov	[BX+EXTRA],DI
		mov	[BX+EXTRA+2],ES
		pop	cx
		pop	di			; restore current BDS
		pop	DS
;;		MOV	AH,6			; INVALID MEDIA CHANGE
		mov	AH,0Fh			; set error as 'invalid media change'
		stc				; indicate error by setting carry flag
		retn

;
; Media_Set_VID:
;
; Moves the pointer to the volid for the drive into the original request packet
; On entry, DS:BX points to the original packet.
; No attempt is made to preserve registers.
;

MEDIA_SET_VID:
		call	init_vid_loop		; Sets ES:DI -> vid
		lds	bx,[cs:PTRSAV]		; get pointer to packet
		mov	[BX+TRANS+1],DI
		mov	[BX+TRANS+3],ES
		retn

;
;   HiDensity - examine a drive/media descriptor to set the media type.  If
;   the media descriptor is NOT F9 (not 96tpi or 3 1/2), we return and let the
;   caller do the rest.  Otherwise, we pop off the return and jump to the tail
;   of GETBP. For 3.5" media, we just return.
;
;   Inputs:	DS:DI point to correct BDS for this drive
;		AH has media byte
;
;   Outputs:	Carry clear
;		    No registers modified
;		Carry set
;		    AL = sectors/fat
;		    BH = number of root directory entries
;		    BL = sectors per track
;		    CX = number of sectors
;		    DH = sectors per allocation unit
;		    DL = number of heads
;

hidensity:
; Check for correct drive
;
		test	word [DI+BDS.Flags],fChangeline	; is it special?
		jz	short DoFloppy		; no, do normal floppy test
;
; We have a media byte that is pretty complex.	Examine drive information
; table to see what kind it is.
;
		cmp	byte [DI+BDS.FormFactor],ffSmall;  Is it single-media?
		jz	short DoFloppy		; yes, use fatid...
;
; 96 tpi drive
;
		cmp	AH,0F9h
		jnz	short DoFloppy
		mov	al,7			; seven sectors / fat
		mov	bx,224*256+0Fh		; 224 root dir entries & 0f sector max
		mov	cx,80*15*2		; 80 tracks, 15 sectors/track, 2 sides
		mov	dx,1*256+2		; sectors/allocation unit & head max
popr:
		add	SP,2			; pop off return address
		jmp	HAS1_res		; return to tail of GETBP


DoFloppy:
		retn

;
; Certain poorly designed programs avoid DOS altogether and use INT 13 directly.
; These programs even retry operations and, thus, will ignore the disk change
; logic.
;
; We hook INT 13 and note all errors.
;

REAL13:		dd	0
OLDINT:		dd	0
DMY:		dw	0

INT13:
		; 16/06/2018
		pop	word [cs:OLDINT]
		pop	word [cs:OLDINT+2]
		pop	word [cs:DMY]
		pushf
		call	far [cs:REAL13]	; simulate another INT 13
		jc	short Err13	; did an error occur?
		jmp	far [cs:OLDINT]	; no, return and pop off flags
Err13:
		pushf			; save state
		cmp	AH,06h		; is error a 'change' error?
		jz	short GotErr	; yes, jump down
B:		
		popf			; no, some other error, ignore it ;;End of Modification
		jmp	far [cs:OLDINT]	; return and pop off flags

GotErr: 	
		or	DL,DL		; is this for the hard disk?
		js	short B		; yes, ignore
		mov	word [cs:FlagBits],fChanged
		call	Set_Changed_DL
		jmp	short B

;
; Set_Changed_DL - Sets flag bits according to bits set in [FlagBits].
;		   Essentially used to indicate Changeline, or Format.
;
;   Inputs:	DL contains physical drive number
;		[FlagBits] contains bits to set in the flag field in the BDSs
;   Outputs:	None
;   Registers modified: Flags
;

Set_Changed_DL:
		push	BX
		push	DX
		mov	BL,DL
ALL_SET:
		mov	dx,[cs:FlagBits] ; get bits to set in flag field
		xor	BH,BH
;
; In the virtual drive system we *must* flag the other drives as being changed
;
; assume first BDS is in this segment
		push	ax
		push	ds		; save current BDS
		push	di
		lds	di,[cs:START_BDS]
Scan_BDS:
		cmp	di,-1
		jz	short SkipSet
		cmp	byte [DI+BDS.DriveNum],bl
		jnz	short Get_Next_BDS
;
; Someone may complain, but this *always* must be done when a disk change is
; noted.  There are *no* other compromising circumstances.
;
SetChanged:
		or	[DI+BDS.Flags],dx ; signal change on other drive
Get_Next_BDS:
		mov	ax,[DI+BDS.Link+2]  ; go to next BDS
		;mov	di,[DI+BDS.Link]
		mov	di,[di] ; 05/07/2019
		mov	ds,ax
		jmp	short Scan_BDS
SkipSet:
		pop	di		    ; restore current BDS
		pop	ds
		pop	ax
		pop	DX
		pop	BX
		retn

;
; CheckROMChange - see if external program has diddled ROM change line.
;
;   Inputs:	DS:DI points to current BDS.
;   Outputs:	Zero set - no change
;		Zero reset - change
;   Registers modified: none

CheckROMChange:
		test	word [DI+BDS.Flags],fChanged
		retn

;
; ResetChanged - restore value of change line
;
;   Inputs:	DS:DI points to current BDS
;   Outputs:	none
;   Registers modified: none

ResetChanged:
		and	word [DI+BDS.Flags],~fChanged
		retn

;
; HasChange - see if drive can supply change line
;
;   Inputs:	DS:DI points to current BDS
;   Outputs:	Zero set - no change line available
;		Zero reset - change line available
;   Registers modified: none

HasChange:
		test	word [DI+BDS.Flags],fChangeline
		retn

; 01/06/2018 - Retro DOS v3.0

; include msvolid.inc

;-------------------------------------------------------------------------
;
;  File: msvolid.asm
;	This file contains the volume_id subroutines and data structures.
;
;	Routines in this file are:
;	   Set_Volume_ID       -	main routine, calls other routines.
;	   read_volume_id      -	read the volume ID and tells if it has
;					   been changed.
;	   Transfer_volume_id  -	copy the volume ID from TMP to special
;					   drive.
;	   Check_Volume_ID     -	compare volume ID in TMP area with one
;					   expected for drive.
;	   Fat_Check	       -	see of the fatID has changed in the
;					   specified drive.
;	   Init_Vid_loop       -	set up for VID scan or move
;
;
;-------------------------------------------------------------------------

;
; length of the volume id
;

vid_size    equ 12

;
; null volume id
;

nul_vid:
	db  "NO NAME    ",0

;
; data scratch area used to hold volume ids
;

tmp_vid:
	db  "NO NAME    ",0

;
; Set_Volume_ID
;   If drive has changeline support, read in and set the volume_ID
; and the last FAT_ID byte.  If no change line support then do nothing.
;
;   On entry:
;	DS:DI points to the BDS for this disk.
;	AH contains media byte
;
;   On Exit:
;	Carry clear:
;	   Successful call
;	Carry set
;	   Error and AX has error code
;

Set_Volume_ID:
		push	dx		; save registers
		push	ax
		CALL	HasChange	; does drive have changeline support?
		jz	short setvret	; no, get out
		push	di
		call	Read_Volume_ID	; read the volume ID
		pop	di
		jc	short SetErr	; if error go to error routine
		call	transfer_volume_ID ; copy the volume id to special drive
		call	ResetChanged	; restore value of change line
setvret:				; SET Volume RETurn
		clc			; no error, clear carry flag
		pop	ax		; restore registers
		pop	dx
		retn
SetErr:
		pop	dx		; pop stack but don't overwrite AX
		pop	dx		; restore DX
		retn

root_sec:
		DW	0		;Root sector #

;
; read_volume_id read the volume ID and tells if it has been changed.
;
;   On entry:
;	DS:DI points to current BDS for drive.
;   On Exit:
;	Carry Clear
;	    SI = 1  No change
;	    SI = 0  ?
;	    SI = -1 Change
;
;	Carry Set:
;	    Error and AX has error code.
;

Read_Volume_ID:
		push	ES		; preserve registers
		push	DX
		push	CX
		push	BX
		push	AX
		push	DS		; Preserve Current BDS
		push	DI
		push	cs		; get ES segment correct
		pop	es
		push	cs		; get DS segment correct
		pop	ds
		mov	di,tmp_vid
		mov	si,nul_vid
		mov	cx,vid_size
		rep	movsb		; initialize tmp_vid to null vi_id

		pop	DI		; Restore Current BDS
		pop	DS
		mov	al,[DI+BDS.cFAT] ; # of fats
		mov	cx,[DI+BDS.cSecFat] ; sectors / fat
		mul	cl			    ; size taken by fats
		add	ax,[DI+BDS.RESSEC] ; add on reserved sectors
					; AX is now sector # (0 based)
		mov	[cs:root_sec],ax ; set initial value
		mov	ax,[DI+BDS.cDir] ; # root dir entries
		mov	cl,4		; 16 entries/sector
		shr	ax,cl		; divide by 16
		mov	cx,ax		; cx is # of sectors to scan
next_sec:
		push	cx		; save outer loop counter
		mov	ax,[cs:root_sec] ; get sector #
		mov	cx,[DI+BDS.SECLIM] ; sectors / track
		xor	DX,DX
		div	cx
				; set up registers for call to read_sector
		inc	DX	; dx= sectors into track, ax= track count from 0
		mov	cl,dl	; sector to read
		xor	DX,DX
		div	word [DI+BDS.HDLIM] ; # heads on this disc
		mov	dh,dl	; Head number
		mov	ch,al	; Track #
		call	READ_SECTOR ; get first sector of the root directory,
				; ES:BX -> BOOT
		jc	short ReadVIDErr ; error on read
		mov	cx,16		; # of dir entries in a block of root
		mov	al,08h		; volume label bit
fvid_loop:
		cmp	byte [es:bx],0	; End of dir?
		jz	short no_vid	; yes, no vol id
		cmp	byte [es:bx],0E5h ; empty entry?
		jz	short ent_loop	; yes, skip
		test	[es:bx+11],al	; is volume label bit set in fcb?
		jnz	short found_vid	; jmp yes
ent_loop:
		ADD	BX,32		;ADD LENGTH OF DIRECTORY ENTRY
		loop	fvid_loop
		pop	cx		; outer loop
		inc	word [cs:root_sec] ; next sector
		loop	next_sec	; continue
NotFound:
		XOR	SI,SI
		jmp	short fvid_ret

found_vid:
		pop	cx		; clean stack of outer loop counter
		mov	si,bx		; point to volume_id
		push	ds		; preserve currnet BDS
		push	di
		push	es		; es:si points to volume id.
		pop	ds		; source segment
		push	cs
		pop	es		; destination segment
		mov	di,tmp_vid	; dest of volume_id
		mov	cx,vid_size-1	; length of string minus NUL
		rep	movsb		; mov volume label to tmp_vid
		xor	al,al
		stosb			; Null terminate
		XOR	SI,SI
		pop	DI		; restore current BDS
		pop	DS
fvid_ret:
		pop	ax
		clc
RVIDRet:
		pop	BX		; restore register
		pop	CX
		pop	DX
		pop	ES
		retn
no_vid:
		pop	cx		; clean stack of outer loop counter
		jmp	short NotFound	; not found
ReadVIDErr:
		pop	SI
		pop	SI
		jmp	short RVIDRet

;
;   Transfer_volume_id - copy the volume ID from TMP to special drive
;
;   Inputs:	DS:DI nas current BDS
;   Outputs:	BDS for drive has volume ID from TMP
;

transfer_volume_ID:
		push	DS		; preserve current BDS
		push	DI
		push	ES
		push	SI
		push	CX
		call	init_vid_loop
		cld
		rep	MOVSB		; transfer
		pop	CX
		pop	SI
		pop	ES
		pop	DI		; restore current BDS
		pop	DS
		retn

;
;   Check_Volume_ID - compare volume ID in TMP area with one expected for
;	drive
;
;   Inputs:	DS:DI has current BDS for drive
;   Outputs:	SI = 0 if compare succeeds
;		SI = -1 if compare fails.

Check_Volume_ID:
		push	DS		; preserve current BDS for drive
		push	DI
		push	ES
		push	CX
		call	init_vid_loop
		cld
		repz	cmpsb		; are the 2 volume_ids the same?
		mov	si,0		; assume unknown
		jz	short check_vid_ret ; carry clear if jump taken
		mov	si,-1		; failure
check_vid_ret:
		pop	CX
		pop	ES
		pop	DI		; restore current BDS
		pop	DS
		retn

;
;   Fat_Check - see of the fatID has changed in the specified drive.
;	      - uses the FAT ID obtained from the boot sector.
;
;   Inputs:	MedByt is expected FAT ID
;		DS:DI points to current BDS
;   Output:	Carry Clear
;		    SI = -1 if fat ID different,
;		    SI = 0 otherwise
;   No other registers changed.

FAT_CHECK:
		push	AX
		xor	SI,SI		; say FAT ID's are same.
		mov	AL,[cs:MedByt]
					 ; compare it with the BDS medbyte
		cmp	AL,[DI+BDS.Mediad]
		jz	short OkRet1	; carry clear
		dec	SI
OkRet1:		
		clc
		pop	AX
		retn

;
;   Init_Vid_loop - set up for VID scan or move
;
;   Inputs:	DS:DI pionts to BDS for the drive
;   Outputs:	DS:SI points to tmp_vid
;		ES:DI points to vid for drive
;		CX has size for VID compare
;

init_vid_loop:
		push	ax
		push	ds
		pop	es
		push	cs
		pop	ds
		mov	si,tmp_vid	; source
		add	di,BDS.Volid
		mov	cx,vid_size
		pop	ax
		retn

;-----------------------------------------------------------------------------

End96tpi:

;Rev 3.30 modification ----------------------------
;Memory allocation for BDSM table.

BDSMs:
		;times BDSM.size*Max_mini_dsk_num db 0	;currently max. 23

	; 02/08/2019
		%rep	Max_mini_dsk_num ; 23 
;BDSM:
		DW	-1			; Link to next structure
		DW	0
		DB	50h			; physical drive number
		DB	3                    	; Logical Drive Letter
;DRIVEM:
		DW	512
		;DB	0
		db	1	; Sectors/allocation unit
		DW	1			; Reserved sectors for DOS
		DB	2			; No. of allocation tables
		DW	16			; Number of directory entries
		DW	0			; Number of sectors (at 512 bytes each)
		DB	11111000b  ; 0F8h	; Media descriptor
		DW	1			; Number of FAT sectors
		DW	0			; Sector limit
		DW	0			; Head limit
		DW	0			; Hidden sector count
		DB	0			; TRUE => bigfat
;OPCNTM:
		DW	0			; Open Ref. Count
;VOLIDM: 
		db	"NO NAME    ",0	; Volume ID for this disk
		DB	3			; Form Factor
;FLAGSM:	
		DW	20h			; Various Flags
		DW	40			; number of cylinders
;RecBPBM:
		times	31 db 0			; Recommended BPB for drive
;TRACKM:
		DB	-1			; Last track accessed on this drive
;BDSM_ISMINI:
		DW	1			; Keep these two contiguous
;BDSM_HIDDEN_TRKS:
		DW	0

		%endrep

;** End_of_BDSM defined in MSINIT.ASM will be used to set the appropriate
;** ending address of BDSM table.
;;End of modification ----------------------------

;
;;3.3 BUG FIX -SP ------------------------------
;Paragraph buffer between the BDSMs and MSHARD
;
;The relocation code for MSHARD needs this. this cannot be used for 
;anything. nothing can come before this or after this.....IMPORTANT!!!!
;don't get too smart and using this buffer for anything!!!!!!
;
		times 16 db 0
;
;end of bug fix buffer
;;
;;3.3 BUG FIX -SP------------------------------

;-----------------------------------------------------------------------------
; MSHARD.ASM - MSDOS 3.3 - 24/07/1987
;-----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0

;***
;	Title:	Disk
;	By:	Michael Hanson
;	C:	Copyright (C) 1985-1987 by Microsoft corp.
;	Date:	1/11/85
;
;		There is a bug in some versions of IBM's AT ROM BIOS
;		interupts are not disabled during read operations.
;
;	Use:	This program should be chained in line with the disk
;		interrupt 13h, it intercepts read calls to the hard disk
;		and handles them appropriately.  For other functions it
;		passes controll to OLD13, which should contain the
;		address of the AT ROM disk routine. The entry point for
;		this program is IBM_DISK_IO.
;

	; .286c		;Use 80286 non-protected mode

BIOSEG	equ	040h	;Segment for ROM BIOS Data
ROMSEG	equ	0F000h	;Segment of ROM

BAD_DISK equ	01

HF_PORT	equ	01F0h
HF_REG_PORT equ	03F6h

;*	Offsets into Fixed disk parameter table
FDP_PRECOMP	equ 5
FDP_CONTROL	equ 8

;DATA	SEGMENT AT BIOSEG	;ROM BIOS data segment

	;ORG 42h
;CMD_BLOCK	DB 6 DUP (?)

CMD_BLOCK equ 42h ; 6 bytes, hard disk controller command bytes	

;*	Offsets into CMD_BLOCK for registers
PRE_COMP equ 0	;Write Pre-compensation
SEC_CNT	 equ 1	;Sector count
SEC_NUM	 equ 2	;Sector number
CYL_LOW	 equ 3	;Cylinder number, low part
CYL_HIGH equ 4	;Cylinder number, high part
DRV_HEAD equ 5	;Drive/Head (Bit 7 = ECC mode, Bit 5 = 512 byte sectors, 
		;            Bit 4 = drive number, Bits 3-0 have head number)
CMD_REG  equ 6	;Command register

	;ORG 074h

;DISK_STATUS1 	DB ?
;HF_NUM		DB ?
;CONTROL_BYTE	DB ?

DISK_STATUS1 equ 74h ; 1 byte
HF_NUM	     equ 75h ; 1 byte
CONTROL_BYTE equ 76h ; 1 byte 	  	

;DATA	ENDS

;***	Define where the ROM routines are actually located
;ROM	SEGMENT AT ROMSEG

	;ORG 02E1Eh
;ROMCOMMAND PROC FAR
;ROMCOMMAND ENDP

ROMCOMMAND equ 02E1Eh

	;ORG 02E7Fh
;ROMWAIT PROC FAR
;ROMWAIT ENDP

ROMWAIT	equ 02E7Fh

	;ORG 02EE2h
;ROMWAIT_DRQ PROC FAR
;ROMWAIT_DRQ ENDP

ROMWAIT_DRQ equ 02EE2h

	;ORG 02EF8h
;ROMCHECK_STATUS PROC FAR
;ROMCHECK_STATUS ENDP

ROMCHECK_STATUS equ 02EF8h

;	ORG 02F69h
;ROMCHECK_DMA PROC FAR
;ROMCHECK_DMA ENDP
 
ROMCHECK_DMA equ 02F69h

;	ORG 02F8Eh
;ROMGET_VEC PROC FAR
;ROMGET_VEC ENDP

ROMGET_VEC equ 2F8Eh	

;	ORG 0FF65h
;ROMFRET PROC FAR	;Far return at F000:FF65 in AT ROM.
;ROMFRET ENDP

ROMFRET	equ 0FF65h

;ROM	ENDS


;CODE	SEGMENT BYTE PUBLIC 'code'

;EXTRN	OLD13:DWORD		;Link to AT bios int 13h

;PUBLIC	IBM_DISK_IO	

;	ASSUME CS:CODE
;	ASSUME DS:DATA


;***	IBM_DISK_IO - main routine, fixes AT ROM bug
;
;	ENTRY:	(AH) = function, 02 or 0A for read.
;		(DL) = drive number (80h or 81h).
;		(DH) = head number.
;		(CH) = cylinder number.
;		(CL) = Sector number (high 2 bits has cylinder number).
;		(AL) = number of sectors.
;		(ES:BX) = address of read buffer.
;		For more on register contents see ROM BIOS listing.
;		Stack set up for return by an IRET.
;
;	EXIT:	(AH) = status of current operation.
;		(CY) = 1 IF failed, 0 if successful.
;		For other register contents see ROM BIOS listing.
;
;	USES:	
;
;
;	WARNING: Uses OLD13 vector for non-read calls.
;		Does direct calls to the AT ROM.
;		Does segment arithmatic.
;
;	EFFECTS: Performs DISK I/O operation.
;
;IBM_DISK_IO PROC FAR

IBM_DISK_IO:
		CMP	DL,80h
		JB	short ATD1	;Pass through floppy disk calls.
		CMP	AH,02h
		JE	short ATD2	;Intercept call 02 (read sectors).
		CMP	AH,0Ah
		JE	short ATD2	;and call 0Ah (read long).
ATD1:
		; 16/06/2018
		JMP	far [cs:OLD13]	;Use ROM INT 13h handler.
ATD2:
		PUSH	BX
		PUSH	CX
		PUSH	DX
		PUSH	DI
		PUSH	DS
		PUSH	ES
		PUSH	AX
		MOV	AX,BIOSEG	;Establish BIOS segment addressing.
		MOV	DS,AX
		MOV	byte [DISK_STATUS1],0 ;Initially no error code.
		AND	DL,07Fh		;Mask to hard disk number
		CMP	DL,[HF_NUM]
		JB	short ATD3	;Disk number in range
		MOV	byte [DISK_STATUS1],BAD_DISK
		JMP	SHORT ATD4	;Disk number out of range error, return

ATD3:
		PUSH	BX
		MOV	AX,ES		;Make ES:BX to Seg:000x form.
		SHR	BX,4
		ADD	AX,BX
		MOV	ES,AX
		POP	BX
		AND	BX,000Fh
		PUSH	CS
		CALL	CHECK_DMA
		JC	short ATD4	;Abort if DMA across segment boundary

		POP	AX		;Restore AX register for SETCMD
		PUSH	AX
		CALL	SETCMD		;Set up command block for disk op
		MOV	DX,HF_REG_PORT
		OUT	DX,AL		;Write out command modifier
		CALL	DOCMD		;Carry out command
ATD4:
		POP	AX
		MOV	AH,[DISK_STATUS1] ;On return AH has error code
		; 16/06/2018
		;STC
		;OR	AH,AH
		;JNZ	short ATD5	;Carry set if error
		;CLC
;ATD5:
		cmp	ah,1
		cmc	

		POP	ES
		POP	DS
		POP	DI
		POP	DX
		POP	CX
		POP	BX
		; 16/06/2018
		RETF	2		;Far return, dropping flags

;IBM_DISK_IO ENDP

;***	SETCMD - Set up CMD_BLOCK for the disk operation
;
;	ENTRY:	(DS) = BIOS Data segment.
;		(ES:BX) in seg:000x form.
;		Other registers as in INT 13h call
;	
;	EXIT:	CMD_BLOCK set up for disk read call.
;		CONTROL_BYTE set up for disk operation.
;		(AL) = Control byte modifier
;
;
;	Sets the fields of CMD_BLOCK using the register contents
;	and the contents of the disk parameter block for the given drive.
;
;	WARNING: (AX) destroyed.
;		Does direct calls to the AT ROM.
;

;SETCMD	PROC NEAR

SETCMD:
		MOV	[CMD_BLOCK+SEC_CNT],AL
		MOV	byte [CMD_BLOCK+CMD_REG],020h ;Assume function 02
		CMP	AH, 2
		JE	short SETC1	;CMD_REG = 20h if function 02 (read)
   					;CMD_REG = 22h if function 0A (" long)
		MOV	byte [CMD_BLOCK+CMD_REG],022h
SETC1:					;No longer need value in AX
		MOV	AL,CL
		AND	AL,03Fh		;Mask to sector number
		MOV 	[CMD_BLOCK+SEC_NUM],AL
		MOV	[CMD_BLOCK+CYL_LOW],CH
		MOV	AL,CL
		SHR	AL,6		;Get two high bits of cylinder number
		MOV	[CMD_BLOCK+CYL_HIGH],AL
		MOV	AX,DX
		SHL	AL,4		;Drive number
		AND	AH,0Fh
		OR	AL,AH		;Head number
		OR	AL,0A0h		;Set ECC and 512 bytes per sector
		MOV	[CMD_BLOCK+DRV_HEAD],AL
		PUSH	ES		;GET_VEC destroys ES:BX
		PUSH	BX
		PUSH	CS
		CALL	GET_VEC
					;Write pre-comp from disk parameters
		MOV	AX,[ES:BX+FDP_PRECOMP]
		SHR	AX,2
		MOV	[CMD_BLOCK+PRE_COMP],AL	;Only use low part
		MOV	AL,[ES:BX+FDP_CONTROL]	;Control byte modifier
		POP	BX
		POP	ES
		MOV	AH,[CONTROL_BYTE]
		AND	AH,0C0h		;Keep disable retry bits
		OR	AH,AL
		MOV	[CONTROL_BYTE],AH
		RETN

;SETCMD	ENDP	

;***	DOCMD - Carry out READ operation to AT hard disk
;
;	ENTRY:	(ES:BX) = address for read in data.
;		CMD_BLOCK set up for disk read.
;
;	EXIT:	Buffer at (ES:BX) contains data read.
;		DISK_STATUS1 set to error code (0 if success).
;
;	
;
;	WARNING: (AX), (BL), (CX), (DX), (DI) destroyed.
;		No check is made for DMA boundary overrun.
;
;	EFFECTS: Programs disk controller.
;		Performs disk input.
;

;DOCMD	PROC NEAR
DOCMD:
		MOV	DI,BX	;(ES:DI) = data buffer addr.
		PUSH	CS
		CALL	COMMAND
		JNZ	short DOC3
DOC1:
		PUSH	CS
		CALL	_WAIT	;Wait for controller to complete read
		JNZ	short DOC3
		MOV	CX,100h	;256 words per sector
		MOV	DX,HF_PORT
		CLD		;String op goes up
		CLI		;Disable interrupts (BUG WAS FORGETTING THIS)
		REPZ	INSW	;Read in sector
		STI
		TEST	byte [CMD_BLOCK+CMD_REG],02h
		JZ	short DOC2 ;No ECC bytes to read.
		PUSH	CS
		CALL	WAIT_DRQ
		JC	short DOC3
		MOV	CX,4	;4 bytes of ECC
		MOV	DX,HF_PORT
		CLI
		REPZ	INSB	;Read in ECC
		STI
DOC2:
		PUSH	CS
		CALL	CHECK_STATUS
		JNZ	short DOC3 ;Operation failed
		DEC	byte  [CMD_BLOCK+SEC_CNT]	
		JNZ	short DOC1 ;Loop while more sectors to read
DOC3:
		RETN

;DOCMD	ENDP

;***	GET_VEC - Get pointer to hard disk parameters.
;
;	ENTRY:	(DL) = Low bit has hard disk number (0 or 1).
;
;	EXIT:	(ES:BX) = address of disk parameters table.
;
;	USES:	AX for segment computation.
;
;	Loads ES:BX from interrupt table in low memory, vector 46h (disk 0)
;	or 70h (disk 1).
;	
;	WARNING: (AX) destroyed.
;		This does a direct call to the AT ROM.
;

;GET_VEC PROC NEAR
GET_VEC:
		;PUSH	OFFSET ROMFRET
		push	ROMFRET
		;JMP	ROMGET_VEC
		jmp	ROMSEG:ROMGET_VEC
;GET_VEC ENDP

;***	COMMAND - Send contents of CMD_BLOCK to disk controller.
;
;	ENTRY:	Control_byte 
;		CMD_BLOCK - set up with values for hard disk controller.
;
;	EXIT:	DISK_STATUS1 = Error code.
;		NZ if error, ZR for no error.
;
;
;	WARNING: (AX), (CX), (DX) destroyed.
;		Does a direct call to the AT ROM.
;
;	EFFECTS: Programs disk controller.
;

;COMMAND PROC NEAR
COMMAND:
		;PUSH	OFFSET ROMFRET
		push	ROMFRET	
		;JMP	ROMCOMMAND
		jmp	ROMSEG:ROMCOMMAND
;COMMAND ENDP

;***	WAIT - Wait for disk interrupt
;
;	ENTRY:	Nothing.
;
;	EXIT:	DISK_STATUS1 = Error code.
;		NZ if error, ZR if no error.
;
;
;	WARNING: (AX), (BL), (CX) destroyed.
;		Does a direct call to the AT ROM.
;		
;	EFFECTS: Calls int 15h, function 9000h.
;

;WAIT	PROC NEAR
_WAIT:
		;PUSH	OFFSET ROMFRET 
		push	ROMFRET
		;JMP	ROMWAIT
		jmp	ROMSEG:ROMWAIT
;WAIT	ENDP

;***	WAIT_DRQ - Wait for data request.
;
;	ENTRY:	Nothing.
;
;	EXIT:	DISK_STATUS1 = Error code.
;		CY if error, NC if no error.
;
;
;	WARNING: (AL), (CX), (DX) destroyed.
;		Does a direct call to the AT ROM.
;

;WAIT_DRQ PROC NEAR
WAIT_DRQ:
		;PUSH	OFFSET ROMFRET 
		push	ROMFRET
		;JMP	ROMWAIT_DRQ
		jmp	ROMSEG:ROMWAIT_DRQ
;WAIT_DRQ ENDP

;***	CHECK_STATUS - Check hard disk status.
;
;	ENTRY:	Nothing.
;
;	EXIT:	DISK_STATUS1 = Error code.
;		NZ if error, ZR if no error.
;
;
;	WARNING: (AX), (CX), (DX) destroyed.
;		Does a direct call to the AT ROM.
;

;CHECK_STATUS PROC NEAR
CHECK_STATUS:
		;PUSH	OFFSET ROMFRET 
		push	ROMFRET
		;JMP	ROMCHECK_STATUS
		jmp	ROMSEG:ROMCHECK_STATUS
;CHECK_STATUS ENDP

;***	CHECK_DMA - check for DMA overrun 64k segment.
;
;	ENTRY:	(ES:BX) = addr. of memory buffer in seg:000x form.
;		CMD_BLOCK set up for operation.
;
;	EXIT:	DISK_STATUS1 - Error code.
;		CY if error, NC if no error.
;
;
;	WARNING: Does a direct call to the AT ROM.
;

;CHECK_DMA PROC NEAR
CHECK_DMA:
		;PUSH	OFFSET ROMFRET 
		push	ROMFRET
		;JMP	ROMCHECK_DMA
		jmp	ROMSEG:ROMCHECK_DMA	
;CHECK_DMA ENDP


;CODE	ENDS
;	END

;-----------------------------------------------------------------------------
; MSINIT.ASM - MSDOS 3.3 - 02/02/1988
;-----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0
; 19/03/2018 - Retro DOS v2.0

ENDATROM:

;CMOS Clock setting support routines used by MSCLOCK.		
;Warning!!! This code will be dynamically relocated by MSINIT.

Daycnt_To_Day:
;Entry: [DAYCNT] = number of days since 1-1-80
;Return: CH=centry in BCD, CL=year in BCD, DH=month in BCD, DL=day in BCD
									 
		push	word [cs:DAYCNT]	;save daycnt			 
		cmp	word [cs:DAYCNT], (365*20+(20/4)) 
						;# days from 1-1-1980 to 1-1-2000
		jae	short century20						 
		mov	byte [cs:base_century], 19					 
		mov	byte [cs:base_year], 80						 
		jmp	short years							 
century20:				;20th century			 
		mov	byte [cs:base_century], 20					 
		mov	byte [cs:base_year], 0						 
		sub	word [cs:DAYCNT], (365*20+(20/4)) ;adjust daycnt			 
years:									 
		xor	dx, dx							 
		mov	ax, [cs:DAYCNT]						 
		mov	bx, (366+365*3) 	;# of days in a Leap year block  
		div	bx			;AX = # of leap block, DX=daycnt 
		mov	[cs:DAYCNT], dx		;save daycnt left		 
;	or	ah, ah				;ax should be less than 256	 
;	jz	short OK1							 
;	jmp	Erroroccur						 
;OK1:									 
		mov	bl, 4							 
		mul	bl			;AX=# of years. Less than 100	 
		add	[cs:base_year], al	;So, ah = 0. Adjust year	 
		inc	word [cs:DAYCNT]	;set daycnt to 1 base		 
		cmp	word [cs:DAYCNT], 366	;daycnt=remainder of leap year bk
		jbe	short Leapyear		;within 366+355+355+355 days.	 
		inc	byte [cs:base_year]	;if daycnt <= 366, then leap year
		sub	word [cs:DAYCNT], 366	;else daycnt--, base_year++;	 
					;And next three years are normal 
		mov	cx, 3							 
Regularyear:								 
		cmp	word [cs:DAYCNT], 365	;for(i=1; i>3 or daycnt <=365;i++)
		jbe	short Yeardone		;{if (daycnt > 365)		
		inc	byte [cs:base_year]	;  { daycnt -= 365		
		sub	word [cs:DAYCNT], 365	;  }				
		loop	Regularyear		;}				
;	jmp	Erroroccur		 ;cannot come to here		
Leapyear:								
		mov	byte [cs:month_tab+1],29 ;leap year. change month table. 
Yeardone:								
		xor	bx, bx							
		xor	dx, dx							
		mov	ax, [cs:DAYCNT]						
		mov	si, month_tab					
		mov	cx, 12							
Months: 								
		inc	bl			;				
		;mov	dl, [si] ; 28/03/2018
		mov	dl, [cs:si]		;cmp daycnt for each month til fit
		cmp	ax, dx			;dh=0.				 
		jbe	short Month_done						 
		inc	si			;next month			 
		sub	ax, dx			;adjust daycnt			 
		loop	Months							 
;	jmp	Erroroccur						 
Month_done:								 
		mov	byte [cs:month_tab+1], 28 ;restore month table value	 
		mov	dl, bl							 
		mov	dh, [cs:base_year]						 
		mov	cl, [cs:base_century]	;al=day,dl=month,dh=year,cl=cntry
		call	word [cs:BinToBCD]	;To save 15 bytes, Bin_To_BCD proc
						;was rel from Daycnt_to_Day proc. 
;	call Bin_To_BCD 			;convert "day" to bcd             
		xchg	dl, al			;dl = bcd day, al = month	  
		call	word [cs:BinToBCD]					  
;	call Bin_To_BCD 						  
		xchg	dh, al			;dh = bcd month, al = year	  
		call	word [cs:BinToBCD]					  
;	call Bin_To_BCD 						  
		xchg	cl, al			;cl = bcd year, al = century	 
		call	word [cs:BinToBCD]					 
;	call Bin_To_BCD						 
		mov	ch, al			;ch = bcd century		 
		pop	word [cs:DAYCNT]	;restore original value 	 
		retn								 
						 
	;public	EndDaycntToDay						 
EndDaycntToDay:						 
									 
	;public	Bin_To_BCD						 
Bin_To_BCD:			; real time clock sup
;Convert a binary input in AL (less than 63h or 99 decimal)
;into a bcd value in AL.  AH destroyed. 			
		push	cx						
		xor	ah, ah						
		mov	cl, 10						
		div	cl			;al=high digit bcd, ah=low digit bcd
		mov	cl, 4						
		shl	al, cl			;mov the high digit to high nibble
		or	al, ah							
		pop	cx							
		retn								
					
	;Public	EndCMOSClockset 	;End of routines for CMOS clock 
EndCMOSClockSet:

		;evenb
align 2

; 02/06/2018 - Retro DOS v3.0

;									
; THE K09 REQUIRES ROUTINES FOR READING THE CLOCK BECAUSE OF THE SUSPEND/
; RESUME FACILITY. THE SYSTEM CLOCK NEEDS TO BE RESET AFTER RESUME.	 
;

;	ASSUME	ES:NOTHING									 
									 
; THE FOLLOWING ROUTINE IS EXECUTED AT RESUME TIME WHEN THE SYSTEM	 
; POWERED ON AFTER SUSPENSION. IT READS THE REAL TIME CLOCK AND 	 
; RESETS THE SYSTEM TIME AND DATE, AND THEN IRETS.			 
; Warning!!! This code will be dynamically relocated by MSINIT.

;INT6C	PROC	FAR							 

INT6C:
		PUSH	CS							 
		POP	DS							 
									 
	;ASSUME DS:CODE							 
									 
		POP	WORD [INT6C_RET_ADDR] ; POP OFF RETURN ADDRESS	 
		POP	WORD [INT6C_RET_ADDR+2]				 
		POPF								 
		CALL	READ_REAL_DATE		; GET THE DATE FROM THE CLOCK	 
		CLI								 
		MOV	[DAYCNT],SI		; UPDATE DOS COPY OF DATE 	 
		STI								 
		CALL	READ_REAL_TIME		; GET THE TIME FROM THE RTC	 
		CLI								 
		MOV	AH,01h			; COMMAND TO SET THE TIME
		INT	1Ah			; CALL ROM-BIOS TIME ROUTINE 
		STI								
		JMP	far [INT6C_RET_ADDR]	; LONG JUMP			
									
;INT6C	ENDP

	;INCLUDE READCLOCK.INC						
	;INCLUDE CLOCKSUB.INC								

; 26/03/2018 - READCLOCK.INC, MSDOS 3.3, 24/07/1987

;************************************************************************
;
;   read_real_date reads real-time clock for date and returns the number
;   of days elapsed since 1-1-80 in si
;
READ_REAL_DATE: 			;MJB002
		PUSH	AX
		PUSH	CX
		PUSH	DX
		XOR	AH,AH		; throw away clock roll over
		INT	1AH					
		POP	DX
		POP	CX
		POP	AX

		PUSH	AX
		PUSH	BX
		PUSH	CX
		PUSH	DX
		;mov	word [CS:DAYCNT2],1 ; REAL TIME CLOCK ERROR FLAG (+1 DAY)
		mov	word [DAYCNT2],1
		mov	AH,4		; read date function code       
		int	1Ah		; read real-time clock	     
		;jnc	short read_ok 	; jmp success
		;jmp	r_d_ret 	; jmp error
		jc	r_d_ret
read_ok:				; ******* get bcd values in binary *****
		mov	[BIN_DATE_TIME+0],ch  ; store as hex value
		mov	[BIN_DATE_TIME+1],cl  ; ...
		mov	[BIN_DATE_TIME+2],dh  ; ...
		mov	[BIN_DATE_TIME+3],dl  ; ...
		;mov	word [CS:DAYCNT2],2  ; READ OF R-T CLOCK SUCCESSFUL
		mov	word [DAYCNT2],2
		call	bcd_verify	; verify bcd values in range
		jc	short r_d_ret 	; jmp some value out of range
		;mov	word [CS:DAYCNT2],3  ; READ OF R-T CLOCK SUCCESSFUL
		mov	word [DAYCNT2],3
		call	date_verify	; verify date values in range
		jc	short r_d_ret 	; jmp some value out of range
		;mov	word [CS:DAYCNT2],0  ; VERIFY SUCCESSFUL		  
		mov	word [DAYCNT2],0
		call	in_bin		; convert date to binary
					; ******* years since 1-1-80 *********
		mov	al,[BIN_DATE_TIME+1]  ; get years into century
		cbw				     ;
		cmp	byte [BIN_DATE_TIME+0],20  ; 20th century?
		jnz	short century_19 ; jmp no
		add	ax,100		; add in a century
century_19:			;
		sub	ax,80		; subtract off 1-1-80
		mov	cl,4		; leap year every 4
		div	cl		; al= # leap year blocks, ah= remainder
		mov	bl,ah		; save odd years
		cbw			; zero ah
		mov	cx,366+3*365	; # of days in leap year blocks
		mul	cx		; dx:ax is result
		;mov	[CS:DAYCNT2],AX	; SAVE COUNT OF DAYS
		mov	[DAYCNT2],AX
		mov	al,bl		; get odd years count
		cbw			;
		or	ax,ax		; is ax= 0?
		jz	short leap_year	; jmp if none
		mov	cx,365		; days in year
		mul	cx		; dx:ax is result
		;add	[CS:DAYCNT2],AX ; ADD ON DAYS IN ODD YEARS
		add	[DAYCNT2],AX
		jmp	short leap_adjustment ; account for leap year
leap_year:			; possibly account for a leap day
		cmp	byte [BIN_DATE_TIME+2],2 ; is month february
		jbe	short no_leap_adjustment ; jan or feb. no leap day yet.
leap_adjustment:			; account for leap day
		;INC	word [CS:DAYCNT2] ; ...
		INC	word [DAYCNT2]
no_leap_adjustment:			; ******* get days of month *******
		mov	cl,[BIN_DATE_TIME+3] ; ...
		xor	ch,ch		;
		dec	cx		; because of offset from day 1, not day 0
		;add	[CS:DAYCNT2],CX	; ******* GET DAYS IN MONTHS PRECEEDING *****
		add	[DAYCNT2],CX
		mov	cl,[BIN_DATE_TIME+2] ; get month
		xor	ch,ch		;
		dec	cx		; january starts at offset 0
		shl	cx,1		; word offset
		mov	si,MONTH_TABLE	; beginning of month_table
		add	si,cx		; point into month table
		mov	ax,[si]		; get # days in previous months
		;add	[CS:DAYCNT2],AX ; ...
		add	[DAYCNT2],AX
r_d_ret:			;
		;MOV	SI,[CS:DAYCNT2] ; RESULT IN SI
		MOV	SI,[DAYCNT2]
		POP	DX
		POP	CX
		POP	BX
		POP	AX
		retn			;

r_t_retj:
		xor	cx,cx
		xor	dx,dx
		jmp	short r_t_ret
;
; Read_Real_Time reads the time from the RTC. on exit, it has the number of
; ticks (at 18.2 ticks per sec.) in CX:DX.
;
READ_REAL_TIME:
		mov	ah,2
		int	1AH
		jc	short r_t_retj
oktime:
		mov	[BIN_DATE_TIME],ch		; hours
		mov	[BIN_DATE_TIME+1],cl		; minutes
		mov	[BIN_DATE_TIME+2],dh		; seconds
		mov	byte [BIN_DATE_TIME+3],0	; unused for time
		call	bcd_verify
		jc	short r_t_retj
		call	time_verify
		jc	short r_t_retj
		call	in_bin
		mov	ch,[BIN_DATE_TIME]
		mov	cl,[BIN_DATE_TIME+1]
		mov	dh,[BIN_DATE_TIME+2]
		mov	dl,[BIN_DATE_TIME+3]
; get time in ticks in CX:DX
		;CALL	near [cs:TimeToTicks]
		CALL	word [TimeToTicks]
r_t_ret:
		retn

;
;   in_bin converts bin_date_time values from bcd to bin
;
in_bin: 			      ;
		mov	al,[BIN_DATE_TIME+0]  ; century or hours
		call	bcd_to_bin	      ; ...
		mov	[BIN_DATE_TIME+0],al  ;
		mov	al,[BIN_DATE_TIME+1]  ; years or minutes
		call	bcd_to_bin	      ; ...
		mov	[BIN_DATE_TIME+1],al  ;
		mov	al,[BIN_DATE_TIME+2]  ; months or seconds
		call	bcd_to_bin	      ; ...
		mov	[BIN_DATE_TIME+2],al  ;
		mov	al,[BIN_DATE_TIME+3]  ; days (not used for time)
		call	bcd_to_bin	      ; ...
		mov	[BIN_DATE_TIME+3],al  ;
		retn			      ;
;
;   bcd_to_bin converts two bcd nibbles in al (value <= 99.) to
;   a binary representation in al
;   ah is destroyed
;
bcd_to_bin:				;
		mov	ah,al		; copy bcd number to ah
		and	ax,0F00Fh	; clear unwanted nibbles
		mov	bl,al		; save units place
		xchg	ah,al		; 10's place to al
		xor	ah,ah		; ah not wanted
		mov	cl,4		; shift count
		shr	ax,cl		;mjb004 swap nibbles
		mov	cl,10		; convert al to ...
		mul	cl		; ... its binary value
		add	al,bl		; add in units
		retn

; 26/03/2018 - CLOCKSUB.INC, MSDOS 3.3, 24/07/1987					

;
;   date_verify loosely checks bcd date values to be in range in bin_date_time
;
date_verify:				    	    ;
		cmp	byte [BIN_DATE_TIME+0],20h  ; century check
		ja	short date_error	    ; jmp error
		jz	short century_20	    ; jmp in 20th century
		cmp	byte [BIN_DATE_TIME+0],19h  ; century check
		jb	short date_error	    ; jmp error
		cmp	byte [BIN_DATE_TIME+1],80h  ; year check
		jb	short date_error	    ; jmp error
century_20:				    	    ;
		cmp	byte [BIN_DATE_TIME+1],99h  ; year check
		ja	short date_error	    ; jmp error
		cmp	byte [BIN_DATE_TIME+2],12h  ; month check
		ja	short date_error	    ; jmp error
		cmp	byte [BIN_DATE_TIME+2],00h  ; month check
		jbe	short date_error	    ; jmp error
		cmp	byte [BIN_DATE_TIME+3],31h  ; day check
		ja	short date_error	    ; jmp error
		cmp	byte [BIN_DATE_TIME+3],00h  ; day check
		jbe	short date_error	    ; jmp error
		clc				    ; set success flag
		ret				    ;
date_error:				   	    ;
		stc				    ; set error flag
		retn				    ;

;
; time_verify very loosely checks bcd date values to be in range in bin_date_time
;
time_verify:
		cmp	byte [BIN_DATE_TIME+0],24h
		ja	short time_error
		cmp	byte [BIN_DATE_TIME+1],59h
		ja	short time_error
		cmp	byte [BIN_DATE_TIME+2],59h
		ja	short time_error
		clc
		retn
time_error:
		stc
		retn

;
;   bcd_verify checks values in bin_date_time to be valid
;   bcd numerals.  carry set if any nibble out of range
;
bcd_verify:				  ;
		mov	cx,4		  ; 4 bytes to check
		mov	bx,BIN_DATE_TIME  ;
bv_loop:				  ;
		mov	al,[bx]		  ; get a bcd number (0..99)
		mov	ah,al		  ;
		and	ax,0F00Fh	  ; 10's place in high ah, 1's in al
		cmp	al,10		  ; is 1's place in range?
		ja	short bv_error	  ; jmp out of range
		shr	ah,1		  ; swap nibbles
		shr	ah,1		  ; ...
		shr	ah,1		  ; ...
		shr	ah,1		  ; ...
		and	ah,0fh		  ; get rid of any erroneous bits
		cmp	ah,10		  ; is 10's place in range
		ja	short bv_error	  ; jmp out of range
		inc	bx		  ; next byte
		dec	cx		  ;
		jnz	short bv_loop 	  ;
		clc			  ; set success flag
		retn			  ;
bv_error:				  ;
		stc			  ; set error flag
		retn			  ;

; 02/06/2018 - Retro DOS v3.0

ENDK09:

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

; DRVFAT must be the first location of freeable space!
	;EVENB

align 2

DRVFAT:	DW	0			; Drive and FAT ID of DOS
; 03/07/2018
;BIOS$:	DW	0			; First sector of data
;DOSCNT:DW	0			; How many sectors to read
fBigFAT: DB	0			; Flags for drive
; 01/07/2018
;FatLen:	DW	0		; number of sectors in FAT.
;FatLoc:	DW	0		; seg addr of fat sector

;;Rev 3.30 Modification -----------------------------------------------
; THE FOLLOWING TWO BYTES ARE USED TO SAVE INFO RETURNED BY INT 13, AH=8
; CALL TO DETERMINE DRIVE PARAMETERS.					
NUM_HEADS: DB	2			; NUMBER OF HEADS RETURNED BY ROM
SEC_TRK:   DB	9			; SEC/TRK RETURNED BY ROM	 
NUM_CYLN:  DB	40			; NUMBER OF CYLINDERS RET BY ROM 
									 
MODEL_BYTE: DB	0FFH			; MODEL BYTE. SET UP AT INIT TIME.
					; FF - PC-1, EXPANSION, OLD PC-2 
					; FE - NEWER PC-2 (64/256K PLANAR)
					; FD - 
					; FC - 
Secondary_Model_Byte: DB 0						 
;;End of Modification -----------------------------------------------

;BOOTBIAS equ	200H							 

	;EVENB
align 2
					 
DiskTable:  DW	512,	0100h,	64,	0
	    DW	2048,	0201h,	112,	0
	    DW	8192,	0402h,	256,	0
	    DW	32680,	0803h,	512,	0
	    DW	65535,	1004h,	1024,	0

DiskTable2: DW	32680,	0803h,	512,	0
	    DW	65535,	0402h,	512,	fBIG

;;Rev 3.30 Modification -----------------------------------------------

;*************************************************************************
;Variables for Mini disk initialization
;*************************************************************************
End_Of_BDSM:	dw	0		;offset value of the ending add
					;of BDSM table. Needed to figure
					;the Final_DOS_Location.
numh:		db	0		;number of hard files
mininum: 	db	0		;logical drive num for mini disk 
num_mini_dsk:	db	0		;# of mini disk installed	 
Rom_Minidsk_num: db	80h		;physical mini disk number	 
Mini_HDLIM:	dw	0						 
Mini_SECLIM:	dw	0						 
Mini_BPB_ptr:	dw	0		;temporary variable used to save 
					;Mini Disk BPB pt add in DskDrvs.
;;End of Modification -----------------------------------------------

Bios_Date:   DB	'01/10/84',0

; 02/06/2018 - Retro DOS v3.0

PatchTable:
		DW	10,Media_Patch
		DW	3,GETBP1_PATCH
		DW	3,SET_PATCH
		DW	3,DiskIO_PATCH
		DW	3,DSKERR
		DW	10,Changed_Patch
		DW	3,INIT_PATCH
		DW	0

;-----------------------------------------------------------------------------

; 28/03/2018
; MSDOS 6.0 - MSINIT.ASM, 1991
init_bootseg:	dw	0	; seg addr of buffer for reading boot record

BootBias equ 200h ; 19/03/2018

;-----------------------------------------------------------------------------

align 16

;-----------------------------------------------------------------------------
; entry point from boot sector
;-----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0

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
; 04/06/2018 - Retro DOS v3.0 (MSDOS 3.3)
;MEMORY_SIZE	     EQU 18  ; dw (paragraphs)	
;DEFAULT_DRIVE	     EQU 20  ; db
SYSI_COUNTRY	     EQU 18  ; dd (segment:offset) - country table pointer	
STACK_COUNT	     EQU 22  ; dw 	
STACK_SIZE	     EQU 24  ; dw
STACK_ADDR	     EQU 26  ; dd (segment:offset)  
MEMORY_SIZE	     EQU 30  ; dw (paragraphs)
DEFAULT_DRIVE	     EQU 32  ; db 	 						

;-----------------------------------------------------------------------------
; initialization - stage 1
;-----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0

		; Retro DOS 3.0 Modification on MSDOS 3.3 IBMBIO.COM code
		; (Because of Retro DOS 2.0, MSDOS 3.3 boot sector difference)

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
		; 17/06/2018 
		;mov	ds, bx
INIT0:
		push	es
		push	INIT1 ; 07/04/2018
		retf	; jump to 0070h:INIT1

;INIT:
INIT1:
; 02/06/2018 - Retro DOS v3.0

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

		;XOR	BX,BX
		;CLI		; turn interrupts off while manupulating stack
		;MOV	SS,BX	; set stack segment register
		;MOV	SP,700h	; set stack pointer
		;STI		; turn interrupts on

; 02/06/2018 - Retro DOS v3.0

;
; Entry from boot sector.  The register contents are:
;   DL = INT 13 drive number we booted from
;   CH = media byte
;   BX = First data sector on disk (0-based)
;
		CLI
		XOR	AX,AX
		MOV	DS,AX
;
; Preserve original int 13 vector
;   We need to save INT13 in two places in case we are running on an AT.
; On ATs we install the IBM supplied ROM_BIOS patch DISK.OBJ which hooks
; INT13 ahead of ORIG13.  Since INT19 must unhook INT13 to point to the
; ROM INT13 routine, we must have that ROM address also stored away.
;
		; 16/06/2018
		MOV	AX,[13h*4]
		MOV	[CS:OLD13],AX
		MOV	[CS:ORIG13],AX
		MOV	AX,[13h*4+2]
		MOV	[CS:OLD13+2],AX
		MOV	[CS:ORIG13+2],AX

;
; Set up INT 13 for new action
;
		MOV	WORD [13h*4],Block13
		MOV	[13h*4+2],CS
;
; Preserve original int 19 vector
;
		MOV	AX,[19h*4]
		MOV	[CS:ORIG19],AX	
		MOV	AX,[19h*4+2]
		MOV	[CS:ORIG19+2],AX
;
; Set up INT 19 for new action
;
		MOV	WORD[19h*4],INT19
		MOV	[19h*4+2],CS
		STI

		;; 07/04/2018
		;; Retro DOS v2.0
		;INT	11h		; rom-bios equipment determination
		;; 10/04/2018
		;TEST	AL,1
		;JNZ	SHORT CHECKSINGLEFD
		;XOR	AX,AX
		;JMP	SHORT NOFD
;CHECKSINGLEFD:
		;MOV	CL, 6
		;SHR	AL, CL
		;JNZ	short NOTSINGLE	;ZERO MEANS SINGLE DRIVE SYSTEM
		;INC	byte [Single]	;REMEMBER THIS
;NOTSINGLE:
		;XOR	AH,AH
		;INC	AL ; 10/04/2018

		;; 02/06/2018
		;; Retro DOS v3.0  (MSDOS 3.3, MSINIT.ASM, 02/02/1988)

		INT	11h		; rom-bios equipment determination
		ROL	AL,1		;PUT BITS 6 & 7 INTO BITS 0 & 1
		ROL	AL,1
		AND	AX,3		;ONLY LOOK AT BITS 0 & 1
		JNZ	short NOTSINGLE	;ZERO MEANS SINGLE DRIVE SYSTEM
		INC	AX		;PRETEND IT'S A TWO DRIVE SYSTEM
		INC	byte [CS:Single] ;REMEMBER THIS
NOTSINGLE:
		INC	AX		;AX HAS NUMBER OF DRIVES, 2-4
					;IS ALSO 0 INDEXED BOOT DRIVE IF WE
					;  BOOTED OFF HARD FILE
		mov	CL,AL		; save number of diskette drives in CL
		test	DL,80h		; booted from hard disk ?
		jnz	short GotHrd	; yes, jump down
		xor	AX,AX		; no - indicate boot from drive A
GotHrd:		
		; At this point the registers contain these values:
		;   AX = 0-based drive we booted from
		;   BX = the logical number of the first data sector on the disk
		;   CL = number of floppies including logical one
		;   CH = media byte
		;
;
; set up local stack
;
		mov	ch, dh	; 01/07/2018

		xor	DX,DX
		cli			; turn interrupts off while manupulating stack
		mov	SS,DX		; set stack segment register
		mov	SP,700h 	; set stack pointer
		sti			; turn interrupts on
	
					; preserve some of the values in registers
		push	CX ; (***)	; save number of floppies and media byte
		; 03/07/2018
		; 16/06/2018
		;mov	[CS:BIOS$],BX	; save first data sector
		mov	AH,CH		; FAT ID to AH too
		push	AX ; (**)	; save boot drive number and media byte

;;Rev 3.30 Modification -----------------------------------------------
; Let Model_byte, Secondary_Model_Byte be set here!!!	
		mov	ah,0C0h 	; return system environment 
		int	15h		; call ROM-Bios routine     
		jc	short No_Rom_System_Conf ; just use Model_Byte	
		cmp	ah,0		; double check		
		jne	short No_Rom_System_Conf
			 		;get the model byte				
		mov	al,[ES:BX+ROMBIOS_DESC.bios_SD_modelbyte]
		mov	[CS:MODEL_BYTE],al
					 ;secondary model byte					
		mov	al, [ES:BX+ROMBIOS_DESC.bios_SD_scnd_modelbyte]
		mov	[CS:Secondary_Model_Byte],al				
		jmp	short Turn_Timer_On					
No_Rom_System_Conf:							
		MOV	SI,0FFFFH	;MJB001 			
		MOV	ES,SI		;MJB001 			
		MOV	AL,[ES:0EH]	; GET MODEL BYTE ARR 2.41	
		MOV	[CS:MODEL_BYTE],AL ; SAVE MODEL BYTE ARR 2.41	
;;End of Modification -----------------------------------------------
	
Turn_Timer_On:								
		mov	AL,EOI
		out	AKPORT,AL	; turn on the timer

		; 02/06/2018

;;Rev 3.30 Modification -----------------------------------------------
		mov	si,COM4DEV 
		call	AUX_INIT
		mov	si,COM3DEV
		call	AUX_INIT
;;End of Modification -----------------------------------------------
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

		xor	AX,AX
		mov	DI,INITSPOT
		stosw				;INIT four bytes to 0
		stosw

		mov	AX,CS			;FETCH SEGMENT

		mov	word [BRKADR],CBREAK	;BREAK ENTRY POINT
		mov	[BRKADR+2],AX		;VECTOR FOR BREAK

		mov	word [CHROUT*4],OUTCHR
		mov	[CHROUT*4+2],AX

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

		mov	[500H],DX		;SET PRINT SCREEN & BREAK =0
		mov	[LSTDRV],DX		;clean out last drive spec

		mov	SI,[DSKADR]
		mov	DS,[DSKADR+2]		; DS:SI -> current table

		mov	DI,SEC9 		; ES:DI -> New Table
		mov	CX,DISK_PARMS.size ; 11
		rep	MOVSB			; Copy Table
		push	ES			;
		pop	DS			; DS = 0
							
		mov	word [DSKADR],SEC9 	; Point disk parm vector to new table	
		mov	[DSKADR+2],DS 

;-----------------------------------------------
;
; THE FOLLOWING DEPEND ON THE TYPE OF MACHINE.
;
		; 16/06/2018
		CMP	byte [CS:MODEL_BYTE],0FDH ; IS THIS AN OLD ROM?
		JB	short NO_DIDDLE		; NO
		MOV	WORD [SEC9+DISK_PARMS.DISK_HEAD_STTL],0200H+NORMSETTLE
						; SET HEAD SETTLE AND MOTOR START
						; ON PC-1 PC-2 PC-XT HAL0
		MOV	byte [SEC9+DISK_PARMS.DISK_SPECIFY_1],0DFH 
						; SET 1ST SPECIFY BYTE
						; ON PC-1 PC-2 PC-XT HAL0
NO_DIDDLE:
		int	12h			; call rom-bios for memory size
		mov	CL,6			; get ready for shift
		shl	AX,CL			; change from K to 16 byte blocks
		pop	CX ; (**)		; restore CX
		; 03/07/2018
		push	cs
		pop	ds
		mov	[DRVFAT],cx
		; 16/06/2018
		;mov	[CS:DRVFAT],CX
		;push	AX ; (*)
		; 03/07/2018
		mov	dx, ax ; (*)		

		; 02/06/2018
		;mov	dx,[7C00h + 16h]	; number of sectors/fat from boot sec
		
		;01/07/2018
		;mov	dx,[600h+16h]		; ((Retro DOS v2.0/v3.0 modification))
		
		;xor	dh,dh
		; 17/06/2018
		;push	cs
		;pop	ds
		; 01/07/2018
		;mov	[FatLen],dx
		; 16/06/2018
		;mov	[CS:FatLen],DX
;
; Convert sector count to paragraph count:512 bytes / sec / 16 bytes / para
; = 32 para /sector
;
;;Rev 3.30 Modification -----------------------------------------------
		; 01/07/2018
		;SHL	DX,1
		;SHL	DX,1
		;SHL	DX,1
		;SHL	DX,1
		;SHL	DX,1
;;End of Modification -----------------------------------------------
		;SUB	AX,DX			; room for FAT
		; 16/06/2018
		;MOV	[CS:FatLoc],AX		; location to read fat
		; 01/07/2018
		;; 17/06/2018
		;mov	[FatLoc],ax

		; 01/07/2018

		; 08/04/2018
		; 28/03/2018
		; MSDOS 6.0 - MSINIT.ASM, 1991
		sub	ax,64		;room for boot record buffer segment (1 kb)
		mov	[init_bootseg],ax

		;POP	AX ; (*)
		; 03/07/2018
		;pop	dx ; (*)

		;; 02/06/2018
		;MOV	word [SYSINIT_START+DEVICE_LIST],CONHeader
		;MOV	[SYSINIT_START+DEVICE_LIST+2],CS
		; 05/07/2018
		MOV	word [SYSINIT+DEVICE_LIST],CONHeader
		MOV	[SYSINIT+DEVICE_LIST+2],CS

		;; 06/04/2018
		;;MOV	[SYSINIT_START+MEMORY_SIZE],AX ; Top of memory (in paragraphs)
		;mov	[SYSINIT_START+MEMORY_SIZE],dx ; 01/07/2018
		;INC	CL
		;MOV	[SYSINIT_START+DEFAULT_DRIVE],CL ; SAVE DEFAULT DRIVE SPEC

		; 05/07/2018
		;MOV	[SYSINIT+MEMORY_SIZE],AX
		mov	[SYSINIT+MEMORY_SIZE],dx
		INC	CL
		MOV	[SYSINIT+DEFAULT_DRIVE],CL
		
		; 02/06/2018
		;mov	dx,ax

		; 08/04/2018
		; 28/03/2018
		; MSDOS 6.0 - MSINIT.ASM, 1991
		;sub	ax,64		;room for boot record buffer segment (1 kb)
		;mov	[init_bootseg],ax

		mov	ax,MSDOS_BIN_SEGMENT  ; MSDOS_BIN_OFFSET >> 4
		add	ax,KERNEL_SEGMENT ; 26/03/2018
		;MOV	[SYSINIT_START+CURRENT_DOS_LOCATION],AX
		;05/07/2018
		MOV	[SYSINIT+CURRENT_DOS_LOCATION],AX
		
; 02/06/2018
;
; IMPORTANT: Some old IBM hardware generates spurious INT F's due to bogus
; printer cards.  We initialize this value to point to an IRET ONLY IF
;
; 1) The original segment points to storage inside valid RAM.
;
; 2) The original segment is 0F000:xxxx
;
; Theses are capricious requests from our OEM for reasons behind them, read
; the DCR's for the IBM DOS 3.2 project.
;
		; 03/07/2018
		;push	ax
		
		; 02/06/2018 - Retro DOS v3.0
		;push	ds
		;pop	es

		xor	ax,ax
		mov	ds,ax

		mov	ax,[0Fh*4+2]		; segment for Int 15

		;;cmp	ax,[es:SYSINIT_START+MEMORY_SIZE] ; Condition 1
		; 05/07/2018
		;cmp	ax,[es:SYSINIT+MEMORY_SIZE]
		cmp	ax,dx
		jna	short ResetIntF

		cmp	ax,0F000h		; Condition 2
		jne	short KeepIntF
ResetIntF:
		mov	word [0Fh*4],INTRET
		mov	[0Fh*4+2],cs
KeepIntF:
		;pop	ax
;
; END IMPORTANT
;

; 02/06/2018 - Retro DOS v3.0

;**************************************************************
;	WILL INITIALIZE THE NUMBER OF DRIVES		
;	AFTER THE EQUIPMENT CALL (INT 11H) BITS 6&7 WILL TELL	
;	THE INDICATIONS ARE AS FOLLOWS: 		
;							
;	BITS	7	6	DRIVES			
;		0	0	1			
;		0	1	2			
;		1	0	3			
;		1	1	4			
;************************************************************** 
		PUSH	CS
		POP	DS
		PUSH	CS
		POP	ES

		call	CMOS_CLOCK_READ  ;Before doing anything if CMOS clock,
				 ;then set the system time accordingly.
				 ;Also, reset the cmos clock rate.

		XOR	SI,SI
		MOV	word [SI],HARDDRV ;set up pointer to hdrive

		POP	AX ; (***) ;number of floppies and FAT ID
		XOR	AH,AH	; Chuck FAT ID byte
		MOV	[HARDNUM],AL ;Remember which drive is hard disk
		MOV	[DRVMAX],AL  ;And set initial number of drives
		SHL	AX,1	;Two bytes per address
		MOV	DI,DSKDRVS
		ADD	DI,AX	;Point to hardfile location
		MOV	SI,HDSKTAB
		MOVSW		;Two addresses to move
		MOVSW
		mov	DL,80h	; tell rom bios to look at hard drives
		mov	AH,08h	; set command to get drive parameter
		int	13h	; call ROM-BIOS to get number of drives
		jc	short ENDDRV ; old, rom therefore no hard disks
		mov	[HNUM],DL ; save number of hard drives in HNUM
		; ES:DI = Address of Harddisk Parameter Table ; ((*))
ENDDRV:
;
; Scan the list of drives to determine their type.  We have three flavors of
; diskette drives:
;
;   48tpi drives    We do nothing special for them
;   96tpi drives    Mark the fact that they have changeline support.
;   3 1/2 drives    Mark changeline support and small.
;
; The following code uses registers for certain values:
;   DL - Physical Drive
;   DS:DI - points to current BDS
;   CX - Flag bits for BDS
;   DH - Form Factor for the drive (1 - 48tpi, 2 - 96tpi, 3 - 3.5" medium)
;
		XOR	DL,DL	; start out with drive 0.
		; 17/06/2018
		;push	cs
		;pop	ds
		MOV	byte [EOT],9
		mov	di,START_BDS
LOOP_DRIVE:
		cmp	dl,[DRVMAX]
		jb	short GOT_MORE
		jmp	DONE_DRIVES
GOT_MORE:
		xor	cx,cx		; zero all flags
		;mov	di,[DI+BDS.Link] ; get next BDS
		mov	di,[di] ; 05/07/2019
		mov	dh,ff48tpi	; Set Form Factor to 48 tpi
		MOV	byte [NUM_CYLN],40 ; 40 TRACKS PER SIDE
								
		;PUSH	DS						
		PUSH	DI						
		PUSH	DX						
		PUSH	CX
		; 17/06/2018						
		;PUSH	ES						
		push	cs ; ((*))		
						
		MOV	AH,08h		;GET DRIVE PARAMETERS
		INT	13h		;CALL ROM-BIOS		
		;JNC	short PARMSFROMROM					
		;JMP	short NOPARMSFROMROM ; GOT AN OLD ROM
		jc	short NOPARMSFROMROM ; 03/06/2018
		; ES:DI = Address of Harddisk Parameter Table ; ((*))	
PARMSFROMROM:							
;If CMOS is bad, it gives ES,AX,BX,CX,DH,DI=0. CY=0.	 
;In this case, we are going to put bogus informations to BDS table.
;We are going to set CH=39,CL=9,DH=1 to avoid divide overflow when	
;they are calculated at the later time.  This is just for the Diagnostic
;Diskette which need IO.SYS,MSDOS to boot up before it sets CMOS.	
;This should only happen with drive B.	

;;Rev 3.30 Modification -----------------------------------------------
		CMP	CH,0	; if ch=0, then cl,dh=0 too.	 
		JNE	short PFR_OK							 
		MOV	CH,39	; ROM gave wrong info.		 
		MOV	CL,9	; Let's default to 360K.         
		MOV	DH,1							 
PFR_OK: 								 
		INC	DH	; MAKE NUMBER OF HEADS 1-BASED	 
		INC	CH	; MAKE NUMBER OF CYLINDERS 1-BASED
		MOV	[NUM_HEADS],DH	; SAVE PARMS RETURNED BY ROM	 
		AND	CL,00111111B	; EXTRACT SECTORS/TRACK 	 
		MOV	[SEC_TRK],CL						 
		MOV	[NUM_CYLN],CH	; ASSUME LESS THAN 256 CYLINDERS!! 
; MAKE SURE THAT EOT CONTAINS THE MAX NUM OF SEC/TRK IN SYSTEM OF FLOPPIES
		CMP	CL,[EOT]	; MAY SET CARRY 		 
		JBE	short EOT_OK							
		MOV	[EOT],CL						
EOT_OK: 							
		POP	ES ; ((*)) es = cs = ds						
		POP	CX						
		POP	DX						
		POP	DI						
		;POP	DS						
;
; Check for presence of changeline
;
		mov	AH,15h      	; set command to get DASD type
		int	13h	     	; call ROM-BIOS
		JC	short CHANGELINE_DONE 	
		CMP	AH,2		; CHECK FOR PRESENCE OF CHANGELINE
		JNE	short CHANGELINE_DONE 					 
;;End of Modification -----------------------------------------------

;
; We have a drive with change line support.
;
		or	CL,fChangeline	; signal type
		mov	byte [fHave96],1 ; Remember that we have 96tpi disks
;									  
; WE NOW TRY TO SET UP THE FORM FACTOR FOR THE TYPES OF MEDIA THAT WE KNOW
; AND CAN RECOGNISE. FOR THE REST, WE SET THE FORM FACTOR AS "OTHER".     
;									  
CHANGELINE_DONE:							  
; 40 CYLINDERS AND 9 OR LESS SEC/TRK, TREAT AS 48 TPI MEDIUM.		  
		CMP	byte [NUM_CYLN],40						  
		JNZ	short TRY_80							  
		CMP	byte [SEC_TRK],9						  
		;JBE	short GOT_FF
		jbe	short NEXTDRIVE ; 03/06/2018							  
GOTOTHER:								  
		MOV	DH,ffOther	; WE HAVE A "STRANGE" MEDIUM     
		;JMP	SHORT GOT_FF
		jmp	short NEXTDRIVE	; 03/06/2018				  

;									  
; 80 CYLINDERS AND 9 SECTORS/TRACK => 720 KB DEVICE			  
; 80 CYLINDERS AND 15 SEC/TRK => 96 TPI MEDIUM				  
;									  
TRY_80: 
	; 03/06/2018 - Retro DOS v3.0 - MSDOS 6.0 (MSINIT.ASM, 1991) modifications
	
		CMP	byte [NUM_CYLN],80						
		JNZ	short GOTOTHER

		; 03/06/2018 (MSDOS 6.0, MSINIT.ASM, 1991)
		mov	dh,ff288	   ; assume 2.88 MB drive m037
		cmp	byte [SEC_TRK],36  ; is it ?		  m037
		je	short NEXTDRIVE	   ; yeah, go update	  m037

		CMP	byte [SEC_TRK],15						
		JZ	short GOT96							
		CMP	byte [SEC_TRK],9						
		JNZ	short GOTOTHER	 ; (1.44MB, 3.5" diskette drive)
		MOV	DH,ffSmall
		;JMP	SHORT GOT_FF
		jmp	short NEXTDRIVE
GOT96:	
		MOV	DH,ff96tpi
;GOT_FF: 
		JMP	SHORT NEXTDRIVE 

; WE HAVE AN OLD ROM, SO WE EITHER HAVE A 48TPI OR 96TPI DRIVE. IF THE DRIVE
; HAS CHANGELINE, WE ASSUME IT IS A 96TPI, OTHERWISE IT IS A 48TPI.

NOPARMSFROMROM:
		POP	ES ; ; ((*))
		POP	CX
		POP	DX
		POP	DI
		;POP	DS

		MOV	AH,15h		; SET COMMAND TO GET DASD TYPE
		INT	13h		; CALL ROM-BIOS
		JC	short NEXTDRIVE
		CMP	AH,2		; IS THERE CHANGELINE?
		JNZ	short NEXTDRIVE
		OR	CL,fChangeline
		MOV	byte [fHave96],1 ; REMEMBER WE HAVE 96TPI DRIVES
		MOV	byte [NUM_CYLN],80
		MOV	DH,ff96tpi
		MOV	AL,15		; SET EOT IF NECESSARY
		CMP	AL,[EOT]
		JBE	short EOT_OK2
		MOV	[EOT],AL
EOT_OK2:
NEXTDRIVE:
		or	cl,fI_Own_Physical ; set this true for all drives
		mov	bh,dl	 	;save Int13 drive number

; we need to do special things if we have a single drive system and are setting
; up a logical drive. it needs to have the same int13 drive number as its
; counterpart, but the next drive letter. also reset ownership flag.
; we detect the presence of this situation by examining the flag single for the
; value 2.

		cmp	byte [Single],2
		jnz	short NOT_SPECIAL
		dec	bh	; int13 drive number same for logical drive
	 			; reset ownership flag for logical drive
		xor	cl,fI_Own_Physical
NOT_SPECIAL:
; The values that we put in for RHdlim and RSeclim will only remain if the
; form factor is of type "ffOther".
		
		xor	ax,ax			; fill BDS for drive
		mov	al,[NUM_HEADS]
		mov	[di+BDS.RHDLIM],ax
		mov	al,[SEC_TRK]
		mov	[di+BDS.RSECLIM],ax
		mov	[di+BDS.Flags],cx
		mov	[di+BDS.FormFactor],dh
		mov	[di+BDS.DriveLet],dl
		mov	[di+BDS.DriveNum],bh
		mov	bl,[NUM_CYLN]
		mov	[di+BDS.cCyln],bl	; only the l.s. byte is set here
		cmp	byte [Single],1		; Special case for single drive system
		jnz	short NO_SINGLE
						; Don't forget we have
		mov	byte [Single],2		;   single drive system
		or	cx,fI_Am_Mult		; set that this is one of
						;   several drives
		or	[di+BDS.Flags],cx	; save flags
		;mov	di,[di+BDS.Link]	; move to next BDS in list
		mov	di,[di] ; 05/07/2019
		inc	dl			; add a number
		jmp	short NEXTDRIVE		; Use same info for BDS as previous
NO_SINGLE:
		inc	dl
		jmp	LOOP_DRIVE	

DONE_DRIVES:
		;mov	ax,-1			; Signify end of list by
		;mov	[di+BDS.Link],ax	;    setting pointer to -1
		;mov	word [di+BDS.Link],-1 ; 03/07/2018
		mov	word [di],-1 ; 05/07/2019
;
; Set up all the hard drives in the system
;
		; 03/06/2018	
DoHard:
		cmp	byte [HNUM], 0		; IF (No_Hard_files)
		jle	short STATIC_CONFIGURE 	;    THEN EXIT TO CONFIGURE
		mov	DL,80h			; set first hard file number
		mov	di,BDSH		    	; Set up first hard file.
		mov	bl,[HARDNUM]
		call	SETHARD
		jnc	short HardFile1_OK
		dec	byte [HNUM]		; First hard file is bad.
		cmp	byte [HNUM],0		; IF (Second_Hard_File)
		jg	short Second_Hard	;   THEN Set up second hard file
		JMP	SHORT STATIC_CONFIGURE	
	
HardFile1_OK:
		call	INSTALL_BDS	    	; install BDS into linked list
		cmp	byte [HNUM],2		; IF (Only_one_hardfile)
		jb	short SetIt	    	;    THEN SetIt "in place"

		mov	bl,[HARDNUM]
		inc	BL			; next drive letter
		mov	di,BDSX
Second_Hard:					; SETUP Second Hard FILE
		mov	DL,81h			; set second hard file number
		call	SETHARD
		jnc	short HardFile2_OK
		dec	byte [HNUM]
		jmp	short SetIt

HardFile2_OK:
		call	INSTALL_BDS
SetIt:
		mov	al,[HNUM]
		or	al,al
		jz	short STATIC_CONFIGURE
		add	al,[HARDNUM]
		mov	[DRVMAX],al

; End of physical drive initialization.	
; *** Do not change the position of the following statement.
; *** DoMini routine will use [DRVMAX] value for the start of the logical
; *** drive number of Mini disk(s).

		call    DoMini		;For setting up mini disks, if found

; End of drive initialization.

; 03/06/2018 - Retro DOS v3.0

;9/24/86 We now decide, based on the configurations available so far,
;what code or data we need to keep as a stay resident code. The following
;table shows the configurations under consideration. They are listed in  
;the order of their current position memory.				  
;Configuration will be done in two ways:				  
;First, we are going to set "Static configuration".  Static configuration 
;will consider from basic configuration to ENDOF96TPI configuration.	  
;The result of static configuration will be the address the Dynamic	  
;configuration will use to start with.					  
;Secondly, "Dynamic cofiguration" will be performed. Dynamic configuration
;involves possible relocation of CODE/DATA. Dynamic configuration routine 
;will take care of BDSM tables and AT ROM Fix module thru K09 suspend/res 
;code individually. After these operation, FINAL_DOS_LOCATION will be set.
;This will be the place SYSINIT routine will relocate MSDOS module.	  
;									  
;   1.	 BASIC CONFIGURATION FOR MSBIO (EndFloppy, EndSwap)		  
;   2.	 ENDONEHARD							  
;   3.	 ENDTWOHARD							  
;   4.	 END96TPI	;a system that supports "Change Line Error"       
;   5.	 End of BDSM	;BDSM tables for mini disks.			  
;   6.	 ENDATROM	;Some of AT ROM fix module.			  
;   7.	 ENDCMOSCLOCKSET;Supporting program for CMOS clock write.	  
;   8.	 ENDK09 	;K09 CMOS Clock module to handle SUSPEND/RESUME   
;									  
;9/24/86.								  
									  
; *** For mini disk configuration. 4/7/86	 			  
; *** END_OF_BDSM will contain the ending address(off) of BDSM table for  
; *** mini disks which is located right after the label END96TPI.	  
; *** The variable NUM_MINI_DSK will indicate the existance. 4/7/86	  
									  
STATIC_CONFIGURE:							  
		; 05/07/2019						  
		;PUSH	AX ; !@	
							  
		mov	ax,End96tpi		;let's start with the biggest one.
		cmp	byte [fHave96],0	;Is change line support there?	  
		jnz	short Config96		;Yes.				 
									  
		mov	ax,EndTwoHard					
		cmp	byte [HNUM],1 		;1 hard file?			
		jbe	short No_Two_HRD						 
		jmp	short ConfigTwoHard						
No_Two_HRD:								
		mov	ax,EndOneHard					
		jnz	short Basic_Floppy						
		jmp	short ConfigOneHard						
Basic_Floppy:								
		mov	ax,EndFloppy					 
		jmp	short DYNAMIC_CONFIGURE	;static configuration is done!	
;
; Keep the 96tpi code
;
Config96:
;
; Save old INT 13 vector
;
		PUSH	AX
		PUSH	DS
		XOR	AX,AX
		MOV	DS,AX
		MOV	AX,[4*13h]
		MOV	[CS:REAL13],AX
		MOV	AX,[4*13h+2]
		MOV	[CS:REAL13+2],AX
;
; Insert new vector
;
		MOV	word [4*13h],INT13
		MOV	[4*13h+2],CS
		POP	DS
		POP	AX
;
; Keep two hard disk BPBs
;
ConfigTwoHard:
;
; Keep one hard disk BPB
;
ConfigOneHard:
;
; Adjust the number of drives to include the hard disks.
;
		PUSH	AX
		MOV	AL,[HARDNUM]
		ADD	AL,[HNUM]
		add	al,[num_mini_dsk] ;4/7/86 for mini disks installed	
				;if not installed, then num_mini_dsk = 0.
		MOV	[DRVMAX],AL
		POP	AX

DYNAMIC_CONFIGURE:
		call	Get_Para_Offset ;For dynamic allocation, we are
					;going to use offset address that
					;is in paragraph boundary.
		; 17/06/2018 (es=cs=ds)
		;push	cs
		;pop	es		;es -> code
		
		cld			;clear direction

		cmp	byte [num_mini_dsk], 0	;Mini disk(s) installed ?
		jz	short CheckATROM	;No.
		; 03/07/2018
		mov	ax,[End_Of_BDSM]	;set the new ending address
		call	Get_Para_Offset 	
CheckATROM:				
		cmp	byte [MODEL_BYTE],0FCh	;AT ?
		jnz	short CheckCMOSClock				
		cmp	byte [HNUM],0 		;No hard file?	
		jz	short CheckCMOSClock			
		mov	si,0F000h					
		mov	es,si			;ES -> BIOS segment
		mov	si,Bios_Date			
		mov	di,0FFF5H	;ROM BIOS string is at F000:FFF5
Cmpbyte:				;Only patch ROM for bios 01/10/84
		cmpsb							
		jnz	short CheckCMOSClock					
		cmp	byte [si-1],0				
		jnz	short Cmpbyte 					
SetRomCode:				;Now we have to install ROM fix
					;AX is the address to move.
		push	cs						
		pop	es		;set ES to CODE seg	
		mov	[ORIG13],ax				
		mov	[ORIG13+2],cs	;set new ROM bios int 13 vector
		mov	cx,ENDATROM				
		mov	si,IBM_DISK_IO				
		sub	cx,si		;size of AT ROM FIX module
		mov	di,ax		;destination	
		rep	movsb		;relocate it		
		mov	ax,di		;new ending address
		call	Get_Para_Offset ;in AX			
CheckCMOSClock: 						
		push	cs						
		pop	es		 ;set ES to CODE seg	
		cmp	byte [HaveCMOSClock],1 ;CMOS Clock exists?	
		jne	short CheckK09					
		mov	[DaycntToDay],ax ;set the address for MSCLOCK
		mov	cx,EndDaycntToDay			
		mov	si,Daycnt_To_Day				 
		sub	cx,si		;size of CMOS clock sub routine  
		mov	di,ax							  
		rep	movsb							
		mov	ax,di						
		call	Get_Para_Offset 					 
		mov	[BinToBCD],ax	;set the address for MSCLOCK
		mov	cx,EndCMOSClockSet				
		mov	si,Bin_To_BCD				
		sub	cx,si							
		mov	di,ax							
		rep	movsb							
		mov	ax,di							
		call	Get_Para_Offset 					
CheckK09:								
		push	ax		;save ax	     		
		mov	ax,4100h	;Q: is it a K09 	     
		mov	bl,0		;
		;xor	bl,bl			  
		int	15h		;			   
		pop	ax						
		jc	short ConfigDone					
								
		mov	si,INT6C				
		mov	cx,ENDK09					
		sub	cx,si		;size of K09 routine	
		mov	di,ax							
		push	di		;save destination		
		rep	movsb							
		mov	ax,di					
		call	Get_Para_Offset ;AX = new ending address	
		pop	di							
								
		push	ax							 
		push	ds							
		mov	byte [fHaveK09],1 ;remember we have a K09 type	
		xor	ax,ax							
		mov	ds,ax							
		mov	[4*6Ch],di	;new INT 6Ch handler	
		mov	[4*6Ch+2],cs					
		pop	ds							
		pop	ax		;restore the ending address	
;
; Set up config stuff for SYSINIT
;
ConfigDone:
		; 03/06/2018 - Retro DOS v3.0	
		;; 07/04/2018
		;MOV	AX,END_OF_HD2P_TBL
		; 06/04/2018
		;MOV	AX,INIT
		ADD	AX,15
		;SHR	AX,1
		RCR	AX,1
		SHR	AX,1
		SHR	AX,1
		SHR	AX,1
		ADD	AX,KERNEL_SEGMENT
		;MOV	[SYSINIT_START+FINAL_DOS_LOCATION],AX
		; 05/07/2018
		MOV	[SYSINIT+FINAL_DOS_LOCATION],AX

		; 05/07/2019
		;POP	AX ; !@

		; 03/06/2018
		; 19/03/2018
		mov	cx,msdos_bin_size+1
		shr	cx,1
		;mov	[SYSINIT_START+DOSSIZE],cx ; DOS size in words
		; 05/07/2018
		mov	[SYSINIT+DOSSIZE],cx

		; 03/06/2018
		CMP	BYTE [fHave96],0
		JNZ	SHORT ReadDos
		call	PURGE_96TPI	;mjb001 eliminate calls to 96tpi hoohah

ReadDos:
		; 06/07/2019

		; 19/03/2018 (*) No need to read remain clusters of
		; MSDOS kernel because of Retro DOS v2.0 boot sector
		; has loaded all of the kernel file before.
		; ;("MSINIT.ASM" contains kernel file reading code here...)  

		; 30/03/2018
		;mov	ax,[DRVFAT]
		;call	SETDRIVE
		;call	GETBP
		
		; 24/02/2018 - Retro DOS 2.0 - MSDOS 3.3 ("MSINIT.ASM")

		CALL	SETDRVPARMS
		
		; 06/07/2018
		
		; 03/06/2018 - Retro DOS v3.0
		;JMP	SYSINIT
		; 05/07/2018
		jmp	_SYSINIT ; GOINIT

;-----------------------------------------------------------------------------
; 03/06/2018 - Retro DOS v3.0

Get_Para_Offset:
;in:  AX - offset value
;out: AX - offset value adjusted for the next paragraph boundary.
		add	ax, 15		;make a paragraph
		rcr	ax, 1	
		shr	ax, 1	
		shr	ax, 1
		shr	ax, 1
		shl	ax, 1		;now, make it back to offset value
		shl	ax, 1
		shl	ax, 1
		shl	ax, 1
		retn

; 01/07/2018
;;
;;	READ A FAT SECTOR INTO fat location
;;
;GETFAT:
;		XOR	DI,DI			; offset
;		MOV	DX,1			; relative sector (1st sector of fat)
;		MOV	CX,[FatLen]		; read entire fat.
;		MOV	AX,[FatLoc]		;
;		MOV	ES,AX			; location to read
;		MOV	AX,[DRVFAT]		; AH FAT ID byte, AL drive
;		JMP	DISKRD


; 02/06/2018 - Retro DOS v3.0	
; 19/03/2018 - Retro DOS v2.0

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
		cmp	WORD [ES:BootBias+1FEH],0AA55H	; DAVE LITTON MAGIC WORD?
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
		; 03/06/2018 - Retro DOS v3.0
		; (20/03/2018 - Retro DOS v2.0)

		push	di
		push	bx
		push	ds
		mov	byte [di+BDS.DriveLet],bl
		mov	byte [di+BDS.DriveNum],dl
		xor	ax,ax
		or	al,fNon_Removable
		or	[di+BDS.Flags],ax
		mov	byte [di+BDS.FormFactor],ffHardFile
		mov	byte [fBigFAT],0	; Assume 12 bit FAT
		PUSH	DX

		mov	AH,8		; set command to get drive parameters
		int	13h		; call rom-bios disk routine

				; DH is number of heads-1
				; DL is number of hard disks attached
				; Low 6 bits of CL is sectors/track
				; High 2 bits of CL with CH are max # of cylinders
		INC	DH		; get number of heads
		MOV	[DI+BDS.HDLIM],DH
		POP	DX
		JC	short SETRET	; carry here means no hard disk
		AND	CL,3FH		; extract number of sectors/track
		MOV	[DI+BDS.SECLIM],CL
		CALL	GETBOOT		;   if (getBoot ())
		JC	short SETRET	;	return -1;
		MOV	BX,1C2H+BootBias ;   p = &boot[0x1C2];
SET1:
		CMP	BYTE [ES:BX],1	;   while (p->PartitionType != 1 &&
		JZ	short SET2
		CMP	Byte [ES:BX],4	;	p->PartitionType != 4) {
		JZ	short SET2
		ADD	BX,16		;	p += sizeof Partition;
		CMP	BX,202H+BootBias ;	if (p == &boot[0x202h])
		JNZ	short SET1	;	    return -1;
SETRET:
		STC			;	}
		jmp	Ret_Hard

; --- install a mini disk. BX value saved ---

SetMini:								 
		push	di							 
		push	bx							 
		push	ds							 
		jmp	short SET1	;will be returned to Find mini partition routine.
					;Some logic has been added to SET1 to 
					;deal with Mini disks.
SET2:
		PUSH	DX
		MOV	AX,[ES:BX+4]
		MOV	DX,[ES:BX+6]

;Decrement the sector count by 1 to make it zero based. Exactly 64k
;sectors should be allowed	 
;				 
		SUB	AX,1		; PTM 901    12/12/86 MT
		SBB	DX,0		; PTM 901    12/12/86 MT

		ADD	AX,[ES:BX+8]
		ADC	DX,[ES:BX+10]
		JZ	short OKDrive
		OR	byte [fBigFAT],fTOOBIG
OKDrive:
		POP	DX
		MOV	AX,[ES:BX+4]
		MOV	[DI+BDS.HIDSEC],AX	; BPB->HidSecCt = p->PartitionBegin;
		MOV	AX,[ES:BX+8]
		CMP	AX,64		;  if (p->PartitionLength < 64)
		JB	short SETRET	;	return -1;

		MOV	[DI+BDS.DRVLIM],AX ; BPB->MaxSec = p->PartitionLength;
		PUSH	AX

		PUSH	DX
		MOV	AX,[DI+BDS.HIDSEC]	; boot sector number
		XOR	DX,DX
		MOV	BH,DH
		MOV	BL,[DI+BDS.SECLIM]
		DIV	BX
		MOV	CL,DL			; CL is sector number
		INC	CL			; sectors are 1 based
		CWD
		MOV	BL,[DI+BDS.HDLIM]
		DIV	BX			; DL is head, AX is cylinder
;
; DL is head.
; AX is cylinder
; CL is sector number
; TOS is drive
;
;
		; 20/03/2018
;*** For Mini Disks ***  4/7/86 
		cmp	word [di+BDSM.isMini], 1 ;check for mini disk - 4/7/86
		jnz	short OKnotMini		;not mini disk. - 4/7/86
		add	ax, [di+BDSM.Hidden_Trks] ;set phy track num - 4/7/86
OKnotMini:				; 4/7/86
;*** End of added logic for mini disk 

		ROR	AH,1			; move high two bits of cyl to high
		ROR	AH,1			; two bits of upper byte
		AND	AH,0C0h 		; turn off remainder of bits
		OR	CL,AH			; move two bits to correct spot
		MOV	CH,AL			; CH is Cylinder
;
; CL is sector + 2 high bits of cylinder
; CH is low 8 bits of cylinder
; DL is head
; TOS is drive
;
		POP	AX			; AL is drive
		MOV	DH,DL			; DH is head
		MOV	DL,AL			; DL is drive
;
; CL is sector + 2 high bits of cylinder
; CH is low 8 bits of cylinder
; DH is head
; DL is drive
;
		xor	BX, BX			; clear BX -- ES:BX points to buffer
		mov	ax, 0201h		; set command to read one sector
		int	13h			; call rom-bios to read sector
		pop	AX

;
; ES:[0] points to the boot sector.  In theory, (ha ha) the BPB in this thing
; is correct.  We can, therefore, pull out all the relevant statistics on the
; media if we recognize the version number.
;
		CMP	WORD [ES:3], "IB"  ; "B" SHL 8 + "I"
		JNZ	short unknownj
		CMP	WORD [ES:5], "M "  ; " " SHL 8 + "M"
		JNZ	short unknownj
		CMP	WORD [ES:8], "2."  ; "." SHL 8 + "2"
		JNZ	short Try5
		CMP	BYTE [ES:10], "0"
		JNZ	short Try5
		JMP	SHORT CopyBPB
unknownj:
		jmp	short unknown
Try5:
		CMP	WORD [ES:8],"3."  ; "." SHL 8 + "3"
		JNZ	short unknownj
		cmp	byte [es:10],"1"    ;do not trust 3.0 boot record.  4/15/86
		jb	short unknownj	    ;if version >= 3.1, then O.K.  4/15/86

CopyBPB:
; We have a valid Boot sector. Use the BPB in it to build the
; BPB in BIOS. It is assumed that ONLY SecPerClus, cDIR, and
; cSecFat need to be set (all other values in already). fBigFat
; is also set.
		MOV	AX,[ES:11+BDS.DRVLIM-BDS.BytePerSec] ; Total sectors
		DEC	AX			; Subtract # reserved (always 1)
		MOV	DX,[ES:11+BDS.cSecFat-BDS.BytePerSec] ; Sectors for 1 fat
		MOV	[DI+BDS.cSecFat],DX 	; Set in BIOS BPB
		SHL	DX,1			; Always 2 FATs
		SUB	AX,DX			; Sub # FAT sectors
		MOV	DX,[ES:11+BDS.cDir-BDS.BytePerSec] ; # root entries
		MOV	[DI+BDS.cDir],DX	; Set in BIOS BPB
		MOV	CL,4
		SHR	DX,CL			; Div by 16 ents/sector
		SUB	AX,DX			; Sub # dir sectors
		; AX now contains the # of data sectors.
		MOV	CL,[ES:11+BDS.SecPerClus-BDS.BytePerSec] ; Sectors per cluster
		MOV	[DI+BDS.SecPerClus],CL	; Set in BIOS BPB
		XOR	DX,DX
		MOV	CH,DH
		DIV	CX
		; AX now contains the # clusters.
		CMP	AX,4096-10		; is this 16-bit fat?
		JB	short GoodRetj		; No
		OR	byte [fBigFAT],fBIG	; 16 bit FAT
GoodRetj:
		JMP	short GoodRet

unknown:
		MOV	SI,DiskTable2
Scan:
		CMP	AX,[SI]
		JBE	short GotParm
		ADD	SI,4*2
		JMP	short Scan
GotParm:
		MOV	CL,[SI+6]
		OR	[fBigFAT],CL
		MOV	CX,[SI+2]
		MOV	DX,[SI+4]
;
;	AX = number of sectors on disk drive
;	DX = number of dir entries,
;	CH = number of sectors per cluster
;	CL = log base 2 of ch
;
;	NOW CALCULATE SIZE OF FAT TABLE
;
		MOV	[DI+BDS.cDir],DX	;SAVE NUMBER OF DIR ENTRIES
		MOV	[DI+BDS.SecPerClus],CH  ;SAVE SECTORS PER CLUSTER
		TEST	byte [fBigFAT],fBIG	;   if (fBigFat)
		JNZ	short DoBig	;	goto DoBig;
		XOR	BX,BX
		MOV	BL,CH
		DEC	BX
		ADD	BX,AX
		SHR	BX,CL			;   BX = 1+(BPB->MaxSec+SecPerClus-1)/
		INC	BX			;	    SecPerClus
		AND	BL,11111110b		;   BX &= ~1; (=number of clusters)
		MOV	SI,BX
		SHR	BX,1
		ADD	BX,SI
		ADD	BX,511			;   BX += 511 + BX/2
		SHR	BH,1			;   BH >>= 1; (=BX/512)
		MOV	BYTE [DI+BDS.cSecFat],BH ;SAVE NUMBER OF FAT SECTORS
		JMP	SHORT GoodRet	 
DoBig:
		MOV	CL,4			; 16 (2^4) directory entries per sector
		SHR	DX,CL			; cSecDir = cDir / 16;
		SUB	AX,DX			; AX -= cSecDir; AX -= cSecReserved;
		DEC	AX			; ax = t - r - d
		MOV	BL,2
		MOV	BH,[DI+BDS.SecPerClus]	; bx = 256 * secperclus + 2
		XOR	DX,DX
		ADD	AX,BX			; ax = t-r-d+256*spc+2
		ADC	DX,0
		SUB	AX,1			; ax = t-r-d+256*spc+1
		SBB	DX,0
		DIV	BX			; cSecFat = ceil((total-dir-res)/
						;		 (256*secperclus+2));
		MOV	[DI+BDS.cSecFat],AX	; number of fat sectors
GoodRet:
		MOV	BL,[fBigFAT]
		MOV	[DI+BDS.FatSiz],BL	; set size of fat on media
		CLC
Ret_Hard:
		pop	ds
		pop	bx
		pop	di
		RETN	

; 03/08/2019
; (Note: Following offset may change when SYSINIT code will be changed.)
;BPB_Table equ SYSINIT_START + 1D46h  ; (BPBTable offset in sysinit2.s)
;BPBSIZ	  equ 25  ; Retro DOS v3.0 (02/06/2018)	

; 22/11/2022
BPB_Table equ SYSINIT_START + 1D3Eh  ; (BPBTable offset in sysinit2.s -2022-)
BPBSIZ	  equ 25  ; Retro DOS v3.0 (02/06/2018)	

;
; SetDrvParms sets up the recommended BPB in each BDS in the system based on
; the form factor. It is assumed that the BPBs for the various form factors
; are present in the BPBTable. For hard files, the Recommended BPB is the same
; as the BPB on the drive.
; No attempt is made to preserve registers since we are going to jump to
; SYSINIT straight after this routine.
;

SETDRVPARMS:
		xor	bx,bx
		;les	di,dword ptr [cs:START_BDS] ; get first BDS in list
		les	di,[cs:START_BDS] ; 03/07/2018
NextBDS:
		cmp	di,-1
		jnz	short Do_SetP
Done_SetParms:
		RETN
Do_SetP:
		push	es
		push	di			; preserve pointer to BDS
		;mov	bl,es:[di].FormFactor
		mov	bl,[es:di+BDS.FormFactor]
		cmp	bl,ffHardFile
		jnz	short NotHardFF
		;mov	ax,es:[di].DrvLim
		mov	ax,[es:di+BDS.DRVLIM]
		push	ax
		;mov	ax,wo"rd ptr es:[di].hdlim
		mov	ax,[es:di+BDS.HDLIM]
		;mul	word ptr es:[di].seclim
		mul	word [es:di+BDS.SECLIM]
		mov	cx,ax			; cx has # sectors per cylinder
		pop	ax
		xor	dx,dx			; set up for div
		div	cx			; div #sec by sec/cyl to get # cyl
		or	dx,dx
		jz	short No_Cyl_Rnd	; came out even
		inc	ax			; round up
No_Cyl_Rnd:
		;mov	es:[di].cCyln,ax
		mov	[es:di+BDS.cCyln],ax
		push	es
		pop	ds
		;lea	si,[di].BytePerSec	; ds:si -> BPB for hard file
		lea	si,[di+BDS.BytePerSec]

		jmp	short Set_RecBPB
NotHardFF:
		push	cs
		pop	ds
		cmp	bl,ffOther		; Special case "other" type of medium
		JNZ	short NOT_PROCESS_OTHER
Process_Other:
		xor	dx,dx
		;mov	ax,[di].cCyln
		;mov	bx,[di].RHdlim
		mov	ax,[di+BDS.cCyln]
		mov	bx,[di+BDS.RHDLIM]
		mul	bx
		;mov	bx,[di].RSeclim
		mov	bx,[di+BDS.RSECLIM]
		mul	bx
		;mov	[di].RDrvlim,ax 	; Have the total number of sectors
		mov	[di+BDS.RDRVLIM],ax
		dec	ax

; New logic to get the sectors/fat area.				  
						;Fat entry assumed to be 1.5 bytes
		mov	bx,3
		mul	bx
		mov	bx,2
		div	bx
		xor	dx,dx
		mov	bx,512
		div	bx
		inc	ax
No_Round_Up:
		;mov	[di].RcSecFat,ax
		mov	[di+BDS.RcSecFat],ax
		jmp	short Go_To_Next_BDS
NOT_PROCESS_OTHER:
		shl	bx,1			; bx is word index into table of BPBs
		;mov	si,offset BPBTable
		; 06/07/2018
		mov	si,BPB_Table  ; Retro DOS v3.0 modification (06/07/2018)
				      ; (FD BPB Tables are in SYINIT section)
		;mov	si,word ptr [si+bx]	; get address of BPB
		mov	si,[si+bx]

		; 28/08/2023 (BugFix)
		add	si,SYSINIT_START
Set_RecBPB:
		;lea	di,[di].RBytePerSec	; es:di -> RecBPB
		lea	di,[di+BDS.RBytePerSec]
		mov	cx,BPBSIZ
		REP	MOVSB			; MOVE BPBSIZ BYTES
Go_To_Next_BDS:
		pop	di
		pop	es			; restore pointer to BDS
		;mov	bx,word ptr es:[di].link+2
		;mov	di,word ptr es:[di].link
		mov	bx,[es:di+BDS.Link+2]
		;mov	di,[es:di+BDS.Link]
		mov	di,[es:di] ; 05/07/2019
		mov	es,bx
		jmp	NextBDS

; ----------------------------------------------------------------------------
; 03/06/2018 - Retro DOS v3.0


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
		; 16/06/2018
		MOV	AL,[CS:SI+13]	;GET DEVICE NUMBER FROM THE NAME
		;MOV	AL,[SI+13]
		SUB	AL,"1"
		CBW
		MOV	DX,AX
		RETN

; ----------------------------------------------------------------------------

;
;   purge_96tpi NOP's calls to 96tpi support.
;
PURGE_96TPI:
		PUSH	DS
		PUSH	ES

		push	cs			;mjb001
		pop	es			;mjb001
		push	cs			;mjb001
		pop	ds			;mjb001
		
		MOV	SI,PatchTable
PatchLoop:
		LODSW
		MOV	CX,AX
		JCXZ	PatchDone
		LODSW
		MOV	DI,AX
		MOV	AL,90h
		REP	STOSB
		JMP	short PatchLoop

PatchDone:
		mov	di,TABLE_PATCH		;ARR 2.42
		MOV	AX,EXIT
		STOSW
		STOSW

		POP	ES
		POP	DS
		retn				;mjb001

; ----------------------------------------------------------------------------
; 03/06/2018 - Retro DOS v3.0
; (19/03/2018 - Retro DOS v2.0)

;Mini disk initialization routine. Called right after DoHard - 4/7/86
; DoMini **************************************************************** 
; **CS=DS=ES=code							
; **DoMini will search for every extended partition in the system, and
;   initialize it.							
; **BDSM stands for BDS table for Mini disk and located right after the
;   label End96Tpi. End_Of_BDSM will have the offset value of the ending
;   address of BDSM table.						 
; **BDSM is the same as usual BDS except that TIM_LO, TIM_HI entries are
;   overlapped and used to id mini disk and the number of Hidden_trks.
;   Right now, they are called as IsMini, Hidden_Trks respectively.
; **DoMini will use the same routine in SETHARD routine after label SET1
;   to save coding.							
; **DRVMAX determined in DoHard routine will be used for the next
;   available logical mini disk drive number.	
;								
; Input: DRVMAX, DSKDRVS					
;								
; Output: MiniDisk installed. BDSM table established and installed to BDS.
;	  num_mini_dsk - number of mini disks installed in the system.
;	  End_Of_BDSM - ending offset address of BDSM.		
;								
;									
; Called modules:						
;		  GetBoot, WRMSG, int 13h (AH=8, Rom)		
;		  FIND_MINI_PARTITION (new), Install_BDSM (new),
;		  SetMini (new, it will use SET1 routine)	
; Variables used: End_Of_BDSM, numh, mininum, num_mini_dsk,	
;		  Rom_Minidsk_num, Mini_HDLIM, Mini_SECLIM		
;		  BDSMs, BDSM_type (struc), START_BDS			
;***********************************************************************
;

DoMini:
		;push	ax			;Do I need to do this?

		mov	di,BDSMs		;from now on, DI points to BDSM
		mov	dl,80h 			;look at first hard drive
		mov	ah,08h			;get drive parameters
		int	13h			;call ROM-Bios
		cmp	dl, 0
		jz	short DoMiniRet		;no hard file? Then exit.
		mov	[numh],dl		;save the number of hard files.
		xor	ax,ax
		mov	al,[DRVMAX]
		mov	[mininum],al		;this will be logical drive letter
						;for mini disk to start with.

		shl	ax,1			;ax=number of devices. word bndry
		push	bx
		mov	bx,DSKDRVS
		add	bx,ax
		mov	[Mini_BPB_ptr], BX	;Mini_BPB_ptr points to first avlb
						;spot in DskDrvs for Mini disk
						;which points to BPB area of BDSM.
		pop	bx

		mov	byte [Rom_Minidsk_num],80h
DoMiniBegin:
		inc	dh			;Get # of heads (conv to 1 based)
		xor	ax,ax
		mov	al,dh
		mov	[Mini_HDLIM],ax		;save it.
		xor	ax,ax
		and	cl,3Fh	 		;Get # of sectors/track
		mov	al,cl
		mov	[Mini_SECLIM],ax 	;and save it.

		mov	dl,[Rom_Minidsk_num]	;drive number <DL>
		call	GETBOOT 		;rd master boot rec 7c0:BootBias
		jc	short DoMiniNext
		call	FIND_MINI_PARTITION
DoMiniNext:
		dec	byte [numh]
		jz	short DoMiniRet
		inc	byte [Rom_Minidsk_num] 	;Next hard file
		mov	dl,[Rom_Minidsk_num]	;look at next hard drive
		mov	ah,08h			;get drive parameters
		int	13h			;call ROM-Bios
		jmp	short DoMiniBegin

DoMiniRet:
		;pop	ax
FmpRet: 			; 19/03/2018 -  Retro DOS v2.0
		retn

;Find_Mini_Partition tries to find every Extended partition on a disk.
;At entry:	DI -> BDSM entry
;		ES:BX -> 07C0:BootBias - Master Boot Record
;		Rom_Minidsk_num - ROM drive number
;		MiniNum - Logical drive number
;		Mini_HDLIM, Mini_SECLIM 
;
;Called routine: SETMINI which uses SET1 (in SETHARD routine)
;Variables & equates used from orig BIOS - flags, fNon_Removable, fBigfat
;

FIND_MINI_PARTITION:

		add	bx,1C2h			;BX -> system id.
FmpNext:
		; 05/07/2019 - BugFix
		cmp	byte [ES:BX],5		; 5 = extended partition ID.
		je	short FmpGot
		add	bx,16			; for next entry
		cmp	bx,202h+BootBias ; 402h
		jnz	short FmpNext
		;jmp	short FmpRet		;not found extended partition
		retn
FmpGot:
		xor	ax,ax
		or	al,fNon_Removable
		or	[DI+BDS.Flags],ax
		mov	byte [DI+BDS.FormFactor],ffHardFile
		mov	byte [fBigFAT],0	;assume 12 bit Fat.
		mov	ax,[Mini_HDLIM]
		mov	[DI+BDS.HDLIM],ax
		mov	ax,[Mini_SECLIM]
		mov	[DI+BDS.SECLIM],ax
		mov	al,[Rom_Minidsk_num]
		mov	[DI+BDS.DriveNum],al	;set physical number
		mov	al,[mininum]
		mov	[DI+BDS.DriveLet],al	;set logical number

		cmp	word [ES:BX+8],64 ;**With current BPB, only lower word	
					; is meaningful.			
		je	short FmpRet	;should be bigger than 64 sectors at least
		sub	bx,4		;let BX point to the start of the entry 
		mov	dh,[ES:BX+2]						
		and	dh,11000000b		;get higher bits of cyl 
		rol	dh,1
		rol	dh,1	
		mov	dl,[ES:BX+3]		;cyl byte
		mov	[DI+BDSM.Hidden_Trks],dx ;set hidden trks
;** Now, read the volume boot record into BootBias.
		mov	cx,[ES:BX+2]		;cylinder,cylinder/sector
		mov	dh,[ES:BX+1]		;head
		mov	dl,[Rom_Minidsk_num]	;drive
		; 03/07/2018
		;mov	ax,7C0h 		;
		mov	ax,[init_bootseg]
		mov	es,ax			;buffer segment
		mov	bx,BootBias		;buffer offset
		mov	ax,0201h		;read,1 sector
		int	13h			;call ROM-Bios routine
		jc	short FmpRet		;cannot continue.
		mov	bx,1c2h+BootBias

		call	SetMini 		;install a mini disk. BX value saved.
		jc	short FmpnextChain

		call	INSTALL_BDSM		;install the BDSM into the BDS table
		inc	byte [mininum] 		;increase the logical drive number for next
		inc	byte [num_mini_dsk]	;increase the number of mini disk installed.

		push	bx			;now, set the DskDrvs pointer to BPB info.
		mov	bx,[Mini_BPB_ptr]
		;lea	si,[di].BytePerSec	;points to BPB of BDSM
		lea	si,[di+BDSM.mBytePerSec]
		mov	[bx],si
		inc	word [Mini_BPB_ptr]	;advance to the next address
		inc	word [Mini_BPB_ptr]
		pop	bx							 
								 
		;add	DI,type BDSM_type	;adjust to the next BDSM table entry. 
		add	DI,BDSM.size
		mov	[End_Of_BDSM],DI	;set the ending address of BDSM table to this. 

FmpnextChain:
		jmp	FmpNext		;let's find out if we have any chained partition
;FmpRet: 								 
;		retn

;SetMini:								 
;		push	di							 
;		push	bx							 
;		push	ds							 
;		jmp	short SET1	;will be returned to Find mini partition routine.
;					;Some logic has been added to SET1 to 
;					;deal with Mini disks.				  ;3.30
														  ;3.30
;									 
;Install BDSM installs a BDSM (pointed by DS:DI) into the end of the current 
;linked list of BDS.							 
;Also, set the current BDSM pointer segment to DS.			 
;At entry: DS:DI -> BDSM						 
;
									 
INSTALL_BDSM:								 
		push	ax							 
		push	si							 
		push	es							 
									 
		;les	si, dword ptr cs:START_BDS	;start of the beginning of list
		les	si, [START_BDS]
I_BDSM_Next:								 
		cmp	word [es:si], -1		;end of the list?	 
		jz	short I_BDSM_New						 
		;mov	si, [es:si+BDSM.mLink]
		mov	si, [es:si] ; 05/07/2019				 
		mov	ax, [es:si+BDSM.mLink+2]	;next pointer		 
		mov	es, ax							 
		jmp	short I_BDSM_Next					 
I_BDSM_New:								 
		mov	ax, ds							 
		mov	[di+BDSM.mLink+2], ax	;BDSM segment had not been initialized.
		mov	[es:si+BDSM.mLink+2], ax				 
		;mov	[es:si+BDSM.mLink], di
		mov	[es:si], di ; 05/07/2019			 
		;mov	word [di+BDSM.mLink], -1	;make sure it is a null ptr.
		mov	word [di], -1 ; 05/07/2019 
I_BDSM_ret:								 
		pop	es							 
		pop	si							 
		pop	ax							 
		retn								 
									 
;**End of mini disk initialization**	; 4/7/86

; ----------------------------------------------------------------------------
; 03/06/2018 - Retro DOS v3.0

CMOS_CLOCK_READ:
														  ;3.30
; IN ORDER TO DETERMINE IF THERE IS A CLOCK PRESENT IN THE SYSTEM, THE FOLLOWING 
; NEEDS TO BE DONE.							 
		PUSH	AX							 
		PUSH	CX							 
		PUSH	DX							 
		PUSH	BP							 
									 
		XOR	BP,BP							 
LOOP_CLOCK:								 
		XOR	CX,CX							 
		XOR	DX,DX							 
		MOV	AH,2			;READ REAL TIME CLOCK
		INT	1Ah			;CALL ROM-BIOS ROUTINE
		CMP	CX,0							 
		JNZ	short CLOCK_PRESENT						 
									 
		CMP	DX,0							 
		JNZ	short CLOCK_PRESENT						 
									 
		CMP	BP,1			; READ AGAIN AFTER A SLIGHT DELAY, IN CASE CLOCK
		JZ	short NO_READDATE	; WAS AT ZERO SETTING.		 
									 
		INC	BP			; ONLY PERFORM DELAY ONCE.	 
		MOV	CX,4000H						 
DELAY:									 
		LOOP	DELAY							 
		JMP	LOOP_CLOCK						 
									 
CLOCK_PRESENT:								 
		;mov	byte [cs:HaveCMOSClock], 1 ; Set the flag for cmos clock 
		mov	byte [HaveCMOSClock], 1
									 
		call	CMOSCK			; Reset CMOS clock rate that may be  
						; possibly destroyed by CP DOS and 
						; POST routine did not restore that.			 
 		PUSH	SI							 
		CALL	READ_REAL_DATE		;READ REAL-TIME CLOCK FOR DATE 
									 
		CLI			 
		MOV	[DAYCNT],SI		;SET SYSTEM DATE		 
		STI			 
		POP	SI			 
NO_READDATE:								 
		POP	BP							 
		POP	DX							 
		POP	CX							 
		POP	AX							 
		RETN								 
				 
;									 
; 10/28/86								 
; THE FOLLOWING CODE IS WRITTEN BY JACK GULLEY IN ENGINEERING GROUP. 
; CP DOS IS CHANGING CMOS CLOCK RATE FOR ITS OWN PURPOSES AND IF THE 
; USE COLD BOOT THE SYSTEM TO USE PC DOS WHILE RUNNING CP DOS, THE CMOS 
; CLOCK RATE ARE STILL SLOW WHICH SLOW DOWN DISK OPERATIONS OF PC DOS 
; WHICH USES CMOS CLOCK. PC DOS IS PUT THIS CODE IN MSINIT TO FIX THIS 
; PROBLEM AT THE REQUEST OF CP DOS. 				 
; THE PROGRAM IS MODIFIED TO BE RUN ON MSINIT. Equates are defined in CMOSEQU.INC. 
; This program will be called by CMOS_Clock_Read procedure. 	 
;									 
;  The following code CMOSCK is used to insure that the CMOS has not	 
;	had its rate controls left in an invalid state on older AT's.
;									 
;	It checks for an AT model byte "FC" with a submodel type of
;	00, 01, 02, 03 or 06 and resets the periodic interrupt rate	 
;	bits incase POST has not done it.  This initilization routine	 
;	is only needed once when DOS loads.  It should be ran as soon	 
;	as possible to prevent slow diskette access.			 
;									 
;	This code exposes one to DOS clearing CMOS setup done by a	 
;	resident program that hides and re-boots the system.		 
;									 
CMOSCK:					; CHECK AND RESET RTC RATE BITS 
									 
;Model byte and Submodel byte were already determined in MSINIT.	 
		push	ax

	; 16/06/2018 - Retro DOS v3.0

	; 19/03/2018 (Model: 0FCh, Sub Model: 01h, REF: AMIBIOS Prog. Guide)							 

	;cmp	cs:Model_byte, 0FCh ;check for PC-AT model byte	 
		cmp	byte [MODEL_BYTE],0FCh	
	;			 	; EXIT IF NOT "FC" FOR A PC-AT
		JNE	short CMOSCK9	; Exit if not an AT model	 
									 
	;CMP	cs:Secondary_Model_Byte,06H  ; Is it 06 for the industral AT 
		CMP	byte [Secondary_Model_Byte],06h
		JE	short CMOSCK4 	; Go reset CMOS periodic rate if 06 
	;CMP	cs:Secondary_Model_Byte,04H  ; Is it 00, 01, 02, or 03	 
		cmp	byte [Secondary_Model_Byte],04h
		JNB	short CMOSCK9 	; EXIT if problem fixed by POST  
					; Also,Secondary_model_byte = 0 
					;   when AH=0c0h, int 15h failed.
CMOSCK4:				;	RESET THE CMOS PERIODIC RATE 
					;  Model=FC submodel=00,01,02,03 or 06 

		;mov	al,CMOS_REG_A or NMI	;NMI disabled on return
		mov	al,CMOS_REG_A + NMI	
		mov	ah,00100110b		;Set divider & rate selection
		call	CMOS_WRITE

		;mov	al,CMOS_REG_B or NMI	;NMI disabled on return
		mov	al,CMOS_REG_B + NMI	
		call	CMOS_READ
		and	al,00000111b		;clear SET,PIE,AIE,UIE,SQWE
		mov	ah,al
		mov	al,CMOS_REG_B		;NMI enabled on return
		call	CMOS_WRITE

CMOSCK9:					; EXIT ROUTINE		 
		pop	ax							 
		RETN				; RETurn to caller		 
						;  Flags modifyied		 
							 
;--- CMOS_READ ----------------------------------------------------------------- 
;		READ BYTE FROM CMOS SYSTEM CLOCK CONFIGURATION TABLE	       :
;									       :
; INPUT: (AL)=	CMOS TABLE ADDRESS TO BE READ				       :
;		BIT    7 = 0 FOR NMI ENABLED AND 1 FOR NMI DISABLED ON EXIT    :
;		BITS 6-0 = ADDRESS OF TABLE LOCATION TO READ		       :
;									       :
; OUTPUT: (AL)	VALUE AT LOCATION (AL) MOVED INTO (AL).  IF BIT 7 OF (AL) WAS  :
;		ON THEN NMI LEFT DISABLED.  DURING THE CMOS READ BOTH NMI AND  :
;		NORMAL INTERRUPTS ARE DISABLED TO PROTECT CMOS DATA INTEGRITY. :
;		THE CMOS ADDRESS REGISTER IS POINTED TO A DEFAULT VALUE AND    :
;		THE INTERRUPT FLAG RESTORED TO THE ENTRY STATE ON RETURN.      :
;		ONLY THE (AL) REGISTER AND THE NMI STATE IS CHANGED.	       :
;------------------------------------------------------------------------------- 
									 
CMOS_READ:				; READ LOCATION (AL) INTO (AL) 
		PUSHF			; SAVE INTERRUPT ENABLE STATUS AND FLAGS 

		cli
		push	bx
		push	ax		;save user NMI state
		or	al,NMI		;disable NMI for us
		out	CMOS_PORT,al
		nop			;undocumented delay needed
		in	al,CMOS_DATA	;get data value

		 ;set NMI state to user specified 
		mov	bx,ax		;save data value
		pop	ax		;get user NMI
		and	al,NMI
		or	al,CMOS_SHUT_DOWN
		out	CMOS_PORT,al
		nop
		in	al,CMOS_DATA

		mov	ax,bx		;data value
		pop	bx

		PUSH	CS		; *PLACE CODE SEGMENT IN STACK AND 
		CALL	CMOS_POPF	; *HANDLE POPF FOR B- LEVEL 80286 
		RETN			; RETURN WITH FLAGS RESTORED	 
									 
CMOS_POPF:				; POPF FOR LEVEL B- PARTS  
		IRET			; RETURN FAR AND RESTORE FLAGS	 
									 
							 
;--- CMOS_WRITE ----------------------------------------------------------------
;		WRITE BYTE TO CMOS SYSTEM CLOCK CONFIGURATION TABLE	       :
;									       :
; INPUT: (AL)=	CMOS TABLE ADDRESS TO BE WRITTEN TO			       :
;		BIT    7 = 0 FOR NMI ENABLED AND 1 FOR NMI DISABLED ON EXIT    :
;		BITS 6-0 = ADDRESS OF TABLE LOCATION TO WRITE		       :
;	 (AH)=	NEW VALUE TO BE PLACED IN THE ADDRESSED TABLE LOCATION	       :
;									       :
; OUTPUT:	VALUE IN (AH) PLACED IN LOCATION (AL) WITH NMI LEFT DISABLED   :
;		IF BIT 7 OF (AL) IS ON.  DURING THE CMOS UPDATE BOTH NMI AND   :
;		NORMAL INTERRUPTS ARE DISABLED TO PROTECT CMOS DATA INTEGRITY. :
;		THE CMOS ADDRESS REGISTER IS POINTED TO A DEFAULT VALUE AND    :
;		THE INTERRUPT FLAG RESTORED TO THE ENTRY STATE ON RETURN.      :
;		ONLY THE CMOS LOCATION AND THE NMI STATE IS CHANGED.	       :
;-------------------------------------------------------------------------------
									 
CMOS_WRITE:				; WRITE (AH) TO LOCATION (AL) 
		PUSHF			; SAVE INTERRUPT ENABLE STATUS AND FLAGS 
		PUSH	AX		; SAVE WORK REGISTER VALUES	 

		cli
		push	ax		;save user NMI state
		or	al,NMI		;disable NMI for us
		out	CMOS_PORT,al
		nop
		mov	al,ah
		out	CMOS_DATA,al	;write data

		 ;set NMI state to user specified 
		pop	ax 		;get user NMI
		and	al,NMI
		or	al,CMOS_SHUT_DOWN
		out	CMOS_PORT,al
		nop
		in	al,CMOS_DATA

		POP	AX		; RESTORE WORK REGISTERS	 
		PUSH	CS		; *PLACE CODE SEGMENT IN STACK AND 
		CALL	CMOS_POPF	; *HANDLE POPF FOR B- LEVEL 80286 
		RETN

;-----------------------------------------------------------------------------
; 03/06/2018 - Retro DOS v3.0 
	
		; 07/04/2018 - Retro DOS v2.0

; *** SYSINIT1.ASM ***
; ----------------------------------------------------------------------------
; START OF MSDOS 3.3 SYSINIT CODE - SYSINIT1.ASM - 24/07/1987
; ----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0	
; 25/03/2018 - Retro DOS v2.0

_SYSINIT: 	; 05/07/2018
		;JMP	GOINIT

; ..SYSINIT DATA .............................................................

GOINIT:
		; 03/07/2018
Move_Myself:
		; 03/05/2018
		; 25/02/2018 - Retro DOS 2.0 - MSDOS 2.0 "SYSINIT.ASM"
		; (Modified for Retro DOS 2.0, for NASM 'incbin' method)

		SYSINITSIZE	EQU  sysinit_code_end - sysinit_code_start

		; 28/03/2018
        	;CLD
        	MOV     SI,SYSINIT_START   ; MSSTACK (05/07/2018)
        	XOR     DI,DI
		; 19/03/2018
	       	;mov	CX,[SYSINIT_START+MEMORY_SIZE]
		; 05/07/2018
	       	mov	CX,[SYSINIT+MEMORY_SIZE]

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
		;XOR	AX,AX ; 0
		;PUSH	AX

		; 03/07/2018
		;PUSH	CX ; 0
		; 05/07/2018
		mov	ax,07F0h ; ((**))
		push	ax

		RETF	; far jump to final location of SYSINIT code

align 16

; ----------------------------------------------------------------------------
; MSDOS 3.3 -IBMBIO.COM- SYSINIT CODE -will be relocated-
; ----------------------------------------------------------------------------
; 11/06/2018 - Retro DOS v3.0 

SYSINIT_START equ $

; 05/07/2018 - Retro DOS v3.0
; (NOTE: Following address is sysinit code start address after msstack code
;  in 'sysinit2.s', it is 07F0h for current SYSINIT, 05/07/2018)	
SYSINIT	equ SYSINIT_START + 07F0h ; ((**))

sysinit_code_start:
		; 22/11/2022
		; 06/07/2019
		; 06/07/2018 ('sysinit2.s' last modification)
		incbin	'SYSINIT2.BIN' ; Retro DOS 3.0 - MSDOS 3.3 'SYSINIT'
sysinit_code_end:
		db 90h

align 16 ; Paragraph alignment is necessary here for MSDOS kernel relocation

; ----------------------------------------------------------------------------
; START OF MSDOS 3.3 -IBMDOS.COM- KERNEL CODE (MSDOS.SYS) -will be relocated-
; ----------------------------------------------------------------------------
; 11/06/2018 - Retro DOS v3.0 

MSDOS_BIN_OFFSET:  ; this offset must be paragraph aligned
		; 16/01/2024 (BugFix)
		; 22/11/2022 (BugFix)
		; 22/07/2019 ('msdos3.s' last modification)
		; 20/05/2019 (BugFixed MSDOS3.BIN)
		; 10/09/2018 ('msdos3.s') 
		incbin	'MSDOS3.BIN' ; 05/07/2019
msdos_bin_size	equ $ - MSDOS_BIN_OFFSET

align 2
END_OF_KERNEL equ $