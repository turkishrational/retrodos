; ****************************************************************************
; SYSINIT.BIN (MSDOS 5.0 IO.SYS) - RETRO DOS v4.0 by ERDOGAN TAN - 21/10/2022
; ----------------------------------------------------------------------------
; Last Update: 02/11/2022 (Modified IO.SYS)
; ----------------------------------------------------------------------------
; Beginning: 03/06/2018 (Retro DOS 3.0), 21/03/2019 (Retro DOS 4.0)
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 (2.15) 
; ----------------------------------------------------------------------------
;	   ((nasm sysinit5.s -l sysinit5.lst -o SYSINIT5.BIN -Z error.txt)) 	
; ----------------------------------------------------------------------------
; Modified from 'sysinit2.s' (SYSINIT2.BIN) file of Retro DOS v3.0 (6/7/2018)
; ----------------------------------------------------------------------------
; Derived from 'SYSINIT1.ASM' and 'SYSINIT2.ASM' files of MSDOS 6.0
; source code by Microsoft, 1991 
; ----------------------------------------------------------------------------
; Derived from 'SYSINIT.ASM' file of MSDOS 2.0 (IBM PCDOS v2.0) source code
; by Microsoft, 12/10/1983
; ****************************************************************************
; main file: 'retrodos4.s'
; incbin 'SYSINIT3.BIN' ; (SYINITSEG)

; 21/10/2022
; ----------------------------------------------------------------------------
; This source code (version) is based on SYSINIT source code of disassembled
; MSDOS 5.0 IO.SYS file (SYSINIT.BIN) 
; Dissassembler: Hex-Rays Interactive Disassembler (IDA)
; ----------------------------------------------------------------------------
; Binary file splitter & joiner: FFSJ v3.3

;--------------------------------------------------------------
; SYSINIT.TXT (27/01/1983)
;--------------------------------------------------------------
;    SYSINIT is  a module linked behind the OEM bios.  It takes
;over  the  system  initialization  after  the  OEM  bios   has
;performed any  initialization  it  needs  to  do.   Control is
;transfered with a long jump to the external  variable  SYSINIT
;
;
;   The OEM  has  the  following  variables declared external:
;
;   CURRENT_DOS_LOCATION    WORD
;
;This word  contains  the  segment  number of the DOS before it
;is relocated.  The OEM bios must set this value.
;
;   FINAL_DOS_LOCATION      WORD
;
;This word contains the segment number of the DOS after SYSINIT
;moves it.  The OEM bios must set this value.
;
;   DEVICE_LIST             DWORD
;
;This  double  word  pointer  points  to  the  linked  list  of
;character and block device drivers.  The  OEM  must  set  this
;value.
;
;   MEMORY_SIZE             WORD
;
;This word  contains  the  number  of  RAM  paragraphs.  If the
;bios doesn't set  this  variable  SYSINIT  will  automatically
;calculate it.   NOTE:  systems with PARITY checked memory must
;size memory in the BIOS.  SYSINITs method is to  write  memory
;and read it back until it gets a mismatch.
;
;   DEFAULT_DRIVE           BYTE
;
;This is  the initial default drive when the system first comes
;up.  drive a=0, drive b=1,  etc.   If  the  bios  doesn't  set
;it then drive a is assumed.
;
;   BUFFERS                 BYTE
;
;This is  the  default  number of buffers for the system.  This
;value may be overridden by the user in  the  CONFIG.SYS  file.
;It is DBed to 2 in SYSINIT it should be greater than 1.
;
;   FILES                   BYTE
;
;This is  the  default  number  of  files for the system.  This
;value may be overridden by the user in  the  CONFIG.SYS  file.
;It is  DBed  to  8 in SYSINIT, values less than 5 are ignored.
;
;   SYSINIT                 FAR
;
;The entry  point  of  the  SYSINIT  module.  OEM BIOS jumps to
;this label at the end of its INIT code.
;
;   The OEM  has  the  following  variables declared public:
;
;   RE_INIT                 FAR
;
;This is an entry point which allows the BIOS to do some INIT
;work  after  the  DOS is initialized.  ALL REGISTERS MUST BE
;PRESERVED.  On entry DS points to the first available memory
;(after  the DOS).  DS:0 points to a 100H byte program header
;prefix which represents  the  "program"  currently  running.
;This  program  should  be  thought  of  as  the OEM BIOS and
;SYSINIT taken together.  This is not  a  normal  program  in
;that  no  memory  is  allocated to it, it is running in free
;memory.
;NOTES:
;     At the time this routine is called SYSINIT occupies the
;highest 10K of memory ("highest" is determined by the  value
;of the MEMORY_SIZE variable), DO NOT DO WRITES THERE.
;     Since this is called AFTER DOS is initialized, you can
;make system calls.  This also implies that the code for this
;routine    CANNOT   be   thrown   away   by   use   of   the
;FINAL_DOS_LOCATION since the DOS has already been moved.
;     If you don't want  anything done just set this to point
;at a FAR RET instruction.

; ----------------------------------------------------------------------
; TITLE   BIOS SYSTEM INITIALIZATION
; ----------------------------------------------------------------------

;include version.inc
; ----------------------------------------------------------------------

;FALSE   EQU     0
;TRUE    EQU     0FFFFh

;IBMVER	    EQU     TRUE
;IBMCOPYRIGHT EQU   FALSE
;STACKSW    EQU	    TRUE		;Include Switchable Hardware Stacks
;IBMJAPVER  EQU     FALSE		; If TRUE set KANJI true also
;MSVER      EQU     FALSE
;ALTVECT    EQU     FALSE		; Switch to build ALTVECT version
;KANJI      EQU     FALSE

;(MSDOS 6.0, versiona.inc, 1991)
; ----------------------------------------------------------------------
;MAJOR_VERSION  EQU	6
;;MINOR_VERSION	EQU	0	;6.00
;MINOR_VERSION  EQU	21	;6.21  ; 21/03/2019 - Retro DOS v4.0

; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0)
; ----------------------------------------------------------------------
MAJOR_VERSION   EQU	5
MINOR_VERSION   EQU	0

expected_version equ	(MINOR_VERSION<<8)+MAJOR_VERSION

;DOSREVNM equ	00000000b       ; m037 - bits 0-2 = revision number of DOS
				; currently 0.
DOSREVNM equ	00000111b	; [[[ 7 for Retro DOS v4.0 ]]] (21/03/2019)	
DOSINROM equ	00001000B       ; bit 3 of ver flags returned in BH
DOSINHMA equ	00010000B       ; bit 4 of ver flags 

;      if1
;      %OUT  ... for DOS Version 5.00 ...
;      endif

       ;******************************
       ;Each assembler program should:
       ;  mov ah,030h                   ;DOS Get Version function
       ;  int 021h                      ;Version ret. in AX,minor version first
       ;  cmp ax,expected_version       ;ALL utilities should check for an
       ;  jne error_handler             ; EXACT version match.
       ;******************************

; ----------------------------------------------------------------------
; device definitions

;Attribute bit masks
DEVTYP  EQU     8000h           ;Bit 15 - 1  if Char, 0 if block
DEVIOCTL EQU    4000h           ;Bit 14 - CONTROL mode bit
ISFATBYDEV EQU  2000h           ;Bit 13 - Device uses FAT ID bytes, comp media.
ISCIN   EQU     0001h           ;Bit 0 - This device is the console input.
ISCOUT  EQU     0002h           ;Bit 1 - This device is the console output.
ISNULL  EQU     0004h           ;Bit 2 - This device is the null device.
ISCLOCK EQU     0008h           ;Bit 3 - This device is the clock device.
ISIBM   EQU     0010h           ;Bit 4 - This device is special

; The device table list has the form:
struc	SYSDEV
.NEXT:		resd 1		;Pointer to next device header
.ATT:		resw 1		;Attributes of the device
.STRAT:		resw 1		;Strategy entry point
.INT:		resw 1		;Interrupt entry point
.NAME:		resb 8		;Name of device (only first byte used for block)
.size:
endstruc

;Static Reguest Header
struc	SRHEAD
.REQLEN:	resb 1		;Length in bytes of request block
.REQUNIT:	resb 1		;Device unit number
.REQFUNC:	resb 1		;Type of request
.REQSTAT:	resw 1		;Status Word
        	resb 8		;Reserved for queue links
.size:
endstruc

;Status word masks
STERR   EQU     8000H           ;Bit 15 - Error
STBUI   EQU     0200H           ;Bit 9 - Buisy
STDON   EQU     0100H           ;Bit 8 - Done
STECODE EQU     00FFH           ;Error code
WRECODE EQU     0

;Function codes
DEVINIT EQU     0               ;Initialization
DINITHL EQU     26              ;Size of init header
DEVMDCH EQU     1               ;Media check
DMEDHL  EQU     15              ;Size of media check header
DEVBPB  EQU     2               ;Get BPB
DEVRDIOCTL EQU  3               ;IOCTL read
DBPBHL  EQU     22              ;Size of Get BPB header
DEVRD   EQU     4               ;Read
DRDWRHL EQU     22              ;Size of RD/WR header
DEVRDND EQU     5               ;Non destructive read no wait (character devs)
DRDNDHL EQU     14              ;Size of non destructive read header
DEVIST  EQU     6               ;Input status
DSTATHL EQU     13              ;Size of status header
DEVIFL  EQU     7               ;Input flush
DFLSHL  EQU     15              ;Size of flush header
DEVWRT  EQU     8               ;Write
DEVWRTV EQU     9               ;Write with verify
DEVOST  EQU     10              ;Output status
DEVOFL  EQU     11              ;Output flush
DEVWRIOCTL EQU  12              ;IOCTL write

; ----------------------------------------------------------------------
struc	SYS_FCB
.fcb_drive:	resb 1
.fcb_name:	resb 8
.fcb_ext:	resb 3
.fcb_EXTENT:	resw 1
.fcb_RECSIZ:	resw 1	; Size of record (user settable)
.fcb_FILSIZ:	resw 1	; Size of file in bytes; used with the following
                        ; word
.fcb_DRVBP:	resw 1	; BP for SEARCH FIRST and SEARCH NEXT
.fcb_FDATE:	resw 1	; Date of last writing
.fcb_FTIME:	resw 1	; Time of last writing
.fcb_DEVID:	resb 1	; Device ID number, bits 0-5 if file.
                        ; bit 7=0 for file, bit 7=1 for I/O device
                        ; If file, bit 6=0 if dirty
                        ; If I/O device, bit 6=0 if EOF (input)
                        ;               Bit 5=1 if Raw mode
                        ;               Bit 0=1 if console input device
                        ;               Bit 1=1 if console output device
                        ;               Bit 2=1 if null device
                        ;               Bit 3=1 if clock device
.fcb_FIRCLUS:	resw 1	; First cluster of file
.fcb_CLUSPOS:	resw 1	; Position of last cluster accessed
.fcb_LSTCLUS:	resw 1	; Last cluster accessed and directory
             	resb 1	; pack 2 12 bit numbers into 24 bits...
.fcb_NR:	resb 1	; Next record
.fcb_RR:	resb 4	; Random record
.size:
endstruc

; ----------------------------------------------------------------------
; Field definition for I/O buffer information

; 22/03/2019 - Retro DOS v4.0 (MSDOS 6.0, BUFFER.INC, 1991)

struc buffinfo
.buf_next:	resw 1	; Pointer to next buffer in list
.buf_prev:	resw 1	; Pointer to previous buffer in list
.buf_ID:	resb 1	; Drive of buffer (bit 7 = 0)
			; SFT table index (bit 7 = 1)
			; = FFh if buffer free
.buf_flags:	resb 1	; Bit 7 = 1 if Remote file buffer
			;	= 0 if Local device buffer
			; Bit 6 = 1 if buffer dirty
			; Bit 5 = Reserved
			; Bit 4 = Search bit (bit 7 = 1)
			; Bit 3 = 1 if buffer is DATA
			; Bit 2 = 1 if buffer is DIR
			; Bit 1 = 1 if buffer is FAT
			; Bit 0 = Reserved
.buf_sector:	resd 1	; Sector number of buffer (bit 7 = 0)
; The next two items are often refed as a word (bit 7 = 0)
.buf_wrtcnt:	resb 1	; For FAT sectors, # times sector written out
.buf_wrtcntinc:	resw 1	; "   "     "   , # sectors between each write
.buf_DPB :	resd 1	; Pointer to drive parameters
.buf_fill:	resw 1	; How full buffer is (bit 7 = 1)
.buf_reserved:	resb 1	; make DWORD boundary for 386
.size:
endstruc

%define buf_offset	dword [buf_sector]
			;For bit 7 = 1, this is the byte
			;offset of the start of the buffer in
			;the file pointed to by buf_ID.  Thus
			;the buffer starts at location
			;buf_offset in the file and contains
			;buf_fill bytes.

bufinsiz	equ	buffinfo.size ; ; Size of structure in bytes


buf_Free	equ	0FFh		; buf_id of free buffer

;Flag byte masks
buf_isnet	EQU	10000000B
buf_dirty	EQU	01000000B
;***
buf_visit	EQU	00100000B
;***
buf_snbuf	EQU	00010000B

buf_isDATA	EQU	00001000B
buf_isDIR	EQU	00000100B
buf_isFAT	EQU	00000010B
buf_type_0	EQU	11110001B	; AND sets type to "none"

buf_NetID	EQU	bufinsiz

; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
;**	DPB - Drive Parameter Block

; 25/03/2019 - Retro DOS v4.0 (MSDOS 6.0, DPB.INC, 1991)

;	BUGBUG - this isn't authorative - it's my probably incomplete and
;	possibly inaccurate deductions from code study... - jgl
;
;	The DPB is DOS's main structure for describing block devices.
;	It contains info about the "Drive" intermingled with info about
;	the FAT file system which is presumably on the drive.  I don't know
;	how those fields are used if it's not the FAT file system - BUGBUG
;
;	The DPBs are statically allocated and chained off of DPBHead.
;	Users scan this chain looking for a match on DPB_DRIVE.
;	The DPBs are built at init time from info in the SYSDEV structure.

; 08/06/2018 - Retro DOS v3.0 (MSDOS 3.3, DPB.INC, 24/07/1987)

; 12/05/2019 - Retro DOS v4.0

struc	DPB
.DRIVE:		resb 1		; Logical drive # assoc with DPB (A=0,B=1,...)
.UNIT:		resb 1		; Driver unit number of DPB
.SECTOR_SIZE:	resw 1		; Size of physical sector in bytes
.CLUSTER_MASK:	resb 1		; Sectors/cluster - 1
.CLUSTER_SHIFT:	resb 1		; Log2 of sectors/cluster
.FIRST_FAT:	resw 1		; Starting record of FATs
.FAT_COUNT:	resb 1		; Number of FATs for this drive
.ROOT_ENTRIES:	resw 1		; Number of directory entries
.FIRST_SECTOR:	resw 1		; First sector of first cluster
.MAX_CLUSTER:	resw 1		; Number of clusters on drive + 1
;.FAT_SIZE:	resb 1  ; MSDOS 3.3
.FAT_SIZE:	resw 1		; Number of records occupied by FAT
.DIR_SECTOR:	resw 1		; Starting record of directory
.DRIVER_ADDR:	resd 1		; Pointer to driver
.MEDIA:		resb 1		; Media byte
.FIRST_ACCESS:	resb 1		; This is initialized to -1 to force a media
				; check the first time this DPB is used
.NEXT_DPB:	resd 1		; Pointer to next Drive parameter block
.NEXT_FREE:	resw 1		; Cluster # of last allocated cluster
.FREE_CNT:	resw 1		; Count of free clusters, -1 if unknown
.size:
endstruc

DPBSIZ  EQU     DPB.size	; Size of the structure in bytes

DSKSIZ  EQU	DPB.MAX_CLUSTER	; Size of disk (temp used during init only)

; ----------------------------------------------------------------------
; 26/03/2018

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
IOCTL_SHARING_RETRY	EQU	11
GENERIC_IOCTL_HANDLE	EQU	12
GENERIC_IOCTL		EQU	13

; GENERIC IOCTL SUB-FUNCTIONS
RAWIO			EQU	8

; RAWIO SUB-FUNCTIONS
GET_DEVICE_PARAMETERS	EQU	60H
SET_DEVICE_PARAMETERS	EQU	40H
READ_TRACK		EQU	61H
WRITE_TRACK		EQU	41H
VERIFY_TRACK		EQU	62H
FORMAT_TRACK		EQU	42H

; DEVICETYPE VALUES
MAX_SECTORS_IN_TRACK	EQU	63
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

; 25/03/2019 - Retro DOS v4.0  (MSDOS 6.0, BPB.INC, IOCTL.INC)

;**	BIOS PARAMETER BLOCK DEFINITION
;
;	The BPB contains information about the disk structure.  It dates
;	back to the earliest FAT systems and so FAT information is
;	intermingled with physical driver information.
;
;	A boot sector contains a BPB for its device; for other disks
;	the driver creates a BPB.  DOS keeps copies of some of this
;	information in the DPB.
;
;	The BDS structure contains a BPB within it.

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

; ----------------------------------------------------------------------
; structure, equates for devmark for mem command.

; 25/03/2019 - Retro DOS v4.0 (MSDOS 6.0, DEVMARK.INC, 1991)

struc devmark
 .id:	    resb 1
 .seg:	    resw 1
 .size:	    resw 1
 .dum:	    resb 3
 .filename: resb 8
endstruc

devmark_stk	equ	'S'
devmark_device	equ	'D'
devmark_ifs	equ	'I'
devmark_buf	equ	'B'
devmark_cds	equ	'L' ; lastdrive
devmark_files	equ	'F'
devmark_fcbs	equ	'X'
devmark_inst	equ	'T' ; used for sysinit base for install= command.
devmark_ems_stub equ	'E'

setbrkdone	equ	00000001b
for_devmark	equ	00000010b
not_for_devmark equ	11111101b

; ----------------------------------------------------------------------
; Memory arena structure

; 24/03/2019 - Retro DOS v4.0 
; (MSDOS 6.0, ARENA.INC)

;** Arena Header

struc ARENA
.SIGNATURE:	resb 1		; 4D for valid item, 5A for last item
.OWNER:		resw 1		; owner of arena item
.SIZE:		resw 1		; size in paragraphs of item
.RESERVED	resb 3		; reserved
.NAME:		resb 8		; owner file name
endstruc

; 12/04/2019

arena_owner_system	EQU 0	; free block indication

arena_signature_normal	EQU 4Dh ; valid signature, not end of arena
arena_signature_end	EQU 5Ah ; valid signature, last block in arena

; ----------------------------------------------------------------------
; Process data block (otherwise known as program header)

; 23/03/2019 - Retro DOS v4.0

; (MSDOS 6.0 - PDB.INC, 1991)

FILPERPROC	EQU     20

struc PDB	; Process_data_block
.EXIT_CALL:	resw 1   	; INT int_abort system terminate
.BLOCK_LEN:	resw 1		; size of execution block
                resb 1
.CPM_CALL:	resb 5		; ancient call to system
.EXIT:		resd 1		; pointer to exit routine
.CTRL_C:	resd 1		; pointer to ^C routine
.FATAL_ABORT:	resd 1		; pointer to fatal error
.PARENT_PID:	resw 1		; PID of parent (terminate PID)
.JFN_TABLE:     resb FILPERPROC ; indices into system table
.ENVIRON:	resw 1		; seg addr of environment
.USER_STACK:	resd 1		; stack of self during system calls
.JFN_LENGTH:	resw 1 		; number of handles allowed
.JFN_POINTER:	resd 1 		; pointer to JFN table
.NEXT_PDB:	resd 1		; pointer to nested PDB's
.INTERCON:	resb 1 		; *** jh-3/28/90 ***
.APPEND:	resb 1		; *** Not sure if still used ***
.NOVELL_USED:	resb 2		; Novell shell (redir) uses these
.VERSION:	resw 1		; DOS version reported to this app
.PAD1:		resb 14		; 	
.CALL_SYSTEM:	resb 5		; portable method of system call
.PAD2:		resb 7 		; reserved so FCB 1 can be used as an extended FCB
.FCB1:		resb 16		; default FCB 1
.FCB2:		resb 16		; default FCB 2
.PAD3:		resb 4		; not sure if this is used by PDB_FCB2
.TAIL:		resb 128	; command tail and default DTA
;.size:
endstruc

; ----------------------------------------------------------------------
; <system call definitions>

; 23/03/2019 - Retro DOS v4.0

; (MSDOS 6.0 - SYSCALL.INC, 1991)

ABORT                           EQU 0   ;  0      0
STD_CON_INPUT                   EQU 1   ;  1      1
STD_CON_OUTPUT                  EQU 2   ;  2      2
STD_AUX_INPUT                   EQU 3   ;  3      3
STD_AUX_OUTPUT                  EQU 4   ;  4      4
STD_PRINTER_OUTPUT              EQU 5   ;  5      5
RAW_CON_IO                      EQU 6   ;  6      6
RAW_CON_INPUT                   EQU 7   ;  7      7
STD_CON_INPUT_NO_ECHO           EQU 8   ;  8      8
STD_CON_STRING_OUTPUT           EQU 9   ;  9      9
STD_CON_STRING_INPUT            EQU 10  ; 10      A
STD_CON_INPUT_STATUS            EQU 11  ; 11      B
STD_CON_INPUT_FLUSH             EQU 12  ; 12      C
DISK_RESET                      EQU 13  ; 13      D
SET_DEFAULT_DRIVE               EQU 14  ; 14      E
FCB_OPEN                        EQU 15  ; 15      F
FCB_CLOSE                       EQU 16  ; 16     10
DIR_SEARCH_FIRST                EQU 17  ; 17     11
DIR_SEARCH_NEXT                 EQU 18  ; 18     12
FCB_DELETE                      EQU 19  ; 19     13
FCB_SEQ_READ                    EQU 20  ; 20     14
FCB_SEQ_WRITE                   EQU 21  ; 21     15
FCB_CREATE                      EQU 22  ; 22     16
FCB_RENAME                      EQU 23  ; 23     17
GET_DEFAULT_DRIVE               EQU 25  ; 25     19
SET_DMA                         EQU 26  ; 26     1A
GET_DEFAULT_DPB                 EQU 31  ; 31     1F
FCB_RANDOM_READ                 EQU 33  ; 33     21
FCB_RANDOM_WRITE                EQU 34  ; 34     22
GET_FCB_FILE_LENGTH             EQU 35  ; 35     23
GET_FCB_POSITION                EQU 36  ; 36     24
SET_INTERRUPT_VECTOR            EQU 37  ; 37     25
CREATE_PROCESS_DATA_BLOCK       EQU 38  ; 38     26
FCB_RANDOM_READ_BLOCK           EQU 39  ; 39     27
FCB_RANDOM_WRITE_BLOCK          EQU 40  ; 40     28
PARSE_FILE_DESCRIPTOR           EQU 41  ; 41     29
GET_DATE                        EQU 42  ; 42     2A
SET_DATE                        EQU 43  ; 43     2B
GET_TIME                        EQU 44  ; 44     2C
SET_TIME                        EQU 45  ; 45     2D
SET_VERIFY_ON_WRITE             EQU 46  ; 46     2E
; Extended functionality group
GET_DMA                         EQU 47  ; 47     2F
GET_VERSION                     EQU 48  ; 48     30
KEEP_PROCESS                    EQU 49  ; 49     31
GET_DPB                         EQU 50  ; 50     32
SET_CTRL_C_TRAPPING             EQU 51  ; 51     33
GET_INDOS_FLAG                  EQU 52  ; 52     34
GET_INTERRUPT_VECTOR            EQU 53  ; 53     35
GET_DRIVE_FREESPACE             EQU 54  ; 54     36
CHAR_OPER                       EQU 55  ; 55     37
INTERNATIONAL                   EQU 56  ; 56     38
;   Directory Group
MKDIR                           EQU 57  ; 57     39
RMDIR                           EQU 58  ; 58     3A
CHDIR                           EQU 59  ; 59     3B
;   File Group
CREAT                           EQU 60  ; 60     3C
OPEN                            EQU 61  ; 61     3D
CLOSE                           EQU 62  ; 62     3E
READ                            EQU 63  ; 63     3F
WRITE                           EQU 64  ; 64     40
UNLINK                          EQU 65  ; 65     41
LSEEK                           EQU 66  ; 66     42
CHMOD                           EQU 67  ; 67     43
IOCTL                           EQU 68  ; 68     44
XDUP                            EQU 69  ; 69     45
XDUP2                           EQU 70  ; 70     46
CURRENT_DIR                     EQU 71  ; 71     47
;    Memory Group
ALLOC                           EQU 72  ; 72     48
DEALLOC                         EQU 73  ; 73     49
SETBLOCK                        EQU 74  ; 74     4A
;    Process Group
EXEC                            EQU 75  ; 75     4B
EXIT                            EQU 76  ; 76     4C
WAITPROCESS			EQU 77  ; 77     4D
FIND_FIRST                      EQU 78  ; 78     4E
;   Special Group
FIND_NEXT                       EQU 79  ; 79     4F
; SPECIAL SYSTEM GROUP
SET_CURRENT_PDB                 EQU 80  ; 80     50
GET_CURRENT_PDB                 EQU 81  ; 81     51
GET_IN_VARS                     EQU 82  ; 82     52
SETDPB                          EQU 83  ; 83     53
GET_VERIFY_ON_WRITE             EQU 84  ; 84     54
DUP_PDB                         EQU 85  ; 85     55
RENAME                          EQU 86  ; 86     56
FILE_TIMES                      EQU 87  ; 87     57
;
ALLOCOPER			EQU 88	; 88     58	
; Network extention system calls
GetExtendedError		EQU 89	; 89	 59
CreateTempFile			EQU 90	; 90	 5A
CreateNewFile			EQU 91	; 91	 5B
LockOper			EQU 92	; 92	 5C Lock and Unlock
ServerCall			EQU 93	; 93	 5D CommitAll, ServerDOSCall,
					;	    CloseByName, CloseUser,
					;	    CloseUserProcess,
					;	    GetOpenFileList
UserOper			EQU 94	; 94	 5E Get and Set
AssignOper			EQU 95	; 95	 5F On, Off, Get, Set, Cancel
xNameTrans			EQU 96	; 96	 60
PathParse			EQU 97	; 97	 61
GetCurrentPSP			EQU 98	; 98	 62
Hongeul 			EQU 99	; 99	 63
ECS_CALL			EQU 99	; 99	 63  ;; DBCS support
Set_Printer_Flag		EQU 100 ; 100	 64
GetExtCntry			EQU 101 ; 101	 65
GetSetCdPg			EQU 102 ; 102	 66
ExtHandle			EQU 103 ; 103	 67
Commit				EQU 104 ; 104	 68
GetSetMediaID			EQU 105 ; 105	 69
IFS_IOCTL			EQU 107 ; 107	 6B
ExtOpen 			EQU 108 ; 108	 6C
;
;ifdef ROMEXEC
;ROM_FIND_FIRST			EQU 109 ; 109    6D
;ROM_FIND_NEXT			EQU 110 ; 110    6E
;ROM_EXCLUDE			EQU 111 ; 111	 6F
;endif
;
Set_Oem_Handler 		EQU 248 ; 248	 F8
OEM_C1				EQU 249 ; 249	 F9
OEM_C2				EQU 250 ; 250	 FA
OEM_C3				EQU 251 ; 251	 FB
OEM_C4				EQU 252 ; 252	 FC
OEM_C5				EQU 253 ; 253	 FD
OEM_C6				EQU 254 ; 254	 FE
OEM_C7				EQU 255 ; 255	 FF

; ----------------------------------------------------------------------
; SYSCONF.ASM (MSDOS 3.3 - 24/07/1987) 	
; ----------------------------------------------------------------------

;;	IF	STACKSW

;;
;; Internal Stack Parameters
;EntrySize		equ	8
;
;MinCount		equ	8
;DefaultCount		equ	9
;MaxCount		equ	64
;
;MinSize 		equ	32
;DefaultSize		equ	128
;MaxSize 		equ	512

;;	ENDIF

; ----------------------------------------------------------------------
; BIOSTRUC.INC (MSDOS 3.3 - 24/07/1987) 	
; ----------------------------------------------------------------------
					  ;;Rev 3.30 Modification
; ROM BIOS CALL PACKET STRUCTURES					  
									  
;*******************************					  
;System Service call ( Int 15h )					  
;*******************************					  
;Function AH = 0C0h, Return system configuration			  
;For PC and PCJR on return:						  
;	(AH)	= 80h							  
;	(CY)	= 1							  
;For PCXT, PC PORTABLE and PCAT on return:				  
;	(AH)	= 86h							  
;	(CY)	= 1							  
;For all others:							  
;	(AH)	= 0							  
;	(CY)	= 0							  
;	(ES:BX) = pointer to system descriptor vector in ROS		  
; System descriptor :							  
;	DW	xxxx		length of descriptor in bytes,		  
;				minimum length = 8			  
;	DB	xx		model byte				  
;				0FFh	= PC				  
;				0FEh	= PC/XT, Portable		  
;				0FDh	= PC/JR 			  
;				0FCh	= PC/AT				  
;				0F9h	= Convertable			  
;				0F8h	= Model 80			  
;				0E0 thru 0EFh = reserved		  
;									  
;	DB	xx		secondary model byte			  
;				000h	= PC1				  
;				000h	= PC/XT, Portable		  
;				000h	= PC/JR 			  
;				000h	= PC/AT 			  
;				001h	= PC/AT Model 339		  
;				003h	= PC/RT				  
;				000h	= Convertable			  
;									  
;	DB	xx		bios revision level			  
;				00 for first release, subsequent release  
;				of code with same model byte and	  
;				secondary model byte require revison level
;				to increase by one.			  
;									  
;	DB	xx		feature information byte 1		  
;				X0000000 = 1, bios use DMA channel 3	  
;					 = 0, DMA channel 3 not used	  
;									  
;				0X000000 = 1, 2nd Interrupt chip present  
;					 = 0, 2nd Interrupt chip not present
;									  
;				00X00000 = 1, Real Time Clock present	  
;					 = 0, Real Time Clock not present 
;									  
;				000X0000 = 1, Keyboard escape sequence(INT 15h)
;						called in keyboard interrupt
;						(Int 09h).		  
;					 = 0, Keyboard escape sequence not
;						called. 		  
;				0000XXXX reserved			  
;									  
;	DB	xx		feature information byte 2 - reserved	  
;									  
;	DB	xx		feature information byte 2 - reserved	  
;									  
;	DB	xx		feature information byte 2 - reserved	  
;									  
;	DB	xx		feature information byte 2 - reserved	  
;									  

; 22/03/2019									  
struc ROMBIOS_DESC		; BIOS_SYSTEM_DESCRIPTOR						  
.bios_sd_leng:		resw 1				  
.bios_sd_modelbyte:	resb 1					  
.bios_sd_scnd_modelbyte: 
			resb 1					  
			resb 1					  
.bios_sd_featurebyte1:	resb 1					  
			resb 4					  
endstruc					  
									  
;FeatureByte1	bit map equates 					  
DMAchannel3		equ 10000000b					  
ScndIntController	equ 01000000b					  
RealTimeClock		equ 00100000b					  
KeyEscapeSeq		equ 00010000b					  
					;;End of Modification

; ----------------------------------------------------------------------
; SYSVAR.INC (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 22/03/2019 - Retro DOS v4.0

;	SCCSID = @(#)sysvar.asm 1.1 85/04/10

struc SysInitVars
; MSDOS 3.3
.SYSI_DPB:    resd 1			; DPB chain
.SYSI_SFT:    resd 1			; SFT chain
.SYSI_CLOCK:  resd 1			; CLOCK device
.SYSI_CON:    resd 1			; CON device
.SYSI_MAXSEC: resw 1			; maximum sector size
.SYSI_BUF:    resd 1			; buffer chain
.SYSI_CDS:    resd 1			; CDS list
.SYSI_FCB:    resd 1			; FCB chain
.SYSI_KEEP:   resw 1			; keep count
.SYSI_NUMIO:  resb 1			; number of block devices
.SYSI_NCDS:   resb 1			; number of CDS's
.SYSI_DEV:    resd 1			; device list
; MSDOS 6.0
.SYSI_ATTR:	    resw 1		; null device attribute word
.SYSI_STRAT:	    resw 1		; null device strategy entry point
.SYSI_INTER:	    resw 1		; null device interrupt entry point
.SYSI_NAME:	    resb 8		; null device name
.SYSI_SPLICE:	    resb 0		; TRUE -> splicees being done
.SYSI_IBMDOS_SIZE:  resw 1		; DOS size in paragraphs
.SYSI_IFS_DOSCALL@: resd 1		; IFS DOS service rountine entry
.SYSI_IFS:	    resd 1	 	; IFS header chain
.SYSI_BUFFERS:	    resw 2		; BUFFERS= values (m,n)
.SYSI_BOOT_DRIVE:   resb 1		; boot drive A=1 B=2,..
.SYSI_DWMOVE:	    resb 1		; 1 if 386 machine
.SYSI_EXT_MEM:	    resw 1		; Extended memory size in KB.
.size:
endstruc

;This is added for more information exchage between DOS, BIOS.
;DOS will give the pointer to SysInitTable in ES:DI. - J.K. 5/29/86

; 22/03/2019
struc SysInitVars_Ext
.SYSI_InitVars:	   resd 1	; Points to the above structure.
.SYSI_Country_Tab: resd 1	; DOS_Country_cdpg_info
endstruc

; 09/06/2018
; 08/06/2018 - Retro DOS v3.0 (MSDOS 3.3)
SYSI_DPB    equ	0
SYSI_SFT    equ 4
SYSI_CLOCK  equ 8
SYSI_CON    equ 12
SYSI_MAXSEC equ 16
SYSI_BUF    equ 18 		
SYSI_CDS    equ 22
SYSI_FCB    equ 26
SYSI_KEEP   equ 30
SYSI_NUMIO  equ	32
SYSI_NCDS   equ	33
SYSI_DEV    equ 34
; 22/03/2019 - Retro DOS v4.0 (MSDOS 6.0)
SYSI_ATTR	  equ 38
SYSI_STRAT	  equ 40
SYSI_INTER	  equ 42
SYSI_NAME	  equ 44
SYSI_SPLICE	  equ 52
SYSI_IBMDOS_SIZE  equ 53
SYSI_IFS_DOSCALL@ equ 55
SYSI_IFS	  equ 59
SYSI_BUFFERS	  equ 63
SYSI_BOOT_DRIVE   equ 67
SYSI_DWMOVE	  equ 68
SYSI_EXT_MEM	  equ 69

;The SYSI_BUF of SysInitVars points to the following structure

EMS_MAP_BUFF_SIZE EQU 12	; EMS map buffer size

struc BUFFINF 	; BUFFINFO
.Buff_Queue:	   resd	1	; Head of list of buffers
.Dirty_Buff_Count: resw 1	; number of dirty buffers in list
.Cache_ptr:	   resd 1	; pointer to secondary cache
.Cache_count:      resw 1	; number of secondary cache entries

.Buff_In_HMA:	   resb 1	; flag to indicate that buffers
				; are in HMA
.Lo_Mem_Buff:	   resd 1	; Ptr to scratch buff in Low Mem
				;  used to read/write on disks
.UU_EMS_FIRST_PAGE:	resw 2
.UU_EMS_NPA640:		resw 1
.UU_EMS_mode:		resb 1	 ; no EMS = -1
.UU_EMS_handle:		resw 1	 ; EMS handle for buffers
.UU_EMS_PageFrame_Number: resw 1 ; EMS page frame number
.UU_EMS_Seg_Cnt:	resw 1	 ; EMS segment count
.UU_EMS_Page_Frame:	resw 1	 ; EMS page frame segment address
.UU_EMS_reserved:	resw 1	 ; EMS segment count
.UU_EMS_Map_Buff:	resb 1	 ; map buffer
.size:
endstruc

; ----------------------------------------------------------------------
; CURDIR.INC (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 22/03/2019 - Retro DOS v4.0

;**	CDS - Current Directory Structure
;
; CDS items are used bu the internal routines to store cluster numbers and
; network identifiers for each logical name.  The ID field is used dually,
; both as net ID and for a cluster number for local devices. In the case
; of local devices, the cluster number will be -1 if there is a potential
; of the disk being changed or if the path must be recracked.
;
;	Some pathnames have special preambles, such as
;
;		\\machine\sharename\...
;	For these pathnames we can't allow ".." processing to back us
;	up into the special front part of the name.  The CURDIR_END field
;	holds the address of the seperator character which marks
;	the split between the special preamble and the regular
;	path list; ".." processing isn't allowed to back us up past
;	(i.e., before) CURDIR_END
;	For the root, it points at the leading /. For net
;	assignments it points at the end (nul) of the initial assignment:
;	A:/	\\foo\bar	    \\foo\bar\blech\bozo
;	  ^		 ^		     ^

DIRSTRLEN	EQU	64+3		; Max length in bytes of directory strings
TEMPLEN 	EQU	DIRSTRLEN*2

struc 		curdir_list
; MSDOS 3.3
.cdir_text	resb	DIRSTRLEN	; text of assignment and curdir
.cdir_flags	resw	1		; various flags
.cdir_devptr	resd	1		; local pointer to DPB or net device
.cdir_ID	resw	2		; cluster of current dir (net ID)
.cdir_usr_word	resw	1
.cdir_end	resw	1		; end of assignment
; MSDOS 6.0
.cdir_type:	resb	1		; IFS drive (2=ifs, 4=netuse)
.cdir_ifd_hdr:	resd	1		; Ptr to File System Header
.cdir_fsda:	resb	2		; File System Dependent Data Area
.size:
endstruc

curdirlen	EQU	curdir_list.size	; Needed for screwed up
						; ASM87 which doesn't allow
						; Size directive as a macro
						; argument
%define curdir_netID	dword [curdir_list.cdir_ID]

;**	Flag values for CURDIR_FLAGS

;Flag word masks
curdir_isnet	EQU	1000000000000000B
curdir_isifs	EQU	1000000000000000B
curdir_inuse	EQU	0100000000000000B
curdir_splice	EQU	0010000000000000B
curdir_local	EQU	0001000000000000B

; ----------------------------------------------------------------------
; SF.INC (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 25/03/2019 - Retro DOS v4.0

; system file table

;**	System File Table SuperStructure
;
;	The system file table entries are allocated in contiguous groups.
;	There may be more than one such groups; the SF "superstructure"
;	tracks the groups.

struc	SF
.SFLink:	resd	1
.SFCount:	resw	1		; number of entries
.SFTable:	resw	1		; beginning of array of the following
.size:
endstruc

;**	System file table entry
;
;	These are the structures which are at SFTABLE in the SF structure.

struc	SF_ENTRY
.sf_ref_count:	resw	1		; number of processes sharing entry
					;   if FCB then ref count
.sf_mode: 	resw	1		; mode of access or high bit on if FCB
.sf_attr: 	resb	1		; attribute of file
.sf_flags:	resw	1		;Bits 8-15
					; Bit 15 = 1 if remote file
					;	 = 0 if local file or device
					; Bit 14 = 1 if date/time is not to be
					;   set from clock at CLOSE.  Set by
					;   FILETIMES and FCB_CLOSE.  Reset by
					;   other reseters of the dirty bit
					;   (WRITE)
					; Bit 13 = Pipe bit (reserved)
					;
					; Bits 0-7 (old FCB_devid bits)
					; If remote file or local file, bit
					; 6=0 if dirty Device ID number, bits
					; 0-5 if local file.
					; bit 7=0 for local file, bit 7
					;      =1 for local I/O device
					; If local I/O device, bit 6=0 if EOF (input)
					;		Bit 5=1 if Raw mode
					;		Bit 0=1 if console input device
					;		Bit 1=1 if console output device
					;		Bit 2=1 if null device
					;		Bit 3=1 if clock device
.sf_devptr:	resd	1		; Points to DPB if local file, points
					; to device header if local device,
					; points to net device header if
					; remote
.sf_firclus:	resw	1		; First cluster of file (bit 15 = 0)
;.sf_lstclus:	resw	1 ; *	
.sf_time: 	resw	1		; Time associated with file
.sf_date: 	resw	1		; Date associated with file
.sf_size: 	resd	1		; Size associated with file
.sf_position:	resd	1		; Read/Write pointer or LRU count for FCBs
;
; Starting here, the next 7 bytes may be used by the file system to store an
; ID
;
.sf_cluspos:	resw	1		; Position of last cluster accessed
.sf_dirsec:	resw	1		; Sector number of directory sector for this file
.sf_dirpos:	resb	1		; Offset of this entry in the above
;
; End of 7 bytes of file-system specific info.
;
.sf_name:	resb	11		; 11 character name that is in the
					; directory entry.  This is used by
					; close to detect file deleted and
					; disk changed errors.
; SHARING INFO
.sf_chain:	resd	1		; link to next SF
.sf_UID:	resw	1
.sf_PID:	resw	1
.sf_MFT:	resw	1
.sf_lstclus:	resw	1 ; *		; Last cluster accessed
.sf_IFS_HDR:	resd 	1 ; **
.size:
endstruc

; ----------------------------------------------------------------------
; DOSCNTRY.INC (MSDOS 3.3 - 24/07/1987) 	
; ----------------------------------------------------------------------
; 11/06/2018 - Retro DOS v3.0

;Equates for COUNTRY INFORMATION.
SetCountryInfo		EQU	1	;country info
SetUcase		EQU	2	;uppercase table
SetLcase		EQU	3	;lowercase table (Reserved)
SetUcaseFile		EQU	4	;uppercase file spec table
SetFileList		EQU	5	;valid file character list
SetCollate		EQU	6	;collating sequence
SetDBCS 		EQU	7	;double byte character set
SetALL			EQU	-1	;all the entries

;DOS country and code page information table structure.
;Internally, IBMDOS gives a pointer to this table.
;IBMBIO, MODE and NLSFUNC modules communicate with IBMDOS through
;this structure.

struc country_cdpg_info ; DOS_country_cdpg_info
.ccInfo_reserved :	resb	8	;reserved for internal use
.ccPath_CountrySys:	resb	64	;path and filename for country info
.ccSysCodePage:		resw	1	;system code page id
.ccNumber_of_entries:	resw	1 ; dw 5
.ccSetUcase:		resb	1 ; db SetUcase ; = 2
.ccUcase_ptr:		resd	1	;pointer to Ucase table

.ccSetUcaseFile:	resb	1 ; db SetUcaseFile ; = 4
.ccFileUcase_ptr: 	resd	1	;pointer to File Ucase table

.ccSetFileList:		resb	1 ; db SetFileList ; = 5
.ccFileChar_ptr:	resd	1	;pointer to File char list table

.ccSetCollate:		resb	1 ; db SetCollate ; = 6
.ccCollate_ptr:		resd	1	;pointer to collate table

.ccSetCountryInfo:	resb	1 ; db SetCountryInfo ; = 1
.ccCountryInfoLen:	resw	1	;length of country info
.ccDosCountry:		resw	1	;system country code id
.ccDosCodePage:		resw	1	;system code page id
.ccDFormat:		resw	1	;date format
.ccCurSymbol:		resb	5 ; db "    ",0
					;5 byte of (currency symbol+0)
.cc1000Sep:		resb	2 ; db " ",0 ;2 byte of (1000 sep. + 0)
.ccDecSep:		resb	2 ; db " ",0 ;2 byte of (Decimal sep. + 0)
.ccDateSep:		resb	2 ; db " ",0 ;2 byte of (date sep. + 0)
.ccTimeSep:		resb 	2 ; db " ",0 ;2 byte of (time sep. + 0)
.ccCFormat:		resb	1 	;currency format flags
.ccCSigDigits:		resb	1	;# of digits in currency
.ccTFormat:		resb	1	;time format
.ccMono_Ptr:		resd	1	;monocase routine entry point
.ccListSep:		resb	2 ; db " ",0 ;data list separator
.ccReserved_area: 	resw	5 ; dw 5 dup(?) ;reserved
.size:
endstruc

NEW_COUNTRY_SIZE    equ  country_cdpg_info.size - country_cdpg_info.ccDosCountry

; ======================================================================
; retrodos4.s (offset addresses in MSDOS.SYS or RETRODOS.SYS)
; ======================================================================
; 21/03/2019 - Retro DOS v4.0
; 21/10/2022 - Retro DOS v4.0 (MOdified MSDOS 5.0 IO.SYS)

;KERNEL_SEGMENT	equ 0070h  ; (IO.SYS loading segment, BIOS_DATA segment)
; 21/10/2022
DOSBIODATASEG equ 0070h	; (IO.SYS loading segment, BIOS_DATA segment)
; 22/10/2022
DOSBIOCODESEG equ 02C7h ; (MSDOS 5.0 IO.SYS, BIOS_CODE segment)

; Note: These offset addresses must be chanqed when the code 
; 	in retrodos4.s (MSDOS.SYS) file will be changed.

; (following addresses can be verified by searching them in retrodos4.lst) 

; 13/05/2019

;IsWin386         equ 08CFh
;V86_Crit_SetFocus equ 08D0h
; 21/10/2022
IsWin386          equ 08D0h
V86_Crit_SetFocus equ 08D1h 

;seg_reinit	  equ 0772h ; not used in Retro DOS v4.0
; 21/10/2022 - Retro DOS v4.0 (MOdified MSDOS 5.0 IO.SYS)
seg_reinit	  equ 0032h ; DOSBIOCODESEG:0032h

;SysinitPresent	  equ 08FCh
; 21/10/2022
SysinitPresent	  equ 08FDh

inHMA		  equ 000Dh
xms		  equ 000Eh
;FreeHMAPtr	  equ 08F6h
;multrk_flag	  equ 0533h
;ec35_flag	  equ 0535h
;EOT		  equ 012Eh
; 21/10/2022
FreeHMAPtr	  equ 08F7h
multrk_flag	  equ 052Fh
ec35_flag	  equ 0531h
EOT		  equ 012Ch

;NextStack	  equ 08BFh
;IT_StackLoc	  equ 08C5h
;IT_StackSize	  equ 08C9h
; 21/10/2022
NextStack	  equ 08C0h
IT_StackLoc	  equ 08C6h
IT_StackSize	  equ 08CAh

;MoveDOSIntoHMA	  equ 08F8h
; 21/10/2022
MoveDOSIntoHMA	  equ 08F9h

;INT19SEM equ 0644h ; 01/05/2019 - retrodos4.lst
;I19_LST  equ 0645h ; 27/03/2019 - retrodos4.lst
; 21/10/2022
INT19SEM equ 0640h ; (iosys5.txt)
I19_LST  equ 0641h ; (iosys5.txt)

INT19OLD02 equ I19_LST+1 ; 0642h ; 21/10/2022
INT19OLD08 equ I19_LST+6
INT19OLD09 equ I19_LST+11
INT19OLD0A equ I19_LST+16
INT19OLD0B equ I19_LST+21
INT19OLD0C equ I19_LST+26
INT19OLD0D equ I19_LST+31
INT19OLD0E equ I19_LST+36
INT19OLD70 equ I19_LST+41
INT19OLD72 equ I19_LST+46
INT19OLD73 equ I19_LST+51
INT19OLD74 equ I19_LST+56
INT19OLD76 equ I19_LST+61
INT19OLD77 equ I19_LST+66 ; 0683h ; 21/10/2022

;keyrd_func	equ 04E9h
;keysts_func	equ 04EAh
;t_switch	equ 04F6h
; 21/10/2022
keyrd_func	equ 04E5h
keysts_func	equ 04E6h
t_switch	equ 04F2h

; 22/10/2022
SYSINITSEG	equ 046Dh  ; SYSINIT segment
BCODE_END	equ (SYSINITSEG-DOSBIOCODESEG)*16 ; = 1A60h
BCODE_START	equ 30h  ; (offset BiosDataWord in DOSBIOCODESEG) 
RE_INIT		equ 089Bh ; (re_init offset in DOSBIODATASEG)

; ----------------------------------------------------------------------
; CONFIG.INC (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 15/04/2019 - Retro DOS v4.0

CONFIG_BEGIN        equ  '['
CONFIG_BREAK        equ  'C'
CONFIG_BUFFERS      equ  'B'
CONFIG_COMMENT      equ  'Y'
CONFIG_COUNTRY      equ  'Q'
CONFIG_DEVICE       equ  'D'
CONFIG_DEVICEHIGH   equ  'U'
CONFIG_DOS          equ  'H'
CONFIG_DRIVPARM     equ  'P'
CONFIG_FCBS         equ  'X'
CONFIG_FILES        equ  'F'
CONFIG_INCLUDE      equ  'J'
CONFIG_INSTALL      equ  'I'
CONFIG_INSTALLHIGH  equ  'W'
CONFIG_LASTDRIVE    equ  'L'
CONFIG_MENUCOLOR    equ  'R'
CONFIG_MENUDEFAULT  equ  'A'
CONFIG_MENUITEM     equ  'E'
CONFIG_MULTITRACK   equ  'M'
CONFIG_NUMLOCK      equ  'N'
CONFIG_REM          equ  '0'
CONFIG_SEMICOLON    equ  ';'
CONFIG_SET          equ  'V'
CONFIG_SHELL        equ  'S'
CONFIG_STACKS       equ  'K'
CONFIG_SUBMENU      equ  'O'
CONFIG_SWITCHES     equ  '1'

CONFIG_UNKNOWN      equ  'Z'

CONFIG_OPTION_QUERY equ 80h

; ----------------------------------------------------------------------
; SYSINIT1.ASM (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 21/03/2019 - Retro DOS v4.0

true	equ	0FFFFh
false	equ	0
cr	equ	13
lf	equ	10
tab	equ	9

multMULT	   equ	4Ah
multMULTGETHMAPTR  equ	1
multMULTALLOCHMA   equ	2

;NOEXEC    equ	FALSE

stacksw    equ	true	;include switchable hardware stacks
mycds_size equ	88	;size of curdir_list. if it is not
			;the same, then will generate compile error.

entrysize   equ     8

mincount    equ     8
defaultcount equ    9
maxcount    equ     64

minsize     equ     32
defaultsize equ     128
maxsize     equ     512

;%define allocbyte  byte [es:bp+0]
;%define intlevel   byte [es:bp+1]
;%define savedsp    word [es:bp+2]
;%define savedss    word [es:bp+4]
;%define newsp	    word [es:bp+6]

allocbyte   equ     0
intlevel    equ     1
savedsp     equ     2
savedss     equ     4
newsp       equ     6

free	    equ     0
allocated   equ     1
overflowed  equ     2
clobbered   equ     3

;---------------------------------------
; external variable defined in ibmbio module for multi-track

multrk_on equ	10000000b ;user specified mutitrack=on,or system turns
			  ; it on after handling config.sys file as a
			  ; default value,if multrk_flag = multrk_off1.
multrk_off1 equ 00000000b ;initial value. no "multitrack=" command entered.
multrk_off2 equ 00000001b ;user specified multitrack=off.

; SYSINITSEG	SEGMENT PUBLIC 'SYSTEM_INIT'

SYSINIT$:
	;IF	STACKSW 
	; include MSSTACK.INC	;Main stack program and data definitions
	; include STKMES.INC	;Fatal stack error message
	;   public Endstackcode
;Endstackcode	label byte
	;ENDIF

; 05/07/2018
; ----------------------------------------------------------------------
; 04/06/2018 - Retro DOS v3.0

; ----------------------------------------------------------------------
; 21/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS - SYSINIT)
; ----------------------------------------------------------------------

;	MSStack.inc
;
;	Interrupt level 2, 3, 4, 5, 6, 7,(10, 11, 12, 14, 15 - AT level)
;	should follow the standard Interrupt Sharing Scheme which has
;	a standard header structure.
;	Fyi, the following shows the relations between
;	the interrupt vector and interrupt level.
; VEC(Hex)    2  8  9  A  B  C	D  E  70  72  73  74  76  77
; LVL(Deci)   9  0  1  2  3  4	5  6   8  10  11  12  14  15
;	MSSTACK module modifies the following interrupt vectors
;	to meet the standard Interrupt Sharing standard;
;	  A, B, C, D, E, 72, 73, 74, 76, 77.
;	Also, for interrupt level 7 and 15, the FirstFlag in a standard header
;	should be initialized to indicat whether this interrupt handler is
;	the first (= 80h) or not.  The FirstFlag entry of INT77h's
;	program header is initialized in STKINIT.INC module.
;	FirstFlag is only meaningful for interrupt level 7 and 15.
;

;  User specifies the number of stack elements - default = 9
;						 minimum = 8
;						 maximum = 64
;
;  Intercepts Asynchronous Hardware Interrupts only
;
;  Picks a stack from pool of stacks and switches to it
;
;  Calls the previously saved interrupt vector after pushing flags
;
;  On return, returns the stack to the stack pool
;

; This is a modification of STACKS:
; 1. To fix a bug which was causing the program to take up too much space.
; 2. To dispense stack space from hi-mem first rather than low-mem first.
;    . Clobbers the stack that got too big instead of innocent stack
;    . Allows system to work if the only stack that got too big was the most
;      deeply nested one
; 3. Disables NMI interrupts while setting the NMI vector.
; 4. Does not intercept any interupts on a PCjr.
; 5. Double checks that a nested interrupt didn't get the same stack.
; 6. Intercepts Ints 70, 72-77 for PC-ATs and other future products

		;EVEN
;align 2

		; 21/10/2022

		dw	0	; spare field but leave these in order
stackcount:	dw	0
stackat: 	dw	0
stacksize:	dw	0
stacks:		dw	0
		dw	0

firstentry:	dw	stacks
lastentry:	dw	stacks+(defaultcount*entrysize)-entrysize
nextentry:	dw	stacks+(defaultcount*entrysize)-entrysize

;***********************************************************************
; THESE ARE THE INDIVIDUAL INTERRUPT HANDLERS

; ----------------------------------------------------------------------

old02:	dd	0

int02:

; *********************************************************************
;
; this is special support for the pc convertible / nmi handler
;
;	on the pc convertible, there is a situation where an nmi can be 
;	caused by using the "out" instructions to certain ports. when this
;	occurs, the pc convertible hardware *guarantees* that **nothing** 
;	can stop the nmi or interfere with getting to the nmi handler. this
;	includes other type of interrupts (hardware and software), and
;	also includes other type of nmi's. when any nmi has occured,
;	no other interrtupt (hardware, software or nmi) can occur until
;	the software takes specific steps to allow further interrupting.
;
;	for pc convertible, the situation where the nmi is generated by the
;	"out" to a control port requires "fixing-up" and re-attempting. in
;	otherwords, it is actually a "restartable exception". in this
;	case, the software handler must be able to get to the stack in
;	order to figure out what instruction caused the problem, where
;	it was "out"ing to and what value it was "out"ing.  therefore,
;	we will not switch stacks in this situation. this situation is
;	detected by interrogating port 62h, and checking for a bit value
;	of 80h. if set, *****do not switch stacks*****.
;
; *********************************************************************

	push	ax
	push	es
	mov	ax,0F000h
	mov	es,ax
	; 02/11/2022
	cmp	byte [es:0FFFEh],0F9h ; mdl_convert ;check if convertible
	pop	es
	jne	short normal02

	in	al,62h		; PC/XT PPI port C. Bits:
				; 0-3: values of DIP switches
				; 5: 1=Timer 2 channel out
				; 6: 1=I/O channel check
				; 7: 1=RAM parity check error occurred.
	test	al,80h
	jz	short normal02
special02:
	pop	ax
	jmp	far [cs:old02]
normal02:
	pop	ax
	call	do_int_stacks
	dw	old02

; ----------------------------------------------------------------------

old08:	dd	0

int08:
	call	do_int_stacks
	dw	old08

; ----------------------------------------------------------------------

old09:	dd	0

int09:

; keyboard interrupt must have a three byte jump, a nop and a zero byte
; as its first instruction for compatibility reasons

	jmp	short keyboard_lbl
	nop
	db	0

keyboard_lbl:
	call	do_int_stacks
	dw	old09

; ----------------------------------------------------------------------

old70:	dd	0

int70:
	call	do_int_stacks
	dw	old70

; ----------------------------------------------------------------------

;	irp	a,<0a,0b,0c,0d,0e,72,73,74,76,77>
;public	int&a
;public	old&a
;public	firstflag&a
;int&a	proc	far
;	jmp	short entry_int&a&_stk
;old&a	dd	  0		;forward pointer
;	dw	  424bh 	;compatible signature for int. sharing
;firstflag&a db   0		;the firstly hooked.
;	jmp	short intret_&a	;reset routine. we don't care this.
;	db	7 dup (0)	;reserved for future.
;entry_int&a&_stk:
;	call	do_int_stacks
;	dw	old&a
;intret_&a:
;	iret
;int&a	endp
;	endm

; ----------------------------------------------------------------------

int0A:
	jmp	short entry_int0A_stk
old0A:	dd	0	
	dw	424Bh
firstflag0A:
	db	0
	jmp	short intret_0A
	times	7 db 0

entry_int0A_stk:
	call	do_int_stacks
	dw	old0A
intret_0A:
	iret

; ----------------------------------------------------------------------

int0B:
	jmp	short entry_int0B_stk
old0B:	dd	0	
	dw	424Bh
firstflag0B:
	db	0
	jmp	short intret_0B
	times	7 db 0

entry_int0B_stk:
	call	do_int_stacks
	dw	old0B
intret_0B:
	iret

; ----------------------------------------------------------------------

int0C:
	jmp	short entry_int0C_stk
old0C:	dd	0	
	dw	424Bh
firstflag0C:
	db	0
	jmp	short intret_0C
	times	7 db 0

entry_int0C_stk:
	call	do_int_stacks
	dw	old0C
intret_0C:
	iret

; ----------------------------------------------------------------------

int0D:
	jmp	short entry_int0D_stk
old0D:	dd	0	
	dw	424Bh
firstflag0D:
	db	0
	jmp	short intret_0D
	times	7 db 0

entry_int0D_stk:
	call	do_int_stacks
	dw	old0D
intret_0D:
	iret

; ----------------------------------------------------------------------

int0E:
	jmp	short entry_int0E_stk
old0E:	dd	0	
	dw	424Bh
firstflag0E:
	db	0
	jmp	short intret_0E
	times	7 db 0

entry_int0E_stk:
	call	do_int_stacks
	dw	old0E
intret_0E:
	iret

; ----------------------------------------------------------------------

int72:
	jmp	short entry_int72_stk
old72:	dd	0	
	dw	424Bh
firstflag72:
	db	0
	jmp	short intret_72
	times	7 db 0

entry_int72_stk:
	call	do_int_stacks
	dw	old72
intret_72:
	iret

; ----------------------------------------------------------------------

int73:
	jmp	short entry_int73_stk
old73:	dd	0	
	dw	424Bh
firstflag73:
	db	0
	jmp	short intret_73
	times	7 db 0

entry_int73_stk:
	call	do_int_stacks
	dw	old73
intret_73:
	iret

; ----------------------------------------------------------------------

int74:
	jmp	short entry_int74_stk
old74:	dd	0	
	dw	424Bh
firstflag74:
	db	0
	jmp	short intret_74
	times	7 db 0

entry_int74_stk:
	call	do_int_stacks
	dw	old74
intret_74:
	iret

; ----------------------------------------------------------------------

int76:
	jmp	short entry_int76_stk
old76:	dd	0	
	dw	424Bh
firstflag76:
	db	0
	jmp	short intret_76
	times	7 db 0

entry_int76_stk:
	call	do_int_stacks
	dw	old76
intret_76:
	iret

; ----------------------------------------------------------------------

int77:
	jmp	short entry_int77_stk
old77:	dd	0	
	dw	424Bh
firstflag77:
	db	0
	jmp	short intret_77
	times	7 db 0

entry_int77_stk:
	call	do_int_stacks
	dw	old77
intret_77:
	iret

; ----------------------------------------------------------------------

;********************************************************************
;common routines
;********************************************************************

; do interrupt stack switching. the fake return address holds
; a pointer to the far-pointer of the actual interrupt
; service routine

; 21/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 SYSINIT)
; 21/03/2019 - Retro DOS v4.0

;allocbyte   equ 0
;intlevel    equ 1
;savedsp     equ 2
;savedss     equ 4
;newsp       equ 6

do_int_stacks:
	push	ax
	push	bp
	push	es
	mov	es,[cs:stacks+2]	; Get segment of stacks
	mov	bp,[cs:nextentry]	; get most likely candidate
	mov	al,allocated ; 1
	; 21/10/2022
	xchg	[es:bp+allocbyte],al 
	;xchg	[es:bp],al		; grab the entry
	cmp	al,free ; 0		; still avail?
	jne	short notfree02

	sub	word [cs:nextentry],entrysize ; set for next interrupt

found02:
	mov	[es:bp+savedsp],sp	; save sp value
	mov	[es:bp+savedss],ss	; save ss also

	mov	ax,bp			; temp save of table offset

	mov	bp,[es:bp+newsp]	; get new SP value
	; 21/10/2022
	;mov	bp,[es:bp+6]
	cmp	[es:bp+0],ax	
	;cmp	[es:bp],ax		; check for offset into table
	jne	short foundbad02

	;mov	ax,es			; point ss,sp to the new stack
	;mov	es,bp
	;mov	bp,sp
	;mov	bp,[bp+6]
	;mov	ss,ax
	;mov	sp,es
	;mov	es,ax
	;mov	bp,[cs:bp]

	; 21/10/2022 (MSDOS 5.0 code SYSINIT code)
	push    bp
	mov     bp, sp
	mov     ax, [bp+8]
	pop     bp
	push    es
	pop     ss
	mov     sp, bp
	mov     bp, ax
	mov     bp, [cs:bp+0]	
	
	pushf				; go execute the real interrupt handler
	;call	far [cs:bp]		;  which will iret back to here
	; 21/10/2022
	call	far [cs:bp+0]

	mov	bp,sp			; retrieve the table offset for us
	;mov	bp,[es:bp]		;  but leave it on the stack
	; 21/10/2022
	mov	bp, [es:bp+0]
	mov	ss,[es:bp+savedss]	; get old stack back
	mov	sp,[es:bp+savedsp]

	mov	byte [es:bp+allocbyte],free ; free the entry
	; 21/10/2022
	;mov	byte [es:bp],free ; 0
	mov	[cs:nextentry],bp	; setup to use next time

	pop	es
	pop	bp			; saved on entry
	pop	ax			; saved on entry
	add	sp,2
	iret				; done with this interrupt

notfree02:
	cmp	al,allocated		; error flag
	je	short findnext02	;  no, continue
	xchg	[es:bp+allocbyte],al	;  yes, restore error value
	; 21/10/2022
	;xchg	[es:bp],al

findnext02:
	call	longpath
	jmp	short found02

foundbad02:
	cmp	bp,[cs:firstentry]
	jc	short findnext02
	mov	bp,ax			; flag this entry
	mov	byte [es:bp+allocbyte],clobbered
	; 21/10/2022
	;mov	byte [es:bp],clobbered ; 3
	jmp	short findnext02	; keep looking

; ----------------------------------------------------------------------

; Common routines

longpath:
	; 21/03/2019
	mov	bp,[cs:lastentry]	; start with last entry in table
lploopp:
	cmp	byte [es:bp+allocbyte],free ; is entry free?
	; 21/10/2022
	;cmp	byte [es:bp],free
	jne	short inuse		;  no, try next one

	mov	al,allocated
	xchg	[es:bp+allocbyte],al	; allocate entry
	; 21/10/2022
	;xchg	[es:bp],al
	cmp	al,free 		; is it still free?
	je	short found		;  yes, go use it

	cmp	al,allocated		; is it other than Allocated or Free?
	je	short inuse		;  no, check the next one

	mov	[es:bp+allocbyte],al	;  yes, put back the error state
	; 21/10/2022
	;mov	[es:bp],al
inuse:
	cmp	bp,[cs:firstentry]
	je	short fatal
	sub	bp,entrysize
	jmp	short lploopp
found:
	retn
fatal:
	push	ds
	mov	ax,0F000h		;look at the model byte
	mov	ds,ax
	cmp	byte [0FFFEh],0F9h ; mdl_convert ; convertible?
	pop	ds
	jne	short skip_nmis

	mov	al,07h			; disable pc convertible nmis
	out	72h,al

skip_nmis:
	cli				; disable and mask
	mov	al,0FFh			;   all other ints
	out	021h,al
	out	0A1h,al

	mov	si,cs
	mov	ds,si
	mov	si,fatal_msg
;SR;
;   We set all foci to this VM to issue the stack failure message
;
	push	ax
	push	ds
	;;mov	ax,Bios_Data ; 0070h
	;mov	ax,KERNEL_SEGMENT ; 0070h
	; 21/10/2022
	mov	ax,DOSBIODATASEG
	mov	ds,ax

	;test	byte [08D0h],1 	; (MSDOS 6.21, IO.SYS - SYSINIT:021Eh)
	test	byte [IsWin386],1 ; (retrodos4.sys, offset: ****h)
	pop	ds
	pop	ax
	jz	short fatal_loop	; win386 not present, continue

	;;call	far ptr 0070h:08D1h ; (MSDOS 621, IO.SYS - SYSINIT:0227h)
	;call	KERNEL_SEGMENT:V86_Crit_SetFocus ; set focus to this VM
	; 21/10/2022
	call	DOSBIODATASEG:V86_Crit_SetFocus ; 0070h:08D1h
;
;SR; We do not bother about the returned status of this call. 
;
fatal_loop:
	lodsb
	cmp	al,'$'
	je	short fatal_done

	mov	bl,7
	mov	ah,14
	int	10h			; whoops, this enables ints
	jmp	short fatal_loop

fatal_done:
	jmp	short fatal_done


; 21/03/2019 - Retro DOS v4.0 (MSDOS 6.0, SYSINIT1.ASM, 1991)
; ----------------------------------------------------------------------
;	include msbio.cl5		; fatal stack error message

; MSDOS 6.21, IO.SYS, SYSINIT:023Bh

; STKMES.INC - MSDOS 3.3 (24/07/1987)
; ----------------------------------------------------------------------
; 04/06/2018 - Retro DOS v3.0

fatal_msg:
	db	0Dh,0Ah
	db	7,0Dh,0Ah
	db	"Internal stack overflow",0Dh,0Ah
	db	"System halted",0Dh,0Ah,"$" 

endstackcode:

; ----------------------------------------------------------------------
; SYINIT1.ASM (MSDOS 6.0, 1991) 'SYSINIT' jump addr from 'MSINIT.ASM'
; ----------------------------------------------------------------------
; 04/06/2018 - Retro DOS v3.0 (MSDOS 3.3, SYSINIT1.ASM, 24/07/1987)

; 22/03/2019 - Retro DOS v4.0

; SYSINIT:0269h (MSDOS 6.21 IO.SYS, SYSINIT segment, offset: 0269h)

; ('SYSINIT:' location/address is used in 'retrodos4.s'. If following
; address will be changed, it must also be changed in 'retrodos4.s'.)

; 21/10/2022- Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; ----------------------------------------------------------------------
; SYSINITSEG:0267h (MSDOS 5.0 IO.SYS, SYSINIT segment, offset: 0267h)

SYSINIT:	
        JMP	GOINIT
	;JMP	SYSIN ; 25/02/2018 - Retro DOS 2.0 modification

; ----------------------------------------------------------------------

struc DDHighInfo
 .ddhigh_CSegPtr resd 1	; pointer to code segment to be relocated
 .ddhigh_CSegLen resw 1	; length of code segment to be relocated
 .ddhigh_CallBak resd 1	; pointer to the call back routine
endstruc

; 22/03/2019 - Retro DOS v4.0

runhigh: db	0

; 02/11/2022
;align 4

DOSINFO: 
	dd	0	; address of the DOS Sysini Variables
;MSDOS:
dos_temp_location: ; dword ; MSDOS 6.0
dosinit:		 ; MSDOS 6.0
	dw	0

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;FINAL_DOS_LOCATION: ; 20/04/2019 - Retro DOS v4.0
;	dw	0
;MSDOS 5.0 IO.SYS - SYSINIT:0271h

CURRENT_DOS_LOCATION:
	dw	0

;DOSSIZE: ; Retro DOS 2.0 feature - 25/02/2018
;	dw	0   ; 'MSDOS.BIN' kernel size in words

; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; (MSDOS 5.0 MSDOS.SYS size is 37394 bytes)
DOSSIZE	equ	0A000h	; (MSDOS 6.0 - SYSINIT1.ASM - 1991)

DEVICE_LIST:
	dd	0

; 04/06/2018 - Retro DOS v3.0
; 28/03/2018
;; MSDOS 3.3 - SYSINIT1.ASM - 24/07/1987
;
sysi_country:	
	dd	0 ; 5/29/86 Pointer to country table in DOS

; MSDOS 6.0
dos_segreinit:	dw	0,0	; room for dword

lo_doscod_size:	dw	0	; dos code size when in low mem
hi_doscod_size:	dw	0	; dos code size when in HMA

def_php:	dw	0

; M022--
; pointer for calling into Bios_Code for re-initializing segment values.
;  call with ax = new segment for Bios_Code. Notice that we'll
;  call it in its temporary home, cuz seg_reinit won't get moved to
;  the new home.

;Bios_Code	equ	KERNEL_SEGMENT  ; 0070h
; 21/10/2022
;DOSBIOCODESEG	equ	02C7h ; (MSDOS 5.0 IO.SYS)

; 22/10/2022
seg_reinit_ptr:	; label dword
		dw	seg_reinit ; Bios_Code:0032h for MSDOS 6.21 IO.SYS
temp_bcode_seg:
		;dw	Bios_Code  ; 02CCh for MSDOS 6.21 IO.SYS
		; 22/10/2022
		dw	DOSBIOCODESEG ; 02C7h for MSDOS 5.0 IO.SYS 		

fake_floppy_drv:
		db	0	; set to 1 if this machine
				; does not have any floppies!!!

; Internal Stack Parameters

stack_count:	dW	defaultcount ; 9
stack_size:	dw	defaultsize  ; 128
stack_addr:	dd	0	

; 05/06/2018 - Retro DOS v3.0

; various default values

MEMORY_SIZE:	dw	1

; 22/03/2019 - Retro DOS v4.0 (MSDOS 6.0 source, MSDOS 6.21 disassembled src.)

RPLMemTop:	dw	0  ; 22/10/2022 (MSDOS 5.0 IO.SYS SYSINIT:0294h)	
DEFAULT_DRIVE:	db	0	;initialized by ibminit.
buffers:	dw	0FFFFh	; initialized during buffer allocation
h_buffers:	dw	0	; # of the heuristic buffers. initially 0.
singlebuffersize: dw	0	; maximum sector size + buffer head

FILES:	db	8	; enough files for pipe
FCBS:	db	4	; performance for recycling
KEEP:	db	0	; keep original set
NUM_CDS: db	5	; 5 net drives
; 22/10/2022 (MSDOS 5.0 SYSINIT)
;CONFBOT: dw	0
;ALLOCLIM: dw	0
CONFBOT: ; 02/11/2022
top_of_cdss: dw 0
; 02/11/2022 (MSDOS 5.0 SYSINIT)
ALLOCLIM: dw	0	; (SYSINIT:02A3h)	

DirStrng: db	"A:\",0	; string for the root directory of a drive
; 30/10/2022 (MSDOS 5.0 IO.SYS SAYSINIT compatibility)
; (SYSINIT:02A9h)
command_line:
	db	2,0
	db	'P'
	db	0
	times	124 db 0 ; db 124 dup(0)	
; (SYSINIT:0329h)
ZERO:	db	0
sepchr:	db	0
linecount: dw	0			; line count in config.sys
showcount: db	'     ',cr,lf,'$'	; used to convert linecount to ascii.
buffer_linenum: dw	0		; line count for "buffers=" command if entered.

sys_model_byte:	db	0FFh		; model byte used in sysinit
sys_scnd_model_byte: db 0		; secondary model byte used in sysinit

buf_prev_off:	dw	0

        ;IF      NOT NOEXEC
;COMEXE EXEC0 <0,COMMAND_LINE,DEFAULT_DRIVE,ZERO>
        ;ENDIF

; 01/05/2018
COMEXE:
EXEC0.ENVIRON:	dw	0	; seg addr of environment
EXEC0.COM_LINE:	dw	command_line ; pointer to asciz command line
		dw	0 	; SYSINIT segment (0473h for MSDOS 6.21 IO.SYS)
EXEC0.5C_FCB:	dw	DEFAULT_DRIVE ; default fcb at 5C
		dw	0	; SYSINIT segment (0473h for MSDOS 6.21 IO.SYS)
EXEC0.6C_FCB:	dw	ZERO	; default fcb at 6C
		dw	0

; variables for install= command.

multi_pass_id:	db	0		; parameter passed to multi_pass
					;  indicating the pass number
					; 0 - do scan for DOS=HIGH/LOW
					; 1 - load device drivers
					; 2 - was to load IFS
					;      now it is unused
					; 3 - do install=
					; >3 - nop
install_flag:	dw	0

have_install_cmd equ	00000001b	; config.sys has install= commands
has_installed	equ	00000010b	; sysinit_base installed.

config_size:	dw	0		; size of config.sys file. set by sysconf.asm
sysinit_base_ptr: dd	0		; pointer to sysinit_base
sysinit_ptr:	dd	0		; returning addr. from sysinit_base
checksum:	dw	0		; used by sum_up

ldexec_fcb:	times 20 db 20h ; db 20 dup (' ') ;big enough
ldexec_line:	db	0		;# of parm characters
ldexec_start:	db	' '
ldexec_parm:	times 80 db 0	; db 80 dup (0)

;instexe exec0	<0,ldexec_line,ldexec_fcb,ldexec_fcb>

instexe:
iexec.environ:	dw	0		; seg addr of environment
iexec.ldexec_line: dw	ldexec_line ; pointer to asciz command line
		dw	0 	; SYSINIT segment (0473h for MSDOS 6.21 IO.SYS)
iexec.ldexec_5c_fcb: dw	ldexec_fcb	; default fcb at 5C
		dw	0	; SYSINIT segment (0473h for MSDOS 6.21 IO.SYS)
iexec.ldexec_6c_fcb: dw	ldexec_fcb	; default fcb at 6C
		dw	0

; variables for comment=

com_level:	db	0		; level of " " in command line
cmmt:		db	0		; length of comment string token
cmmt1:		db	0		; token
cmmt2:		db	0		; token
cmd_indicator:	db	0
donotshownum:	db	0

count:		dw	0
org_count:	dw	0
chrptr:		dw	0
cntryfilehandle: dw	0
old_area:	dw	0
impossible_owner_size: dw 0		; paragraph

bucketptr: ; label dword
bufptr:	   ; label dword		; leave this stuff in order!
memlo:	dw	0
prmblk:	   ; label word
memhi:	dw	0
ldoff:	dw	0
area:	dw	0

; Following is the request packet used to call INIT routines for 
; all device drivers. Some fields may be accessed individually in
; the code, and hence have individual labels, but they should not
; be separated.

packet:	db	24			; was 22
	db	0
	db	0			; initialize code
	dw	0
	times	8 db 0	; db 8 dup (?)

unitcount:	db	0
break_addr:	dd	0
bpb_addr:	dd	0
drivenumber:	; 22/10/2022
devdrivenum:	db	0 
configmsgflag:	dw	0  ; used to control "error in config.sys line #" message

; end of request packet

;drivenumber:	db	0  ; 22/03/2019

toomanydrivesflag:
		db	0  ; >24 fixed disk partitions flag ; M029 
align 2

BCodeSeg:	; 21/10/2022
	dw	DOSBIOCODESEG ; (02C7h for MSDOS 5.0 IO.SYS)
	;dw	Bios_Code ; = KERNEL_SEGMENT = 0070h (for Retro DOS v4.0)
			   ; BCodeSeg = 2CCh (for MSDOS 6.21 IO.SYS)

; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
;; 19/04/2019
;_timer_lw_:
;	dw	0  ; MSDOS 6.21 IO.SYS - SYSINIT:038Ch
	
;SR;
; This is the communication block between the DOS and the BIOS. It starts at
;the SysinitPresent flag. Any other data that needs to be communicated 
;to the DOS should be added after SysinitPresent. The pointer to this block
;is passed to DOS as part of the DOSINIT call.
;

BiosComBlock:
	;dd	Bios_Data:SysinitPresent 
		; 0070h:08FDh for MSDOS 6.21 IO.SYS
	dw	SysinitPresent  ; (retrodos4.sys, offset: ****h)
	;dw	KERNEL_SEGMENT ; 0070h
	; 21/10/2022
	dw	DOSBIODATASEG ; 0070h

;align 2

	; 22/10/2022 - (MSDOS 5.0 IO.SYS, SYSINIT:0406h)
tempstack:	
	times	128 db 0  ; db	80h dup (?)

; ----------------------------------------------------------------------------

	; 22/10/2022 - Retro DOS v4.0
	;	; (MSDOS 5.0 IO.SYS, SYSINIT:0486h)
GOINIT:		; (MSDOS 6.21 IO.SYS, SYSINIT:0412h)
	; 22/03/2019 - Retro DOS v4.0
	; 06/07/2018
	; 04/06/2018 - Retro DOS v3.0
; before doing anything else, let's set the model byte
	mov	ah,0C0h 		; get system configuration
	int	15h			; *
	jc	short no_rom_config

	cmp	ah,0			; double check
	jne	short no_rom_config
	mov	al,[es:bx+ROMBIOS_DESC.bios_sd_modelbyte]
	mov	[cs:sys_model_byte],al 
	mov	al,[es:bx+ROMBIOS_DESC.bios_sd_scnd_modelbyte]
	mov	[cs:sys_scnd_model_byte],al

	;jmp	short SYSIN
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short move_myself

no_rom_config:				; Old ROM
	mov	ax,0F000h
	mov	ds,ax
	mov	al,[0FFFEh]
	mov	[cs:sys_model_byte],al	; set the model byte.

; set fake_floppy_drv if there is no diskette drives in this machine.
; execute the equipment determination interrupt and then
; check the returned value to see if we have any floppy drives
; if we have no floppy drive we set cs:fake_floppy_drv to 1
; see the at tech ref bios listings for help on the equipment
; flag interrupt (11h)	

	; 22/10/2022
check_for_fake_floppy:			; entry point for rom_config above
	int	11h			; check equipment flag

	test	ax,1			; have any floppies?
	jnz	short move_myself	; yes,normal system

; Some ROM BIOSs lie that there are no floppy drives. Lets find out
; whether it is an old ROM BIOS or a new one
;
; WARNING !!!
;
; This sequence of code is present in MSINIT.ASM also. Any modification
; here will require an equivalent modification in MSINIT.ASM also

	push	es

	xor	cl,cl	
	mov	ah,8			; get disk parameters
	mov	dl,0			; of drive 0
	int	13h

	pop	es

	jc	short move_myself	; if error lets assume that the
					;  ROM BIOS lied
	cmp	cl,0			; double check (max sec no cannot be 0
	je	short move_myself

	or	dl,dl			; number of flp drvs == 0?
	jnz	short move_myself

	mov	byte [cs:fake_floppy_drv],1 ; set fake flag.

move_myself:
	cld				; set up move
	xor	si,si
	mov	di,si

	mov	cx,[cs:MEMORY_SIZE]

	; (MSDOS 6.0 - SYSINIT1.ASM - 1991)
;;;	if	msver
;	cmp	cx,1		; 1 means do scan
;	jnz	short noscan
;	mov	cx,2048		; start scanning at 32k boundary
;	xor	bx,bx
;
;memscan:inc	cx
;	jz	setend
;	mov	ds,cx
;	mov	al,[bx]
;	not	al
;	mov	[bx],al
;	cmp	al,[bx]
;	not	al
;	mov	[bx],al
;	jz	short memscan
;setend:
;	mov	cs:[memory_size],cx
;;;	endif

;noscan: 				; cx is mem size in para
;;
;;	cas -- a) if we got our memory size from the ROM, we should test it
;;		  before we try to run.
;;	       b) in any case, we should check for sufficient memory and give
;;		  an appropriate error diagnostic if there isn't enough
;
;	push	cs
;	pop	ds
;
;;	cas note:  It would be better to put dos + bios_code BELOW sysinit
;;	  that way it would be easier to slide them down home in a minimal
;;	  memory system after sysinit.  As it is, you need room to keep
;;	  two full non-overlapping copies, since sysinit sits between the
;;	  temporary home and the final one.  the problem with doing that
;;	  is that sys*.asm are filled with "mov ax,cs, sub ax,11h" type stuff.
;
;	dec	cx			; one para for an arena at end of mem
;					; in case of UMBs

	; 22/10/2022
	; (MSDOS 5.0 IOSYSY SYSINIT:04DBh)

	push	cs
	pop	ds
	dec	cx

;------ Check if an RPL program is present at TOM and do not tromp over it

	xor	bx,bx
	mov	es,bx
	mov	bx,[es:(2Fh*4)] ; INT 2Fh address (0:0BCh)
	mov	es,[es:((2Fh*4)+2)] ; INT 2Fh segment (0:0BEh)
	cmp	word [es:bx+3],'RP'
	jne	short NoRPL
	cmp	byte [es:bx+5],'L'
	jne	short NoRPL

	mov	dx,cx			; get TOM into DX
	push	dx
	mov	ax,4A06h
	;mov	ax,(multMULT<<8)+multMULTRPLTOM
	int	2Fh			; Get new TOM from any RPL
	pop	ax
	mov	cx,dx
	cmp	dx,ax
	je	short NoRPL
	
	;mov	[RPLMemTop],dx
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	[cs:RPLMemTop],dx
	
	dec	cx
NoRPL:
	mov	ax,SI_end		; need this much room for sysinit
					; (SI_end == sysinit code size)
	call	off_to_para
	sub	cx,ax

; we need to leave room for the DOS and (if not ROMDOS) for the BIOS
; code above sysinit in memory
;
	sub	cx,DOSSIZE/16		; leave this much room for DOS

	mov	ax,BCODE_END 		; (1A60h for MSDOS 5.0 IO.SYS)
	call	off_to_para		; leave this much room for BIOS code
	sub	cx,ax
	mov	es,cx			; offset where sysinit will be located

	mov	cx,SI_end		; (sysinit code size)
	shr	cx,1			; divide by 2 to get words
	rep	movsw			; relocate sysinit

	push	es			; push relocated segment
	mov	ax,SYSIN
	push	ax			; push relocated entry point

	retf				; far jump to relocated sysinit

; ----------------------------------------------------------------------------

;	MOVE THE DOS TO ITS PROPER LOCATION

	; 22/10/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0533h)
SYSIN:
	; Retro DOS 4.0 - 22/03/2019
	; Retro DOS 2.0 - 25/02/2018

	; 23/04/2019
	;;mov	ax,Bios_Data
	;mov	ax,KERNEL_SEGMENT ; 0070h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax
	mov	[MoveDOSIntoHMA+2],cs	; set seg of routine to move DOS
	mov	byte [SysinitPresent],1	; flag that MoveDOSIntoHMA can be called

; first move the MSDOS.SYS image up to a harmless place 
; on top of our new sysinitseg

	; 22/10/2022
	mov	ax,SI_end		; how big is sysinitseg?
	call	off_to_para
	mov	cx,cs			; pick a buffer for msdos above us
	add	ax,cx
	mov	es,ax
	
	xor	si,si
	mov	di,si

	mov	ds,[cs:CURRENT_DOS_LOCATION] ; where it is (set by msinit)

	;mov	ax,cs	
	;mov	ds,ax

	;;;mov	cx,20480  ; MSDOS 6.21 IO.SYS - SYSINIT:04E2h
	;;mov	cx,dossize/2 ; MSDOS 6.0
	;mov	cx,[DOSSIZE] ; words (not bytes!)  ; Retro DOS v4.0 (3.0, 2.0)
	;mov	es,[FINAL_DOS_LOCATION] ; on top of SYSINIT code
	;mov	ds,[CURRENT_DOS_LOCATION]

	; 22/10/2022
	mov	cx,DOSSIZE/2 ; 5000h
	rep     movsw
	mov	[cs:CURRENT_DOS_LOCATION],es

; The DOS code is ORGed at a non-zero value to allow it to be located in
; HIMEM. Thus, the DOS segment location must be adjusted accordingly.
; If this is ROMDOS, however, only the init code is loaded into RAM, so
; this ORG is not done. The entry point is at offset zero in the segment.

	; 22/04/2019 (MSDOS 6.0 & MSDOS 6.21 kernel address modification)
	;mov	ax,cs
	; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)
	;mov	ds,ax

; 29/04/2019 - Retro DOS v4.0 ! important MODIFICATION !

;	; 24/04/2019 
;;ifndef ROMDOS
;	mov	ax,[es:3] 		; get offset of dos
;		; ax = 3DE0h for MSDOS 6.21 kernel (MSDOS.SYS, offset 3) 
;	mov	[dosinit],ax		; that's the entry point offset
;	call	off_to_para		; subtract this much from segment
;	; 23/04/2019
;	;sub	[CURRENT_DOS_LOCATION],ax
;	sub	[FINAL_DOS_LOCATION],ax
;;else
;;	mov	word [dosinit],0	; entry to init is at zero
;;
;;endif ; ROMDOS

	; 29/04/2019 - Retro DOS v4.0 ! important MODIFICATION !
	; (! MSDOS6.BIN starts with DOSDATA ! - Retro DOS v4.0 modification) 

	;mov	ax,[es:0] ; DOSCODE start address = DOSDATA size (= 136Ah)
	;		  ; (Valid for Retro DOS v4.0 only!)

	; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)
	; (SYSINIT:0563h for MSDOS 5.0 IO.SYS SYSINIT)
	mov	ax, [3]		; mov ax, word ptr ds:3

	mov	[cs:dosinit],ax ; (SYSINIT:0563h for MSDOS 5.0 IO.SYS SYSINIT)
	; 02/11/2022
	call	off_to_para		; subtract this much from segment
	sub	[cs:CURRENT_DOS_LOCATION],ax

	; Current DOSCODE start address = dword [dosinit]

;; If this is not ROMDOS, then the BIOS code is moved to the top of memory
;; until it is determined whether it will be running in HIMEM or not.

;ifndef ROMDOS

; now put Bios_Code up on top of that. Assume Bios_Code + dossize < 64k

	; 22/10/2022
	mov	ax,es
	add	ax,DOSSIZE/16		; get paragraph of end of dos
	mov	es,ax
	xchg	ax,[cs:temp_bcode_seg]	; swap with original home of Bios_Code
	mov	ds,ax			; point to loaded image of Bios_Code

	mov	si,BCODE_START ; mov si,30h
	; 02/11/2022
	mov	di,si
	mov	cx,BCODE_END   ; mov cx,1A60h
	sub	cx,si
	shr	cx,1
	rep	movsw			; move Bios_Code into place

	mov	ax,es			; tell it what segment it's in
	call	far [cs:seg_reinit_ptr]	; far call to seg_reinit in Bios_Code (M022)

;endif	; not ROMDOS

; now call dosinit while it's in its temporary home

	;mov	ax,cs
	;mov	ds,ax	 

	;mov	dx,[MEMORY_SIZE]	; set for call to dosinit

	; 22/10/2022

	les	di,[cs:BiosComBlock]	; ptr to BIOS communication block
		; es = KERNEL_SEGMENT (70h), di = 'SysInitPresent' address
	lds	si,[cs:DEVICE_LIST]	; set for call to dosinit
		; ds = KERNEL_SEGMENT (70h), si = 'res_dev_list' address

	mov	dx,[cs:MEMORY_SIZE]	; set for call to dosinit

	cli
	mov	ax,cs
	mov	ss,ax

; 22/03/2019 - Retro DOS v4.0 (MSDOS 6.0, SYSINIT1.ASM)
%define locstack ($ - SYSINIT$) & 0FFFEh  ; 532h in MSDOS 6.21 IO.SYS
					  ; 5A6h in MSDOS 5.0 IO.SYS SYSINIT
;SYSINIT:0532h:

; 22/10/2022
; ----------------------------------------------------------------------------
;SYSINIT:05A6h:
;locstack:	; (at SYSINIT:05A6h for MSDOS 5.0 IO.SYS)

	;mov	sp, 05A6h
	mov     sp,locstack		; set stack

	sti

;align 2
	; 30/03/2018
;LOCSTACK:
        ;CALL	FAR [CS:MSDOS]	; FINAL_DOS_LOCATION:0 
		       		;('jmp DOSINIT' in 'MSHEAD.ASM')
		       		;('DOSINIT:' is in 'MSINIT.ASM')

	; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; 22/03/2019 - Retro DOS v4.0 (MSDOS 6.0, 6.21)

; This call to DOSINIT will relocate the DOS data from its present location
; at the top of memory, to its final location in low memory just above the
; BIOS data. It will then build important DOS data structures in low 
; memory following the DOS data. It returns (among many other things) the
; new starting address of free memory.

	call	far [cs:dosinit]	; call dosinit	
			 ; es:di -> sysinitvars_ext

	mov	[cs:def_php],ds		; save pointer to PSP

	; 22/03/2019
	;push	cs
	;pop	ds
	; 22/10/2022
	;mov	[hi_doscod_size],ax
	;mov	[lo_doscod_size],cx
	;mov	[dos_segreinit],dx

	mov	[cs:hi_doscod_size],ax	; size of doscode (including exepatch)
	mov	[cs:lo_doscod_size],cx	; (not including exepatch)
	mov	[cs:dos_segreinit],dx	; save offset of segreinit

	; 05/06/2018 - Retro DOS v3.0
	; ES:DI = Address of pointer to SYSINITVARS structure (MSDOS 3.3)

	; 22/10/2022
	;mov	ax,[es:di+SysInitVars_Ext.SYSI_InitVars] ; 5/29/86
	mov	ax,[es:di] ; 22/03/2019
	mov	[cs:DOSINFO],ax
	;mov	[DOSINFO],ax
	;mov	ax,[es:di+SysInitVars_Ext.SYSI_InitVars+2]
	mov	ax,[es:di+2]
	mov	[cs:DOSINFO+2],ax
	;mov	[DOSINFO+2],ax	; set the sysvar pointer

	;mov	ax,[es:di+SysInitVars_Ext.SYSI_Country_Tab]
	mov	ax,[es:di+4]
	mov	[cs:sysi_country],ax
	;mov	[sysi_country],ax
	;mov	ax,[es:di+SysInitVars_Ext.SYSI_Country_Tab+2]
	mov	ax,[es:di+6]
	mov	[cs:sysi_country+2],ax
	;mov	[sysi_country+2],ax	; set the SYSI_Country pointer

	; 20/04/2019
	;mov	ax,[CURRENT_DOS_LOCATION]
	;;mov	es,[CURRENT_DOS_LOCATION]
	;mov	ax,[FINAL_DOS_LOCATION] ; give dos its temporary location
	; 22/10/2022
	;mov	ax,[cs:CURRENT_DOS_LOCATION]
	;;;mov	[dos_segreinit+2],es
	;;mov	[dos_segreinit+2],ax
	;mov	[cs:dos_segreinit+2],ax
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	es,[cs:CURRENT_DOS_LOCATION]
	mov	[cs:dos_segreinit+2],es

; ----------------------------------------------------------------------------

;SYSINIT:0577h:
	; ... RPLArena ... MSDOS 6.21 IO.SYS (SYSINIT:0577h to SYSINIT:05D1h)
;SYSINIT:05D1h:	; NoPRLArena 

	; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS SYSINIT)
;------ Cover up RPL code with an arena
;SYSINIT:05EBh:
	cmp	word [cs:RPLMemTop],0
	je	short NoRPLArena

;------ alloc all memory

	mov	bx,0FFFFh
	mov	ah,48h
	int	21h
			; DOS - 2+ - ALLOCATE MEMORY
			; BX = number of 16-byte paragraphs desired
	mov	ah,48h
	int	21h

	mov	es,ax			; get it into ES and save it
	push	es

;------ resize upto RPL mem

	sub	ax,[cs:RPLMemTop]
	neg	ax
	dec	ax
	mov	bx,ax
	mov	ah,4Ah
	int	21h
  			; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
			; ES = segment address of block to change
			; BX = new size in paragraphs

;------ allocate the free (RPL MEM)

	mov	bx,0FFFFh
	mov	ah,48h
	int	21h
	mov	ah,48h
	int	21h

;----- mark that it belongs to RPL

	dec	ax
	mov	es,ax
	;mov	word [es:arena_owner],8
	mov	word [es:1],8
	;mov	word [es:arena_name],'RP'
	mov	word [es:8],'RP'
	;mov	word [es:arena_name+2], 'L'
	mov	word [es:10],'L'
	;mov	word [es:arena_name+4], 0
	mov	word [es:12], 0
	;mov	word [es:arena_name+6], 0
	mov	word [es:14], 0	

        pop     es                      ; get back ptr to first block
        mov     ah, 49h	; Dealloc	; and free it
	int	21h		
					; DOS - 2+ - FREE MEMORY
					; ES = segment address of area to be freed

; ----------------------------------------------------------------------------

NoRPLArena:

	; 22/03/2019 - Retro DOS v4.0 (MSDOS 6.0, 6.21, IO.SYS)
	;les	di,[DOSINFO]	; es:di -> dosinfo
	; 22/10/2022 - Retro DOS v4.0 (MSDOS 5.0 IO.SYS SYSINIT)
	les	di,[cs:DOSINFO]	; es:di -> dosinfo

	clc				; get the extended memory size

;	execute the get extended memory size subfunction in the bios int 15h
;	if the function reports an error do nothing else store the extended
;	memory size reported at the appropriate location in the dosinfo buffer
;	currently pointed to by es:di. use the offsets specified in the
;	definition of the sysinitvars struct in inc\sysvar.inc

	mov	ah,88h
	int	15h			; check extended memory size
	jc	short no_ext_memory
			; Get Extended Memory Size
			; Return: CF clear on success
			; AX = size of memory above 1M in K	
	;mov	[es:di+SYSI_EXT_MEM],ax ; save extended memory size
	; 22/10/2022
	mov	[es:di+45h],ax ; save extended memory size
	or	ax,ax
	jz	short no_ext_memory
	call	ClrVDISKHeader
no_ext_memory:
	;mov	ax,[es:di+SYSI_MAXSEC]	; get the sector size
	mov	ax,[es:di+10h]
	;add	ax,bufinsiz
	add	ax,20			; size of buffer header
	;mov	[singlebuffersize],ax	; total size for a buffer
	mov	[cs:singlebuffersize],ax	

	;mov	al,[DEFAULT_DRIVE]	; get the 1 based boot drive number set by msinit
	mov	al,[cs:DEFAULT_DRIVE]
	;mov	[es:di+SYSI_BOOT_DRIVE],al ; set sysi_boot_drive
	mov	[es:di+43h],al

; determine if 386 system...

	;get_cpu_type			; macro to determine cpu type

get_cpu_type:
	pushf
	push    bx
	xor     bx,bx
	xor     ax,ax
	push    ax
	popf
	pushf
	pop     ax
	and     ax,0F000h
	cmp     ax,0F000h
	je      short cpu_8086
	mov     ax,0F000h
	push    ax
	popf
	pushf
	pop     ax
	and     ax,0F000h
	jz      short cpu_286
cpu_386:
	inc     bx
cpu_286:
	inc     bx
cpu_8086:
	mov     ax,bx
	pop     bx
	popf

	;...

	cmp	ax,2			; is it a 386?
	jne	short not_386_system	; no: don't mess with flag
	;mov	byte [es:di+SYSI_DWMOVE],1
	; 22/10/2022
	mov	byte [es:di+44h],1
not_386_system:
	;mov	al,[es:di+SYSI_NUMIO]
	mov	al,[es:di+20h]
	;mov	[drivenumber],al	; save start of installable block drvs
	mov	[cs:drivenumber],al

	mov	ax,cs
	sub	ax,11h			; room for PSP we will copy shortly
	;mov	cx,[singlebuffersize]	; temporary single buffer area
	mov	cx, [cs:singlebuffersize]
	shr	cx,1			
	shr	cx,1			; divide size by 16...
	shr	cx,1
	shr	cx,1			; ...to get paragraphs...
	inc	cx			; ... and round up

;	cas note: this unorthodox paragraph rounding scheme wastes a byte if
;	  [singlebuffersize] ever happens to be zero mod 16. Could this
;	  ever happen? Only if the buffer overhead was zero mod 16, since
;	  it is probably safe to assume that the sector size always will be.
;
;	 mohans also found a bug in CONFIG.SYS processing where it replaces
;	  EOF's with cr,lf's, without checking for collision with [confbot].
;	  perhaps the extra byte this code guarantees is what has kept that
;	  other code from ever causing a problem???

	sub	ax,cx
	;mov	[top_of_cdss],ax	; temp "unsafe" location
	; 22/10/2022
	mov	[cs:top_of_cdss],ax

;	chuckst -- 25 Jul 92 -- added code here to pre-allocate space
;	for 26 temporary CDSs, which makes it easier to use alloclim
;	for allocating memory for MagicDrv.

	push	es			; preserve pointer to dosinfo
	push	di

	; 22/10/2022
;	mov	cx,ax			; save pointer for buffer
;
;;	now allocate space for 26 CDSs
;
;	sub	ax,((26 *(curdirlen))+15)/16
;	mov	[ALLOCLIM],ax		; init top of free memory pointer
;	mov	[CONFBOT],ax		; init this in case no CONFIG.SYS

; setup and initialize the temporary buffer at cx

	;les	di,[es:di+SYSI_BUF]	; get the buffer chain entry pointer
	les	di,[es:di+12h]
	;xor	ax,ax
	;mov	[es:di+BUFFINF.Dirty_Buff_Count],ax ; 0
	mov	word [es:di+4],0
	;mov	[es:di+BUFFINF.Buff_Queue],ax ; 0
	mov	word [es:di],0
	;mov	[es:di+BUFFINF.Buff_Queue+2],cx
	mov	[es:di+2],ax

	;mov	es,cx
	mov	es,ax

	xor	ax,ax
	mov	di,ax			; es:di -> single buffer

	;mov	[es:di+buffinfo.buf_next],ax ; points to itself
	mov	[es:di],ax
	;mov	[es:di+buffinfo.buf_prev],ax ; points to itself
	mov	[es:di+2],ax

	; 22/10/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS SYINIT)
	; MSDOS 5.0 IO.SYS - SYSINIT:06E0h

	;mov	word [es:di+buffinfo.buf_ID],00FFh ; free buffer,clear flag
	mov	word [es:di+4],00FFh
;SYSINIT:06E6h
	;mov	[es:di+buffinfo.buf_sector],ax ; 0
	mov	word [es:di+6],0
	;mov	[es:di+buffinfo.buf_sector+2],ax ; 0
	mov	word [es:di+8],0

	pop	di			; restore pointer to DOSINFO data
	pop	es

	; 22/10/2022
	push	cs
	pop	ds

	call	TempCDS 		; set up cdss so re_init and sysinit
					;   can make disk system calls
					; tempcds trashes ds
	; 10/05/2019
	mov	ds,[cs:def_php]		; retrieve pointer to PSP returned by DOSINIT

	;if not ibmjapver
	;call	far KERNEL_SEGMENT:re_init ; re-call the bios
	;endif

	; 22/10/2022
;SYSINIT:06FEh:	; (MSDOS 5.0 IO.SYS, SYSINIT)
	;call	far ptr 70h:89Bh
	call	DOSBIODATASEG:RE_INIT

	sti				; ints ok
	cld				; make sure

; 23/03/2019

;SYSINIT:069Ch

; dosinit has set up a default "process" (php) at ds:0. we will move it out
; of the way by putting it just below sysinit at end of memory.

	mov	bx,cs
	sub	bx,10h
	mov	es,bx
	xor	si,si
	mov	di,si
	mov	cx,128
	rep	movsw

	;mov	[es:PDB.JFN_POINTER+2],es ; Relocate
	; 22/10/2022
	mov	[es:36h],es

 	; Set Process Data Block - Program Segment Prefix address
	; BX = PDB/PSP segment
        mov	ah,50h ; SET_CURRENT_PDB
	int	21h			; tell DOS we moved it
			; DOS - 2+ internal - SET PSP SEGMENT
			; BX = segment address of new PSP
	; 22/10/2022
	; 27/03/2019
	push	ds ; */			; preserve DS returned by DOSINIT

	push	cs	
	pop	ds

	; set up temp. critical error handler
	mov	dx,int24		; set up int 24 handler
	;;mov	ax,(SET_INTERRUPT_VECTOR*256)+24h
	;mov	ax,(SET_INTERRUPT_VECTOR<<8)|24h
	mov	ax,2524h
	int	21h

        cmp     byte [toomanydrivesflag],0 ; Q: >24 partitions?      M029
        je      short no_err		   ;  N: continue            M029
        mov     dx,TooManyDrivesMsg	   ;  Y: print error message M029
        ; 22/10/2022
	call	print 			   ;		             M029
	;jmp	short p_dosinit_msg ; 23/03/2019 - Retro DOS v4.0                    
no_err:
	; 12/05/2019
	;----------------------------------------------
	; 27/06/2018 - Retro DOS v3.0	; 23/03/2019 - Retro DOS v4.0
	; 22/10/2022 - Retro DOS v4.0
	;mov	dx,BOOTMES		; Display (fake) MSDOS version message
;p_dosinit_msg:
	;call	print			; Print message
	;----------------------------------------------
	
	; 22/10/2022
	; 23/03/2019 - Retro DOS v4.0
	pop	ds			; start of free memory
	mov	dl,[cs:DEFAULT_DRIVE]
	
	; 27/03/2019
	;mov	dl,[DEFAULT_DRIVE]	
	;pop	ds ; */

	or	dl,dl
	;jz	short nodrvset		; bios didn't say
	jz	short ProcessConfig  ; (Retro DOS v4.0 does not contain DBLSPACE code)
	dec	dl			; A = 0
	mov	ah, 0Eh ; SET_DEFAULT_DRIVE
	int	21h			; select the disk
			; DOS - SELECT DISK
			; DL = new default drive number (0 = A, 1 = B, etc.)
			; Return: AL = number of logical drives
nodrvset:
	;ifdef	dblspace_hooks
	;	....
	;	....
	;endif	

; MSDOS 6.21 IO.SYS, SYSINIT:0744h

; 23/03/2019 - Retro DOS v4.0 (MSDOS 6.0, SYSINIT1.ASM, 1991)
; ----------------------------------------------------------------------------
; 22/10/2022 - Retro DOS v4.0 (MSDOS 5.0 IO.SYS SYSINIT)

ProcessConfig:
	; ds = cs ; 27/03/2019	

; (MSDOS 5.0 IO.SYS - SYSINIT:0746h)

	call	doconf			; do pre-scan for dos=high/low

	; 27/03/2019
	; ds = cs (at return from doconf)

; Now, if this is not romdos, we decide what to do with the DOS code.
; It will either be relocated to low memory, above the DOS data structures,
; or else it will be located in HiMem, in which case a stub with the DOS
; code entry points will be located in low memory. Dos_segreinit is used
; to tell the DOS data where the code has been placed, and to install the
; low memory stub if necessary. If the DOS is going to go into HiMem, we
; must first initialize it in its present location and load the installable
; device drivers. Then, if a HiMem driver has been located, we can actually
; relocate the DOS code into HiMem.
;
; For ROMDOS, if DOS=HIGH is indicated, then we need to call dos_segreinit
; to install the low memory stub (this must be done before allowing any
; device drivers to hook interrupt vectors). Otherwise, we don't need to 
; call dos_segreinit at all, since the interrupt vector table has already 
; been patched.

	; 22/10/2022 - Retro DOS v4.0
	; (MSDOS 5.0 IO.SYS - SYSINIT:0749h)
	cmp	byte [cs:runhigh],0	; Did user choose to run low ?
	;cmp	byte [runhigh],0
	je	short dont_install_stub	; yes, don't install dos low mem stub

;------ user chose to load high

	; 22/10/2022
	mov	es,[cs:CURRENT_DOS_LOCATION] ; MSDOS 6.21 (& MSDOS 6.0) 
	;mov	es,[cs:FINAL_DOS_LOCATION]   ; Retro DOS v4.0
	; 27/03/2019
	;;mov	es,[FINAL_DOS_LOCATION]

	xor	ax,ax			; ax = 0 ---> install stub
	call	far [cs:dos_segreinit]	; call dos segreinit
	;call	far [dos_segreinit]

	jmp	short do_multi_pass

;------ User chose to load dos low

dont_install_stub:
	; 22/10/2022
	xor	bx,bx			; M012
					; don't use int 21 call to alloc mem
	call	MovDOSLo		; move it !

	mov	ax,1			; dont install stub
	mov	es,[cs:CURRENT_DOS_LOCATION] ; set_dos_final_position set it up
	;mov	es,[cs:FINAL_DOS_LOCATION]   ; Retro DOS v4.0
	; 27/03/2019
;do_multi_pass:
	;mov	es,[FINAL_DOS_LOCATION] 

	call	far [cs:dos_segreinit]	; inform dos about new seg
	;call	far [dos_segreinit]
do_multi_pass:
	call	AllocFreeMem		; allocate all the free mem
					; & update [memhi] & [area]
					; start of free memory.
	;ifdef	dblspace_hooks
	;mov	bx,0			; magic backdoor to place int hooks
	;call	cs:MagicBackdoor
	;endif

; Now, process config.sys some more.  
; Load the device drivers and install programs

	; 22/10/2022
	inc	byte [cs:multi_pass_id]	; multi_pass_id = 1
	;inc	byte [multi_pass_id]
	call	multi_pass		; load device drivers
	call	ShrinkUMB
	call	UnlinkUMB		; unlink all UMBs	;M002
	; 02/11/2022
	inc	byte [cs:multi_pass_id]	; multi_pass_id = 2
	;inc	byte [multi_pass_id]
	call	multi_pass		; was load ifs (now does nothing)

	;ifdef	dblspace_hooks
	;call	MagicPostload		; make sure Magicdrv is final placed
	;endif

	; ds = cs
	
	call	endfile			; setup fcbs, files, buffers etc

	;ifdef	dblspace_hooks
	;call	MagicSetCdss		; disable CDSs of reserved drives
	;endif

;Reset SysinitPresent flag here. This is needed for the special fix for lying
;to device drivers. This has been moved up to this point to avoid problems 
;with overlays called from installed programs

	;;mov	ax,Bios_Data ; 0070h
	;mov	ax,KERNEL_SEGMENT
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	es,ax			; point ES to bios data

	mov	byte [es:SysinitPresent],0 ; clear SysinitPresent flag

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	test	word [cs:install_flag],have_install_cmd ; 1
	;;test	byte [cs:install_flag], 1
	;test	byte [cs:install_flag],have_install_cmd
					; are there install commands?
	jz	short dolast		; no, no need for further processing
	inc	byte [cs:multi_pass_id]	; mult_pass_id = 3
	;inc	byte [multi_pass_id]
	call	multi_pass		; execute install= commands

dolast:
	
; [area] has the segment address for the allocated memory of sysinit, confbot.
;  free the confbot area used for config.sys and sysinit itself.

; Now if DOS is supposed to run high, we actually move it into high memory 
; (if HiMem manager is available). For ROMDOS, we don't actually move
; anything, but just set up the ROM area for suballocation (or print
; a message if HiMem is not available).
;
; There is also this little hack for CPM style DOS calls that needs to
; be done when A20 is set...

	; 22/10/2022
	cmp	byte [cs:runhigh],0FFh	; are we still waiting to be moved?
	;cmp	byte [runhigh],0FFh
	jne	short _@@		; no, our job is over
	call	LoadDOSHiOrLo
_@@:
	cmp	byte [cs:runhigh],0	; are we running low
	;cmp	byte [runhigh],0
	;je	short _@@@
	je	short ConfigDone	; yes, no CPM hack needed
	call	CPMHack			; make ffff:d0 same as 0:c0
_@@@:

; We are now done with CONFIG.SYS processing

ConfigDone:
	; 22/10/2022
	mov	byte [cs:donotshownum],1 
					; done with config.sys.
					; do not show line number message.
	mov	es,[cs:area]
	; 27/03/2019
	; ds = cs
	;mov	byte [donotshownum],1
	;mov	es,[area]

        mov     ah, 49h ; DEALLOC	; free allocated memory for command.com
	int	21h
			; DOS - 2+ - FREE MEMORY
			; ES = segment address of area to be freed

	; 22/10/2022
	;test	word [cs:install_flag],2
	test	word [cs:install_flag],has_installed ; sysinit_base installed?
	;test	byte [cs:install_flag],has_installed
	;test	byte [install_flag],has_installed
	jz	short skip_free_sysinitbase ; no.

; set block from the old_area with impossible_owner_size.
; this will free the unnecessary sysinit_base that had been put in memory to
; handle install= command.

        push    es                      ; BUGBUG 3-30-92 JeffPar: no reason to save ES
	push	bx
	; 22/10/2022
	mov	es,[cs:old_area]
	mov	bx,[cs:impossible_owner_size]
	;mov	es,[old_area]
	;mov	bx,[impossible_owner_size]
	mov	ah, 4Ah ; SETBLOCK
	int	21h
			; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
			; ES = segment address of block to change
			; BX = new size in paragraphs
	mov	ax,es
	dec	ax
	mov	es,ax			; point to arena
	;mov	word [es:ARENA.OWNER],8	; set impossible owner
	mov	word [es:1],8
	;mov	word [es:ARENA.NAME],'SD' ; 4453h ; System Data
	mov	word [es:8], 'SD'
	pop	bx
        pop     es                      ; BUGBUG 3-30-92 JeffPar: no reason to save ES

skip_free_sysinitbase:
	; 22/10/2022
	cmp	byte [cs:runhigh],0
	;cmp	byte [runhigh],0	
	je	short _@@@@
	call	InstVDiskHeader		; Install VDISK header (allocates some mem from DOS)

; ----------------------------------------------------------------------------

_@@@@:
	; 22/10/2022
	; 27/03/2019
	push	cs
	pop	ds			; point DS to sysinitseg

; set up the parameters for command

	; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS SYSINIT)
;;ifdef  MULTI_CONFIG
;	mov	byte [config_cmd],0	; set special code for query_user
;       call    query_user		; to issue the AUTOEXEC prompt
;	jnc	short process_autoexec	; we should process autoexec normally
;	; !!!
;	or	byte [bQueryOpt],4 ; MSDOS 6.21 IO.SYS - SYSINIT:081Fh
;       ; !!!
;	call    disable_autoexec        ; no, we should disable it
;process_autoexec:
;;endif	; !!!
;	call	CheckQueryOpt	; MSDOS 6.21 IO.SYS - SYSINIT:0827h	
;	; !!!

	; 22/10/2022 
	;mov     cl,[command_line]
        ;mov     ch,0
        ;inc     cx
        ;mov     si,command_line	
	;add     si,cx
        ;mov     byte [si],cr	; cr-terminate command line

	; 22/10/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0809h)
	
	;mov	si, (offset command_line+1)
	mov	si, command_line+1
	push    ds
	pop     es
	mov     di, si
	mov     cl, 0FFh ; -1
_@_loop:                                  ; CODE XREF: seg000:0818j
	inc     cl ; +1
	lodsb
	stosb
	or      al, al
	jnz     short _@_loop
	dec     di
	mov     al, 0Dh
	stosb			; cr-terminate command line
	mov     [command_line], cl ; command line length (except CR)

; ----------------------------------------------------------------------------

;   Once we get to this point, the above code, which is below "retry"
;   in memory, can be trashed (and in fact is -- see references to retry
;   which follow....)

retry:
	mov	dx,commnd	; now pointing to file description

; we are going to open the command interpreter and size it as is done in
; ldfil. the reason we must do this is that sysinit is in free memory. if
; there is not enough room for the command interpreter,exec will probably
; overlay our stack and code so when it returns with an error sysinit won't be
; here to catch it. this code is not perfect (for instance .exe command
; interpreters are possible) because it does its sizing based on the
; assumption that the file being loaded is a .com file. it is close enough to
; correctness to be usable.

; first, find out where the command interpreter is going to go.

	push	dx		; save pointer to name
	mov	bx,0FFFFh
	mov	ah,48h	; ALLOC
        int     21h             ; get biggest piece
	mov	ah,48h	; ALLOC
	int	21h		; second time gets it
	jc	short memerrjx	; oooops

	mov	es,ax
	mov	ah,49h	; DEALLOC
	int	21h		; give it right back
	mov	bp,bx

; es:0 points to block,and bp is the size of the block in para.

; we will now adjust the size in bp down by the size of sysinit.
; we need to do this because exec might get upset if some of the exec
; data in sysinit is overlayed during the exec.

	; 22/10/2022
	; (MSDOS 5.0 IO.SYS SYSINIT:083Bh)
        mov     bx,[MEMORY_SIZE] ; get location of end of memory
	mov	ax,cs		 ; get location of beginning of sysinit

; Note that the "config_wrkseg" environment data is a segment in
; unallocated memory (as of the Dealloc of [area], above). This is ideal
; in one sense, because Exec is going to make a copy of it for COMMAND.COM
; anyway, and no one has responsibility for freeing "config_wrkseg". But
; we need to make sure that there's no way Exec will stomp on that data
; before it can copy it, and one way to do that is to make the available
; memory calculation even more "paranoid", by subtracting "config_wrkseg"
; from the "memory_size" segment value (which is typically A000h) instead
; of the current sysinit CS....
;
; The reason I use the term "paranoid" is because this code should have
; slid the data required by Exec up to the very top of memory, because as
; it stands, you have to have sizeof(COMMAND.COM) PLUS 64K to load just
; COMMAND.COM (64k is about what sysinit, and all the goop above sysinit,
; consumes). Now it's just a little worse (65K or more, depending on
; the size of your CONFIG.SYS, since the size of the environment workspace
; is determined by the size of CONFIG.SYS.... -JTP

	; 22/10/2022
	;mov	cx,[config_envlen]
        ;jcxz	no_env		; use config_wrkseg only if there's env data
        ;mov	ax,[config_wrkseg]
;no_env:
	; 22/10/2022
	; (MSDOS 5.0 IO.SYS SYSINIT:0841h)
  	sub     bx,ax           ; bx is size of sysinit in para
	add	bx,11h		; add the sysinit php
	sub	bp,bx		; sub sysinit size from amount of free memory
	jc	short memerrjx	; if there isn't even this much memory, give up

        ;mov	ax,(OPEN<<8)	; open the file being execed
        mov	ax,3D00h
	stc                     ; in case of int 24
	int	21h
	jc	short comerr	; ooops
			; DOS - 2+ - OPEN DISK FILE WITH HANDLE
			; DS:DX -> ASCIZ filename
			; AL = access mode
			; 0 - read
	; 22/10/2022
	; (MSDOS 5.0 IO.SYS SYSINIT:0852h)
        mov     bx,ax           ; handle in bx

;   If the standard command interpreter is being used, verify it is correct

	; 22/10/2022
	;cmp	byte [newcmd],0	; was a new shell selected?
	;jne	short skip_validation ; yes
	;mov	dx,retry-4
	;mov	cx,4		;
	;mov	ah,READ		;
	;int	21h		;
	;cmp	byte [retry-4],0E9h
	;jne	short comerr
	;; 20/04/2019 - Retro DOS v4.0
	;cmp	byte [retry-1],64h ; MSDOS 6.21 IO.SYS - SYSINIT:088Ch
	;;cmp	byte [retry-1],((MAJOR_VERSION&0Fh)<<4)|(MINOR_VERSION&0Fh)
	;jne	short comerr	;

;skip_validation:
	; 22/10/2022
	; (MSDOS 5.0 IO.SYS SYSINIT:0854h)
	xor	cx,cx
	xor	dx,dx
	;mov	ax,(LSEEK<<8)|2
	mov	ax,4202h
	stc			;in case of int 24
	int	21h		; get file size in dx:ax
	jc	short comerr
				; convert size in dx:ax to para in ax
	add	ax,15		; round up size for conversion to para
	adc	dx,0
	call	off_to_para
	mov	cl,12
	shl	dx,cl		; low nibble of dx to high nibble
	or	ax,dx		; ax is now # of para for file
	add	ax,10h		; 100h byte php
	cmp	ax,bp		; will command fit in available mem?
	jb	short okld	; jump if yes.

	; 22/10/2022
memerrjx:	; (MSDOS 5.0 IO.SYS SYSINIT:0876h)
	;jmp	memerr	; (MSDOS 5.0 IO.SYS SYSINIT:34D5h)
	; 02/11/2022
	jmp	mem_err

;memerrjx:
;	mov	dx,badmem
;	call	print
;       jmp     short continue

okld:
	mov	ah,3Eh ; CLOSE
	int	21h		; close file

	; 22/10/2022
	pop	dx	; (MSDOS 5.0 IO.SYS SYSINIT:087Dh)

	; 24/03/2019

	push	cs		; point es to sysinitseg
	pop	es
        mov     bx,COMEXE	; point to exec block
	; 22/10/2022
	;pop	dx              ; recover pointer to name

;;ifdef	MULTI_CONFIG

;   If there's any environment data in "config_wrkseg", pass it to shell;
;   there will be data if there were any valid SET commands and/or if a menu
;   selection was made (in which case the CONFIG environment variable will be
;   set to that selection).

	; 23/10/2022
	;mov	cx,[config_envlen]
	;jcxz	no_envdata
        ;mov	cx,[config_wrkseg]
;no_envdata:
	;;mov	[bx+EXEC0.ENVIRON],cx
	;mov	[bx],cx

;;endif	;MULTI_CONFIG

	; 23/10/2022
	; (MSDOS 5.0 IO.SYS SYSINIT:0883h)

	;mov	[bx+EXEC0.COM_LINE+2],cs ; set segments
	mov	[bx+4],cs
	;mov	[bx+EXEC0.5C_FCB+2],cs
	mov	[bx+8],cs
	;mov	[bx+EXEC0.6C_FCB+2],cs
	mov	[bx+12],cs

	;mov	ax,(EXEC<<8) + 0
	; 23/10/2022
	xor	ax,ax
	mov	ah,4Bh        

	stc                     ; in case of int 24
        int     21h             ; go start up command
			; DOS - 2+ - LOAD OR EXECUTE (EXEC)
			; DS:DX -> ASCIZ filename
			; ES:BX -> parameter block
			; AL = subfunc: load & execute program
	;push	cs
	;pop	ds

	; 23/10/2022
	;push	dx		; push to balance fall-through pop

; note fall through if exec returns (an error)
comerr:
	; 23/10/2022
;;ifdef	MULTI_CONFIG
	;cmp	byte [commnd4],0
	;je	short comerr2	; all defaults exhausted, print err msg
	;cmp	byte [newcmd],0
	;je	short continue	; don't print err msg for defaults just yet
;comerr2:
;;endif
        mov     dx,badcom	; want to print command error
	call	badfil
continue:
	; 23/10/2022
	;pop	dx

;;ifndef MULTI_CONFIG
	;jmp	stall
	; 24/10/2022
stall:		; (MSDOS 5.0 IO.SYS, SYSINIT:0899h)
	jmp	short stall
;;else
	; 23/10/2022
	;mov	ah,GET_DEFAULT_DRIVE ; 19h
	;int	21h             ;
	;add	al,'A'          ;
	;mov	dl,al           ; DL == default drive letter
	;mov	si,commnd2
	;cmp	byte [newcmd],0 ; if a SHELL= was given
	;jne	short do_def2	; then try the 2nd alternate;
	;mov	byte [si],0	; otherwise, the default SHELL= was tried,
	;jmp	short do_def3   ; which is the same as our 2nd alt, so skip it
;do_def2:			
	;cmp	byte [si],0	; has 2nd alternate been tried?
        ;jne	short do_alt    ; no
;do_def3:
	;mov	si,commnd3
	;cmp	byte [si],0	; has 3rd alternate been tried?
	;jne	short do_alt	; no
	;mov	si,commnd4
	;cmp	byte [si],0	; has 4th alternate been tried?
	;jne	short do_alt	; no
	;push	dx              ;
	;mov	dx,badcomprmpt
	;call	print		;
	;pop	dx              ; recover default drive letter in DL
;request_input:			;
	;mov	ah,STD_CON_OUTPUT
	;int	21h             ;
	;push	dx              ;
	;mov	dl,'>'          ;
	;int	21h             ;
	;mov	bl,[tmplate+1]	;
	;mov	bh,0            ;
	;mov	byte [commnd+bx],0Dh
	;mov	dx,tmplate
	;mov	ah,STD_CON_STRING_INPUT
	;int	21h             ; read a line of input
	;mov	dx,crlfm	;
	;call	print           ;
	;pop	dx              ;
	;mov	bl,[tmplate+1]	;
	;or	bl,bl           ; was anything typed?
	;jz	short request_input ;
	;mov	byte [newcmd],1 ; disable validation for user-specified binaries
	;mov	byte [commnd+bx],0 ; NULL-terminate it before execing it
	;mov	word [command_line],0D00h
	;jmp	short do_exec   ;
;do_alt:
	;push	ds
	;pop	es
	;mov	byte [newcmd],0 ; force validation for alternate binaries
	;mov	di,commnd	;
;do_alt1:
	;lodsb			; copy the alternate, zapping it as we go,
	;mov	byte [si-1],0	; so that we know it's been tried
	;stosb 			;
	;or	al,al		;
	;jnz	short do_alt1	;
	;mov	di,command_line
	;cmp	byte [si+2],':'
	;jne	short do_alt2	;
	;mov	[si+1],dl	; stuff default drive into alt. command line
;do_alt2:			;
	;lodsb			;
	;stosb			;
	;or	al,al           ;
	;jnz	short do_alt2   ;
	;mov	byte [di-1],cr

;;   Last but not least, see if we need to call disable_autoexec

	; MSDOS 6.0 (SYSINIT1.ASM)
	;;cmp	[command_line-1],0
        ;;jne	short do_exec   ;
        ;;mov	[command_line-1],'/'
	;;call	disable_autoexec ;

	; MSDOS 6.21 IO.SYS (SYSINIT:0994h)
	;mov	byte [dae_flag],0 ; 24/03/2019 - Retro DOS v4.0 	
	;call	disable_autoexec
	;call	CheckQueryOpt	; 24/03/2019 - Retro DOS v4.0
;do_exec:
	;jmp     retry		;

;;endif	;MULTI_CONFIG

; 24/03/2019 - Retro DOS v4.0

; ----------------------------------------------------------------------
; procedure : AllocFreeMem
;
; Allocate Max memory from DOS to find out where to load DOS.
; DOS is at temporary location when this call is being made
;
; Inputs : None
; Outputs: The biggest chunk of memory is allocated (all mem at init time)
;	   [area] & [memhi] set to the para value of the start of the
;	   free memory.
;
; Uses   : AX, BX
;
; ----------------------------------------------------------------------

	; 23/10/2022
AllocFreeMem:
	mov	bx,0FFFFh
	mov	ah,48h ; ALLOC
	int	21h			; first time fails
	mov	ah,48h ; ALLOC
	int	21h			; second time gets it
	mov	[cs:area],ax
	mov	[cs:memhi],ax		; memhi:memlo now points to
	retn				; start of free memory

	; include msbio.cl6
; ----------------------------------------------------------------------
DOSLOMSG:
	db	'HMA not available: Loading DOS low',0Dh,0Ah,'$'
FEmsg:
	db	'Fatal Error: Cannot allocate Memory for DOS',0Dh,0Ah,'$'

; ----------------------------------------------------------------------
;
; procedure : LoadDOSHiOrLo
;
;		Tries to move DOS into HMA. If it fails then loads
;		DOS into Low memory. For ROMDOS, nothing is actually
;		moved; this just tries to allocate the HMA, and prints
;		a message if this is not possible.
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
LoadDOSHiOrLo:
	; 27/03/2019 - Retro DOS v4.0
	; ds = cs
	call	TryToMovDOSHi		; Try moving it into HMA (M024)
	jc	short LdngLo		; If that don't work...
	retn
LdngLo:
	; 23/10/2022
	push	cs
	pop	ds
	mov	ah,9
	mov	dx,DOSLOMSG		; inform user that we are
	int	21h			; loading low

;ifndef ROMDOS
	; actually move the dos, and reinitialize it.

	mov	bx,1				; M012
						;  use int 21 alloc for mem
	call	MovDOSLo
	mov	es,[cs:CURRENT_DOS_LOCATION]	; give dos its temporary loc.
	; 23/10/2022
	;mov	es,[CURRENT_DOS_LOCATION]
	;;mov	es,[cs:FINAL_DOS_LOCATION]  ; 24/03/2019 - Retro DOS v4.0
	;mov	es,[FINAL_DOS_LOCATION] ; 27/03/2019
	xor	ax,ax				; ax = 00 ---> install stub
	call	far [cs:dos_segreinit]		; call dos segreinit
	;call	far [dos_segreinit] ; 27/03/2019
	
;endif ; ROMDOS
	; 23/10/2022
	mov	byte [cs:runhigh],0		; mark that we are running lo
	;mov	byte [runhigh],0 ; 27/03/2019
	retn

; ----------------------------------------------------------------------
;
; procedure : TryToMovDOSHi
;
;		This tries to move DOS into HMA.
;		Returns CY if it failed.
;		If it succeeds returns with carry cleared.
;
;		For ROMDOS, dos_segreinit must be called again to allow
;		the A20 switching code in the low mem stub to be installed.
; 
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (MSDOS 5.0 IO.SYS - SYSINIT:092Ah)
TryToMovDOSHi:
	; 27/03/2019 - Retro DOS v4.0
	; ds = cs
	call	MovDOSHi
	jc	short ttldhx

;ifndef ROMDOS
	; 23/10/2022
	mov	es,[cs:CURRENT_DOS_LOCATION] ; give dos its temporary loc.
	;mov	es,[cs:FINAL_DOS_LOCATION] ; 24/03/2019 - Retro DOS v4.0
;else
;	..
;endif ; ROMDOS

	xor	ax,ax			; ax = 00 ---> install stub
	call	far [cs:dos_segreinit]	; call dos segreinit

	mov	byte [cs:runhigh],1
	clc
ttldhx:
	retn

; ----------------------------------------------------------------------
;
; procedure : MovDOSHi
;
;		Tries to allocate HMA and Move DOS/BIOS code into HMA
;		For ROMDOS, the code is not actually moved, but the
;		HMA is allocated and prepared for sub-allocation.
;
;		Returns : CY if it failed
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
MovDOSHi:
	; 14/05/2019
	; 27/03/2019 - Retro DOS v4.0
	; ds = cs
	call	AllocHMA
	jc	short mdhx			; did we get HMA?
	mov	ax,0FFFFh			; yes, HMA seg = 0ffffh
	mov	es,ax

;ifndef ROMDOS
	; actually move the BIOS and DOS

	; NOTE: Retro DOS v4.0 does not move BIOS (IO.SYS) to HMA
	; 24/03/2019
	
	; 23/10/2022
	call	MovBIOS				; First move BIOS into HMA

	; ES:DI points to free HMA after BIOS
	
	; 14/05/2019
	; 24/03/2019 - Retro DOS v4.0
	;xor	di,di
	
	; 23/10/2022
	mov	cx,[cs:hi_doscod_size]		; pass the code size of DOS
	;mov	cx,[hi_doscod_size]		;  when it is in HMA
	call	MovDOS				; and move it

	; ES:DI points to free HMA after DOS
;else
;	; allocate space at beginning of HMA to allow for CPMHack
;
;	mov	di,0E0h				; room for 5 bytes at ffff:d0
;
;endif ; ROMDOS

	call	SaveFreeHMAPtr			; Save the Free HMA ptr
	clc
mdhx:
	retn

; ----------------------------------------------------------------------
;
; procedure : MovDOSLo
;
;		Allocates memory from DOS and moves BIOS/DOS code into it
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

;ifndef ROMDOS

MovDOSLo:
	; 14/05/2019
	; 27/03/2019 - Retro DOS v4.0
	; ds = cs
	call	AllocMemForDOS			; incestuosly!!!
	
	; 23/10/2022
	; 14/05/2019
	;inc	ax  ; skip MCB
	
	mov	es,ax				; pass the segment to MovBIOS
	; NOTE: Retro DOS v4.0 does not move BIOS (IO.SYS) to HMA
	; 24/03/2019
	
	; 23/10/2022
	call	MovBIOS

;------ ES:DI points memory immediately after BIOS

	; 14/05/2019
	; NOTE: 
	;     Order of (RETRO) DOS kernel sections at memory:
	;	BIOSDATA+BIOSCODE+BIOSDATAINIT+DOSDATA+DOSCODE(LOW)

	; 24/03/2019 - Retro DOS v4.0
	;xor	di,di	

	; 23/10/2022
	mov	cx,[cs:lo_doscod_size]		; DOS code size when loaded
	;mov	cx,[lo_doscod_size]		;  low
	call	MovDOS
	retn

;endif ; ROMDOS

; NOTE: Retro DOS v4.0 does not move BIOS (IO.SYS) to HMA
; 24/03/2019
; ----------------------------------------------------------------------
;
; procedure : MovBIOS
;
;		Moves BIOS code into requested segment
;
;	In : ES - segment to which BIOS is to be moved
;		  ( it moves always into offset BCode_Start)
;
;	Out : ES:DI - pointer to memory immediately after BIOS
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

;ifndef ROMDOS

MovBIOS: ; proc	near
	; 23/10/2022
	mov	ds,[cs:temp_bcode_seg]		; current BIOS code seg
	mov	si,BCODE_START ; mov si,30h
	mov	di,si
	mov	cx,BCODE_END ; mov cx,1A60h
	sub	cx,si				; size of BIOS
	shr	cx,1				; Both the labels are para
						;  aligned
	rep	movsw
	push	es
	push	di				; save end of BIOS
	mov	ax,es
	mov	[cs:BCodeSeg],ax		; save it for later use
	;call	dword ptr cs:_seg_reinit_ptr
	call	far [cs:seg_reinit_ptr]		; far call to seg_reinit (M022)
	pop	di
	pop	es				; get back end of BIOS
	retn

;MovBIOS endp

;endif ; ROMDOS

; 24/03/2019

; ----------------------------------------------------------------------
;
; procedure : MovDOS
;
;		Moves DOS code into requested area
;
;	In : ES:DI - pointer to memory where DOS is to be moved
;	     CX    - size of DOS code to be moved
;
;	Out : ES:DI - pointer to memory immediately after DOS
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

;ifndef ROMDOS

MovDOS:
	; 14/05/2019
	; 27/03/2019 - Retro DOS v4.0
	; ds = cs
	
	; 23/10/2022
	;push	ds ; *//
	
	push	es
	push	di

	; 29/04/2019
	;lds	si,[dosinit]
	; 23/10/2022
	lds	si, [cs:dosinit]
	;mov	ax,si

	rep	movsb
	pop	bx				; get back offset into which
						;  DOS was moved
	mov	ax,[cs:dosinit]			; get the offset at which DOS
						;  wants to run
	sub	ax,bx
	call	off_to_para
	pop	bx				; get the segment at which
						;  we moved DOS into
	sub	bx,ax				; Adjust segment
	; 23/10/2022
	mov	[cs:CURRENT_DOS_LOCATION],bx	; and save it
	;mov	[cs:FINAL_DOS_LOCATION],bx
		
	; 27/03/2019
	;pop	ds ; *//
	; ds = cs
	;mov	[FINAL_DOS_LOCATION],bx

	retn

;endif ;ROMDOS

; ----------------------------------------------------------------------
;
; procedure : AllocMemForDOS
;
;		Allocate memory for DOS/BIOS code from DOS !!!
;
;	Out : AX - seg of allocated memoryblock
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

;ifndef ROMDOS

AllocMemForDOS:
	; 14/05/2019
	; 27/03/2019 - Retro DOS v4.0
	; ds = cs
	;mov	ax,BCode_end
	;sub	ax,BCode_start		; BIOS code size
	; 23/10/2022
	mov	ax,BCODE_END ; 1A60h
	sub	ax,BCODE_START ; 30h
	; 24/03/2019 - Retro DOS v4.0 
	; 02/11/2022
	add	ax,[cs:lo_doscod_size]	; DOS code size
	;add	ax,[lo_doscod_size]
	add	ax,15
	call	off_to_para			; convert to para
	; 23/10/2022
	; 14/05/2019
	;inc	ax ; + 1 paragraph for MCB
	or	bx,bx				; M012
	mov	bx,ax				;  can we use int 21 for alloc
	jz	short update_arena		; M012
	mov	ah,48h				; request DOS
	int	21h
	jc	short FatalErr			; IF ERR WE ARE HOSED
 	; 23/10/2022
	; 24/03/2019 - Retro DOS v4.0 (ORG 0)
	sub	ax,3				; Take care ORG 30h of
						;  BIOS code
	mov	es,ax
	;mov	word [es:20h+ARENA.OWNER],08h	; mark it as system
	;mov	word [es:20h+ARENA.NAME],'SC'	;  code area
	; 14/05/2019
	;mov	word [es:ARENA.OWNER],08h	; mark it as system
	;mov	word [es:ARENA.NAME],'SC'	;  code area
	; 23/10/2022
	mov	word [es:20h+1],08h		; mark it as system
	mov	word [es:20h+8],'SC'		;  code area

	retn

; BUGBUG -- 5 Aug 92 -- chuckst -- Allocating space for DOS
;	  using DOS itself causes an arena to be generated.
;	  Unfortunately, certain programs (like PROTMAN$)
;	  assume that the device drivers are loaded into
;	  the first arena. For this reason, MagicDrv's
;	  main device driver header arena is manually
;	  truncated from the arena chain, and the space
;	  for DOS is allocated using the following
;	  simple code, which also assumes that the
;	  first arena is the free one where DOS's low
;	  stub will go.
;
; M012 : BEGIN

	; 23/10/2022
update_arena:
	push	ds ; ds = cs
	push	di
	push	cx
	push	dx
	; 23/10/2022
	lds	di,[cs:DOSINFO]			; get ptr to DOS var
	;lds	di,[DOSINFO] ; 27/03/2019	
	dec	di
	dec	di				; Arena head is immediately
						;  before sysvar
	mov	es,[di]				; es = arena head
	;mov	cx,[es:ARENA.SIZE]		; cx = total low mem size
	mov	cx,[es:3]
	cmp	cx,bx				; is it sufficient ?
	jb	short FatalErr			; no, fatal error

	;mov	dl,[es:ARENA.SIGNATURE]
	mov	dl,[es:0]
	mov	ax,es
	add	ax,bx				; ax = new arena head
	mov	[di],ax			; store it in DOS data area
	mov	ds,ax
	;mov	[ARENA.SIGNATURE],dl		; type of arena
	mov	[0],dl
	;mov	word [ARENA.OWNER],0		; free
	mov	word [1],0
	sub	cx,bx				; size of the new block
	;mov	[ARENA.SIZE],cx			; store it in the arena
	mov	[3],cx
	mov	ax,es				; return seg to the caller
	; 23/10/2022
	;; 24/03/2019 - Retro DOS v4.0 (ORG 0)	; Take care ORG 30h of
	sub	ax,3				;  BIOS code
	pop	dx
	pop	cx
	pop	di
	pop	ds ; ds = cs
	retn
;
; M012 : END
;
FatalErr:
	push	cs
	pop	ds
	mov	dx,FEmsg
	mov	ah,9
	int	21h 		; DOS - PRINT STRING
				; DS:DX -> string terminated by "$"
        ;jmp     stall
	; 23/10/2022
	cli
	hlt

;endif ;ROMDOS

; 25/03/2019 - Retro DOS v4.0

; ----------------------------------------------------------------------
;
; procedure : AllocHMA
;
;	grab_the_hma tries to enable a20 and make sure there is memory
;	  up there. If it gets any sort of error, it will return with
;	  carry set so that we can resort to running low.
;
;	It also returns ES: -> 0ffffh if it returns success
;
; ----------------------------------------------------------------------

AllocHMA:
;	cas note:  The pre-286 check is no longer needed here since the
;		   presence of XMS is sufficient. However, this code hasn't
;		   been deleted because it can be recycled for skipping the
;		   extra pass of CONFIG.SYS and assuming we're running low
;		   in the case of a pre-286.

;;	see if we're running on a pre-286. If not, force low.
;
;	xor	ax,ax
;	pushf			; save flags (like int)
;	push	ax
;	popf
;	pushf
;	pop	ax
;	popf			; restore original flags (like int)
;	and	ax,0F000h
;	cmp	ax,0F000h	; 8088/8086?
;	jz	short grab_hma_error

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0A26h)

	push	ds
	;;mov	ax,Bios_Data
	;mov	ax,KERNEL_SEGMENT
	; 21/10/2022
	mov	ax,DOSBIODATASEG
	mov	ds,ax

	call	IsXMSLoaded
	jnz	short grabhma_error

	mov	ax,4310h
	int	2Fh		; get the vector into es:bx
		; - Multiplex - XMS - GET DRIVER ADDRESS
		; Return: ES:BX -> driver entry point

	mov	[xms],bx
	;mov	[0Eh], bx
	mov	[xms+2],es
	;mov	[10h], es

	mov	ah,1		; request HMA
	mov	dx,0FFFFh
	;call	dword ptr ds:0Eh
	call	far [xms]
	dec	ax
	jz	short allocHMA_1 ; error if not able to allocate HMA

;------ Himem may be lying because it has allocated mem for int 15

	mov	ah,88h
	int	15h
		; Get Extended Memory Size
		; Return: CF clear on success
		; AX = size of memory above 1M in K
	cmp	ax,64		; less than 64 K of hma ?
	jb	short grabhma_error
allocHMA_1:
	mov	ah,5		; localenableA20
	;call	dword ptr ds:0Eh
	call	far [xms]
	dec	ax
	jnz	short grabhma_error ; error if couldn't enable A20

	call	IsVDiskInstalled
	jz	short grabhma_error ; yes, we cant use HMA

	mov	ax,0FFFFh
	mov	es,ax
	mov	word [es:10h],1234h ; see if we can really read/write there
	cmp	word [es:10h],1234h
	jne	short grabhma_error ; don't try to load there if XMS lied

	clc
	pop	ds
	retn

grabhma_error:
	stc
	pop	ds
	retn

; ----------------------------------------------------------------------
;
; procedure : IsXMSLoaded
;
;             Checks whether a XMS driver is loaded
;
; Returns : Z flag set if XMS driver loaded
;           Z flag reset if no XMS drivers are present
;
; ----------------------------------------------------------------------

IsXMSLoaded:
	mov	ax,4300h
	int	2Fh		; - Multiplex - XMS - INSTALLATION CHECK
				; Return: AL = 80h XMS driver installed
				; AL <> 80h no driver
	cmp	al,80h		; XMS installed?
	retn

; ----------------------------------------------------------------------
; procedure : FTryToMovDOSHi
;
;		Called from HMA suballoc calls
;	
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0A84h)

FTryToMovDOSHi:	; proc	far

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es

	; 23/10/2022
	; 27/03/2019 - Retro DOS v4.0
	;push	cs
	;pop	ds

	cmp	byte [cs:runhigh],0FFh
	;cmp	byte [runhigh],0FFh
	jne	short _ftymdh_1

	; ds = cs
	call	TryToMovDOSHi
_ftymdh_1:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	retf

; ----------------------------------------------------------------------
;
; following piece of code will be moved into a para boundary. And the para
; address posted in seg of int 19h vector. Offset of int 19h will point to
; VDint19. This is to protect HMA from apps which use VDISK header method
; to determine free extended memory.
;
; For more details read "power programming" column by Ray Duncan in the
; May 30 1989 issue of PC Magazine (pp 377-388) [USING EXTENDED MEMORY,PART 1]
;
; ----------------------------------------------------------------------

StartVDHead:
;-------------- what follows is a dummy device driver header (not used by DOS)

	dd	0		; link to next device driver
	dw	8000h		; device attribute
	dw	0		; strategy routine offset
	dw	0		; interrupt routine offset
	db	1		; number of units
	;db	7 dup(0) 
	times	7 db 0 		; reserved area
VDiskSig1:
	db	'VDISK'

VLEN1	equ	($-VDiskSig1)

	db	'  V3.3'	; vdisk label
	;db	15 dup (0)	; pad
	times	15 db 0
	dw	0		; bits 0-15 of free HMA
	db	11h		; bits 16-23 of free HMA (1M + 64K)
VDInt19:
	db	0EAh		; jmp to old vector
OldVDInt19:
	dd	0		; Saved int 19 vector

EndVDHead: ; label byte

VDiskHMAHead:	
	db	0,0,0		; non-bootable disk
VDiskSig2:
	db	'VDISK'

VLEN2	equ	($-VDiskSig2)

	db	'3.3'		; OEM - signature
	dw	128		; number of bytes/sector
	db	1		; sectors/cluster
	dw	1		; reserved sectors
	db	1		; number of FAT copies
	dw	64		; number of root dir entries
	dw	512		; number of sectors
	db	0FEh		; media descriptor
	dw	6		; number of sectors/FAT
	dw	8		; sectors per track
	dw	1		; number of heads
	dw	0		; number of hidden sectors
	dw	440h		; Start of free HMA in K (1M+64K)

EndVDiskHMAHead: ; label byte

; ----------------------------------------------------------------------
;
; procedure : InstVDiskHeader
;
;             Installs the VDISK header to reserve the 64k of HMA
;	      It puts a 32 byte header at 10000:0 and
;	      another header at (seg of int19):0
;
; Inputs : None
;
; Outputs : None
;
; USES : DS,SI,AX,CX,DX
;
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

InstVDiskHeader:
	xor	ax,ax
	mov	ds,ax			; seg of int vect table

;-------------- save old int 19 vector

	; 23/10/2022
	mov	ax,[19h*4]
	;mov	[OldVDInt19],ax
	mov	[cs:OldVDInt19],ax
	mov	ax,[19h*4+2]
	;mov	[OldVDInt19+2],ax
	mov	[cs:OldVDInt19+2],ax

;-------------- calculate seg of new int 19 handler

	mov	ah,48h			; allocate memory
	;mov	bx,(EndVDHead-StartVDHead+15)>>4
	; 23/10/2022
	mov	bx,4
	int	21h

;	if carry, fatal hanging error!!!!!

	dec	ax			; point to arena
	mov	es,ax
	;mov	word [es:ARENA.OWNER],8	; owner = System
	mov	word [es:1],8
	;mov	word [es:ARENA.NAME],'SC' ; System Code
	mov	word [es:8],'SC'
	inc	ax
	mov	es,ax			; get back to allocated memory

;-------------- install new int 19 vector

	cli				; no reboots at this time
	;mov	word [19h*4],(VDInt19-StartVDHead)
	mov	word [19h*4],47
	mov	[19h*4+2],ax

;-------------- move the code into proper place

	;mov	cx,(EndVDHead-StartVDHead)
	mov	cx,52
	mov	si,StartVDHead
	xor	di,di
	push	cs
	pop	ds
	cld
	rep	movsb
	sti				; BUGBUG is sti OK now?

;-------------- mov the HMA VDisk head into HMA

	; 23/10/2022
	push	di
	push	es

	mov	ax,0FFFFh
	mov	es,ax
	mov	di,10h
	;mov	cx,(EndVDiskHMAHead-VDiskHMAHead)
	mov	cx,32
	mov	si,VDiskHMAHead
	rep	movsb			; ds already set to cs

	pop	di
	pop	es

	retn

; ----------------------------------------------------------------------
; procedure : ClrVDISKHeader
;
;		Clears the first 32 bytes at 1MB boundary
;		So that DOS/HIMEM is not confused about the VDISK header
;		left by previous DOS=HIGH session
;
; ----------------------------------------------------------------------

struc desc
 .seg_lim:	resw	1		; seg limit 64K 
 .lo_word:	resw	1		; 24 bit seg physical 
 .hi_byte:	resb 	1		; address
 .acc_rights:	resb	1		; access rights ( CPL0 - R/W )
 .reserved:	resw	1		;
 .size:
endstruc

		; 23/10/2022
bmove:		;label byte

dummy:		;times desc.size db 0	; desc	<>
		times 8 db 0		 
gdt:		;times desc.size db 0	; desc	<>
		times 8 db 0
src_desc:	dw	0FFFFh		; desc	<0ffffh,0,0,93h,0>
		dw	0
		db	0
		db	93h
		dw	0
tgt_desc:	dw	0FFFFh		; desc	<0ffffh,0,10h,93h,0>  ; 1MB
		dw	0
		db	10h
		db	93h
		dw	0

rombios_code:	;times desc.size db 0	; desc	<>
		times 8 db 0
temp_stack:	;times desc.size db 0	; desc	<>
		times 8 db 0

ClrdVDISKHead:	times 32 db 0		; db 32 dup (0)


; 25/03/2019 - Retro DOS v4.0 (MSDOS 6.21 IO.SYS, MSDOS 6.0 SYSINIt1.ASM)

ClrVDISKHeader:	; proc	near

;;-----------------------------------------------------------	      ;I070
;; The following workaround get around a problem with the	      ;I070
;; Tortugas and PS/2 30-286 BIOS when password server mode	      ;I070
;; is set. On those machines the INT 15h block move code	      ;I070
;; goes through the 8042 to twiddle A20 instead of port 92h.	      ;I070
;; In password server mode the 8042 is disabled so the block	      ;I070
;; move crashes the system. We can do this because these	      ;I070
;; systems clear all of memory on a cold boot.			      ;I070
;								      ;I070
;               in      al,64h         ; Test for password servr mode ;I070
;               test    al,10h         ; Is keyboard inhibited?	      ;I070
;               jnz     short ClrVDISKok ; No, go do block move.      ;I070
;                                      ; Check for Tortugas...	      ;I070
;               cmp     word [cs:sys_model_byte],19F8h                ;I070
;               je      short ClrVDISKno                              ;I070
;                                      ; Check for mod 30-286	      ;I070
;               cmp     word [cs:sys_model_byte],09FCh                ;I070
;               jne     short ClrVDISKok			      ;I070
;ClrVDISKno: 	retn	               ; Return w/o block move.	      ;I070
;								      ;I070
;ClrVDISKok:							      ;I070
;-----------------------------------------------------------	      ;I070

	push	es
	mov	ax,cs
	mov	dx,ax
	mov	cl,12
	shr	dx,cl
	mov	cl,4
	shl	ax,cl
	add	ax,ClrdVDISKHead
	adc	dl,0
	; 23/10/2022
	;mov	[cs:src_desc+desc.lo_word],ax
	mov	[cs:src_desc+2],ax
	;mov	[cs:src_desc+desc.hi_byte],dl
	mov	[cs:src_desc+4],dl
	mov	cx,16			; 16 words
	push	cs
	pop	es
	mov	si,bmove
	mov	ah,87h
	int	15h	; EXTENDED MEMORY - BLOCK MOVE (AT,XT286,PS)
			; CX = number of words to move 
			; ES:SI -> global descriptor table
			; Return: CF set on error, AH = status
	pop	es
	retn

; ----------------------------------------------------------------------
;
; procedure : SaveFreeHMAPtr
;
;		Save the Free HMA pointer in BIOS variable for later use.
;		(INT 2f ax==4a01 call returns pointer to free HMA)
;		Normalizes the pointer to ffff:xxxx format and stores only
;		the offset.
;
; Inputs : ES:DI - pointer to free HMA
; Output : FreeHMAPtr in BIOS data segment updated
;
; ----------------------------------------------------------------------

SaveFreeHMAPtr:
	mov	bx,es
	mov	ax,0FFFFh		; HMA segment
	sub	ax,bx
	add	di,15			; para round
	and	di,0FFF0h
	mov	cl,4
	shl	ax,cl
	sub	di,ax
	push	ds
	;;mov	ax,Bios_Data ; 0070h
	;mov	ax,KERNEL_SEGMENT ; 0070h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax
	mov	[FreeHMAPtr],di	 ; (ds:8F7h for MSDOS 6.21 IO.SYS)
	mov	byte [inHMA],0FFh  ; (ds:0Dh)
	pop	ds
	retn

; ----------------------------------------------------------------------
;
; procedure : IsVDiskInstalled
;
;		Checks for the presence of VDISK header at 1MB boundary
;		& INT 19 vector
;
; Inputs  : A20 flag should be ON
; Outputs : Zero set if VDISK header found else Zero cleared
;
; ----------------------------------------------------------------------

IsVDiskInstalled:
	xor	ax,ax
	mov	ds,ax
	mov	ds,[19*4+2]
	;mov	si,VDiskSig1-StartVDHead ; 12h
	; 23/10/2022
	mov	si,18
	;mov	cx,VLEN1 ; 5
	mov	cx,5
	push	cs
	pop	es
	mov	di,VDiskSig1
	rep	cmpsb
	je	short ivdins_retn
	mov	ax,0FFFFh
	mov	ds,ax
	;mov	si,10h+(VDiskSig2-VDiskHMAHead) ; 13h
	mov	si,13h
	mov	di,VDiskSig2
	;mov	cx,VLEN2  ; 5
	mov	cx,5
	rep	cmpsb
ivdins_retn: 
	retn			; returns the Zero flag

; ----------------------------------------------------------------------
;
; procedure : CPMHack
;
;		Copies the code from 0:c0 into ffff:0d0h
;		for CPM compatibility
;
; ----------------------------------------------------------------------

CPMHack:
	push	ds
	mov	cx,0FFFFh
	mov	es,cx		; ES = FFFF
	xor	cx,cx
	mov	ds,cx		; DS = 0
	mov	si,0C0h
	mov	di,0D0h
	mov	cx,5
	cld
	rep	movsb		; move 5 bytes from 0:C0 to FFFF:D0
	pop	ds
	retn

; ----------------------------------------------------------------------
;
; procedure : off_to_para
;
; ----------------------------------------------------------------------
off_to_para:
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	retn

; ----------------------------------------------------------------------
;**	TempCDS - Create (Temporary?) CDS
;
;	ENTRY	?? BUGBUG
;		(DS) = SysInitSeg
;	EXIT	?? BUGBUG
;	USES	?? BUGBUG
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
TempCDS:
	les	di,[DOSINFO]
	mov	cl,[es:di+SYSI_NUMIO]
	;mov	cl, [es:di+20h]
	xor	ch,ch			; (cx) = # of block devices

	mov	[es:di+SYSI_NCDS],cl	; one CDS per device
	;mov	[es:di+21h],cl	

	mov	al,cl
	mov	ah,curdirlen ; curdir_list.size ; 88
	;mov	ah,88
	mul	ah			; (ax) = byte size for those CDSs
	call	ParaRound		; (ax) = paragraph size for CDSs
	mov	si,[top_of_cdss]

;	BUGBUG - we don't update confbot - won't someone else use it?
;	chuckst -- answer: no. Confbot is used to access the CDSs,
;	25 jul 92  which are stored BELOW it. Alloclim is the
;		   variable which has the top of free memory for
;		   device driver loads, etc.

	sub	si,ax

;	chuckst, 25 Jul 92 -- note: I'm removing the code here
;		that automatically updates alloclim every time we
;		set up some new CDSs. Instead, I've added code
;		which pre-allocates space for 26 CDSs. This
;	        way we've got room for worst case CDSs before
;		we place MagicDrv.sys
;
;	mov	[ALLOCLIM],si		; can't alloc past here!

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	; (SYSINIT:0C52h)
	mov	[ALLOCLIM],si

	mov	[es:di+SYSI_CDS+2],si
	;mov	[es:di+18h],si
	mov	ax,si
	mov	word [es:di+SYSI_CDS],0	; set address of CDS list
	;mov	[word es:di+16h],0
	;lds	si,[es:di+SYSI_DPB]	; (ds:si) = address of first DPB
	lds	si,[es:di]
	mov	es,ax
	xor	di,di			; (es:di) = address of 1st CDS

;*	Initialize our temporary CDSs. We'll init each CDS with the
;	info from the corresponding DPB.
;
;	(cx) = count of CDSs left to process
;	(es:di) = address of next CDS

fooset:
	; 23/10/2022
	mov	ax, [cs:DirStrng] ; "A:"
	stosw				; setup the root as the curdir

	;call	get_dpb_for_drive_al	; get dpb for drive in dpb

;	(ds:si) = address of DPB
;		    (si) = -1 if no drive

	mov	ax,[cs:DirStrng+2] ; "\",0
	stosw
	inc	byte [cs:DirStrng]
	xor	ax,ax
	push	cx
	;mov	cx,curdir_list.cdir_flags - 4 ; 63
	mov	cx,63	; 23/10/2022
	rep	stosb			; zero out rest of CURDIR_TEXTs

;	should handle the system that does not have any floppies.
;	in this case,we are going to pretended there are two dummy floppies
;	in the system. still they have dpb and cds,but we are going to
;	0 out curdir_flags,curdir_devptr of cds so ibmdos can issue
;	"invalid drive specification" message when the user try to
;	access them.
;
;	(ax) = 0
;	(es:di) = CURDIR_FLAGS in the CDS records
;	(ds:si) = Next DPB (-1 if none)

	cmp	si,-1	; cmp si,0FFFFh
	je	short fooset_zero	; don't have any physical drive.

;	check to see if we are faking floppy drives. if not go to normcds.
;	if we are faking floppy drives then see if this cds being initialised
;	is for drive a: or b: by checking the appropriate field in the dpb
;	pointed to by ds:si. if not for a: or b: then go to normcds. if
;	for a: or b: then execute the code given below starting at fooset_zero.
;	for dpb offsets look at inc\dpb.inc.

	cmp	byte [cs:fake_floppy_drv],1
	jne	short normcds 		; machine has floppy drives
	;cmp	byte [si+DPB.drive],1	; if dpb_drive = 0 (a) or 1 (b).
	cmp	byte [si],1
	ja	short normcds
	mov	cl,3			; the next dbp pointer
					; AX should be zero here
	rep	stosw
	pop	cx
	jmp	short get_next_dpb

;	(ax) = 0

fooset_zero:
	mov	cl,3
	rep	stosw
	pop	cx
	jmp	short fincds

;*	We have a "normal" DPB and thus a normal CDS.
;
;	(ax) = 0
;	(es:di) = CURDIR_FLAGS in the CDS records
;	(ds:si) = Next DPB (-1 if none)

normcds:
	pop	cx

;	if a non-fat based media is detected (by dpb.numberoffat == 0), then
;	set curdir_flags to 0. this is for signaling ibmdos and ifsfunc that
;	this media is a non-fat based one.

	;cmp	byte [si+DPB.FAT_COUNT],0 ; non fat system?
	; 23/10/2022
	cmp	byte [si+8], 0
	je	short setnormcds	; yes. set curdir_flags to 0. ax = 0 now.
	mov	ax,curdir_inuse ; 4000h	; else,fat system. set the flag to curdir_inuse.
	;mov	ax,4000h
setnormcds:
	stosw				; curdir_flags
	mov	ax,si
	stosw				; curdir_devptr
	mov	ax,ds
	stosw

get_next_dpb:				; entry point for fake_fooset_zero
	; 23/10/2022
	lds	si,[si+19h]
	;lds	si,[si+DPB.NEXT_DPB] ; [si+19h]
fincds:
	mov	ax,-1	; mov ax,0FFFFh
	stosw				; curdir_id
	stosw				; curdir_id
	stosw				; curdir_user_word
	mov	ax,2
	stosw				; curdir_end
	mov	al,0			; clear out 7 bytes (curdir_type,
	stosb
	stosw				;  curdir_ifs_hdr,curdir_fsda)
	stosw
	stosw

	loop	fooset
	
	mov	byte [cs:DirStrng],"A"	; "A:\"
	
	retn

; ----------------------------------------------------------------------
;***	get_dpb_for_drive_al -- lookup the DPB for drive in al
;
;	entry:
;	   al == ASCII CAPS drive letter
;
;	exit:
;	   ds:si -> DPB, or si = -1 if not found
; ----------------------------------------------------------------------

;	; 23/10/2022 (get_dpb_for_drive_al
;
;get_dpb_for_drive_al:
;	lds	si,[cs:DOSINFO]		; point to first DPB
;	;lds	si,[si+SYSI_DPB]	; (ds:si) = address of first DPB
;	lds	si,[si]
;	sub	al,'A'
;
;get_dpb_for_drive_1:
;	;cmp	al,[si+DPB.DRIVE]	; match?
;	cmp	al,[si]
;	je	short got_dpb_for_drive	;  done if so
;
;	lds	si,[si+DPB.NEXT_DPB] ; [si+19h]
;	cmp	si,-1
;	jne	short get_dpb_for_drive_1 ; loop until hit end of DPBs
;
;got_dpb_for_drive:
;	retn

;=======================================================================

;**	EndFile - Build DOS structures
;
; This procedure is called after the config.sys has been processed and
; installable device drivers have been loaded (but before "install="
; programs are loaded) to create the dos structures such as SFTs, buffers,
; FCBs, CDSs, etc. It also loads the sysinit_base module in low memory
; to allow for the safe EXECing of "install=" programs. All memory
; above these structures is deallocated back to DOS.
;
;	ENTRY	?? BUGBUG
;	EXIT	?? BUGBUG
;	USES	?? BUGBUG

;=======================================================================
; allocate files
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0CCDh)

endfile:
; we are now setting up final cdss,buffers,files,fcss strings etc. we no
; longer need the space taken by the temp stuff below confbot,so set alloclim
; to confbot.

;	if this procedure has been called to take care of install= command,
;	    then we have to save es,si registers.

	; 23/10/2022
	; 31/03/2019
	push	ds

	;;mov	ax,Bios_Data ; 0070h
	;mov	ax,KERNEL_SEGMENT ; 0070h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax

	;cmp	word [052Fh], 0
	cmp	word [multrk_flag],multrk_off1 ;=0,multrack= command entered?
	jne	short multrk_flag_done
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	or	word [multrk_flag],multrk_on ; 80h  ; default will be on.
	;or	byte [multrk_flag],multrk_on ; 80h
multrk_flag_done:
	; 23/10/2022
	; 31/03/2019
	pop	ds

	;mov	ax,[cs:CONFBOT]
	;mov	[cs:ALLOCLIM],ax
	; 23/10/2022
	mov	ax, [cs:top_of_cdss]
	mov	[cs:ALLOCLIM], ax 

	push	cs
	pop	ds
	
	;mov	ax,[CONFBOT]
	;mov	[ALLOCLIM],ax

	call	round
	mov	al,[cs:FILES]
	; 23/10/2022
	;mov	al,[FILES]
	sub	al,5
	jbe	short dofcbs

	push	ax
	;mov	al,devmark_files ; 'F'
	mov	al,'F'
	call	setdevmark		; set devmark for sfts (files)
	pop	ax
	xor	ah,ah			; do not use cbw instruction!!!!!
					;  it does sign extend.
	;mov	bx,[memlo]
	;mov	dx,[memhi]
	;lds	di,[DOSINFO]		;get pointer to dos data
	; 23/10/2022
	mov	bx,[cs:memlo]
	mov	dx,[cs:memhi]
	lds	di,[cs:DOSINFO]		

	;lds	di,[di+SYSI_SFT]	;ds:bp points to sft
	lds	di,[di+4]

	;mov	[di+SF.SFLink],bx
	mov	[di],bx
	mov	[di+SF.SFLink+2],dx	;set pointer to new sft

	push	cs
	pop	ds

	;les	di,[memlo]		;point to new sft
	; 23/10/2022
	les	di,[cs:memlo]

	;mov	word [es:di+SF.SFLink],-1
	mov	word [es:di],-1		; 0FFFFh
	;mov	[es:di+SF.SFCount],ax
	mov	[es:di+4],ax
	;mov	bl,SF_ENTRY.size ; 59
	mov	bl,59
	mul	bl			;ax = number of bytes to clear
	mov	cx,ax
	;add	[memlo],ax		;allocate memory
	; 23/10/2022
	add	[cs:memlo],ax
	mov	ax,6
	;add	[memlo],ax		;remember the header too
	add	[cs:memlo],ax
	;or	byte [setdevmarkflag],for_devmark ; 2
	or	byte [cs:setdevmarkflag],2
	call	round			; check for mem error before the stosb
	add	di,ax
	xor	ax,ax
	rep	stosb			;clean out the stuff

; allocate fcbs
; ----------------------------------------------------------------------

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0D48h)
dofcbs:
	push	cs
	pop	ds
	call	round
	;mov	al,devmark_fcbs	; 'X'	;='x'
	mov	al,'X'
	call	setdevmark
	;mov	al,[FCBS]
	mov	al,[cs:FCBS]
	xor	ah,ah			; do not use cbw instruction!!!!!
					;  it does sign extend.
	;mov	bx,[memlo]
	;mov	dx,[memhi]
	;lds	di,[DOSINFO]		;get pointer to dos data
	; 23/10/2022
	mov	bx,[cs:memlo]
	mov	dx,[cs:memhi]
	lds	di,[cs:DOSINFO]

	;mov	[di+SYSI_FCB],bx
	;mov	[di+SYSI_FCB+2],dx ;set pointer to new table
	; 23/10/2022
	mov	[di+1Ah],bx		; [di+SYSI_FCB]
	mov	[di+1Ch],dx		; [di+SYSI_FCB+2]

	mov	bl,[cs:KEEP]
	xor	bh,bh
	;mov	[di+SYSI_KEEP],bx
	mov	[di+1Eh],bx		; [di+SYSI_KEEP]	

	push	cs
	pop	ds
	
	les	di,[memlo]		;point to new table
	;mov	word [es:di+SF.SFLink],-1
	mov	word [es:di],-1
	;mov	[es:di+SF.SFCount],ax
	; 02/11/2022
	mov	[es:di+4],ax
	mov	bl,SF_ENTRY.size ; 59
	mov	cx,ax
	mul	bl			;ax = number of bytes to clear
	add	[memlo],ax		;allocate memory
	;mov	ax,6
	mov	ax,SF.size-2 ; 6
	add	[memlo],ax		;remember the header too
	;or	byte [setdevmarkflag],for_devmark ; 2
	or	byte [setdevmarkflag],2
	call	round			; check for mem error before the stosb
	add	di,ax			;skip over header
	mov	al,'A'
fillloop:
	push	cx			; save count
	mov	cx,SF_ENTRY.size ; 59	; number of bytes to fill
	cld
	rep	stosb			; filled

	;;mov	word [es:di-(SF_ENTRY.size)+SF_ENTRY.sf_ref_count],0  ; [es:di-59]
	;;mov	word [es:di-(SF_ENTRY.size)+SF_ENTRY.sf_position],0   ; [es:di-38]	
	;;mov	word [es:di-(SF_ENTRY.size)+SF_ENTRY.sf_position+2],0 ; [es:di-36]

	;cx = 0
	;mov	[es:di-(SF_ENTRY.size)+SF_ENTRY.sf_ref_count],cx ;0  ; [es:di-59]
	;mov	[es:di-(SF_ENTRY.size)+SF_ENTRY.sf_position],cx ;0   ; [es:di-38]	
	;mov	[es:di-(SF_ENTRY.size)+SF_ENTRY.sf_position+2],cx ;0 ; [es:di-36]
	; 23/10/2022	
	mov     word [es:di-3Bh],0
	mov     word [es:di-26h],0
	mov     word [es:di-24h],0

	pop	cx
	loop	fillloop

; allocate buffers
; ----------------------------------------------------------------------

; search through the list of media supported and allocate 3 buffers if the
; capacity of the drive is > 360kb

	cmp	word [buffers],-1	; has buffers been already set?
	je	short dodefaultbuff
	jmp	dobuff			; the user entered the buffers=.

dodefaultbuff:
	mov	word [h_buffers],0	; default is no heuristic buffers.
	mov	word [buffers],2	; default to 2 buffers

	; 23/10/2022	
	push	ax
	push	ds ; 26/03/2019
	;les	bp,[DOSINFO]		; search through the dpb's
	les	bp,[cs:DOSINFO]
	;;les	bp,[es:bp+SYSI_DPB]	; get first dpb
	;les	bp,[es:bp]
	; 23/10/2022
	les	bp,[es:bp+0]	; ! (MSDOS 5.0 IO.SYS address compability) !	

	push	cs
	pop	ds
;SYSINIT:0DE2h:
nextdpb:				; test if the drive supports removeable media
	;;mov	bl,[es:bp+DPB.drive]
	;mov	bl,[es:bp]
	; 23/10/2022
	mov	bl,[es:bp+0]	; ! (MSDOS 5.0 IO.SYS address compability) !

	inc	bl
	;mov	ax,(IOCTL<<8)|8
	mov	ax,4408h
	int	21h		; DOS - 2+ - IOCTL -

; ignore fixed disks

	or	ax,ax			; ax is nonzero if disk is nonremoveable
	jnz	short nosetbuf

; get parameters of drive

	xor	bx,bx
	;;mov	bl,[es:bp+DPB.drive]
	;mov	bl,[es:bp]
	; 23/10/2022
	mov	bl,[es:bp+0]	; ! (MSDOS 5.0 IO.SYS address compability) !
	
	inc	bl
	mov	dx,deviceparameters
	;mov	ax,(IOCTL<<8)|GENERIC_IOCTL
	mov	ax,440Dh
	;mov	cx,(RAWIO<<8)|GET_DEVICE_PARAMETERS
	mov	cx,860h
	int	21h		; DOS - 2+ - IOCTL -
	jc	short nosetbuf		; get next dpb if driver doesn't support
					; generic ioctl
; determine capacity of drive
; media capacity = #sectors * bytes/sector

	;mov	bx,[deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_TOTALSECTORS]
	; 23/10/2022
	mov	bx,[deviceparameters+15] ; total sectors (16 bit)
	
; to keep the magnitude of the media capacity within a word,
; scale the sector size
; (ie. 1 -> 512 bytes,2 -> 1024 bytes,...)

	;mov	ax,[deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_BYTESPERSECTOR]
	; 23/10/2022
	mov	ax, [deviceparameters+7] ; bytes per sector
	xor	dx,dx
	mov	cx,512
	div	cx			; scale sector size in factor of
					; 512 bytes

	mul	bx			; ax = #sectors * size factor
	or	dx,dx			; just in case of large floppies
	jnz	short setbuf
	cmp	ax,720			; 720 sectors * size factor of 1
	jbe	short nosetbuf
setbuf:
	mov	word [buffers],3
	jmp	short chk_memsize_for_buffers ; now check the memory size
					; for default buffer count
nosetbuf:
	; 23/10/2022
	;cmp	word [es:bp+DPB.NEXT_DPB],-1
	cmp	word [es:bp+19h], -1 ; 0FFFFh
	je	short chk_memsize_for_buffers
	;les	bp,[es:bp+DPB.NEXT_DPB] ; [es:bp+19h]
	les	bp,[es:bp+19h]
	jmp	short nextdpb

;from dos 3.3,the default number of buffers will be changed according to the
;memory size too.
; default buffers = 2
; if diskette media > 360 kb,then default buffers = 3
; if memory size > 128 kb (2000h para),then default buffers = 5
; if memory size > 256 kb (4000h para),then default buffers = 10
; if memory size > 512 kb (8000h para),then default buffers = 15.

chk_memsize_for_buffers:
	cmp	word [MEMORY_SIZE],2000h
	jbe	short bufset
	mov	word [buffers],5
	cmp	word [MEMORY_SIZE],4000h
	jbe	short bufset
	mov	word [buffers],10
	cmp	word [MEMORY_SIZE],8000h
	jbe	short bufset
	mov	word [buffers],15
bufset:
	; 23/10/2022
	; 26/03/2019
	pop	ds
	pop	ax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;j.k. here we should put extended stuff and new allocation scheme!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 26/03/2019

;*******************************************************************************
;									       *
; function: actually allocate buffers in the memory and initialize it. 	       *
; input :								       *
;    memhi:memlo - start of the next available memory			       *
;    buffers = number of buffers					       *
;    h_buffers = number of secondary buffers				       *
;									       *
; output:								       *
;	buffinfo.cache_count - # of caches to be installed.		       *
;	buffinfo set.							       *
;	bufferqueue set.						       *
;									       *
; subroutines to be called:						       *
;									       *
;*******************************************************************************

	; 23/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0E60h)
dobuff:
	; ds = cs ; 31/03/2019
	; 23/10/2022
	lds	bx,[cs:DOSINFO]	; ds:bx -> sysinitvar
	;mov	ax,[buffers] ; 31/03/2019
	;lds	bx,[DOSINFO]
	mov	ax,[cs:buffers]	; set sysi_buffers
	;mov	[bx+SYSI_BUFFERS],ax ; [bx+3Fh]
	mov	[bx+3Fh],ax
	mov	ax,[cs:h_buffers]
	;mov	[bx+SYSI_BUFFERS+2],ax ; [bx+41h]
	mov	[bx+41h],ax
	lds	bx,[bx+12h]
	;lds	bx,[bx+SYSI_BUF] ; now,ds:bx -> buffinfo
	call	round		; get [memhi]:[memlo]
	;mov	al,devmark_buf	; ='B'
	mov	al,'B'	
	call	setdevmark

;allocate buffers

	push	ds			; save buffer info. ptr.
	push	bx

	call	set_buffer

	pop	bx
	pop	ds

;now set the secondary buffer if specified.

	cmp	word [cs:h_buffers],0
	je	short xif16
	call	round
	; 23/10/2022
	mov	cx,[cs:memlo]
	;mov	[bx+BUFFINF.Cache_ptr],cx  ; [bx+6]
	mov	[bx+6],cx
	mov	cx,[cs:memhi]
	;mov	[bx+BUFFINF.Cache_ptr+2],cx ; [bx+8]
	mov	[bx+8],cx
	mov	cx,[cs:h_buffers]
	;mov	[bx+BUFFINF.Cache_count],cx ; [bx+10]
	mov	[bx+10],cx
	mov	ax,512			; 512 byte
	mul	cx
	mov	[cs:memlo],ax
	;or	byte [cs:setdevmarkflag],for_devmark ; 2
	or	byte [cs:setdevmarkflag],2
	call	round
xif16:

; ----------------------------------------------------------------------
; allocate cdss
; ----------------------------------------------------------------------

buf1:
	call	round

	push	ax
	; 23/10/2022
	;mov	ax,devmark_cds		;='L'
	mov	ax, 'L'
	call	setdevmark
	pop	ax

	les	di,[cs:DOSINFO]
	;mov	cl,[es:di+SYSI_NUMIO]
	mov	cl,[es:di+20h]
	cmp	cl,[cs:NUM_CDS]
	jae	short gotncds 		; user setting must be at least numio
	mov	cl,[cs:NUM_CDS]
gotncds:
	xor	ch,ch
	;mov	[es:di+SYSI_NCDS],cl	; [es:di+33]
	mov	[es:di+21h],cl
	mov	ax,[cs:memhi]
	;mov	[es:di+SYSI_CDS+2],ax
	mov	[es:di+18h],ax
	mov	ax,[cs:memlo]
	;mov	[es:di+SYSI_CDS],ax
	mov	[es:di+16h],ax
	mov	al,cl
	;mov	ah,curdirlen ; curdir_list.size
	mov	ah,88
	mul	ah
	call	ParaRound
	add	[cs:memhi],ax

	;or	byte [cs:setdevmarkflag],for_devmark ; 2
	or	byte [cs:setdevmarkflag],2
	call	round			; check for mem error before initializing
	;lds	si,[es:di+SYSI_DPB] ; [es:di+0]
	lds	si,[es:di]
	;les	di,[es:di+SYSI_CDS] ; [es:di+22]
	les	di,[es:di+16h]
	call	fooset

; ----------------------------------------------------------------------
; allocate space for internal stack
; ----------------------------------------------------------------------

	push	cs
	pop	ds

;	if the user did not entered stacks= command, as a default, do not install
;	sytem stacks for pc1,pc xt,pc portable cases.
;	otherwise,install it to the user specified value or to the default
;	value of 9,128 for other systems.

	cmp	word [stack_addr],-1 ; has the user entered "stacks=" command?
	je	short doinstallstack	; then install as specified by the user
	cmp	byte [sys_scnd_model_byte],0 ; pc1,xt has the secondary model byte = 0
	jne	short doinstallstack	; other model should have default stack of 9,128
	cmp	byte [sys_model_byte],0FEh ; pc1, pc/xt or pc portable ?
	jae	short skipstack
doinstallstack:
	mov	ax,[stack_count]	; stack_count = 0?
	or	ax,ax			; then, stack size must be 0 too.
	jz	short skipstack		; don't install stack.

;	dynamic relocation of stack code.

	call	round			;[memhi] = seg. for stack code
					;[memlo] = 0

; set devmark block into memory for mem command
; devmark_id = 's' for stack

	;mov	al,devmark_stk	;='S'
	; 23/10/2022
	mov	al,'S'
	call	setdevmark

	mov	ax,[memhi]
	mov	es,ax		;es -> seg. the stack code is going to move.
	;push	cs
	;pop	ds
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	push	cs
	pop	ds
	xor	si,si		;!!we know that stack code is at the beginning of sysinit.
	xor	di,di
	mov	cx,endstackcode
	mov	[memlo],cx
	call	round		;have enough space for relocation?
	rep	movsb

	push	ds		; stick the location of the NextStack entry
	;;mov	ax,Bios_Data	; into the Win386 Instance Data tables
	;mov	ax,KERNEL_SEGMENT ; 70h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax
	mov	word [NextStack],nextentry ; (8C0h for MSDOS 6.21 IO.SYS)
	mov	[NextStack+2],es	   ; (8C2h for MSDOS 6.21 IO.SYS)

	mov	ax,[cs:memlo]
	mov	[cs:stack_addr],ax ;set for stack area initialization
	mov	[IT_StackLoc],ax ; pass it as Instance Data, too
	mov	ax,[cs:memhi]	;this will be used by stack_init routine.
	mov	[cs:stack_addr+2],ax
	mov	[IT_StackLoc+2],ax

;	space for internal stack area = stack_count(entrysize + stack_size)

	;mov	ax,entrysize ; mov ax,8
	; 23/10/2022
	mov	ax,8
	add	ax,[cs:stack_size]
	mul	word [cs:stack_count]

	mov	[IT_StackSize],ax ; pass through to Instance Tables

	pop	ds		; no more need to access Instance Table

	call	ParaRound	; convert size to paragraphs
	add	[cs:memhi],ax
	;add	[memhi],ax
	;or	byte [cs:setdevmarkflag],for_devmark ; 2
	or	byte [cs:setdevmarkflag],2
	;or	byte [setdevmarkflag],for_devmark 
				;to set the devmark_size for stack by round routine.
	call	round		; check for memory error before
				; continuing
	call	stackinit	; initialize hardware stack. 
				; cs=ds=sysinitseg,es=relocated stack code & data
skipstack:
	; 24/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:0F99h)

	push	cs
	pop	ds

	mov	al,[FILES]
	xor	ah,ah		; do not use cbw instruction!!!!!
				;  it does sign extend.
	mov	cx,ax
	xor	bx,bx		;close standard input
	mov	ah,3Eh ; CLOSE
	int	21h
	mov	bx,2
rcclloop:			;close everybody but standard output
	mov	ah,3Eh ; CLOSE	; need output so we can print message
	int	21h		; in case we can't get new one open.
	inc	bx
	loop	rcclloop

	mov	dx,condev
	mov	al,2
	mov	ah,3Dh ; OPEN 	;open con for read/write
	stc			; set for possible int 24
	int	21h
	jnc	short goaux
	call	badfil
	jmp	short goaux2
goaux:	
	push	ax
	mov	bx,1		;close standard output
	mov	ah,3Eh ; CLOSE
	int	21h
	pop	ax

	mov	bx,ax		;new device handle
	mov	ah,45h ; XDUP
	int	21h		;dup to 1,stdout
	mov	ah,45h ; XDUP
	int	21h		;dup to 2,stderr
goaux2: 
	mov	dx,auxdev
	mov	al,2		;read/write access
	call	open_dev

	mov	dx,prndev
	mov	al,1		;write only
	call	open_dev

;global rearm command for shared interrupt devices attached in the system;
;shared interrupt attachment has some problem when it issues interrupt
;during a warm reboot. once the interrupt is presented by the attachment,
;no further interrupts on that level will be presented until a global rearm
;is issued. by the request of the system architecture group, msbio will
;issue a global rearm after every device driver is loaded.
;to issue a global rearm:	;for pc1,xt,palace
;
;			  out 02f2h,xx  ; interrupt level 2
;			  out 02f3h,xx  ; interrupt level 3
;			  out 02f4h,xx  ; interrupt level 4
;			  out 02f5h,xx  ; interrupt level 5
;			  out 02f6h,xx  ; interrupt level 6
;			  out 02f7h,xx  ; interrupt level 7
;
;	for pc at,in addition to the above commands,
;	need to handle the secondary interrupt handler
;
;			  out 06f2h,xx  ; interrupt level 10
;			  out 06f3h,xx  ; interrupt level 11
;			  out 06f4h,xx  ; interrupt level 12
;			  out 06f6h,xx  ; interrupt level 14
;			  out 06f7h,xx  ; interrupt level 15
;
;	for round-up machine
;
;			  none.

; where xx stands for any value.
;
; for your information,after naples level machine,the system service bios
; call (int 15h),function ah=0c0h returns the system configuration parameters

	; 24/10/2022

	push	ax
	push	bx
	push	dx
	push	es

	mov	al,0FFh 		;reset h/w by writing to port
	mov	dx,2F2h 		;get starting address
	out	dx,al			; out 02f2h,0ffh
	inc	dx
	out	dx,al			; out 02f3h,0ffh
	inc	dx
	out	dx,al			; out 02f4h,0ffh
	inc	dx
	out	dx,al			; out 02f5h,0ffh
	inc	dx
	out	dx,al			; out 02f6h,0ffh
	inc	dx
	out	dx,al			; out 02f7h,0ffh

;sb secondary global rearm

	mov	ax,0F000h		;get machine type
	mov	es,ax
	cmp	byte [es:0FFFEh],0FCh ;q:is it a at type machine
	je	short startrearm	; *if at no need to check

	mov	ah,0C0h 		;get system configuration
	int	15h			; *
	jc	short finishrearm	; *jmp if old rom

; test feature byte for secondary interrupt controller

	test	byte [es:bx+5],40h
	; 24/10/2022
	;test	byte [es:bx+ROMBIOS_DESC.bios_sd_featurebyte1],ScndIntController
	je	short finishrearm	;jmp if it is there

startrearm:
	mov	al,0FFh 		;write any pattern to port
	mov	dx,6F2h 		;get starting address
	out	dx,al			;out 06f2h,0ffh
	inc	dx			;bump address
	out	dx,al			;out 06f3h,0ffh
	inc	dx			;bump address
	out	dx,al			;out 06f4h,0ffh
	inc	dx			;bump address
	inc	dx			;bump address
	out	dx,al			;out 06f6h,0ffh
	inc	dx			;bump address
	out	dx,al			;out 06f7h,0ffh

finishrearm:
	pop	es
	pop	dx
	pop	bx
	pop	ax

;    global rearm end *******************

; ----------------------------------------------------------------------
; allocate sysinit_base for install= command
; ----------------------------------------------------------------------
; sysinit_base allocation.
;   check if endfile has been called to handle install= command.

set_sysinit_base:

; ----------------------------------------------------------------------
;sysinit_base will be established in the secure area of
;lower memory when it handles the first install= command.
;sysinit_base is the place where the actual exec function will be called and
;will check sysinit module in high memory if it is damaged by the application
;program.  if sysinit module has been broken,then "memory error..." message
;is displayed by sysinit_base.
; ----------------------------------------------------------------------

	; 24/10/2022 - Retro DOS v4.0 (MSDOS 5.0 IO.SYS, SYSINIT)
	; (SYSINIT:1028h)

	push	ax			; set devmark for mem command
	mov	ax,[memhi]
	sub	ax,[area]
	mov	[impossible_owner_size],ax ;remember the size in case.
	;mov	al,devmark_inst ; 'T'
	mov	al,'T'
	call	setdevmark
	pop	ax

	mov	di,[memhi]
	mov	es,di
	mov	[sysinit_base_ptr+2],di ; save this entry for the next use.
	xor	di,di
	mov	[sysinit_base_ptr],di	; es:di -> destination.
	mov	si,sysinit_base		;ds:si -> source code to be relocated.
	mov	cx,end_sysinit_base-sysinit_base ; 129
	; 24/10/2022 
	;mov	cx,128	; 11DCh-115Ch 	; (MSDOS 5.0 IO.SYS, SYSINIT)
	add	[memlo],cx
	;or	byte cs:[setdevmarkflag],for_devmark ; 2
	or	byte [cs:setdevmarkflag],2
	;or	byte [setdevmarkflag],for_devmark
	call	round			; check mem error. also,readjust memhi for the next use.
	rep	movsb			; reallocate it.

	mov	word [sysinit_ptr],sysinitptr ; returning address from
	mov	[sysinit_ptr+2],cs	 ; sysinit_base back to sysinit.
	;or	word [install_flag],has_installed ; set the flag.
	;or	byte [install_flag],has_installed ; 2
	; 24/10/2022
	or	word [install_flag],2	

; ----------------------------------------------------------------------
; free the rest of the memory from memhi to confbot. still from confbot to
; the top of the memory will be allocated for sysinit and config.sys if
; have_install_cmd.
; ----------------------------------------------------------------------

	call	round
	mov	bx,[memhi]
	mov	ax,[area]
	mov	[old_area],ax		; save [area]
	mov	es,ax			;calc what we needed
	sub	bx,ax
	; 24/10/2022
	mov	ah,4Ah ; SETBLOCK
	int	21h			;give the rest back
		; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
		; ES = segment address of block to change
		; BX = new size in paragraphs
	push	es
	mov	ax,es
	dec	ax
	mov	es,ax			;point to arena
	;mov	word [es:ARENA.OWNER],8	;set impossible owner
	;;mov	word [es:ARENA.NAME],4453h	; System Data
	;mov	word [es:ARENA.NAME],'SD'	; System Data
	; 24/10/2022
	mov	word [es:1],8		;set impossible owner
	mov	word [es:8],'SD'	; System Data
	pop	es

	mov	bx,0FFFFh
	mov	ah,48h ; ALLOC
	int	21h
	mov	ah,48h ; ALLOC
	int	21h			; allocate the rest of the memory
		; DOS - 2+ - ALLOCATE MEMORY
		; BX = number of 16-byte paragraphs desired
	mov	[memhi],ax		; start of the allocated memory
	mov	word [memlo],0		;  to be used next.

;;;; at this moment,memory from [memhi]:0 to top-of-the memory is
;;;; allocated.
;;;; to protect sysinit,confbot module (from confbot (or =alloclim at
;;;; this time) to the top-of-the memory),here we are going to
;;;; 1). "setblock" from memhi to confbot.
;;;; 2). "alloc" from confbot to the top of the memory.
;;;; 3). "free alloc memory" from memhi to confbot.

;memory allocation for sysinit,confbot module.

	mov	es,ax
	;mov	bx,[CONFBOT]
	; 24/10/2022
	mov	bx,[top_of_cdss]
	sub	bx,ax			; confbot - memhi
	dec	bx			; make a room for the memory block id.
	dec	bx			; make sure!!!.
	mov	ah,4Ah ; SETBLOCK
	int	21h			; this will free (confbot to top of memory)
		; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
		; ES = segment address of block to change
		; BX = new size in paragraphs
	mov	bx,0FFFFh
	mov	ah,48h ; ALLOC
	int	21h
	mov	ah,48h ; ALLOC
	int	21h			; allocate (confbot to top of memory)
		; DOS - 2+ - ALLOCATE MEMORY
		; BX = number of 16-byte paragraphs desired
	mov	[area],ax		; save allocated memory segment.
					; need this to free this area for command.com.
	mov	es,[memhi]
	mov	ah,49h			; free allocated memory.
	int	21h			; free (memhi to confbot(=area))
		; DOS - 2+ - FREE MEMORY
		; ES = segment address of area to be freed
endfile_ret:
	retn

; End of "EndFile" DOS structure configuration.

; ----------------------------------------------------------------------
; 26/03/2019 - Retro DOS v4.0
; 24/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)	
; ----------------------------------------------------------------------
; Do_Install_Exec
;
; This procedure is used to EXEC a program being loaded via the 
; "install=" mechanism in config.sys. It does this by setting up
; the parameters, and then jumping to sysinit_base, which has been
; setup in low memory. When complete, sysinit_base will jump back
; up to this procedure (if sysinit remains uncorrupted by the installed
; program).

;SYSINIT:10CFh:

do_install_exec:			; now,handles install= command.

	push	si			; save si for config.sys again.

; we are going to call load/exec function.
; set es:bx to the parameter block here;;;;;;;
; set ds:dx to the asciiz string. remember that we already has 0
; after the filename. so parameter starts after that. if next
; character is a line feed (i.e. 10),then assume that the 0
; we already encountered used to be a carrage return. in this
; case,let's set the length to 0 which will be followed by
; carridge return.

; es:si -> command line in config.sys. points to the first non blank
;character after =.

	push	es
	push	ds
	pop	es
	pop	ds			; es->sysinitseg,ds->confbot seg
	mov	dx,si			; ds:dx->file name,0 in config.sys image.

	xor	cx,cx
	cld
	mov	byte [cs:ldexec_start],' ' ; clear out the parm area
	mov	di,ldexec_parm
installfilename:			;  skip the file name
	lodsb				;  al = ds:si; si++
	cmp	al,0
	je	short got_installparm
	jmp	short installfilename
got_installparm:			;  copy the parameters to ldexec_parm
	lodsb
	mov	[es:di],al
	cmp	al,lf	; cmp al,0Ah	;  line feed?
	je	short done_installparm
	inc	cl			;  # of char. in the parm.
	inc	di
	jmp	short got_installparm
done_installparm:
	mov	byte [cs:ldexec_line],cl ; length of the parm.
	cmp	cl,0			;if no parm,then
	jne	short install_seg_set 	; let the parm area
	mov	byte [cs:ldexec_start],cr ; 0Dh 
					; starts with cr.
install_seg_set:
	mov	word [cs:0],0		; make a null environment segment
	mov	ax,cs			; by overlap jmp instruction of sysinitseg.

;---------------------------------------------------M067----------------
;
; 	the environment pointer is made 0. so the current environment ptr.
; 	will be the same as pdb_environ which after dosinit is 0.
;
; 	mov	cs:[instexe.exec0_environ],0 ; set the environment seg.
;
; 	instexe.exec0_environ need not be initialized to 0 above. It was
; 	done as a fix for bug #529. The actual bug was in NLSFUNC and
; 	was fixed. 
;
; ----------------------------------------------------------------------

;;ifdef   MULTI_CONFIG

; If there's any environment data in "config_wrkseg", pass to app

	; 24/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS SYSINIT)
        ;mov     cx,ax
        ;cmp     word [cs:config_envlen],0
        ;je      short no_envdata2
        ;mov     cx,[cs:config_wrkseg]
;no_envdata2:
;;endif  ;MULTI_CONFIG

	;mov	[cs:instexe.exec0_environ],cx ; set the environment seg.
	; 24/10/2022
	;mov	[cs:iexec.environ],cx
	; 02/11/2022
	mov	[cs:iexec.environ],ax	

	;mov	[cs:instexe.exec0_com_line+2],ax ; set the seg.
	mov	[cs:iexec.ldexec_line+2],ax
	;mov	[cs:instexe.exec0_5c_fcb+2],ax
	mov	[cs:iexec.ldexec_5c_fcb+2],ax
	;mov	[cs:instexe.exec0_6c_fcb+2],ax
	mov	[cs:iexec.ldexec_6c_fcb+2],ax
	call	sum_up
	mov	[es:checksum],ax	; save the value of the sum
	xor	ax,ax
	mov	ah,4Bh ; EXEC		; load/exec
	mov	bx,instexe		; es:bx -> parm block.
	push	es			; save es,ds for load/exec
	push	ds			; these registers will be restored in sysinit_base.
	jmp	far [cs:sysinit_base_ptr] ; jmp to sysinit_base to execute
					; load/exec function and check sum.

;----------------------------------------

;j.k. this is the returning address from sysinit_base.

	; 24/10/2022 - Retro DSOS v4.0 (Modified MSDOS 5.0 IO.SYS SYSINIT)

sysinitptr:				; returning far address from sysinit_base
	pop	si			; restore si for config.sys file.
	push	es
	push	ds
	pop	es
	pop	ds			; now ds - sysinitseg,es - confbot
        jnc     short install_exit_ret

	push	si			; error in loading the file for install=.
	call	badload 		; es:si-> path,filename,0.
	pop	si

	; 24/10/2022
	jmp	short sysinitptr_retn ; (MSDOS 5.0 IO.SYS, SYSINIT:1140h)

install_exit_ret:
	;retn

	; 24/10/2022 (MSDOS 5.0 IO.SYS SYSINIT)
;SYSINIT:1142h:
	mov     ah, 4Dh
	int     21h             ; DOS - 2+ - GET EXIT CODE OF SUBPROGRAM (WAIT)
	cmp     ah, 3
	jz      short sysinitptr_retn
	call    error_line
	stc
sysinitptr_retn:	; (SYSINIT:114fh)
	retn		

; ----------------------------------------------------------------------

;**	ParaRound - Round Up length to paragraph multiple
;
;	ParaRound rounds a byte count up to a multiple of 16, then divides
;	by 16 yielding a "length in paragraphs" value.
;
;	ENTRY	(ax) = byte length
;	EXIT	(ax) = rounded up length in paragraphs
;	USES	ax, flags

ParaRound:
	add	ax,15
	rcr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	retn

; ----------------------------------------------------------------------
; sysinit_base module.
;
; This module is relocated by the routine EndFile to a location in low
; memory. It is then called by SYSINIT to perform the EXEC of programs
; that are being loaded by the "install=" command. After the EXEC call
; completes, this module performs a checksum on the SYSINIT code (at the
; top of memory) to be sure that the EXECed program did not damage it.
; If it did, then this module will print an error message and stop the
; system. Otherwise, it returns control to SYSINIT.
;
;in: after relocation,
;    ax = 4b00h - load and execute the program dos function.
;    ds = confbot. segment of config.sys file image
;    es = sysinitseg. segment of sysinit module itself.
;    ds:dx = pointer to asciiz string of the path,filename to be executed.
;    es:bx = pointer to a parameter block for load.
;    SI_end (byte) - offset vaule of end of sysinit module label
;    bigsize (word) - # of word from confbot to SI_end.
;    chksum (word) - sum of every byte from confbot to SI_end in a
;			word boundary moduler form.
;    sysinit_ptr (dword ptr) - return address to sysinit module.
;
;note: sysinit should save necessary registers and when the control is back

	; 24/10/2022
	; (SYSINIT:115Ch for MSDOS 5.0 SYSINIT)
sysinit_base:				
	mov	[cs:sysinit_base_ss],ss	; save stack
	mov	[cs:sysinit_base_sp],sp	
	int	21h			; load/exec dos call.
	mov	ss,[cs:sysinit_base_ss]	; restore stack
	mov	sp,[cs:sysinit_base_sp]
	pop	ds			; restore confbot seg
	pop	es			; restore sysinitseg
	jc	short sysinit_base_end	; load/exec function failed.
					; at this time,i don't have to worry about
					; that sysinit module has been broken or not.
	call	sum_up			; otherwise,check if it is good.
	cmp	[es:checksum],ax
	je	short sysinit_base_end

;	memory broken. show "memory allocation error" message and stall.

	mov	ah,9
	push	cs
	pop	ds
	mov	dx,mem_alloc_err_msgx-sysinit_base ; 65h (for MSDOS 5.0 SYSINIT)
	int	21h
		; DOS - PRINT STRING
		; DS:DX -> string terminated by "$"
	; 24/10/2022
_stall:  
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;hlt				;use HLT to minimize energy consumption
        jmp	short _stall

sysinit_base_end: 
	jmp	far [es:sysinit_ptr]	;return back to sysinit module

;-------------------------------------

sum_up:

;in:   es - sysinitseg.
;out:  ax - result
;
;remark: since this routine will only check starting from "locstack" to the end of
;	 sysinit segment,the data area, and the current stack area are not
;	 coverd. in this sense,this check sum routine only gives a minimal
;	 gaurantee to be safe.
;
;first sum up confbot seg.

	push	ds
	;mov	ax,[es:CONFBOT]
	; 24/10/2022
	mov	ax,[es:top_of_cdss]
	mov	ds,ax
	xor	si,si
	xor	ax,ax
	mov	cx,[es:config_size]	; if config_size has been broken,then this
					;whole test better fail.
	shr	cx,1			; make it a word count
	jz	short sum_sys_code	; when config.sys file not exist.
sum1:
	add	ax,[si]
	inc	si
	inc	si
	loop	sum1
;now,sum up sysinit module.
sum_sys_code:
	; 24/10/2022
	mov	si,locstack ; 5A6h (MSDOS 5.0 IO.SYS, SYSINIT)
				        ; starting after the stack.  M069
					;  this does not cover the possible stack code!!!
	;;mov	cx,22688  ; for MSDOS 6.21 IO.SYS
	; 02/11/2022
	mov	cx,3D20h  ; (15648) for MSDOS 5.0 IO.SYS (SYSINIT)	
	;mov	cx,SI_end ; (22688) 	; SI_end is the label at the end of sysinit
	sub	cx,si			;  from after_checksum to SI_end
	shr	cx,1
sum2:
	add	ax,[es:si]
	inc	si
	inc	si
	loop	sum2
	pop	ds
	retn

; 24/10/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)

sysinit_base_ss equ $-sysinit_base  ; = 61 (MSDOS 5.0 IO.SYS, SYSINIT:115Ch)
;SYSINIT:11BDh:
	dw	0
sysinit_base_sp equ $-sysinit_base  ; = 63 (MSDOS 5.0 IO.SYS, SYSINIT:1161h)
;SYSINIT:11BFh:
	dw	0

mem_alloc_err_msgx:

       ;include msbio.cl4		; memory allocation error message

;SYSINIT:12F6:  ; MSDOS 6.21 IO.SYS SYSINIT:12F6h
	db	0Dh,0Ah
	db 	'Memory allocation error $'

end_sysinit_base: ; label byte
	; 24/10/2022
	; (SYSINIT:11DCh for MSDOS 5.0 SYSINIT)

; ----------------------------------------------------------------------
; Set_Buffer
;
;function: set buffers in the real memory.				  
;	   lastly set the memhi,memlo for the next available free address.
;
;input:    ds:bx -> buffinfo.
;	   [memhi]:[memlo = 0] = available space for the hash bucket.	  
;	   singlebuffersize = buffer header size + sector size		  
;
;output:   buffers Queue established.	       				   
;	   [memhi]:[memlo] = address of the next available free space.	   
; ----------------------------------------------------------------------

	; 25/10/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)
	; (SYSINIT:11DCh)

set_buffer:
	xor	dl,dl				; assume buffers not in HMA
	call	GetBufferAddr
	jz	short set_buff_1
	mov	dl,1				; buffers in HMA
set_buff_1:
	; 25/10/2022
	;mov	[bx+BUFFINF.Buff_Queue],di	; head of Buff Q
	mov	[bx],di
	;mov	[bx+BUFFINF.Buff_Queue+2],es
	mov	[bx+2],es
	;mov	word [bx+BUFFINF.Dirty_Buff_Count],0 ;set dirty_count to 0.
	mov	word [bx+4], 0

	mov	ax,di
	mov	cx,[cs:buffers]
	push	di				; remember first buffer

;	for each buffer

nxt_buff:
	call	set_buffer_info 		; set buf_link,buf_id...
	mov	di,ax
	loop	nxt_buff

	sub	di,[cs:singlebuffersize]	; point to last buffer

	pop	cx				; get first buffer
	;mov	[es:di+buffinfo.buf_next],cx	; last->next = first
	mov	[es:di],cx
	xchg	cx,di
	;mov	[es:di+buffinfo.buf_prev],cx	; first->prev = last
	; 25/10/2022
	mov	[es:di+2],cx

	or	dl,dl				; In HMa ?
	jz	short set_buff_2		; no
	;mov	byte [bx+BUFFINF.Buff_In_HMA],1
	mov	byte [bx+12],1
	mov	ax,[cs:memhi]			; seg of scratch buff
	;mov	word [bx+BUFFINF.Lo_Mem_Buff],0	; offset of sctarch buff is 0
	mov	word [bx+13],0
	;mov	[bx+BUFFINF.Lo_Mem_Buff+2],ax
	mov	word [bx+15],ax
	mov	ax,[cs:singlebuffersize]	; size of scratch buff
	;sub	ax,bufinsiz ; 20		; buffer head not required
	sub	ax,20
set_buff_2:
	add	[cs:memlo],ax
	;or	byte [cs:setdevmarkflag],for_devmark ; 2
	or	byte [cs:setdevmarkflag],2
	call	round
	retn

; ----------------------------------------------------------------------
; procedure : GetBufferAddr
;
;	      Gets the buffer address either in HMA or in Lo Mem
;
; returns in es:di the buffer adress
; returns NZ if allocated in HMA
; ----------------------------------------------------------------------

	; 25/10/2022 
GetBufferAddr:
	push	bx
	push	dx
	mov	ax, [cs:singlebuffersize]
	mul	word [cs:buffers]
	;add	ax,0Fh
	add	ax,15 
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	and	ax,~15	; 0FFF0h	; para round
	;and	al,~15	; 0F0h
	mov	bx,ax
	mov	ax,4A02h
	;mov	ax,((multMULT<<8)+multMULTALLOCHMA)
	int	2Fh
	cmp	di,0FFFFh
	jne	short got_hma
	mov	di,0			; dont xor di,di Z flag needed
	;zf=1
	;xor	di,di	; 25/10/2022
	;zf=1
	mov	es,[cs:memhi]
got_hma:
	pop	dx
	pop	bx
	retn

; ----------------------------------------------------------------------

set_buffer_info:

;function: set buf_link,buf_id,buf_sector
;
;in: es:di -> buffer header to be set.
;    ax = di
;
;out:
;    above entries set.

	; 25/10/2022 
	push	word [cs:buf_prev_off]
	;pop	word [es:di+buffinfo.buf_prev]
	pop	word [es:di+2]
	mov	[cs:buf_prev_off],ax
	add	ax,[cs:singlebuffersize]	;adjust ax
	;mov	[es:di+buffinfo.buf_next],ax
	mov	[es:di],ax
	;mov	word [es:di+buffinfo.buf_ID],00FFh  ; new buffer free
	mov	word [es:di+4],00FFh
	;mov	word [es:di+buffinfo.buf_sector],0   ; to compensate the masm 3 bug
	mov	word [es:di+6],0
	;mov	word [es:di+buffinfo.buf_sector+2],0 ; to compensate the masm 3 bug
	mov	word [es:di+8],0
	retn

; ======================================================================
; MSSTACK initialization routine - MSDOS 6.0 - SYSDINIT1.ASM - 1991
; ----------------------------------------------------------------------
; 27/03/2019 - Retro DOS v4.0

; ----------------------------------------------------------------------
; ibmstack initialization routine.
;
;	to follow the standard interrupt sharing scheme, msstack.asm
;	has been modified. this initialization routine also has to
;	be modified because for the interrupt level 7 and 15, firstflag
;	should be set to signal that this interrupt handler is the
;	first handler hooked to this interrupt vector.
;	we determine this by looking at the instruction pointed by
;	this vector. if it is iret, then this handler should be the
;	first one. in our case, only the interrupt vector 77h is the
;	interrupt level 15. (we don't hook interrupt level 7.)
;
;	the followings are mainly due to m.r.t; ptm fix of p886 12/3/86
;	some design changes are needed to the above interrupt sharing
;	method. the above sharing scheme assumes that 1). interrupt
;	sharing is never done on levels that have bios support. 2). "phantom"
;	interrupts would only be generated on levels 7 and 15.
;	these assumptions are not true any more. we have to use the firstflag
;	for every level of interrupt. we will set the firstflag on the following
;	conditions:
;
;	 a.	 if the cs portion of the vector is 0000, then "first"
;	 b. else if cs:ip points to valid shared header, then not "first"
;	 c. else if cs:ip points to an iret, then "first"
;	 d. else if cs:ip points to dummy, then "first"
;
;	where dummy is - the cs portion must be f000, and the ip portion must
;	be equal to the value at f000:ff01. this location is the initial value
;	from vector_table for interrupt 7, one of the preserved addresses in all
;	the bioses for all of the machines.
;
;	system design group requests bios to handle the phantom interrupts.
;
;	the "phantom" interrupt is an illegal interrupt such as an interrupt
;	produced by the bogus adapter card even without interrupt request is
;	set.  more specifically, 1). the 8259 has a feature when running in
;	edge triggered mode to latch a pulse and present the interrupt when
;	the processor indicates interrupt acknowledge (inta). the interrupt
;	pulse was exist at the time of inta to get a "phantom" interrupt.
;	2). or, this is caused by adapter cards placing a glitch on the
;	interrupt line.
;
;	to handle those "phantom" interrupts, the main stack code will check
;	the own firstflag, and if it is not "first" (which means the forward
;	pointer points to the legal shared interrupt handler), then pass the
;	control. if it is the first, then the following action should be
;	taken. we don't have to implement skack logic in this case.
;
;	to implement this logic, we rather choose a simple method.
;	if ont of the above "firstflag" conditions is met, we are not
;	going to hook this interrupt vector. the reason is if the original
;	vector points to "iret" and do nothing, we don't need
;	to implement the stack logic for it. this will simplify implementation
;	while maintaining compatibility with the old version of dos.
;	this implies that in the main stack code, there might be a stack code
;	that will never be used, a dead code.
;
;in - cs, ds -> sysinitseg, es -> relocated stack code & data.

	; 25/10/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)
	; (SYSINIT:1287h)

stackinit:
	push	ax
	push	ds
	push	es
	push	bx
	push	cx
	push	dx
	push	di
	push	si
	push	bp

;currently es -> stack code area

	;mov	ax,[stack_count]
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	; (MSDOS 5.0 IO.SYS - SYSINIT:1290h)
	mov	ax,[cs:stack_count] ; !!	;defined in cs
	mov	[es:stackcount],ax		;defined in stack code area
	; (MSDOS 5.0 IO.SYS - SYSINIT:1298h)
	mov	ax,[stack_size]	 ; !!		;in cs
	mov	[es:stacksize],ax
	;mov	ax,[stack_addr]			; offset
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	; (MSDOS 5.0 IO.SYS - SYSINIT:129Fh)
	mov	ax,[cs:stack_addr]  ; !!
	mov	[es:stacks],ax
	;mov	ax,[stack_addr+2]
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	; (MSDOS 5.0 IO.SYS - SYSINIT:129Fh)
	mov	ax,[cs:stack_addr+2] ; !!	; segment
	mov	[es:stacks+2],ax

; initialize the data fields with the parameters

; "firstentry" will always be at stacks

	mov	bp,[es:stacks]			; get offset of stack
	mov	[es:firstentry],bp

; the stacks will always immediately follow the table entries

	mov	ax,entrysize ; 8
	mov	cx,[es:stackcount]
	mul	cx
	add	ax,bp
	mov	[es:stackat],ax
	mov	bx,ax
	sub	bx,2

; zero the entire stack area to start with

	mov	di,[es:stackat]
	mov	ax,[es:stacksize]
	mul	cx
	mov	cx,ax
	xor	ax,ax
	push	es
	pop	ds				;ds = relocated stack code seg.

;now, ds -> stack code area

	mov	es,[stacks+2]			; get segment of stack area.
	cld
	rep	stosb

	mov	cx,[stackcount]

; loop for "count" times, building a table entry
;  cs = sysinitseg, ds = relocated stack code seg, es = segment of stack space
;  cx = number of entries
;  es:bp => base of stacks - 2
;  es:bx => first table entry

buildloop:
	mov	byte [es:bp+allocbyte],free	; mov [es:bp+0],0
	; 25/10/2022
	;mov	byte [es:bp],free
	mov	[es:bp+intlevel],al	; ax = 0
	;mov	[es:bp+1],al
	mov	[es:bp+savedsp],ax
	;mov	[es:bp2],ax
	mov	[es:bp+savedss],ax
	;mov	[es:bp+4],ax
	add	bx,[stacksize]
	mov	[es:bp+newsp],bx		; mov [es:bp+6],bx
	;mov	[es:bp+6],bx
	mov	[es:bx],bp
	add	bp,entrysize ; 8

	loop	buildloop

	sub	bp,entrysize ; 8
	mov	[lastentry],bp
	mov	[nextentry],bp

	push	ds
	mov	ax,0F000h		;look at the model byte
	mov	ds,ax
	cmp	byte [0FFFEh],0F9h ; mdl_convert ;convertible?
	pop	ds
	jne	short skip_disablenmis

	mov	al,07h			; disable convertible nmis
	out	72h,al

skip_disablenmis:
	xor	ax,ax
	mov	es,ax			;es - segid of vector table at 0
					;ds - relocated stack code segment
	cli

	;irp	aa,<02,08,09,70>
	;
	;mov	si,aa&h*4		;pass where vector is to be adjusted
	;mov	di,offset int19old&aa	;we have to set old&aa for int19 handler too.
	;mov	bx,offset old&aa	;pass where to save original owner pointer
	;mov	dx,offset int&aa	;pass where new handler is
	;call	new_init_loop		;adjust the vector to new handler,
	;				;saving pointer to original owner
	;endm

stkinit_02:
	mov	si,02h*4 ; 8
	mov	di,INT19OLD02
	mov	bx,old02
	mov	dx,int02
	call	new_init_loop
stkinit_08:
	mov	si,08h*4 ; 32
	mov	di,INT19OLD08
	mov	bx,old08
	mov	dx,int08
	call	new_init_loop
stkinit_09:
	mov	si,09h*4 ; 36
	mov	di,INT19OLD09
	mov	bx,old09
	mov	dx,int09
	call	new_init_loop
stkinit_70:
	mov	si,70h*4 ; 448
	mov	di,INT19OLD70
	mov	bx,old70
	mov	dx,int70
	call	new_init_loop

	;irp	aa,<0a,0b,0c,0d,0e,72,73,74,76,77> ;shared interrupts
	;
	;mov	si,aa&h*4		;pass where vector is to be adjusted
	;push	ds			;save relocated stack code segment
	;lds	bx, es:[si]		;ds:bx -> original interrupt handler
	;push	ds
	;pop	dx			;dx = segment value
	;	
	;cmp	dx,0
	;jz	int&aa&_first
	;
	;cmp	byte ptr ds:[bx],0cfh	;does vector point to an iret?
	;jz	int&aa&_first
	;
	;cmp	word ptr ds:[bx.6],424bh ;magic offset (see int&aa, msstack.inc)
	;jz	int&aa&_not_first
	;
	;cmp	dx,0f000h		;rom bios segment
	;jnz	int&aa&_not_first
	;
	;push	es
	;push	dx
	;mov	dx,0f000h
	;mov	es,dx
	;cmp	bx,word ptr es:0ff01h
       	;pop	dx
	;pop	es
	;jz	int&aa&_first
	;
;int&aa&_not_first:			;not the first. we are going to hook vector.
	;pop	ds
	;mov	di, offset int19old&aa	;we have to set old&aa for int19 handler too.
	;mov	bx, offset old&aa	;pass where to save original owner pointer
	;mov	dx, offset int&aa	;pass where new handler is
	;call	new_init_loop		;adjust the vector to new handler, saving
	;				;pointer to original owner.
	;jmp	short int&aa&_end
;int&aa&_first:				;the first. don't have to hook stack code.
	;pop	ds
;int&aa&_end:
	;
	;endm

stkinit_0A:
	mov	si,0Ah*4 ; 40
	
	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_0A_first
	
	cmp	byte [bx],0CFh
	je	short int_0A_first
	
	cmp	word [bx+6],424Bh
	je	short int_0A_not_first
	
	cmp	dx,0F000h
	jne	short int_0A_not_first
	
	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
       	pop	dx
	pop	es
	je	short int_0A_first

	; 25/10/2022
	;call	int_xx_first_check ; 27/03/2019 - Retro DOS v4.0
	;jnc	short int_0A_first
	
int_0A_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD0A
	mov	bx,old0A
	mov	dx,int0A
	call	new_init_loop
	jmp	short int_0A_end

; -----------------------------------

; 25/10/2022
;
;	; 27/03/2019 - Retro DOS v4.0
;int_xx_first_check:
;	push	ds
;	lds	bx,[es:si]
;	push	ds
;	pop	dx
;		
;	cmp	dx,0
;	je	short int_xx_first
;	
;	cmp	byte [bx],0CFh
;	je	short int_xx_first
;	
;	cmp	word [bx+6],424Bh
;	je	short int_xx_not_first
;	
;	cmp	dx,0F000h
;	jne	short int_xx_not_first
;
;	push	es
;	;push	dx
;	;mov	dx,0F000h
;	mov	es,dx
;	cmp	bx,[es:0FF01h]
;      	;pop	dx
;	pop	es
;	je	short int_xx_first
;
;int_xx_not_first:
;	stc
;int_xx_first:
;	pop	ds
;	retn

; -----------------------------------

int_0A_first:
	; 25/10/2022
	pop	ds
int_0A_end:

stkinit_0B:
	mov	si,0Bh*4 ; 44
	
	; 25/10/2022
	;call	int_xx_first_check ; 27/03/2019 - Retro DOS v4.0
	;jnc	short int_0B_end ; int_0B_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_0B_first

	cmp	byte [bx],0CFh
	je	short int_0B_first
	
	cmp	word [bx+6],424Bh
	je	short int_0B_not_first
	
	cmp	dx,0F000h
	jne	short int_0B_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_0B_first
	
int_0B_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD0B
	mov	bx,old0B
	mov	dx,int0B
	call	new_init_loop
	jmp	short int_0B_end

int_0B_first:
	; 25/10/2022
	pop	ds
int_0B_end:
	
stkinit_0C:
	mov	si,0Ch*4 ; 48
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_0B_end ; int_0C_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_0C_first

	cmp	byte [bx],0CFh
	je	short int_0C_first
	
	cmp	word [bx+6],424Bh
	je	short int_0C_not_first
	
	cmp	dx,0F000h
	jne	short int_0C_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_0C_first
	
int_0C_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD0C
	mov	bx,old0C
	mov	dx,int0C
	call	new_init_loop
	jmp	short int_0C_end
int_0C_first:
	; 25/10/2022
	pop	ds
int_0C_end:

stkinit_0D:
	mov	si,0Dh*4 ; 52
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_0D_end ; int_0D_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_0D_first

	cmp	byte [bx],0CFh
	je	short int_0D_first
	
	cmp	word [bx+6],424Bh
	je	short int_0D_not_first
	
	cmp	dx,0F000h
	jne	short int_0D_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_0D_first
	
int_0D_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD0D
	mov	bx,old0D
	mov	dx,int0D
	call	new_init_loop
	jmp	short int_0D_end
	; 02/11/2022
int_0D_first:
	pop	ds
int_0D_end:

stkinit_0E:
	mov	si,0Eh*4 ; 56
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_0E_end ; int_0E_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_0E_first

	cmp	byte [bx],0CFh
	je	short int_0E_first
	
	cmp	word [bx+6],424Bh
	je	short int_0E_not_first
	
	cmp	dx,0F000h
	jne	short int_0E_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_0E_first
	
int_0E_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD0E
	mov	bx,old0E
	mov	dx,int0E
	call	new_init_loop
	jmp	short int_0E_end

int_0E_first:
	; 25/10/2022
	pop	ds	
int_0E_end:

stkinit_72:
	mov	si,72h*4 ; 456
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_72_end ; int_72_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_72_first

	cmp	byte [bx],0CFh
	je	short int_72_first
	
	cmp	word [bx+6],424Bh
	je	short int_72_not_first
	
	cmp	dx,0F000h
	jne	short int_72_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_72_first
	
int_72_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD72
	mov	bx,old72
	mov	dx,int72
	call	new_init_loop
	jmp	short int_72_end

int_72_first:
	; 25/10/2022
	pop	ds
int_72_end:

stkinit_73:
	mov	si,73h*4 ; 460
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_73_end ; int_73_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_73_first

	cmp	byte [bx],0CFh
	je	short int_73_first
	
	cmp	word [bx+6],424Bh
	je	short int_73_not_first
	
	cmp	dx,0F000h
	jne	short int_73_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_73_first
	
int_73_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD73
	mov	bx,old73
	mov	dx,int73
	call	new_init_loop
	jmp	short int_73_end

int_73_first:
	; 25/10/2022
	pop	ds
int_73_end:

stkinit_74:
	mov	si,74h*4 ; 464
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_74_end ; int_74_first
	
	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_74_first

	cmp	byte [bx],0CFh
	je	short int_74_first
	
	cmp	word [bx+6],424Bh
	je	short int_74_not_first
	
	cmp	dx,0F000h
	jne	short int_74_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_74_first

int_74_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD74
	mov	bx,old74
	mov	dx,int74
	call	new_init_loop
	jmp	short int_74_end

int_74_first:
	; 25/10/2022
	pop	ds
int_74_end:

stkinit_76:
	mov	si,76h*4 ; 472
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_76_end ; int_76_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_76_first

	cmp	byte [bx],0CFh
	je	short int_76_first
	
	cmp	word [bx+6],424Bh
	je	short int_76_not_first
	
	cmp	dx,0F000h
	jne	short int_76_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_76_first
	
int_76_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD76
	mov	bx,old76
	mov	dx,int76
	call	new_init_loop
	jmp	short int_76_end

int_76_first:
	; 25/10/2022
	pop	ds
int_76_end:

stkinit_77:
	mov	si,77h*4 ; 476
	
	; 25/10/2022
	;call	int_xx_first_check
	;jnc	short int_77_end ; int_77_first

	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_77_first

	cmp	byte [bx],0CFh
	je	short int_77_first
	
	cmp	word [bx+6],424Bh
	je	short int_77_not_first
	
	cmp	dx,0F000h
	jne	short int_77_not_first

	push	es
	push	dx
	mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
	pop	dx
	pop	es
	je	short int_77_first
	
int_77_not_first:
	; 25/10/2022
	pop	ds
	mov	di,INT19OLD77
	mov	bx,old77
	mov	dx,int77
	call	new_init_loop
	jmp	short int_77_end

int_77_first:
	; 25/10/2022
	pop	ds

int_77_end:
	push	ds
	mov	ax,0F000h		; look at the model byte
	mov	ds,ax
	cmp	byte [0FFFEh],0F9h ; mdl_convert ; pc convertible?
	pop	ds
	jne	short skip_enablenmis

	mov	al,27h			; enable convertible nmis
	out	72h,al

; 25/10/2022
; (MSDOS 5.0 SYSINIT:15FBh)

skip_enablenmis:
	sti
	;;mov	ax,Bios_Data ; 70h
	;mov	ax,KERNEL_SEGMENT ; 70h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax

	;mov	[640h],1 ; SYSINIT:1736h for MSDOS 6.21 IO.SYS

	mov	byte [INT19SEM],1	; indicate that int 19
					; initialization is complete

	pop	bp			; restore all
	pop	si
	pop	di
	pop	dx
	pop	cx
	pop	bx
	pop	es
	pop	ds
	pop	ax
	retn

; ----------------------------------------------------------------------
; 27/03/2019 - Retro DOS v4.0

; 25/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)
; (SYSINIT:1610h)

new_init_loop:

;input: si=ofset into vector table of the particular int vector being adjusted
;	bx=ds:offset of oldxx, where will be saved the pointer to original owner
;	dx=ds:offset of intxx, the new interrupt handler
;	di=offset value of int19old&aa variable in bios.
;	es=zero, segid of vector table
;	ds=relocated stack code segment

	mov	ax,[es:si]		;remember offset in vector
	mov	[bx],ax			; to original owner in ds
	mov	ax,[es:si+2]		;remember segid in vector
	mov	[bx+2],ax		; to original owner in ds

	push	ds
	;;mov	ax,Bios_Data ; 70h
	;mov	ax,KERNEL_SEGMENT ; 70h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax			;set int19oldxx value in bios for
	mov	ax,[es:si]		;int 19 handler
	mov	[di],ax
	mov	ax,[es:si+2]
	mov	word [di+2],ax
	pop	ds

	mov	[es:si],dx  	;set vector to point to new int handler
	mov	[es:si+2],ds
	retn

; End of STACK initialization routine
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
;set the devmark for mem command.
;in: [memhi] - the address to place devmark
;    [memlo] = 0
;    al = id for devmark_id
;out: devmark established.
;     the address saved in cs:[devmark_addr]
;     [memhi] increase by 1.
; ----------------------------------------------------------------------

; 25/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS, SYSINIT)
; (SYSINIT:1637h)

setdevmark:

	push	es
	push	cx

	mov	cx,[cs:memhi]
	mov	[cs:devmark_addr],cx
	mov	es,cx
	; 25/10/2022
	;mov	[es:devmark.id],al
	mov	[es:0],al
	inc	cx
	;mov	[es:devmark.seg],cx
	mov	[es:1],cx

	pop	cx
	pop	es
	inc	word [cs:memhi]
	retn

; ----------------------------------------------------------------------
; SYSCONF.ASM - MSDOS 6.0 - 1991
; ----------------------------------------------------------------------
; 27/03/2019 - Retro DOS v4.0

;MULTI_CONFIG	equ 1

HIGH_FIRST 	equ 080h		; from ARENA.INC - modifier for
                                        ; allocation strategy call

;have_install_cmd equ 00000001b 	; config.sys has install= commands
;has_installed	  equ 00000010b 	; sysinit_base installed.

default_filenum equ 8

;stacksw	equ true		; include switchable hardware stacks

; external variable defined in ibmbio module for multi-track

;multrk_on	equ 10000000b		;user spcified mutitrack=on,or system turns
					; it on after handling config.sys file as a
					; default value,if multrk_flag = multrk_off1.
;multrk_off1	equ 00000000b		;initial value. no "multitrack=" command entered.
;multrk_off2	equ 00000001b		;user specified multitrack=off.

; if stacksw

; internal stack parameters

;entrysize	equ 8

;mincount	equ 8
;defaultcount	equ 9
;maxcount	equ 64

;minsize 	equ 32
;defaultsize	equ 128
;maxsize 	equ 512

DOS_FLAG_OFFSET	equ 86h

;ifdef MULTI_CONFIG
;
;   config_envlen must immediately precede config_wrkseg, because they
;   may be loaded as a dword ptr

; 25/10/2022
;config_envlen:	dw  0  			; when config_wrkseg is being used as
;               			;  a scratch env, this is its length
;config_wrkseg:	dw  0			; config work area (above confbot)
;                   			;  segment of work area
;
;config_cmd:	db  0  			; current config cmd
;                 			;  (with CONFIG_OPTION_QUERY bit intact)
;config_multi:	db  0                   ; non-zero if multi-config config.sys

;endif ; MULTI_CONFIG

multdeviceflag:	db  0

devmark_addr:	dw  0			;segment address for devmark.

setdevmarkflag: db  0			;flag used for devmark

driver_units:	db  0			;total unitcount for driver

ems_stub_installed:
		db  0

badparm_ptr:	; label	dword
badparm_off:	dw  0
badparm_seg:	dw  0

;******************************************************************************
;take care of config.sys file.
;system parser data and code.
;******************************************************************************

;*******************************************************************
; parser options set for msbio sysconf module
;*******************************************************************
;
;**** default assemble swiches definition **************************

;farsw	equ 0		; near call expected
;datesw	equ 0		; check date format
;timesw	equ 0		; check time format
;filesw	equ 1		; check file specification
;capsw	equ 0		; perform caps if specified
;cmpxsw	equ 0		; check complex list
;numsw	equ 1		; check numeric value
;keysw	equ 0		; support keywords
;swsw	equ 1		; support switches
;val1sw	equ 1		; support value definition 1
;val2sw	equ 0		; support value definition 2
;val3sw	equ 1		; support value definition 3
;drvsw	equ 1		; support drive only format
;qussw	equ 0		; support quoted string format

; psdata_seg equ cs

	;.xlist
	;include parse.asm		;together with psdata.inc
	;.list

; PSDATA.INC - MSDOS 6.0 - 1991
; ======================================================================
; 27/03/2019 - Retro DOS v4.0

; 30/03/2019
; VERSION.INC (MSDOS 6.0) 
; Set DBCS Blank constant

; ifndef DBCS
DB_SPACE EQU 2020h
DB_SP_HI EQU 20h
DB_SP_LO EQU 20h
; else

;*******************************************************************
; Parser include file
;*******************************************************************

;**** Equation field
;-------- Character code definition

_$P_DBSP1	   equ	DB_SP_HI	;AN000; 1st byte of DBCS blank
_$P_DBSP2	   equ	DB_SP_LO	;AN000; 2nd byte of DBCS blank
_$P_Period	   equ	"."             ;AN020;
_$P_Slash	   equ	"/"             ;AN020;
_$P_Space	   equ	" "             ;AN000; SBCS blank
_$P_Comma	   equ	","             ;AN000;
_$P_Switch	   equ	"/"             ;AN000;
_$P_Keyword	   equ	"="             ;AN000;
_$P_Colon	   equ	":"             ;AN000;
_$P_Plus 	   equ	"+"             ;AN000;
_$P_Minus	   equ	"-"             ;AN000;
_$P_Rparen	   equ	")"             ;AN000;
_$P_Lparen	   equ	"("             ;AN000;
;_$P_SQuote        equ  "'"			;AN025; deleted
_$P_DQuote	   equ	'"'             ;AN000;
_$P_NULL 	   equ	0		;AN000;
_$P_TAB		   equ	9		;AN000;
_$P_CR		   equ	0Dh		;AN000;
_$P_LF		   equ	0Ah		;AN000;
_$P_ASCII80	   equ	80h		;AN000; ASCII 80h character code

;-------- Masks
_$P_Make_Lower	   equ	20h		;AN000; make lower case character
_$P_Make_Upper	   equ	0FFh-_$P_Make_Lower ;AN000; make upper case character

;-------- DOS function call related equs

_$P_DOS_Get_CDI	   equ	3800h		;AN000; get country dependent information
					; by this call, following information
struc _$P_CDI	
 .DateF: resw 1
 .Money: resb 5
 .1000:	 resb 2
 .Dec:	 resb 2
 .DateS: resb 2
 .TimeS: resb 2
    	 resb 1
	 resb 1
 .TimeF: resb 1	 
	 resw 2
	 resb 2
	 resw 5
 .size:
endstruc

_$P_Date_MDY	   equ	0		;AN000;
_$P_Date_DMY	   equ	1		;AN000;
_$P_Date_YMD	   equ	2		;AN000;
;-------------
_$P_DOS_GetEV	   equ	6300h		;AN000; get DBCS EV call
					;AN000; DS:SI will points to DBCS EV
;-------------
_$P_DOS_Get_TBL	   equ	65h		;AN000; get uppercase table call
					;AN000; following parameters are set
					;AN000; to get casemap table.
_$P_DOSTBL_Def	   equ	-1		;AN000; get default
_$P_DOSTBL_BL	   equ	5		;AN000; buffer length for Tbl pointer
_$P_DOSTBL_File	   equ	4		;AN000; get file uppercase table
_$P_DOSTBL_Char	   equ	2		;AN000; get character uppercase table
					; By this call following information
					; is returned.
struc _$P_DOS_TBL
 .InfoID: resb 1			;AN000; information id for the table
 .Off:	 resw 1				;AN000; offset address of the table
 .Seg:	 resw 1				;AN000; segment address of the table
endstruc

; ----------------------------------------------------------------------------
; PARMS 	LABEL	BYTE
;		DW	PARMSX
;		DB	2		; NUMBER OF STRINGS (0, 1, 2)
;		DB	length		; LENGTH OF THE NEXT LIST, 0 IF NONE
;		DB	" .. "          ; EXTRA DELIMITER LIST,
;					; TYPICAL ARE ";", "="
;					; "," & WHITESPACE ALWAYS
;		DB	length		; LENGTH OF THE NEXT LIST, 0 IF NONE
;		DB	" .. "          ; EXTRA END OF LINE LIST, CR, LF OR 0 ALWAYS
; ----------------------------------------------------------------------------

;-------------------------------- PARMS block structure
struc _$P_PARMS_Blk
 .PARMSX_Address:  resw 1		;AN000; Address of PARMSX
 .Num_Extra:	   resb 1		;AN000; Number of extra stuff
 .Len_Extra_Delim: resb 1		;AN000; Length of extra delimiter
endstruc

_$P_Len_PARMS	   equ	4		;AN000;
_$P_I_Use_Default  equ	0		;AN000; no extra stuff specified
_$P_I_Have_Delim   equ	1		;AN000; extra delimiter specified
_$P_I_Have_EOL	   equ	2		;AN000; extra EOL specified

; ----------------------------------------------------------------------------
; PARMSX	LABEL	BYTE
;		DB	minp,maxp	; MIN, MAX POSITIONAL OPERANDS ALLOWED
;		DW	CONTROL 	; DESCRIPTION OF POSITIONAL 1
;		:			; REPEATS maxp-1 TIMES
;		DB	maxs		; # OF SWITCHES
;		DW	CONTROL 	; DESCRIPTION OF SWITCH 1
;		:			; REPEATS maxs-1 TIMES
;		DB	maxk		; # OF KEYWORD
;		DW	CONTROL 	; DESCRIPTION OF KEYWORD 1
;		:			; REPEATS maxk-1 TIMES
; ----------------------------------------------------------------------------

;-------------------------------- PARMSX block structure
struc _$P_PARMSX_Blk		;AN000;
 .MinP: resb 1			;AN000; Minimum positional number
 .MaxP:	resb 1			;AN000; Maximum positional number
 .1st_Control: resw 1		;AN000; Address of the 1st CONTROL block
endstruc

; ----------------------------------------------------------------------------
; << Control field definition  >>
;
;
;CONTROL   LABEL   BYTE
;	   DW	   MATCH_FLAGS	   ; CONTROLS TYPE MATCHED
;				   ; 8000H=NUMERIC VALUE, (VALUE LIST WILL BE CHECKED)
;				   ; 4000H=SIGNED NUMERIC VALUE (VALUE LIST WILL BE CHECKED)
;				   ; 2000H=SIMPLE STRING(VALUE LIST WILL BE CHECKED)
;				   ; 1000H=DATE STRING (VALUE LIST WON'T BE CHECKED)
;				   ; 0800H=TIME STRING (VALUE LIST WON'T BE CHECKED)
;				   ; 0400H=COMPLEX LIST (VALUE LIST WON'T BE CHECKED)
;				   ; 0200H=FILE SPEC (VALUE LIST WON'T BE CHECKED)
;				   ; 0100H=DRIVE ONLY (VALUE LIST WON'T BE CHECKED)
;				   ; 0080H=QUOTED STRING (VALUE LIST WON'T BE CHECKED)
;				   ; 0010H=IGNORE ":" AT END IN MATCH
;				   ; 0002H=REPEATS ALLOWED
;				   ; 0001H=OPTIONAL
;	   DW	   FUNCTION_FLAGS
;				   ; 0001H=CAP RESULT BY FILE TABLE
;				   ; 0002H=CAP RESULT BY CHAR TABLE
;				   ; 0010H=REMOVE ":" AT END
; (tm10)			   ; 0020H=colon is not necessary for switch
;	   DW	   RESULT	   ; RESULT BUFFER
;	   DW	   VALUES	   ; VALUE LISTS
;	   DB	   nid		   ; NUMBER OF KEYWORD/SWITCH SYNONYMS IN FOLLOWING LIST
;	   DB	   "...",0         ; IF n >0, KEYWORD 1
;	   :
;
;Note:
;    - The MATCH_FLAG is bit significant. You can set, for example, TIME bit and
;      DATE bit simalteniously.
;
;      The parser examins each bit along with the following priority.
;
;      COMPLEX -> DATE -> TIME -> NUMERIC VAL -> SIGNED NUMERIC VAL -> DRIVE ->
;      FILE SPEC -> SIMPLE STRING.
;
;    - When the FUNCTION_FLAG is 0001 or 0002, the STRING pointed to by a pointer
;      in the result buffer is capitalized.
;
;    - Match_Flags 0001H and 0002H have meaning only for the positional.
;
;    - The "...",0 (bottom most line) does require '=' or '/'. When you need a
;      switch, for example, '/A', then STRING points to;
;
;			DB    1 	; number of following synonyms
;			DB   '/A',0
;
;      When you need a keyword, for example, 'CODEPAGE=', then "...",0 will be;
;
;			DB    1 	; number of following synonyms
;			DB   'CODEPAGE=',0
;
;    - "..." must consist of upper case characters only because the parser
;      performs pattern matching after converting input to upper case (by
;      using the current country upper case table)
;
;    - One "..." can contain only one switch or keyword. If you need, for
;      example /A and /B, the format will be;
;
;			DB    2 	; number of following synonyms
;			DB    '/A',0
;			DB    '/B',0
; ----------------------------------------------------------------------------

;**** Match_Flags

_$P_Num_Val	   equ	8000h		;AN000; Numeric Value
_$P_SNum_Val	   equ	4000h		;AN000; Signed numeric value
_$P_Simple_S	   equ	2000h		;AN000; Simple string
_$P_Date_S	   equ	1000h		;AN000; Date string
_$P_Time_S	   equ	0800h		;AN000; Time string
_$P_Cmpx_S	   equ	0400h		;AN000; Complex string
_$P_File_Spc	   equ	0200h		;AN000; File Spec
_$P_Drv_Only	   equ	0100h		;AN000; Drive Only
_$P_Qu_String	   equ	0080h		;AN000; Quoted string
_$P_Ig_Colon	   equ	0010h		;AN000; Ignore colon at end in match
_$P_Repeat	   equ	0002h		;AN000; Repeat allowed
_$P_Optional	   equ	0001h		;AN000; Optional

;**** Function flags

_$P_CAP_File	   equ	0001h		;AN000; CAP result by file table
_$P_CAP_Char	   equ	0002h		;AN000; CAP result by character table
_$P_Rm_Colon	   equ	0010h		;AN000; Remove ":" at the end
_$P_colon_is_not_necessary equ 0020h	;AN000;(tm10) /+10 and /+:10

;-------------------------------- Control block structure
struc _$P_Control_Blk
 .Match_Flag:	 resw 1		;AN000; Controls type matched
 .Function_Flag: resw 1		;AN000; Function should be taken
 .Result_Buf:	 resw 1		; Result buffer address
 .Value_List:	 resw 1		;AN000; Value list address
 .nid:		 resb 1		;AN000; # of keyword/SW synonyms
 .KEYorSW:	 resb 1		;AN000; keyword or sw
endstruc

; ----------------------------------------------------------------------------
; << Value List Definition >>
;
;VALUES 	LABEL	BYTE
;		DB	nval			; NUMBER OF VALUE DEFINITIONS (0 - 3)
;	     +-
;	     |	DB	nrng			; NUMBER OF RANGES
;	     | +DB	ITEM_TAG		; RETURN VALUE IF RANGE MATCHED
;	     | +DD	X,Y			; RANGE OF VALUES
;	     |	:
;	     |	DB	nnval			; NUMBER OF CHOICES
;	     | +DB	ITEM_TAG		; RETURN VALUE IF NUMBER CHOICE MATCHED
;	     | +DD	VALUE			; SPECIFIC CHOICE IF NUMBER
;	     |	:
;	     |	DB	nstrval 		; NUMBER OF CHOICES
;	     | +DB	ITEM_TAG		; RETURN VALUE IF STRING CHOICE MATCHED
;	     | +DW	STRING			; SPECIFIC CHOICE IF STING
;	     +-	:
;
;STRING 	DB	"...",0                 ; ASCIIZ STRING IMAGE
;
;Note:
;    - ITEM_TAG must not be 0FFH, which will be used in the result buffer
;      when no choice lists are provided.
;
;    - STRING must consist of upper case characters only because the parser
;      performs pattern matching after converting input to upper case (by
;      using the current country upper case table)
; ----------------------------------------------------------------------------

_$P_nval_None	   equ	0		;AN000; no value list ID
_$P_nval_Range	   equ	1		;AN000; range list ID
_$P_nval_Value	   equ	2		;AN000; value list ID
_$P_nval_String	   equ	3		;AN000; string list ID
_$P_Len_Range	   equ	9		;AN000; Length of a range choice(two DD plus one DB)
_$P_Len_Value	   equ	5		;AN000; Length of a value choice(one DD plus one DB)
_$P_Len_String	   equ	3		;AN000; Length of a string choice(one DW plus one DB)
_$P_No_nrng	   equ	0		;AN000; (tm07) no nrng. nnval must not be 0.

struc _$P_Val_List
 .NumofList: resb 1			;AN000; number of following choice
 .Val_XL:    resw 1			;AN000; lower word of value
 .Val_XH:    resw 1			;AN000; higher word of value
 .Val_YL:    resw 1			;AN000; lower word of another value
 .Val_YH:    resw 1			;AN000; higher word of another value
endstruc

; ----------------------------------------------------------------------------
; << Result Buffer Definition  >>
;
;RESULT 	LABEL	BYTE			; BELOW FILLED IN FOR DEFAULTS
;		DB	type			; TYPE RETURNED: 0=RESERVED,
;						;	1=NUMBER, 2=LIST INDEX,
;						;	3=STRING, 4=COMPLEX,
;						;	5=FILESPEC, 6=DRIVE
;						;	7=DATE, 8=TIME
;						;	9=QUOTED STRING
;		DB	ITEM_TAG		; MATCHED ITEM TAG
;
;		dw	synonym@		; es:@ points to found SYNONYM if provided.
;
;            +-
;	    | DD	n			; VALUE IF NUMBER
;	    | or
;	    |	DW	i			; INDEX (OFFSET) INTO VALUE LIST
;	    |					; (ES presents Segment address)
;	    | or
;	    |	DD	STRING			; OFFSET OF STRING VALUE
;	    | or
;	    |	DB	drv			; DRIVE NUMBER (1-A, 2-B,..., 26-Z)
;	    | or
;	    |	DW	YEAR	   ;(1980-2099)  IN CASE OF DATE
;	    |	DB	MONTH	   ;(1-12)	 Note: Range check is not performed.
;	    |	DB	DATE	   ;(1-31)	       0 is filled when the corresponding field was not specified.
;	    | or
;	    |	DB	HOUR	   ;(0-23)	  IN CASE OF TIME
;	    |	DB	MINUTES    ;(0-59)	  Note: Range check is not performed .
;	    |	DB	SECONDS    ;(0-59)		0 is filled when the corresponding field was not specified .
;	    |	DB	HUNDREDTHS ;(0-99)
;	    +-
;
;
;Note: ITEM_TAG is 0FFH when the caller does not specify the choice
;      list.
;
;      YEAR: If the input value for the year is less than 100, parser
;	     adds 1900 to it.  For example, when 87 is input to parser for
;	     the year value, he returns 1987.
; ----------------------------------------------------------------------------

;-------------------------------- Result block structure
struc _$P_Result_Blk
 .Type:        resb 1		;AN000; Type returned
 .Item_Tag:    resb 1		;AN000; Matched item tag
 .SYNONYM_Ptr: resw 1		;AN000; pointer to Synonym list returned
 .Picked_Val:  resb 4		;AN000; value
endstruc

;--------------------------------
;**** values for the type field in the result block

_$P_EOL		   equ	0		;AN000; End of line
_$P_Number	   equ	1		;AN000; Number
_$P_List_Idx	   equ	2		;AN000; List Index
_$P_String	   equ	3		;AN000; String
_$P_Complex	   equ	4		;AN000; Complex
_$P_File_Spec	   equ	5		;AN000; File Spec
_$P_Drive	   equ	6		;AN000; Drive
_$P_Date_F	   equ	7		;AN000; Date
_$P_Time_F	   equ	8		;AN000; Time
_$P_Quoted_String  equ	9		;AN000; Quoted String

_$P_No_Tag	   equ	0FFh		;AN000; No ITEM_TAG found

;**** Return code
;
; following return code will be returned in the AX register.

_$P_No_Error	   equ	0		;AN000; No error
_$P_Too_Many	   equ	1		;AN000; Too many operands
_$P_Op_Missing	   equ	2		;AN000; Required operand missing
_$P_Not_In_SW	   equ	3		;AN000; Not in switch list provided
_$P_Not_In_Key	   equ	4		;AN000; Not in keyword list provided
_$P_Out_Of_Range   equ	6		;AN000; Out of range specified
_$P_Not_In_Val	   equ	7		;AN000; Not in value list provided
_$P_Not_In_Str	   equ	8		;AN000; Not in string list provided
_$P_Syntax	   equ	9		;AN000; Syntax error
_$P_RC_EOL	   equ	-1		;AN000; End of command line

; DATA - Retro DOS v4.0 - 27/03/2019

; MSDOS 6.2 IO.SYS SYSINIT:179Ch

;********************** Local Data *************************************
_$P_ORDINAL:	   dw	0		;AN000; Operand ordinal save area
_$P_RC:		   dw	0		;AN000; Return code from parser
_$P_SI_Save:	   dw	0		;AN000; Pointer of command buffer
_$P_DX:		   dw	0		;AN000; Return result buffer address
_$P_Terminator:	   db	0		;AN000; Terminator code (ASCII)
_$P_DBCSEV_OFF:	   dw	0		;AN000; Offset of DBCS EV
_$P_DBCSEV_SEG:	   dw	0		;AN000; Segment of DBCS EV
_$P_Flags:	   dw	0		;AN000; Parser internal flags
%define _$P_Flags1 _$P_Flags		;AN038; to reference first byte flags
%define _$P_Flags2 _$P_Flags+1		;AN038; to reference second byte flags only

;in second byte of _$P_Flags, referenced as _$P_Flags2:
_$P_equ		   equ	01h	      ;AN000; "=" packed in string buffet
_$P_Neg		   equ	02h	      ;AN000; Negative value
_$P_Time12	   equ	04h	      ;AN000; set when PM is specified
_$P_Key_Cmp	   equ	08h	      ;AN000; set when keyword compare
_$P_SW_Cmp	   equ	10h	      ;AN000; set when switch compare
_$P_Extra	   equ	20h	      ;AN000; set when extra delimiter found
_$P_SW		   equ	40h	      ;AN000; set when switch found (tm08)
_$P_Signed	   equ	80h	      ;AN000; signed numeric specified

;in first byte of _$P_Flags, referenced as _$P_Flags1:
_$P_time12am	   equ	01h	      ;AN038; set when AM is specified on time
_$P_TIME_AGAIN	   equ	02h	      ;AN039; SET WHEN READY TO RE-PARSE TIME

_$P_SaveSI_Cmpx:   dw	0		;AN000; save si for later use by complex
_$P_KEYorSW_Ptr:   dw	0		;AN000; points next to "=" or ":" code
_$P_Save_EOB:	   dw	0		;AN000; save pointer to EOB
_$P_Found_SYNONYM: dw	0		;AN000; es:@ points to found synonym

_$P_STRING_BUF:	   times 128 db 0	;AN000; Pick a operand from command line
_$P_STRING_BUF_END equ	$		;AN000;

; 25/10/2022
; (MSDOS 5.0 IO.SYS, SYSINIT:16F8h)

_$P_Char_CAP_Ptr:  db	0FFh		;AN000; info id
		   dw	0		;AN000; offset	of char case map table
		   dw	0		;AN000; segment of char case map table
; 25/10/2022
;_$P_File_CAP_Ptr: db	0FFh		;AN000; info id
;		   dw	0		;AN000; offset	of file case map table
;		   dw	0		;AN000; segment of file case map table

; (tm06) IF FileSW			;AN000;(Check if file spec is supported)
;

;M029
;!!!WARNING!!!
; In routine SYSPARSE (parse.asm), _$P_FileSp_Char is reinitialized using 
;hardcoded strings. If the chars in the string are changed here, corresponding
;changes need to be made in SYSPARSE

;IF FileSW+DrvSW 			;AN000;(Check if file spec is supported)

; 25/10/2022
; (MSDOS 5.0 IO.SYS, SYSINIT:16FDh)

_$P_FileSp_Char	   db	'[]|<>+=;"'     ;AN000; delimitter of file spec
_$P_FileSp_Len	   equ	$-_$P_FileSp_Char ;AN000;

;ENDIF					;AN000;(of FileSW)

; delimiter parsing
_$P_colon_period   equ	01h		;AN032; check for colon & period
_$P_period_only	   equ	02h		;AN032; check only for period

;filespec error flag
_$P_err_flag:	   db	0		;AN033; flag set if filespec parsing error
					;AN033;  was detected.
_$P_error_filespec equ	01h		;AN033; mask to set flag


; PARSE.ASM - MSDOS 6.0 - 1991
; ======================================================================
; 27/03/2019 - Retro DOS v4.0

;***********************************************************************
; SysParse;
;
;  Function : Parser Entry
;
;  Input: DS:SI -> command line
;	  ES:DI -> parameter block
;	  cs -> psdata.inc
;	  CX = operand ordinal
;
;	  Note:  ES is the segment containing all the control blocks defined
;		 by the caller, except for the DOS COMMAND line parms, which
;		 is in DS.
;
;  Output: CY = 1   error of caller, means invalid parameter block or
;		    invalid value list. But this parser does NOT implement
;		    this feature. Therefore CY always zero.
;
;	   CY = 0   AX = return code
;		    BL = terminated delimiter code
;		    CX = new operand ordinal
;		    SI = set past scaned operand
;		    DX = selected result buffer
;
; Use:	_$P_Skip_Delim, _$P_Chk_EOL, _$P_Chk_Delim, _$P_Chk_DBCS
;	_$P_Chk_Swtch, _$P_Chk_Pos_Control, _$P_Chk_Key_Control
;	_$P_Chk_Sw_Control, _$P_Fill_Result
;
; Vars: _$P_Ordinal(RW), _$P_RC(RW), _$P_SI_Save(RW), _$P_DX(R), _$P_Terminator(R)
;	_$P_SaveSI_Cmpx(W), _$P_Flags(RW), _$P_Found_SYNONYM(R), _$P_Save_EOB(W)
;
;-------- Modification History -----------------------------------------
;
;  4/04/87 : Created by K. K,
;  4/28/87 : _$P_Val_YH assemble error (tm01)
;	   : JMP SHORT assemble error (tm02)
;  5/14/87 : Someone doesn't want to include psdata (tm03)
;  6/12/87 : _$P_Bridge is missing when TimeSw equ 0 and (CmpxSw equ 1 or
;	     DateSW equ 1)	      (tm04)
;  6/12/87 : _$P_SorD_Quote is missing when QusSw equ 0 and CmpxSW equ 1
;				      (tm05) in PSDATA.INC
;  6/12/87 : _$P_FileSp_Char and _$P_FileSP_Len are missing
;	     when FileSW equ 0 and DrvSW equ 1 (tm06) in PSDATA.INC
;  6/18/87 : $VAL1 and $VAL3, $VAL2 and $VAL3 can be used in the same
;	     value-list block	      (tm07)
;  6/20/87 : Add _$P_SW to check if there's an omiting parameter after
;	     switch (keyword) or not. If there is, backup si for next call
;	     (tm08)
;  6/24/87 : Complex Item checking does not work correctly when CmpSW equ 1
;	     and DateSW equ 0 and TimeSW equ 0 (tm09)
;  6/24/87 : New function flag _$P_colon_is_not_necessary for switch
;	     /+15 and /+:15 are allowed for user (tm10)
;  6/29/87 : ECS call changes DS register but it causes the address problem
;	     in user's routines. _$P_Chk_DBCS (tm11)
;  7/10/87 : Switch with no_match flag (0x0000H) does not work correctly
;					  (tm12)
;  7/10/87 : Invalid switch/keyword does not work correctly
;					  (tm13)
;  7/10/87 : Drive_only breaks 3 bytes after the result buffer
;					  (tm14)
;  7/12/87 : Too_Many_Operands sets DX=0 as the PARSE result
;					  (tm15)
;  7/24/87 : Negative lower bound on numeric ranges cause trouble

;  7/24/87 : Quoted strings being returned with quotes.

;  7/28/87 : Kerry S (;AN018;)
;	     Non optional value on switch (match flags<>0 and <>1) not flagged
;	     as an error when missing.	Solution: return error 2.  Modules
;	     affected: _$P_Chk_SW_Control.

;  7/29/87 : Kerry S (;AN019;)
;	     Now allow the optional bit in match flags for switches.  This
;	     allows the switch to be encountered with a value or without a
;	     value and no error is returned.
;

;  8/28/87 : Ed K, Kerry S (;AN020;)
;  9/14/87   In PROC _$P_Get_DecNum, when checking for field separators
;	     within a date response, instead of checking just for the one
;	     character defined by the COUNTRY DEPENDENT INFO, check for
;	     all three chars, "-", "/", and ".". Change _$P_Chk_Switch to allow
;	     slashes in date strings when DateSw (assembler switch) is set.

;  9/1/87  : Kerry S (;AN021)
;	     In PROC _$P_String_Comp, when comparing the switch or keyword on
;	     the command line with the string in the control block the
;	     comparing was stopping at a colon (switch) or equal (keyword)
;	     on the command line and assuming a match.	This allowed a shorter
;	     string on the command line than in the synonym list in the control
;	     block.  I put in a test for a null in the control block so the
;	     string in the control block must be the same length as the string
;	     preceeding the colon or equal on the command line.

;  8/28/87 : Kerry S (;AN022;)
;	     All references to data in PSDATA.INC had CS overrides.  This caused
;	     problems for people who included it themselves in a segment other
;	     than CS.  Added switch to allow including PSDATA.INC in any
;	     segment.

;  9/16/87 : Ed K (;AN023;) PTM1040
;	     in _$P_set_cdi PROC, it assumes CS points to psdata. Change Push CS
;	     into PUSH cs.  In _$P_Get_DecNum PROC, fix AN020
;	     forced both TIME and DATE to use the delims, "-","/",".".
;	     Created FLag, in _$P_time_Format PROC, to request the delim in
;	     BL be used if TIME is being parsed.

;  9/24/87 : Ed K
;	     Removed the include to STRUC.INC.	Replaced the STRUC macro
;	     invocations with their normally expanded code; made comments
;	     out of the STRUC macro invocation statements to maintain readability.

;  9/24/87 : Ed K (;AN024;) PTM1222
;	     When no CONTROL for a keyword found, tried to fill in RESULT
;	     pointed to by non-existant CONTROL.

; 10/15/87 : Ed K (;AN025;) PTM1672
;	     A quoted text string can be framed only by double quote.  Remove
;	     support to frame quoted text string with single quote.
;	     (apostrophe) _$P_SorD_Quote is removed from PSDATA.INC.
;	     _$P_SQuote EQU also removed from PSDATA.INC.  Any references to
;	     single quote in PROC prologues are left as is for history reasons.

;	     This fixes another bug, not mentioned in p1672, in that two
;	     quote chars within a quoted string is supposed to be reported as
;	     one quote character, but is reported as two quotes.  This changed
;	     two instructions in PROC _$P_Quoted_Str.

;	     Also fixed are several JMP that caused a NOP, these changed to
;	     have the SHORT operator to avoid the unneeded NOP.

;	     The code and PSDATA.INC have been aligned for ease of reading.

; 10/26/87 : Ed K (;AN026;) PTM2041, DATE within SWITCH, BX reference to
;	     psdata buffer should have cs.

; 10/27/87 : Ed K (;AN027;) PTM2042 comma between keywords implies
;	     positional missing.

; 11/06/87 : Ed K (;AN028;) PTM 2315 Parser should not use line feed
;	     as a line delimiter, should use carriage return.
;	     Define switch: LFEOLSW, if on, accept LF as end of line char.

; 11/11/87 : Ed K (;AN029;) PTM 1651 GET RID OF WHITESPACE AROUND "=".

; 11/18/87 : Ed K (;AN030;) PTM 2551 If filename is just "", then
;	     endless loop since SI is returned still pointing to start
;	     of that parm.

; 11/19/87 : Ed K (;AN031;) PTM 2585 date & time getting bad values.
;	     Vector to returned string has CS instead of cs, but
;	     when tried to fix it on previous version, changed similar
;	     but wrong place.

; 12/09/87 : Bill L (;AN032;) PTM 2772 colon and period are now valid
;	     delimiters between hours, minutes, seconds for time. And period
;	     and comma are valid delimiters between seconds and 100th second.

; 12/14/87 : Bill L (;AN033;) PTM 2722 if illegal delimiter characters
;	     in a filespec, then flag an error.

; 12/22/87 : Bill L (;AN034;)	    All local data to parser is now
;	     indexed off of the cs equate instead of the DS register.
;	     Using this method, DS can point to the segment of PSP or to psdata
;  -->	     local parser data. Why were some references to local data changed
;	     to do this before, but not all ?????

; 02/02/88 : Ed K (;AC035;) INSPECT utility, suggests optimizations.

; 02/05/88 : Ed K (;AN036;) P3372-UPPERCASE TRANSLATION, cs HOSED.
;
; 02/08/88 : Ed K (;AN037;) P3410-AVOID POP OF CS, CHECK BASESW FIRST.

; 02/19/88 : Ed K (;AN038;) p3524 above noon and "am" should be error

; 02/23/88 : Ed K (;AN039;) p3518 accept "comma" and "period" as decimal
;	     separator in TIME before hundredths field.
;
; 08/09/90 : SA	M005	Prevented parser from recognizing '=' signs within
;			strings as keywords.
;
;***********************************************************************

;IF FarSW				;AN000;(Check if need far return)
;SysParse proc far			;AN000;
;ELSE					;AN000;
;SysParse proc near			;AN000;
;ENDIF					;AN000;(of FarSW)

; 27/03/2019 - Retro DOS v4.0
; (MSDOS 6.21 IO.SYS - SYSINIT:1842h)

; 25/10/2022 - Retro DOS v4.0
; (MSDOS 5.0 IO.SYS - SYSINIT:1707h)

SysParse:
	mov	word [cs:_$P_Flags],0	;AC034; Clear all internal flags
	cld				;AN000; confirm forward direction
	mov	word [cs:_$P_ORDINAL],cx ;AC034; save operand ordinal
	mov	word [cs:_$P_RC],_$P_No_Error ;AC034; Assume no error
	mov	word [cs:_$P_Found_SYNONYM],0 ;AC034; initalize synonym pointer

	mov	word [cs:_$P_DX],0	;AC034; (tm15)

;M029 -- Begin changes
; The table of special chars _$P_FileSp_Char should be initialized on every
;entry to SysParse. This is in the non-checksum region and any program that
;corrupts this table but does not corrupt the checksum region will leave
;command.com parsing in an inconsistent state.
; NB: The special characters string has been hardcoded here. If any change
;is made to it in psdata.inc, a corresponding change needs to be made here.

;IF FileSW + DrvSW
	mov	word [cs:_$P_FileSp_Char], ']['
	mov	word [cs:_$P_FileSp_Char+2], '<|'
	mov	word [cs:_$P_FileSp_Char+4], '+>'
	mov 	word [cs:_$P_FileSp_Char+6], ';='
;ENDIFtHHH

;M029 -- End of changes

	call	_$P_Skip_Delim		;AN000; Move si to 1st non white space
	jnc	short _$P_Start		;AN000; If EOL is not encountered, do parse
;--------------------------- End of Line
	mov	ax,_$P_RC_EOL		;AN000; set exit code to -1
	push	bx			;AN000;
	;mov	bx,[es:di+_$P_PARMS_Blk.PARMSX_Address]
					;AN000; Get the PARMSX address to
	mov	bx,[es:di]
	;cmp	cl,[es:bx+_$P_PARMSX_Blk.MinP]
					;AN000; check ORDINAL to see if the minimum
	cmp	cl,[es:bx]	
	jae	short _$P_Fin		;AN000; positional found.

	mov	ax,_$P_Op_Missing	;AN000; If no, set exit code to missing operand
_$P_Fin: 				;AN000;
	pop	bx			;AN000;
	jmp	_$P_Single_Exit		;AN000; return to the caller
;---------------------------
_$P_Start:				;AN000;
	mov	[cs:_$P_SaveSI_Cmpx],si ;AN000;AC034; save ptr to command line for later use by complex,
	push	bx			;AN000; quoted string or file spec.
	push	di			;AN000;
	push	bp			;AN000;
	;lea	bx,[cs:_$P_STRING_BUF] ;AC034; set buffer to copy from command string
	; 02/11/2022
	lea	bx,[_$P_STRING_BUF]
	test	byte [cs:_$P_Flags2],_$P_Extra ;AC034; 3/9 extra delimiter encountered ?
	jnz	short _$P_Pack_End	;AN000; 3/9 if yes, no need to copy

_$P_Pack_Loop:				;AN000;
	lodsb				;AN000; Pick a operand from buffer
	call	_$P_Chk_Switch		;AN000; Check switch character
	jc	short _$P_Pack_End_BY_EOL ;AN020; if carry set found delimiter type slash, need backup si, else continue

	call	_$P_Chk_EOL		;AN000; Check EOL character
	je	short _$P_Pack_End_BY_EOL ;AN000; need backup si

	call	_$P_Chk_Delim		;AN000; Check delimiter
	jne	short _$P_PL01 		;AN000; If no, process next byte

	test	byte [cs:_$P_Flags2],_$P_Extra ;AC034; 3/9 If yes and white spec,
; (tm08)jne	short _$P_Pack_End	;AN000; 3/9 then
	jnz	short _$P_Pack_End_backup_si ;AN000; (tm08)

	call	_$P_Skip_Delim		;AN000; skip subsequent white space,too
	jmp	short _$P_Pack_End	;AN000; finish copy by placing NUL at end

_$P_Pack_End_backup_si:			;AN000; (tm08)
	test	byte [cs:_$P_Flags2],_$P_SW+_$P_equ ;AN000;AC034;  (tm08)
	jz	short _$P_Pack_End	;AN000; (tm08)

	dec	si			;AN000; (tm08)
	jmp	short _$P_Pack_End	;AN025; (tm08)

_$P_PL01:				;AN000;
	mov	[cs:bx],al		;AN000; move byte to STRING_BUF
	cmp	al,_$P_Keyword  ;'='	;AN000; if it is equal character,
	jne	short _$P_PL00 		;AN000; then

	or	byte [cs:_$P_Flags2],_$P_equ ;AC034; remember it in flag
_$P_PL00:				;AN000;
	inc	bx			;AN000; ready to see next byte
	call	_$P_Chk_DBCS		;AN000; was it 1st byte of DBCS ?
	jnc	_$P_Pack_Loop		;AN000; if no, process to next byte

	lodsb				;AN000; if yes, store
	mov	[cs:bx],al		;AN000;    2nd byte of DBCS
	inc	bx			;AN000; update pointer
	jmp	short _$P_Pack_Loop	;AN000; process to next byte

_$P_Pack_End_BY_EOL:			;AN000;
	dec	si			;AN000; backup si pointer
_$P_Pack_End:				;AN000;
	mov	[cs:_$P_SI_Save],si     ;AC034; save next pointer, SI
	mov	byte [cs:bx],_$P_NULL	;AN000; put NULL at the end
	mov	[cs:_$P_Save_EOB],bx    ;AC034; 3/17/87 keep the address for later use of complex
	;mov	bx,[es:di+_$P_PARMS_Blk.PARMSX_Address] ;AN000; get PARMSX address
	mov	bx,[es:di]
	;lea	si,[cs:_$P_STRING_BUF]	;AC034;
	; 02/11/2022
	lea	si,[_$P_STRING_BUF]
	cmp	byte [cs:si],_$P_Switch ;AN000; the operand begins w/ switch char ?
	je	short _$P_SW_Manager	;AN000; if yes, process as switch

	cmp	byte [cs:si],_$P_DQuote	;M005;is it a string?
	je	short _$P_Positional_Manager ;M005;if so, process as one!

	test	byte [cs:_$P_Flags2],_$P_equ ;AC034; the operand includes equal char ?
	jnz	short _$P_Key_Manager	;AN000; if yes, process as keyword

_$P_Positional_Manager:			;AN000; else process as positional
	mov	al,[es:bx+_$P_PARMSX_Blk.MaxP] ;AN000; get maxp
	xor	ah,ah			;AN000; ax = maxp
	cmp	[cs:_$P_ORDINAL],ax	;AC034; too many positional ?
	jae	short _$P_Too_Many_Error ;AN000; if yes, set exit code to too many

	mov	ax,[cs:_$P_ORDINAL]	;AC034; see what the current ordinal
	shl	ax,1			;AN000; ax = ax*2
	inc	bx			;AC035; add '2' to
	inc	bx			;AC035;  BX reg
					;AN000; now bx points to 1st CONTROL
	add	bx,ax			;AN000; now bx points to specified CONTROL address
	mov	bx,[es:bx]		;AN000; now bx points to specified CONTROL itself
	call	_$P_Chk_Pos_Control	;AN000; Do process for positional
	jmp	short _$P_Return_to_Caller ;AN000; and return to the caller

_$P_Too_Many_Error:			;AN000;
	mov	word [cs:_$P_RC],_$P_Too_Many ;AC034; set exit code
	jmp	short _$P_Return_to_Caller ;AN000; and return to the caller

_$P_SW_Manager:				;AN000;
	mov	al,[es:bx+_$P_PARMSX_Blk.MaxP] ;AN000; get maxp
	xor	ah,ah			;AN000; ax = maxp
	inc	ax			;AN000;
	shl	ax,1			;AN000; ax = (ax+1)*2
	add	bx,ax			;AN000; now bx points to maxs
	mov	cl,[es:bx]		;AN000;
	xor	ch,ch			;AN000; cx = maxs
	or	cx,cx			;AN000; at least one switch ?
	jz	short _$P_SW_Not_Found 	;AN000;

	inc	bx			;AN000; now bx points to 1st CONTROL address

_$P_SW_Mgr_Loop: 			;AN000;
	push	bx			;AN000;
	mov	bx,[es:bx]		;AN000; bx points to Switch CONTROL itself
	call	_$P_Chk_SW_Control	;AN000; do process for switch
	pop	bx			;AN000;
	jnc	short _$P_Return_to_Caller ;AN000; if the CONTROL is for the switch, exit

	inc	bx			;AC035; add '2' to
	inc	bx			;AC035;  BX reg
					;AN000; else bx points to the next CONTROL
	loop	_$P_SW_Mgr_Loop		;AN000; and loop

_$P_SW_Not_Found:			;AN000;
	mov	word [cs:_$P_RC],_$P_Not_In_SW ;AC034; here no CONTROL for the switch has
	jmp	short _$P_Return_to_Caller ;AN000; not been found, means error.

_$P_Key_Manager: 			;AN000;
	mov	al,[es:bx+_$P_PARMSX_Blk.MaxP] ;AN000; get maxp
	xor	ah,ah			;AN000; ax = maxp
	inc	ax			;AN000;
	shl	ax,1			;AN000; ax = (ax+1)*2
	add	bx,ax			;AN000; now bx points to maxs
	mov	al,[es:bx]		;AN000;
	xor	ah,ah			;AN000; ax = maxs
	shl	ax,1			;AN000;
	inc	ax			;AN000; ax = ax*2+1
	add	bx,ax			;AN000; now bx points to maxk
	mov	cl,[es:bx]		;AN000;
	xor	ch,ch			;AN000; cx = maxk
	or	cx,cx			;AN000; at least one keyword ?
	je	short _$P_Key_Not_Found	;AN000;

	inc	bx			;AN000; now bx points to 1st CONTROL

_$P_Key_Mgr_Loop:			;AN000;
	push	bx			;AN000;
	mov	bx,[es:bx]		;AN000; bx points to keyword CONTROL itself
	call	_$P_Chk_Key_Control	;AN000; do process for keyword
	pop	bx			;AN000;
	jnc	short _$P_Return_to_Caller ;AN000; if the CONTROL is for the keyword, exit

	inc	bx			;AC035; add '2' to
	inc	bx			;AC035;  BX reg
					;AN000; else bx points to the next CONTROL
	loop	_$P_Key_Mgr_Loop 	;AN000; and loop

_$P_Key_Not_Found:			;AN000;
	mov	word [cs:_$P_RC],_$P_Not_In_Key ;AC034; here no CONTROL for the keyword has
_$P_Return_to_Caller:			;AN000;
	pop	bp			;AN000;
	pop	di			;AN000;
	pop	bx			;AN000;
	mov	cx,[cs:_$P_ORDINAL]	;AC034; return next ordinal
	mov	ax,[cs:_$P_RC]		;AC034; return exit code
	mov	si,[cs:_$P_SI_Save]	;AC034; return next operand pointer
	mov	dx,[cs:_$P_DX]		;AC034; return result buffer address
	mov	bl,[cs:_$P_Terminator]	;AC034; return delimiter code found
_$P_Single_Exit: 			;AN000;
	clc				;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_Chk_Pos_Control
;
; Function: Parse CONTROL block for a positional
;
; Input:     ES:BX -> CONTROL block
;	     cs:SI -> _$P_STRING_BUF
;
; Output:    None
;
; Use:	 _$P_Fill_Result, _$P_Check_Match_Flags
;
; Vars: _$P_Ordinal(W), _$P_RC(W)
;***********************************************************************

_$P_Chk_Pos_Control:
	push	ax			;AN000;
	;mov	ax,[es:bx+_$P_Control_Blk.Match_Flag] ;AN000;
	mov	ax,[es:bx]
	test	ax,_$P_Repeat		;AN000; repeat allowed ?
	jnz	short _$P_CPC00		;AN000; then do not increment ORDINAL

	inc	word [cs:_$P_ORDINAL]	;AC034; update the ordinal
_$P_CPC00:				;AN000;
	cmp	byte [cs:si],_$P_NULL	;AN000; no data ?
	jne	short _$P_CPC01		;AN000;

	test	ax,_$P_Optional		;AN000; yes, then is it optional ?
	jnz	short _$P_CPC02		;AN000;

	mov	word [cs:_$P_RC],_$P_Op_Missing ;AC034; no, then error 3/17/87
	jmp	short _$P_CPC_Exit	;AN000;

_$P_CPC02:				;AN000;
	push	ax			;AN000;
	mov	al,_$P_String		;AN000; if it is optional return NULL
	mov	ah,_$P_No_Tag		;AN000; no item tag indication
	call	_$P_Fill_Result		;AN000;
	pop	ax			;AN000;
	jmp	short _$P_CPC_Exit	;AN000;

_$P_CPC01:				;AN000;
	call	_$P_Check_Match_Flags	;AN000;
_$P_CPC_Exit:				;AN000;
	pop	ax			;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_Chk_Key_Control
;
; Function: Parse CONTROL block for a keyword
;
; Input:     ES:BX -> CONTROL block
;	     cs:SI -> _$P_STRING_BUF
;
; Output:    CY = 1 : not match
;
; Use:	 _$P_Fill_Result, _$P_Search_KEYorSW, _$P_Check_Match_Flags
;
; Vars: _$P_RC(W), _$P_SaveSI_Cmpx(W), _$P_KEYorSW_Ptr(R), _$P_Flags(W)
;***********************************************************************

_$P_Chk_Key_Control:
	stc				;AN000; this logic works when the KeySW
	retn				;AN000; is reset.

;***********************************************************************
; _$P_Search_KEYorSW:
;
; Function: Seach specified keyword or switch from CONTROL
;
; Input:     ES:BX -> CONTROL block
;	     cs:SI -> _$P_STRING_BUF
;
; Output:    CY = 1 : not match
;
; Use:	 _$P_String_Comp, _$P_MoveBP_NUL, _$P_Found_SYNONYM
;***********************************************************************

	; 25/10/2022 - Retro DOS v4.0
	; (MSDOS 5.0 IO.SYS - SYSINIT:18B6h)

_$P_Search_KEYorSW:			;AN000;
	push	bp			;AN000;
	push	cx			;AN000;
	mov	cl,[es:bx+_$P_Control_Blk.nid] ;AN000; Get synonym count
	xor	ch,ch			;AN000; and set it to cx
	or	cx,cx			;AN000; No synonyms specified ?
	jz	short _$P_KEYorSW_Not_Found ;AN000; then indicate not found by CY

	;lea	bp,[es:bx+_$P_Control_Blk.KEYorSW] ;AN000; BP points to the 1st synonym
	; 25/10/2022
	lea	bp,[bx+_$P_Control_Blk.KEYorSW]
	;lea	bp,[bx+9]
_$P_KEYorSW_Loop:			;AN000;
	call	_$P_String_Comp		;AN000; compare string in buffer w/ the synonym
	jnc	short _$P_KEYorSW_Found	;AN000; If match, set it to synonym pointer

	call	_$P_MoveBP_NUL		;AN000; else, bp points to the next string
	loop	_$P_KEYorSW_Loop 	;AN000; loop nid times
_$P_KEYorSW_Not_Found:			;AN000;
	stc				;AN000; indicate not found in synonym list
	jmp	short _$P_KEYorSW_Exit	;AN000; and exit

_$P_KEYorSW_Found:			;AN000;
	mov	[cs:_$P_Found_SYNONYM],bp ;AC034; set synonym pointer
	clc				;AN000; indicate found
_$P_KEYorSW_Exit:			;AN000;
	pop	cx			;AN000;
	pop	bp			;AN000;
	retn				;AN000;
 
;***********************************************************************
; _$P_MoveBP_NUL
;***********************************************************************

_$P_MoveBP_NUL:
_$P_MBP_Loop:				;AN000;
	;cmp	byte [es:bp],_$P_NULL	;AN000; Increment BP that points
	; 25/10/2022 (MSDOS 5.0 IO.SYS compatibility)
	; (SYSINIT:18DBh)
 	cmp     byte [es:bp+0], 0
	je	short _$P_MBP_Exit	;AN000; to the synomym list

	inc	bp			;AN000; until
	jmp	short _$P_MBP_Loop	;AN000; NULL encountered.

_$P_MBP_Exit:				;AN000;
	inc	bp			;AN000; bp points to next to NULL
	retn				;AN000;

;***********************************************************************
; _$P_Chk_SW_Control
;
; Function: Parse CONTROL block for a switch
;
; Input:     ES:BX -> CONTROL block
;	     cs:SI -> _$P_STRING_BUF
;
; Output:    CY = 1 : not match
;
; Use:	 _$P_Fill_Result, _$P_Search_KEYorSW, _$P_Check_Match_Flags
;
; Vars:  _$P_SaveSI_Cmpx(W), _$P_KEYorSW_Ptr(R), _$P_Flags(W)
;***********************************************************************

_$P_Chk_SW_Control:

;IF SwSW				;AN000;(Check if switch is supported)
	;or	byte [cs:_$P_Flags+1],10h
	or	byte [cs:_$P_Flags2],_$P_SW_Cmp ;AC034; Indicate switch for later string comparison
	call	_$P_Search_KEYorSW	;AN000; Search the switch in the CONTROL block
	jc	short _$P_Chk_SW_Err0	;AN000; not found, then try next CONTROL

	;and	[cs:_$P_Flags+],0EFh
	and	byte [cs:_$P_Flags2],0FFh-_$P_SW_Cmp 
					;AC034; reset the indicator previously set
	push	ax			;AN000; 	      /switch:
	mov	ax,[cs:_$P_KEYorSW_Ptr] ;AC034;	      ^       ^
	sub	ax,si			;AN000;  SI	KEYorSW
	add	[cs:_$P_SaveSI_Cmpx],ax	;AC034; update for complex list
	pop	ax			;AN000;

	mov	si,[cs:_$P_KEYorSW_Ptr] ;AC034; set si at the end or colon
	cmp	byte [cs:si],_$P_NULL	;AN000; any data after colon
	jne	short _$P_CSW00		;AN000; if yes, process match flags

	cmp	byte [cs:si-1],_$P_Colon ;AN000; if no, the switch terminated by colon ?
	jne	short _$P_Chk_if_data_required ;AN000; if yes,

	mov	word [cs:_$P_RC],_$P_Syntax ;AC034; return syntax error
	jmp	short _$P_Chk_SW_Exit	;AN000;

_$P_Chk_if_data_required:		;AN018; no data, no colon
	;cmp	word [es:bx+_$P_Control_Blk.Match_Flag],0 
	cmp	word [es:bx],0		;AN018; should have data? zero match flag means switch followed by nothing is OK
	je	short _$P_Chk_SW_Exit	;AN018; match flags not zero so should have something if optional bit is not on

	;test	word [es:bx+_$P_Control_Blk.Match_Flag],_$P_Optional 
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYINIT compatibility)
	;test	word [es:bx],1
	test	word [es:bx],_$P_Optional ;AN019; see if no value is valid
	;test	byte [es:bx],_$P_Optional
	jnz	short _$P_Chk_SW_Exit	;AN019; if so, then leave, else yell

	mov	word [cs:_$P_RC],_$P_Op_Missing ;AC034; return required operand missing
	jmp	short _$P_Chk_SW_Exit	;AN018;

_$P_CSW00:				;AN000;
	call	_$P_Check_Match_Flags	;AN000; process match flag
	clc				;AN000; indicate match
	jmp	short _$P_Chk_SW_Single_Exit ;AN000;

_$P_Chk_SW_Err0: 			;AN000;
	stc				;AN000; not found in switch synonym list
	jmp	short _$P_Chk_SW_Single_Exit ;AN000;

_$P_Chk_SW_Exit: 			;AN000;
	push	ax			;AN000;
	mov	al,_$P_String		;AN000; set
	mov	ah,_$P_No_Tag		;AN000;    result
	call	_$P_Fill_Result		;AN000; 	 buffer
	pop	ax			;AN000;
	clc				;AN000;
_$P_Chk_SW_Single_Exit:			;AN000;
	retn				;AN000;
;ELSE					;AN000;(of IF SwSW)
;	stc				;AN000; this logic works when the SwSW
;	retn				;AN000; is reset.

;***********************************************************************
; _$P_Fill_Result
;
; Function: Fill the result buffer
;
; Input:    AH = Item tag
;	    AL = type
;		  AL = 1: CX,DX has 32bit number (CX = high)
;		  AL = 2: DX has index(offset) into value list
;		  AL = 6: DL has driver # (1-A, 2-B, ... , 26 - Z)
;		  AL = 7: DX has year, CL has month and CH has date
;		  AL = 8: DL has hours, DH has minutes, CL has seconds,
;			  amd CH has hundredths
;		  AL = else: cs:SI points to returned string buffer
;	    ES:BX -> CONTROL block
;
; Output:   None
;
; Use:	_$P_Do_CAPS_String, _$P_Remove_Colon, _$P_Found_SYNONYM
;
; Vars: _$P_DX(W)
;***********************************************************************

_$P_Fill_Result:
	push	di			;AN000;
	mov	di,[es:bx+_$P_Control_Blk.Result_Buf]
					;AN000; di points to result buffer
	mov	[cs:_$P_DX],di		;AC034; set returned result address
	mov	[es:di+_$P_Result_Blk.Type],al ;AN000; store type
	mov	[es:di+_$P_Result_Blk.Item_Tag],ah ;AN000; store item tag
	push	ax			;AN000;
	mov	ax,[cs:_$P_Found_SYNONYM] ;AC034; if yes,
	mov	[es:di+_$P_Result_Blk.SYNONYM_Ptr],ax 
					;AN000;   then set it to the result
	pop	ax			;AN000;
_$P_RLT04:				;AN000;
	cmp	al,_$P_Number		;AN000; if number
	jne	short _$P_RLT00		;AN000;

_$P_RLT02:				;AN000;
	mov	[es:di+_$P_Result_Blk.Picked_Val],dx ;AN000; then store 32bit
	mov	[es:di+_$P_Result_Blk.Picked_Val+2],cx ;AN000; number
	jmp	short _$P_RLT_Exit	;AN000;

_$P_RLT00:				;AN000;
	cmp	al,_$P_List_Idx		;AN000; if list index
	jne	short _$P_RLT01		;AN000;

	mov	word [es:di+_$P_Result_Blk.Picked_Val],dx 
					;AN000; then store list index
	jmp	short _$P_RLT_Exit	;AN000;

_$P_RLT01:				;AN000;
	cmp	al,_$P_Date_F		;AN000; Date format ?
	je	short _$P_RLT02		;AN000;

	cmp	al,_$P_Time_F		;AN000; Time format ?
	je	short _$P_RLT02		;AN000;

	cmp	al,_$P_Drive		;AN000; drive format ?
	jne	short _$P_RLT03		;AN000;

	mov	[es:di+_$P_Result_Blk.Picked_Val],dl ;AN000; store drive number
	jmp	short _$P_RLT_Exit	;AN000;

_$P_RLT03:				;AN000;
	cmp	al,_$P_Complex		;AN000; complex format ?
	jne	short _$P_RLT05		;AN000;

	mov	ax,[cs:_$P_SaveSI_Cmpx] ;AC034; then get pointer in command buffer
	inc	ax			;AN000; skip left Parentheses
	mov	[es:di+_$P_Result_Blk.Picked_Val],ax ;AN000; store offset
	mov	[es:di+_$P_Result_Blk.Picked_Val+2],ds ;AN000; store segment
	jmp	short _$P_RLT_Exit	;AN000;

_$P_RLT05:				;AN000;
;------------------------  AL = 3, 5, or 9
	mov	[es:di+_$P_Result_Blk.Picked_Val],si 
					;AN000; store offset of STRING_BUF
	mov	[es:di+_$P_Result_Blk.Picked_Val+2],cs 
					;AN031; store segment of STRING_BUF
	push	ax			;AN000;
	test	byte [es:bx+_$P_Control_Blk.Function_Flag],_$P_CAP_File 
					;AN000; need CAPS by file table?
	jz	short _$P_RLT_CAP00	;AN000;

	mov	al,_$P_DOSTBL_File	;AN000; use file upper case table
	jmp	short _$P_RLT_CAP02	;AN000;

_$P_RLT_CAP00:				;AN000;
	test	byte [es:bx+_$P_Control_Blk.Function_Flag],_$P_CAP_Char 
					;AN000; need CAPS by char table ?
	jz	short _$P_RLT_CAP01	;AN000;

	mov	al,_$P_DOSTBL_Char	;AN000; use character upper case table
_$P_RLT_CAP02:				;AN000;
	call	_$P_Do_CAPS_String	;AN000;  process CAPS along the table
_$P_RLT_CAP01:				;AN000;
	pop	ax			;AN000;
	test	byte [es:bx+_$P_Control_Blk.Function_Flag],_$P_Rm_Colon 
					;AN000; removing colon at end ?
	jz	short _$P_RLT_Exit	;AN000;

	call	_$P_Remove_Colon 	;AN000; then process it.
_$P_RLT_Exit:				;AN000;
	pop	di			;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_Check_Match_Flags
;
; Function:  Check the mutch_flags and make the exit code and set the
;	     result buffer
;
;	    Check for types in this order:
;		Complex
;		Date
;		Time
;		Drive
;		Filespec
;		Quoted String
;		Simple String
;
; Input:     cs:SI -> _$P_STRING_BUF
;	     ES:BX -> CONTROL block
;
; Output:    None
;
; Use:	     _$P_Value, P$_SValue, _$P_Simple_String, _$P_Date_Format
;	     _$P_Time_Format, _$P_Complex_Format, _$P_File_Foemat
;	     _$P_Drive_Format
;***********************************************************************

	; 25/10/2022 - Retro DOS v4.0
	; (MSDOS 5.0 IO.SYS - SYSINIT:19CFh)

_$P_Check_Match_Flags:
	mov	byte [cs:_$P_err_flag],_$P_NULL 
					;AN033;AC034;; clear filespec error flag.
	push	ax			;AN000;
	;mov	ax,[es:bx+_$P_Control_Blk.Match_Flag]
	mov	ax,[es:bx]		;AN000; load match flag(16bit) to ax
	or	ax,ax			;AC035; test ax for zero
	jnz	short _$P_Mat		;AN000; (tm12)

	push	ax			;AN000; (tm12)
	push	bx			;AN000; (tm12)
	push	dx			;AN000; (tm12)
	push	di			;AN000; (tm12)
	mov	word [cs:_$P_RC],_$P_Syntax ;AC034; (tm12)
	mov	ah,_$P_No_Tag		;AN000; (tm12)
	mov	al,_$P_String		;AN000; (tm12)
	call	_$P_Fill_Result		;AN000; (tm12)
	pop	di			;AN000; (tm12)
	pop	dx			;AN000; (tm12)
	pop	bx			;AN000; (tm12)
	pop	ax			;AN000; (tm12)
	jmp	short _$P_Bridge 	;AC035; (tm12)
_$P_Mat: 				;AN000; (tm12)
	jmp	short _$P_Match03	;AN025; (tm09)
_$P_Bridge:
	jmp	short _$P_Match_Exit	;AN000; (tm02)
	
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	; (SYSINIT:19F9h)
	nop	; db 90h

_$P_Match03:				;AN000;
	test	ax,_$P_Num_Val		;AN000; Numeric value
	jz	short _$P_Match04	;AN000;

	mov	word [cs:_$P_RC],_$P_No_Error ;AC034; assume no error
	call	_$P_Value		;AN000; do process
	cmp	word [cs:_$P_RC],_$P_Syntax ;AC034; if error, examine the next type
	jne	short _$P_Match_Exit	;AN000;
_$P_Match04:				;AN000;
	test	ax,_$P_SNum_Val		;AN000; Signed numeric value
	jz	short _$P_Match05	;AN000;

	mov	word [cs:_$P_RC],_$P_No_Error ;AC034; assume no error
	call	_$P_SValue		;AN000; do process
	cmp	word [cs:_$P_RC],_$P_Syntax ;AC034; if error, examine the next type
	jne	short _$P_Match_Exit	;AN000;
_$P_Match05:				;AN000;
	test	ax,_$P_Drv_Only		;AN000; Drive only
	jz	short _$P_Match06	;AN000;

	mov	word [cs:_$P_RC],_$P_No_Error ;AC034; assume no error
	call	_$P_File_Format		;AN000; 1st, call file format
	call	_$P_Drive_Format	;AN000; check drive format, next
	cmp	word [cs:_$P_RC],_$P_Syntax ;AC034; if error, examine the next type
	jne	short _$P_Match_Exit	;AN000;
_$P_Match06:				;AN000;
	test	ax,_$P_File_Spc		;AN000; File spec
	jz	short _$P_Match07	;AN000;

	mov	word [cs:_$P_RC],_$P_No_Error ;AC034; assume no error
	call	_$P_File_Format		;AN000; do process
	cmp	word [cs:_$P_RC],_$P_Syntax ;AC034; if error, examine the next type
	jne	short _$P_Match_Exit	;AN000;
_$P_Match07:				;AN000;
	test	ax,_$P_Simple_S		;AN000; Simple string
	jz	short _$P_Match09	;AN000;

	mov	word [cs:_$P_RC],_$P_No_Error ;AC034; assume no error
	call	_$P_Simple_String	;AN000; do process
_$P_Match09:				;AN000;
_$P_Match_Exit:				;AN000;
	cmp	word [cs:_$P_err_flag],_$P_error_filespec ;AC034; bad filespec ?
	jne	short _$P_Match2_Exit	;AN033; no, continue
	cmp	word [cs:_$P_RC],_$P_No_Error ;AN033;AC034;; check for other errors ?
	jne	short _$P_Match2_Exit	;AN033; no, continue
	mov	word [cs:_$P_RC],_$P_Syntax ;AN033;AC034;; set error flag
_$P_Match2_Exit: 			;AN033;
	pop	ax			;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_Remove_Colon;
;
; Function: Remove colon at end
;
; Input:    cs:SI points to string buffer to be examineed
;
; Output:   None
;
; Use:	_$P_Chk_DBCS
;***********************************************************************

_$P_Remove_Colon:
	push	ax			;AN000;
	push	si			;AN000;
_$P_RCOL_Loop:				;AN000;
	mov	al,[cs:si]		;AN000; get character
	or	al,al			;AN000; end of string ?
	jz	short _$P_RCOL_Exit	;AN000; if yes, just exit

	cmp	al,_$P_Colon		;AN000; is it colon ?
	jne	short _$P_RCOL00	;AN000;

	cmp	byte [cs:si+1],_$P_NULL ;AN000; if so, next is NULL ?
	jne	short _$P_RCOL00	;AN000; no, then next char

	mov	byte [cs:si],_$P_NULL	;AN000; yes, remove colon
	jmp	short _$P_RCOL_Exit	;AN000; and exit.

_$P_RCOL00:				;AN000;
	call	_$P_Chk_DBCS		;AN000; if not colon, then check if
	jnc	short _$P_RCOL01	;AN000; DBCS leading byte.

	inc	si			;AN000; if yes, skip trailing byte
_$P_RCOL01:				;AN000;
	inc	si			;AN000; si points to next byte
	jmp	short _$P_RCOL_Loop	;AN000; loop until NULL encountered

_$P_RCOL_Exit:				;AN000;
	pop	si			;AN000;
	pop	ax			;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_Do_CAPS_String;
;
; Function: Perform capitalization along with the file case map table
;	    or character case map table.
;
; Input:    AL = 2 : Use character table
;	    AL = 4 : Use file table
;	    cs:SI points to string buffer to be capitalized
;
; Output:   None
;
; Use:	_$P_Do_CAPS_Char, _$P_Chk_DBCS
;***********************************************************************

_$P_Do_CAPS_String:
	push	si			;AN000;
	push	dx			;AN000;
	mov	dl,al			;AN000; save info id

_$P_DCS_Loop:				;AN000;
	mov	al,[cs:si]		;AN000; load charater and
	call	_$P_Chk_DBCS		;AN000; check if DBCS leading byte
	jc	short _$P_DCS00		;AN000; if yes, do not need CAPS

	or	al,al			;AN000; end of string ?
	jz	short _$P_DCS_Exit	;AN000; then exit.

	call	_$P_Do_CAPS_Char 	;AN000; Here a SBCS char need to be CAPS
	mov	[cs:si],al		;AN000; stored upper case char to buffer
	jmp	short _$P_DCS01		;AN000; process next
_$P_DCS00:				;AN000;
	inc	si			;AN000; skip DBCS leading and trailing byte
_$P_DCS01:				;AN000;
	inc	si			;AN000; si point to next byte
	jmp	short _$P_DCS_Loop	;AN000; loop until NULL encountered
_$P_DCS_Exit:				;AN000;
	pop	dx			;AN000;
	pop	si			;AN000;
	retn

;***********************************************************************
; _$P_Do_CAPS_Char;
;
; Function: Perform capitalization along with the file case map table
;	    or character case map table.
;
; Input:    DL = 2 : Use character table
;	    DL = 4 : Use file table
;	    AL = character to be capitalized
;
; Output:   None
;
; Use:	INT 21h /w AH=65h
;***********************************************************************

_$P_Do_CAPS_Char:
	cmp	al,_$P_ASCII80	;80h	;AN000; need upper case table ?
	jae	short _$P_DCC_Go	;AN000;

	cmp	al,"a"                  ;AN000; if no,
	jb	short _$P_CAPS_Ret	;AN000;   check if  "a" <= AL <= "z"

	cmp	al,"z"                  ;AN000;
	ja	short _$P_CAPS_Ret	;AN000;   if yes, make CAPS

	and	al,_$P_Make_Upper ;0DFh ;AN000;   else do nothing.
	jmp	short _$P_CAPS_Ret	;AN000;

_$P_DCC_Go:				;AN000;
	push	bx			;AN000;
	push	es			;AN000;
	push	di			;AN000;

	;lea	di,[cs:_$P_Char_CAP_Ptr] ;AC034; or use char CAPS table ?
	lea	di,[_$P_Char_CAP_Ptr]
_$P_DCC00:				;AN000;
	cmp	[cs:di],dl		;AN000; already got table address ?
	je	short _$P_DCC01		;AN000; if no,

;In this next section, ES will be used to pass a 5 byte workarea to INT 21h,
; the GET COUNTYRY INFO call. This usage of ES is required by the function
; call, regardless of what base register is currently be defined as cs.

	push	ax			;AN000; get CAPS table thru DOS call
	push	cx			;AN000;
	push	dx			;AN000;

	push	cs			;AC036; pass current base seg into
					;(Note: this used to push CS.  BUG...
	pop	es			;AN000;   ES reg, required for
					;get extended country information
	mov	ah,_$P_DOS_Get_TBL	;AN000; get extended CDI
	mov	al,dl			;AN000; upper case table
	mov	bx,_$P_DOSTBL_Def	;AN000; get active CON
	mov	cx,_$P_DOSTBL_BL 	;AN000; buffer length
	mov	dx,_$P_DOSTBL_Def	;AN000; get for default code page
					;DI already set to point to buffer
	int	21h			;AN000; es:di point to buffer that
					;now has been filled in with info
	pop	dx			;AN000;
	pop	cx			;AN000;
	pop	ax			;AN000;

_$P_DCC01:				;AN000;

;In this next section, ES will be used as the base of the XLAT table, provided
; by the previous GET COUNTRY INFO DOS call.  This usage of ES is made
; regardless of which base reg is currently the cs reg.

	mov	bx,[cs:di+_$P_DOS_TBL.Off] ;AN000; get offset of table
	mov	es,[cs:di+_$P_DOS_TBL.Seg] ;AN000; get segment of table
	inc	bx			;AC035; add '2' to
	inc	bx			;AC035;  BX reg
					;AN000; skip length field
	sub	al,_$P_ASCII80 ; 80h	;AN000; make char to index
	;xlat	es:[bx] 		;AN000; perform case map
	es
	xlat
	pop	di			;AN000;
	pop	es			;AN000;
	pop	bx			;AN000;
_$P_CAPS_Ret:				;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_Value / _$P_SValue
;
; Function:  Make 32bit value from cs:SI and see value list
;	     and make result buffer.
;	     _$P_SValue is an entry point for the signed value
;	     and this will simply call _$P_Value after the handling
;	     of the sign character, "+" or "-"
;
; Input:     cs:SI -> _$P_STRING_BUF
;	     ES:BX -> CONTROL block
;
; Output:    None
;
; Use:	_$P_Fill_Result, _$P_Check_OVF
;
; Vars: _$P_RC(W), _$P_Flags(RW)
;***********************************************************************

	; 26/10/2022 - Retro DOS v4.0
	; (MSDOS 5.0 IO.SYS - SYSINIT:1B0Bh)

_$P_SValue:				;AN000; when signed value here
	push	ax			;AN000;
	or	byte [cs:_$P_Flags2],_$P_Signed ;AC034; indicate a signed numeric
	and	byte [cs:_$P_Flags2],0FFh-_$P_Neg ;AC034; assume positive value
	mov	al,[cs:si]		;AN000; get sign
	cmp	al,_$P_Plus		;AN000; "+" ?
	je	short _$P_SVal00	;AN000;

	cmp	al,_$P_Minus		;AN000; "-" ?
	jne	short _$P_Sval01	;AN000; else

	or	byte [cs:_$P_Flags2],_$P_Neg ;AC034; set this is negative value
_$P_SVal00:				;AN000;
	inc	si			;AN000; skip sign char
_$P_Sval01:				;AN000;
	call	_$P_Value		;AN000; and process value
	pop	ax			;AN000;
	retn

;***********************************************************************

	; 26/10/2022
_$P_Value:				;AN000;
	push	ax			;AN000;
	push	cx			;AN000;
	push	dx			;AN000;
	push	si			;AN000;
	xor	cx,cx			;AN000; cx = higher 16 bits
	xor	dx,dx			;AN000; dx = lower 16 bits
	push	bx			;AN000; save control pointer
_$P_Value_Loop:				;AN000;
	mov	al,[cs:si]		;AN000; get character
	or	al,al			;AN000; end of line ?
	jz	short _$P_Value00	;AN000;

	call	_$P_0099 		;AN000; make asc(0..9) to bin(0..9)
	jc	short _$P_Value_Err0	;AN000;

	xor	ah,ah			;AN000;
	mov	bp,ax			;AN000; save binary number
	shl	dx,1			;AN000; to have 2*x
	rcl	cx,1			;AN000; shift left w/ carry
	call	_$P_Check_OVF		;AN000; Overflow occurred ?
	jc	short _$P_Value_Err0	;AN000; then error, exit

	mov	bx,dx			;AN000; save low(2*x)
	mov	ax,cx			;AN000; save high(2*x)
	shl	dx,1			;AN000; to have 4*x
	rcl	cx,1			;AN000; shift left w/ carry
	call	_$P_Check_OVF		;AN000; Overflow occurred ?
	jc	short _$P_Value_Err0	;AN000; then error, exit

	shl	dx,1			;AN000; to have 8*x
	rcl	cx,1			;AN000; shift left w/ carry
	call	_$P_Check_OVF		;AN000; Overflow occurred ?
	jc	short _$P_Value_Err0	;AN000; then error, exit

	add	dx,bx			;AN000; now have 10*x
	adc	cx,ax			;AN000; 32bit ADD
	call	_$P_Check_OVF		;AN000; Overflow occurred ?
	jc	short _$P_Value_Err0	;AN000; then error, exit

	add	dx,bp			;AN000; Add the current one degree decimal
	adc	cx,0			;AN000; if carry, add 1 to high 16bit
	call	_$P_Check_OVF		;AN000; Overflow occurred ?
	jc	short _$P_Value_Err0	;AN000; then error, exit

	inc	si			;AN000; update pointer
	jmp	short _$P_Value_Loop	;AN000; loop until NULL encountered
;
_$P_Value_Err0:				;AN000;
	pop	bx			;AN000;
	jmp	_$P_Value_Err		;AN000; Bridge
;
_$P_Value00:				;AN000;
	pop	bx			;AN000; restore control pointer
	test	byte [cs:_$P_Flags2],_$P_Neg ;AC034; here cx,dx = 32bit value
	jz	short _$P_Value01	;AN000; was it negative ?

	not	cx			;AN000; +
	not	dx			;AN000; |- Make 2's complement
	add	dx,1			;AN000; |
	adc	cx,0			;AN000; +

_$P_Value01:				;AN000; / nval =0
	mov	si,[es:bx+_$P_Control_Blk.Value_List] ;AN000; si points to value list
	mov	al,[es:si]		;AN000; get nval
	cmp	al,_$P_nval_None 	;AN000; no value list ?
	jne	short _$P_Value02	;AN000;

	mov	al,_$P_Number		;AN000; Set type
	mov	ah,_$P_No_Tag		;AN000; No ITEM_TAG set
	jmp	short _$P_Value_Exit	;AN000;

	; 26/10/2022 (MSDOS 5.0 IO.SYS, SYSINIT compatibility)
	; (SYSINIT:1BA5h)
	nop	; db  90h

_$P_Value02:				;AN000; / nval = 1
;IF	Val1SW				;AN000;(Check if value list id #1 is supported)
;(tm07) cmp	al,_$P_nval_Range	;AN000; have range list ?
;(tm07) jne	short _$P_Value03	;AN000;

	inc	si			;AN000;
	mov	al,[es:si]		;AN000; al = number of range
	cmp	al,_$P_No_nrng		;AN000; (tm07)
	je	short _$P_Value03	;AN000; (tm07)

	inc	si			;AN000; si points to 1st item_tag
_$P_Val02_Loop:				;AN000;
	test	byte [cs:_$P_Flags2],_$P_Signed ;AC034;
	jnz	short _$P_Val02_Sign	;AN000;

	cmp	cx,[es:si+_$P_Val_List.Val_XH] ;AN000; comp cx with XH
	jb	short _$P_Val02_Next	;AN000;
	ja	short _$P_Val_In	;AN000;

	cmp	dx,[es:si+_$P_Val_List.Val_XL] ;AN000; comp dx with XL
	jb	short _$P_Val02_Next	;AN000;

_$P_Val_In:				;AN000;
	cmp	cx,[es:si+_$P_Val_List.Val_YH] ;AN000; comp cx with YH (tm01)
	ja	short _$P_Val02_Next	;AN000;
	jb	short _$P_Val_Found	;AN000;

	cmp	dx,[es:si+_$P_Val_List.Val_YL] ;AN000; comp dx with YL
	ja	short _$P_Val02_Next	;AN000;

	jmp	short _$P_Val_Found	;AN000;

_$P_Val02_Sign:				;AN000;
	cmp	cx,[es:si+_$P_Val_List.Val_XH]	;AN000; comp cx with XH
	jl	short _$P_Val02_Next	;AN000;
	jg	short _$P_SVal_In	;AN000;

	cmp	dx,[es:si+_$P_Val_List.Val_XL]	;AN000; comp dx with XL
	jl	short _$P_Val02_Next	;AN000;

_$P_SVal_In:				;AN000;
	cmp	cx,[es:si+_$P_Val_List.Val_YH]	;AN000; comp cx with YH
	jg	short _$P_Val02_Next	;AN000;

	jl	short _$P_Val_Found	;AN000;

	cmp	dx,[es:si+_$P_Val_List.Val_YL]	;AN000; comp dx with YL
	jg	short _$P_Val02_Next	;AN000;

	jmp	short _$P_Val_Found	;AN000;

_$P_Val02_Next:				;AN000;
	add	si,_$P_Len_Range 	;AN000;
	dec	al			;AN000; loop nrng times in AL
	jne	short _$P_Val02_Loop	;AN000;
					; / Not found
	mov	word [cs:_$P_RC],_$P_Out_Of_Range ;AC034;
	mov	al,_$P_Number		;AN000;
	mov	ah,_$P_No_Tag		;AN000; No ITEM_TAG set
	jmp	short _$P_Value_Exit	;AN000;

_$P_Val_Found:				;AN000;
	mov	al,_$P_Number		;AN000;
	mov	ah,[es:si]		;AN000; found ITEM_TAG set
	jmp	short _$P_Value_Exit	;AN000;

_$P_Value03:				;AN000; / nval = 2

;IF	Val2SW				;AN000;(Check if value list id #2 is supported)
;;;;	cmp	al,$P_nval_Value	; have match list ? ASSUME nval=2,
;;;;	jne	$P_Value04		; even if it is 3 or more.
;(tm07) inc	si			;AN000;
;(tm07) mov	al,es:[si]		;AN000; al = nrng
;	mov	ah,$P_Len_Range 	;AN000;
;	mul	ah			;AN000;  Skip nrng field
;	inc	ax			;AN000;
;	add	si,ax			;AN000; si points to nnval
;	mov	al,es:[si]		;AN000; get nnval
;	inc	si			;AN000; si points to 1st item_tag
;$P_Val03_Loop:				;AN000;
;	cmp	cx,es:[si+$P_Val_XH]	;AN000; comp cx with XH
;	jne	$P_Val03_Next		;AN000;
;
;	cmp	dx,es:[si+$P_Val_XL]	;AN000; comp dx with XL
;	je	$P_Val_Found		;AN000;
;
;$P_Val03_Next:				;AN000;
;	add	si,$P_Len_Value 	;AN000; points to next value choice
;	dec	al			;AN000; loop nval times in AL
;	jne	$P_Val03_Loop		;AN000;
;					;AN000; / Not found
;	mov	psdata_seg:$P_RC,$P_Not_in_Val ;AC034;
;	mov	al,$P_Number		;AN000;
;	mov	ah,$P_No_Tag		;AN000; No ITEM_TAG set
;	jmp	short $P_Value_Exit	;AN000;
;
;ENDIF					;AN000;(of Val2SW)
;$P_Value04:

_$P_Value_Err:				;AN000;
	mov	word [cs:_$P_RC],_$P_Syntax ;AC034;
	mov	al,_$P_String		;AN000; Set type
	mov	ah,_$P_No_Tag		;AN000; No ITEM_TAG set
_$P_Value_Exit:				;AN000;
	call	_$P_Fill_Result		;AN000;
	pop	si			;AN000;
	pop	dx			;AN000;
	pop	cx			;AN000;
	pop	ax			;AN000;
	retn				;AN000;

; 28/03/2019 - Retro DOS v4.0

;***********************************************************************
; _$P_Check_OVF
;
; Function:  Check if overflow is occurred with consideration of
;	     signed or un-signed numeric value
;
; Input:     Flag register
;
; Output:    CY = 1  :	Overflow
;
; Vars:     _$P_Flags(R)
;***********************************************************************

	; 26/10/2022
_$P_Check_OVF:
	pushf				;AN000;
	test	byte [cs:_$P_Flags2],_$P_Neg ;AC034; is it negative value ?
	jnz	short _$P_COVF 		;AN000; if no, check overflow

	popf				;AN000; by the CY bit
	retn				;AN000;

_$P_COVF:				;AN000;
	popf				;AN000; else,
	jo	short _$P_COVF00	;AN000; check overflow by the OF

	clc				;AN000; indicate it with CY bit
	retn				;AN000; CY=0 means no overflow

_$P_COVF00:				;AN000;
	stc				;AN000; and CY=1 means overflow
	retn				;AN000;

;***********************************************************************
; _$P_0099;
;
; Function:  Make ASCII 0-9 to Binary 0-9
;
; Input:     AL = character code
;
; Output:    CY = 1 : AL is not number
;	     CY = 0 : AL contains binary value
;***********************************************************************

_$P_0099:
	cmp	al,"0"                  ;AN000;
	jb	short _$P_0099Err	;AN000; must be 0 =< al =< 9

	cmp	al,"9"                  ;AN000;
	ja	short _$P_0099Err	;AN000; must be 0 =< al =< 9

	sub	al,"0"                  ;AN000; make char -> bin
	clc				;AN000; indicate no error
	retn				;AN000;

_$P_0099Err:				;AN000;
	stc				;AN000; indicate error
	retn				;AN000;

;***********************************************************************
; _$P_Simple_String
;
; Function:  See value list for the simple string
;	     and make result buffer.
;
; Input:     cs:SI -> _$P_STRING_BUF
;	     ES:BX -> CONTROL block
;
; Output:    None
;
; Use:	_$P_Fill_Result, _$P_String_Comp
;
; Vars: _$P_RC(W)
;***********************************************************************

_$P_Simple_String:
	push	ax			;AN000;
	push	bx			;AN000;
	push	dx			;AN000;
	push	di			;AN000;
	mov	di,[es:bx+_$P_Control_Blk.Value_List] ;AN000; di points to value list
	mov	al,[es:di]		;AN000; get nval
	or	al,al			;AN000; no value list ?
	jnz	short _$P_Sim00		;AN000; then

	mov	ah,_$P_No_Tag		;AN000; No ITEM_TAG set
	jmp	short _$P_Sim_Exit	;AN000; and set result buffer

_$P_Sim00:				;AN000;
;IF	Val3SW+KeySW			;AN000;(Check if keyword or value list id #3 is supported)
	cmp	al,_$P_nval_String	;AN000; String choice list provided ?
	jne	short _$P_Sim01		;AN000; if no, syntax error

	inc	di			;AN000;
	mov	al,[es:di]		;AN000; al = nrng
	mov	ah,_$P_Len_Range 	;AN000;
	mul	ah			;AN000; Skip nrng field
	inc	ax			;AN000; ax = (nrng*9)+1
	add	di,ax			;AN000; di points to nnval
	mov	al,[es:di]		;AN000; get nnval
	mov	ah,_$P_Len_Value 	;AN000;
	mul	ah			;AN000; Skip nnval field
	inc	ax			;AN000; ax = (nnval*5)+1
	add	di,ax			;AN000; di points to nstrval
	mov	al,[es:di]		;AN000; get nstrval c
	inc	di			;AC035; add '2' to
	inc	di			;AC035;  DI reg
					;AN000; di points to 1st string in list
_$P_Sim_Loop:				;AN000;
	mov	bp,[es:di]		;AN000; get string pointer
	call	_$P_String_Comp		;AN000; compare it with operand
	jnc	short _$P_Sim_Found	;AN000; found on list ?

	add	di,_$P_Len_String ; 3	;AN000; if no, point to next choice
	dec	al			;AN000; loop nstval times in AL
	jne	short _$P_Sim_Loop	;AN000;
					;AN000; / Not found
	mov	word [cs:_$P_RC],_$P_Not_In_Str ;AC034;
	mov	ah,_$P_No_Tag		;AN000; No ITEM_TAG set
	jmp	short _$P_Sim_Exit	;AN000;

_$P_Sim_Found:				;AN000;
	mov	ah,[es:di-1]		;AN000; set item_tag
	mov	al,_$P_List_Idx		;AN000;
	mov	dx,[es:di]		;AN000; get address of STRING
	jmp	short _$P_Sim_Exit0	;AN000;
;ENDIF					;AN000;(of Val3SW+KeySW)
_$P_Sim01:				;AN000;
	mov	word [cs:_$P_RC],_$P_Syntax ;AC034;
	mov	ah,_$P_No_Tag		;AN000; No ITEM_TAG set
_$P_Sim_Exit:				;AN000;
	mov	al,_$P_String		;AN000; Set type
_$P_Sim_Exit0:				;AN000;
	call	_$P_Fill_Result		;AN000;
	pop	di			;AN000;
	pop	dx			;AN000;
	pop	bx			;AN000;
	pop	ax			;AN000;
	retn				;AN000;

;***********************************************************************
; _$P_String_Comp:
;
; Function:  Compare two string
;
; Input:     cs:SI -> 1st string
;	     ES:BP -> 2nd string  (Must be upper case)
;	     ES:BX -> CONTROL block
;
; Output:    CY = 1 if not match
;
; Use:	_$P_Chk_DBCS, _$P_Do_CAPS_Char
;
; Vars: _$P_KEYor_SW_Ptr(W), _$P_Flags(R). _$P_KEYorSW_Ptr
;***********************************************************************

_$P_String_Comp:
	push	ax			;AN000;
	push	bp			;AN000;
	push	dx			;AN000;
	push	si			;AN000;
	mov	dl,_$P_DOSTBL_Char	;AN000; use character case map table
_$P_SCOM_Loop:				;AN000;
	mov	al,[cs:si]		;AN000; get command character
	call	_$P_Chk_DBCS		;AN000; DBCS ?
	jc	short _$P_SCOM00	;AN000; yes,DBCS

	call	_$P_Do_CAPS_Char 	;AN000; else, upper case map before comparison
;IF KeySW+SwSW				;AN000;(Check if keyword or switch is supported)
	test	byte [cs:_$P_Flags2],_$P_Key_Cmp ;AC034; keyword search ?
	jz	short _$P_SCOM04	;AN000;

	cmp	al,_$P_Keyword		;AN000; "=" is delimiter
	jne	short _$P_SCOM03	;AN000; IF "=" on command line AND  (bp+1=> char after the "=" in synonym list)

	cmp	byte [es:bp+1],_$P_NULL ;AN021;  at end of keyword string in the control block THEN
	jne	short _$P_SCOM_Differ	;AN021;

	jmp	short _$P_SCOM05 	;AN000; keyword found in synonym list

_$P_SCOM04:				;AN000;
	test	byte [cs:_$P_Flags2],_$P_SW_Cmp ;AC034; switch search ?
	jz	short _$P_SCOM03	;AN000;

	cmp	al,_$P_Colon		;AN000; ":" is delimiter, at end of switch on command line
	jne	short _$P_SCOM03	;AN000; continue compares

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	cmp	byte [es:bp+0],_$P_NULL
	;cmp	byte [es:bp],_$P_NULL	;AN021; IF at end of switch on command AND
	jne	short _$P_SCOM_Differ	;AN021;   at end of switch string in the control block THEN

_$P_SCOM05:				;AN000;   found a match
	inc	si			;AN000; si points to just after "=" or ":"
	jmp	short _$P_SCOM_Same	;AN000; exit

_$P_SCOM03:				;AN000;
;ENDIF					;AN000;(of KeySW+SwSW)
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	cmp	al,[es:bp+0]
	;cmp	al,[es:bp]		;AN000; compare operand w/ a synonym
	jne	short _$P_SCOM_Differ0 	;AN000; if different, check ignore colon option

	or	al,al			;AN000; end of line
	jz	short _$P_SCOM_Same	;AN000; if so, exit

	inc	si			;AN000; update operand pointer
	inc	bp			;AN000;    and synonym pointer
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short _$P_SCOM01 	;AN000; loop until NULL or "=" or ":" found in case

_$P_SCOM00:				;AN000; Here al is DBCS leading byte
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	cmp	al,[es:bp+0]
	;cmp	al,[es:bp]		;AN000; compare leading byte
	jne	short _$P_SCOM_Differ	;AN000; if not match, say different

	inc	si			;AN000; else, load next byte
	mov	al,[cs:si]		;AN000; and
	inc	bp			;AN000;
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	cmp	al,[es:bp+0]
	;cmp	al,[es:bp]		;AN000; compare 2nd byte
	jne	short _$P_SCOM_Differ	;AN000; if not match, say different, too

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;_$P_SCOM01:
	inc	si			;AN000; else update operand pointer
	inc	bp			;AN000; 		and synonym pointer
_$P_SCOM01:				;AN000;
	jmp	short _$P_SCOM_Loop	;AN000; loop until NULL or "=" or "/" found in case

_$P_SCOM_Differ0:			;AN000;
;IF SwSW				;AN000;(tm10)
	test	byte [cs:_$P_Flags2],_$P_SW ;AC034;(tm10)
	jz	short _$P_not_applicable ;AN000;(tm10)

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	test	word [es:bx+_$P_Control_Blk.Function_Flag],_$P_colon_is_not_necessary ;AN000;(tm10)
	;test	byte [es:bx+_$P_Control_Blk.Function_Flag],_$P_colon_is_not_necessary
	je	short _$P_not_applicable ;AN000;(tm10)

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	cmp	byte [es:bp+0],_$P_NULL
	;cmp	byte [es:bp],_$P_NULL	;AN000;(tm10)
;(deleted ;AN025;) jne short _$P_not_applicable ;AN000;(tm10)
	je	short _$P_SCOM_Same	;AN025;(tm10)

_$P_not_applicable:			;AN000;(tm10)
;ENDIF					;AN000;(tm10)

	;test	word [es:bx+_$P_Control_Blk.Match_Flag],_$P_Ig_Colon 
					;AN000; ignore colon option specified ?
	;test	byte [es:bx+_$P_Control_Blk.Match_Flag],_$P_Ig_Colon
	;test	byte [es:bx],_$P_Ig_Colon
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	test	word [es:bx],_$P_Ig_Colon ; 10h
	jz	short _$P_SCOM_Differ	;AN000; if no, say different.

	cmp	al,_$P_Colon		;AN000; End up with ":" and
	jne	short _$P_SCOM02	;AN000;    subseqently

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	cmp	byte [es:bp+0],_$P_NULL
	;cmp	byte [es:bp],_$P_NULL	;AN000; NULL ?
	jne	short _$P_SCOM_Differ	;AN000; if no, say different

	jmp	short _$P_SCOM_Same	;AN000; else, say same

_$P_SCOM02:				;AN000;
	cmp	al,_$P_NULL		;AN000; end up NULL and :
	jne	short _$P_SCOM_Differ	;AN000;

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	cmp	byte [es:bp+0],_$P_Colon
	;cmp	byte [es:bp],_$P_Colon	;AN000; if no, say different
	je	short _$P_SCOM_Same	;AN000; else, say same

_$P_SCOM_Differ: 			;AN000;
	stc				;AN000; indicate not found
	jmp	short _$P_SCOM_Exit	;AN000;

_$P_SCOM_Same:				;AN000;
	mov	[cs:_$P_KEYorSW_Ptr],si ;AC034; for later use by keyword or switch
	clc				;AN000; indicate found
_$P_SCOM_Exit:				;AN000;
	pop	si			;AN000;
	pop	dx			;AN000;
	pop	bp			;AN000;
	pop	ax			;AN000;
	retn

; 30/03/2019

;IF FileSW+DrvSW			;AN000;(Check if file spec or drive only is supported)

;***********************************************************************
; _$P_File_Format;
;
; Function:  Check if the input string is valid file spec format.
;	     And set the result buffer.
;
; Input:     cs:SI -> _$P_STRING_BUF
;	     ES:BX -> CONTROL block
;
; Output:    None
;
; Use:	_$P_Fill_Result, _$P_Chk_DBCS, _$P_FileSp_Chk
;
; Vars: _$P_RC(W), _$P_SI_Save(W), _$P_Terminator(W), _$P_SaveSI_Cmpx(R)
;	_$P_SaveSI_Cmpx(R)
;***********************************************************************

_$P_File_Format:
	push	ax			;AN000;
	push	di			;AN000;
	push	si			;AN000;
	mov	di,[cs:_$P_SaveSI_Cmpx]	;AC034; get user buffer address
_$P_FileF_Loop0: 			;AN000; / skip special characters
	mov	al,[cs:si]		;AN000; load character
	or	al,al			;AN000; end of line ?
	jz	short _$P_FileF_Err	;AN000; if yes, error exit

	call	_$P_FileSp_Chk		;AN000; else, check if file special character
	jne	short _$P_FileF03	;AN000; if yes,

	mov	byte [cs:_$P_err_flag],_$P_error_filespec 
					;AN033;AC034;; set error flag- bad char.
	pop	si			;AN033;
	mov	byte [cs:si],_$P_NULL	;AN033;
	pop	di			;AN033;
	jmp	short _$P_FileF02	;AN033;

_$P_FileF_Err:				;AN000;
	pop	si			;AN000;
	mov	byte [cs:si],_$P_NULL	;AN000;
	pop	di			;AN000;

	;test	word [es:bx+_$P_Control_Blk.Match_Flag],_$P_Optional ;AN000; is it optional ?
	;test	byte [es:bx+_$P_Control_Blk.Match_Flag],_$P_Optional
	;test	byte [es:bx],_$P_Optional
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	test	word [es:bx],_$P_Optional
	jnz	short _$P_FileF02	;AN000;

	mov	word [cs:_$P_RC],_$P_Op_Missing ;AC034; 3/17/87
	jmp	short _$P_FileF02	;AN000;

_$P_FileF03:				;AN000;
	pop	ax			;AN000; discard save si
	push	si			;AN000; save new si
_$P_FileF_Loop1: 			;AN000;
	mov	al,[cs:si]		;AN000; load character (not special char)
	or	al,al			;AN000; end of line ?
	jz	short _$P_FileF_RLT	;AN000;

	call	_$P_FileSp_Chk		;AN000; File special character ?
	je	short _$P_FileF00	;AN000;

	call	_$P_Chk_DBCS		;AN000; no, then DBCS ?
	jnc	short _$P_FileF01	;AN000;
	inc	di			;AN000; if yes, skip next byte
	inc	si			;AN000;
_$P_FileF01:				;AN000;
	inc	di			;AN000;
	inc	si			;AN000;
	jmp	short _$P_FileF_Loop1	;AN000;
;
_$P_FileF00:				;AN000;
	mov	[cs:_$P_Terminator],al	;AC034;
	mov	byte [cs:si],_$P_NULL	;AN000; update end of string
	inc	di			;AN000;
	mov	[cs:_$P_SI_Save],di	;AC034; update next pointer in command line
_$P_FileF_RLT:				;AN000;
	pop	si			;AN000;
	pop	di			;AN000;
_$P_FileF02:				;AN000;
	pop	ax			;AN000; (tm14)
	test	ax,_$P_File_Spc		;AN000; (tm14)
	jz	short _$P_Drv_Only_Exit	;AN000; (tm14)

	push	ax			;AN000;  (tm14)

	mov	ah,_$P_No_Tag		;AN000; set
	mov	al,_$P_File_Spec 	;AN000;    result
	call	_$P_Fill_Result		;AN000; 	 buffer to file spec
	pop	ax			;AN000;

_$P_Drv_Only_Exit:			;AN000; (tm14)
	retn				;AN000;

;***********************************************************************
; _$P_FileSp_Chk
;
; Function:  Check if the input byte is one of file special characters
;
; Input:     cs:SI -> _$P_STRING_BUF
;	     AL = character code to be examineed
;
; Output:    ZF = 1 , AL is one of special characters
;***********************************************************************

_$P_FileSp_Chk:
	push	bx			;AN000;
	push	cx			;AN000;
	;lea	bx,[cs:_$P_FileSp_Char] ;AC034; special character table
	lea	bx,[_$P_FileSp_Char]
	mov	cx,_$P_FileSp_Len	;AN000; load length of it
_$P_FileSp_Loop: 			;AN000;
	cmp	al,[cs:bx]		;AN000; is it one of special character ?
	je	short _$P_FileSp_Exit	;AN000;

	inc	bx			;AN000;
	loop	_$P_FileSp_Loop		;AN000;

	inc	cx			;AN000; reset ZF
_$P_FileSp_Exit: 			;AN000;
	pop	cx			;AN000;
	pop	bx			;AN000;
	retn

;ENDIF					;AN000;(of FileSW+DrvSW)

;IF	DrvSW				;AN000;(Check if drive only is supported)

;***********************************************************************
; _$P_Drive_Format;
;
; Function:  Check if the input string is valid drive only format.
;	     And set the result buffer.
;
; Input:     cs:SI -> _$P_STRING_BUF
;	     ES:BX -> CONTROL block
;
; Output:    None
;
; Use:	_$P_Fill_Result, _$P_Chk_DBCS
;
; Vars: _$P_RC(W)
;***********************************************************************

_$P_Drive_Format:
	push	ax			;AN000;
	push	dx			;AN000;
	mov	al,[cs:si]		;AN000;
	or	al,al			;AN000; if null string
	je	short _$P_Drv_Exit	;AN000; do nothing

	call	_$P_Chk_DBCS		;AN000; is it leading byte ?
	jc	short _$P_Drv_Err	;AN000;

	cmp	word [cs:si+1],_$P_Colon ;AN000; "d", ":", 0  ?
	je	short _$P_DrvF00	;AN000;

	;test	word [es:bx+_$P_Control_Blk.Match_Flag],_$P_Ig_Colon 
	;test	byte [es:bx+_$P_Control_Blk.Match_Flag],_$P_Ig_Colon ;AN000; colon can be ignored?
	;test	byte [es:bx],_$P_Ig_Colon
	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	test	word [es:bx],_$P_Ig_Colon
	jz	short _$P_Drv_Err	;AN000;

	cmp	byte [cs:si+1],_$P_NULL ;AN000; "d", 0  ?
	jne	short _$P_Drv_Err	;AN000;

_$P_DrvF00:				;AN000;
	or	al,_$P_Make_Lower	;AN000; lower case
	cmp	al,"a"                  ;AN000; drive letter must
	jb	short _$P_Drv_Err	;AN000; in range of

	cmp	al,"z"                  ;AN000; "a" - "z"
	ja	short _$P_Drv_Err	;AN000; if no, error

	sub	al,"a"-1                ;AN000; make text drive to binary drive
	mov	dl,al			;AN000; set
	mov	ah,_$P_No_Tag		;AN000;    result
	mov	al,_$P_Drive		;AN000; 	 buffer
	call	_$P_Fill_Result		;AN000; 	       to drive
	jmp	short _$P_Drv_Exit	;AN000;

_$P_Drv_Err:				;AN000;
	mov	word [cs:_$P_RC],_$P_Syntax ;AC034;
_$P_Drv_Exit:				;AN000;
	pop	dx			;AN000;
	pop	ax			;AN000;
	retn				;AN000;

;ENDIF					;AN000;(of DrvSW)

;***********************************************************************
; _$P_Skip_Delim;
;
; Function: Skip delimiters specified in the PARMS list, white space
;	    and comma.
;
; Input:    DS:SI -> Command String
;	    ES:DI -> Parameter List
;
; Output:   CY = 1 if the end of line encounterd
;	    CY = 0 then SI move to 1st non-delimiter character
;	    AL = Last examineed character
;
; Use:	    _$P_Chk_EOL, _$P_Chk_Delim,
;
; Vars:     _$P_Flags(R)
;***********************************************************************

_$P_Skip_Delim:
_$P_Skip_Delim_Loop:			;AN000;
	lodsb				;AN000;
	call	_$P_Chk_EOL		;AN000; is it EOL character ?
	jz	short _$P_Skip_Delim_CY	;AN000; if yes, exit w/ CY on

	call	_$P_Chk_Delim		;AN000; is it one of delimiters ?
	jnz	short _$P_Skip_Delim_NCY ;AN000; if no, exit w/ CY off

	test	byte [cs:_$P_Flags2],_$P_Extra ;AC034; extra delim or comma found ?
	jz	short _$P_Skip_Delim_Loop ;AN000; if no, loop

	test	byte [cs:_$P_Flags2],_$P_SW+_$P_equ ;AC034; /x , or xxx=zzz , (tm08)
	jz	short _$P_Exit_At_Extra	;AN000; no switch, no keyword (tm08)

	dec	si			;AN000; backup si for next call (tm08)
	jmp	short _$P_Exit_At_Extra	;AN000; else exit w/ CY off

_$P_Skip_Delim_CY:			;AN000;
	stc				;AN000; indicate EOL
	jmp	short _$P_Skip_Delim_Exit ;AN000;

_$P_Skip_Delim_NCY:			;AN000;
	clc				;AN000; indicate non delim
_$P_Skip_Delim_Exit:			;AN000; in this case, need
	dec	si			;AN000;  backup index pointer
	retn				;AN000;

_$P_Exit_At_Extra:			;AN000;
	clc				;AN000; indicate extra delim
	retn				;AN000;

;***********************************************************************
; _$P_Chk_EOL;
;
; Function: Check if AL is one of End of Line characters.
;
; Input:    AL = character code
;	    ES:DI -> Parameter List
;
; Output:   ZF = 1 if one of End of Line characters
;**********************************************************************

_$P_Chk_EOL:
	push	bx			;AN000;
	push	cx			;AN000;
	cmp	al,_$P_CR		;AN000; Carriage return ?
	je	short _$P_Chk_EOL_Exit 	;AN000;

	cmp	al,_$P_NULL		;AN000; zero ?
	je	short _$P_Chk_EOL_Exit 	;AN000;

;IF LFEOLSW				;AN028; IF LF TO BE ACCEPTED AS EOL
	cmp	al,_$P_LF		;AN000; Line feed ?
	je	short _$P_Chk_EOL_Exit 	;AN000;
;ENDIF					;AN028;

	cmp	byte [es:di+_$P_PARMS_Blk.Num_Extra],_$P_I_Have_EOL 
					;AN000; EOL character specified ?
	jb	short _$P_Chk_EOL_Exit 	;AN000;

	xor	bx,bx			;AN000;
	mov	bl,[es:di+_$P_PARMS_Blk.Len_Extra_Delim]
					;AN000; get length of delimiter list
	add	bx,_$P_Len_PARMS 	;AN000; skip it
	cmp	byte [es:bx+di],_$P_I_Use_Default ;AN000; No extra EOL character ?
	je	short _$P_Chk_EOL_NZ	;AN000;

	;xor	cx,cx			;AN000; Get number of extra character
	xor	ch,ch
	mov	cl,[es:bx+di]		;AN000;
_$P_Chk_EOL_Loop:			;AN000;
	inc	bx			;AN000;
	cmp	al,[es:bx+di]		;AN000; Check extra EOL character
	je	short _$P_Chk_EOL_Exit 	;AN000;

	loop	_$P_Chk_EOL_Loop 	;AN000;

_$P_Chk_EOL_NZ:				;AN000;
	cmp	al,_$P_CR		;AN000; reset ZF
_$P_Chk_EOL_Exit:			;AN000;
	pop	cx			;AN000;
	pop	bx			;AN000;
	retn

;***********************************************************************
; _$P_Chk_Delim;
;
; Function: Check if AL is one of delimiter characters.
;	    if AL+[si] is DBCS blank, it is replaced with two SBCS
;	    blanks.
;
; Input:    AL = character code
;	    DS:SI -> Next Character
;	    ES:DI -> Parameter List
;
; Output:   ZF = 1 if one of delimiter characters
;	    SI points to the next character
; Vars:  _$P_Terminator(W), _$P_Flags(W)
;***********************************************************************

_$P_Chk_Delim:
	push	bx			;AN000;
	push	cx			;AN000;
	mov	byte [cs:_$P_Terminator],_$P_Space 
					;AC034; Assume terminated by space
	;and	byte [cs:_$P_Flags20,0DFh
	and	byte [cs:_$P_Flags2],0FFh-_$P_Extra ;AC034;
	cmp	al,_$P_Space ; 20h	;AN000; Space ?
	je	short _$P_Chk_Delim_Exit ;AN000;

	cmp	al,_$P_TAB		;AN000; TAB ?
	je	short _$P_Chk_Delim_Exit ;AN000;

	cmp	al,_$P_Comma		;AN000; Comma ?
	je	short _$P_Chk_Delim_Exit0 ;AN000;

	; 26/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
_$P_Chk_Delim00: 			;AN000;
	cmp	al,_$P_DBSP1	; 20h	;AN000; 1st byte of DBCS Space ?
	jne	short _$P_Chk_Delim01	;AN000;

	cmp	byte [si],_$P_DBSP2 ; 20h ;AN000; 2nd byte of DBCS Space ?
	jne	short _$P_Chk_Delim01	;AN000;

	mov	al,_$P_Space		;AN000;
	inc	si			;AN000; make si point to next character
	cmp	al,al			;AN000; Set ZF
	jmp	short _$P_Chk_Delim_Exit ;AN000;

_$P_Chk_Delim01: 			;AN000;
	cmp	byte [es:di-_$P_PARMS_Blk.Num_Extra],_$P_I_Have_Delim 
					;AN000; delimiter character specified ?
	jb	short _$P_Chk_Delim_Exit ;AN000;

	;xor	cx,cx			;AN000;
	xor	ch,ch
	;mov	cl,[es:di+3]
	mov	cl,[es:di+_$P_PARMS_Blk.Len_Extra_Delim] 
					;AN000; get length of delimiter list
	or	cx,cx			;AN000; No extra Delim character ?
	jz	short _$P_Chk_Delim_NZ 	;AN000;

	mov	bx,_$P_Len_PARMS-1 ; 3	;AN000; set bx to 1st extra delimiter
_$P_Chk_Delim_Loop:			;AN000;
	inc	bx			;AN000;
	cmp	al,[es:bx+di]		;AN000; Check extra Delim character
	je	short _$P_Chk_Delim_Exit0 ;AN000;

	loop	_$P_Chk_Delim_Loop	;AN000; examine all extra delimiter

_$P_Chk_Delim_NZ:			;AN000;
	cmp	al,_$P_Space		;AN000; reset ZF
_$P_Chk_Delim_Exit:			;AN000;
_$P_ChkDfin:				;AN000;
	pop	cx			;AN000;
	pop	bx			;AN000;
	retn				;AN000;

_$P_Chk_Delim_Exit0:			;AN000;
	mov	[cs:_$P_Terminator],al ;AC034; keep terminated delimiter
	test	byte [cs:_$P_Flags2],_$P_equ ;AN027;AC034;; if terminating a key=
	jnz	short _$P_No_Set_Extra 	;AN027; then do not set the EXTRA bit

	or	byte [cs:_$P_Flags2],_$P_Extra 
					;AC034; flag terminated extra delimiter or comma
_$P_No_Set_Extra:			;AN027;
	cmp	al,al			;AN000; set ZF
	jmp	short _$P_Chk_Delim_Exit ;AN000;


;***********************************************************************
; _$P_Chk_Switch;
;
; Function: Check if AL is the switch character not in first position of
;	    _$P_STRING_BUF
;
; Input:    AL = character code
;	    BX = current pointer within _$P_String_Buf
;	    SI =>next char on command line (following the one in AL)
;
; Output:   CF = 1 (set)if AL is switch character, and not in first
;		 position, and has no chance of being part of a date string,
;		 i.e. should be treated as a delimiter.

;	    CF = 0 (reset, cleared) if AL is not a switch char, is in the first
;		 position, or is a slash but may be part of a date string, i.e.
;		 should not be treated as a delimiter.
;
; Vars:  _$P_Terminator(W)

; Use:	 _$P_0099
;***********************************************************************

_$P_Chk_Switch:
	;lea	bp,[cs:_$P_STRING_BUF]	;AN020;AC034
	lea	bp,[_$P_STRING_BUF]	; BP=OFFSET of _$P_String_Buf even in group addressing
;	.IF <BX NE BP> THEN		;AN020;IF not first char THEN
	cmp	bx,bp			;AN000;
	je	short _$P_STRUC_L2	;AN000;

;	    .IF <AL EQ _$P_Switch> THEN	;AN020;otherwise see if a slash
	cmp	al,_$P_Switch		;AN000;
	jne	short _$P_STRUC_L5 	;AN000;

	stc				;AN020;not in first position and is slash
	jmp     short _$P_STRUC_L1	;AN000;

_$P_STRUC_L5:				;AN000;
	CLC				;AN020;not a slash
;	    .ENDIF			;AN020;
;	.ELSE				;AN020;is first char in the buffer, ZF=0
	jmp	short _$P_STRUC_L1	;AN000;

_$P_STRUC_L2:				;AN000;
;	    .IF <AL EQ _$P_Switch> THEN	;AN020;
	cmp     al,_$P_Switch		;AN000;
	jne	short _$P_STRUC_L12	;AN000;

	or	byte [cs:_$P_Flags2],_$P_SW ;AN020;AC034;;could be valid switch, first char and is slash
;	    .ENDIF			;AN020;
_$P_STRUC_L12:				;AN000;
	clc				;AN020;CF=0 indicating first char
;	.ENDIF				;AN020;
_$P_STRUC_L1:				;AN000;
	retn				;AN000;

;**************************************************************************
; _$P_Chk_DBCS:
;
;  Function: Check if a specified byte is in ranges of the DBCS lead bytes
;
;  Input:
;	  AL	= Code to be examineed
;
;  Output:
;	  If CF is on then a lead byte of DBCS
;
; Use: INT 21h w/AH=63
;
; Vars:  _$P_DBCSEV_Seg(RW), _$P_DBCSEV_Off(RW)
;***************************************************************************

_$P_Chk_DBCS:
	push	ds			;AN000;
	push	si			;AN000;
	push	bx			;AN000; (tm11)
	
	cmp	word [cs:_$P_DBCSEV_SEG],0 ;AC034; ALREADY SET ?
	jne	short _$P_DBCS00	;AN000;

	push	ax			;AN000;
	push	ds			;AN000; (tm11)
	push	cx			;AN000;
	push	dx			;AN000;
	push	di			;AN000;
	push	bp			;AN000;
	push	es			;AN000;
	xor	si,si			;AN000;
	mov	ds,si			;AN000;
	MOV	ax,_$P_DOS_GetEV ; 6300h ;AN000; GET DBCS EV CALL
	int	21h			;AN000;
		; DOS - 3.2+ only - GET DOUBLE BYTE CHARACTER SET LEAD TABLE
	mov	bx,ds			;AN000; (tm11)
	or	bx,bx			;AN000; (tm11)
	pop	es			;AN000;
	pop	bp			;AN000;
	pop	di			;AN000;
	pop	dx			;AN000;
	pop	cx			;AN000;
	pop	ds			;AN000; (tm11)
	pop	ax			;AN000;
	jz	short _$P_NON_DBCS	;AN000;

_$P_DBCS02:				;AN000;
	mov	[cs:_$P_DBCSEV_OFF],si	;AC034; save EV offset
	mov	[cs:_$P_DBCSEV_SEG],bx	;AC034; save EV segment (tm11)
_$P_DBCS00:				;AN000;
	mov	si,[cs:_$P_DBCSEV_OFF]	;AC034; load EV offset
	mov	ds,[cs:_$P_DBCSEV_SEG]	;AC034; and segment

_$P_DBCS_LOOP:				;AN000;
	cmp	word [si],0		;AN000; zero vector ?
	je	short _$P_NON_DBCS	;AN000; then exit

	cmp	al,[si] 		;AN000;
	jb	short _$P_DBCS01	;AN000; Check if AL is in

	cmp	al,[si+1]		;AN000;   range of
	ja	short _$P_DBCS01	;AN000;      the vector

	stc				;AN000; if yes, indicate DBCS and exit
	jmp	short _$P_DBCS_EXIT	;AN000;

_$P_DBCS01:				;AN000;
	inc	si			;AC035; add '2' to
	inc	si			;AC035;  SI reg
					;AN000; get next vector
	jmp	short _$P_DBCS_LOOP	;AN000; loop until zero vector found

_$P_NON_DBCS:				;AN000;
	clc				;AN000; indicate SBCS
_$P_DBCS_EXIT:				;AN000;
	pop	bx			;AN000; (tm11)
	pop	si			;AN000;
	pop	ds			;AN000;
	retn				;AN000;

; SYSCONF.ASM - MSDOS 6.0 - 1991
; ======================================================================
; 27/03/2019 - Retro DOS v4.0

;control block definitions for parser.
;-----------------------------------------------------------------------
; buffer = [n | n,m] {/e}

; 30/03/2019

struc p_parms
	resw	1	; dw ?
	resb	1	; db 1	; an extra delimiter list
	resb	1	; db 1	; length is 1
	resb 	1	; db ';' ; delimiter
.size:
endstruc

struc p_pos
	resw	1	; dw ?	; numeric value??
	resw	1	; dw ?	; function
	resw	1	; dw ?	; result value buffer

; note: by defining result_val before this structure, we could remove
;  the "result_val" from every structure invocation

	resw	1	; dw ?	; value list
	resb	1	; db 0	; no switches/keywords
.size:
endstruc

struc	p_range
	resb	1	; db 1	; range definition
	resb 	1	; db 1	; 1 definition of range
	resb 	1	; db 1	; item tag for this range
	resd	1	; dd ?	; numeric min
	resd	1	; dd ?	; numeric max
.size:
endstruc

;-----------------------------------------------------------------------

	; 26/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:1F48h)

; buffer = [n | n,m] {/e}

;buf_parms p_parms <buf_parmsx>
buf_parms: 
	dw	buf_parmsx
	db	1	; an extra delimiter list
	db	1	; length is 1
	db	';'	; delimiter

buf_parmsx:
	dw	201h,buf_pos1,buf_pos2	; min 1, max 2 positionals
	db	1			; one switch
	dw	sw_x_ctrl
	db	0			; no keywords

;buf_pos1 p_pos <8000h,0,result_val,buf_range_1>  ; numeric
buf_pos1:
	dw	8000h	; numeric value??
	dw	0	; function
	dw	result_val ; result value buffer	
	dw	buf_range_1 ; value list
	db	0  	; no switches/keywords

;buf_range_1 p_range <,,,1,99>		; M050
buf_range_1:
	db	1	; range definition
	db	1	; 1 definition of range
	db	1	; item tag for this range
	dd	1	; numeric min
	dd	99	; numeric max

;buf_pos2 p_pos <8001h,0,result_val,buf_range_2> ; optional num.
buf_pos2:
	dw	8001h
	dw	0
	dw	result_val	
	dw	buf_range_2
	db	0

;buf_range_2 p_range <,,,0,8>
buf_range_2:
	db	1
	db	1
	db	1
	dd	0
	dd	8

;sw_x_ctrl p_pos <0,0,result_val,noval,1> ; followed by one switch
sw_x_ctrl:
	dw	0
	dw	0
	dw	result_val	
	dw	noval
	db	1	; 1 switch
	
switch_x:
	db	'/X',0		; M016

p_buffers:
	dw	0	; local variables
p_h_buffers:
	dw	0
	; 26/10/2022  (MSDOS 5.0 IO.SYS SYSINIT compatibility)
p_buffer_slash_x:
	db	0 ; 31/03/2019

;-- common definitions -------------------------------------------------

noval:	db	0

result_val: 	;label	byte
	db	0		; type returned
result_val_itag:
	db	0		; item tag returned
result_val_swoff:
	dw	0		; es:offset of the switch defined
rv_byte:	;label	byte
rv_dword: dd	0		; value if number,or seg:offset to string.

;-----------------------------------------------------------------------

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:1F99h)

; break = [ on | off ]

;brk_parms p_parms  <brk_parmsx>
brk_parms:
	dw	brk_parmsx
	db	1	; an extra delimiter list
	db	1	; length is 1
	db	';'	; delimiter

brk_parmsx:
	dw	101h,brk_pos	; min,max = 1 positional
	db	0		; no switches
	db	0		; no keywords

;brk_pos p_pos <2000h,0,result_val,on_off_string> ; simple string
brk_pos:
	dw	2000h
	dw	0
	dw	result_val	
	dw	on_off_string
	db	0

on_off_string:	;label	byte
	db	3		; signals that there is a string choice
	db	0		; no range definition
	db	0		; no numeric values choice
	db	2		; 2 strings for choice
	db	1		; the 1st string tag
	dw	on_string
	db	2		; the 2nd string tag
	dw	off_string

on_string:
	db	"ON",0
off_string:
	db	"OFF",0

p_ctrl_break:
	db	0	; local variable

;-----------------------------------------------------------------------

	; 27/10/2022

; country = n {m {path}}
; or
; country = n,,path

;cntry_parms p_parms <cntry_parmsx>
cntry_parms:
	dw	cntry_parmsx
	db	1
	db	1
	db	';'
	
cntry_parmsx:
	dw	301h,cntry_pos1,cntry_pos2,cntry_pos3 ; min 1, max 3 pos.
	db	0		; no switches
	db	0		; no keywords

;cntry_pos1 p_pos <8000h,0,result_val,cc_range> ; numeric value
cntry_pos1:
	dw	8000h
	dw	0
	dw	result_val	
	dw	cc_range
	db	0

;cc_range p_range <,,,1,999>
cc_range:
	db	1
	db	1
	db	1
	dd	1
	dd	999

;cntry_pos2 p_pos <8001h,0,result_val,cc_range> ; optional num.
cntry_pos2:
	dw	8001h
	dw	0
	dw	result_val	
	dw	cc_range
	db	0

;cntry_pos3 p_pos <201h,0,result_val,noval>     ; optional filespec
cntry_pos3:
	dw	201h
	dw	0
	dw	result_val	
	dw	noval
	db	0	

p_cntry_code:
	dw	0	; local variable
p_code_page:
	dw	0	; local variable

;-----------------------------------------------------------------------

	; 27/10/2022

; files = n

;files_parms p_parms <files_parmsx>
files_parms:
	dw	files_parmsx
	db	1
	db	1
	db	';'

files_parmsx:
	dw	101h,files_pos	; min,max 1 positional
	db	0		; no switches
	db	0		; no keywords

;files_pos p_pos <8000h,0,result_val,files_range,0> ; numeric value
files_pos:
	dw	8000h
	dw	0
	dw	result_val	
	dw	files_range
	db	0

;files_range p_range <,,,8,255>
files_range:
	db	1
	db	1
	db	1
	dd	8
	dd	255

p_files:
	db	0		; local variable

;-----------------------------------------------------------------------

	; 27/10/2022

; fcbs = n,m

;fcbs_parms p_parms <fcbs_parmsx>
fcbs_parms:
	dw	fcbs_parmsx
	db	1
	db	1
	db	';'

fcbs_parmsx:
	dw	201h,fcbs_pos_1,fcbs_pos_2 ; min,max = 2 positional
	db	0		; no switches
	db	0		; no keywords

;fcbs_pos_1 p_pos <8000h,0,result_val,fcbs_range> ; numeric value
fcbs_pos_1:
	dw	8000h
	dw	0
	dw	result_val	
	dw	fcbs_range
	db	0

;fcbs_range p_range <,,,1,255>
fcbs_range:
	db	1
	db	1
	db	1
	dd	1
	dd	255

;fcbs_pos_2 p_pos <8000h,0,result_val,fcbs_keep_range> ; numeric value
fcbs_pos_2:
	dw	8000h
	dw	0
	dw	result_val	
	dw	fcbs_keep_range
	db	0

;fcbs_keep_range p_range <,,,0,255>
fcbs_keep_range:
	db	1
	db	1
	db	1
	dd	0
	dd	255

p_fcbs:	db	0		; local variable
p_keep:	db	0		; local variable

;-----------------------------------------------------------------------

	; 27/10/2022

; lastdrive = x

;ldrv_parms p_parms <ldrv_parmsx>
ldrv_parms:
	dw	ldrv_parmsx
	db	1
	db	1
	db	';'

ldrv_parmsx:
	dw	101h,ldrv_pos	; min,max = 1 positional
	db	0		; no switches
	db	0		; no keywords

;ldrv_pos p_pos	<110h,10h,result_val,noval> ; drive only, ignore colon
ldrv_pos:				    ; remove colon at end
	dw	110h
	dw	10h
	dw	result_val	
	dw	noval
	db	0
	
p_ldrv:	db	0		; local variable

;-----------------------------------------------------------------------

	; 27/10/2022

; stacks = n,m

;stks_parms p_parms <stks_parmsx>
stks_parms:
	dw	stks_parmsx
	db	1
	db	1
	db	';'

stks_parmsx:
	dw	202h,stks_pos_1,stks_pos_2 ; min,max = 2 positionals
	db	0		; no switches
	db	0		; no keywords

;stks_pos_1 p_pos <8000h,0,result_val,stks_range> ; numeric value
stks_pos_1:
	dw	8000h
	dw	0
	dw	result_val	
	dw	stks_range
	db	0

;stks_range p_range <,,,0,64>
stks_range:
	db	1
	db	1
	db	1
	dd	0
	dd	64

;stks_pos_2 p_pos <8000h,0,result_val,stk_size_range> ; numeric value
stks_pos_2:
	dw	8000h
	dw	0
	dw	result_val	
	dw	stk_size_range
	db	0

;stk_size_range p_range <,,,0,512>
stk_size_range:
	db	1
	db	1
	db	1
	dd	0
	dd	512	

p_stack_count:
	dw	0	; local variable
p_stack_size:
	dw	0	; local variable

;-----------------------------------------------------------------------

	; 27/10/2022

; multitrack = [ on | off ]

;mtrk_parms p_parms <mtrk_parmsx>
mtrk_parms:
	dw	mtrk_parmsx
	db	1
	db	1
	db	';'

mtrk_parmsx:
	dw	101h,mtrk_pos	; min,max = 1 positional
	db	0		; no switches
	db	0		; no keywords

;mtrk_pos p_pos <2000h,0,result_val,on_off_string> ; simple string
mtrk_pos:
	dw	2000h
	dw	0
	dw	result_val	
	dw	on_off_string
	db	0

p_mtrk:	db	0		; local variable

;-----------------------------------------------------------------------

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:20B2h)

; switches=/k

;swit_parms p_parms <swit_parmsx>
swit_parms:
	dw	swit_parmsx
	db	1
	db	1
	db	';'

swit_parmsx:
	dw	0		; no positionals
	;db	5               ; # of switches
	; 27/10/2022 - Retro DOS v4.0 (MSDOS 5.0 IO.SYS, SYSINIT)
	db	3
	dw	swit_k_ctrl	; /k control
	;dw	swit_n_ctrl     ; /n control (for MULTI_CONFIG only)
	;dw	swit_f_ctrl     ; /f control (for MULTI_CONFIG only)
	dw	swit_t_ctrl     ; /t control
	dw	swit_w_ctrl     ; /w control
	db	0		; no keywords

;swit_k_ctrl p_pos <0,0,result_val,noval,1> ; switch string follows
swit_k_ctrl:
	dw	0,0,result_val,noval
	db	1
swit_k:	db	'/K',0

; 27/10/2022 - Retro DOS v4.0 (MSDOS 5.0 IO.SYS, SYSINIT)
;
;;swit_n_ctrl p_pos <0,0,result_val,noval,1> ; switch string follows
;swit_n_ctrl:
;	dw	0,0,result_val,noval
;	db	1
;swit_n: db	'/N',0
;
;;swit_f_ctrl p_pos <0,0,result_val,noval,1> ; switch string follows
;swit_f_ctrl:
;	dw	0,0,result_val,noval
;	db	1
;swit_f: db 	'/F',0

	; 27/10/2022

;swit_t_ctrl p_pos <0,0,result_val,noval,1> ; switch string follows	M059
swit_t_ctrl:
	dw	0,0,result_val,noval
	db	1
swit_t:	db	'/T',0			   ;				M059
;swit_w_ctrl p_pos <0,0,result_val,noval,1> ; switch string follows	M063
swit_w_ctrl:
	dw	0,0,result_val,noval
	db	1
swit_w:	db	'/W',0			   ;				M063

;   There doesn't need to be p_swit_n or p_swit_f because /N and /F are
;   acted upon during MULTI_CONFIG processing; we only needed entries
;   in the above table to prevent the parsing code from complaining about them

p_swit_k:	db     0	; local variable
p_swit_t:	db     0	; local variable			M059
p_swit_w:	db     0	; local variable			M063

;-----------------------------------------------------------------------

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:20E8h)

; DOS = [ high | low ]

;dos_parms p_parms  <dos_parmsx>
dos_parms:
	dw	dos_parmsx
	db	1
	db	1
	db	';'
dos_parmsx:
	db	1		; min parameters
	db	2		; max parameters
	dw	dos_pos		; 
	dw	dos_pos		; 
	db	0		; no switches
	db	0		; no keywords

;dos_pos p_pos	<2000h,0,result_val,dos_strings> ; simple string
;        p_pos	<2000h,0,result_val,dos_strings> ; simple string
dos_pos:
	dw	2000h,0,result_val,dos_strings
	db	0
	dw	2000h,0,result_val,dos_strings
	db	0	

dos_strings:	;label	byte
	db	3		; signals that there is a string choice
	db	0		; no range definition
	db	0		; no numeric values choice
	db	4		; 4 strings for choice
	db	1		; the 1st string tag
	dw	hi_string
	db	2		; the 2nd string tag
	dw	lo_string
	db	3
	dw	umb_string
	db	4
	dw	noumb_string

hi_string:	db	"HIGH",0
lo_string:	db	"LOW",0
umb_string:	db	"UMB",0
noumb_string:	db	"NOUMB",0

p_dos_hi:	db	0	; local variable
				; BUGBUG : I dont know whether PARSER uses
				;          this variable or not

; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

%if 0

;****************************************************************** RICHID ****

;include	highvar.inc	; devicehigh variables (used by loadhigh also)

; 30/03/2019 - Retro DOS v4.0
;------------------------------------------------------------------------------

;   Module:   HIGHVAR.INC - Data common to LOADHIGH and DEVICEHIGH, res seg
;
;   Date:     May 14, 1992
;
;******************************************************************************
;
;   Modification log:
;
;     DATE    WHO      DESCRIPTION
;   --------  -------  --------------------------------------------------------
;   05/14/92  t-richj  Original
;   06/21/92  t-richj  Final revisions before check-in
;
;******************************************************************************
;
; There are two primary definitions which need to be made, selectively, before
; this include file should be used.  These are:
;    HV_Extern - If this has been defined, variables for this module will be
;                declared as external.  Otherwise, variables will be declared
;                public, as well as defined, here.  LoadHigh declares HV_Extern
;                in stub.asm and loadhi.asm, and does not declare it in
;                rdata.asm... DeviceHigh does not declare HV_Extern anywhere
;                (as only one module, sysconf.asm, includes this file).
;    HV_LoadHigh - This should be defined when this module is going into
;                  command.com, for LoadHigh.  All of loadhi.asm, stub.asm and
;                  rdata.asm define this, while io.sys' sysconf.asm does not.
;
;******************************************************************************

; To keep track of which UMBs were specified on the DH/LH command lines, and
; to keep track of the minimum sizes given for each, there're two arrays kept
; in { IO.SYS: sysinitseg / COMMAND.COM: DATARES }... each is MAXUMB elements
; big.  16 should be around 14 too many for most users, so there's no expected
; space problem (it's just such a nice round number, eh?).

MAXUMB	equ	16

; Memory elements owned by the system are marked as PSP address 8 in both the
; USA and Japan; Japanese systems also use 9 under more bizzarre conditions.

FreePSPOwner	equ	0	; Free MCBs all have an owner PSP address of 0
SystemPSPOwner	equ	8
;JapanPSPOwner	equ	9

; for LoadHigh and DeviceHigh:
;
;	fInHigh - Is set to 1 during HideUMBs(), and back to zero in
;	          UnHideUMBs().
;	fUmbTiny - Is set to 1 iff the user has specified /S on the command-
;	           line.
;	SegLoad - Segment address for first UMB specified; set automatically.
;	UmbLoad - The load UMB number; for example, this is 3 if the user has
;	          given a command-line like "/L:3,500;4"
;	UmbUsed - An array of characters, each of which is 1 iff the UMB
;	          matching its index number was specified on the command-line;
;	          for example, after "/L:3,500;4;7", UmbUsed[3], [4] and [7]
;	          will be set to 1.  All others will be set to 0.
;	UmbSize - An array of words, each of which is interpereted as a size
;	          specified by the user for a UMB (in the above example, all
;	          elements would be zero save UmbSize[3], which would be 500.
;	fm_umb - Set to the old UMB link-state (0x80 or 0x00)
;	fm_strat - Set to the old memory-allocation strategy (0$00000???)
;	fm_argc  - Number of arguments received by ParseVar() (see ParseVar()
;	           for details).

fInHigh:  db	0
fUmbTiny: db	0
SegLoad:  dw	0
UmbLoad:  db	0
UmbUsed:  times MAXUMB db 0 ; times 16 db 0  ; db 16 dup(?)
UmbSize:  times MAXUMB dw 0 ; times 16 dw 0  ; dw 16 dup(?)
fm_umb:   db	0
fm_strat: db	0
fm_argc:  db	0	

; UmbLoad is set to UNSPECIFED, below, until /L:umb is read; at which point
; UmbLoad is set to the UMB number given.

UNSPECIFIED	equ	-1

%endif

;****************************************************************** RICHID ****

; 30/03/2019 - Retro DOS v4.0 (MSDOS 6.0, SYSCONF.ASM)
; ((MSDOS 6.21 IO.SYS -> SYNINIT:22BAh))

; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; (SYSINIT:212Bh)	

		;public	DevEntry

DevSize:	dw	0	; size of the device driver being loaded(paras)
DevLoadAddr:	dw	0	; Mem addr where the device driver is 2 b loaded
DevLoadEnd:	dw	0	; MaxAddr to which device can be loaded
DevEntry:	dd	0	; Entry point to the device driver
DevBrkAddr:	dd	0	; Break address of the device driver
; 27/10/2022 
;ConvLoad:	db	0	; Use conventional (dos 5 -style) InitDevLoad?
;
DevUMB:		db	0	; byte indicating whether to load DDs in UMBs
DevUMBAddr:	dw	0	; cuurent UMB used fro loading devices (paras)
DevUMBSize:	dw	0	; Size of the current UMB being used   (paras)
DevUMBFree:	dw	0	; Start of free are in the current UMB (paras)
;
DevXMSAddr:	dd	0
;
DevExecAddr:	dw	0	; Device load address parameter to Exec call
DevExecReloc:	dw	0	; Device load relocation factor
;
DeviceHi:	db	0	; Flag indicating whther the current device
				;  is being loaded into UMB
DevSizeOption:	dw	0	; SIZE= option
;
Int12Lied:	db	0	; did we trap int 12 ?
OldInt12Mem:	dw	0	; value in 40:13h (int 12 ram)
ThreeComName:	db	'PROTMAN$'	; 3Com Device name
;
FirstUMBLinked:	db	0
DevDOSData:	dw	0	; segment of DOS Data
DevCmdLine:	dd	0	; Current Command line
DevSavedDelim:	db	0	; The delimiter which was replaced with null
				; to use the file name in the command line
;
;	ifdef	dblspace_hooks
;MagicHomeFlag:	db	0	; set non-zero when MagicDrv is final placed
;	endif

; ===========================================================================

; 31/03/2019 - Retro DOS v4.0

; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; (SYSINIT:215Eh)

;----------------------------------------------------------------------------
;
; procedure : doconf
;
;             Config file is parsed initially with this routine. For the
;             Subsequent passes 'multi_pass' entry is used .
;
;----------------------------------------------------------------------------

	; 27/10/2022
doconf:
	push	cs
	pop	ds

	mov	ax,3700h
        ;mov	ax,(CHAR_OPER<<8)	; get switch character
	int	21h
	mov	[command_line+1],dl	; set in default command line

; 27/10/2022
;;ifdef	MULTI_CONFIG
;	;mov	[command_line-1],dl     ; save default switchchar
;	mov	[def_swchr],dl ; 31/03/2019 
;;endif	;MULTI_CONFIG

	mov	dx,config ;'\CONFIG.SYS' ;now pointing to file description
	mov	ax,3D00h
	;mov	ax,OPEN<<8		;open file "config.sys"
	stc				;in case of int 24
	int	21h			;function request
	jnc	short noprob		; brif opened okay

; 27/10/2022
;;ifdef	MULTI_CONFIG
;      	call	kbd_read		; we still want to give the guy
;					; a chance to select clean boot!
;;endif					; (ie, no autoexec.bat processing)
	mov	byte [multi_pass_id],11	; set it to unreasonable number
	retn
noprob: 				;get file size (note < 64k!!)
	mov	bx,ax  ; File handle
	xor	cx,cx
	xor	dx,dx
	;mov	ax,4202h
	mov	ax,(LSEEK<<8)|2
	int	21h
	mov	[count],ax

	xor	dx,dx
	;mov	ax,4200h
	mov	ax,LSEEK<<8		;reset pointer to beginning of file
	int	21h

	;mov	dx,[ALLOCLIM]		;use current alloclim value
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	dx,[top_of_cdss] 

	mov	ax,[count]
	mov	[config_size],ax	;save the size of config.sys file.
	call	ParaRound
	sub	dx,ax

; 27/10/2022
;;ifdef	MULTI_CONFIG
;;
;;  The size of the CONFIG.SYS workspace (for recreating the in-memory
;;  CONFIG.SYS image, and later for building the initial environment) need
;;  not be any larger than CONFIG.SYS itself, EXCEPT for the fact that
;;  we (may) add a variable to the environment that does not explicity appear
;;  in CONFIG.SYS, and that variable is CONFIG (as in CONFIG=COMMON).
;;  The default setting for CONFIG cannot result in more than 1 paragraph
;;  of extra space, so here we account for it (the worst case of course is
;;  when CONFIG.SYS is some very small size, like 0 -JTP)
;;
;       dec     dx                      ;reserve 1 additional paragraph
;       mov     [config_wrkseg],dx      ;this is the segment to be used for
;       sub     dx,ax                   ;rebuilding the config.sys memory image
;;endif	;MULTI_CONFIG

	sub	dx,11h			;room for header
	
	;mov	[ALLOCLIM],dx		; config starts here. new alloclim value.
	;mov	[CONFBOT],dx
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	[top_of_cdss],dx
	call    TempCDS 
	mov	dx,[cs:top_of_cdss]
 
	mov	ds,dx
	mov	es,dx

	xor	dx,dx
	mov	cx,[cs:count]
	mov	ah,3Fh
	;mov	ah,READ  ; 3Fh
	stc				;in case of int 24
	int	21h			;function request
	pushf

; find the eof mark in the file. if present,then trim length.

	push	ax
	push	di
	push	cx
	mov	al,1Ah			; eof mark
	mov	di,dx			; point to buffer
	jcxz	puteol			; no chars
	repnz	scasb			; find end
	jnz	short puteol		; none found and count exhausted

; we found a 1a. back up

	dec	di			; backup past 1Ah

;  just for the halibut,stick in an extra eol

puteol:
	mov	al,cr ; 0Dh
	stosb
	mov	al,lf  ;0Ah
	stosb
	sub	di,dx			; difference moved
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	[cs:count],di		; new count
	
	; 31/03/2019 - Retro DOS v4.0
	;push	cs
	;pop	ds

	;mov	[count],di		; new count

	pop	cx
	pop	di
	pop	ax

	; 27/10/2022
	push	cs
	pop	ds

	push	ax
	;mov	ah,CLOSE
	mov	ah,3Eh
	int	21h
	pop	ax
	popf
	jc	short conferr 		;if not we've got a problem
	cmp	cx,ax
	jz	short getcom		;couldn't read the file

conferr:
	mov	dx,config		;want to print config error
	call	badfil
endconv:
	retn

;----------------------------------------------------------------------------
;
; entry : multi_pass
;
;             called to execute device=,install= commands
;
;----------------------------------------------------------------------------

	; 27/10/2022
multi_pass:
	push	cs
	pop	ds

	cmp	byte [multi_pass_id],10
;jae_endconv:
	jae	short endconv 		; do nothing. just return.

	;push	word [CONFBOT]
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	push	word [top_of_cdss]
	pop	es			; es -> confbot

	mov	si,[org_count]
	mov	[count],si		; set count
	xor	si,si ; 0
        mov     [chrptr],si		; reset chrptr
        mov     [linecount],si		; reset linecount

	call	getchr
	jmp	short conflp

getcom:
        call    organize                ; organize the file
	call	getchr

conflp: jc	short endconv

        inc     word [linecount]	; increase linecount

	mov	byte [multdeviceflag],0	; reset multdeviceflag.
	mov	byte [setdevmarkflag],0	; reset setdevmarkflag.
	cmp	al,lf			; linefeed?
	je	short blank_line	;  then ignore this line.

; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
%if 0

;ifdef	MULTI_CONFIG

;   If this is a genuine CONFIG.SYS command, then there should be a line
;   number immediately following it....

        mov     [config_cmd],al         ; save original command code
	;and	al,NOT CONFIG_OPTION_QUERY
	and	al,~CONFIG_OPTION_QUERY ; and al,7Fh
        cmp     byte [config_multi],0	; is this a multi-config config.sys?
        je      short not_final		; no, line number is not embedded

        push    ax                      ;
        call    getchr                  ; ignore end-of-image errors,
        mov     ah,al                   ; because if there's an error
        call    getchr                  ; fetching the line number that's
        xchg    al,ah                   ; supposed to be there, the next
        mov     [linecount],ax          ; getchr call will get the same error
        pop     ax
;
;   HACK: when 4DOS.COM is the shell and it doesn't have an environment from
;   which to obtain its original program name, it grovels through all of
;   memory to find the filename that was used to exec it; it wants to find
;   the SHELL= line in the in-memory copy of CONFIG.SYS, and it knows that
;   sysinit converts the SHELL= keyword to an 'S', so it expects to find an 'S'
;   immediately before the filename, but since we are now storing line # info
;   in the config.sys memory image, 4DOS fails to find the 'S' in the right
;   spot.
;
;   So, on the final pass of CONFIG.SYS, copy the command code (eg, 'S')
;   over the line number info, since we no longer need that info anyway. This
;   relies on the fact that getchr leaves ES:SI pointing to the last byte
;   retrieved.
;
        cmp     byte [multi_pass_id],2	; final pass?
        jb	short not_final		; no
       ;test    word [install_flag],have_install_cmd
	test    byte [install_flag],have_install_cmd ; 1
        jz	short final		; no install cmds, so yes it is
        cmp     byte [multi_pass_id],3	; final pass?
        jb	short not_final		; no
final:                                  ;
	mov	[es:si],al		; save backward-compatible command code
not_final:                              ;
;endif

%endif

	mov	ah,al
	call	getchr
	jnc	short tryi

	cmp	byte [multi_pass_id],2
	;jae	short jae_endconv	; do not show badop again for multi_pass.
	; 27/10/2022
	jnb	short endconv	
	jmp	badop

coff:	push	cs
	pop	ds
	call	newline
	jmp	short conflp	; 13/05/2019

blank_line:
	call	getchr
	jmp	short conflp

	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
coff_p:
	push	cs
	pop	ds

;to handle install= commands,we are going to use multi-pass.
;the first pass handles the other commands and only set install_flag when
;it finds any install command. the second pass will only handle the
;install= command.

;------------------------------------------------------------------------------
;install command
;------------------------------------------------------------------------------

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2250h)

tryi:
	cmp	byte [multi_pass_id],0	; the initial pass for DOS=HI
	jne	short not_init_pass
	jmp	multi_try_doshi
not_init_pass:

	cmp	byte [multi_pass_id],2	; the second pass was for ifs=
        ;je	short multi_pass_coff2	; now it is NOPs
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	je	short multi_pass_coff	
					; This pass can be made use of if
					; we want do some config.sys process
					; after device drivers are loaded
					; and before install= commands
					; are processed

	cmp	byte [multi_pass_id],3	; the third pass for install= ?
	je	short multi_try_i
        cmp     ah, CONFIG_DOS  ; 'H'
	;je	short multi_pass_coff2
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	je	short multi_pass_coff	

;       make note of any INSTALL= or INSTALLHIGH= commands we find,
;       but don't process them now.        
        
        cmp     ah,CONFIG_INSTALL ; 'I'	; install= command?
	;jne	short precheck_installhigh ; the first pass is for normal operation.
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jne	short tryb	
	
	;or	word [install_flag],have_install_cmd ; set the flag
	or	byte [install_flag],have_install_cmd ; 1
multi_pass_coff2:
	jmp	short coff ; 13/05/2019	; and handles the next command

	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;precheck_installhigh:
;       cmp     ah,CONFIG_INSTALLHIGH ; 'W' ; signifier for INSTALLHIGH
;       jne     short tryb		; carry on with normal processing
;	;or	word [install_flag],have_install_cmd
;	or	byte [install_flag],have_install_cmd ; 1
;       jmp	short coff

multi_try_i:
        cmp     ah, CONFIG_INSTALL ; 'I' ; install= command?
	;jne	short multi_try_n	; no, check for installhigh
	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jne	short multi_pass_filter

; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc	short multi_pass_filter	; has the CONFIG_OPTION_QUERY bit set
;;endif

	call	do_install_exec 	;install it.
	jmp	short coff		;to handle next install= command.

; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
%if 0

multi_try_n:
        cmp     ah,CONFIG_INSTALLHIGH   ; installhigh= command?
        jne	short multi_pass_filter	; no. ignore this.
;ifdef	MULTI_CONFIG
        call    query_user              ; query the user if config_cmd
        jc      short multi_pass_filter	; has the CONFIG_OPTION_QUERY bit set
;endif

;       The memory environment is in its normal DOS state, so do
;       the standard calls to set the alloc strategy for loading high

	mov	ax,(ALLOCOPER<<8)|0 ; 5800h
	int	21h			;get alloc strategy
	mov	bx,ax
        push    bx                      ; save for the return

        or	bx,HIGH_FIRST  ; 80h	;set alloc to HighFirst
	mov	ax,(ALLOCOPER<<8)|1 ; 5801h
	int	21h			;set alloc strategy

	mov     ax,(ALLOCOPER<<8)|2 ; 5802h
        int     21h                     ; get link state
        xor     ah,ah                   ; clear top byte
        push    ax                      ; save for return

        mov	ax,(ALLOCOPER<<8)|3 ; 5803h
	mov	bx,1
	int	21h			;link in UMBs

	call	do_install_exec 	;install it.

        mov     ax,(ALLOCOPER<<8)|3
        pop     bx                      ; recover original link state
        int     21h
        pop     bx                      ; recover original alloc strategy
        mov     ax,(ALLOCOPER<<8)|1
        int     21h

	jmp	coff			;to handle next install= command.

%endif

multi_pass_filter:
        cmp     ah,CONFIG_COMMENT ; 'Y' ; comment?
	je	short multi_pass_adjust
        cmp     ah,CONFIG_UNKNOWN ; 'Z' ; bad command?
	je	short multi_pass_adjust
        cmp     ah,CONFIG_REM  ; '0'   ; rem?
	jne	short multi_pass_coff 	; ignore the rest of the commands.

multi_pass_adjust:			; these commands need to
	dec	word [chrptr]		;  adjust chrptr,count
	inc	word [count]		;  for newline proc.

multi_pass_coff:
	jmp	coff			; to handle next install= commands.

;------------------------------------------------------------------------------
; buffer command
;------------------------------------------------------------------------------

;******************************************************************************
;									      *
; function: parse the parameters of buffers= command.			      *
;									      *
; input :								      *
;	es:si -> parameters in command line.				      *
; output:								      *
;	buffers set							      *
;	buffer_slash_x	flag set if /x option chosen.			      *
;	h_buffers set if secondary buffer cache specified.		      *
;									      *
; subroutines to be called:						      *
;	sysinit_parse							      *
; logic:								      *
; {									      *
;	set di points to buf_parms;  /*parse control definition*/	      *
;	set dx,cx to 0; 						      *
;	reset buffer_slash_x;						      *
;	while (end of command line)					      *
;	{ sysinit_parse;						      *
;	  if (no error) then						      *
;	       if (result_val._$P_synonym_ptr == slash_e) then /*not a switch *
;		    buffer_slash_x = 1					      *
;	       else if	 (cx == 1) then 	    /* first positional */    *
;			  buffers = result_val._$P_picked_val;		      *
;		    else  h_buffers = result_val._$P_picked_val; 	      *
;	  else	{show error message;error exit} 			      *
;	};								      *
;	if (buffer_slash_x is off & buffers > 99) then show_error;	      *
; };									      *
;									      *
;******************************************************************************

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:229Ch)

tryb:
        cmp     ah,CONFIG_BUFFERS ; 'B'
	jne	short tryc


; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc      short tryc		; has the CONFIG_OPTION_QUERY bit set
;;endif

	; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	byte [p_buffer_slash_x],0 ; 31/03/2019

	mov	di,buf_parms
	xor	cx,cx
	mov	dx,cx
do7:
	call	sysinit_parse
	jnc	short if7		; parse error,
	call	badparm_p		;   and show messages and end the search loop.
	jmp	short sr7
if7:
	cmp	ax,_$P_RC_EOL ; 0FFFFh	; end of line?
	je	short en7		;  then jmp to $endloop for semantic check
	;cmp	word [result_val_swoff],switch_x
	cmp	word [result_val+_$P_Result_Blk.SYNONYM_Ptr],switch_x
	jne	short if11

;	mov	byte [p_buffer_slash_x],1 ; set the flag M016
	jmp	short en11
if11:
	;mov	ax,[rv_dword]
	mov	ax,[result_val+_$P_Result_Blk.Picked_Val]
	cmp	cx,1
	jnz	short if13

	mov	[p_buffers],ax
	jmp	short en11
if13:
	mov	[p_h_buffers],ax
en11:
	jmp	short do7
en7:
	cmp	word [p_buffers],99
	jbe	short if18

;	cmp	byte [p_buffer_slash_x],0 ; M016
;	jne	short if18

	call	badparm_p
	mov	word [p_h_buffers],0
	jmp	short sr7
if18:
	mov	ax,[p_buffers]	; we don't have any problem.
	mov	[buffers],ax	; now,let's set it really.

	mov	ax,[p_h_buffers]
	mov	[h_buffers],ax

;	mov	al,[p_buffer_slash_x]	; M016
;	mov	[buffer_slash_x],al

	mov	ax,[linecount]
	mov	[buffer_linenum],ax ; save the line number for the future use.
sr7:
	jmp	coff

;------------------------------------------------------------------------------
; break command
;------------------------------------------------------------------------------

;****************************************************************************
;									    *
; function: parse the parameters of break = command.			    *
;									    *
; input :								    *
;	es:si -> parameters in command line.				    *
; output:								    *
;	turn the control-c check on or off.				    *
;									    *
; subroutines to be called:						    *
;	sysinit_parse							    *
; logic:								    *
; {									    *
;	set di to brk_parms;						    *
;	set dx,cx to 0; 						    *
;	while (end of command line)					    *
;	{ sysinit_parse;						    *
;	  if (no error) then						    *
;	       if (result_val._$P_item_tag == 1) then	  /*on		 */ *
;		   set p_ctrl_break,on;					    *
;	       else					  /*off 	 */ *
;		   set p_ctrl_break,off;				    *
;	  else {show message;error_exit};				    *
;	};								    *
;	if (no error) then						    *
;	   dos function call to set ctrl_break check according to	    *
; };									    *
;									    *
;****************************************************************************

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:22FFh)

tryc:
        cmp     ah,CONFIG_BREAK ; 'C'
	jne	short trym

; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc	short trym		; has the CONFIG_OPTION_QUERY bit set
;;endif
	mov	di,brk_parms
	xor	cx,cx
	mov	dx,cx
do22:
	call	sysinit_parse
	jnc	short if22		; parse error
	call	badparm_p		;  show message and end the search loop.
	jmp	short sr22
if22:
	cmp	ax,_$P_RC_EOL		; end of line?
	je	short en22		; then end the $endloop

	;cmp	byte [result_val_itag],1
	cmp	byte [result_val+_$P_Result_Blk.Item_Tag],1
	jne	short if26

	mov	byte [p_ctrl_break],1	; turn it on
	jmp	short en26
if26:
	mov	byte [p_ctrl_break],0	; turn it off
en26:
	jmp	short do22		; we actually set the ctrl break
en22:
	mov	ah,SET_CTRL_C_TRAPPING ; if we don't have any parse error.
	mov	al,1
	mov	dl,[p_ctrl_break]
	int	21h
sr22:
	jmp	coff

;------------------------------------------------------------------------------
; multitrack command
;------------------------------------------------------------------------------

;******************************************************************************
;									      *
; function: parse the parameters of multitrack= command.		      *
;									      *
; input :								      *
;	es:si -> parameters in command line.				      *
; output:								      *
;	turn multrk_flag on or off.					      *
;									      *
; subroutines to be called:						      *
;	sysinit_parse							      *
; logic:								      *
; {									      *
;	set di to brk_parms;						      *
;	set dx,cx to 0; 						      *
;	while (end of command line)					      *
;	{ sysinit_parse;						      *
;	  if (no error) then						      *
;	       if (result_val._$P_item_tag == 1) then	  /*on		 */   *
;		   set p_mtrk,on;					      *
;	       else					  /*off 	 */   *
;		   set p_mtrk,off;					      *
;	  else {show message;error_exit};				      *
;	};								      *
;	if (no error) then						      *
;	   dos function call to set multrk_flag according to p_mtrk.	      *
;									      *
; };									      *
;									      *
;******************************************************************************

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

trym:
        cmp     ah,CONFIG_MULTITRACK  ; 'M'
	jne	short tryu

; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user      ; query the user if config_cmd
;       jc      short tryu	; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	di,mtrk_parms
	xor	cx,cx
	mov	dx,cx
do31:
	call	sysinit_parse
	jnc	short if31	; parse error
	call	badparm_p	;  show message and end the search loop.
	jmp	short sr31
if31:
	cmp	ax,_$P_RC_EOL	; end of line?
	je	short en31	; then end the $endloop

	;cmp	byte [result_val_itag],1
	cmp	byte [result_val+_$P_Result_Blk.Item_Tag],1
	jne	short if35

	mov	byte [p_mtrk],1	; turn it on temporarily.
	jmp	short en35
if35:
	mov	byte [p_mtrk],0	; turn it off temporarily.
en35:
	jmp	short do31	; we actually set the multrk_flag here
en31:
	push	ds
	;;mov	ax,Bios_Data ; 70h
	;mov	ax,KERNEL_SEGMENT ; 70h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax

	cmp	byte [cs:p_mtrk],0
	jne	short if39

	mov	word [multrk_flag],multrk_off2	; 0001h
	jmp	short en39
if39:
	mov	word [multrk_flag],multrk_on	; 0080h
en39:
	pop	ds
sr31:
	jmp	coff

;----------------------------------------------------------------------------
; DOS=HIGH/LOW command
;----------------------------------------------------------------------------

	; 27/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)

multi_try_doshi:
        cmp     ah,CONFIG_DOS ; 'H'
	je	short it_is_h
skip_it:
	jmp	multi_pass_filter
it_is_h:				; M003 - removed initing DevUMB
					;	 & runhigh
; 27/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc      short skip_it		; has the CONFIG_OPTION_QUERY bit set
;;endif
	mov	di,dos_parms
	xor	cx,cx
	mov	dx,cx
h_do_parse:
	call	sysinit_parse
	jnc	short h_parse_ok	; parse error
h_badparm:
	call	badparm_p		; show message and end the search loop.
	jmp	short h_end
h_parse_ok:
	cmp	ax,_$P_RC_EOL		; end of line?
	je	short h_end		; then end the $endloop
	call	ProcDOS
	jmp	short h_do_parse
h_end:
	jmp	coff

;-----------------------------------------------------------------------------
; devicehigh command
;-----------------------------------------------------------------------------

	; 28/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	
tryu:
        cmp     ah,CONFIG_DEVICEHIGH ; 'U'
	jne	short tryd

; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc	short tryd		; has the CONFIG_OPTION_QUERY bit set
;;endif

; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;	call	InitVar
;	call	ParseSize		; process the size= option
;	;jnc	short tryu_0
	
	;jc	short tryu_1 ; 31/03/2019 - Retro DOS v4.0

	; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	[cs:badparm_off], si	; stash it there in case of an error
	mov	[cs:badparm_seg], es
	call	ParseSize
	jnc	short tryu_2	; 28/10/2022
	call	badparm_p
	jmp	coff

; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;tryu_0:
;	mov	ax,[cs:DevSizeOption]
;	or	ax,ax
;	jnz	short tryu_2
;
;	call	ParseVar
;	jnc	short tryu_2
;tryu_1:
;	mov	[cs:badparm_off], si	; If ParseVar up there failed, then
;	mov	[cs:badparm_seg], es	; ES:SI points to its problem area...
;	call	badparm_p		; so all we have to do is choke and
;	jmp	coff			; die, rather verbosely.

tryu_2:	
	push	si
	push	es
tryu_3:
	mov	al,[es:si]
	cmp	al,cr
	je	short tryu_4
	cmp	al,lf
	je	short tryu_4
	call	delim
	jz	short tryu_4
	inc	si
	jmp	short tryu_3
tryu_4:	
	mov	[cs:DevSavedDelim],al	; Save the delimiter before replacing
					;  it with null
	mov	byte [es:si],0
	pop	es
	pop	si

;------------------------------------------------------------------------------
; BEGIN PATCH TO CHECK FOR NON-EXISTANT UMBs   -- t-richj 7-21-92
;------------------------------------------------------------------------------

; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;	call	UmbTest			; See if UMBs are around...
;	jnc	short NrmTst		; ...yep. So do that normal thang.
;
;	mov	byte [cs:DeviceHi], 0	; ...nope... so load low.
;	jmp	short LoadDevice

;------------------------------------------------------------------------------
; END PATCH TO CHECK FOR NON-EXISTANT UMBs   -- t-richj 7-21-92
;------------------------------------------------------------------------------

NrmTst:	mov	byte [cs:DeviceHi],0
	cmp	byte [cs:DevUMB],0	; do we support UMBs
	je	short LoadDevice	; no, we don't
	mov	byte [cs:DeviceHi],1
	jmp	short LoadDevice

;------------------------------------------------------------------------------
; device command
;------------------------------------------------------------------------------

	; 28/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2401h)

tryd:
        cmp     ah,CONFIG_DEVICE ;  'D'
	je	short gotd
skip_it2:
	jmp	tryq
gotd:

; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc	short skip_it2		; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	byte [cs:DeviceHi],0	; not to be loaded in UMB ;M007
	mov	word [cs:DevSizeOption],0
	mov	byte [cs:DevSavedDelim],' ' ; In case of DEVICE= the null has to
					;  be replaced with a ' '
LoadDevice:                             ; device= or devicehigh= command.
	;28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)        
	;
	;push    cs
        ;pop     ds
	;
	;mov	[bpb_addr],si		; pass the command line to the dvice
	;mov	[bpb_addr+2],es
	;
	;mov	[DevCmdLine],si		; save it for ourself
	;mov	[DevCmdLine+2],es
	;
	;mov	byte [driver_units],0	; clear total block units for driver	

	mov	bx,cs
	mov	ds,bx

	mov	[cs:bpb_addr],si	; pass the command line to the dvice
	mov	[cs:bpb_addr+2],es
	
	mov	[cs:DevCmdLine],si	; save it for ourself
	mov	[cs:DevCmdLine+2],es	

	call	round
	
	call	SizeDevice
	jc	short BadFile

; - Begin DeviceHigh primary logic changes ------------------------------------

; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;	mov	byte [ConvLoad],1	; Doesn't matter if DeviceHi==0
;
;	mov	al,[DeviceHi]		; If not using upper memory,
;	or	byte [DeviceHi],0	; Skip all this and go on to
;	jz	short DevConvLoad	; the actual load.
;
;	;call	GetLoadUMB		; Returns first UMB spec'ed in AX
;	mov	al,[UmbLoad]	; 19/04/2019 - Retro DOS v4.0
;
;	cmp	al,-1			; If umb0 not specified, it's old style
;	jz	short DevConvLoad	; so load high even if SIZE= is smaller
;
;	dec	byte [ConvLoad] ; 0 	; They specified /L, so use new loader
;
;	call	GetLoadSize		; Returns size of first UMB specified
;	or	ax,ax
;	jz	short tryd_1		; If size1 not specified, nada to do:
;
;	cmp	ax,[DevSize]		; /L:...,Size < DevSize?
;	jge	short DevConvLoad
;
;tryd_1: mov	ax,[DevSize]		; Size < DevSize, so write DevSize as
;	call	StoLoadSize		; minsize for load UMB.

; - End DeviceHigh primary logic changes --------------------------------------

DevConvLoad:
	; 28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	call	InitDevLoad

	;mov	ax,[DevLoadAddr]
	;add	ax,[DevSize]
	;jc	short NoMem
	;cmp	[DevLoadEnd],ax
	;jae	short LoadDev

	mov	ax,[cs:DevLoadAddr]
	add	ax,[cs:DevSize]
	jc	short NoMem
	cmp	[cs:DevLoadEnd],ax
	jae	short LoadDev

NoMem:
	jmp	mem_err

BadFile:
	;28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;call	RetFromUM		; Does nothing if didn't call HideUMBs
	;cmp    byte [es:si],' '
        ;jae	short tryd_2
	cmp	byte [es:si],0Dh	; cr
        jne	short tryd_2
	jmp	badop
tryd_2:
	call	badload
	jmp	coff

LoadDev:
	push	es
	pop	ds

	mov	dx,si			;ds:dx points to file name
	call	ExecDev			; load device driver using exec call
badldreset:
	push	ds
	pop	es			;es:si back to config.sys
	push	cs
	pop	ds			;ds back to sysinit
	jc	short BadFile
goodld:
	push	es
	push	si
	call	RemoveNull
	push	es
	push	si

	push	cs
	pop	es

	push	ds
	push	si

	lds	si,[cs:DevEntry]	; peeks the header attribute
	
	;test	word [si+4],8000h
	test	word [si+SYSDEV.ATT],DEVTYP ;block device driver?
	jnz	short got_device_com_cont   ;no.

	lds	si,[cs:DOSINFO]		; ds:si -> sys_var
	;cmp	byte [si+32],26
	cmp	byte [si+SYSI_NUMIO],26	; no more than 26 drive number
	jb	short got_device_com_cont

	pop	si
	pop	ds

	pop	si			;clear the stack
	pop	es

	;28/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;call	RetFromUM		; Do this before we leave

	jmp	short badnumblock

got_device_com_cont:
	pop	si
	pop	ds

	call	LieInt12Mem
	call	UpdatePDB		; update the PSP:2 value M020

	cmp	byte [cs:multdeviceflag],0 ; Pass limit only for the 1st device
					;  driver in the file ; M027
	jne	short skip_pass_limit	;		      ; M027

	mov	word [cs:break_addr],0	; pass the limit to the DD
	mov	bx,[cs:DevLoadEnd]
	mov	word [cs:break_addr+2],bx

skip_pass_limit:
;	Note: sysi_numio (in DOS DATA) currently reflects the REAL
;	number of installed devices (including DblSpace drives) where
;	"drivenumber" is the number that the next block device will
;	be assigned to. Because some naughty device drivers (like
;	interlnk) look at the internal DOS variable instead of the
;	value we pass it, we'll temporarily stick our value into
;	DOS DATA while we're initializing the device drivers.
;
;	Note that this will make it impossible for this device
;	driver to access the DblSpace drive letters, whether
;	they are swapped-hosts or unswapped compressed drives,
;	during its initialization phase.

	; 29/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;push	ds
	;lds	bx,[cs:DOSINFO]		; ds:bx -> sys_var
	;
	;mov	al,[cs:drivenumber]	; temporarily use this next drv value
	;mov	[cs:devdrivenum],al	; pass drive number in packet to driver
	;mov	ah,al
	;
	;xchg	ax,[bx+SYSI_NUMIO]	; swap with existing values
	;pop	ds
	;
	;push	ax			; save real sysi_numio/ncds in ax

	; 29/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:24B9h)

	mov	bx,SYSDEV.STRAT ; 6
	call	calldev 		;   calldev (sdevstrat);
	mov	bx,SYSDEV.INT ; 8
	call	calldev 		;   calldev (sdevint);

	; 29/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;pop	ax			; get real sysi_numio value
	;push	ds
	;lds	bx,[cs:DOSINFO]		; ds:bx -> sys_var
	;mov	[bx+SYSI_NUMIO],ax	; swap with existing values
	;pop	ds

	call	TrueInt12Mem

	mov	ax,[cs:break_addr]	; move break addr from the req packet
	mov	[cs:DevBrkAddr],ax
	mov	ax,[cs:break_addr+2]
	mov	[cs:DevBrkAddr+2],ax

	; 29/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;call	RetFromUM		; There we go... all done.

	cmp	byte [cs:DevUMB],0
	je	short tryd_3
	call	AllocUMB
tryd_3:

;ifndef ROMDOS
;------ If we are waiting to be moved into hma lets try it now !!!

	cmp	byte [cs:runhigh],0FFh
	jne	short tryd_4

	call	TryToMovDOSHi		; move DOS into HMA if reqd
tryd_4:
;endif ; ROMDOS

	pop	si
	pop	ds
	mov	byte [si],0		; *p = 0;

	push	cs
	pop	ds

	jmp	short was_device_com

;----------------------------------------------------------------------------

;02/04/2019 - Retro DOS v4.0

badnumblock:
	push	cs
	pop	ds
	mov	dx,badblock
	call	print

;------ fall thru -----------------------------------------------------------

erase_dev_do:				; modified to show message "error in config.sys..."
	
	;call	CheckDoubleSpace ; MSDOS 6.21 IO.SYS SYSINIT:27BBh
	
	pop	si
	pop	es

	push	cs
	pop	ds

skip1_resetmemhi:
	cmp	word [cs:configmsgflag],0
	je	short no_error_line_msg

	call	error_line		; no "error in config.sys" msg for device driver. dcr d493
	mov	word [cs:configmsgflag],0 ;set the default value again.

no_error_line_msg:
	jmp	coff

;----------------------------------------------------------------------------

was_device_com:
	mov	ax,[cs:DevBrkAddr+2] 	; 13/05/2019
	cmp	ax,[cs:DevLoadEnd]
	jbe	short breakok

	pop	si
	pop	es
	jmp	BadFile

breakok:
	lds	dx,[cs:DevEntry]	;set ds:dx to header
	mov	si,dx

	les	di,[cs:DOSINFO] 	;es:di point to dos info
	;mov	ax,[si+4]
	mov	ax,[si+SYSDEV.ATT]	;get attributes
	test	ax,DEVTYP ; 8000h	;test if block dev
	jz	short isblock

;------ lets deal with character devices

	or	byte [cs:setdevmarkflag],for_devmark ; 2
	call	DevSetBreak		; go ahead and alloc mem for device
jc_edd:
	jc	short erase_dev_do	;device driver's init routine failed.

	test	ax,ISCIN ; 1		;is it a console in?
	jz	short tryclk

	mov	[es:di+SYSI_CON],dx   ; es:di+12
	mov	[es:di+SYSI_CON+2],ds ; es:di+14
tryclk: 
	test	ax,ISCLOCK ; 8		;is it a clock device?
	jz	short golink

	mov	[es:di+SYSI_CLOCK],dx	; es:di+8
	mov	[es:di+SYSI_CLOCK+2],ds ; es:di+10
golink: 
	jmp	linkit

;------ deal with block device drivers

isblock:
	mov	al,[cs:unitcount]	;if no units found,erase the device
	or	al,al
	jz	short erase_dev_do
	;mov	[si+10],al
	mov	[si+SYSDEV.NAME],al	; number of units in name field
	; 29/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;add	[cs:driver_units],al	; keep total for all drivers in file
perdrv:
	cbw				; warning no device > 127 units
	mov	cx,ax
	mov	dh,ah
	;mov	dl,[es:di+32]
	mov	dl,[es:di+SYSI_NUMIO]	;get number of devices
	mov	ah,dl
	add	ah,al			; check for too many devices
	cmp	ah,26			; 'A' - 'Z' is 26 devices
	jbe	short ok_block
	jmp	badnumblock

ok_block:
	or	byte [cs:setdevmarkflag],for_devmark ; 2
	call	DevSetBreak		; alloc the device
	jc	short jc_edd
	add	[es:di+SYSI_NUMIO],al	; update the amount

	add	[cs:drivenumber],al	; remember amount for next device
	lds	bx,[cs:bpb_addr]	; point to bpb array
perunit:
	les	bp,[cs:DOSINFO]
	;les	bp,[es:bp+SYSI_DPB]	; get first dpb
	;les	bp,[es:bp]
	; 29/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	les	bp,[es:bp+0]		; [es:bp+SYSI_DPB]
scandpb:
	;cmp	word [es:bp+25],-1
	cmp	word [es:bp+DPB.NEXT_DPB],-1
	je	short foundpb
	;les	bp,[es:bp+25]
	les	bp,[es:bp+DPB.NEXT_DPB]
	jmp	short scandpb
foundpb:
	mov	ax,[cs:DevBrkAddr]
	mov	[es:bp+DPB.NEXT_DPB],ax
	mov	ax,[cs:DevBrkAddr+2]
	mov	[es:bp+DPB.NEXT_DPB+2],ax

	les	bp,[cs:DevBrkAddr]
	add	word [cs:DevBrkAddr],DPBSIZ ; 33

	call	RoundBreakAddr

	mov	word [es:bp+DPB.NEXT_DPB],-1
	mov	byte [es:bp+DPB.FIRST_ACCESS],-1

	mov	si,[bx] 		;ds:si points to bpb
	inc	bx
	inc	bx			;point to next guy
	;;mov	[es:bp+DPB.DRIVE],dx
	;mov	[es:bp],dx ; 13/05/2019
	; 29/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	[es:bp+0],dx		; [es:bp+DPB.DRIVE]
	
	mov	ah,SETDPB ; 53h		;hidden system call
	int	21h
			; DOS - 2+ internal - TRANSLATE BIOS PARAMETER BLOCK
			; DS:SI -> BPB (BIOS Parameter Block)
			; ES:BP -> buffer for DOS Drive Parameter Block

	;mov	ax,[es:bp+2]
	mov	ax,[es:bp+DPB.SECTOR_SIZE]
	push	es
	les	di,[cs:DOSINFO] 	;es:di point to dos info
	;cmp	ax,[es:di+10h]
	cmp	ax,[es:di+SYSI_MAXSEC]
	pop	es
	;jna	short iblk_1
	;jmp	short bad_bpb_size_sector
	; 29/10/2022
	ja	short bad_bpb_size_sector
iblk_1:
	push	ds
	push	dx

	lds	dx,[cs:DevEntry]
	;mov	[es:bp+13h],dx
	mov	[es:bp+DPB.DRIVER_ADDR],dx
	;mov	[es:bp+15h],ds
	mov	[es:bp+DPB.DRIVER_ADDR+2],ds

	pop	dx
	pop	ds

	inc	dx
	inc	dh
	loop	perunit

	push	cs
	pop	ds

	call	TempCDS 		; set cds for new drives
linkit:
	les	di,[cs:DOSINFO] 	;es:di = dos table
	mov	cx,[es:di+SYSI_DEV]	;dx:cx = head of list
	mov	dx,[es:di+SYSI_DEV+2]

	lds	si,[cs:DevEntry]	;ds:si = device location
	mov	[es:di+SYSI_DEV],si	;set head of list in dos
	mov	[es:di+SYSI_DEV+2],ds
	mov	ax,[si]			;get pointer to next device
	mov	[cs:DevEntry],ax	;and save it

	mov	[si],cx			;link in the driver
	mov	[si+2],dx
enddev:
	pop	si
	pop	es
	inc	ax			;ax = ffff (no more devs if yes)?
	jz	short coffj3

	inc	byte [cs:multdeviceflag] ; possibly multiple device driver.
	call	DevBreak		; M009
	; 03/04/2019 - Retro DOS v4.0
	; MSDOS 6.21 IO.SYS - SYSINIT:290Dh
	jmp	goodld			; otherwise pretend we loaded it in
coffj3: 
	mov	byte [cs:multdeviceflag],0 ; reset the flag
	call	DevBreak
	
	; 02/11/2022 (MSDOS 5.0 IO.SYS compatibility)
	;call	CheckProtmanArena	; adjust alloclim if Protman$ just
;					;  created a bogus arena to try
;					;  to protect some of its resident-
;					;  init code.
	;call	CheckDoubleSpace
	jmp	coff

;----------------------------------------------------------------------------

;CheckDoubleSpace:
;;;;	ifdef	dblspace_hooks
;
;;	Now check for two special MagicDrv cases:
;;
;;       a) the last driver load was MagicDrv final placement:
;;	   -> add number of MagicDrv reserved drives to drivenumber
;;
;;       b) MagicDrv is currently in temporary home:
;;          -> call it to give it a chance to mount and shuffle drives
;
;	cmp	byte [cs:MagicHomeFlag],0 ; already home?
;	jnz	short no_more_magic_calls ;  nothing more to do if so
;
;;	Now inquire of driver whether it is present, and final located
;
;	mov	ax,multMagicdrv ; 4A11h
;	mov	bx,MD_VERSION ; 0
;	int	2fh			; ch = number of MagicDrv drive letters
;	or	ax,ax			; is it there?
;	jnz	short no_more_magic_calls ; done if not
;
;	test	dx,8000h		; is it final placed?
;	jnz	short magic_not_yet_home ;  skip if not
;
;;	Okay, now the driver is final placed!  Set the flag so we
;;	don't keep checking it, and add its number of drive letters
;;	to drivenumber.
;
;	mov	byte [cs:MagicHomeFlag],0ffh ; set the flag!
;	add	[cs:drivenumber],ch	; add number of MagicDrv volumes to
;;					;  the drive number we'll pass to the
;;					;  next loadable block device.
;
;	jmp	short no_more_magic_calls ; and finished.
;
;magic_not_yet_home:
;	push	es
;	push	si
;
;	mov	cx,[cs:memhi]		; pass it a work buffer
;	mov	dx,[cs:ALLOCLIM]	;   address in cx (segment)
;	sub	dx,cx			;   for len dx (paragraphs)
;
;	mov	bx,2
;	mov	al,[cs:driver_units]	; shuffle magicdrives and new drives
;;					;   by this many units
;
;;BUGBUG 29-Oct-1992 bens Take this 55h out after Beta 4
;	mov	ah,55h			; backdoor won't shuffle unless it
;;					;  sees this, to prevent bad things
;;					;  from happening if people run the
;;					;  new driver with an old BIOS
;	call	far [cs:MagicBackdoor]
;
;	pop	si
;	pop	es
;
;no_more_magic_calls:
;
;;;;	endif
;	retn

; 03/04/2019 - Retro DOS v4.0

bad_bpb_size_sector:
	pop	si
	pop	es
	mov	dx,badsiz_pre
	mov	bx,crlfm
	call	prnerr

	jmp	coff

;------------------------------------------------------------------------------
; country command
;      the syntax is:
;	country=country id {,codepage {,path}}
;	country=country id {,,path}	:default codepage id in dos
;------------------------------------------------------------------------------

	; 30/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2663h)
tryq:
        cmp     ah,CONFIG_COUNTRY ; 'Q'
	je	short tryq_cont
skip_it3:
	jmp	tryf
tryq_cont:
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user		; query the user if config_cmd
;       jc      short skip_it3		; has the CONFIG_OPTION_QUERY bit set
;;endif
	mov	byte [cs:cntry_drv],0	; reset the drive,path to default value.
	mov	word [cs:p_code_page],0
	mov	di,cntry_parms
	xor	cx,cx
	mov	dx,cx
do52:
	call	sysinit_parse
	jnc	short if52		; parse error,check error code and

	call	cntry_error		; show message and end the search loop.
	mov	word [cs:p_cntry_code],-1 ; signals that parse error.
	jmp	short sr52
if52:
	cmp	ax,_$P_RC_EOL ; 0FFFFh	; end of line?
	jz	short sr52		; then end the search loop

	;cmp	byte [cs:result_val+_$P_Result_Blk.Type],_$P_number ; numeric?
	cmp	byte [cs:result_val],_$P_Number
	jnz	short if56

	;mov	ax,[cs:rw_dword]
	mov	ax,[cs:result_val+_$P_Result_Blk.Picked_Val]
	cmp	cx,1
	jne	short if57

	mov	[cs:p_cntry_code],ax
	
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short en57
	;jmp	short en56

if57:
	mov	[cs:p_code_page],ax
en57:
	jmp	short en56		; path entered

if56:
	push	ds
	push	es
	push	si
	push	di

	push	cs
	pop	es

	lds	si,[cs:rv_dword]	; move the path to known place.
	mov	di,cntry_drv
	call	move_asciiz

	pop	di
	pop	si
	pop	es
	pop	ds

en56:
	jmp	short do52

sr52:
	cmp	word [cs:p_cntry_code],-1	; had a parse error?
	jne	short tryq_open
	jmp	coff

tryqbad:				;"invalid country code or code page"
	stc
	mov     dx,badcountry
	jmp     tryqchkerr

tryq_open:
	cmp	byte [cs:cntry_drv],0
	je	short tryq_def
	mov	dx,cntry_drv
	jmp	short tryq_openit

tryq_def:
	mov	dx,cntry_root
tryq_openit:
	mov	ax,3D00h		;open a file
	stc
	int	21h
	jc	short tryqfilebad	;open failure

	mov	[cs:cntryfilehandle],ax	;save file handle
	mov	bx,ax
	mov	ax,[cs:p_cntry_code]
	mov	dx,[cs:p_code_page]	; now,ax=country id,bx=filehandle
	mov	cx,[cs:memhi]
	add	cx,384			; need 6k buffer to handle country.sys
					; M023
	cmp	cx,[cs:ALLOCLIM]
	ja	short tryqmemory	;cannot allocate the buffer for country.sys

	mov	si,cntry_drv		;ds:si -> cntry_drv
	cmp	byte [si],0 		;default path?
	jne	short tryq_set_for_dos

	inc	si
	inc	si			;ds:si -> cntry_root

tryq_set_for_dos:
	les	di,[cs:sysi_country]	;es:di -> country info tab in dos
	push	di			;save di
	;add	di,8
	add	di,country_cdpg_info.ccPath_CountrySys ; 8
	call	move_asciiz		;set the path to country.sys in dos.
	pop	di			;es:di -> country info tab again.

	mov	cx,[cs:memhi]
	mov	ds,cx
	xor	si,si			;ds:si -> 2k buffer to be used.
	call	setdoscountryinfo	;now do the job!!!
	jnc	short tryqchkerr	;read error or could not find country,code page combination

	cmp	cx,-1			;could not find matching country_id,code page?
	je	short tryqbad 		;then "invalid country code or code page"

tryqfilebad:
	push	cs
	pop	es
	cmp	byte [cs:cntry_drv],0	;is the default file used?
	je	short tryqdefbad

	mov	si,cntry_drv
	jmp	short tryqbadload

tryqdefbad:				;default file has been used.
	mov	si,cntry_root		;es:si -> \country.sys in sysinit_seg
tryqbadload:
	call	badload 		;ds will be restored to sysinit_seg
	;mov	cx,[cs:CONFBOT]
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	cx,[cs:top_of_cdss]
	mov	es,cx			;restore es -> confbot.
	jmp	short coffj4

tryqmemory:
	mov	dx,insufmemory
tryqchkerr:
	;mov	cx,[cs:CONFBOT]
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	cx,[cs:top_of_cdss]
	mov	es,cx			;restore es -> confbot seg
	push	cs
	pop	ds			;restore ds to sysinit_seg
	jnc	short coffj4		;if no error,then exit

	call	print			;else show error message
	call	error_line
coffj4:
	mov	bx,[cs:cntryfilehandle]
	mov	ah,3Eh
	int	21h			;close a file. don't care even if it fails.
	jmp	coff

;--------------------------------------------

cntry_error:

;function: show "invalid country code or code page" messages,or
;		"error in country command" depending on the error code
;		in ax returned by sysparse;
;in:	ax - error code
;	ds - sysinitseg
;	es - confbot
;out:	show message.  dx destroyed.

	cmp	ax,_$P_Out_Of_Range ; 6
	jne	short if64
	mov	dx,badcountry		;"invalid country code or code page"
	jmp	short en64

if64:
	mov	dx,badcountrycom	;"error in contry command"
en64:
	call	print
	call	error_line
	retn

;------------------------------------------------------------------------------
; files command
;------------------------------------------------------------------------------

;******************************************************************************
; function: parse the parameters of files= command.			      *
;									      *
; input :								      *
;	es:si -> parameters in command line.				      *
; output:								      *
;	variable files set.						      *
;									      *
; subroutines to be called:						      *
;	sysinit_parse							      *
; logic:								      *
; {									      *
;	set di points to files_parms;					      *
;	set dx,cx to 0; 						      *
;	while (end of command line)					      *
;	{ sysinit_parse;						      *
;	  if (no error) then						      *
;	     files = result_val._$P_picked_val				      *
;	  else								      *
;	     error exit;						      *
;	};								      *
; };									      *
;									      *
;******************************************************************************

tryf:
        cmp     ah,CONFIG_FILES ;  'F'
	jne	short tryl

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc      short tryl		; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	di,files_parms
	xor	cx,cx
	mov	dx,cx
do67:
	call	sysinit_parse
	jnc	short if67		; parse error
	call	badparm_p		;   and show messages and end the search loop.
	jmp	short sr67
if67:
	cmp	ax,_$P_RC_EOL		; end of line?
	je	short en67		; then end the $endloop
	;mov	al,[cs:rv_dword]
	mov	al,[cs:result_val+_$P_Result_Blk.Picked_Val]
	mov	[cs:p_files],al		; save it temporarily
	jmp	short do67
en67:
	mov	al,[cs:p_files]
	mov	[cs:FILES],al		; no error. really set the value now.
sr67:
	jmp	coff

; 04/04/2019 - Retro DOS v4.0

;------------------------------------------------------------------------------
; lastdrive command
;------------------------------------------------------------------------------

;******************************************************************************
; function: parse the parameters of lastdrive= command. 		      *
;									      *
; input :								      *
;	es:si -> parameters in command line.				      *
; output:								      *
;	set the variable num_cds.					      *
;									      *
; subroutines to be called:						      *
;	sysinit_parse							      *
; logic:								      *
; {									      *
;	set di points to ldrv_parms;					      *
;	set dx,cx to 0; 						      *
;	while (end of command line)					      *
;	{ sysinit_parse;						      *
;	  if (no error) then						      *
;	     set num_cds to the returned value; 			      *
;	  else	/*error exit*/						      *
;	     error exit;						      *
;	};								      *
; };									      *
;									      *
;******************************************************************************

tryl:
        cmp     ah,CONFIG_LASTDRIVE ; 'L'
	jne	short tryp

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user      ; query the user if config_cmd
;       jc	short tryp	; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	di,ldrv_parms
	xor	cx,cx
	mov	dx,cx
do73:
	call	sysinit_parse
	jnc	short if73	; parse error
	call	badparm_p	;   and show messages and end the search loop.
	jmp	short sr73
if73:
	cmp	ax,_$P_RC_EOL	; end of line?
	je	short en73	; then end the $endloop
	;mov	al,[cs:rv_dword]
	mov	al,[cs:rv_byte]	; pick up the drive number
	mov	[cs:p_ldrv],al	; save it temporarily
	jmp	short do73
en73:
	mov	al,[cs:p_ldrv]
	mov	[cs:NUM_CDS],al	; no error. really set the value now.
sr73:
	jmp	coff

;--------------------------------------------------------------------------
; setting drive parameters
;--------------------------------------------------------------------------

tryp:
        cmp     ah,CONFIG_DRIVPARM ; 'P'
	jne	short tryk

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user      ; query the user if config_cmd
;       jc      short tryk	; has the CONFIG_OPTION_QUERY bit set
;;endif

	call	parseline
	jc	short trypbad
	call	setparms
	call	diddleback

; No error check here, because setparms and diddleback have no error 
; returns, and setparms as coded now can return with carry set. 
;       jc	short trypbad

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jc	short trypbad

	jmp	coff
trypbad:
	jmp	badop

;--------------------------------------------------------------------------
; setting internal stack parameters
; stacks=m,n where
;	m is the number of stacks (range 8 to 64,default 9)
;	n is the stack size (range 32 to 512 bytes,default 128)
; j.k. 5/5/86: stacks=0,0 implies no stack installation.
;	any combinations that are not within the specified limits will
;	result in "unrecognized command" error.
;--------------------------------------------------------------------------

;**************************************************************************
;									  *
; function: parse the parameters of stacks= command.			  *
;	    the minimum value for "number of stacks" and "stack size" is  *
;	    8 and 32 each.  in the definition of sysparse value list,they *
;	    are set to 0.  this is for accepting the exceptional case of  *
;	    stacks=0,0 case (,which means do not install the stack.)	  *
;	    so,after sysparse is done,we have to check if the entered	  *
;	    values (stack_count,stack_size) are within the actual range,  *
;	    (or if "0,0" pair has been entered.)			  *
; input :								  *
;	es:si -> parameters in command line.				  *
; output:								  *
;	set the variables stack_count,stack_size.			  *
;									  *
; subroutines to be called:						  *
;	sysinit_parse							  *
; logic:								  *
; {									  *
;	set di points to stks_parms;					  *
;	set dx,cx to 0; 						  *
;	while (end of command line)					  *
;	{ sysinit_parse;						  *
;	  if (no error) then						  *
;	     { if (cx == 1) then /* first positional = stack count */	  *
;		   p_stack_count = result_val._$P_picked_val;		  *
;	       if (cx == 2) then /* second positional = stack size */	  *
;		   p_stack_size = result_val._$P_picked_val;		  *
;	     }								  *
;	  else	/*error exit*/						  *
;	     error exit;						  *
;	};								  *
;	here check p_stack_count,p_stack_size if it meets the condition;  *
;	if o.k.,then set stack_count,stack_size;			  *
;	 else error_exit;						  *
; };									  *
;**************************************************************************

tryk:
        ;if      stacksw

        cmp     ah,CONFIG_STACKS ; 'K'
	je	short do_tryk
skip_it4:
	jmp	trys
do_tryk:

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc	short skip_it4		; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	di,stks_parms
	xor	cx,cx
	mov	dx,cx
do79:
	call	sysinit_parse
	jnc	short if79		; parse error

	mov	dx,badstack		; "invalid stack parameter"
	call	print			;  and show messages and end the search loop.
	call	error_line
	jmp	sr79
if79:
	cmp	ax,_$P_RC_EOL		; end of line?
	je	short en79		; then end the $endloop

	;mov	ax,[cs:rv_dword]
	mov	ax,[cs:result_val+_$P_Result_Blk.Picked_Val]
	cmp	cx,1
	jne	short if83

	mov	[cs:p_stack_count],ax
	jmp	short en83
if83:
	mov	[cs:p_stack_size],ax
en83:
	jmp	short do79
en79:
	cmp	word [cs:p_stack_count],0
	je	short if87

	cmp	word [cs:p_stack_count],mincount ; 8
	jb	short ll88
	cmp	word [cs:p_stack_size],minsize ; 32
	jnb	short if88
ll88:
	mov	word [cs:p_stack_count],-1 ; invalid
if88:
	jmp	short en87
if87:
	cmp	word [cs:p_stack_size],0
	je	short en87
	mov	word [cs:p_stack_count],-1 ; invalid
en87:
	cmp	word [cs:p_stack_count],-1 ; invalid?
	jne	short if94

	mov	word [cs:stack_count],defaultcount ; 9
					; reset to default value.
	mov	word [cs:stack_size],defaultsize ; 128
	mov	word [cs:stack_addr],0

	mov	dx,badstack
	call	print
	call	error_line
	jmp	short sr79
if94:
	mov	ax,[cs:p_stack_count]
	mov	[cs:stack_count],ax
	mov	ax,[cs:p_stack_size]
	mov	[cs:stack_size],ax
	mov	word [cs:stack_addr],-1	; stacks= been accepted.
sr79:
	jmp	coff

	;endif

;------------------------------------------------------------------------
; shell command
;------------------------------------------------------------------------

trys:
        cmp     ah,CONFIG_SHELL ; 'S'
	jne	short tryx

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user              ; query the user if config_cmd
;       jc	short tryx		; has the CONFIG_OPTION_QUERY bit set
;       mov	byte [cs:newcmd],1
;;endif

	;mov	word [cs:command_line],0 ; zap length,first byte of command-line
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	byte [cs:command_line+1],0

        mov     di,commnd+1		; we already have the first char
        mov     [di-1],al               ; of the new shell in AL, save it now
storeshell:
	call	getchr
        or      al,al                   ; this is the normal case: "organize"
        jz	short getshparms	; put a ZERO right after the filename

        cmp     al," "                  ; this may happen if there are no args
        jb	short endofshell	; I suppose...
	mov	[di],al
	inc	di
        ;cmp    di,commnd+63		; this makes sure we don't overflow
        ;jb	short storeshell	; commnd (the filename)
        ;jmp	short endofshell
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short storeshell

; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;getshparms:
;	mov     byte [di],0		; zero-terminate the filename
;	mov     di,command_line+1	; prepare to process the command-line
;
;parmloop:
;	call	getchr
;	cmp	al," "
;	jb	short endofparms
;	mov	[di],al
;	inc	di
;	cmp     di,command_line+126
;	jb	short parmloop
;endofparms:
;	mov     cx,di
;	sub     cx,command_line+1
;	mov     [cs:command_line],cl
;
;endofshell:
;	mov     byte [di],0		; zero-terminate the filename (or
;					; the command-line as the case may be)
;skipline:
;       cmp     al,lf	; 0Ah		; the safest way to eat the rest of
;       je	short endofline		; the line: watch for ever-present LF
;call	getchr
;       jnc	short skipline		; keep it up as long as there are chars
;
;endofline:
;       jmp     conflp

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
endofshell:
     	mov     byte [di],0		; zero-terminate the filename (or
					; the command-line as the case may be)
	call	getchr
skipline:
	cmp     al,lf	; 0Ah		; the safest way to eat the rest of
	je	short endofline		; the line: watch for ever-present LF
	call	getchr
endofline:
	jmp     conflp

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
getshparms:
	mov     byte [di],0		; zero-terminate the filename
	mov     di,command_line+1	; prepare to process the command-line
parmloop:
	call	getchr
	cmp	al," " ; 20h
	jb	short endofshell
	mov	[di],al
	inc	di
	jmp	short parmloop

;------------------------------------------------------------------------
; fcbs command
;------------------------------------------------------------------------

;************************************************************************
; function: parse the parameters of fcbs= command.			*
;									*
; input :								*
;	es:si -> parameters in command line.				*
; output:								*
;	set the variables fcbs,keep.					*
;									*
; subroutines to be called:						*
;	sysinit_parse							*
; logic:								*
; {									*
;	set di points to fcbs_parms;					*
;	set dx,cx to 0; 						*
;	while (end of command line)					*
;	{ sysparse;							*
;	  if (no error) then						*
;	     { if (cx == 1) then /* first positional = fcbs */		*
;		   fcbs = result_val._$P_picked_val;			*
;	       if (cx == 2) then /* second positional = keep */ 	*
;		   keep = result_val._$P_picked_val;			*
;	     }								*
;	  else	/*error exit*/						*
;	     error exit;						*
;	};								*
; };									*
;************************************************************************

tryx:
        cmp     ah,CONFIG_FCBS  ; 'X'
        ;jne	short try1
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jne	short tryy	; comment command

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user      ; query the user if config_cmd
;       jc	short try1	; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	di,fcbs_parms
	xor	cx,cx
	mov	dx,cx
do98:
	call	sysinit_parse
        jnc	short if98	; parse error
        call    badparm_p       ;  and show messages and end the search loop.
	jmp	short sr98
if98:
	cmp	ax,_$P_RC_EOL	; end of line?
	je	short en98	; then end the $endloop

	;mov	al,[cs:rv_dword]
	mov	al,[cs:result_val+_$P_Result_Blk.Picked_Val]
	cmp	cx,1		; the first positional?
	jne	short if102
	mov	[cs:p_fcbs],al
	jmp	short en102

if102:
	mov	[cs:p_keep],al
en102:
	jmp	short do98

en98:
	mov	al,[cs:p_fcbs]	 ; M017
	mov	[cs:FCBS],al	 ; M017
	mov	byte [cs:KEEP],0 ; M017
sr98:
	jmp	coff

; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;-------------------------------------------------------------------------
; comment= do nothing. just decrease chrptr,and increase count for correct
;		line number
;-------------------------------------------------------------------------

tryy:
	cmp     ah,CONFIG_COMMENT ; 'Y'
	jne	short try0

donothing:
	;dec	word [chrptr]
	;inc	word [count]
	; 02/11/2022
	dec	word [cs:chrptr]
	inc	word [cs:count]
	jmp	coff

;------------------------------------------------------------------------
; rem command
;------------------------------------------------------------------------

try0:				; do nothing with this line.
	cmp     ah,CONFIG_REM ; '0'
	je	short donothing

; 07/04/2019 - Retro DOS v4.0

;-----------------------------------------------------------------------
; switches command
;-----------------------------------------------------------------------

;***********************************************************************
;								       *
; function: parse the option switches specified.		       *
; note - this command is intended for the future use also.	       *
; when we need to set system data flag,use this command.	       *
;								       *
; input :							       *
;	es:si -> parameters in command line.			       *
; output:							       *
;	p_swit_k set if /k option chosen.			       *
;								       *
; subroutines to be called:					       *
;	sysinit_parse						       *
; logic:							       *
; {								       *
;	set di points to swit_parms;  /*parse control definition*/     *
;	set dx,cx to 0; 					       *
;	while (end of command line)				       *
;	{ sysinit_parse;					       *
;	  if (no error) then					       *
;	       if (result_val._$P_synonym_ptr == swit_k) then	       *
;		    p_swit_k = 1				       *
;	       endif						       *
;	  else {show error message;error exit}			       *
;	};							       *
; };								       *
;								       *
;***********************************************************************

SUPPRESS_WINA20	EQU 00000010b	; M025 ; (DOSSYM.INC, MSDOS 6.0)

try1:
        cmp     ah,CONFIG_SWITCHES ; '1'
	je	short do_try1	; switches= command entered?
skip_it5:
	;jmp	tryv
	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	tryz

do_try1:

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;       call    query_user      ; query the user if config_cmd
;       jc	short skip_it5	; has the CONFIG_OPTION_QUERY bit set
;;endif

	mov	di,swit_parms
	xor	cx,cx
	mov	dx,cx
do110:
	call	sysinit_parse
	jnc	short if110	; parse error
	call	badparm_p	;  and show messages and end the search loop.
	jmp	short sr110

if110:
	cmp	ax,_$P_RC_EOL	; end of line?
	je	short en110	; then jmp to $endloop for semantic check

	;cmp	word [cs:result_val_swoff],swit_k
	cmp	word [cs:result_val+_$P_Result_Blk.SYNONYM_Ptr],swit_k 
	jne	short if115	;				;M059
	mov	byte [cs:p_swit_k],1	; set the flag
	jmp	short do110
if115:								;M059
	;cmp	word [cs:result_val_swoff],swit_t
	cmp	word [cs:result_val+_$P_Result_Blk.SYNONYM_Ptr],swit_t	;M059
	jne	short if116					;M059 M063
	mov	byte [cs:p_swit_t],1				;M059
	jmp	short do110					;M059
if116:
	;cmp	word [cs:result_val_swoff],swit_w
	cmp	word [cs:result_val+_$P_Result_Blk.SYNONYM_Ptr],swit_w	;M063
	jne	short do110					;M063
	mov	byte [cs:p_swit_w],1				;M063
	jmp	short do110					;M063
en110:
	cmp	byte [cs:p_swit_k],1	; if /k entered,
	push	ds
	;;mov	ax,Bios_Data
	;mov	ax,KERNEL_SEGMENT ; 0070h
	; 21/10/2022
	mov	ax,DOSBIODATASEG ; 0070h
	mov	ds,ax
	jne	short if117
	mov	byte [keyrd_func],0 ; 4E5h ; use the conventional keyboard functions
	mov	byte [keysts_func],1 ; 4E6h (for MSDOS 6.21 IO.SYS)
if117:
	mov	al,[cs:p_swit_t]				;M059
	mov	[t_switch],al	; 4F2h (for MSDOS 6.21 IO.SYS)	;M059

	cmp	byte [cs:p_swit_w],0				;M063
	je	short skip_dos_flag				;M063
	push	es
	push	bx
	mov	ah,GET_IN_VARS ; 52h				;M063
	int	21h						;M063
			; DOS - 2+ internal - GET LIST OF LISTS
			; Return: ES:BX -> DOS list of lists
	;or	bytes [es:86h],2
	or	byte [es:DOS_FLAG_OFFSET],SUPPRESS_WINA20 ; 2	;M063
	pop	bx
	pop	es
skip_dos_flag:							;M063
	pop	ds
sr110:
	jmp	coff

; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;tryv:
;
;;ifdef	MULTI_CONFIG
;;------------------------------------------------------------------------
;; set command (as in "set var=value<cr/lf>")
;;------------------------------------------------------------------------
;
;       cmp     ah,CONFIG_SET  ; 'V'
;       jne	short tryn
;       call    query_user      ; query the user if config_cmd
;       jc	short tryn 	; has the CONFIG_OPTION_QUERY bit set
;       call    copy_envvar     ; copy var at ES:SI to "config_wrkseg"
;       jnc	short sr110	; no error
;err:    
;	call    error_line      ; whoops, display error in line XXX
;       jmp     short sr110     ; jump to coff (to skip to next line)
;
;;------------------------------------------------------------------------
;; numlock command (as in "numlock=on|off")
;;------------------------------------------------------------------------
;tryn:
;       cmp     ah,CONFIG_NUMLOCK  ;'N'
;       jne	short tryy            ;
;       call    query_user      ; query thye user if config_cmd
;       jc	short tryy	; has the CONFIG_OPTION_QUERY bit set
;       call    set_numlock
;       jc	short err
;       jmp	short sr110	; all done
;
;;endif	;MULTI_CONFIG

; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;-------------------------------------------------------------------------
;; comment= do nothing. just decrese chrptr,and increase count for correct
;;		line number
;;-------------------------------------------------------------------------
;
;tryy:
;       cmp     ah,CONFIG_COMMENT ; 'Y'
;	jne	short try0
;donothing:
;	dec	word [chrptr]
;	inc	word [count]
;	jmp	coff
;
;;------------------------------------------------------------------------
;; rem command
;;------------------------------------------------------------------------
;
;try0:				;do nothing with this line.
;	cmp     ah,CONFIG_REM ; '0'
;	je	short donothing

; 30/10/2022
; (MSSOS 5.0 IO.SYS - SYSINIT:29D7h)

;------------------------------------------------------------------------
; bogus command
;------------------------------------------------------------------------

tryz:
        cmp     ah,0FFh		;null command? (BUGBUG - who sets FFh anyway?)
	;je	short donothing
	; 02/11/2022
	je	short tryz_donothing

	dec	word [chrptr]
	inc	word [count]
	jmp	short badop

	; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
tryz_donothing:
	jmp	donothing

; 07/04/2019 - Retro DOS v4.0

;------------------------------------------------------------------------------

; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;
;;***	CheckProtmanArena -- special hack for adjusting alloclim with Protman$
;;
;;	adjusts alloclim if Protman$ reduced our arena through a manual hack.
;
;CheckProtmanArena:
;	push	es
;	mov	ax,[cs:area]	; get our arena header
;	dec	ax
;	mov	es,ax
;	add	ax,[es:3]	; find end of arena
;	inc	ax
;	cmp	ax,[cs:ALLOCLIM] ; is it less than alloclim?
;	ja	short CheckProtmanDone
;
;	mov	[cs:ALLOCLIM],ax ; reduce alloclim then
;CheckProtmanDone:
;	pop	es
;	retn

;------------------------------------------------------------------------------

sysinit_parse:

;------------------------------------------------------------------------------
;set up registers for sysparse
;in)	es:si -> command line in confbot
;	di -> offset of the parse control definition.
;
;out)	calls sysparse.
;	carry will set if parse error.
;	*** the caller should check the eol condition by looking at ax
;	*** after each call.
;	*** if no parameters are found,then ax will contain a error code.
;	*** if the caller needs to look at the synomym@ of the result,
;	***  the caller should use cs:@ instead of es:@.
;	cx register should be set to 0 at the first time the caller calls this
;	 procedure.
;	ax - exit code
;	bl - terminated delimeter code
;	cx - new positional ordinal
;	si - set to pase scanned operand
;	dx - selected result buffer
;------------------------------------------------------------------------------

	; 24/10/2022
	push	es			;save es,ds
	push	ds

	push	es
	pop	ds			;now ds:si -> command line

	push	cs
	pop	es			;now es:di -> control definition

	mov	[cs:badparm_seg],ds	;save the pointer to the parm
	mov	[cs:badparm_off],si	; we are about to parse for badparm msg.
	mov	dx,0
	call	SysParse
	cmp	ax,_$P_No_Error	; 0	;no error

;**cas note:  when zero true after cmp, carry clear

	je	short ll4
	; 24/10/2022 (MSDOS 5.0 IO.SYS compatibility, SYSINIT:2A02h)
	;je	short en4
	cmp	ax,_$P_RC_EOL ; 0FFFFh	;or the end of line?
	jne	short if4
ll4:
	clc
	jmp	short en4
if4:
	; 24/10/2022
	stc
en4:
	pop	ds
	pop	es
	retn

;----------------------------------------------------------------------------
;
; procedure : badop_p
;
;             same thing as badop,but will make sure to set ds register back
;             to sysinitseg and return back to the caller.
;
;----------------------------------------------------------------------------

badop_p:
	push	cs
	pop	ds		;set ds to configsys seg.
	mov	dx,badopm
	call	print
        call    error_line
	retn

;----------------------------------------------------------------------------
;
; label : badop
;
;----------------------------------------------------------------------------

badop:	
	mov	dx,badopm	;want to print command error "unrecognized command..."
	call	print
	call	error_line	;show "error in config.sys ..." .
	jmp	coff

;----------------------------------------------------------------------------
;
; procedure : badparm_p
;
;             show "bad command or parameters - xxxxxx"
;             in badparm_seg,badparm_off -> xxxxx
;
;----------------------------------------------------------------------------

	; 24/10/2022
badparm_p:
	push	ds
	push	dx
	push	si

	push	cs
	pop	ds

	mov	dx,badparm
	call	print			;"bad command or parameters - "
	lds	si,[badparm_ptr]

;	print "xxxx" until cr.

do1:
	mov	dl,[si]			; get next character
	cmp	dl,cr ; 0Dh		; is a carriage return?
	je	short en1			; exit loop if so

	mov	ah,2 ; STD_CON_OUTPUT	; function 2
	int	21h			; display character
	inc	si			; next character
	jmp	short do1
en1:
	push	cs
	pop	ds

	mov	dx,crlfm
	call	print
	call	error_line

	pop	si
	pop	dx
	pop	ds
badparmp_ret:
	retn

;----------------------------------------------------------------------------
;
; procedure : getchr
;
;----------------------------------------------------------------------------

	; 24/10/2022
getchr:
	push	cx
	mov	cx,[count]
	jcxz	nochar

	mov	si,[chrptr]
	mov	al,[es:si]
	dec	word [count]
	inc	word [chrptr]
	clc
get_ret:
	pop	cx
	retn
nochar: 
	stc
	jmp	short get_ret

;----------------------------------------------------------------------------
;
; procedure : incorrect_order
;
;             show "incorrect order in config.sys ..." message.
;
;----------------------------------------------------------------------------

incorrect_order:
	mov	dx,badorder
	call	print
	call	showlinenum
	retn

;----------------------------------------------------------------------------
;
; procedure : error_line
;
;             show "error in config.sys ..." message.
;
;----------------------------------------------------------------------------

	; 24/10/2022
error_line:
	push	cs
	pop	ds
	mov	dx,errorcmd
	call	print
	call	showlinenum
	retn

;----------------------------------------------------------------------------
;
; procedure : showlinenum
;
; convert the binary linecount to decimal ascii string in showcount
;and display showcount at the current curser position.
;in.) linecount
;
;out) the number is printed.
;
;----------------------------------------------------------------------------

	; 24/10/2022
showlinenum:
	push	es
	push	ds
	push	di

	push	cs
	pop	es		; es=cs

	push	cs
	pop	ds

	mov	di,showcount+4	; di -> the least significant decimal field.
	mov	cx,10		; decimal divide factor
	mov	ax,[cs:linecount]
sln_loop:
	cmp	ax,10		; < 10?
	jb	short sln_last

	xor	dx,dx
	div	cx
	or	dl,30h		; add "0" (= 30h) to make it an ascii.
	mov	[di],dl
	dec	di
	jmp	short sln_loop

sln_last:
	or	al,30h	; "0"
	mov	[di],al
	mov	dx,di
	call	print		; show it.
	pop	di
	pop	ds
	pop	es
	retn

; 07/04/2019 - Retro DOS v4.0
; (MSDOS 6.21 IO.SYS, SYSINIT:2E44h)

;----------------------------------------------------------------------------
;
; procedure : ProcDOS
;
;	Process the result of DOS= parsing
;
;	result_val._$P_item_tag	= 1 for DOS=HIGH
;				= 2 for DOS=LOW
;				= 3 for DOS=UMB
;				= 4 for DOS=NOUMB
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modififed MSDOS 5.0 IO.SYS)
	; (SYTSINIT:2AB5h)
ProcDOS:
	xor	ah,ah
	;mov	al,[cs:result_val_itag]
	mov	al,[cs:result_val+_$P_Result_Blk.Item_Tag]
	dec	ax
	jz	short pd_hi
	dec	ax
	jz	short pd_lo
	dec	ax
	jz	short pd_umb
	mov	byte [cs:DevUMB],0
	retn
pd_umb:
	mov	byte [cs:DevUMB],0FFh
	retn
pd_lo:
	mov	byte [cs:runhigh],0
	retn
pd_hi:
	mov	byte [cs:runhigh],0FFh
	retn

;----------------------------------------------------------------------------
;
; procedure : LieInt12Mem
;
;	Input : DevEntry points to Device Start address (offset == 0)
;		alloclim set to the limit of low memory.
;
;	Output : none
;
;	Changes the ROM BIOS variable which stores the total low memory
;	If a 3com device driver (any character device with name 'PROTMAN$')
;	is being loaded alloclim is converted into Ks and stored in 40:13h
;	Else if a device driver being loaded into UMB the DevLoadEnd is
;	converted into Ks and stored in 40:13h
;
;----------------------------------------------------------------------------

LieInt12Mem:
	mov	ax,[cs:ALLOCLIM]	; lie INT 12 as alloclim
					; assuming that it is 3Com
	call	IsIt3Com		; Is it 3Com driver?
	jz	short lim_set		; yes, lie to him differently
	; 13/05/2019
	cmp	byte [cs:DeviceHi],0	; Is the DD being loaded in UMB
	je	short limx		; no, don't lie
	mov	ax,[cs:DevLoadEnd]	; lie INT 12 as end of UMB
lim_set:
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	call	SetInt12Mem
limx:
	retn

;----------------------------------------------------------------------------
;
; procedure : SetInt12Mem
;
;	Input : AX = Memory size to be set (in paras)
;	Output : none
;
;	Sets the variable 40:13 to the memory size passed in AX
;	It saves the old value in 40:13 in OldInt12Mem,
;	It also sets a flag Int12Lied to 0ffh, which is checked before
;	restoring the value of 40:13
;
;----------------------------------------------------------------------------

	; 01/11/2022
SetInt12Mem:
	push	ds
	mov	bx,40h
	mov	ds,bx			; ROM BIOS Data Segment
	mov	bx,[13h]		; INT 12 memory variable
	mov	[cs:OldInt12Mem],bx	; save it
	mov	cl,6
	shr	ax,cl			; convert paras into Ks
	mov	[13h],ax		; Lie
	mov	byte [cs:Int12Lied],0FFh ; mark that we are lying
	pop	ds
;limx:
	retn

;----------------------------------------------------------------------------
;
; procedure : TrueInt12Mem
;
;	Input : Int12Lied = 0 if we are not lying currently
;			  = 0ffh if we are lying
;		OldInt12Mem = Saved value of 40:13h
;
;	Output : none
;
;	Resets the INT 12 Memory variable if we were lying about int 12
;	and resets the flag which indicates that we were lying
;
;----------------------------------------------------------------------------

TrueInt12Mem:
	cmp	byte [cs:Int12Lied],0	; were we lying so far?
	; 01/11/2022 (MSDOS 5.0 IO.SYS, SYS.INIT:2B1Dh)
	;mov	byte [cs:Int12Lied],0	; reset it anyway
	je	short timx		; no, we weren't
	mov	byte [cs:Int12Lied],0
	push	ds
	mov	ax,40h
	mov	ds,ax
	mov	ax,[cs:OldInt12Mem]
	mov	[13h],ax		; restore INT 12 memory
	pop	ds
timx:
	retn

;----------------------------------------------------------------------------
;
; procedure : IsIt3Com?
;
;	Input : DevEntry = Seg:0 of device driver
;	Output : Zero flag set if device name is 'PROTMAN$'
;		 else Zero flag is reset
;
;----------------------------------------------------------------------------

IsIt3Com:
	push	ds
	push	es
	push	si
	lds	si,[cs:DevEntry]	; ptr to device header
	add	si,SYSDEV.NAME ; 10 	; ptr device name
	push	cs
	pop	es
	mov	di,ThreeComName
	mov	cx,8			; name length
	rep	cmpsb
	pop	si
	pop	es
	pop	ds
	retn

;M020 : BEGIN
;----------------------------------------------------------------------------

UpdatePDB:
	push	ds
	mov	ah,62h
	int	21h	; DOS - 3+ - GET PSP ADDRESS
	mov	ds,bx
	mov	bx,[cs:ALLOCLIM]
	;mov	[2],bx
	mov	[PDB.BLOCK_LEN],bx
	pop	ds
	retn

; M020 : END

;----------------------------------------------------------------------------

; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)

%if 0

;include highload.inc	; Routines for devicehigh parsing, control of HIDDEN
;include highexit.inc	; umb's, etc

; ----------------------------------------------------------------------
; HIGHLOAD.INC (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 07/04/2019 - Retro DOS v4.0

;******************************************************************************
;
; This file contains routines needed to parse and implement user-given
; command-line options of the form "/S/L:3,0x500;2;7,127;0x0BE4". InitVar()
; and Parsevar() are used to parse this data and place it in encoded form into
; the variables in highvar.inc, for use by the rest of the routines.
;
; DeviceHigh accepts this command-line (handled in sysconf.asm, not here):
;    DEVICEHIGH SIZE=hhhhhh module opts
; Or, DeviceHigh and LoadHigh accept any of the following:
;    DH/LH module opts
;    DH/LH [/S][/L:umb[,size][;umb[,size]]*] module opts
;    DH/LH [/L:umb[,size][;umb[,size]]*][/S] module opts
; The initial UMB,SIZE pair designates the module's load address; the remainder
; of the UMB and SIZE pairs are used to indicate specific UMBs to be left
; available during the load.
;
; When an actual load is ready to be performed, a call to HideUMBs() will
; temporarily allocate (as owner 8+"HIDDEN  ") all free elements in any
; upper-memory block which was not specified by the user... in addition, if
; UMBs were marked to shrink (/S option) to a certain size ("umb,size"), any
; elements in that umb SAVE the lower-half of the newly-shrunken one are also
; allocated.  After the load, the function UnHideUMBs() (in highexit.inc) will
; free any UMBs so allocated.
;
; When a device driver loads, there is the additional problem of allocating its
; initial load site; this should be restricted to the first UMB specified on
; the command-line. The function FreezeUM temporarily allocates all remaining
; free upper-memory elements (as owner 8+"FROZEN  "), except those in the load
; UMB. Then the initial allocation may be made, and a call to UnFreeze will
; return any so-allocated memory elements to FREE, for the true load. Note
; that UnFreeze leaves HIDDEN elements allocated; it only frees FROZEN ones.
;
;******************************************************************************

SWTCH	equ	'/'		; Switch character

DOS_CHECK_STRATEGY  equ	5800h	; Int 21h, Func 58h, Svc 0 = check alloc strat
DOS_SET_STRATEGY    equ	5801h	; Int 21h, Func 58h, Svc 1 = set alloc strategy
DOS_CHECK_UMBLINK   equ	5802h	; Int 21h, Func 58h, Svc 2 = check link state
DOS_GET_UMBLINK	    equ 5802h ; 20/04/2019
DOS_SET_UMBLINK     equ	5803h	; Int 21h, Func 58h, Svc 3 = set link state
DOS_GET_DOS_LISTS   equ	  52h	; Int 21h, Func 52h = return list of lists
DOS_UMB_HEAD        equ	  8Ch	; Offset from ES (after func52h) to get UMBHead

CR	equ	0Dh		; Carriage Return
LF	equ	0Ah		; Line Feed
TAB	equ	09h		; Tab character (^I)

; -----------------------------------------------------------------------------
;*** InitVar - initializes all the variables used in ParseVar and HideUMBs
; -----------------------------------------------------------------------------
; ENTRY:       None
; EXIT:        Variables listed in highvar.inc are initialized
; ERROR EXIT:  None
; USES:        Flags, variables in highvar.inc
; -----------------------------------------------------------------------------
; Note that element 0 references UMB 0 (conventional), not UMB 1.  Its contents
; are largely ignored, but it is initialized nonetheless.
; -----------------------------------------------------------------------------

InitVar:
	;pushreg <ax, cx, di, es>
	push	ax
	push	cx
	push	di
	push	es

	;dataseg es			;Point ES into appropriate data segment
	push	cs
	pop	es

	xor	ax,ax
	mov	[es:fUmbTiny],al	;Shrink UMBs? (made 1 if /S given)
	mov	[es:fInHigh],al		;Set to 1 when DH/LH has been called
	mov	[es:SegLoad],ax		;Load Address (seg), used for DH only
	mov	byte [es:UmbLoad],UNSPECIFIED ; 0FFh
					;Later is the # of the 1st spec'd UMB
	mov	[es:fm_argc], al	;Start with zero args having been read

	cld

	mov	cx,MAXUMB ; 16		;For each entry
	mov	di,UmbUsed		;on the UmbUsed array,
	rep	stosb			;	Store 0

	mov	cx,MAXUMB ; 16		;Okay... for each entry
	mov	di,UmbSize		;on the UmbSize array,
	rep	stosw			;	Store 0

	;normseg es			; Return ES

	;popreg	<es, di, cx, ax>
	pop	es
	pop	di
	pop	cx
	pop	ax	 	

	retn

; -----------------------------------------------------------------------------
;*** FixMem - scans the upper memory chain and concatenates adjacent free MCBs
; -----------------------------------------------------------------------------
; ENTRY   : None
; EXIT    : None
; ERROR   : None
; USES    : Flags, fm_umb, fm_strat
; -----------------------------------------------------------------------------

FixMem:
	push	ax
	push	bx
	push	cx
	push	dx
	push	es

	call	fm_link		; Link in UMBs

	call	UmbHead		; Get first upper-memory MCB address (0x9FFF)
	jc	short fmX	; (if couldn't get it, leave now).

	mov	es,ax		; It returns in AX, so move it to ES.

; - Walk MCB Chain ------------------------------------------------------------

	xor	dx,dx		; We're keeping the address of the last MCB
	mov 	cx,dx		; in CX... and the last owner
	inc	dx		; in dx as we go through the loop:

; ------------------------------------------
; FM10--DX  = last MCB's owner's PSP address
;       CX  = last MCB's address (segment)
; ------------------------------------------

fm10:	mov	al,[es:ARENA.SIGNATURE] ; if 'Z', don't repeat loop
	mov	bx,[es:ARENA.OWNER]	; if not zero, do nothing
	or	bx,dx			; dx was owner of previous MCB
	jnz	short fm30		; If not both zero, don't cat.

	; - Coalesce memory blocks at ES:00 and CX:00 -------------------------

fm20:	mov	bx,[es:ARENA.SIZE]	; Grab this block's Size,
	mov	es,cx			; Go back to prev MCB's address
	mov	[es:ARENA.SIGNATURE],al ; & move the SECOND sig here

	add	bx,[es:ARENA.SIZE]	; Size += first MCB's size
	add	bx,1			; And add one for the header
	mov	[es:ARENA.SIZE],bx	; Write the size

	; ---------------------------------------------------------------------

fm30:	mov	cx,es			; Put this address on the stack
	mov	dx,[es:ARENA.OWNER]	; And remember its owner

	mov	bx,es			; Move to the next MCB
	add	bx,[es:ARENA.SIZE]
	inc	bx
	mov	es,bx

	;cmp	al,'Z'
	cmp	al,arena_signature_end
	jne	short fm10		; If signature != 'Z', there are more.
fmX:	
	call	fm_unlink		; Unlink UMBs

	pop	es
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	retn

; -----------------------------------------------------------------------------
;*** fm_link - links UMBs not already linked in
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     fm_umb == 0 if not linked in previously, 1 if already linked in
; ERROR:    None
; USES:     AX, BX, fm_umb
; -----------------------------------------------------------------------------

fm_link:
	mov	ax,DOS_CHECK_UMBLINK ; 5802h
	int	21h			; Current link-state is now in al

	;putdata fm_umb,al		; So store it in fm_umb for later
	;
	;push	es
	;push	cs
	;pop	es
	;mov	[es:fm_umb],al
	;pop	es
	
	mov	[cs:fm_umb],al

	mov	ax,DOS_SET_UMBLINK ; 5803h
	mov	bx,1
	int	21h
	retn

; -----------------------------------------------------------------------------
;*** fm_unlink - unlinks UMBs if fm_umb is set to 0
; -----------------------------------------------------------------------------
; ENTRY:    fm_umb == 1 : leave linked, else unlink
; EXIT:     None
; ERROR:    None
; USES:     AX, BX
; -----------------------------------------------------------------------------

fm_unlink:
	xor	bx,bx
	
	;getdata bl,fm_umb		; fm_umb already has the old link-state
	;
	;push	ds
	;push	cs
	;pop	ds
	;mov	bl,[fm_umb]	
	;pop	ds
	
	mov	bl,[cs:fm_umb]

	mov	ax,DOS_SET_UMBLINK ; 5803h
	int	21h			; so just use that, and call int 21h
	retn

; 08/04/2019 - Retro DOS v4.0

; -----------------------------------------------------------------------------
;*** ParseVar - parses [/S][/L:umb[,size][;umb[,size]]*] and builds the table
; laid out in highvar.inc
; -----------------------------------------------------------------------------
; ENTRY:    ES:SI points to command tail of LoadHigh/DeviceHigh (whitespace ok)
; EXIT:     ES:SI points to first character in child program name
; ERROR:    ES:SI points to character which caused error, carry set, AX == code
; USES:     ES:SI, AX, flags, variables in highvar.inc
; -----------------------------------------------------------------------------
; Error codes (in AX if carry set on return):
;
PV_InvArg	equ	1	; Invalid argument passed
PV_BadUMB	equ	2	; Bad UMB number passed (duplicate?)
PV_InvSwt	equ	3	; Unrecognized switch passed
;
; This routine exects ES:SI to point to a string much like the following:
;    "/S/L:1,200;2 module options"
; Optionally, the string can begin with whitespace; neither /S nor /L is
; required, though that's what this routine is supposed to parse.
;
optS		equ	'S'	; /S
optL		equ	'L'	; /L:...
;
; -----------------------------------------------------------------------------
; LoadHigh has a list of arguments, returned by cparse, which is used to create
; a command-line for spawning a child process. For a typical LH command, say,
;     lh /l:1,1000;2 print/d:lpt2
; the arguments would look like (one per line):
;     lh
;     /l
;     1
;     1000
;     2
;     print
;     /d
;     :lpt2
; In short, if "print" were, say, "43", there'd be no way to determine which
; arg was the filename. So, inside this routine, we keep a running counter
; of the number of arguments LH will need to skip in order to get to the
; program name. The "lh" is implicit--it'll always have to skip that. So if
; there's no "/l" or "/s", fm_argc will be 0 ... other than that, 1 is added
; for:
;    Each /L
;    Each /S (there should be only one)
;    Each UMB number (they follow ":" or ";")
;    Each UMB size   (they follow ",")
; So, in the above example, fm_argc would be 4-- and LH would skip right to
; "print".  Note that InitVar initializes fm_argc to zero.
; -----------------------------------------------------------------------------

ParseVar:
	;pushreg <di, ds, es>
	push	di
	push	ds
	push	es

	push	es		; Make DS:SI point to it, as well as ES:SI
	pop	ds		; (regardless if we're in devhigh or loadhigh)
	cld

; ------------------------------------------------
; PV10--ES:SI = any whitespace on the command-line
; ------------------------------------------------

pv10:	lodsb			; here, ES:SI=="  /L..."--must eat whitespace
	call	isWhite
	jz	short pv10	;       ES:SI==" /L..."--keep eating.
	;cmp	al,'/'
	cmp	al,SWTCH
	je	short pv20	;       ES:SI=="/L..."--go process a switch

	dec	si		; Backup--it's now "odule options", and we need
	clc			; that "m" we just read (or whatever it is).
	jmp	short pvX	; Then return with carry clear == we're done.

pv20:	lodsb			; Just read 'S' or 'L', hopefully
	;toUpper al		; So we make it upper-case, and...
	and	al,0DFh
	;cmp	al,'S'
	cmp	al,optS		; just read 'S'?
	jne	short pv30

	;call	incArgc		; If it's /S, it's another arg for LH to skip.
	inc	byte [cs:fm_argc] ; 19/04/2019

	;putdata fUmbTiny,1	; /S, so ES:SI=="  /L..." or " module opts", or
	;
	;push	es
	;push	cs
	;pop	es
	;mov	[es:fUmbTiny],1	
	;pop	es

	mov	byte [cs:fUmbTiny],1

	jmp	short pv10	; possibly even "/L...".

pv30:	;cmp	al,'L'
	cmp	al,optL		; If it's not 'L' either, then 'tis a bad
	jne	short pvE1		; switch!

	;call	incArgc		; If it's /L, it's another arg for LH to skip.
	inc	byte [cs:fm_argc] ; 19/04/2019

	call	parseL
	jnc	short pv10	; If no carry, go back and look for more

	dec	si		; Else, back up and exit.
	jmp	short pvErr	; AX has already been set by parseL

pvE1:	;mov	ax,3
	mov	ax,PV_InvSwt	; Unrecognized switch passed
pvErr:	dec	si
	dec	si
	stc
pvX:	;popreg	<es, ds, di>
	pop	es
	pop	ds
	pop	di
	retn

; -----------------------------------------------------------------------------
;*** parseL - parses ":nnnn[,nnnn][;nnnn[,nnnn]]*" for ParseVar
; -----------------------------------------------------------------------------
; ENTRY:    ES:SI points to colon
; EXIT:     ES:SI points to first character not parsed
; ERROR:    Carry set; rewind three characters and return (see ParseVar)
; USES:     ES:SI, flags, AX, CX, DX, variables in highvar.inc
; -----------------------------------------------------------------------------
; If the string here is terminated with anything other than whitespace or a
; switchchar (perhaps it's /S or another /L:... ), then we return with carry
; set, indicating that they've screwed up the syntax.  The 3-character rewind
; makes sure the app /L: is reported as being the culprit.
; -----------------------------------------------------------------------------

parseL:
	lodsb
	cmp	al,':'		; Make sure they did /L:
	jne	short plE1	; If they didn't, return with carry set.

; ------------------------------------------
; PL10--ES:SI = a UMB number, after /L: or ;
; ------------------------------------------

pl10:	call	GetXNum		; After this, 'tis ",size" or ";umb" or " mod"
	jc	short plE2	; And error if it's a bad number.
	call	convUMB		; Convert any address to a UMB number

	mov	cl,al		; Remember the UMB number
	call	stowUMB		; Mark this UMB # as used;
	jc	short plE2	; If it was already marked, it'll error

	;call	incArgc		; Each UMB number is another arg for LH to skip
	inc	byte [cs:fm_argc] ; 08/04/2019 - Retro DOS v4.0

	lodsb
	cmp	al,';'		; Did "umb;" ?
	je	short pl10	; Yep: go back and get another UMB.

	call	isWhite		; Did "umb " ?
	jz	short plX	; Yep: return (it'll go back to whitespace)

	call	isEOL		; Did "umb" ?
	jz	short plSwX	; If so, backup and exit like everything's ok

	;cmp	al,'/'
	cmp	al,SWTCH 	; Did "umb/" ? (as in, "/L:1,100;2/S")
	je	short plSwX	; If so, back up ES:SI one character and return

	cmp	al,','		; Did "umb," ?
	jne	short plE1	; Just what the heck DID they do? Return error.

; --- Read a size -------------------------------------------------------------

	call	GetXNum		; Stop on "size;" or "size " or anything else
	jc	short plE1	; And error if it's a bad size.

	call	toPara		; Convert from bytes to paragraphs

	call	stowSiz		; CL still has the UMB number for this routine

	;call	incArgc		; Each UMB size is another arg for LH to skip
	inc	byte [cs:fm_argc] ; 08/04/2019 - Retro DOS v4.0

	lodsb
	cmp	al,';'		; They did "umb,size;", so get another UMB.
	je	short pl10	;

	call	isWhite		; Did it end with whitespace?
	jz	short plX	; If so, we're done here--go back.

	call	isEOL		; Did they do "umb,size" and end??? (stupid)
	jz	short plSwX	; If so, backup and exit like everything's ok

	;cmp	al,'/'
	cmp	al,SWTCH	; Did they do "umb,size/" ?
	je	short plSwX	; If so, again, we're done here.

plE1:	;mov	ax,1
	mov	ax,PV_InvArg	; If not, we don't know WHAT they did...
	dec	si
	stc
	retn

plE2:	;mov	ax,2
	mov	ax,PV_BadUMB	; In this case, they've specified a UMB twice
	stc
	retn
plSwX:	
	dec	si		; If we hit a '/' character, back up one char
				; so the whitespace checker will see it too.
plX:	clc			; Then just return with carry clear, so
	retn			; ParseVar will go about its business.

; -----------------------------------------------------------------------------
;*** incArgc - increments fm_argc, for use with LoadHigh command-line parsing
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     None
; ERROR:    None
; USES:     fm_argc, flags
; -----------------------------------------------------------------------------

;incArgc:
	;push	ax

	;;getdata al, fm_argc	; Obtain previous value of fm_argc,

	;mov	al,[cs:fm_argc]

	;inc	al		; Increment it,

	;;putdata fm_argc, al	; And store it right back.

	;mov	[cs:fm_argc],al

	;pop	ax
	;retn

; -----------------------------------------------------------------------------
;*** isEOL - returns with ZF set if AL contains CR or LF, or 0
; -----------------------------------------------------------------------------
; ENTRY:    AL contains character to test
; EXIT:     ZF set iff AL contains CR or LF, or 0
; ERROR:    None
; USES:     ZF
; -----------------------------------------------------------------------------

isEOL:
	cmp	al,0		; Null-terminator
	je	short ieX
	cmp	al,CR ; 0Dh	; Carriage Return
	je	short ieX
	cmp	al,LF ; 0Ah	; LineFeed
ieX:	
	retn

; -----------------------------------------------------------------------------
;*** isWhite - returns with ZF set if AL contains whitespace (or "=")
; -----------------------------------------------------------------------------
; ENTRY:    AL contains character to test
; EXIT:     ZF set iff AL contains space, tab, or equals
; ERROR:    None
; USES:     ZF
; -----------------------------------------------------------------------------

isWhite:
	cmp	al,' '		; Space
	je	short iwX
	cmp	al,'='		; Equals (treat as whitespace)
	je	short iwX
	cmp	al,tab ; 9	; Tab
iwX:	
	retn

; -----------------------------------------------------------------------------
;*** unMarkUMB - marks a given UMB as unused, even if previously marked used
; -----------------------------------------------------------------------------
; ENTRY:    AL contains UMB number
; EXIT:     None
; ERROR:    None
; USES:     Flags, variables in highvar.inc
; -----------------------------------------------------------------------------

unMarkUMB:
	push	ax
	push	bx
	push	di
	push	es

	push	cs
	pop	es

	xor	ah,ah
	mov	bx,ax

	; 19/04/2019
	
	;mov	byte [es:bx+UmbUsed],0
	mov	[es:bx+UmbUsed],ah ; 0

	cmp	[es:UmbLoad],al
	jne	short umu10

	;mov	[es:UmbLoad],0	; If unmarked the load UMB, load into convent.
	mov	[es:UmbLoad],ah ; 0
umu10:	
	pop	es
	pop	di
	pop	bx
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** stowUMB - marks a given UMB as used, if it hasn't been so marked before
;            -- accepts a UMB # in AL, and makes sure it hasn't yet been
; listed in the /L:... chain. If it's the first one specified, it sets UmbLoad
; to that UMB #... and in any case, it marks the UMB as specified.
; -----------------------------------------------------------------------------
; ENTRY:    AL contains UMB number, as specified by the user
; EXIT:     None
; ERROR:    Carry set if UMB # is less than 0 or >= MAXUMB (see highvar.inc)
; USES:     AX, Flags, variables in highvar.inc
; -----------------------------------------------------------------------------

stowUMB:
	cmp	al,MAXUMB ; 16
	jb	short su10
	stc
	retn			; Ooops-- UMB>=MAXUMB
su10:	
	push	bx
	push	di
	push	si
	push	ds
	push	es
	push	cs
	pop	es
	push	cs
	pop	ds
	
	;cmp	byte [UmbLoad],0FFh
	cmp	byte [UmbLoad],UNSPECIFIED ; If this, we haven't been here before
	jne	short su20
	mov	[UmbLoad],al	; So remember this UMB as the load UMB slot.
su20:	
	or	al,al		; If they gave UMB 0, there's really nothing
	jz	short su30	; that we should do here.

	mov	bl,al
	xor	bh,bh
	mov	ax,1		; Now, AX = 1, and BX = UMB Number

	xchg	[es:bx+UmbUsed],al

	or	al,al		; If it was already 1, then al==1... and that
	jz	short su30	; means an error.

	stc			; OOOPS!  This one's been used before.  :(
su30:	
	pop	es
	pop	ds
	pop	si
	pop	di
	pop	bx
	retn

; -----------------------------------------------------------------------------
;*** stowSiz - marks a given UMB as having a given minimum size
; -----------------------------------------------------------------------------
; ENTRY:    CL contains UMB number, AX contains size
; EXIT:     None
; ERROR:    None
; USES:     AX, DX, Flags, variables in highvar.inc
; -----------------------------------------------------------------------------

; 13/05/2019

stowSiz:
	push	bx
	;push	di ; ?
	push	es

	push	cs
	pop	es	

	mov	bl,cl			; Now bl==UMB number, AX==size
	mov	bh,0			;     bx==UMB number, AX==size
	shl	bl,1			;     bx==offset into array, AX=size
	mov	[es:bx+UmbSize],ax	; Store the size
	
	pop	es
	;pop	di ; ?
	pop	bx

	retn

; -----------------------------------------------------------------------------
;*** toDigit - converts a character-digit to its binary counterpart
;            -- verifies that CL contains a valid character-digit; if so, it
; changes CL to its counterpart binary digit ((CL-'0') or (CL-'A'+10)).
; A-F are considered valid iff gnradix is 16.
; -----------------------------------------------------------------------------
; ENTRY:    CL contains a digit ('0' to '9' or, if gnradix==16, 'A' to 'F')
; EXIT:     CL contains digit in binary (0 to 9 or, if gnradix==16, 0 to 15)
; ERROR:    Carry set indicates invalid digit; carry clear indicates good digit
; USES:     CL, Flags
; -----------------------------------------------------------------------------
; If the string is preceeded with "0x", the value is read as hexadecimal; else,
; as decimal. After a read, you may check the radix by examining gnradix--it
; will be 10 or 16.
; -----------------------------------------------------------------------------

gnradix:
	dw	0		; Must be a word--16x16 multiplication

toDigit:
	cmp	word [cs:gnradix],16
	jne	short td20	; Don't check hex digits if radix isn't 16

toDigit_hex:
	cmp	cl,'a'	; 61h
	jb	short td10
	cmp	cl,'f'	; 66h
	ja	short tdE	; Nothing valid above 'z' at all...
	sub	cl,'a'-10 ; 57h	; Make 'a'==10 and return.
	;clc			; <- CLC is implicit from last SUB
	retn
td10:	
	cmp	cl,'A'  ; 41h
	jb	short td20	; Below 'A'?  Not a letter...
	cmp	cl,'F'	; 46h
	ja	short tdE	; Above 'F'?  Not a digit.
	sub	cl,'A'-10 ; 37h	; Make 'A'==10 and return.
	;clc			; <- CLC is implicit from last SUB
	retn
toDigit_dec:
td20:	
	cmp	cl,'0'		; If less than zero,
	;jb	short tdE	; Done.
	jb	short tdEr ; 08/04/2019
	cmp	cl,'9'		; Or, if greater than nine,
	ja	short tdE	; Done.
	sub	cl,'0'	; 30h	; Okay--make '0'==0 and return.
	;clc			; <- CLC is implicit from last SUB
	retn
tdE:	
	stc
tdEr:		; 08/04/2019 - Retro DOS v4.0	
	retn

; -----------------------------------------------------------------------------
;*** GetXNum - reads a 32-bit ASCII number at ES:SI and returns it in DX:AX
; -----------------------------------------------------------------------------
; ENTRY:    ES:SI points to an ascii string to scan
; EXIT:     ES:SI moved to first invalid digit, DX:AX contains value read
; ERROR:    Carry set if # is too big, or has no digits (EOL possibly)
; USES:     ES:SI, DX, AX, Flags, gnradix
; -----------------------------------------------------------------------------
; If the string is preceeded with "0x", the value is read as hexadecimal; else,
; as decimal. After a read, you may check the radix by examining gnradix--it
; will be 10 or 16.
; -----------------------------------------------------------------------------

; 08/04/2019 - Retro DOS v4.0

GetXNum:
	;pushreg <bx, cx, ds>
	push	bx
	push	cx
	push	ds

	cld
	xor	ax,ax
	xor	bx,bx
	xor	cx,cx
	xor	dx,dx			; Start with 0 (makes sense)

	mov	word [cs:gnradix],10	; And default to a radix of 10 (dec)

	mov	cl,[es:si]		; Now AX=0, BX=0, CH=0/CL=char, DX=0
	;call	toDigit
	call	toDigit_dec
	jc	short gxnE		; If it's not a digit, leave now.

	or	cl,cl
	jnz	short gxn20		; Doesn't have '0x'
	mov	cl,[es:si+1]
	cmp	cl,'x'			; Either 'x'...
	je	short gxn10
	cmp	cl,'X'			; ...or 'X' means it's hexadecimal
	jne	short gxn20

gxn10:	
	mov	word [cs:gnradix], 16
	inc	si			; Since we read "0x", march over it.
	inc	si

; ------------------------------------------------------
; GXN20--ES:SI = a digit in a number; if not, we're done
;        DX:AX = current total
;        BX    = 0
;        CH    = 0
; ------------------------------------------------------

gxn20:	
	mov	cl,[es:si]	; Now DX:AX=current total, CH=0/CL=char
	inc	si

	call	toDigit		; Accepts only valid digits, A-F -> 10-16
	jc	short gxnQ	; <- Ah... wasn't a digit. Stop.

	call	mul32		; Multiply DX:AX by gnradix
	jc	short gxnX	; (if it's too big, error out)

	add	ax,cx		; Add the digit
	adc	dx,bx		; (BX is 0!)--Adds 1 iff last add wrapped
	;jc	short gxnX	; If _that_ wrapped, it's too big.
	;jmp	short gxn20
	jnc	short gxn20
gxnE:	
	;stc			; In this case, we need to set the carry
	jmp	short gxnX	; and leave--there were no digits given.
gxnQ:	
	dec	si		; Don't read in the offensive character.
	clc			; And clear carry, so they know it's okay.
gxnX:	
	pop	ds
	pop	cx
	pop	bx
	retn

; -----------------------------------------------------------------------------
;*** mul32 - multiplies the number in DX:AX by gnradix
; -----------------------------------------------------------------------------
; ENTRY:   DX:AX = the number to be multiplied, BX = 0, gnradix = multiplier
; EXIT:    DX:AX has been multiplied by gnradix if carry clear; BX still 0
; ERROR:   Carry set if number was too large
; USES:    Flags, AX, DX
; -----------------------------------------------------------------------------

mul32:
	push	ax		; DX=old:hi, AX=old:lo, TOS=old:lo, BX=0
	mov	ax,dx		; DX=old:hi, AX=old:hi, TOS=old:lo, BX=0
	mul	word [cs:gnradix] ; DX=?, AX=new:hi, TOS=old:lo, BX=0
	jc	short m32E	; Too big?

	mov	dx,ax		; DX=new:hi, AX=new:hi, TOS=old:lo, BX=0
	pop	ax		; DX=new:hi, AX=old:lo, TOS=orig, BX=0

	xchg	dx,bx		; DX=0, AX=old:lo, TOS=orig, BX=new:hi
	mul	word [cs:gnradix] ; DX=carry,  AX=new:lo, TOS=orig, BX=new:hi
	xchg	dx,bx		; DX=new:hi, AX=new:lo, TOS=orig, BX=carry
	add	dx,bx		; DX=new:hi, AX=new:lo, TOS=orig, BX=carry
	xor	bx,bx		; DX=new:hi, AX=new:lo, TOS=orig, BX=0
	retn
m32E:	
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** toPara - divides DX:AX by 16; result in AX only (discards extra DX data)
; -----------------------------------------------------------------------------
; ENTRY:   DX:AX = the number to be divided
; EXIT:    Interpereting DX:AX as bytes, AX=paragraph equivalent, 0xFFFF max
; ERROR:   None
; USES:    Flags, AX, DX
; -----------------------------------------------------------------------------
; Note: The 386 has a 32-bit SHR, which would work perfectly for this... but we
;       can't ensure a 386 host machine. Sorry.
; -----------------------------------------------------------------------------

toPara:
	push	cx		; DX:AX=HHHH hhhh hhhh hhhh:LLLL llll llll llll

	mov	cl,4		;
	shr	ax,cl		; DX:AX=HHHH hhhh hhhh hhhh:0000 LLLL llll llll
	xchg	ax,dx		; DX:AX=0000 LLLL llll llll:HHHH hhhh hhhh hhhh
	mov	cl,12
	shl	ax,cl		; DX:AX=0000 LLLL llll llll:hhhh 0000 0000 0000
	or	ax,dx		;    AX=hhhh LLLL llll llll

	pop	cx
	retn

; -----------------------------------------------------------------------------
;*** UmbHead - returns in AX the address of the first UMB block (0x9FFF)
; -----------------------------------------------------------------------------
; ENTRY:  Nothing
; EXIT:   AX contains 0x9FFF for most systems
; ERROR:  Carry set if pointer is 0xFFFF (if not set up yet--DH runs into this)
; USES:   Flags, AX
; -----------------------------------------------------------------------------
; Early in the boot-cycle, the pointer used to obtain this value isn't set up;
; to be precise, before a UMB provider is around. In this event, the pointer
; is always set to 0xFFFF; it changes once a provider is around. On most
; machines (all of 'em I've seen), it changes to 0x9FFF at that point.
; -----------------------------------------------------------------------------

UmbHead:
	; 13/05/2019 (because of callers, pushs & pops are not needed here)

	;push	si ; ?
	;push	ds ; ? 
	;push	es
	;push	bx ; *	

	; 09/04/2019
	; !!! No need to save es,bx,ds,si above !!! (es,bx are changed here)

	mov	ah,GET_IN_VARS		; Call int 21h, function 52h...
	int	21h

	mov	ax,[es:DOS_UMB_HEAD]	; And read what's in ES:[008C]
	cmp	ax,0FFFFh
	je	short uhE		; If it's 0xFFFF, it's an error...

	clc				; Else, it isn't (CLC done by prev cmp)
	jmp	short uhX
uhE:	
	stc
uhX:	
	;pop	bx ; *
	;pop	es
	;pop	ds ; ?
	;pop	si ; ?
	retn

; -----------------------------------------------------------------------------
;*** isSysMCB - sets ZF if ES points to an MCB owned by "SC" + (8 or 9)
; -----------------------------------------------------------------------------
; ENTRY:  ES:0 should point to a valid MCB
; EXIT:   ZF set if owned by SC+8 or SC+9 (for japan)
; USES:   Flags
; -----------------------------------------------------------------------------

isSysMCB:
	;push	ax

	;mov	ax,[es:ARENA.OWNER]	; Check the owner...
	;cmp	ax,SystemPSPOwner	; 8 (for US OR Japan) is valid
	;je	short ism10
	;cmp	ax,JapanPSPOwner	; 9 (for Japan) is valid
	;;je	short ism10
	;;jmp	short ismX		; Anything else isn't.
	;jne	short ismX
	cmp	word [es:ARENA.OWNER],SystemPSPOwner ; 8  ; 09/04/2019
	jne	short ismX 
ism10:	
	;mov	ax,[es:ARENA.NAME]	; Check the name...
	;cmp	ax,'SC' ; 4353h
	cmp	word [es:ARENA.NAME],'SC'
ismX:	
	;pop	ax
	retn

; 09/04/2019 - Retro DOS v4.0

; -----------------------------------------------------------------------------
;*** AddrToUmb - converts a segment address in AX to its appropriate UMB number
; -----------------------------------------------------------------------------
; ENTRY:  AX contains a segment address
; EXIT:   AX will contain the UMB number which contains the address (0==conv)
; ERROR:  If the address is above UM Range, AX will return as FFFF.
; USES:   Flags, AX
; -----------------------------------------------------------------------------
; An address in the following areas is treated as:
;    0      <-> umbhead (0x9FFF)          = Conventional memory
;    0x9FFF <-> addr of first UM sys MCB  = UMB #1
;      ...
;    addr of last UM sys MCB <-> TOM      = invalid; returns #0xFFFF
; -----------------------------------------------------------------------------

AddrToUmb:
	push	cx
	push	dx
	push	es

	mov	dx,ax		; DX = address to search for

	call	UmbHead		; AX = first segment
	jc	short atuE	; If it couldn't get it, error out.

	mov	es,ax		; ES = first UMB segment
	xor	cx,cx		; Pretend we're on UMB 0 for now... (cx = UMB#)

; ----------------------------------------
; ATU10--ES - Current MCB address
;        DX - Address given for conversion
;        CX - Current UMB #
; ----------------------------------------

atu10:	mov	ax,es
        cmp	ax,dx		; Present segment >= given segment?
	jae	short atuX	; Yep--done.

	call	isSysMCB	; Returns with ZF set if this is a system MCB
	jnz	short atu20

	inc	cx		; If it _was_ a system MCB, we're in a new UMB.
atu20:	
	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end  ; 'Z'
	je	short atu30		; 'Z' means this was the last MCB... that's it.

	;NextMCB es, ax

	mov     ax,es
	;add	ax,[es:3]
	add     ax,[es:ARENA.SIZE]
	inc     ax
	mov     es,ax
	
	jmp	short atu10

; -----------------------------------------------------------------------------
; if we get to atu30, they specified a number that was past the last MCB.
; make sure it's not _inside_ that MCB before we return an error condition.
; -----------------------------------------------------------------------------

atu30:	
	mov	ax,es
	add	ax,[es:ARENA.SIZE]
	cmp	ax,dx		; Present >= given?
	jae	short atuX	; Yep! It _was_ inside.
atuE:	
	xor	cx,cx		; Else, fall through with UMB # == -1
	dec	cx		; (that makes it return 0xFFFF and sets CF)
atuX:	
	mov	ax, cx		; Return the UMB number in AX
	
	pop	es	
	pop	dx
	pop	cx
	retn

; -----------------------------------------------------------------------------
;*** convUMB - checks after GetXNum to convert an address to a UMB number
;            -- if GetXNum read a hex number, we interperete that as a segment
; address rather than a UMB number... and use that address to look up a UMB.
; This routine checks for that condition and calls AddrToUmb if necessary.
; -----------------------------------------------------------------------------
; ENTRY:  AX contains a UMB number or segment, gnradix has been set by GetXNum
; EXIT:   AX will contain a UMB number
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

convUMB:
	cmp	word [cs:gnradix],16
	jne	short cu10	; If it didn't read in hex, it's not an address
	call	AddrToUmb	; Else, convert the address to a UMB number
	cmp	ax,0FFFFh
	jne	short cu10
	inc	ax		; If too high, ignore it (make it conventional)
cu10:	
	retn

; -----------------------------------------------------------------------------
;*** setUMBs - links umbs and sets allocation strategy for a load
;            -- if LoadHigh, the allocation strategy MAY be LOW_FIRST instead
; of the usual HIGH_FIRST.  See the code.
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   None
; ERROR:  None
; USES:   Flags, fm_umb, fm_strat
; -----------------------------------------------------------------------------

setUMBs:
	push	ax
	push	bx
	call	fm_link
	pop	bx
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** loadLow - returns AL==0 if UMB0 == 0, else AL==1
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   AL==0 if mem strategy should be set to LOW_FIRST, else AL==1
;         Carry set if UMB0 not specified (_NOT_ an error)
; ERROR:  None
; USES:   Flags, fm_strat, fm_umb
; -----------------------------------------------------------------------------
; We want to set the memory strategy to LOW_FIRST if the user specified a
; load UMB, and it is 0.  That 0 can be either from the user having _specified_
; zero (/L:0;...), or from having specified a too-big min size (/L:1,99999999)
; such that the load UMB is too small, and shouldn't be used.
; -----------------------------------------------------------------------------

loadLow:
	;push	ds
	;push	cs		; Point DS into appropriate data segment
	;pop	ds	

	;mov	al,[UmbLoad]
	mov	al,[cs:UmbLoad]
	cmp	al,UNSPECIFIED ; 0FFh, -1
	jne	short ll10

	stc
ll15:
	mov	al,1		; Return with AL==1 && STC if no UMBs specified
	;stc
	;jmp	short llX
	retn
ll10:	
	or	al,al		; AL=the load UMB: Is it == 0?
	;jz	short llX	; Yep... CF==0 (from OR) && AL=0, so just exit

	jnz	short ll15	; 09/04/2019 - Retro DOS v4.0
	retn

	;mov	al,1
	;clc
;llX:
	;pop	ds		; Return DS to where it was
	;retn

; -----------------------------------------------------------------------------
;*** HideUMBs - links UMBs and hides upper-memory as appropriate
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   None
; ERROR:  None
; USES:   Flags, fm_strat, fm_umb
; -----------------------------------------------------------------------------

HideUMBs:
	push	ax
	push	cx
	push	ds
	push	es

	call	UmbTest		; See if we REALLY linked in anything...
	jc	short husX	; ...if not, there's nothing for us to do.

	call	FixMem		; Concatenate adjacent free MCBs in upper mem
	call	setUMBs		; Link UMBs and set memory-allocation strategy

	;putdata fInHigh,1	; Remember that we're now running high
	mov	byte [cs:fInHigh],1

	;call	GetLoadUMB	; See if they gave us a list to leave free
	mov	al,[cs:UmbLoad] ; 09/04/2019 - Retro DOS v4.0

	cmp	al,UNSPECIFIED	; If they didn't,
	je	short husX	; then we shouldn't do this loop:

	xor	cx,cx

; -----------------------------------------------
; HUS10-CX - UMB number (after inc, 1==first UMB)
; -----------------------------------------------

hus10:	inc	cx		; For each UMB:
	cmp	cx,MAXUMB ; 16
	jae	short hus20

	mov	al,cl		; (stopping as soon as we're outside of the
	push	es
	call	findUMB		; valid range of UMBs)
	pop	es		; push/pop: trash what findumb finds.  :-)
	jc	short hus20

	call	_hideUMB_	; hide what we need to hide.

	jmp	short hus10
hus20:	
	;call	GetLoadUMB	; Now check if they offered /L:0
	mov	al,[cs:UmbLoad] ; 09/04/2019 - Retro DOS v4.0	

	or	al,al		; --Is the load UMB 0? (-1==unspecified)
	jnz	short husX	; If not, we're done.

	call	hl_unlink	; If so, however, fix UMBs and strategy.
husX:	
	pop	es
	pop	ds
	pop	cx
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** GetLoadUMB - Returns the load UMB number in AL (-1 if not specified)
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   AL == load UMB
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

;GetLoadUMB:
;	;getdata al, UmbLoad
;	push	ds
;	push	cs
;	pop	ds
;	mov	al,[UmLoad]
;	pop	ds
;	retn

; -----------------------------------------------------------------------------
;*** GetLoadSize - Returns the load UMB minimum size (0 if not specified)
; -----------------------------------------------------------------------------
; ENTRY:  None
; EXIT:   AX == load UMB minimum size
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

GetLoadSize:
	; 09/04/2019 - Retro DOS v4.0
	mov	al,[cs:UmbLoad]
	jmp	short GetSize

	;push	bx
	;;push	si
	;push	ds
	;push	cs
	;pop	ds

	;mov	al,[UmbLoad]

	;xor	ah,ah			;    ax==UMB
	;mov	bx,UmbSize		;    bx==array
	;shl	al,1	                ;    ax==offset
	;;add	ax,bx			;    ax==element index
	;;mov	si,ax			; ds:si==element index

	;;lodsw				;    ax==size

	;add	bx,ax
	;mov	ax,[bx]

	;pop	ds
	;;pop	si
	;pop	bx
	;retn

; -----------------------------------------------------------------------------
;*** GetSize - Returns the UMB in AL's minimum size (0 if not specified)
; -----------------------------------------------------------------------------
; ENTRY:  AL == a UMB number
; EXIT:   AX == UMB minimum size, as specified by the user
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

GetSize:
	; 09/04/2019 - Retro DOS v4.0

	push	bx
	;push	si
	;push	ds
	;push	cs
	;pop	ds

	xor	ah,ah			;    ax==UMB
	mov	bx,UmbSize		;    bx==array
	shl	al,1	                ;    ax==offset
	;add	ax,bx			;    ax==element index
	;mov	si,ax			; ds:si==element index

	;lodsw				;    ax==size

	add	bx,ax
	;mov	ax,[bx]
	mov	ax,[cs:bx]

	;pop	ds
	;pop	si
	pop	bx
	retn

; -----------------------------------------------------------------------------
;*** StoLoadUMB - Overrides the load UMB number with what's in AL
; -----------------------------------------------------------------------------
; ENTRY:   AL == new load UMB
; EXIT:    None
; ERROR:   None
; USES:    Flags, AX
; -----------------------------------------------------------------------------
; CAUTION: Should only be used if /L:... was used.  Logically, that is the only
;          time you would ever need this, so that's okay.
; -----------------------------------------------------------------------------

;StoLoadUMB:
;	;putdata UmbLoad, al
;	push	es
;	push	cs
;	pop	es
;	mov	[es:UmbLoad],al
;	pop	es
;	retn

; -----------------------------------------------------------------------------
;*** StoLoadSize - Overrides the load UMB minimum size with what's in AX
; -----------------------------------------------------------------------------
; ENTRY:  AL == new load size
; EXIT:   None
; ERROR:  None
; USES:   Flags, AX
; -----------------------------------------------------------------------------

StoLoadSize:
	push	dx

	;getdata dl, UmbLoad		; Put UMB# in DL and size in AX
	;
	;push	ds
	;push	cs
	;pop	ds
	;mov	dl,[UmbLoad]
	;pop	ds	

	mov	dl,[cs:UmbLoad]

	cmp	dl,UNSPECIFIED ; 0FFh
	je	short sls10

	call	stowSiz			; We've got a function to do just this
sls10:	
	pop	dx
	retn

; -----------------------------------------------------------------------------
;*** hideUMB - marks as HIDDEN all FREE elements in UMB passed as AL
; -----------------------------------------------------------------------------
; ENTRY:    AL must indicate a valid UMB; 0==conv && is invalid.
; EXIT:     None; free elements in UMB marked as hidden
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

hideUMB:
	push	ax
	push	es

	call	findUMB		; Returns with carry if err, else ES == MCB
	jc	short huX

; ------------------------------------------------
; HU10--ES - MCB inside UMB; if it's a system MCB,
;            we're not in the same UMB, so exit.
; ------------------------------------------------

hu10:	call	isSysMCB	; Returns with ZF set if owner is SYSTEM
	jz	short huX	; If it is, we've finished the UMB.
	;call	isFreeMCB	; Returns with ZF set if owner is 0
	or	word [es:ARENA.OWNER],0
	jnz	short hu20

	call	hideMCB
hu20:	
	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end  ;'Z'
	jz	short huX	; 'Z' means this was the last MCB... that's it.

	;NextMCB es, ax		; Go on forward.
	mov     ax,es
	;add	ax,[es:3]
	add     ax,[es:ARENA.SIZE]
	inc     ax
	mov     es,ax

	jmp	short hu10
huX:	
	pop	es
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** isTiny - returns with ZF set if user didn't specify /S
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     ZF set if user DIDN'T specify /S
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isTiny:
	push	ax

	;getdata al, fUmbTiny
	;
	;push	ds
	;push	cs
	;pop	ds
	;mov	al,[fUmbTiny]
	;pop	ds

	mov	al,[cs:fUmbTiny]		

	or	al,al
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** isFreeMCB - returns with ZF set if current MCB (ES:0) is FREE
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     ZF set if MCB is free, else !ZF
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

;isFreeMCB:
;	or	word [es:ARENA.OWNER],0
;	retn

; -----------------------------------------------------------------------------
;*** hideMCB - marks as HIDDEN the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB marked as HIDDEN
; ERROR:    None
; USES:     None
; -----------------------------------------------------------------------------

hideMCB:
	mov	word [es:ARENA.OWNER],SystemPSPOwner ; 8
	mov	word [es:ARENA.NAME+0], 'HI' ; 4948h
	mov	word [es:ARENA.NAME+2], 'DD' ; 4444h
	mov	word [es:ARENA.NAME+4], 'EN' ; 4E45h
	mov	word [es:ARENA.NAME+6], '  ' ; 2020h	
	retn

; -----------------------------------------------------------------------------
;*** unHideMCB - marks as FREE the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB marked as FREE
; ERROR:    None
; USES:     None
; -----------------------------------------------------------------------------

unHideMCB:
	push	ax
	mov	word [es:ARENA.OWNER],FreePSPOwner ; 0
	mov	ax,'  ' ; 2020h
	mov	[es:ARENA.NAME+0],ax
	mov	[es:ARENA.NAME+2],ax
	mov	[es:ARENA.NAME+4],ax
	mov	[es:ARENA.NAME+6],ax
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** findUMB - makes ES:0 point to the first MCB in UMB given as AL
;            -- returns UmbHEAD pointer (0x9FFF) if passed AL==0
; -----------------------------------------------------------------------------
; ENTRY:    AL should be to a valid UMB number
; EXIT:     ES:0 points to first MCB in UMB (_not_ the 8+SC MCB that heads it)
; ERROR:    Carry set if couldn't reach UMB (too high)
; USES:     Flags, ES
; -----------------------------------------------------------------------------

findUMB:
	push	ax
	push	cx
	push	dx

	xor	ah,ah		; Zap ah, so al==ax

	mov	dx,ax		; Store the to-be-found UMB number in DX

	call	UmbHead		; Returns first UMB segment in AX
	mov	es,ax
	xor	cx,cx		; Pretend we're on UMB 0 for now...

; ---------------------------------------------
; FU10--CX - This UMB number; 0 == conventional
;       DX - The UMB number they're looking for
;       ES - The current MCB address
; ---------------------------------------------

fu10:	
	cmp	cx,dx		; If CX==DX, we've found the UMB we're
	je	short fuX	; searching for--so exit.

	call	isSysMCB	; Returns with ZF set if owner is SYSTEM
	jnz	short fu20

	inc	cx		; If it _was_ SYSTEM, we're in a new UMB.
fu20:	
	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end ; 'Z'
	je	short fuE	; 'Z' means this was the last MCB... that's it.

	;NextMCB es, ax		; Go on forward.
	mov     ax,es
	;add	ax,[es:3]
	add     ax,[es:ARENA.SIZE]
	inc     ax
	mov     es,ax

	jmp	short fu10
fuE:	
	stc
fuX:
	pop	dx
	pop	cx
	pop	ax		; The address is already in ES.
	retn

; -----------------------------------------------------------------------------
;*** BigFree - makes ES:0 point to the largest free MCB in UMB given as AL
; -----------------------------------------------------------------------------
; ENTRY:    AL should be to a valid UMB number
; EXIT:     ES:0 points to largest free MCB in UMB, AX returns its size
; ERROR:    Carry set if couldn't reach UMB (0 or too high)
; USES:     Flags, ES
; -----------------------------------------------------------------------------

BigFree:
	push	bx
	push	cx

	call	findUMB			; Returns with CF if err, else ES==MCB
	jc	short bfX		; (would be "jc bfE"; it just does stc)

	xor	bx,bx			; Segment address of largest free MCB
	xor	cx,cx			; Size of largest free MCB

; ---------------------------------------------
; BF10--ES - Current MCB address
;       BX - Address of largest free MCB so far
;       CX - Size of largest free MCB so far
; ---------------------------------------------

bf10:	
	call	isSysMCB		; If we've left the MCB, we're done.
	jz	short bf30

	;call	isFreeMCB		; Returns with ZF set if owner is 0
	or	word [es:ARENA.OWNER],0
	jnz	short bf20

	mov	ax,[es:ARENA.SIZE]
	;cmp	cx,[es:ARENA.SIZE]	; Compare sizes...
	cmp	cx,ax
	jg	short bf20		; Unless we're bigger,

	mov	bx,es			; Store this new element's address,
	;mov	cx,[es:ARENA.SIZE]	; and its size.
	mov	cx,ax

bf20:	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end	; 'Z'
	jz	short bf30		; 'Z' means this was the last MCB.

	;NextMCB es,ax		; Go on forward.
	mov     ax,es
	;add	ax,[es:3]
	add     ax,[es:ARENA.SIZE]
	inc     ax
	mov     es,ax

	jmp	short bf10

bf30:	mov	es,bx			; Return the address
	mov	ax,cx			; Return the size
	or	bx,bx
	jnz	short bfX		; (if size==0, there's nothing free)
bfE:	
	stc
bfX:
	pop	cx
	pop	bx
	retn

; -----------------------------------------------------------------------------
;*** isSpecified - sets ZF if UMB in AL wasn't specified in DH/LH line.
; -----------------------------------------------------------------------------
; ENTRY:    AL should be to a valid UMB number
; EXIT:     ZF set if UMB wasn't specified, ZF clear if it was
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isSpecified:
	push	ax

	xor	bh,bh
	mov	bl,al

	;getdata al, DS:UmbUsed[bx]
	;
	;push	ds
	;push	cs
	;pop	ds
	;mov	al,[bx+UmbUsed]
	;pop	ds
	
	mov	al,[cs:bx+UmbUsed]
	or	al,al			; Sets ZF if al==0 (ie, if unspecified)

	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** shrinkMCB - breaks an MCB into two pieces, the lowest one's size==AX
; -----------------------------------------------------------------------------
; ENTRY:    AX == new size, ES:0 == current MCB
; EXIT:     None; MCB broken if carry clear
; ERROR:    Carry set if MCB isn't as large as AX+0x20 (not a useful split)
; USES:     Flags
; -----------------------------------------------------------------------------
; If the size of the to-be-split MCB isn't at least 0x20 bytes greater than
; the specified new size, the split is useless; if it's only 0x10 bytes, that
; 0x10 will be used to make a header that mentions a 0-byte free space, and
; that just sucks up 0x10 bytes for nothing. So we make 0x20 bytes the
; minimum for performing a split.
; -----------------------------------------------------------------------------

MIN_SPLIT_SIZE	equ	20h

shrinkMCB:
	;pushreg <bx, cx, es>
	push	bx
	push	cx
	push	es

	mov	bx,ax			; Move things around... and
	mov	ax,es			; save this one for later.

	mov	cx,[es:ARENA.SIZE]
	sub	cx,MIN_SPLIT_SIZE ; 32
	cmp	bx,cx			; {New size} vs {Current Size-20h}
	ja	short smE		; if wanted_size > cur-20h, abort.

	mov	dl,[es:ARENA.SIGNATURE]
	mov	cx,[es:ARENA.SIZE]

	mov	[es:ARENA.SIZE],bx
	mov	byte [es:ARENA.SIGNATURE],'M'

	add	ax,bx
	inc	ax
	mov	es,ax			; Move to new arena area

	mov	ax,cx
	sub	ax,bx
	dec	ax			; And prepare the new size

	mov	[es:ARENA.SIGNATURE],dl
	mov	word [es:ARENA.OWNER],0
	mov	[es:ARENA.SIZE],ax
	mov	ax,'  ' ; 2020h
	mov	[es:ARENA.NAME+0],ax
	mov	[es:ARENA.NAME+2],ax
	mov	[es:ARENA.NAME+4],ax
	mov	[es:ARENA.NAME+6],ax

	clc
	jmp	short smX
smE:	
	stc
smX:	
	;popreg	<es, cx, bx>
	pop	es
	pop	cx
	pop	bx
	retn

; -----------------------------------------------------------------------------
;*** hideUMB? - hides as appropriate the UMB in CL
; -----------------------------------------------------------------------------
; ENTRY:    CL should be to a valid UMB number, and AX to its address (findUMB)
; EXIT:     None; UMB is hidden as necessary
; ERROR:    None
; USES:     Flags, AX, CX
; -----------------------------------------------------------------------------
; PRIMARY LOGIC:
;
; If the UMB is specified in the DH/LH statement, then:
;    If the largest free segment is too small (check specified size), then:
;       Pretend it wasn't ever specified, and fall out of this IF.
;    Else, if largest free segment is LARGER than specified size, then:
;       If /S was given on the command-line, then:
;          Break that element into two pieces
;          Set a flag that we're shrinking
;       Endif
;    Endif
; Endif
; If the UMB is NOT specified (or was removed by the above):
;    Hide all free elements in the UMB
;    If the flag that we're shrinking was set, then:
;       UN-hide the lower portion of the shrunken UMB
;    ENDIF
; ENDIF
; -----------------------------------------------------------------------------

_hideUMB_:
	push	bx
	push	dx
	push	es

	mov	al,cl
	call	isSpecified	; Returns ZF set if al's umb was NOT specified
	jz	short hu_20

	mov	al,cl		; Retrieve the size of the largest
	call	BigFree		; free element in AX; put its address in ES
	jc	short hu_20	; Oops. Errors mean skip this part.

	push	ax		; TOS==size of BigFree in UMB (popped as BX)
	mov	al,cl		; Retrieve the user's specified
	call	GetSize		; minimum size for this umb (into AX)
	pop	bx		; Now BX==BigFree, AX==Specified Size

	or	ax,ax		; If they didn't specify one,
	jz	short hu_20	; Skip over all this.

	cmp	ax,bx		; Ah... if (specified > max free)
	jbe	short hu_10

	mov	al,cl		;   Then mark that UMB as unused. Nya nya.
	call	unMarkUMB
	jmp	short hu_20
hu_10:	
	call	isTiny		; Returns ZF clear if user specified /S
	jz	short hu_20

	call	shrinkMCB	; They specified /S, so shrink the MCB to AX
	jc	short hu_20	; Ah... if didn't shrink after all, skip this:

	mov	dx,es
	jmp	short hu_30	; Skip the spec check.. we wanna hide this one.

hu_20:	mov	ax,cx
	call	isSpecified	; If they specified this UMB, we're done...
	jnz	short hu_X	; so leave.

	xor	dx,dx
hu_30:	
	mov	al,cl

	call	hideUMB		; Hides everything in UMB #al

	or	dx,dx		; Did we shrink a UMB? If not, DX==0,
	jz	short hu_X	; So we should leave.

	mov	es,dx		; Ah, but if it isn't, DX==the MCB's address;
	call	unHideMCB	; Un-hides the lower portion of that MCB.
hu_X:	
	pop	es
	pop	dx
	pop	bx
	retn

; -----------------------------------------------------------------------------
;*** UnFreeze - Marks FROZEN elements as FREE
; -----------------------------------------------------------------------------
; Entry:  None
; Exit:   None; all 8+FROZEN elements are marked as FREE, from any UMB.
; Error:  None
; Uses:   Flags
; -----------------------------------------------------------------------------

UnFreeze:
	push	ax
	push	es

	call	UmbHead		; Returns with carry if err, else ES == MCB
	jc	short ufX

	mov	es,ax

; ------------------------------
; UF10--ES - Current MCB address
; ------------------------------

uf10:	call	isFrozMCB	; Returns with ZF set if MCB is FROZEN
	jnz	short uf20
	call	unHideMCB
uf20:	
	mov	al,[es:ARENA.SIGNATURE]

	cmp	al,arena_signature_end ; 'Z'
	jz	short ufX	; 'Z' means this was the last MCB... that's it.

	;NextMCB es, ax		; Go on forward.
	mov     ax,es
	;add	ax,[es:3]
	add     ax,[es:ARENA.SIZE]
	inc     ax
	mov     es,ax

	jmp	short uf10
ufX:	
	pop	es
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** isFrozMCB - returns with ZF set if current MCB (ES:0) is FROZEN
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     ZF set if MCB is frozen, else !ZF
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isFrozMCB:
	;push	ax

	;mov	ax,[es:ARENA.OWNER]	; Check the owner...
	;cmp	ax,SystemPSPOwner	; 8 (for US OR Japan) is valid
	cmp	word [es:ARENA.OWNER],SystemPSPOwner
	jne	short ifmX

	;mov	ax,[es:ARENA.NAME+0]
	;cmp	ax,'FR' ; 5246h
	cmp	word [es:ARENA.NAME+0],'FR'
	jne	short ifmX
	;mov	ax,[es:ARENA.NAME+2]
	;cmp	ax,'OZ' ; 5A4Fh
	cmp	word [es:ARENA.NAME+2],'OZ'
	jne	short ifmX
	;mov	ax,[es:ARENA.NAME+4]
	;cmp	ax,'EN' ; 4E45h
	cmp	word [es:ARENA.NAME+4],'EN'
	jne	short ifmX
	;mov	ax,[es:ARENA.NAME+6]
	;cmp	ax,'  ' ; 2020h
	cmp	word [es:ARENA.NAME+6],'  '
ifmX:	
	;pop	ax
	retn

; -----------------------------------------------------------------------------
;*** frezMCB - marks as 8+FROZEN the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB frozen
; ERROR:    None
; USES:     None
; -----------------------------------------------------------------------------

frezMCB:
	mov	word [es:ARENA.OWNER],SystemPSPOwner ; 8
	mov	word [es:ARENA.NAME+0],'FR'
	mov	word [es:ARENA.NAME+2],'OZ'
	mov	word [es:ARENA.NAME+4],'EN'
	mov	word [es:ARENA.NAME+6],'  '
	retn

; -----------------------------------------------------------------------------
;*** FreezeUM - Marks FROZEN all UM elements now FREE, save those in load UMB
; -----------------------------------------------------------------------------
; Entry:  None
; Exit:   None; all free elements not in load UMB marked as 8+FROZEN
; Error:  None
; Uses:   Flags
; -----------------------------------------------------------------------------

FreezeUM:
	push	ax
	push	cx
	push	dx
	push	es

	;call	GetLoadUMB
	mov	al,[cs:UmbLoad] ; 19/04/2019 - Retro DOS v4.0

	xor	ah,ah		; Zap ah, so al==ax
	mov	dx,ax		; Store the load UMB in DX, so we can skip it

	call	UmbHead		; Returns first UMB segment in AX
	mov	es,ax
	xor	cx,cx		; Pretend we're on UMB 0 for now...

; -----------------------------------------
; FUM10--ES - Current MCB address
;        CX - Current UMB number
;        DX - UMB number to skip (load UMB)
; -----------------------------------------

fum10:	call	isSysMCB	; Returns with ZF set if owner is SYSTEM
	jnz	short fum20

	inc	cx		; If it _was_ SYSTEM, we're in a new UMB.
fum20:	
	cmp	cx,dx		; If this is the load UMB, we don't want to
	je	short fum30	; freeze anything... so skip that section.

	;call	isFreeMCB	; Oh.  If it's not free, we can't freeze it
	or	word [es:ARENA.OWNER],0
	jnz	short fum30	; either.

	call	frezMCB
fum30:	
	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end ; 'Z'
	je	short fumX	; 'Z' means this was the last MCB... that's it.

	;NextMCB es, ax		; Go on forward.
	mov     ax,es
	;add	ax,[es:3]
	add     ax,[es:ARENA.SIZE]
	inc     ax
	mov     es,ax
	
	jmp	short fum10

fumX:	pop	es
	pop	dx
	pop	cx
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** UmbTest - returns with carry set if UMBs are not available, else CF==false
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     Carry is clear if UMBs are available, or set if they are not
; ERROR:    None
; USES:     CF (AX,BX,DS,ES pushed 'cause they're used by others)
; -----------------------------------------------------------------------------

UmbTest:
	push	ax
	push	bx
	push	ds
	push	es

	call	fm_link			; Link in UMBs (if not already linked)
	call	WalkMem			; Check to see if they're really linked
	pushf				; And remember what we found out
	call	fm_unlink		; Unlink UMBs (if WE have linked 'em)
	popf				; And restore what we found out.

	pop	es
	pop	ds
	pop	bx
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** WalkMem - travels memory chain and returns carry clear iff UMBs are linked
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     Carry SET if MCB chain stops before 9FFF, CLEAR if stops >= 9FFF.
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

WalkMem:
	push	ax ; ?
	push	bx ; ?
	push	es ; ? no need to save contents of these registers ?
		   	
	mov	ah,GET_IN_VARS		; Call int 21h, function 52h...
	int	21h

	mov	ax,[es:bx-2]
	mov	es,ax

; ------------------------------
; UM10: ES = Current MCB pointer
; ------------------------------

um10:	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end ;  'Z'
	je	short um20		; If signature == 'Z', hay no more.

	;NextMCB es, bx			; Move to the next MCB

	mov     bx,es
	;add	bx,[es:3]
	add     bx,[es:ARENA.SIZE]
	inc     bx
	mov     es,bx
		
	jmp	short um10		; And restart the loop.
um20:	
	mov	ax,es

	cmp	ax,9FFFh		; This sets CF iff ax < 9FFF.

	pop	es ; ?
	pop	bx ; ?
	pop	ax ; ?
	retn

; -----------------------------------------------------------------------------
;*** hl_unlink - unlinks UMBs if fm_umb is set to 0; restores strategy too
; -----------------------------------------------------------------------------
; ENTRY:    fm_umb == 1 : leave linked, else unlink
; EXIT:     None
; ERROR:    None
; USES:     AX, BX
; -----------------------------------------------------------------------------

hl_unlink:
	xor	bh,bh

	;getdata bl,fm_umb		; Restore original link-state
	;
	;push	ds
	;push	cs
	;pop	ds
	;mov	bl,[fm_umb]
	;pop	ds

	mov	bl,[cs:fm_umb]

	mov	ax,DOS_SET_UMBLINK ; 5803h
	int	21h
	retn

; ----------------------------------------------------------------------
; HIGHEXIT.INC (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 09/04/2019 - Retro DOS v4.0

;   Module:   HIGHEXIT.INC - Code executed after LoadHigh or DeviceHigh
;   Date:     May 14, 1992

;   Modification log:
;
;     DATE    WHO      DESCRIPTION
;   --------  -------  --------------------------------------------------------
;   05/14/92  t-richj  Original
;   06/21/92  t-richj  Final revisions before check-in

UMB_HeadIdx	equ	8Ch	; Offset from ES (after func52h) to get UMBHead

; -----------------------------------------------------------------------------
;*** UnHideUMBs - Marks HIDDEN elements as FREE
; -----------------------------------------------------------------------------
; ENTRY:  None; perhaps, earlier, HideUMBs was called... if not, we have
;               very little to do, as no elelments will be marked as HIDDEN.
; EXIT:   Sets InHigh to zero; carry clear if HideUMBs was called earlier.
; ERROR:  None
; USES:   fInHigh (from highvar.inc), carry flag
; -----------------------------------------------------------------------------

UnHideUMBs:
	push	ax		; Save ax for what we're about to do

; -----------------------------------------------------------------------------
; BUGBUG t-richj 11-8-92: The following six lines were commented out for a good
;    length of time. Those six constitute a check of whether or not we should
;    indeed clean up the upper-memory chain; without such a check, COMMAND.COM
;    will destroy the current link-state and memory-allocation strategy after
;    every command execution.
; -----------------------------------------------------------------------------

	;getdata al,fInHigh	; Get InHigh from data segment
	;
	;push	ds
	;push	cs
	;pop	ds
	;mov	al,[fInHigh]
	;pop	ds	

	mov	al,[cs:fInHigh]

	or	al,al
	jnz	short uhu10	; If didn't call loadhigh/devicehigh earlier,

	pop	ax		; then there's nothing to do here... so
	stc			; restore everything and return. Just like
	retn			; that.
uhu10:	
	call	linkumb		; Make sure UMBs are linked in.
	call	FreeUMBs

	;putdata fInHigh,0	; We're leaving, so update fInHigh.
	;
	;push	es
	;push	cs
	;pop	es
	;mov	byte [es:fInHigh],0
	;pop	ds	

	mov	byte [cs:fInHigh],0	

	call	he_unlink	; Unlink UMBs

	pop	ax
	clc
	retn

; -----------------------------------------------------------------------------
;*** he_unlink - unlinks UMBs if fm_umb is set to 0
; -----------------------------------------------------------------------------
; ENTRY:    fm_umb == 1 : leave linked, else unlink
; EXIT:     None
; ERROR:    None
; USES:     AX, BX
; -----------------------------------------------------------------------------

he_unlink:
	xor	bh, bh

	;getdata bl, fm_umb	; Restore original link-state
	mov	bl,[cs:fm_umb]	

	mov	ax,DOS_SET_UMBLINK ; 5803h
	int	21h
	retn

; -----------------------------------------------------------------------------
;*** freeUMBs - frees all HIDDEN memory elements in upper-memory.
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     None; HIDDEN memory elements returned to FREE
; ERROR:    None (ignore CF)
; USES:     Flags
; -----------------------------------------------------------------------------

FreeUMBs:
	push	ax
	push	es

	call	HeadUmb		; Returns with carry if err, else ES == MCB
	jc	short fusX

	mov	es,ax		; Prepare for the loop; ES = current MCB addr.
fus10:	
	call	isHideMCB	; Returns with ZF set if owner is 0
	jnz	short fus20
	call	freeMCB
fus20:	   
	mov	al,[es:ARENA.SIGNATURE]
	cmp	al,arena_signature_end ; 'Z'
	jz	short fusX	; That means this was the last MCB--that's it.

	mov	ax,es
	add	ax,[es:ARENA.SIZE]
	inc	ax
	mov	es,ax		; Go on forward.

	jmp	short fus10
fusX:	
	pop	es
	pop	ax
	retn

; -----------------------------------------------------------------------------
;*** isHideMCB - returns with ZF set if current MCB (ES:0) is HIDDEN
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     ZF set if MCB is hidden, else !ZF
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isHideMCB:
	;push	ax

	cmp	word [es:ARENA.OWNER],SystemPSPOwner ; If the owner's SYSTEM
	jne	short ihm_x				; then check for HIDDEN

	;mov	ax,[es:ARENA.NAME]
	;cmp	ax,'HI' ; 4948h
	cmp	word [es:ARENA.NAME+0],'HI'
	jne	short ihm_x
	;mov	ax,[es:ARENA.NAME+2]
	;cmp	ax,'DD' ; 4444h
	cmp	word [es:ARENA.NAME+2],'DD'
	jne	short ihm_x
	;mov	ax,[es:ARENA.NAME+4]
	;cmp	ax,'EN' ; 4E45h
	cmp	word [es:ARENA.NAME+4],'EN'
	jne	short ihm_x
	;mov	ax,[es:ARENA.NAME+6]
	;cmp	ax,'  ' ; 2020h
	cmp	word [es:ARENA.NAME+6],'  '
ihm_x:	
	;pop	ax
	retn

; -----------------------------------------------------------------------------
;*** freeMCB - marks as free the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB free'd
; ERROR:    None
; USES:     AX
; -----------------------------------------------------------------------------

freeMCB:
	mov	word [es:ARENA.OWNER],0
	mov	ax,'  '
	mov	[es:ARENA.NAME+0],ax
	mov	[es:ARENA.NAME+2],ax
	mov	[es:ARENA.NAME+4],ax
	mov	[es:ARENA.NAME+6],ax
	retn

; -----------------------------------------------------------------------------
;*** HeadUmb - returns in AX the address of the first UMB block (0x9FFF)
; -----------------------------------------------------------------------------
; ENTRY:  Nothing
; EXIT:   AX contains 0x9FFF for most systems
; ERROR:  Carry set if pointer is 0xFFFF (if not set up yet--DH runs into this)
; USES:   Flags, AX
; -----------------------------------------------------------------------------

HeadUmb:
	; 13/05/2019

	;push	si ; ?
	;push	ds ; ?
	;push	es
	;push	bx ; *

	; 09/04/2019
	; !!! No need to save es,bx,ds,si above !!! (es,bx are changed here)

	mov	ah,GET_IN_VARS		; Call int 21h, function 52h...
	int	21h
			; DOS - 2+ internal - GET LIST OF LISTS
			; Return: ES:BX -> DOS list of lists
	;mov	ax,[es:8Ch]
	mov	ax,[es:UMB_HeadIdx]	; And read what's in ES:008C
	cmp	ax,0FFFFh
	;je	short xhu_e		; If it's 0xFFFF, it's an error...

	;clc				; Else, it isn't.
	;jmp	short xhu_x
xhu_e:	
	;stc
	cmc	; 09/04/2019 - Retro DOS v4.0 ; *
xhu_x:	
	;pop	bx ; *
	;pop	es	
	;pop	ds ; ?
	;pop	si ; ?
	retn

; -----------------------------------------------------------------------------
;*** linkumb - links UMBs not already linked in; updates fm_umb as needed
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     fm_umb == 0 if not linked in previously, 1 if already linked in
; ERROR:    None
; USES:     AX, BX, fm_umb
; -----------------------------------------------------------------------------

linkumb:
	mov	ax,DOS_GET_UMBLINK ; 5802h
	int	21h			; Current link-state is now in al

	or	al,al			; BUGBUG: proper check?
	jnz	short lumbX		; Jumps if UMBs already linked in

	mov	ax,DOS_SET_UMBLINK ; 5803h
	mov	bx,1
	int	21h
lumbX:
	retn

%endif

; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; (SYSINIT:2B5Fh)

; ----------------------------------------------------------------------
; SYSCONF.ASM (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 09/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
; procedure : InitDevLoad
;
;	Input : DeviceHi = 0 indicates load DD in low memory
;			 = 1 indicates load in UMB:
;		           ConvLoad = 0 indicates a new-style load (see below)
;		                    = 1 indicates a DOS 5-style load
;		DevSize  = Size of the device driver file in paras
;
;	Output : none
;
;	Initializes DevLoadAddr, DevLoadEnd & DevEntry.
;	Also sets up a header for the Device driver entry for mem utility
;
;----------------------------------------------------------------------------
; For a "new-style load", we break off the current DevEntry and link the umbs
; as we see fit, using HideUMBs (and UnHideUMBs at exit, though _it_ decides
; whether it's entitled to do anything). HideUMBs uses the chart built by
; ParseVar to determine which UMBs to leave FREE, and which not.
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
InitDevLoad:
	cmp	byte [cs:DeviceHi],0	; Are we loading in UMB ?
	;je	short InitForLo		; no, init for lo mem
	je	short initforlo_x ; 09/04/2019

; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;	cmp	byte [cs:ConvLoad],1	; Are we loading as per DOS 5?
;	je	short InitForConv
;
;; There are two stages to preparing upper-memory; first, we mark as 8+HIDDEN
;; any areas not specified on the /L:... chain. Second, we mark as 8+FROZEN
;; any areas left in upper-memory, except for elements in the load UMB...
;; we then malloc space as per Dos-5 style, and mark as free any spaces which
;; are 8+FROZEN (but leave 8+HIDDEN still hidden). The load is performed,
;; and UnHideUMBs later on marks all 8+HIDDEN as free.
;
;	call	ShrinkUMB		; Stop using the old device arena
;
;	call	HideUMBs		; Mark up the UM area as we see fit
;	call	FreezeUM		; Hide everything BUT the load area
;	call	GetUMBForDev		; And grab that load area as needed
;	pushf
;	call	UnFreeze		; Then unhide everything frozen
;	popf
;
;	jc	short InitForLo		; (if carry, it's loading low)
;
;	jmp	short InitForHi

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2B67h)
InitForConv:
	call	SpaceInUMB		; Do we have space left in the
					;  current UMB ?
	jnc	short InitForHi		; yes, we have
	call	ShrinkUMB		; shrink the current UMB in use
	call	GetUMBForDev		; else try to allocate new UMB
	jc	short InitForLo		; we didn't succeed, so load
					;  in low memory
InitForHi:
	mov	ax,[cs:DevUMBFree]	; get Para addr of free mem
	mov	dx,[cs:DevUMBAddr]	; UMB start addr
	add	dx,[cs:DevUMBSize]	; DX = UMB End addr
	jmp	short idl1

InitForLo:
	mov	byte [cs:DeviceHi],0	; in case we failed to load
initforlo_x:
					;  into UMB indicate that
					;  we are loading low
	mov	ax,[cs:memhi]		; AX = start of Low memory
	mov	dx,[cs:ALLOCLIM]	; DX = End of Low memory

idl1:
	call	DevSetMark		; setup a sub-arena for DD
	mov	[cs:DevLoadAddr],ax	; init the Device load address

	mov	[cs:DevLoadEnd],dx	; init the limit of the block
	mov	word [cs:DevEntry],0	; init Entry point to DD
	mov	[cs:DevEntry+2],ax
	retn

;----------------------------------------------------------------------------
;
; procedure : SpaceInUMB?
;
;	Input : DevUMBAddr, DevUMBSize, DevUMBFree & DevSize
;	Output : Carry set if no space in UMB
;		 Carry clear if Space is available for the device in
;		   current UMB
;
;----------------------------------------------------------------------------

SpaceInUMB:
	mov	ax,[cs:DevUMBSize]
	add	ax,[cs:DevUMBAddr]	; End of UMB
	sub	ax,[cs:DevUMBFree]	; - Free = Remaining space
	or	ax,ax			; Nospace ?
	jnz	short spcinumb1
	stc
	retn
spcinumb1:
	dec	ax			; space for sub-arena
	cmp	ax,[cs:DevSize]		; do we have space ?
	retn

;----------------------------------------------------------------------------
;
; procedure : PrepareMark
;
;	Input : AX==Address of MCB (not addr of free space), BX==Size
;	Output : None; MCB marked appropriately and DevUMB* set as needed.
;
;----------------------------------------------------------------------------

;	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;PrepareMark:
;	push	ds
;	mov	ds,ax
;	mov	word [ARENA.OWNER],8
;	mov	word [ARENA.NAME],'SD' ; 4453h
;	pop	ds
;
;	inc	ax
;	mov	[cs:DevUMBAddr],ax
;	mov	[cs:DevUMBFree],ax
;	mov	[cs:DevUMBSize],bx	; update the UMB Variables
;	retn

;----------------------------------------------------------------------------
;
; procedure : GetUMBForDev
;
;	Input : DevSize
;	Output : Carry set if couldn't allocate a UMB to fit the
;		 the device.
;		 If success carry clear
;
;	Allocates the biggest UMB for loading devices and updates
;	DevUMBSize, DevUMBAddr & DevUMBFree if it succeeded in allocating
;	UMB.
;
;	This routine relies on the fact that all of the low memory
;	is allocated, and any DOS alloc calls should return memory
;	from the UMB pool.
;
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2BC6h)

GetUMBForDev:
	mov	bx,0FFFFh
	mov	ax,4800h
	int	21h
		; DOS - 2+ - ALLOCATE MEMORY
		; BX = number of 16-byte paragraphs desired

	or	bx,bx
	jz	short gufd_err

	dec	bx
	cmp	[cs:DevSize],bx
	ja	short gufd_err
	inc	bx

	mov	ax,4800h
	int	21h
	jc	short gufd_err

	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;dec	ax
	;call	PrepareMark
	;
PrepareMark:
	push	ds
	dec	ax
	mov	ds,ax
	mov	word [ARENA.OWNER],8
	mov	word [ARENA.NAME],'SD' ; 4453h
	inc	ax
	pop	ds
	mov	[cs:DevUMBSize],bx	; update the UMB Variables
	mov	[cs:DevUMBAddr],ax
	mov	[cs:DevUMBFree],ax
	;
	clc				; mark no error
	retn
gufd_err:
	xor	ax,ax ; 0
	mov	[cs:DevUMBSize],ax	; erase the previous values
	mov	[cs:DevUMBAddr],ax
	mov	[cs:DevUMBFree],ax
	stc
	retn

;----------------------------------------------------------------------------
;
; procedure : DevSetMark
;
;	Input : AX - Free segment were device is going to be loaded
;	Output : AX - Segment at which device can be loaded (AX=AX+1)
;
;	Creates a sub-arena for the device driver
;	puts 'D' marker in the sub-arena
;	Put the owner of the sub-arena as (AX+1)
;	Copies the file name into sub-arena name field
;
;	Size field of the sub-arena will be set only at succesful
;	completion of Device load.
;
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2C13h)

DevSetMark:
	push	es
	push	di
	push	ds
	push	si
	mov	es,ax
	mov	byte [es:devmark.id],devmark_device ; 'D'
	inc	ax
	mov	[es:devmark.seg],ax

;-------------- Copy file name

	push	ax			; save load addr
	lds	si,[cs:bpb_addr]	; command line is still there

	mov	di,si
	cld
dsm_again:
	lodsb
	cmp	al,':'
	jne	short isit_slash
	mov	di,si
	jmp	short dsm_again
isit_slash:
	cmp	al, '\'
	jne	short isit_null
	mov	di,si
	jmp	short dsm_again
isit_null:
	or	al,al
	jnz	short dsm_again
	mov	si,di

	mov	di,devmark.filename ; 8
	mov	cx,8			; maximum 8 characters
dsm_next_char:
	lodsb
	or	al, al
	jz	short blankout
	cmp	al, '.'
	je	short blankout
	stosb
	loop	dsm_next_char
blankout:
	jcxz	dsm_exit
	mov	al, ' '
	rep	stosb			; blank out the rest
dsm_exit:
	pop	ax			; restore load addr
	pop	si
	pop	ds
	pop	di
	pop	es
	retn

;----------------------------------------------------------------------------
;
; procedure : SizeDevice
;
;	Input : ES:SI - points to device file to be sized
;
;	Output : Carry set if file cannot be opened or if it is an OS2EXE file
;
;	Calculates the size of the device file in paras and stores it
;	in DevSize
;
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
SizeDevice:
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;push	ds
	push	es
	pop	ds
	mov	dx,si			; ds:dx -> file name
	mov	ax,3D00h		; open
	int	21h
	jc	short sd_err		; open failed

	mov	bx,ax			; BX - file handle
	mov	ax,4202h		; seek
	xor	cx,cx
	mov	dx,cx			; to end of file
	int	21h
	jc	short sd_close		; did seek fail (impossible)
	add	ax,15			; para convert
	adc	dx,0
	test	dx,0FFF0h		; size > 0ffff paras ?
	jz	short szdev1		; no
	mov	word [cs:DevSize],0FFFFh ; invalid device size
					; assuming that we fail later
	jmp	short sd_close
szdev1:
	mov	cl,4			; convert it to paras
	shr	ax,cl
	mov	cl,12
	shl	dx,cl
	or	ax,dx
	;
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	cmp     ax, [cs:DevSizeOption]
	ja      short szdev2
	mov     ax, [cs:DevSizeOption]
szdev2:
	;
	mov	[cs:DevSize],ax		; save file size

	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc
sd_close:
	pushf				; let close not spoil our
					;  carry flag
	mov	ax,3E00h		; close
	int	21h			; we are not checking for err
	popf
sd_err:
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;pop     ds
	retn

;----------------------------------------------------------------------------
;
; procedure : ExecDev
;
;	Input : ds:dx -> device to be executed
;		DevLoadAddr - contains where device has to be loaded
;
;	Output : Carry if error
;		 Carry clear if no error
;
;	Loads a device driver using the 4b03h function call
;
;----------------------------------------------------------------------------

	; 01/11/2022
ExecDev:
	mov	bx,[cs:DevLoadAddr]
	mov	[cs:DevExecAddr],bx	; Load the parameter block
					;  block for exec with
					;  Load address
	mov	[cs:DevExecReloc],bx
	mov	bx,cs
	mov	es,bx
	mov	bx,DevExecAddr		;es:bx points to parameters
	mov	al,3	; (load program only)
	mov	ah,EXEC ; 4Bh
	int	21h			;load in the device driver
 		; DOS - 2+ - LOAD OR EXECUTE (EXEC)
		; DS:DX -> ASCIZ filename
		; ES:BX -> parameter block
		; AL = subfunction 
	retn

;----------------------------------------------------------------------------
;
; procedure : RetFromUM
;
;	Input : None
;	Output : ConvLoad set if didn't previously call HideUMBs
;		 ConvLoad clear if did.
;
;	Prepares memory for more devices after returning from loading one
;	using the DOS 6 options (/L:... etc).
;
;----------------------------------------------------------------------------

	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;RetFromUM:
;	pushf
;	mov	byte [cs:ConvLoad],1
;	call	UnHideUMBs
;	jc	short rfUM1		; Skip this if didn't HideUMBs
;	mov	byte [cs:ConvLoad],0
;rfUM1:	
;	popf
;	retn

;----------------------------------------------------------------------------
;
; procedure : RemoveNull
;
;	Input : ES:SI points to a null terminated string
;
;	Output : none
;
;	Replaces the null at the end of a string with blank
;
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2CCEh)
RemoveNull:
rn_next:
	mov	bl,[es:si]
	or	bl,bl			; null ?
	jz	short rn_gotnull
	inc	si			; advance the pointer
	jmp	short rn_next
rn_gotnull:
	mov	bl,[cs:DevSavedDelim]
	mov	[es:si],bl		; replace null with blank
	; 02/11/2022
;rba_ok: ; 10/04/2019
	retn

;----------------------------------------------------------------------------
;
; procedure : RoundBreakAddr
;
;	Input : DevBrkAddr
;	Output : DevBrkAddr
;
;	Rounds DevBrkAddr to a para address so that it is of the form xxxx:0
;
;----------------------------------------------------------------------------

RoundBreakAddr:
	mov	ax,[cs:DevBrkAddr]
	call	ParaRound
	add	[cs:DevBrkAddr+2],ax
	mov	word [cs:DevBrkAddr],0
	mov	ax,[cs:DevLoadEnd]
	cmp	[cs:DevBrkAddr+2],ax
	jbe	short rba_ok
	jmp	mem_err
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
rba_ok:
	retn

;----------------------------------------------------------------------------
;
; procedure : DevSetBreak
;
;	Input : DevBrkAddr
;	Output : Carry set if Device returned Init failed
;		 Else carry clear
;
;----------------------------------------------------------------------------

DevSetBreak:
	push	ax

	mov	ax,[cs:DevBrkAddr+2]	 ;remove the init code
	cmp	byte [cs:multdeviceflag],0
	jne	short set_break_continue ;do not check it.
	cmp	ax,[cs:DevLoadAddr]
	jne	short set_break_continue ;if not same, then o.k.

	cmp	word [cs:DevBrkAddr],0
	je	short break_failed	;[DevBrkAddr+2]=[memhi] & [DevBrkAddr]=0

set_break_continue:
	call	RoundBreakAddr
	pop	ax
	clc
	retn
break_failed:
	pop	ax
	stc
	retn

;----------------------------------------------------------------------------
;
; procedure : DevBreak
;
;	Input : DevLoadAddr & DevBrkAddr
;	Output : none
;
;	Marks a succesful install of a device driver
;	Sets device size field in sub-arena &
;	Updates Free ptr in UMB or adjusts memhi
;
;----------------------------------------------------------------------------

DevBreak:
	push	ds
	mov	ax,[cs:DevLoadAddr]
	mov	bx,[cs:DevBrkAddr+2]
	dec	ax			; seg of sub-arena
	mov	ds,ax
	inc	ax			; Back to Device segment
	sub	ax,bx
	neg	ax			; size of device in paras
	mov	[devmark.size],ax	; store it in sub-arena
	cmp	byte [cs:DeviceHi],0
	je	short db_lo
	mov	[cs:DevUMBFree],bx	; update Free ptr in UMB
	jmp	short db_exit
db_lo:
	mov	[cs:memhi],bx
	mov	word [cs:memlo],0
db_exit:
	pop	ds
	retn

; 10/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
; procedure : ParseSize
;
;	Parses the command line for SIZE= command
;
;	ES:SI = command line to parsed
;
;	returns ptr to command line after SIZE= option in ES:SI
;	updates the DevSizeOption variable with value supplied
;	in SIZE=option
;	Returns carry if the SIZE option was invalid
;
;----------------------------------------------------------------------------

	; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2D5Ah)
ParseSize:
	;push	bx
	;mov	bx,si

	mov	word [cs:DevSizeOption],0 ; init the value
	mov	[cs:DevCmdLine],si
	mov	[cs:DevCmdLine+2],es
	call	SkipDelim
	cmp	word [es:si],'SI' ; 4953h
	jne	short ps_no_size
	cmp	word [es:si+2],'ZE' ; 455Ah
	jne	short ps_no_size
	mov	al,[es:si+4]
	call	delim
	jne	short ps_no_size
	add	si,5
	call	GetHexNum
	jc	short ps_err
	mov	[cs:DevSizeOption],ax
	call	SkipDelim
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;mov	bx,si
ps_no_size:	
	;mov	si,bx
	;pop	bx
	clc
	;retn
	; 02/11/2022
	retn
ps_err:
	; 02/11/2022
	;pop	bx
	;stc
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	stc
;sd_ret: ; 12/04/2019
	retn

; 12/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
; procedure : SkipDelim
;
;	Skips delimiters in the string pointed to by ES:SI
;	Returns ptr to first non-delimiter character in ES:SI
;
;----------------------------------------------------------------------------
	
	; 01/11/2022
SkipDelim:
sd_next_char:
	mov	al,[es:si]
	call	delim
	jnz	short sd_ret
	inc	si
	jmp	short sd_next_char ; 01/11/2022
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
sd_ret:
	retn

;----------------------------------------------------------------------------
;
; procedure : GetHexNum
;
;	Converts an ascii string terminated by a delimiter into binary.
;	Assumes that the ES:SI points to a Hexadecimal string
;
;	Returns in AX the number number of paras equivalent to the
;	hex number of bytes specified by the hexadecimal string.
;
;	Returns carry in case it encountered a non-hex character or
;	if it encountered crlf
;
;----------------------------------------------------------------------------

; 13/05/2019

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2DA5h)
GetHexNum:
	xor	ax, ax
	xor	dx, dx
ghn_next:
	mov	bl,[es:si]
	cmp	bl,cr  ; 0Dh
	je	short ghn_err
	cmp	bl,lf  ; 0Ah
	je	short ghn_err
	push	ax
	mov	al,bl
	call	delim
	pop	ax
	jz	short ghn_into_paras
	call	GetNibble
	jc	short ghn_err
	mov	cx,4
ghn_shift1:
	shl	ax,1
	rcl	dx,1
	loop	ghn_shift1
	or	al,bl
	inc	si
	jmp	short ghn_next
ghn_into_paras:
	add	ax,15
	adc	dx,0
	test	dx,0FFF0h
	jnz	short ghn_err
	mov	cx,4
ghn_shift2:
	clc
	rcr	dx,1
	rcr	ax,1
	loop	ghn_shift2
	clc
	retn
ghn_err:
	stc
	retn

;----------------------------------------------------------------------------
;
; procedure : GetNibble
;
;	Convert one nibble (hex digit) in BL into binary
;
;	Retruns binary value in BL
;
;	Returns carry if BL contains non-hex digit
;
;----------------------------------------------------------------------------

GetNibble:
	cmp	bl,'0'
	jb	short gnib_err
	cmp	bl,'9'
	ja	short is_it_hex
	sub	bl,'0'		; clc
	retn
is_it_hex:
	cmp	bl,'A'
	jb	short gnib_err
	cmp	bl,'F'
	ja	gnib_err
	sub	bl,'A'- 10	; clc
	retn
gnib_err:
	stc
	retn

;============================================================================

; 12/04/2019 - Retro DOS v4.0

; umb.inc (MSDOS 6.0, 1991)
DOS_ARENA	equ 24h		; offset of arena_head var in DOS data segm.
UMB_ARENA	equ 8Ch		; offset of umb_head in DOS data

XMM_REQUEST_UMB	equ 10h
XMM_RELEASE_UMB	equ 11h

;----------------------------------------------------------------------------
;
; procedure : AllocUMB
;
;	Allocate all UMBs and link it to DOS arena chain
;
;----------------------------------------------------------------------------

AllocUMB:
	call	InitAllocUMB		; link in the first UMB
	jc	short au_exit		; quit on error
au_next:
	call	umb_allocate		; allocate
	jc	short au_coalesce
	call	umb_insert		; & insert till no UMBs
	jmp	short au_next
au_coalesce:
	call	umb_coalesce		; coalesce all UMBs
au_exit:
	retn

;----------------------------------------------------------------------------
;
; procedure : InitAllocUMB
;
;----------------------------------------------------------------------------

InitAllocUMB:
	call	IsXMSLoaded
	jnz	short iau_err		; quit on no XMS driver
	mov	ah,52h
	int	21h			; get DOS DATA seg
	mov	[cs:DevDOSData],es	; & save it for later
	mov	ax,4310h
	int	2Fh
	mov	[cs:DevXMSAddr],bx	; get XMS driver address
	mov	[cs:DevXMSAddr+2],es
	cmp	byte [cs:FirstUMBLinked],0 ; have we already linked a UMB?
	jne	short ia_1		; quit if we already did it
	call	LinkFirstUMB		; else link the first UMB
	jc	short iau_err
	mov	byte [cs:FirstUMBLinked],0FFh ; mark that 1st UMB linked
ia_1:
	clc
	retn
iau_err:
	stc
	retn

;-------------------------------------------------------------------------
;
; Procedure Name	: umb_allocate
;
; Inputs		: DS = data
;
; Outputs		: if UMB available
;				Allocates the largest available UMB and 
;			  	BX = segment of allocated block
;				DX = size of allocated block
;				NC
;			  else 
;				CY
;
; Uses			: BX, DX
;
;-------------------------------------------------------------------------

umb_allocate:
	push	ax
	mov	ah,XMM_REQUEST_UMB ; 16
	mov	dx,0FFFFh		; try to allocate largest
					;   possible
	call	far [cs:DevXMSAddr]
					; dx now contains the size of
					; the largest UMB
	or	dx,dx
	jz	short ua_err
	
	mov	ah,XMM_REQUEST_UMB ; 16
	call	far [cs:DevXMSAddr]

	cmp	ax,1			; Q: was the reqst successful
	jne	short ua_err		; N: error
	;clc
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc 
ua_done:
	pop	ax
	retn		
ua_err:
	stc
	jmp	short ua_done

;---------------------------------------------------------------------------
;
; Procedure Name	: umb_insert
;
; Inputs		: DOSDATA:UMB_HEAD = start of umb chain
;			: BX = seg address of UMB to be linked in
;			: DX = size of UMB to be linked in paras
;			; DS = data
;
; Outputs		: links the UMB into the arena chain
;
; Uses			: AX, CX, ES, DX, BX
;
;---------------------------------------------------------------------------

umb_insert:
	push	ds

	mov	ds,[cs:DevDOSData]
	;mov	ds,[8Ch]
	mov	ds,[UMB_ARENA]		; es = UMB_HEAD
	mov	ax,ds
	mov	es,ax
ui_next:
	cmp	ax,bx			; Q: is current block above
					;    new block
	ja	short ui_insert		; Y: insert it
					; Q: is current block the
					;    last
	cmp	byte [es:ARENA.SIGNATURE],arena_signature_end ; 'Z'
	je	short ui_append		; Y: append new block to chain
					; N: get next block
	mov	ds,ax			; M005
	;call	get_next		; ax = es = next block
	call	_get_next_ ; 13/04/2019 - Retro DOS v4.0
	jmp	short ui_next

ui_insert:
	mov	cx,ds			; ds = previous arena
	inc	cx			; top of previous block

	sub	cx,bx
	neg	cx			; cx = size of used block
	;mov	byte [0],'M'
	mov	byte [ARENA.SIGNATURE],arena_signature_normal ; 'M'
	;mov	word [1],8
	mov	word [ARENA.OWNER],8	; mark as system owned
	;mov	[3],cx
	mov	[ARENA.SIZE],cx	
	;mov	word [8],4353h ; 'SC'
	mov	word [ARENA.NAME],'SC' ; 4353h

; prepare the arena at start of new block

	mov	es,bx
	mov	byte [es:ARENA.SIGNATURE],arena_signature_normal ; 'M'
	mov	word [es:ARENA.OWNER],arena_owner_system ; 0
					; mark as free
	sub	dx,2			; make room for arena at
					; start & end of new block
	mov	[es:ARENA.SIZE],dx

; prepare arena at end of new block
	
	add	bx,dx
	inc	bx
	mov	es,bx			; es=arena at top of new block
	inc	bx			; bx=top of new block

					; ax contains arena just above
					; this block
	sub	ax,bx			; ax = size of used block
	
	mov	byte [es:ARENA.SIGNATURE],arena_signature_normal
	mov	word [es:ARENA.OWNER],8	; mark as system owned
	mov	[es:ARENA.SIZE],ax	
	mov	word [es:ARENA.NAME],'SC' ; 4353h

	jmp	short ui_done

ui_append:
					; es = arena of last block	
	add	ax,[es:ARENA.SIZE]	; ax=top of last block-1 para
	sub	word [es:ARENA.SIZE],1	; reflect the space we are
					; going to rsrv on top of this 
					; block for the next arena.
	; 13/05/2019
	mov	byte [es:ARENA.SIGNATURE],arena_signature_normal

	mov	cx,ax			; cx=top of prev block-1
	inc	ax
	sub	ax,bx			; ax=top of prev block - 
					;    seg. address of new block
	neg	ax

	mov	es,cx			; ds = arena of unused block

	mov	byte [es:ARENA.SIGNATURE],arena_signature_normal
	mov	word [es:ARENA.OWNER],8	; mark as system owned
	mov	[es:ARENA.SIZE],ax	
	mov	word [es:ARENA.NAME],'SC'

; prepare the arena at start of new block
	mov	es,bx
	mov	byte [es:ARENA.SIGNATURE],arena_signature_end
	mov	word [es:ARENA.OWNER],arena_owner_system
					; mark as free
	dec	dx			; make room for arena
	mov	[es:ARENA.SIZE],dx	
ui_done:
	pop	ds
	retn

; 13/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
;**	umb_coalesce - Combine free blocks ahead with current block
;
;	Coalesce adds the block following the argument to the argument block,
;	iff it's free.  Coalesce is usually used to join free blocks, but
;	some callers (such as $setblock) use it to join a free block to it's
;	preceeding allocated block.
;
;	EXIT	'C' clear if OK
;		  (ds) unchanged, this block updated
;		  (ax) = address of next block, IFF not at end
;		'C' set if arena trashed
;	USES	cx, di, ds, es
;
;----------------------------------------------------------------------------

umb_coalesce:
	xor	di, di

	mov	es,[cs:DevDOSData]
	mov	es,[es:UMB_ARENA]	; es = UMB_HEAD
uc_nextfree:
	mov	ax,es
	mov	ds,ax
	;cmp	[es:1],di
	cmp	[es:ARENA.OWNER],di	; Q: is current arena free
	je	short uc_again		; Y: try to coalesce with next block
					; N: get next arena
	call	get_next		; es, ax = next arena
	jc	short uc_done
	jmp	short uc_nextfree
uc_again:
	call	get_next		; ES, AX <- next block
	jc	short uc_done
uc_check:
	cmp     [es:ARENA.OWNER],di	; Q: is arena free
	jne	short uc_nextfree	; N: get next free arena
					; Y: coalesce
	mov     cx,[es:ARENA.SIZE]      ; cx <- next block size
	inc     cx                      ; cx <- cx + 1 (for header size)
	;add	[3],cx
	add     [ARENA.SIZE],cx		; current size <- current size + cx
	mov     cl,[es:di]              ; move up signature
	mov     [di],cl
	jmp     short uc_again		; try again
uc_done:
	retn

;----------------------------------------------------------------------------
;
;**	get_next - Find Next item in Arena
;
;	ENTRY	dS - pointer to block head
;	EXIT	AX,ES - pointers to next head
;		'C' set if arena damaged
;
;----------------------------------------------------------------------------

	; 01/11/2022
get_next:
	cmp	byte [0],arena_signature_end ; 'Z'
	je	short gn_err
_get_next_:
	mov     ax,ds                   ; ax=current block
	add     ax,[ARENA.SIZE]		; ax=ax + current block length
	inc     ax                      ; remember that header!
	mov	es,ax
	;clc
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc
	retn
gn_err:
	stc
	retn

;----------------------------------------------------------------------------
;
; procedure : LinkFirstUMB
;
;----------------------------------------------------------------------------

	; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:2F81h)
LinkFirstUMB:
	call	umb_allocate
	jc	short lfu_err

; bx = segment of allocated UMB
; dx = size of UMB

	int	12h			; ax = size of memory
	mov	cl,6
	shl	ax,cl			; ax = size in paragraphs

	mov	cx,ax			; cx = size in paras
	sub	ax,bx			; ax = - size of unused block

	neg	ax

	sub	cx,1			; cx = first umb_arena
	mov	es,cx			; es = first umb_arena
	
	mov	byte [es:ARENA.SIGNATURE],arena_signature_normal ; 'M'
	mov	word [es:ARENA.OWNER],8	; mark as system owned
					
	mov	[es:ARENA.SIZE],ax	
	mov	word [es:ARENA.NAME],'SC' ; 4353h

; put in the arena for the first UMB

	mov	es,bx			; es has first free umb seg
	mov	byte [es:ARENA.SIGNATURE],arena_signature_end ; 'Z'
	mov	word [es:ARENA.OWNER],arena_owner_system ; 0	
					; mark as free 
	dec	dx			; make room for arena
	mov	[es:ARENA.SIZE],dx	

	mov	es,[cs:DevDOSData]
	mov	di,UMB_ARENA ; 8Ch
	mov	[es:di],cx		; initialize umb_head in DOS
					;  data segment with the arena
					;  just below Top of Mem

; we must now scan the arena chain and update the size of the last arena

	mov	di,DOS_ARENA ; 24h
	mov	es,[es:di]		; es = start arena
	xor	di,di
scan_next:
	cmp	byte [es:di],arena_signature_end  ; 'Z'
	je	short got_last
	
	mov	ax,es
	add	ax,[es:ARENA.SIZE]
	inc	ax
	mov	es,ax
	jmp	short scan_next
got_last:
	sub	word [es:ARENA.SIZE],1
	mov	byte [es:ARENA.SIGNATURE],arena_signature_normal ; 'M'
	;clc
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc
	retn
lfu_err:
	;stc
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	stc
	retn

;----------------------------------------------------------------------------
;
; procedure : ShrinkUMB
;
;	Shrinks the current UMB in use, so that the unused portions
;	of the UMB is given back to the DOS free mem pool
;
;----------------------------------------------------------------------------

ShrinkUMB:
	cmp	word [cs:DevUMBAddr],0
	je	short su_exit
	push	es
	push	bx
	mov	bx,[cs:DevUMBFree]
	sub	bx,[cs:DevUMBAddr]
	mov	es,[cs:DevUMBAddr]
	mov	ax,4A00h
	int	21h
		; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
		; ES = segment address of block to change
		; BX = new size in paragraphs
	mov	ax,es
	dec	ax
	mov	es,ax
	mov	word [es:ARENA.OWNER],8
	pop	bx
	pop	es
su_exit:
	retn

;----------------------------------------------------------------------------
;
; procedure : UnlinkUMB
;
;	Unlinks the UMBs from the DOS arena chain
;
;----------------------------------------------------------------------------

UnlinkUMB:
	push	ds
	push	es
	cmp	byte [cs:FirstUMBLinked],0
	je	short ulu_x		; nothing to unlink
	mov	es,[cs:DevDOSData]	; get DOS data seg
	mov	ds,[es:DOS_ARENA]
	mov	di,[es:UMB_ARENA]
ulu_next:
	call	get_next
	jc	short ulu_x
	cmp	di,ax			; is the next one UMB ?
	je	short ulu_found
	mov	ds,ax
	jmp	short ulu_next
ulu_found:
	;mov	byte [0],'Z'
	mov     byte [ARENA.SIGNATURE],arena_signature_end ; 'Z'
ulu_x:
	pop	es
	pop	ds
	retn

; ----------------------------------------------------------------------
; SYSINIT2.ASM - MSDOS 6.0 - 1991
; ----------------------------------------------------------------------
; 14/04/2019 - Retro DOS v4.0

; Multiple configuration block support  Created 16-Mar-1992 by JeffPar
;
; Summary:
;
;   The procedure "organize" crunches the in-memory copy of config.sys
;   into lines delimited by CR/LF (sometimes no CR, but *always* an LF)
;   with the leading "keyword=" replaced by single character codes (eg, B
;   for BUFFERS, D for DEVICE, Z for any unrecognized keyword); see comtab
;   and/or config.inc for the full list.
;
;   [blockname] and INCLUDE are the major syntactical additions for multi-
;   configuration support. blockname is either MENU, which contains one
;   or more MENUITEM lines, an optional MENUDEFAULT (which includes optional
;   time-out), or any user-defined keyword, such as NETWORK, CD-ROM, etc.
;   INCLUDE allows the current block to name another block for inclusion
;   during the processing phase of CONFIG.SYS. An INCLUDE is only honored
;   once, precluding nasty infinite-loop scenarios. If blocks are present
;   without a MENU block, then only lines inside COMMON blocks are processed.
;
; Example:
;
;   [menu]
;   menuitem=misc,Miscellaneous
;   menuitem=network,Network Configuration
;   menudefault=network,15
;
;   [network]
;   include misc
;   device=foo
;
;   [misc]
;   device=bar
;   include alternate
;
;   [alternate]
;   device=tar
;
;
;   When the menu is displayed
;
;    1. Miscellaneous
;    2. Network Configuration
;
;   #2 is highlighted as the default option, and will be automatically
;   selected after 15 seconds. It will invoke the following lines in the
;   following order:
;
;       DEVICE=BAR
;       DEVICE=TAR
;       DEVICE=FOO
;

;MULTI_CONFIG equ 1

; the following depend on the positions of the various letters in switchlist

switchnum	equ 11111000b ; 0F8h	; which switches require number

flagec35	equ 00000100b ; 4	; electrically compatible 3.5 inch disk drive
flagdrive	equ 00001000b ; 8 
flagcyln	equ 00010000b ; 16
flagseclim	equ 00100000b ; 32
flagheads	equ 01000000b ; 64
flagff		equ 10000000b ; 128

;----------------------------------------------------------------------------
; 19/04/2019 - Retro DOS v4.0

; MSDOS 6.21 IO.SYS - SYSINIT:3E78h

; 01/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; MSDOS 5.0 IO.SYS - SYSINIT:3054h

insert_blank:	db 	0

;----------------------------------------------------------------------------
;
; procedure : setparms
;
; the following set of routines is used to parse the drivparm = command in
; the config.sys file to change the default drive parameters.
;
;----------------------------------------------------------------------------

setparms:
	push	ds
	push	ax
	push	bx
	push	cx
	push	dx

	push	cs
	pop	ds

	xor	bx,bx
	mov	bl,[drive]
	inc	bl			; get it correct for ioctl call
					; (1=a,2=b...)
	mov	dx,deviceparameters
	mov	ah,IOCTL ; 44h
	mov	al,GENERIC_IOCTL ; 0Dh
	mov	ch,RAWIO ; 8
	mov	cl,SET_DEVICE_PARAMETERS ; 40h
	int	21h

; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;	;;mov	ax,Bios_Data		; get Bios_Data segment
;	;mov	ax,KERNEL_SEGMENT ; 70h
;	; 21/10/2022
;	mov	ax,DOSBIODATASEG ; 0070h	
;	mov	ds,ax			; set Bios_Data segment
;
;	test	word [cs:switches],flagec35 ; 4
;	jz	short not_ec35

	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;test	byte [switches],flagec35 
	test	word [switches],flagec35 ; 4
	jz	short eot_ok

	;mov	cl,[cs:drive]		; which drive was this for?
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	cl,[drive]
	mov	ax,DOSBIODATASEG ; 0070h	
	mov	ds,ax	

	mov	al,1			; assume drive 0
	shl	al,cl			; set proper bit depending on drive
	;or	[531h],al ; (MSDOS 6.21 IO.SYS Offset SYINIT:3EACh)
	or	[ec35_flag],al		; set the bit in the permanent flags


; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;not_ec35:
;;	Now adjust the BIOS's EOT variable if our new drive has more
;;	sectors per track than any old ones.
;
;	;mov	al,[cs:deviceparameters+20]
;	mov	al,[cs:deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK]
;	;cmp	al,[12Ch] ; (MSDOS 6.21 IO.SYS Offset SYINIT:3EB4h)
;	cmp	al,[EOT]
;	jbe	short eot_ok
;	mov	[EOT],al

eot_ok:
	pop	dx			; fix up all the registers
	pop	cx
	pop	bx
	pop	ax
	pop	ds ; 13/05/2019
	retn

;----------------------------------------------------------------------------
;
; procedure : diddleback
;
; replace default values for further drivparm commands
;
;----------------------------------------------------------------------------

diddleback:
	push	ds
	push	cs
	pop	ds
	;mov	word [deviceparameters+4],80
	mov	word [deviceparameters+A_DEVICEPARAMETERS.DP_CYLINDERS],80
	;mov	byte [deviceparameters+1],2
	mov	byte [deviceparameters+A_DEVICEPARAMETERS.DP_DEVICETYPE],DEV_3INCH720KB ; 2
	;mov	word [deviceparameters+2],0
	mov	word [deviceparameters+A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES],0
	mov	word [switches],0	    ; zero all switches
	pop	ds
	retn


; 15/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
; procedure : parseline
;
; entry point is parseline. al contains the first character in command line.
;
;----------------------------------------------------------------------------

	; 01/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:30ACh)
parseline:
	push	ds

	push	cs
	pop	ds

nextswtch:
	cmp	al,cr			; carriage return?
	je	short done_line
	cmp	al,lf			; linefeed?
	je	short put_back		; put it back and done

; anything less or equal to a space is ignored.

	cmp	al,' '                  ; space?
	jbe	short getnext		; skip over space
	cmp	al,'/'
	je	short getparm
	stc				; mark error invalid-character-in-input
	jmp	short exitpl

getparm:
	call	check_switch
	mov	[switches],bx		; save switches read so far
	jc	short swterr
getnext:
	call	getchr
	jc	short done_line
	jmp	short nextswtch
swterr:
	jmp	short exitpl		; exit if error

done_line:
	test	word [switches],flagdrive ; 8 ; see if drive specified
	jnz	short okay
	stc				; mark error no-drive-specified
	jmp	short exitpl

okay:
	mov	ax,[switches]
	and	ax,0003h	    ; get flag bits for changeline and non-rem
	mov	[deviceparameters+A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES],ax
	mov	word [deviceparameters+A_DEVICEPARAMETERS.DP_TRACKTABLEENTRIES],0
	;clc			    ; everything is fine
	; 01/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc
	call	setdeviceparameters
exitpl:
	pop	ds
	retn
put_back:
	inc	word [count]		; one more char to scan
	dec	word [chrptr]		; back up over linefeed
	jmp	short done_line

;----------------------------------------------------------------------------
;
; procedure : check_switch
;
; processes a switch in the input. it ensures that the switch is valid, and
; gets the number, if any required, following the switch. the switch and the
; number *must* be separated by a colon. carry is set if there is any kind of
; error.
;
;----------------------------------------------------------------------------

check_switch:
	call	getchr
	;jc	short err_check
	jc	short err_chk
        and     al,0DFh                 ; convert it to upper case
	cmp	al,'A'
	;jb	short err_check
	jb	short err_chk ; 15/04/2019 - Retro DOS v4.0
	cmp	al,'Z'
	ja	short err_check

	push	es

	push	cs
	pop	es

	mov	cl,[switchlist]		; get number of valid switches
	mov	ch,0
	mov	di,1+switchlist		; point to string of valid switches
	repne	scasb

	pop	es
	jnz	short err_check

	mov	ax,1
	shl	ax,cl			; set bit to indicate switch
	mov	bx,[switches]		; get switches so far
	or	bx,ax			; save this with other switches
	mov	cx,ax
	test	ax,switchnum ; 0F8h	; test against switches that require number to follow
	jz	short done_swtch

	call	getchr
	jc	short err_swtch

	cmp	al,':'
	jne	short err_swtch

	call	getchr
	push	bx			; preserve switches
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	byte [cs:sepchr],' '	; allow space separators
	;mov	byte [sepchr],' '
	call	getnum
	mov	byte [cs:sepchr],0
	;mov	byte [sepchr],0
	pop	bx			; restore switches

; because getnum does not consider carriage-return or line-feed as ok, we do
; not check for carry set here. if there is an error, it will be detected
; further on (hopefully).

	call	process_num

done_swtch:
	;clc
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc
	retn

err_swtch:
	xor	bx,cx			; remove this switch from the records
err_check:
	stc
err_chk:
	retn

;----------------------------------------------------------------------------
;
; procedure : process_num
;
; this routine takes the switch just input, and the number following (if any),
; and sets the value in the appropriate variable. if the number input is zero
; then it does nothing - it assumes the default value that is present in the
; variable at the beginning. zero is ok for form factor and drive, however.
;
;----------------------------------------------------------------------------

	; 02/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:3156h)
process_num:
	test	[switches],cx		; if this switch has been done before,
	jnz	short done_ret		; ignore this one.
	test	cx,flagdrive ; 8
	jz	short try_f
	mov	byte [drive],al
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short done_ret
	;retn	; 13/05/2019
try_f:
	test	cx,flagff ; 80h
	jz	short try_t

; ensure that we do not get bogus form factors that are not supported

	;mov	[deviceparameters+1],al
	mov	[deviceparameters+A_DEVICEPARAMETERS.DP_DEVICETYPE],al
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short done_ret
	;retn	; 13/05/2019
try_t:
	or	ax,ax
	jz	short done_ret		; if number entered was 0, assume default value
	test	cx,flagcyln ; 10h
	jz	short try_s

	;mov	[deviceparameters+4],ax
	mov	[deviceparameters+A_DEVICEPARAMETERS.DP_CYLINDERS],ax
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short done_ret
	;retn	; 13/05/2019
try_s:
	test	cx,flagseclim ; 20h
	jz	short try_h
	mov	[slim],ax
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	jmp	short done_ret
	;retn	; 13/05/2019

; must be for number of heads

try_h:
	mov	[hlim],ax
done_ret:
	;clc
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	clc
	retn

;	M047 -- Begin modifications (too numerous to mark specifically)

;----------------------------------------------------------------------------
;
; procedure : setdeviceparameters
;
; setdeviceparameters sets up the recommended bpb in each bds in the
; system based on the form factor. it is assumed that the bpbs for the
; various form factors are present in the bpbtable. for hard files,
; the recommended bpb is the same as the bpb on the drive.
; no attempt is made to preserve registers since we are going to jump to
; sysinit straight after this routine.
;
;	if we return carry, the DRIVPARM will be aborted, but presently
;	  we always return no carry
;
;	note:  there is a routine by the same name in msdioctl.asm
;
;----------------------------------------------------------------------------

; 15/04/2019 - Retro DOS v4.0

	; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
setdeviceparameters:
	push	es

	push	cs
	pop	es

	xor	bx,bx
	mov	bl,[deviceparameters+A_DEVICEPARAMETERS.DP_DEVICETYPE]
	cmp	bl,DEV_5INCH ; 0
	jne	short got_80

	mov	word [deviceparameters+A_DEVICEPARAMETERS.DP_CYLINDERS],40
							; 48 tpi=40 cyl
got_80:
	shl	bx,1			; get index into bpb table
	mov	si,[bpbtable+bx]	; get address of bpb

	;mov	di,deviceparameters+7	
	; 02/11/2022
	mov	di,deviceparameters+A_DEVICEPARAMETERS.DP_BPB ; es:di -> bpb
	mov	cx,A_BPB.size ; 31
	cld
	;repe	movsb
	; 02/11/2022
	rep	movsb

	pop	es

	test	word [switches],flagseclim ; 20h
	jz	short see_heads

	mov	ax,[slim]
	;mov	[deviceparameters+20],ax
	mov	[deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK],ax

see_heads:
	test	word [switches],flagheads ; 40h
	jz	short heads_not_altered

	mov	ax,[hlim]
	;mov	[deviceparameters+22],ax	
	mov	[deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_HEADS],ax

heads_not_altered:

; set up correct media descriptor byte and sectors/cluster
;   sectors/cluster is always 2 except for any one sided disk or 1.44M

	;mov	byte [deviceparameters+9],2
	; 02/11/2022
	mov	byte [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERCLUSTER],2
	mov	bl,0F0h			; get default mediabyte

;	preload the mediadescriptor from the bpb into bh for convenient access

	;mov	bh,[deviceparameters+17]
	; 02/11/2022
	mov	bh,[deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_MEDIADESCRIPTOR]

	cmp	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_HEADS],2 ; >2 heads?
	ja	short got_correct_mediad ; just use default if heads>2

	jne	short only_one_head	; one head, do one head stuff

;	two head drives will use the mediadescriptor from the bpb

	mov	bl,bh			; get mediadescriptor from bpb

;	two sided drives have two special cases to look for. One is
;	   a 320K diskette (40 tracks, 8 secs per track). It uses
;	   a mediaid of 0fch. The other is 1.44M, which uses only
;	   one sector/cluster.

;	any drive with 18secs/trk, 2 heads, 80 tracks, will be assumed
;	   to be a 1.44M and use only 1 sector per cluster. Any other
;	   type of 2 headed drive is all set.

	cmp	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK],18
	jne	short not_144m
	cmp	word [deviceparameters+A_DEVICEPARAMETERS.DP_CYLINDERS],80
	jne	short not_144m

;	We've got cyl=80, heads=2, secpertrack=18. Set cluster size to 1.

	jmp	short got_one_secperclus_drive

;	check for 320K

not_144m:
	cmp	word [deviceparameters+A_DEVICEPARAMETERS.DP_CYLINDERS],40
	jne	short got_correct_mediad
	cmp	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK],8
	jne	short got_correct_mediad

	mov	bl,0FCh
	jmp	short got_correct_mediad

only_one_head:

;	if we don't have a 360K drive, then just go use 0f0h as media descr.

	cmp	byte [deviceparameters+A_DEVICEPARAMETERS.DP_DEVICETYPE],DEV_5INCH ; 0
	je	short got_one_secperclus_drive

;	single sided 360K drive uses either 0fch or 0feh, depending on
;	  whether sectorspertrack is 8 or 9. For our purposes, anything
;	  besides 8 will be considered 0fch

	mov	bl,0FCh			; single sided 9 sector media id
	cmp	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK],8
	jne	got_one_secperclus_drive ; okay if anything besides 8

	mov	bl,0FEh			; 160K mediaid

;	we've either got a one sided drive, or a 1.44M drive
;	  either case we'll use 1 sector per cluster instead of 2

got_one_secperclus_drive:
	mov	byte [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERCLUSTER],1

got_correct_mediad:
	mov	[deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_MEDIADESCRIPTOR],bl

;	 Calculate the correct number of Total Sectors on medium

	mov	ax,[deviceparameters+A_DEVICEPARAMETERS.DP_CYLINDERS]
	mul	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_HEADS]
	mul	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK]
	mov	word [deviceparameters+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_TOTALSECTORS],ax
	clc				; we currently return no errors

	retn

;	M047 -- end rewritten routine

;----------------------------------------------------------------------------
;
; procedure : organize
;
;----------------------------------------------------------------------------

	; 02/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:3234h)

organize:
	mov	cx,[cs:count]
	jcxz	nochar1

;ifndef	MULTI_CONFIG
;
;;   In MULTI_CONFIG, we map to upper case on a line-by-line basis,
;;   because we the case of values in SET commands preserved
;
;	call	mapcase
;endif

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	call	mapcase

	xor	si,si
	mov	di,si
	xor	ax,ax
	mov	byte [cs:com_level],0
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)	
	;mov	[cs:com_level],al
org1:
	call	skip_comment
	jz	short end_commd_line	; found a comment string and skipped.
	call	get2			; not a comment string. then get a char.
	cmp	al,lf ; 0Ah
	je	short end_commd_line	; starts with a blank line.
	cmp	al,' ' ; 20h
	jbe	short org1		; skip leading control characters
	jmp	short findit

end_commd_line:
	stosb				; store line feed char in buffer for the linecount.
	mov	byte [cs:com_level], 0	; reset the command level.
	jmp	short org1

nochar1:
	stc
	retn

findit:
	push	cx
	push	si
	push	di
	mov	bp,si
	dec	bp
        mov     si,comtab		; prepare to search command table
	mov	ch,0
findcom:
	mov	di,bp
	mov	cl,[si]
	inc	si
	jcxz	nocom

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)

;ifdef	MULTI_CONFIG
;
;;   Simplify future parsing by collapsing ";" onto "REM", and at the same
;;   time skip the upcoming delimiter test (since ";" need not be followed by
;;   anything in particular)
;
;       cmp     byte [es:di],CONFIG_SEMICOLON  ; ';'
;       je	short semicolon
;loopcom:
;       mov     al,[es:di]
;       inc     di
;       and     al,~20h ; 0DFh		; force upper case
;       inc     si                      ; compare to byte @es:di
;       cmp     al,[si-1]
;       loope   loopcom
;;else
;;	repe	cmpsb
;endif
	; 02/11/2022
	repe	cmpsb

	lahf
        add     si,cx                   ; bump to next position without affecting flags
	sahf
        lodsb                           ; get indicator letter
	jnz	short findcom
        cmp     byte [es:di],cr		; the next char might be cr,lf
	je	short gotcom0 		; such as in "rem",cr,lf case.
	cmp	byte [es:di],lf
	je	short gotcom0

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;;ifdef	MULTI_CONFIG
;
;;   Skip the delimiter test for the BEGIN identifier (it doesn't have one).
;
;       cmp     al,CONFIG_BEGIN  ; '['
;       je	short gotcom0
;;endif
;	push	ax
;       mov     al,[es:di]		; now the next char. should be a delim.
;
;;ifdef	MULTI_CONFIG
;
;;   If keyword is *immediately* followed by a question mark (?), then
;;   set the high bit of the ASCII command code (CONFIG_OPTION_QUERY) that is
;;   stored in the CONFIG.SYS memory image.
;
;       cmp     al,'?'                  ; explicit interactive command?
;       jne	short no_query		; no
;       pop     ax                      ; yes, so retrieve the original code
;       or      al,CONFIG_OPTION_QUERY  ; and set the QUERY bit
;       jmp     short gotcom0           ;
;semicolon:
;       mov     al,CONFIG_REM
;       jmp     short gotcom0
;no_query:
;;endif  ;MULTI_CONFIG

	; 02/11/2022
	push	ax
	mov	al,[es:di]		; now the next char. should be a delim.

	call	delim
no_delim:
	pop	ax
	jnz	short findcom
gotcom0:
	pop	di
	pop	si
	pop	cx
	jmp	short gotcom
nocom:
	pop	di
	pop	si
	pop	cx
        mov     al,CONFIG_UNKNOWN  ; 'Z'
	stosb				; save indicator char.
_skipline:
	call	get2
	cmp	al,lf ; 0Ah		; skip this bad command line
        jne     short _skipline
	jmp	short end_commd_line	; handle next command line
gotcom:
        stosb                           ; save indicator char in buffer

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;
;;   Don't pollute "cmd_indicator" with the CONFIG_OPTION_QUERY bit though;
;;   it screws up the direct comparisons below.
;
;       and     al,~CONFIG_OPTION_QUERY ; 7Fh
;;endif
;	mov	[cs:cmd_indicator],al	; save it for the future use.
;
;;ifdef	MULTI_CONFIG
;
;;   There is no whitespace/delimiter between the "begin block" character
;;   ([) and the name of block (eg, [menu]), therefore skip this delimiter
;;   skipping code
;
;       cmp     al,CONFIG_BEGIN
;       je	short org31
;       cmp     al,CONFIG_SUBMENU ; 'O'
;       je      short no_mapcase
;       cmp     al,CONFIG_MENUITEM ; 'E'
;       je      short no_mapcase
;       cmp     al,CONFIG_MENUDEFAULT ; 'A'
;       je      short no_mapcase
;       cmp     al,CONFIG_INCLUDE ; 'J'
;       je      short no_mapcase
;       call    mapcase                 ; map case of rest of line to UPPER
;no_mapcase:
;;endif

	; 02/11/2022
	mov	[cs:cmd_indicator],al	; save it for the future use.
org2:	
	call    get2                    ; skip the command name until delimiter
        cmp     al,lf
	je	short org21
	cmp	al,cr
	je	short org21
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;cmp	al, '/'			; T-RICHJ: Added to allow DEVHIGH/L:...
	;je	short org21		; T-RICHJ: to be parsed properly.

	call	delim
        jnz	short org2
	jmp	short org3
org21:					;if cr or lf then
	dec	si			; undo si, cx register
	inc	cx			;  and continue
org3:	
	cmp	byte [cs:cmd_indicator],CONFIG_COMMENT ; 'Y'
	je	short get_cmt_token
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
        ;cmp     byte [cs:cmd_indicator],CONFIG_DEVICE ; 'D'
	;je	short org_file
        cmp     byte [cs:cmd_indicator],CONFIG_INSTALL ; 'I'
	je	short org_file
        ;cmp	byte [cs:cmd_indicator],CONFIG_INSTALLHIGH ; 'W'
        ;je	short org_file
	; 02/11/2022
	cmp     byte [cs:cmd_indicator],CONFIG_DEVICE ; 'D'
	je	short org_file
        cmp     byte [cs:cmd_indicator],CONFIG_SHELL ; 'S'
	je	short org_file
        cmp	byte [cs:cmd_indicator],CONFIG_SWITCHES ; '1'
	je	short org_switch
org31:
	jmp	org4

org_switch:
	call	skip_comment
	jz	short end_commd_line_brdg

	call	get2
	call	org_delim
	jz	short org_switch

	stosb
	jmp	org5

org_file:			; get the filename and put 0 at end
	call	skip_comment
	jz	short org_put_zero

	call	get2		; not a comment
	call	delim
	jz	short org_file	; skip the possible delimiters

	stosb			; copy the first non delim char found in buffer

org_copy_file:
	call	skip_comment	; comment char in the filename?
	jz	short org_put_zero ; then stop copying filename at that point

	call	get2
	cmp	al,'/'		; a switch char? (device=filename/xxx)
	je	short end_file_slash ; this will be the special case.

	stosb			; save the char. in buffer
	call	delim
	jz	short end_copy_file

	cmp	al, ' '
	ja	short org_copy_file ; keep copying
	jmp	short end_copy_file ; otherwise, assume end of the filename.

get_cmt_token:			; get the token. just max. 2 char.
	call	get2
	cmp	al,' '		; skip white spaces or "=" char.
	je	short get_cmt_token ; (we are allowing the other special
	cmp	al,tab ; 9 	;  characters can used for comment id.
	je	short get_cmt_token ;  character.)
	cmp	al,'='		; = is special in this case.
	je	short get_cmt_token
	cmp	al,cr
	je	short get_cmt_end ; cannot accept the carriage return
	cmp	al,lf
	je	short get_cmt_end

	mov	[cs:cmmt1],al	; store it
	mov	byte [cs:cmmt],1 ; 1 char. so far.
	call	get2
	cmp	al,' ' ; 20h
	je	short get_cmt_end
	cmp	al,tab ; 9
	je	short get_cmt_end
	cmp	al,cr  ; 0Dh
	je	short get_cmt_end
	cmp	al,lf  ; 0Ah
	je	short end_commd_line_brdg

	mov	[cs:cmmt2],al
	inc	byte [cs:cmmt]

get_cmt_end:
	call	get2
	cmp	al,lf
	jne	short get_cmt_end	; skip it.
end_commd_line_brdg: 
	jmp	end_commd_line 		; else jmp to end_commd_line

org_put_zero:				; make the filename in front of
	mov	byte [es:di],0		;  the comment string to be an asciiz.
	inc	di
	jmp	end_commd_line		;  (maybe null if device=/*)

end_file_slash: 			; al = "/" option char.
	mov	byte [es:di],0		; make a filename an asciiz
	inc	di			; and
	stosb				; store "/" after that.
	jmp	short org5		; continue with the rest of the line

end_copy_file:
	mov	byte [es:di-1],0	; make it an asciiz and handle the next char.
	cmp	al,lf
	je	short end_commd_line_brdg
	jmp	short org5

org4:					; org4 skips all delimiters after the command name except for '/'
	call	skip_comment
	jz	short end_commd_line_brdg

	call	get2
	call	org_delim		; skip delimiters except '/' (mrw 4/88)
	jz	short org4
	jmp	short org51

org5:					; rest of the line
	call	skip_comment		; comment?
	jz	short end_commd_line_brdg
	call	get2			; not a comment.

org51:
	stosb				; copy the character
	cmp	al,'"' 	; 22h		; a quote ?
	je	short at_quote
	cmp	al,' '  ; 20h
	ja	short org5
					; M051 - Start

        cmp	byte [cs:cmd_indicator],CONFIG_DEVICEHIGH ; Q: is this devicehigh
	jne	short not_dh		; N: 
	cmp	al,lf			; Q: is this line feed
	je	short org_dhlf		; Y: stuff a blank before the lf
	cmp	al,cr			; Q: is this a cr
	jne	short org5		; N: 
	mov	byte [es:di-1],' '	; overwrite cr with blank
	stosb				; put cr after blank
	inc	byte [cs:insert_blank]	; indicate that blank has been 
					; inserted
	jmp	short org5
not_dh:					; M051 - End

	cmp	al,lf			; line feed?
	je	short org1_brdg		; handles the next command line.
	jmp	short org5		; handles next char in this line.

org_dhlf:				; M051 - Start
	cmp	byte [cs:insert_blank],1 ; Q:has a blank already been inserted
	je	short org1_brdg		; Y:
	mov	byte [es:di-1],' '	; overwrite lf with blank
	stosb				; put lf after blank
					; M051 - End
org1_brdg: 
	mov	byte [cs:insert_blank],0 ; M051: clear blank indicator for 
					; M051: devicehigh
	jmp	org1

at_quote:
	cmp	byte [cs:com_level],0
	je	short up_level
	mov	byte [cs:com_level],0	; reset it.
	jmp	short org5

up_level:
	inc	byte [cs:com_level]	; set it.
	jmp	short org5

;----------------------------------------------------------------------------
;
; procedure : get2
;
;----------------------------------------------------------------------------

	; 02/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:33FAh)
get2:
	jcxz	noget
	;
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	;;lods	byte ptr es:[si]
	;es	
	;lodsb
	mov	al, [es:si]
	inc	si
	;
	dec	cx
	retn
noget:
	pop	cx
	mov	[cs:count],di ; 13/05/2019
	mov	[cs:org_count],di
	xor	si,si
	mov	[cs:chrptr],si

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifndef MULTI_CONFIG
;;	retn
;;else
;
;;   This was the rather kludgy way out of procedure "organize", but instead
;;   of returning to doconf, we now want to check config.sys BEGIN/END blocks
;;   and the new boot menu stuff...
;
;	mov     cx,di
;	jmp     menu_check
;;endif

	; 02/11/2022
	retn

;----------------------------------------------------------------------------
;
; procedure : skip_comment
;
;skip the commented string until lf, if current es:si-> a comment string.
;in) es:si-> string
;	 cx -> length.
;out) zero flag not set if not found a comment string.
;	  zero flag set if found a comment string and skipped it. al will contain
;	  the line feed character at this moment when return.
;	  ax register destroyed.
;	  if found, si, cx register adjusted accordingly.
;
;----------------------------------------------------------------------------

skip_comment:
	jcxz	noget		; get out of the organize routine.
	cmp	byte [cs:com_level],0 ; only check it if parameter level is 0.
	jne	short no_commt	;  (not inside quotations)

	cmp	byte [cs:cmmt],1
	jb	short no_commt

	mov	al,[es:si]
	cmp	[cs:cmmt1],al
	jne	short no_commt

	cmp	byte [cs:cmmt],2
	jne	short skip_cmmt

	mov	al,[es:si+1]
	cmp	[cs:cmmt2],al
	jne	short no_commt
skip_cmmt:
	jcxz	noget		; get out of organize routine.
	mov	al,[es:si]
	inc	si
	dec	cx
	cmp	al,lf		; line feed?
	jne	short skip_cmmt
no_commt:
	retn

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
%if 0

;ifdef	MULTI_CONFIG

;----------------------------------------------------------------------------
;
;   kbd_read: wait for keystroke
;
;   INPUT
;       DS == CS == sysinitseg
;
;   OUTPUT
;       Carry SET to clean boot, CLEAR otherwise
;
;   OTHER REGS USED
;       All
;
;   HISTORY
;       Created 16-Nov-1992 by JeffPar
;
;----------------------------------------------------------------------------

kbd_read:
        test    byte [bDisableUI],2
        jnz     short kbd_nodelay

        push    ds              ; the bios timer tick count is incremented
        sub     ax,ax           ; 18.2 times per second;
        mov     ds,ax           ; watch the timer tick count for 37 transitions
	;mov	dx,[046Ch]	; get initial value
kbd_loop:
        mov     ah,1            ;
        int     16h             ; peek the keyboard
        jnz	short kbd_loopdone ; something's there, get out
        mov     ah,2            ; peek the shift states
        int     16h             ;
        test    al,03h          ; either right or left shift key bits set?
        jnz	short kbd_loopdone ; yes
        mov     ax,[046Ch]	;
	;sub	ax,dx           ; get difference
	; 15/04/2019 - Retro DOS v4.0
	sub	ax,[cs:_timer_lw_] ; MSDOS 6.21 IO.SYS - SYSINIT:42E5h        

	cmp     al,37           ; reached limit?
        jb	short kbd_loop	; not yet
kbd_loopdone:
        pop     ds              ; delay complete!
kbd_nodelay:
        sub     bx,bx           ; assume clean boot
        mov     ah,2            ; peek the shift states
        int     16h             ;
        test    al,03h          ; either right or left shift key bits set?
        jz      short kbd_notshift ; no
        inc     bx              ; yes
        inc     bx
	; MSDOS 6.21 IO.SYS - SYSINIT:4301h
	or	byte [bQueryOpt],4
kbd_notshift:                   ;
        mov     ah,1            ; peek the keyboard
        int     16h             ;
        jz	short kbd_test	; no key present
        or      al,al           ; is it a function key?
        jnz	short kbd_test	; no

	; MSDOS 6.21 IO.SYS - SYSINIT:430Bh
        cmp     ah,62h          ; CTRL F5
        je	short kbd_cfg_bypass
	
        cmp     ah,3Fh          ; F5 function key?
        jne	short kbd_notf5	; no
kbd_cfg_bypass:
        mov     dx,_$CleanMsg
        call    print
	; MSDOS 6.21 IO.SYS - SYSINIT:431Bh
	or	byte [bQueryOpt],4           ;
        jmp     short kbd_eat   ; yes, clean boot selected
kbd_notf5:
	; MSDOS 6.21 IO.SYS - SYSINIT:4322h
        cmp     ah,65h          ; CTRL F8
        je	short kbd_cfg_confirm

        cmp     ah,42h          ; F8 function key?
        jne	short kbd_exit	; no
kbd_cfg_confirm:
        mov     dx,_$InterMsg
        call    print           ;
        mov     bl,1            ; yes, interactive-boot option enabled
        mov     [bQueryOpt],bl  ; change default setting
kbd_eat:                        ;
        mov     ah,0            ;
        int     16h             ; eat the key we assumed was a signal
        mov	byte [secElapsed],-1
        or      bx,bx           ;
        jz	short kbd_clean	;
kbd_test:                       ;
        cmp     bl,2            ;
        jb	short kbd_exit	;
kbd_clean:                      ;
        call    disable_autoexec; yes, tell COMMAND to skip autoexec.bat
        stc                     ; set carry to indicate abort
        retn			;
kbd_exit:                       ;
        clc                     ; clear carry to indicate success
        retn			;

;----------------------------------------------------------------------------
;
;   set_numlock: set numlock LED
;
;   INPUT
;       ES:SI -> numlock setting (ie, "ON" or "OFF")
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   HISTORY
;       Created 16-Nov-1992 by JeffPar
;
;----------------------------------------------------------------------------

set_numlock:
        push    ax
        push    ds
        sub     ax,ax
        mov     ds,ax
        mov     ax,[es:si]      ; get 1st 2 bytes of value (ON or OF)
        cmp     ax,[cs:OnOff+2]	; should we turn it off?
        jne	short not_off	; no
        and     byte [0417h],~20h ; 0DFh
        jmp     short set_done
not_off:
        cmp     ax,[cs:OnOff]	; should we turn it on?
        stc
        jne	short set_done	; no
        or      byte [0417h],20h
set_done:
        pop     ds
        pop     ax
        retn

; 16/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
;   menu_check:  check for presence of menu (and other) configuration blocks
;
;   INPUT
;       CX == "organized" config.sys memory image length
;    ES:SI -> "organized" config.sys memory image
;       DS == CS == sysinitseg
;
;   OUTPUT
;       Same as above;  the idea is that menu_check simply transforms
;       a block-structured config.sys image into a conventional image,
;       based on the user's block selection and any other boot-time options
;       the user may have employed...
;
;   OTHER REGS USED
;       All
;
;   NOTES
;       [count] and [org_count] are set to the new config.sys image length
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

menu_check:

;   Search for SWITCHES, determine if /N or /F are present;  if so, then
;   disable clean/interactive boot options

        push    cx              ;
        push    si              ;
        sub     bx,bx           ; remains ZERO until first block
swchk_loop:                     ;
        call    get_char        ; get first char of current line
        jc	short swchk_end	; hit eof
        cmp     al,CONFIG_BEGIN ;
        jne	short swchk_next1 ;
        inc     bx              ; remember that we've seen a block
        jmp     short swchk_nextline
swchk_next1:                    ;
        cmp     al,CONFIG_NUMLOCK
        jne	short swchk_next2 ;
        or      bx,bx           ; only do NUMLOCK commands that exist
        jnz	short swchk_nextline ; before the first block
        call    set_numlock     ; REM it out so we don't act on it later, too
        mov     byte [es:si-1],CONFIG_REM
        jmp     short swchk_nextline
swchk_next2:                    ;
        cmp     al,CONFIG_SWITCHES
        jne	short swchk_nextline ; this line ain't it
swchk_scan:                     ;
        call    get_char        ; look for /N or /F
swchk_scan1:                    ;
        cmp     al,LF           ; end of line?
        je	short swchk_nextline ; yes
        cmp     al,'/'          ; switch-char?
        jne	short swchk_scan ; no
        call    get_char        ;
        and     al,~20h ; 0DFh	; convert to upper case
        cmp     al,[swit_n+1]
        jne	short swchk_scan2 ; no
        or      byte [bDisableUI],1
        jmp	short swchk_scan ; continue looking for switches of interest
swchk_scan2:                    ;
        cmp     al,[swit_f+1]
        jne	short swchk_scan1 ; no
        or      byte [bDisableUI],2
        jmp     short swchk_scan ; continue looking for switches of interest
swchk_nextline:                 ;
        call    skip_opt_line   ;
        jmp     short swchk_loop ;
swchk_end:                      ;
        pop     si              ;
        pop     cx              ;

;   Do the keyboard tests for clean/interactive boot now, but only if
;   the DisableUI flag is still clear

        test    byte [bDisableUI],1
        jnz	short menu_search
;
;   Wait for 2 seconds first, UNLESS the /F bit was set in bDisableUI, or
;   there is anything at all in the keyboard buffer
;
        call    kbd_read
        jnc	short menu_search
        jmp	menu_abort

;   Search for MENU block;  it is allowed to be anywhere in config.sys

menu_search:
        sub     bx,bx           ; if no MENU, default to zero for no_selection
        mov     di,szMenu	;
        call    find_block      ; find the MENU block
        jnc	short menu_found ;
        mov     byte [szBoot],0
        jmp	no_selection ; not found

;   Process the requested menu color(s)

menu_color:
        push    cx              ;
        push    dx              ;
        mov     dx,0007h        ; default color setting
        call    get_number	; get first number
        and     bl,0Fh		; first # is foreground color (for low nibble)
        mov     ch,bl           ; save it in CH
        and     dl,0F0h         ;
        or      dl,bl           ;
        call    delim           ; did we hit a delimiter
        jne	short check_color ; no, all done
        call    get_number	; get next number
        and     bl,0Fh		; second # is background color (for high nibble)
        mov     dh,bl           ; save it in DH
        and     dl,0Fh		;
        mov     cl,4            ;
        shl     bl,cl           ;
        or      dl,bl           ;
check_color:                    ;
        cmp     ch,dh           ; are foreground/background the same?
        jne	short set_color	; no
        xor     dl,08h          ; yes, so modify the fgnd intensity
set_color:
        mov     [bMenuColor],dl ;
        pop     dx              ;
        pop     cx              ;
        jmp	menu_nextitem

;   Back to our regularly scheduled program (the COLOR and other goop
;   above is there simply to alleviate short jump problems)

menu_found:
        mov     byte [bDefBlock],1
        mov     word [offDefBlock],0
        mov     byte [secTimeOut],-1
        and     byte [bQueryOpt],~2 ; 0FDh

        call    skip_opt_line   ; skip to next line
        sub     dx,dx           ; initialize total block count (0 => none yet)

;   Process the menu block now

menu_process:
        call    get_char        ; get first char of current line
        jc	short to_menu_getdefault ; could happen if menu block at end (rare)
        and     al,~CONFIG_OPTION_QUERY ; 7Fh
        cmp     al,CONFIG_BEGIN ; BEGIN implies END
        je	short to_menu_getdefault
        cmp     al,CONFIG_SUBMENU
        je	short menu_item	; go process sub-menu
        cmp     al,CONFIG_MENUITEM
        je	short menu_item	; go process menu item
        cmp     al,CONFIG_MENUDEFAULT
        je	short menu_default ; go process menu default
        cmp     al,CONFIG_MENUCOLOR
        je	short menu_color ; go process menu color
        cmp     al,CONFIG_NUMLOCK
        je	short menu_numlock ;
        cmp     al,CONFIG_REM   ; allow remarks in menu block
        je	short menu_nextitem ;
        call    any_delim       ; allow blank lines and such
        je	short menu_nextitem ;
        stc                     ;
        call    print_error     ; non-MENU command!
        jmp     short menu_nextitem
menu_numlock:
        call    set_numlock
        jmp     short menu_nextitem
to_menu_getdefault:
        jmp     short menu_getdefault

;   Save the offset of the default block name, we'll need it later

menu_default:
        mov     [offDefBlock],si; save address of default block name
        cmp     byte [secElapsed],0
        jne	short timeout_skip ; secElapsed is only zero for the FIRST menu,
        call    skip_token      ; and for subsequent menus IF nothing was typed;
        jc	short menu_nextitem ; secElapsed becomes -1 forever as soon as
        call    skip_delim      ; something is typed
        jc	short menu_nextitem ;
        mov     si,bx           ;
        call    get_number      ; get number (of seconds for timeout)
        cmp     bl,90           ; limit it to a reasonable number
        jb	short timeout_ok ; (besides, 99 is the largest # my simple
        mov     bl,90           ;  display function can handle)
timeout_ok:                     ;
        mov     [secTimeOut],bl ;
timeout_skip:
        jmp     short menu_nextitem

;   Verify that this is a valid menu item by searching for the named block

menu_item:
        cmp     dl,MAX_MULTI_CONFIG ; have we reached the max # of items yet?
        jae	short menu_nextitem ;
        mov     di,si           ; DS:DI -> block name to search for
        call    srch_block      ;
        je	short menu_itemfound ;
        stc                     ;
        call    print_error     ; print error and pause
        jmp     short menu_nextitem ; if not found, ignore this menu item

;   srch_block, having succeeded, returns DI -> past the token that it
;   just matched, which in this case should be a descriptive string; ES:SI
;   and CX are unmodified

menu_itemfound:
        inc     dx              ; otherwise, increment total block count
        mov     bx,dx           ; and use it to index the arrays of offsets
        mov	[abBlockType+bx],al
        add     bx,bx           ; of recorded block names and descriptions

;   There should be a description immediately following the block name on
;   MENUITEM line; failing that, we'll just use the block name as the
;   description...

        mov     [aoffBlockName+bx],si
        mov     [aoffBlockDesc+bx],si
        mov     di,bx           ; skip_delim modifies BX, so stash it in DI
        call    skip_token      ;
        jc	short menu_nextitem ; hit eol/eof
        call    skip_delim      ;
        jc	short menu_nextitem ; hit eol/eof
        xchg    bx,di           ;
        mov     [aoffBlockDesc+bx],di

menu_nextitem:
        call    skip_opt_line   ;
        jmp     menu_process    ; go back for more lines

;   Display menu items now, after determining which one is default

menu_getdefault:
        or      dl,dl           ; where there any valid blocks at all?
        jnz	short menu_valid ; yes
        sub     bx,bx           ; no, so force autoselect of 0
        jmp     menu_autoselect ; (meaning: process common blocks only)
menu_valid:
        sub     bx,bx           ;
        mov     [bMaxBlock],dl  ; first, record how many blocks we found
        mov     di,[offDefBlock];
        or      di,di           ; does a default block exist?
        jz	short menu_nodefault ; no
        inc     bx              ; yes, walk name table, looking for default
menu_chkdefault:
        push    bx              ;
        add     bx,bx           ;
        mov     si,[aoffBlockName+bx]
        mov     cx,128          ; arbitrary maximum length of a name
        push    ds              ;
        push    es              ;
        pop     ds              ;
        call    comp_names      ; is this block the same as the default?
        pop     ds              ;
        pop     bx              ;
        je	short menu_setdefault ; yes
        inc     bx              ;
        cmp     bl,[bMaxBlock]  ; all done searching?
        jbe	short menu_chkdefault ; not yet
menu_nodefault:
        mov     bl,1            ; if no default, force default to #1
menu_setdefault:
        mov     [bDefBlock],bl  ; yes, this will be the initial current block

;   If the timeout was explicitly set to 0 (or technically, anything that
;   failed to resolve to a number, like "NONE" or "EAT POTATOES"), then we're
;   supposed to skip menu display and run with the specified default block;
;   however, if the user hit Enter prior to boot, thereby requesting fully
;   INTERACTIVE boot, then we shall display the menu block anyway (though still
;   with no timeout)

        cmp     byte [secTimeOut],0 ; is timeout zero? (ie, assume default)
        jne	short menu_display ; no
        test    byte [bQueryOpt],1 ; yes, but was INTERACTIVE requested?
        jnz	short menu_display ; yes, so *don't* assume default after all
        jmp     not_topmenu	;

;   Reset the mode, so that we know screen is clean and cursor is home

menu_display:
        mov     ah,0Fh          ; get current video mode
        int     10h             ;
        mov     ah,00h          ; just re-select that mode
        int     10h             ;
        push    es              ;
        mov     ax,40h          ; reach down into the ROM BIOS data area
        mov     es,ax           ; and save the current (default) video page
        mov     ax,[es:004Eh]   ; start address and page #, in case the
        mov     [wCRTStart],ax  ; undocumented QUIET option was enabled
        mov     al,[es:0062h]   ;
        mov     [bCRTPage],al   ;
        mov     ax,[bMenuPage]	; select new page for menu
        int     10h             ;
        mov     ax,0600h        ; clear entire screen
        mov     bh,[bMenuColor] ; using this color
        sub     cx,cx           ; upper left row/col
        ;mov	dl,[es:CRT_Cols] 
        mov	dl,[es:4Ah]
	dec     dl              ;
        ;mov	dh,[es:CRT_Rows];
        mov	dh,[es:84h]
	or      dh,dh           ; # of rows valid?
        jnz	short menu_clear ; hopefully
        mov     dh,[bLastRow]   ; no, use a default
menu_clear:
        int     10h             ; clear the screen using the req. attribute
        pop     es              ;
        mov     [bLastRow],dh   ; save DH
        mov     dx,_$MenuHeader
        call    print           ; cursor now on row 3 (numbered from 0)

        test    byte [bDisableUI],1
        jnz     short menu_nostatus
        mov     bh,[bMenuPage]  ;
        mov     dh,[bLastRow]   ; restore DH
        mov     dl,0            ; print the status line on row DH, col 0,
        mov     ah,02h          ; now that we can trash the cursor position
        int     10h             ;
        mov     dx,_$StatusLine
        call    print           ;
        mov     ah,3            ; get cursor position
        int     10h             ;
        sub     dl,2            ;
        mov     [bLastCol],dl   ; save column where status char will go

menu_nostatus:
        mov     bx,1            ; now prepare to display all the menu items
menu_disploop:
        call    print_item	; print item #BL
        inc     bx              ; why "inc bx"?  because it's a 1-byte opcode
        cmp     bl,[bMaxBlock]  ; all done?
        jbe	short menu_disploop ; not yet

;   Set cursor position to just below the menu items

        mov     dl,0            ; select column
        mov     dh,bl           ;
        add     dh,4            ; select row below menu
        mov     bh,[bMenuPage]  ;
        mov     ah,02h          ; set cursor position beneath the block list
        int     10h             ;

        mov     dx,_$MenuPrmpt
        call    print           ;
        call    select_item     ; make a selection, return # in BX
        mov     dx,crlfm	
        call    print           ;
        push    word [bDisableUI]
        or      byte [bDisableUI],1
        call    show_status     ; clear the status line now
        pop     word [bDisableUI]

;   Now begins the "re-organization" process...

menu_autoselect:
        cmp     bx,-1           ; clean boot requested?
        jne	short normal_boot ; no
        call    disable_autoexec; basically, add a /D to the command.com line
menu_abort:
        sub     cx,cx           ; then immediately exit with 0 config.sys image
        jmp	menu_exit	;

normal_boot:
        cmp     bx,-2           ; back to top-level menu?
        jne	short not_topmenu ; no
        mov     cx,[count]      ; yes, start all over
        sub     si,si           ;
        jmp     menu_search

not_topmenu:
        cmp     byte [abBlockType+bx],CONFIG_SUBMENU
        jne	short not_submenu
        add     bx,bx           ;
        mov     di,[aoffBlockName+bx]
        call    srch_block      ; THIS CANNOT FAIL!
        mov     si,di           ;
        mov     cx,bx           ; ES:SI and CX are ready for another round
        jmp     menu_found

not_submenu:
        add     bx,bx           ; get BX -> name of selected block
        mov     bx,[aoffBlockName+bx]

;   BX should now either be ZERO (meaning no block has been selected) or
;   the offset relative to ES of the block name to be processed (along with
;   all the "common" lines of course)

no_selection:
        mov     [offDefBlock],bx; save selection
        mov     cx,[count]      ; reset ES:SI and CX for reprocessing
        sub     si,si           ;
        push    ds              ;
        mov     ds,[config_wrkseg]; this is where we'll store new config.sys image
        sub     di,di           ;

;   ES:SI-> config.sys, DS:DI -> new config.sys workspace
;
;   Work our way through the config.sys image again, this time copying
;   all lines that are (A) "common" lines outside any block or (B) lines
;   within the requested block.  Lines inside INCLUDEd blocks are transparently
;   copied by copy_block in a recursive fashion;  the amount of recursion is
;   limited by the fact INCLUDE statements are REMed by copy_block as they are
;   processed and by the number of unique INCLUDE stmts in config.sys...
;
;   BUGBUG 20-Mar-1992 JeffPar: If we can figure out the lower bound of the
;   stack we're running on, then we should check it inside copy_block

copyblock_loop:
        push    bx              ; save selected block name
        call    copy_block      ; process (named or common) block
        pop     bx              ;
        jc	short move_config ; hit eof

;   copy_block can only return for two reasons:  it hit eof or a new block

copyblock_begin:
        push    ax              ;
        push    cx              ;
        push    si              ;
        push    di              ; always do "common" blocks
        mov     di,szCommon
        push    ds              ;
        push    cs              ;
        pop     ds              ;
        call    comp_names      ;
        pop     ds              ;
        pop     di              ;
        pop     si              ;
        pop     cx              ;
        pop     ax              ;
        je	short copyblock_check
        or      bx,bx           ; is there a block name to check?
        jz	short copyblock_skip ; no
        push    di              ;
        mov     di,bx           ; check block against given block name
        push    ds              ;
        push    es              ;
        pop     ds              ;
        call    comp_names      ; is this the block we really want to do?
        pop     ds              ;
        pop     di              ;
copyblock_check:
        jc	short move_config ; hit eof
        jne	short copyblock_skip  ;
        call    skip_opt_line   ;
        jmp	short copyblock_loop 

copyblock_skip:                 ;
        call    skip_opt_line   ; this ain't the block we wanted, so skip it
        call    get_char        ;
        jc	short move_config ; hit eof
        and     al,~CONFIG_OPTION_QUERY ; 7Fh
        cmp     al,CONFIG_BEGIN ;
        je	short copyblock_begin
        jmp     short copyblock_skip ; anything else is just skipped
;
;   To create as little risk to the rest of SysInit as little as possible,
;   and to free the workspace at "config_wrkseg" for creating an environment,
;   copy the new config.sys image to "confbot"
;
move_config:
        mov     cx,di           ; now copy workspace at DS:DI to "confbot"
        push    cx              ;
;
;   But first, copy the CONFIG=<configuration><0> string to the workspace,
;   since the configuration name only currently exists in the "confbot" area
;
 	;mov	cx,7
	mov     cx,szMenu-szBoot-1
        mov     si,szBoot	; first copy the CONFIG= part
        inc     di              ; skip a byte, in case absolutely nothing
                                ; was copied to the workspace, because we always
                                ; zero the first byte of the workspace (below)
copy_boot: 
	;lods    byte ptr cs:[si];
        cs
	lodsb
	mov     [di],al         ;
        inc     di              ;
        loop    copy_boot       ;

        push    es              ; then copy the configuration name
        mov     cx,128-7        ; put an upper limit on the name, to be safe
        mov     si,[cs:offDefBlock]; ES:SI -> default block name
        or      si,si           ; valid?
        jnz	short l1	; yes
        push    cs              ;
        pop     es              ;
        mov     si,szCommon
l1:     mov     al,[es:si]      ;
        call    any_delim       ;
        je	short l2	;
        mov     [di],al         ;
        inc     si              ;
        inc     di              ;
        loop    l1              ;
l2:     mov     byte [di],lf	; terminate the configuration string
        pop     es              ;

;   Now we can copy "config_wrkseg" (DS) to "confbot" (ES)

        sub     di,di           ;
        mov     [cs:config_envlen],di
        sub     si,si           ;
        pop     cx              ; recover the size of "config_wrkseg"

        push    cx              ;
        rep     movsb           ; moved!
        pop     cx              ;
        mov     ax,ds           ;
        pop     ds              ;

;   Now that the config_wrkseg is available once again, we shall
;   use it to create an environment. The first thing to go in will be
;   the "CONFIG=configuration" thing. It is also important to zero
;   the first byte of the workspace, so that copy_envvar knows the buffer
;   is empty.

        push    es              ;
        mov     es,ax           ;
        inc     si              ; ES:SI -> "CONFIG=configuration"
        mov     byte [es:0],0	;empty the environment block
        call    copy_envvar     ; copy envvar at ES:SI to "config_wrkseg"
        pop     es

;   Before returning, restore the default video page setting but do NOT
;   do it using INT 10h's Set Active Page function, because if the menu was
;   displayed on a different page, then it's because we don't want to see
;   all the device driver/TSR goop (which goes to the default page)

menu_done:
        cmp     byte [bMenuPage],0
        je	short menu_exit	;
        push    es              ;
        mov     ax,40h          ;
        mov     es,ax           ;
        mov     ax,[wCRTStart]  ;
        mov     [es:004Eh],ax   ;
        mov     al,[bCRTPage]   ;
        mov     [es:0062h],al   ;
        pop     es              ;
menu_exit:
        mov     [count],cx      ; set new counts
        mov     [org_count],cx  ;
        sub     si,si           ; always return ES:SI pointing to config.sys
        retn

;----------------------------------------------------------------------------
;
;   copy_envvar:  copy the envvar at ES:SI to "config_wrkseg"
;
;   INPUT
;    ES:SI -> environment variable (in the form "var=string<cr/lf>")
;
;   OUTPUT
;       config_envlen (ie, where to put next envvar) updated appropriately
;       carry set if error (eg, missing =); clear otherwise
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 29-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

copy_envvar:
        push    cx              ;
        push    si              ;
        push    ds              ;
        push    es              ;
        push    es              ;
        mov     es,[config_wrkseg] ; ES:DI to point to next available byte
        pop     ds                 ; DS:SI to point to envvar

;   Have to calculate the length of the variable name (and if we hit
;   the end of the line before we hit '=', then it's curtains for this
;   config.sys line)
;
;   The check for NULL is important because copy_envvar is also used to copy
;   the initial CONFIG= setting, which will have been zapped by a NULL if no
;   menu block existed (in order to prevent the creation of an environment)

        sub     cx,cx           ;
copy_varlen:                    ;
        lodsb                   ;
        or      al,al           ; NULL?
        stc                     ;
        jz	short copy_envexit ; yes, abort
        cmp     al,cr          ;
        stc                     ;
        je	short copy_envexit
        cmp     al,lf          ;
        stc                     ;
        je	short copy_envexit
        inc     cx              ;
        cmp     al,'='          ;
        jne	short copy_varlen
        mov     al,0            ;
        mov     ah,[si]         ; save char after '='
        sub     si,cx           ; back up to given varname
        dec     cx              ; CX == # of bytes in varname
        sub     di,di           ; start looking for DS:SI at ES:0
copy_varsrch:
        cmp     byte [es:di],al
        je	short copy_envprep ; search failed, just copy var
        mov     bx,di           ; ES:BX -> start of this varname
        push    cx              ;
        push    si              ;
        repe    cmpsb           ;
        pop     si              ;
        pop     cx              ;
        jne	short copy_varnext ; no match, skip to next varname
        cmp     byte [es:di],'='
        jne     short copy_varnext ; no match, there's more characters

;   Previous occurrence of variable has been found; determine the
;   entire length and then destroy it

        mov     cx,-1           ;
        repne   scasb           ; guaranteed to get null (since we put it there)
        push    si              ;
        mov     si,di           ;
        mov     di,bx           ;
        mov     cx,[cs:config_envlen]
        sub     cx,si           ; destroy variable now
	;rep movs byte ptr es:[di],byte ptr es:[si]
	;;db 0F3h,26h,0A4h ; MSDOS 6.21 IO:SYS - SYSINIT:4724h

	rep	; 0F3h
	es	; 26h
	movsb	; 0A4h

	pop     si
copy_envprep:
        cmp     ah,cr          ; if there is nothing after the '='
        je	short copy_envdel ; then just exit with variable deleted
        cmp     ah,lf           ;
        je	short copy_envdel
        jmp     short copy_envloop

copy_varnext:                   ;
        push    cx              ;
        mov     cx,-1           ;
        repne   scasb           ;
        pop     cx              ;
        jmp	short copy_varsrch

copy_envloop:                   ;
        lodsb                   ;
        cmp     al,cr           ;
        je	short copy_envdone
        cmp     al,lf           ;
        je	short copy_envdone
        stosb                   ;
        jmp     short copy_envloop

copy_envdone:                   ;
        sub     al,al           ; do SUB to clear carry as well
        stosb                   ; always null-terminate these puppies
copy_envdel:                    ;
        mov     [es:di],al      ; and stick another null to terminate the env.
        mov     [cs:config_envlen],di

copy_envexit:                   ;
        pop     es              ;
        pop     ds              ;
        pop     si              ;
        pop     cx              ;
        retn

;----------------------------------------------------------------------------
;
;   copy_block:  copy the current block to the new config.sys workspace
;
;   INPUT
;       CX == remaining bytes in "organized" config.sys memory image
;    ES:SI -> remaining bytes in "organized" config.sys memory image
;    DS:DI -> new config.sys workspace (equal in size to the original
;             config.sys image) where the current block is to be copied
;
;   OUTPUT
;       Same as above
;       AL also equals the last character read from the organized image
;
;   OTHER REGS USED
;       All
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

copy_block:
        call    get_char        ; check for include
        jc	short copy_done	;
	and     al,~CONFIG_OPTION_QUERY ; 7Fh
	cmp     al,CONFIG_BEGIN ; another BEGIN implies END as well
        je	short copy_done ;

        cmp     al,CONFIG_INCLUDE ; 'J'
        mov     al,ah           ; AL == the original line code
        jne	short copy_line	; not an "include" line

;   We have hit an "INCLUDE" line; first, REM out the line so that we
;   never try to include the block again (no infinite include loops please),
;   then search for the named block and call copy_block again.

        mov     byte [es:si-1],CONFIG_REM ; '0'
        push    di              ;

        mov     di,szMenu
        call    comp_names_safe ; don't allow INCLUDE MENU
        je	short copy_skip	;

        mov     di,szCommon
        call    comp_names_safe ; don't allow INCLUDE COMMON
        je	short copy_skip	;

        mov     di,si           ; try to find the block
        call    srch_block      ;
        mov     dx,di           ;
        pop     di              ;
        jne	short copy_error ; no such block
        push    cx              ;
        mov     cx,bx           ;
        push    si              ;
        dec     dx              ;
        mov     si,dx           ;
        call    skip_line       ; skip the rest of the "block name" line
        call    copy_block      ; and copy in the rest of that block
        pop     si              ;
        pop     cx              ;
        sub     al,al           ; force skip_opt_line to skip...
        jmp     short copy_nextline

copy_skip:
        pop     di
copy_error:
        clc                     ;
        call    print_error     ; note that carry is clear, no pause
        jmp     short copy_nextline

;   Copy the line at ES:SI to the current location at DS:DI

copy_line:
        mov     [di],al         ;
        inc     di              ;
        cmp     al,' '          ; is this is a "real" line with a "real" code?
        jb	short copy_nextline ; no
        cmp     byte [cs:config_multi],0
        je	short copy_loop	; not a multi-config config.sys, don't embed #s
        call    get_linenum	; BX == line # of line @ES:SI
        mov     [di],bx         ; stash it immediately following the line code
        inc     di              ;
        inc     di              ;
        jmp     short copy_next ;
copy_loop:                      ;
        call    get_char        ;
        jc	short copy_done ; end of file
        mov     [di],al         ;
        inc     di              ;
copy_next:
        cmp     al,lf ; 0Ah	; done with line?
        jne	short copy_loop	; nope

copy_nextline:
        call    skip_opt_line   ;
        jmp     short copy_block ;
copy_done:
        retn

;----------------------------------------------------------------------------
;
;   get_linenum:  return line # (in BX) of current line (@ES:SI)
;
;   INPUT
;    ES:SI -> some line in the config.sys memory image
;
;   OUTPUT
;       BX == line # (relative to 1)
;
;   OTHER REGS USED
;       DX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

get_linenum:
        push    ax              ;
        sub     bx,bx           ; BX == line # (to be returned)
        push    cx              ;
        mov     dx,si           ; DX == the offset we're looking for
        push    si              ;
        mov     cx,[cs:count]   ;
        sub     si,si           ; prepare to scan entire file
get_linenum_loop:               ;
        call    skip_line       ;
        jc	short get_linenum_done
        inc     bx              ;
        cmp     si,dx           ; have we exceeded the desired offset yet?
        jb	short get_linenum_loop ; no
get_linenum_done:               ;
        pop     si              ;
        pop     cx              ;
        pop     ax              ;
        retn

;----------------------------------------------------------------------------
;
;   srch_block:  searches entire config.sys for block name @ES:DI
;
;   INPUT
;       ES -> config.sys image
;    ES:DI -> block name to find
;
;   OUTPUT
;       ZF flag set, if found
;    ES:DI -> just past the name in the block heading, if found
;       BX == # bytes remaining from that point, if found
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       This differs from "find_block" in that it searches the ENTIRE
;       config.sys image, not merely the remaining portion, and that it
;       takes a pointer to block name that is *elsewhere* in the image
;       (ie, ES) as opposed to some string constant in our own segment (DS).
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

srch_block:	          ; returns BX -> named block in CONFIG.SYS
        push    ax              ;
        push    cx              ;
        mov     cx,[cs:count]   ;
        push    si              ;
        sub     si,si           ;
        push    ds              ;
        push    es              ;
        pop     ds              ;
        call    find_block      ;
        mov     di,si           ;
        mov     bx,cx           ;
        pop     ds              ;
        pop     si              ;
        pop     cx              ;
        pop     ax              ;
find_exit: ; 16/04/2019
        retn			;

;----------------------------------------------------------------------------
;
;   find_block:  searches rest of config.sys for block name @DS:DI
;
;   INPUT
;    DS:DI -> block name to find
;    ES:SI -> remainder of config.sys image
;       CX == remaining size of config.sys image
;
;   OUTPUT
;       ZF flag set, if found (also, CF set if EOF)
;    ES:SI -> where the search stopped (at end of block name or EOF)
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       This differs from "srch_block" in that it searches only the
;       remaining portion of the config.sys image and leaves SI and CX
;       pointing to where the search left off, and that it takes a pointer
;       to search string in our own segment (DS:DI instead of ES:DI).
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

find_block:
        call    get_char        ; get line code
        jc	short find_exit	; end of file
        and     al,~CONFIG_OPTION_QUERY
        cmp     al,CONFIG_BEGIN ; beginning of a block?
        je	short check_line ; no
        cmp     al,CONFIG_INCLUDE
        jne	short next_line	;
        or	byte [cs:config_multi],1
        jmp     short next_line ;
check_line:
        or      byte [cs:config_multi],1
        call    comp_names      ; compare block names
        jbe	short find_exit	; end of file, or names matched
next_line:
        call    skip_opt_line   ; no, so skip to next line
        jmp	short find_block  ;
;find_exit:
;	retn

;----------------------------------------------------------------------------
;
;   comp_names:  compares keyword @DS:DI to position in config.sys @ES:SI
;
;   INPUT
;    DS:DI -> keyword to compare
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       ZF flag set, if match (also, CF set if EOF)
;    ES:SI -> where the comparison stopped (at end of block name or EOF)
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

comp_names:
        push    di              ;
comp_loop:                      ;
        call    get_char        ;
        jc	short comp_exit	;
        call    any_delim       ; is next character a delimiter?
        mov     ah,[di]         ; (get next character we're supposed to match)
        je	short comp_almost ; yes, it *could* be a match
        inc     di              ;
        and     ax,~2020h ; 0DFDFh 
				; BUGBUG -- assumes both names are alphanumeric -JTP
        cmp     al,ah           ; match?
        je	short comp_loop ; yes, keep looking at the characters
        clc                     ; prevent erroneous eof indication: clear carry
comp_exit:                      ;
        pop     di              ;
        retn			;
comp_almost:                    ;
        xchg    al,ah           ; we don't know for sure if it's a match
        call    any_delim       ; until we verify that the second string has
        xchg    al,ah           ; been exhausted also...
        jmp     comp_exit       ; if we are, this call to any_delim will tell...

;----------------------------------------------------------------------------

comp_names_safe:
        push    ax
        push    cx
        push	si
        push    ds
        push    cs
        pop     ds
        call    comp_names
        pop     ds
	pop	si
        pop     cx
        pop     ax
        retn

;----------------------------------------------------------------------------
;
;   print_item:  display menu item #BL
;
;   INPUT
;       BL == menu item # to display
;
;   OUTPUT
;       Menu item displayed, with appropriate highlighting if BL == bDefBlock
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       This function saves/restores the current cursor position, so you
;       needn't worry about it.
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

print_item:		; prints menu item #BL (1 to N)
        push    ax              ;
        push    bx              ;
        push    cx              ;
        push    dx              ;
        push    si              ;
        mov     ah,03h          ; get cursor position
        mov     bh,[bMenuPage]  ; always page zero
        int     10h             ; DH/DL = row/column
        push    dx              ; save it
        mov     ah,02h          ; set cursor position
        mov     dh,bl           ;
        add     dh,3            ;
        mov     dl,5            ;
        int     10h             ; set cursor position for correct row/col
        mov     al,bl           ;
        add     al,'0'          ; convert menu item # to ASCII digit
        mov     ah,[bMenuColor] ; normal attribute
        cmp     bl,[bDefBlock]  ; are we printing the current block?
        jne	short print_other ; no
        or      ah,70h          ; yes, set bgnd color to white
        mov     ch,ah           ;
        mov     cl,4            ;
        rol     ch,cl           ;
        cmp     ch,ah           ; are fgnd/bgnd the same?
        jne	short print_other ; no
        xor     ah,08h          ; yes, so modify the fgnd intensity
print_other:                    ;
        mov     bh,0            ;
        add     bx,bx           ;
        mov     di,[aoffBlockDesc+bx]
        mov     bl,ah           ; put the attribute in the correct register now
        mov     bh,[bMenuPage]  ; get correct video page #
        mov     ah,09h          ; write char/attr
        mov     cx,1            ;
        int     10h             ;
        inc     dl              ; increment column
        mov     ah,02h          ;
        int     10h             ;
        ;mov	ax,0900h+'.'    ;
        mov	ax,092Eh
	int     10h             ; display '.'
        inc     dl              ; increment column
        mov     ah,02h          ;
        int     10h             ;
        ;mov	ax,0900h+' '    ;
        mov	ax,0920h
	int     10h             ; display ' '
        inc     dl              ; increment column
        mov     ah,02h          ;
        int     10h             ;
        push    es              ;
print_loop:                     ;
        mov     al,[es:di]	; get a character of the description
        inc     di              ;
        cmp     al,TAB ; 9	; substitute spaces for tabs
        jne	short print_nontab ;
        mov     al,' '          ;
print_nontab:                   ;
        cmp     al,' '          ;
        jb	short print_done ; stop at the 1st character < space
        cmp     al,'$'          ;
        je	short print_done ; also stop on $
        mov     ah,09h          ; display function #
        int     10h             ;
        inc     dl              ; increment column
        cmp     dl,78           ; far enough?
        jae	short print_done ; yes
        mov     ah,02h          ;
        int     10h             ;
        jmp     print_loop      ;
print_done:                     ;
        pop     es              ;
        pop     dx              ;
        mov     ah,02h          ;
        int     10h             ; restore previous row/col
        pop     si              ;
        pop     dx              ;
        pop     cx              ;
        pop     bx              ;
        pop     ax              ;
        retn			;

;----------------------------------------------------------------------------
;
;   select_item:  wait for user to select menu item, with time-out
;
;   INPUT
;       None
;
;   OUTPUT
;       BX == menu item # (1-N), or -1 for clean boot
;       Selected menu item highlighted
;       Cursor positioned beneath menu, ready for tty-style output now
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

select_item:		; returns digit value in BX (trashes AX/CX/DX)
        mov     bl,[bDefBlock]  ; BL will be the default block #
        mov     al,bl           ;
        call    disp_num        ;
        call    show_status     ; display current interactive status
        cmp     byte [secTimeOut],-1
        je	short input_key	; no time-out, just go to input
        mov     ah,GET_TIME ; 2Ch
        int     21h             ;
        mov     bh,dh           ; BH = initial # of seconds
check_time:
        mov     al,[secTimeOut] ;
        sub     al,[secElapsed] ;
        jae	short show_time	;
        or      byte [bQueryOpt],2  ; disable all further prompting
        mov     byte [secElapsed],0
        jmp	select_done	; time's up!
show_time:
        push    bx              ;
        mov     bl,al           ; save # in BL
        mov     bh,[bMenuPage]  ;
        mov     ah,03h          ; get cursor position
        int     10h             ;
        push    dx              ;
	add	dl,8		; move cursor to the right
        mov     ah,02h          ; set cursor position
        int     10h             ;
        mov     dx,_$TimeOut
        call    print           ; print the "Time remaining: " prompt
        mov     al,bl           ; recover # from BL
        cbw                     ; this works because AL is always <= 90
        mov     cl,10           ;
        div     cl              ; AL = tens digit, AH = ones digit
        mov     cl,ah           ;
        add     al,'0'          ;
        mov     ah,0Eh          ;
        int     10h             ; write TTY tens digit
        mov     al,cl           ;
        add     al,'0'          ;
        mov     ah,0Eh          ;
        int     10h             ; write TTY ones digit
        pop     dx
        mov     ah,02h          ; set cursor position back to where it was
        int     10h             ;
        pop     bx              ;
input_key:
        mov     ah,RAW_CON_IO ; 6
        mov     dl,0FFh         ; input request
        int     21h             ;
        jnz	short got_key	;
        cmp     byte [secTimeOut],-1; is there a time-out?
        je	short input_key	; no, just go back to input
        mov     ah,GET_TIME     ;
        int     21h             ; DH = seconds
        mov     ah,dh           ;
        sub     dh,bh           ; should generally be zero or one
        mov     bh,ah           ;
        jnc	short got_time	;
        mov     dh,1            ; it wrapped back to zero, so assume one
got_time:
        or      dh,dh           ; any change?
        jz	short input_key	; no
        add     [secElapsed],dh ;
        jmp	short check_time ;
got_key:
        push    ax              ;
        mov     ax,-1           ; zap both secTimeOut and secElapsed
        xchg    [secTimeOut],ax
        cmp     al,-1           ; was time-out already disabled?
        je	short timeout_disabled ; yes
        push    bx              ; let's disable # seconds display
        mov     ax,0A20h        ; write multiple spaces
        mov     bx,[bMenuColor]
        mov     cx,80           ; 80 of them, to be safe
        int     10h             ; to completely obliterate # seconds display
        pop     bx   		;

timeout_disabled:
        pop     ax              ;
        or      al,al           ; extended key pressed?
        jnz	short normal_key ; no
        int     21h             ; get the next part of the key then
        jz	short input_key	; hmmm, what happened to the second part?

        cmp     al,48h          ; up arrow?
        jne	short not_up	; no
        cmp     bl,1            ; are we as up as up can get?
        jbe	short input_key	; yes, ignore it
        dec     byte [bDefBlock] ;
        call    print_item      ; re-print the current item
        dec     bl              ; and then print the new current item
        jmp     short print1
not_up:
        cmp     al,50h          ; down arrow?
        jne	short not_down	; no
        cmp     bl,[bMaxBlock]  ; are we as down as down can get?
        jae	short to_input_key ; yes, ignore it
        inc     byte [bDefBlock] ;
        call    print_item      ; re-print the current item
        inc     bx              ; and then print the new current item
print1: 
	mov     al,bl           ;
print2: 
	call    print_item      ;
        call    disp_num        ;
to_input_key:
        jmp     input_key       ;
not_down:
        test    byte [bDisableUI],1
        jnz	short to_input_key ; don't allow F8 or F5
        cmp     al,42h          ; F8 function key?
        jne	short not_f8	; no
        xor     byte [bQueryOpt],1
        call    show_status     ;
        jmp     input_key	;
not_f8:
        cmp     al,3Fh          ; F5 function key?
        jne	short to_input_key ; no
        mov     bx,-1           ; special return code (-1) indicating clean boot
        mov     al,' '          ; don't want to display anything really;
        jmp     short disp_input; just want to display the cr/lf sequence...

normal_key:
        cmp     al,0Dh          ; Enter?
        je	short select_done ; yes
        cmp     al,08h          ; backspace?
        jne	short not_backspace ; no
        mov     bx,-2 ; 0FFFEh	; yes, special return code
        retn			;
not_backspace:
        sub     al,'0'          ; is greater than '0'?
        jbe	short to_input_key ; no
        cmp     al,[bMaxBlock]  ; is less than or equal to the maximum digit?
        ja	short to_input_key ; no
        mov	[bDefBlock],al  ;
        call    print_item      ; redisplay the current selection
        mov     bl,al           ; set new selection
        jmp	short print2

select_done:
        mov     bh,0            ; return a full 16-bit value (for indexing)
        mov     al,bl           ;
        add     al,'0'          ; convert it into a digit, then display it

	; fall into disp_input

; 16/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
;   disp_input:  display a single character + cr/lf
;
;   INPUT
;       AL == character to display
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       This function is used not only for the menu input selection but
;       also for the interactive line prompting (the y/n/a thing).
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

disp_input:
        push    ax
        cmp     al,' '
        jae	short disp_ok
        mov     al,' '
disp_ok:
        mov     dl,al
        mov     ah,STD_CON_OUTPUT ; 2
        int     21h
        mov     dx,crlfm
        call    print
        pop     ax
        retn

;----------------------------------------------------------------------------

disp_num:
        push    bx
        add     al,'0'
        mov     ah,0Ah
        mov     bx,[bMenuColor]
        mov     cx,1
        int     10h
        pop     bx
        retn

;----------------------------------------------------------------------------
;
;   show_status:  display current interactive mode setting (on/off/none)
;
;   INPUT
;       None
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

show_status:
        push    bx              ; BL = video page #
        mov     bx,[bMenuColor]
        mov     ah,03h          ; get cursor position
        int     10h             ;
        push    dx              ; save it
        mov     ah,02h          ; set cursor position
        mov     dx,[bLastCol]   ; set correct row/col
        test    byte [bDisableUI],1
        jz	short show_onoff ; just show on/off
        mov     dl,0            ;
        int     10h             ;
        mov     ax,0A20h        ; write multiple spaces
        mov     cx,80           ; 80 of them, to be exact
        int     10h             ; to obliterate the status line
        jmp     short show_done ;
show_onoff: 
        int     10h
		; - VIDEO - WRITE CHARACTERS ONLY AT CURSOR POSITION
		; AL = character, BH = display page - alpha mode
		; BL = color of character (graphics mode, PCjr only)
		; CX = number of times to write character

        mov     al,[_$NO]	; assume OFF
        cmp     byte [bQueryOpt],1 ; is interactive mode on?
        jne	short show_noton ; no
        mov     al,[_$YES]	; yes
show_noton:                     ;
        mov     ah,0Eh          ; write TTY
        int     10h             ;
show_done:                      ;
        pop     dx              ;
        mov     ah,02h          ;
        int     10h             ; restore original cursor position
        pop     bx              ;
        retn			;

; 16/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
;   skip_token: advances ES:SI/CX past the current token
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOL/EOF hit
;       AL == 1st char of delimiter
;    ES:SI -> just past the delimiter
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

skip_token:
        call    get_char
        jc	short skip_token_done
        call    any_delim
        jne	short skip_token
skip_check_eol:
        cmp     al,cr ; 0Dh
        je	short skip_token_eol
        cmp     al,lf ; 0Ah
        je	short skip_token_eol
        clc
        ;jmp	short skip_token_done
	retn
skip_token_eol:
        stc
skip_token_done:
        retn

;----------------------------------------------------------------------------
;
;   skip_delim: advances ES:SI/CX past the current delimiter
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOF hit
;       AL == 1st char of token
;    ES:SI -> just past the token
;       CX == # bytes remaining from that point
;    ES:BX -> new token (since ES:SI is already pointing 1 byte past token)
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

skip_delim:       ; returns carry set if eol/eof
        call    get_char        ;
        lea     bx,[si-1]       ; also returns BX -> next token
        jc	short skip_token_done ;
        call    delim           ;
        je	short skip_delim ;
        jmp	short skip_check_eol  ; 13/05/2019

;----------------------------------------------------------------------------
;
;   skip_opt_line: same as skip_line provided AL != LF
;
;   INPUT
;       AL == last character read
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOF hit
;       AL == 1st char of new line
;    ES:SI -> just past 1st char of new line
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       In other words, the purpose here is to skip to the next line,
;       unless ES:SI is already sitting at the front of the next line (which
;       it would be if the last character fetched -- AL -- was a linefeed)
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

skip_opt_line:
        cmp     al,lf ; 0Ah
        je	short skip_line_done
     
	; fall into skip_line

;----------------------------------------------------------------------------
;
;   skip_line: skip to the next line
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOF hit
;    ES:SI -> just past 1st char of new line
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

skip_line:
        call    get_char
        jc	short skip_line_done
        cmp     al,lf ; 0Ah
        jne	short skip_line
skip_line_done:
        retn

;----------------------------------------------------------------------------
;
;   get_number: return binary equivalent of numeric string
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       AL == non-digit encountered
;       BX == binary #
;    ES:SI -> just past 1st non-digit
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

; 13/05/2019

get_number:
        sub     bx,bx           ; BX = result
num_loop:
        call    get_char        ;
        jc	short num_done	;
        cmp     al,'0'          ; convert to value
        jb	short num_done	; no more number
        cmp     al,'9'          ;
        ja	short num_done	;
        push    ax              ;
        mov     ax,10           ;
        push    dx              ;
        mul     bx              ;
        pop     dx              ;
        mov     bx,ax           ;
        pop     ax              ;
        sub     al,'0'          ;
        cbw                     ;
        add     bx,ax           ;
        jmp	short num_loop	;
num_done:
        retn

;----------------------------------------------------------------------------
;
;   get_char:  return next character, advance ES:SI, and decrement CX
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       AL == next character
;    ES:SI -> just past next character
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

get_char:
        sub     cx,1            ; use SUB to set carry,zero
        jb	short get_fail	; out of data
        ;lods	byte ptrf es:[si] ;
	es	
	lodsb
        mov     ah,al           ;
        retn			;
get_fail:                       ; restore CX to zero
        mov     cx,0            ; leave carry set, zero not set
nearby_ret:
        retn

;----------------------------------------------------------------------------
;
;   query_user: ask user whether to execute current config.sys command
;
;   INPUT
;       AL == current command code
;    ES:SI -> current command line in config.sys
;    config_cmd == current command code, but with QUERY bit intact
;                  (00h used to generate "Process AUTOEXEC.BAT" prompt)
;
;   OUTPUT
;       CF set if command should be ignored (it is also REM'ed out)
;
;   OTHER REGS USED
;       BX, CX, DX, DI
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

query_user:
        test    byte  [bQueryOpt],4	; answer no to everything?
        jz	short qu_1		;
        jmp	skip_all		;
qu_1:
	test    byte [bQueryOpt],2	; answer yes to everything?
        jnz	short nearby_ret	; yes (and return carry clear!)
        push    ax                      ;
        mov     al,[config_cmd]         ;
        test    byte [bQueryOpt],1	; query every command?
        jnz	short query_all		; yes
        test    al,CONFIG_OPTION_QUERY  ;
        jnz	short query_all		;
        jmp	do_cmd			;
query_all:

;   Search for the command code (AL) in "comtab", and then print
;   out the corresponding keyword, followed by the rest of the actual
;   line pointed to by ES:SI

        push    si                      ; save pointer to rest of CONFIG.SYS line
        mov     dx,_$AutoPrmpt    	;
        and     al,~CONFIG_OPTION_QUERY ; 7Fh
        jz	short generic_prompt	; config_cmd must have been 0

        mov     dh,al                   ; save config_cmd in DH
        sub     bx,bx                   ;
        mov     di,comtab		;
find_match:                             ;
        mov     bl,[di]                 ; get size of current keyword
        or      bl,bl                   ;
        jz	short line_print	; end of table
        inc     di                      ;
        cmp     al,[di+bx]              ; match?
        je	short cmd_match		; yes
        lea     di,[di+bx+1]            ; otherwise, skip this command code
	; 13/05/2019	
        jmp	short find_match	; loop
cmd_match:                              ;
        mov     cl,[di-1]               ;
        mov     ch,0                    ;
        mov     ah,STD_CON_OUTPUT ; 2
cmd_print:                              ;
        mov     al,[di]                 ;
        inc     di                      ;
        mov     dl,al                   ;
        int     21h                     ;
        loop    cmd_print               ;
        mov     dl,'='                  ;
        cmp     dh,CONFIG_SET  ; 'V'    ; for SET commands, don't display a '='
        jne	short cmd_notset	;
        mov     dl,' '                  ;
cmd_notset:
        int     21h                     ; '=' looks funny on SET commands
line_print:                             ;
	;lods	byte ptr es:[si]        ;
        es
	lodsb
	or      al,al                   ;
        jnz	short non_null		;
        mov     al,' '                  ;
non_null:                               ;
        cmp     al,' '                  ; control code?
        jb	short prompt_user	; yes, assume end of line
        jne	short non_space		;
        cmp     byte [es:si],' '	;
        jb	short prompt_user	;
non_space:                              ;
        mov     dl,al                   ;
        mov     ah,STD_CON_OUTPUT ; 2	;
        int     21h                     ;
        jmp	short line_print	;
prompt_user:                            ;
        mov     dx,_$InterPrmpt		;

generic_prompt:
        call    print                   ;
input_loop:                             ;
        mov     ah,0                    ; read a key
        int     16h                     ;
        or      al,al                   ; is it a function key?
        jnz	short not_func		; no
        cmp     ah,3Fh                  ; F5 function key?
        jne	short input_loop	; no
        mov     al,[_$NO]                  ;
        or      byte [bQueryOpt],4	; no more queries
        jmp     short legal_char        ;
not_func:
        and     al,~20h ; 0DFh		; converting to upper case
        cmp     al,[_$NO]		; verify character is legal
        je	short legal_char	;
        cmp     al,[_$YES]		;
        je	short legal_char	;
        cmp     byte [config_cmd],0	;
        je	short input_loop	; don't allow Esc on this query
        cmp     al,1Bh                  ; Esc?
        jne	short input_loop	;
        or      byte [bQueryOpt],2	; no more interactive boot prompts
        mov     al,[_$YES]
legal_char:                             ;
        call    disp_input              ;
        pop     si                      ; restore pointer to rest of CONFIG.SYS line

        cmp     al,[_$NO]		; process line?
        je	short skip_cmd		; no
do_cmd:
        pop     ax                      ;
        clc                             ; just do the command
        retn

skip_cmd:
        pop     ax                      ;
skip_all:
        mov     ah,CONFIG_REM ; '0'	; fake out the rest of sysinit's processing
        stc
        retn

;----------------------------------------------------------------------------
;
;   print_error: displays multi-config error conditions
;
;   INPUT
;    Carry set to pause, clear to not
;    ES:SI -> current command line in config.sys
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------

print_error:
        push    ax
        push    bx
        push    cx
        push    dx
        push    ds
        push    cs
        pop     ds
        pushf
        call    get_linenum
        mov     [linecount],bx
        call    error_line
        popf
        jnc	short pe_ret
        mov     dx,_$PauseMsg
        call    print
        mov     ax,0C07h		; flush input buffer, then wait for key
        int     21h			; wait for a key
        or      al,al			; extended key?
        jnz	short pe_1		; no
        mov     ah,07h			; yes
        int     21h			; eat it too
pe_1:     
	mov     dx,crlfm
        call    print
pe_ret: 
	pop     ds
        pop     dx
        pop     cx
        pop     bx
        pop     ax
	retn

;----------------------------------------------------------------------------

;   This function is very simple:  it merely prepends a "/D" to the
;   command-line for the shell;  this (undocumented) switch disables
;   AUTOEXEC.BAT processing and the date/time prompt that is usually
;   displayed when there's no AUTOEXEC.BAT.

disable_autoexec:
	; MSDOS 6.21 IO.SYS -  SYSINIT:4BE2h
	; 17/04/2019 - Retro DOS v4.0

	test	byte [bQueryOpt],4
	jz	short disable_exit
	test	byte [dae_flag],1
	jnz	short disable_exit
	or	byte [dae_flag],1
        ;or	byte [bQueryOpt],2 ; MSDOS 6.0 
	or      word [bQueryOpt],102h	; [bDefBlock] = 1
	mov     dx,'D ' ; 2044h
dae_1:
        ;mov	al,[def_swchr]
	mov     al,[command_line-1]     ; get default switchchar
        or      al,al                   ; anything there?
        jz	short disable_exit	; no, disable_autoexec already called
        mov     bl,[command_line]       ;
        mov     bh,0                    ; BX == command-line length
        mov     cx,bx                   ;
        add     bl,3                    ;
        cmp     bl,126                  ;
        ja	short disable_exit	;
        mov     [command_line],bl       ; update length
        add     bx,command_line+1	; make sure we move the NULL too
        inc     cx                      ; (just for consistency sake)
disable_loop:                           ;
        mov     ah,[bx-3]               ;
        mov     [bx],ah                 ;
        dec     bx                      ;
        loop    disable_loop            ;
        mov     [bx-2],al               ;
	;mov	word [bx-1],'D ' ; 2044h ; /D is stuffed into place now
	mov	[bx-1],dx  ; MSDOS 6.21 IO.SYS - SYSINIT:4C29h		
        ;mov	byte [command_line-1],0 ;
disable_exit:                           ;
        retn

CheckQueryOpt:	; MSDOS 6.21 IO.YSYS - SYSINIT:4C2Dh
	cmp     byte [bQueryOpt],1
	jnz     short disable_exit
	test	byte [dae_flag],2
	jnz     short disable_exit
	or      byte [dae_flag],2
	mov     dx,' Y'
	jmp     short dae_1

;endif  ;MULTI_CONFIG

%endif	; 02/11/2022


; 19/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
;
; procedure : delim
;
;----------------------------------------------------------------------------

;	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;
;;ifdef	MULTI_CONFIG
;
;any_delim:
;       cmp     al,cr
;       je	short delim_ret
;       cmp     al,lf
;       je	short delim_ret
;       cmp     al,'['
;       je	short delim_ret
;       cmp     al,']'
;       je	short delim_ret
;
;;endif  ;MULTI_CONFIG

	; 02/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:3450h)	
delim:
	cmp	al,'/'		; ibm will assume "/" as an delimeter.
	je	short delim_ret

	cmp	al,0		; special case for sysinit!!!
	je	short delim_ret

org_delim:			; used by organize routine except for getting
	cmp	al,' '          ; the filename.
	je	short delim_ret
        cmp     al,tab ; 9
	je	short delim_ret
	cmp	al,'='
	je	short delim_ret
	cmp	al,','
	je	short delim_ret
	cmp	al,';'

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;;   Make sure there's no chance of a false EOF indication
;	clc
;;endif

	; 02/11/2022
delim_ret:
	retn

;----------------------------------------------------------------------------
;
; procedure : newline
;
;  newline returns with first character of next line
;
;----------------------------------------------------------------------------

newline:
	call	getchr			;skip non-control characters
	jc	short nl_ret
	cmp	al,lf			;look for line feed
	jne	short newline
	call	getchr
nl_ret:
	retn

;----------------------------------------------------------------------------
; 
; procedure : mapcase
;
;----------------------------------------------------------------------------

	; 02/11/2022 - Retro DOS 4.0 (Modified MSDOS 5.0 IO.SYS)
mapcase:
	push	cx
	push	si
	push	ds

	push	es
	pop	ds

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;
;	mov	bl,al                   ; same cmd code this line
;;else
;;	xor	si,si
;;endif
	; 02/11/2022
	xor	si, si

convloop:
	lodsb
	cmp	al,'a'
	jb	short noconv
	cmp	al,'z'
	ja	short noconv
	sub	al,20h
	mov	[si-1],al
noconv:

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;;
;;   When MULTI_CONFIG enabled, "mapcase" is used to map everything to
;;   upper-case a line at a time, after we've been able to figure out whether
;;   the line is a SET command or not (since we don't want to upper-case
;;   anything after the "=" in a SET)
;;
;       cmp     bl,CONFIG_SET  ; 'V'	; preserve case for part of the line?
;       jne	short check_eol		; no, just check for end-of-line
;       cmp     al,'='                  ; separator between SET var and value?
;       je	short convdone		; yes
;check_eol:
;       cmp     al,cr
;       je	short convdone
;       cmp     al,lf
;       je	short convdone
;;endif
	
	; 02/11/2022
	loop	convloop
convdone:
	pop	ds
	pop	si
	pop	cx
	retn

;----------------------------------------------------------------------------
;
; procedure : round
;
; round the values in memlo and memhi to paragraph boundary.
; perform bounds check.
;
;----------------------------------------------------------------------------

round:
	push	ax
	mov	ax,[cs:memlo]

	call	ParaRound		; para round up

	add	[cs:memhi],ax
	mov	word [cs:memlo],0
	mov	ax,[cs:memhi]		; ax = new memhi
	cmp	ax,[cs:ALLOCLIM]	; if new memhi >= alloclim, error
	jae	short mem_err
	test	byte [cs:setdevmarkflag],for_devmark ; 2
	jz	short skip_set_devmarksize
	push	es
	push	si
	mov	si,[cs:devmark_addr]
	mov	es,si
	sub	ax,si
	dec	ax
	;mov	[es:3],ax
	mov	[es:devmark.size],ax	; paragraph
	and	byte [cs:setdevmarkflag],not_for_devmark ; 0FDh
	pop	si
	pop	es
skip_set_devmarksize:
	pop	ax
	; 02/11/2022
	clc	; ? (not needed here)	; clear carry
	retn

;----------------------------------------------------------------------------

mem_err:
	mov	dx,badmem
	push	cs
	pop	ds
	call	print
	jmp	stall

;----------------------------------------------------------------------------
;
; procedure : calldev
;
;----------------------------------------------------------------------------

	; 02/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:34E0h)
calldev:
	mov	ds,[cs:DevEntry+2]
	add	bx,[cs:DevEntry]	; do a little relocation
	mov	ax,[bx]

	push	word [cs:DevEntry]
	mov	word [cs:DevEntry],ax
	mov	bx,packet
	call	far [cs:DevEntry]
	pop	word [cs:DevEntry]
	retn

;----------------------------------------------------------------------------
;
; procedure : todigit
;
;----------------------------------------------------------------------------

todigit:
	sub	al,'0'
	jb	short notdig
	;jb	short notdig2 ; 02/11/2022
	cmp	al,9
	ja	short notdig
	clc
	retn
notdig:
	stc
;notdig2:
	retn

;----------------------------------------------------------------------------
;
; procedure : getnum
;
; getnum parses a decimal number.
; returns it in ax, sets zero flag if ax = 0 (may be considered an
; error), if number is bad carry is set, zero is set, ax=0.
;
;----------------------------------------------------------------------------

getnum:
	push	bx
	xor	bx,bx			; running count is zero
b2:
	call	todigit 		; do we have a digit ?
	jc	short badnum		; no, bomb

	xchg	ax,bx			; put total in ax
	push	bx			; save digit (0 to 9)
	mov	bx,10			; base of arithmetic
	mul	bx			; shift by one decimal digit
	pop	bx			; get back digit (0 to 9)
	add	al,bl			; get total
	adc	ah,0			; make that 16 bits
	jc	short badnum		; too big a number

	xchg	ax,bx			; stash total

	call	getchr			;get next digit
	jc	short b1		; no more characters
	cmp	al,' ' 			; space?
	je	short b15		; then end of digits
	cmp	al,',' 			; ',' is a seperator!!!
	je	short b15		; then end of digits.
	cmp	al, tab ; 9		; tab
	je	short b15
	cmp	al,[cs:sepchr]		; allow 0 or special separators
	je	short b15
	cmp	al,'/'			; see if another switch follows
	;nop				; cas - remnant of old bad code
	;nop
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	nop
	nop
	je	short b15
	cmp	al,lf			; line-feed?
	je	short b15
	cmp	al,cr			; carriage return?
	je	short b15
	or	al,al			; end of line separator?
	jnz	short b2		; no, try as a valid char...
b15:
	inc	word [cs:count]		; one more character to s...
	dec	word [cs:chrptr]	; back up over separator
b1:
	mov	ax,bx			; get proper count
	or	ax,ax			; clears carry, sets zero accordingly
	pop	bx
	retn
badnum:
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	mov	byte [cs:sepchr],0
	xor	ax,ax			; set zero flag, and ax = 0
	;mov	[cs:sepchr],al ; 0
	pop	bx
	stc				; and carry set
	retn

;****************************************************************************

setdoscountryinfo:

;----------------------------------------------------------------------------
;input: es:di -> pointer to dos_country_cdpg_info
;	ds:0  -> buffer.
;	si = 0
;	ax = country id
;	dx = code page id. (if 0, then use ccsyscodepage as a default.)
;	bx = file handle
;	this routine can handle maximum 438 country_data entries.
;
;output: dos_country_cdpg_info set.
;	 carry set if any file read failure or wrong information in the file.
;	 carry set and cx = -1 if cannot find the matching country_id, 
;	 codepage_id in the file.
;----------------------------------------------------------------------------

	push	di
	push	ax
	push	dx

	xor	cx,cx
	xor	dx,dx
	mov	ax,512			;read 512 bytes
	call	readincontrolbuffer	;read the file header
	jc	short setdosdata_fail

	push	es
	push	si

	push	cs
	pop	es

	mov	di,country_file_signature ; db 0FFh,'COUNTRY'
	mov	cx,8			;length of the signature
	repz	cmpsb

	pop	si
	pop	es
	jnz	short setdosdata_fail 	;signature mismatch

	add	si,18			;si -> county info type
	cmp	byte [si],1		;only accept type 1 (currently only 1 header type)
	jne	short setdosdata_fail 	;cannot proceed. error return

	inc	si			;si -> file offset
	mov	dx,[si]			;get the info file offset.
	mov	cx,[si+2]
	mov	ax,6144			;read 6144 bytes.
	call	readincontrolbuffer	;read info
	jc	short setdosdata_fail

	mov	cx,[si]			;get the # of country, codepage combination entries
	cmp	cx, 438			;cannot handle more than 438 entries.
	ja	short setdosdata_fail

	inc	si
	inc	si			;si -> entry information packet
	pop	dx			;restore code page id
	pop	ax			;restore country id
	pop	di

setdoscntry_find:			;search for desired country_id,codepage_id.
	cmp	ax,[si+2]		;compare country_id
	jne	short setdoscntry_next

	cmp	dx,0			;no user specified code page ?
	je	short setdoscntry_any_codepage ;then no need to match code page id.
	cmp	dx,[si+4]		;compare code page id
	je	short setdoscntry_got_it

setdoscntry_next:
	add	si,[si]			;next entry
	inc	si
	inc	si			;take a word for size of entry itself
	loop	setdoscntry_find

	mov	cx,-1			;signals that bad country id entered.
setdoscntry_fail:
	stc
	retn

setdosdata_fail:
	pop	si
	pop	cx
	pop	di
	jmp	short setdoscntry_fail

setdoscntry_any_codepage:		;use the code_page_id of the country_id found.
	mov	dx,[si+4]

setdoscntry_got_it:			;found the matching entry
	mov	[cs:cntrycodepage_id],dx ;save code page id for this country.
	mov	dx,[si+10]		;get the file offset of country data
	mov	cx,[si+12]
	mov	ax,512 			;read 512 bytes
	call	readincontrolbuffer
	jc	short setdoscntry_fail

	mov	cx,[si]			;get the number of entries to handle.
	inc	si
	inc	si			;si -> first entry

setdoscntry_data:
	push	di			;es:di -> dos_country_cdpg_info
	push	cx			;save # of entry left
	push	si			;si -> current entry in control buffer

	mov	al,[si+2]		;get data entry id
	call	getcountrydestination	;get the address of destination in es:di
	jc	short setdoscntry_data_next ;no matching data entry id in dos

	mov	dx,[si+4]		;get offset of data
	mov	cx,[si+6]
	mov	ax,4200h
	stc
	int	21h			;move pointer
	jc	short setdosdata_fail

	mov	dx,512			;start of data buffer
	mov	cx,20			;read 20 bytes only. we only need to
	mov	ah,3Fh			;look at the length of the data in the file.
	stc
	int	21h			;read the country.sys data
	jc	short setdosdata_fail 	;read failure

	cmp	ax,cx
	jne	short setdosdata_fail ; 13/05/2019

	mov	dx,[si+4]		;get offset of data again.
	mov	cx,[si+6]
	mov	ax,4200h
	stc
	int	21h			;move pointer back again
	jc	short setdosdata_fail

	push	si
	mov	si,(512+8)		;get length of the data from the file
	mov	cx,[si]
	pop	si
	mov	dx,512			;start of data buffer
	add	cx,10			;signature + a word for the length itself
	mov	ah,3Fh			;read the data from the file.
	stc
	int	21h
	jc	short setdosdata_fail

	cmp	ax,cx
	jne	short setdosdata_fail

	mov	al,[si+2]		;save data id for future use.
	mov	si,(512+8)		;si-> data buffer + id tag field
	mov	cx,[si]			;get the length of the file
	inc	cx			;take care of a word for lenght of tab
	inc	cx			;itself.
	cmp	cx,(2048-512-8)	; 1528	;fit into the buffer?
	ja	short setdosdata_fail

	;if	bugfix
	call	setdbcs_before_copy
	;endif

	cmp	al,SetCountryInfo ; 1	;is the data for setcountryinfo table?
	jne	short setdoscntry_mov 	;no, don't worry

	push	word [es:di+country_cdpg_info.ccMono_Ptr-country_cdpg_info.ccCountryInfoLen]  
	;push	word [es:di+24]		;cannot destroy ccmono_ptr address. save them.
	push	word [es:di+country_cdpg_info.ccMono_Ptr-country_cdpg_info.ccCountryInfoLen+2] 
	;push	word [es:di+26]		;at this time di -> cccountryinfolen
	
	push	di			;save di

	push	ax
	mov	ax,[cs:cntrycodepage_id] ;do not use the code page info in country_info
	mov	[si+4],ax		;use the saved one for this !!!!
	pop	ax

setdoscntry_mov:
	rep	movsb			;copy the table into dos
	cmp	al,SetCountryInfo	;was the ccmono_ptr saved?
	jne	short setdoscntry_data_next

	pop	di			;restore di
	pop	word [es:di+country_cdpg_info.ccMono_Ptr-country_cdpg_info.ccCountryInfoLen+2]
	;pop	word [es:di+26]		;restore
	pop	word [es:di+country_cdpg_info.ccMono_Ptr-country_cdpg_info.ccCountryInfoLen] 
	;pop	word [es:di+24]

setdoscntry_data_next:
	pop	si			;restore control buffer pointer
	pop	cx			;restore # of entries left
	pop	di			;restore pointer to dso_country_cdpg
	add	si,[si]			;try to get the next entry
	inc	si
	inc	si			;take a word of entry length itself
	dec	cx
	cmp	cx,0
	je	short setdoscntry_ok
	jmp	setdoscntry_data

setdoscntry_ok:
	retn

;----------------------------------------------------------------------------

	;if	bugfix

setdbcs_before_copy:
	cmp	al,SetDBCS ; 7		; dbcs vector set?
	jne	short sdbcsbc		; jump if not
	cmp	word [es:di],0		; zero byte data block?
	je	short sdbcsbc		; jump if so

	push	di
	push	ax
	push	cx
	mov	cx,[es:di]		; load block length
	add	di,2			; points actual data
	xor	al,al			; fill bytes
	rep	stosb			; clear data block
	pop	cx
	pop	ax
	pop	di
sdbcsbc:
	retn

	;endif

;----------------------------------------------------------------------------

getcountrydestination:

;get the destination address in the dos country info table.
;input: al - data id
;	es:di -> dos_country_cdpg_info
;on return:
;	es:di -> destination address of the matching data id
;	carry set if no matching data id found in dos.

	push	cx
	;add	di,74
	add	di,country_cdpg_info.ccNumber_of_entries	
					;skip the reserved area, syscodepage etc.
	mov	cx,[es:di]		;get the number of entries
	inc	di
	inc	di			;si -> the first start entry id

getcntrydest:
	cmp	byte [es:di],al
	je	short getcntrydest_ok
	cmp	byte [es:di],SetCountryInfo ;was it setcountryinfo entry?
	je	short getcntrydest_1

	add	di,5			;next data id
	jmp	short getcntrydest_loop

getcntrydest_1:
	;add	di,41
	add	di,NEW_COUNTRY_SIZE+3	;next data id
getcntrydest_loop:
	loop	getcntrydest
	stc
	jmp	short getcntrydest_exit

getcntrydest_ok:
	cmp	al,SetCountryInfo ; 1	;select country info?
	jne	short getcntrydest_ok1

	inc	di			;now di -> cccountryinfolen
	jmp	short getcntrydest_exit

getcntrydest_ok1:
	les	di,[es:di+1]		;get the destination in es:di

getcntrydest_exit:
	pop	cx
	retn

;----------------------------------------------------------------------------

readincontrolbuffer:

;move file pointer to cx:dx
;read ax bytes into the control buffer. (should be less than 2 kb)
;si will be set to 0 hence ds:si points to the control buffer.
;entry:  cx,dx offset from the start of the file where the read/write pointer
;	 be moved.
;	 ax - # of bytes to read
;	 bx - file handle
;	 ds - buffer seg.
;return: the control data information is read into ds:0 - ds:0200.
;	 cx,dx value destroyed.
;	 carry set if error in reading file.

	push	ax			;# of bytes to read
	mov	ax,4200h
	stc
	int	21h			;move pointer
	pop	cx			;# of bytes to read
	jc	short ricb_exit

	xor	dx,dx			;ds:dx -> control buffer
	xor	si,si
	mov	ah,3Fh			;read into the buffer
	stc
	int	21h			;should be less than 1024 bytes.
ricb_exit:
	retn

;----------------------------------------------------------------------------

set_country_path:

;in:  ds - sysinitseg, es - confbot, si -> start of the asciiz path string
;     dosinfo_ext, cntry_drv, cntry_root, cntry_path
;     assumes current directory is the root directory.
;out: ds:di -> full path (cntry_drv).
;     set the cntry_drv string from the country=,,path command.
;     ds, es, si value saved.

	push	si

	push	ds			;switch ds, es
	push	es
	pop	ds
	pop	es			;now ds -> confbot, es -> sysinitseg

	call	chk_drive_letter	;current ds:[si] is a drive letter?
	jc	short scp_default_drv 	;no, use current default drive.

	mov	al,[si]
	inc	si
	inc	si			;si -> next char after ":"
	jmp	short scp_setdrv

scp_default_drv:
	mov	ah,19h
	int	21h
	add	al,"A"			;convert it to a character.

scp_setdrv:
	mov	[cs:cntry_drv],al	;set the drive letter.
	mov	di,cntry_path
	mov	al,[si]
	cmp	al, "\"
	je	short scp_root_dir

	cmp	al,"/"			;let's accept "/" as an directory delim
	je	short scp_root_dir

	jmp	short scp_path

scp_root_dir:
	dec	di			;di -> cntry_root
scp_path:
	call	move_asciiz		;copy it

	mov	di,cntry_drv
scpath_exit:

	push	ds			;switch ds, es
	push	es
	pop	ds
	pop	es			;ds, es value restored

	pop	si
	retn

;----------------------------------------------------------------------------

chk_drive_letter:

;check if ds:[si] is a drive letter followed by ":".
;assume that every alpha character is already converted to upper case.
;carry set if not.

	push	ax
	cmp	byte [si],"A"
	;jb	short cdletter_no
	jb	short cdletter_exit
	cmp	byte [si],"Z"
	ja	short cdletter_no
	cmp	byte [si+1],":"
	jne	short cdletter_no

	jmp	short cdletter_exit

cdletter_no:
	stc
cdletter_exit:
	pop	ax
	retn

;----------------------------------------------------------------------------

move_asciiz:

;in: ds:si -> source es:di -> target
;out: copy the string until 0.
;assumes there exists a 0.

masciiz_loop:
	movsb
	cmp	byte [si-1],0	;was it 0?
	jne	short masciiz_loop
	retn

;----------------------------------------------------------------------------

;	ds:dx points to string to output (asciz)
;
;	prints <badld_pre> <string> <badld_post>

badfil:
	push	cs
	pop	es

	mov	si,dx
badload:
	mov	dx,badld_pre	; want to print config error
	mov	bx,crlfm

prnerr:
	push	cs
	pop	ds
	call	print
prn1:
	mov	dl,[es:si]
	or	dl,dl
	jz	short prn2
	mov	ah,STD_CON_OUTPUT ; 2 
	int	21h
	inc	si
	jmp	short prn1
prn2:
	mov	dx,bx
	call	print

	cmp	byte [cs:donotshownum],1 ; suppress line number when handling command.com
	je	short prnexit
	call	error_line
prnexit:
	retn

;----------------------------------------------------------------------------

print:
	mov	ah,STD_CON_STRING_OUTPUT ; 9
	int	21h
	retn

;----------------------------------------------------------------------------

;  open device pointed to by dx, al has access code
;   if unable to open do a device open null device instead

	; 02/11/2022 - Retrodos v4.0 (Modified MSDOS 5.0 IO.SYS)
	; (SYSINIT:3764h)
open_dev:
	call	open_file
	jnc	short open_dev3

open_dev1:
	mov	dx,nuldev
	call	open_file
of_retn:
	retn

open_dev3:
	mov	bx,ax			; handle from open to bx
	;;xor	ax,ax			; get device info
	;;mov	ah,IOCTL ; 44h
	;mov	ax,(IOCTL<<8) ; 13/05/2019
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	xor	ax,ax
	mov	ah,44h	; IOCTL

	int	21h

	test	dl,10000000b ; 80h
	jnz	short of_retn

	mov	ah,CLOSE ; 3Eh
	int	21h
	jmp	short open_dev1

;----------------------------------------------------------------------------

open_file:
	mov	ah,OPEN	; 3Dh
	stc
	int	21h
	retn

;----------------------------------------------------------------------------

; test int24. return back to dos with the fake user response of "fail"

int24:
	mov	al,3			; fail the system call
	iret				; return back to dos.

; 19/04/2019 - Retro DOS v4.0

;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

;include copyrigh.inc			; copyright statement

; MSDOS 6.21 IO.SYS - SYSINIT:4FA3h

;MsDosVersion6Copyr:
;	db	'MS DOS Version 6 (C)Copyright 1981-1993 Microsoft Corp '
;	db	'Licensed Material - Property of Microsoft All rights reserved '

; 22/10/2022
; MSDOS 5.0 IO.SYS - SYSINIT:378Ch

	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
MsDosVersion5Copyr:
	db	'MS DOS Version 5.00 (C)Copyright 1981-1991 Microsoft Corp '
	db	'Licensed Material - Property of Microsoft All rights reserved '

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
; 22/10/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; 20/04/2019 - Retro DOS v4.0
;BOOTMES:
;	db      13
;	db      10
;	db      "MS-DOS version "
;	db      MAJOR_VERSION + "0"
;	db      "."
;	db      (MINOR_VERSION / 10) + "0"
;	db      (MINOR_VERSION % 10) + "0"
;	db      13,10
;	;db	"Copyright 1981-1993 Microsoft Corp.",13,10,"$"
;	; 22/10/2022
;	db	"Copyright 1981-1991 Microsoft Corp.",13,10,"$"
;	;
;	db	0

nuldev:	db	"NUL",0
condev:	db	"CON",0
auxdev:	db	"AUX",0
prndev:	db	"PRN",0

;IFDEF	CONFIGPROC
config:	db	"\CONFIG.SYS",0

cntry_drv:  db	"A:"
cntry_root: db	"\"
cntry_path: db	"COUNTRY.SYS",0
	    ;db	52 dup (0)
	    times 52 db 0	

country_file_signature:
	db	0FFh,'COUNTRY'

cntrycodepage_id: 
	dw	0 	

;ENDIF ; CONFIGPROC

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;newcmd:  db	0			; non-zero if non-std shell specified
;tmplate: db	64                      ; must precede commnd
;;endif
	; 02/11/2022
	;db	12                      ; size of commnd line (excl. null)

commnd:	db	"\COMMAND.COM",0
	;db	51 dup (0)
	times	51 db 0
;endif

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;;ifdef	MULTI_CONFIG
;commnd2: db 	"\COMMAND.COM",0	; alternate commands to exec,
;	 db	2,"/P",0 		; followed by their respective alternate
;commnd3: db	"\MSDOS\COMMAND.COM",0	; command lines
;	 db	11,"A:\MSDOS /P",0 	;(the drive letter are dynamically replaced)
;commnd4: db	"\DOS\COMMAND.COM",0 	;
;	 db	9,"A:\DOS /P",0		;
;def_swchr:	
;	 db	0			; default switchchar (referenced as command_line-1)
;;endif

	; 30/10/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;command_line:
;	db	2,"/P"			; default command.com args
;	;db	125 dup (0)
;	times	125 db 0

pathstring:
	;db	64 dup (0)
	times	64 db 0

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
%if 0

dae_flag:
	db	0 ; MSDOS 6.21 IO.SYS - SYSINIT:51D2h 	

;ifdef	MULTI_CONFIG

MAX_MULTI_CONFIG equ 10

;   Beware of byte pairs accessed as words (see all "KEEP AFTER" notes below)

bMenuColor:	db      07h ;1Fh        ; default fgnd/bgnd color
bMenuPage:	db      0               ; menu video page (KEEP AFTER bMenuColor)
		db      5               ; video page function # (KEEP AFTER bMenuPage)
bLastCol:	db      0               ; ending column on status line
bLastRow:	db      24              ; row # of status line (KEEP AFTER bLastCol)
bDisableUI:	db      0               ; 1=disable clean/interactive
                                        ; 2=disable default 2-second delay
bCRTPage:	db      0               ; value saved from BIOS data area
wCRTStart:	dw      0               ; value saved from BIOS data area
bQueryOpt:	db      0               ; 0=off, 1=prompt all, 2=prompt none, 4=skip all
bDefBlock:	db      1               ; default block #
bMaxBlock:	db      0               ; maxmimum block #
offDefBlock:	dw      0               ; offset of name of default block (if any)
secTimeOut:	db      -1              ; # of seconds for timeout (-1 == indefinite)
secElapsed:	db      0               ; # of seconds elapsed so far (KEEP AFTER secTimeOut)
abBlockType:	times MAX_MULTI_CONFIG+1 db 0 ; array of block types
aoffBlockName:	times MAX_MULTI_CONFIG+1 dw 0 ; array of offsets of block names
aoffBlockDesc:	times MAX_MULTI_CONFIG+1 dw 0 ; array of offsets of block descriptions

szBoot:		db      "CONFIG=",0
szMenu:		db      "MENU",0
szCommon:	db      "COMMON",0

;endif	;MULTI_CONFIG

comtab:	 ; label byte

;            cmd len    command         cmd code
;            -------    -------         --------

;ifdef MULTI_CONFIG
        db      1,      "[",            CONFIG_BEGIN
;endif
        db      5,      "BREAK",        CONFIG_BREAK
        db      7,      "BUFFERS",      CONFIG_BUFFERS
        db      7,      "COMMENT",      CONFIG_COMMENT
        db      7,      "COUNTRY",      CONFIG_COUNTRY
        db      6,      "DEVICE",       CONFIG_DEVICE
        db      10,     "DEVICEHIGH",   CONFIG_DEVICEHIGH
        db      3,      "DOS",          CONFIG_DOS
        db      8,      "DRIVPARM",     CONFIG_DRIVPARM
        db      4,      "FCBS",         CONFIG_FCBS
        db      5,      "FILES",        CONFIG_FILES
;ifdef MULTI_CONFIG
        db      7,      "INCLUDE",      CONFIG_INCLUDE
;endif
        db      7,      "INSTALL",      CONFIG_INSTALL
        db      11,     "INSTALLHIGH",  CONFIG_INSTALLHIGH
        db      9,      "LASTDRIVE",    CONFIG_LASTDRIVE
;ifdef MULTI_CONFIG
        db      7,      "SUBMENU",      CONFIG_SUBMENU
        db      9,      "MENUCOLOR",    CONFIG_MENUCOLOR
        db      11,     "MENUDEFAULT",  CONFIG_MENUDEFAULT
        db      8,      "MENUITEM",     CONFIG_MENUITEM
;endif
        db      10,     "MULTITRACK",   CONFIG_MULTITRACK
;ifdef MULTI_CONFIG
        db      7,      "NUMLOCK",      CONFIG_NUMLOCK
;endif
        db      3,      "REM",          CONFIG_REM
;ifdef MULTI_CONFIG
        db      3,      "SET",          CONFIG_SET
;endif
        db      5,      "SHELL",        CONFIG_SHELL
;if    STACKSW
        db      6,      "STACKS",       CONFIG_STACKS
;endif
        db      8,      "SWITCHES",     CONFIG_SWITCHES
	db	0
%endif

comtab:
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
	; (SYSINIT:38EDh)
	db      7,      "BUFFERS",      CONFIG_BUFFERS
	db      5,      "BREAK",        CONFIG_BREAK
	db      6,      "DEVICE",       CONFIG_DEVICE
	db      10,     "DEVICEHIGH",   CONFIG_DEVICEHIGH
	db      5,      "FILES",        CONFIG_FILES
	db      4,      "FCBS",         CONFIG_FCBS
	db      9,      "LASTDRIVE",    CONFIG_LASTDRIVE
	db      10,     "MULTITRACK",   CONFIG_MULTITRACK
	db      8,      "DRIVPARM",     CONFIG_DRIVPARM
	db      6,      "STACKS",       CONFIG_STACKS
	db      7,      "COUNTRY",      CONFIG_COUNTRY
	db      5,      "SHELL",        CONFIG_SHELL
	db      7,      "INSTALL",      CONFIG_INSTALL
	db      7,      "COMMENT",      CONFIG_COMMENT
	db      3,      "REM",          CONFIG_REM
	db      8,      "SWITCHES",     CONFIG_SWITCHES
	db      3,      "DOS",          CONFIG_DOS
	db	0

deviceparameters:	
	; A_DEVICEPARAMETERS <0,dev_3inch720kb,0,80>
devp.specialfunc:	; deviceparameters +
	db	0	; A_DEVICEPARAMETERS.DP_SPECIALFUNCTIONS
devp.devtype:
	db	2	; A_DEVICEPARAMETERS.DP_DEVICETYPE
devp.devattr:
	dw	0	; A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES
devp.cylinders:
	dw	80	; A_DEVICEPARAMETERS.DP_CYLINDERS

	times	286	db 0
	
hlim:	dw	2
slim:	dw	9

drive:	db	0

switches:
	dw	0

; the following are the recommended bpbs for the media that
; we know of so far.

; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; MSDOS 5.0 IO.SYS - SYSINIT:3AA9h

; 48 tpi diskettes

bpb48t	dw	512
	db	2
	dw	1
	db	2
	dw	112
	dw	2*9*40 ; 720
	db	0FDh
	dw	2
	dw	9
	dw	2
	dd	0
        dd      0

; 96tpi diskettes

bpb96t:	dw	512
	db	1
	dw	1
	db	2
	dw	224
	dw	2*15*80 ; 2400
	db	0F9h
	dw	7
	dw	15
	dw	2
	dd	0
        dd      0

; 3 1/2 inch diskette bpb

bpb35:	dw	512
	db	2
	dw	1
	db	2
	dw	112
	dw	2*9*80 ; 1440
	db	0F9h
	dw	3
	dw	9
	dw	2
	dd	0
        dd      0
      
bpb35h:	dw	512
	db	1
	dw	1
	db	2
	dw	224
	dw	2*18*80 ; 2880
	db	0F0h
	dw	9
	dw	18
	dw	2
	dd	0
        dd      0

; m037 - BEGIN

bpb288:	dw	512
	db	2
	dw	1
	db	2
	dw	240
	dw	2*36*80 ; 5760
	db	0F0h
	dw	9
	dw	36
	dw	2
	dd	0
        dd      0

; m037 - END

; 12/05/2019

align 2

; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; MSDOS 5.0 IO.SYS - SYSINIT:3B26h

bpbtable:   dw	    bpb48t		; 48tpi drives
	    dw	    bpb96t		; 96tpi drives
	    dw	    bpb35		; 3.5" drives
; the following are not supported, so default to 3.5" media layout
	    dw	    bpb35		; not used - 8" drives
	    dw	    bpb35		; not used - 8" drives
	    dw	    bpb35		; not used - hard files
	    dw	    bpb35		; not used - tape drives
	    dw	    bpb35h		; 3-1/2" 1.44mb drive
	    dw	    bpb35		; ERIMO				m037
	    dw	    bpb288		; 2.88 MB diskette drives	m037

switchlist: 
	db	8,"FHSTDICN"	     ; preserve the positions of n and c.

;----------------------------------------------------------------------------
; Messages
;----------------------------------------------------------------------------

; 19/04/2019 - Retro DOS v4.0

; MSDOS 6.21 IO.SYS - SYSINIT:54D1h

	db 	0

; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; MSDOS 5.0 IO.SYS - SYSINIT:3B44h

badopm:
	db	0Dh,0Ah 
	db	'Unrecognized command in CONFIG.SYS'
crlfm:
	db	0Dh,0Ah,'$'
badparm:
	db	0Dh,0Ah
	db	'Bad command or parameters - $'
badsiz_pre:
	db	0Dh,0Ah
	db	'Sector size too large in file $'
badld_pre:
	db	0Dh,0Ah
	db	'Bad or missing $'
badcom:
	db	'Command Interpreter',0
badcountry:
	db	0Dh,0Ah
	db	'Invalid country code or code page',0Dh,0Ah,'$'
badcountrycom:
	db	0Dh,0Ah
	db	'Error in COUNTRY command',0Dh,0Ah,'$'
insufmemory:
	db	0Dh,0Ah
	db	'Insufficient memory for COUNTRY.SYS file',0Dh,0Ah,'$'
badmem:
	db	0Dh,0Ah
	db	'Configuration too large for memory',0Dh,0Ah,'$'
badblock:
	db	0Dh,0Ah
	db	'Too many block devices',0Dh,0Ah,'$'
badstack:
	db	0Dh,0Ah
	db	'Invalid STACK parameters',0Dh,0Ah,'$'
badorder:
	db	0Dh,0Ah
	db	'Incorrect order in CONFIG.SYS line $'
errorcmd:
	db	'Error in CONFIG.SYS line $'

; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
%if 0

OnOff:	db	'ON'
OnOff2:	db	'OFF'

StartMsg:
	db	'Starting MS-DOS...',0Dh,0Ah
	db	0Ah,0
_$PauseMsg:
	db	'Press any key to continue . . .',0Dh,0Ah,'$'
_$CleanMsg:
	db	'MS-DOS is bypassing your CONFIG.SYS and AUTOEXEC.BAT files.',0Dh,0Ah,'$'
_$InterMsg:
	db	'MS-DOS will prompt you to confirm each CONFIG.SYS command.',0Dh,0Ah,'$'
_$MenuHeader:
	db	0Dh,0Ah
	db	'  MS-DOS 6.2 Startup Menu',0Dh,0Ah
	db	'  =======================',0Dh,0Ah,'$'
_$MenuPrmpt:
	db	'  Enter a choice: $'
_$StatusLine:
	db	'F5=Bypass startup files F8=Confirm each line of CONFIG.SYS'
	db	'and AUTOEXEC.BAT [ ]$'
_$InterPrmpt:
	db	' [Y,N]?$'
_$YES:	db	'YES$'
_$NO:	db	'NO $'
_$TimeOut:
	db	'Time remaining: $'
badcomprmpt:
	db	'Enter correct name of Command Interpreter (eg, C:\COMMAND.COM)'
	db	0Dh,0Ah,'$'
_$AutoPrmpt:
	db	'Process AUTOEXEC.BAT [Y,N]?$'

%endif

; 02/11/2022 - Retro DOS v4.0 (Modified MSDOS 5.0 IO.SYS)
; MSDOS 5.0 IO.SYS - SYSINIT:3CE0h

TooManyDrivesMsg:
	db	'WARNING! Logical drives past Z: exist and will be ignored',0Dh,0Ah,'$'

;MSDOS 6.21 IO.SYS - SYSINIT:587Ch
	;db	'Wrong DBLSPACE.BIN version',0Dh,0Ah,'$'
	;db	7 dup(0)

	;times	7 db 0
	; 02/11/2022 (MSDOS 5.0 IO.SYS SYSINIT compatibility)
;MSDOS 5.0 IO.SYS - SYSINIT:3D1Ch
	times 4 db 0

;MSDOS 6.21 IO.SYS - SYSINIT:5899h

;----------------------------------------------------------------------------
; 20/04/2019 - Retro DOS v4.0

bss_start:

ABSOLUTE bss_start

alignb 16

SI_end:  ; SI_end equ $

;----------------------------------------------------------------------------

;sysinitseg	ends

; ****************************************************************************