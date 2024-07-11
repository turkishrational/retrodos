; ****************************************************************************
; SYSINIT.BIN (MSDOS 3.3 Kernel) - RETRO DOS v3.1 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 22/11/2022 (Previous: 03/08/2019)
; ----------------------------------------------------------------------------
; Beginning: 03/06/2018 (Retro DOS 3.0), 29/06/2019 (Retro DOS 3.1)
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.14 
; ----------------------------------------------------------------------------
;	    ((nasm sysinit.asm -l sysinit.txt -o SYSINIT.BIN)) 	
; ----------------------------------------------------------------------------
; Derived from 'SYSINIT1.ASM' and 'SYSINIT2.ASM' files of MSDOS 3.3
; source code by Microsoft, 24/07/1987
; ----------------------------------------------------------------------------
; Derived from 'SYSINIT.ASM' file of MSDOS 2.0 (IBM PCDOS v2.0) source code
; by Microsoft, 12/10/1983
; ****************************************************************************
; main file: 'retrodos.asm'
; incbin 'SYSINIT.BIN' ; (SYINITSEG)

; -----------------------------------------
; SYSINIT2.S - Retro DOS v3.0, 06/07/2018
; SYSINIT2.ASM - Retro DOS v3.1, 29/06/2019 
; -----------------------------------------

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

; ----------------------------------------------------------------------------
; TITLE   BIOS SYSTEM INITIALIZATION
; ----------------------------------------------------------------------------

FALSE   EQU     0
TRUE    EQU     0FFFFh

;IBMVER     EQU     TRUE
;IBM        EQU     IBMVER
;STACKSW    EQU	    TRUE		;Include Switchable Hardware Stacks
;IBMJAPVER  EQU     FALSE		; If TRUE set KANJI true also
;MSVER      EQU     FALSE
;ALTVECT    EQU     FALSE		; Switch to build ALTVECT version
;KANJI      EQU     FALSE

;include version.inc

;	IF	IBMJAPVER
;NOEXEC	EQU	TRUE
;       ELSE
;NOEXEC	EQU	FALSE
;       ENDIF

MAJOR_VERSION   EQU     3
MINOR_VERSION   EQU     30	;3.30

;DOSSIZE EQU	0A000H

;; (MSDOS 2.0) SYSINITVAR table offsets
;; ----------------------------------------------------------------------

;DPBHEAD  EQU	0  ; dd ; Pointer to head of DPB-FAT list	
;SFT_ADDR EQU	4  ; dd ; Pointer to first FCB table
;BCLOCK	  EQU	8  ; dd ; Pointer to the CLOCK device	
;BCON	  EQU	12 ; dd ; Console device entry points
;NUMIO	  EQU	16 ; db ; Number of disk tables
;MAXSEC   EQU	17 ; dw ; Maximum allowed sector size
;BUFFHEAD EQU	19 ; dd	; Pointer to head of buffer queue
;DEVHEAD  EQU	23 ; dd ; Set to list start passed by BIOS at DOS Init
;NUL_DEV  EQU	23 ; dd	; NUL device header, first 4 bytes of dev header
;			; (4+14 bytes)
;			; Points to the next device in device chain 
;DEVTYPE  EQU   27 ; dw ; = ISNULL
;SNULDEV  EQU 	29 ; dw
;INULDEV  EQU	31 ; dw
;DEVNAME  EQU	33 ; 8 BYTES ; "NUL     "

;; ----------------------------------------------------------------------
;; Internal DOS data returned by DOSINIT
;;
;struc	SYSINITVAR
;.DPBHEAD:	resd 1			; Pointer to head of DPB-FAT list
;.SFT_ADDR:	resd 1			; Pointer to first FCB table
;; The following address points to the CLOCK device
;.BCLOCK:	resd 1
;; The following address is used by DISKSTATCHK it is always
;; points to the console input device header
;.BCON:		resd 1			; Console device entry points
;.NUMIO:	resb 1			; Number of disk tables
;.MAXSEC:	resw 1			; Maximum allowed sector size
;.BUFFHEAD:	resd 1			; Head of buffer queue
;.DEVHEAD:	resd 1			; NUL dev head points to next dev
;.size:
;endstruc

; ----------------------------------------------------------------------
; device definitions

;Attribute bit masks
DEVTYP  EQU     8000H           ;Bit 15 - 1  if Char, 0 if block
DEVIOCTL EQU    4000H           ;Bit 14 - CONTROL mode bit
ISFATBYDEV EQU  2000H           ;Bit 13 - Device uses FAT ID bytes, comp media.
ISCIN   EQU     0001H           ;Bit 0 - This device is the console input.
ISCOUT  EQU     0002H           ;Bit 1 - This device is the console output.
ISNULL  EQU     0004H           ;Bit 2 - This device is the null device.
ISCLOCK EQU     0008H           ;Bit 3 - This device is the clock device.
ISIBM   EQU     0010H           ;Bit 4 - This device is special

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

; 10/06/2018 - Retro DOS v3.0 (MSDOS 3.3, BUFFER.INC, 24/07/1987)

struc	BUFFINFO
.buf_link:	resd 1	; Pointer to next buffer in list
.buf_ID:	resb 1	; Drive of buffer (bit 7 = 0)
			; SFT table index (bit 7 = 1)
			; = FFH if buffer free
.buf_flags:	resb 1	; Bit 7 = 1 if Remote file buffer
			;	= 0 if Local device buffer
			; Bit 6 = 1 if buffer dirty
			; Bit 5 = Reserved
			; Bit 4 = Search bit (bit 7 = 1)
			; Bit 3 = 1 if buffer is DATA
			; Bit 2 = 1 if buffer is DIR
			; Bit 1 = 1 if buffer is FAT
			; Bit 0 = Reserved
.buf_sector:	resw 1	; Sector number of buffer (bit 7 = 0)
; The next two items are often refed as a word (bit 7 = 0)
.buf_wrtcnt:	resb 1	; For FAT sectors, # times sector written out
.buf_wrtcntinc:	resb 1	; "   "     "   , # sectors between each write
.buf_DPB :	resd 1	; Pointer to drive parameters
.buf_fill:	resw 1	; How full buffer is (bit 7 = 1)
.size:
endstruc

%define buf_offset	dword [buf_sector]
			;For bit 7 = 1, this is the byte
			;offset of the start of the buffer in
			;the file pointed to by buf_ID.  Thus
			;the buffer starts at location
			;buf_offset in the file and contains
			;buf_fill bytes.

BUFINSIZ        EQU     BUFFINFO.size ; ; Size of structure in bytes


buf_Free	EQU	0FFh		; buf_id of free buffer

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

buf_NetID	EQU	BUFINSIZ


; ----------------------------------------------------------------------
; DPB structure

; 08/06/2018 - Retro DOS v3.0 (MSDOS 3.3, DPB.INC, 24/07/1987)

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
.FAT_SIZE:	resb 1		; Number of records occupied by FAT
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
; Memory arena structure
struc ARENA
.SIGNATURE:	resb 1		; 4D for valid item, 5A for last item
.OWNER:		resw 1		; owner of arena item
.SIZE:		resw 1		; size in paragraphs of item
endstruc

; ----------------------------------------------------------------------
;
; Process data block (otherwise known as program header)
;

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
.JFN_LENGTH:	resw 1 ; 25/03/18 ; number of handles allowed
.JFN_POINTER:	resd 1 ; 25/03/18 ; pointer to JFN table
.NEXT_PDB:	resd 1 ; 25/03/18 ; pointer to nested PDB's
.PAD1:		resb 20 ; 25/03/2018
.CALL_SYSTEM:	resb 5		; portable method of system call
.PAD2:		resb 7 ; 25/03/2018
endstruc

; ----------------------------------------------------------------------
; <system call definitions>

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
; XENIX CALLS
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
_WAIT				EQU 77  ; 77     4D
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
									  
struc ROMBIOS_DESC		; BIOS_SYSTEM_DESCRIPTOR						  
.bios_SD_leng:		resw 1				  
.bios_SD_modelbyte:	resb 1					  
.bios_SD_scnd_modelbyte: 
			resb 1					  
			resb 1					  
.bios_SD_featurebyte1:	resb 1					  
			resb 4					  
endstruc					  
									  
;FeatureByte1	bit map equates 					  
DMAchannel3		equ 10000000b					  
ScndIntController	equ 01000000b					  
RealTimeClock		equ 00100000b					  
KeyEscapeSeq		equ 00010000b					  
					;;End of Modification

; ----------------------------------------------------------------------
; SYSVAR.INC (MSDOS 3.3 - 24/07/1987) 	
; ----------------------------------------------------------------------
; 05/06/2018 - Retro DOS v3.0

;	SCCSID = @(#)sysvar.asm 1.1 85/04/10
struc SysInitVars
.SYSI_DPB:    resd 1			; DPB chain
.SYSI_SFT:    resd 1			; SFT chain
.SYSI_CLOCK:  resd 1			; CLOCK device
.SYSI_CON:    resd 1			; CON device
.SYSI_MAXSEC: resw 1			; maximum sector size
.SYSI_BUF:    resd 1			; buffer chain
.SYSI_CDS:    resd 1			; CDS list
.SYSI_FCB:    resd 1			; FCB chain
.SYSI_KEEP:   resw 1			; keep count
.SYSI_NUMIO:  resb 1			; Number of block devices
.SYSI_NCDS:   resb 1			; number of CDS's
.SYSI_DEV:    resd 1			; device list
.size:
endstruc

;This is added for more information exchage between DOS, BIOS.
;DOS will give the pointer to SysInitTable in ES:DI. - J.K. 5/29/86
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

; ----------------------------------------------------------------------
; CURDIR.INC (MSDOS 3.3 - 24/07/1987) 	
; ----------------------------------------------------------------------
; 08/06/2018 - Retro DOS v3.0

;	SCCSID = @(#)curdir.asm	1.1 85/04/10
;BREAK <Current directory list structure>
									  
; CDS items are used by the internal routines to store cluster numbers and
; network identifiers for each logical name.  The ID field is used dually,
; both as net ID and for a cluster number for local devices.  In the case
; of local devices, the cluster number will be -1 if there is a potential
; of the disk being changed or if the path must be recracked.  The END
; field is the location of the end of the definition.  No .. is allowed
; past this point							

DIRSTRLEN	EQU	64+3		; Max length in bytes of directory strings
TEMPLEN 	EQU	DIRSTRLEN*2

struc 		curdir_list
.cdir_text	resb	DIRSTRLEN	; text of assignment and curdir
.cdir_flags	resw	1		; various flags
.cdir_devptr	resd	1		; local pointer to DPB or net device
.cdir_ID	resw	1		; cluster of current dir (net ID)
		resw	1
.cdir_usr_word	resw	1
.cdir_end	resw	1		; end of assignment
.size:
endstruc

curdirLen	EQU	curdir_list.size	; Needed for screwed up
						; ASM87 which doesn't allow
						; Size directive as a macro
						; argument
%define curdir_netID	dword [curdir_list.cdir_ID]

;Flag word masks
curdir_isnet	EQU	1000000000000000B
curdir_inuse	EQU	0100000000000000B
curdir_splice	EQU	0010000000000000B
curdir_local	EQU	0001000000000000B

; ----------------------------------------------------------------------
; SF.INC (MSDOS 3.3 - 24/07/1987) 	
; ----------------------------------------------------------------------
; 09/06/2018 - Retro DOS v3.0

;
; system file table
;

struc	SF
.SFLink:	resd	1
.SFCount:	resw	1		; number of entries
.SFTable:	resw	1		; beginning of array of the following
.size:
endstruc

;
; system file table entry
;

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
.sf_time: 	resw	1		; Time associated with file
.sf_date: 	resw	1		; Date associated with file
.sf_size: 	resd	1		; Size associated with file
.sf_position:	resd	1		; Read/Write pointer or LRU count for FCBs
;
; Starting here, the next 7 bytes may be used by the file system to store an
; ID
;
.sf_cluspos:	resw	1		; Position of last cluster accessed
.sf_lstclus:	resw	1		; Last cluster accessed
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

; ----------------------------------------------------------------------

        ;INCLUDE DOSSYM.ASM
        ;INCLUDE DEVSYM.ASM

        ;IF      NOT IBM
        ;IF      NOT IBMJAPVER
        ;EXTRN   RE_INIT:FAR
        ;ENDIF
        ;ENDIF

;SYSINITSEG      SEGMENT PUBLIC 'SYSTEM_INIT'

	;ASSUME  CS:SYSINITSEG,DS:NOTHING,ES:NOTHING,SS:NOTHING

SYSINITSEG:

	[org 0]

	; 04/06/2018 - Retro DOS v3.0 (MSDOS 3.3)

	;EXTRN	BADCOM:BYTE					
	;EXTRN	SYSSIZE:BYTE					
	;EXTRN	CONDEV:BYTE,AUXDEV:BYTE,PRNDEV:BYTE,COMMND:BYTE
	;EXTRN	DeviceParameters:byte
				
	;EXTRN	INT24:NEAR,MEM_ERR:NEAR
	;EXTRN	DOCONF:NEAR

        ;PUBLIC  CURRENT_DOS_LOCATION
        ;PUBLIC  FINAL_DOS_LOCATION
        ;PUBLIC  DEVICE_LIST
        ;PUBLIC  MEMORY_SIZE
        ;PUBLIC  DEFAULT_DRIVE
        ;PUBLIC  BUFFERS
        ;PUBLIC  FILES
        ;PUBLIC  SYSINIT
	;PUBLIC	 CNTRYFILEHANDLE
	;PUBLIC	 COMMAND_LINE

; 29/06/2019 - Retro DOS v3.1

; ----------------------------------------------------------------------
; SYSINIT1.ASM (MSDOS 6.0 - 1991) 	
; ----------------------------------------------------------------------
; 21/03/2019 - Retro DOS v4.0

true	equ	0FFFFh
false	equ	0
cr	equ	13
lf	equ	10
tab	equ	9

multMULT		equ	4Ah
multMULTGETHMAPTR	equ	1
multMULTALLOCHMA	equ	2

;NOEXEC  equ     FALSE

;stacksw    equ	true	;include switchable hardware stacks
;mycds_size equ	88	;size of curdir_list. if it is not
			;the same, then will generate compile error.

EntrySize   equ     8

MinCount    equ     8
DefaultCount equ    9
MaxCount    equ     64

MinSize     equ     32
DefaultSize equ     128
MaxSize     equ     512

;%define allocbyte	byte [es:bp+0]  ; byte [es:bp]
;%define intlevel	byte [es:bp+1]
;%define savedsp 	word [es:bp+2]
;%define savedss 	word [es:bp+4]
;%define newsp		word [es:bp+6]

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
		dw	0	; spare field but leave these in order
StackCount:	dw	0
StackAt: 	dw	0
StackSize:	dw	0
Stacks:		dw	0
		dw	0

FirstEntry:	dw	Stacks
LastEntry:	dw	Stacks+(DefaultCount*EntrySize)-EntrySize
NextEntry:	dw	Stacks+(DefaultCount*EntrySize)-EntrySize

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
;	no other interrupt (hardware, software or nmi) can occur until
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
	; 26/05/2019
	cmp	byte [es:0FFFEh],0F9h ; mdl_convert ;check if convertible
	pop	es
	jne	short normal02

	in	al,62h
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
	mov	es,[cs:Stacks+2]	; Get segment of stacks
	mov	bp,[cs:NextEntry]	; get most likely candidate
	mov	al,allocated ; 1
	;xchg	[es:bp+allocbyte],al 
	xchg	[es:bp],al		; grab the entry

	cmp	al,free ; 0		; still avail?
	jne	short notfree02

	sub	word [cs:NextEntry],EntrySize ; set for next interrupt

found02:
	mov	[es:bp+savedsp],sp	; save sp value
	mov	[es:bp+savedss],ss	; save ss also

	mov	ax,bp			; temp save of table offset

	mov	bp,[es:bp+newsp]	; get new SP value
	cmp	[es:bp],ax		; check for offset into table
	jne	short foundbad02

	mov	ax,es			; point ss,sp to the new stack
	mov	es,bp
	mov	bp,sp
	mov	bp,[bp+6]
	mov	ss,ax
	mov	sp,es
	mov	es,ax
	mov	bp,[cs:bp]

	pushf				; go execute the real interrupt handler
	call	far [cs:bp]		;  which will iret back to here

	mov	bp,sp			; retrieve the table offset for us
	mov	bp,[es:bp]		;  but leave it on the stack
	mov	ss,[es:bp+savedss]	; get old stack back
	mov	sp,[es:bp+savedsp]

	;mov	byte [es:bp+allocbyte],free ; free the entry
	mov	byte [es:bp],free ; 0
	mov	[cs:NextEntry],bp	; setup to use next time

	pop	es
	pop	bp			; saved on entry
	pop	ax			; saved on entry
	add	sp,2
	iret				; done with this interrupt

notfree02:
	cmp	al,allocated ; 1	; error flag
	je	short findnext02	;  no, continue
	;xchg	[es:bp+allocbyte],al	;  yes, restore error value
	xchg	[es:bp],al

findnext02:
	call	longpath
	jmp	short found02

foundbad02:
	cmp	bp,[cs:FirstEntry]
	jc	short findnext02
	mov	bp,ax			; flag this entry
	;mov	byte [es:bp+allocbyte],clobbered
	mov	byte [es:bp],clobbered ; 3
	jmp	short findnext02	; keep looking

; ----------------------------------------------------------------------

; Common routines

longpath:
	; 21/03/2019
	mov	bp,[cs:LastEntry]	; start with last entry in table
lploopp:
	;cmp	byte [es:bp+allocbyte],free ; is entry free?
	cmp	byte [es:bp],free
	jne	short inuse		;  no, try next one

	mov	al,allocated
	;xchg	[es:bp+allocbyte],al	; allocate entry
	xchg	[es:bp],al
	cmp	al,free 		; is it still free?
	je	short found		;  yes, go use it

	cmp	al,allocated		; is it other than Allocated or Free?
	je	short inuse		;  no, check the next one

	;mov	[es:bp+allocbyte],al	;  yes, put back the error state
	mov	[es:bp],al
inuse:
	cmp	bp,[cs:FirstEntry]
	je	short fatal
	sub	bp,EntrySize
	jmp	short lploopp
found:
	retn
fatal:
	; 27/05/2019
	;push	ds
	mov	ax,0F000h		;look at the model byte
	mov	ds,ax
	cmp	byte [0FFFEh],0F9h ; mdl_convert ; convertible?
	;pop	ds
	jne	short skip_nmis

	mov	al,07h			; disable pc convertible nmis
	out	72h,al

skip_nmis:
	cli				; disable and mask
	mov	al,0FFh			;   all other ints
	out	021h,al
	out	0A1h,al
	
	; 29/06/2019 - Retro DOS v3.1
	mov	si,cs
	mov	ds,si
	
	mov	si,fatal_msg
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

;align 16

	;times	07F0h-252h db 0 ; 29/06/2019

endstackcode:

	;db	0 ; 29/06/2019 - Retro DOS v3.1

align 16

; ----------------------------------------------------------------------
; SYINIT1.ASM (MSDOS 3.3) - SYSINIT.ASM (MSDOS 2.0)
; ----------------------------------------------------------------------
; 04/06/2018 - Retro DOS v3.0 (MSDOS 3.3, SYSINIT1.ASM, 24/07/1987)

SYSINIT:
        JMP	GOINIT
	;JMP	SYSIN ; 25/02/2018 - Retro DOS 2.0 modification
;align 4
align 2  ; 04/07/2019
DOSINFO: 
	dw	0
CURRENT_DOS_LOCATION: 
	dw	0
MSDOS:	
ENTRY_POINT:
	dw	0
FINAL_DOS_LOCATION:
	dw	0
DEVICE_LIST:
	dd	0
DOSSIZE: ; Retro DOS 2.0 feature - 25/02/2018
	dw	0   ; 'MSDOS.BIN' kernel size in words

; 04/06/2018 - Retro DOS v3.0
; 28/03/2018
;; MSDOS 3.3 - SYSINIT1.ASM - 24/07/1987
;
SYSI_Country:	
	dw	0 ; 5/29/86 Pointer to
	dw	0 ; country table in DOS

;; STACKSW   EQU   TRUE ;Include Switchable Hardware Stacks
;; (SYSCONF.ASM, 1987)
;; Internal Stack Parameters
;EntrySize	equ	8
;MinCount	equ	8
;DefaultCount	equ	9
;MaxCount	equ	64
;MinSize 	equ	32
;DefaultSize	equ	128
;MaxSize 	equ	512

	;IF	STACKSW    ; STACKSW TRUE
;
; Internal Stack Parameters

STACK_COUNT:	DW	DefaultCount
STACK_SIZE:	DW	DefaultSize
STACK_ADDR:	DD	0
	;ENDIF

; 05/06/2018 - Retro DOS v3.0

; various default values

MEMORY_SIZE:
	dw	1
DEFAULT_DRIVE:
	db	0
BUFFERS:
	DW	-1	; initialized during buffer allocation
;BUFFERS: 
;	DW	2	; two buffers
FILES:
	db	8	; enough files for pipe
FCBS:
	db	4	; performance for recycling
KEEP:
	db	0	; keep original set
NUM_CDS:
	db	5	; 5 net drives
CONFBOT:
	dw	0
ALLOCLIM:
	dw	0
FOOSTRNG:
	db	"A:\",0
COMMAND_LINE:
	db	2,0,"P" ; Default Command.com Args
        
	times	29 db 0
ZERO:
	db	0
SEPCHR:
	db	0

; 10/06/2018
Sys_Model_Byte:
	db	0FFh	;model byte used in SYSINIT
Sys_Scnd_Model_Byte:
	db	0	;secondary model byte used in SYSINIT

        ;IF      NOT NOEXEC
;COMEXE EXEC0 <0,COMMAND_LINE,DEFAULT_DRIVE,ZERO>
        ;ENDIF

; 01/05/2018
COMEXE:
EXEC0.ENVIRON:	DW	0	; seg addr of environment
EXEC0.COM_LINE:	DW	COMMAND_LINE ; pointer to asciz command line
		DW	0
EXEC0.5C_FCB:	DW	DEFAULT_DRIVE ; default fcb at 5C
		DW	0
EXEC0.6C_FCB:	DW	ZERO	; default fcb at 6C
		DW	0

COUNT:
	dw	0
CHRPTR:
	dw	0

; 04/06/2018
CntryFileHandle:
	DW 	0 

;align 4
align 2 ; 04/07/2019 

BUFPTR:
	; LEAVE THIS STUFF IN ORDER!
MEMLO:
	dw	0
PRMBLK:
MEMHI:
	dw	0
LDOFF:
	dw	0
AREA:
	dw	0

PACKET:
	db	22
	db	0
	db	0	; INITIALIZE CODE
	dw	0
	times	8 db 0
UNITCOUNT:
	db	0
BREAK_ADDR:
	dd	0
BPB_ADDR:
	dd	0
DriveNumber:
	db	0

align 2

; 06/07/2019 - Retro DOS v3.1
TempStack:
	times	128 db 0

GOINIT:
	; 06/07/2018
	; 04/06/2018 - Retro DOS v3.0
; before doing anything else, let's set the model byte
	mov	ah,0C0h 		;get system configuration
	int	15h			; *
	jc	short No_ROM_Config
	cmp	ah,0			; double check
	jne	short No_ROM_Config
	mov	al,[ES:BX+ROMBIOS_DESC.bios_SD_modelbyte]
	mov	[cs:Sys_Model_Byte],al 
	mov	al,[ES:BX+ROMBIOS_DESC.bios_SD_scnd_modelbyte]
	mov	[cs:Sys_Scnd_Model_Byte],al
	;jmp	short Move_Myself
	jmp	short SYSIN
No_ROM_Config:				; Old ROM
	mov	ax,0F000h
	mov	ds,ax
	mov	al,[0FFFEh]
	mov	[cs:Sys_Model_Byte],al	;set the model byte.

;Move_Myself:
;	; 25/02/2018 - Retro DOS 2.0 - MSDOS 2.0 "SYSINIT.ASM"
;	; (Modified for Retro DOS 2.0, for NASM 'incbin' method)
;
;	SYSINITSIZE	EQU  sysinit_code_end - sysinit_code_start
;
;      	;CLD
;      	XOR     SI,SI
;      	MOV     DI,SI
;	; 19/03/2018
;      	mov	CX,[SYSINIT_START+MEMORY_SIZE]
;;	CMP     CX,1
;;      JNZ     short NOSCAN
;;	MOV     CX,2048		; START SCANNING AT 32K BOUNDARY
;;	XOR     BX,BX
;;MEMSCAN:
;;	INC     CX
;;    	JZ	short SETEND
;;     	MOV     DS,CX
;;     	MOV     AL,[BX]
;;     	NOT     AL
;;     	MOV     [BX],AL
;;     	CMP     AL,[BX]
;;     	NOT     AL
;;     	MOV     [BX],AL
;;     	JZ      short MEMSCAN
;;SETEND:
;;	MOV     AX,CS
;;      MOV     DS,AX   
;;	MOV	[SYSINIT_START+MEMORY_SIZE],CX 
;;NOSCAN:
;	MOV     AX,SYSINITSIZE + 15
;	SHR     AX,1                    ; Divide by 16 for paras
;	SHR     AX,1
;	SHR     AX,1
;	SHR     AX,1
;
;	SUB     CX,AX
;	MOV     ES,CX ; SYSINITSEG = [MEMORY_SIZE] - (SYSIZE+15)/16
;	MOV     CX,SYSINITSIZE + 1
;	SHR     CX,1                    ; Divide by 2 to get words
;	REP     MOVSW                   ; RELOCATE SYSINIT
;
;	PUSH    ES
;	MOV	AX,SYSIN  ; 05/06/2018 (MSDOS 3.3, SYSINIT1.ASM)
;	PUSH    AX
;	
;	RETF	; far jump to final location of SYSINIT code
;

;
;       MOVE THE DOS TO ITS PROPER LOCATION
;

	;nop	; 30/03/2018
SYSIN:
	; Retro DOS 2.0 - 25/02/2018 

	PUSH	CS
	POP	DS

        MOV     CX,[DOSSIZE] ; words (not bytes!)
	        
	MOV	AX,[FINAL_DOS_LOCATION]
	MOV     ES,AX
        MOV     AX,[CURRENT_DOS_LOCATION]
        MOV     DS,AX

        XOR     SI,SI
        MOV     DI,SI
	;INC	CX
	;SHR	CX,1
        REP     MOVSW

	MOV	AX,CS
	MOV	DS,AX
        MOV	DX,[MEMORY_SIZE]
	LDS     SI,[DEVICE_LIST]

        CLI
        MOV     SS,AX
        MOV     SP,LOCSTACK	
	STI
align 2
	; 30/03/2018
LOCSTACK:
        CALL	FAR [CS:MSDOS]	; FINAL_DOS_LOCATION:0 
		       		;('jmp DOSINIT' in 'MSHEAD.ASM')
		       		;('DOSINIT:' is in 'MSINIT.ASM')

	; DS = PSP address for default process
	; ES:DI = Address of SYSINITVAR (MSDOS 2.0/2.11, Retro DOS 2.0)

	;MOV     [CS:DOSINFO+2],ES ; SAVE POINTER TO DOS INFO
	;MOV     [CS:DOSINFO],DI

	; 17/06/2018
	PUSH	DS			; Save as input to RE_INIT
	
	PUSH	CS
	POP	DS

	; 05/06/2018 - Retro DOS v3.0
	; ES:DI = Address of pointer to SYSINITVARS structure (MSDOS 3.3)

	;mov	ax,[es:di+SysInitVars_Ext.SYSI_InitVars] ; 5/29/86
	mov	ax,[es:di] ; 05/07/2019 
	;mov	[CS:DOSINFO],ax
	mov	[DOSINFO],ax
	mov	ax,[es:di+SysInitVars_Ext.SYSI_InitVars+2]
	;mov	[CS:DOSINFO+2],ax
	mov	[DOSINFO+2],ax ;set the sysvar pointer

	mov	ax,[es:di+SysInitVars_Ext.SYSI_Country_Tab]
	;mov	[CS:SYSI_Country],ax
	mov	[SYSI_Country],ax
	mov	ax,[es:di+SysInitVars_Ext.SYSI_Country_Tab+2]
	;mov	[CS:SYSI_Country+2],ax
	mov	[SYSI_Country+2],ax	;set the SYSI_Country pointer

	les	di,[DOSINFO]	;es:di -> dosinfo

	; 09/06/2018
	; 08/06/2018 - Retro DOS v3.0 (MSDOS 3.3)
	;SYSI_NUMIO equ	32
 	
	; 25/03/2018 - Retro DOS v2.0 (MSDOS 2.0 <-> 3.3)
	;SYSI_NUMIO equ	16 ; SYSINITVAR NUMIO offset for MSDOS 2.0

	; MSDOS 3.3 - SYSINIT1.ASM
	MOV	AL,[ES:DI+SYSI_NUMIO] ; SYSINITVAR.NUMIO
	MOV	[DriveNumber],AL ; Save start of installable block drvs

	MOV	AX,CS
	SUB	AX,11H			; room for header we will copy shortly

	MOV	[CONFBOT],AX		; Temp "unsafe" location

	; 08/06/2018
	;PUSH	DS			; Save as input to RE_INIT
	;PUSH	CS
	;POP	DS
	CALL	TEMPCDS 		; Set up CDSs so RE_INIT and SYSINIT
					;   can make DISK system calls

	POP	DS			; Recover DS input to RE_INIT

	; 06/07/2019

        ;IF	NOT IBMJAPVER
	
	CALL	KERNEL_SEGMENT:RE_INIT	; Re-call the BIOS
        
	;ENDIF

        STI
        CLD

; DOSINIT has set up a default "process" (PHP) at DS:0. We will move it out
; of the way by putting it just below SYSINIT at end of memory.

        MOV     BX,CS
        SUB     BX,10H
        MOV     ES,BX
        XOR     SI,SI
        MOV     DI,SI
        MOV     CX,80H
        REP     MOVSW

	MOV	[ES:PDB.JFN_POINTER+2],ES ; Relocate

 	; Set Process Data Block - Program Segment Prefix address
	; BX = PDB/PSP segment
        MOV     AH,SET_CURRENT_PDB
        INT     21H			; Tell DOS we moved it

        PUSH	DS

        PUSH    CS
        POP     DS

        MOV     DX,INT24	; SET UP INT 24 HANDLER
        MOV     AX,(SET_INTERRUPT_VECTOR*256)+24H
        INT     21H

	; 25/03/2018 - MSDOS 3.3 - SYSINIT1.ASM
	MOV	BX,0FFFFH
	MOV	AH,ALLOC
	INT	21H			;FIRST TIME FAILS

; 09/07/2019 - Retro DOS v3.1 test
; X = BX = 9623h

	MOV	AH,ALLOC
	INT	21H			;SECOND TIME GETS IT
	MOV	[AREA],AX
	MOV	[MEMHI],AX		; MEMHI:MEMLO now points to
					; start of free memory
; 09/07/2019 - Retro DOS v3.1 test
; M = [MEMHI] = 099Dh

; 07/07/2019 (Test by using Bochs emulator)
; [MEMHI] = [AREA] = 099Dh (for two harddisks)
;		   = 0996h (for single harddisk)
;		   = 098Fh (for floppy disks, without harddisk)		

        ;IF	ALTVECT
	; 27/06/2018
	MOV	DX,BOOTMES
        CALL	PRINT		; Print message DOSINIT couldn't
        ;ENDIF

	POP	DS 

	; 17/06/2018
        MOV     DL,[CS:DEFAULT_DRIVE]

	OR      DL,DL
        JZ      SHORT NODRVSET
        
	DEC     DL                      ; A = 0
        MOV     AH,SET_DEFAULT_DRIVE
        INT     21H                     ; SELECT THE DISK

NODRVSET:
	CALL	DOCONF                  ; DO THE CONFIG STUFF

	CALL	ENDFILE	; 25/03/2018 - MSDOS 3.3 - SYSINIT1.ASM

	; 01/05/2018 (NOEXEC = FALSE)
	;IF	NOEXEC

	;MOV	BP,DS                   ; SAVE COMMAND.COM SEGMENT

	;;PUSH	DS
	;;POP	ES

	;; 13/04/2018
	;MOV	ES,BP	

        ;MOV	BX,CS
        ;SUB	BX,10H
        ;MOV	DS,BX
        ;XOR	SI,SI
        ;MOV	DI,SI
        ;MOV	CX,80H
        ;REP	MOVSW
	;MOV	[ES:PDB.JFN_POINTER+2],ES ; Relocate ; 25/03/2018
        ;MOV	BX,ES

        ;MOV	AH,SET_CURRENT_PDB
        ;INT	21H

        ;MOV	[ES:PDB.PARENT_PID],ES  ; WE ARE THE ROOT
        ;ENDIF

; We must now close all handles and reopen STDIN,STDOUT,STDERR in order
; to get a possibly NEW device driver for CON. STDAUX and STDPRN must
; also be openned.

        PUSH    CS
        POP     DS

        MOV     AL,[FILES]
	;CBW
	XOR	AH,AH			; DO NOT USE CBW INSTRUCTION!!!!!
					;  IT DOES SIGN EXTEND.
        MOV     CX,AX
        XOR     BX,BX                   ; Close standard input
        MOV     AH,CLOSE
        INT     21H

        MOV     BX,2
RCCLLOOP: 				; Close everybody but standard output
        MOV     AH,CLOSE
        INT     21H
        INC     BX
        LOOP    RCCLLOOP

        MOV     DX,CONDEV
        MOV     AL,2
        MOV     AH,OPEN                 ; OPEN CON FOR READ/WRITE
        STC
        INT     21H
        JNC     SHORT GOAUX
        CALL    BADFIL
        JMP     SHORT GOAUX2

GOAUX:  PUSH    AX
        MOV     BX,1                    ; close standard output
        MOV     AH,CLOSE
        INT     21H
        POP     AX

        MOV     BX,AX                   ; New device handle
        MOV     AH,XDUP
        INT     21H                     ; Dup to 1, STDOUT
        MOV     AH,XDUP
        INT     21H                     ; Dup to 2, STDERR

GOAUX2: MOV     DX,AUXDEV
        MOV     AL,2                    ; READ/WRITE ACCESS
        CALL    OPEN_DEV

        MOV     DX,PRNDEV
        MOV     AL,1                    ; WRITE ONLY
        CALL    OPEN_DEV

	; 08/06/2018 - Retro DOS v3.0

;Global Rearm command for Shared Interrupt devices attached in the system;
;Shared interrupt attachment has some problem when it issues interrupt
;during a warm reboot.	Once the interrupt is presented by the attachment,
;no further interrupts on that level will be presented until a global rearm
;is issued. BIOS will issue a global rearm after each device driver is loaded.
;To issue a global rearm:	;For PC1, XT, Palace
;			  OUT 02F2h, XX  ; Interrupt level 2
;			  OUT 02F3h, XX  ; Interrupt level 3
;			  OUT 02F4h, XX  ; Interrupt level 4
;			  OUT 02F5h, XX  ; Interrupt level 5
;			  OUT 02F6h, XX  ; Interrupt level 6
;			  OUT 02F7h, XX  ; Interrupt level 7
;
;				;For PC AT, in addition to the above commands,
;				;need to handle the secondary interrupt handler
;			  OUT 06F2h, XX  ; Interrupt level 10
;			  OUT 06F3h, XX  ; Interrupt level 11
;			  OUT 06F4h, XX  ; Interrupt level 12
;			  OUT 06F6h, XX  ; Interrupt level 14
;			  OUT 06F7h, XX  ; Interrupt level 15
;
;				;For All others machine
;			  None.
; where XX stands for any value.

	;PUSH	AX			;Save register
	;PUSH	BX			;Save register
	;PUSH	DX			;Save register
	;PUSH	ES			;Save register

	MOV	AL,0FFH 		;Set up registers for global rearm
	MOV	DX,02F2H		;
	OUT	DX,AL			;OUT 02F2H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 02F3H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 02F4H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 02F5H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 02F6H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 02F7H,0FFH

	MOV	AX,0F000H		;Get machine type
	MOV	ES,AX			;
	CMP	BYTE [ES:0FFFEH],0FCH	;Q: Is it an AT type machine?
	JNE	short REARMDONE		;  N: Skip next rearm
	MOV	AH,0C0H 		;Get system configuration
	INT	15H			;Q: Is it an old ROM?
	JC	short REARMDONE		;  Y: Skip next rearm
	TEST	byte [ES:BX+ROMBIOS_DESC.bios_SD_featurebyte1],ScndIntController
					; Q: Present?
	JE	short REARMDONE		;  N: Skip next rearm

	MOV	AL,0FFH 		;Set up registers for global rearm
	MOV	DX,06F2H		;
	OUT	DX,AL			;OUT 06F2H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 06F3H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 06F4H,0FFH
	INC	DX			;
	INC	DX			;
	OUT	DX,AL			;OUT 02F6H,0FFH
	INC	DX			;
	OUT	DX,AL			;OUT 02F7H,0FFH

REARMDONE:
	;POP	ES			;Restore register
	;POP	DX			;Restore register
	;POP	BX			;Restore register
	;POP	AX			;Restore register

;Global Rearm end *******************

;
; SET UP THE PARAMETERS FOR COMMAND
;
GOSET:
        MOV     SI,COMMAND_LINE+1

	; 01/05/2018 (NOEXEC = FALSE)
        ;IF	NOEXEC
        ;MOV	DI,81H
        ;ELSE
        PUSH	DS
        POP	ES
        MOV	DI,SI
        ;ENDIF

        MOV     CL,-1
COMTRANLP:                              ; FIND LENGTH OF COMMAND LINE
        INC     CL
        LODSB
        STOSB                           ; COPY COMMAND LINE IN
        OR      AL,AL
        JNZ     SHORT COMTRANLP
        DEC     DI
        MOV     AL,0DH
        STOSB

	; 01/05/2018 (NOEXEC = FALSE)
        ;IF	NOEXEC
        ;MOV	[ES:80H],CL
        ;MOV	AL,[DEFAULT_DRIVE]
        ;MOV	[ES:5CH],AL
        ;ELSE
        MOV	[COMMAND_LINE],CL       ; Count
        ;ENDIF

        ;PUSH    CS
        ;POP     ES

        MOV     DX,COMMND		; NOW POINTING TO FILE DESCRIPTION

	; 01/05/2018 (NOEXEC = FALSE)
        ;IF	NOEXEC
        ;MOV     ES,BP			; SET LOAD ADDRESS
        ;MOV     BX,100H
        ;CALL    LDFIL			; READ IN COMMAND
        ;JC      SHORT COMERR
        ;MOV     DS,BP
        ;MOV     DX,80H
        ;MOV     AH,SET_DMA		; SET DISK TRANSFER ADDRESS
        ;INT     21H
        ;CLI
        ;MOV     SS,BP
        ;MOV     SP,DX
        ;STI
        ;XOR     AX,AX			; PUSH A WORD OF ZEROS
        ;PUSH    AX
        ;PUSH    BP			; SET HIGH PART OF JUMP ADDRESS
        ;MOV     AX,100H
        ;PUSH    AX			; SET LOW PART OF JUMP ADDRESS
        ;RETF				; CRANK UP COMMAND!

        ;ELSE

	; 08/06/2018 - Retro DOS v3.0  (MSDOS 3.3, SYSINIT1.ASM, 1987)

; We are going to open the command interpreter and size it as is done in
; LDFIL. The reason we must do this is that SYSINIT is in free memory. If
; there is not enough room for the command interpreter, EXEC will probably
; overlay our stack and code so when it returns with an error SYSINIT won't be
; here to catch it. This code is not perfect (for instance .EXE command
; interpreters are possible) because it does its sizing based on the
; assumption that the file being loaded is a .COM file. It is close enough to
; correctness to be usable.

	PUSH	DX			; Save pointer to name

; First, find out where the command interpreter is going to go.
	MOV	BX,0FFFFH
	MOV	AH,ALLOC
	INT	21H			;Get biggest piece
	MOV	AH,ALLOC
	INT	21H			;SECOND TIME GETS IT
	JC	short MEMERRJX		; Oooops
	MOV	ES,AX

; 09/07/2019 - Retro DOS v3.1 test
; A = ES = 0C60h

	MOV	AH,DEALLOC
	INT	21H			; Give it right back
	MOV	BP,BX
; ES:0 points to Block, and BP is the size of the block
;   in para.

; 09/07/2019 - Retro DOS v3.1 test
; B = BP = 9360h

; We will now adjust the size in BP DOWN by the size of SYSINIT. We
;   need to do this because EXEC might get upset if some of the EXEC
;   data in SYSINIT is overlayed during the EXEC.
	MOV	BX,[MEMORY_SIZE]
	MOV	AX,CS
	SUB	BX,AX			; BX is size of SYSINIT in Para
	ADD	BX,11H			; Add the SYSINIT PHP
	SUB	BP,BX			; BAIS down
	JC	short MEMERRJX		; No Way.

; 09/07/2019 - Retro DOS v3.1 test
; C = [MEMORY_SIZE]-CS+11h = 019Dh

	MOV	AX,OPEN*256	 	;OPEN THE FILE being EXECED
	STC				;IN CASE OF INT 24
	INT	21H
	JC	short COMERR		; Ooops
	MOV	BX,AX			;Handle in BX
	XOR	CX,CX
	XOR	DX,DX
	MOV	AX,(LSEEK*256)|2
	STC				;IN CASE OF INT 24
	INT	21H			; Get file size in DX:AX
	JC	short COMERR
    ; Convert size in DX:AX to para in AX
	ADD	AX,15			; Round up size for conversion to para
	ADC	DX,0
	MOV	CL,4
	SHR	AX,CL
	MOV	CL,12
	SHL	DX,CL			; Low nibble of DX to high nibble
	OR	AX,DX			; AX is now # of para for file
	ADD	AX,10H			; 100H byte PHP
	CMP	AX,BP			; Will it fit?
	JB	short OKLD		; Jump if yes.
MEMERRJX:
	JMP	MEM_ERR

OKLD:
	MOV	AH,CLOSE
	INT	21H			; Close file

	POP	DX			; Recover pointer to name

	PUSH	CS
	POP	ES

	MOV	BX,COMEXE
        ;MOV	[BX+EXEC0.COM_LINE+2],CS
        ;MOV	[BX+EXEC0.5C_FCB+2],CS
        ;MOV	[BX+EXEC0.6C_FCB+2],CS
	MOV	[EXEC0.COM_LINE+2],CS
        MOV	[EXEC0.5C_FCB+2],CS
        MOV	[EXEC0.6C_FCB+2],CS

        ;XOR	AX,AX
        ;MOV	AH,EXEC
        MOV	AX,EXEC*256
	STC				; IN CASE OF INT 24
        INT	21H			; GO START UP COMMAND
        ;ENDIF

COMERR:
        MOV     DX,BADCOM		; WANT TO PRINT COMMAND ERROR
        CALL    BADFIL
STALL:  
	JMP     SHORT STALL

	; 08/06/2018 - Retro DOS v3.0

TEMPCDS:
	LES	DI,[DOSINFO]

	MOV	CL,[ES:DI+SYSI_NUMIO]
	XOR	CH,CH
	MOV	[ES:DI+SYSI_NCDS],CL
	MOV	AL,CL
	MOV	AH,curdir_list.size ; 81
	MUL	AH
	call	ParaRound
	MOV	SI,[CONFBOT]
	SUB	SI,AX
	MOV	[ALLOCLIM],SI		; Can't alloc past here!
	MOV	[ES:DI+SYSI_CDS+2],SI
	MOV	AX,SI
	MOV	WORD [ES:DI+SYSI_CDS],0
	;LDS	SI,[ES:DI+SYSI_DPB]
	lds	si,[es:di] ; 05/07/2019
	MOV	ES,AX
	XOR	DI,DI
FOOSET: 				; Init CDSs
	; 18/06/2018
	MOV	AX,[CS:FOOSTRNG]
	STOSW
	MOV	AX,[CS:FOOSTRNG+2]
	STOSW
	INC	BYTE [CS:FOOSTRNG]
	XOR	AX,AX
	PUSH	CX
	MOV	CX,curdir_list.cdir_flags - 4
	REP	STOSB
	CMP	SI,-1
	JNZ	short NORMCDS
	;XOR	AX,AX ; 06/07/2019
	MOV	CL,3
	REP	STOSW
	POP	CX
	JMP	SHORT FINCDS

NORMCDS:
	POP	CX
	MOV	AX,curdir_inuse
	STOSW				; curdir_flags
	MOV	AX,SI
	STOSW				; curdir_devptr
	MOV	AX,DS
	STOSW
	LDS	SI,[SI+DPB.NEXT_DPB]
FINCDS:
	MOV	AX,-1
	STOSW				; curdir_ID
	STOSW				; curdir_ID
	STOSW				; curdir_user_word
	mov	ax,2
	stosw				; curdir_end
	LOOP	FOOSET
	MOV	BYTE [CS:FOOSTRNG],"A"
	retn

; 09/06/2018 - Retro DOS v3.0  (MSDOS 3.3, SYSINIT1.ASM, 1987)

; Allocate FILEs
;------------------------------------------------------------------------------
ENDFILE:

; WE ARE NOW SETTING UP FINAL CDSs, BUFFERS, FILES, FCSs STRINGs etc. We no
; longer need the space taken by The TEMP stuff below CONFBOT, so set ALLOCLIM
; to CONFBOT.

	; 18/06/2018
	;MOV	AX,[CS:CONFBOT]
	;MOV	[CS:ALLOCLIM],AX
	PUSH	CS
	POP	DS
	MOV	AX,[CONFBOT]
	MOV	[ALLOCLIM],AX
	call	ROUND
	MOV	AL,[FILES]
	SUB	AL,5
	JBE	short DOFCBS
	XOR	AH,AH			; DO NOT USE CBW INSTRUCTION!!!!!
					;  IT DOES SIGN EXTEND.
	MOV	BX,[MEMLO]
	MOV	DX,[MEMHI]

	LDS	DI,[DOSINFO]		;GET POINTER TO DOS DATA
	LDS	DI,[DI+SYSI_SFT]	;DS:BP POINTS TO SFT
	;MOV	[DI+SF.SFLink],BX
	mov	[di],bx ; 05/07/2019
	MOV	[DI+SF.SFLink+2],DX	;SET POINTER TO NEW SFT
	PUSH	CS
	POP	DS
	LES	DI,[MEMLO]		;POINT TO NEW SFT
	;MOV	WORD [ES:DI+SF.SFLink],-1
	mov	word [es:di],-1	 ; 05/07/2019	
	MOV	[ES:DI+SF.SFCount],AX
	MOV	BL,SF_ENTRY.size ; 53
	MUL	BL			;AX = NUMBER OF BYTES TO CLEAR
	MOV	CX,AX
	ADD	[MEMLO],AX		;ALLOCATE MEMORY
	MOV	AX,6
	ADD	[MEMLO],AX		;REMEMBER THE HEADER TOO
	call	ROUND			; Check for mem error before the STOSB
	ADD	DI,AX
	XOR	AX,AX
	REP	STOSB			;CLEAN OUT THE STUFF

; Allocate FCBs
;------------------------------------------------------------------------------
DOFCBS:
	; 24/06/2018
	;PUSH	CS
	;POP	DS
	call	ROUND
	MOV	AL,[FCBS]
	XOR	AH,AH			; DO NOT USE CBW INSTRUCTION!!!!!
					;  IT DOES SIGN EXTEND.
	MOV	BX,[MEMLO]
	MOV	DX,[MEMHI]
	LDS	DI,[DOSINFO]		;GET POINTER TO DOS DATA

	MOV	[DI+SYSI_FCB],BX
	MOV	[DI+SYSI_FCB+2],DX	;SET POINTER TO NEW Table

	MOV	BL,[CS:KEEP]
	XOR	BH,BH
	MOV	[DI+SYSI_KEEP],BX
	PUSH	CS
	POP	DS
	LES	DI,[MEMLO]		;POINT TO NEW Table
	;MOV	WORD [ES:DI+SF.SFLink],-1
	mov	word [es:di],-1 ; 05/07/2019
	MOV	[ES:DI+SF.SFCount],AX
	MOV	BL,SF_ENTRY.size ; 53
	MOV	CX,AX
	MUL	BL			;AX = NUMBER OF BYTES TO CLEAR
	ADD	[MEMLO],AX		;ALLOCATE MEMORY
	MOV	AX,SF.size-2
	ADD	[MEMLO],AX		;REMEMBER THE HEADER TOO
	call	ROUND			; Check for mem error before the STOSB
	ADD	DI,AX			;Skip over header
	MOV	AL,"A"
FillLoop:
	PUSH	CX			; save count
	MOV	CX,SF_ENTRY.size	; number of bytes to fill
	cld
	REP	STOSB			; filled
	
	;MOV	WORD [ES:DI-SF_ENTRY.size+SF_ENTRY.sf_ref_count],0
	;MOV	WORD [ES:DI-SF_ENTRY.size+SF_ENTRY.sf_position],0
	;MOV	WORD [ES:DI-SF_ENTRY.size+SF_ENTRY.sf_position+2],0
	
	; 06/07/2019
	MOV	[ES:DI-SF_ENTRY.size+SF_ENTRY.sf_ref_count],cx ; 0
	MOV	[ES:DI-SF_ENTRY.size+SF_ENTRY.sf_position],cx  ; 0
	MOV	[ES:DI-SF_ENTRY.size+SF_ENTRY.sf_position+2],cx ; 0

	POP	CX
	LOOP	FillLoop

; Allocate Buffers
;------------------------------------------------------------------------------

; Search through the list of media supported and allocate 3 buffers if the
; capacity of the drive is > 360KB

	CMP	byte [BUFFERS], -1	; Has buffers been already set?
	je	short DoDefaultBuff
	jmp	DOBUFF			; the user entered the buffers=.

DoDefaultBuff:
	MOV	byte [BUFFERS], 2	; Default to 2 buffers
	; 24/06/2018
	;PUSH	AX
	;PUSH	DS
	;
	;LES	BP,[CS:DOSINFO] 	; Search through the DPB's
	;LES	BP,[ES:BP+SYSI_DPB]	; Get first DPB

	; 24/06/2018
	;PUSH	CS
	;POP	DS
	; 18/06/2018
	LES	BP,[DOSINFO] 		; Search through the DPB's
	;LES	BP,[ES:BP+SYSI_DPB]	; Get first DPB
	les	bp,[es:bp] ; 05/07/2019

NEXTDPB:
	; Test if the drive supports removeable media
	;MOV	BL,[ES:BP+DPB.DRIVE]
	mov	bl,[es:bp] ; 05/07/2019
	INC	BL
	MOV	AX,(IOCTL*256)|8
	INT	21H

; Ignore fixed disks
	OR	AX, AX			; AX is nonzero if disk is nonremoveable
	JNZ	short NOSETBUF

; Get parameters of drive
	XOR	BX,BX
	;MOV	BL,[ES:BP+DPB.DRIVE]
	mov	bl,[es:bp] ; 05/07/2019
	INC	BL
	MOV	DX,DEVICEPARAMETERS
	MOV	AX,(IOCTL*256)|GENERIC_IOCTL
	MOV	CX,(RAWIO*256)|GET_DEVICE_PARAMETERS
	INT	21H
	JC	short NOSETBUF		; Get next DPB if driver doesn't support
					; Generic IOCTL
; Determine capacity of drive
; Media Capacity = #Sectors * Bytes/Sector
	MOV	BX,[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_TOTALSECTORS]

; To keep the magnitude of the media capacity within a word,
; scale the sector size
; (ie. 1 -> 512 bytes, 2 -> 1024 bytes, ...)
	MOV	AX,[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_BYTESPERSECTOR]
	XOR	DX,DX
	MOV	CX,512
	DIV	CX				; Scale sector size in factor of
						; 512 bytes

	MUL	BX				; AX = #sectors * size factor
	OR	DX,DX				; Just in case of LARGE floppies
	JNZ	short SETBUF
	CMP	AX,720				; 720 Sectors * size factor of 1
	JBE	short NOSETBUF
SETBUF:
	MOV	byte [BUFFERS],3
	jmp	short Chk_Memsize_for_Buffers 	; Now check the mem size for default buff count
NOSETBUF:
	CMP	WORD [ES:BP+DPB.NEXT_DPB],-1
	jz	short Chk_Memsize_for_Buffers
	LES	BP,[ES:BP+DPB.NEXT_DPB]
	JMP	short NEXTDPB

;From DOS 3.3, the default number of buffers will be changed according to the
;memory size too.
; Default buffers = 2
; If diskette Media > 360 kb, then default buffers = 3
; If memory size > 128 kb (2000H para), then default buffers = 5
; If memory size > 256 kb (4000H para), then default buffers = 10
; If memory size > 512 kb (8000H para), then default buffers = 15.

Chk_Memsize_for_Buffers:
	cmp	word [MEMORY_SIZE], 2000h
	jbe	short BUFSET

	;mov	byte [BUFFERS], 5
	;cmp	word [MEMORY_SIZE], 4000h
	;jbe	short BUFSET
	;mov	byte [BUFFERS], 10
	;cmp	word [MEMORY_SIZE], 8000h
	;jbe	short BUFSET

	mov	byte [BUFFERS], 15

	cmp	word [MEMORY_SIZE], 8000h
	;jnb	short BUFSET
	ja	short BUFSET ; 06/07/2019

	mov	byte [BUFFERS], 10

	cmp	word [MEMORY_SIZE], 4000h
	;jnb	short BUFSET
	ja	short BUFSET ; 06/07/2019

	mov	byte [BUFFERS], 5

	;cmp	word [MEMORY_SIZE], 8000h
	;jbe	short BUFSET
	;mov	byte [BUFFERS], 15

BUFSET:
	; 24/06/2018
	;POP	DS
	;POP	AX
DOBUFF:
	call	ROUND
	DEC	byte [BUFFERS]		; FIRST DEC acounts for buffer already
					;    in system.
	JZ	short BUF1		; All done
	PUSH	DS
	LES	DI,[BUFPTR]
	LDS	BX,[DOSINFO]
	MOV	AX,[BX+SYSI_BUF]	; Link in new buffer
	;MOV	[ES:DI+BUFFINFO.buf_link],AX
	mov	[es:di],ax ; 05/07/2019
	MOV	AX,[BX+SYSI_BUF+2]
	MOV	[ES:DI+BUFFINFO.buf_link+2],AX
	MOV	[BX+SYSI_BUF],DI
	MOV	[BX+SYSI_BUF+2],ES
	MOV	word [ES:DI+BUFFINFO.buf_ID],00FFH ; NEW BUFFER FREE
	MOV	BX,[BX+SYSI_MAXSEC]
	POP	DS
	ADD	BX,BUFINSIZ ; 16
	ADD	[MEMLO],BX
	JMP	short DOBUFF

; 10/06/2018 - Retro DOS v3.0

; Allocate CDSs
;------------------------------------------------------------------------------
BUF1:
	call	ROUND
	LES	DI,[DOSINFO]
	MOV	CL,[ES:DI+SYSI_NUMIO]
	CMP	CL,[NUM_CDS]
	JAE	short GOTNCDS		; User setting must be at least NUMIO
	MOV	CL,[NUM_CDS]
GOTNCDS:
	XOR	CH,CH
	MOV	[ES:DI+SYSI_NCDS],CL
	MOV	AX,[MEMHI]
	MOV	[ES:DI+SYSI_CDS+2],AX
	MOV	AX,[MEMLO]
	MOV	[ES:DI+SYSI_CDS],AX
	MOV	AL,CL
	MOV	AH,curdir_list.size
	MUL	AH
	call	ParaRound
	ADD	[MEMHI],AX
	call	ROUND			; Check for mem error before initializing
	;LDS	SI,[ES:DI+SYSI_DPB]
	lds	si,[es:di] ; 05/07/2019
	LES	DI,[ES:DI+SYSI_CDS]
	CALL	FOOSET

; Allocate Space for Internal Stack
;------------------------------------------------------------------------------

	;IF	STACKSW
	PUSH	CS
	POP	DS
	;ASSUME	DS:SYSINITSEG

; 07/07/2019 (Bochs emulator, two harddisks)
; [MEMHI] = 0BA4h (at thispoint)

	; !!! 07/07/2019 - Retro DOS v3.1 !!! by Erdogan Tan
	; (move stack code -destination- address 1024 bytes upward
	;	it is seen as enough to prevent overlap!) 
	; (Note: Currently, I don't know the reason which causes overlap!?)	
	
	;add	word [MEMHI],64  ; 64 = 1024/16
	add	word [MEMHI],72	 ; 09/07/2019

	;IF	IBM
;Don't install the system stack on the PCjr. Ignore STACKS=command too.
	CMP	byte [Sys_Model_Byte],0FDh	; PCjr = 0FDh
	;JE	short SkipStack
	JE	short SkipStack1
	;ENDIF

;If the user does not entered STACKS= command, as a default, do not install
;sytem stacks for PC1, PC XT, PC Portable cases.
;Otherwise, install it to the user specified value or to the default
;value of 9, 128 for the rest of the system.

	; 09/07/2019

	cmp	word [STACK_ADDR],-1		;Has the user entered "stacks=" command?
	je	short DoInstallStack		;Then install as specified by the user
	cmp	byte [Sys_Scnd_Model_Byte],0	;PC1, XT has the secondary model byte = 0
	jne	short DoInstallStack		;Other model should have def. stack of 9, 128
	cmp	byte [Sys_Model_Byte],0FFh	;PC1 ?
	;je	short SkipStack
	je	short SkipStack1
	cmp	byte [Sys_Model_Byte],0FEh	;PC/XT or PC Portable ?	
	;je	short SkipStack
	jne	short DoInstallStack

SkipStack1:
	; 09/07/2019
	mov	word [MEMLO],((EntrySize+DefaultSize)*DefaultCount)+endstackcode
	jmp	short SkipStack

DoInstallStack:
	mov	ax,[STACK_COUNT]		; Stack_count = 0?
	cmp	ax,0				;then, stack size must be 0 too.
	;jz	short SkipStack			;Don't install stack.
	jz	short SkipStack1

; 10/21/86 Dynamic Relocation of Stack code.			
	call	ROUND				;[memhi] = Seg. for stack code
						;[memlo] = 0
	mov	ax,[MEMHI]
	mov	es,ax				;ES -> Seg. the stack code is going to move.

	;assume	es:nothing
	; 18/06/2018
	;push	cs
	;pop	ds
	; 05/07/2018
	xor	si,si			;!!We know that Stack code is at the beginning of SYSINIT.
	xor	di,di
	mov	cx,endstackcode
	mov	[MEMLO],cx
	call	ROUND				;Have enough space for relocation?
	rep	movsb

	;add word [MEMHI],64  ; 64 = 1024/16

	MOV	AX,[MEMLO]			; Set address of stacks
	MOV	[STACK_ADDR],AX
	MOV	AX,[MEMHI]
	MOV	[STACK_ADDR+2],AX

; 07/07/2019 (Bochs emulator, two harddisks)
; [MEMHI] = 0BD1h (at thispoint)

;	Space for Internal Stack = STACK_COUNT(ENTRYSIZE + STACK_SIZE)
	MOV	AX,EntrySize
	ADD	AX,[STACK_SIZE]
	MOV	CX,[STACK_COUNT]
	MUL	CX
	call	ParaRound		; Convert size to paragraphs
	ADD	[MEMHI],AX
	call	ROUND			; Check for memory error before
					; continuing
	CALL	stackinit		; Initialize hardware stack

SkipStack:
	;ENDIF

	; 18/06/2018
	;PUSH	CS
	;POP	DS
	;ASSUME	DS:SYSINITSEG

; Allocate rest of memory to system
;------------------------------------------------------------------------------
	call	ROUND

	; !!! 07/07/2019 - Retro DOS v3.1 !!! by Erdogan Tan
	; (move stack code -destination- address 1024 bytes upward
	;	it is seen as enough to prevent overlap!) 
	; (Note: Currently, I don't know the reason which causes overlap!?)	

	;add	word [MEMHI],64  ; 64 = 1024/16

	MOV	BX,[MEMHI]

; 07/07/2019 (Bochs emulator, two harddisks)
; [MEMHI] = 0C5Eh (at thispoint)

	MOV	AX,[AREA]
	MOV	ES,AX			;CALC WHAT WE NEEDED
	SUB	BX,AX

; 09/07/2019 - Retro DOS v3.1 test
; Z = BX = 02BAh

	MOV	AH,SETBLOCK
	INT	21H			;GIVE THE REST BACK
	PUSH	ES
	MOV	AX,ES
	DEC	AX
	MOV	ES,AX			;Point to arena
	MOV	word [ES:ARENA.OWNER],8	;Set impossible owner

	POP	ES

	;IF	NOEXEC   ; (NOEXEC = FALSE)
	;MOV	BX,0FFFFH		;ALLOCATE THE REST OF MEM FOR COMMAND
	;MOV	AH,ALLOC
	;INT	21H
	;MOV	AH,ALLOC
	;INT	21H
	;MOV	DS,AX
	;ENDIF

	retn

ParaRound:
	ADD	AX,15
	RCR	AX,1
	SHR	AX,1
	SHR	AX,1
	SHR	AX,1
	retn

;-------------------------------------------------------------------------
; 5/6/86. MSSTACK initialization routine.
	;IF	STACKSW 

;INCLUDE STKINIT.INC

	;ENDIF
;--------------------------------------------------------------------------

; 05/07/2018

; ----------------------------------------------------------------------
; STKINIT.INC - MSDOS 3.3 - 24/07/1987
; ----------------------------------------------------------------------
; 10/06/2018 - Retro DOS v3.0

KERNEL_SEGMENT	equ 0070h

; retrodos3.asm (offset addresses in MSDOS.SYS or RETRODOS.SYS)
; -----------------------------------------------------------
; Note: These offset addresses must be chanqed when the code 
; 	in retrodos3.asm (MSDOS.SYS) file will be changed.
INT19SEM equ 05A4h ; 29/06/2019 - retrodos3.txt
ORIG19	 equ 05A8h ; 29/06/2019 - retrodos3.txt
INT19OLD02 equ ORIG19+4
INT19OLD08 equ ORIG19+8
INT19OLD09 equ ORIG19+12
INT19OLD0A equ ORIG19+16
INT19OLD0B equ ORIG19+20
INT19OLD0C equ ORIG19+24
INT19OLD0D equ ORIG19+28
INT19OLD0E equ ORIG19+32
INT19OLD70 equ ORIG19+36
INT19OLD72 equ ORIG19+40
INT19OLD73 equ ORIG19+44
INT19OLD74 equ ORIG19+48
INT19OLD76 equ ORIG19+52
INT19OLD77 equ ORIG19+56

; 06/07/2019
RE_INIT	equ 1A86h ; 1AA6h for MSDOS 3.3 IBMBIO.COM

; 29/06/2019 - Retro DOS v3.1
		
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

stackinit:
	; 01/06/2019 - Retro DOS v4.0
	; SYSINIT:13BCh (MSDOS 6.21, MSDOS.SYS)

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

	;mov	ax,[STACK_COUNT]		;defined in cs
	;mov	[es:StackCount],ax		;defined in stack code area
	; 06/06/2019
	mov	cx,[STACK_COUNT]
	mov	[es:StackCount],cx

	;mov	ax,[STACK_SIZE]			;in cs
	;mov	[es:StackSize],ax
	; 06/06/2019
	mov	di,[STACK_SIZE]
	mov	[es:StackSize],di  ; *#*

	;mov	ax,[STACK_ADDR]			; offset
	;mov	[es:Stacks],ax
	; 06/06/2019
	mov	bp,[STACK_ADDR]			; offset
	mov	[es:Stacks],bp

	mov	ax,[STACK_ADDR+2]		; segment
	mov	[es:Stacks+2],ax

; initialize the data fields with the parameters

; "firstentry" will always be at stacks

	;mov	bp,[es:Stacks]			; get offset of stack
	mov	[es:FirstEntry],bp

; the stacks will always immediately follow the table entries

	mov	ax,EntrySize ; 8
	;mov	cx,[es:StackCount]
	mul	cx
	add	ax,bp

	mov	[es:StackAt],ax	 ; *#*
	mov	bx,ax
	sub	bx,2

; zero the entire stack area to start with

	;mov	di,[es:StackAt]
	;mov	ax,[es:StackSize]
	; 06/06/2019
	xchg	ax,di ; *#*

	mul	cx
	mov	cx,ax
	xor	ax,ax

	push	es
	pop	ds				; ds = relocated stack code seg.

;now, ds -> stack code area

	mov	es,[Stacks+2]			; get segment of stack area.
	cld
	rep	stosb

	mov	cx,[StackCount]

; loop for "count" times, building a table entry
;  cs = sysinitseg, ds = relocated stack code seg, es = segment of stack space
;  cx = number of entries
;  es:bp => base of stacks - 2
;  es:bx => first table entry

buildloop:
	;mov	byte [es:bp+allocbyte],free	; mov [es:bp+0],0
	mov	byte [es:bp],free
	mov	[es:bp+intlevel],al	; ax = 0
	mov	[es:bp+savedsp],ax
	mov	[es:bp+savedss],ax
	add	bx,[StackSize]
	mov	[es:bp+newsp],bx		; mov [es:bp+6],bx
	mov	[es:bx],bp
	add	bp,EntrySize ; 8

	loop	buildloop

	sub	bp,EntrySize ; 8
	mov	[LastEntry],bp
	mov	[NextEntry],bp

	push	ds
	mov	ax,0F000h		; look at the model byte
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

	; 01/06/2019 - Retro DOS v4.0
	; SYSINIT:1462h (MSDOS 6.21, MSDOS.SYS)

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

	; 01/06/2019
	; SYSINIT:149Eh (MSDOS 6.21, MSDOS.SYS)

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
	
	;push	ds
	;
	;lds	bx,[es:si]
	;push	ds
	;pop	dx
	;	
	;cmp	dx,0
	;je	short int_0A_first
	;
	;cmp	byte [bx],0CFh
	;je	short int_0A_first
	;
	;cmp	word [bx+6],424Bh
	;je	short int_0A_not_first
	;
	;cmp	dx,0F000h
	;jne	short int_0A_not_first
	
	;push	es
	;;push	dx
	;;mov	dx,0F000h
	;mov	es,dx
	;cmp	bx,[es:0FF01h]
       	;;pop	dx
	;pop	es
	;je	short int_0A_first

	call	int_xx_first_check ; 27/03/2019 -  Retro DOS v4.0
	jnc	short int_0A_first
	
int_0A_not_first:
	;pop	ds
	mov	di,INT19OLD0A
	mov	bx,old0A
	mov	dx,int0A
	call	new_init_loop
	
	jmp	short int_0A_end

; -----------------------------------
	; 27/03/2019 - Retro DOS v4.0
int_xx_first_check:
	push	ds
	lds	bx,[es:si]
	push	ds
	pop	dx
		
	cmp	dx,0
	je	short int_xx_first
	
	cmp	byte [bx],0CFh
	je	short int_xx_first
	
	cmp	word [bx+6],424Bh
	je	short int_xx_not_first
	
	cmp	dx,0F000h
	jne	short int_xx_not_first

	push	es
	;push	dx
	;mov	dx,0F000h
	mov	es,dx
	cmp	bx,[es:0FF01h]
       	;pop	dx
	pop	es
	je	short int_xx_first

int_xx_not_first:
	stc
int_xx_first:
	pop	ds
	retn
; -----------------------------------

int_0A_first:
	;pop	ds
int_0A_end:

stkinit_0B:
	mov	si,0Bh*4 ; 44
	
	call	int_xx_first_check ; 27/03/2019 -  Retro DOS v4.0
	jnc	short int_0B_end ; int_0B_first
	
int_0B_not_first:
	mov	di,INT19OLD0B
	mov	bx,old0B
	mov	dx,int0B
	call	new_init_loop
int_0B_end:

stkinit_0C:
	mov	si,0Ch*4 ; 48
	
	call	int_xx_first_check
	jnc	short int_0C_end ; int_0C_first ; 01/06/2019
	
int_0C_not_first:
	mov	di,INT19OLD0C
	mov	bx,old0C
	mov	dx,int0C
	call	new_init_loop
int_0C_end:

stkinit_0D:
	mov	si,0Dh*4 ; 52
	
	call	int_xx_first_check
	jnc	short int_0D_end ; int_0D_first
	
int_0D_not_first:
	mov	di,INT19OLD0D
	mov	bx,old0D
	mov	dx,int0D
	call	new_init_loop
int_0D_end:

stkinit_0E:
	mov	si,0Eh*4 ; 56
	
	call	int_xx_first_check
	jnc	short int_0E_end ; int_0E_first
	
int_0E_not_first:
	mov	di,INT19OLD0E
	mov	bx,old0E
	mov	dx,int0E
	call	new_init_loop
int_0E_end:

stkinit_72:
	mov	si,72h*4 ; 456
	
	call	int_xx_first_check
	jnc	short int_72_end ; int_72_first
	
int_72_not_first:
	mov	di,INT19OLD72
	mov	bx,old72
	mov	dx,int72
	call	new_init_loop
int_72_end:

stkinit_73:
	mov	si,73h*4 ; 460
	
	call	int_xx_first_check
	jnc	short int_73_end ; int_73_first
	
int_73_not_first:
	mov	di,INT19OLD73
	mov	bx,old73
	mov	dx,int73
	call	new_init_loop
int_73_end:

stkinit_74:
	mov	si,74h*4 ; 464
	
	call	int_xx_first_check
	jnc	short int_74_end ; int_74_first
	
int_74_not_first:
	mov	di,INT19OLD74
	mov	bx,old74
	mov	dx,int74
	call	new_init_loop
int_74_end:

stkinit_76:
	mov	si,76h*4 ; 472
	
	call	int_xx_first_check
	jnc	short int_76_end ; int_76_first
	
int_76_not_first:
	mov	di,INT19OLD76
	mov	bx,old76
	mov	dx,int76
	call	new_init_loop
int_76_end:

stkinit_77:
	mov	si,77h*4 ; 476
	
	call	int_xx_first_check
	jnc	short int_77_end ; int_77_first
	
int_77_not_first:
	mov	di,INT19OLD77
	mov	bx,old77
	mov	dx,int77
	call	new_init_loop

int_77_end:
	push	ds
	mov	ax,0F000h		;loook at the model byte
	mov	ds,ax
	cmp	byte [0FFFEh],0F9h ; mdl_convert ;pc convertible?
	pop	ds
	jne	short skip_enablenmis

	mov	al,27h			; enable convertible nmis
	out	72h,al

skip_enablenmis:
	sti
	;mov	ax,Bios_Data ; 70h
	mov	ax,KERNEL_SEGMENT ; 70h
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

; 29/06/2019 - Retro DOS v3.1
								
; ----------------------------------------------------------------------
; 27/03/2019 - Retro DOS v4.0

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
	;mov	ax,Bios_Data ; 70h
	mov	ax,KERNEL_SEGMENT ; 70h
	mov	ds,ax			;set int19oldxx value in bios for
	mov	ax,[es:si]		;int 19 handler
	mov	[di],ax
	mov	ax,[es:si+2]
	mov	[di+2],ax
	pop	ds

	mov	[es:si],dx  	;set vector to point to new int handler
	mov	[es:si+2],ds
	retn

; End of STACK initialization routine
; ----------------------------------------------------------------------

;align 16 ; 19/06/2018

; ----------------------------------------------------------------------
; SYSCONF.ASM - MSDOS 3.3 - 24/07/1987
; ----------------------------------------------------------------------
; 10/06/2018 - Retro DOS v3.0
; 25/03/2018 - Retro DOS v2.0

;	IF	STACKSW
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
;
;	extrn  stack_count:word
;	extrn  stack_size:word
;	extrn  stack_addr:dword
;
;	ENDIF

DOCONF: 		;Take care of Config.sys file.
	
	; 10/06/2018 - Retro DOS v3.0	
	
	PUSH    CS
        POP     DS

	; 13/04/2018 - Retro DOS v2.0
	
	MOV     AX,(CHAR_OPER*256)	; GET SWITCH CHARACTER
	INT     21H
	MOV     [COMMAND_LINE+1],DL

        MOV     DX,CONFIG		; NOW POINTING TO FILE DESCRIPTION
        MOV     AX,(OPEN*256)		; OPEN FILE "CONFIG.SYS"
        STC				; IN CASE OF INT 24
        INT     21H                     ; FUNCTION REQUEST
        ;JC     SHORT ENDCONF
        ;JMP    NOPROB			; PROBLEM WITH OPEN
	JNC	short NOPROB

ENDCONF:
	RETN

BADOP:  
	MOV     DX,BADOPM		; WANT TO PRINT COMMAND ERROR
        CALL    PRINT
        JMP     COFF

NOPROB:
					; GET FILE SIZE (NOTE < 64K!!)
	MOV     BX,AX
        XOR     CX,CX
        XOR     DX,DX
        MOV     AX,(LSEEK*256)+2
        INT     21H
        MOV     [COUNT],AX
        XOR     DX,DX
        MOV     AX,(LSEEK*256)		; Reset pointer to beginning of file
        INT     21H
        
	MOV     DX,CS
        MOV     AX,[COUNT]
        call	ParaRound
        SUB     DX,AX
        SUB     DX,11H                  ; ROOM FOR HEADER
	MOV	[CONFBOT],DX		; Config starts here
	CALL	TEMPCDS 		; Finally get CDS to "safe" location

        MOV	DX,[CS:CONFBOT] ; 19/06/2018
        MOV     DS,DX
        MOV     ES,DX
        XOR     DX,DX
	MOV	CX,[CS:COUNT] ; 19/06/2018
        MOV     AH,READ
        STC                             ; IN CASE OF INT 24
        INT     21H                     ; Function request
        PUSHF
;
; Find the EOF mark in the file. If present, then trim length.
;	
	PUSH	AX
	PUSH	DI
	PUSH	CX
	MOV	AL,1Ah			; eof mark
	MOV	DI,DX			; point ro buffer
	JCXZ	PUTEOL			; no chars
	REPNZ	SCASB			; find end
	JNZ	SHORT PUTEOL		; none found and count exahusted
;
; We found a 1A. Back up
;
	DEC	DI			; backup past 1A
;
;  Just for the halibut, stick in an extra EOL
;
PUTEOL:
	MOV	AL,13
	STOSB				; CR
	MOV	AL,10
	STOSB				; LF
	SUB	DI,DX			; difference moved
	; 19/06/2018
	MOV	[CS:COUNT],DI		; new count
;
; Restore registers
;
	POP	CX
	POP	DI
	POP	AX

        PUSH	CS
        POP	DS

        PUSH    AX
        MOV     AH,CLOSE
        INT     21H
        POP     AX
        POPF
        JC      SHORT CONFERR		; IF NOT WE'VE GOT A PROBLEM
        CMP     CX,AX
        JZ      SHORT GETCOM		; COULDN'T READ THE FILE
CONFERR:
        MOV     DX,CONFIG		; WANT TO PRINT CONFIG ERROR
        CALL    BADFIL
ENDCONV:
	;JMP	ENDCONF
	RETN

GETCOM:
        CALL    ORGANIZE                ; ORGANIZE THE FILE
        CALL    GETCHR

CONFLP: JC      SHORT ENDCONV
        MOV     AH,AL
        CALL    GETCHR
	JNC	SHORT TRYB
	JMP	BADOP

COFF:   
	PUSH    CS
        POP     DS
        CALL    NEWLINE
        JMP     SHORT CONFLP

;------------------------------------------------------------------------------
; Buffer command
;------------------------------------------------------------------------------
TRYB:	
	CMP     AH,'B'                  ; BUFFER COMMAND?
        JNZ     SHORT TRYC
        CALL    GETNUM
        JZ      SHORT TRYBBAD
        CMP     AX,100
        JB	SHORT SAVEBUF
TRYBBAD:
	JMP	BADOP
SAVEBUF:
        MOV     [BUFFERS],AL
COFFJ1:
        JMP     SHORT COFF

;------------------------------------------------------------------------------
; Break command
;------------------------------------------------------------------------------
TRYC:   
	CMP     AH,'C'
        ;JZ	SHORT GOTC
        ;JMP	SHORT TRYD
	JNZ	SHORT TRYD
GOTC:
        CMP     AL,'O'                  ; FIRST LETTER OF "ON" OR "OFF"
        JNZ     SHORT TRYCBAD
        CALL    GETCHR
        JC      SHORT TRYCBAD
        CMP     AL,'N'                  ; SECOND LETTER OF "ON"
        JNZ     SHORT TRYCOFF
        MOV     AH,SET_CTRL_C_TRAPPING  ; TURN ON CONTROL-C CHECK
        MOV     AL,1
        MOV     DL,AL
        INT     21H
COFFJ2:
	JMP	SHORT COFF
TRYCOFF: 
	CMP	AL,'F'
	JNZ	SHORT TRYCBAD		; Check for "OFF"
	CALL	GETCHR
	JC	SHORT TRYCBAD
	CMP	AL,'F'
	JZ	SHORT COFFJ2
TRYCBAD:
	JMP	BADOP

;------------------------------------------------------------------------------
; Device command
;------------------------------------------------------------------------------

; 10/06/2018 - Retro DOS v3.0 (MSDOS 3.3)

TRYD:   
	CMP     AH,'D'
        JZ      SHORT GOTD
        JMP     TRYQ
GOTD:   
	MOV     BX,CS
        MOV     DS,BX

        MOV     [BPB_ADDR],SI
        MOV     [BPB_ADDR+2],ES

        CALL    ROUND

	XOR     AX,AX
        MOV     [ENTRY_POINT],AX
        MOV     AX,[MEMHI]
        MOV     [ENTRY_POINT+2],AX	; SET ENTRY POINT

	; 01/05/2018 (NOEXEC = FALSE)
        ;IF     NOT NOEXEC
        MOV	[LDOFF],AX		; SET LOAD OFFSET
        ;ENDIF

        PUSH    ES
        POP     DS
        MOV     DX,SI                   ; DS:DX POINTS TO FILE NAME

        ;IF	NOEXEC
        ;LES	BX,[CS:MEMLO]
        ;CALL	LDFIL			; LOAD IN THE DEVICE DRIVER
        ;ELSE

; We are going to open the cdevice driver and size it as is done
;  in LDFIL. The reason we must do this is that EXEC does NO checking
;  for us. We must make sure there is room to load the device without
;  trashing SYSINIT. This code is not
;  perfect (for instance .EXE device drivers are possible) because
;  it does its sizing based on the assumption that the file being loaded
;  is a .COM file. It is close enough to correctness to be usable.

	MOV	ES,AX			;ES:0 is LOAD addr
	MOV	AX,OPEN*256		;OPEN THE FILE
	STC				;IN CASE OF INT 24
	INT	21H
	JC	short BADLDRESET
	MOV	BX,AX			;Handle in BX
	PUSH	DX			; Save pointer to name
	XOR	CX,CX
	XOR	DX,DX
	MOV	AX,(LSEEK*256)+2
	STC				;IN CASE OF INT 24
	INT	21H			; Get file size in DX:AX
	JNC	short GO_AHEAD_LOAD
	MOV	AH,CLOSE		; Close file
	INT	21H
	POP	DX			; Clean stack
	STC				; Close may clear carry
	JMP	SHORT BADLDRESET

GO_AHEAD_LOAD:
    ; Convert size in DX:AX to para in AX
	ADD	AX,15			; Round up size for conversion to para
	ADC	DX,0
	MOV	CL,4
	SHR	AX,CL
	MOV	CL,12
	SHL	DX,CL			; Low nibble of DX to high nibble
	OR	AX,DX			; AX is now # of para for file

	MOV	CX,ES			; CX:0 is xaddr
	ADD	CX,AX			; New device will take up to here
	JC	short MEM_ERRJY		; WOW!!!!
	CMP	CX,[CS:ALLOCLIM]
	JB	short OKLDX
MEM_ERRJY:
	JMP	MEM_ERR
OKLDX:
	POP	DX			; Recover name pointer
	MOV	AH,CLOSE		; Close file
	INT	21H
	MOV	BX,CS
        MOV	ES,BX
        MOV	BX,PRMBLK		; ES:BX POINTS TO PARAMETERS
        MOV	AL,3
        MOV	AH,EXEC
        STC				; IN CASE OF INT 24
        INT	21H                     ; LOAD IN THE DEVICE DRIVER
        ;ENDIF

BADLDRESET:
        PUSH    DS
        POP     ES                      ; ES:SI BACK TO CONFIG.SYS
        PUSH    CS
        POP     DS                      ; DS BACK TO SYSINIT
        JNC     SHORT GOODLD
BADBRK: 
	cmp	BYTE [ES:SI], 13	; file name is CR?
	jne	SHORT BADBRK_1		;(entered "device=" without filename)
	jmp	BADOP			;"Unrecognized command in CONFIG.SYS"
BADBRK_1:
	CALL    BADLOAD
        JMP     COFF
GOODLD: 
	PUSH    ES                      ; INITIALIZE THE DEVICE
        PUSH    SI
RESTORE:
	MOV	BL,[ES:SI]		;   while ((c=*p) != 0)
	OR	BL,BL
	JZ	SHORT GOT
	INC	SI			;	p++;
	JMP	SHORT RESTORE
GOT:	
	MOV	BYTE [ES:SI], ' '	;   *p = ' ';

	PUSH    ES
        PUSH    SI        
	PUSH    CS
        POP     ES
        MOV     BX,SYSDEV.STRAT
        CALL    CALLDEV			;   CallDev (SDevStrat);
        MOV     BX,SYSDEV.INT
        CALL    CALLDEV			;   CallDev (SDevInt);
	POP	SI
	POP	DS
	MOV	BYTE [SI],0		;   *p = 0;
		
        PUSH    CS
        POP     DS
	MOV     AX,[BREAK_ADDR+2]	; REMOVE THE INIT CODE
        CMP     AX,[MEMORY_SIZE]
        JB      SHORT BREAKOK
        POP     SI
        POP     ES
        JMP     SHORT BADBRK
BREAKOK:
        LDS     DX,[ENTRY_POINT]        ; SET DS:DX TO HEADER
        MOV     SI,DX
        ADD     SI,SYSDEV.ATT		; DS:SI POINTS TO ATTRIBUTES
        LES     DI,[CS:DOSINFO]         ; ES:DI POINT TO DOS INFO
        MOV     AX,[SI]			; GET ATTRIBUTES
        TEST    AX,DEVTYP               ; TEST IF BLOCK DEV
        JZ      SHORT ISBLOCK
	CALL	SET_BREAK		; Go ahead and alloc mem for device
	JC	SHORT ERASE_DEV
        TEST    AX,ISCIN                ; IS IT A CONSOLE IN?
        JZ      SHORT TRYCLK
        MOV     [ES:DI+SYSI_CON],DX
        MOV     [ES:DI+SYSI_CON+2],DS

TRYCLK: 
	TEST    AX,ISCLOCK              ; IS IT A CLOCK DEVICE?
        JZ      SHORT GOLINK
        MOV     [ES:DI+SYSI_CLOCK],DX
        MOV     [ES:DI+SYSI_CLOCK+2],DS
GOLINK: 
	JMP	LINKIT

ISBLOCK:
        MOV     AL,[CS:UNITCOUNT]	; IF NO UNITS FOUND, ERASE THE DEVICE
        OR      AL,AL
        JNZ     SHORT PERDRV
ERASE_DEV:					
        MOV     AX,-1			; No call to SET_BREAK yet, so no alloc
	JMP	ENDDEV

PERDRV:
        CBW
        MOV     CX,AX
        MOV     DH,AH
        MOV     DL,[ES:DI+SYSI_NUMIO]	; GET NUMBER OF DEVICES
	MOV	AH,DL
	ADD	AH,AL			; Check for too many devices
	CMP	AH,26			; 'A' - 'Z' is 26 devices
	JBE	SHORT OK_BLOCK
	PUSH	CS
	POP	DS
	MOV	DX,BADBLOCK
	CALL	PRINT
	JMP	SHORT ERASE_DEV

OK_BLOCK:
	CALL	SET_BREAK		; Alloc the device
	ADD	[ES:DI+SYSI_NUMIO],AL	; UPDATE THE AMOUNT
	ADD	[CS:DriveNumber],AL	; remember amount for next device
        LDS     BX,[CS:BPB_ADDR]        ; POINT TO BPB ARRAY
PERUNIT:
        LES     BP,[CS:DOSINFO]
	;LES	BP,[ES:BP+SYSI_DPB]	; GET FIRST DPB
	les	bp,[es:bp] ; 05/07/2019
SCANDPB:
	CMP     WORD [ES:BP+DPB.NEXT_DPB],-1
        JZ      SHORT FOUNDPB
        LES     BP,[ES:BP+DPB.NEXT_DPB]
        JMP     SHORT SCANDPB
FOUNDPB:
        MOV     AX,[CS:MEMLO]
        MOV     [ES:BP+DPB.NEXT_DPB],AX
        MOV     AX,[CS:MEMHI]
        MOV     [ES:BP+DPB.NEXT_DPB+2],AX
        LES     BP,[CS:MEMLO]
        ADD     WORD [CS:MEMLO],DPBSIZ
	CALL	ROUND			;Check for alloc error
        MOV     WORD [ES:BP+DPB.NEXT_DPB],-1
        MOV     BYTE [ES:BP+DPB.FIRST_ACCESS],-1

        MOV     SI,[BX]                 ; DS:SI POINTS TO BPB
        INC     BX
        INC     BX                      ; POINT TO NEXT GUY
      	;MOV	[ES:BP+DPB.DRIVE],DX
	mov	[es:bp],dx ; 05/07/2019
        MOV     AH,SETDPB               ; HIDDEN SYSTEM CALL
        INT     21H
        MOV     AX,[ES:BP+DPB.SECTOR_SIZE]
        PUSH    ES
        LES     DI,[CS:DOSINFO]		; ES:DI POINT TO DOS INFO
        CMP     AX,[ES:DI+SYSI_MAXSEC]
        POP     ES
        JBE     SHORT NOTMAX
        POP     SI
        POP     ES
        MOV     DX,BADSIZ_PRE
        MOV     BX,BADSIZ_POST
        CALL    PRNERR
        JMP	COFF

NOTMAX: 
	PUSH    DS
        PUSH    DX
        LDS     DX,[CS:ENTRY_POINT]
        MOV     [ES:BP+DPB.DRIVER_ADDR],DX
        MOV     [ES:BP+DPB.DRIVER_ADDR+2],DS
        POP     DX
        POP     DS
        INC     DX
        INC     DH
        LOOP    PERUNIT
	PUSH	CS
	POP	DS
	CALL	TEMPCDS 		; Set CDS for new drives

LINKIT:
        LES     DI,[CS:DOSINFO]         ; ES:DI = DOS TABLE
        MOV     CX,[ES:DI+SYSI_DEV] 	; DX:CX = HEAD OF LIST
        MOV     DX,[ES:DI+SYSI_DEV+2]

        LDS     SI,[CS:ENTRY_POINT]     ; DS:SI = DEVICE LOCATION
        MOV     [ES:DI+SYSI_DEV],SI 	; SET HEAD OF LIST IN DOS
        MOV     [ES:DI+SYSI_DEV+2],DS
        MOV     AX,[SI]			; GET POINTER TO NEXT DEVICE
        MOV     [CS:ENTRY_POINT],AX	; AND SAVE IT

        MOV    [SI],CX			; LINK IN THE DRIVER
        MOV    [SI+2],DX
ENDDEV:
        POP     SI
        POP     ES
        INC     AX                      ;AX = FFFF (no more devs if YES)?
        JZ      SHORT COFFJ3
        JMP	GOODLD			; OTHERWISE PRETEND WE LOADED IT IN
COFFJ3:
COFFV:  
	JMP	COFF

;------------------------------------------------------------------------------
; Country command
;  The syntax is:							  ;3.30
;	COUNTRY=country id {,codepage {,path}}				  ;3.30
;	COUNTRY=country id {,,path}	:Default CODEPAGE ID in DOS	  ;3.30
;------------------------------------------------------------------------------
; SYSCONF.ASM, MSDOS 3.3, 24/07/1987

TRYQ:
	; 10/06/2018 - Retro DOS v3.0
 
        CMP     AH,'Q'
        ;JNZ	SHORT TRYF
	JZ	short TRYQ_CONT
	JMP	TRYF
TRYQ_CONT:
        CALL    GETNUM
        JZ	short TryQBad		; 0 is never a valid code, or number is
					; bad
	MOV	BX,AX			; Country code in BX
					; 5/26/86
	MOV	DX,0			; assume no code page id
	call	SKIP_DELIM		;skip the delimeters after the first num
	jc	short TryQ_Def_File	;no more characters left? then use default file
	cmp	al,13			;
	je	short TryQ_Def_File
	cmp	al,10
	jne	short TRYQ_YES_EXTENDED
	inc	word [COUNT] 		;This is for NEWLINE routine in COFF.
	dec	word [CHRPTR]
COFFJ41:
	JMP	short TryQ_Def_File	;O.K. no code page, 
					; no path specified.Use default path.    
TRYQ_YES_EXTENDED:
	cmp	al,','			;was the second comma?
	jne	short TRYQ_GETNUM
	call	SKIP_DELIM		;Yes, skip ',' and other possible delim
	jmp	short TRYQ_PATH 	;and No code page id entered.
TRYQ_GETNUM:
	call	GETNUM
	jc	short TryQBadCOM	;"Country=xxx,path" will not be accepted.
;	jc	short TRYQ_PATH		;Codepage is not specified. No code page.
;					;At this point, AL already contain the
;					;first char of the PATH.
	jz	short TryQBad 		;codepage=0 entered. Error
	mov	DX,AX			;save code page in DX
	call	SKIP_DELIM		;move CHRPTR to the path string
	jc	short TryQ_Def_File	;no more char? then use default filename
	cmp	al, 13
	je	short TryQ_Def_File
	cmp	al, 10	
	jne	short TRYQ_PATH		;path entered.
	inc	word [COUNT]
	dec	word [CHRPTR]
TryQ_Def_File:
	push	dx			;save code page
	mov	byte [cs:CNTRY_DRV],0 	;flag that the default path has been used!!!
	mov	dx,CNTRY_ROOT		;the default path
	jmp	short TRYQ_OPEN

TryQBad:				;"Invalid country code or code page"
	STC
	MOV	DX,BADCOUNTRY
	jmp	TryQChkErr

TryQBadCOM:				;Error in COUNTRY command
	STC
	MOV	DX,BADCOUNTRYCOM
	jmp	TryQChkErr

TRYQ_PATH:				;DS - sysinitseg, ES - CONFBOT,
	mov	CX,[COUNT]		;AL - the first char of path
	inc	CX			;BX - country id, DX - codepage id, 0 = No code page
	mov	DI,SI
TRYQ_PATH_LOOP: 			;find the end of path to put 0 after that.
	mov	AL,[ES:DI]
	call	DELIM
	jz	short TRYQ_PATH_END
	cmp	al, 13
	jz	short TRYQ_PATH_END
	inc	DI
	jmp	short TRYQ_PATH_LOOP
TryQBad_Brg:
	jmp	short TryQBad
TRYQ_PATH_END:
	mov	byte [es:di],0		;make it a ASCIIZ string. 
					; (Organize did not handle this string)
	push	ds			;switch ds,es
	push	es
	pop	ds
	pop	es

	mov	di,CNTRY_DRV	;move the user specified path to CNTRY_DRV
	call	MOVE_ASCIIZ

	push	ds			;restore ds,es
	push	es
	pop	ds
	pop	es

;	call	 Set_Country_Path	;set CNTRY_DRV

	push	dx			;save DX
	mov	dx,CNTRY_DRV		;Now DS:DX -> CNTRY_DRV
TRYQ_OPEN:
	mov	ax,3D00h		;open a file
	stc
	int	21h
	pop	dx			;restore codepage id
	jc	short TryQFileBad	;open failure

	mov	[cs:CntryFileHandle],ax ;save file handle
	xchg	ax,bx			;now, AX = country id, BX = file handle
	mov	cx,[cs:MEMHI]
	add	cx,128			;I need 2K buffer to handle COUNTRY.SYS
	cmp	cx,[cs:ALLOCLIM]
	ja	short TryQMemory	;cannot allocate the buffer for country.sys

	mov	si,CNTRY_DRV		;DS:SI -> CNTRY_DRV
	cmp	byte [si],0 		;default path?
	jne	short TRYQ_Set_for_DOS
	inc	si
	inc	si			;DS:SI -> CNTRY_ROOT
TRYQ_Set_for_DOS:
	les	di,[cs:SYSI_Country]	;ES:DI -> country info tab in DOS
	push	di			;save di
	add	di,country_cdpg_info.ccPath_CountrySys
	call	MOVE_ASCIIZ		;Set the path to COUNTRY.SYS in DOS.
	pop	di			;ES:DI -> country info tab again.
	mov	cx,[cs:MEMHI]
	mov	ds, cx
	xor	si, si			;DS:SI -> 2K buffer to be used.
	call	SetDOSCountryInfo	;now do the job!!!
	jnc	short TryQChkErr	;read error or could not find country,
					;	code page combination
	cmp	cx, -1			;Could not find matching country_id,code page?
	je	short TryQBad_Brg	;then "Invalid country code or code page"
TryQFileBad:
	cmp	byte [cs:CNTRY_DRV],0	;Is the default file used?
	je	short TryQDefBad
	mov	si,[cs:CONFBOT]
	mov	es,si
	mov	si,[cs:CHRPTR]
	dec	si			;ES:SI -> path in CONFBOT
	jmp	short TryQBADLOAD
TryQDefBad:				;Default file has been used.
	push	cs
	pop	es
	mov	si,CNTRY_ROOT		;ES:SI -> \COUNTRY.SYS in SYSINIT_SEG
TryQBADLOAD:
	call	BADLOAD 		;DS will be restored to SYSINIT_SEG
	mov	cx,[cs:CONFBOT]
	mov	es, cx			;Restore ES -> CONFBOT.
	jmp	short CoffJ4
TryQMemory:
	MOV	DX,INSUFMEMORY
TryQChkErr:
	mov	cx,[cs:CONFBOT]
	mov	es,cx			;restore ES -> CONFBOT seg
	push	cs
	pop	ds			;restore DS to SYSINIT_SEG
	jnc	short CoffJ4		;if no error, then exit
	call	PRINT			;else show error message
CoffJ4:
	mov	bx,[CntryFileHandle]
	mov	ah,3Eh
	int	21h			;close a file. Don't care even if it fails.
	JMP	COFF

;------------------------------------------------------------------------------
; Files command
;------------------------------------------------------------------------------
; SYSCONF.ASM, MSDOS 3.3, 24/07/1987

TRYF:
        CMP     AH,'F'
	JNZ	SHORT TRYL
	CALL    GETNUM
	CMP	AX,5
	JB	SHORT TRYFBAD		; Gotta have at least 5
	CMP	AX,256
	JAE	SHORT TRYFBAD		; Has to be a byte
	MOV	[FILES],AL
COFFJ5: 
	JMP	COFF
TRYFBAD:
	JMP	BADOP

;------------------------------------------------------------------------------
; LastDrive command
;------------------------------------------------------------------------------
; SYSCONF.ASM, MSDOS 3.3, 24/07/1987

TRYL:
	CMP	AH,'L'
	JNZ	SHORT TRYP
	OR	AL,020h
	SUB	AL,'a'
	JB	SHORT TRYLBAD
	INC	AL
	CMP	AL,26			; a-z are allowed
	JA	SHORT TRYLBAD
	MOV	[NUM_CDS],AL
COFFJ6: 
	JMP	COFF
TRYLBAD:
	JMP	BADOP

;-------------------------------------------------------------------------------
; Setting Drive Parameters
;-------------------------------------------------------------------------------
; SYSCONF.ASM, MSDOS 3.3, 24/07/1987

TRYP:
	CMP	AH,'P'
	JNZ	SHORT TRYK
	CALL	PARSELINE
	JC	SHORT TRYLBAD
	CALL	SETPARMS
	CALL	DIDDLEBACK
	JC	SHORT TRYLBAD
	JMP	COFF

;-------------------------------------------------------------------------------
; Setting Internal Stack Parameters
; STACK=M,N where
;	M is the number of stacks (range 8 to 64, default 9)
;	N is the stack size (range 32 to 512 bytes, default 128)
;  5/5/86: STACKS=0,0 implies no stack installation.			  ;3.30
;	Any combinations that are not within the specified limits will	  ;3.30
;	result in "Unrecognized command" error.                    
;-------------------------------------------------------------------------------
; SYSCONF.ASM, MSDOS 3.3, 24/07/1987

TRYK:
	CMP	AH,'K'
	;JNZ	short TRYW
	JNZ	short TRYS ; 03/08/2019

	;IF	STACKSW

	MOV	byte [SEPCHR],','
	call	GETNUM			; Get number of stacks
	MOV	byte [SEPCHR],0
	cmp	ax, 0			; 5/5/86
	je	short TRYK_0		; Let's accept 0.
	CMP	AX,MinCount		; 8 <= Number of Stacks <= 64
	JB	short TryKBad
	CMP	AX,MaxCount
	JA	short TryKBad
TRYK_0:
	MOV	[STACK_COUNT],AX
;
; Skip delimiters after the ,
;
	call	SKIP_DELIM
	JC	short TryKBad

	call	GETNUM			; Get size of individual stack
	JC	short TryKBad 		; Number bad

	cmp	ax, 0			; 5/5/86
	je	short TRYK_SIZE0	; 5/5/86. Accept 0

	CMP	AX,MinSize		; 32 <= Stack Size <= 512
	JB	short TryKBad
	CMP	AX,MaxSize
	JA	short TryKBad
TRYK_SIZE0:
	MOV	[STACK_SIZE],AX
	cmp	ax,0
	je	short TRYK_BOTH0
TRYK_OK:
	mov	word [STACK_ADDR], -1	;set flag. user entered stacks=
	JMP	COFF
TRYK_BOTH0:
	cmp	word [STACK_COUNT],0 	;stack_size=0. Stack_Count=0 too?
	je	short TRYK_OK 		;yes. accepted.
TryKBad:
	MOV	DX,BADSTACK		; 5/26/86 "Invalid stack parameter"
	call	PRINT
	JMP	COFF

	;ENDIF

;------------------------------------------------------------------------------
; Switch command
;------------------------------------------------------------------------------

	; 03/08/2019
;TRYW:
;	CMP     AH,'W'
;	JNZ     SHORT TRYA
;	; 10/06/2018 - Retro DOS 3.0
;	JMP	BADOP			; no longer implemented (MSDOS 3.3)

	; 01/04/2018 - Retro DOS 2.0
;	MOV     DL,AL
;	MOV     AX,(CHAR_OPER*256)+1	; SET SWITCH CHARACTER
;	MOV     [COMMAND_LINE+1],DL
;	INT     21H
;	JMP     COFF

;------------------------------------------------------------------------------
; Availdev command
;------------------------------------------------------------------------------

	; 03/08/2019
;TRYA:
;	CMP     AH,'A'
;	JNZ     SHORT TRYS
;	JMP	BADOP			; no longer implemented (MSDOS 3.3)

;	CMP     AL,'F'			; FIRST LETTER OF "FALSE"
;	JNZ     SHORT COFFJ
;	MOV     AX,(CHAR_OPER*256)+3	; TURN ON "/DEV" PREFIX
;	XOR     DL,DL
;	INT     21H
;COFFJ7:  
;	JMP     COFF

;------------------------------------------------------------------------------
; shell command
;------------------------------------------------------------------------------

TRYS:
        CMP     AH,'S'
        JNZ     SHORT TRYX
        MOV     BYTE [COMMAND_LINE+1],0
        MOV     DI,COMMND + 1
        MOV     [DI-1],AL
STORESHELL:
        CALL    GETCHR
        OR      AL,AL
        JZ      SHORT GETSHPARMS
        CMP     AL," "
        JB      SHORT ENDSH
        MOV     [DI],AL
        INC     DI
        JMP     SHORT STORESHELL

ENDSH:
        MOV     BYTE [DI],0
        CALL    GETCHR
        CMP     AL,10
        JNZ     SHORT CONV
        CALL    GETCHR
CONV:   
	JMP     CONFLP

;------------------------------------------------------------------------------
; FCBS Command
;------------------------------------------------------------------------------
; SYSCONF.ASM, MSDOS 3.3, 24/07/1987

TRYX:
	; 10/06/2018 -  Retro DOS v3.0
 
	;JMP	BADOP	; 25/03/2018 - Retro DOS v2.0 

	CMP	AH,'X'
	JNZ	SHORT TRYZ
	CALL	GETNUM
	JZ	SHORT TRYXBAD 		; gotta have at least one
	CMP	AX,256
	JAE	SHORT TRYXBAD 		; Can't be more than 8 bits worth
	MOV	[FCBS],AL
;
; Skip delimiters after the ,
;
	CALL	SKIP_DELIM
	JC	SHORT TRYXBAD
	CALL	GETNUM
	JC	SHORT TRYXBAD		; Number bad (Zero is OK here)
	CMP	AX,256
	JAE	SHORT TRYXBAD
	CMP	AL,[FCBS]
	JA	SHORT TRYXBAD
	MOV	[KEEP],AL
	JMP	COFF
TRYXBAD:
	JMP	BADOP

;------------------------------------------------------------------------------
; Bogus command
;------------------------------------------------------------------------------

TRYZ:
	JMP	BADOP

GETSHPARMS:
	MOV     BYTE [DI],0
	MOV     DI,COMMAND_LINE+1
PARMLOOP:
        CALL    GETCHR
        CMP     AL," "
        JB      SHORT ENDSH
        MOV     [DI],AL
        INC     DI
        JMP     SHORT PARMLOOP

GETCHR: 
	PUSH	CX
	MOV     CX,[COUNT] ; 05/07/2018
        JCXZ    NOCHAR
        MOV     SI,[CHRPTR]
        MOV     AL,[ES:SI]
        DEC     WORD [COUNT]
        INC     WORD [CHRPTR]
        CLC
GET_RET:
	POP	CX
        RETN
NOCHAR: 
	STC
        JMP	SHORT GET_RET

; ----------------------------------------------------------------------
; SYINIT2.ASM (MSDOS 3.3, 02/02/1988)
; ----------------------------------------------------------------------
; 10/06/2018 - Retro DOS v3.0

;
; The following set of routines is used to parse the DRIVPARM = command in
; the CONFIG.SYS file to change the default drive parameters.
;
SETPARMS:
	push	ds
	push	ax
	push	bx
	push	cx
	push	dx
	xor	bx,bx
	mov	bl,[DRIVE]
	inc	bl		; get it correct for IOCTL call (1=A,2=B...)
	push	cs
	pop	ds
	mov	dx,DEVICEPARAMETERS
	mov	ah,IOCTL
	mov	al,GENERIC_IOCTL
	mov	ch,RAWIO
	mov	cl,SET_DEVICE_PARAMETERS
	int	21H
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	ds
	retn

;
; Replace default values for further DRIVPARM commands
;
DIDDLEBACK:
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_CYLINDERS],80
	mov	byte [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_DEVICETYPE],DEV_3INCH720KB
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES],0
	mov	word [SWITCHES],0	    ; zero all switches
	retn

;
; Entry point is ParseLine. AL contains the first character in command line.
;
PARSELINE:			; don't get character first time
	push	ds
	push	cs
	pop	ds
NextSwtch:
	cmp	al,13			; carriage return?
	jz	short done_line
	cmp	al,10			; linefeed?
	jz	short put_back		; put it back and done
; Anything less or equal to a space is ignored.
	cmp	al,' '                  ; space?
	jbe	short get_next		; skip over space
	cmp	al,'/'
	jz	short getparm
	stc				; mark error invalid-character-in-input
	jmp	short exitpl

getparm:
	call	Check_Switch
	mov	[SWITCHES],BX		; save switches read so far
	jc	short swterr
get_next:
	call	GETCHR
	jc	short done_line
	jmp	short NextSwtch
swterr:
	jmp	short exitpl		; exit if error

done_line:
	test	word [SWITCHES],flagdrive  ; see if drive specified
	jnz	short okay
	stc				; mark error no-drive-specified
	jmp	short exitpl

okay:
	mov	ax,[SWITCHES]
	and	ax,0003H	    ; get flag bits for changeline and non-rem
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_DEVICEATTRIBUTES],ax
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_TRACKTABLEENTRIES],0
	clc			    ; everything is fine
	call	SetDeviceParameters
exitpl:
	pop	ds
	retn

put_back:
	inc	word [COUNT]		; one more char to scan
	dec	word [CHRPTR]		; back up over linefeed
	jmp	short done_line

;
; Processes a switch in the input. It ensures that the switch is valid, and
; gets the number, if any required, following the switch. The switch and the
; number *must* be separated by a colon. Carry is set if there is any kind of
; error.
;
Check_Switch:
	call	GETCHR
	jc	short err_check
	and	al,0DFH			; convert it to upper case
	cmp	al,'A'
	jb	short err_check
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
	shl	ax,cl		; set bit to indicate switch
	mov	bx,[SWITCHES]	 ; get switches so far
	or	bx,ax		; save this with other switches
	mov	cx,ax
	test	ax,7CH		; test against switches that require number to follow
	jz	short done_swtch
	call	GETCHR
	jc	short err_swtch
	cmp	al,':'
	jnz	short err_swtch
	call	GETCHR
	push	bx			; preserve switches
	mov	byte [cs:SEPCHR],' '	; allow space separators
	call	GETNUM
	mov	byte [cs:SEPCHR],0
	pop	bx			; restore switches
; Because GetNum does not consider carriage-return or line-feed as OK, we do
; not check for carry set here. If there is an error, it will be detected
; further on (hopefully).
	call	Process_Num

done_swtch:
	clc
	retn

err_swtch:
	xor	bx,cx			; remove this switch from the records
err_check:
	stc
	retn

;
; This routine takes the switch just input, and the number following (if any),
; and sets the value in the appropriate variable. If the number input is zero
; then it does nothing - it assumes the default value that is present in the
; variable at the beginning. Zero is OK for form factor and drive, however.
;
Process_Num:
	test	[SWITCHES],cx		; if this switch has been done before,
	jnz	short done_ret		; ignore this one.
	test	cx,flagdrive
	jz	short try_f
	mov	[DRIVE],al
	jmp	short done_ret

try_f:
	test	cx,flagff
	jz	short try_t
; Ensure that we do not get bogus form factors that are not supported
	;cmp	al,Max_Dev_Type
	;ja	short done_ret
	mov	byte [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_DEVICETYPE],al
	jmp	short done_ret

try_t:
	or	ax,ax
	jz	short done_ret	 ; if number entered was 0, assume default value
	test	cx,flagcyln
	jz	short try_s
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_CYLINDERS],ax
	jmp	short done_ret

try_s:
	test	cx,flagseclim
	jz	short try_h
	mov	[SLIM],ax
	jmp	short done_ret
;
; Must be for number of heads
try_h:
	mov	[HLIM],ax

done_ret:
	clc
	retn

; 05/07/2018 - Retro DOS v3.0

;
; SetDeviceParameters sets up the recommended BPB in each BDS in the
; system based on the form factor. It is assumed that the BPBs for the
; various form factors are present in the BPBTable. For hard files,
; the Recommended BPB is the same as the BPB on the drive.
; No attempt is made to preserve registers since we are going to jump to
; SYSINIT straight after this routine.
;
SetDeviceParameters:
	; 06/07/2018 - Retro DOS v3.0
	push	es
	push	cs
	pop	es
;ASSUME ES:SYSINITSEG
	xor	bx,bx
	mov	bl,[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_DEVICETYPE]
	cmp	bl,DEV_5INCH
	jnz	short Got_80
	mov	cx,40			; 48tpi has 40 cylinders
	mov	[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_CYLINDERS],cx
Got_80:
	shl	bx,1			; get index into BPB table
	; 06/07/2018
	;mov	si,BPBTable
	add	bx,BPBTable
	;mov	si,[si+bx]		; get address of BPB
	mov	si,[bx]
Set_RecBPB:
	mov	di,DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB ; es:di -> BPB
	; 06/07/2018
	;mov	cx,A_BPB.size ; = 31
	mov	cx,BPBSIZ ; = 25 
	cld
	repe	movsb
	pop	es
;ASSUME ES:NOTHING
	test	word [SWITCHES],flagseclim
	jz	short see_heads
	mov	ax,[SLIM]
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERTRACK],ax
see_heads:
	test	word [SWITCHES],flagheads
	jz	short Set_All_Done
	mov	ax,[HLIM]
	mov	word [DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_HEADS],ax
;
; We need to set the media byte and the total number of sectors to reflect the
; number of heads. We do this by multiplying the number of heads by the number
; of 'sectors per head'. This is not a fool-proof scheme!!
;
	mov	cx,ax			; cx has number of heads
	dec	cl			; get it 0-based
	mov	ax,[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_TOTALSECTORS]
					; this is OK for two heads
	sar	ax,1			; ax contains # of sectors/head
	sal	ax,cl
	jc	short Set_All_Done	; We have too many sectors - overflow!!
	mov	[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_TOTALSECTORS],ax
; Set up correct Media Descriptor Byte
	cmp	cl,1
	mov	bl,0F0H
	mov	al,2			; AL contains sectors/cluster
	ja	short Got_Correct_Mediad
	mov	bl,[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_MEDIADESCRIPTOR]
	je	short Got_Correct_Mediad
; We have one head - OK for 48tpi medium
	mov	al,1			; AL contains sectors/cluster
	mov	ch,[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_DEVICETYPE]
	cmp	ch,DEV_5INCH
	jz	short Dec_Mediad
	mov	bl,0F0H
	jmp	short Got_Correct_Mediad
Dec_Mediad:
	dec	bl			; adjust for one head
Got_Correct_Mediad:
	mov	[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_MEDIADESCRIPTOR],bl
	mov	[DEVICEPARAMETERS+A_DEVICEPARAMETERS.DP_BPB+A_BPB.BPB_SECTORSPERCLUSTER],al
	clc
Set_All_Done:
	RETN

;ASSUME DS:NOTHING, ES:NOTHING

NOCHAR1: STC
	 retn

; ---------------------------------------

ORGANIZE:
	;mov	cx,[cs:COUNT] ; 05/07/2018
        MOV     CX,[COUNT]
        JCXZ    NOCHAR1
        CALL    MAPCASE
        XOR     SI,SI
        MOV     DI,SI

ORG1:   
	CALL    GET                     ; SKIP LEADING CONTROL CHARACTERS
        CMP     AL,' '
        JB      SHORT ORG1

        PUSH    CX
        PUSH    SI
        PUSH    DI
        MOV     BP,SI
        DEC     BP
        MOV     SI,COMTAB		; Prepare to search command table
        MOV     CH,0
FINDCOM:
        MOV     DI,BP
        MOV     CL,[SI]
        INC     SI
        JCXZ    NOCOM
        REPE    CMPSB
        LAHF
        ADD     SI,CX                   ; Bump to next position without affecting flags
        SAHF
        LODSB                           ; Get indicator letter
        JNZ     SHORT FINDCOM
        POP     DI
        POP     SI
        POP     CX
        JMP     SHORT GOTCOM

NOCOM:
        POP     DI
        POP     SI
        POP     CX
        MOV     AL,'Z'
GOTCOM: 
	STOSB                           ; SAVE INDICATOR CHAR IN BUFFER

ORG2:   
	CALL    GET2                    ; SKIP NAME UNTIL DELIMETER
        CALL    DELIM                   ;
        JNZ     SHORT ORG2

; 10/06/2018 - Retro DOS v3.0 (MSDOS 3.3, SYSINIT2.ASM, 02/02/2018)
;--------------------------------------------------------------bug330a03
; - isp
;* the following two lines in the parsing caused the drivparm line to break
;* we cannot let the "/" character be counted as a delimiter here. 
;	CALL	GET			;GET CHARS TO RIGHT OF EQUALS SIGN
;	STOSB
;--------------------------------------------------------------bug330a03

;--------------------------------------------------------------bug330a03
; - isp
;* the following lines replaced the lines taken out
ORG3:	CALL	GET2
	call	DELIM1
	jz	short ORG3
	cmp	al,'/'
	jz	short ORG_EXT
	stosb
;--------------------------------------------------------------bug330a03

ORG4:   
	CALL    GET2
	CALL	DELIM ; 26/03/2018
	JZ	SHORT ORG_EXT
        STOSB
        CMP     AL,' '
        JA      SHORT ORG4
        CMP     AL,10
        JZ      SHORT ORG1

        MOV     BYTE [ES:DI-1],0
ORG5:   
	CALL    GET2
        STOSB
        CMP     AL,10
        JNZ     SHORT ORG5
        JMP     SHORT ORG1

ORG_EXT: ; 26/03/2018
	MOV	BYTE [ES:DI], 0		;put 0 at DI to make it an ASCIIZ
	INC	DI
	STOSB				;and copy the delimeter char.
	JMP	SHORT ORG5		;and continue as usual.

GET2:
        JCXZ    NOGET
        MOV     AL,[ES:SI]
        INC     SI
        DEC     CX
        RETN

GET:    
	JCXZ    NOGET
        MOV     AL,[ES:SI]
        INC     SI
        DEC     CX
        CALL    DELIM
        JZ      SHORT GET
GRET:   
	RETN

; 10/06/2018 - Retro DOS v3.0 (MSDOS 3.3, SYSINIT2.ASM, 02/02/2018)
;--------------------------------------------------------------bug330a03
; isp - small changes here, new entry point into routine
DELIM:	
	CMP	AL,'/'          ; 5/30/86. IBM will assume "/" delim  ;3.30
	jz	short GRET					      ;3.30
DELIM1:
	CMP	AL,' '
	jz	short GRET
	CMP	AL,9
	jz	short GRET
	CMP	AL,'='
	jz	short GRET
	CMP	AL,','
	jz	short GRET
	CMP	AL,';'
	jz	short GRET	; 5/23/86			      ;3.30
	cmp	al, 0		; 5/23/86 Special case for sysinit!!! ;3.30
	retn

;--------------------------------------------------------------bug330a03

NOGET:  
	POP     CX
	; 20/06/2018
        ;MOV	[cs:COUNT],DI
        MOV     [COUNT],DI ; 05/07/2018
        XOR     SI,SI
        ;MOV	[cs:CHRPTR],SI
        MOV	[CHRPTR],SI ; 05/07/2018
        RETN

;
;  NEWLINE RETURNS WITH FIRST CHARACTER OF NEXT LINE
;
NEWLINE:
	CALL    GETCHR                  ; SKIP NON-CONTROL CHARACTERS
        JC      SHORT NONEW
        CMP     AL,10                   ; LOOK FOR LINE FEED
        JNZ     SHORT NEWLINE
        CALL    GETCHR
NONEW:  
	RETN

MAPCASE:
        PUSH    CX
        PUSH    SI
        PUSH    DS
        PUSH    ES
        POP     DS
        XOR     SI,SI
CONVLOOP:
        LODSB

        ;IF      KANJI
        ;CALL    TESTKANJ
        ;JZ      SHORT NORMCONV
        ;INC     SI			; Skip next char
        ;DEC     CX
        ;JCXZ    CONVDONE		; Just ignore 1/2 kanji error
; Fall through, know AL is not in 'a'-'z' range
;NORMCONV:
        ;ENDIF

        CMP     AL,'a'
        JB      SHORT NOCONV
        CMP     AL,'z'
        JA      SHORT NOCONV
        SUB     AL,20H
        MOV     [SI-1],AL
NOCONV:
        LOOP    CONVLOOP
CONVDONE:
        POP     DS
        POP     SI
        POP     CX
        RETN

        ;IF      KANJI
;TESTKANJ:
        ;CMP     AL,81H
        ;JB      SHORT NOTLEAD
        ;CMP     AL,9FH
        ;JBE     SHORT ISLEAD
        ;CMP     AL,0E0H
        ;JB      SHORT NOTLEAD
        ;CMP     AL,0FCH
        ;JBE    SHORT  ISLEAD
;NOTLEAD:
        ;PUSH    AX
        ;XOR     AX,AX			; Set zero
        ;POP     AX
        ;RETN
;ISLEAD:
        ;PUSH    AX
        ;XOR     AX,AX			; Set zero
        ;INC     AX			; Reset zero
        ;POP     AX
        ;RETN
        ;ENDIF

Yes_Break_Failed:			;dev drv Init failed and aborted.
	stc
	pop	ax
	retn

SET_BREAK:
	; 22/06/2018
	; 10/06/2018 - Retro DOS v3.0

; 8/14/86  For DOS 3.3, this routine is modified to take care of the
;Device driver's initialization error and abort.
;If [break_addr+2] == [memhi] && [break_addr] = 0 then assume
;that the device driver's initialization has an error and wanted to
;abort the device driver.  In this case, this routine will set carry
;and return to the caller.
;
	PUSH	AX
	MOV	AX,[CS:BREAK_ADDR+2]	;REMOVE THE INIT CODE
	cmp	ax,[CS:MEMHI]
	jne	short Set_Break_Continue  ;if not same, then O.K.

	cmp	word [CS:BREAK_ADDR],0 
	je	SHORT Yes_Break_Failed	;[Break_addr+2]=[MEMHI] & [Break_addr]=0 

Set_Break_Continue:
	MOV	[CS:MEMHI],AX
	MOV	AX,[CS:BREAK_ADDR]
	MOV	[CS:MEMLO],AX
	POP	AX			    ; NOTE FALL THROUGH

;
; Round the values in MEMLO and MEMHI to paragraph boundary.
; Perform bounds check.
;

ROUND: 
	; 22/06/2018 
	; 10/06/2018 - Retro DOS v3.0
	; 26/03/2018
	PUSH	AX
	MOV     AX,[CS:MEMLO]
	CALL	ParaRound
        ADD     [CS:MEMHI],AX
        ;XOR     AX,AX
        ;MOV     [CS:MEMLO],AX
	; 06/07/2018
	mov	word [CS:MEMLO], 0
	MOV	AX,[CS:MEMHI]		; ax = new memhi
	CMP	AX,[CS:ALLOCLIM]	; if new memhi >= alloclim, error
	JAE	SHORT MEM_ERR
	POP	AX
	CLC
        RETN

MEM_ERR:
	MOV	DX,BADMEM
	PUSH	CS
	POP	DS
	CALL	PRINT
	JMP	STALL

CALLDEV:
	MOV     DS,[CS:ENTRY_POINT+2]
        ADD     BX,[CS:ENTRY_POINT]	; Do a little relocation
        MOV     AX,[BX]
        PUSH    WORD [CS:ENTRY_POINT]
        MOV     [CS:ENTRY_POINT],AX
        MOV     BX,PACKET
        CALL    FAR [CS:ENTRY_POINT]	; 10/06/2018
        POP     WORD [CS:ENTRY_POINT]
        RETN

BADNUM:
	; 10/06/2018 - Retro DOS v3.0
	mov	byte [CS:SEPCHR],0 ; 06/07/2018
	XOR	AX,AX		; Set Zero flag, and AX = 0
	pop	bx
	stc			; AND carry set
	retn

TODIGIT:
        SUB     AL,'0'
        JB      SHORT NOTDIG
        CMP     AL,9
        JA      SHORT NOTDIG
        CLC
        RETN
NOTDIG: 
	STC
        RETN

; GetNum parses a decimal number.
; Returns it in AX, sets zero flag if AX = 0 (MAY BE considered an
; error), if number is BAD carry is set, zero is set, AX=0.

GETNUM: 
	push	bx	; 10/06/2018 (Retro DOS 3.0, MSDOS 3.3)
	XOR     BX,BX                   ; running count is zero
B2:     
	CALL    TODIGIT                 ; do we have a digit
        JC      SHORT BADNUM		; no, bomb
        XCHG    AX,BX                   ; put total in AX
        PUSH    BX                      ; save digit
        MOV     BX,10                   ; base of arithmetic
        MUL     BX                      ; shift by one decimal digit
        POP     BX                      ; get back digit
        ADD     AL,BL                   ; get total
        ADC     AH,0                    ; make that 16 bits
        JC      SHORT BADNUM		; too big a number
        XCHG    AX,BX                   ; stash total
        CALL    GETCHR                  ; GET NEXT DIGIT
        JC      SHORT B1		; no more characters

	; 10/06/2018
	cmp	al,' '			; 5/23/86 space? 
	jz	short B15		; 5/23/86 then end of digits
	cmp	al,','			; 5/23/86 ',' is a seperator!
	jz	short B15		; 5/23/86 then end of digits.
	cmp	al,9			; 5/23/86 TAB
	jz	short B15		;
	; 22/06/2018
	CMP	AL,[CS:SEPCHR]		; allow , separators
	JZ	short B15
	cmp	al,SWTCHR		; See if another switch follows
	JZ	short B15
	cmp	al,10			; Line-feed?
	jz	short B15
	cmp	al,13			; Carriage return?
	jz	short B15
	
	OR      AL,AL                   ; end of line separator?
        JNZ     SHORT B2		; no, try as a valid character
B15:        
	INC     WORD [CS:COUNT]		; one more character to scan
        DEC     WORD [CS:CHRPTR]	; back up over separator
B1:     
	MOV     AX,BX                   ; get proper count
        OR      AX,AX
	pop	bx	; 10/06/2018
        RETN

SKIP_DELIM:
;Skip the delimeters pointed by CHRPTR. AL will contain the first non delimeter
;character encountered and CHRPTR will point to the next character.
;This routine will assume the second "," found as a non delimiter character.
;So, in case if the string is " , , ", this routine will stop at the second ",".
;At this time, Zero flag is set.
;If COUNT is exhausted, then carry will be set.
Skip_delim_char:
	call	GETCHR
	jc	short Skip_delim_exit
	cmp	al,','			;the first comma?
	je	short Skip_delim_next
	call	DELIM			;check the charater in AL.
	jz	short Skip_delim_char
	; 06/07/2018
	;jmp	short Skip_delim_exit	;found a non delim char
	retn
Skip_delim_next:
	call	GETCHR
	jc	short Skip_delim_exit
	cmp	al,','			;the second comma?
	je	short Skip_delim_exit 	;done
	call	DELIM
	jz	short Skip_delim_next
Skip_delim_exit:
	retn

; 10/06/2018 (Retro DOS 3.0, MSDOS 3.3)

; 5/26/86 *****************************************************************
SetDOSCountryInfo:
;Input: ES:DI -> pointer to DOS_COUNTRY_CDPG_INFO			 
;	DS:0  -> buffer.						 
;	SI = 0								 
;	AX = country id 						 
;	DX = code page id. (If 0, then use ccSysCodePage as a default.) 
;	BX = file handle						 
;	This routine can handle maxium 72 COUNTRY_DATA entries. 	 
;Output: DOS_country_cdpg_info set.					 
;	 Carry set if any file read failure or wrong information in the file.
;	 Carry set and CX = -1 if cannot find the matching COUNTRY_id, CODEPAGE
;	 _id in the file.						 
									 
	push	di							 
	push	ax							 
	push	dx							 
									 
	xor	cx,cx							 
	xor	dx,dx							 
	mov	ax,512			;read 512 bytes 		 
	call	ReadInControlBuffer	;Read the file header		 
	jc	short SetDOSData_fail 					 
	push	es							 
	push	si							 
	push	cs							 
	pop	es							 
	mov	di,COUNTRY_FILE_SIGNATURE			 
	mov	cx,8			;length of the signature	 
	repz	cmpsb							 
	pop	si							 
	pop	es							 
	jnz	short SetDOSData_fail 	;signature mismatch		 
									 
	add	si,18			;SI -> county info type 	 
	cmp	byte [si],1		;Only accept type 1 (Currently only 1 header type)
	jne	short SetDOSData_fail 	;cannot proceed. error return	 
	inc	si			;SI -> file offset		 
	mov	dx,[si]			;Get the INFO file offset.	 
	mov	cx,[si+2]					 
	mov	ax,1024			;read 1024 bytes.		 
	call	ReadInControlBuffer	;Read INFO			 
	jc	short SetDOSData_fail 					 
	mov	cx,[si]			;get the # of country, codepage combination entries
	cmp	cx,72			;cannot handle more than 72 entries.
	ja	short SetDOSData_fail 					 
	inc	si							 
	inc	si			;SI -> entry information packet 
	pop	dx			;restore code page id		 
	pop	ax			;restore country id		 
	pop	di							 
									 
SetDOSCntry_find:			;Search for desired country_id,codepage_id.
	cmp	ax,[si+2]		;compare country_id		 
	jne	short SetDOSCntry_next					 
	cmp	dx,0			;No user specified code page ?	 
	je	short SetDOSCntry_any_CodePage ;then no need to match code page id.
	cmp	dx,[si+4]		;compare code page id		 
	je	short SetDOSCntry_got_it					 
SetDOSCntry_next:							 
	add	si,[si]			;next entry			 
	inc	si							 
	inc	si			;take a word for size of entry itself
	loop	SetDOSCntry_find					 
	mov	cx, -1			;signals that bad country id entered.
SetDOSCntry_fail:							 
	stc								 
	retn								 
									 
SetDOSData_fail:							 
	pop	si							 
	pop	cx							 
	pop	di							 
	jmp	short SetDOSCntry_fail				 
									 
SetDOSCntry_any_CodePage:		;use the code_page_id of the country_id found.
	mov	dx,[si+4]					 
SetDOSCntry_got_it:			;found the matching entry	 
	mov	[cs:CntryCodePage_Id],dx ;save code page ID for this country.
	mov	dx,[si+10]		;get the file offset of country data
	mov	cx,[si+12] 				 
	mov	ax,512			;read 512 bytes 		 
	call	ReadInControlBuffer					 
	jc	short SetDOSCntry_fail					 
	mov	cx,[si]			;get the number of entries to handle.
	inc	si							 
	inc	si			;SI -> first entry		 
									 
SetDOSCntry_data:							 
	push	di			;ES:DI -> DOS_COUNTRY_CDPG_INFO 
	push	cx			;save # of entry left		 
	push	si			;si -> current entry in Control buffer
									 
	mov	al,[si+2]		;get data entry id		 
	call	GetCountryDestination	;get the address of destination in ES:DI
	jc	short SetDOSCntry_data_next ;No matching data entry id in DOS 
									 
	mov	dx,[si+4]		;get offset of data		 
	mov	cx,[si+6]					 
	mov	ax,4200h						 
	stc								 
	int	21h			;move pointer			 
	jc	short SetDOSData_fail 					 
	mov	dx,512			;start of data buffer		 
	mov	cx,[es:di]		;length of the corresponding data in DOS.
	add	cx,10			;Signature + A word for the length itself
	mov	ah,3Fh 						 
	stc								 
	int	21h			;read the country.sys data	 
	jc	short SetDOSData_fail 	;read failure			 
	cmp	ax, cx							 
	jne	short SetDOSData_fail 					 
									 
	mov	al,[si+2]		;save Data id for future use.	 
	mov	si,(512+8)		;SI-> data buffer + id tag field 
	mov	cx,[si]			;get the length of the file	 
	inc	cx			;Take care of a word for lenght of tab
	inc	cx			;itself.			 
	cmp	cx,(2048-512-8)		;Fit into the buffer?		 
	ja	short SetDOSData_fail 					 
	cmp	al,SetCountryInfo	;is the data for SetCountryInfo table?
	jne	short SetDOSCntry_Mov 	;no, don't worry                
	push	word [es:di+24]		;Cannot destroy ccMono_ptr address. Save them.
	push	word [es:di+26]					 
	push	di			;save DI			 
									 
	push	ax							 
	mov	ax,[cs:CntryCodePage_Id] ;Do not use the Code Page info in Country_Info
	mov	[si+4], ax		;Use the saved one for this !!!! 
	pop	ax							 
									 
SetDOSCntry_Mov:							 
	rep	movsb			;copy the table into DOS	 
	cmp	al,SetCountryInfo	;was the ccMono_ptr saved?	 
	jne	short SetDOSCntry_data_next					 
	pop	di			;restore DI			 
	pop	word [es:di+26]		;restore ccMono_ptr in DOS.	 
	pop	word [es:di+24]					 
									 
SetDOSCntry_data_next:							 
	pop	si			;restore control buffer pointer 
	pop	cx			;restore # of entries left	 
	pop	di			;restore pointer to DSO_COUNTRY_CDPG
	add	si,[si]			;try to get the next entry	 
	inc	si							 
	inc	si			;take a word of entry length itself
	loop	SetDOSCntry_data					 
	retn								 
					 
GetCountryDestination:				 
;Get the destination address in the DOS country info table.		 
;Input: AL - Data ID							 
;	ES:DI -> DOS_COUNTRY_CDPG_INFO					 
;On return:								 
;	ES:DI -> Destination address of the matching data id		 
;	carry set if no matching data id found in DOS.			 
									 
	push	cx							 
	add	di,country_cdpg_info.ccNumber_of_entries
					;skip the reserved area, syscodepage etc.
	mov	cx,[es:di]		;get the number of entries	 
	inc	di							 
	inc	di			;SI -> the first start entry id 
GetCntryDest:								 
	cmp	[es:di],al					 
	je	short GetCntryDest_OK 					 
	cmp	byte [es:di],SetCountryInfo ;was it SetCountryInfo entry?
	je	short GetCntryDest_1						 
	add	di,5			;next data id			 
	jmp	short GetCntryDest_loop 				 
GetCntryDest_1: 							 
	add	di,NEW_COUNTRY_SIZE+3	;next data id			 
GetCntryDest_loop:							 
	loop	GetCntryDest						 
	stc								 
	jmp	short GetCntryDest_Exit				 
GetCntryDest_OK:							 
	cmp	al,SetCountryInfo	;select country info?		 
	jne	short GetCntryDest_OK1					 
	inc	di			;now DI -> ccCountryInfoLen	 
	jmp	short GetCntryDest_Exit				 
GetCntryDest_OK1:							 
	les	di,[es:di+1]		;get the destination in ES:DI	 
GetCntryDest_Exit:							 
	pop	cx							 
	retn								 
								 
ReadInControlBuffer:				 
;Move file pointer to CX:DX						 
;Read AX bytes into the control buffer. (Should be less than 2 Kb)	 
;SI will be set to 0 hence DS:SI points to the control buffer.		 
;Entry:  CX,DX offset from the start of the file where the read/write pointer
;	 be moved.							 
;	 AX - # of bytes to read					 
;	 BX - file handle						 
;	 DS - buffer seg.						 
;Return: The control data information is read into DS:0 - DS:0200.	 
;	 CX,DX value destroyed. 					 
;	 Carry set if error in Reading file.				 
;									 
	push	ax			;# of bytes to read		 
	mov	ax, 4200h						 
	stc								 
	int	21h			;move pointer			 
	pop	cx			;# of bytes to read		 
	jc	short RICB_exit						 
	xor	dx,dx			;ds:dx -> control buffer	 
	xor	si,si							 
	mov	ah,3Fh			;read into the buffer		 
	stc								 
	int	21h			;should be less than 1024 bytes. 
RICB_exit:								 
	retn								 
									 
SET_COUNTRY_PATH:					 
;In:  DS - SYSINITSEG, ES - CONFBOT, SI -> start of the asciiz path string
;     DOSINFO_EXT, CNTRY_DRV, CNTRY_ROOT, CNTRY_PATH			 
;     Assumes current directory is the ROOT directory.			 
;Out: DS:DI -> full path (CNTRY_DRV).					 
;     Set the CNTRY_DRV string from the COUNTRY=,,path command. 	 
;     DS, ES, SI value saved.						 
									 
	push	si							 
	push	ds			;switch ds, es			 
	push	es							 
	pop	ds							 
	pop	es			;now DS -> CONFBOT, ES -> SYSINITSEG
									 
	call	CHK_DRIVE_LETTER	;current DS:[SI] is a drive letter? 
	jc	short SCP_Default_drv 	;no, use current default drive. 
	mov	al,[SI]					 
	inc	si							 
	inc	si			;SI -> next char after ":"      
	jmp	short SCP_SetDrv					 
SCP_Default_drv:							 
	mov	ah,19h 						 
	int	21h							 
	add	al,"A"			;convert it to a character.     
SCP_SetDrv:								 
	mov	[cs:CNTRY_DRV],al	;set the drive letter.		 
	mov	di,CNTRY_PATH					 
	mov	al,[SI]	
	cmp	al, "\"                                                 
	je	short SCP_Root_Dir						 
	; 23/06/2018 - Retro DOS v3.0
	;cmp	al,[cs:SWTCHR]	!!!	;let's accept "/" as an directory delim
	cmp	al,SWTCHR 
	je	short SCP_Root_Dir						 
	jmp	short SCP_Path						 
SCP_Root_Dir:								 
	dec	di			;DI -> CNTRY_ROOT		 
SCP_Path:								 
	call	MOVE_ASCIIZ		;copy it			 
	mov	di,CNTRY_DRV					 
SCPath_Exit:								 
	push	ds			;switch ds, es			 
	push	es							 
	pop	ds							 
	pop	es			;DS, ES value restored		 
	pop	si							 
	RETN
								 
CHK_DRIVE_LETTER:				 
;Check if DS:[SI] is a drive letter followed by ":".                    
;Assume that every alpha charater is already converted to UPPER CASE.	 
;Carry set if not.							 
;									 
	push	ax							 
	cmp	byte [si], "A"                                   
	jb	short CDLetter_NO						 
	cmp	byte [si], "Z"                                   
	ja	short CDLetter_NO						 
	cmp	byte [si+1], ":"                                 
	jne	short CDLetter_NO						 
	jmp	short CDLetter_exit					 
CDLetter_NO:								 
	stc								 
CDLetter_exit:								 
	pop	ax							 
	retn								 
									 
MOVE_ASCIIZ:				 
;In: DS:SI -> source ES:DI -> target					 
;Out: copy the string until 0.						 
;Assumes there exists a 0.						 
MASCIIZ_loop:								 
	movsb								 
	cmp	byte [SI-1], 0	;Was it 0?			 
	jne	short MASCIIZ_loop						 
	retn

; 10/06/2018 - Retro DOS v3.0
								 
;
;	DS:DX POINTS TO STRING TO OUTPUT (ASCIZ)
;
;	PRINTS <BADLD_PRE> <STRING> <BADLD_POST>
;

BADFIL:
        PUSH    CS
        POP     ES
        MOV     SI,DX
BADLOAD:
        MOV     DX,BADLD_PRE		; WANT TO PRINT CONFIG ERROR
        ;MOV	BX,BADLD_POST
PRNERR:
        PUSH    CS
        POP     DS
        MOV     AH,STD_CON_STRING_OUTPUT
        INT     21H
PRN1:   
	MOV     DL,[ES:SI]
        OR      DL,DL
        JZ      SHORT PRN2
        MOV     AH,STD_CON_OUTPUT
        INT     21H
        INC     SI
        JMP     SHORT PRN1
PRN2:   
	;MOV	DX,BX
	MOV	DX,BADLD_POST ; 10/04/2018
PRINT:  
	MOV     AH,STD_CON_STRING_OUTPUT
        INT     21H
        RETN

	;IF	NOEXEC  ; (NOEXEC = FALSE)
;;
;; LOAD FILE CALLED [DS:DX] AT MEMORY LOCATION ES:BX
;;
;LDFIL:
;	PUSH    AX
;	PUSH    BX
;	PUSH    CX
;	PUSH    DX
;	PUSH    SI
;	PUSH    DS
;	PUSH    BX
;
;	XOR     AX,AX                   ; OPEN THE FILE
;	MOV     AH,OPEN
;	STC                             ; IN CASE OF INT 24
;	INT     21H
;	POP     DX                      ; Trans addr is DS:DX
;	JC      SHORT LDRET
;
;	; 10/06/2018
;	PUSH	DX
;	MOV     BX,AX                   ; Handle in BX
;	XOR	CX,CX
;	XOR	DX,DX
;	MOV	AX,(LSEEK*256)+2
;	STC				;IN CASE OF INT 24
;	INT	21H			; Get file size in DX:AX
;	JC	short LDCLSP
;	POP	DX
;	PUSH	DX
;	MOV	CX,ES			; CX:DX is xaddr
;	ADD	DX,AX			; Add file size to Xaddr
;	JNC	short DOSIZE
;	ADD	CX,1000H		; ripple carry
;DOSIZE:
;	mov	ax,dx
;	call	ParaRound
;	mov	dx,ax
;
;	ADD	CX,DX
;	CMP	CX,[ALLOCLIM]
;	JB	short OKLD
;	JMP	short MEM_ERR
;
;OKLD:
;	XOR	CX,CX
;	XOR	DX,DX
;	MOV	AX,LSEEK SHL 8		;Reset pointer to beginning of file
;	STC				;IN CASE OF INT 24
;	INT	21H
;	JC	LDCLSP
;	POP	DX
;	PUSH	ES			;READ THE FILE IN
;	POP	DS			;Trans addr is DS:DX
;	MOV	CX,0FF00H		; .COM files arn't any bigger than
;					; 64k-100H
;	MOV	AH,READ
;	STC				;IN CASE OF INT 24
;	INT	21H
;	JC	short LDCLS
;	MOV	SI,DX			;CHECK FOR EXE FILE
;	CMP	WORD [SI],"MZ"
;	CLC				; Assume OK
;	JNZ	short LDCLS		; Only know how to do .COM files
;	STC
;	JMP	SHORT LDCLS
;
;LDERRP:  
;	STC
;LDCLSP:
;	POP	DX
;LDCLS:  
;	PUSHF
;	MOV     AH,CLOSE                ; CLOSE THE FILE
;       STC
;       INT     21H
;	POPF
;
;LDRET:  
;	POP     DS
;	POP     SI
;	POP     DX
;	POP     CX
;	POP     BX
;	POP     AX
;	RETN

	;ENDIF
;
;  OPEN DEVICE POINTED TO BY DX, AL HAS ACCESS CODE
;   IF UNABLE TO OPEN DO A DEVICE OPEN NULL DEVICE INSTEAD
;
OPEN_DEV:
        CALL    OPEN_FILE
        JNC     SHORT OPEN_DEV3
OPEN_DEV1:
        MOV     DX,NULDEV
        CALL    OPEN_FILE
OPEN_DEV2:
        RETN
OPEN_DEV3:
	; 05/07/2018
	MOV	BX,AX			; Handle from open to BX
        XOR     AX,AX                   ; GET DEVICE INFO
        MOV     AH,IOCTL
        INT     21H
        TEST    DL,10000000B
        JNZ     SHORT OPEN_DEV2
        MOV     AH,CLOSE
        INT     21H
        JMP     SHORT OPEN_DEV1

OPEN_FILE:
        MOV     AH,OPEN
        STC
        INT     21H
        RETN

INT24:  
	ADD     SP,6                    ; RESTORE MACHINE STATE
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES
        PUSH    AX
        MOV     AH,GET_DEFAULT_DRIVE    ; INITIALIZE DOS
        INT     21H
        POP     AX
        IRET                            ; BACK TO USER

        ;IF	ALTVECT
BOOTMES:
	DB      13
TEN:	DB      10
	; 22/11/2022
	;DB	"MS-DOS version "
	;DB	MAJOR_VERSION + "0"
	;DB	"."
	;DB	(MINOR_VERSION / 10) + "0"
	;DB	(MINOR_VERSION % 10) + "0"
	;DB	13,10
	;;DB	"Copyright 1981,82 Microsoft Corp.",13,10,"$"
	;; 27/06/2018
	;DB	"Copyright 1981,87 Microsoft Corp.",13,10,"$"
	;ENDIF
	; 22/11/2022
	db 	"Retro DOS v3.1"
	db	" (2019-2022) "
	db	13,10
	db	"by Erdogan Tan "
	db	13,10
	db	13,10,"$"

	; 10/06/2018 - Retro DOS v3.0
NULDEV: 
	DB      "NUL",0
CONDEV: 
	DB      "CON",0
AUXDEV: 
	DB      "AUX",0
PRNDEV: 
	DB      "PRN",0

CONFIG:
	DB      "\CONFIG.SYS",0

CNTRY_DRV:
	DB	"A:"
CNTRY_ROOT:
	DB	"\"
CNTRY_PATH:
	DB	"COUNTRY.SYS",0
	times	52 db 0

COUNTRY_FILE_SIGNATURE:
	db	0FFh,'COUNTRY' 	 

CntryCodePage_Id:
	DW	0	

COMMND:
	DB      "\COMMAND.COM",0
	times	20 db 0

COMTAB:
        ;DB	8,"AVAILDEV",'A'	; NO LONGER SUPPORTED
	DB	7,"BUFFERS",  'B'
	DB	5,"BREAK",    'C'
	DB	6,"DEVICE",   'D'
	DB	5,"FILES",    'F'
	DB	4,"FCBS",     'X'
	DB	9,"LASTDRIVE",'L'
	DB	8,"DRIVPARM", 'P'       ; RS for DOS 3.2
		;IF	STACKSW					; 3.30
	DB	6,"STACKS",   'K'       ; BAS for DOS 3.2	; 3.30
		;ENDIF						; 3.30
	DB	7,"COUNTRY",  'Q'
	DB	5,"SHELL",    'S'
	;DB	8,"SWITCHAR",'W'	; NO LONGER SUPPORTED

        DB      0

DEVICEPARAMETERS:  ; STRUC A_DEVICEPARAMETERS
;DeviceParameters a_DeviceParameters <0,DEV_3INCH720KB,0,80>
DEVP.SPECIALFUNC:	DB 0
DEVP.DEVICETYPE:	DB DEV_3INCH720KB
DEVP.ATTRIBUTES:	DW 0
DEVP.CYLINDERS:		DW 80
DEVP.MEDIATYPE:		DB 0
DEVP.DP_BPB:		TIMES A_BPB.size DB 0
DEVP.TRACKTBLENTS:	DW 0
DEVP.SECTORTABLE:	DB MAX_SECTORS_IN_TRACK * A_SECTORTABLE.size

HLIM: DW 2
SLIM: DW 9

DRIVE: DB 0
SWITCHES: DW 0

; 06/07/2018

;-----------------------------------------------------------------------------
; BOOT DISK PARAMETERS
;-----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0
; 07/04/2018 - Retro DOS V2.0

;BOOT_DRV_PARMS:
;		; 1.44MB
;		dw 512
;               db 1
;               dw 1	
;               db 2
;               dw 224
;		dw 2880
;		; Retro DOS v1.0 - 10/02/2018
;		db 0F0h		; Media descriptor
;		dw 9		; FAT size in sectors
;		dw 18		; Sectors per track
;		dw 2		; Number of heads
;		; 02/06/2018 - Retro DOS v3.0
;		;dw 0		
;		dd 0		; Hidden sectors
;		dd 0		; Big total sectors		
;
;; Note: These (fd&hd) parameters table sizes are  
;;						 31 bytes for MSDOS 3.3		
;;						 19 bytes for MSDOS 2.11

;-----------------------------------------------------------------------------
; FLOPPY DISK PARAMETERS
;-----------------------------------------------------------------------------
; 02/06/2018 - Retro DOS v3.0
; 07/04/2018 - Retro DOS V2.0


; 02/06/2018 - Retro DOS v3.0 - 25 bytes of DOS disk paramaters


;
; The following are the recommended BPBs for the media that we know of so
; far.

; 48 tpi diskettes
	;EVENB
;align 2
;BPB48T:
;	DW	512
;	DB	2
;	DW	1
;	DB	2
;	DW	112
;	DW	2*9*40
;	DB	0FDH
;	DW	2
;	DW	9
;	DW	2
;	DD	0		;hidden sectors - sp
;	DD	0		;big total sectors - sp
;	;DB	6 DUP(?)	;reserved
;	;times	6 db 0

; 96tpi diskettes
	;EVENB
;align 2
;BPB96T:	
;	DW	512
;	DB	1
;	DW	1
;	DB	2
;	DW	224
;	DW	2*15*80
;	DB	0f9H
;	DW	7
;	DW	15
;	DW	2
;	DD	0		;hidden sectors - sp
;	DD	0		;big total sectors - sp
;	;DB	6 DUP(?)	;reserved
;	;times	6 db 0

;BPBSIZ	equ	$-BPB96T
BPBSIZ	equ	25  ; 02/06/2018


; 3 1/2 inch diskette BPB
	;EVENB
;align 2
;BPB35:	
;	DW	512
;	DB	2
;	DW	1		; Double sided with 9 sec/trk
;	DB	2
;	DW	70h
;	DW	2*9*80
;	DB	0f9H
;	DW	3
;	DW	9
;	DW	2
;	DD	0		;hidden sectors - sp
;	DD	0		;big total sectors - sp
;	;DB	6 DUP(?)	;reserved
;	;times	6 db 0

	;EVENB
align 2
		; Retro DOS v3.0 - 25 bytes disk parameters (02/06/2018)
		; Retro DOS v2.0 - 19 bytes disk parameters (07/04/2018)
_FD_parameters:
		; Retro DOS v1.0 - 10/02/2018
		; 17 bytes of DOS disk parameters

; 48 tpi diskettes
BPB48T: 	; 02/06/2018 - Retro DOS v3.0
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
		; Retro DOS v2.0 - 07/04/2018
		;dw 0		; Hidden sectors
		; Retro DOS v3.0 - 02/06/2018
		dd 0		; Hidden sectors
		dd 0		; Big Total sectors
; 96tpi diskettes
BPB96T:		; 02/06/2018 - Retro DOS v3.0
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
		; Retro DOS v2.0 - 07/04/2018
		;dw 0		; Hidden sectors
		; Retro DOS v3.0 - 02/06/2018
		dd 0		; Hidden sectors
		dd 0		; Big Total sectors
; 3 1/2 inch diskette bpb
BPB35:		; 02/06/2018 - Retro DOS v3.0	
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
		; Retro DOS v2.0 - 07/04/2018
		;dw 0
		; Retro DOS v3.0 - 02/06/2018
		dd 0		; Hidden sectors
		dd 0		; Big Total sectors
BPB35H:		; 03/06/2018 - Retro DOS v3.0 (MSDOS 6.0, SYSINIT2.ASM, 1991)		
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
		; Retro DOS v2.0 - 07/04/2018
		;dw 0
		; Retro DOS v3.0 - 02/06/2018
		dd 0		; Hidden sectors
		dd 0		; Big Total sectors
BPB288:		; 03/06/2018 - Retro DOS v3.0 (MSDOS 6.0, SYSINIT2.ASM, 1991)
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
		; Retro DOS v2.0 - 07/04/2018
		;dw 0
		; Retro DOS v3.0 - 02/06/2018
		dd 0		; Hidden sectors
		dd 0		; Big Total sectors

; 06/07/2018

align 2

BPBTable:
	; Retro DOS v3.0 - 03/06/2018  (MSDOS 6.0, SYSINIT2.ASM, 1991)
		dw	BPB48T	; 48tpi drives
		dw	BPB96T	; 96tpi drives
		dw	BPB35	; 3.5" drives
; the following are not supported, so default to 3.5" media layout
		dw	BPB35	; not used - 8" drives
		dw	BPB35	; not used - 8" drives
		dw	BPB35	; not used - hard files
		dw	BPB35	; not used - tape drives
		dw	BPB35H	; 3 1/2" 1.44mb drive
		dw	BPB35	; ERIMO
		dw	BPB288	; 2.88 MB diskette drives
		;dw	BPB35H	; 3 1/2" 1.44mb drive

END_OF_FDP_TBLS:

; ----------------------------------------------------------------------------

; 10/06/2018

switchlist:
	 db	7,"FHSTDCN"         ; Preserve the positions of N and C.

; The following depend on the positions of the various letters in SwitchList

flagdrive   equ     0004H
flagcyln    equ     0008H
flagseclim  equ     0010H
flagheads   equ     0020H
flagff	    equ     0040H

SWTCHR	EQU "/"	

;SYSINITSEG      ENDS
;       END

; ----------------------------------------------------------------------------
; SYSIMES.ASM, 22/09/1983, -- SYSIMES.INC, 24/07/1987 (MSDOS 3.3) --
; ----------------------------------------------------------------------------

;SYSINITSEG      SEGMENT PUBLIC BYTE 'SYSTEM_INIT'

        ;PUBLIC  BADOPM,CRLFM,BADSIZ,BADLD,BADCOM,SYSSIZE

BADOPM: 
	DB      13,10,"Unrecognized command in CONFIG.SYS"
BADSIZ_POST:
BADLD_POST:
CRLFM:  DB      13,10,'$'

;
;PRINTED when installed device specifies too large a sector size.'$' terminated.
; FORM: 	<BADSIZ_PRE>device name<BADSIZ_POST>
;

BADSIZ_PRE:
	DB      13,10,"Sector size too large in file $"

;
;PRINTED when installed device cannot be found. '$' terminated.
; FORM: 	<BADLD_PRE>device name<BADLD_POST>
;

BADLD_PRE:
	DB      13,10,"Bad or missing $"

;
;PRINTED when command interpreter is not found. NUL terminated.
; FORM: 	<BADLD_PRE><BADCOM><BADLD_POST>
;

BADCOM: DB      "Command Interpreter",0

;PRINTED when country code, code page combination was not found ;3.30
;  in country.sys file. '$' terminated.				;3.30
; FORM: 	<BADCOUNTRY>					; 3.30		

BADCOUNTRY:
	DB	13,10,"Invalid country code or code page",13,10,"$" ; 3.30

; 10/06/2018

;PRINTED when code page id is missing or wrong syntax. - J.K.	;3.30
; FORM: 	<BADCOUNTRYCOM> 				;3.30
BADCOUNTRYCOM:
	DB	13,10,"Error in COUNTRY command",13,10,"$"	;3.30

;PRINTED when the memory left is not sufficient to handle COUTRY.SYS file ;3.30
; FORM: 	<INSUFMEMORY>						  ;3.30
INSUFMEMORY:
	DB	13,10,"Insufficient memory for COUNTRY.SYS file",13,10,"$" ;3.30

; PRINTED when there is insufficient memory. '$' TERMINATED, note
;   that this message includes crlfm!
;
BADMEM:
	DB	13,10,"Configuration too large for memory",13,10,"$"

; 26/03/2018
; PRINTED when the attempt is made to install a block device which would
;   have a drive letter > 'Z'
;

BADBLOCK:
	DB	13,10,"Too many Block Devices",13,10,"$"

; 10/06/2018
; PRINTED when the attempt is made to install a stack with invalid	;3.30
;   combinations of # of stacks, stack size.	- J.K. 5/23/86		;3.30
BADSTACK:
	DB	13,10,"Invalid STACK parameters",13,10,"$" 		;3.30

;SYSSIZE LABEL   BYTE

;SYSINITSEG      ENDS
;       END