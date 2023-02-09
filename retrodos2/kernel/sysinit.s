; ****************************************************************************
; SYSINIT.BIN (MSDOS 2.0 Kernel) - RETRO DOS v2.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 10/06/2018
; ----------------------------------------------------------------------------
; Beginning: 24/02/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm sysinit.s -l sysinit.lst -o SYSINIT.BIN)) 	
; ----------------------------------------------------------------------------
; Derived from 'SYSINIT1.ASM' and 'SYSINIT2.ASM' files of MSDOS 3.3
; source code by Microsoft, 24/07/1987
; ----------------------------------------------------------------------------
; Derived from 'SYSINIT.ASM' file of MSDOS 2.0 (IBM PCDOS v2.0) source code
; by Microsoft, 12/10/1983
; ****************************************************************************
; main file: 'retrodos.s'
; incbin 'SYSINIT.BIN' ; (SYINITSEG)

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
TRUE    EQU     ~FALSE ; NOT FALSE

;IBMVER     EQU     FALSE
;IBM        EQU     IBMVER
;IBMJAPVER  EQU     FALSE                ; If TRUE set KANJI true also
;MSVER      EQU     TRUE 
;ALTVECT    EQU     FALSE                ; Switch to build ALTVECT version
;HIGHMEM    EQU     FALSE
;KANJI      EQU     FALSE

;       IF      IBMVER OR IBMJAPVER
;NOEXEC  EQU     TRUE
;       ELSE
;NOEXEC  EQU     FALSE
;       ENDIF

; Set to agree with those in DOST:MSHEAD.ASM, ALTVECT version only
MAJOR_VERSION   EQU     2
MINOR_VERSION   EQU     11	;2.11

;DOSSIZE EQU	5000H

; (MSDOS 2.0) SYSINITVAR table offsets
; ----------------------------------------------------------------------

DPBHEAD	 EQU	0  ; dd ; Pointer to head of DPB-FAT list	
SFT_ADDR EQU	4  ; dd ; Pointer to first FCB table
BCLOCK	 EQU	8  ; dd ; Pointer to the CLOCK device	
BCON	 EQU	12 ; dd ; Console device entry points
NUMIO	 EQU	16 ; db ; Number of disk tables
MAXSEC   EQU	17 ; dw ; Maximum allowed sector size
BUFFHEAD EQU	19 ; dd	; Pointer to head of buffer queue
DEVHEAD  EQU	23 ; dd ; Set to list start passed by BIOS at DOS Init
NUL_DEV	 EQU	23 ; dd	; NUL device header, first 4 bytes of dev header
			; (4+14 bytes)
			; Points to the next device in device chain 
DEVTYPE  EQU    27 ; dw ; = ISNULL
SNULDEV	 EQU 	29 ; dw
INULDEV	 EQU	31 ; dw
DEVNAME	 EQU	33 ; 8 BYTES ; "NUL     "

; ----------------------------------------------------------------------
; Internal DOS data returned by DOSINIT
;
struc	SYSINITVAR
.DPBHEAD:	resd 1			; Pointer to head of DPB-FAT list
.SFT_ADDR:	resd 1			; Pointer to first FCB table
; The following address points to the CLOCK device
.BCLOCK:	resd 1
; The following address is used by DISKSTATCHK it is always
; points to the console input device header
.BCON:		resd 1			; Console device entry points
.NUMIO:		resb 1			; Number of disk tables
.MAXSEC:	resw 1			; Maximum allowed sector size
.BUFFHEAD:	resd 1			; Head of buffer queue
.DEVHEAD:	resd 1			; NUL dev head points to next dev
.size:
endstruc

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
; system file table
;
struc	SFT
.SFT_LINK:	resd 1
.SFT_COUNT:	resw 1		; number of entries
.SFT_TABLE:	resw 1		; beginning of array of the following
.size:
endstruc

; ----------------------------------------------------------------------
; system file table entry
;
struc	SF_ENTRY
.sf_ref_count:	resb 1		; number of processes sharing fcb
.sf_mode:	resb 1		; mode of access
.sf_attr:	resb 1		; attribute of file
.sf_fcb:	resb SYS_FCB.size  ; actual FCB
.size:
endstruc

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

struc	BUFFINFO
.NEXTBUF:	resd 1	; Pointer to next buffer in list
; The next two items are often refed as a word
.BUFDRV:	resb 1	; Logical drive # assoc with buffer FF = free
.BUFDIRTY:	resb 1	; Dirty flag
.BUFPRI:	resb 1	; Buffer selection priority (see EQUs below)
.VISIT:		resb 1	; Visit flag for buffer pool scans
.BUFSECNO:	resw 1	; Sector number of buffer
; The next two items are often refed as a word
.BUFWRTCNT:	resb 1	; For FAT sectors, # times sector written out
.BUFWRTINC:	resb 1	; "   "     "   , # sectors between each write
.BUFDRVDP:	resd 1	; Pointer to drive parameters
.size:
endstruc

BUFINSIZ        EQU     BUFFINFO.size ; ; Size of structure in bytes

; ----------------------------------------------------------------------
; DPB structure

DIRSTRLEN       EQU     64	; Max length in bytes of directory strings

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
.CURRENT_DIR:	resw 1		; Cluster number of start of current directory
				; 0 indicates root, -1 indicates invalid
				; (disk ? changed)
.DIR_TEXT:	resb DIRSTRLEN	; ASCIZ string of current directory
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
endstruc									   ;

struc A_DEVICEPARAMETERS
.DP_SPECIALFUNCTIONS:	resb	1
.DP_DEVICETYPE:		resb	1
.DP_DEVICEATTRIBUTES:	resw	1
.DP_CYLINDERS:		resw	1
.DP_MEDIATYPE:		resb	1
.DP_BPB:		resb	A_BPB.size
.DP_TRACKTABLEENTRIES.	resw	1
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

        ;EXTRN   BADOPM:BYTE,CRLFM:BYTE,BADCOM:BYTE
        ;EXTRN   BADSIZ_PRE:BYTE,BADLD_PRE:BYTE
        ;EXTRN   BADSIZ_POST:BYTE,BADLD_POST:BYTE
        ;EXTRN   SYSSIZE:BYTE,BADCOUNTRY:BYTE

        ;PUBLIC  CURRENT_DOS_LOCATION
        ;PUBLIC  FINAL_DOS_LOCATION
        ;PUBLIC  DEVICE_LIST
        ;PUBLIC  MEMORY_SIZE
        ;PUBLIC  DEFAULT_DRIVE
        ;PUBLIC  BUFFERS
        ;PUBLIC  FILES
        ;PUBLIC  SYSINIT

        ;IF      HIGHMEM
        ;PUBLIC  DPBBUF_SIZ
        ;ENDIF

; ----------------------------------------------------------------------
; SYINIT1.ASM (MSDOS 3.3) - SYSINIT.ASM (MSDOS 2.0)
; ----------------------------------------------------------------------

SYSINIT:
        ;JMP	GOINIT
	JMP	SYSIN ; 25/02/2018 - Retro DOS 2.0 modification
align 4
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

; 28/03/2018
;; MSDOS 3.3 - SYSINIT1.ASM - 24/07/1987
;
;SYSI_Country:	
;	dw	0 ; 5/29/86 Pointer to
;	dw	0 ;country table in DOS

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

	;;IF	STACKSW    ; STACKSW TRUE
;;
;; Internal Stack Parameters
;
;STACK_COUNT:	DW	DefaultCount
;STACK_SIZE:	DW	DefaultSize
;STACK_ADDR:	DD	0
;	;ENDIF

; various default values

MEMORY_SIZE:
	dw	1
DEFAULT_DRIVE:
	db	0
BUFFERS:
	db	-1	; initialized during buffer allocation
	;db	2	; two buffers
FILES:
	db	8	; enough files for pipe
;FCBS:
;	db	4	; performance for recycling
;KEEP:
;	db	0	; keep original set
;NUM_CDS:
;	db	5	; 5 net drives
;CONFBOT:
;	dw	0
;ALLOCLIM:
;	dw	0
;FOOSTRNG:
;	db	"A:\",0
COMMAND_LINE:
	db	2,0,"P" ; Default Command.com Args
        
	times	29 db 0
ZERO:
	db	0
;SEPCHR:
;	db	0

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

align 4

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
	db	0		; INITIALIZE CODE
	dw	0
	times	8 db 0
UNITCOUNT:
	db	0
BREAK_ADDR:
	dd	0
BPB_ADDR:
	dd	0
;DriveNumber:
;	db	0

;align 2

TempStack:
	times	128 db 0

;GOINIT:
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
;;      	JNZ     short NOSCAN
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
;	XOR	AX,AX ; 0
;	PUSH    AX
;	
;	RETF	; far jump to final location of SYSINIT code
;

;
;       MOVE THE DOS TO ITS PROPER LOCATION
;

	nop	; 30/03/2018
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
	;SHR	CX, 1
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

	; DS = Segment for COMMAND.COM
	; ES:DI = Address of SYSINITVAR

        MOV     [CS:DOSINFO+2],ES ; SAVE POINTER TO DOS INFO
        MOV     [CS:DOSINFO],DI

	;; 25/03/2018 - Retro DOS v2.0 (MSDOS 2.0 <-> 3.3)
	;SYSI_NUMIO equ	16 ; SYSINITVAR NUMIO offset for MSDOS 2.0

	;NOTE: Here, MSDOS 3.3 SYSINIT code is modified for
	;      MSDOS 2.0 kernel.
	; (This code is not compatible with MSDOS 3.3 kernel!)

	;; MSDOS 3.3 - SYSINIT1.ASM
	;MOV	AL,[ES:DI+SYSI_NUMIO] ; SYSINITVAR.NUMIO
	;MOV	[DriveNumber],AL ; Save start of installable block drvs

	;MOV	AX,CS
	;SUB	AX,11H			; room for header we will copy shortly
	;MOV	[CONFBOT],AX		; Temp "unsafe" location

        ;IF      NOT IBMJAPVER
        ;CALL    RE_INIT                 ; Re-call the BIOS
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

 	; Set Process Data Block - Program Segment Prefix address
	; BX = PDB/PSP segment
        MOV     AH,SET_CURRENT_PDB
        INT     21H

        ;PUSH	DS ; 13/04/2018
        PUSH    CS
        POP     DS
        MOV     DX,INT24	; SET UP INT 24 HANDLER
        MOV     AX,(SET_INTERRUPT_VECTOR*256)+24H
        INT     21H

	; 25/03/2018 - MSDOS 3.3 - SYSINIT1.ASM
	MOV	BX,0FFFFH
	MOV	AH,ALLOC
	INT	21H			;FIRST TIME FAILS
	MOV	AH,ALLOC
	INT	21H			;SECOND TIME GETS IT
	MOV	[AREA],AX
	MOV	[MEMHI],AX		; MEMHI:MEMLO now points to
					; start of free memory
        ;IF	ALTVECT
	;MOV	DX,BOOTMES
        ;CALL	PRINT		; Print message DOSINIT couldn't
        ;ENDIF

        MOV     DL,[DEFAULT_DRIVE]

	;POP	DS ; 13/04/2018
	
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
	;MOV	ES, BP	

        ;MOV     BX,CS
        ;SUB     BX,10H
        ;MOV     DS,BX
        ;XOR     SI,SI
        ;MOV     DI,SI
        ;MOV     CX,80H
        ;REP     MOVSW
	;MOV	[ES:PDB.JFN_POINTER + 2],ES ; Relocate ; 25/03/2018
        ;MOV     BX,ES

        ;MOV     AH,SET_CURRENT_PDB
        ;INT     21H

        ;MOV     [ES:PDB.PARENT_PID],ES  ; WE ARE THE ROOT
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
;
; SET UP THE PARAMETERS FOR COMMAND
;
GOSET:
        MOV     SI,COMMAND_LINE+1

	; 01/05/2018 (NOEXEC = FALSE)
        ;IF	NOEXEC
        ;MOV     DI,81H
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
        ;MOV     [ES:80H],CL
        ;MOV     AL,[DEFAULT_DRIVE]
        ;MOV     [ES:5CH],AL
        ;ELSE
        MOV	[COMMAND_LINE],CL       ; Count
        ;ENDIF

        PUSH    CS
        POP     ES

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
	MOV	BX,COMEXE
        ;MOV	[BX+EXEC0.COM_LINE+2],CS
        ;MOV	[BX+EXEC0.5C_FCB+2],CS
        ;MOV	[BX+EXEC0.6C_FCB+2],CS
	MOV	[EXEC0.COM_LINE+2],CS
        MOV	[EXEC0.5C_FCB+2],CS
        MOV	[EXEC0.6C_FCB+2],CS

        ;XOR	AX,AX
        ;MOV	AH,EXEC
        MOV	AX, EXEC*256
	STC				; IN CASE OF INT 24
        INT	21H			; GO START UP COMMAND
        ;ENDIF

COMERR:
        MOV     DX,BADCOM		; WANT TO PRINT COMMAND ERROR
        CALL    BADFIL
STALL:  
	JMP     SHORT STALL

; ----------------------------------------------------------------------
; SYSCONF.ASM - MSDOS 3.3 - 24/07/1987
; ----------------------------------------------------------------------
; 25/03/2018 - Retro DOS v2.0

DOCONF: 		;Take care of Config.sys file.	
        ; 13/04/2018
	;PUSH    CS
        ;POP     DS
	
	MOV     AX,(CHAR_OPER*256)	; GET SWITCH CHARACTER
	INT     21H
	MOV     [COMMAND_LINE+1],DL

        MOV     DX,CONFIG		; NOW POINTING TO FILE DESCRIPTION
        MOV     AX,(OPEN*256)		; OPEN FILE "CONFIG.SYS"
        STC                             ; IN CASE OF INT 24
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
        ADD     AX,15
        MOV     CL,4
        SHR     AX,CL                   ; NUMBER OF SEGMENTS
        SUB     DX,AX
        SUB     DX,11H                  ; ROOM FOR HEADER

        MOV     CX,[COUNT]
        MOV     DS,DX
        MOV     ES,DX
        XOR     DX,DX
        ;MOV	CX,[CS:COUNT]
        MOV     AH,READ
        STC                             ; IN CASE OF INT 24
        INT     21H                     ; Function request
        PUSHF
;
; Find the EOF mark in the file.  If present, then trim length.
;	
       	PUSH    CS
        POP     DS

	PUSH	AX
	PUSH	DI
	PUSH	CX
	MOV	AL,1Ah			; eof mark
	MOV	DI,DX			; point ro buffer
	JCXZ	PUTEOL			; no chars
	REPNZ	SCASB			; find end
	JNZ	SHORT PUTEOL		; none found and count exahusted
;
; We found a 1A.  Back up
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
	;MOV	[CS:COUNT],DI	
	MOV	[COUNT],DI		; new count
;
; Restore registers
;
	POP	CX
	POP	DI
	POP	AX

        ;PUSH	CS
        ;POP	DS

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
        ;LES     BX,[CS:MEMLO]
        ;CALL    LDFIL			; LOAD IN THE DEVICE DRIVER
        ;ELSE
        MOV	ES,BX
        MOV	BX,PRMBLK		; ES:BX POINTS TO PARAMETERS
        MOV	AL,3
        MOV	AH,EXEC
        STC				; IN CASE OF INT 24
        INT	21H                     ; LOAD IN THE DEVICE DRIVER
        ;ENDIF

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
        MOV     AX,[DS:SI]              ; GET ATTRIBUTES
        TEST    AX,DEVTYP               ; TEST IF BLOCK DEV
        JZ      SHORT ISBLOCK
	CALL	SET_BREAK		; Go ahead and alloc mem for device
	JC	SHORT ERASE_DEV
        TEST    AX,ISCIN                ; IS IT A CONSOLE IN?
        JZ      SHORT TRYCLK
        MOV     [ES:DI+BCON],DX
        MOV     [ES:DI+BCON+2],DS

TRYCLK: 
	TEST    AX,ISCLOCK              ; IS IT A CLOCK DEVICE?
        JZ      SHORT GOLINK
        MOV     [ES:DI+BCLOCK],DX
        MOV     [ES:DI+BCLOCK+2],DS
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
        MOV     DL,[ES:DI+NUMIO]        ; GET NUMBER OF DEVICES
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
	ADD	[ES:DI+NUMIO],AL	; UPDATE THE AMOUNT
	;ADD	[CS:DriveNumber],AL	; remember amount for next device
        LDS     BX,[CS:BPB_ADDR]        ; POINT TO BPB ARRAY
PERUNIT:
        LES     BP,[CS:DOSINFO]
        LES     BP,[ES:BP+DPBHEAD]	; GET FIRST DPB

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
        MOV     [ES:BP+DPB.DRIVE],DX
        MOV     AH,SETDPB               ; HIDDEN SYSTEM CALL
        INT     21H
        MOV     AX,[ES:BP+DPB.SECTOR_SIZE]
        PUSH    ES
        LES     DI,[CS:DOSINFO]		; ES:DI POINT TO DOS INFO
        CMP     AX,[ES:DI+MAXSEC]
        POP     ES
        JBE     SHORT NOTMAX
        POP     SI
        POP     ES
        MOV     DX,BADSIZ_PRE
        MOV     BX,BADSIZ_POST
        CALL    PRNERR
        JMP     COFF

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

LINKIT:
        LES     DI,[CS:DOSINFO]         ; ES:DI = DOS TABLE
        MOV     CX,[ES:DI+DEVHEAD] 	; DX:CX = HEAD OF LIST
        MOV     DX,[ES:DI+DEVHEAD+2]

        LDS     SI,[CS:ENTRY_POINT]     ; DS:SI = DEVICE LOCATION
        MOV     [ES:DI+DEVHEAD],SI 	; SET HEAD OF LIST IN DOS
        MOV     [ES:DI+DEVHEAD+2],DS
        MOV     AX,[DS:SI]              ; GET POINTER TO NEXT DEVICE
        MOV     [CS:ENTRY_POINT],AX	; AND SAVE IT

        MOV    [DS:SI],CX		; LINK IN THE DRIVER
        MOV    [DS:SI+2],DX
ENDDEV:
        POP     SI
        POP     ES
        INC     AX                      ; AX = FFFF?
        JZ      SHORT COFFJ3
        JMP	GOODLD                  ; OTHERWISE PRETEND WE LOADED IT IN
COFFJ3:
COFFV:  
	JMP     COFF

;------------------------------------------------------------------------------
; Country command
; (NOTE: MSDOS 2.0 COUNTRY CONFIG. - not as in MSDOS 3.3 SYSCONF.ASM, 1987) 
;------------------------------------------------------------------------------
; SYSINIT.ASM, 1983
TRYQ:
        CMP     AH,'Q'
        JNZ     SHORT TRYF
        CALL    GETNUM
        JZ      SHORT COFFV
        OR      AH,AH
        JNZ	SHORT  COFFV
        MOV     AH,INTERNATIONAL        ; AL is country code
        MOV     DX,-1                   ; Set country
        INT     21H
        JNC     SHORT COFFV
        MOV     DX,BADCOUNTRY
        CALL    PRINT
        JMP     SHORT COFFV

;------------------------------------------------------------------------------
; Files command
;------------------------------------------------------------------------------
; SYSCONF.ASM, 1987
TRYF:
        CMP     AH,'F'
	;JNZ    SHORT TRYL
        JNZ	SHORT TRYW	; Retro DOS v2.0 - 25/03/2018 
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
;TRYL:
;	CMP	AH,'L'
;	JNZ	SHORT TRYP
;	OR	AL,020h
;	SUB	AL,'a'
;	JB	SHORT TRYLBAD
;	INC	AL
;	CMP	AL,26			; a-z are allowed
;	JA	SHORT TRYLBAD
;	MOV	[NUM_CDS],AL
;COFFJ6: 
;	JMP	COFF
;TRYLBAD:
;	JMP	BADOP

;-------------------------------------------------------------------------------
; Setting Drive Parameters
;-------------------------------------------------------------------------------
;TRYP:
;	CMP	AH,'P'
;	JNZ	SHORT TRYK
;	CALL	PARSELINE
;	JC	SHORT TRYLBAD
;	CALL	SETPARMS
;	CALL	DIDDLEBACK
;	JC	SHORT TRYLBAD
;	JMP	COFF

;------------------------------------------------------------------------------
; Switch command
;------------------------------------------------------------------------------
TRYW:
        CMP     AH,'W'
        JNZ     SHORT TRYA
	;JMP	BADOP		; no longer implemented (MSDOS 3.3)
	; 01/04/2018
	MOV     DL,AL
	MOV     AX,(CHAR_OPER*256)+1	; SET SWITCH CHARACTER
	MOV     [COMMAND_LINE+1],DL
	INT     21H
	JMP     COFF

;------------------------------------------------------------------------------
; Availdev command
;------------------------------------------------------------------------------
TRYA:
        CMP     AH,'A'
        JNZ     SHORT TRYS
	JMP	BADOP		; no longer implemented (MSDOS 3.3)
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
TRYX:
	JMP	BADOP	; 25/03/2018 - Retro DOS v2.0 

;	CMP	AH,'X'
;	JNZ	SHORT TRYZ
;	CALL	GETNUM
;	JZ	SHORT TRYXBAD 		; gotta have at least one
;	CMP	AX,256
;	JAE	SHORT TRYXBAD 		; Can't be more than 8 bits worth
;	MOV	[FCBS],AL
;;
;; Skip delimiters after the ,
;;
;	CALL	SKIP_DELIM
;	JC	SHORT TRYXBAD
;	CALL	GETNUM
;	JC	SHORT TRYXBAD		; Number bad (Zero is OK here)
;	CMP	AX,256
;	JAE	SHORT TRYXBAD
;	CMP	AL,[FCBS]
;	JA	SHORT TRYXBAD
;	MOV	[KEEP],AL
;	JMP	COFF
;TRYXBAD:
;	JMP	BADOP

;------------------------------------------------------------------------------
; Bogus command
;------------------------------------------------------------------------------
;TRYZ:
;	JMP	BADOP

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
	MOV     CX,COUNT
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
; SYINIT1.ASM (MSDOS 3.3) - SYSINIT.ASM (MSDOS 2.0)
; ----------------------------------------------------------------------

ENDFILE:
	; 26/03/2018 - Retro DOS v2.0

        ;PUSH    CS
        ;POP     DS

	;;MOV	AX,[CONFBOT]
	;;MOV	[ALLOCLIM],AX

        CALL    ROUND
        MOV     AL,[FILES]
        SUB     AL,5
        ;JBE	SHORT DOFCBS
	JBE     SHORT DOBUFF
	;CBW
	XOR	AH,AH			; DO NOT USE CBW INSTRUCTION!!!!!
					;  IT DOES SIGN EXTEND.
        MOV     BX,[MEMLO]
        MOV     DX,[MEMHI]
        LDS     DI,[DOSINFO]            ; GET POINTER TO DOS DATA
        LDS     DI,[DI+SFT_ADDR]        ; DS:BP POINTS TO SFT
        MOV     [DI+SFT.SFT_LINK],BX
        MOV     [DI+SFT.SFT_LINK+2],DX ; SET POINTER TO NEW SFT
        PUSH    CS
        POP     DS
        LES     DI,[MEMLO]		; POINT TO NEW SFT
        MOV     WORD [ES:DI+SFT.SFT_LINK],-1
        MOV     [ES:DI+SFT.SFT_COUNT],AX
        MOV     BL,SF_ENTRY.size
        MUL     BL                      ; AX = NUMBER OF BYTES TO CLEAR
        MOV     CX,AX

	ADD     [MEMLO],AX		; ALLOCATE MEMORY
        MOV     AX,6
        ADD     [MEMLO],AX		; REMEMBER THE HEADER TOO
	CALL	ROUND			; Check for mem error before the STOSB
        ADD     DI,AX
        XOR     AX,AX
        REP     STOSB                   ; CLEAN OUT THE STUFF

;------------------------------------------------------------------------------
; Allocate Buffers
;------------------------------------------------------------------------------

; Search through the list of media supported and allocate 3 buffers if the
; capacity of the drive is > 360KB

DOBUFF: 
	; 26/03/2018

	CMP	BYTE [BUFFERS], -1	; Has buffers been already set?
	;je	SHORT DODEFAULTBUFF
	;jmp	SHORT DOBUFF2		; the user entered the buffers=.
	JNE	short DOBUFF2 ; 13/04/2018

DODEFAULTBUFF:
	MOV	BYTE [BUFFERS], 2	; Default to 2 buffers

; 10/04/2018

;	;PUSH	AX ; *?
;	;PUSH	DS ; *?
;	;LES	BP,CS:[DOSINFO] 	; Search through the DPB's
;	LES	BP,[DOSINFO] 
;	;LES	BP,ES:[BP+.SYSI_DPB]	; Get first DPB,
;	LES	BP,[ES:BP+DPBHEAD]
;
;	;PUSH	CS
;	;POP	DS
;
;NEXTDPB:
;	; Test if the drive supports removeable media
;	MOV	BL, [ES:BP+DPB.DRIVE]
;	INC	BL
;	;MOV	AX, (IOCTL SHL 8) OR 8
;	MOV	AX, (IOCTL*256) + 8
;	INT	21H
;
;; Ignore fixed disks
;	OR	AX, AX			; AX is nonzero if disk is nonremoveable
;	JNZ	SHORT NOSETBUF
;
;; Get parameters of drive
;	XOR	BX, BX
;	;MOV	BL, BYTE PTR ES:[BP.DPB_DRIVE]
;	MOV	BL, [ES:BP+DPB.DRIVE]
;	INC	BL
;	MOV	DX, DEVICEPARAMETERS
;	;MOV	AX, (IOCTL SHL 8) OR GENERIC_IOCTL
;	MOV	AX, (IOCTL*256) + GENERIC_IOCTL
;	;MOV	CX, (RAWIO SHL 8) OR GET_DEVICE_PARAMETERS
;	MOV	CX, (RAWIO*256) + GET_DEVICE_PARAMETERS
;	INT	21H
;	JC	SHORT NOSETBUF		; Get next DPB if driver doesn't support
;					; Generic IOCTL
;
;	; Determine capacity of drive
;	; Media Capacity = #Sectors * Bytes/Sector
;	;MOV	BX, WORD PTR DeviceParameters.DP_BPB.BPB_TotalSectors
;	MOV	BX, [DEVP.DP_BPB+A_BPB.BPB_TOTALSECTORS]
;
;; To keep the magnitude of the media capacity within a word,
;; scale the sector size
;; (ie. 1 -> 512 bytes, 2 -> 1024 bytes, ...)
;	;MOV	AX, WORD PTR DeviceParameters.DP_BPB.BPB_BytesPerSector
;	MOV	AX, [DEVP.DP_BPB+A_BPB.BPB_BYTESPERSECTOR]
;	XOR	DX, DX
;	MOV	CX, 512
;	DIV	CX				; Scale sector size in factor of
;						; 512 bytes
;
;	MUL	BX				; AX = #sectors * size factor
;	OR	DX, DX				; Just in case of LARGE floppies
;	JNZ	SHORT SETBUF
;	CMP	AX, 720 			; 720 Sectors * size factor of 1
;	JBE	SHORT NOSETBUF
;SETBUF:
;	MOV	BYTE [BUFFERS], 3
;	jmp	SHORT CHK_MEMSIZE_FOR_BUFFERS	; Check the mem. size for def. buff. count
;NOSETBUF:
;	CMP	WORD [ES:BP+DPB.NEXT_DPB],-1
;	JE	SHORT CHK_MEMSIZE_FOR_BUFFERS
;	LES	BP,[ES:BP+DPB.NEXT_DPB]
;	JMP	SHORT NEXTDPB

;From DOS 3.3, the default number of buffers will be changed according to the
;memory size too.						
; Default buffers = 2						
; If diskette Media > 360 kb, then default buffers = 3		
; If memory size > 128 kb (2000H para), then default buffers = 5
; If memory size > 256 kb (4000H para), then default buffers = 10
; If memory size > 512 kb (8000H para), then default buffers = 15.

CHK_MEMSIZE_FOR_BUFFERS:
	cmp	WORD [MEMORY_SIZE], 2000h ; 128KB
	jbe	SHORT BUFSET
	mov	BYTE [BUFFERS], 5
	cmp	WORD [MEMORY_SIZE], 4000h ; 256KB
	jbe	SHORT BUFSET
	mov	BYTE [BUFFERS], 10
	cmp	WORD [MEMORY_SIZE], 8000h ; 512KB
	jbe	SHORT BUFSET
	mov	BYTE [BUFFERS], 15

BUFSET:
	;POP	DS ; *?
	;POP	AX ; *?

DOBUFF2:
	CALL    ROUND
	
	DEC     BYTE [BUFFERS]
        JZ      SHORT BUF1

        PUSH    DS
        LES     DI,[BUFPTR]
        LDS     BX,[DOSINFO]

        MOV     AX,[BX+BUFFHEAD]
        ;MOV    [ES:DI+BUFFINFO.NEXTBUF],AX
       	MOV	[ES:DI],AX	; BUFFINFO.NEXTBUF
	MOV     AX,[BX+BUFFHEAD+2]
        ;MOV	[ES:DI+BUFFINFO.NEXTBUF+2],AX
        MOV     [ES:DI+2],AX	; BUFFINFO.NEXTBUF+2 

        MOV     [BX+BUFFHEAD],DI
        MOV     [BX+BUFFHEAD+2],ES

        MOV     WORD [ES:DI+BUFFINFO.BUFDRV],00FFH  ; NEW BUFFER FREE
        MOV     BX,[BX+MAXSEC]
        POP     DS

        ADD     BX,BUFINSIZ
        ADD     [MEMLO],BX

        JMP     SHORT DOBUFF2

BUF1:   
	CALL    ROUND
        MOV     BX,[MEMHI]
        MOV     AX,[AREA]
        MOV     ES,AX                   ; CALC WHAT WE NEEDED
        SUB     BX,AX

        MOV     AH,SETBLOCK
        INT     21H                     ; GIVE THE REST BACK

        PUSH    ES
        MOV     AX,ES
        DEC     AX
        MOV     ES,AX
        MOV     WORD [ES:ARENA.OWNER],8	; Set impossible owner
        POP     ES

	; 01/05/2018 (NOEXEC = FALSE)
	;IF      NOEXEC
        ;MOV     BX,0FFFFH		; ALLOCATE THE REST OF MEM FOR COMMAND
        ;MOV     AH,ALLOC
        ;INT     21H
        ;MOV     AH,ALLOC
        ;INT     21H
        ;MOV     DS,AX
        ;ENDIF

        RETN

NOCHAR1:
	STC
	RETN

ORGANIZE:
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

        CALL    GET                     ; GET CHARS TO RIGHT OF EQUALS SIGN
        STOSB

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


DELIM:  CMP     AL,' '
        JZ      SHORT GRET
        CMP     AL,9
        JZ      SHORT GRET
        CMP     AL,'='
        JZ      SHORT GRET
        CMP     AL,','
        JZ      SHORT GRET
        CMP     AL,';'
	; 26/03/2018
        JZ      SHORT GRET
	CMP	AL, '/'
	JZ	SHORT GRET
	CMP	AL, 0
        RETN

NOGET:  
	POP     CX
        MOV     [COUNT],DI
        XOR     SI,SI
        MOV     [CHRPTR],SI
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

ROUND:  
	; 26/03/2018
	;PUSH	AX
	MOV     AX,[CS:MEMLO]
	;CALL	PARAROUND
;ParaRound:
        ADD     AX,15
        ;RCR	AX,1
	SHR	AX,1 ; 10/04/2018
        SHR     AX,1
        SHR     AX,1
        SHR     AX,1
	; RETN
        ADD     [CS:MEMHI],AX
        XOR     AX,AX
        MOV     [CS:MEMLO],AX
	;MOV	AX,[CS:MEMHI]		; ax = new memhi
	;CMP	AX,[CS:ALLOCLIM]	; if new memhi >= alloclim, error
	;JAE	SHORT MEM_ER
	;POP	AX
	;CLC
        RETN

;MEM_ERR:
;	MOV	DX,BADMEM
;	;PUSH	CS
;	;HJPOP	DS
;	CALL	PRINT
;	JMP	STALL

CALLDEV:
	MOV     DS,[CS:ENTRY_POINT+2]
        ADD     BX,[CS:ENTRY_POINT]	; Do a little relocation
        MOV     AX,[DS:BX]
        PUSH    WORD [CS:ENTRY_POINT]
        MOV     WORD [CS:ENTRY_POINT],AX
        MOV     BX,PACKET
        CALL    NEAR [SS:ENTRY_POINT]	; 06/04/2018
        POP     WORD [CS:ENTRY_POINT]
        RETN

BADNUM: 
	POP     AX                      ; POP RETURN ADDRESS
        JMP	BADOP

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

GETNUM: 
	XOR     BX,BX                   ; running count is zero
B2:     CALL    TODIGIT                 ; do we have a digit
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
        OR      AL,AL                   ; end of line separator?
        JNZ     SHORT B2		; no, try as a valid character
        INC     WORD [COUNT]            ; one more character to scan
        DEC     WORD [CHRPTR]           ; back up over separator
B1:     
	MOV     AX,BX                   ; get proper count
        OR      AX,AX
        RETN
;
;       ES:SI POINTS TO FILE NAME (NUL TERMINATED)
;       DS:DX POINTS TO STRING TO OUTPUT IN FRONT OF NAME ($ TERM)
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

	; 10/06/2018  (NOEXEC = FALSE)

	;IF	NOEXEC
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
;	XOR     AX,AX			; OPEN THE FILE
;	MOV     AH,OPEN
;	STC				; IN CASE OF INT 24
;	INT     21H
;	POP     DX			; Trans addr is DS:DX
;	JC      SHORT LDRET
;
;	PUSH    ES			; READ THE FILE IN
;	POP     DS
;	MOV     BX,AX			; Handle in BX
;	MOV     CX,0FF00H
;	MOV     AH,READ
;	STC				; IN CASE OF INT 24
;	INT     21H
;	JC      SHORT LDRET
;	MOV     SI,DX			; CHECK FOR EXE FILE
;	CMP     WORD [SI],"MZ"
;	JNZ     SHORT LDCLS
;LDERR:  
;	STC
;	JMP     SHORT LDRET
;
;LDCLS:  
;	MOV     AH,CLOSE		; CLOSE THE FILE
;	STC
;	INT     21H
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
	DB      "MS-DOS version "
	DB      MAJOR_VERSION + "0"
	DB      "."
	DB      (MINOR_VERSION / 10) + "0"
	DB      (MINOR_VERSION % 10) + "0"
	DB      13,10
	DB      "Copyright 1981,82 Microsoft Corp.",13,10,"$"
	;ENDIF

NULDEV: 
	DB      "\DEV\NUL",0
CONDEV: 
	DB      "\DEV\CON",0
AUXDEV: 
	DB      "\DEV\AUX",0
PRNDEV: 
	DB      "\DEV\PRN",0

CONFIG:
	DB      "\CONFIG.SYS",0
COMMND:
	DB      "\COMMAND.COM",0

COMTAB:
        DB      7,"BUFFERS",'B'
        DB      5,"BREAK",'C'
        DB      5,"SHELL",'S'
        DB      6,"DEVICE",'D'
        DB      5,"FILES",'F'
        DB      8,"SWITCHAR",'W'
        DB      8,"AVAILDEV",'A'
	;DB	7,"COUNTRY",'Q'
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
; FORM: 	<BADCOUNTRY>		

BADCOUNTRY:
	DB	13,10,"Invalid country code or code page",13,10,"$" 

; 26/03/2018
; PRINTED when the attempt is made to install a block device which would
;   have a drive letter > 'Z'
;

BADBLOCK:
	DB	13,10,"Too many Block Devices",13,10,"$"

;SYSSIZE LABEL   BYTE

;SYSINITSEG      ENDS
;       END