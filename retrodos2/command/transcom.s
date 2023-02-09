; TRANSIENT PART OF COMMAND.COM
; ****************************************************************************
; COMMAND.COM (MSDOS 2.0 Command Interpreter) - RETRO DOS v2.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 01/05/2018
; ----------------------------------------------------------------------------
; Beginning: 15/02/2018 (COMMAND.COM v1.17) - 21/04/2018 (COMMAND.COM v2.11)
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm command2.s -l command2.lst -o COMMAND.COM)) 	
; ----------------------------------------------------------------------------
; Derived from 'COMMAND.ASM' file of MSDOS 2.11 (IBM PCDOS v2.11) source code
; by Microsoft, 18/08/1983
; ****************************************************************************
; Modified from 'COMMAND.S' (MSDOS 1.25 COMMAND.COM) source code
; in NASM syntax (by Erdogan Tan), 23/02/2018
; ----------------------------------------------------------------------------
;
;;============================================================================
; This MSDOS source code is verified & modified by using IDA Pro Disassembler
; (IBMDOS 2.10 COMMAND.COM) output in TASM syntax (21/04/2018..23/04/2018)
;;============================================================================
;
; COMMAND.COM v2.11 source files:
;      COMMAND.ASM (+ DOSYM.ASM,DEVSYM.ASM,COMSW.ASM,COMEQU.ASM,IFEQU.ASM)
;      RUCODE.ASM, RDATA.ASM, INIT.ASM, UINIT.ASM
;      TCODE.ASM, TCODE2.ASM, TCODE3.ASM, TCODE4.ASM, TCODE5.ASM,
;      TUCODE.ASM, COPY.ASM, COPYPROC.ASM, CPARSE.ASM, TDATA.ASM, TSPC.ASM
;
;=============================================================================
; COMLINK
;=============================================================================
;command rucode rdata init uinit tcode tcode2 tcode3 +
;tcode4 tcode5 tucode copy copyproc cparse tdata tspc;

; (command2.s -> COMTRANS: INCBIN "TRANSCOM.BIN") ; 21/04/2018

;
; This version of COMMAND is divided into three distinct parts.  First is the
; resident portion, which includes handlers for interrupts 22H (terminate),
; 23H (Cntrl-C), 24H (fatal error), and 27H (stay resident); it also has code
; to test and, if necessary, reload the transient portion.  Following the
; resident is the init code, which is overwritten after use.  Then comes the
; transient portion, which includes all command processing (whether internal
; or external).  The transient portion loads at the end of physical memory,
; and it may be overlayed by programs that need as much memory as possible.
; When the resident portion of command regains control from a user program, a
; checksum is performed on the transient portion to see if it must be
; reloaded.  Thus programs which do not need maximum memory will save the time
; required to reload COMMAND when they terminate.

;
; REV 1.17
;    05/19/82   Fixed bug in BADEXE error (relocation error must return to
;               resident since the EXELOAD may have overwritten the transient.
; REV 1.18
;    05/21/82   IBM version always looks on drive A
;               MSVER always looks on default drive
;
; REV 1.19
;    06/03/82   Drive spec now entered in command line
;    06/07/82   Added VER command (print DOS version number) and VOL command
;               (print volume label)
; REV 1.20
;    06/09/82   Prints "directory" after directories
;    06/13/82   MKDIR, CHDIR, PWD, RMDIR added
; REV 1.50
;               Some code for new 2.0 DOS, sort of HACKey. Not enough time to
;               do it right.
; REV 1.70
;               EXEC used to fork off new processes
; REV 1.80
;               C switch for single command execution
; REV 1.90
;               Batch uses XENIX
; Rev 2.00
;               Lots of neato stuff
;               IBM 2.00 level
; Rev 2.01
;               'D' switch for date time suppression
; Rev 2.02
;               Default userpath is NUL rather than BIN
;                       same as IBM
;               COMMAND split into pieces
; Rev 2.10
;               INTERNATIONAL SUPPORT
; Rev 2.11      COMMAND split into more pieces

; ----------------------------------------------------------------------------

;	INCLUDE DOSSYM.ASM
;	INCLUDE DEVSYM.ASM
;	INCLUDE COMSW.ASM
;	INCLUDE COMEQU.ASM

; ----------------------------------------------------------------------------

;=============================================================================
; DOSSYM.ASM
;=============================================================================

;<Control character definitions>

c_DEL       EQU     7Fh         ;    ASCII rubout or delete previous char
c_BS        EQU     08h         ; ^H ASCII backspace
c_CR        EQU     0Dh         ; ^M ASCII carriage return
c_LF        EQU     0Ah         ; ^J ASCII linefeed
c_ETB       EQU     17h         ; ^W ASCII end of transmission
c_NAK       EQU     15h         ; ^U ASCII negative acknowledge
c_ETX       EQU     03h         ; ^C ASCII end of text
c_HT        EQU     09h         ; ^I ASCII tab

;<BPB Definition>

;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
; Certain structures, constants and system  calls  below  are  private  to ;
; the DOS  and  are  extremely  version-dependent.  They may change at any ;
; time at  the  implementors'  whim.   As  a  result,  they  must  not  be ;
; documented to  the  general  public.   If  an  extreme case arises, they ;
; must be documented with this warning.                                    ;
;                                                                          ;
; Those structures and constants that are subject to  the  above  will  be ;
; marked and bracketed with the flag:                                      ;
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; <Bios Parameter Block>
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;

; Bios Parameter Block definition
; This structure is used to build a full DPB

struc BPBLOCK
.SECSZ: resw 1		; Size in bytes of physical sector
.CLUS:  resb 1		; Sectors/Alloc unit
.RES:   resw 1		; Number of reserved sectors
.FTCNT: resb 1		; Number of FATs
.DRCNT: resw 1		; Number of directory entries
.SCCNT: resw 1		; Total number of sectors
.MEDIA: resb 1		; Media descriptor byte
.FTSEC:	resw 1		; Number of sectors taken up by one FAT
.size:
endstruc
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; <Disk I/O Buffer Header>
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;

; Field definition for I/O buffer information

struc BUFFINFO
.NEXTBUF:	resd 1		; Pointer to next buffer in list
; The next two items are often refed as a word
.BUFDRV:	resb 1		; Logical drive # assoc with buffer FF = free
.BUFDIRTY:	resb 1		; Dirty flag
.BUFPRI:	resb 1		; Buffer selection priority (see EQUs below)
.VISIT:		resb 1		; Visit flag for buffer pool scans
.BUFSECNO:	resw 1		; Sector number of buffer
; The next two items are often refed as a word
.BUFWRTCNT:	resb 1		; For FAT sectors, # times sector written out
.BUFWRTINC:	resb 1		; "   "     "   , # sectors between each write
.BUFDRVDP:	resd 1		; Pointer to drive parameters
.size:
endstruc

BUFINSIZ        EQU     BUFFINFO.size

                                ; Size of structure in bytes
FREEPRI EQU     0
LBRPRI  EQU     2               ; Last byte of buffer read
LBWPRI  EQU     4               ; Last byte written
RPRI    EQU     6               ; Read but not last byte
WPRI    EQU     8               ; Written but not last byte
DIRPRI  EQU     15              ; Directory Sector
FATPRI  EQU     30              ; FAT sector
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; <User stack inside of system call>
; Location of user registers relative user stack pointer

struc	user_env   ; user_environ
.user_AX: resw 1
.user_BX: resw 1
.user_CX: resw 1
.user_DX: resw 1
.user_SI: resw 1
.user_DI: resw 1
.user_BP: resw 1
.user_DS: resw 1
.user_ES: resw 1
.user_IP: resw 1
.user_CS: resw 1
.user_F:  resw 1
.size:
endstruc

; <interrupt definitions>

INTTAB          EQU     20H
INTBASE         EQU     4 * INTTAB
ENTRYPOINT      EQU     INTBASE+40H

;	IF      ALTVECT
;ALTTAB  EQU     0F0H
;ALTBASE EQU     4 * ALTTAB
;	ENDIF

;
; interrupt assignments
;
;	IF	NOT ALTVECT
int_abort           EQU     INTTAB          ; abort process
int_command         EQU     int_abort+1     ; call MSDOS
int_terminate       EQU     int_abort+2     ; int to terminate address
int_ctrl_c          EQU     int_abort+3     ; ^c trapper
int_fatal_abort     EQU     int_abort+4     ; hard disk error
int_disk_read       EQU     int_abort+5     ; logical sector disk read
int_disk_write      EQU     int_abort+6     ; logical sector disk write
int_keep_process    EQU     int_abort+7     ; terminate program and stay resident
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
int_spooler         EQU     int_abort+8     ; spooler call
int_fastcon         EQU     int_abort+9     ; fast CON interrupt
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
        ;ELSE
;int_abort          EQU     INTTAB          ; abort process
;int_command        EQU     int_abort+1     ; call MSDOS
;int_terminate      EQU     ALTTAB          ; int to terminate address
;int_ctrl_c         EQU     int_terminate+1 ; ^c trapper
;int_fatal_abort    EQU     int_terminate+2 ; hard disk error
;int_disk_read      EQU     int_abort+5     ; logical sector disk read
;int_disk_write     EQU     int_abort+6     ; logical sector disk write
;int_keep_process   EQU     int_abort+7     ; terminate program and stay resident
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
;int_spooler        EQU     int_terminate+3 ; spooler call
;int_fastcon        EQU     int_abort+9     ; fast CON interrupt
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
        ;ENDIF

addr_int_abort          EQU    4 * int_abort
addr_int_command        EQU    4 * int_command
addr_int_terminate      EQU    4 * int_terminate
addr_int_ctrl_c         EQU    4 * int_ctrl_c
addr_int_fatal_abort    EQU    4 * int_fatal_abort
addr_int_disk_read      EQU    4 * int_disk_read
addr_int_disk_write     EQU    4 * int_disk_write
addr_int_keep_process   EQU    4 * int_keep_process
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
addr_int_spooler        EQU    4 * int_spooler
addr_int_fastcon        EQU    4 * int_fastcon
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; <Disk map>
;  MSDOS partitions the disk into 4 sections:
;
;  phys sector 0:   +-------------------+
;       |           | boot/reserved     |
;       |           +-------------------+
;       |           |  File allocation  |
;       v           |      table(s)     |
;                   |  (multiple copies |
;                   |     are kept)     |
;                   +-------------------+
;                   |     Directory     |
;                   +-------------------+
;                   |     File space    |
;                   +-------------------+
;                   |   Unaddressable   |
;                   |  (to end of disk) |
;                   +-------------------+
;
; All partition boundaries are sector boundaries.  The size of the FAT is
; adjusted to maximize the file space addressable.

; BREAK <Directory entry>

;
;       +---------------------------+
;       |  (12 BYTE) filename/ext   |       0       0
;       +---------------------------+
;       |     (BYTE) attributes     |       11      B
;       +---------------------------+
;       |    (10 BYTE) reserved     |       12      C
;       +---------------------------+
;       | (WORD) time of last write |       22      16
;       +---------------------------+
;       | (WORD) date of last write |       24      18
;       +---------------------------+
;       |   (WORD) First cluster    |       26      1A
;       +---------------------------+
;       |     (DWORD) file size     |       28      1C
;       +---------------------------+
;
;   First byte of filename  = E5 -> free directory entry
;                           = 00 -> end of allocated directory
;   Time:   Bits 0-4=seconds/2, bits 5-10=minute, 11-15=hour
;   Date:   Bits 0-4=day, bits 5-8=month, bits 9-15=year-1980
;
struc dir_entry
.dir_name:	resb 11			; file name
.dir_attr:	resb 1			; attribute bits
.dir_pad:	resb 10			; reserved for expansion
.dir_time:	resw 1			; time of last write
.dir_date:	resw 1			; date of last write
.dir_first:	resw 1			; first allocation unit of file
.dir_size_l:	resw 1			; low 16 bits of file size
.dir_size_h:	resw 1			; high 16 bits of file size
.size:
endstruc

attr_read_only      EQU      1h
attr_hidden         EQU      2h
attr_system         EQU      4h
attr_volume_id      EQU      8h
attr_directory      EQU     10h
attr_archive        EQU     20h

attr_all            EQU     attr_hidden+attr_system+attr_directory
                                        ; OR of hard attributes for FINDENTRY

attr_ignore         EQU     attr_read_only+attr_archive
                                        ; ignore this(ese) attribute(s)
                                        ; during search first/next

attr_changeable     EQU     attr_read_only+attr_hidden+attr_system+attr_archive
                                        ; changeable via CHMOD

; <File allocation Table information>
;
; The File Allocation Table uses a 12-bit entry for each allocation unit on the
; disk.  These entries are packed, two for every three bytes.  The contents of
; entry number N is found by 1) multiplying N by 1.5; 2) adding the result to
; the base address of the Allocation Table; 3) fetching the 16-bit word at this
; address; 4) If N was odd (so that N*1.5 was not an integer), shift the word
; right four bits; 5) mask to 12 bits (AND with 0FFF hex).  Entry number zero
; is used as an end-of-file trap in the OS and is passed to the BIOS to help
; determine disk format.  Entry 1 is reserved for future use.  The first
; available allocation unit is assigned entry number two, and even though it is
; the first, is called cluster 2.  Entries greater than 0FF8H are end of file
; marks; entries of zero are unallocated.  Otherwise, the contents of a FAT
; entry is the number of the next cluster in the file.
;
; Clusters with bad sectors are tagged with FF7H.  Any non-zero number would do
; because these clusters show as allocated, but are not part of any allocation
; chain and thus will never be allocated to a file.  A particular number is
; selected so that disk checking programs know what to do (ie.  a cluster with
; entry FF7H which is not in a chain is not an error).

; BREAK <DPB structure>
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;

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

;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; BREAK <File Control Block definition>
;
; Field definition for FCBs
; The FCB has the following structure:
;
;       +---------------------------+
;       |   Drive indicator(byte)   |
;       +---------------------------+
;       |    Filename (8 chars)     |
;       +---------------------------+
;       |    Extension (3 chars)    |
;       +---------------------------+
;       |   Current Extent(word)    |
;       +---------------------------+
;       |    Record size (word)     |
;       +---------------------------+
;       |    File Size (2 words)    |
;       +---------------------------+
;       |       Date of write       |
;       +---------------------------+
;       |       Time of write       |
;       +---------------------------+
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
;       | Flags:                    |
;       |  bit 7=0 file/1 device    |
;       |  bit 6=0 if dirty         |
;       |  bits 0-5 deviceid        |
;       +---------------------------+
;       |   first cluster in file   |
;       +---------------------------+
;       | position of last cluster  |
;       +---------------------------+
;       |   last cluster accessed   |   12 bit-+--- packed in 3 bytes
;       +---------------------------+          |
;       |     parent directory      |   <------+
;       +---------------------------+
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;       |    next record number     |
;       +---------------------------+
;       |   random record number    |
;       +---------------------------+
;

struc	SYS_FCB
.drive:	resb 1
.name:	resb 8
.ext:	resb 3
.EXTENT: resw 1
.RECSIZ: resw 1	; Size of record (user settable)
.FILSIZ: resw 1	; Size of file in bytes; used with the following
                        ; word
.DRVBP:	resw 1	; BP for SEARCH FIRST and SEARCH NEXT
.FDATE:	resw 1	; Date of last writing
.FTIME:	resw 1	; Time of last writing
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
.DEVID:	resb 1	; Device ID number, bits 0-5 if file.
                        ; bit 7=0 for file, bit 7=1 for I/O device
                        ; If file, bit 6=0 if dirty
                        ; If I/O device, bit 6=0 if EOF (input)
                        ;               Bit 5=1 if Raw mode
                        ;               Bit 0=1 if console input device
                        ;               Bit 1=1 if console output device
                        ;               Bit 2=1 if null device
                        ;               Bit 3=1 if clock device
.FIRCLUS: resw 1	; First cluster of file
.CLUSPOS: resw 1	; Position of last cluster accessed
.LSTCLUS: resw 1	; Last cluster accessed and directory
          resb 1	; pack 2 12 bit numbers into 24 bits...
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
.NR:	resb 1	; Next record
.RR:	resb 4	; Random record
.size:
endstruc

FILDIRENT       EQU SYS_FCB.FILSIZ	; Used only by SEARCH FIRST and
                                        ; SEARCH NEXT

;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
devid_file_clean        EQU     40h     ; true if file and not written
devid_file_mask_drive   EQU     3Fh     ; mask for drive number

devid_device            EQU     80h     ; true if a device
devid_device_EOF        EQU     40h     ; true if end of file reached
devid_device_raw        EQU     20h     ; true if in raw mode
devid_device_special    EQU     10h     ; true if special device
devid_device_clock      EQU     08h     ; true if clock device
devid_device_null       EQU     04h     ; true if null device
devid_device_con_out    EQU     02h     ; true if console output
devid_device_con_in     EQU     01h     ; true if consle input

;
; structure of devid field as returned by IOCTL is:
;
;       BIT     7   6   5   4   3   2   1   0
;             |---|---|---|---|---|---|---|---|
;             | I | E | R | S | I | I | I | I |
;             | S | O | A | P | S | S | S | S |
;             | D | F | W | E | C | N | C | C |
;             | E |   |   | C | L | U | O | I |
;             | V |   |   | L | K | L | T | N |
;             |---|---|---|---|---|---|---|---|
;       ISDEV = 1 if this channel is a device
;             = 0 if this channel is a disk file
;
;       If ISDEV = 1
;
;             EOF = 0 if End Of File on input
;             RAW = 1 if this device is in Raw mode
;                 = 0 if this device is cooked
;             ISCLK = 1 if this device is the clock device
;             ISNUL = 1 if this device is the null device
;             ISCOT = 1 if this device is the console output
;             ISCIN = 1 if this device is the console input
;
;       If ISDEV = 0
;             EOF = 0 if channel has been written
;             Bits 0-5  are  the  block  device  number  for
;                 the channel (0 = A, 1 = B, ...)
;
devid_ISDEV     EQU     80h
devid_EOF       EQU     40h
devid_RAW       EQU     20h
devid_SPECIAL   EQU     10H
devid_ISCLK     EQU     08h
devid_ISNUL     EQU     04h
devid_ISCOT     EQU     02h
devid_ISCIN     EQU     01h

devid_block_dev EQU     1Fh             ; mask for block device number

;
; find first/next buffer
;
struc	find_buf
.sattr:	   resb 1	; attribute of search
.drive:	   resb 1	; drive of search
.name:	   resb 11	; formatted name
.LastEnt:  resw 1	; LastEnt
.ThisDPB:  resd 1	; This DPB
.DirStart: resw 1	; DirStart
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

.attr:	 resb 1		; attribute found
.time:   resb 1		; time
.date:   resb 1		; date
.size_l: resw 1		; low(size)
.size_h: resw 1		; high(size)
.pname:	 resb 13	; packed name
.size:
endstruc

; BREAK <Process data block>
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
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
.PARENT_PID:	resw 1		; PID of parent (terminate PID)
.JFN_TABLE:     resb FILPERPROC ; indices into system table
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
.ENVIRON:	resw 1		; seg addr of environment
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
.USER_STACK:	resd 1		; stack of self during system calls
.PAD1:		resb 30 ; 1Eh
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
.CALL_SYSTEM:	resb 5		; portable method of system call
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
.PAD2:		resb 6
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
endstruc

; BREAK <EXEC and EXE file structures>
;
; EXEC arg block - load/go program
;

;
; The following get used as arguments to the EXEC system call.  They indicate
; whether or not the program is executed or whether or not a program header
; gets created.
;
exec_func_no_execute    EQU 1           ; no execute bit
exec_func_overlay       EQU 2           ; overlay bit

struc EXEC0
.ENVIRON:	resw 1		; seg addr of environment
.COM_LINE:	resd 1		; pointer to asciz command line
.5C_FCB:	resd 1		; default fcb at 5C
.6C_FCB:	resd 1		; default fcb at 6C
.size:
endstruc

struc EXEC1
.ENVIRON:	resw 1		; seg addr of environment
.COM_LINE:	resd 1		; pointer to asciz command line
.5C_FCB:	resd 1		; default fcb at 5C
.6C_FCB:	resd 1		; default fcb at 6C
.SP:		resw 1		; stack pointer of program
.SS:		resw 1		; stack seg register of program
.IP:		resw 1		; entry point IP
.CS:		resw 1		; entry point CS
.size:
endstruc

struc EXEC3
.load_addr:	resw 1		; seg address of load point
.reloc_fac:	resw 1		; relocation factor
endstruc

;
; Exit codes in upper byte
;
Exit_Terminate      EQU     0
Exit_Abort          EQU     0
Exit_Ctrl_C         EQU     1
Exit_Hard_Error     EQU     2
Exit_Keep_Process   EQU     3

;
; EXE file header
;

struc EXE
.signature:   resw 1		; must contain 4D5A (yay zibo!)
.len_mod_512: resw 1		; low 9 bits of length
.pages:       resw 1		; number of 512b pages in file
.rle_count:   resw 1		; count of reloc entries
.par_dir:     resw 1		; number of paragraphs before image
.min_BSS:     resw 1		; minimum number of para of BSS
.max_BSS:     resw 1		; max number of para of BSS
.SS:          resw 1		; stack of image
.SP:          resw 1		; SP of image
.chksum:      resw 1		; checksum of file (ignored)
.IP:          resw 1		; IP of entry
.CS:          resw 1		; CS of entry
.rle_table:   resw 1		; byte offset of reloc table
.iov:         resw 1		; overlay number (0 for root)
.sym_tab:     resd 1		; offset of symbol table in file
.size:
endstruc

exe_valid_signature     EQU 5A4Dh
exe_valid_old_signature EQU 4D5Ah

struc symbol_entry
.value:	resd 1
.type:	resw 1
.len:	resb 1
.name:	resb 255
endstruc

; <Internal system file table format>
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
;
; system file table
;

struc	SFT
.SFT_LINK:	resd 1
.SFT_COUNT:	resw 1		; number of entries
.SFT_TABLE:	resw 1		; beginning of array of the following
.size:
endstruc

;
; system file table entry
;

struc	SF_ENTRY
.sf_ref_count:	resb 1		; number of processes sharing fcb
.sf_mode:	resb 1		; mode of access
.sf_attr:	resb 1		; attribute of file
.sf_fcb:	resb SYS_FCB.size  ; actual FCB
.size:
endstruc

sf_default_number   EQU     5h
; 19/04/2018
sf_entry_size equ SF_ENTRY.size ; 40

;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; <Memory arena structure>
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
;
; arena item
;

struc ARENA
.SIGNATURE:	resb 1		; 4D for valid item, 5A for last item
.OWNER:		resw 1		; owner of arena item
.SIZE:		resw 1		; size in paragraphs of item
endstruc

arena_owner_system  EQU 0               ; free block indication

arena_signature_normal  EQU 4Dh         ; valid signature, not end of arena
arena_signature_end     EQU 5Ah         ; valid signature, last block in arena
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;

; <Machine instruction definitions>

mi_INT          EQU     0CDh
mi_Long_JMP     EQU     0EAh
mi_Long_CALL    EQU     09Ah
mi_Long_RET     EQU     0CBh

; <Standard I/O assignments>

stdin       EQU     0
stdout      EQU     1
stderr      EQU     2
stdaux      EQU     3
stdprn      EQU     4

; <Xenix subfunction assignments>

open_for_read   EQU 0
open_for_write  EQU 1
open_for_both   EQU 2

; <Xenix error codes>

;
; XENIX calls all return error codes through AX.  If an error occurred then the
; carry bit will be set and the error code is in AX.  If no error occurred then
; the carry bit is reset and AX contains returned info.
;

no_error_occurred		EQU     0

error_invalid_function          EQU     1
error_file_not_found            EQU     2
error_path_not_found            EQU     3
error_too_many_open_files       EQU     4
error_access_denied             EQU     5
error_invalid_handle            EQU     6
error_arena_trashed             EQU     7
error_not_enough_memory         EQU     8
error_invalid_block             EQU     9
error_bad_environment           EQU     10
error_bad_format                EQU     11
error_invalid_access            EQU     12
error_invalid_data              EQU     13
;**** unused                    EQU     14
error_invalid_drive             EQU     15
error_current_directory         EQU     16
error_not_same_device           EQU     17
error_no_more_files             EQU     18

country_not_found               EQU     error_file_not_found ; DOSSYM_V211.ASM
alloc_not_enough_memory         EQU     error_not_enough_memory
alloc_arena_trashed             EQU     error_arena_trashed

close_invalid_handle            EQU     error_invalid_handle
close_invalid_function          EQU     error_invalid_function

chdir_path_not_found            EQU     error_path_not_found

chmod_path_not_found            EQU     error_path_not_found
chmod_access_denied             EQU     error_access_denied
chmod_invalid_function          EQU     error_invalid_function

creat_access_denied             EQU     error_access_denied
creat_path_not_found            EQU     error_path_not_found
creat_too_many_open_files       EQU     error_too_many_open_files

curdir_invalid_drive            EQU     error_invalid_drive

dealloc_invalid_block           EQU     error_invalid_block
dealloc_arena_trashed           EQU     error_arena_trashed

dup_invalid_handle              EQU     error_invalid_handle
dup_too_many_open_files         EQU     error_too_many_open_files

dup2_invalid_handle             EQU     error_invalid_handle

exec_invalid_function           EQU     error_invalid_function
exec_bad_environment            EQU     error_bad_environment
exec_bad_format                 EQU     error_bad_format
exec_not_enough_memory          EQU     error_not_enough_memory
exec_file_not_found             EQU     error_file_not_found

filetimes_invalid_function      EQU     error_invalid_function
filetimes_invalid_handle        EQU     error_invalid_handle

findfirst_file_not_found        EQU     error_file_not_found
findfirst_no_more_files         EQU     error_no_more_files
findnext_no_more_files          EQU     error_no_more_files

international_invalid_function  EQU     error_invalid_function

ioctl_invalid_handle            EQU     error_invalid_handle
ioctl_invalid_function          EQU     error_invalid_function
ioctl_invalid_data              EQU     error_invalid_data

lseek_invalid_handle            EQU     error_invalid_handle
lseek_invalid_function          EQU     error_invalid_function

mkdir_path_not_found            EQU     error_path_not_found
mkdir_access_denied             EQU     error_access_denied

open_invalid_access             EQU     error_invalid_access
open_file_not_found             EQU     error_file_not_found
open_access_denied              EQU     error_access_denied
open_too_many_open_files        EQU     error_too_many_open_files

read_invalid_handle             EQU     error_invalid_handle
read_access_denied              EQU     error_access_denied

rename_file_not_found           EQU     error_file_not_found
rename_not_same_device          EQU     error_not_same_device
rename_access_denied            EQU     error_access_denied

rmdir_path_not_found            EQU     error_path_not_found
rmdir_access_denied             EQU     error_access_denied
rmdir_current_directory         EQU     error_current_directory

setblock_invalid_block          EQU     error_invalid_block
setblock_arena_trashed          EQU     error_arena_trashed
setblock_not_enough_memory      EQU     error_not_enough_memory
setblock_invalid_function       EQU     error_invalid_function

unlink_file_not_found           EQU     error_file_not_found
unlink_access_denied            EQU     error_access_denied

write_invalid_handle            EQU     error_invalid_handle
write_access_denied             EQU     error_access_denied

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
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
GET_DEFAULT_DPB                 EQU 31  ; 31     1F
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
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
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
GET_DPB                         EQU 50  ; 50     32
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
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
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
SET_CURRENT_PDB                 EQU 80  ; 80     50
GET_CURRENT_PDB                 EQU 81  ; 81     51
GET_IN_VARS                     EQU 82  ; 82     52
SETDPB                          EQU 83  ; 83     53
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
GET_VERIFY_ON_WRITE             EQU 84  ; 84     54
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
DUP_PDB                         EQU 85  ; 85     55
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
RENAME                          EQU 86  ; 86     56
FILE_TIMES                      EQU 87  ; 87     57

SET_OEM_HANDLER			EQU 248 ; 248    F8
;OEM_C1				EQU 249 ; 249    F9
;OEM_C2				EQU 250 ; 250    FA
;OEM_C3				EQU 251 ; 251    FB
;OEM_C4				EQU 252 ; 252    FC
;OEM_C5				EQU 253 ; 253    FD
;OEM_C6				EQU 254 ; 254    FE
;OEM_C7				EQU 255 ; 255    FF

;============================================================================
; DOSSYM_V211.ASM
;============================================================================

DOS_MAJOR_VERSION   EQU      2
DOS_MINOR_VERSION   EQU     11

;
; Current structure of the data returned by the international call
;

struc	INTERNAT_BLOCK
.Date_tim_format:
		RESW 1		; 0-USA, 1-EUR, 2-JAP
.Currency_sym:
		RESB 5		; Currency Symbol 5 bytes
.Thous_sep:
		RESB 2		; Thousands separator 2 bytes
.Decimal_sep:
		RESB 2		; Decimal separator 2 bytes
.Date_sep:
		RESB 2		; Date separator 2 bytes
.Time_sep:
		RESB 2		; Time separator 2 bytes
.Bit_field:	
		RESB 1		; Bit values
                                   ;   Bit 0 = 0 if currency symbol first
                                   ;         = 1 if currency symbol last
                                   ;   Bit 1 = 0 if No space after currency symbol
                                   ;         = 1 if space after currency symbol
.Currency_cents:
		RESB 	1	; Number of places after currency dec point
.Time_24:
		RESB 	1	; 1 if 24 hour time, 0 if 12 hour time
.Map_call:
		RESW	1	; Address of case mapping call (DWORD)
                RESW	1       ; THIS IS TWO WORDS SO IT CAN BE INITIALIZED
				;  in pieces.
.Data_sep:
		RESB	2	; Data list separator character
.size:		
endstruc

; Max size of the block returned by the INTERNATIONAL call
;
internat_block_max      EQU     32

;============================================================================
; DEVSYM.ASM
;============================================================================

;SUBTTL DEVICE TABLE AND SRH DEFINITION
;PAGE

; The device table list has the form:
struc	SYSDEV
.NEXT:		resd 1		;Pointer to next device header
.ATT:		resw 1		;Attributes of the device
.STRAT:		resw 1		;Strategy entry point
.INT:		resw 1		;Interrupt entry point
.NAME:		resb 8		;Name of device (only first byte used for block)
.size:
endstruc

;Attribute bit masks
DEVTYP  EQU     8000H           ;Bit 15 - 1  if Char, 0 if block
DEVIOCTL EQU    4000H           ;Bit 14 - CONTROL mode bit
ISFATBYDEV EQU  2000H           ;Bit 13 - Device uses FAT ID bytes, comp media.
ISCIN   EQU     0001H           ;Bit 0 - This device is the console input.
ISCOUT  EQU     0002H           ;Bit 1 - This device is the console output.
ISNULL  EQU     0004H           ;Bit 2 - This device is the null device.
ISCLOCK EQU     0008H           ;Bit 3 - This device is the clock device.
;ISIBM	EQU     0010H		;Bit 4 - This device is special
ISSPEC	EQU     0010H		;Bit 4 - This device is special ; 15/03/2018

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

;=============================================================================
; COMSW.ASM
;=============================================================================

;; Use the following booleans to set assembly flags
;FALSE   EQU	0
;TRUE    EQU	~FALSE	; NOT FALSE

;IBMVER  EQU	TRUE  	; Switch to build IBM version of Command
;IBM     EQU	IBMVER
;MSVER   EQU	FALSE	; Switch to build MS-DOS version of Command

;HIGHMEM EQU	FALSE   ; Run resident part above transient (high memory)
;KANJI   EQU	FALSE	; Support for dual byte Microsoft KANJI standard
;IBMJAPAN EQU	FALSE   ; MUST BE TRUE (along with IBM and KANJI)

;=============================================================================
; COMEQU.ASM
;=============================================================================

; COMMAND EQUs which are not switch dependant

;IFDEF	IBM
;	INCLUDE IFEQU.ASM
;ENDIF

SYM	EQU	">"

LINPERPAG	EQU     23
NORMPERLIN	EQU     1
WIDEPERLIN	EQU     5
COMBUFLEN	EQU     128     ; Length of commmand buffer

DRVCHAR		EQU     ":"

FCB	EQU	5CH

struc VARSTRUC
.ISDIR:	resb 1
.SIZ:	resb 1
.TTAIL:	resw 1
.INFO:	resb 1
.BUF:	resb DIRSTRLEN + 20
.size:
endstruc

WSWITCH EQU     1               ; Wide display during DIR
PSWITCH EQU     2               ; Pause (or Page) mode during DIR
ASWITCH EQU     4               ; ASCII mode during COPY
BSWITCH EQU     8               ; Binary mode during COPY
VSWITCH EQU     10H             ; Verify switch
GOTSWITCH EQU   8000H		; Meta switch set if switch char encountered

; ----------------------------------------------------------------------------
; IFEQU.ASM
; ----------------------------------------------------------------------------

; COMMAND EQUs which are switch dependant

;IF1
;    IF	IBM
;	%OUT IBM version
;    ELSE
;	%OUT Normal version
;    ENDIF
;
;    IF	HIGHMEM
;	%OUT Highmem version
;    ENDIF
;
;    IF	KANJI
;	%OUT Kanji version
;    ENDIF
;ENDIF

;=============================================================================
; COMMAND.ASM
;=============================================================================

;CODERES SEGMENT PUBLIC
;CODERES ENDS

;DATARES SEGMENT PUBLIC BYTE
;        EXTRN   COMBAD:BYTE,NEEDCOM:BYTE,DRVMSG:BYTE
;        EXTRN   DEFMSG:BYTE,PROMPT:BYTE,EXECEMES:BYTE,EXEBAD:BYTE
;        EXTRN   TOOBIG:BYTE,NOCOM:BYTE,RBADNAM:BYTE,INT_2E_RET:DWORD
;        EXTRN   NOHANDMES:BYTE,BMEMMES:BYTE,HALTMES:BYTE,FRETMES:BYTE
;        EXTRN   PARENT:WORD,HANDLE01:WORD,LOADING:BYTE,BATCH:WORD
;        EXTRN   TRNSEG:WORD,COMDRV:BYTE,MEMSIZ:WORD,SUM:WORD,EXTCOM:BYTE
;        EXTRN   IO_SAVE:WORD,PERMCOM:BYTE,SINGLECOM:WORD,VERVAL:WORD
;        EXTRN   PIPEFLAG:BYTE,SAVE_PDB:WORD,COMSPEC:BYTE,TRANS:WORD
;        EXTRN   TRANVARS:BYTE,LTPA:WORD,RSWITCHAR:BYTE,RDIRCHAR:BYTE
;        EXTRN   RETCODE:WORD,FORFLAG:BYTE
;
;        IF      IBMVER
;        EXTRN   SYS_CALL:DWORD,ZEXEC:WORD,EXESEG:WORD,EXESUM:WORD
;        EXTRN   USER_SS:WORD,USER_SP:WORD
;        ENDIF
;
;DATARES ENDS

;ENVIRONMENT SEGMENT PUBLIC PARA        ; Default COMMAND environment
;ENVIRONMENT ENDS

;INIT    SEGMENT PUBLIC PARA
;        EXTRN   CONPROC:NEAR
;INIT    ENDS

;TAIL    SEGMENT PUBLIC PARA
;TAIL    ENDS

;TRANCODE        SEGMENT PUBLIC PARA
;TRANCODE        ENDS

;TRANDATA        SEGMENT PUBLIC BYTE
;        EXTRN   TRANDATAEND:BYTE
;TRANDATA        ENDS

;TRANSPACE       SEGMENT PUBLIC BYTE
;        EXTRN   TRANSPACEEND:BYTE,HEADCALL:DWORD
;TRANSPACE       ENDS

;TRANTAIL        SEGMENT PUBLIC PARA
;TRANTAIL        ENDS

;ZEXEC_CODE      SEGMENT PUBLIC PARA
;ZEXEC_CODE      ENDS

;ZEXEC_DATA      SEGMENT PUBLIC BYTE
;ZEXEC_DATA      ENDS

;RESGROUP        GROUP   CODERES,DATARES,ENVIRONMENT,INIT,TAIL
;TRANGROUP       GROUP   TRANCODE,TRANDATA,TRANSPACE,TRANTAIL
;EGROUP          GROUP   ZEXEC_CODE,ZEXEC_DATA

;ENVIRONMENT SEGMENT PUBLIC PARA	; Default COMMAND environment

;	PUBLIC  ECOMSPEC,ENVIREND,PATHSTRING

;        ORG     0
;ENVARENA DB     10H DUP (?)     ; Pad for mem arena
;PATHSTRING DB   "PATH="
;USERPATH LABEL  BYTE

;        DB      0               ; Null path
;        DB      "COMSPEC="
;ECOMSPEC DB     "/COMMAND.COM"
;        DB      134 DUP (0)

;ENVIREND        LABEL   BYTE

;ENVIRONSIZ EQU  $-PATHSTRING
;ENVIRONSIZ2 EQU $-ECOMSPEC
;ENVIRONMENT ENDS

;=============================================================================
; COMMAND.COM Resident Portion Addresses - 29/04/2018
;=============================================================================

; Transient portion loader code offset in resident code segment
LODCOM1	equ	02CFh  ; 30/04/2018

; Note: Following offset addresses are in data section of 'command2.s' file.
; They must be verified by using assembly list (command2.lst) of COMMAND.COM.  

RETRABASE equ 0827h ; 30/04/2018 (Base address for following common params.)	

; RESGROUP       [RESSEG]
; DATA		  offset
; --------     	------------
PARENT	  equ	RETRABASE+6	; 082Dh
BATCH	  equ	RETRABASE+13	; 0834h
BATLOC	  equ	RETRABASE+15	; 0836h
BATBYT	  equ	RETRABASE+64	; 0867h
EXTCOM	  equ	RETRABASE+69	; 086Ch
RETCODE	  equ	RETRABASE+70	; 086Dh
ECHOFLAG  equ	RETRABASE+72	; 086Fh
IO_SAVE	  equ	RETRABASE+73	; 0870h
RESTDIR	  equ	RETRABASE+75	; 0872h
PERMCOM	  equ	RETRABASE+76	; 0873h
SINGLECOM equ	RETRABASE+77	; 0874h
VERVAL	  equ	RETRABASE+79	; 0876h
FORFLAG	  equ	RETRABASE+81	; 0878h
UFORDRV   equ	RETRABASE+82	; 0879h
FORSET	  equ	RETRABASE+83	; 087Ah
FORCOM	  equ	RETRABASE+123	; 08A2h
FORVAR	  equ	RETRABASE+213 	; 08FCh
FORPTR	  equ	RETRABASE+214	; 08FDh
FORUFCB	  equ	RETRABASE+216	; 08FFh
FORFCB	  equ	RETRABASE+217	; 0900h
RE_INSTR  equ 	RETRABASE+256	; 0927h
RE_OUT_APP equ	RETRABASE+296	; 094Fh
RE_OUTSTR equ	RETRABASE+297	; 0950h
PIPEFLAG  equ	RETRABASE+337	; 0978h
PIPEFILES equ	RETRABASE+338	; 0979h
PIPEPTR	  equ	RETRABASE+339	; 097Ah
PIPESTR	  equ	RETRABASE+341	; 097Ch
PIPE1	  equ	RETRABASE+470	; 09FDh	
PIPE2	  equ	RETRABASE+484	; 0A0Fh
INPIPEPTR equ	RETRABASE+498	; 0A19h
OUTPIPEPTR equ	RETRABASE+500	; 0A1Bh
ENVIRSEG  equ	RETRABASE+502	; 0A1Dh
EXEC_BLOCK equ	RETRABASE+502 	; 0A1Dh  ; ENVIRSEG
PARMBUF	  equ	RETRABASE+516	; 0A2Bh
LTPA	  equ	RETRABASE+649	; 0AB0h

;=============================================================================
					
; (command2.s -> offset TRANGOUP)

TRANGROUP:

; ----------------------------------------------------------------------------
; START OF TRANSIENT PORTION
; ----------------------------------------------------------------------------
; SEGMENT - TRANSCODE
; ----------------------------------------------------------------------------

;=============================================================================
; TCODE.ASM
;=============================================================================

;TITLE   PART1 - COMMAND Transient routines.

;        INCLUDE COMSW.ASM
;.xlist
;.xcref
;        INCLUDE DOSSYM.ASM
;        INCLUDE DEVSYM.ASM
;        INCLUDE COMSEG.ASM
;.list
;.cref
;        INCLUDE COMEQU.ASM

; START OF TRANSIENT PORTION
; This code is loaded at the end of memory and may be overwritten by
; memory-intensive user programs.

;TRANCODE	SEGMENT PUBLIC PARA
;ASSUME  CS:TRANGROUP,DS:NOTHING,ES:NOTHING,SS:NOTHING

        ;ORG     0
;ZERO    =       $

        ;ORG     100H                    ; Allow for 100H parameter area

	[ORG 100H]  ; Nasm syntax ; 21/04/2018

SETDRV:
        MOV     AH,SET_DEFAULT_DRIVE
        INT     int_command
TCOMMAND:
        MOV     DS,[CS:RESSEG]
;ASSUME DS:RESGROUP
        MOV     AX,-1
        XCHG    AX,[VERVAL]
        CMP     AX,-1
        JZ	short NOSETVER2
        MOV     AH,SET_VERIFY_ON_WRITE  ; AL has correct value
        INT     int_command
NOSETVER2:
        CALL    FAR [CS:HEADCALL]	; Make sure header fixed
        XOR     BP,BP                   ; Flag transient not read
        CMP	word [SINGLECOM],-1
        JNZ	short COMMAND
_$EXITPREP:
        PUSH    CS
        POP     DS
        JMP     _$EXIT			; Have finished the single command
;ASSUME  DS:NOTHING
COMMAND:
        CLD
        MOV     AX,CS
        MOV     SS,AX
;ASSUME  SS:TRANGROUP
	;MOV	SP,OFFSET TRANGROUP:STACK
	MOV	SP,STACK			
        MOV     ES,AX
;ASSUME  ES:TRANGROUP
        MOV     DS,[SS:RESSEG]
;ASSUME  DS:RESGROUP
        STI
        MOV     byte [SS:UCOMBUF],COMBUFLEN ; Init UCOMBUF
        MOV     byte [SS:COMBUF],COMBUFLEN ; Init COMBUF (Autoexec doing DATE)
        OR      BP,BP                   ; See if just read
        JZ      short TESTRDIR		; Not read, check user directory
        MOV     WORD [SS:UCOMBUF+1],0D01H  ; Reset buffer
        JMP     SHORT NOSETBUF
TESTRDIR:
        CMP     byte [RESTDIR],0
        JZ      short NOSETBUF		; User directory OK
        PUSH    DS
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        ;MOV     DX,OFFSET TRANGROUP:USERDIR1
	MOV	DX,USERDIR1 
        MOV     AH,CHDIR
        INT     int_command             ; Restore users directory
        POP     DS
;ASSUME  DS:RESGROUP
NOSETBUF:
        CMP     byte [PIPEFILES],0
        JZ      short NOPCLOSE		; Don't bother if they don't exist
        CMP     byte [PIPEFLAG],0
        JNZ     short NOPCLOSE		; Don't del if still piping
        CALL    PIPEDEL
NOPCLOSE:
        MOV     byte [EXTCOM],0		; Flag internal command
        MOV     byte [RESTDIR],0	; Flag users dirs OK
        MOV     AX,CS                   ; Get segment we're in
        MOV     DS,AX
;ASSUME  DS:TRANGROUP
        PUSH    AX
        ;MOV     DX,OFFSET TRANGROUP:INTERNATVARS
        MOV     DX,INTERNATVARS
        ;MOV     AX,INTERNATIONAL SHL 8
	MOV     AX,INTERNATIONAL*256
        INT     21H
        POP     AX
        SUB     AX,[TPA]                ; AX=size of TPA in paragraphs
        MOV     DX,16
        MUL     DX                      ; DX:AX=size of TPA in bytes
        OR      DX,DX                   ; See if over 64K
        JZ      short SAVSIZ		; OK if not
        MOV     AX,-1                   ; If so, limit to 65535 bytes
SAVSIZ:
        MOV     [BYTCNT],AX             ; Max no. of bytes that can be buffered
        MOV     DS,[RESSEG]             ; All batch work must use resident seg.
;ASSUME  DS:RESGROUP
        TEST    byte [ECHOFLAG],-1
        JZ      short GETCOM		; Don't do the CRLF
        CALL    SINGLETEST
        JB      short GETCOM
        CALL    CRLF2
GETCOM:
        MOV     AH,GET_DEFAULT_DRIVE
        INT     int_command
        MOV     [SS:CURDRV],AL
        TEST    byte [ECHOFLAG],-1
        JZ      short NOPDRV		; No prompt if echo off
        CALL    SINGLETEST
        JB      short NOPDRV
        CALL    PRINT_PROMPT            ; Prompt the user
NOPDRV:
        TEST    byte [PIPEFLAG],-1	; Pipe has highest presedence
        JZ      short NOPIPE
        JMP     PIPEPROC                ; Continue the pipeline
NOPIPE:
        TEST    byte [FORFLAG],-1	; FOR has next highest precedence
        JZ      short TESTFORBAT
        JMP     FORPROC                 ; Continue the FOR
TESTFORBAT:
        MOV     byte [RE_INSTR],0	; Turn redirection back off
        MOV     byte [RE_OUTSTR],0
        MOV     byte [RE_OUT_APP],0
        TEST    word [BATCH],-1		; Batch has lowest precedence
        JZ      short ISNOBAT
        JMP     READBAT                 ; Continue BATCH

ISNOBAT:
        CMP     word [SINGLECOM],0
        JZ      short REGCOM
        MOV     SI,-1
        XCHG    SI,[SINGLECOM]
        ;MOV     DI,OFFSET TRANGROUP:COMBUF + 2
        MOV     DI,COMBUF+2
        XOR     CX,CX
SINGLELOOP:
        LODSB
        STOSB
        INC     CX
        CMP     AL,0DH
        JNZ     short SINGLELOOP
        DEC     CX
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        MOV     [COMBUF+1],CL
        JMP     DOCOM

REGCOM:
        PUSH    CS
        POP     DS                      ; Need local segment to point to buffer
        ;MOV     DX,OFFSET TRANGROUP:UCOMBUF
        MOV     DX,UCOMBUF
        MOV     AH,STD_CON_STRING_INPUT
        INT     int_command             ; Get a command
        MOV     CL,[UCOMBUF]
        XOR     CH,CH
        ADD     CX,3
        ;MOV     SI,OFFSET TRANGROUP:UCOMBUF
        MOV     SI,UCOMBUF
        ;MOV     DI,OFFSET TRANGROUP:COMBUF
        MOV     DI,COMBUF
        REP     MOVSB                   ; Transfer it to the cooked buffer
        JMP     DOCOM

; All batch proccessing has DS set to segment of resident portion
;ASSUME  DS:RESGROUP,ES:TRANGROUP

NEEDENV:
        PUSH    DS
        PUSH    SI
        PUSH    DI

        ;MOV     DI,OFFSET TRANGROUP:ID
        MOV     DI,ID
        ADD     AL,"0"
        STOSB
GETENV1:
        CALL    GETBATBYT
        STOSB
        CMP     AL,13
        JZ      short GETENV2
        CMP     AL,"%"
        JNZ     short GETENV1
        ;MOV     BYTE PTR ES:[DI-1],"="
        MOV     BYTE [ES:DI-1],"="
GETENV2:
        ;MOV     SI,OFFSET TRANGROUP:ID
        MOV     SI,ID
        PUSH    CS
        POP     DS                      ; DS:SI POINTS TO NAME
;ASSUME DS:TRANGROUP,ES:RESGROUP
        CALL    FIND_NAME_IN_ENVIRONMENT
        PUSH    ES
        POP     DS
        PUSH    CS
        POP     ES
;ASSUME DS:RESGROUP,ES:TRANGROUP
        MOV     SI,DI
        POP     DI                      ; get back pointer to command line
        JNC     short GETENV4

GETENV3:                                ; Parameter not found
        PUSH    CS
        POP     DS
        ;MOV     SI,OFFSET TRANGROUP:ID
	MOV     SI,ID

GETENV4:
        LODSB                           ; From resident segment
        OR      AL,AL                   ; Check for end of parameter
        JZ      short GETENV6
        CMP     AL,13
        JZ      short GETENV6
        CMP     AL,"="
        JZ      short GETENVX
        STOSB
        JMP     short GETENV4

GETENVX:
        MOV     AL,"%"
        STOSB
GETENV6:
        POP     SI
        POP     DS
        CMP     AL,13
        JZ      short SAVBATBYTJ
        JMP     RDBAT

NEEDPARM:
        CALL    GETBATBYT
        CMP     AL,"%"                  ; Check for two consecutive %
        JZ      short SAVBATBYTJ
        CMP     AL,13                   ; Check for end-of-line
        JNZ     short PAROK
SAVBATBYTJ:
        JMP     SAVBATBYT
PAROK:
        SUB     AL,"0"
        JB      short NEEDENV		; look for parameter in the environment
        CMP     AL,9
        JA	short NEEDENV

        CBW
        MOV     SI,AX
        SHL     SI,1                    ; Two bytes per entry
        PUSH    ES
        PUSH    DI
        MOV     ES,[BATCH]
        XOR     CX,CX
        MOV     AX,CX
        MOV     DI,CX
        DEC     CX
        REPNZ   SCASB
        ADD     DI,SI
        MOV     SI,[ES:DI]
        POP     DI
        POP     ES
        CMP     SI,-1                   ; Check if parameter exists
        JZ      short RDBAT		; Ignore if it doesn't
RDPARM:
        LODSB                           ; From resident segment
        CMP     AL,0DH                  ; Check for end of parameter
        JZ      short RDBAT
        STOSB
        JMP     short RDPARM

PROMPTBAT:
        ;MOV     DX,OFFSET TRANGROUP:NEEDBAT
        MOV     DX,NEEDBAT
	;CALL    [RCH_ADDR]
        CALL    FAR [SS:RCH_ADDR]
        JZ      short ASKFORBAT		; Media is removable
NoAskForBat:
        MOV     ES,[BATCH]              ; Turn off batch
        MOV     AH,DEALLOC
        INT     int_command             ; free up the batch piece
        MOV     word [BATCH],0		; AFTER DEALLOC in case of ^C
        MOV     byte [FORFLAG],0	; Turn off for processing
        MOV     byte [PIPEFLAG],0	; Turn off any pipe
        PUSH    CS
        POP     DS
        ;MOV     DX,OFFSET TRANGROUP:BADBAT
        MOV     DX,BADBAT
        CALL    ERROR_PRINT             ; Tell user no batch file
        JMP     TCOMMAND

ASKFORBAT:
        PUSH    CS
        POP     DS
        CALL    ERROR_PRINT             ; Prompt for batch file
        CALL    GetKeystroke
        JMP     TCOMMAND

; read the next keystroke

GetKeystroke:
        ;MOV     AX,(STD_CON_INPUT_FLUSH SHL 8) OR STD_CON_INPUT_no_echo
        MOV     AX,(STD_CON_INPUT_FLUSH*256) | STD_CON_INPUT_NO_ECHO
        INT     int_command             ; Get character with KB buffer flush
        ;MOV     AX,(STD_CON_INPUT_FLUSH SHL 8) + 0
	MOV     AX,STD_CON_INPUT_FLUSH*256
        INT     int_command
        ;return
	RETN

READBAT:
        CALL    BATOPEN
        JC      short PROMPTBAT
        ;MOV     DI,OFFSET TRANGROUP:COMBUF+2
        MOV     DI,COMBUF+2
TESTNOP:
        CALL    GETBATBYT
        CMP     AL,':'                  ; Label/Comment?
        JNZ     short NOTLABEL
NOPLINE:                                ; Consume the line
        CALL    GETBATBYT
        CMP     AL,0DH
        JNZ     short NOPLINE
        CALL    GETBATBYT               ; Eat Linefeed
        TEST    word [BATCH],-1
        JNZ     short TESTNOP
        JMP     TCOMMAND                ; Hit EOF

RDBAT:
        CALL    GETBATBYT
NOTLABEL:
        CMP     AL,"%"                  ; Check for parameter
        JNZ     short SAVBATBYT
        JMP     NEEDPARM
SAVBATBYT:
        STOSB
        CMP     AL,0DH
        JNZ     short RDBAT
        ;SUB     DI,OFFSET TRANGROUP:COMBUF+3
        SUB     DI,COMBUF+3
        MOV     AX,DI
        MOV     [ES:COMBUF+1],AL        ; Set length of line
        CALL    GETBATBYT               ; Eat linefeed
        CALL    BATCLOSE
        TEST    byte [ECHOFLAG],-1
        PUSH    CS
        POP     DS                      ; Go back to local segment
        JZ      short NOECHO2
;ASSUME DS:TRANGROUP
        ;MOV     DX,OFFSET TRANGROUP:COMBUF+2
	MOV     DX,COMBUF+2
        CALL    CRPRINT
DOCOM:
; All segments are local for command line processing
        CALL    CRLF2
DOCOM1:

NOECHO2:
        CALL    PRESCAN                 ; Cook the input buffer
        JZ      short NOPIPEPROC
        JMP     PIPEPROCSTRT            ; Fire up the pipe
NOPIPEPROC:
	;MOV     SI,OFFSET TRANGROUP:COMBUF+2
	MOV     SI,COMBUF+2
        ;MOV     DI,OFFSET TRANGROUP:IDLEN
        MOV     DI,IDLEN
        ;MOV	AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 01H ; Make FCB with blank scan-off
        MOV	AX,(PARSE_FILE_DESCRIPTOR*256) | 01H
        INT     int_command
        CMP     AL,1                    ; Check for ambiguous command name
        JZ      short BADCOMJ1		; Ambiguous commands not allowed
        CMP     AL,-1
        JNZ     short DRVGD
        JMP     DRVBAD

BADCOMJ1:
        JMP	BADCOM

DRVGD:
        MOV     AL,[DI]
        MOV     [SPECDRV],AL
        MOV     AL," "
        MOV     CX,9
        INC     DI
        REPNE   SCASB                   ; Count no. of letters in command name
        MOV     AL,9
        SUB     AL,CL
        MOV     [IDLEN],AL
        MOV     DI,81H
        XOR     CX,CX
        PUSH    SI
COMTAIL:
        LODSB
        STOSB                           ; Move command tail to 80H
        CMP     AL,13
        LOOPNZ  COMTAIL
        NOT     CL
        ;MOV     BYTE PTR DS:[80H],CL
        MOV     [80H],CL
        POP     SI
; If the command has 0 parameters must check here for
; any switches that might be present.
; SI -> first character after the command.
        CALL    SWITCH          	; Is the next character a SWITCHAR
        MOV     [COMSW],AX
        MOV     DI,FCB
	;MOV     AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 01H
	MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 01H
        INT     int_command
        MOV     [PARM1],AL		; Save result of parse

PRBEG:
        LODSB
        CMP     AL,[SWITCHAR]
        JZ      short PRFIN
        CMP     AL,13
        JZ      short PRFIN
        CALL    DELIM
        JNZ     short PRBEG
PRFIN:
        DEC     SI
        CALL    SWITCH
        MOV     [ARG1S],AX
        MOV     DI,FCB+10H
        ;MOV     AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 01H
        MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 01H
        INT     int_command             ; Parse file name
        MOV     [PARM2],AL      	; Save result
        CALL    SWITCH
        MOV     [ARG2S],AX
        OR      AX,[ARG1S]
        MOV     [ARGTS],AX
SWTLP:                          	; Find any remaining switches
        CMP     BYTE [SI],0DH
        JZ      short GOTALLSW
        INC     SI
        CALL    SWITCH
        OR      [ARGTS],AX
        JMP     SHORT SWTLP

GOTALLSW:
        MOV     AL,[IDLEN]
        MOV     DL,[SPECDRV]
        OR      DL,DL           	; Check if drive was specified
        JZ      short OK
        JMP     DRVCHK
OK:
        DEC     AL              	; Check for null command
        JNZ     short FNDCOM
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        CMP     word [SINGLECOM],-1
        JZ      short EXITJ
        JMP     GETCOM

EXITJ:
        JMP     _$EXITPREP

;ASSUME  DS:TRANGROUP

RETSW:
        XCHG    AX,BX           	; Put switches in AX
        RETN

SWITCH:
        XOR     BX,BX           	; Initialize - no switches set
SWLOOP:
        CALL    SCANOFF         	; Skip any delimiters
        CMP     AL,[SWITCHAR]   	; Is it a switch specifier?
        JNZ     short RETSW		; No -- we're finished
        OR      BX,GOTSWITCH    	; Indicate there is a switch specified
        INC     SI              	; Skip over the switch character
        CALL    SCANOFF
        CMP     AL,0DH
        JZ      short RETSW		; Oops
        INC     SI
; Convert lower case input to upper case
        CALL    UPCONV
        ;MOV     DI,OFFSET TRANGROUP:SWLIST
        MOV     DI,SWLIST
        MOV     CX,SWCOUNT
        REPNE   SCASB			; Look for matching switch
        JNZ     short BADSW
        MOV     AX,1
        SHL     AX,CL           	; Set a bit for the switch
        OR      BX,AX
        JMP     SHORT SWLOOP

BADSW:
        JMP     SHORT SWLOOP

SWLIST  DB      "VBAPW"
SWCOUNT EQU     $-SWLIST

DRVBAD:
        ;MOV     DX,OFFSET TRANGROUP:BADDRV
        MOV     DX,BADDRV
        JMP     CERROR

FNDCOM:
        ;MOV     SI,OFFSET TRANGROUP:COMTAB ; Prepare to search command table
        MOV     SI,COMTAB
        MOV     CH,0
FINDCOM:
        ;MOV     DI,OFFSET TRANGROUP:IDLEN
        MOV     DI,IDLEN
        MOV     CL,[SI]
        JCXZ    EXTERNAL
        REPE    CMPSB
        LAHF
        ADD     SI,CX           ; Bump to next position without affecting flags
        SAHF
        LODSB           	; Get flag for drive check
        MOV     [CHKDRV],AL
        LODSW           	; Get address of command
        JNZ     short FINDCOM
        MOV     DX,AX
        CMP     byte [CHKDRV],0
        JZ      short NOCHECK
        MOV     AL,[PARM1]
        OR      AL,[PARM2]      	; Check if either parm. had invalid drive
        CMP     AL,-1
        JZ      short DRVBAD
NOCHECK:
        CALL    IOSET
        CALL    DX              	; Call the internal
COMJMP:
        JMP     TCOMMAND

SETDRV1:
        JMP     SETDRV

DRVCHK:
        DEC     DL              	; Adjust for correct drive number
        DEC     AL              	; Check if anything else is on line
        JZ      short SETDRV1
EXTERNAL:
        MOV     byte [FILTYP],0
        MOV     DL,[SPECDRV]
        MOV     [IDLEN],DL
        CALL    SAVUDIR			; Drive letter already checked
        MOV     AL,'?'
        ;MOV     DI,OFFSET TRANGROUP:COM
        MOV     DI,COM
        STOSB				; Look for any extension
        STOSB
        STOSB
        ;MOV     DX,OFFSET TRANGROUP:DIRBUF ; Command will end up here
	MOV     DX,DIRBUF
        MOV     AH,SET_DMA
        INT     int_command
        PUSH    ES
        CALL    FIND_PATH
        MOV     SI,DI
        POP     ES

        ;MOV     DI,OFFSET TRANGROUP:EXECPATH
        MOV     DI,EXECPATH
        MOV     BYTE [DI],0         	; Initialize to current directory
RESEARCH:
        MOV     AH,DIR_SEARCH_FIRST
COMSRCH:
        PUSH    CS
        POP     DS
        ;MOV     DX,OFFSET TRANGROUP:IDLEN
        MOV     DX,IDLEN
        INT     int_command
        OR      AL,AL
        MOV     AH,DIR_SEARCH_NEXT      ; Do search-next next
        JNZ     short PATHCHK
        CMP     WORD [DIRBUF+9],4F00H + "C" ; 4F43h ; "CO"
        JNZ     short CHKEXE
        CMP     byte [DIRBUF+11],"M"	; 4Dh
        JNZ     short CHKEXE
        OR      byte [FILTYP],4
        JMP     EXECUTE                 ; If we find a COM were done

CHKEXE:
        CMP     WORD [DIRBUF+9],5800H + "E" ; 5845h ; "EX"
        JNZ     short CHKBAT
        CMP     byte [DIRBUF+11],"E"	; 45h
        JNZ     short CHKBAT
        OR      byte [FILTYP],1		; Flag an EXE found
        JMP     short COMSRCH		; Continue search

CHKBAT:
        CMP     WORD [DIRBUF+9],4100H + "B" ; 4142h ; "BA"
        JNZ	short COMSRCH
        CMP     byte [DIRBUF+11],"T"	; 54h
        JNZ	short COMSRCH
        OR      byte [FILTYP],2		; Flag BAT found
        JMP     short COMSRCH		; Continue search

PATHCHK:
        TEST    byte [FILTYP],1
        JZ      short TESTBAT
        MOV     WORD [DIRBUF+9],5800H+"E"
        MOV     byte [DIRBUF+11],"E"
        JMP     EXECUTE                 ; Found EXE

TESTBAT:
        TEST    byte [FILTYP],2
        JZ	short NEXTPATH		; Found nothing, try next path
        MOV     WORD [DIRBUF+9],4100H+"B"
        MOV     byte [DIRBUF+11],"T"
        ;MOV     DX,OFFSET TRANGROUP:DIRBUF ; Found BAT
        MOV     DX,DIRBUF
        MOV     AH,FCB_OPEN
        INT     int_command
        OR      AL,AL
        JZ      short BATCOMJ		; Bat exists
        CALL    RESTUDIR
        JMP     BADCOM

BATCOMJ:
        JMP	BATCOM

NEXTPATH:
        ;MOV     DX,OFFSET TRANGROUP:USERDIR1 ; Restore users dir
        MOV     DX,USERDIR1
        MOV     AH,CHDIR
        INT     int_command
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        MOV     byte [RESTDIR],0
BADPATHEL:
        ;MOV     DI,OFFSET TRANGROUP:EXECPATH ; Build a full path here
        MOV     DI,EXECPATH
        MOV     DX,SI
        MOV     DS,[ENVIRSEG]   	; Point into environment
;ASSUME  DS:NOTHING
        LODSB

        ;IF      KANJI
        ;MOV     [KPARSE],0
        ;ENDIF

        OR      AL,AL
        JZ      short BADCOMJ		; NUL, command not found
        XOR     BL,BL                   ; Make BL a NUL
PSKIPLP:                                ; Get the path
        STOSB
        OR      AL,AL
        JZ      short LASTPATH
        CMP     AL,';'
        JZ      short GOTNEXTPATH
        ;CMP     DI,15+DirStrLen+(OFFSET TRANGROUP:EXECPATH)
        CMP     DI,15+DIRSTRLEN+EXECPATH
        JB      short OKPath
SKIPPathElem:
        LODSB                           ; scan to end of path element
        OR      AL,AL
        JZ      short BADPATHEL
        CMP     AL,';'
        JZ      short BADPATHEL
        JMP     short SKIPPathElem

OKPath:
        ;IF      KANJI
        ;MOV     [KPARSE],0
        ;CALL    TESTKANJ
        ;JZ      NXTPTCHR
        ;INC     [KPARSE]
        ;MOVSB
;NXTPTCHR:
        ;ENDIF

        LODSB
        JMP     SHORT PSKIPLP

BADCOMJ:
        JMP     BADCOM

LASTPATH:
        MOV     BYTE [ES:DI-1],';'	; Fix up the NUL in EXECPATH
        DEC     SI                      ; Point to the NUL in PATHSTRING
        MOV     BL,[SI-1]               ; Change substi char to char before NUL

GOTNEXTPATH:
        DEC     DI              	; Point to the end of the dir
        PUSH    BX
        PUSH    SI
        PUSH    DX
        MOV     SI,DX
        XOR     DL,DL
        CMP     BYTE [SI+1],DRVCHAR
        JNZ     short DEFDRVPATH	; No drive spec
        MOV     DL,[SI]
        SUB     DL,'@'
DEFDRVPATH:
        PUSH    DS
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        MOV     [IDLEN],DL      	; New drive
        PUSH    DI
        CALL    SAVUDIR         	; Save the users dir
        POP     DI
        JNC     short PATHTRY
        ;MOV    DX,OFFSET TRANGROUP:BADPMES ; Tell the user bad stuff in path
        MOV	DX,BADPMES
        CALL    PRINT
PATHTRY:
        POP     DS
;ASSUME  DS:NOTHING
        POP     DX
        POP     SI
        POP     BX
        XCHG    BL,[SI-1]      		; Stick in NUL, or same thing if LASTPATH
CDPATH:
        MOV     AH,CHDIR
        INT     int_command
        MOV     [SI-1],BL       	; Fix the path string back up
        MOV     DS,[SS:RESSEG]
;ASSUME  DS:RESGROUP
        INC     byte [RESTDIR]		; Say users dir needs restoring
        JNC	short ResearchJ
        JMP     short BADPATHEL       	; Ignore a directory which doesn't exist
ResearchJ:
        JMP     RESEARCH        	; Try looking in this one

BATCOM:
;ASSUME	DS:TRANGROUP
; Batch parameters are read with ES set to segment of resident part
        CALL    IOSET           ; Set up any redirection
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
;Since BATCH has lower precedence than PIPE or FOR. If a new BATCH file
;is being started it MUST be true that no FOR or PIPE is currently in
;progress.
        MOV     byte [ES:FORFLAG],0	; Turn off for processing
        MOV     byte [ES:PIPEFLAG],0	; Turn off any pipe
        TEST    word [ES:BATCH],-1
        JNZ	short CHAINBAT        	; Don't need allocation if chaining
        CALL    FREE_TPA
;ASSUME  ES:RESGROUP
        MOV     BX,6			; 64 + 32 bytes
        MOV     AH,ALLOC
        INT     int_command             ; Suck up a little piece for batch processing
        MOV     [ES:BATCH],AX
        CALL    ALLOC_TPA
CHAINBAT:
        PUSH    ES
        MOV     ES,[ES:BATCH]
;ASSUME  ES:NOTHING
        MOV     DL,[DIRBUF]
        XOR     DI,DI
        CALL    SAVUDIR1		; ES:DI set up, get dir containing Batch file
        XOR     AX,AX
        MOV     CX,AX
        DEC     CX
        REPNZ   SCASB 			; Find the NUL
        DEC     DI			; Point at the NUL
        MOV     AL,[DIRCHAR]
        CMP     AL,[ES:DI-1]
        JZ      short NOPUTSLASH
        STOSB
NOPUTSLASH:
        ;MOV     SI,OFFSET TRANGROUP:DIRBUF+1
        MOV     SI,DIRBUF+1
        CALL    FCB_TO_ASCZ		; Tack on batch file name
        MOV     AX,-1
        MOV     BX,DI
        MOV     CX,10
        REP     STOSW			; Init Parmtab to no parms
        POP     ES
;ASSUME  ES:RESGROUP
        CALL    RESTUDIR
        ;MOV     SI,OFFSET TRANGROUP:COMBUF+2
        MOV     SI,COMBUF+2
        ;MOV     DI,OFFSET RESGROUP:PARMBUF
        MOV     DI,PARMBUF
        MOV     CX,10
EACHPARM:
        CALL    SCANOFF
        CMP     AL,0DH
        JZ      short HAVPARM
        JCXZ    MOVPARM			; Only first 10 parms get pointers
        PUSH    ES
        MOV     ES,[ES:BATCH]
        MOV     [ES:BX],DI		; Set pointer table to point to actual parameter
        POP     ES
        INC     BX
        INC     BX
MOVPARM:
        LODSB
        CALL    DELIM
        JZ      short ENDPARM		; Check for end of parameter
        STOSB
        CMP     AL,0DH
        JZ      short HAVPARM
        JMP     SHORT MOVPARM
ENDPARM:
        MOV     AL,0DH
        STOSB           		; End-of-parameter marker
        JCXZ    EACHPARM
        DEC     CX
        JMP     SHORT EACHPARM
HAVPARM:
        XOR     AL,AL
        STOSB                   	; Nul terminate the parms
        XOR     AX,AX
        PUSH    ES
        POP     DS                      ; Simply batch FCB setup
;ASSUME  DS:RESGROUP
        MOV     [BATLOC],AX		; Start at beginning of file
        MOV     [BATLOC+2],AX
        CMP     word [SINGLECOM],-1
        JNZ     short NOBATSING
        MOV     word [SINGLECOM],0FFF0H	; Flag single command BATCH job
NOBATSING:
        JMP     TCOMMAND

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

EXECUTE:
        CALL    RESTUDIR
NeoExecute:
        CMP     BYTE [DI],0		; Command in current directory
        JZ	short NNSLSH
        MOV     AL,[DI-1]

        ;IF      KANJI
        ;CMP     [KPARSE],0
        ;JNZ     StuffPath		; Last char is second KANJI byte, might be '\'
        ;ENDIF

        CALL    PATHCHRCMP
        JZ	short HAVEXP		; Don't double slash
StuffPath:
        MOV     AL,[DIRCHAR]
        STOSB
        JMP     SHORT HAVEXP

NNSLSH:
        MOV     AL,[DIRBUF]             ; Specify a drive
        ADD     AL,'@'
        STOSB
        MOV     AL,DRVCHAR
        STOSB
HAVEXP:
        ;MOV     SI,OFFSET TRANGROUP:DIRBUF+1
        MOV     SI,DIRBUF+1
        CALL    FCB_TO_ASCZ             ; Tack on the filename
        CALL    IOSET
        MOV     ES,[TPA]
        MOV     AH,DEALLOC
        INT     int_command		; Now running in "free" space
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        INC     byte [ES:EXTCOM]       	; Indicate external command
        MOV     byte [ES:RESTDIR],0    	; Since USERDIR1 is in transient, insure
                                	;  this flag value for re-entry to COMMAND
        MOV     DI,FCB
        MOV     SI,DI
        MOV     CX,52H
        REP     MOVSW           	; Transfer parameters to resident header
        ;MOV     DX,OFFSET TRANGROUP:EXECPATH
        MOV     DX,EXECPATH
	;MOV     BX,OFFSET RESGROUP:EXEC_BLOCK
	MOV     BX,EXEC_BLOCK
        ;MOV     AX,EXEC SHL 8
        MOV     AX,EXEC*256
        ;JMP	[EXEC_ADDR]     	; Jmp to the EXEC in the resident
	JMP	FAR [EXEC_ADDR]

BADCOM:
        PUSH    CS
        POP     DS
        ;MOV     DX,OFFSET TRANGROUP:BADNAM
        MOV     DX,BADNAM
CERROR:
        CALL    ERROR_PRINT
        JMP     TCOMMAND

SINGLETEST:
;ASSUME  DS:RESGROUP
        CMP     word [SINGLECOM],0
        JZ	short RET5
        CMP     word [SINGLECOM],0EFFFH
        ;return
	RETN

;ASSUME  DS:TRANGROUP

SETREST1:
        MOV     AL,1
SETREST:
        PUSH    DS
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        MOV     [RESTDIR],AL
        POP     DS
;ASSUME  DS:TRANGROUP
RET5:
	;return
	RETN

CHKCNT:
        TEST    word [FILECNT],-1
        JNZ     short ENDDIR
NOTFNDERR:
	;MOV     DX,OFFSET TRANGROUP:NOTFND
	MOV     DX,NOTFND
        JMP     short CERROR

ENDDIR:
; Make sure last line ends with CR/LF
        MOV     AL,[LINLEN]
        CMP     AL,[LINCNT]     	; Will be equal if just had CR/LF
        JZ      short MESSAGE
        CALL    CRLF2
MESSAGE:
        ;MOV     DX,OFFSET TRANGROUP:DIRMES_PRE
        MOV     DX,DIRMES_PRE
        CALL    PRINT
        MOV     SI,[FILECNT]
        XOR     DI,DI
        CALL    DISP32BITS
        ;MOV     DX,OFFSET TRANGROUP:DIRMES_POST
        MOV     DX,DIRMES_POST
        CALL    PRINT
        MOV     AH,GET_DRIVE_FREESPACE
	;MOV     DL,BYTE PTR DS:[FCB]
	MOV     DL,[FCB]
        INT     int_command
        CMP     AX,-1
        ;retz
	jz	short RET5
        ;MOV     DX,OFFSET TRANGROUP:BYTMES_PRE
        MOV     DX,BYTMES_PRE
        CALL    PRINT
        MUL     CX              		; AX is bytes per cluster
        MUL     BX
        MOV     DI,DX
        MOV     SI,AX
        CALL    DISP32BITS
	;MOV     DX,OFFSET TRANGROUP:BYTMES_POST
	MOV     DX,BYTMES_POST
        JMP     PRINT

;ASSUME  DS:RESGROUP

PIPEDEL:
        PUSH    DX
        ;MOV     DX,OFFSET RESGROUP:PIPE1	; Clean up in case ^C
        MOV     DX,PIPE1
        MOV     AH,UNLINK
        INT     int_command
        ;MOV     DX,OFFSET RESGROUP:PIPE2
        MOV     DX,PIPE2
        MOV     AH,UNLINK
        INT     int_command
        XOR     AX,AX
        MOV     WORD [PIPEFLAG],AX		; Pipe files and pipe gone
        MOV     byte [ECHOFLAG],1	; Make sure ^C to pipe doesn't leave ECHO OFF
        POP     DX
        RETN

PIPEERRSYN:
        ;MOV     DX,OFFSET TRANGROUP:SYNTMES
	MOV     DX,SYNTMES
        JMP     SHORT PIPPERR
PIPEERR:
        ;MOV     DX,OFFSET TRANGROUP:PIPEEMES
        MOV     DX,PIPEEMES
PIPPERR:
        CALL    PIPEDEL
        PUSH    CS
        POP     DS
        JMP     CERROR

PIPEPROCSTRT:
;ASSUME  DS:TRANGROUP,ES:TRANGROUP
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        INC     byte [PIPEFILES]	; Flag that the pipe files exist
        MOV     AH,19H                  ; Get current drive
        INT     int_command
        ADD     AL,'A'
        MOV     [PIPE2],AL              ; Make pipe files in root of def drv
        ;MOV     BX,OFFSET RESGROUP:PIPE1
        MOV     BX,PIPE1
        MOV     [BX],AL
        MOV     DX,BX
        XOR     CX,CX
        MOV     AH,CREAT
        INT     int_command
        JC      short PIPEERR		; Couldn't create
        MOV     BX,AX
        MOV     AH,CLOSE                ; Don't proliferate handles
        INT     int_command
        ;MOV     DX,OFFSET RESGROUP:PIPE2
        MOV     DX,PIPE2
        MOV     AH,CREAT
        INT     int_command
        JC      short PIPEERR
        MOV     BX,AX
        MOV     AH,CLOSE
        INT     int_command
        CALL    TESTDOREIN      	; Set up a redirection if specified
        MOV     byte [ECHOFLAG],0    	; No echo on pipes
        MOV     SI,[PIPEPTR]
        CMP     word [SINGLECOM],-1
        JNZ     short NOSINGP
        MOV     word [SINGLECOM],0F000H	; Flag single command pipe
NOSINGP:
        JMP     SHORT FIRSTPIPE

PIPEPROC:
;ASSUME  DS:RESGROUP
        MOV     byte [ECHOFLAG],0	; No echo on pipes
        MOV     SI,[PIPEPTR]
        LODSB
        CMP     AL,'|'		; 7Ch
        JNZ	short PIPEEND		; Pipe done
        MOV     DX,[INPIPEPTR]  	; Get the input file name
        ;MOV     AX,(OPEN SHL 8)
        MOV     AX,OPEN*256
        INT     int_command
PIPEERRJ:
        JC	short PIPEERR		; Lost the pipe file
        MOV     BX,AX
        MOV     AL,0FFH
	;XCHG    AL,[BX.PDB_JFN_Table]
	XCHG    AL,[BX+PDB.JFN_TABLE]
	;MOV     DS:[PDB_JFN_Table],AL	; Redirect
	MOV     [PDB.JFN_TABLE],AL
FIRSTPIPE:
        ;MOV     DI,OFFSET TRANGROUP:COMBUF + 2
        MOV     DI,COMBUF+2
        XOR     CX,CX
        CMP     BYTE [SI],0DH		; '|<CR>'
        JNZ	short PIPEOK1
PIPEERRSYNJ:
        JMP     PIPEERRSYN
PIPEOK1:
        CMP     BYTE [SI],'|'		; '||'
        JZ	short PIPEERRSYNJ
PIPECOMLP:
        LODSB
        STOSB

        ;IF	KANJI
        ;CALL	TESTKANJ
        ;JZ	NOTKANJ5
        ;MOVSB
        ;JMP	PIPECOMLP
;NOTKANJ5:
        ;ENDIF

        CMP     AL,0DH
        JZ	short LASTPIPE
        INC     CX
        CMP     AL,'|'			; 7Ch
        JNZ     PIPECOMLP
        MOV     BYTE [ES:DI-1],0DH
        DEC     CX
        ;MOV     [COMBUF+1],CL
        MOV     [SS:COMBUF+1],CL
        DEC     SI
        MOV     [PIPEPTR],SI            ; On to next pipe element
        MOV     DX,[OUTPIPEPTR]
        PUSH    CX
        XOR     CX,CX
        ;MOV     AX,(CREAT SHL 8)
        MOV     AX,CREAT*256
        INT     int_command
        POP     CX
        JC      short PIPEERRJ		; Lost the file
        MOV     BX,AX
        MOV     AL,0FFH
        ;XCHG    AL,[BX.PDB_JFN_Table]
        XCHG    AL,[BX+PDB.JFN_TABLE]
        ;MOV     DS:[PDB_JFN_Table+1],AL
        MOV     [PDB.JFN_TABLE+1],AL
        XCHG    DX,[INPIPEPTR]          ; Swap for next element of pipe
        MOV     [OUTPIPEPTR],DX
        JMP     SHORT PIPECOM

LASTPIPE:
        MOV     [SS:COMBUF+1],CL
        DEC     SI
        MOV     [PIPEPTR],SI    ; Point at the CR (anything not '|' will do)
        CALL    TESTDOREOUT     ; Set up the redirection if specified
PIPECOM:
        PUSH    CS
        POP     DS
        JMP     NOPIPEPROC      	; Process the pipe element

PIPEEND:
        CALL    PIPEDEL
        CMP     word [SINGLECOM],0F000H
        JNZ     short NOSINGP2
        MOV     word [SINGLECOM],-1	; Make it return
NOSINGP2:
        JMP	TCOMMAND

;TRANCODE  ENDS
;          END

;=============================================================================
; TCODE2.ASM
;=============================================================================

;TITLE   PART2 - COMMAND Transient routines.

;TRANCODE SEGMENT PUBLIC BYTE
;ASSUME  CS:TRANGROUP,DS:NOTHING,ES:NOTHING,SS:NOTHING

;BREAK   <Environment utilities>

;ASSUME DS:TRANGROUP

ADD_PROMPT:
        CALL    DELETE_PROMPT		; DELETE ANY EXISTING PROMPT
        CALL    SCAN_DOUBLE_NULL
ADD_PROMPT2:
        PUSH    SI
        CALL    GETARG
        POP     SI
	;retz				; PRE SCAN FOR ARGUMENTS
	JZ	SHORT ADD_NAME_retn
        CALL    MOVE_NAME		; MOVE IN NAME
        CALL    GETARG
        JMP     SHORT ADD_NAME
;
; Input: DS:SI points to a CR terminated string
; Output: carry flag is set if no room
;         otherwise name is added to environment
;
ADD_NAME_TO_ENVIRONMENT:
        CALL    GETARG
        JZ	short DISP_ENV
;
; check if line contains exactly one equals sign
;
        XOR     BX,BX           	;= COUNT IS 0
        PUSH    SI              	;SAVE POINTER TO BEGINNING OF LINE
EQLP:
        LODSB                   	;GET A CHAR
        CMP     AL,13  ; 0Dh		;IF CR WE'RE ALL DONE
        JZ	short QUEQ
        CMP     AL,"=" ; 3Dh		;LOOK FOR = SIGN
        JNZ     short EQLP		;NOT THERE, GET NEXT CHAR
        INC     BL              	;OTHERWISE INCREMENT EQ COUNT
        CMP     BYTE [SI],13		;LOOK FOR CR FOLLOWING = SIGN
        JNZ	short EQLP
        INC     BH              	;SET BH=1 MEANS NO PARAMETERS
        JMP     short EQLP		;AND LOOK FOR MORE
QUEQ:
        POP     SI              	;RESTORE BEGINNING OF LINE
        DEC     BL              	;ZERO FLAG MEANS ONLY ONE EQ
        JZ	short ONEQ		;GOOD LINE
        ;MOV     DX,OFFSET TRANGROUP:SYNTMES
        MOV     DX,SYNTMES
        JMP     CERROR

ONEQ:
        PUSH    BX
        CALL    DELETE_NAME_IN_ENVIRONMENT
        POP     BX
        DEC     BH
        ;retz
	JZ	SHORT ADD_NAME_retn

        CALL    SCAN_DOUBLE_NULL
        CALL    MOVE_NAME
ADD_NAME:
        LODSB
        CMP     AL,13
        ;retz
	JZ	SHORT ADD_NAME_retn
        CALL    STORE_CHAR
        JMP     short ADD_NAME

ADD_NAME_retn:
DISP_ENV_retn:
	RETN

DISP_ENV:
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        MOV     DS,[ENVIRSEG]
;ASSUME  DS:NOTHING
        XOR     SI,SI
PENVLP:
        CMP     BYTE [SI],0
	;retz
	JZ	SHORT DISP_ENV_retn

        MOV     DX,SI
        CALL    ZPRINT
        CALL    CRLF2
PENVLP2:
        LODSB
        OR      AL,AL
        JNZ	short PENVLP2
        JMP	short PENVLP

;ASSUME  DS:TRANGROUP

DELETE_PATH:
	;MOV     SI,OFFSET TRANGROUP:PATH_TEXT
        MOV     SI,PATH_TEXT
        JMP     SHORT DELETE_NAME_IN_ENVIRONMENT

DELETE_PROMPT:
	;MOV     SI,OFFSET TRANGROUP:PROMPT_TEXT
	MOV     SI,PROMPT_TEXT

DELETE_NAME_IN_ENVIRONMENT:
;
; Input: DS:SI points to a "=" terminated string
; Output: carry flag is set if name not found
;         otherwise name is deleted
;
        PUSH    SI
        PUSH    DS
        CALL    FIND            	; ES:DI POINTS TO NAME
        JC	short DEL1
        MOV     SI,DI           	; SAVE IT
        CALL    SCASB2          	; SCAN FOR THE NUL
        XCHG    SI,DI
        CALL    GETENVSIZ
        SUB     CX,SI
        PUSH    ES
        POP     DS	; ES:DI POINTS TO NAME, DS:SI POINTS TO NEXT NAME
        REP     MOVSB 	; DELETE THE NAME
DEL1:
        POP     DS
        POP     SI
        RETN

FIND_PATH:
        ;MOV     SI,OFFSET TRANGROUP:PATH_TEXT
	MOV     SI,PATH_TEXT
        JMP     SHORT FIND_NAME_IN_ENVIRONMENT

FIND_PROMPT:
        ;MOV     SI,OFFSET TRANGROUP:PROMPT_TEXT
	MOV     SI,PROMPT_TEXT

FIND_NAME_IN_ENVIRONMENT:
;
; Input: DS:SI points to a "=" terminated string
; Output: ES:DI points to the arguments in the environment
;         zero is set if name not found
;         carry flag is set if name not valid format
;
        CALL    FIND                    ; FIND THE NAME
        ;retc				; CARRY MEANS NOT FOUND
	jb	short FIND13
        JMP     SCASB1                  ; SCAN FOR = SIGN
;
; On return of FIND1, ES:DI points to beginning of name
;
FIND:
        CLD
        CALL    COUNT0                  ; CX = LENGTH OF NAME
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        ;MOV     ES,[ENVIRSEG]
	MOV     ES,[ES:ENVIRSEG]
;ASSUME  ES:NOTHING
        XOR     DI,DI
FIND1:
        PUSH    CX
        PUSH    SI
        PUSH    DI
FIND11:
        LODSB

        ;IF      KANJI
        ;CALL    TESTKANJ
        ;JZ      NOTKANJ3
        ;DEC     SI
        ;LODSW
        ;INC     DI
        ;INC     DI
        ;CMP     AX,ES:[DI-2]
        ;JNZ     FIND12
        ;DEC     CX
        ;LOOP    FIND11
        ;JMP     SHORT FIND12
;NOTKANJ3:
	;ENDIF

        CALL    UPCONV
        INC     DI
        CMP     AL,[ES:DI-1]
        JNZ	short FIND12
        LOOP    FIND11
FIND12:
        POP     DI
        POP     SI
        POP     CX
        ;retz
	JZ	short FIND13
        PUSH    CX
        CALL    SCASB2                  ; SCAN FOR A NUL
        POP     CX
        CMP     BYTE [ES:DI],0
        JNZ     short FIND1
        STC                             ; INDICATE NOT FOUND
FIND13:
        RETN

COUNT0:
        PUSH    DS
        POP     ES
        MOV     DI,SI

;COUNT1:
        PUSH    DI                      ; COUNT NUMBER OF CHARS UNTIL "="
        CALL    SCASB1
;       JMP     SHORT COUNTX
;COUNT2:
;	PUSH    DI                      ; COUNT NUMBER OF CHARS UNTIL NUL
;	CALL    SCASB2
;COUNTX:
        POP     CX
        SUB     DI,CX
        XCHG    DI,CX
        RETN

MOVE_NAME:
        ;CMP     BYTE PTR DS:[SI],13
        ;retz
	CMP     BYTE [SI],13
	JZ	short MVNM_retn	
        LODSB

        ;IF      KANJI
	;CALL    TESTKANJ
        ;JZ      NOTKANJ1
        ;CALL    STORE_CHAR
        ;LODSB
        ;CALL    STORE_CHAR
        ;JMP     SHORT MOVE_NAME
;NOTKANJ1:
	;ENDIF

        CALL    UPCONV
        CALL    STORE_CHAR
        CMP     AL,"="
        JNZ     MOVE_NAME
MVNM_retn:
        ;return
	RETN

GETARG:
        MOV     SI,80H
        LODSB
        OR      AL,AL
        ;retz
	jz	short GETARG_retn
        CALL    SCANOFF
        CMP     AL,13
GETARG_retn:
        ;return
	RETN

SCAN_DOUBLE_NULL:
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        MOV     ES,[ES:ENVIRSEG]
;ASSUME  ES:NOTHING
        XOR     DI,DI
SDN1:
        CALL    SCASB2
        CMP     BYTE  [ES:DI],0
        JNZ     short SDN1
        RETN

SCASB1:
        MOV     AL,"="                  ; SCAN FOR AN =
        JMP     SHORT SCASBX
SCASB2:
        XOR     AL,AL                   ; SCAN FOR A NUL
SCASBX:
        MOV     CX,100H
        REPNZ   SCASB
        ;return
	RETN

        ;IF      KANJI
;TESTKANJ:
        ;CMP     AL,81H
        ;JB      NOTLEAD
        ;CMP     AL,9FH
        ;JBE     ISLEAD
        ;CMP     AL,0E0H
        ;JB      NOTLEAD
        ;CMP     AL,0FCH
        ;JBE     ISLEAD
;NOTLEAD:
        ;PUSH    AX
        ;XOR     AX,AX		;Set zero
        ;POP     AX
        ;return
;ISLEAD:
        ;PUSH    AX
        ;XOR     AX,AX		;Set zero
        ;INC     AX		;Reset zero
        ;POP     AX
        ;return
        ;ENDIF

UPCONV:
        CMP     AL,"a"
        JB	short RET22C
        CMP     AL,"z"
        JA	short RET22C
        SUB     AL,20H          ; Lower-case changed to upper-case
RET22C:
        ;CALL	DWORD PTR CS:[INTERNATVARS.Map_call]
        CALL	FAR [CS:INTERNATVARS+INTERNAT_BLOCK.Map_call]
        RETN
;
; STORE A CHAR IN environment, GROWING IT IF NECESSARY
;
STORE_CHAR:
        PUSH    CX
        PUSH    BX
        CALL    GETENVSIZ
        MOV     BX,CX
        SUB     BX,2            ; SAVE ROOM FOR DOUBLE NULL
        CMP     DI,BX
        JB	short STORE1

        PUSH    AX
        PUSH    CX
        PUSH    BX              ; Save Size of environment
        CALL    FREE_TPA
        POP     BX
        ADD     BX,2            ; Recover true environment size
        MOV     CL,4
        SHR     BX,CL           ; Convert back to paragraphs
        INC     BX              ; Try to grow environment by one para
        MOV     AH,SETBLOCK
        INT     int_command
        PUSHF
        PUSH    ES
        MOV     ES,[RESSEG]
        CALL    ALLOC_TPA
        POP     ES
        POPF
        POP     CX
        POP     AX
        JNC	short STORE1
        ;MOV     DX,OFFSET TRANGROUP:ENVERR
        MOV     DX,ENVERR
        JMP     CERROR
STORE1:
        STOSB
        MOV     WORD [ES:DI],0		; NULL IS AT END
        POP     BX
        POP     CX
        RETN

GETENVSIZ:
;Get size of environment in bytes, rounded up to paragraph boundry
;ES has environment segment
;Size returned in CX, all other registers preserved

        PUSH    ES
        PUSH    AX
        MOV     AX,ES
        DEC     AX              	;Point at arena
        MOV     ES,AX
	;MOV     AX,ES:[arena_size]
	MOV	AX, [ES:ARENA.SIZE]
        MOV     CL,4
        SHL     AX,CL           	;Convert to bytes
        MOV     CX,AX
        POP     AX
        POP     ES
        RETN

PRINT_DRIVE:
        MOV     AH,GET_DEFAULT_DRIVE
        INT     int_command
        ADD     AL,"A"
        JMP     OUT

;ASSUME  DS:TRANGROUP,ES:TRANGROUP
PWD:
        CALL    PRINT_DIRECTORY
        ;CALL	CRLF2
	;RETN
	JMP	CRLF2

PRINT_DEFAULT_DIRECTORY:
        ;MOV     BYTE PTR DS:[FCB],0
        MOV     BYTE [FCB],0
PRINT_DIRECTORY:
        ;MOV     DL,DS:[FCB]
        MOV     DL,[FCB]
        MOV     AL,DL
        ADD     AL,'@'
        CMP     AL,'@'
        JNZ     short GOTDRIVE
        ADD     AL,[CURDRV]
        INC     AL
GOTDRIVE:
        PUSH    AX
	;MOV     SI,OFFSET TRANGROUP:PWDBUF+3
	MOV     SI,PWDBUF+3
        MOV     AH,CURRENT_DIR
        INT     int_command
        JNC     short DPBISOK
        PUSH    CS
        POP     DS
        JMP     DRVBAD
DPBISOK:
	;MOV     DI,OFFSET TRANGROUP:PWDBUF
	MOV     DI,PWDBUF
        MOV     DX,DI
        POP     AX
        MOV     AH,DRVCHAR
        STOSW
        MOV     AL,[DIRCHAR]
        STOSB
        JMP     ZPRINT

_$EXIT:
        PUSH    ES
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        ;MOV     AX,[PARENT]
	MOV	AX,[ES:PARENT] ; 30/04/2018
        ;MOV     WORD PTR ES:[PDB_Parent_PID],AX
        MOV     [ES:PDB.PARENT_PID],AX

;IF IBM
	;CMP     byte [PERMCOM],0
	;JNZ     short NORESETVEC	;Don't reset the vector if a PERMCOM
        ;;LDS     DX,DWORD PTR ES:[SYS_CALL]
        ;LDS     DX,[ES:SYS_CALL]
;ASSUME  DS:NOTHING
        ;MOV     AX,(SET_INTERRUPT_VECTOR SHL 8) + INT_COMMAND
        ;MOV     AX,(SET_INTERRUPT_VECTOR*256) + INT_COMMAND
        ;INT     int_command
;NORESETVEC:
;ENDIF

        POP     ES
;ASSUME  ES:TRANGROUP
        ;MOV     ES,[TPA]
        ;MOV     ES,[SS:TPA]
        MOV     ES,[TPA] ; 30/04/2018
        MOV     AH,DEALLOC
        INT     int_command		; Now running in "free" space
	;MOV     AX,(EXIT SHL 8)
	MOV     AX,EXIT*256
        INT     int_command

CTTY:
        CALL    SETPATH         	; Get spec
        ;MOV     AX,(OPEN SHL 8) OR 2	; Read and write
        MOV     AX,(OPEN*256) | 2
        INT     int_command             ; Open new device
        JC	short ISBADDEV
        MOV     BX,AX
	;MOV     AX,IOCTL SHL 8
	MOV     AX,IOCTL*256
        INT     int_command
        TEST    DL,80H
        JNZ	short DEVISOK
        MOV     AH,CLOSE		; Close initial handle
        INT     int_command
ISBADDEV:
        ;MOV     DX,OFFSET TRANGROUP:BADDEV
        MOV     DX,BADDEV
        CALL    PRINT
        JMP     short RESRET

DEVISOK:
        XOR     DH,DH
        OR      DL,3            	; Make sure has CON attributes
        ;MOV     AX,(IOCTL SHL 8) OR 1
        MOV     AX,(IOCTL*256) | 1
        INT     int_command
        PUSH    BX                      ; Save handle
        MOV     CX,3
        XOR     BX,BX
ICLLOOP:                                ; Close basic handles
        MOV     AH,CLOSE
        INT     int_command
        INC     BX
        LOOP    ICLLOOP
        POP     BX              	; Get handle
        MOV     AH,XDUP
        INT     int_command             ; Dup it to 0
        MOV     AH,XDUP
        INT     int_command             ; Dup to 1
        MOV     AH,XDUP
        INT     int_command             ; Dup to 2
        MOV     AH,CLOSE        	; Close initial handle
        INT     int_command
RESRET:
        ;MOV     DS,[SS:RESSEG]
        MOV     DS,[RESSEG] ; 30/04/2018
;ASSUME  DS:RESGROUP
        PUSH    DS
        ;MOV	AX,WORD PTR DS:[PDB_JFN_Table] ; Get new 0 and 1
        MOV	AX,[PDB.JFN_TABLE]
        MOV     [IO_SAVE],AX
        ;MOV	AX,OFFSET RESGROUP:LODCOM1
        MOV     AX,LODCOM1  ; (Transient portion loader address)	
        PUSH    AX
;ZMMMM	PROC FAR
        ;RET				; Force header to be checked
	RETF
;ZMMMM	ENDP

;TRANCODE ENDS
        ;END

;=============================================================================
; TCODE3.ASM
;=============================================================================

;TITLE   PART3 - COMMAND Transient routines.

;TRANCODE  SEGMENT PUBLIC BYTE
;ASSUME  CS:TRANGROUP,DS:NOTHING,ES:NOTHING,SS:NOTHING

; ASSUME  DS:RESGROUP

FORTERM:
        MOV     byte [FORFLAG],0
        CMP     word [SINGLECOM],0FF00H
        JNZ	short NOFORP2
        MOV	word [SINGLECOM],-1	; Cause a terminate
NOFORP2:
        JMP     TCOMMAND

FORPROC:
;ASSUME  DS:RESGROUP
        CMP     byte [FORUFCB],-1
        JZ	short NORMFOR
        ;MOV     DX,OFFSET TRANGROUP:DIRBUF
        MOV     DX,DIRBUF
        PUSH    DS
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        MOV     AH,SET_DMA
        INT     int_command
        POP     DS
;ASSUME  DS:RESGROUP
        ;MOV     DX,OFFSET RESGROUP:FORFCB
        MOV     DX,FORFCB
        MOV     AH,DIR_SEARCH_NEXT
        CMP     byte [FORUFCB],0
        JZ	short DOFORSRCH
        MOV     AH,DIR_SEARCH_FIRST
        MOV	byte [FORUFCB],0
DOFORSRCH:
        INT     int_command
        OR      AL,AL
        JNZ	short FORTERM
        PUSH    DS
        POP     ES
;ASSUME  ES:RESGROUP
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        ;MOV     SI,OFFSET TRANGROUP:DIRBUF
        MOV     SI,DIRBUF
	;MOV     DI,OFFSET RESGROUP:FORSET
	MOV     DI,FORSET
        ;MOV     [FORPTR],DI
        MOV     [ES:FORPTR],DI
        LODSB				;Get drive spec
        ADD     AL,'@'
        CMP     AL,'@'
        JZ	short NDRV8
        ;CMP     [UFORDRV],0
        CMP     byte [ES:UFORDRV],0
        JZ	short NDRV8
        MOV     AH,':'
        STOSW
NDRV8:
        CALL    FCB_TO_ASCZ
        ;MOV     BYTE PTR ES:[DI-1],0DH
        MOV     BYTE [ES:DI-1],0DH
        PUSH    ES
        POP     DS
;ASSUME  DS:RESGROUP
NORMFOR:
        PUSH    CS
        POP     ES
;ASSUME  ES:TRANGROUP
        MOV     BX,[FORPTR]
        CMP     BYTE [BX],0
        JZ	short FORTERM
        MOV     SI,BX
PARMSUB0:
        LODSB
        CMP     AL,0DH
        JNZ	short PARMSUB0
        MOV     DX,SI           ; DX points to next parm
        ;MOV     SI,OFFSET RESGROUP:FORCOM
        MOV     SI,FORCOM
        ;MOV     DI,OFFSET TRANGROUP:COMBUF+2
        MOV     DI,COMBUF+2
        XOR     CX,CX
TFORCOM:
        LODSB
        CMP     AL,'%'
        JNZ	short NOFORPARM
        MOV     AH,[FORVAR]
        CMP     AH,[SI]
        JNZ	short NOFORPARM
        INC     SI
        PUSH    SI
        MOV     SI,BX
PARMSUB:
        LODSB
        CMP     AL,0DH
        JZ	short PARMSUBDONE
        INC     CX
        STOSB
        JMP     SHORT PARMSUB
PARMSUBDONE:
        POP     SI			; Get back command line pointer
        JMP	short TFORCOM
NOFORPARM:
        STOSB
        INC     CX
        CMP     AL,0DH
        JNZ	short TFORCOM
        DEC     CX
        ;MOV     [COMBUF+1],CL
        ;MOV     [SS:COMBUF+1],CL
        MOV     [CS:COMBUF+1],CL
        MOV     [FORPTR],DX		; Point to next set element
        TEST    byte [ECHOFLAG],-1
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        JZ	short NOECHO3
        ;MOV     BYTE PTR ES:[DI-1],'$'
	MOV	BYTE [ES:DI-1],'$'
	;MOV     DX,OFFSET TRANGROUP:COMBUF+2
	MOV     DX,COMBUF+2
        CALL    PRINT
        ;MOV     BYTE PTR ES:[DI-1],0DH
        MOV     BYTE [ES:DI-1],0DH
        JMP     DOCOM
NOECHO3:
        JMP     DOCOM1

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

FORNESTERR:
        PUSH    DS
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
	;MOV     DX,OFFSET TRANGROUP:FORNESTMES
	MOV     DX,FORNESTMES
        CMP     word [SINGLECOM],0FF00H
        JNZ	short NOFORP3
        MOV	word [SINGLECOM],-1	; Cause termination
NOFORP3:
        POP     DS
;ASSUME  DS:TRANGROUP
        JMP     CERROR

_$FOR:
        MOV     SI,81H
        XOR     CX,CX
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        ;MOV     DI,OFFSET RESGROUP:FORSET
        MOV     DI,FORSET
        XOR     AL,AL
        ;MOV     [UFORDRV],AL
        MOV     [ES:UFORDRV],AL
        ;XCHG    AL,[FORFLAG]
        XCHG    AL,[ES:FORFLAG]
        OR      AL,AL
        JNZ	short FORNESTERR
        ;MOV     [FORPTR],DI
        MOV     [ES:FORPTR],DI
        ;MOV     [FORUFCB],-1
        MOV     byte [ES:FORUFCB],-1
        CALL    SCANOFF
        LODSW
        CMP     AL,'%'
        JNZ	short FORERRORJ
        ;MOV     [FORVAR],AH
        MOV     [ES:FORVAR],AH
        CALL    SCANOFF
        CMP     AL,0DH
        JZ	short FORERRORJ2
        LODSW
        ;CMP     AX,('N' SHL 8) OR 'I'
	CMP     AX,4E49H ; 'IN'
        JZ	SHORT FOROK1
        ;CMP     AX,('n' SHL 8) OR 'i'
        CMP     AX,6E69H ; 'in'
        JNZ	SHORT FORERRORJ
FOROK1:
        CALL    SCANOFF
        LODSB
        CMP     AL,'('   ; 28H
        JNZ	short FORERRORJ
        CALL    SCANOFF
        CMP     AL,')'	 ; 29H		; Special check for null set
        JNZ	short FORSETLP
        MOV     DS,[RESSEG]
        JMP     FORTERM
FORSETLP:
        LODSB
        CMP     AL,0DH
FORERRORJ2:
        JZ	short FORERRORJ3
        CMP     AL,')'   ; 29H
        JZ	short FORSETEND
        STOSB
        CMP     AL,'*'	 ; 2AH
        JZ	short SETFORSCAN
        CMP     AL,'?'	 ; 3FH
        JNZ	short NOFORSCAN
SETFORSCAN:
        ;MOV     [FORUFCB],1
        MOV	byte [ES:FORUFCB],1
NOFORSCAN:
        CALL    DELIM
        JNZ	short FORSETLP
        MOV     BYTE [ES:DI-1],0DH
        CALL    SCANOFF
        JMP	short FORSETLP

FORSETEND:
        MOV     AX,000DH
        CMP     BYTE [ES:DI-1],0DH
        JNZ	short FORSETTERM
        XOR     AX,AX
FORSETTERM:
        STOSW
        CALL    SCANOFF
        LODSW
        ;CMP     AX,('O' SHL 8) OR 'D'
	CMP     AX,4F44H ; 'DO'
        JZ	short FOROK2
        ;CMP     AX,('o' SHL 8) OR 'd'
        CMP     AX,6F64H ; 'do'
FORERRORJ:
        JNZ	short FORERROR
FOROK2:
        CALL    SCANOFF
        CMP     AL,0DH
FORERRORJ3:
        JZ	short FORERROR
	;MOV     DI,OFFSET RESGROUP:FORCOM
	MOV     DI,FORCOM
FORCOMLP:
        LODSB
        STOSB
        CMP     AL,0DH
        JNZ	short FORCOMLP
        ;INC     [FORFLAG]
        INC     byte [ES:FORFLAG]
        ;CMP     [SINGLECOM],-1
        CMP     word [ES:SINGLECOM],-1
        JNZ	short NOFORP
	;MOV     [SINGLECOM],0FF00H	; Flag single command for
	MOV     word [ES:SINGLECOM],0FF00H
NOFORP:
        ;CMP     [FORUFCB],1
        CMP     byte [ES:FORUFCB],1
        ;retnz
	JNZ	short FOR_retn
        PUSH    ES
        POP     DS
;ASSUME  DS:RESGROUP
        ;MOV     DI,OFFSET RESGROUP:FORFCB
        MOV     DI,FORFCB
        ;MOV     SI,OFFSET RESGROUP:FORSET
        MOV     SI,FORSET
        CMP     BYTE [SI+1],':'  ;3AH
        JNZ	short NOSETUDRV
        INC     byte [UFORDRV]
NOSETUDRV:
        ;MOV     AX,PARSE_FILE_DESCRIPTOR SHL 8
        MOV     AX,PARSE_FILE_DESCRIPTOR*256
        INT     int_command
FOR_retn:
        RETN


;ASSUME  DS:TRANGROUP,ES:TRANGROUP

IFERRORP:
        POP     AX
IFERROR:
FORERROR:
	;MOV     DX,OFFSET TRANGROUP:SYNTMES
	MOV     DX,SYNTMES
        JMP     CERROR

_$IF:
        MOV     byte [IFNOTFLAG],0
        MOV     SI,81H
IFREENT:
        CALL    SCANOFF
        CMP     AL,0DH
        JZ	short IFERROR
        MOV     BP,SI
	;MOV     DI,OFFSET TRANGROUP:IFTAB ; Prepare to search if table
	MOV     DI,IFTAB
        MOV     CH,0
IFINDCOM:
        MOV     SI,BP
        MOV     CL,[DI]
        INC     DI
        JCXZ    IFSTRING
        JMP     SHORT FIRSTCOMP
IFCOMP:
        JNZ	short IFDIF
FIRSTCOMP:
        LODSB
        MOV     AH,[ES:DI]
        INC     DI
        CMP     AL,AH
        JZ	short IFLP
        OR      AH,20H          ; Try lower case
        CMP     AL,AH
IFLP:
        LOOP    IFCOMP
IFDIF:
        LAHF
        ADD     DI,CX           ; Bump to next position without affecting flags
        MOV     BX,[DI]         ; Get handler address
        INC     DI
        INC     DI
        SAHF
        JNZ	short IFINDCOM
        LODSB
        CMP     AL,0DH
IFERRORJ:
        JZ	short IFERROR
        CALL    DELIM
        JNZ	short IFINDCOM
        CALL    SCANOFF
        JMP     BX

IFNOT:
        NOT     byte [IFNOTFLAG]
        JMP     short IFREENT

IFSTRING:
        PUSH    SI
        XOR     CX,CX
FIRST_STRING:
        LODSB
        CMP     AL,0DH
        JZ	short IFERRORP
        CALL    DELIM
        JZ	short EQUAL_CHECK
        INC     CX
        JMP     SHORT FIRST_STRING
EQUAL_CHECK:
        CMP     AL,'='
        JZ	short EQUAL_CHECK2
        CMP     AL,0DH
        JZ	short IFERRORP
        LODSB
        JMP     SHORT EQUAL_CHECK
EQUAL_CHECK2:
        LODSB
        CMP     AL,'='
        JNZ	short IFERRORP
        CALL    SCANOFF
        CMP     AL,0DH
        JZ	short IFERRORP
        POP     DI
        REPE    CMPSB
        JZ	short MATCH
        CMP     BYTE [SI-1],0DH
        JZ	short IFERRORJ
SKIPSTRINGEND:
        LODSB
NOTMATCH:
        CMP     AL,0DH
IFERRORJ2:
        JZ	short IFERRORJ
        CALL    DELIM
        JNZ	short SKIPSTRINGEND
        MOV     AL,-1
        JMP     SHORT IFRET
MATCH:
        LODSB
        CALL    DELIM
        JNZ	short NOTMATCH
        XOR     AL,AL
        JMP     SHORT IFRET

IFEXISTS:
        ;MOV     DI,OFFSET TRANGROUP:DIRBUF
        MOV     DI,DIRBUF
        ;MOV     AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 01H
        MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 01H
        INT     int_command
        MOV     AH,FCB_OPEN
        MOV     DX,DI
        INT     int_command
IFRET:
        TEST    byte [IFNOTFLAG],-1
        JZ	short REALTEST
        NOT     AL
REALTEST:
        OR      AL,AL
        JZ	short IFTRUE
        JMP     TCOMMAND
IFTRUE:
        CALL    SCANOFF
        MOV     CX,SI
        SUB     CX,81H
        ;SUB     DS:[80H],CL
        SUB     [80H],CL
        ;MOV     CL,DS:[80H]
        MOV     CL,[80H]
        MOV     [COMBUF+1],CL
        ;MOV     DI,OFFSET TRANGROUP:COMBUF+2
        MOV     DI,COMBUF+2
        REP     MOVSB
        MOV     AL,0DH
        STOSB
        JMP     DOCOM1

IFERLEV:
        MOV     BH,10
        XOR     BL,BL
GETNUMLP:
        LODSB
        CMP     AL,0DH
        JZ	short IFERRORJ2
        CALL    DELIM
        JZ	short GOTNUM
        SUB     AL,'0'
        XCHG    AL,BL
        MUL     BH
        ADD     AL,BL
        XCHG    AL,BL
        JMP     SHORT GETNUMLP
GOTNUM:
        PUSH    DS
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        ;MOV     AH,BYTE PTR [RETCODE]
        MOV     AH,[RETCODE]
        POP     DS
;ASSUME  DS:TRANGROUP
        XOR     AL,AL
        CMP     AH,BL
        JAE	short IFRET
        DEC     AL
        JMP     SHORT IFRET

;ASSUME  DS:TRANGROUP

SHIFT:
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        MOV     AX,[BATCH]
        TEST    AX,-1
        ;retz
	JZ	short SHIFT_retn
        MOV     ES,AX
        MOV     DS,AX
;ASSUME  DS:NOTHING,ES:NOTHING
        XOR     CX,CX
        MOV     AX,CX
        MOV     DI,CX
        DEC     CX
        REPNZ   SCASB
        MOV     SI,DI
        INC     SI
        INC     SI
        MOV     CX,9
        REP     MOVSW                   ; Perform shift of existing parms
        CMP	WORD [DI],-1
        ;retz				; No new parm
	JZ	short SHIFT_retn
        MOV     SI,[DI]
        MOV     WORD [DI],-1		; Assume no parm
        ;MOV     DS,[RESSEG]
        ;MOV     DS,[SS:RESSEG]
        MOV     DS,[CS:RESSEG] ; 30/04/2018
;ASSUME  DS:RESGROUP
SKIPCRLP:
        LODSB
        CMP     AL,0DH
        JNZ	short SKIPCRLP
        CMP     BYTE [SI],0
	;retz				; End of parms
	JZ	short SHIFT_retn
        MOV     [ES:DI],SI		; Pointer to next parm as %9
SHIFT_retn:
GOTO_retn:
        RETN

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

GOTO:
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        TEST    word [BATCH],-1
        ;retz				; If not in batch mode, a nop
	JZ	short GOTO_retn
        XOR     DX,DX
        MOV     [BATLOC],DX		; Back to start
        MOV     [BATLOC+2],DX
        CALL    BATOPEN                 ; Find the batch file
        MOV     DI,FCB+1		; Get the label
        MOV     CX,11
        MOV     AL,' '
        REPNE   SCASB
        JNZ	short NOINC
        INC     CX
NOINC:
        SUB     CX,11
        NEG     CX
        ;MOV     [GOTOLEN],CX
        ;MOV     [SS:GOTOLEN],CX
        MOV     [CS:GOTOLEN],CX ; 30/04/2018
        CALL    GETBATBYT
        CMP     AL,':'
        JZ	short CHKLABEL
LABLKLP:				; Look for the label
        CALL    GETBATBYT
        CMP     AL,0AH
        JNZ	short LABLKTST
        CALL    GETBATBYT
        CMP     AL,':'
        JZ	short CHKLABEL
LABLKTST:
        TEST    word [BATCH],-1
        JNZ	short LABLKLP
        CALL    BATCLOSE
        PUSH    CS
        POP     DS
        ;MOV     DX,OFFSET TRANGROUP:BADLAB
        MOV     DX,BADLAB
        JMP     CERROR

CHKLABEL:
        MOV     DI,FCB+1
        ;MOV     CX,[GOTOLEN]
	;MOV     CX,[SS:GOTOLEN]
	MOV     CX,[CS:GOTOLEN] ; 30/04/2018
NEXTCHRLP:
        PUSH    CX
        CALL    GETBATBYT
        POP     CX
        OR      AL,20H
        CMP     AL,[ES:DI]
        JNZ	short TRYUPPER
        JMP     SHORT NEXTLABCHR
TRYUPPER:
        SUB     AL,20H
        CMP     AL,[ES:DI]
        JNZ	short LABLKTST
NEXTLABCHR:
        INC     DI
        LOOP    NEXTCHRLP
        CALL    GETBATBYT
        CMP     AL,' '
        JA	short LABLKTST
        CMP     AL,0DH
        JZ	short SKIPLFEED
TONEXTBATLIN:
        CALL    GETBATBYT
        CMP     AL,0DH
        JNZ	short TONEXTBATLIN
SKIPLFEED:
        CALL    GETBATBYT
BATCLOSE:
        ;MOV     BX,CS:[BATHAND]
        MOV     BX,[CS:BATHAND]
        MOV     AH,CLOSE
        INT     int_command
        RETN

BATOPEN:
;Open the BATCH file, If open fails, AL is drive of batch file (A=1)
;ASSUME  DS:RESGROUP,ES:TRANGROUP
        PUSH    DS
        MOV     DS,[BATCH]
;ASSUME  DS:NOTHING
        XOR     DX,DX
        ;MOV     AX,OPEN SHL 8
        MOV     AX,OPEN*256
        INT     int_command             ; Open the batch file
        JC	short SETERRDL
        POP     DS
;ASSUME  DS:RESGROUP
        ;MOV     [BATHAND],AX
        ;MOV     [SS:BATHAND],AX
        MOV     [CS:BATHAND],AX ; 30/04/2018
        MOV     BX,AX
        MOV     DX,[BATLOC]
        MOV     CX,[BATLOC+2]
        ;MOV     AX,LSEEK SHL 8          ; Go to the right spot
        MOV     AX,LSEEK*256
        INT     int_command
        RETN

SETERRDL:
        MOV     BX,DX
        MOV     AL,[BX]                 ; Get drive spec
        SUB     AL,'@'                  ; A = 1
        POP     DS
        STC                             ; SUB mucked over carry
MESTRAN_retn:
        RETN

MESTRAN:
;ASSUME  DS:NOTHING,ES:NOTHING
        LODSB
        CMP     AL,"$"
        ;retz
	JZ	short MESTRAN_retn
        STOSB
        JMP     MESTRAN
IOSET:
; ALL REGISTERS PRESERVED
;ASSUME  DS:NOTHING,ES:NOTHING,SS:NOTHING
        PUSH    DS
        PUSH    DX
        PUSH    AX
        PUSH    BX
        PUSH    CX
        ;MOV     DS,[RESSEG]
	MOV	DS, [CS:RESSEG]
;ASSUME  DS:RESGROUP
        CMP     byte [PIPEFLAG],0
        JNZ     short NOREDIR		; Don't muck up the pipe
        CALL    TESTDOREIN
        CALL    TESTDOREOUT
NOREDIR:
        POP     CX
        POP     BX
        POP     AX
        POP     DX
        POP     DS
;ASSUME  DS:NOTHING
        RETN

TESTDOREIN:
;ASSUME  DS:RESGROUP
        CMP     byte [RE_INSTR],0
	;retz
	JZ	short TESTDOREIN_retn
        ;MOV     DX,OFFSET RESGROUP:RE_INSTR
        MOV     DX,RE_INSTR
        ;MOV     AX,(OPEN SHL 8)
        MOV     AX,OPEN*256
        INT     int_command
        ;MOV     DX,OFFSET TRANGROUP:NOTFND
        MOV     DX,NOTFND
        JC      short REDIRERR
        MOV     BX,AX
        MOV     AL,0FFH
        ;XCHG    AL,[BX.PDB_JFN_Table]
        XCHG    AL,[BX+PDB.JFN_TABLE]
        ;MOV     DS:[PDB_JFN_Table],AL
        MOV     [PDB.JFN_TABLE],AL
TESTDOREIN_retn:
        RETN

REDIRERR:
        PUSH    CS
        POP     DS
        JMP     CERROR

TESTDOREOUT:
;ASSUME  DS:RESGROUP
        CMP     byte [RE_OUTSTR],0
        JZ	short NOREOUT
        CMP     byte [RE_OUT_APP],0
        JZ	short REOUTCRT
        ;MOV     DX,OFFSET RESGROUP:RE_OUTSTR
        MOV     DX,RE_OUTSTR
        ;MOV     AX,(OPEN SHL 8) OR 1
        MOV     AX,(OPEN*256) | 1
        INT     int_command
        JC	short REOUTCRT
        XOR     DX,DX
        XOR     CX,CX
        MOV     BX,AX
        ;MOV     AX,(LSEEK SHL 8) OR 2
        MOV     AX,(LSEEK*256) | 2
        INT     int_command
        JMP     SHORT SET_REOUT
REOUTCRT:
        ;MOV     DX,OFFSET RESGROUP:RE_OUTSTR
        MOV     DX,RE_OUTSTR
        XOR     CX,CX
        MOV     AH,CREAT
        INT     int_command
        ;MOV     DX,OFFSET TRANGROUP:FULDIR
        MOV     DX,FULDIR
        JC	short REDIRERR
        MOV     BX,AX
SET_REOUT:
        MOV     AL,0FFH
	;XCHG    AL,[BX.PDB_JFN_Table]
	XCHG    AL,[BX+PDB.JFN_TABLE]
	;MOV     DS:[PDB_JFN_Table+1],AL
	MOV	[PDB.JFN_TABLE+1],AL
NOREOUT:
        RETN	

STRCOMP:
; Compare ASCIZ DS:SI with ES:DI.
; SI,DI destroyed.
        CMPSB
        ;retnz				; Strings not equal
	JNZ	short NOREOUT	
        cmp     byte [SI-1],0		; Hit NUL terminator?
        ;retz				; Yes, strings equal
	JZ	short NOREOUT
        jmp     short STRCOMP           ; Equal so far, keep going

;TRANCODE  ENDS
;        END


;=============================================================================
; TCODE4.ASM
;=============================================================================

;TITLE   PART4 - COMMAND Transient routines.

;TRANCODE SEGMENT PUBLIC BYTE
;ASSUME  CS:TRANGROUP,DS:NOTHING,ES:NOTHING,SS:NOTHING

CATALOG:
        CALL    OKVOLARG
        MOV     AL,"?"                  ; *.* is default file spec.
        MOV     DI,5DH
        MOV     CX,11
        REP     STOSB
        MOV     SI,81H
        CALL    SWITCH
        MOV     DI,FCB
	          			; Parse with default name and extension
	;MOV     AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 0DH
	MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 0DH
        INT     int_command

; Begin by processing any switches that may have been specified.
; BITS will contain any information about switches that was
; found when the command line was parsed.

SETSWT:
        MOV     AX,[COMSW]		; Get switches from command
        OR      AX,[ARGTS]		; OR in switches from all of tail
        MOV     [_BITS],AX
        MOV     BYTE [FULLSCR],LINPERPAG
        TEST    AL,1                    ; Look for W switch
        MOV     AL,NORMPERLIN
        JZ	short DIR
        MOV     AL,WIDEPERLIN
DIR:
        MOV     [LINLEN],AL             ; Set number of entries per line
        MOV     [LINCNT],AL
        MOV     word [FILECNT],0     	; Keep track of how many files found
        ;MOV     DX,OFFSET TRANGROUP:DIRBUF ; Set Disk transfer address
        MOV     DX,DIRBUF		; Set Disk transfer address
        MOV     AH,SET_DMA
        INT     int_command
        CALL    PATHCRUNCH		; Get where we're going
        PUSHF
        JNC	short NOTEST
        CMP     byte [DESTISDIR],0	; No CHDIRs worked
        JZ	short NOTEST		; see if they should have
        JMP     BADCDERR

NOTEST:
        MOV     SI,FCB
        ;MOV     DI,OFFSET TRANGROUP:DIRBUF
        MOV     DI,DIRBUF
        MOV     DX,DI
        MOV     CX,12
        REP     MOVSB
        MOV     AH,FCB_OPEN
        INT     int_command
	;MOV     DX,OFFSET TRANGROUP:DIRHEAD_PRE ; Print "Directory of"
	MOV     DX,DIRHEAD_PRE		; Print "Directory of"
        PUSH    AX 			; save return code
        CALL    PRINT
        CALL    PWD			; print the path
	;MOV     DX,OFFSET TRANGROUP:DIRHEAD_POST
	MOV     DX,DIRHEAD_POST
        CALL    PRINT
        POP     AX
        OR      AL,AL
        JNZ	short OKDODIR		; Go ahead and dir if open fail
        TEST    byte [DIRBUF+SYS_FCB.DEVID],devid_device
        JZ	short OKDODIR
        JMP     NOTFNDERR		; Can't DIR a device
OKDODIR:
        MOV     AH,DIR_SEARCH_FIRST
        ;MOV     BYTE PTR DS:[FCB-7],0FFH
        ;MOV     BYTE PTR DS:[FCB-1],010H
        MOV     BYTE [FCB-7],0FFH
        MOV     BYTE [FCB-1],010H
        POPF
        JC	short SHOWDIR		; Current dir
        JZ	short DOFIRST		; FCB is *.*
        MOV     AL,"?"
        MOV     DI,5DH
        MOV     CX,11
        REP     STOSB			; Remake default FCB
        MOV     SI,[DESTTAIL]
        MOV     DI,FCB
				; Parse with default drive, name and extension
        ;MOV     AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 0EH
        MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 0EH
        INT     int_command
        MOV     AH,DIR_SEARCH_FIRST
DOFIRST:
        MOV     DX,FCB-7
        INT     int_command
        PUSH    AX
        CALL    RESTUDIR
        POP     AX
        JMP     SHORT DIRSTART

SHOWDIR:
        MOV     DX,FCB-7        	; DX -> Unopened FCB
        INT     int_command             ; Search for a file to match FCB
DIRSTART:
        INC     AL			; FF = file not found
        JNZ	short AGAIN		; Either an error or we are finished
        JMP     CHKCNT
NEXENTJ:
        JMP	NEXENT
AGAIN:
        INC     word [FILECNT]       	; Keep track of how many we find
        ;MOV	SI,OFFSET TRANGROUP:DIRBUF+8
        MOV	SI,DIRBUF+8 		; SI -> information returned by sys call
        CALL    SHONAME
        TEST    BYTE [_BITS],WSWITCH 	; W switch set?
        JNZ	short NEXENTJ		; If so, no size, date, or time
        ;MOV     SI,OFFSET TRANGROUP:DIRBUF+8+dir_attr
	MOV     SI,DIRBUF+8+dir_entry.dir_attr
        TEST    BYTE [SI],attr_directory
        JZ	short FILEENT
        ;MOV     DX,OFFSET TRANGROUP:DMES
        MOV     DX,DMES
        CALL    PRINT
        JMP     SHORT NOFSIZ
FILEENT:
        CALL    DISPSIZE		; Print size of file
NOFSIZ:
	;MOV     AX,WORD PTR [DIRBUF+8+dir_date]
	MOV     AX,[DIRBUF+8+dir_entry.dir_date]  ; Get date
        OR      AX,AX
        JZ	short NEXENT		; Skip if no date
	;MOV     DI,OFFSET TRANGROUP:CHARBUF
	MOV     DI,CHARBUF
        PUSH    AX
        MOV     AX,"  " ; 2020h
        STOSW
        POP     AX
        MOV     BX,AX
        AND     AX,1FH			; get day
        MOV     DL,AL
        MOV     AX,BX
        MOV     CL,5
        SHR     AX,CL			; Align month
        AND     AL,0FH			; Get month
        MOV     DH,AL
        MOV     CL,BH
        SHR     CL,1			; Align year
        XOR     CH,CH
        ADD     CX,80			; Relative 1980
        CMP     CL,100
        JB	short MILLENIUM
        SUB     CL,100
MILLENIUM:
        CALL    DATE_CXDX
        ;MOV     CX,WORD PTR[DIRBUF+8+dir_time]
        MOV     CX,[DIRBUF+8+dir_entry.dir_time]  ; Get time
        JCXZ    PRBUF			; Time field present?
        MOV     AX,"  " ; 2020h
        STOSW
        SHR     CX,1
        SHR     CX,1
        SHR     CX,1
        SHR     CL,1
        SHR     CL,1            ; Hours in CH, minutes in CL
        MOV     BL,[INTERNATVARS+INTERNAT_BLOCK.Time_24]
        OR      BL,80H          ; Tell P_TIME called from DIR
        CALL    P_TIME          ; Don't care about DX, never used with DIR
PRBUF:
        XOR     AX,AX
        STOSB
        ;MOV     DX,OFFSET TRANGROUP:CHARBUF
        MOV     DX,CHARBUF
        CALL    ZPRINT
NEXENT:
        DEC	byte [LINCNT]
        JNZ     short SAMLIN
NEXLIN:
        MOV     AL,[LINLEN]
        MOV     [LINCNT],AL
        CALL    CRLF2
        TEST    BYTE [_BITS],PSWITCH ; P switch present?
        JZ	short SCROLL	; If not, just continue
        DEC     BYTE [FULLSCR]
        JNZ	short SCROLL
        MOV     BYTE [FULLSCR],LINPERPAG
	;MOV     DX,OFFSET TRANGROUP:PAUSEMES
	MOV     DX,PAUSEMES
        CALL    PRINT
        CALL    GetKeystroke
        CALL    CRLF2
SCROLL:
        MOV     AH,DIR_SEARCH_NEXT
        JMP     SHOWDIR

SAMLIN:
        MOV     AL,9            ; Output a tab
        CALL    OUT
        JMP     SHORT SCROLL

SHONAME:
        ;MOV     DI,OFFSET TRANGROUP:CHARBUF
	MOV     DI,CHARBUF
        MOV     CX,8
        REP     MOVSB
        MOV     AL," "
        STOSB
        MOV     CX,3
        REP     MOVSB
        XOR     AX,AX
        STOSB
        PUSH    DX
	;MOV     DX,OFFSET TRANGROUP:CHARBUF
	MOV     DX,CHARBUF
        CALL    ZPRINT
        POP     DX
        RETN

ONESPC:
        MOV     AL," "
        JMP     OUT

CRLF2:
        PUSH    DX
        ;MOV     DX,OFFSET TRANGROUP:ACRLF
        MOV     DX,ACRLF
PR:
        PUSH    DS
        PUSH    CS
        POP     DS
        CALL    PRINT
        POP     DS
        POP     DX
        RETN

PAUSE:
        ;MOV     DX,OFFSET TRANGROUP:PAUSEMES
        MOV     DX,PAUSEMES
        CALL    ERROR_PRINT
        CALL    GetKeystroke
        CALL    CRLF2
PAUSE_retn:
        RETN

ERASE:
        ;MOV     DX,OFFSET TRANGROUP:BADARGS
        MOV     DX,BADARGS
        MOV     SI,80H
        LODSB
        OR      AL,AL
        JZ	short ERRJ2
        CALL    SCANOFF
        CMP     AL,13			; RETURN KEY?
        JZ	short ERRJ2		; IF SO NO PARAMETERS SPECIFIED

ERA1:
        CALL    PATHCRUNCH
        JNC	short NOTEST2J
        CMP	byte [DESTISDIR],0	; No CHDIRs worked
        JZ	short NOTEST2J		; see if they should have
BADCDERR:
        ;MOV     DX,OFFSET TRANGROUP:BADCD
        MOV     DX,BADCD
ERRJ2:
        JMP     CERROR

NOTEST2J:
        JMP     NOTEST2

CRENAME:
        CALL    PATHCRUNCH
        JNC	short NOTEST3
        CMP	byte [DESTISDIR],0	; No CHDIRs worked
        JZ	short NOTEST3		; see if they should have
        JMP	short BADCDERR

NOTEST3:
        MOV     SI,[PATHPOS]
        MOV     DI,FCB+10H
	CALL    SCANOFF
	;MOV	AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 01H
	MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 01H
        INT     int_command
	;CMP	BYTE PTR DS:[FCB+10H+1]," "  ; Check if parameter exists
	CMP	BYTE [FCB+10H+1]," " ; 20h
	;MOV     DX,OFFSET TRANGROUP:BADARGS
	MOV     DX,BADARGS
        JZ	short ERRJ		; Error if missing parameter
        MOV     AH,FCB_RENAME
        MOV     DX,FCB
        INT     int_command
        PUSH    AX
        CALL    RESTUDIR
        POP     AX
	;MOV     DX,OFFSET TRANGROUP:RENERR
	MOV     DX,RENERR
        INC     AL
        ;retnz
	jnz	short PAUSE_retn
ERRJ:
        JMP     CERROR

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

TYPEFIL:
        mov     si,81H
        call    SCANOFF         	; Skip to first non-delim
        cmp     al,0DH
        jnz	short GOTTARG
        jmp     NOARGERR        	; No args
GOTTARG:
        CALL    SETPATH
        ;MOV     AX,OPEN SHL 8
        MOV     AX,OPEN*256
        INT     int_command
        ;MOV     DX,OFFSET TRANGROUP:NOTFND
        MOV     DX,NOTFND
        JC	short ERRJ
        MOV     BX,AX           	; Handle
        MOV     DS,[TPA]
        XOR     DX,DX
;ASSUME  DS:NOTHING
TYPELP:
        ;MOV     CX,[BYTCNT]
        ;MOV     CX,[SS:BYTCNT]
        MOV     CX,[CS:BYTCNT] ; 30/04/2018
        MOV     AH,READ
        INT     int_command
        MOV     CX,AX
        JCXZ    RET56
        PUSH    BX
        MOV     BX,1
        MOV     AH,WRITE
        INT     int_command
        POP     BX
        JC	short ERROR_OUTPUTJ
        CMP     AX,CX
        JZ	short TYPELP
        DEC     CX
        CMP     AX,CX
        ;retz				; One less byte OK (^Z)
	jz	short RET56
ERROR_OUTPUTJ:
        MOV     BX,1
        ;MOV     AX,IOCTL SHL 8
        MOV     AX,IOCTL*256
        INT     int_command
        TEST    DL,devid_ISDEV
	;retnz				; If device, no error message
	jnz	short RET56
        JMP     ERROR_OUTPUT

RESTUDIR1:
        PUSH    DS
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        CMP     byte [RESTDIR],0
        POP     DS
;ASSUME  DS:TRANGROUP
        ;retz
	jz	short RET56
RESTUDIR:
        ;MOV     DX,OFFSET TRANGROUP:USERDIR1
	MOV     DX,USERDIR1
        MOV     AH,CHDIR
        INT     int_command             ; Restore users DIR
        XOR     AL,AL
        CALL    SETREST
RET56:
        ;return
	RETN

VOLUME:
        mov     si,81H
        call    SCANOFF         ; Skip to first non-delim
        ;CMP     BYTE PTR DS:[FCB],0	;Default drive?
        CMP     BYTE [FCB],0
        JZ	short CHECKNOARG	;Yes
        INC     SI
        INC     SI      		;Skip over d:
        MOV     BX,SI
        CALL    SCANOFF
        CMP     BX,SI
        JNZ	short OKVOLARG	; If we skipped some delims at this point, OK
CHECKNOARG:
        cmp     al,0DH
        JZ	short OKVOLARG
BADVOLARG:
        ;MOV	DX,OFFSET TRANGROUP:BADDRV
        MOV	DX,BADDRV
        JMP     CERROR

OKVOLARG:
        CALL    CRLF2
        PUSH    DS
        POP     ES
        MOV     DI,FCB-7        ; Set up extended FCB
        MOV     AX,-1
        STOSB
        XOR     AX,AX
        STOSW
        STOSW
        STOSB
        MOV     AL,8            ; Look for volume label
        STOSB
        INC     DI              ; Skip drive byte
        MOV     CX,11
        MOV     AL,'?'
        REP     STOSB
        ;MOV     DX,OFFSET TRANGROUP:DIRBUF
        MOV     DX,DIRBUF
        MOV     AH,SET_DMA
        INT     int_command
        MOV     DX,FCB-7
        MOV     AH,DIR_SEARCH_FIRST
        INT     int_command
        JMP	PRINTVOL

VERSION:
        CALL    CRLF2
        CALL    PRINT_VERSION
        JMP     CRLF2

PRINT_VERSION:
        ;MOV     DI,OFFSET TRANGROUP:CHARBUF
	MOV     DI,CHARBUF
	;MOV     SI,OFFSET TRANGROUP:VERMES_PRE
	MOV     SI,VERMES_PRE
        CALL    MESTRAN
        MOV     AH,GET_VERSION
        INT     int_command
        PUSH    AX
        XOR     AH,AH
        MOV     CL,10
        DIV     CL
        MOV     CL,4
        SHL     AL,CL
        OR      AL,AH
        MOV     CX,1110H
        MOV     DL,AL
        CALL    OUTBYTE
        MOV     AL,'.'
        STOSB
        POP     AX
        MOV     AL,AH
        XOR     AH,AH
        MOV     CL,10
        DIV     CL
        MOV     CL,4
        SHL     AL,CL
        OR      AL,AH
        MOV     CX,1010H
        MOV     DL,AL
        CALL    OUTBYTE
        ;MOV     SI,OFFSET TRANGROUP:VERMES_POST
        MOV     SI,VERMES_POST
        CALL    MESTRAN
        XOR     AX,AX
        STOSB
        ;MOV     DX,OFFSET TRANGROUP:CHARBUF
        MOV     DX,CHARBUF
        JMP     ZPRINT

;ASSUME  DS:TRANGROUP

CLS:
        ;IF      IBMVER
        MOV     BX,1
        ;MOV     AX,IOCTL SHL 8
        MOV     AX,IOCTL*256
        INT     int_command
        TEST    DL,devid_ISDEV
        JZ	short ANSICLS	; If a file put out ANSI
        TEST    DL,devid_SPECIAL
        JZ	short ANSICLS	; If not special CON, do ANSI
        ;MOV     AX,(GET_INTERRUPT_VECTOR SHL 8) OR 29H
        MOV     AX,(GET_INTERRUPT_VECTOR*256) | 29H
        INT     int_command
        MOV     DX,ES
        ;MOV     AX,(GET_INTERRUPT_VECTOR SHL 8) OR 20H
        MOV     AX,(GET_INTERRUPT_VECTOR*256) | 20H
        INT     int_command
        MOV     AX,ES
        CMP     DX,AX
        JA	short ANSICLS	; If not default driver, do ANSI
        MOV     AH,0Bh          ; Set overscan to black
        XOR     BX,BX
        INT     10h
        MOV     AH,0Fh
        INT     10h
        MOV     DL,AH
        DEC     DL

        ;IF      KANJI
        ;MOV     DH,23
        ;ELSE
        MOV     DH,25
        ;ENDIF

        XOR     AX,AX
        MOV     CX,AX

        ;IF      KANJI
        ;MOV     BH,0
        ;ELSE
        MOV     BH,7
        ;ENDIF

        MOV     AH,6
        INT     10h
        XOR     DX,DX
        MOV     BH,0
        MOV     AH,2
        INT     10h
        ;return
	RETN

ANSICLS:
        ;ENDIF

	;MOV     SI,OFFSET TRANGROUP:CLSSTRING
	MOV     SI,CLSSTRING
        LODSB
        MOV     CL,AL
        XOR     CH,CH
        MOV     AH,RAW_CON_IO
CLRLOOP:
        LODSB
        MOV     DL,AL
        INT     int_command
        LOOP    CLRLOOP
ANSICLS_retn:
        RETN

_$CHDIR:
        MOV     AX,[COMSW]
        OR      AX,[ARGTS]
        ;MOV     DX,OFFSET TRANGROUP:BADSWT
        MOV     DX,BADSWT
        JNZ	short CERRORJ3
        mov     si,81H
        call    SCANOFF         ; Skip to first non-delim
        cmp     al,0DH
        jz	short PWDJ 	; No args
        inc     si              ; Skip first char
        lodsw
        ;cmp	ax,(0DH SHL 8) OR ':' ; d:<CR> ?
        cmp	ax,0D3Ah	; d:<CR> ?
        jnz	short REALCD	; no
PWDJ:
        jmp     PWD             ; Drive only specified
REALCD:
        CALL    SETPATH
        TEST    byte [DESTINFO],2
        JNZ     BADCDERRJ
        MOV     AH,CHDIR
        INT     int_command
        ;retnc
	jnc	short ANSICLS_retn
BADCDERRJ:
        JMP	BADCDERR

_$MKDIR:
        CALL    SETRMMK
        JNZ	short BADMDERR
        MOV     AH,MKDIR
        INT     int_command
        ;retnc
	jnc	short ANSICLS_retn
BADMDERR:
        ;MOV     DX,OFFSET TRANGROUP:BADMKD
	MOV     DX,BADMKD
CERRORJ3:
        JMP	CERROR

NOARGERR:
        ;MOV     DX,OFFSET TRANGROUP:BADARGS
        MOV     DX,BADARGS
        JMP     SHORT CERRORJ3

SETRMMK:
        mov     si,81H
        call    SCANOFF         ; Skip to first non-delim
        cmp     al,0DH
        jz	short NOARGERR	; No args
        MOV     AX,[COMSW]
        OR      AX,[ARGTS]
        ;MOV     DX,OFFSET TRANGROUP:BADSWT
	MOV     DX,BADSWT
        JNZ	short CERRORJ3
        CALL    SETPATH
        TEST	byte [DESTINFO],2
SETRMMK_retn:
        RETN

_$RMDIR:
        CALL    SETRMMK
        JNZ	short BADRDERR
        MOV     AH,RMDIR
        INT     int_command
        ;retnc
	jnc	short SETRMMK_retn
BADRDERR:
        ;MOV     DX,OFFSET TRANGROUP:BADRMD
        MOV     DX,BADRMD
        JMP     CERROR

SAVUDIR:
; DL is drive number A=1
        ;MOV     DI,OFFSET TRANGROUP:USERDIR1
	MOV     DI,USERDIR1
SAVUDIR1:
        MOV     AL,DL
        ADD     AL,'@'
        CMP     AL,'@'
        JNZ	short GOTUDRV
        ADD     AL,[CURDRV]
        INC     AL		; A = 1
GOTUDRV:
        STOSB
        MOV     AH,[DIRCHAR]
        MOV     AL,DRVCHAR
        STOSW
        PUSH    ES
        POP     DS
;ASSUME  DS:NOTHING
        MOV     SI,DI
        MOV     AH,CURRENT_DIR	; Get the Directory Text
        INT     int_command
        ;retc
	jc	short SAVUDIR_retn
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
SAVUDIR_retn:
        RETN

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

; Date and time are set during initialization and use
; this routines since they need to do a long return

;DATINIT PROC    FAR

DATINIT:
        PUSH    ES
        PUSH    DS              ; Going to use the previous stack
        MOV     AX,CS           ; Set up the appropriate segment registers
        MOV     ES,AX
        MOV     DS,AX
        ;MOV     DX,OFFSET TRANGROUP:INTERNATVARS ;Set up internat vars
        MOV     DX,INTERNATVARS
        ;MOV     AX,INTERNATIONAL SHL 8
        MOV     AX,INTERNATIONAL*256
        INT     21H
        ;MOV     WORD PTR DS:[81H],13
        MOV     word [81H],13 ; Want to prompt for date during initialization
        MOV     byte [COMBUF],COMBUFLEN ; Init COMBUF
        MOV     WORD [COMBUF+1],0D01H
        CALL    DATE
        CALL    CTIME
        POP     DS
        POP     ES
        RETF

;DATINIT ENDP

; DATE - Gets and sets the time

DATE_CXDX:
        MOV     BX,CX
P_DATE:
        MOV     AX,BX
        MOV     CX,DX
        MOV     DL,100
        DIV     DL
        XCHG    AL,AH
        XCHG    AX,DX
        MOV     BH,"0"-" "      ; Enable leading zero suppression
        ;MOV     AX,WORD PTR [INTERNATVARS.Date_tim_format]
        MOV     AX,[INTERNATVARS+INTERNAT_BLOCK.Date_tim_format]
        OR      AX,AX
        JZ	short USPDAT
        DEC     AX
        JZ	short EUPDAT
        MOV     BH,0            ; Disable leading zero suppression
        CALL    P_YR
        CALL    P_DSEP
        CALL    P_MON
        CALL    P_DSEP
        CALL    P_DAY
        RETN

USPDAT:
        CALL    P_MON
        CALL    P_DSEP
        CALL    P_DAY
PLST:
        CALL    P_DSEP
        CALL    P_YR
        RETN

EUPDAT:
        CALL    P_DAY
        CALL    P_DSEP
        CALL    P_MON
        JMP	short PLST

P_MON:
        MOV     AL,CH
        CALL    OUT2
        RETN

P_DSEP:
        ;MOV     AL,BYTE PTR [INTERNATVARS.Date_sep]
        MOV     AL,[INTERNATVARS+INTERNAT_BLOCK.Date_sep]
        STOSB
        RETN

P_DAY:
        MOV     AL,CL
        CALL    OUT2
	RETN

P_YR:
        MOV     AL,DH
        OR      AL,AL
        JZ	short TWODIGYR	; Two instead of 4 digit year
        CALL    OUT2
TWODIGYR:
        MOV     AL,DL
        CALL    OUT2
        RETN

DATE:
        MOV     SI,81H          ; Accepting argument for date inline
        CALL    SCANOFF
        CMP     AL,13
        JZ	short PRMTDAT
DATE_retn:
        JMP	short COMDAT

PRMTDAT:
	;MOV     DX,OFFSET TRANGROUP:CURDAT_PRE
	MOV     DX,CURDAT_PRE
        CALL    PRINT           ; Print "Current date is "
        CALL    PRINT_DATE
	;MOV     DX,OFFSET TRANGROUP:CURDAT_POST
	MOV     DX,CURDAT_POST
        CALL    PRINT
GETDAT:
	;MOV     DX,OFFSET TRANGROUP:NEWDAT
	MOV     DX,NEWDAT
        CALL    ERROR_PRINT     ; Print "Enter new date: "
        MOV     AH,STD_CON_STRING_INPUT
        ;MOV     DX,OFFSET TRANGROUP:COMBUF
        MOV     DX,COMBUF
        INT     int_command	; Get input line
        CALL    CRLF2
	;MOV     SI,OFFSET TRANGROUP:COMBUF+2
	MOV     SI,COMBUF+2
        CMP     BYTE [SI],13	; Check if new date entered
        ;retz
	jz	short DATE_retn
COMDAT:
        ;MOV     AX,WORD PTR [INTERNATVARS.Date_tim_format]
        MOV     AX,[INTERNATVARS+INTERNAT_BLOCK.Date_tim_format]
        OR      AX,AX
        JZ	short USSDAT
        DEC     AX
        JZ	short EUSDAT
        CALL    GET_YR
        JC	short DATERRJ
        CALL    GET_DSEP
        JC	short DATERRJ
        CALL    GET_MON
        JC	short DATERRJ
        CALL    GET_DSEP
        JC	short DATERRJ
        CALL    GET_DAY
DAT_SET:
        JC	short DATERR
        LODSB
        CMP     AL,13
        JNZ	short DATERR
        MOV     AH,SET_DATE
        INT     int_command
        OR      AL,AL
        JNZ	short DATERR
DAT_SET_retn:
        RETN

USSDAT:
        CALL    GET_MON
        JC	short DATERR
        CALL    GET_DSEP
DATERRJ:
        JC	short DATERR
        CALL    GET_DAY
TGET:
        JC	short DATERR
        CALL    GET_DSEP
        JC	short DATERR
        CALL    GET_YR
        JMP	short DAT_SET

EUSDAT:
        CALL    GET_DAY
        JC	short DATERR
        CALL    GET_DSEP
        JC	short DATERR
        CALL    GET_MON
        JMP	short TGET

GET_MON:
        CALL    GETNUM          ; Get one or two digit number
        ;retc
	jc	short  DAT_SET_retn
        MOV     DH,AH           ; Put in position
	RETN

GET_DAY:
        CALL    GETNUM
        MOV     DL,AH           ; Put in position
GET_DAY_retn:
        RETN

GET_YR:
        CALL    GETNUM
        ;retc
	jc	short GET_DAY_retn
        MOV     CX,1900
        CALL    GET_DSEP
        PUSHF
        DEC     SI
        POPF
        JZ      short BIAS
        CMP     BYTE [SI],13
        JZ      short BIAS
        MOV     AL,100
        MUL     AH
        MOV     CX,AX
        CALL    GETNUM
	;retc
	jc	short GET_DAY_retn
BIAS:
        MOV     AL,AH
        MOV     AH,0
        ADD     CX,AX

        ;IF IBM AND KANJI
;
; Gross hack for PC-J machine: CMOS clock cannot handle years after 2079
;
	;CMP	CX,2080
	;JB	short YearOk
        ;STC
        ;return
;YearOk: CLC
        ;ENDIF
BIAS_retn:
        ;return
	RETN

DATERR:
        ;MOV     DX,OFFSET TRANGROUP:BADDAT
        MOV     DX,BADDAT
        CALL    PRINT
        JMP     GETDAT

GET_DSEP:
        LODSB
        CMP     AL,'/'
        ;retz
	jz	short BIAS_retn
        CMP     AL,'.'
        ;retz
	jz	short BIAS_retn
        CMP     AL,'-'
        ;retz
	jz	short BIAS_retn
        STC
        ;return
	RETN

; TIME gets and sets the time

CTIME:
        MOV     SI,81H                  ; Accepting argument for time inline
        CALL    SCANOFF
        CMP     AL,13
        JZ	short PRMTTIM
        MOV     BX, 2E3Ah ; ":."  ; 01/05/2018
        CALL    INLINE
        JMP	COMTIM

PRINT_TIME:
        MOV     AH,GET_TIME
        INT     int_command              ; Get time in CX:DX
        PUSH    DI
        PUSH    ES
        PUSH    CS
        POP     ES
        ;MOV     DI,OFFSET TRANGROUP:CHARBUF
        MOV     DI,CHARBUF
        MOV     BL,1           		; Always 24 hour time
        CALL    P_TIME
        XOR     AX,AX
        STOSB
        ;MOV     DX,OFFSET TRANGROUP:CHARBUF
        MOV     DX,CHARBUF
        CALL    ZPRINT
        POP     ES
        POP     DI
P_TIME_retn:
        RETN

P_TIME:
        MOV     AL,CH
        TEST    BL,07FH         ; Ignore high bit
        JNZ	short T24	; 24 hr time?
        MOV     BH,"a"          ; Assume A.M.
        CMP     AL,12           ; In the afternoon?
        JB	short MORN
        MOV     BH,"p"
        JE	short MORN
        SUB     AL,12           ; Keep it to 12 hours or less
MORN:
        OR      AL,AL           ; Before 1 am?
        JNZ	short T24
        MOV     AL,12
T24:
        PUSH    BX
        MOV     BH,"0"-" "      ; Enable leading zero suppression
        CALL    OUT2
        CALL    P_TSEP
        MOV     AL,CL
        CALL    OUT2
        POP     BX
        PUSH    BX
        TEST    BL,80H
        JNZ	short PAP	; If from DIR, go directly to am pm
        MOV     BH,0            ; Disable leading zero suppression
        CALL    P_TSEP
        MOV     AL,DH
        CALL    OUT2
        ;IF NOT IBMJAPAN
        MOV     AL,"."
        STOSB
        MOV     AL,DL
        CALL    OUT2
        ;ENDIF
PAP:
        POP     BX
        TEST    BL,07FH         ; Ignore high bit
	;retnz			; 24 hour time, no am pm
	jnz	short P_TIME_retn
        MOV     AL,BH
        STOSB                   ; Store 'a' or 'p'
;P_TIME_retn:
        RETN

P_TSEP:
        ;MOV     AL,[INTERNATVAR.Time_sep]
        MOV     AL,[INTERNATVARS+INTERNAT_BLOCK.Time_sep]
        STOSB
P_TSEP_retn:
        RETN

PRMTTIM:
	;MOV     DX,OFFSET TRANGROUP:CURTIM_PRE
	MOV     DX,CURTIM_PRE
        CALL    PRINT           ; Print "Current time is "
        CALL    PRINT_TIME
        ;MOV     DX,OFFSET TRANGROUP:CURTIM_POST
        MOV     DX,CURTIM_POST
        CALL    PRINT
GETTIM:
        XOR     CX,CX           ; Initialize hours and minutes to zero
        ;MOV     DX,OFFSET TRANGROUP:NEWTIM
        MOV     DX,NEWTIM
        MOV     BX, 2E3Ah ; ":."
        CALL    GETBUF
COMTIM:
       ;retz			; If no time present, don't change it
	jz	short P_TSEP_retn
        JC	short TIMERR
        MOV     CX,DX
        XOR     DX,DX
        LODSB
        CMP     AL,13
        JZ	short SAVTIM
        CMP     AL,BL
        JZ	short GOTSEC
        CMP     AL,BH
        JNZ 	short TIMERR
GOTSEC:
        CALL    GETNUM
        JC	short TIMERR
        MOV     DH,AH           ; Position seconds
        LODSB
        CMP     AL,13
        JZ	short SAVTIM
        CMP     AL,"."
        JNZ	short TIMERR
        CALL    GETNUM
        JC	short TIMERR
        MOV     DL,AH
        LODSB
        CMP     AL,13
        JNZ	short TIMERR
SAVTIM:
        MOV     AH,SET_TIME
        INT     int_command
        OR      AL,AL
        ;retz                    ; Error in time?
	jz	short P_TSEP_retn
TIMERR:
        ;MOV     DX,OFFSET TRANGROUP:BADTIM
        MOV     DX,BADTIM
        CALL    PRINT           ; Print error message
        JMP	short GETTIM	; Try again

GETBUF:
        CALL    ERROR_PRINT     ; Print "Enter new time: "
        MOV     AH,STD_CON_STRING_INPUT
	;MOV     DX,OFFSET TRANGROUP:COMBUF
	MOV     DX,COMBUF
        INT     int_command	; Get input line
        CALL    CRLF2
	;MOV     SI,OFFSET TRANGROUP:COMBUF+2
	MOV     SI,COMBUF+2
        CMP     BYTE [SI],13	; Check if new time entered
        ;retz
	jz	short P_TSEP_retn

INLINE:
        CALL    GETNUM          ; Get one or two digit number
        ;retc
	jc	short P_TSEP_retn
        MOV     DH,AH           ; Put in position
        LODSB
        CMP     AL,BL
        JZ	short NEXT
        CMP     AL,BH
        JZ	short NEXT
        DEC     SI              ; Clears zero flag
        CLC
        MOV     DL,0
        RETN			; Time may have only an hour specified

NEXT:
        CALL    GETNUM
        MOV     DL,AH           ; Put in position
        RETN

;TRANCODE	ENDS
;		END

;=============================================================================
; TCODE5.ASM
;=============================================================================

;TITLE   PART5 - COMMAND Transient routines.

;TRANCODE SEGMENT PUBLIC BYTE
;ASSUME  CS:TRANGROUP,DS:NOTHING,ES:NOTHING,SS:NOTHING

FREE_TPA:
;ASSUME  DS:TRANGROUP,ES:NOTHING
        PUSH    ES
        MOV     ES,[TPA]
        MOV     AH,DEALLOC
        INT     int_command		; Make lots of free memory
        POP     ES
        RETN

ALLOC_TPA:
;ASSUME DS:TRANGROUP,ES:RESGROUP
        MOV     BX,0FFFFH		; Re-allocate the transient
        MOV     AH,ALLOC
        INT     int_command
        MOV     AH,ALLOC
        INT     int_command
        ;MOV     [LTPA],AX		; Re-compute evrything
        MOV     [ES:LTPA],AX
        MOV     [TPA],AX
        MOV     BX,AX
        MOV     AX,CS
        SUB     AX,BX
        MOV     DX,16
        MUL     DX
        OR      DX,DX
        JZ	short SAVSIZ2
        MOV     AX,-1
SAVSIZ2:
        MOV     [BYTCNT],AX
        RETN


PRESCAN:                        	; Cook the input buffer
;ASSUME  DS:TRANGROUP,ES:TRANGROUP
        XOR     CX,CX
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        ;MOV     SI,OFFSET TRANGROUP:COMBUF+2
        MOV     SI,COMBUF+2
        MOV     DI,SI

CountQuotes:
        LODSB                           ; get a byte
        CMP     AL,22h                  ; is it a quote?
        JNZ	SHORT CountEnd		; no, try for end of road
        INC     CH                      ; bump count
        JMP	short CountQuotes	; go get next char
CountEnd:
        CMP     AL,13                   ; end of road?
        JNZ	short CountQuotes	; no, go back for next char

        ;IF      KANJI
        ;PUSH    CX			; save count
        ;MOV     SI,DI			; get back beginning of buffer
;KanjiScan:
        ;LODSB				; get a byte
        ;CALL    TestKanj 		; is it a leadin byte
        ;JZ      KanjiQuote		; no, check for quotes
        ;MOV     AH,A			; save leadin
        ;LODSB				; get trailing byte
        ;CMP     AX,8140h		; is it Kanji space
        ;JNZ     KanjiScan		; no, go get next
        ;MOV     [SI-2],2020h		; replace with spaces
        ;JMP     KanjiScan		; go get next char
;KanjiQuote:
        ;CMP     AL,22h			; beginning of quoted string
        ;JNZ     KanjiEnd		; no, check for end
        ;DEC     CH			; drop count
        ;JZ      KanjiScan		; if count is zero, no quoting
;KanjiQuoteLoop:
        ;LODSB				; get next byte
        ;CMP     AL,22h			; is it another quote
        ;JNZ     KanjiQuoteLoop		; no, get another
        ;DEC     CH			; yes, drop count
        ;JMP     KanjiScan		; go get next char
;KanjiEnd:
        CMP     AL,13                   ; end of line character?
        ;JNZ     KanjiScan		; go back to beginning
        ;POP     CX			; get back original count
        ;ENDIF

        MOV     SI,DI                   ; restore pointer to begining
PRESCANLP:
        LODSB

        ;IF      KANJI
        ;CALL    TESTKANJ
        ;JZ      NOTKANJ6
        ;MOV     [DI],AL
        ;INC     DI			; fake STOSB into DS
        ;LODSB				; grab second byte
        ;MOV     [DI],AL		; fake stosb into DS
        ;INC     DI
        ;INC     CL
        ;INC     CL
        ;JMP     PRESCANLP
;NOTKANJ6:
        ;ENDIF

        CMP     AL,22H			; " character
        JNZ	short TRYGREATER
        DEC     CH
        JZ	short TRYGREATER
QLOOP:
        MOV     [DI],AL
        INC     DI
        INC     CL
        LODSB
        CMP     AL,22H			; " character
        JNZ	short QLOOP
        DEC     CH

TRYGREATER:
        CMP     AL,'>'
        JNZ	short NOOUT
        CMP     BYTE [SI],'>'
        JNZ	short NOAPPND
        LODSB
	;INC	[RE_OUT_APP]		; Flag >>
	INC	byte [ES:RE_OUT_APP]
NOAPPND:
        CALL    SCANOFF
        CMP     AL,0DH
        JNZ	short GOTREOFIL
        ;MOV     WORD PTR [RE_OUTSTR],09H ; Cause an error later
        MOV     WORD [ES:RE_OUTSTR],09H
        JMP     SHORT PRESCANEND
GOTREOFIL:
        PUSH    DI
        ;MOV     DI,OFFSET RESGROUP:RE_OUTSTR
        MOV     DI,RE_OUTSTR
SETREOUTSTR:                            ; Get the output redirection name
        LODSB
        CMP     AL,0DH
        JZ	short GOTRESTR
        CALL    DELIM
        JZ	short GOTRESTR
        CMP     AL,[SWITCHAR]
        JZ	short GOTRESTR
        STOSB                           ; store it into resgroup
        JMP     SHORT SETREOUTSTR

NOOUT:
        CMP     AL,'<'
        JNZ	short CHKPIPE
        CALL    SCANOFF
        CMP     AL,0DH
        JNZ	short GOTREIFIL
        ;MOV     WORD PTR [RE_INSTR],09H ; Cause an error later
        MOV     WORD [ES:RE_INSTR],09H
        JMP     SHORT PRESCANEND
GOTREIFIL:
        PUSH    DI
        ;MOV     DI,OFFSET RESGROUP:RE_INSTR
        MOV     DI,RE_INSTR
        JMP     SHORT SETREOUTSTR       ; Get the input redirection name

CHKPIPE:
        MOV     AH,AL
        CMP     AH,'|' ; 7Ch
        JNZ	short CONTPRESCAN
        ;INC	[PIPEFLAG]
        INC	byte [ES:PIPEFLAG]
        CALL    SCANOFF
        CMP     AL,0DH
        JZ	short PIPEERRSYNJ5
        CMP     AL,'|'          	; Double '|'?
        JNZ	short CONTPRESCAN
PIPEERRSYNJ5:
        PUSH    ES
        POP     DS              	; DS->RESGROUP
        JMP     PIPEERRSYN

GOTRESTR:
        XCHG    AH,AL
        ;CMP     BYTE PTR ES:[DI-1],':'	; Trailing ':' OK on devices
	CMP     BYTE [ES:DI-1],':'  ; 3Ah
        JNZ	short NOTTRAILCOL
        DEC     DI              	; Back up over trailing ':'
NOTTRAILCOL:
        XOR     AL,AL
        STOSB                   	; NUL terminate the string
        POP     DI              	; Remember the start
CONTPRESCAN:
        MOV     [DI],AH         	; "delete" the redirection string
        INC     DI
        CMP     AH,0DH
        JZ      PRESCANEND
        INC     CL
        JMP     PRESCANLP
PRESCANEND:
        ;CMP     [PIPEFLAG],0
        CMP	byte [ES:PIPEFLAG],0
        JZ	short ISNOPIPE
        ;MOV     DI,OFFSET RESGROUP:PIPESTR
        MOV     DI,PIPESTR
        ;MOV     [PIPEPTR],DI
        MOV     [ES:PIPEPTR],DI
	;MOV     SI,OFFSET TRANGROUP:COMBUF+2
	MOV     SI,COMBUF+2
        CALL    SCANOFF
PIPESETLP:                      ; Transfer the pipe into the resident pipe buffer
        LODSB
        STOSB
        CMP     AL,0DH
        JNZ	short PIPESETLP
ISNOPIPE:
        MOV     [COMBUF+1],CL
        ;CMP     [PIPEFLAG],0
	CMP     byte [ES:PIPEFLAG],0
        PUSH    CS
        POP     ES
        RETN

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

PATHCHRCMP:
        CMP     byte [SWITCHAR],'/'
        JZ	short NOSLASHT
        CMP     AL,'/'
	;retz
	jz	short PATHCHRCMP_retn
NOSLASHT:
        CMP     AL,'\'
PATHCHRCMP_retn:
        RETN

PATHCRUNCH:
; Drive taken from FCB
; DI = Dirsave pointer
;
; Zero set if path dir, CHDIR to this dir, FCB filled with ?
; NZ set if path/file, CHDIR to file, FCB has file (parsed fill ' ')
;       [DESTTAIL] points to parse point
; Carry set if no CHDIRs worked, FCB not altered.
; DESTISDIR set non zero if PATHCHRs in path (via SETPATH)

        ;MOV     DL,DS:[FCB]
        MOV     DL,[FCB]
        CALL    SAVUDIR
        CALL    SETPATH
        TEST    byte [DESTINFO],2
        JNZ     short TRYPEEL 		; If ? or * cannot be pure dir
        MOV     AH,CHDIR
        INT     int_command
        JC	short TRYPEEL
        CALL    SETREST1
        MOV     AL,"?"          	; *.* is default file spec if pure dir
        MOV     DI,5DH
        MOV     CX,11
        REP     STOSB
        XOR     AL,AL           	; Set zero
        RETN

TRYPEEL:
        MOV     SI,[PATHPOS]
        DEC     SI              	; Point at NUL
        MOV     AL,[SI-1]

        ;IF      KANJI
        ;CMP     [KPARSE],0
        ;JNZ     DELSTRT	; Last char is second KANJI byte, might be '\'
        ;ENDIF

        CALL    PATHCHRCMP
        JZ	short PEELFAIL		; Trailing '/'

        ;IF      KANJI
;DELSTRT:
        ;MOV     CX,SI
        ;MOV     SI,DX
        ;PUSH    DX
;DELLOOP:
        ;CMP     SI,CX
        ;JZ      GOTDELE
        ;LODSB
        ;CALL    TESTKANJ
        ;JZ      NOTKANJ8
        ;INC     SI
        ;JMP     DELLOOP
;NOTKANJ8:
        ;CALL    PATHCHRCMP
        ;JNZ     DELLOOP
        ;MOV     DX,SI
        ;DEC     DX
        ;JMP     DELLOOP
;GOTDELE:
        ;MOV     SI,DX
        ;POP     DX
        ;CMP     SI,DX
	;JZ      BADRET
        ;MOV     CX,SI
        ;MOV     SI,DX
;DELLOOP2:			; Set value of KPARSE
        ;CMP     SI,CX
        ;JZ      KSET
        ;MOV     [KPARSE],0
        ;LODSB
        ;CALL    TESTKANJ
        ;JZ      DELLOOP2
        ;INC     SI
        ;INC     [KPARSE]
        ;JMP     DELLOOP2
;KSET:
        ;ELSE
DELLOOP:
        CMP     SI,DX
        JZ	short BADRET
        MOV     AL,[SI]
        CALL    PATHCHRCMP
        JZ	short TRYCD
        DEC     SI
        JMP     SHORT DELLOOP
        ;ENDIF

TRYCD:
        CMP     BYTE [SI+1],'.'
        JZ	short PEELFAIL		; If . or .., pure cd should have worked
        mov     al,[si-1]
        CMP     al,DRVCHAR		; Special case dDRVCHAR,DIRCHARfile
        JZ	short BADRET

        ;IF      KANJI
        ;CMP     [KPARSE],0
        ;JNZ     NOTDOUBLESL	; Last char is second KANJI byte, might be '\'
        ;ENDIF

        CALL    PATHCHRCMP
        JNZ	short NOTDOUBLESL
PEELFAIL:
        STC					; //
        RETN
NOTDOUBLESL:
        MOV     BYTE [SI],0
        MOV     AH,CHDIR
        INT     int_command
        JNC	short CDSUCC
PATHCRUNCH_retn:
        RETN

BADRET:
        MOV     AL,[SI]
        CALL    PATHCHRCMP			; Special case 'DIRCHAR'file
        STC
        ;retnz
        jnz	short PATHCRUNCH_retn
	XOR     BL,BL
        XCHG    BL,[SI+1]
        MOV     AH,CHDIR
        INT     int_command
	;retc
	jc	short PATHCRUNCH_retn
        MOV     [SI+1],BL
CDSUCC:
        CALL    SETREST1
        INC     SI				; Reset zero
        MOV     [DESTTAIL],SI
        MOV     DI,FCB
        ;MOV     AX,(PARSE_FILE_DESCRIPTOR SHL 8) OR 02H
        MOV     AX,(PARSE_FILE_DESCRIPTOR*256) | 02H ; Parse with default drive
	INT     int_command
        RETN

DISPSIZE:
        ;MOV     SI,WORD PTR[DIRBUF+29+7]
        MOV     SI,[DIRBUF+29+7]
	;MOV     DI,WORD PTR[DIRBUF+31+7]
	MOV     DI,[DIRBUF+31+7]

DISP32BITS:
; Prints the 32-bit number DI:SI on the console in decimal. Uses a total
; of 9 digit positions with leading blanks.
        XOR     AX,AX
        MOV     BX,AX
        MOV     BP,AX
        MOV     CX,32
CONVLP:
        SHL     SI,1
        RCL     DI,1
        XCHG    AX,BP
        CALL    CONVWRD
        XCHG    AX,BP
        XCHG    AX,BX
        CALL    CONVWRD
        XCHG    AX,BX
        ADC     AL,0
        LOOP    CONVLP

; Conversion complete. Print 9-digit number.

        ;MOV     DI,OFFSET TRANGROUP:CHARBUF
        MOV     DI,CHARBUF
        MOV     CX,1810H        ; Allow leading zero blanking for 8 digits
        XCHG    DX,AX
        CALL    DIGIT
        XCHG    AX,BX
        CALL    OUTWORD
        XCHG    AX,BP
        CALL    OUTWORD
        XOR     AX,AX
        STOSB
        ;MOV     DX,OFFSET TRANGROUP:CHARBUF
        MOV     DX,CHARBUF
        JMP     ZPRINT

OUTWORD:
        PUSH    AX
        MOV     DL,AH
        CALL    OUTBYTE
        POP     DX
OUTBYTE:
        MOV     DH,DL
        SHR     DL,1
        SHR     DL,1
        SHR     DL,1
        SHR     DL,1
        CALL    DIGIT
        MOV     DL,DH
DIGIT:
        AND     DL,0FH
        JZ	short BLANKZER
        MOV     CL,0
BLANKZER:
        DEC     CH
        AND     CL,CH
        OR      DL,30H
        SUB     DL,CL
        MOV     AL,DL
        STOSB
        RETN

CONVWRD:
        ADC     AL,AL
        DAA
        XCHG    AL,AH
        ADC     AL,AL
        DAA
        XCHG    AL,AH
        RETN

GETBATBYT:
; Get one byte from the batch file and return it in AL. End-of-file
; returns <CR> and ends batch mode. DS must be set to resident segment.
; AH, CX, DX destroyed.

;ASSUME  DS:RESGROUP

        ADD     WORD [BATLOC],1		; Add one to file location
        ADC     WORD [BATLOC+2],0
        PUSH    BX
        ;MOV     DX,OFFSET RESGROUP:BATBYT
        MOV     DX,BATBYT
        ;MOV     BX,[BATHAND]
        ;MOV     BX,[SS:BATHAND]
        MOV     BX,[CS:BATHAND] ; 30/04/2018
        MOV     AH,READ
        MOV     CX,1
        INT     int_command		; Get one more byte from batch file
        POP     BX
        MOV     CX,AX
        JC	short BATEOF
        JCXZ    BATEOF
        MOV     AL,[BATBYT]
        CMP     AL,1AH
        ;retnz
	jnz	short GETBATBYT_retn
BATEOF:
        PUSH    ES
        MOV     ES,[BATCH]		; Turn off batch
        MOV     AH,DEALLOC
        INT     int_command             ; free up the batch piece
        POP     ES
        MOV     word [BATCH],0		; AFTER DEALLOC in case of ^C
        CALL    BATCLOSE
        MOV     AL,0DH			; If end-of-file, then end of line
        CMP     word [SINGLECOM],0FFF0H	; See if we need to set SINGLECOM
        JNZ     NOSETSING2
        MOV     word [SINGLECOM],-1	; Cause termination
NOSETSING2:
        MOV     byte [ECHOFLAG],1
GETBATBYT_retn:
        RETN

;ASSUME  DS:TRANGROUP

SCANOFF:
        LODSB
        CALL    DELIM
        JZ 	short SCANOFF
        DEC     SI			; Point to first non-delimiter
        RETN

DELIM:
        CMP     AL," "
        ;retz
	jz	short DELIM_retn
        CMP     AL,"="
        ;retz
	jz	short DELIM_retn
        CMP     AL,","
        ;retz
	jz	short DELIM_retn
        CMP     AL,";"
        ;retz
	jz	short DELIM_retn
        CMP     AL,9			; Check for TAB character
DELIM_retn:
        RETN

PRINT_PROMPT:
        PUSH    DS
        PUSH    CS
        POP     DS              	; MAKE SURE DS IS IN TRANGROUP

        PUSH    ES
        CALL    FIND_PROMPT     	; LOOK FOR PROMPT STRING
        JC	short PP0		; CAN'T FIND ONE
        CMP     BYTE [ES:DI],0
        JNZ	short PP1
PP0:
        CALL    PRINT_DRIVE     	; USE DEFAULT PROMPT
        MOV     AL,SYM  	; 3Eh	; '>'
        CALL    OUT
        JMP     SHORT PP5

PP1:
        MOV     AL,[ES:DI]		; GET A CHAR
        INC     DI
        OR      AL,AL
        JZ	short PP5		; NUL TERMINATED
        CMP     AL,"$"          	; META CHARACTER?
        JZ	short PP2		; NOPE
PPP1:
        CALL    OUT
        JMP	short PP1

PP2:
        MOV     AL,[ES:DI]
        INC     DI
        ;MOV     BX,OFFSET TRANGROUP:PROMPT_TABLE-3
        MOV     BX,PROMPT_TABLE-3
        OR      AL,AL
        JZ	short PP5

PP3:
        ADD     BX,3
        CALL    UPCONV
        CMP     AL,[BX]
        JZ	short PP4
        CMP     BYTE [BX],0
        JNZ	short PP3
        JMP	short PP1

PP4:
        PUSH    ES
        PUSH    DI
        PUSH    CS
        POP     ES
        CALL    word [BX+1]
        POP     DI
        POP     ES
        JMP	short PP1

PP5:
        POP     ES              	; RESTORE SEGMENTS
        POP     DS
        RETN

PRINT_BACK:
        ;MOV     DX,OFFSET TRANGROUP:DBACK
        MOV     DX,DBACK
        JMP     ZPRINT

PRINT_EQ:
        MOV     AL,"="
        JMP     SHORT OUTV
PRINT_ESC:
        MOV     AL,1BH
        JMP     SHORT OUTV
PRINT_G:
        MOV     AL,">"
        JMP     SHORT OUTV
PRINT_L:
        MOV     AL,"<"
        JMP     SHORT OUTV
PRINT_B:
        MOV     AL,"|"
OUTV:
        JMP     OUT

SETPATH:
; Get an ASCIZ argument from the unformatted parms
; DESTISDIR set if pathchars in string
; DESTINFO  set if ? or * in string
        MOV     SI,80H
        LODSB
        XOR     AH,AH
        MOV     [PATHCNT],AX
        MOV     [PATHPOS],SI
GETPATH:
        MOV     byte [DESTINFO],0
        MOV     byte [DESTISDIR],0
        MOV     SI,[PATHPOS]
        MOV     CX,[PATHCNT]
        MOV     DX,SI
        JCXZ    PATHDONE
        PUSH    CX
        PUSH    SI
        CALL    SWITCH
        MOV     [PATHSW],AX
        POP     BX
        SUB     BX,SI
        POP     CX
        ADD     CX,BX
        MOV     DX,SI
SKIPPATH:

        ;IF	KANJI
        ;MOV	[KPARSE],0
;SKIPPATH2:
        ;ENDIF

        JCXZ	PATHDONE
        DEC     CX
        LODSB

        ;IF	KANJI
        ;CALL	TESTKANJ
        ;JZ	TESTPPSEP
        ;DEC	CX
        ;INC	SI
        ;INC	[KPARSE]
        ;JMP	SKIPPATH2
;TESTPPSEP:
        ;ENDIF

        CALL    PATHCHRCMP
        JNZ	short TESTPMETA
        INC	byte [DESTISDIR]
TESTPMETA:
        CMP     AL,'?'
        JNZ	short TESTPSTAR
        OR      byte [DESTINFO],2
TESTPSTAR:
        CMP     AL,'*'
        JNZ	short TESTPDELIM
        OR      byte [DESTINFO],2
TESTPDELIM:
        CALL    DELIM
        JZ	short PATHDONEDEC
        CMP     AL,[SWITCHAR]
        JNZ	short SKIPPATH
PATHDONEDEC:
        DEC     SI
PATHDONE:
        XOR     AL,AL
        XCHG    AL,[SI]
        INC     SI
        CMP     AL,0DH
        JNZ	short NOPSTORE
        MOV     [SI],AL			;Don't loose the CR
NOPSTORE:
        MOV     [PATHPOS],SI
        MOV     [PATHCNT],CX
        RETN

PGETARG:
        MOV     SI,80H
        LODSB
        OR      AL,AL
        ;retz
	jz	short PGETARG_retn
        CALL    PSCANOFF
        CMP     AL,13
PGETARG_retn:
        RETN

PSCANOFF:
        LODSB
        CALL    DELIM
        JNZ	short PSCANOFFD
        CMP     AL,';'
        JNZ	short PSCANOFF		; ';' is not a delimiter
PSCANOFFD:
        DEC     SI			; Point to first non-delimiter
        RETN

PATH:
        CALL    FIND_PATH
        CALL    PGETARG         	; Pre scan for arguments
        JZ	short DISPPATH		; Print the current path
        CALL    DELETE_PATH     	; DELETE ANY OFFENDING NAME
        CALL    SCAN_DOUBLE_NULL
        CALL    MOVE_NAME       	; MOVE IN PATH=
        CALL    PGETARG
        CMP     AL,';'          	; NUL path argument?
        JZ	short GOTPATHS
PATHSLP:                        	; Get the user specified path
        LODSB
        CMP     AL,0DH
        JZ	short GOTPATHS

        ;IF      KANJI
        ;CALL    TESTKANJ
        ;JZ      NOTKANJ2
        ;CALL    STORE_CHAR
        ;LODSB
        ;CALL    STORE_CHAR
        ;JMP     SHORT PATHSLP
;NOTKANJ2:
        ;ENDIF

        CALL    UPCONV
        CMP     AL,';'          	; ';' not a delimiter on PATH
        JZ	short NOTDELIM
        CALL    DELIM
        JZ	short GOTPATHS
NOTDELIM:
        CALL    STORE_CHAR
        JMP     SHORT PATHSLP

GOTPATHS:
        XOR     AX,AX
        STOSW
        RETN

DISPPATH:
        CALL    PRINT_PATH
        CALL    CRLF2
        RETN

PRINT_PATH:
        CMP     BYTE [ES:DI],0
        JNZ	short PATH1
PATH0:
        ;MOV     DX,OFFSET TRANGROUP:NULPATH
        MOV     DX,NULPATH
        PUSH    CS
        POP     DS
        JMP     PRINT
PATH1:
        PUSH    ES
        POP     DS
        SUB     DI,5
        MOV     DX,DI
;ASSUME  DS:RESGROUP
        CALL    SCASB2                  ; LOOK FOR NUL
        CMP     CX,0FFH
        JZ	short PATH0
        JMP     ZPRINT

FCB_TO_ASCZ:                            ; Convert DS:SI to ASCIZ ES:DI
        MOV     CX,8
MAINNAME:
        LODSB
        CMP     AL,' '
        JZ	short SKIPSPC
        STOSB
SKIPSPC:
        LOOP    MAINNAME
        LODSB
        CMP     AL,' '
        JZ	short GOTNAME
        MOV     AH,AL
        MOV     AL,'.'
        STOSB
        XCHG    AL,AH
        STOSB
        MOV     CL,2
EXTNAME:
        LODSB
        CMP     AL,' '
        JZ	short GOTNAME
        STOSB
        LOOP    EXTNAME

GOTNAME:
        XOR     AL,AL
        STOSB
FCB_TO_ASCZ_retn:
        RETN

GETNUM:
        CALL    INDIG
	;retc
	;jc	short FCB_TO_ASCZ_retn
	jc	short GETNUM_retn
        MOV     AH,AL           	; Save first digit
        CALL    INDIG           	; Another digit?
        JC      short OKRET
        AAD                     	; Convert unpacked BCD to decimal
        MOV     AH,AL
OKRET:
        OR      AL,1
GETNUM_retn:
        RETN

INDIG:
        MOV     AL,[SI]
        SUB     AL,"0"
        ;retc
	jc	short GETNUM_retn
        CMP     AL,10
        CMC
        ;retc
	jc	short GETNUM_retn
        INC     SI
        RETN

OUT2:   ; Output binary number as two ASCII digits
        AAM                     	; Convert binary to unpacked BCD
        XCHG    AL,AH
        OR      AX,3030H        	; Add "0" bias to both digits
        CMP     AL,"0"          	; Is MSD zero?
        JNZ	short NOSUP
        SUB     AL,BH           	; Suppress leading zero if enabled
NOSUP:
        MOV     BH,0            	; Disable zero suppression
        STOSW
        RETN

OUT:
; Print char in AL without affecting registers
        XCHG    AX,DX
        PUSH    AX
        CALL    OUT_CHAR
        POP     AX
        XCHG    AX,DX
        RETN

OUT_CHAR:
        PUSH    DS
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX
        PUSH    CS
        POP     DS
        ;MOV     BX,OFFSET TRANGROUP:CHARBUF
        MOV     BX,CHARBUF
        MOV     [BX],DL
        MOV     DX,BX
        MOV     BX,1
        MOV     CX,BX
        MOV     AH,WRITE
        INT     int_command
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     DS
        RETN


ERROR_PRINT:
        PUSH    AX
        PUSH    BX
        MOV     AL,"$"
        MOV     BX,2            	;STD ERROR
        JMP     SHORT STRING_OUT

CRPRINT:
        PUSH    AX
        MOV     AL,13
        JMP     SHORT _Z$PRINT
PRINT:                          	;$ TERMINATED STRING
        PUSH    AX
        MOV     AL,"$"
        JMP     SHORT _Z$PRINT
ZPRINT:
        PUSH    AX
        XOR     AX,AX           	;NUL TERMINATED STRING
_Z$PRINT:
        PUSH    BX
        MOV     BX,1            	;STD CON OUT
;
; output string terminated by AL to handle BX, DS:DX points to string
;
STRING_OUT:
        PUSH    CX
        PUSH    DI
        MOV     DI,DX
        MOV     CX,-1
        PUSH    ES
        PUSH    DS
        POP     ES
        REPNZ   SCASB           	; LOOK FOR TERMINATOR
        POP     ES
        NEG     CX
        DEC     CX
        DEC     CX
;
; WRITE CHARS AT DS:DX TO HANDLE IN BX, COUNT IN CX
;
        MOV     AH,WRITE
        INT     int_command
        JC	short ERROR_OUTPUT
        CMP     AX,CX
        JNZ	short ERROR_OUTPUT
        POP     DI
        POP     CX
        POP     BX
        POP     AX
        RETN

ERROR_OUTPUT:
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        MOV     ES,[RESSEG]
;ASSUME  ES:RESGROUP
        ;MOV     DX,OFFSET TRANGROUP:NOSPACE
        MOV     DX,NOSPACE
        ;CMP     [PIPEFLAG],0
        CMP	byte [ES:PIPEFLAG],0
        JZ	short GO_TO_ERROR
        ;MOV     [PIPEFLAG],0
        MOV     byte [ES:PIPEFLAG],0
        ;MOV     DX,OFFSET TRANGROUP:PIPEEMES
        MOV     DX,PIPEEMES
GO_TO_ERROR:
        JMP     CERROR

;TRANCODE   ENDS
;           END

;=============================================================================
; TUCODE.ASM
;=============================================================================

;TITLE   COMMAND Language midifiable Code Transient

;TRANCODE	SEGMENT PUBLIC BYTE

;ASSUME  CS:TRANGROUP,DS:TRANGROUP,ES:TRANGROUP,SS:NOTHING

; ARE YOU SURE prompt when deleting *.*

NOTEST2:
        MOV     CX,11
        MOV     SI,FCB+1
AMBSPEC:
        LODSB
        CMP     AL,"?"
        JNZ	short ALLFIL
        LOOP    AMBSPEC
ALLFIL:
        CMP     CX,0
        JNZ	short NOPRMPT
ASKAGN:
        ;MOV	DX,OFFSET TRANGROUP:SUREMES ; "Are you sure (Y/N)?"
        MOV	DX,SUREMES
        CALL    PRINT
        MOV     SI,80H
        MOV     DX,SI
        MOV     WORD [SI],120		; zero length
        ;MOV	AX,(STD_CON_INPUT_FLUSH SHL 8) OR STD_CON_STRING_INPUT
        MOV	AX,(STD_CON_INPUT_FLUSH*256) | STD_CON_STRING_INPUT
        INT     int_command
        LODSW
        OR      AH,AH
        JZ	short ASKAGN
        CALL    SCANOFF
        OR      AL,20H                  ; Convert to lower case
        CMP     AL,'n'
        JZ	short RETERA
        CMP     AL,'y'
        PUSHF
        CALL    CRLF2
        POPF
        JNZ	short ASKAGN
NOPRMPT:
        MOV     AH,FCB_DELETE
        MOV     DX,FCB
        INT     int_command
        PUSH    AX
        CALL    RESTUDIR
        POP     AX
        ;MOV     DX,OFFSET TRANGROUP:NOTFND
        MOV     DX,NOTFND
        INC     AL
        JZ	short CERRORJ
RETERA:
        RET

; ECHO, BREAK, and VERIFY commands. Check for "ON" and "OFF"

ECHO:
;ASSUME  DS:TRANGROUP,ES:TRANGROUP
        CALL    ON_OFF
        JC	short DOEMES
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        JNZ	short ECH_OFF
        MOV	byte [ECHOFLAG],1
        RET
ECH_OFF:
        MOV     byte [ECHOFLAG],0
        RETN

;ASSUME  DS:TRANGROUP
DOEMES:
        ;MOV     AL,BYTE PTR DS:[80H]
        MOV     AL,[80H]
        CMP     AL,2
        JB	short PECHO		; Gota have at least 2 characters
        MOV     DX,82H                  ; Skip one char after "ECHO"
        CALL    CRPRINT
        JMP     CRLF2

PECHO:
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        MOV     BL,[ECHOFLAG]
        PUSH    CS
        POP     DS
;ASSUME  DS:TRANGROUP
        ;MOV     DX,OFFSET TRANGROUP:ECHOMES
        MOV     DX,ECHOMES
        JMP     SHORT PYN

CERRORJ:
        JMP     CERROR

; is rest of line blank?
IsBlank:
        MOV     SI,81h                  ; point at text spot
        CALL    SCANOFF                 ; skip separators
        SUB     SI,81h                  ; number of characters advanced
        MOV     CX,SI                   ; put count in byte addressable spot
	;CMP	CL,DS:[80h]             ; compare with count
	CMP     CL,[80h]
        RETN				; bye!

;The BREAK command
CNTRLC:
        CALL    ON_OFF
        ;MOV     AX,(SET_CTRL_C_TRAPPING SHL 8) OR 1
        MOV     AX,(SET_CTRL_C_TRAPPING*256) | 1
        JC	short PCNTRLC
        JNZ	short CNTRLC_OFF
        MOV     DL,1
        INT     int_command             ; Set ^C
        RETN
CNTRLC_OFF:
        XOR     DL,DL
        INT     int_command             ; Turn off ^C check
        RETN

PCNTRLC:
        CALL    IsBlank                 ; rest of line blank?
        JNZ	short CERRORJ		; no, oops!
        XOR     AL,AL
        INT     int_command
        MOV     BL,DL
	;MOV	DX,OFFSET TRANGROUP:CTRLCMES
	MOV	DX,CTRLCMES
PYN:
        CALL    PRINT
        ;MOV	DX,OFFSET TRANGROUP:ONMES
	MOV     DX,ONMES
        OR      BL,BL
        JNZ	short PRINTVAL
        ;MOV	DX,OFFSET TRANGROUP:OFFMES
        MOV	DX,OFFMES
PRINTVAL:
        JMP     PRINT

VERIFY:
        CALL    ON_OFF
        ;MOV	AX,(SET_VERIFY_ON_WRITE SHL 8) OR 1
        MOV	AX,(SET_VERIFY_ON_WRITE*256) | 1
        JC	short PVERIFY
        JNZ	short VER_OFF
        INT     int_command             ; Set verify
        RETN
VER_OFF:
        DEC     AL
        INT     int_command             ; Turn off verify after write
        RETN

PVERIFY:
        CALL    IsBlank                 ; is rest of line blank?
        JNZ	short CERRORJ		; nope...
        MOV     AH,GET_VERIFY_ON_WRITE
        INT     int_command
        MOV     BL,AL
        ;MOV     DX,OFFSET TRANGROUP:VERIMES
        MOV     DX,VERIMES
        JMP	short PYN

ON_OFF:
        MOV     SI,FCB+1
        LODSB
        OR      AL,20H
        CMP     AL,'o'
        JNZ	short BADONF
        LODSW
        OR      AX,2020H                ; Convert to lower case
        CMP     AL,'n'
        JNZ	short OFFCHK
        CMP     AH,' '                  ; ' ' ORed with 20H is still ' '
        JNZ	short BADONF
        RETN                             ; Carry clear from CMP
OFFCHK:
        CMP     AX,6666H                ; 'ff'
        JNZ	short BADONF
        LODSB
        CMP     AL,' '
        JNZ	short BADONF
        INC     AL                      ; Reset zero Carry clear from CMP
        RETN
BADONF:
        ;MOV     DX,OFFSET TRANGROUP:BAD_ON_OFF
        MOV     DX,BAD_ON_OFF
        STC
        RETN

; Print volume ID info

;ASSUME  DS:TRANGROUP,ES:TRANGROUP

PRINTVOL:
        PUSH    AX		; AX return from SEARCH_FIRST for VOL ID
        ;MOV     DX,OFFSET TRANGROUP:VOLMES
        MOV     DX,VOLMES
        CALL    PRINT
        ;MOV     AL,DS:[FCB]
        MOV     AL,[FCB]
        ADD     AL,'@'
        CMP     AL,'@'
        JNZ	short DRVOK
        MOV     AL,[CURDRV]
        ADD     AL,'A'
DRVOK:
        CALL    OUT
        POP     AX
        OR      AL,AL
        JZ	short GOODVOL
        ;MOV     DX,OFFSET TRANGROUP:NOVOL
        MOV     DX,NOVOL
        CALL    PRINT
        JMP     CRLF2
GOODVOL:
        ;MOV     DX,OFFSET TRANGROUP:GOTVOL
        MOV     DX,GOTVOL
        CALL    PRINT
        ;MOV     SI,OFFSET TRANGROUP:DIRBUF+8
        MOV     SI,DIRBUF+8
        MOV     CX,11
        ;MOV     DI,OFFSET TRANGROUP:CHARBUF
        MOV     DI,CHARBUF
        MOV     DX,DI
        REP     MOVSB
        MOV     AX,0A0DH
        STOSW
        XOR     AX,AX
        STOSB
        JMP     ZPRINT

; print date

PRINT_DATE:
        PUSH    ES
        PUSH    DI
        PUSH    CS
        POP     ES
	;MOV     DI,OFFSET TRANGROUP:CHARBUF
	MOV     DI,CHARBUF
        MOV     AH,GET_DATE
        INT     int_command             ; Get date in CX:DX
        CBW
        CALL    GetDate                 ; get date and put into DI
        MOV     AL," "
        STOSB
	;MOV     SI,OFFSET TRANGROUP:CURDAT_MID
        MOV     SI,CURDAT_MID
        CALL    MESTRAN
        CALL    P_DATE
        XOR     AX,AX
        STOSB
        ;MOV     DX,OFFSET TRANGROUP:CHARBUF
        MOV     DX,CHARBUF
        CALL    ZPRINT
        POP     ES
        POP     DI
        RETN

GetDate:
        MOV     SI,AX
        SHL     SI,1
        ADD     SI,AX           ; SI=AX*3
        ;ADD     SI,OFFSET TRANGROUP:WEEKTAB
        ADD     SI,WEEKTAB
        MOV     BX,CX
        MOV     CX,3
        REP     MOVSB
        RETN

;TRANCODE	ENDS
;	END

;=============================================================================
; COPY.ASM
;=============================================================================

;TITLE   COMMAND COPY routines.

; COPY CODE

;TRANCODE	SEGMENT PUBLIC BYTE

;ASSUME  CS:TRANGROUP,DS:TRANGROUP,ES:TRANGROUP,SS:NOTHING

DOMELCOPY:
        cmp     byte [MELCOPY],0FFH
        jz	short CONTMEL
        mov     SI,[SRCPT]
        mov     [MELSTART],si
        mov     byte [MELCOPY],0FFH
CONTMEL:
        xor     BP,BP
        mov     si,[SRCPT]
        mov     bl,'+'
SCANSRC2:
        ;mov     di,OFFSET TRANGROUP:SCANBUF
	mov     di,SCANBUF
        call    CPARSE
        test    bh,80H
        jz	short NEXTMEL		; Go back to start
        test    bh,1                    ; Switch ?
        jnz	short SCANSRC2		; Yes
        call    SOURCEPROC
        call    RESTUDIR1
        ;mov     di,OFFSET TRANGROUP:DESTFCB2
	mov     di,DESTFCB2
        ;mov     ax,PARSE_FILE_DESCRIPTOR SHL 8
        mov     ax,PARSE_FILE_DESCRIPTOR*256
        INT     int_command
        ;mov     bx,OFFSET TRANGROUP:SDIRBUF + 1
        mov     bx,SDIRBUF+1
        ;mov     si,OFFSET TRANGROUP:DESTFCB2 + 1
        mov     si,DESTFCB2+1
        mov     di,[SRCTAIL]
        call    BUILDNAME
        jmp     MELDO


NEXTMEL:
        call    CLOSEDEST
        xor     ax,ax
        mov     [CFLAG],al
        mov     [NXTADD],ax
        mov     [DESTCLOSED],al
        mov     si,[MELSTART]
        mov     [SRCPT],si
        call    SEARCHNEXT
        jz	short SETNMELJ
        jmp     ENDCOPY2
SETNMELJ:
        jmp     SETNMEL

COPY:
; First order of buisness is to find out about the destination
;ASSUME  DS:TRANGROUP,ES:TRANGROUP
        xor     ax,ax
        mov     [ALLSWITCH],AX          ; no switches
        mov     [ARGC],al               ; no arguments
        mov     [PLUS],al               ; no concatination
        mov     [BINARY],al             ; Binary not specifically specified
        mov     [ASCII],al              ; ASCII not specifically specified
        mov     [FILECNT],ax            ; No files yet
        mov     [WRITTEN],al            ; Nothing written yet
        mov     [CONCAT],al             ; No concatination
        mov     [MELCOPY],al            ; Not a Mel Hallerman copy
        mov     [SCANBUF],ax   		; Init buffer
        mov     [DESTBUF],ax   		; Init buffer
        mov     [SRCBUF],ax    		; Init buffer
        mov     [SDIRBUF],ax   		; Init buffer
        mov     [DIRBUF],ax    		; Init buffer
        mov     [DESTFCB],ax   		; Init buffer
        dec     ax
        mov     [FRSTSRCH],al           ; First search call
        mov     [FIRSTDEST],al          ; First time
        mov     [DESTISDIR],al          ; Don't know about dest
        mov     si,81H
        mov     bl,'+'                  ; include '+' as a delimiter
DESTSCAN:
        xor     bp,bp                   ; no switches
	;mov     di,offset trangroup:SCANBUF
	mov     di,SCANBUF
        call    CPARSE
        PUSHF                           ; save flags
        test    bh,80H                  ; A '+' argument?
        jz	short NOPLUS		; no
        mov	byte [PLUS],1		; yes
NOPLUS:
        POPF                            ; get flags back
        jc	short CHECKDONE		; Hit CR?
        test    bh,1                    ; Switch?
        jz	short TESTP2		; no
        or      [DESTSWITCH],BP         ; Yes, assume destination
        or      [ALLSWITCH],BP          ; keep tabs on all switches
        jmp     short DESTSCAN

TESTP2:
        test    bh,80H                  ; Plus?
        jnz	short GOTPLUS		; Yes, not a separate arg
        inc	byte [ARGC]		; found a real arg

GOTPLUS:
        push    SI
        mov     ax,[STARTEL]
        ;mov     SI,offset trangroup:SCANBUF ; Adjust to copy
        mov     SI,SCANBUF
        sub     ax,SI
        ;mov     DI,offset trangroup:DESTBUF
        mov     DI,DESTBUF
        add     ax,DI
        mov     [DESTTAIL],AX
        mov     [DESTSIZ],cl            ; Save its size
        inc     cx                      ; Include the NUL
        rep     movsb                   ; Save potential destination
        mov     [DESTINFO],bh           ; Save info about it
        mov     word [DESTSWITCH],0	; reset switches
        pop     SI
        jmp     short DESTSCAN          ; keep going

CHECKDONE:
        mov     al,[PLUS]
        mov     [CONCAT],al             ; PLUS -> Concatination
        shl     al,1
        shl     al,1
        mov     [INEXACT],al            ; CONCAT -> inexact copy
        ;mov     dx,offset trangroup:BADARGS
	mov     dx,BADARGS
        mov     al,[ARGC]
        or      al,al                   ; Good number of args?
        jz	short CERROR4J		; no, not enough
        cmp     al,2
        jbe	short ACOUNTOK
CERROR4J:
        jmp	CERROR			; no, too many
ACOUNTOK:
	;mov     bp,offset trangroup:DESTVARS
	mov     bp,DESTVARS
        cmp     al,1
        jnz	short GOT2ARGS
        mov     al,[CURDRV]             ; Dest is default drive:*.*
        add     al,'A'
        mov     ah,':'
        ;mov	[bp.SIZ],2
        mov	byte [bp+VARSTRUC.SIZ],2
	;mov     di,offset trangroup:DESTBUF
	mov     di,DESTBUF
        stosw
        mov     word [DESTSWITCH],0	; no switches on dest
        ;mov     [bp.INFO],2		; Flag dest is ambig
        mov     byte [bp+VARSTRUC.INFO],2
        ;mov     [bp.ISDIR],0		; Know destination specs file
        mov     byte [bp+VARSTRUC.ISDIR],0
        call    SETSTARS
GOT2ARGS:
        cmp     byte [bp+VARSTRUC.SIZ],2
        jnz	short NOTSHORTDEST
        cmp     byte [DESTBUF+1],':'
        jnz     NOTSHORTDEST            ; Two char file name
        or      byte [bp+VARSTRUC.INFO],2 ; Know dest is d:
	;mov     di,offset trangroup:DESTBUF + 2
        mov     di,DESTBUF+2
        mov     byte [bp+VARSTRUC.ISDIR],0 ; Know destination specs file
        call    SETSTARS
NOTSHORTDEST:
        ;mov     di,[bp.TTAIL]
        mov     di,[bp+VARSTRUC.TTAIL]
        cmp     byte [DI],0
        jnz	short CHKSWTCHES
        ;mov     dx,offset trangroup:BADCD
        mov     dx,BADCD
        cmp     byte [DI-2],':'
        jnz	short CERROR4J		; Trailing '/' error
        mov     byte [bp+VARSTRUC.ISDIR],2 ; Know destination is d:/
        or      byte [bp+VARSTRUC.INFO],6
        call    SETSTARS
CHKSWTCHES:
        ;mov     dx,offset trangroup:BADSWT
        mov     dx,BADSWT
        mov     ax,[ALLSWITCH]
        cmp     ax,GOTSWITCH
        jz	short CERROR4J		; Switch specified which is not known

; Now know most of the information needed about the destination

        TEST    AX,VSWITCH              ; Verify requested?
        JZ	short NOVERIF                 ; No
        MOV     AH,GET_VERIFY_ON_WRITE
        INT     int_command             ; Get current setting
        PUSH    DS
        MOV     DS,[RESSEG]
;ASSUME  DS:RESGROUP
        XOR     AH,AH
        MOV     [VERVAL],AX             ; Save current setting
        POP     DS
;ASSUME  DS:TRANGROUP
	;MOV     AX,(SET_VERIFY_ON_WRITE SHL 8) OR 1 ; Set verify
        MOV     AX,(SET_VERIFY_ON_WRITE*256) | 1
        INT     int_command
NOVERIF:
        xor     bp,bp                   ; no switches
        mov     si,81H
        mov     bl,'+'                  ; include '+' as a delimiter
SCANFSRC:
	;mov     di,offset trangroup:SCANBUF
	mov     di,SCANBUF
        call    CPARSE                  ; Parse first source name
        test    bh,1                    ; Switch?
        jnz	short SCANFSRC		; Yes, try again
        or      [DESTSWITCH],bp         ; Include copy wide switches on DEST
        test    bp,BSWITCH
        jnz	short NOSETCASC		; Binary explicit
        cmp	byte [CONCAT],0
        JZ	short NOSETCASC		; Not Concat
        mov     byte [ASCII],ASWITCH	; Concat -> ASCII copy if no B switch
NOSETCASC:
        push    SI
        mov     ax,[STARTEL]
	;mov     SI,offset trangroup:SCANBUF ; Adjust to copy
	mov     SI,SCANBUF
        sub     ax,SI
        ;mov     DI,offset trangroup:SRCBUF
        mov     DI,SRCBUF
        add     ax,DI
        mov     [SRCTAIL],AX
        mov     [SRCSIZ],cl             ; Save its size
        inc     cx                      ; Include the NUL
        rep     movsb                   ; Save this source
        mov     [SRCINFO],bh            ; Save info about it
        pop     SI
        mov     ax,bp                   ; Switches so far
        call    SETASC                  ; Set A,B switches accordingly
        call    SWITCH                  ; Get any more switches on this arg
        call    SETASC                  ; Set
        call    FRSTSRC
        jmp     FIRSTENT

ENDCOPY:
        CALL    CLOSEDEST
ENDCOPY2:
        ;MOV     DX,OFFSET TRANGROUP:COPIED_PRE
        MOV     DX,COPIED_PRE
        CALL    PRINT
        MOV     SI,[FILECNT]
        XOR     DI,DI
        CALL    DISP32BITS
        ;MOV     DX,OFFSET TRANGROUP:COPIED_POST
	MOV     DX,COPIED_POST
        CALL    PRINT
        JMP     TCOMMAND                ; Stack could be messed up

SRCNONEXIST:
        cmp     byte [CONCAT],0
        jnz	short NEXTSRC		; If in concat mode, ignore error
        ;mov     dx,offset trangroup:SRCBUF
        mov     dx,SRCBUF
        call    ZPRINT
        CALL    ONESPC
        ;mov     dx,offset trangroup:NOTFND
        mov     dx,NOTFND
        jmp     COPERR

SOURCEPROC:
        push    SI
        mov     ax,[STARTEL]
	;mov     SI,offset trangroup:SCANBUF ; Adjust to copy
	mov     SI,SCANBUF
        sub     ax,SI
	;mov     DI,offset trangroup:SRCBUF
	mov     DI,SRCBUF
        add     ax,DI
        mov     [SRCTAIL],AX
        mov     [SRCSIZ],cl             ; Save its size
        inc     cx                      ; Include the NUL
        rep     movsb                   ; Save this sorce
        mov     [SRCINFO],bh            ; Save info about it
        pop     SI
        mov     ax,bp                   ; Switches so far
        call    SETASC                  ; Set A,B switches accordingly
        call    SWITCH                  ; Get any more switches on this arg
        call    SETASC                  ; Set
        cmp     byte [CONCAT],0
        jnz	short LEAVECFLAG	; Leave CFLAG if concatination
FRSTSRC:
        xor     ax,ax
        mov     [CFLAG],al              ; Flag destination not created
        mov     [NXTADD],ax             ; Zero out buffer
        mov     [DESTCLOSED],al         ; Not created -> not closed
LEAVECFLAG:
        mov     [SRCPT],SI              ; remember where we are
	;mov     di,offset trangroup:USERDIR1
	mov     di,USERDIR1
        ;mov     bp,offset trangroup:SRCVARS
        mov     bp,SRCVARS
        call    BUILDPATH               ; Figure out everything about the source
        mov     si,[SRCTAIL]            ; Create the search FCB
        RETN

NEXTSRC:
        cmp     byte [PLUS],0
        jnz	short MORECP
ENDCOPYJ2:
        jmp     ENDCOPY                 ; Done
MORECP:
        xor     bp,bp                   ; no switches
        mov     si,[SRCPT]
        mov     bl,'+'                  ; include '+' as a delimiter
SCANSRC:
        ;mov     di,offset trangroup:SCANBUF
        mov     di,SCANBUF
        call    CPARSE                  ; Parse first source name
        JC	short ENDCOPYJ2		; if error, then end (trailing + case)
        test    bh,80H
        jz	short ENDCOPYJ2		; If no '+' we're done
        test    bh,1                    ; Switch?
        jnz	short SCANSRC		; Yes, try again
        call    SOURCEPROC
FIRSTENT:
        mov     di,FCB
	;mov     ax,PARSE_FILE_DESCRIPTOR SHL 8
	mov     ax,PARSE_FILE_DESCRIPTOR*256
        INT     int_command
        mov     ax,[SRCBUF]		; Get drive
        cmp     ah,':'
        jz	short DRVSPEC1
        mov     al,'@'
DRVSPEC1:
        sub     al,'@'
        mov     [FCB],al
        mov     ah,DIR_SEARCH_FIRST
        call    SEARCH
        pushf                           ; Save result of search
        call    RESTUDIR1               ; Restore users dir
        popf
        jz	short NEXTAMBIG0
        jmp     SRCNONEXIST             ; Failed
NEXTAMBIG0:
        xor     al,al
        xchg    al,[FRSTSRCH]
        or      al,al
        jz	short NEXTAMBIG
SETNMEL:
        mov     cx,12
        ;mov     di,OFFSET TRANGROUP:SDIRBUF
        mov     di,SDIRBUF
        ;mov     si,OFFSET TRANGROUP:DIRBUF
        mov     si,DIRBUF
        rep     movsb                   ; Save very first source name
NEXTAMBIG:
        xor     al,al
        mov     [NOWRITE],al            ; Turn off NOWRITE
        mov     di,[SRCTAIL]
	;mov     si,offset trangroup:DIRBUF + 1
	mov     si,DIRBUF+1
        call    FCB_TO_ASCZ             ; SRCBUF has complete name
MELDO:
        cmp	byte [CONCAT],0
        jnz	short SHOWCPNAM		; Show name if concat
        test    byte [SRCINFO],2	; Show name if multi
        jz	short DOREAD
SHOWCPNAM:
        ;mov     dx,offset trangroup:SRCBUF
        mov     dx,SRCBUF
        call    ZPRINT
        call    CRLF2
DOREAD:
        call    DOCOPY
        cmp     byte [CONCAT],0
        jnz	short NODCLOSE		; If concat, do not close
        call    CLOSEDEST               ; else close current destination
        jc	short NODCLOSE		; Concat flag got set, close didn't really happen
        mov     byte [CFLAG],0		; Flag destination not created
NODCLOSE:
        cmp     byte [CONCAT],0		; Check CONCAT again
        jz	short NOFLUSH
        CALL    FLSHFIL                 ; Flush output between source files on CONCAT
                                        ;  so LOSTERR stuff works correctly
        TEST    byte [MELCOPY],0FFH
        jz	short NOFLUSH
        jmp     DOMELCOPY

NOFLUSH:
        call    SEARCHNEXT              ; Try next match
        jnz	short NEXTSRCJ		; Finished with this source spec
        mov	byte [DESTCLOSED],0	; Not created or concat -> not closed
        jmp     NEXTAMBIG               ; Do next ambig

NEXTSRCJ:
        jmp   NEXTSRC

BUILDPATH:
	;test    [BP.INFO],2
	test    byte [BP+VARSTRUC.INFO],2
        jnz	short NOTPFILE		; If ambig don't bother with open
        mov     dx,bp
        add     dx,VARSTRUC.BUF		; Set DX to spec
        ;mov     ax,OPEN SHL 8
        mov     ax,OPEN*256
        INT     int_command
        jc	short NOTPFILE
        mov     bx,ax                   ; Is pure file
        ;mov     ax,IOCTL SHL 8
	mov     ax,IOCTL*256
        INT     int_command
        mov     ah,CLOSE
        INT     int_command
        test    dl,devid_ISDEV
        jnz	short ISADEV		; If device, done
        ;test    [BP.INFO],4
        test    byte [BP+VARSTRUC.INFO],4
        jz	short ISSIMPFILE	; If no path seps, done
NOTPFILE:
        ;mov     dx,word ptr [BP.BUF]
        mov     dx,[BP+VARSTRUC.BUF]
        cmp     dh,':'
        jz	short DRVSPEC5
        mov     dl,'@'
DRVSPEC5:
        sub     dl,'@'                  ; A = 1
        call    SAVUDIR1
        mov     dx,bp
        add     dx,VARSTRUC.BUF		; Set DX for upcomming CHDIRs
        ;mov     bh,[BP.INFO]
        mov     bh,[BP+VARSTRUC.INFO]
        and     bh,6
        cmp     bh,6                    ; Ambig and path ?
        jnz	short CHECKAMB		; jmp if no
        ;mov     si,[BP.TTAIL]
        mov     si,[BP+VARSTRUC.TTAIL]
        cmp     byte [si-2],':'
        jnz	short KNOWNOTSPEC
	;mov     [BP.ISDIR],2		; Know is d:/file
	mov     byte [BP+VARSTRUC.ISDIR],2
        jmp     short DOPCDJ

KNOWNOTSPEC:
	;mov     [BP.ISDIR],1		; Know is path/file
	mov     byte [BP+VARSTRUC.ISDIR],1
        dec     si                      ; Point to the /
DOPCDJ:
        jmp     short DOPCD

CHECKAMB:
        cmp     bh,2
        jnz	short CHECKCD
ISSIMPFILE:
ISADEV:
	;mov     [BP.ISDIR],0		; Know is file since ambig but no path
        mov     byte [BP+VARSTRUC.ISDIR],0
        ;return
	RETN

CHECKCD:
        call    SETREST1
        mov     ah,CHDIR
        INT     int_command
        jc	short NOTPDIR
        mov     di,dx
        xor     ax,ax
        mov     cx,ax
        dec     cx
        repne   scasb
        dec     di
        mov     al,[DIRCHAR]
	mov     byte [bp+VARSTRUC.ISDIR],2 ; assume d:/file
        cmp     al,[di-1]
        jz	short GOTSRCSLSH
        stosb
        mov     byte [bp+VARSTRUC.ISDIR],1 ; know path/file
GOTSRCSLSH:
        or      byte [bp+VARSTRUC.INFO],6
        call    SETSTARS
NOTPDIR_retn:
        RETN

NOTPDIR:
        mov     byte [bp+VARSTRUC.ISDIR],0 ; assume pure file
        mov     bh,[bp+VARSTRUC.INFO]
        test    bh,4
	;retz				; Know pure file, no path seps
	jz	short NOTPDIR_retn 				
        mov     byte [bp+VARSTRUC.ISDIR],2 ; assume d:/file
        mov     si,[bp+VARSTRUC.TTAIL]
        cmp     byte [si],0
        jz	short BADCDERRJ2	; Trailing '/'
        cmp     byte [si],'.'
        jz	short BADCDERRJ2	; If . or .. pure cd should have worked
        cmp     byte [si-2],':'
        jz	short DOPCD		; Know d:/file
        mov	byte [bp+VARSTRUC.ISDIR],1 ; Know path/file
        dec     si                      ; Point at last '/'
DOPCD:
        xor     bl,bl
        xchg    bl,[SI]                 ; Stick in a NUL
        call    SETREST1
        mov     ah,CHDIR
        INT     int_command
        xchg    bl,[SI]
        ;retnc
	jnc	short NOTPDIR_retn
BADCDERRJ2:
        JMP     BADCDERR

SETSTARS:
        mov     [bp+VARSTRUC.TTAIL],DI
        add     byte [bp+VARSTRUC.SIZ],12
        ;mov     ax,('.' SHL 8) OR '?'
        mov     ax,2E3Fh ; '?.'
        mov     cx,8
        rep     stosb
        xchg    al,ah
        stosb
        xchg    al,ah
        mov     cl,3
        rep     stosb
        xor     al,al
        stosb
        retn

COMPNAME:
        PUSH    CX
        PUSH    AX
	;MOV     si,offset trangroup:SRCBUF
	MOV     si,SRCBUF
	;MOV     di,offset trangroup:DESTBUF
	MOV     di,DESTBUF
        MOV     CL,[CURDRV]
        MOV     CH,CL
        CMP     BYTE [SI+1],':'
        JNZ	short NOSRCDRV
        LODSW
        SUB     AL,'A'
        MOV     CL,AL
NOSRCDRV:
        CMP     BYTE [DI+1],':'
        JNZ	short NODSTDRV
        MOV     AL,[DI]
        INC     DI
        INC     DI
        SUB     AL,'A'
        MOV     CH,AL
NODSTDRV:
        CMP     CH,CL
        jnz	short RET81P
        call    STRCOMP
        jz	short RET81P
        mov     ax,[si-1]
        mov     cx,[di-1]
        push    ax
        and     al,cl
        pop     ax
        jnz	short RET81P		; Neither of the mismatch chars was a NUL
; Know one of the mismatch chars is a NUL
; Check for ".NUL" compared with NUL
        cmp     al,'.'
        jnz	short CHECKCL
        or      ah,ah
        jmp     short RET81P            ; If NUL return match, else no match
CHECKCL:
        cmp     cl,'.'
        jnz	short RET81P		; Mismatch
        or      ch,ch                   ; If NUL return match, else no match
RET81P:
        POP     AX
        POP     CX
        RETN

;TRANCODE	ENDS

;	END

;=============================================================================
; COPYPROC.ASM
;=============================================================================

;TITLE   COPYRPOC		;Procedures called by COPY

;TRANCODE	SEGMENT PUBLIC BYTE

;ASSUME  CS:TRANGROUP,DS:TRANGROUP,ES:TRANGROUP,SS:NOTHING

SEARCHNEXT:
        MOV     AH,DIR_SEARCH_NEXT
        TEST	byte [SRCINFO],2
        JNZ	short SEARCH		; Do search-next if ambig
        OR      AH,AH                   ; Reset zero flag
	RETN
SEARCH:
        PUSH    AX
        MOV     AH,SET_DMA
	;MOV     DX,OFFSET TRANGROUP:DIRBUF
	MOV     DX,DIRBUF
        INT     int_command             ; Put result of search in DIRBUF
        POP     AX                      ; Restore search first/next command
        MOV     DX,FCB
        INT     int_command             ; Do the search
        OR      AL,AL
DOCOPY_retn:
        ;return
	RETN

DOCOPY:
        mov     byte [RDEOF],0		; No EOF yet
	;mov     dx,offset trangroup:SRCBUF
	mov     dx,SRCBUF
        ;mov     ax,OPEN SHL 8
        mov     ax,OPEN*256
        INT     int_command
        ;retc				; If open fails, ignore
        jc	short DOCOPY_retn
	mov     bx,ax                   ; Save handle
        mov     [SRCHAND],bx            ; Save handle
        ;mov     ax,(FILE_TIMES SHL 8)
        mov     ax,FILE_TIMES*256
        INT     int_command
        mov     [CPDATE],dx             ; Save DATE
        mov     [CPTIME],cx             ; Save TIME
	;mov     ax,(IOCTL SHL 8)
	mov     ax,IOCTL*256
        INT     int_command             ; Get device stuff
        and     dl,devid_ISDEV
        mov     [SRCISDEV],dl           ; Set source info
        jz	short COPYLP		; Source not a device
        cmp	byte [BINARY],0
        jz	short COPYLP		; ASCII device OK
	;mov     dx,offset trangroup:INBDEV  ; Cannot do binary input
	mov     dx,INBDEV
        jmp     COPERR

COPYLP:
        mov     bx,[SRCHAND]
        mov     cx,[BYTCNT]
        mov     dx,[NXTADD]
        sub     cx,dx                   ; Compute available space
        jnz	short GOTROOM
        call    FLSHFIL
        CMP	byte [TERMREAD],0
        JNZ	short CLOSESRC		; Give up
        mov     cx,[BYTCNT]
GOTROOM:
        push    ds
        mov     ds,[TPA]
;ASSUME  DS:NOTHING
        mov     ah,READ
        INT     int_command
        pop     ds
;ASSUME  DS:TRANGROUP
        jc	short CLOSESRC		; Give up if error
        mov     cx,ax                   ; Get count
        jcxz    CLOSESRC                ; No more to read
        cmp	byte [SRCISDEV],0
        jnz	short NOTESTA		; Is a device, ASCII mode
        cmp     byte [ASCII],0
        jz	short BINREAD
NOTESTA:
        MOV     DX,CX
        MOV     DI,[NXTADD]
        MOV     AL,1AH
        PUSH    ES
        MOV     ES,[TPA]
        REPNE   SCASB                   ; Scan for EOF
        POP     ES
        JNZ	short USEALL
        INC	byte [RDEOF]
        INC     CX
USEALL:
        SUB     DX,CX
        MOV     CX,DX
BINREAD:
        ADD     CX,[NXTADD]
        MOV     [NXTADD],CX
        CMP     CX,[BYTCNT]             ; Is buffer full?
        JB	short TESTDEV		; If not, we may have found EOF
        CALL    FLSHFIL
        CMP	byte [TERMREAD],0
        JNZ	short CLOSESRC		; Give up
        JMP     SHORT COPYLP

TESTDEV:
        cmp	byte [SRCISDEV],0
        JZ	short CLOSESRC		; If file then EOF
        CMP	byte [RDEOF],0
        JZ	short COPYLP		; On device, go till ^Z
CLOSESRC:
        mov     bx,[SRCHAND]
        mov     ah,CLOSE
        INT     int_command
CLOSESRCDEST_retn:
        ;return
	RETN

CLOSEDEST:
        cmp	byte [DESTCLOSED],0
	;retnz				; Don't double close
	jnz	short CLOSESRCDEST_retn
        MOV     AL,[DESTSWITCH]
        CALL    SETASC                  ; Check for B or A switch on destination
        JZ	short BINCLOS
        MOV     BX,[NXTADD]
        CMP     BX,[BYTCNT]             ; Is memory full?
        JNZ	short PUTZ
        call    TRYFLUSH                ; Make room for one lousy byte
        jz	short NOCONC
CONCHNG:                                ; Concat flag changed on us
        STC
        RETN
NOCONC:
        XOR     BX,BX
PUTZ:
        PUSH    DS
        MOV     DS,[TPA]
        MOV     WORD [BX],1AH		; Add End-of-file mark (Ctrl-Z)
        POP     DS
        INC     word [NXTADD]
        MOV     byte [NOWRITE],0	; Make sure our ^Z gets written
        MOV     AL,[WRITTEN]
        XOR     AH,AH
        ADD     AX,[NXTADD]
        JC	short BINCLOS		; > 1
        CMP     AX,1
        JZ	short FORGETIT		; WRITTEN = 0 NXTADD = 1 (the ^Z)
BINCLOS:
        call    TRYFLUSH
        jnz	short CONCHNG
        cmp	byte [WRITTEN],0
        jz	short FORGETIT		; Never wrote nothin
        MOV     BX,[DESTHAND]
        MOV     CX,[CPTIME]
        MOV     DX,[CPDATE]
        CMP	byte [INEXACT],0	; Copy not exact?
        JZ	short DODCLOSE		; If no, copy date & time
        MOV     AH,GET_TIME
        INT     int_command
        SHL     CL,1
        SHL     CL,1                    ; Left justify min in CL
        SHL     CX,1
        SHL     CX,1
        SHL     CX,1                    ; hours to high 5 bits, min to 5-10
        SHR     DH,1                    ; Divide seconds by 2 (now 5 bits)
        OR      CL,DH                   ; And stick into low 5 bits of CX
        PUSH    CX                      ; Save packed time
        MOV     AH,GET_DATE
        INT     int_command
        SUB     CX,1980
        XCHG    CH,CL
        SHL     CX,1                    ; Year to high 7 bits
        SHL     DH,1                    ; Month to high 3 bits
        SHL     DH,1
        SHL     DH,1
        SHL     DH,1
        SHL     DH,1                    ; Most sig bit of month in carry
        ADC     CH,0                    ; Put that bit next to year
        OR      DL,DH                   ; Or low three of month into day
        MOV     DH,CH                   ; Get year and high bit of month
        POP     CX                      ; Get time back
DODCLOSE:
	;MOV     AX,(FILE_TIMES SHL 8) OR 1
	MOV     AX,(FILE_TIMES*256) | 1
        INT     int_command             ; Set date and time
        MOV     AH,CLOSE
        INT     int_command
        INC	word [FILECNT]
        INC	byte [DESTCLOSED]
RET50:
        CLC
        RETN

FORGETIT:
        MOV     BX,[DESTHAND]
        CALL    DODCLOSE                ; Close the dest
        ;MOV     DX,OFFSET TRANGROUP:DESTBUF
        MOV     DX,DESTBUF
        MOV     AH,UNLINK
        INT     int_command             ; And delete it
        MOV	word [FILECNT],0	; No files transferred
        JMP	short RET50

TRYFLUSH:
        mov     al,[CONCAT]
        push    ax
        call    FLSHFIL
        pop     ax
        cmp     al,[CONCAT]
        ;return
	retn

FLSHFIL:
; Write out any data remaining in memory.
; Inputs:
;       [NXTADD] = No. of bytes to write
;       [CFLAG] <>0 if file has been created
; Outputs:
;       [NXTADD] = 0

        MOV     byte [TERMREAD],0
        cmp     byte [CFLAG],0
        JZ	short NOTEXISTS
        JMP     EXISTS
NOTEXISTS:
        call    BUILDDEST               ; Find out all about the destination
        CALL    COMPNAME                ; Source and dest. the same?
        JNZ	short PROCDEST		; If not, go ahead
        CMP	byte [SRCISDEV],0
        JNZ	short PROCDEST		; Same name on device OK
        CMP	byte [CONCAT],0		; Concatenation?
        ;MOV     DX,OFFSET TRANGROUP:OVERWR
        MOV     DX,OVERWR
        JZ	short COPERRJ		; If not, overwrite error
        MOV	byte [NOWRITE],1	; Flag not writting (just seeking)
PROCDEST:
	;mov     ax,(OPEN SHL 8) OR 1
	mov     ax,(OPEN*256) | 1
        CMP     byte [NOWRITE],0
        JNZ	short DODESTOPEN	; Don't actually create if NOWRITE set
        mov     ah,CREAT
        xor     cx,cx
DODESTOPEN:
	;mov     dx,offset trangroup:DESTBUF
	mov     dx,DESTBUF
        INT     int_command
        ;MOV     DX,OFFSET TRANGROUP:FULDIR
        MOV     DX,FULDIR
        JC	short COPERRJ
        mov     [DESTHAND],ax           ; Save handle
        mov	byte [CFLAG],1		; Destination now exists
        mov     bx,ax
	;mov     ax,(IOCTL SHL 8)
	mov     ax,IOCTL*256
        INT     int_command             ; Get device stuff
        mov     [DESTISDEV],dl          ; Set dest info
        test    dl,devid_ISDEV
        jz	short EXISTS		; Dest not a device
        mov     al,[DESTSWITCH]
        AND     AL,ASWITCH+BSWITCH
        JNZ	short TESTBOTH
        MOV     AL,[ASCII]              ; Neither set, use current setting
        OR      AL,[BINARY]
        JZ	short EXSETA		; Neither set, default to ASCII
TESTBOTH:
        JPE	short EXISTS		; Both are set, ignore
        test    AL,BSWITCH
        jz	short EXISTS		; Leave in cooked mode
        ;mov     ax,(IOCTL SHL 8) OR 1
        mov     ax,(IOCTL*256) | 1
        xor     dh,dh
        or      dl,devid_RAW
        mov     byte [DESTISDEV],dl	; New value
        INT     int_command             ; Set device to RAW mode
        jmp     short EXISTS

COPERRJ:
        jmp     SHORT COPERR

EXSETA:
; What we read in may have been in binary mode, flag zapped write OK
        mov     byte [ASCII],ASWITCH	; Set ASCII mode
        or	byte [INEXACT],ASWITCH	; ASCII -> INEXACT
EXISTS:
        cmp	byte [NOWRITE],0
        jnz	short NOCHECKING	; If nowrite don't bother with name check
        CALL    COMPNAME                ; Source and dest. the same?
        JNZ	short NOCHECKING	; If not, go ahead
        CMP	byte [SRCISDEV],0
        JNZ	short NOCHECKING	; Same name on device OK
; At this point we know in append (would have gotten overwrite error on first
; destination create otherwise), and user trying to specify destination which
; has been scribbled already (if dest had been named first, NOWRITE would
; be set).
	;MOV	 DX,OFFSET TRANGROUP:LOSTERR ; Tell him he's not going to get it
	MOV	 DX,LOSTERR
        CALL    PRINT
        MOV     word [NXTADD],0		; Set return
        INC	byte [TERMREAD]		; Tell Read to give up
RET60:
        RETN

NOCHECKING:
        mov     bx,[DESTHAND]           ; Get handle
        XOR     CX,CX
        XCHG    CX,[NXTADD]
        JCXZ    RET60                   ; If Nothing to write, forget it
        INC	byte [WRITTEN]		; Flag that we wrote something
        CMP	byte [NOWRITE],0	; If NOWRITE set, just seek CX bytes
        JNZ	short SEEKEND
        XOR     DX,DX
        PUSH    DS
        MOV     DS,[TPA]
;ASSUME  DS:NOTHING
        MOV     AH,WRITE
        INT     int_command
        POP     DS
;ASSUME  DS:TRANGROUP
        ;MOV     DX,OFFSET TRANGROUP:NOSPACE
        MOV     DX,NOSPACE
        JC	short COPERR		; Failure
        sub     cx,ax
        ;retz				; Wrote all supposed to
	jz	short RET60
        test    byte [DESTISDEV],devid_ISDEV
        jz	short COPERR		; Is a file, error
        test    byte [DESTISDEV],devid_RAW
        jnz	short DEVWRTERR		; Is a raw device, error
        cmp	byte [INEXACT],0
        ;retnz				; INEXACT so OK
	jnz	short SEEKEND_retn
	dec     cx
        ;retz				; Wrote one byte less (the ^Z)
	jz	short SEEKEND_retn
DEVWRTERR:
	;MOV     DX,OFFSET TRANGROUP:DEVWMES
	MOV     DX,DEVWMES
COPERR:
        CALL    PRINT
        inc	byte [DESTCLOSED]
        cmp	byte [CFLAG],0
        jz	short ENDCOPYJ		; Never actually got it open
        MOV     bx,[DESTHAND]
        MOV     AH,CLOSE                ; Close the file
        INT     int_command
        ;MOV     DX,OFFSET TRANGROUP:DESTBUF
        MOV     DX,DESTBUF
        MOV     AH,UNLINK
        INT     int_command             ; And delete it
        MOV     byte [CFLAG],0
ENDCOPYJ:
        JMP	ENDCOPY

SEEKEND:
        xor     dx,dx                   ; Zero high half of offset
        xchg    dx,cx                   ; cx:dx is seek location
	;mov     ax,(LSEEK SHL 8) OR 1
	mov     ax,(LSEEK*256) | 1
        INT     int_command             ; Seek ahead in the file
        cmp	byte [RDEOF],0
        ;retz
	jz	short SEEKEND_retn
; If a ^Z has been read we must set the file size to the current
; file pointer location
        MOV     AH,WRITE
        INT     int_command             ; CX is zero, truncates file
SEEKEND_retn:
        ;return
	RETN

SETASC:
; Given switch vector in AX,
;       Set ASCII switch if A is set
;       Clear ASCII switch if B is set
;       BINARY set if B specified
;       Leave ASCII unchanged if neither or both are set
; Also sets INEXACT if ASCII is ever set. AL = ASCII on exit, flags set
        AND     AL,ASWITCH+BSWITCH
        JPE	short LOADSW		; PE means both or neither are set
        PUSH    AX
        AND     AL,BSWITCH
        MOV     [BINARY],AL
        POP     AX
        AND     AL,ASWITCH
        MOV     [ASCII],AL
        OR      [INEXACT],AL
LOADSW:
        MOV     AL,[ASCII]
        OR      AL,AL
        RETN

BUILDDEST:
        cmp     byte [DESTISDIR],-1
        jnz	short KNOWABOUTDEST	; Already done the figuring
	;MOV     DI,OFFSET TRANGROUP:USERDIR1
        MOV     DI,USERDIR1
        ;mov     bp,offset trangroup:DESTVARS
        mov     bp,DESTVARS
        call    BUILDPATH
        call    RESTUDIR1

; Now know all about the destination

KNOWABOUTDEST:
        xor     al,al
        xchg    al,[FIRSTDEST]
        or      al,al
        jnz	short FIRSTDST
        jmp     NOTFIRSTDEST
FIRSTDST:
        mov     si,[DESTTAIL]           ; Create an FCB of the original DEST
	;mov     di,offset trangroup:DESTFCB
	mov     di,DESTFCB
	;mov     ax,PARSE_FILE_DESCRIPTOR SHL 8
	mov     ax,PARSE_FILE_DESCRIPTOR*256
        INT     int_command
        mov     ax,[DESTBUF]		; Get drive
        cmp     ah,':'
        jz	short DRVSPEC4
        mov     al,'@'
DRVSPEC4:
        MOV     CL,[ASCII]              ; Save current ASCII setting
        sub     al,'@'
        mov     [DESTFCB],al
        mov     al,[DESTINFO]
        mov     ah,[SRCINFO]
        and     ax,0202H
        or      al,al
        jz	short NOTMELCOPY
        cmp     al,ah
        jnz	short NOTMELCOPY
        cmp     byte [PLUS],0
        jz	short NOTMELCOPY
        inc     byte [MELCOPY]		; ambig source, ambig dest, and pluses
        xor     al,al
        jmp     short SETCONC

NOTMELCOPY:
        xor     al,2                    ; al=2 if unambig dest, =0 if ambig dest
        and     al,ah
        shr     al,1                    ; al=1 if unambig dest AND ambig sorce
                                        ;   Implies concatination
SETCONC:
        or      al,[PLUS]               ; al=1 if concat
        mov     [CONCAT],al
        shl     al,1
        shl     al,1
        mov     [INEXACT],al            ; Concat -> inexact copy
        cmp	byte [BINARY],0
        jnz	short NOTFIRSTDEST	; Binary explicitly given, all OK
        mov     [ASCII],al              ; Concat -> ASCII
        or      cl,cl
        jnz	short NOTFIRSTDEST	; ASCII flag set before, DATA read correctly
        or      al,al
        JZ	short NOTFIRSTDEST	; ASCII flag did not change states
; At this point there may already be binary read data in the read buffer.
; We need to find the first ^Z (if there is one) and trim the amount
; of data in the buffer correctly.
        MOV     CX,[NXTADD]
        JCXZ    NOTFIRSTDEST            ; No data, everything OK
        MOV     AL,1AH
        PUSH    ES
        XOR     DI,DI
        MOV     ES,[TPA]
        REPNE   SCASB                   ; Scan for EOF
        POP     ES
        JNZ	short NOTFIRSTDEST	; No ^Z in buffer, everything OK
        DEC     DI                      ; Point at ^Z
        MOV     [NXTADD],DI             ; New buffer
NOTFIRSTDEST:
	;mov     bx,offset trangroup:DIRBUF+1 ; Source of replacement chars
	mov     bx,DIRBUF+1		; Source of replacement chars
        cmp	byte [CONCAT],0
        jz	short GOTCHRSRC		; Not a concat
	;mov     bx,offset trangroup:SDIRBUF+1 ; Source of replacement chars
	mov     bx,SDIRBUF+1
GOTCHRSRC:
	;mov     si,offset trangroup:DESTFCB+1 ; Original dest name
	mov     si,DESTFCB+1
        mov     di,[DESTTAIL]           ; Where to put result

BUILDNAME:
        mov     cx,8
BUILDMAIN:
        lodsb
        cmp     al,"?"
        jnz	short NOTAMBIG
        mov     al,[BX]
NOTAMBIG:
        cmp     al,' '
        jz	short NOSTORE
        stosb
NOSTORE:
        inc     bx
        loop    BUILDMAIN
        mov     cl,3
        cmp     byte [SI],' '
        jz	short ENDDEST		; No extension
        mov     al,'.'
        stosb
BUILDEXT:
        lodsb
        cmp     al,"?"
        jnz	short NOTAMBIGE
        mov     al,[BX]
NOTAMBIGE:
        cmp     al,' '
        jz	short NOSTOREE
        stosb
NOSTOREE:
        inc     bx
        loop    BUILDEXT
ENDDEST:
        xor     al,al
        stosb                           ; NUL terminate
        retn

;TRANCODE	ENDS
;	END

;=============================================================================
; CPARSE.ASM
;=============================================================================

;TITLE   CPARSE

;TRANCODE        SEGMENT PUBLIC BYTE

;ASSUME  CS:TRANGROUP,DS:TRANGROUP,ES:TRANGROUP

;SWCOUNT EQU 5

	;PUBLIC  CPARSE

CPARSE:

;-----------------------------------------------------------------------;
; ENTRY:                                                                ;
;       DS:SI   Points input buffer                                     ;
;       ES:DI   Points to the token buffer                              ;
;       BL      Special delimiter for this call                         ;
;                   Always checked last                                 ;
;                   set it to space if there is no special delimiter    ;
; EXIT:                                                                 ;
;       DS:SI   Points to next char in the input buffer                 ;
;       ES:DI   Points to the token buffer                              ;
;       [STARTEL] Points to start of last element of path in token      ;
;               points to a NUL for no element strings 'd:' 'd:/'       ;
;       CX      Character count                                         ;
;       BH      Condition Code                                          ;
;                       Bit 1H of BH set if switch character            ;
;                               Token buffer contains char after        ;
;                               switch character                        ;
;                               BP has switch bits set (ORing only)     ;
;                       Bit 2H of BH set if ? or * in token             ;
;                               if * found element ? filled             ;
;                       Bit 4H of BH set if path sep in token           ;
;                       Bit 80H of BH set if the special delimiter      ;
;                          was skipped at the start of this token       ;
;               Token buffer always starts d: for non switch tokens     ;
;       CARRY SET                                                       ;
;           if CR on input                                              ;
;               token buffer not altered                                ;
;                                                                       ;
;       DOES NOT RETURN ON BAD PATH ERROR                               ;
; MODIFIES:                                                             ;
;       CX, SI, AX, BH, DX and the Carry Flag                           ;       ;
;                                                                       ;
; -----------------------------------------------------------------------;

        xor     ax,ax
        mov     [STARTEL],DI            ; No path element (Is DI correct?)
        mov     [ELPOS],al              ; Start in 8 char prefix
        mov     [SKPDEL],al             ; No skip delimiter yet
        mov     bh,al                   ; Init nothing
        pushf                           ; save flags
        push    di                      ; save the token buffer addrss
        xor     cx,cx                   ; no chars in token buffer
moredelim:
        LODSB
        CALL    DELIM
        JNZ	short SCANCDONE
        CMP     AL,' '
        JZ	short moredelim
        CMP     AL,9
        JZ	short moredelim
        xchg    al,[SKPDEL]
        or      al,al
        jz	short moredelim		; One non space/tab delimiter allowed
        JMP     x_done                  ; Nul argument

SCANCDONE:
        ;IF      NOT KANJI
        call    UPCONV
        ;ENDIF

        cmp     al,bl                   ; Special delimiter?
        jnz	short nospec
        or      bh,80H
        jmp     short moredelim

nospec:
        cmp     al,0DH                  ; a CR?
        jne	short ncperror
        jmp     cperror
ncperror:
        cmp     al,[SWITCHAR]           ; is the char the switch char?
        jne	short na_switch		; yes, process...
        jmp     a_switch
na_switch:
        cmp     byte [si],':'
        jne	short anum_chard	; Drive not specified

        ;IF      KANJI
        ;call    UPCONV
        ;ENDIF

        call    move_char
        lodsb                           ; Get the ':'
        call    move_char
        mov     [STARTEL],di
        mov     byte [ELCNT],0
        jmp     anum_test

anum_chard:
        mov     [STARTEL],di
        mov     byte [ELCNT],0		; Store of this char sets it to one
        call    PATHCHRCMP              ; Starts with a pathchar?
        jnz	short anum_char		; no
        push    ax
        mov     al,[CURDRV]             ; Insert drive spec
        add     al,'A'
        call    move_char
        mov     al,':'
        call    move_char
        pop     ax
        mov     [STARTEL],di
        mov     byte [ELCNT],0

anum_char:

        ;IF      KANJI
        ;call    TESTKANJ
        ;jz      TESTDOT
        ;call    move_char
        ;lodsb
        ;jmp     short notspecial
;TESTDOT:
	;ENDIF

        cmp     al,'.'
        jnz	short testquest
        inc     byte [ELPOS]		; flag in extension
        mov     byte [ELCNT],0FFH	; Store of the '.' resets it to 0
testquest:
        cmp     al,'?'
        jnz	short testsplat
        or      bh,2
testsplat:
        cmp     al,'*'
        jnz	short testpath
        or      bh,2
        mov     ah,7
        cmp     byte [ELPOS],0
        jz	short gotelcnt
        mov     ah,2
gotelcnt:
        mov     al,'?'
        sub     ah,[ELCNT]
        jc	short badperr2
        xchg    ah,cl
        jcxz    testpathx
qmove:
        xchg    ah,cl
        call    move_char
        xchg    ah,cl
        loop    qmove
testpathx:
        xchg    ah,cl
testpath:
        call    PATHCHRCMP
        jnz	short notspecial
        or      bh,4
        test    bh,2                    ; If just hit a '/', cannot have ? or * yet
        jnz	short badperr
        mov     [STARTEL],di            ; New element
        INC     word [STARTEL]		; Point to char after /
        mov     byte [ELCNT],0FFH	; Store of '/' sets it to 0
        mov     byte [ELPOS],0
notspecial:
        call    move_char               ; just an alphanum string
anum_test:
        lodsb

        ;IF      NOT KANJI
        call    UPCONV
        ;ENDIF

        call    DELIM
        je	short x_done
        cmp     al,0DH
        je	short x_done
        cmp     al,[SWITCHAR]
        je	short x_done
        cmp     al,bl
        je	short x_done
        cmp     al,':'                  ; ':' allowed as trailer because
                                        ; of devices
        ;IF      KANJI
        ;je      FOO15
        ;jmp     anum_char
;FOO15:
        ;ELSE
        jne     anum_char
        ;ENDIF

	mov     byte [si-1],' '		; Change the trailing ':' to a space
        jmp     short x_done

badperr2:
        ;mov     dx,offset trangroup:BADCPMES
	mov     dx,BADCPMES
        jmp     CERROR

badperr:
        jmp     BADCDERR

cperror:
        dec     si                      ; adjust the pointer
        pop     di                      ; retrive token buffer address
        popf                            ; restore flags
        stc                             ; set the carry bit
        retn

x_done:
        dec     si                      ; adjust for next round
        jmp     short out_token

a_switch:
        OR      BH,1                    ; Indicate switch
        OR      BP,GOTSWITCH
        CALL    SCANOFF
        INC     SI
        cmp     al,0DH
        je	short cperror
        call    move_char               ; store the character
        CALL    UPCONV
        PUSH    ES
        PUSH    DI
        PUSH    CX
        PUSH    CS
        POP     ES
;ASSUME  ES:TRANGROUP
        ;MOV     DI,OFFSET TRANGROUP:SWLIST
        MOV     DI,SWLIST
        MOV     CX,SWCOUNT
        REPNE   SCASB
        JNZ	short out_tokenp
        MOV     AX,1
        SHL     AX,CL
        OR      BP,AX
out_tokenp:
        POP     CX
        POP     DI
        POP     ES
;ASSUME  ES:NOTHING
out_token:
        mov     al,0
        stosb                           ; null at the end
        pop     di                      ; restore token buffer pointer
        popf
        clc                             ; clear carry flag
        retn

move_char:
        stosb                           ; store char in token buffer
        inc     cx                      ; increment char count
        inc     byte [ELCNT]		; increment element count for * substi
        retn

;TRANCODE ENDS
;	END

; ----------------------------------------------------------------------------
; SEGMENT - TRANDATA
; ----------------------------------------------------------------------------
ALIGN 16
; ----------------------------------------------------------------------------
;Data for transient portion
; ----------------------------------------------------------------------------

TRANDATA:

;=============================================================================
; TDATA.ASM
;=============================================================================

;TITLE   COMMAND Transient Initialized DATA

; Data for transient portion

;TRANDATA SEGMENT PUBLIC BYTE

        ;ORG     0
;ZERO    =       $
BADBAT  DB      13,10,"Batch file missing",13,10,"$"	 ; offset 2580h 
NEEDBAT DB      13,10,"Insert disk with batch file"	 ; offset 2597h
        DB      13,10,"and press any key when ready",13,10,"$"
BADNAM  DB      "Bad command or file name",13,10,"$"
RENERR  DB      "Duplicate file name or "
NOTFND  DB      "File not found",13,10,"$"
NOSPACE DB      "Insufficient disk space",13,10,"$"
ENVERR  DB      "Out of environment space",13,10,"$"
FULDIR  DB      "File creation error",13,10,"$"
OVERWR  DB      "File cannot be copied onto itself",13,10,"$"
LOSTERR DB      "Content of destination lost before copy",13,10,"$"

;"COPIED_PRE<# files copied>COPIED_POST"
COPIED_POST  DB      " File(s) copied",13,10
COPIED_PRE   DB      "$"

;"DIRMES_PRE<# files in dir>DIRMES_POST"
DIRMES_POST  DB	" File(s) "
DIRMES_PRE   DB	"$"

;"BYTMES_PRE<# free bytes>BYTMES_POST"
BYTMES_POST  DB	" bytes free",13,10
BYTMES_PRE   DB	"$"

BADDRV  DB	"Invalid drive specification",13,10,"$"
PAUSEMES DB	"Strike a key when ready . . . $"
BADSWT  DB	"Invalid parameter",13,10,"$"
WEEKTAB DB	"SunMonTueWedThuFriSat"
BADDAT  DB	13,10,"Invalid date$"

;"CURDAT_PRE<day of week>CURDAT_MID<MO,DAY,YR>CURDAT_POST"
;Note: CURDAT_MID also appears in the date printed via PROMPT command
CURDAT_PRE DB	"Current date is "
;CURDAT_MID LABEL BYTE
CURDAT_MID:
CURDAT_POST DB	"$"

NEWDAT  DB	13,10,"Enter new date: $"
BADTIM  DB	13,10,"Invalid time$"

;"CURTIM_PRE<HR,MIN,SEC,HSEC>CURTIM_POST"
CURTIM_PRE  DB	"Current time is "
CURTIM_POST DB	"$"

NEWTIM  DB      13,10,"Enter new time: $"
SUREMES DB      "Are you sure (Y/N)? $"
DMES    DB      " <DIR>   $"

;"VERMES_PRE<version #>VERMES_POST"
	;IF	IBMVER
;VERMES_PRE DB	"TeleVideo Personal Computer DOS Version "
        ;ENDIF
        ;IF	MSVER
VERMES_PRE  DB	"MS-DOS Version "
        ;ENDIF
VERMES_POST DB	"$"

VOLMES  DB	" Volume in drive $"
GOTVOL  DB      " is $"
NOVOL   DB      " has no label$"

BADCD   DB      "Invalid directory",13,10,"$"
BADMKD  DB      "Unable to create directory",13,10,"$"
BADRMD  DB      "Invalid path, not directory,",13,10
        DB      "or directory not empty",13,10,"$"
BAD_ON_OFF DB   "Must specify ON or OFF"        ;Note Run over to next message

;"DIRHEAD_PRE<path of dir>DIRHEAD_POST"
DIRHEAD_POST DB	13,10,"$"
DIRHEAD_PRE  DB	" Directory of  $"
NULPATH DB      "No Path $"
PATH_TEXT    DB "PATH="
PROMPT_TEXT  DB "PROMPT="
BADPMES DB      "Invalid drive in search path",13,10,"$"
BADDEV  DB      "Invalid device",13,10,"$"
BADLAB  DB      "Label not found",13,10,"$"
SYNTMES DB      "Syntax error",13,10,"$"
FORNESTMES DB   13,"FOR cannot be nested",13,10,"$"	; Offset 28F2h
PIPEEMES DB     "Intermediate file error during pipe",13,10,"$"
INBDEV  DB      "Cannot do binary reads from a device",13,10,"$"
OFFMES  DB      "off",13,10,"$"
ONMES   DB      "on",13,10,"$"
CTRLCMES DB     "BREAK is $"
VERIMES DB      "VERIFY is $"
ECHOMES DB      "ECHO is $"
BADCPMES DB     "Invalid path or file name",13,10,"$"
BADARGS DB      "Invalid number of parameters",13,10,"$"
DEVWMES DB      "Error writing to device"
ACRLF   DB      13,10,"$"
DBACK   DB      8," ",8,0               ; DESTRUCTIVE BACK SPACE

CLSSTRING DB	4,01BH,"[2J"            ; ANSI Clear screen

;PROMPT_TABLE LABEL BYTE
PROMPT_TABLE:
        DB      "D"
        DW      PRINT_DATE	; OFFSET TRANGROUP:PRINT_DATE
        DB      "T"
        DW      PRINT_TIME	; OFFSET TRANGROUP:PRINT_TIME
        DB      "P"
        DW      PRINT_DEFAULT_DIRECTORY ; OFFSET TRANGROUP:PRINT_DEFAULT_DIRECTORY
        DB      "N"
        DW      PRINT_DRIVE	; OFFSET TRANGROUP:PRINT_DRIVE
        DB      "V"
        DW      PRINT_VERSION	; OFFSET TRANGROUP:PRINT_VERSION
        DB      "G"
        DW      PRINT_G		; OFFSET TRANGROUP:PRINT_G
        DB      "L"
        DW      PRINT_L		; OFFSET TRANGROUP:PRINT_L
        DB      "B"
        DW      PRINT_B		; OFFSET TRANGROUP:PRINT_B
        DB      "_"
        DW      CRLF2		; OFFSET TRANGROUP:CRLF2
        DB      "$"
        DW      OUT		; OFFSET TRANGROUP:OUT
        DB      "E"
        DW      PRINT_ESC	; OFFSET TRANGROUP:PRINT_ESC
        DB      "H"
        DW      PRINT_BACK	; OFFSET TRANGROUP:PRINT_BACK
        DB      "Q"
        DW      PRINT_EQ	; OFFSET TRANGROUP:PRINT_EQ
        DB      0                       ; NUL TERMINATED

;IFTAB   LABEL   BYTE			; Table of IF conditionals
IFTAB:
        DB      3,"NOT"                 ; First byte is count
	DW      IFNOT		; OFFSET TRANGROUP:IFNOT
        DB      10,"ERRORLEVEL"
        DW      IFERLEV		; OFFSET TRANGROUP:IFERLEV
        DB      5,"EXIST"
        DW      IFEXISTS	; OFFSET TRANGROUP:IFEXISTS
        DB      0

COMTAB:					; Table for internal command names	
	DB      4,"DIR",1
        DW      CATALOG		; OFFSET TRANGROUP:CATALOG
        DB      7,"RENAME",1
        DW      CRENAME		; OFFSET TRANGROUP:CRENAME
        DB      4,"REN",1
        DW	CRENAME		; OFFSET TRANGROUP:CRENAME
        DB      6,"ERASE",1
        DW      ERASE		; OFFSET TRANGROUP:ERASE
        DB      4,"DEL",1
        DW      ERASE		; OFFSET TRANGROUP:ERASE
        DB      5,"TYPE",1
        DW      TYPEFIL		; OFFSET TRANGROUP:TYPEFIL
        DB      4,"REM",0
        DW      TCOMMAND	; OFFSET TRANGROUP:TCOMMAND
        DB      5,"COPY",1
        DW      COPY		; OFFSET TRANGROUP:COPY
        DB      6,"PAUSE",0
        DW      PAUSE		; OFFSET TRANGROUP:PAUSE
        DB      5,"DATE",0
        DW      DATE		; OFFSET TRANGROUP:DATE
        DB      5,"TIME",0
        DW	CTIME 		; OFFSET TRANGROUP:CTIME
        DB      4,"VER",0
        DW      VERSION 	; OFFSET TRANGROUP:VERSION
        DB      4,"VOL",1
        DW      VOLUME 		; OFFSET TRANGROUP:VOLUME
        DB      3,"CD",1
        DW      _$CHDIR		; OFFSET TRANGROUP:$CHDIR
        DB      6,"CHDIR",1
        DW      _$CHDIR		; OFFSET TRANGROUP:$CHDIR
        DB      3,"MD",1
        DW      _$MKDIR		; OFFSET TRANGROUP:$MKDIR
        DB      6,"MKDIR",1
        DW	_$MKDIR		; OFFSET TRANGROUP:$MKDIR
        DB      3,"RD",1
        DW      _$RMDIR		; OFFSET TRANGROUP:$RMDIR
        DB      6,"RMDIR",1
        DW      _$RMDIR		; OFFSET TRANGROUP:$RMDIR
        DB      6,"BREAK",0
        DW      CNTRLC		; OFFSET TRANGROUP:CNTRLC
        DB      7,"VERIFY",0
        DW      VERIFY		; OFFSET TRANGROUP:VERIFY
        DB      4,"SET",0
        DW      ADD_NAME_TO_ENVIRONMENT	; OFFSET TRANGROUP:ADD_NAME_TO_ENVIRONMENT
        DB      7,"PROMPT",0
        DW      ADD_PROMPT	; OFFSET TRANGROUP:ADD_PROMPT
        DB      5,"PATH",0
        DW      PATH		; OFFSET TRANGROUP:PATH
	DB      5,"EXIT",0
        DW      _$EXIT		; OFFSET TRANGROUP:$EXIT
        DB      5,"CTTY",1
        DW      CTTY		; OFFSET TRANGROUP:CTTY
        DB      5,"ECHO",0
        DW      ECHO		; OFFSET TRANGROUP:ECHO
        DB      5,"GOTO",0
        DW	GOTO		; OFFSET TRANGROUP:GOTO
        DB      6,"SHIFT",0
        DW      SHIFT		; OFFSET TRANGROUP:SHIFT
        DB      3,"IF",0
        DW      _$IF		; OFFSET TRANGROUP:$IF
        DB      4,"FOR",0
        DW	_$FOR		; OFFSET TRANGROUP:$FOR
        DB      4,"CLS",0
        DW      CLS		; OFFSET TRANGROUP:CLS
        DB      0               ; Terminate command table

;TRANDATAEND	LABEL   BYTE

	DB	0FFh ; 30/04/2018

TRANDATAEND:

;TRANDATA	ENDS
;	END

;COMLEN	EQU	$-$$

;BSS_START EQU	COMLEN+100H

BSS_START EQU	$-$$

; ----------------------------------------------------------------------------
; SEGMENT - TRANDATA
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;Uninitialized transient data
; ----------------------------------------------------------------------------

TRANSPACEPARAGS EQU (BSS_START+15)/16	

TRANSPACE	EQU (16*TRANSPACEPARAGS)+100h ; 01/05/2018

ABSOLUTE TRANSPACE

;=============================================================================
; TSPC.ASM
;=============================================================================

;TITLE   COMMAND Transient Uninitialized DATA

; Uninitialized transient data
;TRANSPACE SEGMENT PUBLIC BYTE

	;ORG     0
;ZERO    =       $

UCOMBUF	RESB	COMBUFLEN+3	; Raw console buffer	; offset 2B20h
COMBUF	RESB	COMBUFLEN+3	; Cooked console buffer	; offset 2BA3h
USERDIR1 RESB	DIRSTRLEN+3	; Storage for users current directory
EXECPATH RESB	DIRSTRLEN+15	; Path for external command

; Variables passed up from resident
;HEADCALL LABEL  DWORD
HEADCALL:		; offset 2CD8h
        RESW	1	
RESSEG	RESW	1	; offset 2CDAh
TPA	RESW	1	; offset 2CDCh	
SWITCHAR RESB	1
DIRCHAR	RESB 	1
EXEC_ADDR RESD	1
RCH_ADDR RESD	1	; offset 2CE4h

CHKDRV	RESB	1
;RDEOF	LABEL   BYTE		; Misc flags
RDEOF:
;IFNOTFLAG LABEL BYTE
IFNOTFLAG:
FILTYP	RESB	1
CURDRV	RESB	1	; offset 2CEAh		
;CONCAT	LABEL   BYTE
CONCAT:
PARM1	RESB	1
;ARGC	LABEL   BYTE
ARGC:	
PARM2	RESB	1
COMSW	RESW	1		; Switches between command and 1st arg
ARG1S	RESW	1		; Switches between 1st and 2nd arg
;DESTSWITCH LABEL WORD
DESTSWITCH:
ARG2S	RESW	1		; Switches after 2nd arg
;ALLSWITCH LABEL WORD
ALLSWITCH:
ARGTS	RESW	1		; ALL switches except for COMSW
CFLAG	RESB	1
;DESTCLOSED LABEL BYTE
DESTCLOSED:
SPECDRV	RESB	1
BYTCNT	RESW	1 ; off 2CF7h	; Size of buffer between RES and TRANS
NXTADD	RESW	1
FRSTSRCH RESB	1
LINCNT	RESB	1
LINLEN	RESB	1
FILECNT	RESW	1
CHARBUF	RESB	80 ; off 2D00h	; line byte character buffer for xenix write
;DESTFCB2 LABEL  BYTE
DESTFCB2:		; offset 2D50h		
IDLEN	RESB	1
ID	RESB	8 	; offset 2D51h	
COM	RESB	3
DEST	RESB	37
DESTNAME RESB	11
;DESTFCB LABEL   BYTE
DESTFCB:
DESTDIR	RESB	DIRSTRLEN	; Directory for PATH searches
;GOTOLEN LABEL   WORD
GOTOLEN:		; offset 2DCCh
;PWDBUF  LABEL   BYTE
PWDBUF:
;EXEFCB  LABEL   WORD
EXEFCB:
DIRBUF	RESB	DIRSTRLEN+3	; offset 2DCCh 
SDIRBUF	RESB	12
_BITS	RESW	1
PATHCNT	RESW	1
PATHPOS	RESW	1
PATHSW	RESW	1
FULLSCR	RESW	1

;IF  IBM
;ROM_CALL RESB	1		; flag for rom function
;ROM_IP	RESW	1
;ROM_CS	RESW	1
;ENDIF

;DESTVARS LABEL  BYTE
DESTVARS:
DESTISDIR RESB 	1
DESTSIZ	RESB	1
DESTTAIL RESW	1
DESTINFO RESB	1
DESTBUF	RESB	DIRSTRLEN+20

DESTHAND RESW	1	
DESTISDEV RESB	1
FIRSTDEST RESB	1
MELCOPY RESB	1	; offset 2E82h
MELSTART RESW	1	; offset 2E83h

;SRCVARS  LABEL  BYTE
SRCVARS:
SRCISDIR RESB	1
SRCSIZ	RESB	1
SRCTAIL RESW	1
SRCINFO	RESB	1
SRCBUF	RESB	DIRSTRLEN+20

SRCHAND	RESW	1
SRCISDEV RESB	1

SCANBUF	RESB	DIRSTRLEN+20 ; offset 2EE1h

SRCPT   RESW	1	; offset 2F35h
INEXACT RESB	1
APPEND  RESB	1
NOWRITE RESB	1
BINARY  RESB	1
WRITTEN RESB	1
TERMREAD RESB	1
ASCII   RESB	1
PLUS    RESB	1
CPDATE  RESW	1
CPTIME  RESW	1
BATHAND RESW	1		; Batch handle
STARTEL RESW	1	; offset 2F45h
ELCNT   RESB	1	; offset 2F47h
ELPOS   RESB	1	; offset 2F48h
SKPDEL  RESB	1
SOURCE  RESB	11

	resb 1 ; 30/04/2018

        ;IF     KANJI
;KPARSE DB      ?	
        ;ENDIF

;INTERNATVARS    internat_block <>
;		DB      (internat_block_max - ($ - INTERNATVARS)) DUP (?)

INTERNATVARS	RESB	INTERNAT_BLOCK.size

		RESB	(internat_block_max - ($ - INTERNATVARS))

        ;DB      80H DUP(0) ; Init to 0 to make sure the linker is not fooled
	RESB	128
;STACK   LABEL   WORD
alignb 2
STACK:		; offset 2FF6h

;TRANSPACEEND	LABEL   BYTE
TRANSPACEND:

;TRANSPACE ENDS
;	END

;ABSOLUTE STACK

TRANSPACESIZE 	EQU	$-BSS_START

;Length of transient in paragraphs
TRNLEN  EQU     (BSS_START+TRANSPACESIZE+15)/16