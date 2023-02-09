; ****************************************************************************
; COMMAND.COM (MSDOS 2.0 Command Interpreter) - RETRO DOS v2.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 05/05/2018
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
; (IBMDOS 2.10 COMMAND.COM) output in TASM syntax (21/04/2018..22/04/2018)
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
;               Some code for new 2.0 DOS, sort of HACKey.  Not enough time to
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

; ----------------------------------------------------------------------------
; START OF RESIDENT PORTION
; ----------------------------------------------------------------------------
; SEGMENT - CODERES
; ----------------------------------------------------------------------------

	;[ORG	0]

;ZERO	EQU	$

	[ORG 100H]

PROGSTART:
	JMP	CONPROC

        TIMES	128-($-PROGSTART) DB 0

RSTACK	EQU	$ ; EQU 180H

; offset 180h

;IF IBMVER
;SYSCALL:
;        CMP     AH,EXEC
;        JZ      short do_exec
;        JMP     FAR [CS:SYS_CALL]
;
;do_exec:
;        PUSH    ES
;        PUSH    DS
;        PUSH    BP
;        PUSH    DI
;        PUSH    SI
;        PUSH    DX
;        PUSH    CX
;        PUSH    BX
;        PUSH    AX
;        MOV     [CS:user_ss],SS
;        MOV     [CS:user_sp],SP
;;
;; are we running on RSTACK already?
;;
;        PUSH    CS
;        POP     BX              ; BX <- CS
;        PUSH    SS
;        POP     AX              ; AX <- SS
;        CMP     AX,BX           ; IF AX == BX then no stack switch!
;        JZ      short Get_mem
;        MOV     SS,BX
;;ASSUME  SS:RESGROUP
;	;MOV	SP,OFFSET RESGROUP:RSTACK
;	MOV	SP,RSTACK
;
;Get_mem:
;        MOV     BX,0FFFFH       ; allocate all of memory
;        MOV     AH,ALLOC
;        INT     int_command
;        ;MOV     AX,OFFSET EGROUP:ZEXECDATAEND + 15
;        MOV     AX,ZEXECDATAEND + 15
;	MOV     CL,4
;        SHR     AX,CL
;        MOV     CX,AX		; Save in CX
;        CMP     BX,AX		; enough for EXEC?
;        JB      short EXECMER	; nope... cry
;        MOV     AH,ALLOC
;        INT     int_command
;        JC	short EXECMER	; Memory arenas probably trashed
;        ADD     BX,AX
;        MOV     [SS:MEMSIZ],BX
;        SUB     BX,CX
;        MOV     [SS:EXESEG],BX	; exec
;        MOV     ES,AX
;        MOV     AH,DEALLOC
;        INT     int_command
;        PUSH    CS
;        POP     DS
;;ASSUME  DS:RESGROUP
;        CALL    EXECHK
;        CMP     DX,[EXESUM]
;        JZ      short HAVEXEC	; EXEC OK
;        ;MOV     DX,OFFSET RESGROUP:COMSPEC
;        ;MOV     AX,OPEN SHL 8
;        MOV     DX,COMSPEC
;        MOV     AX,OPEN*256
;        INT     int_command	; Open COMMAND.COM
;        JC      short EXECMER
;        MOV     BX,AX           ; Handle
;        ;MOV     DX,OFFSET RESGROUP:TRANSTART
;        MOV     DX,TRANSTART
;	;ADD     DX,OFFSET TRANGROUP:EXECSTART - 100H
;	ADD     DX,EXECSTART - 100H
;        XOR     CX,CX           ; Seek loc
;        ;MOV     AX,LSEEK SHL 8
;        MOV     AX,LSEEK*256
;        INT     int_command
;        ;MOV     CX,OFFSET EGROUP:ZEXECCODEEND
;        MOV     CX,EGROUP+ZEXECCODEEND
;        MOV     DS,[EXESEG]
;;ASSUME  DS:NOTHING
;        MOV     AH,READ
;        INT     int_command
;        PUSH    AX
;        MOV     AH,CLOSE
;        INT     int_command	; Close COMMAND.COM
;        POP     CX
;	;CMP     CX,OFFSET EGROUP:ZEXECCODEEND
;	 CMP     CX,EGROUP+ZEXECCODEEND
;        JNZ     short EXECMER	; Size matched
;
;        CALL    EXECHK
;        CMP     DX,[SS:EXESUM]
;        JNZ     short EXECMER
;HAVEXEC:
;        MOV     byte [SS:LOADING],0 	; Flag to DSKERR
;        CALL    FAR [SS:ZEXEC]
;        JMP     SHORT EXECRET
;execmer:
;        LDS	SI,[user_Sp]
;        MOV	word [SI+user_AX],exec_not_enough_memory
;        PUSH	word [SI+user_F]
;        POPF
;        STC
;        PUSHF
;        POP     word [SI+user_F]
;execret:
;        MOV     SS,[SS:user_SS]
;;ASSUME  SS:NOTHING
;        MOV     SP,[CS:user_SP]
;        POP     AX              ; PUSH    ES
;        POP     BX              ; PUSH    DS
;        POP     CX              ; PUSH    BP
;        POP     DX              ; PUSH    DI
;        POP     SI              ; PUSH    SI
;        POP     DI              ; PUSH    DX
;        POP     BP              ; PUSH    CX
;        POP     DS              ; PUSH    BX
;        POP     ES              ; PUSH    AX
;        IRET
;
;EXECHK:
;;ASSUME  DS:NOTHING,ES:NOTHING,SS:NOTHING
;        PUSH    DS
;        MOV     DS,[CS:EXESEG]
;        ;MOV     CX,OFFSET EGROUP:ZEXECCODEEND
;        MOV     CX,EGROUP+ZEXECCODEEND
;	 XOR     SI,SI
;        JMP     CHECK_SUM
;ENDIF

EXEC_ERR:                       ; Select the correct error message
        ;MOV     DX,OFFSET RESGROUP:RBADNAM
        MOV     DX,RBADNAM
        CMP     AX,exec_file_not_found
        JZ      short GOTEXECEMES
        CMP     AX,error_access_denied
        JZ      short GOTEXECEMES
        ;MOV     DX,OFFSET RESGROUP:TOOBIG
        MOV     DX,TOOBIG
        CMP     AX,exec_not_enough_memory
        JZ      short GOTEXECEMES
        ;MOV     DX,OFFSET RESGROUP:EXEBAD
        MOV     DX,EXEBAD
        CMP     AX,exec_bad_format
        JZ      short GOTEXECEMES
        ;MOV     DX,OFFSET RESGROUP:EXECEMES
        MOV     DX,EXECEMES
GOTEXECEMES:
        PUSH    CS
        POP     DS
        CALL    RPRINT
        JMP     SHORT NOEXEC

EXT_EXEC:
;
; we are now running in free space.  anything we do from here
; on may get trashed.  Move the stack (also in free space) to
; allocated space because since EXEC restores the stack,
; somebody may trash what is on the stack.
;
        MOV     CX,CS
        MOV     SS,CX
        ;MOV     SP,OFFSET RESGROUP:RSTACK
        MOV     SP,RSTACK
;
; Oops!! We have to make sure that the EXEC code doesn't blop a newstack!
;
;
        INT     int_command     ; Do the EXEC
        JC      short EXEC_ERR	; EXEC failed
EXEC_WAIT:
        MOV     AH,_WAIT
        INT     int_command     ; Get the return code
        MOV     [CS:RETCODE],AX
NOEXEC:
        JMP     LODCOM

CONTC:
        STI
        MOV     AX,CS
        MOV     DS,AX
;ASSUME  DS:RESGROUP
        MOV     AH,DISK_RESET
        INT     int_command     ; Reset disks in case files were open
        TEST    word [BATCH],-1
        JZ	short CONTCTERM
        JMP     ASKEND          ; See if user wants to terminate batch
CONTCTERM:
        XOR     BP,BP           ; Indicate no read
        MOV     byte [FORFLAG],0     ; Turn off for processing
        MOV     byte [PIPEFLAG],0    ; Turn off any pipe
        CMP     word [SINGLECOM],0   ; See if we need to set SINGLECOM
        JZ      short NOSETSING
        MOV     word [SINGLECOM],-1  ; Cause termination on pipe, batch, for
NOSETSING:
        CMP     byte [EXTCOM],0
        JNZ     short DODAB	; Internal ^C
        JMP     LODCOM1
DODAB:
        STC			; Tell DOS to abort
	RETF			; Leave flags on stack

BADMEMERR:			; Allocation error loading transient
        ;MOV     DX,OFFSET RESGROUP:BMEMMES
	MOV     DX,BMEMMES
FATALC:
        PUSH    CS
        POP     DS
        CALL    RPRINT
        CMP     byte [PERMCOM],0
        JZ      short FATALRET
        CMP     word [SINGLECOM],0 ; If PERMCOM and SINGLECOM
        JNZ     short FATALRET	; Must take INT_2E exit
        ;MOV     DX,OFFSET RESGROUP:HALTMES
        MOV     DX,HALTMES
	CALL    RPRINT
STALL:
        JMP     STALL		; Crash the system nicely

FATALRET:
	;MOV     DX,OFFSET RESGROUP:FRETMES
	MOV     DX,FRETMES
        CALL    RPRINT
FATALRET2:
        CMP     byte [PERMCOM],0 ; If we get here and PERMCOM,
        JNZ	short RET_2E	; must be INT_2E
;IF IBM
;        LDS     DX,[SYS_CALL]
;ASSUME  DS:NOTHING
;       ;MOV     AX,(SET_INTERRUPT_VECTOR SHL 8) + INT_COMMAND
;	MOV	AX,(SET_INTERRUPT_VECTOR*256)+INT_COMMAND
;       INT     int_command
;ENDIF
	; 30/04/2018
        ;MOV     AX,[CS:PARENT]
        MOV	AX,[PARENT]
	;;MOV    [CS:PDB_Parent_PID],AX
	;MOV    [CS:PDB.PARENT_PID],AX
	MOV	[PDB.PARENT_PID],AX
        MOV     AX,EXIT*256	; Return to lower level
        INT     int_command

RET_2E:
        PUSH    CS
        POP     DS
;ASSUME  DS:RESGROUP,ES:NOTHING,SS:NOTHING
        MOV     word [SINGLECOM],0 ; Turn off singlecom
        MOV     ES,[LTPA]
        MOV     AH,DEALLOC
        INT     int_command 	; Free up space used by transient
        MOV     BX,[SAVE_PDB]
        MOV     AH,SET_CURRENT_PDB
        INT     int_command	; Current process is user
        MOV     AX,[RETCODE]
        CMP     byte [EXTCOM],0
        JNZ     short GOTECODE
        XOR     AX,AX           ; Internals always return 0
GOTECODE:
        MOV     byte [EXTCOM],1	; Force external
        JMP     FAR [INT_2E_RET] ;"IRET"

INT_2E:                         ; Magic command executer
;ASSUME  DS:NOTHING,ES:NOTHING,SS:NOTHING
        POP     WORD [CS:INT_2E_RET]
        POP     WORD [CS:INT_2E_RET+2] ;Get return address
        POP     AX              ;Chuck flags
        PUSH    CS
        POP     ES
        MOV     DI,80H
        MOV     CX,64
        REP     MOVSW
        MOV     AH,GET_CURRENT_PDB
        INT     int_command	; Get user's header
        MOV     [CS:SAVE_PDB],BX
        MOV     AH,SET_CURRENT_PDB
        MOV     BX,CS
        INT     int_command	; Current process is me
        MOV     word [CS:SINGLECOM],81H
        MOV     byte [CS:EXTCOM],1 ; Make sure this case forced

LODCOM:                         ; Termination handler
        CMP     byte [CS:EXTCOM],0
        JZ      short LODCOM1	; If internal, memory already allocated

        MOV     BX,0FFFFH
        MOV     AH,ALLOC
        INT     int_command
        ;MOV     AX,OFFSET TRANGROUP:TRANSPACEEND + 15
        MOV	AX,TRANSPACEEND + 15
	MOV     CL,4
        SHR     AX,CL

        ;IF	IBM
        ;PUSH    AX
        ;;MOV     AX,OFFSET EGROUP:ZEXECDATAEND + 15
        ;MOV     AX,EGROUP+ZEXECDATAEND + 15
        ;MOV     CL,4
        ;SHR     AX,CL
        ;POP     CX
        ;ADD     AX,CX
        ;ENDIF

        ADD     AX,20H
        CMP     BX,AX           ; Is less than 512 byte buffer worth it?
        JNC	short MEMOK
BADMEMERRJ:
        JMP	BADMEMERR	; Not enough memory
MEMOK:
        MOV     AH,ALLOC
        INT     int_command
        JC	short BADMEMERRJ ; Memory arenas probably trashed
        MOV     byte [CS:EXTCOM],0 ; Flag not to ALLOC again
        MOV     [CS:LTPA],AX	 ; New TPA is base just allocated
        ADD     BX,AX
        MOV     [CS:MEMSIZ],BX

        ;MOV     AX,OFFSET TRANGROUP:TRANSPACEEND + 15
        MOV     AX,TRANSPACEEND + 15
        MOV     CL,4
        SHR     AX,CL

        ;IF      IBM
        ;PUSH    AX
        ;;MOV     AX,OFFSET EGROUP:ZEXECDATAEND + 15
        ;MOV     AX,EGROUP+ZEXECDATAEND + 15
        ;MOV     CL,4
        ;SHR     AX,CL
        ;POP     CX
        ;ADD     AX,CX
        ;ENDIF

        SUB     BX,AX
        MOV     [CS:TRNSEG],BX	; Transient starts here
LODCOM1:
        MOV     AX,CS
        MOV     SS,AX
;ASSUME  SS:RESGROUP
        ;MOV     SP,OFFSET RESGROUP:RSTACK
        MOV     SP,RSTACK
	MOV     DS,AX
;ASSUME  DS:RESGROUP
        CALL    HEADFIX		; Make sure files closed stdin and stdout restored
        XOR     BP,BP           ; Flag command ok
        MOV     AX,-1
        XCHG    AX,[VERVAL]
        CMP     AX,-1
        JZ      short NOSETVER
        MOV     AH,SET_VERIFY_ON_WRITE  ; AL has correct value
        INT     int_command
NOSETVER:
        CMP     word [SINGLECOM],-1
        JNZ     short NOSNG
        JMP     FATALRET2       ; We have finished the single command
NOSNG:
        CALL    SETVECT

;IF IBMVER
;       CALL    EXECHK          ; Check exe loader
;       CMP     DX,[EXESUM]
;       JNZ	short BOGUS_COM
;ENDIF

        CALL    CHKSUM          ; Check the transient
        CMP     DX,[SUM]
        JZ      short HAVCOM	; Transient OK
BOGUS_COM:
        MOV     byte [LOADING],1 ; Flag DSKERR routine
        CALL    LOADCOM
CHKSAME:

;IF IBMVER
;       CALL    EXECHK
;       CMP     DX,[EXESUM]
;       JNZ     short ALSO_BOGUS
;ENDIF

        CALL    CHKSUM
        CMP     DX,[SUM]
        JZ      short HAVCOM	; Same COMMAND
ALSO_BOGUS:
        CALL    WRONGCOM
        JMP     SHORT CHKSAME
HAVCOM:
        ;MOV     AX,CHAR_OPER SHL 8
        MOV	AX,CHAR_OPER*256
	INT     int_command
        MOV     byte [RSWITCHAR],DL
        CMP     DL,'/'
        JNZ     short USESLASH
        MOV     byte [RDIRCHAR],'\'	; Select alt path separator
USESLASH:
        MOV     byte [LOADING],0	; Flag to DSKERR
        ;MOV     SI,OFFSET RESGROUP:TRANVARS
        MOV     SI,TRANVARS
        ;MOV     DI,OFFSET TRANGROUP:HEADCALL
        MOV     DI,HEADCALL
        MOV     ES,[TRNSEG]
        CLD
        MOV     CX,8
        REP     MOVSW                   ; Transfer INFO to transient
        MOV     AX,[MEMSIZ]
        ;MOV     WORD PTR DS:[PDB_block_len],AX  ; Adjust my own header
        MOV     [PDB.BLOCK_LEN],AX
        ;JMP     DWORD PTR [TRANS]

        JMP     FAR [TRANS]

; Far call to REMCHECK for TRANSIENT
TREMCHECK:
        CALL    REMCHECK
        RETF

REMCHECK:
;All registers preserved. Returns zero if media removable, NZ if fixed
; AL is drive (0=DEF, 1=A,...)
        ;IF      IBM
        ;PUSH    AX
        ;OR      AL,AL
        ;JNZ     short GOTDRV2
        ;MOV     AH,GET_DEFAULT_DRIVE
        ;INT     int_command
        ;INC     AL              ;A=1
;GOTDRV2:
        ;PUSH    BX
        ;MOV     BL,AL
        ;INT     11H             ;IBM EQUIP CALL
        ;ROL     AL,1
        ;ROL     AL,1
        ;AND     AL,3
        ;JNZ     short NOT_SINGLE
        ;INC     AL
;NOT_SINGLE:
        ;INC     AL              ; AL is now MAX floppy #
        ;CMP     BL,AL
        ;POP     BX
        ;JBE     short SETREM	; Is an IBM floppy and so is removable
        ;OR      AL,AL           ; Know AL is non-zero
        ;JMP     SHORT SETNREM
;SETREM:
        ;ELSE
        PUSH    AX
        ;ENDIF

        XOR     AX,AX           ;Zero

        ;IF      IBM
;SETNREM:
        ;ENDIF

        POP     AX
        RETN

; Far call to HEADFIX for TRANSIENT
THEADFIX:
        CALL    HEADFIX
	RETF

HEADFIX:
        XOR     BX,BX           ; Clean up header
        MOV     CX,[IO_SAVE]
	;MOV     DX,WORD PTR DS:[PDB_JFN_Table]
        MOV     DX,[PDB.JFN_TABLE]
        CMP     CL,DL
        JZ      short CHK1	; Stdin matches
        MOV     AH,CLOSE
        INT     int_command
        ;MOV     DS:[PDB_JFN_Table],CL ; Restore stdin
        MOV	[PDB.JFN_TABLE],CL ; Restore stdin
CHK1:
        INC     BX
        CMP     CH,DH           ; Stdout matches
        JZ      short CHKOTHERHAND
        MOV     AH,CLOSE
        INT     int_command
        ;MOV     DS:[PDB_JFN_Table+1],CH ; Restore stdout
        MOV	[PDB.JFN_TABLE+1],CH
CHKOTHERHAND:
        ADD     BX,4                    ; Skip 2,3,4
        MOV     CX,FILPERPROC - 5       ; Already done 0,1,2,3,4
CLOSELOOP:
        MOV     AH,CLOSE
        INT     int_command
        INC     BX
        LOOP    CLOSELOOP
        RETN

SAVHAND:
;ASSUME  DS:NOTHING,ES:NOTHING,SS:NOTHING
        PUSH    DS
        PUSH    BX              ; Set stdin to sterr, stdout to stderr
        PUSH    AX
        MOV     AH,GET_CURRENT_PDB
        INT     int_command	; Get user's header
        MOV     DS,BX
        ;MOV     AX,WORD PTR DS:[PDB_JFN_Table]
        MOV	AX, [PDB.JFN_TABLE]
	MOV     [CS:HANDLE01],AX	; Save user's stdin, stdout
        ;MOV     AL,DS:[PDB_JFN_Table+2]
        MOV     AL,[PDB.JFN_TABLE+2]
	MOV     AH,AL
        ;MOV     WORD PTR DS:[PDB_JFN_Table],AX   ; Dup stderr
        MOV	[PDB.JFN_TABLE],AX
        POP     AX
        POP     BX
        POP     DS
        RETN

;ASSUME  DS:RESGROUP

GETCOMDSK2:
        CALL    GETCOMDSK
        JMP     LODCOM1         ; Memory already allocated

RESTHAND:
        PUSH    DS
        PUSH    BX              ; Restore stdin, stdout to user
        PUSH    AX
        MOV     AH,GET_CURRENT_PDB
        INT     int_command             ; Point to user's header
        MOV     AX,[HANDLE01]
        MOV     DS,BX
;ASSUME DS:NOTHING
        ;MOV     WORD PTR DS:[PDB_JFN_Table],AX ; Stuff his old 0 and 1
        MOV	[PDB.JFN_TABLE],AX
        POP     AX
        POP     BX
        POP     DS
        RETN

;ASSUME DS:RESGROUP,SS:RESGROUP

HOPELESS:
        ;MOV     DX,OFFSET RESGROUP:NOCOM
	MOV     DX,NOCOM
        JMP     FATALC

GETCOMDSK:
        ;MOV     DX,OFFSET RESGROUP:NEEDCOM
	MOV     DX,NEEDCOM
GETCOMDSK3:
        MOV     AL,[COMDRV]
        CALL    REMCHECK
        JNZ     short HOPELESS	; Non-removable media
        CALL    RPRINT
        ;MOV     DX,OFFSET RESGROUP:DRVMSG
        MOV     DX,DRVMSG
        CMP     byte [COMDRV],0
        JNZ     short GETCOM1
        ;MOV     DX,OFFSET RESGROUP:DEFMSG
        MOV     DX,DEFMSG
GETCOM1:
        CALL    RPRINT
	;MOV     DX,OFFSET RESGROUP:PROMPT
	MOV     DX,PROMPT
        CALL    RPRINT
        ;CALL    GetRawFlushedByte
        ;RETN

; flush world and get raw input
GetRawFlushedByte:
        ;MOV     AX,(STD_CON_INPUT_FLUSH SHL 8) OR RAW_CON_INPUT
        MOV     AX,(STD_CON_INPUT_FLUSH*256) | RAW_CON_INPUT
        INT     int_command             ; Get char without testing or echo
        ;MOV     AX,(STD_CON_INPUT_FLUSH SHL 8) + 0
        MOV     AX,STD_CON_INPUT_FLUSH*256
        INT     int_command
        ;return
	RETN

LOADCOM: 
				; Load in transient
        INC     BP              ; Flag command read
        ;MOV     DX,OFFSET RESGROUP:COMSPEC
	MOV     DX,COMSPEC
        ;MOV     AX,OPEN SHL 8
        MOV     AX,OPEN*256
        INT     int_command             ; Open COMMAND.COM
        JNC     short READCOM
        CMP     AX,open_too_many_open_files
        JNZ     short TRYDOOPEN
        ;MOV     DX,OFFSET RESGROUP:NOHANDMES
        MOV     DX,NOHANDMES
        JMP     FATALC          ; Fatal, will never find a handle

TRYDOOPEN:
        CALL    GETCOMDSK
        JMP     SHORT LOADCOM

READCOM:
	MOV     BX,AX           ; Handle
       ;MOV     DX,OFFSET RESGROUP:TRANSTART
       	MOV     DX,TRANSTART ; COMTRANS
        XOR     CX,CX           ; Seek loc
        ;MOV     AX,LSEEK SHL 8
        MOV     AX,LSEEK*256
        INT     int_command
        JC      short WRONGCOM1
        ;MOV     CX,OFFSET TRANGROUP:TRANSPACEEND - 100H
	;MOV     CX,TRANSPACEEND - 100H
	MOV	CX,COMLEN ; 30/04/2018

        ;IF	IBM
        ;ADD     CX,15
        ;AND     CX,0FFF0H
        ;;ADD     CX,OFFSET EGROUP:ZEXECCODEEND
        ;ADD     CX,EGROUP+ZEXECCODEEND
        ;ENDIF

        PUSH    DS
        MOV     DS,[TRNSEG]
;ASSUME  DS:NOTHING
        MOV     DX,100H
        MOV     AH,READ
        INT     int_command
        POP     DS
;ASSUME  DS:RESGROUP
WRONGCOM1:
        PUSHF
        PUSH    AX
        MOV     AH,CLOSE
        INT     int_command             ; Close COMMAND.COM
        POP     AX
        POPF
        JC      short WRONGCOM	; If error on READ
        CMP     AX,CX
        JZ      short RET10	; Size matched
WRONGCOM:
        ;MOV     DX,OFFSET RESGROUP:COMBAD
	MOV     DX,COMBAD
        CALL    GETCOMDSK3
        JMP     SHORT LOADCOM   ; Try again

CHKSUM:                         ; Compute transient checksum
        PUSH    DS
        MOV     DS,[TRNSEG]
        MOV     SI,100H
        ;MOV     CX,OFFSET TRANGROUP:TRANDATAEND - 100H
        MOV     CX,TRANDATAEND - 100H

CHECK_SUM:
        CLD
        SHR     CX,1
        XOR     DX,DX
CHK:
        LODSW
        ADD     DX,AX
        LOOP    CHK
        POP     DS
RET10:
	RETN

SETVECT:                        ; Set useful vectors
        ;MOV     DX,OFFSET RESGROUP:LODCOM
        MOV     DX,LODCOM
        ;MOV     AX,(SET_INTERRUPT_VECTOR SHL 8) OR 22H
        MOV     AX,(SET_INTERRUPT_VECTOR*256) | 22H ; Set Terminate address
        INT     int_command
        ;MOV     DX,OFFSET RESGROUP:CONTC
        MOV     DX,CONTC
        MOV     AX,(SET_INTERRUPT_VECTOR*256) | 23H ; Set Ctrl-C address
        INT     int_command
        ;MOV     DX,OFFSET RESGROUP:DSKERR
        MOV     DX,DSKERR
        MOV     AX,(SET_INTERRUPT_VECTOR*256) | 24H ; Set Hard Disk Error address
        INT     int_command
        RETN

;CODERES ENDS

; This TAIL segment is used to produce a PARA aligned label in the resident
; group which is the location where the transient segments will be loaded
; initial.

;TAIL    SEGMENT PUBLIC PARA
;        ORG     0
;TRANSTART       LABEL   WORD
;TAIL    ENDS

; This TAIL segment is used to produce a PARA aligned label in the transient
; group which is the location where the exec segments will be loaded
; initial.

;TRANTAIL    SEGMENT PUBLIC PARA
;        ORG     0
;EXECSTART   LABEL   WORD
;TRANTAIL    ENDS
;
;IF IBMVER
;        INCLUDE EXEC.ASM
;ENDIF
;
;        END     PROGSTART

;=============================================================================
; RUCODE.ASM
;=============================================================================

;TITLE   COMMAND Language modifiable Code Resident

;        PUBLIC  ASKEND,DSKERR,RPRINT

;ASSUME  CS:RESGROUP,DS:NOTHING,ES:NOTHING,SS:NOTHING

; TERMINATE BATCH JOB PROMPTER

;ASSUME  DS:RESGROUP
ASKEND:
        CALL    SAVHAND
ASKEND2:
        ;MOV     DX,OFFSET RESGROUP:ENDBATMES
        MOV     DX,ENDBATMES
        CALL    RPRINT
	;MOV     AX,(STD_CON_INPUT_FLUSH SHL 8)+STD_CON_INPUT
	MOV     AX,(STD_CON_INPUT_FLUSH*256)+STD_CON_INPUT
        INT     int_command
        AND     AL,5FH
        CMP     AL,"N"
        JZ      short RESTHJ
        CMP     AL,"Y"
        JNZ     ASKEND2
        MOV     ES,[BATCH]
        MOV     AH,DEALLOC
        INT     int_command
        MOV     word [BATCH],0		; Flag no batch AFTER DEALLOC in case
                                        ;   of ^C
        MOV     byte [ECHOFLAG],1	; Make sure ECHO turned back on
RESTHJ:
        CALL    RESTHAND
        JMP     CONTCTERM

DSKERR:
;ASSUME  DS:NOTHING,ES:NOTHING,SS:NOTHING
        ; ******************************************************
        ;       THIS IS THE DEFAULT DISK ERROR HANDLING CODE
        ;       AVAILABLE TO ALL USERS IF THEY DO NOT TRY TO
        ;       INTERCEPT INTERRUPT 24H.
        ; ******************************************************
        STI
        PUSH    DS
        PUSH    ES
        PUSH    DI
        PUSH    CX
        PUSH    AX
        MOV     DS,BP
        ;MOV     AX,[SI.SDEVATT]
        MOV     AX,[SI+SYSDEV.ATT]
        MOV     [CS:CDEVAT],AH
        PUSH    CS
        POP     ES
        ;MOV     DI,OFFSET RESGROUP:DEVENAM
        MOV     DI,DEVENAM
        MOV     CX,8
        ;ADD     SI,SDEVNAME	; Suck up device name (even on Block)
        ADD     SI,SYSDEV.NAME
        REP     MOVSB
        POP     AX
        POP     CX
        POP     DI
        POP     ES		; Stack just contains DS at this point
        CALL    SAVHAND
        PUSH    CS
        POP     DS              ; Set up local data segment
;ASSUME  DS:RESGROUP

        PUSH    DX
        CALL    CRLF
        POP     DX

        ADD     AL,"A"          ; Compute drive letter (even on character)
        MOV     [DRVLET],AL
        TEST    AH,80H          ; Check if hard disk error
        JZ      SHORT NOHARDE
        ;TEST    [CDEVAT],DEVTYP SHR 8
        TEST    byte [CDEVAT],DEVTYP >> 8
        JNZ     short NOHARDE
        JMP     FATERR
NOHARDE:
        ;MOV     SI,OFFSET RESGROUP:MREAD
        MOV     SI,MREAD
        TEST    AH,1
        JZ      short SAVMES
        ;MOV     SI,OFFSET RESGROUP:MWRITE
        MOV     SI,MWRITE
SAVMES:
        LODSW
        MOV     [IOTYP],AX
        LODSW
        MOV     [IOTYP+2],AX
        AND     DI,0FFH
        CMP     DI,12
        JBE     short HAVCOD
        MOV     DI,12
HAVCOD:
        MOV     [ERRCD_24],DI
        SHL     DI,1
        ;MOV     DI,WORD PTR [DI+MESBAS] ; Get pointer to error message
        MOV     DI,[DI+MESBAS]
        XCHG    DI,DX           ; May need DX later
        CALL    RPRINT          ; Print error type
        ;MOV     DX,OFFSET RESGROUP:ERRMES
        MOV     DX,ERRMES
        CALL    RPRINT
        ;TEST    [CDEVAT],DEVTYP SHR 8
        TEST	byte [CDEVAT],DEVTYP >> 8
	JZ      short BLKERR
        ;MOV     DX,OFFSET RESGROUP:DEVEMES
        MOV	DX,DEVEMES
        MOV     AH,STD_CON_STRING_OUTPUT
        INT     int_command
        JMP     SHORT ASK       ; Don't ralph on COMMAND

BLKERR:
        ;MOV     DX,OFFSET RESGROUP:DRVNUM
       	MOV     DX,DRVNUM
        CALL    RPRINT
        CMP     byte [LOADING],0
        JZ      short ASK
        CALL    RESTHAND
        JMP     GETCOMDSK2	; If error loading COMMAND, re-prompt
ASK:
        ;MOV     DX,OFFSET RESGROUP:REQUEST
        MOV	DX,REQUEST
        CALL    RPRINT
	;MOV     AX,(STD_CON_INPUT_FLUSH SHL 8)+STD_CON_INPUT
	MOV     AX,(STD_CON_INPUT_FLUSH*256)+STD_CON_INPUT
        INT     int_command             ; Get response
        CALL    CRLF
        OR      AL,20H          ; Convert to lower case
        MOV     AH,0            ; Return code for ignore
        CMP     AL,"i"          ; Ignore?
        JZ	short EEXIT
        INC     AH
        CMP     AL,"r"          ; Retry?
        JZ	short EEXIT
        INC     AH
        CMP     AL,"a"          ; Abort?
        JNZ     ASK
        XOR     DX,DX
        XCHG    DL,[PIPEFLAG]   ; Abort a pipe in progress
        OR      DL,DL
        JZ	short CHECKFORA
        CMP     word [SINGLECOM],0
        JZ      short CHECKFORA
        MOV     word [SINGLECOM],-1 ; Make sure SINGLECOM exits
CHECKFORA:
        CMP     word [ERRCD_24],0 ; Write protect
        JZ      short ABORTFOR
        CMP     word [ERRCD_24],2 ; Drive not ready
        JNZ     short EEXIT	; Don't abort the FOR
ABORTFOR:
        MOV     byte [FORFLAG],0 ; Abort a FOR in progress
        CMP	word [SINGLECOM],0
        JZ      short EEXIT
        MOV     word [SINGLECOM],-1 ; Make sure SINGLECOM exits
EEXIT:
        MOV     AL,AH
        MOV     DX,DI
RESTHD:
        CALL    RESTHAND
        POP     DS
        IRET

FATERR:
        ;MOV     DX,OFFSET RESGROUP:BADFAT
        MOV     DX,BADFAT
        CALL    RPRINT
        ;MOV     DX,OFFSET RESGROUP:ERRMES
        MOV     DX,ERRMES
        CALL    RPRINT
        ;MOV     DX,OFFSET RESGROUP:DRVNUM
        MOV     DX,DRVNUM
        CALL    RPRINT
        MOV     AL,2            ; Abort
        JMP     short RESTHD

; Print routines for Tokenized resident messages

;ASSUME DS:RESGROUP,SS:RESGROUP

CRLF:
	;MOV     DX,OFFSET RESGROUP:NEWLIN
	MOV     DX,NEWLIN
RPRINT:
        PUSH    AX              ; Tokenized message printer
        PUSH    BX
        PUSH    DX
        PUSH    SI
        MOV     SI,DX
RPRINT1:
        LODSB
        PUSH    AX
        AND     AL,7FH
        CMP     AL,"0"
        JB      RPRINT2
        CMP     AL,"9"
        JA      RPRINT2
        SUB     AL,"0"
        CBW
        SHL     AX,1
        ;MOV     BX,OFFSET RESGROUP:MESADD
        MOV	BX,MESADD
        ADD     BX,AX
        MOV     DX,[BX]
        CALL    RPRINT
        JMP     SHORT RPRINT3
RPRINT2:
        MOV     DL,AL
        MOV     AH,STD_CON_OUTPUT
        INT     int_command
RPRINT3:
        POP     AX
        TEST    AL,10000000B                    ; High bit set indicates end
        JZ      RPRINT1
        POP     SI
        POP     DX
        POP     BX
        POP     AX
        RETN

;CODERES ENDS
;        END

;=============================================================================
; RDATA.ASM
;=============================================================================

ALIGN 2

RDATA	EQU	$

;TITLE   COMMAND Resident DATA

; Data for resident portion

;DATARES SEGMENT PUBLIC BYTE

	;ORG     0
;ZERO    =       $

MESBAS  DW      ERR0
        DW      ERR1
        DW      ERR2
        DW      ERR3
        DW      ERR4
        DW      ERR5
        DW      ERR6
        DW      ERR7
        DW      ERR8
        DW      ERR9
        DW      ERR10
        DW      ERR11
        DW      ERR12

MESADD  DW      NEWLIN          ;"0"
        DW      COM$1           ;"1"
        DW      ERR3            ;"2"
        DW      ALLOC$3         ;"3"
        DW      FILE$4          ;"4"
        DW      RROR$5          ;"5"
        DW      CAN$6           ;"6"
        DW      EMORY$7         ;"7"
        DW      BAT$8           ;"8"
        DW      INS$9           ;"9"

ERR0    DB      "Write protec","t"+80h
ERR1    DB      "Bad uni","t"+80h
ERR2    DB      "Not read","y"+80h
ERR3    DB      "Bad command"," "+80h
ERR4    DB      "Dat","a"+80h
ERR5    DB      "Bad call forma","t"+80h
ERR6    DB      "See","k"+80h
ERR7    DB      "Non-DOS dis","k"+80h
ERR8    DB      "Sector not foun","d"+80h
ERR9    DB      "No pape","r"+80h
ERR10   DB      "Write faul","t"+80h
ERR11   DB      "Read faul","t"+80h
ERR12   DB      "Dis","k"+80h
MREAD   DB      "read"
MWRITE  DB      "writ"
ERRMES  DB      " e5"
IOTYP   DB      "writin","g"+80h
DRVNUM  DB      " drive "
DRVLET  DB      "A"
NEWLIN  DB      13,10+80h
DEVEMES DB      " device "
;DEVENAM DB      8 DUP (?)
DEVENAM TIMES	8 DB 0
        DB      13,10,"$"	;Must be $ terminated
COM$1   DB      " COMMAN","D"+80h
ALLOC$3 DB      " allocation"," "+80h
FILE$4  DB      " file"," "+80h
RROR$5  DB      "rror"," "+80h
CAN$6   DB      "Cannot"," "+80h
EMORY$7 DB      "emor","y"+80h
BAT$8   DB      " batc","h"+80h
INS$9   DB      "Inser","t"+80h
REQUEST DB      "Abort, Retry, Ignore?"," "+80h
CDEVAT  DB      0
BADFAT  DB      "0File3table bad",","+80h
COMBAD  DB      "0Invalid1.COM"
NEEDCOM DB      "091.COM disk in"," "+80h
DRVMSG  DB      "drive "
COMLET  DB      "A"," "+80h
DEFMSG  DB      "default driv","e"+80h
PROMPT  DB      "0and strike any key when ready","0"+80h
ENDBATMES DB    "0Terminate8 job (Y/N)?"," "+80h
EXECEMES DB     "EXEC failure","0"+80h
EXEBAD  DB      "E5in EXE4","0"+80h
TOOBIG  DB      "Program too big to fit in m7","0"+80h
NOCOM   DB      "0Bad or missing Command Interprete","r"+80H
NOHANDMES DB    "0No free4handle","s"+80h
BMEMMES DB      "0M73e","5"+80h
HALTMES DB      "06load1, system halte","d"+80h
FRETMES DB      "06start1, exiting","0"+80h
RBADNAM DB      "2or4name","0"+80h

RETRABASE: ; 30/04/2018 (Base addr for common params, for transient portion)
	   ; Note: Order of following parameters must not be changed
	   ; without changing address references of them in 'transcom.s').	

INT_2E_RET DD   0               ; Magic command executer return address
SAVE_PDB DW     0
PARENT   DW     0
ERRCD_24 DW     0
HANDLE01 DW     0
LOADING DB      0
BATCH   DW      0               ; Assume no batch mode initially
BATLOC  DD      0               ; Position in batch file
;COMSPEC DB	40 DUP(0)
COMSPEC	TIMES	40 DB 0
;TRANS   DW      OFFSET TRANGROUP:COMMAND
TRANS   DW      COMMAND
TRNSEG  DW      0
COMDRV  DB      0               ; DRIVE SPEC TO LOAD AUTOEXEC AND COMMAND
BATBYT  DB      0
MEMSIZ  DW      0
SUM     DW      0
EXTCOM  DB      1               ; For init, pretend just did an external
RETCODE DW      0
ECHOFLAG DB     1
IO_SAVE DW      0
RESTDIR DB      0
PERMCOM DB      0
SINGLECOM DW    0
VERVAL  DW      -1

FORFLAG DB      0
UFORDRV DB      0
;FORSET  DB      40 DUP (?)
FORSET  TIMES	40 DB 0
;FORCOM  DB      90 DUP (?)
FORCOM	TIMES	90 DB 0
FORVAR  DB      0
FORPTR  DW      0
FORUFCB DB      -1
FORFCB  DB      0
        ;DB      34 DUP(?)
        TIMES	34 DB 0
	DW      0
        DW      0               ; Initialize RR field to zero

;RE_INSTR DB     40 DUP(0)
RE_INSTR TIMES	40 DB 0
RE_OUT_APP DB   0
;RE_OUTSTR DB    40 DUP(0)
RE_OUTSTR TIMES	40 DB 0

;These two bytes refed as a word
PIPEFLAG  DB    0
PIPEFILES DB    0

PIPEPTR DW      0
;PIPESTR DB      129 DUP(?)
PIPESTR	TIMES	129 DB 0
PIPE1   DB      "_:/%PIPE1.$$$",0
PIPE2   DB      "_:/%PIPE2.$$$",0
;INPIPEPTR DW    OFFSET RESGROUP:PIPE1
INPIPEPTR DW    PIPE1
;OUTPIPEPTR DW   OFFSET RESGROUP:PIPE2
OUTPIPEPTR DW   PIPE2

;EXEC_BLOCK LABEL BYTE		; The data block for EXEC calls
EXEC_BLOCK:
ENVIRSEG DW	0
;COM_PTR LABEL   DWORD
COM_PTR:
        DW      80H             ; Point at unformatted parameters
        DW      0
;COM_FCB1 LABEL  DWORD
COM_FCB1:
        DW      5CH
        DW      0
;COM_FCB2 LABEL  DWORD
COM_FCB2:
        DW      6CH
        DW      0

;PARMBUF DB      129 DUP (?)
PARMBUF	TIMES	129 DB 0

;IF IBMVER
;SYS_CALL DD  0
;;ZEXEC    DW  OFFSET ZEXEC_CODE:$EXEC
;ZEXEC    DW  ZEXEC_CODE+$EXEC
;EXESEG   DW  0
;USER_SP  DW  0
;USER_SS  DW  0
;EXESUM   DW  0
;ENDIF

;TRANVARS LABEL  BYTE		; Variables passed to transient
TRANVARS:
        ;DW      OFFSET RESGROUP:THEADFIX
	DW      THEADFIX
MYSEG   DW      0               ; Put our own segment here
LTPA    DW      0               ; WILL STORE TPA SEGMENT HERE
RSWITCHAR DB    "-"
RDIRCHAR DB     "/"
        ;DW      OFFSET RESGROUP:EXT_EXEC
        DW      EXT_EXEC
MYSEG1  DW      0
	;DW      OFFSET RESGROUP:TREMCHECK
	DW      TREMCHECK
MYSEG2  DW      0

;DATARESEND	LABEL   BYTE
DATARESEND:

;DATARES ENDS
;        END

;=============================================================================
; COMMAND.ASM
;=============================================================================

align 16

;ENVIRONMENT SEGMENT PUBLIC PARA        ; Default COMMAND environment

;	PUBLIC  ECOMSPEC,ENVIREND,PATHSTRING

        ;ORG     0
;ENVARENA DB     10H DUP (?)	; Pad for mem arena
ENVARENA TIMES	16 DB 0		
ENVIRONMENT: 			; 30/04/2018
PATHSTRING DB   "PATH="
;USERPATH LABEL  BYTE
USERPATH:
        DB      0               ; Null path
        DB      "COMSPEC="
ECOMSPEC DB     "/COMMAND.COM"
        ;DB      134 DUP (0)
	TIMES	134 DB 0

;ENVIREND	LABEL   BYTE
ENVIREND:

ENVIRONSIZ EQU  $-PATHSTRING	; 160 = 0A0h
ENVIRONSIZ2 EQU $-ECOMSPEC	; 146 = 092h
;ENVIRONMENT ENDS

;=============================================================================
; INIT.ASM
;=============================================================================

;TITLE   COMMAND Initialization

;ENVIRONSIZ EQU  0A0H		;Must agree with values in EVIRONMENT segment
;ENVIRONSIZ2 EQU 092H

; START OF INIT PORTION
; This code is overlayed the first time the TPA is used.

;INIT    SEGMENT PUBLIC PARA

; 	EXTRN   HEADER:BYTE
;	EXTRN   BADCOMLKMES:BYTE

;	PUBLIC  CONPROC

;ASSUME  CS:RESGROUP,DS:RESGROUP,ES:RESGROUP,SS:RESGROUP

        ;ORG     0
;ZERO	=       $

CONPROC:
	;MOV	SP,OFFSET RESGROUP:RSTACK
	MOV	SP,RSTACK

        ;IF      HIGHMEM
        ;MOV     BX,WORD PTR DS:[PDB_block_len]
        ;MOV     AX,OFFSET RESGROUP:ENVIREND + 15
        ;MOV     CL,4
        ;SHR     AX,CL
        ;PUSH    AX                         ; Save size to alloc
        ;INC     AX                         ; Plus one for arena
        ;SUB     BX,AX                      ; Subtract size of resident
        ;MOV     WORD PTR DS:[PDB_block_len],BX
        ;MOV     AX,CS
        ;SUB     BX,AX
        ;MOV     AH,SETBLOCK
        ;INT     21H
        ;POP     BX			    ; Get back size to alloc
        ;MOV     AH,ALLOC
        ;INT     21H
        ;MOV     [REALRES],AX
        ;MOV     ES,AX
        ;XOR     SI,SI
        ;MOV     DI,SI
        ;MOV     CX,OFFSET RESGROUP:ENVIREND
        ;SHR     CX,1          ; Length of resident and environment in words
        ;                      ; Last byte doesn't matter
        ;REP     MOVSW                   ; Move to end of memory
        ;MOV     DS,AX
        ;MOV     BX,AX
        ;MOV     AH,SET_CURRENT_PDB
        ;INT     21H
        ;MOV     AX,BX
        ;MOV     BX,OFFSET RESGROUP:DATARESEND + 15
        ;MOV     CL,4
        ;SHR     BX,CL           ; BX is size for SETBLOCK
        ;MOV     WORD PTR DS:[PDB_block_len],BX
        ;ADD     WORD PTR DS:[PDB_block_len],AX
        ;MOV     [LTPA],CS
        ;MOV     AH,SETBLOCK
        ;INT     21H             ;Shrink to not include environment
        ;MOV     BX,(ENVIRONSIZ + 15) / 16
        ;MOV     AH,ALLOC
        ;INT     21H             ;Allocate the environment
        ;MOV     [ENVIRSEG],AX
        ;MOV     CS:[ENVIRSEGSAV],AX
        ;MOV     ES,AX
;ASSUME	ES:ENVIRONMENT
        ;XOR     DI,DI
        ;MOV     SI,OFFSET RESGROUP:PATHSTRING
        ;MOV     CX,ENVIRONSIZ
        ;REP     MOVSB
        ;MOV     AX,WORD PTR CS:[PDB_block_len]
        ;ENDIF

	;IF      NOT HIGHMEM
        ;MOV     AX,OFFSET RESGROUP:ENVIREND + 15
        MOV     AX,ENVIREND+15
        MOV     CL,4
        SHR     AX,CL
        MOV     CX,CS
        ADD     AX,CX                         ; Compute segment of TPA
        MOV     [LTPA],AX                     ; Good enough for the moment
        ;MOV     AX,WORD PTR DS:[PDB_block_len]
        MOV     AX,[PDB.BLOCK_LEN]
        ;ENDIF

        MOV     [MYSEG1],DS
        MOV     [MYSEG2],DS
        MOV     [MYSEG],DS
        MOV     [MEMSIZ],AX

        ;MOV     DX,OFFSET TRANGROUP:TRANSPACEEND + 15
        MOV     DX,TRANSPACEEND + 15
        MOV     CL,4
        SHR     DX,CL

        ;IF      IBM
        ;PUSH    DX
        ;;MOV     DX,OFFSET EGROUP:ZEXECDATAEND + 15
        ;MOV     DX,EGROUP+ZEXECDATAEND + 15
        ;MOV     CL,4
        ;SHR     DX,CL
        ;POP     CX
        ;ADD     DX,CX
        ;ENDIF

        SUB     AX,DX
        MOV     [TRNSEG],AX		; Read it in here
        ;MOV     AX,DS:[PDB_environ]
        MOV     AX,[PDB.ENVIRON]
        OR      AX,AX
        JZ      SHORT BUILDENV		; Need to make an environment

        ;IF      HIGHMEM
        ;INC     BYTE PTR CS:[CHUCKENV]	; Flag no ENVIRONSEG
        ;ELSE
        INC     BYTE [CHUCKENV]		; Flag no ENVIRONSEG
        ;ENDIF

        JMP     SHORT ENVIRONPASSED

BUILDENV:

	;IF      NOT HIGHMEM
        ;MOV     AX,OFFSET RESGROUP:PATHSTRING ; Figure environment pointer
	MOV	AX, PATHSTRING
        MOV     CL,4
        SHR     AX,CL
        MOV     DX,DS
        ADD     AX,DX
        ;ELSE
        ;JMP     SHORT GOTTHEENVIR
        ;ENDIF

ENVIRONPASSED:
        MOV     [ENVIRSEG],AX

        ;IF      HIGHMEM
        ;DEC     AX
        ;MOV     ES,AX
        ;INC     AX
        ;MOV     ES:[arena_owner],DS	; Adjust owner of passed envir
        ;ENDIF

        MOV     ES,AX
;ASSUME  ES:ENVIRONMENT

GOTTHEENVIR:
        ;MOV     AX,CHAR_OPER SHL 8
        MOV     AX,CHAR_OPER*256
        INT     int_command
        MOV     [RSWITCHAR],DL

        CMP     DL,'/'
        JNZ     short IUSESLASH

        ;IF      HIGHMEM
        ;MOV     CS:[COMSPECT],'\'
        ;ELSE
        MOV     byte [COMSPECT],'\'
        ;ENDIF

        ;IF      HIGHMEM
        ;CMP     BYTE PTR CS:[CHUCKENV],0
        ;ELSE
        CMP     BYTE [CHUCKENV],0
        ;ENDIF

        JNZ     short IUSESLASH

        ;MOV     ES:[ECOMSPEC-10H],'\'
	; 30/04/2018
        MOV     byte [ES:(ECOMSPEC-ENVIRONMENT)],'\' ; [ES:0Eh]

IUSESLASH:

;IF IBMVER
        ;PUSH    ES
        ;;MOV     AX,(Get_interrupt_vector SHL 8) + int_command
        ;MOV     AX,(Get_interrupt_vector*256) + int_command
        ;INT     int_command
        ;MOV     WORD [SYS_CALL],BX
        ;MOV     WORD [SYS_CALL+2],ES
        ;;MOV     DX,OFFSET RESGROUP:SYSCALL
        ;MOV     DX,SYSCALL
        ;;MOV     AX,(Set_interrupt_vector SHL 8) + int_command
        ;MOV     AX,(Set_interrupt_vector*256) + int_command
        ;INT     int_command
        ;POP     ES
;ENDIF

        ;MOV     AL,BYTE PTR DS:[FCB]	; get drive spec for default
        MOV     AL,[FCB]
        MOV     AH,DRVCHAR
        MOV     [COMDRV],AL
        ADD     AL,40H                  ; Convert to letter
        CMP     AL,40H
        JZ      short NOCOMDRV
        STD
        ;IF      HIGHMEM
        ;CMP     BYTE PTR CS:[CHUCKENV],0
        ;ELSE
        CMP     BYTE [CHUCKENV],0
        ;ENDIF

        JNZ     short NOTWIDENV

        PUSH    DS
        PUSH    ES
        POP     DS
        ;MOV	DI,OFFSET ENVIRONMENT:ECOMSPEC + ENVIRONSIZ2 - 1 - 10H
	; 30/04/2018
        MOV	DI,(ECOMSPEC-ENVIRONMENT) + ENVIRONSIZ2 - 1 ; mov di,9Fh
        ;MOV	SI,OFFSET ENVIRONMENT:ECOMSPEC + ENVIRONSIZ2 - 3 - 10H
	; 30/04/2018
        MOV	SI,(ECOMSPEC-ENVIRONMENT) + ENVIRONSIZ2 - 3 ; mov si,9Dh 
        MOV     CX,ENVIRONSIZ2 - 2 ; mov cx,90h
        REP     MOVSB

        POP     DS
	;MOV	WORD PTR ES:[ECOMSPEC-10H],AX
	; 30/04/2018
	MOV	[ES:(ECOMSPEC-ENVIRONMENT)],AX   ; mov [ES:0Eh], ax

NOTWIDENV:
        CLD
	;IF	HIGHMEM
        ;MOV	WORD PTR CS:[AUTOBAT],AX
        ;ELSE
        MOV     [AUTOBAT],AX
        ;ENDIF

        MOV     [COMLET],AL
NOCOMDRV:
        CALL    SETVECT         ; Set the vectors

        MOV     SI,80H
        LODSB
        MOV     CL,AL
        XOR     CH,CH
        JCXZ    COMRETURNSJ     ; No parameters
        MOV     SI,81H          ; Start of parms
CHKARG:
        LODSB
        CMP     AL,' '
        JZ      short NEXTCH
        CMP     AL,9            ; Tab only other delimiter
        JZ      short NEXTCH
        CMP     AL,[RSWITCHAR]	; Switch?
        JNZ     short CHKOTHERARGS ; No
        DEC     CX
        JCXZ    ARGSDONEJ       ; oops
        LODSB
        OR      AL,20H          ; Lower case
        CMP     AL,'p'          ; PERMCOM switch
        JNZ     short NEXTCH
        JMP     SETPERM

NEXTCH:
        CMP     AL,'d'
        JNZ     short NEXTCH3

        ;IF	HIGHMEM
        ;MOV	BYTE PTR CS:[PRDATTM],1  ; User explicitly says no date time
        ;ELSE
        MOV     BYTE [PRDATTM],1  ; User explicitly says no date time
	;ENDIF

        LOOP    CHKARG
        JMP     SHORT ARGSDONEJ
NEXTCH3:
        CMP     AL,'c'
        JNZ	short NEXTCH2	     ; SINGLECOM switch 2
        MOV     [SINGLECOM],SI	     ; Point to the rest of the command line
        MOV     byte [PERMCOM],0     ; A SINGLECOM must not be a PERMCOM

        ;IF      HIGHMEM
        ;MOV     BYTE PTR CS:[PRDATTM],1  ; No date or time either, explicit
        ;ELSE
        MOV     BYTE [PRDATTM],1     ; No date or time either, explicit
        ;ENDIF

ARGSDONEJ:
        JMP	ARGSDONE

NEXTCH2:
        LOOP    CHKARG

COMRETURNSJ:
        JMP	COMRETURNS

CHKOTHERARGS:
        DEC     SI
        MOV     DX,SI
        PUSH    CX
        PUSH    SI
CONTRLOOP:
        LODSB
        DEC     CX
        CMP     AL,' '
        JZ      short SETCDEV
        CMP     AL,9
        JZ      short SETCDEV
        JCXZ    SETCDEVA
        JMP     SHORT CONTRLOOP

SETCDEVA:
        INC     SI
SETCDEV:
        MOV     BYTE [SI-1],0
        ;MOV     AX,(OPEN SHL 8) OR 2	; Read and write
        MOV     AX,(OPEN*256) | 2
        INT     int_command
        JC      short CHKSRCHSPEC	; Wasn't a file
        MOV     BX,AX
        ;MOV     AX,IOCTL SHL 8
        MOV     AX,IOCTL*256
        INT     int_command
        TEST    DL,80H
        JNZ     short ISADEVICE
        MOV     AH,CLOSE       ; Close initial handle, wasn't a device
        INT     int_command
        JMP     short CHKSRCHSPEC

ISADEVICE:
        XOR     DH,DH
        OR      DL,3            ; Make sure has CON attributes
        ;MOV     AX,(IOCTL SHL 8) OR 1
        MOV     AX,(IOCTL*256) | 1
        INT     int_command
        MOV     DX,BX           ; Save new handle
        POP     BX              ; Throw away saved SI
        POP     BX              ; Throw away saved CX
        PUSH    CX
        MOV     CX,3
        XOR     BX,BX
RCCLLOOP:			; Close 0,1 and 2
        MOV     AH,CLOSE
        INT     int_command
        INC     BX
        LOOP    RCCLLOOP
        MOV     BX,DX           ; New device handle
        MOV     AH,XDUP
        INT     int_command	; Dup to 0
        MOV     AH,XDUP
        INT     int_command	; Dup to 1
        MOV     AH,XDUP
        INT     int_command	; Dup to 2
        MOV     AH,CLOSE
        INT     int_command	; Close initial handle
        POP     CX
        JCXZ    ARGSDONEJ2
        JMP     CHKARG

CHKSRCHSPEC:                    ; Not a device, so must be directory spec

        ;IF      HIGHMEM
        ;MOV     BYTE PTR CS:[CHUCKENV],0 ; If search specified -- no inheritance
        ;MOV     AX,CS:[ENVIRSEGSAV]
        ;MOV     [ENVIRSEG],AX
        ;ELSE
        MOV     BYTE [CHUCKENV],0 ; If search specified -- no inheritance
        ;MOV     AX,OFFSET RESGROUP:PATHSTRING   ; Figure environment pointer
        MOV     AX,PATHSTRING
        MOV     CL,4
        SHR     AX,CL
        MOV     DX,DS
        ADD     AX,DX
        MOV     [ENVIRSEG],AX
	;ENDIF

        MOV     ES,AX
        MOV     BYTE [SI-1],' '
        POP     SI                      ; Remember location
        POP     CX                      ; and count

        ;IF      HIGHMEM
        ;MOV     DI,CS:[ECOMLOC]
        ;ELSE
        MOV     DI,[ECOMLOC]
        ;ENDIF

COMTRLOOP:
        LODSB
        DEC     CX
        CMP     AL,' '
        JZ      short SETCOMSR
        CMP     AL,9
        JZ      short SETCOMSR
        STOSB

        ;IF	KANJI
        ;XOR	AH,AH
        ;ENDIF

        JCXZ    SETCOMSR

        ;IF      KANJI
        ;CALL    ITESTKANJ
        ;JZ      COMTRLOOP
        ;DEC     CX
        ;MOVSB
        ;INC     AH
        ;JCXZ    SETCOMSR
        ;ENDIF

        JMP     SHORT COMTRLOOP

SETCOMSR:
        PUSH    SI
        PUSH    CX

        PUSH    DS

        ;IF      HIGHMEM
        ;PUSH    CS
        ;POP     DS
        ;ENDIF

	;MOV     SI,OFFSET RESGROUP:COMSPECT
	MOV     SI,COMSPECT
        MOV     CX,14

        ;MOV     AL,ES:[DI-1]
        MOV     AL,[ES:DI-1]

        ;IF	KANJI
        ;OR 	AH,AH
        ;JNZ	INOTROOT ; Last char was KANJI second byte, might be '\'
        ;ENDIF

        CALL    PATHCHRCMPR
        JNZ     short INOTROOT
        INC     SI              ; Don't make a double /
        DEC     CX
INOTROOT:
        REP     MOVSB

        MOV     DX,[ECOMLOC]    ; Now lets make sure its good!
        PUSH    ES
        POP     DS

        ;MOV     AX,OPEN SHL 8
        MOV     AX,OPEN*256
        INT     int_command	; Open COMMAND.COM
        POP     DS
        JC      short SETCOMSRBAD ; No COMMAND.COM here
        MOV     BX,AX           ; Handle
        MOV     AH,CLOSE
        INT     int_command	; Close COMMAND.COM
SETCOMSRRET:
        POP     CX
        POP     SI
ARGSDONEJ2:
        JCXZ    ARGSDONE
        JMP     CHKARG

SETCOMSRBAD:

        ;IF	HIGHMEM
        ;PUSH	DS
        ;PUSH	CS
        ;POP	DS
        ;ENDIF

        ;MOV     DX,OFFSET RESGROUP:BADCOMLKMES
        MOV     DX,BADCOMLKMES
        MOV     AH,STD_CON_STRING_OUTPUT
        INT     int_command
        ;MOV     SI,OFFSET RESGROUP:COMSPECT
        MOV     SI,COMSPECT
        MOV     DI,[ECOMLOC]
        MOV     CX,14
        REP     MOVSB           ; Get my default back

        ;IF	HIGHMEM
        ;POP	DS
        ;ENDIF

        JMP     SHORT SETCOMSRRET

CHKARGJ:
        JMP	CHKARG

SETPERM:
        INC     byte [PERMCOM]

        ;IF	HIGHMEM
        ;CMP	BYTE PTR CS:[PRDATTM],-1
        ;ELSE
        CMP     BYTE [PRDATTM],-1
        ;ENDIF

        JNZ     short LOOPIT

        ;IF	HIGHMEM
        ;MOV	BYTE PTR CS:[PRDATTM],0 ; If not set explicit, set to prompt
        ;ELSE
        MOV     BYTE [PRDATTM],0
        ;ENDIF

LOOPIT:
        LOOP    CHKARGJ
ARGSDONE:
        CMP     byte [PERMCOM],0
        JZ      short COMRETURNS
        PUSH    ES                      ; Save environment pointer
        MOV     AH,SET_CURRENT_PDB
        MOV     BX,DS
        MOV     ES,BX
        INT     int_command		; Current process is me
        ;MOV	DI,PDB_Exit             ; Diddle the addresses in my header
        MOV	DI,PDB.EXIT
        ;MOV	AX,OFFSET RESGROUP:LODCOM
        MOV	AX,LODCOM
        STOSW
        MOV     AX,DS
        STOSW
        ;MOV     AX,OFFSET RESGROUP:CONTC
        MOV     AX,CONTC
        STOSW
        MOV     AX,DS
        STOSW
	;MOV	WORD PTR DS:[PDB_Parent_PID],DS ; Parent is me forever
	MOV	[PDB.PARENT_PID],DS
        ;MOV	DX,OFFSET RESGROUP:INT_2E
        MOV	DX,INT_2E
        ;MOV	AX,(SET_INTERRUPT_VECTOR SHL 8) OR 02EH
        MOV	AX,(SET_INTERRUPT_VECTOR*256) | 02EH
        INT     int_command                     ;Set magic interrupt
        POP     ES                              ;Remember environment
COMRETURNS:
        ;MOV     AX,WORD PTR DS:[PDB_Parent_PID]
	MOV     AX,[PDB.PARENT_PID]
        MOV     [PARENT],AX                     ; Save parent
        ;MOV     WORD PTR DS:[PDB_Parent_PID],DS ; Parent is me
        MOV     [PDB.PARENT_PID],DS 
        ;MOV     AX,WORD PTR DS:[PDB_JFN_Table]
        MOV	AX,[PDB.JFN_TABLE]
        MOV     [IO_SAVE],AX		; Get the default stdin and out
        MOV     [COM_PTR+2],DS		; Set all these to resident
        MOV     [COM_FCB1+2],DS
        MOV     [COM_FCB2+2],DS
        ;MOV     DI,OFFSET RESGROUP:COMSPEC
        MOV	DI,COMSPEC

        ;IF      HIGHMEM
        ;MOV     SI,CS:[ECOMLOC]
        ;CMP     BYTE PTR CS:[CHUCKENV],0
        ;ELSE
        MOV     SI,[ECOMLOC]
        CMP     BYTE [CHUCKENV],0
        ;ENDIF

        MOV     AX,DS                   ; XCHG ES,DS
        PUSH    ES
        POP     DS
        MOV     ES,AX

        JZ	short COPYCOMSP		; All set up for copy

        PUSH    CS
        POP     DS

        ;MOV     SI,OFFSET RESGROUP:COMSPSTRING
	MOV     SI,COMSPSTRING
        PUSH    ES
        PUSH    DI
        CALL    IFINDE
        MOV     SI,DI
        PUSH    ES
        POP     DS
        POP     DI
        POP     ES
        JNC     short COPYCOMSP

COMSPECNOFND:

        ;IF      HIGHMEM
        ;MOV     DS,CS:[ENVIRSEG]
        ;MOV     SI,CS:[ECOMLOC]
        ;ELSE
        MOV     SI,[ECOMLOC]
        ;ADD     SI,OFFSET RESGROUP:PATHSTRING
        ADD	SI,PATHSTRING
	PUSH    CS
        POP     DS
        ;ENDIF

COPYCOMSP:
        LODSB
        STOSB
        OR      AL,AL
        JNZ     short COPYCOMSP

        ;IF      HIGHMEM
        ;MOV     DS,CS:[REALRES]
        ;PUSH    CS
        ;POP     ES
        ;MOV     AH,DEALLOC
        ;INT     21H
        ;CMP     BYTE PTR CS:[CHUCKENV],0
        ;JZ      GOTENVIR		; Environment is ok
        ;MOV     ES,CS:[ENVIRSEGSAV]
        ;MOV     AH,DEALLOC
        ;INT     21H
        ;ELSE
        PUSH    CS
        POP     DS
        ;MOV     BX,OFFSET RESGROUP:DATARESEND + 15
	mov	BX, DATARESEND + 15
        MOV     CL,4
        SHR     BX,CL
        MOV     AH,SETBLOCK
        INT     int_command		; Shrink me to the resident only
        CMP     BYTE [CHUCKENV],0
        JNZ     SHORT GOTENVIR		; Environment was passed
        MOV     BX,(ENVIRONSIZ + 15) /16
        MOV     AH,ALLOC
        INT     int_command		; "ALLOCATE" the environment
        MOV     DS,[ENVIRSEG]
        ;MOV     [ENVIRSEG],AX
        MOV     [CS:ENVIRSEG],AX
        MOV     ES,AX
        XOR     SI,SI
        MOV     DI,SI
        MOV     CX,ENVIRONSIZ
        REP     MOVSB
        PUSH    CS
        POP     DS
        ;ENDIF

GOTENVIR:
        CALL    LOADCOM                 ; Load the transient in the right place
        CALL    CHKSUM                  ; Compute the checksum
        MOV     [SUM],DX                ; Save it
;IF IBM
        ;MOV     AX,[MEMSIZ]
        ;;MOV     DX,OFFSET EGROUP:ZEXECDATAEND + 15
        ;MOV     DX,EGROUP+ZEXECDATAEND + 15
        ;MOV     CL,4
        ;SHR     DX,CL
        ;SUB     AX,DX
        ;MOV     [EXESEG],AX
        ;CALL    EXECHK
        ;MOV     [EXESUM],DX
;ENDIF
        ;IF MSVER
        CMP     word [SINGLECOM],0
        JNZ     short NOPHEAD	; Don't print header if SINGLECOM
        ;IF	HIGHMEM
        ;PUSH    DS
        ;PUSH    CS
        ;POP     DS
        ;ENDIF
        ;MOV     DX,OFFSET RESGROUP:HEADER
	MOV     DX,HEADER
        MOV     AH,STD_CON_STRING_OUTPUT
        INT     int_command
        ;IF      HIGHMEM
        ;POP     DS
        ;ENDIF
NOPHEAD:
	;ENDIF

        ;IF      HIGHMEM
        ;CMP     BYTE PTR CS:[PRDATTM],0
        ;ELSE
        CMP     BYTE [PRDATTM],0
        ;ENDIF

        JNZ	short NODTTM	; Don't do AUTOEXEC or date time
        MOV     BX,3		; 48 BYTES ENOUGH
        MOV     AH,ALLOC
        INT     int_command
        JC      short DODTTM	; PRETEND NO BATCH
        MOV     [BATCH],AX
        MOV     ES,AX
        XOR     DI,DI

        ;IF      HIGHMEM
        ;CMP     BYTE PTR CS:[AUTOBAT],0
        ;ELSE
        CMP     BYTE [AUTOBAT],0
        ;ENDIF

        JNZ	short NOAUTSET
        MOV     AH,GET_DEFAULT_DRIVE
        INT     int_command
        ADD     AL,'A'

        ;IF      HIGHMEM
        ;MOV     CS:[AUTOBAT],AL
        ;ELSE
        MOV     [AUTOBAT],AL
        ;ENDIF

NOAUTSET:

        ;IF      HIGHMEM
        ;PUSH    DS
        ;PUSH    CS
        ;POP     DS
        ;ENDIF

        ;MOV     SI,OFFSET RESGROUP:AUTOBAT
	MOV     SI,AUTOBAT
        MOV     CX,8
        REP     MOVSW   ; NAME
        MOV     AX,-1
        MOV     CL,10
        REP     STOSW   ; PARMS
        ;MOV     DX,OFFSET RESGROUP:AUTOBAT
        MOV     DX,AUTOBAT
        ;MOV     AX,OPEN SHL 8
        MOV     AX,OPEN*256
        INT     int_command	; See if AUTOEXEC.BAT exists
        JC	short NOABAT
        MOV     BX,AX
        MOV     AH,CLOSE
        INT     int_command

	;IF      HIGHMEM
        ;POP     DS
        ;ENDIF

        JMP     SHORT DRV0 	

NOABAT:
        ;IF      HIGHMEM
        ;POP     DS
        ;ENDIF

        MOV     ES,[BATCH]      ; Not found--turn off batch job
        MOV     AH,DEALLOC
        INT     int_command
        MOV     word [BATCH],0	; AFTER DEALLOC in case of ^C
DODTTM:

        ;IF      HIGHMEM
        ;MOV     AX,OFFSET TRANGROUP:DATINIT
        ;MOV     WORD PTR CS:[INITADD],AX
        ;MOV     AX,[TRNSEG]
        ;MOV     WORD PTR CS:[INITADD+2],AX
        ;CALL    DWORD PTR CS:[INITADD]
        ;ELSE
        ;MOV     AX,OFFSET TRANGROUP:DATINIT
	MOV     AX,DATINIT
        ;MOV     WORD PTR[INITADD],AX
        MOV     [INITADD],AX
        MOV     AX,[TRNSEG]
        ;MOV     WORD PTR[INITADD+2],AX
        MOV     [INITADD+2],AX
        ;CALL    DWORD PTR [INITADD]
        CALL	FAR [INITADD]
        ;ENDIF

NODTTM:
        ;;IF IBMVER
        ;CMP	WORD [SINGLECOM],0
        ;JNZ	short DRV0	; Don't print header if SINGLECOM
        ;;MOV	 DX,OFFSET RESGROUP:HEADER
        ;MOV	DX,HEADER
        ;MOV	AH,STD_CON_STRING_OUTPUT
        ;INT	int_command
        ;;ENDIF

DRV0:
        ;IF      HIGHMEM
        ;PUSH    DS
        ;MOV     AX,OFFSET RESGROUP:LODCOM
        ;PUSH    AX
;MQQ    PROC    FAR
;	RET
;MQQ	ENDP
        ;ELSE
        JMP     LODCOM		; Allocate the transient
        ;ENDIF

PATHCHRCMPR:
        CMP     byte [RSWITCHAR],'/'
        JZ      short RNOSLASHT
        CMP     AL,'/'
        JZ      short RET41
RNOSLASHT:
        CMP     AL,'\'
RET41:
        RETN

IFINDE:
        CALL    IFIND		; FIND THE NAME
        JC      short IFIND2	; CARRY MEANS NOT FOUND
        JMP     short ISCASB1	; SCAN FOR = SIGN
;
; On return of FIND1, ES:DI points to beginning of name
;
IFIND:
        CLD

        CALL    ICOUNT0		; CX = LENGTH OF NAME

        ;IF      HIGHMEM
        ;MOV     ES,CS:[REALRES]
;ASSUME  ES:RESGROUP
;        MOV     ES,ES:[ENVIRSEG]
;ASSUME  ES:NOTHING
        ;ELSE
        MOV     ES,[ENVIRSEG]
        ;ENDIF

        XOR     DI,DI
IFIND1:
        PUSH    CX
        PUSH    SI
        PUSH    DI
IFIND11:
        LODSB

        ;IF      KANJI
        ;CALL    ITESTKANJ
        ;JZ      NOTKANJ4
        ;DEC     SI
        ;LODSW
        ;INC     DI
        ;INC     DI
        ;CMP     AX,ES:[DI-2]
        ;JNZ     IFIND12
        ;DEC     CX
        ;LOOP    IFIND11
        ;JMP     SHORT IFIND12
;NOTKANJ4:
        ;ENDIF

        CALL    IUPCONV
        INC     DI
        CMP     AL,[ES:DI-1]
        JNZ     short IFIND12
        LOOP    IFIND11
IFIND12:
        POP     DI
        POP     SI
        POP     CX
        JZ      short IFIND2
        PUSH    CX
        CALL    ISCASB2		; SCAN FOR A NUL
        POP     CX
        CMP     BYTE [ES:DI],0
        JNZ     short IFIND1
        STC			; INDICATE NOT FOUND
IFIND2:
        RETN

ICOUNT0:
        PUSH    DS
        POP     ES
        MOV     DI,SI

        PUSH    DI 		; COUNT NUMBER OF CHARS UNTIL "="
        CALL    ISCASB1
;	JMP     SHORT ICOUNTX
;	PUSH    DI		; COUNT NUMBER OF CHARS UNTIL NUL
;	CALL    ISCASB2
ICOUNTX:
        POP     CX
        SUB     DI,CX
        XCHG    DI,CX
        RETN

ISCASB1:
        MOV     AL,"=" 		; SCAN FOR AN =
        JMP     SHORT ISCASBX
ISCASB2:
        XOR     AL,AL		; SCAN FOR A NUL
ISCASBX:
        MOV     CX,100H
        REPNZ   SCASB
        RETN

        ;IF      KANJI
;ITESTKANJ:
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
        ;XOR     AX,AX           ;Set zero
        ;POP     AX
        ;RET

;ISLEAD:
        ;PUSH    AX
        ;XOR     AX,AX           ;Set zero
        ;INC     AX              ;Reset zero
        ;POP     AX
        ;RET
        ;ENDIF

IUPCONV:
        CMP     AL,"a"
        JB      IRET22
        CMP     AL,"z"
        JA      IRET22
        SUB     AL,20H          ; Lower-case changed to upper-case
IRET22:
        RETN

        ;IF IBM
;HEADER	db 0Dh,0Ah
	;db 0Dh,0Ah
	;db 'The IBM Personal Computer DOS',0Dh,0Ah
	;db 'Version 2.10 (C)Copyright IBM Corp 1981, 1982, 1983',0Dh,0Ah
	;db '$Licensed Material - Program Property of IBM/DEV/CON',0
        ;ENDIF

;ICONDEV LABEL BYTE
ICONDEV:
        DB      "/DEV/"
        DB      "CON",0,0,0,0,0,0          ; Room for 8 char device
BADCSPFL DB     0
COMSPECT DB     "/COMMAND.COM",0,0
AUTOBAT DB      0,":\AUTOEXEC.BAT",0

PRDATTM DB      -1                      ;Init not to prompt for date time
;INITADD DD     ?
INITADD DD      0
CHUCKENV DB     0
;ECOMLOC DW     OFFSET ENVIRONMENT:ECOMSPEC-10H
ECOMLOC	DW	ECOMSPEC-ENVIRONMENT ; 30/04/2018	

        ;IF      HIGHMEM
;REALRES DW      ?
;ENVIRSEGSAV DW  ?
        ;ENDIF

COMSPSTRING DB  "COMSPEC="

;INIT    ENDS

;        END

;=============================================================================
; UINIT.ASM
;=============================================================================

;TITLE   COMMAND Initialization messages

;INIT    SEGMENT PUBLIC BYTE

        ;PUBLIC  BADCOMLKMES

        ;PUBLIC  HEADER
        ;IF      NOT IBM
HEADER  DB      13,10,"Command v. 2.11"
        ;IF      HIGHMEM
        ;DB      "H"
        ;ENDIF
        DB      13,10,"$"
        ;ENDIF

        ;IF IBM
;HEADER DB      13,10,13,10,"TeleVideo Personal Computer DOS Vers. 2.11",13,10
        ;DB      "(C) Copyright TeleVideo Systems, Inc. 1983",13,10
	;DB	"(C) Copyright Microsoft Corp. 1981, 1982, 1983",13,10,"$"
        ;ENDIF

BADCOMLKMES DB  "Specified COMMAND search directory bad",13,10,"$"

	db  0 ; 01/05/2018 (15699+1 = 15700 bytes of COMMAND.COM size) 

;INIT    ENDS

;        END

; ----------------------------------------------------------------------------

;TAIL    SEGMENT PUBLIC PARA
;        ORG     0
;TRANSTART       LABEL   WORD
;TAIL    ENDS

;ALIGN 16

;TRANSTART:

; 21/04/2018 (Retro DOS v2.0 COMMAND)
;	times	128 db 0	

; 16/02/2018 (Retro DOS v2.0 COMMAND)
; ----------------------------------------------------------------------------
; SEGMENT - TRANSCODE
; ----------------------------------------------------------------------------

TRANGROUP: ; 21/04/2018

;=============================================================================
; TRANCODE.ASM
;=============================================================================

; 21/04/2018
; transcom.s (COMMAND.COM source file 2 of 2) code/data addresses 
; (these values must be changed when transcom.s source code is changed
; and data offsets are changed)

; 30/04/2018
; 29/04/2018

COMMAND      EQU  002CH + 100h
DATINIT	     EQU  1248H + 100h
HEADCALL     EQU  2BD8H + 100h
TRANSPACEEND EQU  2EF6H + 100h
TRANDATAEND  EQU  2A40h	+ 100h					

;TPA	EQU  2BDCH + 100h
;TRNLEN	EQU  0300H

; ----------------------------------------------------------------------------
;START OF TRANSIENT PORTION
;This code is loaded at the end of memory and may be overwritten by
;memory-intensive user programs.
; ----------------------------------------------------------------------------

TRANSTART EQU $-100h

COMTRANS:

INCBIN	"TRANSCOM.BIN"

;COMLEN	EQU $-COMTRANS ; End of COMMAND load.

; 29/04/2018
BSS_SIZE EQU TRANSPACEEND-TRANDATAEND	

TIMES BSS_SIZE db 0

COMLEN	EQU $-COMTRANS ; 30/04/2018

COMMANDCOMSIZE equ $ - 100h