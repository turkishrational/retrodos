; ****************************************************************************
; MSDOS.SYS (MSDOS 1.25 Kernel) - RETRO DOS v1.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 20/04/2018
; ----------------------------------------------------------------------------
; Beginning: 04/02/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm msdos.s -l msdos.lst -o MSDOS.BIN)) 	
; ----------------------------------------------------------------------------
; Derived from 'MSDOS.ASM' file of MSDOS 1.25 (IBM PCDOS v1.1) source code
; by Microsoft (by Tim Paterson), 03/03/1982
; ****************************************************************************

; 86-DOS  High-performance operating system for the 8086  version 1.25
;       by Tim Paterson

; ****************** Revision History *************************
;          >> EVERY change must noted below!! <<
;
; 0.34 12/29/80 General release, updating all past customers
; 0.42 02/25/81 32-byte directory entries added
; 0.56 03/23/81 Variable record and sector sizes
; 0.60 03/27/81 Ctrl-C exit changes, including register save on user stack
; 0.74 04/15/81 Recognize I/O devices with file names
; 0.75 04/17/81 Improve and correct buffer handling
; 0.76 04/23/81 Correct directory size when not 2^N entries
; 0.80 04/27/81 Add console input without echo, Functions 7 & 8
; 1.00 04/28/81 Renumber for general release
; 1.01 05/12/81 Fix bug in `STORE'
; 1.10 07/21/81 Fatal error trapping, NUL device, hidden files, date & time,
;               RENAME fix, general cleanup
; 1.11 09/03/81 Don't set CURRENT BLOCK to 0 on open; fix SET FILE SIZE
; 1.12 10/09/81 Zero high half of CURRENT BLOCK after all (CP/M programs don't)
; 1.13 10/29/81 Fix classic "no write-through" error in buffer handling
; 1.20 12/31/81 Add time to FCB; separate FAT from DPT; Kill SMALLDIR;
;               Add FLUSH and MAPDEV calls; allow disk mapping in DSKCHG;
;               Lots of smaller improvements
; 1.21 01/06/82 HIGHMEM switch to run DOS in high memory
; 1.22 01/12/82 Add VERIFY system call to enable/disable verify after write
; 1.23 02/11/82 Add defaulting to parser; use variable escape character
;               Don't zero extent field in IBM version (back to 1.01!)
; 1.24 03/01/82 Restore fcn. 27 to 1.0 level; add fcn. 28
; 1.25 03/03/82 Put marker (00) at end of directory to speed searches
;
; *************************************************************


; Interrupt Entry Points:

; INTBASE:      ABORT
; INTBASE+4:    COMMAND
; INTBASE+8:    BASE EXIT ADDRESS
; INTBASE+C:    CONTROL-C ABORT
; INTBASE+10H:  FATAL ERROR ABORT
; INTBASE+14H:  BIOS DISK READ
; INTBASE+18H:  BIOS DISK WRITE
; INTBASE+40H:  Long jump to CALL entry point

ESCCH   EQU     0
CANCEL  EQU     1BH             ;Cancel with ESC
NUMDEV  EQU     6               ;Include "COM1" as I/O device name

MAXCALL EQU     36
MAXCOM  EQU     46
INTBASE EQU     80H
INTTAB  EQU     20H
ENTRYPOINTSEG   EQU     0CH
ENTRYPOINT      EQU     INTBASE+40H
CONTC   EQU     INTTAB+3
EXIT    EQU     INTBASE+8
LONGJUMP EQU    0EAH
LONGCALL EQU    9AH
MAXDIF  EQU     0FFFH
SAVEXIT EQU     10

; Field definition for FCBs

struc FCBLOCK
        resb	12	;Drive code and name
.EXTENT: resw	1
.RECSIZ: resw	1	;Size of record (user settable)
.FILSIZ: resw	1	;Size of file in bytes
.DRVBP:	resw	1	;BP for SEARCH FIRST and SEARCH NEXT
.FDATE:	resw	1	;Date of last writing
.FTIME:	resw	1	;Time of last writing
.DEVID:	resb	1	;Device ID number, bits 0-5
                        ;bit 7=0 for file, bit 7=1 for I/O device
                        ;If file, bit 6=0 if dirty
                        ;If I/O device, bit 6=0 if EOF (input)
.FIRCLUS: resw	1       ;First cluster of file
.LSTCLUS: resw	1	;Last cluster accessed
.CLUSPOS: resw	1	;Position of last cluster accessed
	resb	1	;Forces NR to offset 32
.NR:	resb	1	;Next record
.RR:	resb	3	;Random record
endstruc

FCBLOCK.FILDIRENT equ FCBLOCK.FILSIZ
			;Used only by SEARCH FIRST and SEARCH NEXT

; Description of 32-byte directory entry (same as returned by SEARCH FIRST
; and SEARCH NEXT, functions 17 and 18).
;
; Location      bytes   Description
;
;    0          11      File name and extension ( 0E5H if empty)
;   11           1      Attributes. Bits 1 or 2 make file hidden
;   12          10      Zero field (for expansion)
;   22           2      Time. Bits 0-4=seconds/2, bits 5-10=minute, 11-15=hour
;   24           2      Date. Bits 0-4=day, bits 5-8=month, bits 9-15=year-1980
;   26           2      First allocation unit ( < 4080 )
;   28           4      File size, in bytes (LSB first, 30 bits max.)
;
; The File Allocation Table uses a 12-bit entry for each allocation unit on
; the disk. These entries are packed, two for every three bytes. The contents
; of entry number N is found by 1) multiplying N by 1.5; 2) adding the result
; to the base address of the Allocation Table; 3) fetching the 16-bit word at
; this address; 4) If N was odd (so that N*1.5 was not an integer), shift the
; word right four bits; 5) mask to 12 bits (AND with 0FFF hex). Entry number
; zero is used as an end-of-file trap in the OS and as a flag for directory
; entry size (if SMALLDIR selected). Entry 1 is reserved for future use. The
; first available allocation unit is assigned entry number two, and even
; though it is the first, is called cluster 2. Entries greater than 0FF8H are
; end of file marks; entries of zero are unallocated. Otherwise, the contents
; of a FAT entry is the number of the next cluster in the file.


; Field definition for Drive Parameter Block

struc	DPBLOCK
.DEVNUM: resb	1	;I/O driver number
.DRVNUM: resb	1	;Physical Unit number
.SECSIZ: resw	1	;Size of physical sector in bytes
.CLUSMSK: resb	1	;Sectors/cluster - 1
.CLUSSHFT: resb 1	;Log2 of sectors/cluster
.FIRFAT: resw	1	;Starting record of FATs
.FATCNT: resb	1	;Number of FATs for this drive
.MAXENT: resw	1	;Number of directory entries
.FIRREC: resw	1	;First sector of first cluster
.MAXCLUS: resw	1	;Number of clusters on drive + 1
.FATSIZ: resb	1	;Number of records occupied by FAT
.FIRDIR: resw	1	;Starting record of directory
.FAT:	resw	1	;Pointer to start of FAT
endstruc

DPBSIZ	EQU	20	;Size of the structure in bytes
DPBLOCK.DIRSEC	EQU DPBLOCK.FIRREC  ;Number of dir. sectors (init temporary)
DPBLOCK.DSKSIZ	EQU DPBLOCK.MAXCLUS ;Size of disk (temp used during init only)

;The following are all of the segments used
;They are declared in the order that they should be placed in the executable

; Location of user registers relative user stack pointer

struc STKPTRS
.AXSAVE:  resw	1
.BXSAVE:  resw	1
.CXSAVE:  resw	1
.DXSAVE:  resw	1
.SISAVE:  resw	1
.DISAVE:  resw	1
.BPSAVE:  resw	1
.DSSAVE:  resw	1
.ESSAVE:  resw	1
.IPSAVE:  resw	1
.CSSAVE:  resw	1
.FSAVE:   resw	1
endstruc

;-------------------------------------------------------------------------------------
BIOSSEG     EQU 60H 
;-------------------------------------------------------------------------------------

BIOSSTAT    EQU	03H
BIOSIN	    EQU	06H
BIOSOUT     EQU	09H
BIOSPRINT   EQU	0CH
BIOSAUXIN   EQU	0FH
BIOSAUXOUT  EQU	12H
BIOSREAD    EQU	15H
BIOSWRITE   EQU	18H
BIOSDSKCHG  EQU	1BH
BIOSSETDATE EQU 1EH
BIOSSETTIME EQU 21H
BIOSGETTIME EQU 24H
BIOSFLUSH   EQU	27H
BIOSMAPDEV  EQU	2AH

;-------------------------------------------------------------------------------------
; Start of code
;-------------------------------------------------------------------------------------

        [ORG 0]

CODSTRT EQU     $
        JMP     DOSINIT

ESCCHAR: DB	ESCCH   ;Lead-in character for escape sequences
ESCTAB: 
        DB      64      ;Crtl-Z - F6
        DB      77      ;Copy one char - -->
        DB      59      ;Copy one char - F1
        DB      83      ;Skip one char - DEL
        DB      60      ;Copy to char - F2
        DB      62      ;Skip to char - F4
        DB      61      ;Copy line - F3
        DB      61      ;Kill line (no change to template ) - Not used
        DB      63      ;Reedit line (new template) - F5
        DB      75      ;Backspace - <--
        DB      82      ;Enter insert mode - INS (toggle)
        DB      65      ;Escape character - F7
        DB      65      ;End of table

ESCTABLEN EQU   $-ESCTAB

HEADER:
	DB      13,10,"MS-DOS version 1.25"
	DB      13,10
        DB      "Copyright 1981,82 Microsoft, Inc.",13,10,"$"
QUIT:
        MOV     AH,0
        JMP     SHORT SAVREGS

COMMAND: ;Interrupt call entry point
        CMP     AH,MAXCOM
        JBE     SHORT SAVREGS
	
	;
	; Retro DOS v1.0 - 11/02/2018
	CMP	AH, 0FFh
	JB	SHORT BADCALL

	PUSHA
	MOV     SI,RETRODOSMSG
        CALL    OUTMES
	POPA
	;
		
BADCALL:
        MOV     AL,0
IRET:   IRET

ENTRY:  ;System call entry point and dispatcher
        POP     AX              ;IP from the long call at 5
        POP     AX              ;Segment from the long call at 5
        POP     WORD [CS:TEMP]	;IP from the CALL 5
        PUSHF                   ;Start re-ordering the stack
        CLI
        PUSH    AX              ;Save segment
        PUSH    WORD [CS:TEMP]	;Stack now ordered as if INT had been used
        CMP     CL,MAXCALL      ;This entry point doesn't get as many calls
        JA      SHORT BADCALL
        MOV     AH,CL
SAVREGS:
        PUSH    ES
        PUSH    DS
        PUSH    BP
        PUSH    DI
        PUSH    SI
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX

        MOV     [CS:SPSAVE],SP
        MOV     [CS:SSSAVE],SS
        MOV     SP,CS
        MOV     SS,SP
REDISP:
        MOV     SP, IOSTACK
        STI                     ;Stack OK now
        MOV     BL,AH
        MOV     BH,0
        SHL     BX,1
        CLD
        CMP     AH,12
        JLE     SHORT SAMSTK
        MOV     SP,DSKSTACK
SAMSTK:
        CALL    [CS:BX+DISPATCH]
LEAVE:
        CLI
        MOV     SP,[CS:SPSAVE]
        MOV     SS,[CS:SSSAVE]
        MOV     BP,SP
        MOV     [BP+STKPTRS.AXSAVE],AL

        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES
        IRET

; Standard Functions
DISPATCH:
	DW	ABORT           ;0
        DW	CONIN
        DW	CONOUT
        DW	READER
        DW	PUNCH
        DW	LIST            ;5
        DW	RAWIO
        DW	RAWINP
        DW	IN
        DW	PRTBUF
        DW	BUFIN           ;10
        DW	CONSTAT
        DW	FLUSHKB
        DW	DSKRESET
        DW	SELDSK
        DW	OPEN            ;15
        DW	CLOSE
        DW	SRCHFRST
        DW	SRCHNXT
        DW	DELETE
        DW	SEQRD           ;20
        DW	SEQWRT
        DW	CREATE
        DW	RENAME
        DW	INUSE
        DW	GETDRV          ;25
        DW	SETDMA
        DW	GETFATPT
        DW	GETFATPTDL
        DW	GETRDONLY
        DW	SETATTRIB       ;30
        DW	GETDSKPT
        DW	USERCODE
        DW	RNDRD
        DW	RNDWRT
        DW	FILESIZE        ;35
        DW	SETRNDREC
; Extended Functions
        DW	SETVECT
        DW	NEWBASE
        DW	BLKRD
        DW	BLKWRT          ;40
        DW	MAKEFCB
        DW	GETDATE
        DW	SETDATE
        DW	GETTIME
        DW	SETTIME         ;45
        DW	VERIFY

INUSE:
GETIO:
SETIO:
GETRDONLY:
SETATTRIB:
USERCODE:
        MOV     AL,0
        RET

VERIFY:
        AND     AL,1
        MOV     [CS:VERFLG],AL
        RETN

FLUSHKB:
        PUSH    AX
        CALL    BIOSSEG:BIOSFLUSH
        POP     AX
        MOV     AH,AL
        CMP     AL,1
        JZ      SHORT REDISPJ
        CMP     AL,6
        JZ      SHORT REDISPJ
        CMP     AL,7
        JZ      SHORT REDISPJ
        CMP     AL,8
        JZ      SHORT REDISPJ
        CMP     AL,10
        JZ      SHORT REDISPJ
        MOV     AL,0
        RETN

REDISPJ:
	JMP     REDISP

READER:
AUXIN:
        CALL    STATCHK
        CALL    BIOSSEG:BIOSAUXIN 
        RETN

PUNCH:
        MOV     AL,DL
AUXOUT:
        PUSH    AX
        CALL    STATCHK
        POP     AX
        CALL    BIOSSEG:BIOSAUXOUT
        RETN


UNPACK:

; Inputs:
;       DS = CS
;       BX = Cluster number
;       BP = Base of drive parameters
;       SI = Pointer to drive FAT
; Outputs:
;       DI = Contents of FAT for given cluster
;       Zero set means DI=0 (free cluster)
; No other registers affected. Fatal error if cluster too big.

        CMP     BX,[BP+DPBLOCK.MAXCLUS]
        JA      SHORT HURTFAT
        LEA     DI,[SI+BX]
        SHR     BX,1
        MOV     DI,[DI+BX]
        JNC     SHORT HAVCLUS
        SHR     DI,1
        SHR     DI,1
        SHR     DI,1
        SHR     DI,1
        STC
HAVCLUS:
        RCL     BX,1
        AND     DI,0FFFH
        RETN
HURTFAT:
        PUSH    AX
        MOV     AH,80H          ;Signal Bad FAT to INT 24H handler
        MOV     DI,0FFFH        ;In case INT 24H returns (it shouldn't)
        CALL    FATAL
        POP     AX              ;Try to ignore bad FAT
        RETN

PACK:

; Inputs:
;       DS = CS
;       BX = Cluster number
;       DX = Data
;       SI = Pointer to drive FAT
; Outputs:
;       The data is stored in the FAT at the given cluster.
;       BX,DX,DI all destroyed
;       No other registers affected

        MOV     DI,BX
        SHR     BX,1
        ADD     BX,SI
        ADD     BX,DI
        SHR     DI,1
        MOV     DI,[BX]
        JNC     ALIGNED
        SHL     DX,1
        SHL     DX,1
        SHL     DX,1
        SHL     DX,1
        AND     DI,0FH
        JMP     SHORT PACKIN
ALIGNED:
        AND     DI,0F000H
PACKIN:
        OR      DI,DX
        MOV     [BX],DI
        RETN

DEVNAME:
        MOV     SI,IONAME		;List of I/O devices with file names
        MOV     BH,NUMDEV		;BH = number of device names
LOOKIO:
        MOV     DI,NAME1
        MOV     CX,4                    ;All devices are 4 letters
        REPE    CMPSB                   ;Check for name in list
        JZ      SHORT IOCHK		;If first 3 letters OK, check for the rest
        ADD     SI,CX                   ;Point to next device name
        DEC     BH
        JNZ     SHORT LOOKIO
CRET:
        STC                             ;Not found
        RETN

IOCHK:
        CMP     BH,NUMDEV       ;Is it the first device?
        JNZ     SHORT NOTCOM1
        MOV     BH,2            ;Make it the same as AUX
NOTCOM1:
        NEG     BH
        MOV     CX,2            ;Check rest of name but not extension
        MOV     AX,2020H
        REPE    SCASW           ;Make sure rest of name is blanks
        JNZ     SHORT CRET
RET1:   RETN			;Zero set so CREATE works

GETFILE:
; Same as GETNAME except ES:DI points to FCB on successful return
        CALL    MOVNAME
        JC      SHORT RET1
        PUSH    DX
        PUSH    DS
        CALL    FINDNAME
        POP     ES
        POP     DI
RET2:   RETN


GETNAME:

; Inputs:
;       DS,DX point to FCB
; Function:
;       Find file name in disk directory. First byte is
;       drive number (0=current disk). "?" matches any
;       character.
; Outputs:
;       Carry set if file not found
;       ELSE
;       Zero set if attributes match (always except when creating)
;       BP = Base of drive parameters
;       DS = CS
;       ES = CS
;       BX = Pointer into directory buffer
;       SI = Pointer to First Cluster field in directory entry
;       [DIRBUF] has directory record with match
;       [NAME1] has file name
; All other registers destroyed.

        CALL    MOVNAME
        JC      SHORT RET2		;Bad file name?
FINDNAME:
        MOV     AX,CS
        MOV     DS,AX
        CALL    DEVNAME
        JNC     SHORT RET2
        CALL    STARTSRCH
CONTSRCH:
        CALL    GETENTRY
        JC      SHORT RET2
SRCH:
        MOV     AH,[BX]
        OR      AH,AH                   ;End of directory?
        JZ      SHORT FREE
        CMP     AH,[DELALL]             ;Free entry?
        JZ      SHORT FREE
        MOV     SI,BX
        MOV     DI,NAME1
        MOV     CX,11
WILDCRD:
        REPE    CMPSB
        JZ      SHORT FOUND
        CMP     BYTE [DI-1],"?"
        JZ      SHORT WILDCRD
NEXTENT:
        CALL    NEXTENTRY
        JNC     SHORT SRCH
RET3:   RETN

FREE:
        CMP     WORD [ENTFREE],-1	;Found a free entry before?
        JNZ     SHORT TSTALL		;If so, ignore this one
        MOV     CX,[LASTENT]
        MOV     [ENTFREE],CX
TSTALL:
        CMP     AH,[DELALL]             ;At end of directory?
        JZ      SHORT NEXTENT		;No - continue search
        STC                             ;Report not found
        RETN
 
FOUND:
;Check if attributes allow finding it
        MOV     AH,[ATTRIB]             ;Attributes of search
        NOT     AH
        AND     AH,[SI]                 ;Compare with attributes of file
        ADD     SI,15   
        AND     AH,6                    ;Only look at bits 1 and 2
        JZ      SHORT RET3
        TEST    BYTE [CREATING],-1	;Pass back mismatch if creating
        JZ      SHORT NEXTENT		;Otherwise continue searching
        RETN


GETENTRY:

; Inputs:
;       [LASTENT] has previously searched directory entry
; Function:
;       Locates next sequential directory entry in preparation for search
; Outputs:
;       Carry set if none
;       ELSE
;       AL = Current directory block
;       BX = Pointer to next directory entry in [DIRBUF]
;       DX = Pointer to first byte after end of DIRBUF
;       [LASTENT] = New directory entry number

        MOV     AX,[LASTENT]
        INC     AX                      ;Start with next entry
        CMP     AX,[BP+DPBLOCK.MAXENT]
        JAE     SHORT NONE
GETENT:
        MOV     [LASTENT],AX
        MOV     CL,4
        SHL     AX,CL
        XOR     DX,DX
        SHL     AX,1
        RCL     DX,1                    ;Account for overflow in last shift
        MOV     BX,[BP+DPBLOCK.SECSIZ]
        AND     BL,255-31               ;Must be multiple of 32
        DIV     BX
        MOV     BX,DX                   ;Position within sector
        MOV     AH,[BP+DPBLOCK.DEVNUM]	;AL=Directory sector no.
        CMP     AX,[DIRBUFID]
        JZ      SHORT HAVDIRBUF
        PUSH    BX
        CALL    DIRREAD
        POP     BX
HAVDIRBUF:
        MOV     DX,DIRBUF
        ADD     BX,DX
        ADD     DX,[BP+DPBLOCK.SECSIZ]
        RETN

NEXTENTRY:

; Inputs:
;       Same as outputs of GETENTRY, above
; Function:
;       Update AL, BX, and [LASTENT] for next directory entry.
;       Carry set if no more.

        MOV     DI,[LASTENT]
        INC     DI
        CMP     DI,[BP+DPBLOCK.MAXENT]
        JAE     SHORT NONE
        MOV     [LASTENT],DI
        ADD     BX,32
        CMP     BX,DX
        JB      SHORT HAVIT
        INC     AL                      ;Next directory sector
        PUSH    DX                      ;Save limit
        CALL    DIRREAD
        POP     DX
        MOV     BX,DIRBUF
HAVIT:
        CLC
        RETN

NONE:
        CALL    CHKDIRWRITE
        STC
RET4:   RETN


DELETE: ; System call 19
        CALL    MOVNAME
        MOV     AL,-1
        JC      SHORT RET4
        MOV     AL,[CS:ATTRIB]
        AND     AL,6                    ;Look only at hidden bits
        CMP     AL,6                    ;Both must be set
        JNZ     SHORT NOTALL
        MOV     CX,11
        MOV     AL,"?"
        MOV     DI,NAME1
        REPE    SCASB                   ;See if name is *.*
        JNZ     SHORT NOTALL
        MOV     BYTE [CS:DELALL],0	;DEL *.* - flag deleting all
NOTALL:
        CALL    FINDNAME
        MOV     AL,-1
        JC      SHORT RET4
        OR      BH,BH			;Check if device name
        JS      SHORT RET4		;Can't delete I/O devices
DELFILE:
        MOV     BYTE [DIRTYDIR],-1
        MOV     AH,[DELALL]
        MOV     [BX],AH
        MOV     BX,[SI]
        MOV     SI,[BP+DPBLOCK.FAT]
        OR      BX,BX
        JZ      SHORT DELNXT
        CMP     BX,[BP+DPBLOCK.MAXCLUS]
        JA      SHORT DELNXT
        CALL    RELEASE
DELNXT:
        CALL    CONTSRCH
        JNC     SHORT DELFILE
        CALL    FATWRT
        CALL    CHKDIRWRITE
        XOR     AL,AL
        RETN

RENAME: ;System call 23
        CALL    MOVNAME
        JC      SHORT ERRET
        ADD     SI,5
        MOV     DI,NAME2
        CALL    LODNAME
        JC      SHORT ERRET	;Report error if second name invalid
        CALL    FINDNAME
        JC      SHORT ERRET
        OR      BH,BH           ;Check if I/O device name
        JS      SHORT ERRET	;If so, can't rename it
        MOV     SI,NAME1
        MOV     DI,NAME3
        MOV     CX,6            ;6 words (12 bytes)--include attribute byte
        REP     MOVSW           ;Copy name to search for
RENFIL:
        MOV     DI,NAME1
        MOV     SI,NAME2
        MOV     CX,11
NEWNAM:
        LODSB
        CMP     AL,"?"
        JNZ	SHORT NOCHG
        MOV     AL,[BX]
NOCHG:
        STOSB
        INC     BX
        LOOP    NEWNAM
        MOV     BYTE [DI],6	;Stop duplicates with any attributes
        CALL    DEVNAME         ;Check if giving it a device name
        JNC     SHORT RENERR
        PUSH    WORD [LASTENT]	;Save position of match
        MOV     WORD [LASTENT],-1  ;Search entire directory for duplicate
        CALL    CONTSRCH        ;See if new name already exists
        POP     AX
        JNC     SHORT RENERR	;Error if found
        CALL    GETENT		;Re-read matching entry
        MOV     DI,BX
        MOV     SI,NAME1
        MOV     CX,5
        MOVSB
        REP     MOVSW		;Replace old name with new one
        MOV     BYTE [DIRTYDIR],-1 ;Flag change in directory
        MOV     SI,NAME3
        MOV     DI,NAME1
        MOV     CX,6                    ;Include attribute byte
        REP     MOVSW                   ;Copy name back into search buffer
        CALL    CONTSRCH
        JNC     SHORT RENFIL
        CALL    CHKDIRWRITE
        XOR     AL,AL
        RETN

RENERR:
        CALL    CHKDIRWRITE
ERRET:
        MOV     AL,-1
RET5:   RETN


MOVNAME:

; Inputs:
;       DS, DX point to FCB or extended FCB
; Outputs:
;       DS:DX point to normal FCB
;       ES = CS
;       If file name OK:
;       BP has base of driver parameters
;       [NAME1] has name in upper case
; All registers except DX destroyed
; Carry set if bad file name or drive

        MOV     WORD [CS:CREATING],0E500H ;Not creating, not DEL *.*
        MOV     AX,CS
        MOV     ES,AX
        MOV     DI,NAME1
        MOV     SI,DX
        LODSB
        MOV     [CS:EXTFCB],AL  ;Set flag if extended FCB in use
        MOV     AH,0            ;Set default attributes
        CMP     AL,-1           ;Is it an extended FCB?
        JNZ     SHORT HAVATTRB
        ADD     DX,7            ;Adjust to point to normal FCB
        ADD     SI,6            ;Point to drive select byte
        MOV     AH,[SI-1]       ;Get attribute byte
        LODSB			;Get drive select byte
HAVATTRB:
        MOV     [CS:ATTRIB],AH	;Save attributes
        CALL    GETTHISDRV
LODNAME:
; This entry point copies a file name from DS,SI
; to ES,DI converting to upper case.
        CMP     BYTE [SI]," "	;Don't allow blank as first letter
        STC                     ;In case of error
        JZ      SHORT RET5
        MOV     CX,11
MOVCHK:
        CALL    GETLET
        JB      SHORT RET5
        JNZ     STOLET          ;Is it a delimiter?
        CMP     AL," "          ;This is the only delimiter allowed
        STC                     ;In case of error
        JNZ     SHORT RET5
STOLET:
        STOSB
        LOOP    MOVCHK
        CLC                     ;Got through whole name - no error
RET6:   RETN

GETTHISDRV:
        CMP     [CS:NUMDRV],AL
        JC      SHORT RET6
        DEC     AL
        JNS     SHORT PHYDRV
        MOV     AL,[CS:CURDRV]
PHYDRV:
        MOV     [CS:THISDRV],AL
        RETN
        

OPEN:   ;System call 15
        CALL    GETFILE
DOOPEN:
; Enter here to perform OPEN on file already found
; in directory. DS=CS, BX points to directory
; entry in DIRBUF, SI points to First Cluster field, and
; ES:DI point to the FCB to be opened. This entry point
; is used by CREATE.
        JC      SHORT ERRET
        OR      BH,BH           ;Check if file is I/O device
        JS      SHORT OPENDEV	;Special handler if so
        MOV     AL,[THISDRV]
        INC     AX
        STOSB
        XOR     AX,AX
        ADD     DI,11
        STOSW                   ;Zero low byte of extent field if IBM only
        MOV     AL,128          ;Default record size
        STOSW                   ;Set record size
        LODSW                   ;Get starting cluster
        MOV     DX,AX           ;Save it for the moment
        MOVSW                   ;Transfer size to FCB
        MOVSW
        MOV     AX,[SI-8]       ;Get date
        STOSW                   ;Save date in FCB
        MOV     AX,[SI-10]      ;Get time
        STOSW                   ;Save it in FCB
        MOV     AL,[BP+DPBLOCK.DEVNUM]
        OR      AL,40H
        STOSB
        MOV     AX,DX           ;Restore starting cluster
        STOSW                   ; first cluster
        STOSW                   ; last cluster accessed
        XOR     AX,AX
        STOSW                   ; position of last cluster
        RETN

OPENDEV:
        ADD     DI,13           ;point to 2nd half of extent field
        XOR     AX,AX
        STOSB                   ;Set it to zero
        MOV     AL,128
        STOSW                   ;Set record size to 128
        XOR     AX,AX
        STOSW
        STOSW                   ;Set current size to zero
        CALL    DATE16
        STOSW                   ;Date is todays
        XCHG    AX,DX
        STOSW                   ;Use current time
        MOV     AL,BH           ;Get device number
        STOSB
        XOR     AL,AL           ;No error
        RETN
FATERR:
        XCHG    AX,DI           ;Put error code in DI
        MOV     AH,2            ;While trying to read FAT
        MOV     AL,[THISDRV]    ;Tell which drive
        CALL    FATAL1
        JMP     SHORT FATREAD
STARTSRCH:
        MOV     AX,-1
        MOV     [LASTENT],AX
        MOV     [ENTFREE],AX

FATREAD:

; Inputs:
;       DS = CS
; Function:
;       If disk may have been changed, FAT is read in and buffers are
;       flagged invalid. If not, no action is taken.
; Outputs:
;       BP = Base of drive parameters
;       Carry set if invalid drive returned by MAPDEV
; All other registers destroyed

        MOV     AL,[THISDRV]
        XOR     AH,AH		;Set default response to zero & clear carry
        CALL    BIOSSEG:BIOSDSKCHG ;See what BIOS has to say
        JC      SHORT FATERR
        CALL    GETBP
        MOV     AL,[THISDRV]    ;Use physical unit number
        MOV     SI,[BP+DPBLOCK.FAT]
        OR      AH,[SI-1]       ;Dirty byte for FAT
        JS      SHORT NEWDSK    ;If either say new disk, then it's so
        JNZ     SHORT MAPDRV
        MOV     AH,1
        CMP     AX,[BUFDRVNO]  ;Does buffer have dirty sector of this drive?
        JZ      SHORT MAPDRV
NEWDSK:
        CMP     AL,[BUFDRVNO]   ;See if buffer is for this drive
        JNZ     SHORT BUFOK	;If not, don't touch it
        MOV     WORD [BUFSECNO], 0  ;Flag buffers invalid
        MOV     WORD [BUFDRVNO],00FFH
BUFOK:
        MOV     WORD [DIRBUFID],-1
        CALL    FIGFAT
NEXTFAT:
        PUSH    AX
        CALL    DSKREAD
        POP     AX
        JC      SHORT BADFAT
        SUB     AL,[BP+DPBLOCK.FATCNT]
        JZ      SHORT NEWFAT
        CALL    FATWRT
NEWFAT:
        MOV     SI,[BP+DPBLOCK.FAT]
        MOV     AL,[BP+DPBLOCK.DEVNUM]
        MOV     AH,[SI]         ;Get first byte of FAT
        OR      AH,0F8H         ;Put in range
        CALL    BIOSSEG:BIOSMAPDEV
        MOV     AH,0
        MOV     [SI-2],AX       ;Set device no. and reset dirty bit
MAPDRV:
        MOV     AL,[SI-2]       ;Get device number
GETBP:
        MOV     BP,[DRVTAB]     ;Just in case drive isn't valid
        AND     AL,3FH          ;Mask out dirty bit
        CMP     AL,[NUMIO]
        CMC
        JC      SHORT RET7
        PUSH    AX
        MOV     AH,DPBSIZ
        MUL     AH
        ADD     BP,AX
        POP     AX
RET7:   
	RETN

BADFAT:
        MOV     CX,DI
        ADD     DX,CX
        DEC     AL
        JNZ     NEXTFAT
        CALL    FIGFAT                          ;Reset registers
        CALL    DREAD                           ;Try first FAT once more
        JMP     SHORT NEWFAT

OKRET1:
        MOV     AL,0
        RETN

CLOSE:  ;System call 16
        MOV     DI,DX
        CMP     BYTE [DI],-1			;Check for extended FCB
        JNZ     NORMFCB3
        ADD     DI,7
NORMFCB3:
        TEST    BYTE [DI+FCBLOCK.DEVID],0C0H	;Allow only dirty files
        JNZ     SHORT OKRET1			;can't close if I/O device, or not writen
        MOV     AL,[DI]                         ;Get physical unit number
        DEC     AL                              ;Make zero = drive A
        MOV     AH,1                            ;Look for dirty buffer
        CMP     AX,[CS:BUFDRVNO]
        JNZ     SHORT FNDDIR
;Write back dirty buffer if on same drive
        PUSH    DX
        PUSH    DS
        PUSH    CS
        POP     DS
        MOV     BYTE [DIRTYBUF],0
        MOV     BX,[BUFFER]
        MOV     CX,1
        MOV     DX,[BUFSECNO]
        MOV     BP,[BUFDRVBP]
        CALL    DWRITE
        POP     DS
        POP     DX
FNDDIR:
        CALL    GETFILE
BADCLOSEJ:
        JC      SHORT BADCLOSE
        MOV     CX,[ES:DI+FCBLOCK.FIRCLUS]
        MOV     [SI],CX
        MOV     DX,[ES:DI+FCBLOCK.FILSIZ]
        MOV     [SI+2],DX
        MOV     DX,[ES:DI+FCBLOCK.FILSIZ+2]
        MOV     [SI+4],DX
        MOV     DX,[ES:DI+FCBLOCK.FDATE]
        MOV     [SI-2],DX
        MOV     DX,[ES:DI+FCBLOCK.FTIME]
        MOV     [SI-4],DX
        CALL    DIRWRITE

CHKFATWRT:
; Do FATWRT only if FAT is dirty and uses same I/O driver
        MOV     SI,[BP+DPBLOCK.FAT]
        MOV     AL,[BP+DPBLOCK.DEVNUM]
        MOV     AH,1
        CMP     [SI-2],AX       ;See if FAT dirty and uses same driver
        JNZ     SHORT OKRET

FATWRT:

; Inputs:
;       DS = CS
;       BP = Base of drive parameter table
; Function:
;       Write the FAT back to disk and reset FAT
;       dirty bit.
; Outputs:
;       AL = 0
;       BP unchanged
; All other registers destroyed

        CALL    FIGFAT
        MOV     BYTE [BX-1],0
EACHFAT:
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX
        CALL    DWRITE
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        ADD     DX,CX
        DEC     AL
        JNZ     SHORT EACHFAT
OKRET:
        MOV     AL,0
        RETN

BADCLOSE:
        MOV     SI,[BP+DPBLOCK.FAT]
        MOV     BYTE [SI-1],0
        MOV     AL,-1
        RETN


FIGFAT:
; Loads registers with values needed to read or
; write a FAT.
        MOV     AL,[BP+DPBLOCK.FATCNT]
        MOV     BX,[BP+DPBLOCK.FAT]
        MOV     CL,[BP+DPBLOCK.FATSIZ]  ;No. of records occupied by FAT
        MOV     CH,0
        MOV     DX,[BP+DPBLOCK.FIRFAT]  ;Record number of start of FATs
        RETN


DIRCOMP:
; Prepare registers for directory read or write
        CBW
        ADD     AX,[BP+DPBLOCK.FIRDIR]
        MOV     DX,AX
        MOV     BX,DIRBUF
        MOV     CX,1
        RETN


CREATE: ;System call 22
        CALL    MOVNAME
        JC      SHORT ERRET3
        MOV     DI,NAME1
        MOV     CX,11
        MOV     AL,"?"
        REPNE   SCASB
        JZ      SHORT ERRET3
        MOV     BYTE [CS:CREATING],-1
        PUSH    DX
        PUSH    DS
        CALL    FINDNAME
        JNC     SHORT EXISTENT
        MOV     AX,[ENTFREE]    ;First free entry found in FINDNAME
        CMP     AX,-1
        JZ      SHORT ERRPOP
        CALL    GETENT          ;Point at that free entry
        JMP     SHORT FREESPOT
ERRPOP:
        POP     DS
        POP     DX
ERRET3:
        MOV     AL,-1
        RETN

EXISTENT:
        JNZ     SHORT ERRPOP	;Error if attributes don't match
        OR      BH,BH           ;Check if file is I/O device
        JS      SHORT OPENJMP	;If so, no action
        MOV     CX,[SI]         ;Get pointer to clusters
        JCXZ    FREESPOT
        CMP     CX,[BP+DPBLOCK.MAXCLUS]
        JA      SHORT FREESPOT
        PUSH    BX
        MOV     BX,CX
        MOV     SI,[BP+DPBLOCK.FAT]
        CALL    RELEASE         ;Free any data already allocated
        CALL    FATWRT
        POP     BX
FREESPOT:
        MOV     DI,BX
        MOV     SI,NAME1
        MOV     CX,5
        MOVSB
        REP     MOVSW
        MOV     AL,[ATTRIB]
        STOSB
        XOR     AX,AX
        MOV     CL,5
        REP     STOSW
        CALL    DATE16
        XCHG    AX,DX
        STOSW
        XCHG    AX,DX
        STOSW
        XOR     AX,AX
        PUSH    DI
        MOV     CL,6
SMALLENT:
        REP     STOSB
        PUSH    BX
        CALL    DIRWRITE
        POP     BX
        POP     SI
OPENJMP:
        CLC                     ;Clear carry so OPEN won't fail
        POP     ES
        POP     DI
        JMP     DOOPEN

DIRREAD:

; Inputs:
;       DS = CS
;       AL = Directory block number
;       BP = Base of drive parameters
; Function:
;       Read the directory block into DIRBUF.
; Outputs:
;       AX,BP unchanged
; All other registers destroyed.

        PUSH    AX
        CALL    CHKDIRWRITE
        POP     AX
        PUSH    AX
        MOV     AH,[BP+DPBLOCK.DEVNUM]
        MOV     [DIRBUFID],AX
        CALL    DIRCOMP
        CALL    DREAD
        POP     AX
RET8:   RETN


DREAD:

; Inputs:
;       BX,DS = Transfer address
;       CX = Number of sectors
;       DX = Absolute record number
;       BP = Base of drive parameters
; Function:
;       Calls BIOS to perform disk read. If BIOS reports
;       errors, will call HARDERR for further action.
; BP preserved. All other registers destroyed.

        CALL    DSKREAD
        JNC     SHORT RET8
        MOV     BYTE [CS:READOP],0
        CALL    HARDERR
        CMP     AL,1            ;Check for retry
        JZ      SHORT DREAD
        RETN                    ;Ignore otherwise


HARDERR:

;Hard disk error handler. Entry conditions:
;       DS:BX = Original disk transfer address
;       DX = Original logical sector number
;       CX = Number of sectors to go (first one gave the error)
;       AX = Hardware error code
;       DI = Original sector transfer count
;       BP = Base of drive parameters
;       [READOP] = 0 for read, 1 for write

        XCHG    AX,DI           ;Error code in DI, count in AX
        SUB     AX,CX           ;Number of sectors successfully transferred
        ADD     DX,AX           ;First sector number to retry
        PUSH    DX
        MUL     WORD [BP+DPBLOCK.SECSIZ] ;Number of bytes transferred
        POP     DX
        ADD     BX,AX           ;First address for retry
        MOV     AH,0            ;Flag disk section in error
        CMP     DX,[BP+DPBLOCK.FIRFAT]  ;In reserved area?
        JB      SHORT ERRINT
        INC     AH              ;Flag for FAT
        CMP     DX,[BP+DPBLOCK.FIRDIR]  ;In FAT?
        JB      SHORT ERRINT
        INC     AH
        CMP     DX,[BP+DPBLOCK.FIRREC]  ;In directory?
        JB      SHORT ERRINT
        INC     AH              ;Must be in data area
ERRINT:
        SHL     AH,1            ;Make room for read/write bit
        OR      AH,[CS:READOP]
FATAL:
        MOV     AL,[BP+DPBLOCK.DRVNUM]  ;Get drive number
FATAL1:
        PUSH    BP              ;The only thing we preserve
        MOV     [CS:CONTSTK],SP
        CLI                     ;Prepare to play with stack
        MOV     SS,[CS:SSSAVE]
        MOV     SP,[CS:SPSAVE]  ;User stack pointer restored
        INT     24H             ;Fatal error interrupt vector
        MOV     [CS:SPSAVE],SP
        MOV     [CS:SSSAVE],SS
        MOV     SP,CS
        MOV     SS,SP
        MOV     SP,[CS:CONTSTK]
        STI
        POP     BP
        CMP     AL,2
        JZ	SHORT ERROR
        RETN

DSKREAD:
        MOV     AL,[BP+DPBLOCK.DEVNUM]
        PUSH    BP
        PUSH    BX
        PUSH    CX
        PUSH    DX
        CALL    BIOSSEG:BIOSREAD
        POP     DX
        POP     DI
        POP     BX
        POP     BP
RET9:   RETN


CHKDIRWRITE:
        TEST    BYTE [DIRTYDIR],-1
        JZ      SHORT RET9

DIRWRITE:

; Inputs:
;       DS = CS
;       AL = Directory block number
;       BP = Base of drive parameters
; Function:
;       Write the directory block into DIRBUF.
; Outputs:
;       BP unchanged
; All other registers destroyed.

        MOV     BYTE [DIRTYDIR],0
        MOV     AL,BYTE [DIRBUFID]
        CALL    DIRCOMP

DWRITE:

; Inputs:
;       BX,DS = Transfer address
;       CX = Number of sectors
;       DX = Absolute record number
;       BP = Base of drive parameters
; Function:
;       Calls BIOS to perform disk write. If BIOS reports
;       errors, will call HARDERR for further action.
; BP preserved. All other registers destroyed.

        MOV     AL,[BP+DPBLOCK.DEVNUM]
        MOV     AH,[CS:VERFLG]
        PUSH    BP
        PUSH    BX
        PUSH    CX
        PUSH    DX
        CALL    BIOSSEG:BIOSWRITE
        POP     DX
        POP     DI
        POP     BX
        POP     BP
        JNC     SHORT RET9
        MOV     BYTE [CS:READOP],1
        CALL    HARDERR
        CMP     AL,1            ;Check for retry
        JZ      SHORT DWRITE
        RETN

ABORT:
        LDS     SI,[CS:SPSAVE]
        MOV     DS,[SI+STKPTRS.CSSAVE]
        XOR     AX,AX
        MOV     ES,AX
        MOV     SI,SAVEXIT
        MOV     DI,EXIT
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
ERROR:
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        CALL    WRTFATS
        XOR     AX,AX
        CLI
        MOV     SS,[SSSAVE]
        MOV     SP,[SPSAVE]
        MOV     DS,AX
        MOV     SI,EXIT
        MOV     DI,EXITHOLD
        MOVSW
        MOVSW
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES
        STI             ;Stack OK now
        JMP     FAR [CS:EXITHOLD]


SEQRD:  ;System call 20
        CALL    GETREC
        CALL    LOAD
        JMP     SHORT FINSEQ

SEQWRT: ;System call 21
        CALL    GETREC
        CALL    STORE
FINSEQ:
        JCXZ    SETNREX
        ADD     AX,1
        ADC     DX,0
        JMP     SHORT SETNREX

RNDRD:  ;System call 33
        CALL    GETRRPOS1
        CALL    LOAD
        JMP     SHORT FINRND

RNDWRT: ;System call 34
        CALL    GETRRPOS1
        CALL    STORE
        JMP     SHORT FINRND

BLKRD:  ;System call 39
        CALL    GETRRPOS
        CALL    LOAD
        JMP     SHORT FINBLK

BLKWRT: ;System call 40
        CALL    GETRRPOS
        CALL    STORE
FINBLK:
        LDS     SI,[SPSAVE]
        MOV     [SI+STKPTRS.CXSAVE],CX
        JCXZ    FINRND
        ADD     AX,1
        ADC     DX,0
FINRND:
        MOV     [ES:DI+FCBLOCK.RR],AX
        MOV     [ES:DI+FCBLOCK.RR+2],DL
        OR      DH,DH
        JZ      SHORT SETNREX
        MOV     [ES:DI+FCBLOCK.RR+3],DH 
				;Save 4 byte of RECPOS only if significant
SETNREX:
        MOV     CX,AX
        AND     AL,7FH
        MOV     [ES:DI+FCBLOCK.NR],AL
        AND     CL,80H
        SHL     CX,1
        RCL     DX,1
        MOV     AL,CH
        MOV     AH,DL
        MOV     [ES:DI+FCBLOCK.EXTENT],AX
        MOV     AL,[CS:DSKERR]
        RETN

GETRRPOS1:
        MOV     CX,1
GETRRPOS:
        MOV     DI,DX
        CMP     BYTE [DI],-1
        JNZ     SHORT NORMFCB1
        ADD     DI,7
NORMFCB1:
        MOV     AX,[DI+FCBLOCK.RR]
        MOV     DX,[DI+FCBLOCK.RR+2]
        RETN

NOFILERR:
        XOR     CX,CX
        MOV     BYTE [DSKERR],4
        POP     BX
        RETN

SETUP:

; Inputs:
;       DS:DI point to FCB
;       DX:AX = Record position in file of disk transfer
;       CX = Record count
; Outputs:
;       DS = CS
;       ES:DI point to FCB
;       BL = DEVID from FCB
;       CX = No. of bytes to transfer
;       BP = Base of drive parameters
;       SI = FAT pointer
;       [RECCNT] = Record count
;       [RECPOS] = Record position in file
;       [FCB] = DI
;       [NEXTADD] = Displacement of disk transfer within segment
;       [SECPOS] = Position of first sector
;       [BYTPOS] = Byte position in file
;       [BYTSECPOS] = Byte position in first sector
;       [CLUSNUM] = First cluster
;       [SECCLUSPOS] = Sector within first cluster
;       [DSKERR] = 0 (no errors yet)
;       [TRANS] = 0 (No transfers yet)
;       [THISDRV] = Physical drive unit number
; If SETUP detects no records will be transfered, it returns 1 level up 
; with CX = 0.

        PUSH    AX
        MOV     AL,[DI]
        DEC     AL
        MOV     [CS:THISDRV],AL
        MOV     AL,[DI+FCBLOCK.DEVID]
        MOV     SI,[DI+FCBLOCK.RECSIZ]
        OR      SI,SI
        JNZ     SHORT HAVRECSIZ
        MOV     SI,128
        MOV     [DI+FCBLOCK.RECSIZ],SI
HAVRECSIZ:
        PUSH    DS
        POP     ES              ;Set ES to DS
        PUSH    CS
        POP     DS              ;Set DS to CS
        OR      AL,AL           ;Is it a device?
        JNS     SHORT NOTDEVICE
        MOV     AL,0            ;Fake in drive 0 so we can get SP
NOTDEVICE:
        CALL    GETBP
        POP     AX
        JC      SHORT NOFILERR
        CMP     SI,64           ;Check if highest byte of RECPOS is significant
        JB      SHORT SMALREC
        MOV     DH,0            ;Ignore MSB if record >= 64 bytes
SMALREC:
        MOV     [RECCNT],CX
        MOV     [RECPOS],AX
        MOV     [RECPOS+2],DX
        MOV     [FCB],DI
        MOV     BX,[DMAADD]
        MOV     [NEXTADD],BX
        MOV     BYTE [DSKERR],0
        MOV     BYTE [TRANS],0
        MOV     BX,DX
        MUL     SI
        MOV     [BYTPOS],AX
        PUSH    DX
        MOV     AX,BX
        MUL     SI
        POP     BX
        ADD     AX,BX
        ADC     DX,0            ;Ripple carry
        JNZ     SHORT EOFERR
        MOV     [BYTPOS+2],AX
        MOV     DX,AX
        MOV     AX,[BYTPOS]
        MOV     BX,[BP+DPBLOCK.SECSIZ]
        CMP     DX,BX           ;See if divide will overflow
        JNC     SHORT EOFERR
        DIV     BX
        MOV     [SECPOS],AX
        MOV     [BYTSECPOS],DX
        MOV     DX,AX
        AND     AL,[BP+DPBLOCK.CLUSMSK]
        MOV     [SECCLUSPOS],AL
        MOV     AX,CX           ;Record count
        MOV     CL,[BP+DPBLOCK.CLUSSHFT]
        SHR     DX,CL
        MOV     [CLUSNUM],DX
        MUL     SI              ;Multiply by bytes per record
        MOV     CX,AX
        ADD     AX,[DMAADD]     ;See if it will fit in one segment
        ADC     DX,0
        JZ      SHORT OK	;Must be less than 64K
        MOV     AX,[DMAADD]
        NEG     AX              ;Amount of room left in segment
        JNZ     SHORT PARTSEG	;All 64K available?
        DEC     AX              ;If so, reduce by one
PARTSEG:
        XOR     DX,DX
        DIV     SI              ;How many records will fit?
        MOV     [RECCNT],AX
        MUL     SI              ;Translate that back into bytes
        MOV     BYTE [DSKERR],2	;Flag that trimming took place
        MOV     CX,AX
        JCXZ    NOROOM
OK:
        MOV     BL,[ES:DI+FCBLOCK.DEVID]
        MOV     SI,[BP+DPBLOCK.FAT]
        RETN

EOFERR:
        MOV     BYTE [DSKERR],1
        XOR     CX,CX
NOROOM:
        POP     BX              ;Kill return address
        RETN

BREAKDOWN:

;Inputs:
;       DS = CS
;       CX = Length of disk transfer in bytes
;       BP = Base of drive parameters
;       [BYTSECPOS] = Byte position witin first sector
;Outputs:
;       [BYTCNT1] = Bytes to transfer in first sector
;       [SECCNT] = No. of whole sectors to transfer
;       [BYTCNT2] = Bytes to transfer in last sector
;AX, BX, DX destroyed. No other registers affected.

        MOV     AX,[BYTSECPOS]
        MOV     BX,CX
        OR      AX,AX
        JZ      SHORT SAVFIR	;Partial first sector?
        SUB     AX,[BP+DPBLOCK.SECSIZ]
        NEG     AX              ;Max number of bytes left in first sector
        SUB     BX,AX           ;Subtract from total length
        JAE     SHORT SAVFIR
        ADD     AX,BX           ;Don't use all of the rest of the sector
        XOR     BX,BX           ;And no bytes are left
SAVFIR:
        MOV     [BYTCNT1],AX
        MOV     AX,BX
        XOR     DX,DX
        DIV     WORD [BP+DPBLOCK.SECSIZ]  ;How many whole sectors?
        MOV     [SECCNT],AX
        MOV     [BYTCNT2],DX    ;Bytes remaining for last sector
RET10:  RETN


FNDCLUS:

; Inputs:
;       DS = CS
;       CX = No. of clusters to skip
;       BP = Base of drive parameters
;       SI = FAT pointer
;       ES:DI point to FCB
; Outputs:
;       BX = Last cluster skipped to
;       CX = No. of clusters remaining (0 unless EOF)
;       DX = Position of last cluster
; DI destroyed. No other registers affected.

        MOV     BX,[ES:DI+FCBLOCK.LSTCLUS]
        MOV     DX,[ES:DI+FCBLOCK.CLUSPOS]
        OR      BX,BX
        JZ      SHORT NOCLUS
        SUB     CX,DX
        JNB     SHORT FINDIT
        ADD     CX,DX
        XOR     DX,DX
        MOV     BX,[ES:DI+FCBLOCK.FIRCLUS]
FINDIT:
        JCXZ    RET10
SKPCLP:
        CALL    UNPACK
        CMP     DI,0FF8H
        JAE     SHORT RET10
        XCHG    BX,DI
        INC     DX
        LOOP    SKPCLP
        RETN
NOCLUS:
        INC     CX
        DEC     DX
        RETN


BUFSEC:
; Inputs:
;       AL = 0 if buffer must be read, 1 if no pre-read needed
;       BP = Base of drive parameters
;       [CLUSNUM] = Physical cluster number
;       [SECCLUSPOS] = Sector position of transfer within cluster
;       [BYTCNT1] = Size of transfer
; Function:
;       Insure specified sector is in buffer, flushing buffer before
;       read if necessary.
; Outputs:
;       SI = Pointer to buffer
;       DI = Pointer to transfer address
;       CX = Number of bytes
;       [NEXTADD] updated
;       [TRANS] set to indicate a transfer will occur

        MOV     DX,[CLUSNUM]
        MOV     BL,[SECCLUSPOS]
        CALL    FIGREC
        MOV     [PREREAD],AL
        CMP     DX,[BUFSECNO]
        JNZ     SHORT GETSEC
        MOV     AL,[BUFDRVNO]
        CMP     AL,[THISDRV]
        JZ      SHORT FINBUF	;Already have it?
GETSEC:
        XOR     AL,AL
        XCHG    [DIRTYBUF],AL   ;Read dirty flag and reset it
        OR      AL,AL
        JZ      SHORT RDSEC
        PUSH    DX
        PUSH    BP
        MOV     BP,[BUFDRVBP]
        MOV     BX,[BUFFER]
        MOV     CX,1
        MOV     DX,[BUFSECNO]
        CALL    DWRITE
        POP     BP
        POP     DX
RDSEC:
        TEST    BYTE [PREREAD],-1
        JNZ     SHORT SETBUF
        XOR     AX,AX
        MOV     [BUFSECNO],AX           ;Set buffer valid in case of disk error
        DEC     AX
        MOV     [BUFDRVNO],AL
        MOV     BX,[BUFFER]
        MOV     CX,1
        PUSH    DX
        CALL    DREAD
        POP     DX
SETBUF:
        MOV     [BUFSECNO],DX
        MOV     AL,[THISDRV]
        MOV     [BUFDRVNO],AL
        MOV     [BUFDRVBP],BP
FINBUF:
        MOV     BYTE [TRANS],1		;A transfer is taking place
        MOV     DI,[NEXTADD]
        MOV     SI,DI
        MOV     CX,[BYTCNT1]
        ADD     SI,CX
        MOV     [NEXTADD],SI
        MOV     SI,[BUFFER]
        ADD     SI,[BYTSECPOS]
        RETN

BUFRD:
        XOR     AL,AL           ;Pre-read necessary
        CALL    BUFSEC
        PUSH    ES
        MOV     ES,[DMAADD+2]
        SHR     CX,1
        JNC     SHORT EVENRD
        MOVSB
EVENRD:
        REP     MOVSW
        POP     ES
        RETN

BUFWRT:
        MOV     AX,[SECPOS]
        INC     AX              ;Set for next sector
        MOV     [SECPOS],AX
        CMP     AX,[VALSEC]     ;Has sector been written before?
        MOV     AL,1
        JA      SHORT NOREAD	;Skip preread if SECPOS>VALSEC
        MOV     AL,0
NOREAD:
        CALL    BUFSEC
        XCHG    DI,SI
        PUSH    DS
        PUSH    ES
        PUSH    CS
        POP     ES
        MOV     DS,[DMAADD+2]
        SHR     CX,1
        JNC     SHORT EVENWRT
        MOVSB
EVENWRT:
        REP     MOVSW
        POP     ES
        POP     DS
        MOV     BYTE [DIRTYBUF],1
        RETN

NEXTSEC:
        TEST    BYTE [TRANS],-1
        JZ      SHORT CLRET
        MOV     AL,[SECCLUSPOS]
        INC     AL
        CMP     AL,[BP+DPBLOCK.CLUSMSK]
        JBE     SHORT SAVPOS
        MOV     BX,[CLUSNUM]
        CMP     BX,0FF8H
        JAE     SHORT NONEXT
        MOV     SI,[BP+DPBLOCK.FAT]
        CALL    UNPACK
        MOV     [CLUSNUM],DI
        INC     WORD [LASTPOS]
        MOV     AL,0
SAVPOS:
        MOV     [SECCLUSPOS],AL
CLRET:
        CLC
        RETN
NONEXT:
        STC
        RETN

TRANBUF:
        LODSB
        STOSB
        CMP     AL,13           ;Check for carriage return
        JNZ     SHORT NORMCH
        MOV     BYTE [SI],10
NORMCH:
        CMP     AL,10
        LOOPNZ  TRANBUF
        JNZ     SHORT ENDRDCON
        CALL    OUT             ;Transmit linefeed
        XOR     SI,SI
        OR      CX,CX
        JNZ     SHORT GETBUF
        OR      AL,1            ;Clear zero flag--not end of file
ENDRDCON:
        MOV     [CONTPOS],SI
ENDRDDEV:
        MOV     [NEXTADD],DI
        POP     ES
        JNZ     SHORT SETFCBJ         ;Zero set if Ctrl-Z found in input
        MOV     DI,[FCB]
        AND     BYTE [ES:DI+FCBLOCK.DEVID],0FFH-40H 
					;Mark as no more data available
SETFCBJ:
        JMP     SETFCB

READDEV:
        PUSH    ES
        LES     DI,[DMAADD]
        INC     BL
        JZ      SHORT READCON
        INC     BL
        JNZ     SHORT ENDRDDEV
READAUX:
        CALL    AUXIN
        STOSB
        CMP     AL,1AH
        LOOPNZ  READAUX
        JMP     SHORT ENDRDDEV

READCON:
        PUSH    CS
        POP     DS
        MOV     SI,[CONTPOS]
        OR      SI,SI
        JNZ     SHORT TRANBUF
        CMP     BYTE [CONBUF],128
        JZ      SHORT GETBUF
        MOV     WORD [CONBUF],0FF80H	;Set up 128-byte buffer with no template
GETBUF:
        PUSH    CX
        PUSH    ES
        PUSH    DI
        MOV     DX,CONBUF
        CALL    BUFIN           ;Get input buffer
        POP     DI
        POP     ES
        POP     CX
        MOV     SI,2 + CONBUF
        CMP     BYTE [SI],1AH	;Check for Ctrl-Z in first character
        JNZ     SHORT TRANBUF
        MOV     AL,1AH
        STOSB
        MOV     AL,10
        CALL    OUT             ;Send linefeed
        XOR     SI,SI
        JMP     SHORT ENDRDCON

RDERR:
        XOR     CX,CX
        JMP     WRTERR

RDLASTJ:JMP     RDLAST

LOAD:

; Inputs:
;       DS:DI point to FCB
;       DX:AX = Position in file to read
;       CX = No. of records to read
; Outputs:
;       DX:AX = Position of last record read
;       CX = No. of bytes read
;       ES:DI point to FCB
;       LSTCLUS, CLUSPOS fields in FCB set

        CALL    SETUP
        OR      BL,BL           ;Check for named device I/O
        JS      SHORT READDEV
        MOV     AX,[ES:DI+FCBLOCK.FILSIZ]
        MOV     BX,[ES:DI+FCBLOCK.FILSIZ+2]
        SUB     AX,[BYTPOS]
        SBB     BX,[BYTPOS+2]
        JB      SHORT RDERR
        JNZ     SHORT ENUF
        OR      AX,AX
        JZ      SHORT RDERR
        CMP     AX,CX
        JAE     SHORT ENUF
        MOV     CX,AX
ENUF:
        CALL    BREAKDOWN
        MOV     CX,[CLUSNUM]
        CALL    FNDCLUS
        OR      CX,CX
        JNZ     SHORT RDERR
        MOV     [LASTPOS],DX
        MOV     [CLUSNUM],BX
        CMP     WORD [BYTCNT1],0
        JZ      SHORT RDMID
        CALL    BUFRD
RDMID:
        CMP     WORD [SECCNT],0
        JZ      SHORT RDLASTJ
        CALL    NEXTSEC
        JC      SHORT SETFCB
        MOV     BYTE [TRANS],1      ;A transfer is taking place
ONSEC:
        MOV     DL,[SECCLUSPOS]
        MOV     CX,[SECCNT]
        MOV     BX,[CLUSNUM]
RDLP:
        CALL    OPTIMIZE
        PUSH    DI
        PUSH    AX
        PUSH    DS
        MOV     DS,[DMAADD+2]
        PUSH    DX
        PUSH    BX
        PUSHF                   ;Save carry flag
        CALL	DREAD
        POPF                    ;Restore carry flag
        POP     DI              ;Initial transfer address
        POP     AX              ;First sector transfered
        POP     DS
        JC      SHORT NOTBUFFED	;Was one of those sectors in the buffer?
        CMP     BYTE [DIRTYBUF],0 ;Is buffer dirty?
        JZ      SHORT NOTBUFFED	;If not no problem
;We have transfered in a sector from disk when a dirty copy of it is in the buffer.
;We must transfer the sector from the buffer to correct memory address
        SUB     AX,[BUFSECNO]   ;How many sectors into the transfer?
        NEG     AX
        MOV     CX,[BP+DPBLOCK.SECSIZ]
        MUL     CX              ;How many bytes into the transfer?
        ADD     DI,AX
        MOV     SI,[BUFFER]
        PUSH    ES
        MOV     ES,[DMAADD+2]   ;Get disk transfer segment
        SHR     CX,1
        REP     MOVSW
        JNC     SHORT EVENMOV
        MOVSB
EVENMOV:
        POP     ES
NOTBUFFED:
        POP     CX
        POP     BX
        JCXZ    RDLAST
        CMP     BX,0FF8H
        JAE     SHORT SETFCB
        MOV     DL,0
        INC     WORD [LASTPOS]	;We'll be using next cluster
        JMP     SHORT RDLP

SETFCB:
        MOV     SI,[FCB]
        MOV     AX,[NEXTADD]
        MOV     DI,AX
        SUB     AX,[DMAADD]     ;Number of bytes transfered
        XOR     DX,DX
        MOV     CX,[ES:SI+FCBLOCK.RECSIZ]
        DIV     CX              ;Number of records
        CMP     AX,[RECCNT]     ;Check if all records transferred
        JZ      SHORT FULLREC
        MOV     BYTE [DSKERR],1
        OR      DX,DX
        JZ      SHORT FULLREC	;If remainder 0, then full record transfered
        MOV     BYTE [DSKERR],3	;Flag partial last record
        SUB     CX,DX           ;Bytes left in last record
        PUSH    ES
        MOV     ES,[DMAADD+2]
        XCHG    AX,BX           ;Save the record count temporarily
        XOR     AX,AX           ;Fill with zeros
        SHR     CX,1
        JNC     SHORT EVENFIL
        STOSB
EVENFIL:
        REP     STOSW
        XCHG    AX,BX           ;Restore record count to AX
        POP     ES
        INC     AX              ;Add last (partial) record to total
FULLREC:
        MOV     CX,AX
        MOV     DI,SI           ;ES:DI point to FCB
SETCLUS:
        MOV     AX,[CLUSNUM]
        MOV     [ES:DI+FCBLOCK.LSTCLUS],AX
        MOV     AX,[LASTPOS]
        MOV     [ES:DI+FCBLOCK.CLUSPOS],AX
ADDREC:
        MOV     AX,[RECPOS]
        MOV     DX,[RECPOS+2]
        JCXZ    RET28           ;If no records read, don't change position
        DEC     CX
        ADD     AX,CX           ;Update current record position
        ADC     DX,0
        INC     CX      
RET28:  
	RETN

RDLAST:
        MOV     AX,[BYTCNT2]
        OR      AX,AX
        JZ      SHORT SETFCB
        MOV     [BYTCNT1],AX
        CALL    NEXTSEC
        JC      SHORT SETFCB
        MOV     WORD [BYTSECPOS],0
        CALL    BUFRD
        JMP     SHORT SETFCB

WRTDEV:
        PUSH    DS
        LDS     SI,[DMAADD]
        OR      BL,40H
        INC     BL
        JZ      SHORT WRTCON
        INC     BL
        JZ      SHORT WRTAUX
        INC     BL
        JZ      SHORT ENDWRDEV	;Done if device is NUL
WRTLST:
        LODSB
        CMP     AL,1AH
        JZ      SHORT ENDWRDEV
        CALL    LISTOUT
        LOOP    WRTLST
        JMP     SHORT ENDWRDEV

WRTAUX:
        LODSB
        CALL    AUXOUT
        CMP     AL,1AH
        LOOPNZ  WRTAUX
        JMP     SHORT ENDWRDEV

WRTCON:
        LODSB
        CMP     AL,1AH
        JZ      SHORT ENDWRDEV
        CALL    OUT
        LOOP    WRTCON
ENDWRDEV:
        POP     DS
        MOV     CX,[RECCNT]
        MOV     DI,[FCB]
        JMP     SHORT ADDREC

HAVSTART:
        MOV     CX,AX
        CALL    SKPCLP
        JCXZ    DOWRTJ
        CALL    ALLOCATE
        JNC     SHORT DOWRTJ
WRTERR:
        MOV     BYTE [DSKERR],1
LVDSK:
        MOV     AX,[RECPOS]
        MOV     DX,[RECPOS+2]
        MOV     DI,[FCB]
        RETN

DOWRTJ: 
	JMP     SHORT DOWRT

WRTEOFJ:
        JMP     WRTEOF

STORE:

; Inputs:
;       DS:DI point to FCB
;       DX:AX = Position in file of disk transfer
;       CX = Record count
; Outputs:
;       DX:AX = Position of last record written
;       CX = No. of records written
;       ES:DI point to FCB
;       LSTCLUS, CLUSPOS fields in FCB set

        CALL    SETUP
        CALL    DATE16
        MOV     [ES:DI+FCBLOCK.FDATE],AX
        MOV     [ES:DI+FCBLOCK.FTIME],DX
        OR      BL,BL
        JS      SHORT WRTDEV
        AND     BL,3FH          ;Mark file as dirty
        MOV     [ES:DI+FCBLOCK.DEVID],BL
        CALL    BREAKDOWN
        MOV     AX,[BYTPOS]
        MOV     DX,[BYTPOS+2]
        JCXZ    WRTEOFJ
        DEC     CX
        ADD     AX,CX
        ADC     DX,0            ;AX:DX=last byte accessed
        DIV     WORD [BP+DPBLOCK.SECSIZ]    ;AX=last sector accessed
        MOV     CL,[BP+DPBLOCK.CLUSSHFT]
        SHR     AX,CL           ;Last cluster to be accessed
        PUSH    AX
        MOV     AX,[ES:DI+FCBLOCK.FILSIZ]
        MOV     DX,[ES:DI+FCBLOCK.FILSIZ+2]
        DIV     WORD [BP+DPBLOCK.SECSIZ]
        OR      DX,DX
        JZ      SHORT NORNDUP
        INC     AX              ;Round up if any remainder
NORNDUP:
        MOV     [VALSEC],AX     ;Number of sectors that have been written
        POP     AX
        MOV     CX,[CLUSNUM]    ;First cluster accessed
        CALL    FNDCLUS
        MOV     [CLUSNUM],BX
        MOV     [LASTPOS],DX
        SUB     AX,DX           ;Last cluster minus current cluster
        JZ      SHORT DOWRT	;If we have last clus, we must have first
        JCXZ    HAVSTART        ;See if no more data
        PUSH    CX              ;No. of clusters short of first
        MOV     CX,AX
        CALL    ALLOCATE
        POP     AX
        JC      SHORT WRTERR
        MOV     CX,AX
        MOV     DX,[LASTPOS]
        INC     DX
        DEC     CX
        JZ      SHORT NOSKIP
        CALL    SKPCLP
NOSKIP:
        MOV     [CLUSNUM],BX
        MOV     [LASTPOS],DX
DOWRT:
        CMP     WORD [BYTCNT1],0
        JZ      SHORT WRTMID
        MOV     BX,[CLUSNUM]
        CALL    BUFWRT  
WRTMID:
        MOV     AX,[SECCNT]
        OR      AX,AX
        JZ      SHORT WRTLAST
        ADD     [SECPOS],AX
        CALL    NEXTSEC
        MOV     BYTE [TRANS],1	;A transfer is taking place
        MOV     DL,[SECCLUSPOS]
        MOV     BX,[CLUSNUM]
        MOV     CX,[SECCNT]
WRTLP:
        CALL    OPTIMIZE
        JC      SHORT NOTINBUF	;Is one of the sectors buffered?
        MOV     WORD [BUFSECNO],0	
				;If so, invalidate the buffer since we're
        MOV     WORD [BUFDRVNO],0FFH	;       completely rewritting it
NOTINBUF:
        PUSH    DI
        PUSH    AX
        PUSH    DS
        MOV     DS,[DMAADD+2]
        CALL    DWRITE
        POP     DS
        POP     CX
        POP     BX
        JCXZ    WRTLAST
        MOV     DL,0
        INC     WORD [LASTPOS]		;We'll be using next cluster
        JMP     SHORT WRTLP
WRTLAST:
        MOV     AX,[BYTCNT2]
        OR      AX,AX
        JZ      SHORT FINWRT
        MOV     [BYTCNT1],AX
        CALL    NEXTSEC
        MOV     WORD [BYTSECPOS],0
        CALL    BUFWRT
FINWRT:
        MOV     AX,[NEXTADD]
        SUB     AX,[DMAADD]
        ADD     AX,[BYTPOS]
        MOV     DX,[BYTPOS+2]
        ADC     DX,0
        MOV     CX,DX
        MOV     DI,[FCB]
        CMP     AX,[ES:DI+FCBLOCK.FILSIZ]
        SBB     CX,[ES:DI+FCBLOCK.FILSIZ+2]
        JB      SHORT SAMSIZ
        MOV     [ES:DI+FCBLOCK.FILSIZ],AX
        MOV     [ES:DI+FCBLOCK.FILSIZ+2],DX
SAMSIZ:
        MOV     CX,[RECCNT]
        JMP     SETCLUS

WRTERRJ:
	JMP     WRTERR

WRTEOF:
        MOV     CX,AX
        OR      CX,DX
        JZ      SHORT KILLFIL
        SUB     AX,1
        SBB     DX,0
        DIV     WORD [BP+DPBLOCK.SECSIZ]
        MOV     CL,[BP+DPBLOCK.CLUSSHFT]
        SHR     AX,CL
        MOV     CX,AX
        CALL    FNDCLUS
        JCXZ    RELFILE
        CALL    ALLOCATE
        JC      SHORT WRTERRJ
UPDATE:
        MOV     DI,[FCB]
        MOV     AX,[BYTPOS]
        MOV     [ES:DI+FCBLOCK.FILSIZ],AX
        MOV     AX,[BYTPOS+2]
        MOV     [ES:DI+FCBLOCK.FILSIZ+2],AX
        XOR     CX,CX
        JMP     ADDREC

RELFILE:
        MOV     DX,0FFFH
        CALL    RELBLKS
SETDIRT:
        MOV     BYTE [SI-1],1
        JMP     SHORT UPDATE

KILLFIL:
        XOR     BX,BX
        XCHG    BX,[ES:DI+FCBLOCK.FIRCLUS]
        OR      BX,BX
        JZ      SHORT UPDATE
        CALL    RELEASE
        JMP     SHORT SETDIRT


OPTIMIZE:

; Inputs:
;       DS = CS
;       BX = Physical cluster
;       CX = No. of records
;       DL = sector within cluster
;       BP = Base of drives parameters
;       [NEXTADD] = transfer address
; Outputs:
;       AX = No. of records remaining
;       BX = Transfer address
;       CX = No. or records to be transferred
;       DX = Physical sector address
;       DI = Next cluster
;       Carry clear if a sector to transfer is in the buffer
;       Carry set otherwise
;       [CLUSNUM] = Last cluster accessed
;       [NEXTADD] updated
; BP unchanged. Note that segment of transfer not set.

        PUSH    DX
        PUSH    BX
        MOV     AL,[BP+DPBLOCK.CLUSMSK]
        INC     AL              ;Number of sectors per cluster
        MOV     AH,AL
        SUB     AL,DL           ;AL = Number of sectors left in first cluster
        MOV     DX,CX
        MOV     SI,[BP+DPBLOCK.FAT]
        MOV     CX,0
OPTCLUS:
;AL has number of sectors available in current cluster
;AH has number of sectors available in next cluster
;BX has current physical cluster
;CX has number of sequential sectors found so far
;DX has number of sectors left to transfer
;SI has FAT pointer
        CALL    UNPACK
        ADD     CL,AL
        ADC     CH,0
        CMP     CX,DX
        JAE     SHORT BLKDON
        MOV     AL,AH
        INC     BX
        CMP     DI,BX
        JZ      SHORT OPTCLUS
        DEC     BX
FINCLUS:
        MOV     [CLUSNUM],BX    ;Last cluster accessed
        SUB     DX,CX           ;Number of sectors still needed
        PUSH    DX
        MOV     AX,CX
        MUL     WORD [BP+DPBLOCK.SECSIZ] 
				;Number of sectors times sector size
        MOV     SI,[NEXTADD]
        ADD     AX,SI           ;Adjust by size of transfer
        MOV     [NEXTADD],AX
        POP     AX              ;Number of sectors still needed
        POP     DX              ;Starting cluster
        SUB     BX,DX           ;Number of new clusters accessed
        ADD     [LASTPOS],BX
        POP     BX              ;BL = sector postion within cluster
        CALL    FIGREC
        MOV     BX,SI
;Now let's see if any of these sectors are already in the buffer
        CMP     [BUFSECNO],DX
        JC      SHORT RET100	;If DX > [BUFSECNO] then not in buffer
        MOV     SI,DX
        ADD     SI,CX           ;Last sector + 1
        CMP     [BUFSECNO],SI
        CMC
        JC	SHORT RET100	;If SI <= [BUFSECNO] then not in buffer
        PUSH    AX
        MOV     AL,[BP+DPBLOCK.DEVNUM]
        CMP     AL,[BUFDRVNO]   ;Is buffer for this drive?
        POP     AX
        JZ	SHORT RET100	;If so, then we match 
        STC                     ;No match
RET100: 
	RETN
BLKDON:
        SUB     CX,DX           ;Number of sectors in cluster we don't want
        SUB     AH,CL           ;Number of sectors in cluster we accepted
        DEC     AH              ;Adjust to mean position within cluster
        MOV     [SECCLUSPOS],AH
        MOV     CX,DX           ;Anyway, make the total equal to the request
        JMP     SHORT FINCLUS

FIGREC:

;Inputs:
;       DX = Physical cluster number
;       BL = Sector position within cluster
;       BP = Base of drive parameters
;Outputs:
;       DX = physical sector number
;No other registers affected.

        PUSH    CX
        MOV     CL,[BP+DPBLOCK.CLUSSHFT]
        DEC     DX
        DEC     DX
        SHL     DX,CL
        OR      DL,BL
        ADD     DX,[BP+DPBLOCK.FIRREC]
        POP     CX
        RETN

GETREC:

; Inputs:
;       DS:DX point to FCB
; Outputs:
;       CX = 1
;       DX:AX = Record number determined by EXTENT and NR fields
;       DS:DI point to FCB
; No other registers affected.

        MOV     DI,DX
        CMP     BYTE [DI],-1	;Check for extended FCB
        JNZ     SHORT NORMFCB2
        ADD     DI,7
NORMFCB2:
        MOV     CX,1
        MOV     AL,[DI+FCBLOCK.NR]
        MOV     DX,[DI+FCBLOCK.EXTENT]
        SHL     AL,1
        SHR     DX,1
        RCR     AL,1
        MOV     AH,DL
        MOV     DL,DH
        MOV     DH,0
        RETN


ALLOCATE:

; Inputs:
;       DS = CS
;       ES = Segment of FCB
;       BX = Last cluster of file (0 if null file)
;       CX = No. of clusters to allocate
;       DX = Position of cluster BX
;       BP = Base of drive parameters
;       SI = FAT pointer
;       [FCB] = Displacement of FCB within segment
; Outputs:
;       IF insufficient space
;         THEN
;       Carry set
;       CX = max. no. of records that could be added to file
;         ELSE
;       Carry clear
;       BX = First cluster allocated
;       FAT is fully updated including dirty bit
;       FIRCLUS field of FCB set if file was null
; SI,BP unchanged. All other registers destroyed.

        PUSH    WORD [SI]
        PUSH    DX
        PUSH    CX
        PUSH    BX
        MOV     AX,BX
ALLOC:
        MOV     DX,BX
FINDFRE:
        INC     BX
        CMP     BX,[BP+DPBLOCK.MAXCLUS]
        JLE     SHORT TRYOUT
        CMP     AX,1
        JG      SHORT TRYIN
        POP     BX
        MOV     DX,0FFFH
        CALL    RELBLKS
        POP     AX              ;No. of clusters requested
        SUB     AX,CX           ;AX=No. of clusters allocated
        POP     DX
        POP     WORD [SI]
        INC     DX              ;Position of first cluster allocated
        ADD     AX,DX           ;AX=max no. of cluster in file
        MOV     DL,[BP+DPBLOCK.CLUSMSK]
        MOV     DH,0
        INC     DX              ;DX=records/cluster
        MUL     DX              ;AX=max no. of records in file
        MOV     CX,AX
        SUB     CX,[RECPOS]	;CX=max no. of records that could be written
        JA      SHORT MAXREC
        XOR     CX,CX           ;If CX was negative, zero it
MAXREC:
        STC
RET11:  
	RETN

TRYOUT:
        CALL    UNPACK
        JZ      SHORT HAVFRE
TRYIN:
        DEC     AX
        JLE     SHORT FINDFRE
        XCHG    AX,BX
        CALL    UNPACK
        JZ      SHORT HAVFRE
        XCHG    AX,BX
        JMP     SHORT FINDFRE
HAVFRE:
        XCHG    BX,DX
        MOV     AX,DX
        CALL    PACK
        MOV     BX,AX
        LOOP    ALLOC
        MOV     DX,0FFFH
        CALL    PACK
        MOV     BYTE [SI-1],1
        POP     BX
        POP     CX              ;Don't need this stuff since we're successful
        POP     DX
        CALL    UNPACK
        POP     WORD [SI]
        XCHG    BX,DI
        OR      DI,DI
        JNZ     SHORT RET11
        MOV     DI,[FCB]
        MOV     [ES:DI+FCBLOCK.FIRCLUS],BX
RET12:  
	RETN


RELEASE:

; Inputs:
;       DS = CS
;       BX = Cluster in file
;       SI = FAT pointer
;       BP = Base of drive parameters
; Function:
;       Frees cluster chain starting with [BX]
; AX,BX,DX,DI all destroyed. Other registers unchanged.

        XOR     DX,DX
RELBLKS:
; Enter here with DX=0FFFH to put an end-of-file mark
; in the first cluster and free the rest in the chain.
        CALL    UNPACK
        JZ      SHORT RET12
        MOV     AX,DI
        CALL    PACK
        CMP     AX,0FF8H
        MOV     BX,AX
        JB      SHORT RELEASE
RET13:  
	RETN


GETEOF:

; Inputs:
;       BX = Cluster in a file
;       SI = Base of drive FAT
;       DS = CS
; Outputs:
;       BX = Last cluster in the file
; DI destroyed. No other registers affected.

        CALL    UNPACK
        CMP     DI,0FF8H
        JAE     SHORT RET13
        MOV     BX,DI
        JMP     SHORT GETEOF


SRCHFRST: ;System call 17
        CALL    GETFILE
SAVPLCE:
; Search-for-next enters here to save place and report
; findings.
        JC      SHORT KILLSRCH
        OR      BH,BH
        JS      SHORT SRCHDEV
        MOV     AX,[LASTENT]
        MOV     [ES:DI+FCBLOCK.FILDIRENT],AX
        MOV     [ES:DI+FCBLOCK.DRVBP],BP
;Information in directory entry must be copied into the first
; 33 bytes starting at the disk transfer address.
        MOV     SI,BX
        LES     DI,[DMAADD]
        MOV     AX,00FFH
        CMP     AL,[EXTFCB]
        JNZ     SHORT NORMFCB
        STOSW
        INC     AL
        STOSW
        STOSW
        MOV     AL,[ATTRIB]
        STOSB
NORMFCB:
        MOV     AL,[THISDRV]
        INC     AL
        STOSB   ;Set drive number
        MOV     CX,16
        REP     MOVSW   ;Copy remaining 10 characters of name
        XOR     AL,AL
        RETN

KILLSRCH:
KILLSRCH1       EQU     KILLSRCH+1
;The purpose of the KILLSRCH1 label is to provide a jump label to the following
;   instruction which leaves out the segment override.
        MOV     WORD [ES:DI+FCBLOCK.FILDIRENT],-1
        MOV     AL,-1
        RETN

SRCHDEV:
        MOV     [ES:DI+FCBLOCK.FILDIRENT],BX
        LES     DI,[DMAADD]
        XOR     AX,AX
        STOSB           ;Zero drive byte
        SUB     SI,4            ;Point to device name
        MOVSW
        MOVSW
        MOV     AX,2020H
        STOSB
        STOSW
        STOSW
        STOSW                   ;Fill with 8 blanks
        XOR     AX,AX
        MOV     CX,10
        REP     STOSW
        STOSB
RET14:  RETN

SRCHNXT: ;System call 18
        CALL    MOVNAME
        MOV     DI,DX
        JC      NEAR KILLSRCH1
        MOV     BP,[DI+FCBLOCK.DRVBP]
        MOV     AX,[DI+FCBLOCK.FILDIRENT]
        OR      AX,AX
        JS      NEAR KILLSRCH1
        PUSH    DX
        PUSH    DS
        PUSH    CS
        POP     DS
        MOV     [LASTENT],AX
        CALL    CONTSRCH
        POP     ES
        POP     DI
        JMP     SAVPLCE


FILESIZE: ;System call 35
        CALL    GETFILE
        MOV     AL,-1
        JC      SHORT RET14
        ADD     DI,33           ;Write size in RR field
        MOV     CX,[ES:DI+FCBLOCK.RECSIZ-33]
        OR      CX,CX
        JNZ     SHORT RECOK
        MOV     CX,128
RECOK:
        XOR     AX,AX
        XOR     DX,DX           ;Intialize size to zero
        OR      BH,BH           ;Check for named I/O device
        JS      SHORT DEVSIZ
        INC     SI
        INC     SI              ;Point to length field
        MOV     AX,[SI+2]       ;Get high word of size
        DIV     CX
        PUSH    AX              ;Save high part of result
        LODSW			;Get low word of size
        DIV     CX
        OR      DX,DX           ;Check for zero remainder
        POP     DX
        JZ      SHORT DEVSIZ
        INC     AX              ;Round up for partial record
        JNZ     SHORT DEVSIZ	;Propagate carry?
        INC     DX
DEVSIZ:
        STOSW
        MOV     AX,DX
        STOSB
        MOV     AL,0
        CMP     CX,64
        JAE     SHORT RET14	;Only 3-byte field if RECSIZ >= 64
        MOV     [ES:DI],AH
        RETN


SETDMA: ;System call 26
        MOV     [CS:DMAADD],DX
        MOV     [CS:DMAADD+2],DS
        RETN

NOSUCHDRV:
        MOV     AL,-1
        RETN

GETFATPT: ;System call 27
        MOV     DL,0                    ;Use default drive

GETFATPTDL:     ;System call 28
        PUSH    CS
        POP     DS
        MOV     AL,DL
        CALL    GETTHISDRV
        JC      SHORT NOSUCHDRV
        CALL    FATREAD
        MOV     BX,[BP+DPBLOCK.FAT]
        MOV     AL,[BP+DPBLOCK.CLUSMSK]
        INC     AL
        MOV     DX,[BP+DPBLOCK.MAXCLUS]
        DEC     DX
        MOV     CX,[BP+DPBLOCK.SECSIZ]
        LDS     SI,[SPSAVE]
        MOV     [SI+STKPTRS.BXSAVE],BX
        MOV     [SI+STKPTRS.DXSAVE],DX
        MOV     [SI+STKPTRS.CXSAVE],CX
        MOV     [SI+STKPTRS.DSSAVE],CS
        RETN


GETDSKPT: ;System call 31
        PUSH    CS
        POP     DS
        MOV     AL,[CURDRV]
        MOV     [THISDRV],AL
        CALL    FATREAD
        LDS     SI,[SPSAVE]
        MOV     [SI+STKPTRS.BXSAVE],BP
        MOV     [SI+STKPTRS.DSSAVE],CS
        RETN


DSKRESET: ;System call 13
        PUSH    CS
        POP     DS
WRTFATS:
; DS=CS. Writes back all dirty FATs. All registers destroyed.
        XOR     AL,AL
        XCHG    AL,[DIRTYBUF]
        OR      AL,AL
        JZ      SHORT NOBUF
        MOV     BP,[BUFDRVBP]
        MOV     DX,[BUFSECNO]
        MOV     BX,[BUFFER]
        MOV     CX,1
        CALL    DWRITE
NOBUF:
        MOV     CL,[NUMIO]
        MOV     CH,0
        MOV     BP,[DRVTAB]
WRTFAT:
        PUSH    CX
        CALL    CHKFATWRT
        POP     CX
        ADD     BP,DPBSIZ
        LOOP    WRTFAT
        RETN


GETDRV: ;System call 25
        MOV     AL,[CS:CURDRV]
RET15:  RETN


SETRNDREC: ;System call 36
        CALL    GETREC
        MOV     [DI+33],AX
        MOV     [DI+35],DL
        CMP     WORD [DI+FCBLOCK.RECSIZ],64
        JAE     SHORT RET15
        MOV     [DI+36],DH      ;Set 4th byte only if record size < 64
RET16:  RETN


SELDSK: ;System call 14
        MOV     AL,[CS:NUMDRV]
        CMP     DL,AL
        JNB     SHORT RET17
        MOV     [CS:CURDRV],DL
RET17:  RETN

BUFIN:  ;System call 10
        MOV     AX,CS
        MOV     ES,AX
        MOV     SI,DX
        MOV     CH,0
        LODSW
        OR      AL,AL
        JZ      SHORT RET17
        MOV     BL,AH
        MOV     BH,CH
        CMP     AL,BL
        JBE     SHORT NOEDIT
        CMP     BYTE [BX+SI],0DH
        JZ      SHORT EDITON
NOEDIT:
        MOV     BL,CH
EDITON:
        MOV     DL,AL
        DEC     DX
NEWLIN:
        MOV     AL,[CS:CARPOS]
        MOV     [CS:STARTPOS],AL
        PUSH    SI
        MOV     DI,INBUF
        MOV     AH,CH
        MOV     BH,CH
        MOV     DH,CH
GETCH:
        CALL    IN
        CMP     AL,"F"-"@"      ;Ignore ^F
        JZ      SHORT GETCH
        CMP     AL,[CS:ESCCHAR]
        JZ      SHORT ESC
        CMP     AL,7FH
        JZ      SHORT BACKSP
        CMP     AL,8
        JZ      SHORT BACKSP
        CMP     AL,13
        JZ      SHORT ENDLIN
        CMP     AL,10
        JZ      SHORT PHYCRLF
        CMP     AL,CANCEL
        JZ      SHORT KILNEW
SAVCH:
        CMP     DH,DL
        JAE     SHORT BUFFUL
        STOSB
        INC     DH
        CALL    BUFOUT
        OR      AH,AH
        JNZ     SHORT GETCH
        CMP     BH,BL
        JAE     SHORT GETCH
        INC     SI
        INC     BH
        JMP     SHORT GETCH

BUFFUL:
        MOV     AL,7
        CALL    OUT
        JMP     SHORT GETCH

ESC:
        CALL    IN
        MOV     CL,ESCTABLEN
        PUSH    DI
        MOV     DI,ESCTAB
        REPNE   SCASB
        POP     DI
        SHL     CX,1
        MOV     BP,CX
        JMP     WORD [BP+ESCFUNC]

ENDLIN:
        STOSB
        CALL    OUT
        POP     DI
        MOV     [DI-1],DH
        INC     DH
COPYNEW:
        MOV     BP,ES
        MOV     BX,DS
        MOV     ES,BX
        MOV     DS,BP
        MOV     SI,INBUF
        MOV     CL,DH
        REP     MOVSB
        RETN
CRLF:
        MOV     AL,13
        CALL    OUT
        MOV     AL,10
        JMP     OUT

PHYCRLF:
        CALL    CRLF
        JMP     SHORT GETCH

KILNEW:
        MOV     AL,"\"
        CALL    OUT
        POP     SI
PUTNEW:
        CALL    CRLF
        MOV     AL,[CS:STARTPOS]
        CALL    TAB
        JMP     NEWLIN

BACKSP:
        OR      DH,DH
        JZ      SHORT OLDBAK
        CALL    BACKUP
        MOV     AL,[ES:DI]
        CMP     AL," "
        JAE     SHORT OLDBAK
        CMP     AL,9
        JZ      SHORT BAKTAB
        CALL    BACKMES
OLDBAK:
        OR      AH,AH
        JNZ     SHORT GETCH1
        OR      BH,BH
        JZ      SHORT GETCH1
        DEC     BH
        DEC     SI
GETCH1:
        JMP     GETCH
BAKTAB:
        PUSH    DI
        DEC     DI
        STD
        MOV     CL,DH
        MOV     AL," "
        PUSH    BX
        MOV     BL,7
        JCXZ    FIGTAB
FNDPOS:
        SCASB
        JNA     SHORT CHKCNT
        CMP     BYTE [ES:DI+1],9
        JZ      SHORT HAVTAB
        DEC     BL
CHKCNT:
        LOOP    FNDPOS
FIGTAB:
        SUB     BL,[CS:STARTPOS]
HAVTAB:
        SUB     BL,DH
        ADD     CL,BL
        AND     CL,7
        CLD
        POP     BX
        POP     DI
        JZ      SHORT OLDBAK
TABBAK:
        CALL    BACKMES
        LOOP    TABBAK
        JMP     SHORT OLDBAK
BACKUP:
        DEC     DH
        DEC     DI
BACKMES:
        MOV     AL,8
        CALL    OUT
        MOV     AL," "
        CALL    OUT
        MOV     AL,8
        JMP     OUT

TWOESC:
        MOV     AL,ESCCH
        JMP     SAVCH

COPYLIN:
        MOV     CL,BL
        SUB     CL,BH
        JMP     SHORT COPYEACH

COPYSTR:
        CALL    FINDOLD
        JMP     SHORT COPYEACH

COPYONE:
        MOV     CL,1
COPYEACH:
        MOV     AH,0
        CMP     DH,DL
        JZ      SHORT GETCH2
        CMP     BH,BL
        JZ      SHORT GETCH2
        LODSB
        STOSB
        CALL    BUFOUT
        INC     BH
        INC     DH
        LOOP    COPYEACH
GETCH2:
        JMP     GETCH

SKIPONE:
        CMP     BH,BL
        JZ      SHORT GETCH2
        INC     BH
        INC     SI
        JMP     GETCH

SKIPSTR:
        CALL    FINDOLD
        ADD     SI,CX
        ADD     BH,CL
        JMP     GETCH

FINDOLD:
        CALL    IN
        MOV     CL,BL
        SUB     CL,BH
        JZ      SHORT NOTFND
        DEC     CX
        JZ      SHORT NOTFND
        PUSH    ES
        PUSH    DS
        POP     ES
        PUSH    DI
        MOV     DI,SI
        INC     DI
        REPNE   SCASB
        POP     DI
        POP     ES
        JNZ     SHORT NOTFND
        NOT     CL
        ADD     CL,BL
        SUB     CL,BH
RET30:  RETN
NOTFND:
        POP     BP
        JMP     GETCH

REEDIT:
        MOV     AL,"@"
        CALL    OUT
        POP     DI
        PUSH    DI
        PUSH    ES
        PUSH    DS
        CALL    COPYNEW
        POP     DS
        POP     ES
        POP     SI
        MOV     BL,DH
        JMP     PUTNEW

ENTERINS:
        NOT     AH
        JMP     GETCH

ESCFUNC:
	DW      GETCH
        DW      TWOESC
        DW      ENTERINS
        DW      BACKSP
        DW      REEDIT
        DW      KILNEW
        DW      COPYLIN
        DW      SKIPSTR
        DW      COPYSTR
        DW      SKIPONE
        DW      COPYONE
        DW      COPYONE
        DW      CTRLZ
CTRLZ:
        MOV     AL,"Z"-"@"
        JMP     SAVCH
BUFOUT:
        CMP     AL," "
        JAE     SHORT OUT
        CMP     AL,9
        JZ      SHORT OUT
        PUSH    AX
        MOV     AL,"^"
        CALL    OUT
        POP     AX
        OR      AL,40H
        JMP     SHORT OUT

NOSTOP:
        CMP     AL,"P"-"@"
        JZ      SHORT INCHK
        CMP     AL,"C"-"@"
        JZ      SHORT INCHK
        RETN

CONOUT: ;System call 2
        MOV     AL,DL
OUT:
        CMP     AL,20H
        JB      SHORT CTRLOUT
        CMP     AL,7FH
        JZ      SHORT OUTCH
        INC     BYTE [CS:CARPOS]
OUTCH:
        PUSH    AX
        CALL    STATCHK
        POP     AX
        CALL    BIOSSEG:BIOSOUT
        TEST    BYTE [CS:PFLAG],-1
        JZ      SHORT RET18
        CALL    BIOSSEG:BIOSPRINT
RET18:  
	RETN

STATCHK:
        CALL    BIOSSEG:BIOSSTAT 
        JZ      SHORT RET18
        CMP     AL,'S'-'@'
        JNZ     SHORT NOSTOP
        CALL    BIOSSEG:BIOSIN		;Eat Cntrl-S
INCHK:
        CALL    BIOSSEG:BIOSIN
        CMP     AL,'P'-'@'
        JZ      SHORT PRINTON
        CMP     AL,'C'-'@'
        JNZ     SHORT RET18
; Ctrl-C handler.
; "^C" and CR/LF is printed. Then the user registers are restored and the
; user CTRL-C handler is executed. At this point the top of the stack has
; 1) the interrupt return address should the user CTRL-C handler wish to
; allow processing to continue; 2) the original interrupt return address
; to the code that performed the function call in the first place. If the
; user CTRL-C handler wishes to continue, it must leave all registers
; unchanged and IRET. The function that was interrupted will simply be
; repeated.
        MOV     AL,3            ;Display "^C"
        CALL    BUFOUT
        CALL    CRLF
        CLI                     ;Prepare to play with stack
        MOV     SS,[CS:SSSAVE]
        MOV     SP,[CS:SPSAVE]  ;User stack now restored
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES              ;User registers now restored
        INT     CONTC           ;Execute user Ctrl-C handler
        JMP     COMMAND		;Repeat command otherwise

PRINTON:
        NOT     BYTE [CS:PFLAG]
        RETN

CTRLOUT:
        CMP     AL,13
        JZ      SHORT ZERPOS
        CMP     AL,8
        JZ      SHORT BACKPOS
        CMP     AL,9
        JNZ     SHORT OUTCHJ
        MOV     AL,[CS:CARPOS]
        OR      AL,0F8H
        NEG     AL
TAB:
        PUSH    CX
        MOV     CL,AL
        MOV     CH,0
        JCXZ    POPTAB
TABLP:
        MOV     AL," "
        CALL    OUT
        LOOP    TABLP
POPTAB:
        POP     CX
RET19:  
	RETN

ZERPOS:
        MOV     BYTE [CS:CARPOS],0
OUTCHJ: 
	JMP     OUTCH

BACKPOS:
        DEC     BYTE [CS:CARPOS]
        JMP     OUTCH


CONSTAT: ;System call 11
        CALL    STATCHK
        MOV     AL,0 
        JZ      SHORT RET19
        OR      AL,-1
        RETN


CONIN:  ;System call 1
        CALL    IN
        PUSH    AX
        CALL    OUT
        POP     AX
        RETN


IN:     ;System call 8
        CALL    INCHK
        JZ      SHORT IN
RET29:  RETN

RAWIO:  ;System call 6
        MOV     AL,DL
        CMP     AL,-1
        JNZ     SHORT RAWOUT
        LDS     SI,[CS:SPSAVE]		;Get pointer to register save area
        CALL    BIOSSEG:BIOSSTAT
        JNZ     SHORT RESFLG
        OR      BYTE [SI+STKPTRS.FSAVE],40H	;Set user's zero flag
        XOR     AL,AL
        RETN

RESFLG:
        AND     BYTE [SI+STKPTRS.FSAVE],0FFH-40H ;Reset user's zero flag
RAWINP: ;System call 7
        CALL    BIOSSEG:BIOSIN 
        RETN
RAWOUT:
        CALL    BIOSSEG:BIOSOUT
        RETN

LIST:   ;System call 5
        MOV     AL,DL
LISTOUT:
        PUSH    AX
        CALL    STATCHK
        POP     AX
        CALL    BIOSSEG:BIOSPRINT
RET20:  RETN

PRTBUF: ;System call 9
        MOV     SI,DX
OUTSTR:
        LODSB
        CMP     AL,"$"
        JZ      SHORT RET20
        CALL    OUT
        JMP     SHORT OUTSTR

OUTMES: ;String output for internal messages
        MOV	AL, [CS:SI]
	INC	SI	
        CMP     AL,"$"
        JZ      SHORT RET20
        CALL    OUT
        JMP     SHORT OUTMES


MAKEFCB: ;Interrupt call 41
DRVBIT  EQU     2
NAMBIT  EQU     4
EXTBIT  EQU     8
        MOV     DL,0            ;Flag--not ambiguous file name
        TEST    AL,DRVBIT       ;Use current drive field if default?
        JNZ     SHORT DEFDRV
        MOV     BYTE [ES:DI],0	;No - use default drive
DEFDRV:
        INC     DI
        MOV     CX,8
        TEST    AL,NAMBIT       ;Use current name fiels as defualt?
        XCHG    AX,BX           ;Save bits in BX
        MOV     AL," "
        JZ      SHORT FILLB	;If not, go fill with blanks
        ADD     DI,CX
        XOR     CX,CX           ;Don't fill any
FILLB:
        REP     STOSB
        MOV     CL,3
        TEST    BL,EXTBIT       ;Use current extension as default
        JZ      SHORT FILLB2
        ADD     DI,CX
        XOR     CX,CX
FILLB2:
        REP     STOSB
        XCHG    AX,CX           ;Put zero in AX
        STOSW
        STOSW                   ;Initialize two words after to zero
        SUB     DI,16           ;Point back at start
        TEST    BL,1            ;Scan off separators if not zero
        JZ      SHORT SKPSPC
        CALL    SCANB           ;Peel off blanks and tabs
        CALL    DELIM           ;Is it a one-time-only delimiter?
        JNZ     SHORT NOSCAN
        INC     SI              ;Skip over the delimiter
SKPSPC:
        CALL    SCANB           ;Always kill preceding blanks and tabs
NOSCAN:
        CALL    GETLET
        JBE     SHORT NODRV	;Quit if termination character
        CMP     BYTE [SI],":"	;Check for potential drive specifier
        JNZ     SHORT NODRV
        INC     SI              ;Skip over colon
        SUB     AL,"@"          ;Convert drive letter to binary drive number
        JBE     SHORT BADDRV	;Valid drive numbers are 1-15
        CMP     AL,[CS:NUMDRV]
        JBE     SHORT HAVDRV
BADDRV:
        MOV     DL,-1
HAVDRV:
        STOSB           ;Put drive specifier in first byte
        INC     SI
        DEC     DI      ;Counteract next two instructions
NODRV:
        DEC     SI      ;Back up
        INC     DI      ;Skip drive byte
        MOV     CX,8
        CALL    GETWORD         ;Get 8-letter file name
        CMP     BYTE [SI],"."
        JNZ     SHORT NODOT
        INC     SI              ;Skip over dot if present
        MOV     CX,3            ;Get 3-letter extension
        CALL    MUSTGETWORD
NODOT:
        LDS     BX,[CS:SPSAVE]
        MOV     [BX+STKPTRS.SISAVE],SI
        MOV     AL,DL
        RETN

NONAM:
        ADD     DI,CX
        DEC     SI
        RETN

GETWORD:
        CALL    GETLET
        JBE     SHORT NONAM	;Exit if invalid character
        DEC     SI
MUSTGETWORD:
        CALL    GETLET
        JBE     SHORT FILLNAM
        JCXZ    MUSTGETWORD
        DEC     CX
        CMP     AL,"*"		;Check for ambiguous file specifier
        JNZ     SHORT NOSTAR
        MOV     AL,"?"
        REP     STOSB
NOSTAR:
        STOSB
        CMP     AL,"?"
        JNZ     SHORT MUSTGETWORD
        OR      DL,1            ;Flag ambiguous file name
        JMP     SHORT MUSTGETWORD
FILLNAM:
        MOV     AL," "
        REP     STOSB
        DEC     SI
RET21:  
	RETN

SCANB:
        LODSB
        CALL    SPCHK
        JZ      SHORT SCANB
        DEC     SI
        RETN

GETLET:
;Get a byte from [SI], convert it to upper case, and compare for delimiter.
;ZF set if a delimiter, CY set if a control character (other than TAB).
        LODSB
        AND     AL,7FH
        CMP     AL,"a"
        JB      SHORT CHK
        CMP     AL,"z"
        JA      SHORT CHK
        SUB     AL,20H          ;Convert to upper case
CHK:
        CMP     AL,"."
        JZ      SHORT RET21
        CMP     AL,'"'
        JZ      SHORT RET21
        CMP     AL,"/"
        JZ      SHORT RET21
        CMP     AL,"["
        JZ      SHORT RET21
        CMP     AL,"]"
        JZ      SHORT RET21
DELIM:
        CMP     AL,":"          ;Allow ":" as separator in IBM version
        JZ      SHORT RET21

        CMP     AL,"+"
        JZ      SHORT RET101
        CMP     AL,"="
        JZ      SHORT RET101
        CMP     AL,";"
        JZ      SHORT RET101
        CMP     AL,","
        JZ      SHORT RET101
SPCHK:
        CMP     AL,9            ;Filter out tabs too
        JZ      SHORT RET101
;WARNING! " " MUST be the last compare
        CMP     AL," "
RET101: 
	RETN

SETVECT: ; Interrupt call 37
        XOR     BX,BX
        MOV     ES,BX
        MOV     BL,AL
        SHL     BX,1
        SHL     BX,1
        MOV     [ES:BX],DX
        MOV     [ES:BX+2],DS
        RETN


NEWBASE: ; Interrupt call 38
        MOV     ES,DX
        LDS     SI,[CS:SPSAVE]
        MOV     DS,[SI+STKPTRS.CSSAVE]
        XOR     SI,SI
        MOV     DI,SI
        MOV     AX,[DS:2]
        MOV     CX,80H
        REP     MOVSW

SETMEM:

; Inputs:
;       AX = Size of memory in paragraphs
;       DX = Segment
; Function:
;       Completely prepares a program base at the 
;       specified segment.
; Outputs:
;       DS = DX
;       ES = DX
;       [0] has INT 20H
;       [2] = First unavailable segment ([ENDMEM])
;       [5] to [9] form a long call to the entry point
;       [10] to [13] have exit address (from INT 22H)
;       [14] to [17] have ctrl-C exit address (from INT 23H)
;       [18] to [21] have fatal error address (from INT 24H)
; DX,BP unchanged. All other registers destroyed.

        XOR     CX,CX
        MOV     DS,CX
        MOV     ES,DX
        MOV     SI,EXIT
        MOV     DI,SAVEXIT
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOV     [ES:2],AX
        SUB     AX,DX
        CMP     AX,MAXDIF
        JBE     SHORT HAVDIF
        MOV     AX,MAXDIF
HAVDIF:
        MOV     BX,ENTRYPOINTSEG
        SUB     BX,AX
        SHL     AX,1
        SHL     AX,1
        SHL     AX,1
        SHL     AX,1
        MOV     DS,DX
        MOV     [DS:6],AX
        MOV     [DS:8],BX
        MOV     WORD [DS:0],20CDH  ;"INT INTTAB"
        MOV     BYTE [DS:5],LONGCALL
        RETN

DATE16:
        PUSH    CX
        CALL    READTIME
        SHL     CL,1            ;Minutes to left part of byte
        SHL     CL,1
        SHL     CX,1            ;Push hours and minutes to left end
        SHL     CX,1
        SHL     CX,1
        SHR     DH,1            ;Count every two seconds
        OR      CL,DH           ;Combine seconds with hours and minutes
        MOV     DX,CX
        POP     CX
        MOV     AX,[MONTH]	;Fetch month and year
        SHL     AL,1		;Push month to left to make room for day
        SHL     AL,1
        SHL     AL,1
        SHL     AL,1
        SHL     AX,1
        OR      AL,[DAY]
RET22:  RETN

FOURYEARS	EQU     3*365+366

READTIME:
;Gets time in CX:DX. Figures new date if it has changed.
;Uses AX, CX, DX.
        CALL    BIOSSEG:BIOSGETTIME
        CMP     AX,[DAYCNT]     ;See if day count is the same
        JZ      SHORT RET22
        CMP     AX,FOURYEARS*30 ;Number of days in 120 years
        JAE     SHORT RET22	;Ignore if too large
        MOV     [DAYCNT],AX
        PUSH    SI
        PUSH    CX
        PUSH    DX              ;Save time
        XOR     DX,DX
        MOV     CX,FOURYEARS    ;Number of days in 4 years
        DIV     CX              ;Compute number of 4-year units
        SHL     AX,1
        SHL     AX,1
        SHL     AX,1            ;Multiply by 8 (no. of half-years)
        MOV     CX,AX           ;<240 implies AH=0
        MOV     SI,YRTAB	;Table of days in each year
        CALL    DSLIDE          ;Find out which of four years we're in
        SHR     CX,1            ;Convert half-years to whole years
        JNC     SHORT SK	;Extra half-year?
        ADD     DX,200
SK:
        CALL    SETYEAR
        MOV     CL,1            ;At least at first month in year
        MOV     SI,MONTAB       ;Table of days in each month
        CALL    DSLIDE          ;Find out which month we're in
        MOV     [MONTH],CL
        INC     DX              ;Remainder is day of month (start with one)
        MOV     [DAY],DL
        CALL    WKDAY           ;Set day of week
        POP     DX
        POP     CX
        POP     SI
RET23:  RETN

DSLIDE:
        MOV     AH,0
DSLIDE1:
        LODSB			;Get count of days
        CMP     DX,AX           ;See if it will fit
        JB	SHORT RET23	;If not, done
        SUB     DX,AX
        INC     CX              ;Count one more month/year
        JMP     SHORT DSLIDE1

SETYEAR:
;Set year with value in CX. Adjust length of February for this year.
        MOV     [YEAR],CL
CHKYR:
        TEST    CL,3            ;Check for leap year
        MOV     AL,28
        JNZ     SHORT SAVFEB	;28 days if no leap year
        INC     AL              ;Add leap day
SAVFEB:
        MOV     [MONTAB+1],AL   ;Store for February
        RETN

;Days in year
YRTAB:  DB      200,166         ;Leap year
        DB      200,165
        DB      200,165
        DB      200,165

;Days of each month
MONTAB: DB      31              ;January
        DB      28              ;February--reset each time year changes
        DB      31              ;March
        DB      30              ;April
        DB      31              ;May
        DB      30              ;June
        DB      31              ;July
        DB      31              ;August
        DB      30              ;September
        DB      31              ;October
        DB      30              ;November
        DB      31              ;December

GETDATE: ;Function call 42
        PUSH    CS
        POP     DS
        CALL    READTIME        ;Check for rollover to next day
        MOV     AX,[YEAR]
        MOV     BX,[DAY]
        LDS     SI,[SPSAVE]	;Get pointer to user registers
        MOV     [SI+STKPTRS.DXSAVE],BX  ;DH=month, DL=day
        ADD     AX,1980         ;Put bias back
        MOV     [SI+STKPTRS.CXSAVE],AX  ;CX=year
        MOV     AL,[CS:WEEKDAY]
RET24:  RETN

SETDATE: ;Function call 43
        MOV     AL,-1           ;Be ready to flag error
        SUB     CX,1980         ;Fix bias in year
        JC	SHORT RET24	;Error if not big enough
        CMP     CX,119          ;Year must be less than 2100
        JA	SHORT RET24
        OR      DH,DH
        JZ	SHORT RET24
        OR      DL,DL
        JZ	SHORT RET24	;Error if either month or day is 0
        CMP     DH,12           ;Check against max. month
        JA	SHORT RET24
        PUSH    CS
        POP     DS
        CALL    CHKYR           ;Set Feb. up for new year
        MOV     AL,DH
        MOV     BX,MONTAB-1
        XLAT                    ;Look up days in month
        CMP     AL,DL
        MOV     AL,-1           ;Restore error flag, just in case
        JB	SHORT RET24	;Error if too many days
        CALL    SETYEAR
        MOV     [DAY],DX	;Set both day and month
        SHR     CX,1
        SHR     CX,1
        MOV     AX,FOURYEARS
        MOV     BX,DX
        MUL     CX
        MOV     CL,[YEAR]
        AND     CL,3
        MOV     SI,YRTAB
        MOV     DX,AX
        SHL     CX,1            ;Two entries per year, so double count
        CALL    DSUM            ;Add up the days in each year
        MOV     CL,BH           ;Month of year
        MOV     SI,MONTAB
        DEC     CX              ;Account for months starting with one
        CALL    DSUM            ;Add up days in each month
        MOV     CL,BL           ;Day of month
        DEC     CX              ;Account for days starting with one
        ADD     DX,CX           ;Add in to day total
        XCHG    AX,DX           ;Get day count in AX
        MOV     [DAYCNT],AX
        CALL    BIOSSEG:BIOSSETDATE
WKDAY:
        MOV     AX,[DAYCNT]
        XOR     DX,DX
        MOV     CX,7
        INC     AX
        INC     AX              ;First day was Tuesday
        DIV     CX              ;Compute day of week
        MOV     [WEEKDAY],DL
        XOR     AL,AL           ;Flag OK
RET25:  RETN

DSUM:
        MOV     AH,0
        JCXZ    RET25
DSUM1:
        LODSB
        ADD     DX,AX
        LOOP    DSUM1
        RETN

GETTIME: ;Function call 44
        PUSH    CS
        POP     DS
        CALL    READTIME
        LDS     SI,[SPSAVE]	;Get pointer to user registers
        MOV     [SI+STKPTRS.DXSAVE],DX
        MOV     [SI+STKPTRS.CXSAVE],CX
        XOR     AL,AL
RET26:  RETN

SETTIME: ;Function call 45
;Time is in CX:DX in hours, minutes, seconds, 1/100 sec.
        MOV     AL,-1           ;Flag in case of error
        CMP     CH,24           ;Check hours
        JAE     SHORT RET26
        CMP     CL,60           ;Check minutes
        JAE     SHORT RET26
        CMP     DH,60           ;Check seconds
        JAE     SHORT RET26
        CMP     DL,100          ;Check 1/100's
        JAE     SHORT RET26
        CALL    BIOSSEG:BIOSSETTIME
        XOR     AL,AL
        RETN


; Default handler for division overflow trap
DIVOV:
        PUSH    SI
        PUSH    AX
        MOV     SI,DIVMES
        CALL    OUTMES
        POP     AX
        POP     SI
        INT     23H             ;Use Ctrl-C abort on divide overflow
        IRET

CODSIZ  EQU	$-CODSTRT       ;Size of code segment

ALIGN 2

;-------------------------------------------------------------------------------------
;***** DATA AREA *****
;-------------------------------------------------------------------------------------

CONSTRT EQU     $		;Start of constants segment

IONAME:
        DB 	"COM1","PRN ","LPT1","NUL ","AUX ","CON "

DIVMES:
	DB	13,10,"Divide overflow",13,10,"$"
CARPOS:
	DB	0
STARTPOS:
	DB	0
PFLAG:
	DB	0
DIRTYDIR:
	DB	0		;Dirty buffer flag
NUMDRV:
	DB	0		;Number of drives
NUMIO:
	DB	0		;Number of disk tables
VERFLG:
	DB	0		;Initialize with verify off
CONTPOS:
	DW	0
DMAADD:
	DW	80H		;User's disk transfer address (disp/seg)
        DW	0
ENDMEM:
	DW	0
MAXSEC:
	DW	0
BUFFER:
	DW	0
BUFSECNO:
	DW	0
BUFDRVNO:
	DB	-1
DIRTYBUF:
	DB	0
BUFDRVBP:
	DW	0
DIRBUFID:
	DW	-1
DAY:
	DB	0
MONTH:
	DB	0
YEAR:
	DW	0
DAYCNT:
	DW	-1
WEEKDAY:
	DB	0
CURDRV:
	DB	0		;Default to drive A
DRVTAB :
	DW	0		;Address of start of DPBs

;-------------------------------------------------------------------------------------
	db	0
;-------------------------------------------------------------------------------------
RETRODOSMSG:
	DB	13,10
	db	"Retro DOS v1.0 by Erdogan Tan [2018]"
	db	13,10,"$", 0 
;-------------------------------------------------------------------------------------

ALIGN 16

DOSLEN  EQU     CODSIZ+($-CONSTRT) ;Size of CODE + CONSTANTS segments

;-------------------------------------------------------------------------------------
; DATA	SEGMENT WORD
; Init code overlaps with data area below
;-------------------------------------------------------------------------------------
DATASEGMENT EQU $

INBUF	EQU DATASEGMENT		; DB 128 DUP (?)
CONBUF	EQU DATASEGMENT+128	; DB 131 DUP (?) ;The rest of INBUF and console buffer
LASTENT	EQU DATASEGMENT+259	; DW ?
EXITHOLD EQU DATASEGMENT+261	; DB 4 DUP (?)
FATBASE	EQU DATASEGMENT+265	; DW ?
NAME1	EQU DATASEGMENT+267	; DB 11 DUP (?)	;File name buffer
ATTRIB	EQU DATASEGMENT+278	; DB ?
NAME2	EQU DATASEGMENT+279	; DB 11 DUP (?)
NAME3	EQU DATASEGMENT+290	; DB 12 DUP (?)
EXTFCB	EQU DATASEGMENT+302	; DB ?
;WARNING - the following two items are accessed as a word
CREATING EQU DATASEGMENT+304	; DB ?
DELALL	EQU DATASEGMENT+305	; DB ?
TEMP	EQU DATASEGMENT+306	; LABEL WORD
SPSAVE  EQU DATASEGMENT+306	; DW ?
SSSAVE	EQU DATASEGMENT+308	; DW ?
CONTSTK	EQU DATASEGMENT+310	; DW ?
SECCLUSPOS EQU DATASEGMENT+312  ; DB ?	;Position of first sector within cluster
DSKERR	EQU DATASEGMENT+313	; DB ?
TRANS   EQU DATASEGMENT+314	; DB ?
PREREAD EQU DATASEGMENT+315	; DB ?	;0 means preread; 1 means optional
READOP	EQU DATASEGMENT+316	; DB ?
THISDRV EQU DATASEGMENT+317	; DB ?

;ALING 2	; EVEN

FCB	EQU DATASEGMENT+318	; DW ?	;Address of user FCB
NEXTADD	EQU DATASEGMENT+320	; DW ?
RECPOS	EQU DATASEGMENT+322	; DB 4 DUP (?)
RECCNT	EQU DATASEGMENT+326	; DW ?
LASTPOS	EQU DATASEGMENT+328	; DW ?
CLUSNUM	EQU DATASEGMENT+330	; DW ?
SECPOS	EQU DATASEGMENT+332	; DW ?	;Position of first sector accessed
VALSEC  EQU DATASEGMENT+334	; DW ?  ;Number of valid (previously written) sectors
BYTSECPOS EQU DATASEGMENT+336	; DW ?	;Position of first byte within sector
BYTPOS	EQU DATASEGMENT+338	; DB 4 DUP (?)	;Byte position in file of access
BYTCNT1 EQU DATASEGMENT+342	; DW ?	;No. of bytes in first sector
BYTCNT2	EQU DATASEGMENT+344	; DW ?	;No. of bytes in last sector
SECCNT	EQU DATASEGMENT+346	; DW ?	;No. of whole sectors
ENTFREE	EQU DATASEGMENT+348	; DW ?
				; DB 80H DUP (?)     ;Stack space
IOSTACK EQU DATASEGMENT+476	; LABEL   BYTE
        			; DB 80H DUP (?)
DSKSTACK EQU DATASEGMENT+604	; LABEL  BYTE 

DIRBUF	EQU DATASEGMENT+606	; LABEL WORD

;-------------------------------------------------------------------------------------
;Init code below overlaps with data area above
;-------------------------------------------------------------------------------------

DOSINIT:
        CLI
        CLD
        PUSH    CS
        POP     ES
        MOV     [ES:ENDMEM],DX
        LODSB                   ;Get no. of drives & no. of I/O drivers
        MOV     [ES:NUMIO],AL
        MOV     DI,MEMSTRT
PERDRV:
        MOV     BP,DI
        MOV     AL,[ES:DRVCNT]
        STOSB           ;DEVNUM
        LODSB           ;Physical unit no.
        STOSB           ;DRVNUM
        CMP     AL,15
        JA      BADINIT
        CBW             ;Index into FAT size table
        SHL     AX,1
        ADD     AX,FATSIZTAB
        XCHG    BX,AX
        LODSW           ;Pointer to DPT
        PUSH    SI
        MOV     SI,AX
        LODSW
        STOSW           ;SECSIZ
        MOV     DX,AX
        CMP     AX,[ES:MAXSEC]
        JBE     SHORT NOTMAX
        MOV     [ES:MAXSEC],AX
NOTMAX:
        LODSB
        DEC     AL
        STOSB           ;CLUSMSK
        JZ      SHORT HAVSHFT
        CBW
FIGSHFT:
        INC     AH
        SAR     AL,1
        JNZ     SHORT FIGSHFT
        MOV     AL,AH
HAVSHFT:
        STOSB           ;CLUSSHFT
        MOVSW           ;FIRFAT (= number of reserved sectors)
        MOVSB           ;FATCNT
        MOVSW           ;MAXENT
        MOV     AX,DX 	;SECSIZ again
        MOV     CL,5
        SHR     AX,CL
        MOV     CX,AX	;Directory entries per sector
        DEC     AX
        ADD     AX,[ES:BP+DPBLOCK.MAXENT]
        XOR     DX,DX
        DIV     CX
        STOSW		;DIRSEC (temporarily)
        MOVSW		;DSKSIZ (temporarily)

;FNDFATSIZ:
;       MOV     AL,1
;       MOV     DX,1
;GETFATSIZ:
;       PUSH    DX
;       CALL    FIGFATSIZ
;       POP     DX
;       CMP     AL,DL           ;Compare newly computed FAT size with trial
;       JZ      SHORT HAVFATSIZ ;Has sequence converged?
;       CMP     AL,DH           ;Compare with previous trial
;       MOV     DH,DL
;       MOV     DL,AL           ;Shuffle trials
;       JNZ     SHORT GETFATSIZ ;Continue iterations if not oscillating
;       DEC     WORD [ES:BP.DSKSIZ] ;Damp those oscillations
;       JMP     SHORT FNDFATSIZ ;Try again

	;	
	; Retro DOS v1.0 - 12/02/2018
	LODSB	; Media Descriptor byte
	LODSW	; FAT size in sectors
	;

HAVFATSIZ:
        STOSB                   ;FATSIZ
        MUL     BYTE [ES:BP+DPBLOCK.FATCNT]  ;Space occupied by all FATs
        ADD     AX,[ES:BP+DPBLOCK.FIRFAT]
        STOSW                   ;FIRDIR
        ADD     AX,[ES:BP+DPBLOCK.DIRSEC]
        MOV     [ES:BP+DPBLOCK.FIRREC],AX	;Destroys DIRSEC
        CALL    FIGMAX
        MOV     [ES:BP+DPBLOCK.MAXCLUS],CX
        MOV     AX,BX           ;Pointer into FAT size table
        STOSW                   ;Allocate space for FAT pointer
        MOV     AL,[ES:BP+DPBLOCK.FATSIZ]
        XOR     AH,AH
        MUL     WORD [ES:BP+DPBLOCK.SECSIZ]
        CMP     AX,[ES:BX]      ;Bigger than already allocated
        JBE     SHORT SMFAT
        MOV     [ES:BX],AX
SMFAT:
        POP     SI              ;Restore pointer to init. table
        MOV     AL,[ES:DRVCNT]
        INC     AL
        MOV     [ES:DRVCNT],AL
        CMP     AL,[ES:NUMIO]
        JAE	SHORT CONTINIT
        JMP	PERDRV  

BADINIT:
        MOV     SI,BADMES
        CALL    OUTMES
        STI
        HLT

CONTINIT:
        PUSH    CS
        POP     DS
;Calculate true address of buffers, FATs, free space
        MOV     BP,[MAXSEC]
        MOV     AX,DIRBUF
        ADD     AX,BP
        MOV     [BUFFER],AX     ;Start of buffer
        ADD     AX,BP
        MOV     [DRVTAB],AX     ;Start of DPBs
        SHL     BP,1            ;Two sectors - directory and buffer
        ADD     BP,DI           ;Allocate buffer space
        ADD     BP,ADJFAC       ;True address of FATs
        PUSH    BP
        MOV     SI,FATSIZTAB
        MOV     DI,SI
        MOV     CX,16
TOTFATSIZ:
        INC     BP              ;Add one for Dirty byte
        INC     BP              ;Add one for I/O device number
        LODSW                   ;Get size of this FAT
        XCHG    AX,BP
        STOSW                   ;Save address of this FAT
        ADD     BP,AX           ;Compute size of next FAT
        CMP     AX,BP           ;If size was zero done
        LOOPNZ  TOTFATSIZ
        MOV     AL,15
        SUB     AL,CL           ;Compute number of FATs used
        MOV     [NUMDRV],AL
        XOR     AX,AX           ;Set zero flag
        REPZ    SCASW           ;Make sure all other entries are zero
        JNZ     SHORT BADINIT
        ADD     BP,15           ;True start of free space
        MOV     CL,4
        SHR     BP,CL           ;First free segment
        MOV     DX,CS
        ADD     DX,BP
        MOV     BX,0FH
        MOV     CX,[ENDMEM]
        CMP     CX,1            ;Use memory scan?
        JNZ     SHORT SETEND
        MOV     CX,DX           ;Start scanning just after DOS
MEMSCAN:
        INC     CX
        ;JZ	SHORT SETEND
        JZ	SHORT SETEND0
	MOV     DS,CX
        MOV     AL,[BX]
        NOT     AL
        MOV     [BX],AL
        CMP     AL,[BX]
        NOT     AL
        MOV     [BX],AL
        JZ      SHORT MEMSCAN
SETEND0:
        MOV	[CS:ENDMEM],CX
SETEND:
        MOV     BP,CS
; BP has segment of DOS (whether to load high or run in place)
; DX has program segment (whether after DOS or overlaying DOS)
; CX has size of memory in paragraphs (reduced by DOS size if HIGHMEM)
        ;MOV	[CS:ENDMEM],CX
        XOR     AX,AX
        MOV     DS,AX
        MOV     ES,AX
        MOV     DI,INTBASE
        MOV     AX,QUIT
        STOSW			;Set abort address--displacement
        MOV     AX,BP
        MOV     BYTE [ENTRYPOINT],LONGJUMP
        MOV     WORD [ENTRYPOINT+1],ENTRY
        MOV     WORD [ENTRYPOINT+3],AX
        MOV     WORD [0],DIVOV   ;Set default divide trap address
        MOV     [2],AX
        MOV     CX,9
        REP	STOSW		;Set 5 segments (skip 2 between each)
        MOV     WORD [INTBASE+4],COMMAND
        MOV     WORD [INTBASE+12],IRET   ;Ctrl-C exit
        MOV     WORD [INTBASE+16],IRET   ;Fatal error exit
        MOV     AX,BIOSREAD
        STOSW
        MOV     AX,BIOSSEG
        STOSW
        STOSW			;Add 2 to DI
        STOSW
	MOV	AX,BIOSWRITE
        MOV     WORD [INTBASE+18H],AX
        MOV     WORD [EXIT],100H
        MOV     WORD [EXIT+2],DX
        MOV     SI,HEADER
        CALL    OUTMES
        PUSH    CS
        POP     DS
        PUSH    CS
        POP     ES
;Move the FATs into position
        MOV     AL,[NUMIO]
        CBW
        XCHG    AX,CX
        MOV     DI,MEMSTRT+DPBLOCK.FAT
FATPOINT:
        MOV     SI,[DI]			;Get address within FAT address table
        MOVSW                           ;Set address of this FAT
        ADD     DI,DPBSIZ-2             ;Point to next DPB
        LOOP    FATPOINT
        POP     CX                      ;True address of first FAT
        MOV     SI,MEMSTRT		;Place to move DPBs from
        MOV     DI,[DRVTAB]             ;Place to move DPBs to
        SUB     CX,DI                   ;Total length of DPBs
        CMP     DI,SI
        JBE     SHORT MOVJMP		;Are we moving to higher or lower memory?
        DEC     CX                      ;Move backwards to higher memory
        ADD     DI,CX
        ADD     SI,CX
        INC     CX
        STD
MOVJMP:
        MOV     ES,BP
        ;JMP	SHORT MOVFAT

MOVFAT:
;This section of code is safe from being overwritten by block move
        REP     MOVSB
        CLD
        MOV     [ES:DMAADD+2],DX
        MOV     SI,[DRVTAB]     ;Address of first DPB
        MOV     AL,-1
        MOV     CL,[NUMIO]      ;Number of DPBs
FLGFAT:
        MOV     DI,[ES:SI+DPBLOCK.FAT]  ;get pointer to FAT
        DEC     DI              ;Point to dirty byte
        STOSB                   ;Flag as unused
        ADD     SI,DPBSIZ       ;Point to next DPB
        LOOP    FLGFAT
        MOV     AX,[ENDMEM]
        CALL    SETMEM          ;Set up segment

        RETF

;FIGFATSIZ:
;	MUL     BYTE [ES:BP+DPBLOCK.FATCNT]
;	ADD     AX,[ES:BP+DPBLOCK.FIRFAT]
;	ADD     AX,[ES:BP+DPBLOCK.DIRSEC]

FIGMAX:
;AX has equivalent of FIRREC
        SUB     AX,[ES:BP+DPBLOCK.DSKSIZ]
        NEG     AX
        MOV     CL,[ES:BP+DPBLOCK.CLUSSHFT]
        SHR     AX,CL
        INC     AX
        MOV     CX,AX           ;MAXCLUS
        INC     AX
        MOV     DX,AX
        SHR     DX,1
        ADC     AX,DX           ;Size of FAT in bytes
        MOV     SI,[ES:BP+DPBLOCK.SECSIZ]
        ADD     AX,SI
        DEC     AX
        XOR     DX,DX
        DIV     SI
        RETN

BADMES:
        DB      13,10,"INIT TABLE BAD",13,10,"$"

FATSIZTAB:
        DW      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

DRVCNT  DB      0

ALIGN 16

MEMSTRT:
ADJFAC  EQU     DIRBUF-MEMSTRT
