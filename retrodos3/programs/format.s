; ***************************************************************************
; FORMAT.COM (MSDOS 2.0 FORMAT utility) - for RETRO DOS v2.0 by ERDOGAN TAN
; ---------------------------------------------------------------------------
; Last Update: 05/05/2018
; ---------------------------------------------------------------------------
; Beginning: 05/05/2018 
; ---------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ---------------------------------------------------------------------------
;	    ((nasm format.s -l format.lst -o FORMAT.COM)) 	
; ---------------------------------------------------------------------------
; Derived from 'FORMAT.ASM' file of MSDOS 2.0 (IBM PCDOS v2.0) source code
; by Microsoft, 1983
; ***************************************************************************
; FORMAT.COM (1983) source files:
; 	FORMAT.ASM, FORMES.ASM, GENFOR.ASM  (OEM file)

;============================================================================
; DOSSYM.ASM - (MSDOS 2.0/2.11, 1983)
;============================================================================

DIRSTRLEN       EQU     64	; Max length in bytes of directory strings

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

attr_read_only      EQU      1h
attr_hidden         EQU      2h
attr_system         EQU      4h
attr_volume_id      EQU      8h
attr_directory      EQU     10h
attr_archive        EQU     20h

;============================================================================
; FORMAT.ASM - 01/05/1983 (MSDOS 2.11)
;============================================================================

;****************************************************************************
;
;       86-DOS FORMAT DISK UTILITY
;
;       This routine formats a new disk,clears the FAT and DIRECTORY
;       then optionally copies the SYSTEM and COMMAND.COM to this
;       new disk
;
;       SYNTAX: FORMAT  [drive][/switch1][/switch2]...[/switch16]
;
;       Regardless of the drive designator , the user will be
;       prompted to insert the diskette to be formatted.
;
;****************************************************************************

;Mod to ask for volume ID ARR 5/12/82
; 05/19/82 Fixed rounding bug in CLUSCAL:       ARR
;REV 1.5
;               Added rev number message
;               Added dir attribute to DELALL FCB
;REV 2.00
;               Redone for 2.0
;REV 2.10
;               5/1/83 ARR Re-do to transfer system on small memory systems

;FALSE   EQU     0
;TRUE    EQU     NOT FALSE

;IBMJAPVER EQU   FALSE           ; SET ONLY ONE SWITCH TO TRUE!
;IBMVER  EQU     FALSE
;MSVER   EQU     TRUE

;KANJI   EQU     FALSE

        ;.xlist
        ;INCLUDE DOSSYM.ASM
        ;.list


;FORMAT Pre-defined switches
SYSSW   EQU     1               ; System transfer
VOLSW   EQU     2               ; Volume ID prompt
OLDSW   EQU     4               ; E5 dir terminator


DRNUM   EQU     5CH

;Per system file data structure

struc FILE
.HANDLE: RESW	1		; Source handle
.SIZEP:	 RESW	1		; File size in para
.SIZEB:	 RESD	1		; File size in bytes
.OFFSET: RESD	1		; Offset in file (partial)
.START:	 RESW	1		; Para number of start in buffer
.DATE:	 RESW	1		; Date of file
.TIME:	 RESW	1		; Time of file
.NAME:	 RESB	1		; Start of name
endstruc

[BITS 16]
[ORG 100H]

_START:
        JMP     SHORT FSTRT

HEADER  DB      "Vers 2.10"

FSTRT:
        MOV     SP,STACK	;Use internal stack

;Code to print header
;       PUSH    AX
;       MOV     DX,HEADER
;       CALL    PRINT
;       POP     AX

DOSVER_HIGH     EQU  020BH   ;2.11 in hex

        PUSH    AX              ;Save DRIVE validity info
        MOV     AH,GET_VERSION
        INT     21H
        XCHG    AH,AL           ;Turn it around to AH.AL
        CMP     AX,DOSVER_HIGH
        JAE	short OKDOS
GOTBADDOS:
        MOV     DX,BADVER
        MOV     AH,STD_CON_STRING_OUTPUT
        INT     21H
        INT     20H

OKDOS:
	POP     AX

        CMP     AL,0FFH                 ;See if invalid drive specified
        JNZ	short DRVGD		;If not proceed
        MOV     DX,INVDRV		;Invalid drive message
        CALL    PRINT                   ;Print the message
        JMP	FEXIT2			;Exit
DRVGD:
        MOV     AH,GET_DEFAULT_DRIVE    ;Must get the default drive
        INT     21H                     ;Default now in AL
        MOV     [DEFALT],AL		;Save for later
        ADD     AL,"A"
	; Retro DOS v2.0 modification (05/05/2018)
	;MOV	[BIODRV],AL
	;MOV	[DOSDRV],AL	
        MOV     [KERNELDRV],AL
        MOV     [SYSDRV],AL
        MOV     [COMDRV],AL
        MOV     SI,DRNUM		;So we can get our parameters
        LODSB                           ;Fetch drive designation
        OR      AL,AL                   ;See if specified
        JNZ	short DRVSPEC		;If specified proceed
        MOV     AL,[DEFALT]
        INC     AL
DRVSPEC:
        DEC     AL                      ;Drive designator now correct
        MOV     [DRNUM],AL  		;And updated
        MOV     [DRIVE],AL		;Save copy
        MOV     DX,INT_23
        MOV     AH,SET_INTERRUPT_VECTOR
        MOV     AL,23H
        INT     21H                     ;Set ^C vector
        ;Get all the switch information from the command line
        XOR     AX,AX
        MOV     AH,CHAR_OPER            ;GET SWITCH CHARACTER
        INT     21H                     ;CALL THE DOS
        MOV     [SWTCH],DL

        XOR     BX,BX                   ;Store switch information in BX
        MOV     SI,81H                  ;Point to the command line buffer
NXTSWT:
        CALL    SCANOFF
        LODSB
        CMP     AL,[SWTCH]
        JZ	short GETPARM
        CMP     AL,13
        JZ	short SAVSWT
        LODSB                           ;Get next character
        CMP     AL,":"                  ;Is it a drive specifier?
        JNZ	short INVALID		;No -- invalid parameter
        CMP     BYTE [DBLFLG],0		;Is it the only drive specifier we've seen
        JNZ	short INVALID 		;No -- invalid parameter
        INC     BYTE [DBLFLG]		;Yes -- set the flag
        JMP     SHORT NXTSWT
GETPARM:
        LODSB
        ;Convert any lower case input into upper case
        CMP     AL,41H
        JL	short GETCHR		;Switch is a digit don't try to convert it
        AND     AL,0DFH
GETCHR:
        MOV     CL,[SWITCHLIST]		;Number of legal switches
        OR      CL,CL                   ;If it's none we shouldn't be here
        JZ	short INVALID		;Report the error
        MOV     CH,0
        MOV     DI,SWITCHLIST+1		;Point to the legal switch characters
        REPNE   SCASB
        JNZ	short INVALID
        MOV     AX,1
        SHL     AX,CL
        OR      BX,AX                   ;Set the appropriate bit in SWITCHMAP
        JMP     SHORT NXTSWT            ;See if there are anymore

INVALID:
        MOV     DX,INVPAR
        CALL    PRINT
        JMP	FEXIT

SCANOFF:
        LODSB
        CMP     AL,20H
        JZ	short SCANOFF
        CMP     AL,9
        JZ	short SCANOFF
        DEC     SI
        RETN

MEMERR:
        MOV     DX,MEMEX
        CALL    PRINT
        JMP     FEXIT

SAVSWT:
        MOV     [SWITCHMAP],BX
        TEST    word [SWITCHMAP],SYSSW
        JZ	short INITCALL
        CALL    SAVUDIRS
        MOV     BX,[FREESPACE]
        ADD     BX,15
        MOV     CL,4
        SHR     BX,CL
        PUSH    CS
        POP     ES
        MOV     AH,SETBLOCK
        INT     21H
        MOV     BX,0FFFFH
        MOV     AH,ALLOC
        INT     21H
        OR      BX,BX
        JZ	short MEMERR		;No memory
        MOV     [MSIZE],BX
        MOV     AH,ALLOC
        INT     21H
        JC	short MEMERR		;No memory
        MOV     [MSTART],AX
        MOV     DX,SWTCH
        MOV     AH,CHDIR
        INT     21H                     ;Go to root on default drive (source)

RDFRST:
        CALL    READDOS                 ;Read BIOS and DOS
        JNC	short INITCALL		;OK -- read next file
NEEDSYS:
        CALL    SYSPRM                  ;Prompt for system disk
        JMP	short RDFRST		;Try again

INITCALL:
        CALL    INIT                    ;Let OEM read any files before disk is changed
        JNC	short SWITCHCHK
        MOV     DX,FRMTERR
        CALL    PRINT
        JMP	FEXIT

SWITCHCHK:
        MOV     DX,[SWITCHMAP]
        MOV     [SWITCHCOPY],DX

SYSLOOP:
        MOV     WORD [BADSIZ],0		;Must intialize for each iteration
        MOV     WORD [BADSIZ+2],0
        MOV     WORD [SYSSIZ],0
        MOV     WORD [SYSSIZ+2],0
        MOV     BYTE [DBLFLG],0
        MOV     BYTE [CLEARFLG],0
        MOV     DX,[SWITCHCOPY]
        MOV     [SWITCHMAP],DX		;Restore original Switches
        MOV     AL,[DRIVE]		;Fetch drive
        ADD     AL,"A"                  ;(AL)= ASCII designation
        MOV     [SNGDRV],AL		;Fill out the message
        MOV     [TARGDRV],AL
        MOV     [HRDDRV],AL
        CALL    DSKPRM                  ;Prompt for new disk
        CALL    DISKFORMAT              ;Format the disk
        JNC	short GETTRK
FRMTPROB:
        MOV     DX,FRMTERR
        CALL    PRINT
        JMP     SHORT SYSLOOP

        ;Mark any bad sectors in the FATs
        ;And keep track of how many bytes there are in bad sectors

GETTRK:
        CALL    BADSECTOR               ;Do bad track fix-up
        JC	short FRMTPROB		;Had an error in Formatting - can't recover
        CMP     AX,0                    ;Are we finished?
        JNZ	short TRKFND		;No - check error conditions
        JMP	DRTFAT			;Yes
TRKFND:
        CMP     BX,[STARTSECTOR]	;Are any sectors in the system area bad?
        JGE	short CLRTEST
        MOV     DX,NOUSE		;Can't build FATs of Directory
        CALL    PRINT
        JMP	short FRMTPROB		;Bad disk -- try again
CLRTEST:
        MOV     [SECTORS],AX		;Save the number of sectors on the track
        CMP     BYTE [CLEARFLG],0	;Have we already cleared the FAT and DIR?
        JNZ	short SYSTEST		;Yes - all set
        INC	byte [CLEARFLG]		;Set the flag
        PUSH    BX
        CALL    CLEAR                   ;Fix-up fat and directory
        POP     BX
SYSTEST:
        TEST    word [SWITCHMAP],SYSSW 	;If system requested calculate size
        JZ	short BAD100
        CMP     BYTE [DBLFLG],0		;Have we already calculated System space?
        JNZ	short CMPTRKS		;Yes -- all ready for the compare
        INC     BYTE [DBLFLG]		;No -- set the flag
        CALL    GETSIZE                 ;Calculate the system size
        MOV     DX,[SYSSIZ+2]
        MOV     AX,[SYSSIZ]
        DIV     word [SECSIZ]
        ADD     AX,[STARTSECTOR]
        MOV     [SYSTRKS],AX		;Space FAT,Dir,and system files require
CMPTRKS:
        CMP     BX,[SYSTRKS]
        JG	short BAD100
        MOV     DX,NOTSYS		;Can't transfer a system
        CALL    PRINT
        AND     word [SWITCHMAP],~SYSSW ;Turn off system transfer switch
        MOV     WORD [SYSSIZ+2],0	;No system to transfer
        MOV     WORD [SYSSIZ],0		;No system to transfer
BAD100:
; BX is the first bad sector #, SECTORS is the number of bad sectors starting
; at BX. This needs to be converted to clusters. The start sector number may
; need to be rounded down to a cluster boundry, the end sector may need to be
; rounded up to a cluster boundry. Know BX >= STARTSECTOR
        SUB     BX,[STARTSECTOR]	; BX is now DATA area relative
        MOV     CX,BX
        ADD     CX,[SECTORS]
        DEC     CX			; CX is now the last bad sector #
        MOV     AX,BX
        XOR     DX,DX
        DIV     word [CLUSSIZ]
        MOV     BX,AX                   ; BX is rounded down and converted
                                        ; to a cluster #. Where cluster 0 =
                                        ; first cluster of data. First bad
                                        ; Sector is in cluster BX.
        MOV     AX,CX
        XOR     DX,DX
        DIV     word [CLUSSIZ]
        MOV     CX,AX                   ; CX is rounded up and converted to a
                                        ; to a cluster #. Where cluster 0 =
                                        ; first cluster of data. Last bad
                                        ; Sector is in cluster CX.
        SUB     CX,BX
        INC     CX                      ; CX is number of clusters to mark bad
        ADD     BX,2                    ; Bias start by correct amount since
                                        ; first cluster of data is really
                                        ; cluster 2.
        MOV     AX,[CLUSSIZ]		; Sectors/Cluster
        MUL	word [SECSIZ]		; Times Bytes/Sector
        MOV     BP,AX                   ; = Bytes/Cluster

; Mark CX clusters bad starting at cluster BX
PACKIT:
        MOV     DX,0FF7H                ;0FF7H indicates a bad sector
        CALL    PACK                    ;Put it in the allocation map
        CMP     DX,DI                   ;Have we already marked it bad?
        JZ	short BAD150		;if so, don't add it in
        ADD     [BADSIZ],BP		;Add in number of bad bytes
        JNB	short BAD150
        INC     WORD [BADSIZ+2]
BAD150:
        INC     BX                      ;Next cluster
        LOOP    PACKIT                  ;Continue for # of clusters
        JMP	GETTRK

; Inputs:
        ;BX = Cluster number
        ;DX = Data
; Outputs:
        ;The data is stored in the FAT at the given cluster.
        ;SI is destroyed
        ;DI contains the former contents
        ;No other registers affected
PACK:
        PUSH    BX
        PUSH    CX
        PUSH    DX
        MOV     SI,BX
        SHR     BX,1
        ADD     BX,[FATSPACE]
        ADD     BX,SI
        SHR     SI,1
        MOV     SI,[BX]
        MOV     DI,SI
        JNB	short ALIGNED
        MOV     CL,4
        SHL     DX,CL
        SHR     DI,CL
        AND     SI,15
        JMP     SHORT PACKIN

ALIGNED:
        AND     SI,0F000H
PACKIN:
        AND     DI,00FFFH               ;DI CONTAINS FORMER CONTENTS
        OR      SI,DX
        MOV     [BX],SI
        POP     DX
        POP     CX
        POP     BX
        RETN

DRTFAT:
        CMP     BYTE [CLEARFLG],0
        JNZ	short CLEARED
        CALL    CLEAR                   ;Clear the FAT and Dir
        TEST    word [SWITCHMAP],SYSSW	;If system requested, calculate size
        JZ	short CLEARED
        CMP     BYTE [DBLFLG],0		;Have we already calculated System space?
        JNZ	short CLEARED		;Yes
        INC     BYTE [DBLFLG]		;No -- set the flag
        CALL    GETSIZE                 ;Calculate the system size
CLEARED:
        CALL    WRTFAT
        JNC	short FATWRT
        MOV     DX,NOUSE
        CALL    PRINT
        JMP	FRMTPROB

FATWRT:
        TEST    word [SWITCHMAP],SYSSW	;System desired
        JZ	short STATUS
        CALL    WRITEDOS                ;Write the BIOS & DOS
        JNC	short SYSOK
        MOV     DX,NOTSYS		;Can't transfer a system
        CALL    PRINT
        MOV     WORD [SYSSIZ+2],0	;No system transfered
        MOV     WORD [SYSSIZ],0		;No system transfered
        JMP     SHORT STATUS

SYSOK:
        MOV     DX,SYSTRAN
        CALL    PRINT
STATUS:
        CALL    CRLF
        CALL    VOLID
        MOV     AH,DISK_RESET
        INT     21H
        CALL    DONE                    ;Final call to OEM module
        JNC	short REPORTC
        JMP	FRMTPROB		;Report an error

REPORTC:
        CALL    REPORT

        CALL    MORE                    ;See if more disks to format
        JMP	SYSLOOP			;If we returned from MORE then continue

DISP32BITS:
        PUSH    BX
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
        ; Conversion complete. Print 8-digit number with 2 leading blanks.
        MOV     CX,1810H
        XCHG    DX,AX
        CALL    DIGIT
        XCHG    AX,BX
        CALL    OUTWORD
        XCHG    AX,BP
        CALL    OUTWORD
        POP     DX
        CMP     DX,0
        JZ	short RET3
        CALL    PRINT
RET3:   
	RETN

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
        MOV     AH,STD_CON_OUTPUT
        INT     21H
        RETN

CONVWRD:
        ADC     AL,AL
        DAA
        XCHG    AL,AH
        ADC     AL,AL
        DAA
        XCHG    AL,AH
RET2:   
	RETN

UNSCALE:
        SHR     CX,1
        JC	short RET2
        SHL     AX,1
        RCL     DX,1
        JMP     SHORT UNSCALE


;******************************************
; Calculate the size in bytes of the system rounded up to sector and
;   cluster boundries, Answer in SYSSIZ

; 05/05/2018
; Retro DOS v2.0 modification (BIOS+DOS KERNEL file = "MSDOS.SYS")

GETSIZE:
        MOV     AX,[KERNELSIZB]		;And calculate the system size
        MOV     DX,[KERNELSIZB+2]
        CALL    FNDSIZ
        MOV     AX,[COMSIZB]
        MOV     DX,[COMSIZB+2]

;Calculate the number of sectors used for the system
FNDSIZ:
        DIV	word [SECSIZ]
        OR      DX,DX
        JZ	short FNDSIZ0
        INC     AX                      ; Round up to next sector
FNDSIZ0:
        PUSH    AX
        XOR     DX,DX
        DIV	word [CLUSSIZ]
        POP     AX
        OR      DX,DX
        JZ	short ONCLUS
        SUB     DX,[CLUSSIZ]
        NEG     DX
        ADD     AX,DX                   ; Round up sector count to cluster
                                        ;       boundary
ONCLUS:
        MUL	word [SECSIZ]		; Turn it back into bytes
        ADD     [SYSSIZ],AX
        ADC     [SYSSIZ+2],DX
        RETN

PRINT:  MOV     AH,STD_CON_STRING_OUTPUT ;Print msg pointed to by DX
        INT     21H
        RETN

MORE:   CMP     BYTE [HARDFLAG],0	;Check if removable media
        JNZ	short FEXIT
        CALL    WAITYN                  ;Get yes or no response
        JB	short FEXIT		;Exit if CF=1
        CALL    CRLF
CRLF:
        MOV     DX,CRLFMSG
        CALL    PRINT
        RETN

PERROR: CALL    PRINT                   ;Print message and exit
FEXIT:
        CALL    RESTUDIR                ;Restore users dirs
FEXIT2:
        INT     20H

        ;Prompt the user for a system diskette in the default drive
SYSPRM:
        MOV     AH,GET_DEFAULT_DRIVE    ;Will find out the default drive
        INT     21H                     ;Default now in AL
	ADD     AL,41H                  ;Now in Ascii
        MOV     [SYSDRV],AL		;Text now ok

        MOV     DX,SYSMSG
        CALL    PRINT                   ;Print first line
        CALL    WAITKY                  ;Wait for a key
        CALL    CRLF
        RETN

TARGPRM:
        MOV     DX,TARGMSG
        CALL    PRINT                   ;Print first line
        CALL    WAITKY                  ;Wait for a key
        CALL    CRLF
        RETN

DSKPRM:
        MOV     DX,SNGMSG		;Point to the message
        CMP     BYTE [HARDFLAG],0	;Check if removable media
        JZ	short GOPRNIT
        MOV     DX,HRDMSG
GOPRNIT:
        CALL    PRINT                   ;Print the message
        CALL    WAITKY                  ;Wait for space bar
        CALL    CRLF
        CALL    CRLF
        RETN

        ;Will wait for any key to be depressed.
WAITKY:
        MOV     AX,(STD_CON_INPUT_FLUSH*256) | STD_CON_INPUT_NO_ECHO
        INT     21H
        MOV     AX,(STD_CON_INPUT_FLUSH*256) + 0
        INT     21H
	RETN

FDPB:   MOV     DL,[DRIVE]
        INC     DL
        MOV     AH,GET_DPB
        PUSH    DS
        INT     21H
        INC     AL
        JZ	short DRVERR
        MOV     DX,[BX+13]
        DEC     DX
        MOV     AL,[BX+4]
        INC     AL
        MOV     CX,[BX+2]
        POP     DS
        RETN
DRVERR:
        POP     DS
        MOV     DX,INVDRV
        JMP	short PERROR

        ;Clear the FAT and directory and set Dirty byte in the FAT
CLEAR:
        MOV     AL,[FATID]
        OR      AL,0F8H                 ;Make sure it's a legal value
        MOV     AH,0FFH
        MOV     DI,[FATSPACE]
        MOV     [DI],AX
        MOV     [DI+2],AH
        MOV     AH,DISK_RESET
        INT     21H
        CALL    WRTFAT

        CALL    FDPB
        MOV     [FDSKSIZ],DX
        MOV     [SECSIZ],CX
        MOV     AH,0
        MOV     [CLUSSIZ],AX
        SHR     DX,1
        JNC	short ROUNDED
        INC     DX
ROUNDED:
        ADD     DX,[FDSKSIZ]
        XOR     AX,AX
        MOV     CX,DX
        MOV     DI,[FATSPACE]
        ADD     DI,3
        REP     STOSB
        MOV     AH,DISK_RESET
        INT     21H
        CALL    WRTFAT
        MOV     DL,[DRIVE]
        ADD     DL,'A'
        MOV     [ROOTSTR],DL
        MOV     DX,ROOTSTR
        MOV     AH,CHDIR
        INT     21H                     ;Go to root on target drive
        MOV     AL,[DRIVE]
        INC     AL
        MOV     [ALLDRV],AL
        MOV     AH,FCB_DELETE
        MOV     DX,ALLFILE
        INT     21H

        TEST    word [SWITCHMAP],OLDSW	;See if E5 terminated DIR requested
        JZ	short RET25
        MOV     AL,[DRIVE]
        INC     AL
        MOV     [CLEANFILE],AL		;Get the drive
        MOV     DX,CLEANFILE
        MOV     AH,FCB_CREATE
MAKE_NEXT:
        INT     21H
        OR      AL,AL
        JNZ	short DELETE_THEM
        INC     BYTE [CLNNAM]
        CMP     BYTE [CLNNAM],"Z"+1
        JNZ	short MAKE_NEXT
        MOV     BYTE [CLNNAM],"A"
        INC     BYTE [CLNNAM+1]
        CMP     BYTE [CLNNAM+1],"Z"+1
        JNZ     short MAKE_NEXT
        MOV     BYTE [CLNNAM+1],"A"
        INC     BYTE [CLNNAM+2]
        JMP     short MAKE_NEXT

DELETE_THEM:
        MOV     WORD [CLNNAM],"??"
        MOV     BYTE [CLNNAM+2],"?"
        MOV     AH,FCB_DELETE
        INT     21H
RET25:
	RETN				;And return

;*****************************************
; Process V switch if set

VOLID:
        TEST    word [SWITCHMAP],VOLSW
        JNZ	short DOVOL
VRET:   
	CLC
        RETN

DOVOL:
        PUSH    CX
        PUSH    SI
        PUSH    DI
        PUSH    ES
        PUSH    DS
        POP     ES
VOL_LOOP:
        MOV     AL,[DRIVE]
        INC     AL
        MOV     [VOLFCB+7],AL
        MOV     DX,LABPRMT
        CALL    PRINT
        MOV     DX,INBUFF
        MOV     AH,STD_CON_STRING_INPUT
        INT     21H
        MOV     DX,CRLFMSG
        CALL    PRINT
        MOV     DX,CRLFMSG
        CALL    PRINT
        MOV     CL,[INBUFF+1]
        OR      CL,CL
        JZ	short VOLRET
        XOR     CH,CH
        MOV     SI,INBUFF+2
        MOV     DI,SI
        ADD     DI,CX
        MOV     CX,11
        MOV     AL,' '
        REP     STOSB
        MOV     CX,5
        MOV     DI,VOLNAM
        REP     MOVSW
        MOVSB
        MOV     DX,VOLFCB
        MOV     AH,FCB_CREATE
        INT     21H
        OR      AL,AL
        JZ	short GOOD_CREATE
        MOV     DX,INVCHR		;PRINT INVALID CHARS MESSAGE
        CALL    PRINT
        JMP	short VOL_LOOP
GOOD_CREATE:
        MOV     DX,VOLFCB
        MOV     AH,FCB_CLOSE
        INT     21H
        CALL    CRLF
VOLRET:
        POP     ES
        POP     DI
        POP     SI
        POP     CX
        RETN

;****************************************
;Copy IO.SYS, MSDOS.SYS and COMMAND.COM into data area.
; Carry set if problems

; 05/05/2018
; Retro DOS v.20 modification 
;	copy MSDOS.SYS & COMMAND.COM into data area

READDOS:
        CALL    TESTSYSDISK
        JNC	short RDFILS
        RETN

RDFILS:
        MOV     BYTE [FILSTAT],0
        MOV     BX,[KERNELHandle]
	MOV	AX,[MSTART]
        MOV     DX,AX
        ADD     DX,[MSIZE]   
        MOV     [KERNELSTRT],AX
        MOV     CX,[KERNELSIZP]
        ADD     AX,CX
        CMP     AX,DX
        JBE	short GOTDOS
        OR      BYTE [FILSTAT],00000100B ; Got part of DOS
        MOV     SI,[MSIZE]
        XOR     DI,DI
        CALL    DISIX4
        MOV     DS,[KERNELSTRT]
;ASSUME  DS:NOTHING
        CALL    READFILE
;ASSUME  DS:CODE
        JC	short CLSALL
        XOR     DX,DX
        MOV     CX,DX
        MOV     AX,(LSEEK*256) | 1
        INT     21H
        MOV     [KERNELOFFS],AX
        MOV     [KERNELOFFS+2],DX
FILESDONE:
        CLC
CLSALL:
        PUSHF
        CALL    COMCLS
        POPF
        RETN
GOTDOS:
        OR      BYTE [FILSTAT],00001000B ; Got all of DOS
        LES     SI,[KERNELSIZB]
        MOV     DI,ES
        MOV     DS,[KERNELSTRT]
;ASSUME  DS:NOTHING
        CALL    READFILE
;ASSUME  DS:CODE
	JC	short CLSALL
        MOV     BX,[COMHandle]
        MOV     [COMSTRT],AX
        CMP     AX,DX                   ; No room left?
        JZ	short CLSALL		; Yes
        MOV     CX,[COMSIZP]
        ADD     AX,CX
        CMP     AX,DX
        JBE	short GOTCOM
        OR      BYTE [FILSTAT],00010000B ; Got part of COMMAND
        SUB     DX,[COMSTRT]
        MOV     SI,DX
        XOR     DI,DI
        CALL    DISIX4
        MOV     DS,[COMSTRT]
;ASSUME  DS:NOTHING
        CALL    READFILE
;ASSUME  DS:CODE
        JC	short CLSALL
        XOR     DX,DX
        MOV     CX,DX
        MOV     AX,(LSEEK*256) | 1
        INT     21H
        MOV     [COMOFFS],AX
        MOV     [COMOFFS+2],DX
        JMP     short FILESDONE

GOTCOM:
        OR      BYTE [FILSTAT],00100000B ; Got all of COMMAND
        LES     SI,[COMSIZB]
        MOV     DI,ES
        MOV     DS,[COMSTRT]
;ASSUME  DS:NOTHING
        CALL    READFILE
;ASSUME  DS:CODE
        JMP	short CLSALL

;**************************************************
;Write BIOS DOS COMMAND to the newly formatted disk.

; 05/05/2018 - Retro DOS v2.0 modification
; (*) Single MSDOS.SYS instead of IO.SYS+MSDOS.SYS	

WRITEDOS:
        MOV     CX,KERNELATT
        MOV     DX,KERNELFIL
        LES     SI,[KERNELSIZB]
        MOV     DI,ES
        CALL    MAKEFIL
        JNC	SHORT GOTNDOS
RET34:
	RETN

GOTNDOS:
        MOV     [TempHandle],BX
        TEST    BYTE [FILSTAT],00001000B
        JNZ	short GOTALLDOS
	;MOV     BP,DOSData
	MOV	BP,KERNELData ; *
        TEST    BYTE [FILSTAT],00000100B
        JNZ	short PARTDOS
        MOV     WORD [KERNELOFFS],0
        MOV     WORD [KERNELOFFS+2],0
        CALL    GETSYS3
RET34J: 
	JC	short RET34
        JMP     SHORT DOSDONE

PARTDOS:
        ;LES     SI,[DOSOFFS]
	LES	SI,[KERNELOFFS] ; *
        MOV     DI,ES
        MOV     [IOCNT],SI
        MOV     [IOCNT+2],DI
        CALL    GOTTARG
        JC	short RET34J
        JMP     SHORT DOSDONE

GOTALLDOS:
        ;LES     SI,[DOSSIZB]
        LES     SI,[KERNELSIZB] ; *
        MOV     DI,ES
        ;MOV     DS,[DOSSTRT]
        MOV     DS,[KERNELSTRT] ; *
;ASSUME  DS:NOTHING
        CALL    WRITEFILE
;ASSUME  DS:CODE
DOSDONE:
        MOV     BX,[TempHandle]
        MOV     CX,[KTIME]
        MOV     DX,[KDATE]
        CALL    CLOSETARG
        MOV     CX,COMATT
        MOV     DX,COMFIL
        LES     SI,[COMSIZB]
        MOV     DI,ES
        CALL    MAKEFIL
        JNC	short GOTNCOM
RET35:  
	RETN

GOTNCOM:
        MOV     [TempHandle],BX
        TEST    BYTE [FILSTAT],00100000B
        JNZ	short GOTALLCOM
        MOV     BP,COMData
        TEST    BYTE [FILSTAT],00010000B
        JNZ	short PARTCOM
        MOV     word [COMOFFS],0
        MOV     word [COMOFFS+2],0
        CALL    GETSYS3
        JC	short RET35
        JMP     SHORT COMDONE

PARTCOM:
        LES     SI,[COMOFFS]
        MOV     DI,ES
        MOV     [IOCNT],SI
        MOV     [IOCNT+2],DI
        CALL    GOTTARG
        JC	short RET35
        JMP     SHORT COMDONE

GOTALLCOM:
        LES     SI,[COMSIZB]
        MOV     DI,ES
        MOV     DS,[COMSTRT]
;ASSUME  DS:NOTHING
        CALL    WRITEFILE
;ASSUME  DS:CODE
COMDONE:
        MOV     BX,[TempHandle]
        MOV     CX,CTIME
        MOV     DX,CDATE
        CALL    CLOSETARG
        ;CMP     BYTE [FILSTAT],00101010B
	; 05/05/2018 (no BIOS file in Retro DOS, single file: MSDOS.SYS)
        CMP     BYTE [FILSTAT],00101000B ; Retro DOS 2.0 modification
        JZ	short NOREDOS
RDFRST2:
        CALL    READDOS			; Start back with BIOS
        JNC	short NOREDOS
        CALL    SYSPRM                  ;Prompt for system disk
        JMP	short RDFRST2		;Try again
NOREDOS:
        CLC
        RETN

;*********************************************
; Create a file on target disk
; CX = attributes, DX points to name
; DI:SI is size file is to have
;
;   There is a bug in DOS 2.00 and 2.01 having to do with writes
;   from the end of memory. In order to circumvent it this routine
;   must create files with the length in DI:SI
;
; On return BX is handle, carry set if problem

MAKEFIL:
        MOV     BX,DX
        PUSH    WORD [BX]
        MOV     AL,[TARGDRV]
        MOV     [BX],AL
        MOV     AH,CREAT
        INT     21H
        POP     WORD [BX]
        MOV     BX,AX
        JC      short RET50
        MOV     CX,DI
        MOV     DX,SI
        MOV     AX,LSEEK*256
        INT     21H                     ; Seek to eventual EOF
        XOR     CX,CX
        MOV     AH,WRITE
        INT     21H                     ; Set size of file to position
        XOR     CX,CX
        MOV     DX,CX
        MOV     AX,LSEEK*256
        INT     21H                     ; Seek back to start
RET50:
        RETN

;*********************************************
; Close a file on the target disk
; CX/DX is time/date, BX is handle

CLOSETARG:
        MOV     AX,(FILE_TIMES*256) | 1
        INT     21H
        MOV     AH,CLOSE
        INT     21H
        RETN

SAVUDIRS:
        XOR     DL,DL
        MOV     SI,USERDIRS
        MOV     BYTE [SI],'\'
        INC     SI
        MOV     AH,CURRENT_DIR
        INT     21H
RET43:  
	RETN


RESTUDIR:
        TEST    word [SWITCHMAP],SYSSW
        JZ	short RET43
        MOV     DX,USERDIRS
        MOV     AH,CHDIR
        INT     21H		; Restore users DIR
        RETN

INT_23:
        PUSH    CS
        POP     DS
        JMP	FEXIT

;****************************************
; Transfer system files
; BP points to data structure for file involved
; offset is set to current amount read in
; Start set to start of file in buffer
; TempHandle is handle to write to on target

IOLOOP:
        MOV     AL,[SYSDRV]
        CMP     AL,[TARGDRV]
        JNZ	short GOTTARG
        MOV     AH,DISK_RESET
        INT     21H
        CALL    TARGPRM                  ;Get target disk

GOTTARG:
;Enter here if some of file is already in buffer, IOCNT must be set
; to size already in buffer.
        MOV     BX,[TempHandle]
        MOV     SI,[IOCNT]
        MOV     DI,[IOCNT+2]
        MOV     DS,[BP+FILE.START]
;ASSUME  DS:NOTHING
        CALL    WRITEFILE               ; Write next part
;ASSUME  DS:CODE
        JNC	short TESTDONE
        RETN

TESTDONE:
        LES     AX,[BP+FILE.OFFSET]
        CMP     AX,[BP+FILE.SIZEB]
        JNZ	short GETSYS3
        MOV     AX,ES
        CMP     AX,[BP+FILE.SIZEB+2]
        JNZ	short GETSYS3
        RETN				; Carry clear from CMP

GETSYS3:
;Enter here if none of file is in buffer
        MOV     AX,[MSTART]             ; Furthur IO done starting here
        MOV     [BP+FILE.START],AX
        MOV     AL,[SYSDRV]
        CMP     AL,[TARGDRV]
        JNZ	short TESTSYS
        MOV     AH,DISK_RESET
        INT     21H
GSYS:
        CALL    SYSPRM                  ;Prompt for system disk
TESTSYS:
        CALL    TESTSYSDISK
        JC	short GSYS
        MOV     BX,[BP+FILE.HANDLE]
        LES     DX,[BP+FILE.OFFSET]
        PUSH    DX
        MOV     CX,ES
        MOV     AX,LSEEK*256
        INT     21H
        POP     DX
        LES     SI,[BP+FILE.SIZEB]
        MOV     DI,ES
        SUB     SI,DX
        SBB     DI,CX                   ; DI:SI is #bytes to go
        PUSH    DI
        PUSH    SI
        ADD     SI,15
        ADC     DI,0
        CALL    DISID4
        MOV     AX,SI
        POP     SI
        POP     DI
        CMP     AX,[MSIZE]
        JBE	short GOTSIZ2
        MOV     SI,[MSIZE]
        XOR     DI,DI
        CALL    DISIX4
GOTSIZ2:
        MOV     [IOCNT],SI
        MOV     [IOCNT+2],DI
        MOV     DS,[MSTART]
;ASSUME  DS:NOTHING
        CALL    READFILE
;ASSUME  DS:CODE
        JNC	short GETOFFS
        CALL    CLSALL
        JMP	short GSYS
GETOFFS:
        XOR     DX,DX
        MOV     CX,DX
        MOV     AX,(LSEEK*256) | 1
        INT     21H
        MOV     [BP+FILE.OFFSET],AX
        MOV     [BP+FILE.OFFSET+2],DX
        CALL    CLSALL
        JMP	IOLOOP

;*************************************************
; Test to see if correct system disk. Open handles

; 05/05/2018
; Retro DOS v2.0 modification (BIOS+DOS KERNEL file = "MSDOS.SYS")

TESTSYSDISK:
        MOV     AX,OPEN*256
        MOV     DX,KERNELFIL
        INT     21H
        JNC	short SETKERNEL
CRET12: 
	STC
RET12:
	RETN

SETKERNEL:
        MOV     [KERNELHandle],AX
        MOV     BX,AX
        CALL    GETFSIZ
        CMP     word [KERNELSIZP],0
        JZ	short SETKERNELSIZ1
        CMP     [KERNELSIZP],AX
        JZ	short SETKERNELSIZ2
KERNELCLS:
        MOV     AH,CLOSE
        MOV     BX,[KERNELHandle]
        INT     21H
        JMP	short CRET12

SETKERNELSIZ1:
        MOV     [KERNELSIZP],AX
SETKERNELSIZ2:
        MOV     [KERNELSIZB],SI
        MOV     [KERNELSIZB+2],DI
        MOV     [KDATE],DX
        MOV     [KTIME],CX
        MOV     AX,OPEN*256
        MOV     DX,COMFIL
        INT     21H
        JC	short KERNELCLS

        MOV     [COMHandle],AX
        MOV     BX,AX
        CALL    GETFSIZ
        CMP     word [COMSIZP],0
        JZ	short SETCOMSIZ1
        CMP     [COMSIZP],AX
        JZ	short SETCOMSIZ2
COMCLS:
        MOV     AH,CLOSE
        MOV     BX,[COMHandle]
        INT     21H
        JMP	short KERNELCLS

SETCOMSIZ1:
        MOV     [COMSIZP],AX
SETCOMSIZ2:
        MOV     [COMSIZB],SI
        MOV     [COMSIZB+2],DI
        MOV     [CDATE],DX
        MOV     [CTIME],CX
        CLC
        RETN

;*******************************************
; Handle in BX, return file size in para in AX
; File size in bytes DI:SI, file date in DX, file
; time in CX.

GETFSIZ:
        MOV     AX,(LSEEK*256) | 2
        XOR     CX,CX
        MOV     DX,CX
        INT     21H
        MOV     SI,AX
        MOV     DI,DX
        ADD     AX,15           ; Para round up
        ADC     DX,0
        AND     DX,0FH          ; If the file is larger than this
                                ; it is bigger than the 8086 address space!
        MOV     CL,12
        SHL     DX,CL
        MOV     CL,4
        SHR     AX,CL
        OR      AX,DX
        PUSH    AX
        MOV     AX,LSEEK*256
        XOR     CX,CX
        MOV     DX,CX
        INT     21H
        MOV     AX,FILE_TIMES*256
        INT     21H
        POP     AX
        RETN

;********************************************
; Read/Write file
;       DS:0 is Xaddr
;       DI:SI is byte count to I/O
;       BX is handle
; Carry set if screw up
;
; I/O SI bytes
; I/O 64K - 1 bytes DI times
; I/O DI bytes
; DS=CS on output


READFILE:
; Must preserve AX,DX
        PUSH    AX
        PUSH    DX
        PUSH    BP
        MOV     BP,READ*256
        CALL    FILIO
        POP     BP
        POP     DX
        POP     AX
        PUSH    CS
        POP     DS
        RETN

WRITEFILE:
        PUSH    BP
        MOV     BP,WRITE*256
        CALL    FILIO
        POP     BP
        PUSH    CS
        POP     DS
        RETN

FILIO:
        XOR     DX,DX
        MOV     CX,SI
        JCXZ    K64IO
        MOV     AX,BP
        INT     21H
        JC	short IORET
        ADD     DX,AX
        CMP     AX,CX           ; If not =, AX<CX, carry set.
        JNZ	short IORET
        CALL    NORMALIZE
K64IO:
        CLC
        MOV     CX,DI
        JCXZ    IORET
        MOV     AX,BP
        INT     21H
        JC	short IORET
        ADD     DX,AX
        CMP     AX,CX           ; If not =, AX<CX, carry set.
        JNZ	short IORET
        CALL    NORMALIZE
        MOV     CX,DI
K64M1:
        PUSH    CX
        XOR     AX,AX
        OR      DX,DX
        JZ	short NORMIO
        MOV     CX,10H
        SUB     CX,DX
        MOV     AX,BP
        INT     21H
        JC	short IORETP
        ADD     DX,AX
        CMP     AX,CX           ; If not =, AX<CX, carry set.
        JNZ	short IORETP
        CALL    NORMALIZE
NORMIO:
        MOV     CX,0FFFFH
        SUB     CX,AX
        MOV     AX,BP
        INT     21H
        JC	short IORETP
        ADD     DX,AX
        CMP     AX,CX           ; If not =, AX<CX, carry set.
        JNZ	short IORETP
        CALL    NORMALIZE       ; Clears carry
        POP     CX
        LOOP    K64M1
        PUSH    CX
IORETP:
        POP     CX
IORET:
        RETN


;*********************************
; Shift DI:SI left 4 bits
DISIX4:
        MOV     CX,4
SH32:
        SHL     SI,1
        RCL     DI,1
        LOOP    SH32
        RETN

;*********************************
; Shift DI:SI right 4 bits
DISID4:
        MOV     CX,4
SH32B:
        SHR     DI,1
        RCR     SI,1
        LOOP    SH32B
        RETN

;********************************
; Normalize DS:DX

NORMALIZE:
        PUSH    DX
        PUSH    AX
        SHR     DX,1
        SHR     DX,1
        SHR     DX,1
        SHR     DX,1
        MOV     AX,DS
        ADD     AX,DX
        MOV     DS,AX
        POP     AX
        POP     DX
        AND     DX,0FH		; Clears carry
        RETN


ROOTSTR:    DB	0
	    DB	":"
SWTCH:	    DB	"/",0
DBLFLG:	    DB	0		;Initialize flags to zero
CLEARFLG:   DB	0
DRIVE:      DB	0
DEFALT:     DB	0		;Default drive
IOCNT:      DD	0
MSTART:     DW	0 		; Start of sys file buffer (para#)
MSIZE:      DW	0		; Size of above in paragraphs
TempHandle: DW	0
FILSTAT:    DB	0		; In memory status of files
                                ; XXXXXX00B BIOS not in
                                ; XXXXXX01B BIOS partly in
                                ; XXXXXX10B BIOS all in
                                ; XXXX00XXB DOS not in
                                ; XXXX01XXB DOS partly in
                                ; XXXX10XXB DOS all in
                                ; XX00XXXXB COMMAND not in
                                ; XX01XXXXB COMMAND partly in
                                ; XX10XXXXB COMMAND all in

USERDIRS:   times DIRSTRLEN+3 DB 0  ; Storage for users current directory

; 05/05/2018 - Retro DOS v2.0 modification! (KERNEL file = MSDOS.SYS)
KERNELData:
KERNELHandle:	DW	0
KERNELSIZP:	DW	0
KERNELSIZB:	DD	0
KERNELOFFS:	DD	0
KERNELSTRT:	DW	0
KDATE:		DW	0 	; MSDOS.SYS date stored here
KTIME:		DW	0       ; MSDOS.SYS time stored here

KERNELATT EQU	attr_hidden + attr_system + attr_read_only
KERNELFIL:
KERNELDRV:
	DB      "X:\"
        DB      "MSDOS.SYS"	; = RETRODOS.SYS = IBMBIO.COM+IBMDOS.COM
        DB      0	    	;                = IO.SYS+MSDOS.SYS	  

COMData:
COMHandle:	DW	0
COMSIZP:	DW	0
COMSIZB:	DD	0
COMOFFS:	DD	0
COMSTRT:	DW	0
CDATE:		DW	0	; Date of COMMAND.COM
CTIME:		DW	0	; Time of COMMAND.COM

COMATT  EQU     0
COMFIL:
COMDRV:
        DB      "X:\COMMAND.COM",0
VOLFCB: 
	DB      -1,0,0,0,0,0,8
        DB      0
VOLNAM: 
	DB      "           "
        DB      8
        times	26 db 0
ALLFILE:
	DB      -1,0,0,0,0,0,0FFH
ALLDRV:
	DB      0,"???????????"
        times	26 db 0

CLEANFILE:
	DB    0
CLNNAM:
	DB      "AAAFFFFFFOR"
        times	26 db 0

align 2

SWITCHMAP:  DW	0
SWITCHCOPY: DW	0
FAT:	DW	0
	DW	0
CLUSSIZ: DW	0
SECSIZ:	DW	0
SYSSIZ:	DD	0
FDSKSIZ: DD	0
BADSIZ:	DD	0
SYSTRKS: DW	0
SECTORS: DW	0
INBUFF:	DB	80,0
	times	80 db 0

        times	256 db 0
STACK:

;============================================================================
; FORMES.ASM - 22/09/1983 (MSDOS 2.11)
;============================================================================

;TITLE FORMAT Messages

;FALSE   EQU     0
;TRUE    EQU     NOT FALSE

;IBMVER  EQU     FALSE

;.xlist
;.xcref
;        INCLUDE DOSSYM.ASM
;.cref
;.list

	;Wait for "Y" or "N"
WAITYN:
        MOV     DX,MORMSG		;Point to the message
        CALL    PRINT			;And print it
        MOV     AX,(STD_CON_INPUT_FLUSH*256) | STD_CON_INPUT
					;Flush buffer and wait for keystroke
        INT     21H			;Input character now a Y or N
        AND     AL,0DFH			;So lower case works too
        CMP     AL,"Y"
        JZ	short WAIT20
        CMP     AL,"N"
        JZ	short WAIT10
        CALL    CRLF
        JMP     SHORT WAITYN
WAIT10: 
	STC
WAIT20:
	RETN

;*********************************************
; Make a status report including the following information:
; Total disk capacity
; Total system area used
; Total bad space allocated
; Total data space available
;NOTE:
;       The DISP32BITS routine prints the number in DI:SI followed
;          by the message pointed to by BX. If it is desired to print
;          a message before the number, point at the message with DX
;          and call PRINT.

REPORT:
        MOV     AX,[FDSKSIZ]
        MUL     word [SECSIZ]
        MOV     CX,[CLUSSIZ]
        CALL    UNSCALE
        MOV     [FDSKSIZ],AX
        MOV     [FDSKSIZ+2],DX
        MOV     SI,AX
        MOV     DI,DX
        MOV     BX,DSKSPC
        CALL    DISP32BITS		;Report total disk space
        MOV     SI,[SYSSIZ]
        MOV     DI,[SYSSIZ+2]
        CMP     SI,0
        JNZ	short SHOWSYS
        CMP     DI,0
        JZ	short CHKBAD
SHOWSYS:
        MOV     BX,SYSSPC
        CALL    DISP32BITS		;Report space used by system
CHKBAD:
        MOV     SI,[BADSIZ]
        MOV     DI,[BADSIZ+2]
        CMP     SI,0
        JNZ	short SHOWBAD
        CMP     DI,0
        JZ	short SHOWDATA
SHOWBAD:
        MOV     BX,BADSPC
        CALL    DISP32BITS		;Report space used by bad sectors
SHOWDATA:
        MOV     CX,[FDSKSIZ]
        MOV     BX,[FDSKSIZ+2]
        SUB     CX,[BADSIZ]
        SBB     BX,[BADSIZ+2]
        SUB     CX,[SYSSIZ]
        SBB     BX,[SYSSIZ+2]
        MOV     SI,CX
        MOV     DI,BX
        MOV     BX,[DATASPC]
        CALL    DISP32BITS		;Report space left for user
        RETN

BADVER:	 DB	"Incorrect DOS version",13,10,"$"
SNGMSG:	 DB	"Insert new diskette for drive "
SNGDRV:	 DB	 "x:",13,10,"and strike any key when ready$"
HRDMSG:	 DB	"Press any key to begin formatting "
HRDDRV:	 DB	"x: $"
SYSTRAN: DB	"System transferred",13,10,"$"
MORMSG:	 DB	"Format another (Y/N)?$"
CRLFMSG: DB	13,10,"$"
INVCHR:	 DB	"Invalid characters in volume label",13,10,"$"
INVDRV:	 DB	"Invalid drive specification$"
INVPAR:	 DB	"Invalid parameter$"
TARGMSG: DB	"Re-insert diskette for drive "
TARGDRV: DB	"x:",13,10,"and strike any key when ready$"
SYSMSG:	 DB	"Insert DOS disk in drive "
SYSDRV:	 DB	"x:",13,10,"and strike any key when ready$"
FRMTERR: DB	"Format failure",13,10,13,10,"$"
NOTSYS:	 DB	"Disk unsuitable for system disk",13,10,"$"
NOUSE:	 DB	"Track 0 bad - disk unusable",13,10,"$"
MEMEX:	 DB	"Insufficient memory for system transfer",13,10,"$"

;Report messages
DSKSPC:	 DB	" bytes total disk space",13,10,"$"
SYSSPC:	 DB	" bytes used by system",13,10,"$"
BADSPC:	 DB	" bytes in bad sectors",13,10,"$"
DATASPC: DB	" bytes available on disk",13,10,13,10,"$"

LABPRMT: DB	"Volume label (11 characters, ENTER for none)? $"

;============================================================================
; GENFOR.ASM - 03/02/1983 (MSDOS 2.11)
;============================================================================

; Generic FORMAT module for any ms-dos disk erases the directory,
; zeros FAT, and marks bad sectors

WRTFAT:
        MOV     AH,GET_DPB
        MOV     DL,[DRIVE]
        INC     DL              ;A = 1
        INT     21H             ;FORCE A FATREAD
        PUSH    CS
        POP     DS
        MOV     AL,[FATCNT]
        MOV     [CURCNT],AL     ;SET UP FAT COUNT
        MOV     AX,[FATSTART]
        MOV     [COUNT],AX
FATLOOP:
        MOV     AL,[DRIVE]
        CBW
        MOV     CX,[FATSIZE]
        MOV     DX,[COUNT]
        MOV     BX,[FATSPACE]
        INT     26H
        POP     AX
        JC	short GORET
        MOV     CX,[FATSIZE]
        ADD     [COUNT],CX
        DEC     BYTE [CURCNT]
        JNZ	short FATLOOP
        CLC			;Good return
GORET:
        RETN

FATSIZE:    DW	0
FATSTART:   DW	0
COUNT:	    DW	0
STARTSECTOR: DW 0
SPC:	    DB	0		;SECTORS PER CLUSTER
FATCNT:	    DB  0		;NUMBER OF FATS ON THIS DRIVE
CURCNT:	    DB  0
DSKSIZE:    DW  0		;NUMBER OF SECTORS ON THE DRIVE
START:	    DW  0		;CURRENT TEST SECTOR

INIT:
        MOV     AH,GET_DPB
        MOV     DL,[DRIVE]
        INC     DL              ;A = 1
        INT     21H             ;FORCE A FATREAD
        MOV     AL,[BX+4]       ;SECTORS PER CLUSTER - 1
        INC     AL
        MOV     CH,AL           ;CH = SECTORS PER CLUSTER
        CBW
        MOV     BP,[BX+0DH]     ;MAXCLUS + 1
        DEC     BP
        MUL     BP
        MOV     BP,AX
        ADD     BP,[BX+0BH]     ;BP = NUMBER OF SECTORS ON THE DISK
        MOV     AL,[BX+0FH]     ;GET SIZE OF FAT IN SECTORS
        MOV     AH,[BX+8]       ;GET NUMBER OF FATS
        MOV     DX,[BX+6]       ;FIRST SECTOR OF FAT
        MOV     CL,[BX+16H]     ;FATID BYTE
        MOV     SI,[BX+2]       ;SECTOR SIZE
        MOV     BX,[BX+0BH]     ;FIRST SECTOR OF DATA
        PUSH    CS
        POP     DS
        MOV     [FATCNT],AH
        MOV     [DSKSIZE],BP
        MOV     [SPC],CH
        MOV     [FATSTART],DX
        MOV     [ENDLOC],CL
        MOV     [FATID],CL
        MOV     [STARTSECTOR],BX
        XOR     AH,AH
        MOV     [FATSIZE],AX
        MUL     SI              ;AX = SIZE OF FAT
        ADD     [FREESPACE],AX
        ADD     [BUFFER],AX
        MOV     AX,BX
        MUL     SI
        ADD     [FREESPACE],AX  ;AX = SIZE OF TEMP BUFFER
DISKFORMAT:
DONE:
        XOR     AX,AX
        CLC
        RETN

BADSECTOR:
        MOV     DX,[START]
        CMP     DX,[DSKSIZE]
        JAE	short DONE

        MOV     AL,[DRIVE]
        MOV     CL,[SPC]		;READ ONE ALLOCATIONS WORTH
        XOR     CH,CH
        CMP     BYTE [FIRSTFLAG],0
        JZ	short SETBX
        MOV     CX,[STARTSECTOR]	;FIRST TIME THROUGH READ SYSTEM AREA
        MOV     BYTE [FIRSTFLAG],0
        MOV     DX,[START]
SETBX:  MOV     BX,[BUFFER]
        PUSH    CX
        INT     25H                     ;TRY TO READ
        POP     AX                      ;CLEAN UP STACK
        POP     CX
        JC	short GOTBAD		;KEEP LOOKING FOR BADSECTORS
        ADD     [START],CX
        JMP	short BADSECTOR

GOTBAD:
        MOV     AX,CX
        MOV     BX,[START]
        ADD     [START],AX		;SET UP FOR NEXT CALL
        CLC
        RETN

FIRSTFLAG:  DB  1			;1 = FIRST CALL TO BADSECTOR
HARDFLAG:   DB  1
FATID:	    DB  0FEH
SWITCHLIST: DB  3,"OVS"
BUFFER:	    DW  ENDLOC
FREESPACE:  DW  ENDLOC
FATSPACE:   DW  ENDLOC

ENDLOC:	DB	0FEH,0FFH,0FFH

; ---------------------------------------------------------------------------
; END OF FILE