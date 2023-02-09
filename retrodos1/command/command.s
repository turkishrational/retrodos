; ****************************************************************************
; COMMAND.COM (MSDOS 1.25 Command Interpreter) - RETRO DOS v1.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 23/02/2018
; ----------------------------------------------------------------------------
; Beginning: 15/02/2018 
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11  
; ----------------------------------------------------------------------------
;	    ((nasm command.s -l command.lst -o COMMAND.COM)) 	
; ----------------------------------------------------------------------------
; Derived from 'COMMAND.ASM' file of MSDOS 1.25 (IBM PCDOS v1.1) source code
; by Microsoft (by Tim Paterson), 17/05/1983
; ****************************************************************************


; COMMAND version 1.17
;
; This version of COMMAND is divided into three distinct parts. First
; is the resident portion, which includes handlers for interrupts
; 22H (terminate), 23H (Cntrl-C), 24H (fatal error), and 27H (stay
; resident); it also has code to test and, if necessary, reload the
; transient portion. Following the resident is the init code, which is
; overwritten after use. Then comes the transient portion, which
; includes all command processing (whether internal or external).
; The transient portion loads at the end of physical memory, and it may
; be overlayed by programs that need as much memory as possible. When
; the resident portion of command regains control from a user program,
; a checksum is performed on the transient portion to see if it must be
; reloaded. Thus programs which do not need maximum memory will save
; the time required to reload COMMAND when they terminate.

;Use the following booleans to set assembly flags
;FALSE	EQU     0
;TRUE	EQU     ~FALSE	; NOT FALSE

;IBMVER	EQU     FALSE   ;Switch to build IBM version of Command
;MSVER	EQU     TRUE    ;Switch to build MS-DOS version of Command

;HIGHMEM EQU	TRUE	;Run resident part above transient (high memory)

LINPERPAG       EQU     23
NORMPERLIN      EQU     1
WIDEPERLIN      EQU     5

        ;IF	IBMVER
SYM	EQU     ">"
;COMDRV	EQU     1
        ;ENDIF

	;IF	MSVER
;SYM	EQU     ":"
COMDRV	EQU     0
        ;ENDIF

FCB     EQU     5CH
DSKRESET EQU    13
SETBASE EQU     38
SRCHFRST EQU    17
SRCHNXT EQU     18
RENAM   EQU     23
INCHAR  EQU     1
GETFAT  EQU     27
OPEN    EQU     15
CLOSE   EQU     16
MAKE    EQU     22
DELETE  EQU     19
RDBLK   EQU     39
WRBLK   EQU     40
SETDMA  EQU     26
SELDRV  EQU     14
GETDRV  EQU     25
PRINTBUF EQU    9
OUTCH   EQU     2
INBUF   EQU     10
GETDATE EQU     2AH
SETDATE EQU     2BH
GETTIME EQU     2CH
SETTIME EQU     2DH
RR      EQU     33
RECLEN  EQU     14
FILLEN  EQU     16
OFFDATE EQU     20

; ----------------------------------------------------------------------------
; SEGMENT - CODERES
; ----------------------------------------------------------------------------
;START OF RESIDENT PORTION
; ----------------------------------------------------------------------------

	[ORG 100H]

RSTACK	EQU	$ ; EQU 100H

PROGSTART:
	JMP	CONPROC

LTPA:	DW	0               ;WILL STORE TPA SEGMENT HERE
MYSEG:	DW	0               ;Put our own segment here

CONTC:
        MOV     AX,CS
        MOV     DS,AX
        MOV     SS,AX
        MOV     SP,RSTACK
        STI
        CALL    SETVECT
        MOV     AH,DSKRESET
        INT     33              ;Reset disks in case files were open
        TEST    BYTE [BATCH],-1
        JZ      SHORT LODCOM
ASKEND:
        MOV     DX,ENDBATMES
        MOV     AH,PRINTBUF
        INT     33
        MOV     AX,0C00H+INCHAR
        INT     33
        AND     AL,5FH
        CMP     AL,"N"
        JZ      SHORT LODCOM
        CMP     AL,"Y"
        JNZ     SHORT ASKEND
        MOV     BYTE [BATCH],0
LODCOM:
        MOV     AX,CS
        MOV     SS,AX
        MOV     SP,RSTACK
        MOV     DS,AX
        CALL    SETVECT
        CALL    CHKSUM
        CMP     DX,[SUM]
        JZ      SHORT HAVCOM
        MOV     BYTE [LOADING],1
        CALL    LOADCOM
CHKSAME:
        CALL    CHKSUM
        CMP     DX,[SUM]
        JZ      SHORT HAVCOM
        CALL    WRONGCOM
        JMP     SHORT CHKSAME
HAVCOM:
        MOV     BYTE [LOADING],0
        MOV     SI,LTPA
        MOV     DI,TPA
        MOV     ES,[TRNSEG]
        CLD
        MOVSW           ;Move TPA segment to transient storage
        MOVSW           ;Move resident segment too
        MOV     AX,[MEMSIZ]
        MOV     [ES:2],AX
        JMP     FAR [TRANS]

RESIDENT:
        ADD     DX,15
        MOV     CL,4
        SHR     DX,CL           ;Number of paragraphs of new addition
        ADD     [CS:LTPA],DX
        XOR     AX,AX
        MOV     DS,AX
        JMP     FAR [80H] ;Pretend user executed INT 20H

DSKERR:
        ;******************************************************
        ;       THIS IS THE DEFAULT DISK ERROR HANDLING CODE 
        ;       AVAILABLE TO ALL USERS IF THEY DO NOT TRY TO 
        ;       INTERCEPT INTERRUPT 24H.
        ;******************************************************
        STI
        PUSH    DS
        PUSH    CS
        POP     DS              ;Set up local data segment
        PUSH    DX
        CALL    CRLF
        POP     DX
        ADD     AL,"A"          ;Compute drive letter
        MOV     [DRVLET],AL
        TEST    AH,80H          ;Check if hard disk error
        JNZ     SHORT FATERR
        MOV     SI,READ
        TEST    AH,1
        JZ      SHORT SAVMES
        MOV     SI,WRITE
SAVMES:
        LODSW
        MOV     WORD [IOTYP],AX
        LODSW
        MOV     WORD [IOTYP+2],AX
        AND     DI,0FFH
        CMP     DI,12
        JBE     SHORT HAVCOD
        MOV     DI,12
HAVCOD:
        MOV     DI,WORD [DI+MESBAS] ;Get pointer to error message
        XCHG    DI,DX           ;May need DX later
        MOV     AH,PRINTBUF
        INT     33              ;Print error type
        MOV     DX,ERRMES
        INT     33
        CMP     BYTE [LOADING],0
        JNZ     SHORT GETCOMDSK
ASK:
        MOV     DX,REQUEST
        MOV     AH,PRINTBUF
        INT     33
        MOV     AX,0C00H+INCHAR
        INT     33              ;Get response
        CALL    CRLF
        OR      AL,20H          ;Convert to lower case
        MOV     AH,0            ;Return code for ignore
        CMP     AL,"i"          ;Ignore?
        JZ      SHORT EXIT
        INC     AH
        CMP     AL,"r"          ;Retry?
        JZ      SHORT EXIT
        INC     AH
        CMP     AL,"a"          ;Abort?
        JNZ     SHORT ASK
EXIT:
        MOV     AL,AH
        MOV     DX,DI
        POP     DS
        IRET

FATERR:
        MOV     DX,BADFAT
        MOV     AH,PRINTBUF
        INT     33
        MOV     DX,DRVNUM
        INT     33
        MOV     AL,2            ;Abort
        POP     DS
        IRET

GETCOMDSK:
        MOV     DX,NEEDCOM
        MOV     AH,PRINTBUF
        INT     33
        MOV     AX,0C07H        ;Get char without testing or echo
        INT     33
        JMP     LODCOM

CRLF:
        MOV     DX,NEWLIN
        PUSH    AX
        MOV     AH,PRINTBUF
        INT     33
        POP     AX
RET10:  
	RETN

LOADCOM:
        PUSH    DS
        MOV     DS,[TRNSEG]
        MOV     DX,100H
        MOV     AH,SETDMA
        INT     33
        POP     DS
        MOV     DX,COMFCB
        MOV     AH,OPEN
        INT     33              ;Open COMMAND.COM
        OR      AL,AL
        JZ      SHORT READCOM
        MOV     DX,NEEDCOM
PROMPTCOM:
        MOV     AH,PRINTBUF
        INT     33
        MOV     AX,0C07H        ;Get char without testing or echo
        INT     33
        JMP     SHORT LOADCOM
READCOM:
        MOV     WORD [COMFCB+RR],TRANSTART
        XOR     AX,AX
        MOV     [COMFCB+RR+2],AX
        MOV     [COMFCB],AL	;Use default drive
        INC     AX
        MOV     [COMFCB+RECLEN],AX
        MOV     CX,COMLEN
        MOV     DX,COMFCB
        MOV     AH,RDBLK
        INT     33
        OR      AL,AL
        JZ      SHORT RET10
WRONGCOM:
        MOV     DX,COMBAD
        JMP     SHORT PROMPTCOM

CHKSUM:
        CLD
        PUSH    DS
        MOV     DS,[TRNSEG]
        MOV     SI,100H
        MOV     CX,COMLEN
        SHR     CX,1
        XOR     DX,DX
CHK:
        LODSW
        ADD     DX,AX
        LOOP    CHK
        POP     DS
        RETN

SETVECT:
        MOV     DX,LODCOM
        MOV     AX,2522H        ;Set Terminate address
        INT     21H
        MOV     DX,CONTC
        MOV     AX,2523H        ;Set Ctrl-C address
        INT     21H
        MOV     DX,DSKERR
        MOV     AX,2524H        ;Set Hard Disk Error address
        INT     33
        MOV     DX,RESIDENT
        MOV     AX,2527H        ;Set Terminate and Stay Resident address
        INT     33
        RETN

RESCODESIZE     EQU     $-$$

DATARES:

; ----------------------------------------------------------------------------
; SEGMENT - DATA
; ----------------------------------------------------------------------------

ALIGN 16

; ----------------------------------------------------------------------------
; Data for resident portion
; ----------------------------------------------------------------------------

MESBAS: DW	ERR0
        DW	ERR2
        DW	ERR4
        DW	ERR6
        DW 	ERR8
        DW	ERR10
        DW	ERR12
ERR0:	DB	"Write protect$"
ERR2:	DB	"Not ready$"
ERR4:	DB	"Data$"
ERR6:	DB	"Seek$"
ERR8:	DB	"Sector not found$"
ERR10:	DB	"Write fault$"
ERR12:	DB	"Disk$"
READ:	DB	"read$"
WRITE:	DB	"writ$"
ERRMES:	DB	" error "
IOTYP:	DB	"writing"
DRVNUM:	DB	" drive "
DRVLET:	DB	"A"
NEWLIN:	DB	13,10,"$"
REQUEST: DB	"Abort, Retry, Ignore? $"
BADFAT:	DB	13,10,"File allocation table bad,$"
COMBAD:	DB	13,10,"Invalid COMMAND.COM"
NEEDCOM: DB	13,10,"Insert DOS disk in "
        DB	"drive A"
PROMPT:	DB	13,10,"and strike any key when ready",13,10,"$"
NEEDBAT: DB	13,10,"Insert disk with batch file$"
ENDBATMES: DB	13,10,"Terminate batch job (Y/N)? $"
LOADING: DB	0
BATFCB: DB	1,"AUTOEXECBAT"
        TIMES	21 DB 0
        DW	0
        DW	0	;Initialize RR field to zero
PARMTAB: TIMES 10 DW -1	;No parameters initially
BATCH:	DB      1	;Assume batch mode initially
COMFCB:	DB	COMDRV,"COMMAND COM"
        TIMES 25 DB 0
TRANS:	DW	COMMAND
TRNSEG:	DW	0
BATBYT: DB	0
MEMSIZ:	DW	0
SUM:	DW	0
INITADD: TIMES 4 DB 0

ALIGN 16

RESDATASIZE     EQU	$-DATARES

; ----------------------------------------------------------------------------
; SEGMENT - INIT
; ----------------------------------------------------------------------------

;*******************************************************************
;START OF INIT PORTION
;This code is overlayed the first time the TPA is used.

INIT:

INIT_START EQU	$

CONPROC:
        MOV     SP,RSTACK

        ;IF	HIGHMEM
        ;MOV	AX, [2]
        ;SUB	AX,((RESCODESIZE+RESDATASIZE+100H)+15)/16 ;Subtract size of resident
        ;MOV	[2],AX
        ;MOV	ES,AX
        ;MOV	SI,100H
        ;MOV	DI,SI
        ;MOV	CX,(1+(RESCODESIZE+RESDATASIZE))/2 ;Length of resident in words
        ;REP	MOVSW                   ;Move to end of memory
        ;MOV	DS,AX
        ;MOV	[LTPA],CS
        ;ENDIF

        ;IF	NOT HIGHMEM
        MOV	AX,CS
        ADD	AX,((RESCODESIZE+RESDATASIZE+100H)+15)/16 ;Compute segment of TPA
        MOV	[LTPA],AX
        MOV	AX,[2]
        ;ENDIF

        MOV     [MYSEG],DS
        MOV     [MEMSIZ],AX
        SUB     AX,TRNLEN               ;Subtract size of transient
        MOV     [TRNSEG],AX
        CALL    SETVECT
        CALL    LOADCOM
        CALL    CHKSUM
        MOV     [SUM],DX

	; 16/02/2018
	
	;IF MSVER
        ;IF	HIGHMEM
        ;PUSH	DS
        ;PUSH	CS
        ;POP	DS
        ;ENDIF
        MOV	DX,HEADER
        MOV	AH,PRINTBUF
        INT	33
        ;IF	HIGHMEM
        ;POP	DS
        ;ENDIF
	;ENDIF

        MOV     DX,BATFCB
        MOV     AH,OPEN
        INT     33                      ;See if AUTOEXEC.BAT exists
        MOV     WORD [BATFCB+RECLEN],1	;Set record length to 1
        OR      AL,AL                   ;Zero means file found
        JZ      SHORT DRV0
        MOV     BYTE [BATCH],0		;Not found--turn off batch job

        MOV     AX,DATINIT
        MOV     [INITADD],AX
        MOV     AX,[TRNSEG]
        MOV     [INITADD+2],AX
        CALL    FAR [INITADD]
DRV0:
        JMP     HAVCOM

        ; IF MSVER
HEADER:	DB	13,10,"Command v. 1.17"
        ;IF	HIGHMEM
        ;DB	"H"
        ;ENDIF
        DB	13,10,"$"
        ;ENDIF

        ;IF IBMVER
;HEADER: DB	13,10,13,10,"The IBM Personal Computer DOS",13,10
        ;DB	"Version 1.10 (C)Copyright IBM Corp 1981, 1982",13,10,"$"
        ;DB	"Licensed Material - Program Property of IBM"
        ;ENDIF

INITSIZE EQU	$-INIT_START

ALIGN 16

; 16/02/2018
; ----------------------------------------------------------------------------
; SEGMENT - TRANSCODE
; ----------------------------------------------------------------------------

; 16/02/2018
; comtrans.s (COMMAND.COM source file 2 of 2) code/data addresses 
; (these values must be changed when comtrans.s source code is changed
; and data offsets are changed)

COMMAND EQU	0104H
DATINIT	EQU	09DBH

; 23/02/2018
TPA	EQU	0FA0H
TRNLEN	EQU	010CH

;********************************************************************
;START OF TRANSIENT PORTION
;/This code is loaded at the end of memory and may be overwritten by
;memory-intensive user programs.

TRANSTART EQU $-100H

COMTRANS:

INCBIN	"COMTRANS.BIN"

COMLEN	EQU $-COMTRANS ; End of COMMAND load.