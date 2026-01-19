; ERDOGAN TAN - Retro DOS v1.0 - Date&Time Set Sample
; 17/02/2018 - 20/02/2018


	[org 100h]

OUTCH	EQU	2
PRINTBUF EQU	9
INBUF   EQU     10
GETDATE EQU     2AH
GETTIME EQU     2CH
SETDATE EQU     2BH
SETTIME EQU     2DH

	MOV	AH, 0FFH
	INT	21H

	MOV	DX, NEXTLINE
	MOV	AH, PRINTBUF
	INT	21H

DO_LOOP:
        MOV     WORD [81H],13    ;Want to prompt for date during initialization
        CALL    DATE
        CALL    TIME

	MOV	DX, NEXTLINE
	MOV	AH, PRINTBUF
	INT	21H

	INT	20H

SCANOFF:
        LODSB
        CALL    DELIM
        JZ      SHORT SCANOFF
        DEC     SI              ;Point to first non-delimiter
        RETN

DELIM:
        CMP     AL, " "
        JZ      SHORT RET80
        CMP     AL, "="
        JZ      SHORT RET80
        CMP     AL, ","
        JZ      SHORT RET80
        CMP     AL, 9		;Check for TAB character
RET80:  
	RETN


; DATE - Gets and sets the time

DATE:
        MOV     SI, 81H		;Accepting argument for date inline
        CALL    SCANOFF
        CMP     AL, 13
        JZ      SHORT PRMTDAT
        MOV     BX, 2F00H+"-"	;"/-"
        CALL    INLINE
        JMP     SHORT COMDAT

PRMTDAT:
	MOV	DX, CURDAT
        MOV     AH, PRINTBUF
        INT     21H		;Print "Current date is "
        MOV     AH, GETDATE
        INT     21H		;Get date in CX:DX
        CBW
        MOV     SI, AX
        SHL     SI, 1
        ADD     SI, AX		;SI=AX*3
        ADD     SI, WEEKTAB
        MOV     BX, CX
        MOV     CX, 3
        CALL    OUTCNT
        MOV     AL, " "
        CALL    _OUT
        MOV     AX, BX
        MOV     CX, DX
        MOV     DL, 100
        DIV     DL
        XCHG    AL, AH
        XCHG    AX, DX
        MOV     BL, "-"
        CALL    SHOW
GETDAT:
        MOV     DX, NEWDAT
        MOV     BX, 2F00H+"-"	;"/-" in BX
        CALL    GETBUF
COMDAT: 
	JZ      SHORT RET80
        JC      SHORT DATERR
        LODSB   
        CMP     AL, BL
        JZ      SHORT SEPGD
        CMP     AL, BH
        JNZ     SHORT DATERR
SEPGD:  
	CALL    GETNUM
        JC      SHORT DATERR
        MOV     CX, 1900
        CMP     BYTE [SI], 13
        JZ      SHORT BIAS
        MOV     AL, 100
        MUL     AH
        MOV     CX, AX
        CALL    GETNUM
        JC      SHORT DATERR
BIAS:
        MOV     AL, AH
        MOV     AH, 0
        ADD     CX, AX
        LODSB
        CMP     AL, 13
        JNZ     SHORT DATERR
        MOV     AH, SETDATE
        INT     21H
        OR      AL, AL
	JNZ	DATERR
        RETN
DATERR:
        MOV     DX, BADDAT
        MOV     AH, PRINTBUF
        INT     33
        JMP	SHORT GETDAT

	MOV	DX, NEXTLINE
	MOV	AH, PRINTBUF
	INT	21H

TIME:
	MOV     DX, CURTIM
        MOV     AH, PRINTBUF
        INT     21H		;Print "Current time is "
        MOV     AH, GETTIME
        INT     21H		;Get time in CX:DX
        MOV     BL, ":"
        CALL    SHOW
GETTIM:
        XOR     CX, CX		;Initialize hours and minutes to zero
        MOV     DX, NEWTIM
        MOV     BX, 3A00H+":"
        CALL    GETBUF
COMTIM: 
	JZ      SHORT RET100	;If no time present, don't change it
        JC	SHORT TIMERR
        MOV     CX,DX
        XOR     DX,DX
        LODSB
        CMP     AL, 13
        JZ      SHORT SAVTIM
        CMP     AL, BL
        JNZ     SHORT TIMERR
        MOV     BL, "."
        CALL    GETNUM
        JC      SHORT TIMERR
        MOV     DH, AH		;Position seconds
        LODSB
        CMP     AL, 13
        JZ      SHORT SAVTIM
        CMP     AL, BL
        JNZ     SHORT TIMERR  
        CALL    GETNUM
        JC      SHORT TIMERR
        MOV     DL, AH
        LODSB
        CMP     AL, 13
        JNZ     SHORT TIMERR
SAVTIM:
        MOV     AH, SETTIME
        INT     21H
        OR      AL, AL
        JZ      SHORT RET100	;Error in time?
TIMERR:
        MOV     DX, BADTIM
        MOV     AH, PRINTBUF
        INT     21H		;Print error message
        JMP     SHORT GETTIM	;Try again

GETBUF:
        MOV     AH, PRINTBUF
        INT     21H		;Print "Enter new date: "
        MOV     AH, INBUF
        MOV     DX, COMBUF
        INT     21H		;Get input line
        CALL    CRLF2
        MOV     SI, COMBUF+2
        CMP     BYTE [SI], 13	;Check if new date entered
        JZ      SHORT RET100
INLINE:
        CALL    GETNUM          ;Get one or two digit number
        JC      SHORT RET100
        MOV     DH, AH		;Put in position
        LODSB
        CMP     AL, BL
        JZ      SHORT NEXT
        CMP     BL, ":"		;Is it a date seperator?
        JNZ     SHORT DATESEP
        DEC     SI
        MOV     DL, 0
RET100: 
	RETN			;Time may have only an hour specified
DATESEP:
        CMP     AL, BH
        STC
        JNZ     SHORT RET100
NEXT:   
	CALL    GETNUM
        MOV     DL, AH		;Put in position
        RETN

GETNUM:
        CALL    INDIG
        JC      SHORT RET100
        MOV     AH, AL		;Save first digit
        CALL    INDIG		;Another digit?
        JC      SHORT OKRET
        AAD                     ;Convert unpacked BCD to decimal
        MOV     AH, AL
OKRET:
        OR      AL, 1
RET110: 
	RETN

INDIG:
        MOV     AL, [SI]
        SUB     AL, "0"
        JC      SHORT RET110
        CMP     AL, 10
        CMC
        JC      SHORT RET110
        INC     SI
        RETN

OUTCNT:
        LODSB
        CALL    _OUT
        LOOP    OUTCNT
        RETN

SHOW:
        MOV     AL, CH
        MOV     BH, "0"-" "	;Enable leading zero suppression
        CALL    OUT2
        MOV     AL, BL
        CALL    _OUT
        MOV     AL, CL
        CALL    OUT2
        MOV     AL, BL
        CALL    _OUT
        MOV     AL, DH
        CALL    OUT2
        CMP     BL, ":"          ;Are we outputting time?
        JNZ     SKIPIT
        MOV     AL, "."
        CALL    _OUT
SKIPIT: 
	MOV     AL, DL
OUT2:   ;Output binary number as two ASCII digits
        AAM			;Convert binary to unpacked BCD
        XCHG    AL, AH
        OR      AX, 3030H	;Add "0" bias to both digits
        CMP     AL, "0"		;Is MSD zero?
        JNZ     SHORT NOSUP
        SUB     AL, BH		;Suppress leading zero if enabled
NOSUP:
        MOV     BH, 0		;Disable zero suppression
        CALL    _OUT
        MOV     AL, AH
_OUT:
	;Print char in AL without affecting registers
        XCHG    AX, DX
        PUSH    AX
        MOV     AH, OUTCH
        INT     21H
        POP     AX
        XCHG    AX, DX
        RETN

CRLF2:
        MOV     AL, 13
        CALL    _OUT
        MOV     AL, 10
        JMP     _OUT

WEEKTAB:
	DB	"SunMonTueWedThuFriSat"
BADDAT:
	DB      13,10,"Invalid date$"
CURDAT:
	DB	"Current date is $"
NEWDAT:
	DB	13,10,"Enter new date: $"
BADTIM:
	DB	13,10,"Invalid time$"
CURTIM:
	DB	"Current time is $"
NEWTIM:
	DB	13,10,"Enter new time: $"
NEXTLINE:
	DB	13,10,"$"
COMBUF:
	DB      128,1,13
	DB	0


