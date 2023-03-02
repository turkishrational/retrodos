; ****************************************************************************
; COMMAND.COM (MSDOS 3.3 Command Interpreter) - RETRO DOS v3.0 by ERDOGAN TAN
; ----------------------------------------------------------------------------
; Last Update: 02/03/2023 ((Previous: 20/10/2018))
; ----------------------------------------------------------------------------
; Beginning: 21/04/2018 (COMMAND.COM v2.11) - 11/09/2018 (COMMAND.COM v3.30)
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 
; ----------------------------------------------------------------------------
;	    ((nasm command3.s -l command3.lst -o COMMAND.COM)) 	
; ----------------------------------------------------------------------------
; Derived from 'COMMAND.ASM' file of MSDOS 2.11 (IBM PCDOS v2.11) source code
; by Microsoft, 18/08/1983
; ****************************************************************************
; Modified from 'COMMAND2.S' (MSDOS 2.11 COMMAND.COM) source code
; in NASM syntax (by Erdogan Tan), 05/05/2018
; ----------------------------------------------------------------------------
;; 11/09/2018 - Erdogan Tan
; (Note: I haven't got MSDOS 3.3 COMMAND.COM src files; so, I need to continue
; by using disassembled COMMAND.COM -v3.30- and MSDOS 6.0 OS source files.)
; ----------------------------------------------------------------------------
; MSDOS 6.0 source files:
;;============================================================================
; This MSDOS source code is verified & modified by using IDA Pro Disassembler
; (MSDOS 3.30 COMMAND.COM) output in TASM syntax (11/09/2018..29/09/2018)
;;============================================================================
;
; COMMAND.COM v6.0 source files:
;      command1.asm, command2.asm, rucode.asm, stub.asm, rdata.asm, init.asm,
;      iparse.asm, uinit.asm, tcode.asm, tbatch.asm, tbatch2.asm, tfor.asm,
;      dir.asm, cratio.asm, tcmd1b.asm, tcmd2a.asm, tcmd2b.asm, tenv.asm,
;      tenv2.asm, tmisc1.asm, tmisc2.asm, tpipe.asm, parse2.asm, path1.asm,
;      path2.asm, tucode.asm, copy.asm, copypr1.asm, copypr2.asm, cparse.asm,
;      tparse.asm, tprintf.asm, loadhi.asm, tdata.asm, tspc.asm
;
; COMMAND.COM v2.11 source files:
;      COMMAND.ASM (+ DOSYM.ASM,DEVSYM.ASM,COMSW.ASM,COMEQU.ASM,IFEQU.ASM)
;      RUCODE.ASM, RDATA.ASM, INIT.ASM, UINIT.ASM
;      TCODE.ASM, TCODE2.ASM, TCODE3.ASM, TCODE4.ASM, TCODE5.ASM,
;      TUCODE.ASM, COPY.ASM, COPYPROC.ASM, CPARSE.ASM, TDATA.ASM, TSPC.ASM
;

;============================================================================
; SYSCALL.INC, MSDOS 6.0, 1991
;============================================================================
; 21/09/2018 - Retro DOS v3.0

;	SCCSID = @(#)syscall.asm	1.1 85/04/10
;BREAK <system call definitions>

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;SUBTTL	system call definitions
;PAGE

Abort				EQU 0	;  0	  0
STD_CON_INPUT			EQU 1	;  1	  1
Std_Con_Output			EQU 2	;  2	  2
Std_Aux_Input			EQU 3	;  3	  3
Std_Aux_Output			EQU 4	;  4	  4
Std_Printer_Output		EQU 5	;  5	  5
Raw_Con_IO			EQU 6	;  6	  6
RAW_CON_INPUT			EQU 7	;  7	  7
Std_Con_Input_No_Echo		EQU 8	;  8	  8
STD_CON_STRING_OUTPUT		EQU 9	;  9	  9
Std_Con_String_Input		EQU 10	; 10	  A
Std_Con_Input_Status		EQU 11	; 11	  B
STD_CON_INPUT_FLUSH		EQU 12	; 12	  C
DISK_RESET			EQU 13	; 13	  D
Set_Default_Drive		EQU 14	; 14	  E
FCB_Open			EQU 15	; 15	  F
FCB_Close			EQU 16	; 16	 10
Dir_Search_First		EQU 17	; 17	 11
Dir_Search_Next 		EQU 18	; 18	 12
FCB_Delete			EQU 19	; 19	 13
FCB_Seq_Read			EQU 20	; 20	 14
FCB_Seq_Write			EQU 21	; 21	 15
FCB_Create			EQU 22	; 22	 16
FCB_Rename			EQU 23	; 23	 17
GET_DEFAULT_DRIVE		EQU 25	; 25	 19
Set_DMA 			EQU 26	; 26	 1A
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;
Get_Default_DPB 		EQU 31	; 31	 1F
;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
FCB_Random_Read 		EQU 33	; 33	 21
FCB_Random_Write		EQU 34	; 34	 22
Get_FCB_File_Length		EQU 35	; 35	 23
Get_FCB_Position		EQU 36	; 36	 24
SET_INTERRUPT_VECTOR		EQU 37	; 37	 25
Create_Process_Data_Block	EQU 38	; 38	 26
FCB_Random_Read_Block		EQU 39	; 39	 27
FCB_Random_Write_Block		EQU 40	; 40	 28
Parse_File_Descriptor		EQU 41	; 41	 29
Get_Date			EQU 42	; 42	 2A
Set_Date			EQU 43	; 43	 2B
Get_Time			EQU 44	; 44	 2C
Set_Time			EQU 45	; 45	 2D
SET_VERIFY_ON_WRITE		EQU 46	; 46	 2E
; Extended functionality group
Get_DMA 			EQU 47	; 47	 2F
GET_VERSION			EQU 48	; 48	 30
Keep_Process			EQU 49	; 49	 31
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;
Get_DPB 			EQU 50	; 50	 32
;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
Set_CTRL_C_Trapping		EQU 51	; 51	 33
Get_InDOS_Flag			EQU 52	; 52	 34
Get_Interrupt_Vector		EQU 53	; 53	 35
Get_Drive_Freespace		EQU 54	; 54	 36
CHAR_OPER			EQU 55	; 55	 37
International			EQU 56	; 56	 38
;   Directory Group
MKDir				EQU 57	; 57	 39
RMDir				EQU 58	; 58	 3A
CHDir				EQU 59	; 59	 3B
;   File Group
Creat				EQU 60	; 60	 3C
OPEN				EQU 61	; 61	 3D
CLOSE				EQU 62	; 62	 3E
READ				EQU 63	; 63	 3F
Write				EQU 64	; 64	 40
Unlink				EQU 65	; 65	 41
LSEEK				EQU 66	; 66	 42
CHMod				EQU 67	; 67	 43
IOCTL				EQU 68	; 68	 44
XDUP				EQU 69	; 69	 45
XDup2				EQU 70	; 70	 46
Current_Dir			EQU 71	; 71	 47
;    Memory Group
ALLOC				EQU 72	; 72	 48
DEALLOC				EQU 73	; 73	 49
SETBLOCK			EQU 74	; 74	 4A
;    Process Group
Exec				EQU 75	; 75	 4B
EXIT				EQU 76	; 76	 4C
WAITPROCESS			EQU 77	; 77	 4D
Find_First			EQU 78	; 78	 4E
;   Special Group
Find_Next			EQU 79	; 79	 4F
; SPECIAL SYSTEM GROUP
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;
SET_CURRENT_PDB 		EQU 80	; 80	 50
GET_CURRENT_PDB 		EQU 81	; 81	 51
Get_In_Vars			EQU 82	; 82	 52
SetDPB				EQU 83	; 83	 53
;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
Get_Verify_On_Write		EQU 84	; 84	 54
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;
Dup_PDB 			EQU 85	; 85	 55
;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
Rename				EQU 86	; 86	 56
File_Times			EQU 87	; 87	 57
AllocOper			EQU 88	; 88	 58
; Network extention system calls
GetExtendedError		EQU 89	; 89	 59
CreateTempFile			EQU 90	; 90	 5A
CreateNewFile			EQU 91	; 91	 5B
LockOper			EQU 92	; 92	 5C Lock and Unlock
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;
ServerCall			EQU 93	; 93	 5D CommitAll, ServerDOSCall,
					;	    CloseByName, CloseUser,
					;	    CloseUserProcess,
					;	    GetOpenFileList
;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
UserOper			EQU 94	; 94	 5E Get and Set
AssignOper			EQU 95	; 95	 5F On, Off, Get, Set, Cancel
xNameTrans			EQU 96	; 96	 60
PathParse			EQU 97	; 97	 61
GetCurrentPSP			EQU 98	; 98	 62
Hongeul 			EQU 99	; 99	 63
ECS_CALL			EQU 99	; 99	 63  ;; DBCS support
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;
Set_Printer_Flag		EQU 100 ; 100	 64
;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
GetExtCntry			EQU 101 ; 101	 65
GetSetCdPg			EQU 102 ; 102	 66
ExtHandle			EQU 103 ; 103	 67
Commit				EQU 104 ; 104	 68
GetSetMediaID			EQU 105 ; 105	 69
IFS_IOCTL			EQU 107 ; 107	 6B
ExtOpen 			EQU 108 ; 108	 6C
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;                                                                          ;
;ifdef ROMEXEC
;ROM_FIND_FIRST			EQU 109 ; 109    6D
;ROM_FIND_NEXT			EQU 110 ; 110    6E
;ROM_EXCLUDE			EQU 111 ; 111	 6F			; M035
;endif
;                                                                          ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R             ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;
;
Set_Oem_Handler 		EQU 248 ; 248	 F8
OEM_C1				EQU 249 ; 249	 F9
OEM_C2				EQU 250 ; 250	 FA
OEM_C3				EQU 251 ; 251	 FB
OEM_C4				EQU 252 ; 252	 FC
OEM_C5				EQU 253 ; 253	 FD
OEM_C6				EQU 254 ; 254	 FE
OEM_C7				EQU 255 ; 255	 FF

;============================================================================
; DOSSYM.INC, MSDOS 6.0, 1991
;============================================================================
; 21/09/2018 - Retro DOS v3.0

;BREAK <Control character definitions>

c_DEL	    EQU     7Fh 		;    ASCII rubout or delete previous char
c_BS	    EQU     08h 		; ^H ASCII backspace
c_CR	    EQU     0Dh 		; ^M ASCII carriage return
c_LF	    EQU     0Ah 		; ^J ASCII linefeed
c_ETB	    EQU     17h 		; ^W ASCII end of transmission
c_NAK	    EQU     15h 		; ^U ASCII negative acknowledge
c_ETX	    EQU     03h 		; ^C ASCII end of text
c_HT	    EQU     09h 		; ^I ASCII tab

;============================================================================
; DIRENT.INC, MSDOS 6.0, 1991
;============================================================================
; 21/09/2018 - Retro DOS v3.0

;Break <Directory entry>

;	NOTE:  These offsets are also used in the DTA for
;	extended FCB SearchFirst/Next. DIR_NAME lines up
;	with the FCB filename field, and the rest of the
;	DIR_ENTRY fields follow. -DavidOls

;**	DIRENT.INC - FAT Directory Entry Definition
;
;	+---------------------------+
;	|  (12 BYTE) filename/ext   |	    0	    0
;	+---------------------------+
;	|     (BYTE) attributes     |	    11	    B
;	+---------------------------+
;	|    (10 BYTE) reserved     |	    12	    C
;	+---------------------------+
;	| (WORD) time of last write |	    22	    16
;	+---------------------------+
;	| (WORD) date of last write |	    24	    18
;	+---------------------------+
;	|   (WORD) First cluster    |	    26	    1A
;	+---------------------------+
;	|     (DWORD) file size     |	    28	    1C
;	+---------------------------+
;
;   First byte of filename  = E5 -> free directory entry
;			    = 00 -> end of allocated directory
;   Time:   Bits 0-4=seconds/2, bits 5-10=minute, 11-15=hour
;   Date:   Bits 0-4=day, bits 5-8=month, bits 9-15=year-1980
;

STRUC DIR_ENTRY
.DIR_NAME:	RESB  11	; file name
.DIR_ATTR:	RESB  1		; attribute bits
.DIR_CODEPG:	RESW  1		; code page DOS 4.00
.DIR_EXTCLUSTER: RESW 1		; extended attribute starting cluster
.DIR_ATTR2:	RESB  1		; reserved
.DIR_PAD:	RESB  5		; reserved for expansion
.DIR_TIME:	RESW  1		; time of last write
.DIR_DATE:	RESW  1		; date of last write
.DIR_FIRST:	RESW  1		; first allocation unit of file
.DIR_SIZE_L:	RESW  1		; low 16 bits of file size
.DIR_SIZE_H:	RESW  1		; high 16 bits of file size
.size:

;	    Caution: An extended FCB SearchFirst/Next on a network
;	    drive under Novell Netware 286 or 386 returns the time/date
;	    in the SIZE fields for subdirectory files. Ordinarily,
;	    this field is zero for subdirectory files.

ENDSTRUC

ATTR_READ_ONLY	equ	 1h
ATTR_HIDDEN	equ	 2h
ATTR_SYSTEM	equ	 4h
ATTR_VOLUME_ID	equ	 8h
ATTR_DIRECTORY	equ	10h
ATTR_ARCHIVE	equ	20h
ATTR_DEVICE	equ	40h	; This is a VERY special bit.
				;   NO directory entry on a disk EVER
				;   has this bit set. It is set non-zero
				;   when a device is found by GETPATH

ATTR_ALL	equ	ATTR_HIDDEN+ATTR_SYSTEM+ATTR_DIRECTORY
				; OR of hard attributes for FINDENTRY

ATTR_IGNORE	equ	ATTR_READ_ONLY+ATTR_ARCHIVE+ATTR_DEVICE
				; ignore this(ese) attribute(s) during
				; search first/next

ATTR_CHANGEABLE equ	ATTR_READ_ONLY+ATTR_HIDDEN+ATTR_SYSTEM+ATTR_ARCHIVE
				; changeable via CHMOD

DIRFREE 	equ	0E5h	; stored in dir_name[0] to indicate free slot

;============================================================================
; ERROR.INC, MSDOS 6.0, 1991
;============================================================================
; 21/09/2018 - Retro DOS v3.0

;**	ERROR.INC - DOS Error Codes
;
;    The newer (DOS 2.0 and above) "XENIX-style" calls
;    return error codes through AX.	If an error occurred then
;    the carry bit will be set and the error code is in AX.	If no error
;    occurred then the carry bit is reset and AX contains returned info.
;
;    Since the set of error codes is being extended as we extend the operating
;    system, we have provided a means for applications to ask the system for a
;    recommended course of action when they receive an error.
;
;    The GetExtendedError system call returns a universal error, an error
;    location and a recommended course of action.	The universal error code is
;    a symptom of the error REGARDLESS of the context in which GetExtendedError
;    is issued.


;	2.0 error codes

error_invalid_function		EQU	1
ERROR_FILE_NOT_FOUND		EQU	2
error_path_not_found		EQU	3
ERROR_TOO_MANY_OPEN_FILES	EQU	4
ERROR_ACCESS_DENIED		EQU	5
error_invalid_handle		EQU	6
error_arena_trashed		EQU	7
ERROR_NOT_ENOUGH_MEMORY 	EQU	8
error_invalid_block		EQU	9
error_bad_environment		EQU	10
ERROR_BAD_FORMAT		EQU	11
error_invalid_access		EQU	12
error_invalid_data		EQU	13
;**** reserved			EQU	14	; *****
error_invalid_drive		EQU	15
error_current_directory 	EQU	16
error_not_same_device		EQU	17
error_no_more_files		EQU	18

;	These are the universal int 24 mappings for the old INT 24 set of errors

ERROR_WRITE_PROTECT		EQU	19
error_bad_unit			EQU	20
error_not_ready 		EQU	21
error_bad_command		EQU	22
error_CRC			EQU	23
error_bad_length		EQU	24
error_Seek			EQU	25
error_not_DOS_disk		EQU	26
error_sector_not_found		EQU	27
error_out_of_paper		EQU	28
error_write_fault		EQU	29
error_read_fault		EQU	30
ERROR_GEN_FAILURE		EQU	31

;	the new 3.0 error codes reported through INT 24

error_sharing_violation 	EQU	32
error_lock_violation		EQU	33
error_wrong_disk		EQU	34
ERROR_FCB_UNAVAILABLE		EQU	35
ERROR_SHARING_BUFFER_EXCEEDED	EQU	36
error_Code_Page_Mismatched	EQU	37    ; DOS 4.00	;AN000;
error_handle_EOF		EQU	38    ; DOS 4.00	;AN000;
ERROR_HANDLE_DISK_FULL		EQU	39    ; DOS 4.00	;AN000;

;	New OEM network-related errors are 50-79

error_not_supported		EQU	50

error_net_access_denied		EQU	65	;M028

;	End of INT 24 reportable errors

error_file_exists		EQU	80
error_DUP_FCB			EQU	81	; *****
error_cannot_make		EQU	82
error_FAIL_I24			EQU	83

;	New 3.0 network related error codes

error_out_of_structures 	EQU	84
error_Already_assigned		EQU	85
error_invalid_password		EQU	86
error_invalid_parameter 	EQU	87
error_NET_write_fault		EQU	88
error_sys_comp_not_loaded	EQU	90    ; DOS 4.00	;AN000;

;============================================================================
; DEVSYM.INC, MSDOS 6.0, 1991
;============================================================================
; 22/09/2018 - Retro DOS v3.0

;**	DevSym.inc - Device Symbols

;	THE DEVICE TABLE LIST HAS THE FORM:

STRUC SYSDEV
.NEXT:	RESD 1			;POINTER TO NEXT DEVICE HEADER
.ATT:	RESW 1			;ATTRIBUTES OF THE DEVICE
.STRAT:	RESW 1			;STRATEGY ENTRY POINT
.INT:	RESW 1			;INTERRUPT ENTRY POINT
.NAME:	RESB 8			;NAME OF DEVICE (ONLY FIRST BYTE USED FOR BLOCK)
.size:
ENDSTRUC

; 24/09/2018
DEVTYP	EQU   8000H	; BIT 15 - 1  IF CHAR, 0 IF BLOCK

;============================================================================
; CURDIR.INC, MSDOS 6.0, 1991
;============================================================================
; 21/09/2018 - Retro DOS v3.0

DIRSTRLEN	EQU	64+3		; Max length in bytes of directory strings

;============================================================================
; COMEQU.ASM, MSDOS 6.0, 1991
;============================================================================
; 21/09/2018 - Retro DOS v3.0

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;	SCCSID = @(#)comequ.asm 1.1 85/05/14
;	SCCSID = @(#)comequ.asm 1.1 85/05/14
;*************************************
; COMMAND EQUs which are not switch dependant

;		include	curdir.inc	; to get DIRSTRLEN
;		Note dossym.inc must already have been included!

GET_COMMAND_STATE	equ	5500h	; check for existing COMMAND
GET_ROMCOMMAND_STATE	equ	5501h	; check for existing ROM COMMAND

SYM		EQU	">"

LINESPERPAGE	EQU	25		;AC000; default lines per page

NORMPERLIN	EQU	1
WIDEPERLIN	EQU	5
COMBUFLEN	EQU	128		; Length of commmand buffer
BatLen		EQU	32		; buffer for batch files
YES_ECHO	EQU	1		; echo line
NO_ECHO 	EQU	0		; don't echo line
No_Echo_Char	EQU	"@"             ; don't echo line if this is first char
call_in_progress EQU	1		; indicate we're in the CALL command
length_call	EQU	4		; length of CALL
max_nest	EQU    10		; max # levels of batch nesting allowed
FAIL_ALLOWED	EQU    00001000b	; critical error
RETRY_ALLOWED	EQU    00010000b	; critical error
IGNORE_ALLOWED	EQU    00100000b	; critical error
nullcommand	EQU    1		; no command on command line
end_of_line	EQU    -1		;AN000; end of line return from parser
end_of_line_out EQU	0		;AN000; end of line for output
end_of_line_in	EQU	0dh		;AN000; end of line for input
result_number	EQU	1		;AN000; number returned from parser
result_string	EQU	3		;AN000; string returned from parser
result_filespec EQU	5		;AN000; filespec returned from parser
result_drive	EQU	6		;AN000; drive returned from parser
result_date	EQU	7		;AN000; date returned from parser
result_time	EQU	8		;AN000; time returned from parser
result_no_error EQU	0		;AN000; no error returned from parser
no_cont_flag	EQU	0		;AN000; no control flags for message
util_msg_class	EQU	-1		;AN000; message class for utility
ext_msg_class	EQU	1		;AN000; message class for extended error
parse_msg_class EQU	2		;AN000; message class for parse error
crit_msg_class	EQU	3		;AN000; message class for critical error
ext_crlf_class	EQU	081h		;AN054; message class for extended error with no CRLF
colon_char	EQU	":"             ;AN000; colon character
crt_ioctl_ln	EQU	14		;AN000; default length of data for display ioctl
text_mode	EQU	1		;AN000; text mode return from ioctl
get_generic	EQU	07Fh		;AN000; generic ioctl - get device info
set_crit_dev	EQU	0100H		;AN000; device attribute for critical error on I/0
mult_ansi	EQU	01Ah		;AC064; multiplex for ansi.sys
mult_shell_get	EQU	01902h		;AC065; multiplex for Shell - get next command
mult_shell_brk	EQU	01903h		;AN000; multiplex for Shell - ^C batch check
shell_action	equ	0ffh		;AN000; SHELL - return for taking SHELL specific action
bat_not_open	EQU	-1		;AN000; batch handle will be set to this if not open
bat_open_handle EQU	19		;AN000; handle will be in this position in JFN table
Ptr_seg_pos	equ	7		;AN000; Offset from start of message block for subst segment
Ptr_off_pos	equ	5		;AN000; Offset from start of message block for subst offset
%define Parm_off_pos	word [2]	;AN000; Offset from start of subst list for subst offset
parm_block_size equ	11		;AN000; size of message subst block
blank		equ	" "             ;AN000; blank character
no_subst	equ	0		;AN000; no substitutions for messages
one_subst	equ	1		;AN000; one substitution for messages
no_handle_out	equ	-1		;AN000; use function 1 thru 12 for message retriever
res_subst	equ	2		;AN000; offset from start of message definition to number of subst
read_open_mode	equ   0000000000000000b ;AN024; extended open mode for read
deny_write	equ   0000000000100000b	; deny write sharing mode ;M031
deny_none	equ   0000000001000000b	; deny none sharing mode ;Myyy	
read_open_flag	equ   0000000100000001b ;AN000; extended open flags for read
write_open_mode equ   0000000000000001b ;AN024; extended open mode for read
write_open_flag equ   0000000100000001b ;AN000; extended open flags for read
creat_open_flag equ   0000000100010010b ;AN000; extended open flags for read
capital_A	equ	'A'             ;AC000;
vbar		equ	'|'             ;AC000;
labracket	equ	'<'             ;AC000;
rabracket	equ	'>'             ;AC000;
dollar		equ	'$'             ;AC000;
lparen		equ	'('             ;AC000;
rparen		equ	')'             ;AC000;
nullrparen	equ	29h		;AC000;
in_word 	equ	4e49h		;AC000; 'NI'  ('IN' backwards)
do_word 	equ	4f44h		;AC000; 'OD'  ('DO' backwards)
star		equ	'*'             ;AC000;
plus_chr	equ	'+'             ;AC000;
small_a 	equ	'a'             ;AC000;
small_z 	equ	'z'             ;AC000;
dot_chr 	equ	'.'             ;AC000;
tab_chr 	equ	9		;AN032;
equal_chr	equ	'='             ;AN032;
semicolon	equ	';'             ;AN049;
dot_qmark	equ	2e3fh		;AC000; '.?'
dot_colon	equ	2e3ah		;AC000; '.:'
capital_n	equ	0		;AC000; result from Y/N call if N entered
capital_y	equ	1		;AC000; result from Y/N call if Y entered
AppendInstall	equ	0B700H		;AN020; append install check
AppendDOS	equ	0B702H		;AN020; append DOS version check
AppendGetState	equ	0B706H		;AN020; append get current state
AppendSetState	equ	0B707H		;AN020; append set current state
AppendTruename	equ	0B711H		;AN042; Get file's real location for Batch
search_attr	equ	ATTR_READ_ONLY+ATTR_HIDDEN+ATTR_DIRECTORY  ;AC042;

;*************************************
;* PARSE ERROR MESSAGES
;*************************************

MoreArgs_Ptr	equ	1		;AN000;"Too many parameters" message number
LessArgs_Ptr	equ	2		;AN000;"Required parameter missing" message number
BadSwt_Ptr	equ	3		;AN000;"Invalid switch" message number
BadParm_Ptr	equ	10		;AN000;"Invalid parameter" message number

;*************************************
;* EQUATES FOR MESSAGE RETRIEVER
;*************************************

GET_EXTENDED_MSG	EQU	0	;AN000;  get extended message address
SET_EXTENDED_MSG	EQU	1	;AN000;  set extended message address
GET_PARSE_MSG		EQU	2	;AN000;  get parse message address
SET_PARSE_MSG		EQU	3	;AN000;  set parse message address
GET_CRITICAL_MSG	EQU	4	;AN000;  get critical message address
SET_CRITICAL_MSG	EQU	5	;AN000;  set critical message address
MESSAGE_2F		EQU	46	;AN000;  minor code for message retriever

;*********************************
;* EQUATES FOR INT 10H
;*********************************

VIDEO_IO_INT		EQU	10H	;AN000;  equate for int 10h
SET_VIDEO_MODE		EQU	0	;AN000;  set video mode
SET_CURSOR_POSITION	EQU	2	;AN000;  set new cursor position
SCROLL_VIDEO_PAGE	EQU	6	;AN000;  scroll active page up
VIDEO_ATTRIBUTE 	EQU	7	;AN000;  attribute to be used on blank line
SET_COLOR_PALETTE	EQU	11	;AN000;  set color for video
GET_VIDEO_STATE 	EQU	15	;AN000;  get current video state
VIDEO_ALPHA		EQU	3	;AN000;  alpha video is 3 or below
VIDEO_BW		EQU	7	;AN000;  mode for 80X25 black & white

AltPipeChr	equ	"|"             ; alternate pipe character

FCB		EQU	5CH

STRUC VARSTRUC
.ISDIR:		RESB	1
.SIZ:		RESB	1
.TTAIL:		RESW	1
.INFO:		RESB	1
.BUF:		RESB	DIRSTRLEN + 20
.size:
ENDSTRUC
;
; Flags for internal command parsing
;
fCheckDrive	equ	00000001b	; validate drive letter
fSwitchAllowed	equ	00000010b	; switches allowed
fLimitHelp	equ	00000100b	; /? must appear alone

;
; Test switches
;
fParse		EQU	0001h		; display results of parseline

;
; Batch segment structure
;
;   BYTE    type of segment
;   BYTE    echo state of parent on entry to batch file
;   WORD    segment of last batch file
;   WORD    segment for FOR command
;   BYTE    FOR flag state on entry to batch file
;   DWORD   offset for next line
;   10 WORD pointers to parameters.  -1 is empty parameter
;   ASCIZ   file name (with . and ..)
;   BYTES   CR-terminated parameters
;   BYTE    0 flag to indicate end of parameters
;

BATCHTYPE   equ 0

STRUC BATCHSEGMENT
.BatType:	RESB	1		; signature
.BatEchoFlag:	RESB	1		; G state of echo
;.BatchEOF:	RESB	1		; records if EOF reached on file
.BatLast: 	RESW	1		; G segment of last batch file
.BatForPtr:	RESW	1		; G segment for FOR command
.BatForFlag:	RESB	1		; G state of FOR
.BatSeek:	RESD	1		; lseek position of next char
.BatParm:	RESW	10		; pointers to parameters
.BatFile:	RESB	1		; beginning of batch file name
.SIZE:
ENDSTRUC

ANULL		equ	0		; terminates an argv string
ARGMAX		equ	64		; max args on a command line
ARGBLEN 	equ	2*128		; 1char each plus term NUL
tplen		equ	64		; max size of one argument
arg_cnt_error	equ	1		; number of args > MAXARG
arg_buf_ovflow	equ	2		; overflowed argbuffer

STRUC ARGV_ELE				; elements in the argv array
.argpointer:	RESW	1		; pointer to the argstring
.argflags:	RESB	1		; cparse flags for this argstring
.argstartel:	RESW	1		; the result of cparse's [STARTEL]
.arglen:	RESW	1		; cparse's char count + one (for null)
.argsw_word:	RESW	1		; any switches after this? what kinds?
.arg_ocomptr:	RESW	1		; pointer into original command string
.SIZE:
ENDSTRUC

STRUC ARG_UNIT
.argv:		RESB	ARGMAX * ARGV_ELE.SIZE
.argvcnt:	RESW	1		; number of arguments
.argswinfo:	RESW	1		; Switch information for entire line
.argbuf:	RESW	ARGBLEN		; storage for argv strings
.argforcombuf:	RESB	COMBUFLEN	; Original for loop command string
.SIZE:
ENDSTRUC

; Equates for initialization
;
INITINIT	equ	01h		; initialization in progress
INITSPECIAL	equ	02h		; in initialization time/date routine
INITCTRLC	equ	04h		; already in ^C handler

;=============================================================================
; PDB.INC, MSDOS 6.0, 1991
;=============================================================================
; 24/09/2018 - Retro DOS v3.0 (08/07/2018, 'msdos3.s')

;**	Process data block (otherwise known as program header)

;	These offset are documented in the MSDOS Encyclopedia, so nothing
;	can be rearranged here, ever. Reserved areas are probably safe
;	for use.

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
.JFN_Length:	resw 1		; number of handles allowed
.JFN_Pointer:	resd 1		; pointer to JFN table
.Next_PDB:	resd 1		; pointer to nested PDB's
.InterCon:	resb 1	; MSDOS 6.0 ; *** jh-3/28/90 *** 
.Append:	resb 1	; MSDOS 6.0 ; *** Not sure if still used ***
.Novell_Used:	resb 2	; MSDOS 6.0 ; Novell shell (redir) uses these
.Version:	resw 1	; MSDOS 6.0 ; DOS version reported to this app
.PAD1:		resb 14 ; 0Eh
.CALL_SYSTEM:	resb 5		; portable method of system call
.PAD2:		resb 7		; reserved so FCB 1 can be used as
				;  an extended FCB
;endstruc 	; MSDOS 3.3
	  	; MSDOS 6.0
.FCB1:		resb 16 ; 10h	; default FCB 1
.FCB2:		resb 16 ; 10h	; default FCB 2
.PAD3:		resb 4		; not sure if this is used by PDB_FCB2
.TAIL:		resb 128	; command tail and default DTA
endstruc

;=============================================================================
; VERSIONA.INC, MSDOS 6.0, 1991
;=============================================================================
; 24/09/2018 - Retro DOS v3.0

;major_version       equ     6       ;Major DOS version
;minor_version       equ     00      ;Minor DOS Version

;expected_version    equ     (MINOR_VERSION SHL 8)+MAJOR_VERSION

; MSDOS 3.3 COMMAND.COM
MAJOR_VERSION	EQU 3
MINOR_VERSION	EQU 30

EXPECTED_VERSION EQU (MINOR_VERSION<<8)+MAJOR_VERSION	

;-----------------------------------------------------------------------------
; 21/09/2018
;-----------------------------------------------------------------------------
; Retro DOS v3.0 NOTE:  
;	Following source code is as disassembled code of MSDOS 3.3 COMMAND.COM
;	with minor modifications which are done by me (Erdogan Tan).
;	.. but comments and descriptions are from MSDOS 6.0 COMMAND.COM
;	source code files (written by using MASM syntax).
;-----------------------------------------------------------------------------
; All of this Retro DOS 3.0 (2018) source code has been written by using
; NASM (2.11) x86 assembly language/compiler syntax.	
;----------------------------------------------------------------------------- 	

;=============================================================================
; COMMAND1.ASM, MSDOS 6.0, 1991
;=============================================================================
; 21/09/2018 - Retro DOS v3.0

;	page ,132
;	title	COMMAND - resident code for COMMAND.COM
;	name	COMMAND

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;*****************************************************************************
;
; MODULE:	       COMMAND.COM
;
; DESCRIPTIVE NAME:    Default DOS command interpreter
;
; FUNCTION:	       This version of COMMAND is divided into three distinct
;		       parts.  First is the resident portion, which includes
;		       handlers for interrupts	23H (Cntrl-C), 24H (fatal
;		       error), and 2EH (command line execute); it also has
;		       code to test and, if necessary, reload the transient
;		       portion. Following the resident is the init code, which
;		       is overwritten after use.  Then comes the transient
;		       portion, which includes all command processing (whether
;		       internal or external).  The transient portion loads at
;		       the end of physical memory, and it may be overlayed by
;		       programs that need as much memory as possible. When the
;		       resident portion of command regains control from a user
;		       program, a check sum is performed on the transient
;		       portion to see if it must be reloaded.  Thus programs
;		       which do not need maximum memory will save the time
;		       required to reload COMMAND when they terminate.
;
; ENTRY POINT:	       PROGSTART
;
; INPUT:	       command line at offset 81H
;
; EXIT_NORMAL:	       No exit from root level command processor.  Can exit
;		       from a secondary command processor via the EXIT
;		       internal command.
;
; EXIT_ERROR:	       Exit to prior command processor if possible, otherwise
;		       hang the system.
;
; INTERNAL REFERENCES:
;
;     ROUTINES:        See the COMMAND Subroutine Description Document
;		       (COMMAND.DOC)
;
;     DATA AREAS:      See the COMMAND Subroutine Description Document
;		       (COMMAND.DOC)
;
; EXTERNAL REFERENCES:
;
;      ROUTINES:       none
;
;      DATA AREAS:     none
;
;*****************************************************************************
;
;			      REVISION HISTORY
;			      ----------------
;
; DOS 1.00 to DOS 3.30
; --------------------------
; SEE REVISION LOG IN COPY.ASM ALSO
;
; REV 1.17
;    05/19/82  Fixed bug in BADEXE error (relocation error must return to
;	       resident since the EXELOAD may have overwritten the transient.
;
; REV 1.18
;    05/21/82  IBM version always looks on drive A
;	       MSVER always looks on default drive
;
; REV 1.19
;    06/03/82  Drive spec now entered in command line
;    06/07/82  Added VER command (print DOS version number) and VOL command
;	       (print volume label)
;
; REV 1.20
;    06/09/82  Prints "directory" after directories
;    06/13/82  MKDIR, CHDIR, PWD, RMDIR added
;
; REV 1.50
;	       Some code for new 2.0 DOS, sort of HACKey.  Not enough time to
;	       do it right.
;
; REV 1.70
;	       EXEC used to fork off new processes
;
; REV 1.80
;	       C switch for single command execution
;
; REV 1.90
;	       Batch uses XENIX
;
; Rev 2.00
;	       Lots of neato stuff
;	       IBM 2.00 level
;
; Rev 2.01
;	       'D' switch for date time suppression
;
; Rev 2.02
;	       Default userpath is NUL rather than BIN
;		       same as IBM
;	       COMMAND split into pieces
;
; Rev 2.10
;	       INTERNATIONAL SUPPORT
;
; Rev 2.50
;	       all the 2.x new stuff -MU
;
; Rev 3.30     (Ellen G)
;	       CALL internal command (TBATCH2.ASM)
;	       CHCP internal command (TCMD2B.ASM)
;	       INT 24H support of abort, retry, ignore, and fail prompt
;	       @ sign suppression of batch file line
;	       Replaceable environment value support in batch files
;	       INT 2FH calls for APPEND
;	       Lots of PTR fixes!
;
; Beyond 3.30 to forever  (Ellen G)
; ----------------------
;
; A000 DOS 4.00  -	Use SYSPARSE for internal commands
;			Use Message Retriever services
;			/MSG switch for resident extended error msg
;			Convert to new capitalization support
;			Better error recovery on CHCP command
;			Code page file tag support
;			TRUENAME internal command
;			Extended screen line support
;			/P switch on DEL/ERASE command
;			Improved file redirection error recovery
;	(removed)	Improved batch file performance
;			Unconditional DBCS support
;			Volume serial number support
;	(removed)	COMMENT=?? support
;
; A001	PTM P20 	Move system_cpage from TDATA to TSPC
;
; A002	PTM P74 	Fix PRESCAN so that redirection symbols do not
;			require delimiters.
;
; A003	PTM P5,P9,P111	Included in A000 development
;
; A004	PTM P86 	Fix IF command to turn off piping before
;			executing
;
; A005	DCR D17 	If user specifies an extension on the command
;			line search for that extension only.
;
; A006	DCR D15 	New message for MkDir - "Directory already
;			exists"
;
; A007	DCR D2		Change CTTY so that a write is done before XDUP
;
; A008	PTM P182	Change COPY to set default if invalid function
;			returned from code page call.
;
; A009	PTM P179	Add CRLF to invalid disk change message
;
; A010	DCR D43 	Allow APPEND to do a far call to SYSPARSE in
;			transient COMMAND.
;
; A011	DCR D130	Change redirection to overwrite an EOF mark
;			before appending to a file.
;
; A012	PTM P189	Fix redirection error recovery.
;
; A013	PTM P330	Change date format
;
; A014	PTM P455	Fix echo parsing
;
; A015	PTM P517	Fix DIR problem with * vs *.
;
; A016	PTM P354	Fix extended error message addressing
;
; A017	PTM P448	Fix appending to 0 length files
;
; A018	PTM P566,P3903	Fix parse error messages to print out parameter
;			the parser fails on. Fail on duplicate switches.
;
; A019	PTM P542	Fix device name to be printed correctly during
;			critical error
;
; A020	DCR D43 	Set append state off while in DIR
;
; A021	PTM P709	Fix CTTY printing ascii characters.
;
; A022	DCR D209	Enhanced error recovery
;
; A023	PTM P911	Fix ANSI.SYS IOCTL structure.
;
; A024	PTM P899	Fix EXTOPEN open modes.
;
; A025	PTM P922	Fix messages and optimize PARSE switches
;
; A026	DCR D191	Change redirection error recovery support.
;
; A027	PTM P991	Fix so that KAUTOBAT & AUTOEXEC are terminated
;			with a carriage return.
;
; A028	PTM P1076	Print a blank line before printing invalid
;			date and invalid time messages.
;
; A029	PTM P1084	Eliminate calls to parse_check_eol in DATE
;			and TIME.
;
; A030	DCR D201	New extended attribute format.
;
; A031	PTM P1149	Fix DATE/TIME add blank before prompt.
;
; A032	PTM P931	Fix =ON, =OFF for BREAK, VERIFY, ECHO
;
; A033	PTM P1298	Fix problem with system crashes on ECHO >""
;
; A034	PTM P1387	Fix COPY D:fname+,, to work
;
; A035	PTM P1407	Fix so that >> (appending) to a device does
;			do a read to determine eof.
;
; A036	PTM P1406	Use 69h instead of 44h to get volume serial
;			so that ASSIGN works correctly.
;
; A037	PTM P1335	Fix COMMAND /C with FOR
;
; A038	PTM P1635	Fix COPY so that it doesn't accept /V /V
;
; A039	DCR D284	Change invalid code page tag from -1 to 0.
;
; A040	PTM P1787	Fix redirection to cause error when no file is
;			specified.
;
; A041	PTM P1705	Close redirected files after internal APPEND
;			executes.
;
; A042	PTM P1276	Fix problem of APPEND paths changes in batch
;			files causing loss of batch file.
;
; A043	PTM P2208	Make sure redirection is not set up twice for
;			CALL'ed batch files.
;
; A044	PTM P2315	Set switch on PARSE so that 0ah is not used
;			as an end of line character
;
; A045	PTM P2560	Make sure we don't lose parse, critical error,
;			and extended message pointers when we EXIT if
;			COMMAND /P is the top level process.
;
; A046	PTM P2690	Change COPY message "fn File not found" to
;			"File not found - fn"
;
; A047	PTM P2819	Fix transient reload prompt message
;
; A048	PTM P2824	Fix COPY path to be upper cased.  This was broken
;			when DBCS code was added.
;
; A049	PTM P2891	Fix PATH so that it doesn't accept extra characters
;			on line.
;
; A050	PTM P3030	Fix TYPE to work properly on files > 64K
;
; A051	PTM P3011	Fix DIR header to be compatible with prior releases.
;
; A052	PTM P3063,P3228 Fix COPY message for invalid filename on target.
;
; A053	PTM P2865	Fix DIR to work in 40 column mode.
;
; A054	PTM P3407	Code reduction and critical error on single line
;	PTM P3672	(Change to single parser exported under P3407)
;
; A055	PTM P3282	Reset message service variables in INT 23h to fix
;			problems with breaking out of INT 24h
;
; A056	PTM P3389	Fix problem of environment overlaying transient.
;
; A057	PTM P3384	Fix COMMAND /C so that it works if there is no space
;			before the "string".  EX: COMMAND /CDIR
;
; A058	PTM P3493	Fix DBCS so that CPARSE eats second character of
;			DBCS switch.
;
; A059	PTM P3394	Change the TIME command to right align the display of
;			the time.
;
; A060	PTM P3672	Code reduction - change PARSE and EXTENDED ERROR
;			messages to be disk based.  Only keep them if /MSG
;			is used.
;
; A061	PTM P3928	Fix so that transient doesn't reload when breaking
;			out of internal commands, due to substitution blocks
;			not being reset.
;
; A062	PTM P4079	Fix segment override for fetching address of environment
;			of parent copy of COMMAND when no COMSPEC exists in
;			secondary copy of environment.	Change default slash in
;			default comspec string to backslash.
;
; A063	PTM P4140	REDIRECTOR and IFSFUNC changed interface for getting
;			text for critical error messages.
;
; A064	PTM P4934	Multiplex number for ANSI.SYS changed due to conflict
;	5/20/88 	with Microsoft product already shipped.
;
; A065	PTM P4935	Multiplex number for SHELL changed due to conflict
;	 5/20/88	with Microsoft product already shipped.
;
; A066	PTM P4961	DIR /W /P scrolled first line off the screen in some
;	 5/24/88	cases; where the listing would barely fit without the
;			header and space remaining.
;
; A067	PTM P5011	For /E: values of 993 to 1024 the COMSPEC was getting
;	 6/6/88 	trashed.  Turns out that the SETBLOCK for the new
;			environment was putting a "Z block" marker in the old
;			environment.  The fix is to move to the old environment
;			to the new environment before doing the SETBLOCK.
;
; A068  PTM P5568       IR79754 APPEND /x:on not working properly with DIR/VOL
;        09/19/88       because the check for APPEND needed to be performed
;                       before the DIR's findfirst.
;
; A069  PTM P5726       IR80540 COMSPEC_flag not properly initialized and
;        10/30/88       executed.  Causing AUSTIN problem testing LAN/DW4 re-
;                       loading trans w/new comspec with no user change comspec.
;
; A070  PTM P5734       IR80484 Batch file causes sys workspace to be corrupted.
;        11/05/88       Expansion of environment variables into batch line of
;                       128 chars was not being counted and "%" which should be
;                       ignored were being counted.
;
; A071  PTM P5854       IR82061 Invalid COMMAND.COM when Word Perfect, Prompt
;        03/02/89       used.  Comspec_flag was not in protected data file be-
;                       ing included in checksum and was being overwritten by
;                       WP.  Moved var from Tspc to Tdata so Trans would reload.
;                       Also removed fix A069 (because flag now protected).
;
; C001  VERSION 4.1     Add new internal command - SERVICE - to display the DOS
;        07/25/89       version and CSD version in U.S. date format.  Files
;                       changed - TRANMSG,.SKL,COMMAND1,TDATA,TCMD2A,USA.MSG
;
;***********************************************************************************

;
;	Revision History
;	================
;
;	M021	SR	08/23/90	Fixed Ctrl-C handler to handle Ctrl-C
;					at init time (date/time prompt)
;

;
;.xcref
;.xlist
;	include dossym.inc		; basic DOS symbol set
;	include syscall.inc		; DOS function names
;	include comsw.asm		; build version info
;	include comequ.asm		; common command.com symbols
;	include resmsg.equ		; resident message names
;
;	include comseg.asm		;segment ordering
;.list
;.cref

;CODERES segment public byte
;CODERES ends
;
;DATARES 	segment public byte
;		extrn	AccDen:byte
;		extrn	Batch:word
;		extrn	EchoFlag:byte
;		extrn	ExeBad:byte
;		extrn	ExecEMes:byte
;		extrn	ExecErrSubst:byte
;		extrn	ExtCom:byte
;		extrn	ForFlag:byte
;		extrn	IfFlag:byte
;		extrn	InitFlag:BYTE
;		extrn	Nest:word
;		extrn	PipeFlag:byte
;		extrn	RBadNam:byte
;		extrn	RetCode:word
;		extrn	SingleCom:word
;		extrn	TooBig:byte
;
;		extrn	OldDS:word
;
;DATARES 	ends
;
;
;INIT		segment public para
;		extrn	ConProc:near
;		extrn	Init_Contc_SpecialCase:near
;INIT		ends

; ----------------------------------------------------------------------------
; START OF RESIDENT PORTION
; ----------------------------------------------------------------------------
; SEGMENT - CODERES
; ----------------------------------------------------------------------------

	;[ORG	0]

;ZERO	EQU	$

	[ORG 100H]

	; 21/09/2018 - Retro DOS v3.0
StartCode:
	jmp	CONPROC

;***	EXEC error handling
;
;	COMMAND has issued an EXEC system call and it has returned an error.
;	We examine the error code and select an appropriate message.

;	Bugbug:	optimize reg usage in following code?  Careful of DX!
;	Condense the error scan?
;	RBADNAM is checked by transient, no need here?
;	Move below Ext_Exec.

EXEC_ERR:
	mov     dx,ACCDEN
	cmp     ax,ERROR_ACCESS_DENIED	; 5
	jz      short GOTEXECEMES	; access denied
	mov     dx,RBADNAM
	cmp     ax,ERROR_FILE_NOT_FOUND ; 2
	jz      short GOTEXECEMES	; file not found
	mov     dx,TOOBIG
	cmp     ax,ERROR_NOT_ENOUGH_MEMORY ; 8
	jz      short GOTEXECEMES
	mov     dx,EXEBAD
	cmp     ax,ERROR_BAD_FORMAT	; 0Bh
	jz      short GOTEXECEMES	; bad exe file
DEFAULT_MESSAGE:
	mov     dx,EXECEMES
GOTEXECEMES:
	push    cs
	pop     ds
	call    RDISPMSG
	jmp     short NOEXEC

;***	EXEC call
;
;	The transient has set up everything for an EXEC system call.
;	For cleanliness, we issue the EXEC here in the resident 
;	so that we may be able to recover cleanly upon success.
;
;	CS,DS,ES,SS = DATARES seg addr

EXT_EXEC:
;SR;
; The words put on the stack by the stub will be popped off when we finally
;jump to LodCom (by LodCom).
;
	int	21h			; do the exec
EXEC_RET:
	jc	short EXEC_ERR		; exec failed

;	The exec has completed. Retrieve the exit code.

EXEC_WAIT:
	mov	ah,WAITPROCESS ; 4Dh	; get errorlevel
	int	21h			; get the return code
	mov     [cs:RETCODE],ax

;	See if we can reload the transient. The external command
;	may have overwritten part of the transient.

NOEXEC:
;SR;
; ds = es = ss = DATARES when we jump to LodCom
;
	jmp	LODCOM

;***	Int 23 (ctrl-c) handler
;
;	This is the default system INT 23 handler.  All processes
;	(including COMMAND) get it by default.  There are some
;	games that are played:  We ignore ^C during most of the
;	INIT code.  This is because we may perform an ALLOC and
;	diddle the header!  Also, if we are prompting for date/time
;	in the init code, we are to treat ^C as empty responses.

;	Bugbug:	put init ctrl-c handling in init module.

;SR;
; The stub has pushed the previous ds and DATARES onto the stack. We get
;both these values off the stack now
;
;ContC	proc	far

;	assume	cs:CODERES,ds:NOTHING,es:NOTHING,ss:NOTHING

CONTC:

;	pop	ds			;ds = DATARES
;	assume	ds:DATARES
;;	pop	word [OldDS]		;OldDS = old ds

	test	byte [cs:INITFLAG],INITINIT ; 1	; in initialization?
	jz	short NOTATINIT			; no
	test	byte [cs:INITFLAG],INITSPECIAL ; 2 ; doing special stuff?
	jz	short CMDIRET			; no, ignore ^C
	;pop	ds			; restore before jumping; M021
	jmp	INIT_CONTC_SPECIALCASE	; Yes, go handle it
CMDIRET:
;SR;
; Restore ds to its previous value
;

;;	mov	ds,[OLdDS]		;
;	pop	ds
	iret				; yes, ignore the ^C

NOTATINIT:
	test	byte [cs:INITFLAG],INITCTRLC ; 4 ; are we already in a ^C?
	jz	short NOTINIT		; nope too.

;*	We are interrupting ourselves in this ^C handler. We need
;	to set carry and return to the user sans flags only if the
;	system call was a 1-12 one. Otherwise, we ignore the ^C.

	cmp	ah,1
	jb	short CMDIRET
	cmp	ah,12
	ja	short CMDIRET

;	pop	ds			;restore ds to old value
	add	sp,6			; remove int frame
	stc

;;	mov	ds,[OldDS]		;restore ds to its old value
	retf	2			; remove those flags...

NOTINIT:

;*	We have now received a ^C for some process (maybe ourselves
;	but not at INIT).
;	
;	Note that we are running on the user's stack!!! Bad news if
;	any of the system calls below go and issue another INT
;	24... Massive stack overflow! Another bad point is that
;	SavHand will save an already saved handle, thus losing a
;	possible redirection...
;	
;	All we need to do is set the flag to indicate nested ^C. 
;	The above code will correctly flag the ^C diring the
;	message output and prompting while ignoring the ^C the rest
;	of the time.
;	
;	Clean up: flush disk. If we are in the middle of a batch
;	file, we ask if he wants to terminate it. If he does, then
;	we turn off all internal flags and let the DOS abort.

	or	byte [cs:INITFLAG],INITCTRLC ; 4 ; nested ^c is on
	sti

	push	cs			; el yucko!  change the user's ds!!
	pop	ds
;	assume	ds:RESGROUP

	;pop	ax			;discard the old ds value

	mov	ax,[SINGLECOM]
	or	ax,ax
	jnz	short NORESET
	push	ax
	mov	ah,DISK_RESET ; 0Dh
	int	21h			; reset disks in case files were open
	pop	ax

NORESET:

;	In the generalized version of FOR, PIPE and BATCH, we would
;	walk the entire active list and free each segment. Here,
;	we just free the single batch segment.

	test	word [BATCH],-1
	jz	short CONTCTERM
	or	ax,ax
	jnz	short CONTCTERM
	call	SAVHAND
	call	ASKEND			; ask if user wants to end batch

;	If the carry flag is clear, we do NOT free up the batch file

	jnc	short CONTBATCH
	;mov	cl,[EchoFlag]		; get current echo flag
	push	bx

CLEARBATCH:
	mov	es,[BATCH]		; get batch segment
	;mov	di,[BatFile]		; get offset of batch file name
	; MSDOS 3.3 ([ES:4])
	mov	bx,[es:BATCHSEGMENT.BatForPtr] ; [es:4] ; get old FOR segment
	;
	cmp	bx,0			; is a FOR in progress
	je	short NOT_BAT_FOR	; no - don't deallocate
	push	es			;
	mov	es,bx			; yes - free it up...
	mov	ah,DEALLOC ; 49h	;
	int	21h			;
	pop	es			; restore to batch segment

NOT_BAT_FOR:
	mov	cl,[es:BATCHSEGMENT.BatEchoFlag] ; get old echo flag
	mov	bx,[es:BATCHSEGMENT.BatLast] ; get old batch segment
	mov	ah,DEALLOC ; 49h	; free it up...
	int	21h
	mov	[BATCH],bx		; get ready to deallocate next batch
	dec	WORD [NEST]		; is there another batch file?
	jnz	short CLEARBATCH	; keep going until no batch file

;	We are terminating a batch file; restore the echo status

;Shell_Bat_Cont: 			; continue batch for SHELL
	pop	bx
	mov	[ECHOFLAG],cl		; reset echo status
	; 29/05/2018
	mov	byte [PIPEFLAG],0	; turn off pipeflag

CONTBATCH:
	call	CRLF			; print out crlf before returning
	call	RESTHAND

;	Yes, we are terminating.  Turn off flags and allow the DOS to abort.

CONTCTERM:
	xor	ax,ax			; indicate no read
	mov	bp,ax

;	The following resetting of the state flags is good for the
;	generalized batch processing.

	mov	[IFFLAG],al		; turn off iffing
	mov	[FORFLAG],al		; turn off for processing
	call	RESPIPEOFF
	cmp	[SINGLECOM],ax		; see if we need to set singlecom
	jz	short NOSETSING
	mov	word [SINGLECOM],-1	; cause termination on 
					;  pipe, batch, for
NOSETSING:

;	If we are doing an internal command, go through the reload process.
;	If we are doing an external, let DOS abort the process.
;	In both cases, we are now done with the ^C processing.

	and	byte [INITFLAG],~INITCTRLC ; 0FBh
	cmp	[EXTCOM],al
	jnz	short DODAB		; internal ^c
	jmp	LODCOM1
DODAB:
	stc				; tell dos to abort

;SR;
;We dont need to restore ds here because we are forcing DOS to do an abort
;by setting carry and leaving flags on the stack
;
	retf				; Leave flags on stack

;ContC	endp

;SR;
; ds = DATARES on entry. This routine is called from DskErr and LodCom1 and
;both have ds = DATARES
;

RESPIPEOFF:
	push	ax
	xor	ax,ax
	xchg	al,[cs:PIPEFLAG]
	or	al,al
	jz	short NOPIPEPOP
	shr	byte [cs:ECHOFLAG],1
NOPIPEPOP:
	pop	ax
	retn

;CODERES ends

;=============================================================================
; COMMAND2.ASM, MSDOS 6.0, 1991
;=============================================================================
; 21/09/2018 - Retro DOS v3.0

;	title	COMMAND2 - resident code for COMMAND.COM part II
;	name	COMMAND2

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;	Revision History
;	================
;
; M038	SR  11/5/90	Changed stuff for Novell RPL. These guys cannot
;			reserve memory by changing int 12h and then give it
;			back to DOS by changing arenas in autoexec.bat.
;			This makes command.com reload transient and this
;			cannot be done at this stage.
;

;CODERES segment public byte

;*	If we cannot allocate enough memory for the transient or there
;	was some other allocation error, we display a message and
;	then die.

;SR;
; We will have to make sure that at this entry point and at FatalC, 
;ds = DATARES. All jumps to these points are made from only within this file
;and so we should be able to do this

	;assume	ds:DATARES
BADMEMERR:
	mov	dx,BMEMMES			; DX = ptr to msg
FATALC:
	push	cs
	pop	ds
;;	assume	ds:ResGroup
;	invoke	RPrint

	; MSDOS 3.3
	call	RDISPMSG

;	If this is NOT a permanent (top-level) COMMAND, then we exit;
;	we can't do anything else!

	cmp	byte [PERMCOM],0
	je	short FATALRET

;	We are a permanent command. If we are in the process of the
;	magic interrupt (Singlecom) then exit too.

	cmp	word [SINGLECOM],0		; if permcom and singlecom
	jne	short FATALRET			; must take int_2e exit

;	Permanent command. We can't do ANYthing except halt.

	mov	dx,HALTMES			; DX = ptr to msg
	;invoke	RPrint
	; MSDOS 3.3
	call	RDISPMSG
	sti
STALL:
	jmp	short STALL			; crash the system nicely

FATALRET:
	mov	dx,FRETMES			; DX = ptr to msg
	call	RDISPMSG
FATALRET2:
	cmp	byte [PERMCOM],0		; if we get here and permcom,
	jne	SHORT RET_2E			; must be int_2e

;	Bugbug:	this is where we'd want to unhook int 2F, *if* we
;	were a non-permanent COMMAND that had hooked it! (Just in 
;	case we decide to do that.)
	mov	ax,[PARENT]
	mov	[PDB.PARENT_PID],ax	; mov [cs:16h],ax
	mov	ax,[OLDTERM]
	mov	[PDB.EXIT],ax		; mov [cs:0Ah],ax
	mov	ax,[OLDTERM+2]
	mov	[PDB.EXIT+2],ax 	; mov [cs:0Ch],ax
	mov	ax,(EXIT<<8) ; 4C00h		; return to lower level
	int	21h
RET_2E:
;SR;
; We will ensure that ds = DATARES for all entries to this place
;

;;	push	cs
;;	pop	ds
;;	assume	ds:resgroup,es:nothing,ss:nothing
  	
;	assume	ds:DATARES

	;PUSH	CS
	;POP	DS

	mov	word [SINGLECOM],0	; turn off singlecom
	mov	es,[RES_TPA]
	mov	ah,DEALLOC
	int	21h			; free up space used by transient
	mov	bx,[SAVE_PDB]
	mov	ah,SET_CURRENT_PDB ; 50h
	int	21h			; current process is user
	mov	ax,[RETCODE]
	cmp	byte [EXTCOM],0
	jne	short GOTECODE
	xor	ax,ax			; internals always return 0
GOTECODE:
	mov	byte [EXTCOM],1		; force external

;SR; This is actually returning to the caller. However, the old code had
;ds = RESGROUP so I guess we can keep ds = DATARES for us.
;Yes, int 2eh can corrupt all registers so we are ok.
;
	;jmp	INT_2E_RET		;"iret"
	; 12/01/2023 (BugFix)
	jmp	far [INT_2E_RET]

;***	Int_2e, magic command executer

INT_2E:
	;assume	ds:NOTHING,es:NOTHING,ss:NOTHING
;SR;
; We are going to come here from the stub with the old ds and DATARES value
;pushed on the stack in that order. Pick up this stuff off the stack
;
	;pop	ds			;ds = DATARES
	;assume	ds:DATARES
	;pop	ax
;	;pop	ds:OldDS 		;Save old value of ds

	pop	word [cs:INT_2E_RET]
	pop	word [cs:INT_2E_RET+2]	; store return address
	pop	ax			; chuck flags
	;add	sp,2

	push	cs
	pop	es

	;push	ds
	;pop	es			;es = DATARES
;	;mov	ds,OldDS
	;mov	ds,ax
	;assume	ds:nothing		;ds = old value

	mov	di,80h
	mov	cx,64
;	Bugbug:	cld
	rep	movsw
	mov	ah,GET_CURRENT_PDB ; 51h
	int	21h			; get user's header
	;mov	[es:SAVE_PDB],bx
	MOV	[cs:SAVE_PDB],bx
	mov	ah,SET_CURRENT_PDB ; 50h

;;	mov	bx,cs
;SR;
; Set ds = DATARES because BadMemErr expects this
;
	;push	es
	;pop	ds
	;assume	ds:DATARES

	;mov	bx,ds			;es = our PSP now
	mov	bx,cs

	int	21h			; current process is me
	mov	word [cs:SINGLECOM],81h
	mov	byte [cs:EXTCOM],1	; make sure this case forced

;SR;
; We can enter LodCom directly after a command shell is terminated or we
;can fall thru from above. When we enter directly from the stub, the stack
;has the old ds value and the data seg value on the stack, so that ds can
;be properly set. To fake this, we push dummy values here.
;
	;push	ds			;old value of ds
	;push	ds			;data seg value, ds = DATARES

LODCOM: 				; termination handler
	;pop	ds			;ds = DATARES 
	;assume	ds:DATARES
	;add	sp,2
;	;pop	OldDS			;store old ds

	;cmp	EXTCOM,0
	cmp	byte [cs:EXTCOM],0
	;jne	@f	 		; internal cmd - memory allocated
	;jne	short LODCOM0 ; 24/09/2018
	;jmp	LODCOM1 		; jz LODCOM1
	je	short LODCOM1 ; 25/09/2018	

;@@:
LODCOM0: ; 24/09/2018
	mov	bx,0FFFFh
	mov	ah,ALLOC ; 48h	
	int	21h			; DOS - 2+ - ALLOCATE MEMORY
					; BX = number of 16-byte paragraphs desired
	call	SETSIZE
	add	ax,20h
	cmp	bx,ax
	jnb	short MEMOK		; > 512 byte buffer - good enough
BADMEMERRJ:
	jmp	BADMEMERR		; not enough memory

;***	SetSize - get transient size in paragraphs

SETSIZE:
	mov	ax,TRANSPACEEND+15	; mov AX,4D6Bh
	mov	cl,4
	shr	ax,cl
	retn

MEMOK:
	;assume	ds:DATARES		;we have set ds = DATARES 

	mov	ah,ALLOC  ; 48h
	int	21h
	jc	short BADMEMERRJ	; memory arenas probably trashed
	mov	byte [cs:EXTCOM],0	; flag not to alloc again
	mov	[cs:RES_TPA],ax		; save current tpa segment
	and	ax,0F000h
	add	ax,01000h		; round up to next 64k boundary
	jc	short BAD_TPA		; memory wrap if carry set

;	Make sure that new boundary is within allocated range

	mov	dx,[cs:RES_TPA]
	add	dx,bx			; compute maximum address
	cmp	dx,ax			; is 64k address out of range?
	jbe	short BAD_TPA

;	Must have 64K of usable space.

	sub	dx,ax			; compute the usable space
	cmp	dx,01000h		; is space >= 64k ?
	jae	short LTPASET
BAD_TPA:
	mov	ax,[cs:RES_TPA]
LTPASET:
	mov	[cs:LTPA],ax		; usable tpa is 64k buffer aligned
	mov	ax,[cs:RES_TPA]		; actual tpa is buffer allocated
	add	bx,ax
	mov	[cs:MEMSIZ],bx
	call	SETSIZE
	sub	bx,ax

	; MSDOS 6.0
;;
;;M038; Start of changes
;; Changes for Novell RPL. These guys reserve memory for themselves by
;;reducing int 12h size and add this memory to the system at autoexec time by
;;running a program that changes arenas. This changes the largest block that
;;command.com gets and so changes the transient segment. So, command.com does
;;a checksum at the wrong address and thinks that the transient is destroyed
;;and tries to reload it. At this point, no Comspec is defined and so the
;;reload fails, hanging the system. To get around this we just copy the
;;transient from the previous address to the new address(if changed) and
;;then let command.com do the checksum. So, if the transient area is not
;;corrupted, there will not be any reload. In Novell's case, the transient
;;is not really corrupted and so this should work.
;;
;	cmp	bx,[cs:TRNSEG]		;Segment still the same?
;	je	short LODCOM1		;yes, dont copy
;;
;;Check if the new segment is above or below the current move. If the new
;;segment is above (i.e new block is larger than previous block), then we
;;have to move in the reverse direction
;;
;	mov	cx,TRANSPACEEND		;cx = length to move
;	ja	short _MOV_DOWN		;new seg > old seg, reverse move
;	xor	si,si			;normal move
;	mov	di,si
;	cld
;	jmp	short COPY_TRANS
;_MOV_DOWN:
;	mov	si,cx			;reverse move, start from end
;	dec	si
;	mov	di,si
;	std
;COPY_TRANS:
;	push	ds
;	push	es
;	mov	es,bx			;dest segment
;	mov	ds,[cs:TRNSEG]		;source segment
;	;assume	ds:nothing
;
;	rep	movsb			;copy transient
;	cld
;	pop	es
;	pop	ds
;	;assume	ds:DATARES
;;
;;M038; End of changes
;;
	mov	[cs:TRNSEG],bx		;new location of transient

LODCOM1:
;;	mov	ax,cs
;;	mov	ss,ax
;SR; At this point ds = DATARES which is where the stack is located
;
	;mov	ax,ds
	;mov	ss,ax
	;assume	ss:DATARES
	;mov	sp,offset DATARES:RStack

;;	mov	ds,ax

	;assume	ds:DATARES
	
	; MSDOS 3.3
	mov	ax,cs
	mov	ss,ax
	mov	sp,RSTACK
	mov	ds,ax

	call	HEADFIX			; close files, restore stdin, stdout
	xor	bp,bp			; flag command ok
	mov	ax,-1
	xchg	ax,[VERVAL]
	cmp	ax,-1
	je	short NOSETVER
	mov	ah,SET_VERIFY_ON_WRITE ; 2Eh ; AL has correct value
	int	21h 			; DOS - SET VERIFY FLAG
					; DL = 00h, AL = 01h VERIFY on / 00h VERIFY off
NOSETVER:
	cmp	word [SINGLECOM],-1
	jne	short NOSNG
	jmp	FATALRET2		; we have finished the single command
NOSNG:
	call	CHKSUM			; check the transient
	cmp	dx,[SUM]
	je	short HAVCOM		; transient ok
BOGUS_COM:
	mov	byte [LOADING],1	; flag DskErr routine
	call	LOADCOM
CHKSAME:
	call	CHKSUM
	cmp	dx,[SUM]
	jz	short HAVCOM		; same command
ALSO_BOGUS:
	call	WRONGCOM
	jmp	short CHKSAME

HAVCOM:
	; 25/09/2018
	mov     ax,(CHAR_OPER*256) ; 3700h
	int     21h	; DOS - 2+ internal - GET SWITCHAR/AVAILDEV
			; Return: AL = FFh unsupported subfunction
			; DL = current switch character
	mov     [RSWITCHAR],dl
	cmp     dl,'/'
	jnz     short USESLASH
	;mov	cl,'\'
	;mov	[RDIRCHAR],cl
	mov	byte [RDIRCHAR],'\'
USESLASH:
	mov	byte [LOADING],0		; flag to DskErr
	mov	si,TRANVARS
	mov	di,HEADCALL
	mov	es,[TRNSEG]
	cld
	mov	cx,TRANVAREND
	sub	cx,si
	rep	movsb			; transfer info to transient
	mov	ax,[MEMSIZ]
	mov	[PDB.BLOCK_LEN],ax ; mov [ds:2],ax ; adjust my own header

;***	TJmp - jump-off to transient
;
;	Public label so debugger can find this spot.

TJMP:
	jmp	far [TRANS]

;***	TRemCheck - far version of RemCheck for transient

TREMCHECK:
	;pop	ds			;ds = DATARES
	;add	sp,2			;discard old value of ds

	call	REMCHECK
	retf

;***	RemCheck
;
;	ENTRY	AL = drive (0=default, 1=A, ...)
;
;	EXIT	ZR set if removeable media
;		ZR clear if fixed media
;
;	USED	none

REMCHECK:
	push	ax
	push	bx
	mov	bx,ax
	mov	ax,(IOCTL<<8)+8 ; 4408h
	int	21h			; DOS - 2+ - IOCTL -
	jnc	short RCCONT			

;	If an error occurred, assume the media is non-removable.
;	AX contains the non-zero error code from the int 21, so
;	'or ax,ax; sets non-zero. This behavior makes network drives
;	appear to be non-removable.				
					
	or	ax,ax			
	jmp	short RESREGS
RCCONT:
	and	ax,1
	not	ax
RESREGS:
	pop	bx
	pop	ax
	retn

;***	THeadFix
;
;	Far version of HeadFix, called from transient.

THEADFIX:
	;pop	ds			;ds = DATARES
	;add	sp,2			;discard old ds value on stack

	call	HEADFIX
	retf

;***	HeadFix

HEADFIX:
	call	SETVECT			; set vectors to our values

;	Clean up header

;	Bugbug:	optimize:
;	mov	word ptr ds:Pdb_Jfn_Table,cx  instead of separate bytes

	xor	bx,bx				; BX = handle = 0
	mov	cx,[IO_SAVE]			; CX = original stdin, stdout
	mov	dx,[PDB.JFN_TABLE] ; mov dx,[ds:18h] ; DX = current stdin, stdout
	cmp	cl,dl
	je	short CHK1		; stdin matches
	mov	ah,CLOSE  ; 3Eh
	int	21h			; close stdin
	mov	[PDB.JFN_TABLE],cl ; mov [ds:18h],cl ; restore stdin
CHK1:
	inc	bx			; BX = handle = 1
	cmp	ch,dh			
	je	short CHKOTHERHAND	; stdout matches
	mov	ah,CLOSE
	int	21h			; close stdout
	mov	[PDB.JFN_TABLE+1],ch ; mov [ds:19h],ch	; restore stdout

CHKOTHERHAND:
	add	bx,4			; skip handles 2,3,4
	mov	cx,FILPERPROC-5	; 15	; CX = # handles to close
					;   (handles 0-4 already done)
CLOSELOOP:
	mov	ah,CLOSE ; 3Eh
	int	21h			; close each handle
	inc	bx			; BX = next handle
	loop	CLOSELOOP

	; MSDOS 6.0
;;	Bugbug:	since this is for transient code, move it there
;
;;	M012: remove this CS -> DS.  Must've been missed during
;;	purification.
;;;	push	ds			; save data segment
;;;	push	cs			; get local segment into DS
;;;	pop	ds			;
;	cmp	Append_Flag,-1		; do we need to reset APPEND?
;	jne	Append_Fix_End		; no - just exit
;	mov	ax,AppendSetState	; set the state of Append
;	mov	bx,Append_State 	;     back to the original state
;	int	2Fh			;
;	mov	Append_Flag,0		; set append flag to invalid
;Append_Fix_End: 			;
;;;	pop	ds			; get data segment back
;	retn

	; MSDOS 3.3
	retn

;***	SavHand - save current program's stdin/out & set to our stderr
;
;	ENTRY	nothing
;
;	EXIT	nothing
;
;	USED	flags
;
;	EFFECTS
;	  Handle01 = current program's stdin,stdout JFN entries
;	  current program's stdin,stdout set to our stderr
;

;SR;
; Changed ds = DATARES. We need it to access our JFN_Table
; Called from ContC ( ds = DATARES ) and DskErr ( ds = DATARES ).
;
SAVHAND:
	;assume	ds:DATARES,es:NOTHING,ss:NOTHING

	push	ds ; MSDOS 3.3
	push	bx			;preserve registers
	push	ax
	;push	es
	;push	ds			; save DATARES value

	mov	ah,GET_CURRENT_PDB ; 51h
	int	21h			; BX = user's header seg addr
	mov	ds,bx			; DS = user's header seg addr
	lds	bx,[PDB.JFN_Pointer] ; lds bx,[ds:34h]	; DS:BX = ptr to JFN table
	mov	ax,[bx]			; AX = stdin,stdout JFN's

	;pop	es			;es = DATARES
	;push	es			;save it back on stack
	;mov	es:Handle01,ax		; save user's stdin, stdout
	mov	[cs:HANDLE01],ax

;SR;
; Use es to address Handle01 & our JFN_Table
;

	;mov	al,es:[PDB_JFN_TABLE+2] ; AL = COMMAND stderr
	mov	al,[cs:PDB.JFN_TABLE+2] ; mov al,[cs:1Ah]
	mov	ah,al			; AH = COMMAND stderr
	mov	[bx],ax			; set user's stdin/out to our stderr

	;pop	ds			; restore registers
	;pop	es
	pop	ax
	pop	bx
	pop	ds ; MSDOS 3.3
	retn

	;assume	ds:DATARES
GETCOMDSK2:
	call	GETCOMDSK
	jmp	LODCOM1			; memory already allocated

RESTHAND:
	push	ds
	push	bx			; restore stdin, stdout to user
	push	ax
	mov	ah,GET_CURRENT_PDB ; 51h
	int	21h			; point to user's header
	mov	ax,[HANDLE01]
	mov	ds,bx
	;assume ds:NOTHING
	lds	bx,[PDB.JFN_Pointer] ; lds bx,[ds:34h] ; DS:BX = ptr to jfn table
	mov	[bx],ax			; stuff his old 0 and 1
	pop	ax
	pop	bx
	pop	ds
	retn

	;assume ds:DATARES,ss:DATARES
HOPELESS:
	mov	dx,COMBAD
	jmp	FATALC

GETCOMDSK:
	mov	al,[COMDRV]
	call	REMCHECK
	jnz	short HOPELESS		; non-removable media
GETCOMDSK3:
	cmp	dx,COMBAD
	jne	short GETCOMDSK4
	;;mov	dx,offset DATARES:ComBad	; DX = ptr to msg
	;invoke	RPrint				; say COMMAND is invalid
	call	RDISPMSG
GETCOMDSK4:
;	Bugbug:	there's always a drive here?  No need to check?
	cmp	byte [PUTBACKDRV],0	; is there a drive in the comspec?
	jne	short USERS_DRIVE	; yes - use it
	mov	ah,GET_DEFAULT_DRIVE ; 19h ; use default drive
	int	21h
	add	al,"A"                  ; convert to ascii
	mov	[PUTBACKDRV],al		; put in message to print out

USERS_DRIVE:
	; MSDOS 6.0
	;mov	dx,PUTBACKMSG		; prompt for diskette
	;mov	si,offset DATARES:PutBackSubst	;  containing COMMAND
	;invoke	RPrint
	;mov	dx,offset DATARES:Prompt	; "Press any key"
	;invoke	RPrint

	; MSDOS 3.3
	mov	dx,PUTBACKMSG		; prompt for diskette
	call	RDISPMSG
	mov	dx,[PUTBACKSUBSTPTR]
	mov	si,[COMSPEC_END]
	mov	byte [si+1],'$'
	call	RDISPMSG
	mov	byte [si+1],0
	mov	dx,PROMPT
	call    RDISPMSG

	;call	GETRAWFLUSHEDBYTE
	;retn
	; 02/03/2023
	;jmp	short GETRAWFLUSHEDBYTE

;***	GetRawFlushedByte - flush world and get raw input

GETRAWFLUSHEDBYTE:
	mov	ax,(STD_CON_INPUT_FLUSH<<8) | RAW_CON_INPUT ; 0C07h
	int	21h			; get char without testing or echo
	mov	ax,(STD_CON_INPUT_FLUSH<<8) + 0 ; 0C00h
	int	21h
;	Bugbug:	get rid of this return and the following retz.
LOADCOM_RETN:
	retn

;***	LoadCom - load in transient

LOADCOM:
	;assume	ds:DATARES
	
	inc	bp				; flag command read

	mov	dx,COMSPEC
	mov	ax,OPEN<<8	; 3D00h
	int	21h				; open command.com
	jnc	short READCOM
	cmp	ax,ERROR_TOO_MANY_OPEN_FILES
	jnz	short TRYDOOPEN
	mov	dx,NOHANDMES
	jmp	FATALC				; will never find a handle

TRYDOOPEN:
	call	GETCOMDSK
	jmp	short LOADCOM

READCOM:
	mov	bx,ax				; BX = handle
	mov	dx,TRANSTART
	xor	cx,cx				; CX:DX = seek loc
	mov	ax,LSEEK<<8	; 4200h
	int	21h
	jc	short WRONGCOM1
	mov	cx,TRANSPACEEND-100h ; 4C5Ch (for original MSDOS 3.3!)
	push	ds
	mov	ds,[TRNSEG]
	;assume	ds:NOTHING
	mov	dx,100h
	mov	ah,READ	; 3Fh	
	int     21h	; DOS - 2+ - READ FROM FILE WITH HANDLE
			; BX = file handle, CX = number of bytes to read
			; DS:DX -> buffer
	pop	ds
	;assume	ds:DATARES
WRONGCOM1:
	pushf
	push	ax
	mov	ah,CLOSE ; 3Eh
	int	21h			; close command.com
	pop	ax
	popf
	jc	short WRONGCOM		; error on read
	cmp	ax,cx
	;retz				; size matched
	jz	short LOADCOM_RETN
WRONGCOM:
	mov	dx,COMBAD
	call	GETCOMDSK
	jmp	short LOADCOM		; try again

;***	ChkSum - compute transient checksum

CHKSUM:
	push	ds
	mov	ds,[TRNSEG]
	mov	si,100h
	mov	cx,TRANDATAEND-100H	; 3E44h (for original MSDOS 3.3!)
CHECK_SUM:
	cld
	shr	cx,1
	xor	dx,dx
CHK:
	lodsw
	add	dx,ax
	adc	dx,0
	loop	CHK
	pop	ds
	retn

;***	SetVect - set interrupt vectors

SETVECT:
	;mov	dx,offset DATARES:LodCom_Trap 
	mov	dx,LODCOM ; MSDOS 3.3
	mov	ax,(SET_INTERRUPT_VECTOR<<8) | 22h  ; 2522h
	mov	[PDB.EXIT],dx	; mov ds:0Ah,dx
	mov	[PDB.EXIT+2],ds ; mov ds:0Ch,ds
	int	21h
	;mov	dx,offset DATARES:Ctrlc_Trap
	mov	dx,CONTC ; MSDOS 3.3
	inc	al	; 23h
	int	21h
	;mov	dx,offset DATARES:CritErr_Trap
	mov	dx,CRITERR ; MSDOS 3.3
	inc	al	; 24h
	int	21h
	retn

	; MSDOS 6.0
;;SR;
;; We have this to take care of the extra values pushed on the stack by 
;;the stub before jumping to LodCom1. We set up ds here and then jump to
;;Lodcom1
;;
;public	TrnLodCom1
;TrnLodCom1:
;	pop	ds			;ds = DATARES
;	add	sp,2
;;	pop	ds:OldDS
;	jmp	LodCom1

;***	EndInit - end up initialization sequence
;
;	Move the environment to a newly allocated segment.

	; MSDOS 3.3
ENDINIT:
	push	ds			; save segments
	push	es			;
	push	cs			; get resident segment to DS
	pop	ds			;
	;assume	ds:RESGROUP
	mov	cx,[USEDENV]		; get number of bytes to move
	mov	es,[ENVIRSEG]		; get target environment segment
	;assume	es:NOTHING

	cmp	byte [RESETENV],1	; do we need to setblock to env end?
	jne	short NO_RESET 		; no - we already did it
	mov	bx,[ENVSIZ]		; BX = size of environ in paragraphs
	push	es			; save environment - just to be sure
	mov	ah,SETBLOCK  ; 4Ah	;
	int	21h             ; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
				; ES = segment address of block to change
				; BX = new size in paragraphs
	pop	es

NO_RESET:
	mov	[PDB.ENVIRON],es ; mov [ds:2Ch],es ; put new environment in my header
	mov	ds,[OLDENV]		; source environment segment
	;assume	ds:NOTHING
	xor	si,si			; set up offsets to start of segments
	xor	di,di
	cld
	rep	movsb			; move it
	xor	ax,ax
	stosb				; make sure it ends with double-null

	;mov	[cs:InitFlag],FALSE	; turn off init flag
	pop	es
	pop	ds
	jmp	LODCOM			; allocate transient


	; MSDOS 6.0
;
;The init code has been changed to take care of the new way in which the
;environment segment is allocated.
;NB: We can use all the init variables at this point because they are all in
;RESGROUP
;Bugbug: The above approach will not work for ROMDOS
;

;IF 0
;
;EndInit:
;	push	ds
;	push	es			;save segments
;	push	cs
;	pop	ds		
;	assume	ds:RESGROUP
;;
;; AllocedEnv flag signals whether it is a passed environment or not
;;
;	mov	bx,ds
;	mov	es,bx			;es = RESGROUP
;;
;;ResSize is the actual size to be retained -- only data for HIMEM COMMAND, 
;; code + data for low COMMAND
;;
;	mov	bx,ResSize		;Total size of resident
;	mov	ah,SETBLOCK
;	int	21h			;Set block to resident size
;;
;;Allocate the correct size for the environment
;;
;	mov	bx,EnvSiz		;bx = env size in paras
;	mov	ah,ALLOC
;	int	21h			;get memory
;	jc	nomem_err		;out of memory,signal error
;
;	mov	EnvirSeg,ax		;Store new environment segment
;	mov	ds:PDB_Environ,ax		;Put new env seg in PSP
;	mov	es,ax			;es = address of allocated memory
;	assume	es:nothing
;
;;
;;Copy the environment to the newly allocated segment
;;
;	mov	cx,UsedEnv		;number of bytes to move
;
;	push	ds
;	mov	ds,OldEnv		;ds = Old environment segment
;	assume	ds:nothing
;
;	xor	si,si
;	mov	di,si			;Start transfer from 0
;
;	cld
;	rep	movsb			;Do the copy
;
;	xor	ax,ax			
;	stosb				;Make it end with double-null
;
;	pop	ds			;ds = RESGROUP
;	assume	ds:RESGROUP
;;
;;We have to free the old environment block if it was allocated by INIT
;;
;	cmp     AllocedEnv,0            ;has env been allocated by INIT?
;	je      no_free                 ;no, do not free it
;
;	mov	ax,OldEnv		;Get old environment
;	mov	es,ax
;	mov	ah,DEALLOC	
;	int	21h			;Free it
;no_free:
;	mov	InitFlag,FALSE		;indicate INIT is done
;	
;	pop	es
;	pop	ds
;	assume	ds:nothing
;	
;	jmp	LodCom			;allocate transient
;
;nomem_err:
;;
;;We call the error routine which will never return. It will either exit
;;with an error ( if not the first COMMAND ) or just hang after an error 
;;message ( if first COMMAND )
;;
;
;	call	Alloc_error
;ENDIF
;
;CODERES ends

;	This TAIL segment is used to produce a PARA aligned label in
;	the resident group which is the location where the transient
;	segments will be loaded initial.

;TAIL		segment public para
;
;		org	0
;TranStart	label	word
;		public	TranStart
;
;TAIL		ends
;

;	This TAIL segment is used to produce a PARA aligned label in
;	the transient group which is the location where the exec
;	segments will be loaded initial.
;
;	Bugbug:	Is TRANTAIL used anymore?

;TRANTAIL	segment public para
;
;		org	0
;ExecStart   	label   word
;
;TRANTAIL    	ends

;=============================================================================
; RUCODE.ASM, MSDOS 6.0, 1991
;=============================================================================
; 22/09/2018 - Retro DOS v3.0

;	title	Localizable code for resident COMMAND

	;assume	cs:CODERES,ds:NOTHING,es:NOTHING,ss:NOTHING

;***	AskEnd - ask user to confirm batch file termination
;
;	Confirm with user before freeing batch ...
;
;	ENTRY	nothing
;
;	EXIT	CY = set if batch termination is confirmed
;
;		CY = clear if batch should continue
;
;	USED	AX,DX,...

;	Bugbug:	move this to transient, copy to batch segment.
;	Bugbug:	or move it to command1 1st.

;	Bugbug: No_Char and Yes_Char should be constants.

ASKEND:
	;assume	ds:DATARES

	mov	dx,ENDBATMES			; DX = message #
	;call	RPrint
	call	RDISPMSG  ; MSDOS 3.3
	mov	ax,(STD_CON_INPUT_FLUSH<<8) + STD_CON_INPUT  ;0C01h
	int     21h             ; DOS - CLEAR KEYBOARD BUFFER
				; AL must be 01h, 06h, 07h, 08h, or 0Ah.
	call	CHARTOUPPER			; change to upper case
	cmp	al,[NO_CHAR]
	je	short AERET			; answer is no (CY is clear)
	cmp	al,[YES_CHAR]
	jne	short ASKEND			; invalid response, try again
	stc					; answer is yes
AERET:	
	retn


;***	DskErr - critical error handler
;
;	Default critical error handler unless user intercepts int 24h.
;
;	ENTRY	int 24h
;
;	EXIT
;
;	USED
;
;	EFFECTS

;
;SR; 
; The stub is going to push the old ds value and the resident data segment
;onto the stack in that order. Get it off the stack
;

;DskErr	proc	far
DSKERRR:
	;assume	ds:NOTHING,es:NOTHING,ss:NOTHING
	;
	;pop	ds			;ds = DATARES
	;assume ds:DATARES
	;pop	OldDS			;save old ds value

CRITERR: ; MSDOS 3.3
	sti
	push	ds ; 25/09/2018
	push	es
	push	si
	push	cx
	push	di
	push	cx
	push	ax

	;push	ds			;save our data segment
	push	cs ; 25/09/2018
	pop	es			;es = DATARES

	mov	ds,bp
	;assume	ds:nothing

	;mov	ax,[si].SDEVATT
	MOV	ax,[SI+SYSDEV.ATT] ; mov ax,[si+4]	
	mov	[es:CDEVAT],ah

	;push	cs
	;pop	es

	mov	di,DEVNAME
	mov	cx,8
	;add	si,SDEVNAME  ; add si,10
	add	si,SYSDEV.NAME	; save device name (even for block device)
				
	cld
	rep	movsb
	pop	ax
	pop	cx
	pop	di

;	Stack still contains DS and ES.

;SR;
; We need ds = DATARES for SavHand
;
	;push	es
	;pop	ds
	;assume	ds:DATARES

	;invoke	SavHand		; save user's stdin/out, set to our stderr
	call	SAVHAND

	; 25/09/2018
	;push	cs
	push	es
	pop	ds		; set up local data segment
	;assume	ds:resgroup

	push	dx
	call	CRLF
	pop	dx

;	Bugbug:	rename Crit_Err_Info to CritErrAH?

	mov	[CRIT_ERR_INFO],ah	; save critical error flags

;	Compute and save ASCII drive letter (nonsense for char devices)

	add	al,'A'
	mov	[DRVLET],al

;	Bugbug:	These labels are awful. Change, especially 'NoHardE'.

	test	ah,80h
	jz	short NOHARDE		; it's a disk-device error
	test	byte [CDEVAT],DEVTYP>>8 ; 80h
	jnz	short NOHARDE		; it's a character device
	jmp	FATERR			; it's a FAT error

NOHARDE:
	mov	si,MREAD		; SI = "read" msg #
	test	ah,1
	jz	short SAVMES		; it's a read error
	mov	si,MWRITE		; SI = "write" msg #

SAVMES:
	mov	[OLDERRNO],di		; save critical error code

;	Bugbug:	don't need to save/restore all here?
	push	es
	push	ds			; GetExtendedError likes to STOMP
	push	bp
	push	si
	push	dx
	push	cx
	push	bx
	mov	ah,GetExtendedError ; 59h ; get extended error info
	int	21h
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	bp
	pop	ds
	mov	[NEEDVOL],di 	; save possible ptr to volume label
	mov	[NEEDVOL+2],es
	pop	es

;	Bugbug:	AX has extended error code, so no need to zero AH?

	xor	ah,ah
	mov	di,ax			; DI = error code

; Bugbug:  somewhat obsolete documentation?
;
; DI is now the correct error code. Classify things to see what we are
; allowed to report. We convert DI into a 0-based index into a message table.
; This presumes that the int 24 errors (oldstyle) and new errors (sharing and
; the like) are contiguous.
;

;	Bugbug:	simplify following code by cmp'ing instead of sub'ing.
;	Check use of ErrCd_24, though.

	sub	di,ERROR_WRITE_PROTECT ; 13h
	jae	short HAVCOD

;	Bugbug:	wouldn't it be better to display the original error msg,
;	even though it's not a critical error?

	mov	di,ERROR_GEN_FAILURE - ERROR_WRITE_PROTECT ; mov di,0Ch
;
; DI now has the mapped error code. Old style errors are:
;   FOOBAR <read|writ>ing drive ZZ.
; New style errors are:
;   FOOBAR
; We need to figure out which the particular error belongs to.
;

HAVCOD:
	mov	byte [ERRTYPE],0	; assume old style
	cmp	di,ERROR_FCB_UNAVAILABLE - ERROR_WRITE_PROTECT  ; cmp di,10h
	je	short SETSTYLE
	cmp	di,ERROR_SHARING_BUFFER_EXCEEDED - ERROR_WRITE_PROTECT ; cmp di,11h
	jne	short GOTSTYLE

SETSTYLE:
;	Bugbug:	use INC
	;mov	byte [ERRTYPE],1		; must be new type
	inc	byte [ERRTYPE] ; Retro DOS v3.0 COMMAND.COM - 22/09/2018

GOTSTYLE:
	mov	[ERRCD_24],di
	; 25/09/2018
	; MSDOS 6.0
	;cmp	di,ERROR_HANDLE_DISK_FULL - ERROR_WRITE_PROTECT ; cmp di,14h
	; MSDOS 3.3
	cmp	di,ERROR_SHARING_BUFFER_EXCEEDED - ERROR_WRITE_PROTECT ; cmp di,11h

						; If the error message is unknown
	jbe	short NORMALERROR		;  redirector, continue. Otherwise,
;
; We do not know how to handle this error. Ask IFSFUNC if she knows
; how to handle things
;

;input to IFSFUNC:    AL=1
;		      BX=extended error number
;
;output from IFSFUNC: AL=error type (0 or 1)
;			 0=<message> error (read/writ)ing (drive/device) xxx
;			   Abort, Retry, Ignore
;			 1=<message>
;			   Abort, Retry, Ignore
;		      ES:DI=pointer to message text
;		      carry set=>no message

	mov	di,ax			; retrieve correct extended error...
	mov	ax,0500h		; is the redir there?
	int	2Fh	; Multiplex - DOS 3+ CRITICAL ERROR HANDLER - INSTALLATION CHECK
			; Return: AL = 00h not installed, OK to install
			; 01h not installed, can't install
			; FFh installed
	cmp	al,0FFh
	jne	short NOHANDLER		; no, go to NoHandler

	; MSDOS 6.0
	;push	bx
	;mov	bx,di			; get ErrType and ptr to error msg
	;mov	ax,0501h
	int	2Fh	; Multiplex - DOS 3+ CRITICAL ERROR HANDLER -
	;pop	bx 
	;jc	short NOHANDLER
	; MSDOS 3.3
	mov     ax,di
	mov     ah,5
	int     2Fh	; Multiplex - DOS 3+ CRITICAL ERROR HANDLER -
	jc      short NOHANDLER

;	Bugbug:	need to record error type?
	mov	[ERRTYPE],al
	push	ds
	push	es
	pop	ds
	mov	dx,di
	mov	cx,-1			; find end of msg
	xor	al,al

	cld
	repnz	scasb

;	Bugbug:	we can do better than this.

	mov	byte [di-1],'$'
	
	;CALL	RDISPMSG ; MSDOS 3.3
	
	mov	ah,STD_CON_STRING_OUTPUT ; 9	; print the message
	int	21h

	mov	byte [di-1],0			; restore terminal byte

	pop	ds				; clean up and continue
	jmp	short CHECKERRTYPE

;*	Redir isn't available or doesn't recognize the error.
;	Restore regs to unextended error.

NOHANDLER:
	mov	byte [ERRTYPE],0
;	Bugbug:	won't this break, since we add error_write_protect back in?
	mov	di,[OLDERRNO]
	mov	[ERRCD_24],di

NORMALERROR:
	; MSDOS 6.0
	;add	di,ERROR_WRITE_PROTECT
	;xchg	di,dx			; may need dx later
	;call	RPrintCrit		; print error type

	; MSDOS 3.3
	shl     di,1
	mov     di,[CRMSGTBL+di]
	xchg    di,dx
	call    RDISPMSG

CHECKERRTYPE:
	cmp	byte [ERRTYPE],0	; Check error style...
	je	short CONTOLD
	call	CRLF			; if new style then done printing
	jmp	short ASK

	; 02/03/2023
BLKERR:
	; MSDOS 6.0
	;mov	dx,offset DATARES:BlkDevErr	  ; DX = error msg #
	;mov	BlkDevErrRw.SubstPtr,si		  ; "reading","writing" ptr
	;mov	si,offset DATARES:BlkDevErrSubst ; SI = ptr to subst block
	;call	RPrint

	; MSDOS 3.3
	mov     dx,BLKDEVERR
	call    RDISPMSG

	cmp	byte [LOADING],0
	jz	short ASK
	call	RESTHAND
	jmp	GETCOMDSK2		; if error loading COMMAND, re-prompt

CONTOLD:
	; MSDOS 6.0
;	inc	si			; DS:SI = ptr to asciiz string
;
;;	Bugbug:	combine some of the following two sections?
;
;	test	[CDevAt],DEVTYP shr 8
;	jz	BlkErr
;	mov	dx,offset DATARES:CharDevErr	  ; DX = ptr to device message
;	mov	CharDevErrRw.SubstPtr,si	  ; point to read/write string
;	mov	si,offset DATARES:CharDevErrSubst; SI = ptr to subst block
;
;	call	RPrint				; print the message
;	jmp	short Ask			; don't ralph on command

	; MSDOS 3.3
	mov	dx,ERRMES
	call	RDISPMSG
	mov	dx,si
	call	RDISPMSG
	
	test	byte [CDEVAT], 80h
	jz	short BLKERR
	mov	dx,CHARDEVERR	; " device "
	mov	ah,STD_CON_STRING_OUTPUT ; 9
	int	21h		; DOS - PRINT STRING
				; DS:DX -> string terminated by "$"
	; 02/03/2023
	;jmp	short ASK

; 02/03/2023
;BLKERR:
;	; MSDOS 6.0
;	;mov	dx,offset DATARES:BlkDevErr	  ; DX = error msg #
;	;mov	BlkDevErrRw.SubstPtr,si		  ; "reading","writing" ptr
;	;mov	si,offset DATARES:BlkDevErrSubst ; SI = ptr to subst block
;	;call	RPrint
;
;	; MSDOS 3.3
;	mov     dx,BLKDEVERR
;	call    RDISPMSG
;
;	cmp	byte [LOADING],0
;	jz	short ASK
;	call	RESTHAND
;	jmp	GETCOMDSK2		; if error loading COMMAND, re-prompt

ASK:
	cmp	word [ERRCD_24],15	; error 15 has an extra message
	jne	short NOT15		; not error 15

;*	For error 15, tell the user which volume/serial # are needed.

	push	cx

;	Bugbug:	does this push/pop need to be done?
	push	ds
	pop	es
	lds	si,[NEEDVOL]
	;assume	ds:NOTHING
	push	di
	mov	di,VOLNAME
	; MSDOS 6.0
	;mov	cx,16			; copy volume name & serial #
	; MSDOS 3.3
	mov	cx,11			; copy volume name
	cld
	rep	movsb
	pop	di
	push	es
	pop	ds
	pop	cx
	;assume	ds:DATARES
	; MSDOS 6.0
	;mov	dx,offset DATARES:NeedVolMsg	; DX = ptr to msg
	;mov	si,offset DATARES:NeedVolSubst	; DS:SI = ptr to subst block
	;call	RPrint

	; MSDOS 3.3
	mov	dx,NEEDVOLMSG
	mov     ah,STD_CON_STRING_OUTPUT ; 9
	int     21h             ; DOS - PRINT STRING
				; DS:DX -> string terminated by "$"
NOT15:
;*	Print abort, retry, ignore, fail message.
;	Print only options that are valid.

;	Bugbug:	sizzle this.

	mov	dx,REQ_ABORT
	;call	RPrint
	call	RDISPMSG
	test	byte [CRIT_ERR_INFO],RETRY_ALLOWED  ; 10h
	jz	short TRY_IGNORE
	mov	dx,REQ_RETRY
	;call	RPrint
	call	RDISPMSG
TRY_IGNORE:
	test	byte [CRIT_ERR_INFO],IGNORE_ALLOWED ; 20h
	jz	short TRY_FAIL
	mov	dx,REQ_IGNORE
	;call	RPrint
	call	RDISPMSG
TRY_FAIL:
	test	byte [CRIT_ERR_INFO],FAIL_ALLOWED   ; 08h
	jz	short TERM_QUESTION
	mov	dx,REQ_FAIL
	;call	RPrint
	call	RDISPMSG
TERM_QUESTION:
	mov	dx,REQ_END
	;call	RPrint
	call	RDISPMSG

;	If the /f switch was given, we fail all requests.

	test	byte [FFAIL],-1
	jz	short DOPROMPT
	mov	ah,3				; signal fail
	jmp	EEXIT

DOPROMPT:
	mov	ax,(STD_CON_INPUT_FLUSH<<8) + STD_CON_INPUT ; 0C01h
	int	21h				; get response

	call	CRLF
	call	CHARTOUPPER			; convert to upper case
	mov	ah,0				; return code for ignore
	test	byte [CRIT_ERR_INFO],IGNORE_ALLOWED ; 20h ; is ignore allowed?
	jz	short USER_RETRY
	cmp	al,[IGNORE_CHAR]		; ignore?
	jz	short EEXITJ

;	Bugbug:	optimize following code.

USER_RETRY:
	inc	ah				; return code for retry
	test	byte [CRIT_ERR_INFO],RETRY_ALLOWED ; 10h ; is retry allowed?
	jz	short USER_ABORT
	cmp	al,[RETRY_CHAR]			; retry?
	jz	short EEXITJ

USER_ABORT:
	inc	ah				; return code for abort
						;  (abort always allowed)
	cmp	al,[ABORT_CHAR]			; abort?
	jz	short ABORT_PROCESS			; exit user program
	inc	ah				; return code for fail
	test	byte [CRIT_ERR_INFO],FAIL_ALLOWED ; 08h ; is fail allowed?
	jz	short ASKJ
	cmp	al,[FAIL_CHAR]			; fail?
	jz	short EEXITJ
ASKJ:
	jmp	ASK

EEXITJ:
	jmp	short EEXIT

ABORT_PROCESS:
	test	byte [INITFLAG],INITINIT ; 1	; COMMAND init interrupted?
	jz	short ABORTCONT			; no, handle it normally
	cmp	byte [PERMCOM],0		; are we top level process?
	jz	short JUSTEXIT			; yes, just exit

	mov	dx,PATRICIDE			; no, load ptr to error msg
	;call	RPrint				; print it
	call	RDISPMSG

DEADINTHEWATER:
	jmp	short DEADINTHEWATER		; loop until the user reboots

JUSTEXIT:
	;assume	ds:DATARES
	mov	ax,[PARENT]			; load real parent pid
	mov	[PDB.PARENT_PID],ax ; mov ds:16h,ax ; put it back where it belongs
	mov	ax,(EXIT<<8) | 255 ; 4CFFh
	int     21h             ; DOS - 2+ - QUIT WITH EXIT CODE (EXIT)
				; AL = exit code
ABORTCONT:
	test	byte [IN_BATCH],-1		; Are we accessing a batch file?
	jz	short NOT_BATCH_ABORT
	mov	byte [BATCH_ABORT],1		; set flag for abort
NOT_BATCH_ABORT:
	mov	dl,[PIPEFLAG]
	call	RESPIPEOFF
	or	dl,dl
	je	short CHECKFORA
	cmp	word [SINGLECOM],0
	je	short CHECKFORA
	mov	word [SINGLECOM],-1		; make sure singlecom exits
CHECKFORA:
	cmp	word [ERRCD_24],0		; write protect?
	je	short ABORTFOR
	cmp	word [ERRCD_24],2		; drive not ready?
	jne	short EEXIT			; don't abort the FOR
ABORTFOR:
	mov	byte [FORFLAG],0		; abort a FOR in progress
	cmp	word [SINGLECOM],0
	je	short EEXIT
	mov	word [SINGLECOM],-1		; make sure singlecom exits
EEXIT:
	mov	al,ah
	mov	dx,di
RESTHD:
	call    RESTHAND
	pop	cx
	pop	si				; restore registers
	pop	es
	
;	; MSDOS 6.0
;;;	pop	ds
;;SR;
;; ds has to be got from the variable we saved it in
;;
;
; 	mov	ds,OldDS			;restore old value of ds
;;	pop	ds
;	assume	ds:nothing

	; MSDOS 3.3
	pop	ds

	iret

FATERR:
	; MSDOS 6.0
	;mov	dx,offset DATARES:BadFatMsg
	;mov	si,offset DATARES:BadFatSubst
	;call	RPrint

	; MSDOS 3.3
	mov     dx,BADFATMSG
	call    RDISPMSG
	mov     dx,BLKDEVERR
	call    RDISPMSG

	mov	al,2				; abort
	jmp	short RESTHD

;DskErr	endp

	; MSDOS 6.0
;***	RPrint - print message
;***	Crlf - display cr/lf
;
;	ENTRY	DS:DX = ptr to count byte, followed by message text
;		DS:SI = ptr to 1st substitution block for this msg, if any
;		variable fields related to substitution blocks are set
;
;	EXIT	nothing
;
;	USED	flags
;
;	EFFECTS
;	  Message is displayed on stdout.
;
;	NOTE
;	  Number of substitutions (%1, %2,...) in message text must not
;	    be greater than number of substition blocks present.

;
;Crlf: 
;	mov	dx,offset DATARES:Newlin	; cheap newline
;
;RPrint	proc
;
;	assume	ds:DATARES,ss:DATARES
;
;;	Bugbug:	do we need to save all reg's?
;
;	push	si			; preserve registers
;	push	ax
;	push	bx
;	push	cx
;	push	dx
;
;	mov	bx,si			; DS:BX = ptr to subst block
;	mov	si,dx			; DS:SI = ptr to count byte
;	lodsb				; AL = message length
;					; DS:SI = ptr to message text
;	xor	cx,cx
;	mov	cl,al			; CX = message length
;	jcxz	rpRet
;
;	call	RDispMsg
;
;rpRet:	pop	dx
;	pop	cx
;	pop	bx
;	pop	ax
;	pop	si
;	ret
;
;RPrint	endp

	; MSDOS 3.3
CRLF:
	mov     dx,NEWLIN

RDISPMSG: ; Display message/text
	; DS:DX = ($ terminated) Message/Text address 
	push    ax
	mov     ah,STD_CON_STRING_OUTPUT ; 9
	clc
	int     21h             ; DOS - PRINT STRING
				; DS:DX -> string terminated by "$"
	pop     ax
	retn

	; MSDOS 6.0

;***	RPrintCrit - print critical error message
;
;	ENTRY	DX = extended error # (19-39)
;
;	EXIT	nothing
;
;	USED	flags
;
;	EFFECTS
;	  Message is displayed on stdout

;RPrintCrit	proc
;
;	assume	ds:DATARES,ss:DATARES
;
;	push	dx			; preserve DX
;	xchg	bx,dx			; BX = extended error #
;					; DX = saved BX
;	sub	bx,19			; BX = critical error index, from 0
;	shl	bx,1			; BX = offset in word table
;	mov	bx,CritMsgPtrs[bx]	; BX = ptr to error msg
;	xchg	bx,dx			; DX = ptr to error msg
;					; BX = restored
;	call	RPrint			; print the message
;	pop	dx			; restore DX
;	ret
;
;RPrintCrit	endp


;***	RDispMsg - display message
;
;	Display message, with substitutions, for RPrint.
;
;	ENTRY	DS:SI = ptr to message text
;		CX = message length
;		DS:BX = ptr to substitution block, if any
;
;	EXIT	nothing
;
;	USED	AX,CX,DX,SI

;RDispMsg	proc
;
;	assume	ds:DATARES,ss:DATARES
;
;rdNextChar:
;	lodsb				; AL = next char
;	cmp	al,'%'
;	jne	rdOutChar		; not a substitution
;	mov	dl,ds:[si]		; DL = possible '1' - '9'
;	sub	dl,'1'			; DL = 0 - 8 = '1' - '9'
;	cmp	dl,9
;	jae	rdOutChar		; not a substitution
;
;;*	A substitution code %1 - %9 has been encountered.
;;	DL = 0-8, indicating %1-%9
;;	DS:BX = ptr to substitution block
;
;	call	SubstMsg		; display the substitution
;	inc	si			; SI = ptr past %n
;	dec	cx			; count extra character in %n
;	jmp	short rdCharDone
;
;;*	Normal character output.
;
;rdOutChar:
;	mov	dl,al			; DL = char
;	mov	ah,2			; AH = DOS Character Output code
;	int	21h			; call DOS
;rdCharDone:
;	loop	rdNextChar
;	ret
;
;RDispMsg	endp

;***	SubstMsg - display message substitution
;
;	Display a substitution string within a message.
;	Substitution can be a char, an ASCIIZ string, or
;	a word to be displayed as hex digits.
;
;	ENTRY	DL = substitution index 0-8 (for codes %1-%9)
;		DS:BX = ptr to substitution block
;
;	EXIT	nothing
;
;	USED	AX,DX

;SubstMsg	proc
;
;	assume	ds:DATARES,ss:DATARES
;
;	push	bx			; preserve BX
;	push	cx			; preserve CX
;
;	mov	al,size SUBST		; AL = size of substitution block
;	mul	dl			; AX = offset of desired subst block
;	add	bx,ax			; DS:BX = ptr to desired subst block
;
;	mov	al,[bx].SubstType	; AX = substitution type flag
;	mov	bx,[bx].SubstPtr	; BX = ptr to char, str, or hex value
;
;;	AL = 1, 2, or 3 for char, string, or hex type
;
;	dec	al
;	jz	smChar
;	dec	al
;	jz	smStr
;
;;*	Hex number substitution.
;
;	mov	ax,ds:[bx]		; AX = word value
;	mov	cx,4			; CX = # digits to display
;smDigit:
;	rol	ax,1
;	rol	ax,1
;	rol	ax,1
;	rol	ax,1			; AL<3:0> = next digit
;
;	push	ax			; save other digits
;	and	al,0Fh			; AL = binary digit
;	add	al,'0'			; AL = ascii digit if 0-9
;	cmp	al,'9'
;	jbe	@F			; it's 0-9
;	add	al,'A' - '0' - 10	; AL = ascii digit A-F
;@@:
;	mov	dl,al			; DL = ascii digit
;	mov	ah,2
;	int	21h			; output the ascii digit
;	pop	ax			; restore all digits
;
;	loop	smDigit
;	jmp	short smRet
;
;;*	Char substitution.
;
;smChar:
;	mov	dl,ds:[bx]		; DL = char to output
;	mov	ah,2
;	int	21h
;	jmp	short smRet
;
;;*	String substitution.
;
;smStr:
;	mov	dl,ds:[bx]		; DL = next char
;	or	dl,dl
;	jz	smRet			; null char - we're done
;	mov	ah,2
;	int	21h			; display char
;	inc	bx			; DS:BX = ptr to next char
;	jmp	smStr
;
;smRet:	pop	cx
;	pop	bx
;	ret
;
;SubstMsg	endp


	; MSDOS 6.0

;***	CharToUpper - convert character to uppercase
;
;	ENTRY	AL = char
;
;	EXIT	AL = uppercase char
;
;	USED	AX

;CharToUpper	proc
;
;	assume	ds:DATARES
;
;	push	ax		; put char on stack as arg to int 2F
;	mov	ax,1213h	; AX = DOS int 2F 'Convert Char to Uppercase'
;	int	2Fh
;	inc	sp		; throw away old char on stack
;	inc	sp
;	ret
;
;CharToUpper	endp

	; MSDOS 3.3
CHARTOUPPER:
	cmp	al,80h
	jb	short CHARTOUPPER1
	sub	al,80h
	push	ds
	push	bx
	lds	bx,[UPPERCASETBL]
	add	bx,2
	xlat
	pop	bx
	pop	ds
	jmp	short CHARTOUPPER_RETN
CHARTOUPPER1:
	cmp	al,'a'
	jb	short CHARTOUPPER_RETN
	cmp	al,'z'
	ja	short CHARTOUPPER_RETN
	sub	al,20h
CHARTOUPPER_RETN:
	retn

;public	EndCode
;EndCode label	byte

;=============================================================================
; RDATA.ASM, MSDOS 6.0, 1991
;=============================================================================
; 22/09/2018 - Retro DOS v3.0

;NOTE: This initialized rezident data is just as rezident data section/portion
;      of the disassembled MSDOS 3.3 COMMAND.COM code.
;   (MSDOS 6.0 RDATA.ASM is used for comments & descriptions about the RDATA.) 

; RDATA section of Retro DOS v2.0 'command2.s' (05/05/2018) has been modified 
; for 'command3.s' (22/09/2018), here:

ALIGN 2
	; 22/09/2018 - Retro DOS v3.0 (MSDOS 3.3) COMMAND.COM
RSTACK_SPACE:
	times 128 db 0
RSTACK:
	dw 	0

	; 25/09/2018	
	; (filler)
	dW	0

RDATA	EQU	$

;TITLE   COMMAND Resident DATA

; Data for resident portion

;DATARES SEGMENT PUBLIC BYTE

	;ORG     0
;ZERO    =       $

CRMSGTBL:
	dw	CRMSG0		; "Write protect$"
	dw	CRMSG1		; "Bad unit$"
	dw	CRMSG2		; "Not ready$"
	dw	CRMSG3		; "Bad command $"
	dw	CRMSG4		; "Data$"
	dw	CRMSG5		; "Bad call format$"
	dw	CRMSG6		; "Seek$"
	dw	CRMSG7		; "Non-DOS disk$"
	dw	CRMSG8		; "Sector not found$"
	dw	CRMSG9		; "No paper$"
	dw	CRMSG10		; "Write fault$"
	dw	CRMSG11		; "Read fault$"
	dw	CRMSG12		; "General Failure$"
	dw	CRMSG13		; "Sharing Violation$"
	dw	CRMSG14		; "Lock Violation$"
	dw	CRMSG15		; "Invalid Disk Change$"
	dw	CRMSG16		; "FCB unavailable$"
	dw	CRMSG17		; "Sharing buffer exceeded$"

CRMSG0:	 db 'Write protect$'
CRMSG1:	 db 'Bad unit$'
CRMSG2:	 db 'Not ready$'
CRMSG3:	 db 'Bad command $'
CRMSG4:	 db 'Data$'
CRMSG5:	 db 'Bad call format$'
CRMSG6:	 db 'Seek$'
CRMSG7:	 db 'Non-DOS disk$'
CRMSG8:	 db 'Sector not found$'
CRMSG9:	 db 'No paper$'
CRMSG10: db 'Write fault$'
CRMSG11: db 'Read fault$'
CRMSG12: db 'General Failure$'
CRMSG13: db 'Sharing Violation$'
CRMSG14: db 'Lock Violation$'
CRMSG15: db 'Invalid Disk Change$'
CRMSG16: db 'FCB unavailable$'
CRMSG17: db 'Sharing buffer exceeded$'

NEEDVOLMSG:	db 'Please Insert disk '
VOLNAME: times 11 db 0	; db 0Bh dup(0)
		db 0Dh,0Ah,'$'
MREAD:		db 'reading$'
MWRITE:		db 'writing$'
ERRMES:		db ' error $'
BLKDEVERR:	db ' drive '
DRVLET:		db 'A'
NEWLIN:		db 0Dh,0Ah,'$'
CHARDEVERR:	db ' device '
DEVNAME: times 8 db 0 ; db 8 dup(0)
		db 0Dh,0Ah,'$'
REQ_ABORT:	db 'Abort$'
REQ_RETRY:	db ', Retry$'
REQ_IGNORE:	db ', Ignore$'
REQ_FAIL:	db ', Fail$'
REQ_END:	db '? $'
CDEVAT:		db 0
BADFATMSG:	db 0Dh,0Ah,'File allocation table bad,$'
COMBAD:		db 0Dh,0Ah,'Invalid COMMAND.COM',0Dh,0Ah,'$'
PUTBACKMSG:	db 'Insert disk with $'
PROMPT:		db ' in drive '
PUTBACKDRV:	db ' ',0Dh,0Ah, 'and strike any key when ready',0Dh,0Ah,'$'
ENDBATMES:	db 0Dh,0Ah,'Terminate batch job (Y/N)? $'
EXECEMES:	db 'EXEC failure',0Dh,0Ah,'$'
EXEBAD:		db 'Error in EXE file',0Dh,0Ah,'$'
TOOBIG:		db 'Program too big to fit in memory',0Dh,0Ah,'$'
NOHANDMES:	db 0Dh,0Ah,'No free file handles$'
RBADNAM:	db 'Bad Command or file name',0Dh,0Ah,'$'
ACCDEN:		db 'Access denied',0Dh,0Ah,'$'
BMEMMES:	db 0Dh,0Ah,'Memory allocation error $'
HALTMES:	db 0Dh,0Ah,'Cannot load COMMAND, system halted$'
FRETMES:	db 0Dh,0Ah,'Cannot start COMMAND, exiting',0Dh,0Ah,'$'
PATRICIDE:	db 0Dh,0Ah,'Top level process aborted, cannot continue. $'
YES_CHAR:	db 'Y'
NO_CHAR:	db 'N'
RETRY_CHAR:	db 'R'
ABORT_CHAR:	db 'A'
IGNORE_CHAR:	db 'I'
FAIL_CHAR:	db 'F'
NEEDVOL:	dd 0
ERRTYPE:	db 0

; 22/09/2018 -  22/09/2018 - Retro DOS v3.0 (MSDOS 3.3) COMMAND.COM

RETRABASE: ; 30/04/2018 (Base addr for common params, for transient portion)
	   ; Note: Order of following parameters must not be changed
	   ; without changing address references of them in 'transcom.s').	

INT_2E_RET:	DD 0		; Magic command executer return address
SAVE_PDB:	DW 0
PARENT:		DW 0
OLDTERM:	dd 0
ERRCD_24:	DW 0
HANDLE01:	DW 0
LOADING:	DB 0
BATCH:		DW 0		; Assume no batch mode initially

;		Bugbug:	ComSpec should be 64+3+12+1?
;		What's this comspec_end about?
COMSPEC: times 64 DB 0
PUTBACKSUBSTPTR: dw 0
COMSPEC_END:	dw 0
TRANS:		DW COMMAND
TRNSEG:		DW 0
TRNMVFLG:	db 0		; set if transient portion has been moved
IN_BATCH:	db 0		; set if we are in batch processing mode
BATCH_ABORT:	db 0		; set if user wants to abort from batch mode

COMDRV:		DB 0		; DRIVE SPEC TO LOAD AUTOEXEC AND COMMAND
MEMSIZ:		DW 0
SUM:		DW 0
EXTCOM:		DB 1		; For init, pretend just did an external
RETCODE:	DW 0
CRIT_ERR_INFO:	db 0		; hold critical error flags for r,i,f

; The echo flag needs to be pushed and popped around pipes and batch files.
; We implement this as a bit queue that is shr/shl for push and pop.

ECHOFLAG:	DB 1		; low bit true => echo commands
SUPPRESS:	db 1		; used for echo, 1=echo line
IO_SAVE:	DW 0
RESTDIR:	DB 0
PERMCOM:	DB 0		; true => permanent command
SINGLECOM:	DW 0		; true => single command version
VERVAL:	        DW -1
FFAIL:		db 0		; true => fail all int 24s
IFFLAG:		db 0            ; true => IF statement in progress
FORFLAG:	DB 0		; true => FOR statement in progress
FORPTR:		dw 0		
NEST:		dw 0		; nested batch file counter
CALL_FLAG:	db 0		; no CALL (batch command) in progress
CALL_BATCH_FLAG: db 0
NEXT_BATCH:	dw 0		; address of next batch segment
NULLFLAG:	db 0		; flag if no command on command line

UCASE_ADDR:	db 0
UPPERCASETBL:	dd 0

RE_OUT_APP:	db 0
RE_OUTSTR: times (64+3+13) db 0

INITFLAG:	DB INITINIT ; 1 ; 24/09/2018

; Note:  these two bytes are referenced as a word
PIPEFLAG:	DB 0
PIPEFILES:	DB 0

;--- 2.x data for piping
;
; All the "_" are substituted later, the one before the : is substituted
; by the current drive, and the others by the CreateTemp call with the
; unique file name. Note that the first 0 is the first char of the pipe
; name. -MU
;
;--- Order-dependent, do not change

; MSDOS 3.3

PIPE1:		db "_:/"
PIPE1T:		db 0
		db "_______.___",0
PIPE2:		db "_:/"
PIPE2T		db 0
		db "_______.___",0

; MSDOS 3.3 & MSDOS 6.0
PIPEPTR:	dw 0
PIPESTR: times 129 db 0

; MSDOS 6.0

;SR
; Pipe1 & Pipe2 now need to store full-fledged pathnames
;

; Bugbug:  can we find any way around maintaining these
; large buffers?

;Pipe1		db	67+12 dup (?)
;Pipe2		db	67+12 dup (?)
;PipePtr 	dw	?
;PipeStr 	db	129 dup (?)

;EndPipe	label	byte	; marks end of buffers; M004

INPIPEPTR:	dw PIPE1
OUTPIPEPTR:	dw PIPE2

;EXEC_BLOCK LABEL BYTE		; The data block for EXEC calls
EXEC_BLOCK:
ENVIRSEG:	DW 0
;COM_PTR LABEL	DWORD
COM_PTR:	DW 80H		; Point at unformatted parameters
		DW 0
;COM_FCB1 LABEL	DWORD
COM_FCB1:      	DW 5CH
        	DW 0
;COM_FCB2 LABEL	DWORD
COM_FCB2:	DW 6CH
        	DW 0

;TRANVARS LABEL  BYTE		; Variables passed to transient
TRANVARS:
		;DW OFFSET RESGROUP:THEADFIX
		DW THEADFIX
MYSEG:		DW 0		; Put our own segment here
LTPA:		DW 0		; WILL STORE TPA SEGMENT HERE
RSWITCHAR:	DB "-"
RDIRCHAR:	DB "/"
        	;DW OFFSET RESGROUP:EXT_EXEC
		DW EXT_EXEC
MYSEG1:		DW 0
		;DW OFFSET RESGROUP:TREMCHECK
		DW TREMCHECK
MYSEG2:		DW 0

RESTEST:	dw 0
RES_TPA:	DW 0		; original tpa (not rounded to 64k)
;TRANVAREND	label	byte
TRANVAREND:
OLDERRNO:	dw 0

		; Here is Offset 0D27h in original MSDOS 3.3 COMMAND.COM

;DATARESEND	LABEL   BYTE
DATARESEND:
		; 22/09/2018

;DATARES ENDS
;        END

;=============================================================================
; COMMAND.ASM (MSDOS 2.11), ENVDATA.ASM (MSDOS 6.0)
;=============================================================================
; 22/09/2018 - Retro DOS v3.0 ('command3.s')

align 16

	; Here is Offset 0D80h in original MSDOS 3.3 COMMAND.COM

;ENVIRONMENT SEGMENT PUBLIC PARA        ; Default COMMAND environment

;	PUBLIC  ECOMSPEC,ENVIREND,PATHSTRING

        ;ORG	0
;ENVARENA	DB 10H DUP (?)	; Pad for mem arena
ENVARENA:	TIMES 16 DB 0		
ENVIRONMENT:			; 30/04/2018 ('command2.s')
	; Here is Offset 0D90h in original MSDOS 3.3 COMMAND.COM
PATHSTRING:	DB "PATH="
;USERPATH LABEL  BYTE
USERPATH:      	DB 0		; Null path
        	DB "COMSPEC="
ECOMSPEC:	DB "/COMMAND.COM"
        	;DB 134 DUP (0)
		TIMES	134 DB 0

;ENVIREND	LABEL   BYTE
ENVIREND:
	; Here is Offset 0E30h in original MSDOS 3.3 COMMAND.COM
	
ENVIRONSIZ EQU  $-PATHSTRING	; 160 = 0A0h
ENVIRONSIZ2 EQU $-ECOMSPEC	; 146 = 092h

;ENVIRONMENT ENDS

; ENVDATA.ASM
;
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;	SCCSID = @(#)envdata.asm	1.1 85/05/14
;
; This file is included by init.asm and is used as the default environment.
;
;
;Environment Struc                       ; Default COMMAND environment
;
;Env_PathString  db	"path="
;Env_PathSpec    db     "c:\msdos"
;                db     0
;Env_PrmptString db     "prompt="
;Env_PrmptSpec   db     "$p$g"
;                db     0
;Env_ComString   db     "comspec="
;Env_ComSpec     db     "\command.com"
;		 db	134 dup (0)
;
;Environment ends

;MAX_COMSPEC	equ     SIZE Environment - Env_ComSpec

MAX_COMSPEC	EQU	ENVIRONSIZ2 ; = 146  ; 22/09/2018

;-----------------------------------------------------------------------------
; 24/09/2018 - RetRo DOS v3.0
;ENDCODE:
INITSTART:	; End of rezident code and data
		; (Offset 0E30h in original MSDOS 3.3 COMMAND.COM)
;-----------------------------------------------------------------------------

;=============================================================================
; INIT.ASM, MSDOS 6.0 (COMMAND.COM), 1991
;=============================================================================
; 22/09/2018 - Retro DOS v3.0 ('command3.s')

; INIT.ASM (MSDOS 2.11 COMMAND.COM, Retro DOS v2.0, 30/04/2018)

;TITLE   COMMAND Initialization

;ENVIRONSIZ EQU  0A0H		;Must agree with values in ENVIRONMENT segment
;ENVIRONSIZ2 EQU 092H
;MAX_COMSPEC EQU ENVIRONSIZ2 ; = 146  ; 22/09/2018

; UINIT.ASM, MSDOS 6.0, 1991
; 23/09/2018
ENVBIG	EQU	32768			;AN000; maximum environment size
ENVSML	EQU	160			;AN000; minimum environment size

;-----------------------------------------------------------------------------
; START OF INIT PORTION
; This code is deallocated after initialization.
;-----------------------------------------------------------------------------

;INIT    SEGMENT PUBLIC PARA

; 	EXTRN   HEADER:BYTE
;	EXTRN   BADCOMLKMES:BYTE

;	PUBLIC  CONPROC

;ASSUME  CS:RESGROUP,DS:RESGROUP,ES:RESGROUP,SS:RESGROUP

        ;ORG 0
;ZERO = $
	; 23/09/2018
ZERO equ $	; Offset 0E30h for original MSDOS 3.3 COMMAND.COM

CONPROC:
	;MOV	SP,OFFSET RESGROUP:RSTACK	; must be first instruction
	MOV	SP,RSTACK
;
; We need to set the PSP to us right at start because Carousel needs
; to be lied to and it does not set PSP when it transfers control to
; us after loading us as an overlay. By setting PSP, we ensure that
; command.com is also not lied to.
;
	; MSDOS 6.0
        ;mov     ah,SET_CURRENT_PDB
        ;mov     bx,es
        ;int     21h

        mov     ax,GET_VERSION<<8 ; 30h
	int	21h
	cmp	ax,EXPECTED_VERSION ; 1E03h
	je	short OKDOS			; DOS version is ok

	mov	dx,BADVERMSG			; DX = ptr to msg
	;call	RPrint

	; MSDOS 3.3
	mov	ah,STD_CON_STRING_OUTPUT ; 9
	int	21h             ; DOS - PRINT STRING
				; DS:DX -> string terminated by "$"
	mov	ax,es
	cmp	[es:PDB.PARENT_PID],ax
	;cmp	[es:16h],ax			; if COMMAND is own parent,
HERE:	jz	short HERE			;  loop forever
	
	int	20h				; otherwise, exit
OKDOS:
	; 23/09/2018
;
;  Calculate and save the end of the INIT segment (which is also
;  the beginning of TRANGROUP).
;
	; MSDOS 3.3
	mov	ah,65h
	mov	al,2
	mov	dx,-1
	mov	bx,-1
	mov	cx,5
	mov	di,UCASE_ADDR
	int     21h	; AH = 65h : GET EXTENDED COUNTRY INFORMATION (DOS 3.3+)
			; AL = 02h : Get pointer to character translation table
			; BX = code page (-1 = current global code page)
			; DX = country ID (-1 = current country)
			; CX = amount of data to return
	; ES:DI = pointer to output buffer
	; Buffer offset :
	;	00h -  byte,  country Id
	;  	01h -  dword, pointer to uppercase table	

	; MSDOS 6.0 (& MSDOS 3.3)
  	mov	dx,TRANSTART+15			; get end of init code
	; 27/09/2018
        ;mov	dx,TRANSTART ; (paragraph aligned address)
	mov	cl,4				; change to paragraphs
        shr	dx,cl				;
        mov     ax,cs                           ; get current segment
        add     ax,dx                           ; calculate segment of end of init
        mov     [INITEND],ax			; save this

	mov	ax,[PDB.PARENT_PID] ; mov ax,ds:16h ; Init PARENT so we can exit
	mov	[PARENT],ax			    ;  correctly.
	mov	ax,[PDB.EXIT]   ; mov ax,ds:0Ah
	mov	[OLDTERM],ax
	mov	ax,[PDB.EXIT+2] ; mov ax,ds:0Ch
	mov	[OLDTERM+2],ax

	;mov	ax,ENDCODE+15
	mov	ax,INITSTART+15 ; 24/09/2018
	mov	cl,4				; ax = size of resident part of
	shr	ax,cl				;  command in paragraphs. Add
	mov	cx,cs				;  this to CS and you get the
	add	ax,cx				;  segment of the TPA.
	
	mov     [RES_TPA],ax			; Temporarily save the TPA segment
	and     ax,0F000h
	add     ax,1000h			; Round up to next 64K boundary
	jnc     short TPA_SET			; Memory wrap if carry set
	mov     ax,[RES_TPA]
TPA_SET:
	mov	[LTPA],ax			; Good enough for the moment
	mov	ax,[PDB.BLOCK_LEN] ; mov ax,ds:2 ; ax = # of paras given to command

	mov	[MYSEG1],ds			; These 3 variables are used as part of
	mov	[MYSEG2],ds			;  3 long ptrs that the transient will
	mov	[MYSEG],ds			;  use to call resident routines.

        mov     [MEMSIZ],ax			; Needed for execing other programs
;
; Compute maximum size of environment
;
	;mov	word [ENVMAX],69 ; = (160/16)+(973/16)-1 ; (11EEh-0E30h+0Fh/10h) = 3Ch
        mov	word [ENVMAX],((ENVIRONSIZ+15)/16) + ((ENVMAXIMUM-ZERO+15)/16) - 1
;
; Compute minimum size of environment
;
	;mov	word [ENVSIZ],10 ; = 160/16
	mov	word [ENVSIZ],ENVSML/16; 

	;mov	dx,offset TranGroup:Transpaceend + 15 ; dx = size of transient
	mov	dx,TRANSPACEEND+15 ; 4D5Ch+0Fh (for MSDOS 3.3 COMMAND.COM)
	mov	cl,4				;  in paragraphs.
	shr	dx,cl
        mov     [TRNSIZE],dx			; save size of transient in paragraphs

	sub	ax,dx				; max seg addr - # para's needed for transient
	mov	[TRNSEG],ax			;  = seg addr to load the transient at.
	mov	ax,[PDB.ENVIRON] ; mov ax,ds:2Ch ; ax = environment segment

        ; MSDOS 6.0
	;mov	EnvirSeg,ax			;
        ;or	ax,ax				; if there is no environment segment,
        ;jz	buildenv			; make one
	;
        ;cmp	FirstCom,0			; if this is the first command.com,
	;je	environpassed			; do a merge job (make sure COMSPEC exists)

	; MSDOS 3.3
	or	ax,ax
	jz	short BUILDENV
        inc	byte [CHUCKENV]			; Flag no ENVIRONSEG
        jmp	short ENVIRONPASSED

	; MSDOS 6.0
;
; We allocate a buffer here just large enough to hold the 'PATH=' and
; the COMSPEC. After parsing, we will allocate an environment of the right
; size and free this buffer. We need this buffer because we no longer have an
; ENVIRONMENT segment but need a place to store the COMSPEC which can be
; given on the command line before we know the environment size. This routine
; will not return in case of an allocation error. It will either exit or hang
; depending on whether or not this is the first COMMAND.COM or not.
;
;buildenv:
;	call	alloc_env                       ; try to allocate buffer
;environpassed:
;	mov	es,ax                           ; and it load into es.
;	assume	es:nothing
;
;gottheenvir:
;;
;; Initialize the command drive
;;
	; MSDOS 3.3
BUILDENV:
        ;mov	ax,offset RESGROUP:PATHSTRING	; Figure environment pointer
	mov	ax,PATHSTRING ; "PATH="
	mov	cl,4
	shr	ax,cl
	mov	dx,ds
	add	ax,dx

ENVIRONPASSED:
	mov	[ENVIRSEG],ax

	mov	es,ax

	;mov	ax,CHAR_OPER<<8
	mov	ax,CHAR_OPER*256 ; 3700h
	int     21h	; DOS - 2+ internal - GET SWITCHAR/AVAILDEV
			; Return: AL = FFh unsupported subfunction
			; DL = current switch character
	mov	[RSWITCHAR],dl

	; 02/03/2023
	cmp	dl,'/'
	;cmp	dl,[slash_chr]
	jnz	short IUSESLASH
	mov	al,'\'
	;mov	al,[bslash_chr]
	mov	[COMSPECT],al

	cmp	byte [CHUCKENV],0
	jnz	short IUSESLASH

	;mov	ES:[ECOMSPEC-10h],'\'
	; 30/04/2018
	;mov	byte [ES:(ECOMSPEC-ENVIRONMENT)],'\' ; [ES:0Eh]
	; 23/09/2018
	mov	[ES:(ECOMSPEC-ENVIRONMENT)],al ; mov es:0Eh,al

;gottheenvir:
IUSESLASH:
;
; Initialize the command drive
;
	; MSDOS 3.3 & MSDOS 6.0
	mov	ah,GET_DEFAULT_DRIVE	; 19h
	int	21h
	inc	al
	mov	[COMDRV],al

        ;mov	al,byte ptr ds:[FCB]	; al = default drive number for command
        mov	al,[FCB]
	or	al,al
	jz	short NOCOMDRV		; no drive specified

	mov	ah,':'
	mov	[COMDRV],al
	add	al,40h			; convert number to uppercase character

	std

	; MSDOS 6.0
        ;cmp	AllocedEnv,0		; if a new environment is being built,
	;je	notwidenv		;  move the default comspec string in it
	;mov	di,ComspOffset
        ;cmp	byte ptr es:[di+1],':'	; drive specifier already exist?
        ;je	notwidenv               ; yes, must have been inherited that way

	; MSDOS 3.3
        cmp	byte [CHUCKENV],0
	jne	short NOTWIDENV

  	push	ds			;  2 bytes to make room for a drivespec.
	push	es			;  the drivespec is in ax and is copied
	pop	ds			;  on to the front of the string.

	; MSDOS 6.0
        ;lea	si,[di+MAX_COMSPEC-3]	
        ;lea	di,[di+MAX_COMSPEC-1]

	; MSDOS 3.3
	; 23/09/2018
	; 30/04/2018
	;mov	di,159
	;MOV	DI,OFFSET ENVIRONMENT:ECOMSPEC+ENVIRONSIZ2-1-10H
        mov	di,(ECOMSPEC-ENVIRONMENT)+ENVIRONSIZ2-1 ; mov di,9Fh
	;mov	si,157
	;MOV	SI,OFFSET ENVIRONMENT:ECOMSPEC+ENVIRONSIZ2-3-10H
        mov	si,(ECOMSPEC-ENVIRONMENT)+ENVIRONSIZ2-3 ; mov si,9Dh 
	;mov	cx,144
	;MOV	CX,ENVIRONSIZ2-2 ; mov cx,90h
        mov     cx,MAX_COMSPEC-2

	rep	movsb
	pop	ds

	; MSDOS 6.0
	;mov	word ptr es:[di-1],ax

	; MSDOS 3.3
	;mov	es:0Eh,ax
	;MOV	WORD PTR ES:[ECOMSPEC-10H],AX
	mov	[es:(ECOMSPEC-ENVIRONMENT)],ax	; mov [es:0Eh],ax

	; MSDOS 3.3 & MSDOS 6.0
NOTWIDENV:
	cld

	mov    [AUTOBAT],ax ; ":\AUTOEXEC.BAT"

NOCOMDRV:
        CALL    SETVECT         ; Set the vectors

	; MSDOS 3.3
	mov	si,80h		; get command line
	lodsb			; get length of line
	mov	cl,al
	xor	ch,ch		; cx = length of command line
;
; insure that the command line correctly ends with a cr
;
	add	si,cx		; go to end of command line	
	mov	byte [si], 0Dh	; insert a carriage return

	mov	si,81h		; Start of parms
CHKARG:
        jcxz	COMRETURNSJ     ; No parameters
        dec	cx
	lodsb
CHECKSWITCHCHR:
	; 02/03/2023
	cmp	al,' ' ; 20h
	;cmp	al,[space_chr]	 ;Skip blank spaces
	jz	short CHKARG
	cmp	al,9		; Skip TAB characters
	jz	short CHKARG
	cmp	al,[RSWITCHAR]	; Switch?
	jz	short PARSE_CONT ; Yes, get the char after the switch
	jmp	CHKOTHERARGS	; No

PARSE_CONT:
;
; See if a switch was entered
;
	jcxz	COMRETURNSJ
	dec	cx
	lodsb
	or	al,20h		; Lower case
	cmp	al,'f'		; FAIL switch
	jnz     short CHECKPSWITCH
	
	; MSDOS 6.0
;SetFSwitch:
	;cmp	fFail,-1		; has fail switch been set?
	;jnz	failok			; no - set it
        ;mov	ax,Moreargs_ptr         ; set up too many arguments
        ;jmp	parse_line_error        ; go issue error 

	; MSDOS 3.3 & MSDOS 6.0
FAIL_OK:
	mov	byte [FFAIL],-1	; fail all INT 24s.
	; MSDOS 3.3
	jmp	short CHKARG

CHECKPSWITCH:
	; 02/03/2023
	cmp	al,'p'		; Permanent COMMAND switch
	;cmp	al,[letter_p]
        jnz	short CHECKDSWITCH

SETPSWITCH:
;
; We have a permanent COMMAND switch /P.  Flag this and stash the
; termination address.
;
	; MSDOS 6.0
	;cmp	PermCom,0		; has /p switch been set?
	;jz	permcomok		; no - set it
        ;mov	ax,moreargs_ptr		; set up too many arguments
        ;jmp	parse_line_error 		; go issue error 
;permcomok:
	; MSDOS 3.3 & MSDOS 6.0
	inc	byte [PERMCOM]
	mov     word [OLDTERM],LODCOM
	mov     [OLDTERM+2],ds
;
; make sure that we display the date and time. if the flag was not
; initialized, set it to indicate yes, do prompt.
;
	; MSDOS 3.3
	cmp     byte [PRDATTM],-1
	jnz     short CHKARG
	mov     byte [PRDATTM],0
	jmp     short CHKARG

;COMRETURNSJ:
;	; MSDOS 3.3
;	JMP	ARGSDONE

CHECKDSWITCH:
	; 02/03/2023
	cmp	al,'d'
        ;cmp	al,[letter_d]
	jnz     short CHECKCSWITCH
SETDSWITCH:
;
; Flag no date/time prompting.
;
	; MSDOS 6.0
	;cmp	Dswitch,0		; has /D switch been set?
	;jz	setdateok		; no - set it
        ;mov	ax,Moreargs_ptr	; set up too many arguments
        ;jmp	parse_line_error	; go issue error message
;setdateok:
	;inc	Dswitch 		; indicate /D entered

	; MSDOS 3.3 & MSDOS 6.0
        mov	byte [PRDATTM],1  ; User explicitly says no date time
	; MSDOS 3.3
	jmp	short CHKARG

CHECKCSWITCH:
	; 02/03/2023
        cmp	al,'c'
	;cmp	al,[letter_c]
        jnz	short CHECKESWITCH

;SetSSwitch:
SETCSWITCH:
;
; Set up pointer to command line, flag no date/time and turn off singlecom.
;
	mov	byte [PERMCOM],0	; A SINGLECOM must not be a PERMCOM
	mov	[SINGLECOM],si		; Point to the rest of the command line
	;mov	byte [PERMCOM],0
	mov	byte [PRDATTM],1	; no date or time either, explicit
COMRETURNSJ: ; 24/09/2018
	jmp	ARGSDONE

CHECKESWITCH:
	cmp     al,'e'
	jnz     short CHKARG
;
; Look for environment-size setting switch
;
; The environment size is represented in decimal bytes and is
; converted into paragraphs (rounded up to the next paragraph).
;
SETESWITCH:
	; MSDOS 6.0
	;cmp	Eswitch,0		; has environment size switch been set?
	;jz	eswitchok		; no - set it
        ;mov	ax,Moreargs_ptr         ; set up too many arguments
        ;jmp	Parse_line_error	; go issue error message
;eswitchok:
	;inc	Eswitch			; indicate /E entered 	

	; 23/09/2018 - Retro DOS v3.0

	; MSDOS 3.3
	; (COMMAND.COM offset 0FC5h)
ESWITCHOK:
	jcxz    CHKARG
	dec     cx
	lodsb
	cmp     al,':'
	jnz     short CHECKSWITCHCHR
	xor     bx,bx
	mov     ax,bx
GETENVSIZE:
	jcxz    SETENVSIZE
	dec     cx
	lodsb
	cmp     al,'0'
	jb      short NOTDECIMALCHR
	cmp     al,'9'
	ja      short NOTDECIMALCHR
	sub     al,'0'
	mov     dx,bx
	shl     dx,1
	shl     dx,1   ; dx = 4*bx	
	add     bx,dx  ; bx = 5*bx
	shl     bx,1   ; bx = 10*bx	
	add     bx,ax
	jmp     short GETENVSIZE

NOTDECIMALCHR:
	; 02/03/2023
	cmp	al,' ' ; 20h
	;cmp	al,[space_chr]
	jz      short CHECKENVSIZE
	;cmp	al,'/'
	cmp     al,[RSWITCHAR]
	jz      short CHECKENVSIZE

CHECKNEXTECHR:
	jcxz    INVENVSIZE
	dec     cx
	lodsb
	; 02/03/2023
	cmp	al,' ' ; 20h
	;cmp	al,[space_chr]
	jz      short ENVSIZESPC
	;cmp	al,'/'
	cmp     al,[RSWITCHAR]
	jnz     short CHECKNEXTECHR

ENVSIZESPC:
	dec     si
	inc     cx
	jmp     short INVENVSIZE
	
	;nop

CHECKENVSIZE: 
	dec     si
	inc     cx

SETENVSIZE:
	;mov	word [ENVSIZ],2048	; Maximum environment size (32KB)
	mov	word [ENVSIZ],ENVBIG/16
	cmp     bx,32768
	ja      short INVENVSIZE
	;mov	word [ENVSIZ],10	; Minimum environment size (160 bytes)
	mov	word [ENVSIZ],ENVSML/16
	cmp     bx,160
	jb      short INVENVSIZE
	add     bx,15
	shr     bx,1
	shr     bx,1
	shr     bx,1
	shr     bx,1
	mov	[ENVSIZ],bx		; EnvSiz is in paragraphs
	jmp     short NEXTCH2

	;nop

INVENVSIZE:
	mov     dx,BADENVSIZMSG ; "Invalid environment size specified\r\n$"
	mov     ah,STD_CON_STRING_OUTPUT ; 9
	int     21h             ; DOS - PRINT STRING
				; DS:DX -> string terminated by "$"
NEXTCH2:
	; (MSDOS 3.3 COMMAND.COM, offset 1041h)
	jmp     CHKARG

	;jmp	ARGSDONE

CHKOTHERARGS:
;
; We have a non-switch character here.
;
	; MSDOS 6.0
;	push	ds ; ***			;
;	push	si ; **** 			; save place in command line
;	lds	si,Comnd1_Addr			; get address of filespec
;	assume	ds:nothing			;
;
;	mov	dx,si				; put in dx also
;       mov     ax,(OPEN shl 8) or 2            ; Read and write
;	int	21h
;	jc	ChkSrchSpec			; Wasn't a file
;	mov	bx,ax
;	mov	ax,IOCTL shl 8
;	int	21h
;	test	dl,80h
;	jnz	IsaDevice
;
;BadSetCon:					;
;	mov	ah,CLOSE			; Close initial handle, wasn't a device
;	int	21h
;	jmp	short ChkSrchSpec

	; MSDOS 3.3
	; (COMMAND.COM offset 1047h)
        dec	si
	inc	cx
	mov	dx,si
	push	cx ; *
	push	si ; **
CONTRLOOP:
	lodsb
	dec	cx
	; 02/03/2023
	cmp	al,' ' ; 20h
	;cmp	al,[space_chr]
	jz	short SETCDEV
	cmp	al,9
	jz	short SETCDEV
	jcxz	SETCDEVA
	jmp	short CONTRLOOP

SETCDEVA:
        inc	si
SETCDEV:
	mov	byte [SI-1],0
        ;mov	ax,(OPEN SHL 8) OR 2  ; Read and write
        mov	ax,(OPEN*256) | 2 ; 3D02h
        int     21h	; DOS - 2+ - OPEN DISK FILE WITH HANDLE
			; DS:DX -> ASCIZ filename
			; AL = access mode
			; 2 - read & write
        jc	short CHKSRCHSPEC	; Wasn't a file
        mov	bx,ax
        ;mov	ax,IOCTL shl 8
        mov     ax,IOCTL*256 ; 4400h
	int     21h	; DOS - 2+ - IOCTL - GET DEVICE INFORMATION
			; BX = file or device handle
        test	dl,80H
	jnz	short ISADEVICE
BADSETCON: ; MSDOS 6.0
	mov	ah,CLOSE ; 3Eh	; Close initial handle, wasn't a device
        int     21h	; DOS - 2+ - CLOSE A FILE WITH HANDLE
			; BX = file handle
        JMP     short CHKSRCHSPEC

	;nop

ISADEVICE:
	; MSDOS 3.3 & MSDOS 6.0
	xor	dh,dh
	or	dl,3				; Make sure has CON attributes
	;mov	ax,(IOCTL shl 8) or 1
	mov	ax,(IOCTL*256) | 1 ; 4401h
	int	21h
	;
	; 25/09/2018
	pop	dx ; **
	pop	dx ; *
	;
	jc	short BADSETCON	; MSDOS 6.0	; Can't set attributes - quit
	mov	dx,bx				; Save new handle

	; MSDOS 6.0
	;cmp	es:DevFlag,1
	;jz	DevErr

	; MSDOS 3.3
        ;pop	bx ; **				; Throw away saved SI
        ;pop	bx ; *				; Throw away saved CX

	; MSDOS 3.3 & MSDOS 6.0
	push	cx
	mov	cx,3
	xor	bx,bx
RCCLLOOP:
	mov	ah,CLOSE ; 3Eh
	int	21h
	inc	bx
	loop	RCCLLOOP

	mov	bx,dx				; New device handle
	mov	ah,XDUP ; 45h
	int	21h				; Dup to 0
	mov	ah,XDUP
	int	21h				; Dup to 1
	mov	ah,XDUP
	int	21h				; Dup to 2
	mov	ah,CLOSE
	int	21h				; Close initial handle
	
	pop	cx
	
	; MSDOS 6.0
	;pop	si ; ****			; restore position of command line
	;pop	ds ; ***			;
;
; Register the fact that we already have redirected the output and cannot do
; it again
;
	;inc	es:DevFlag			;
        ;jmp	Parse_command_line		; continue parsing

	; MSDOS 3.3
	jcxz	ARGSDONEJ2
	jmp	CHKARG

	; MSDOS 6.0
;DevErr:
	;pop	si ; ****
	;pop	ds ; ***
	;mov	dx,1
        ;call	RPrintParse                     ; "Too many parameters"
        ;call	CrLf
	;jmp	Parse_command_line

CHKSRCHSPEC:                    ; Not a device, so must be directory spec

	; MSDOS 6.0
        ;cmp	es:PathFlag,1			; already set COMSPEC?
        ;jz	DevErr				; yes, error
	;
        ;inc	es:PathFlag			; mark that we have a path
;
; We have to override the passed environment. Allocate a buffer for use now.
; This buffer will later be replaced by a proper environment
;
	;call	alloc_env                       ; environment buffer
	;mov	es,ax
	;assume	es:nothing
	;push	si				; remember location of file
	;xor	cx,cx				; clear cx for counting

;countloop:
	;lodsb					; get a character
	;inc	cx				; increment counter
        ;cmp	al,END_OF_LINE_OUT              ; are we at end of line?
	;jnz	countloop			; no - keep counting
	;
	;mov	al,Space
	;dec	si				; move back one
        ;mov	byte ptr [si],al                ; put a space at end of line
;
; We now know how long the new pathspec for command.com is.  Time to
; figure out how long the current COMSPEC setting is, and then to move
; all the environment data up, throwing that COMSPEC setting away, and
; preparing to append the new COMSPEC.  ComspOffset (the offset of
; where the filespec exists in the environment) is updated as well.
;
	;push	cx                              ;
        ;mov	cx,ENVBIG                       ;
        ;mov	di,ComspOffset                  ; get location of COMSPEC
        ;mov	al,0                            ;
        ;repne	scasb                           ; find the end of COMSPEC
        ;mov	si,di                           ;
;comp_endenv:                                   ;
        ;scasb                                  ; end of env?
        ;je	got_endenv                      ; yes
        ;repne	scasb                           ;
        ;jmp	comp_endenv                     ;
;got_endenv:                                    ;
        ;mov    cx,di                           ;
        ;sub    cx,si                           ;
        ;mov    di,ComspOffset                  ;
        ;sub    di,ComspStrLen                  ;
        ;push   ds                              ;
        ;push   es                              ;
        ;pop    ds                              ;
        ;rep    movsb                           ;
        ;dec    di                              ; copy in new COMSPEC=
        ;push   cs                              ;
        ;pop    ds                              ;
        ;assume ds:ResGroup                     ;
        ;mov    si,offset RESGROUP:ComspString  ;
        ;mov    cx,ComspStrLen                  ;
        ;rep    movsb                           ;
        ;mov    ComspOffset,di                  ;
        ;pop    ds                              ;
        ;assume ds:nothing                      ;
        ;pop    cx                              ;
	;
        ;pop    si                              ; get new comspec location back

	; MSDOS 3.3
	mov     byte [CHUCKENV],0	; If search specified -- no inheritance
	mov     ax,PATHSTRING ; "PATH="	; Figure environment pointer
	mov     cl,4
	shr     ax,cl
	mov     dx,ds
	add     ax,dx
	mov     [ENVIRSEG],ax
	mov     es,ax
	; 02/03/2023
	mov	al,' ' ; 20h
	;mov	al,[space_chr]
	mov     [si-1],al
	pop     si ; **			; Remember location
	pop     cx ; *			; and count
	;mov	di,[ECOMLOC]
	mov     di,[COMSPOFFSET]

COMTRLOOP:
	; MSDOS 3.3 & MSDOS 6.0
	lodsb
	dec	cx
	; 02/03/2023
	cmp	al,' ' ; 20h
	;cmp	al,[space_chr]
	je	short SETCOMSR
	; MSDOS 3.3
	cmp	al,9
	je	short SETCOMSR
	; MSDOS 3.3 & MSDOS 6.0
	stosb
	jcxz	SETCOMSR
	jmp	short COMTRLOOP

SETCOMSR:
	; MSDOS 6.0
	;push	cx
	;
	;push	cs				; Get local segment
	;pop	ds				;
	;assume	ds:ResGroup			;
	;
	;push	ds
	;mov	si,offset ResGroup:ComSpect
	;mov	cx,14
	;
	;mov	al,es:[di-1]
	;
	;cmp	al,RDirChar
	;jnz	iNotRoot
	;inc	si				; Don't make a double /
	;dec	cx
	
	; MSDOS 3.3
	push    si
	push    cx
	push    ds
	mov     si,COMSPECT ; "/COMMAND.COM"
	mov     cx,14
	mov     al,[es:di-1]
	call    PATHCHRCMPR
	jnz     short INOTROOT			
	inc     si				; Don't make a double /
	dec     cx
INOTROOT:
	; MSDOS 3.3 & MSDOS 6.0
	rep	movsb

	;mov	dx,[ECOMLOC]			; Now lets make sure its good!
        mov     dx,[COMSPOFFSET]
	push	es
	pop	ds
	;mov	ax,OPEN shl 8
	mov	ax,OPEN*256 ; 3D00h
	int	21h				; Open COMMAND.COM
	pop	ds
	jc	short SETCOMSRBAD		; No COMMAND.COM here
	mov	bx,ax				; Handle
	mov	ah,CLOSE ; 3Eh
	int	21h				; Close COMMAND.COM
SETCOMSRRET:
	pop	cx
	pop	si

	; MSDOS 6.0
	;pop	ds				;
	;assume	ds:ResGroup			;
	;
	;push	cs				; Make sure local ES is
	;pop	es				;  restored
	;jmp	Parse_command_line		; continue parsing command line

	; MSDOS 3.3
ARGSDONEJ2:
	jcxz	ARGSDONE
	jmp	CHKARG

SETCOMSRBAD:
	; MSDOS 3.3 & MSDOS 6.0
	;mov	dx,offset ResGroup:BadComlkMsg	; dx = ptr to msg
	mov	dx,BADCOMLKMES

;	Note:  we're about to make a near call to TriageError, which
;	lives in a different segment and group.  Some linkers will
;	generate a warning like "Possible fix-up overflow".  We're
;	ok, though, because we all fit in 64 KB and, at init time,
;	we're still all together.

	call	TRIAGEERROR	; TRIAGEERROR procedure is at offset 354Eh
				; in original MSDOS 3.3 COMMAND.COM	
	cmp	ax,65
	jne	short DOPRT
	;mov	dx,offset ResGroup:BadComaccMsg	; dx = ptr to msg
	mov	dx,BADCOMACCMSG
DOPRT:
	call	RPRINT
	;mov	si,offset ResGroup:ComSpect
	mov     si,COMSPECT ; "/COMMAND.COM"
	;mov	di,[ECOMLOC]
        mov     di,[COMSPOFFSET]
	mov	cx,14
	rep	movsb				; get my default back

	jmp	short SETCOMSRRET

ARGSDONE:
	; MSDOS 6.0
	;mov	es,EnvirSeg			; get environment back
	;assume	es:nothing			;

	; MSDOS 3.3 & MSDOS 6.0
        cmp	byte [PERMCOM],0
        jz	short COMRETURNS

	push	es				; Save environment pointer
	mov	ah,SET_CURRENT_PDB ; 50h
	mov	bx,ds
	mov	es,bx
	int	21h				; current process is me
	mov	di,PDB.EXIT ; mov di,0Ah	; Diddle the addresses in my header
	;mov	ax,offset RESGROUP:LODCOM
        mov	ax,LODCOM
        stosw
        mov	ax,ds
        stosw
        ;mov	ax,offset RESGROUP:CONTC
        mov	ax,CONTC
        stosw
        mov	ax,ds
        stosw
	;mov	ax,offset DATARES:CritErr_Trap  ; MSDOS 6.0
	mov     ax,CRITERR
	stosw
	mov     ax,ds
	stosw
	;mov	word ptr ds:16h,ds
	;mov	word ptr ds:[Pdb_Parent_Pid],ds ; Parent is me forever
	mov	[PDB.PARENT_PID],ds
        ;mov	dx,offset RESGROUP:INT_2E
        mov	dx,INT_2E
        ;mov	ax,(SET_INTERRUPT_VECTOR SHL 8) OR 2Eh
	mov	ax,(SET_INTERRUPT_VECTOR*256) | 2Eh ; 252Eh
	int     21h	; DOS - SET INTERRUPT VECTOR
			; AL = interrupt number
			; DS:DX = new vector to be used for specified interrupt
        pop	es				; Remember environment
	
COMRETURNS:
        ;mov	ax,word ptr ds:Pdb_Parent_Pid
	mov	ax,[PDB.PARENT_PID] ; mov ax,ds:16h
        mov	[PARENT],ax			; Save parent
        ;mov	word ptr ds:Pdb_Parent_Pid,ds 	; Parent is me
        mov	[PDB.PARENT_PID],ds ; mov word ptr ds:16h,ds
        ;mov	ax,word ptr ds:PDB_Jfn_Table
        mov	ax,[PDB.JFN_TABLE] ; mov ax, ds:18h
        mov	[IO_SAVE],ax		; Get the default stdin and out
        mov	[COM_PTR+2],ds		; Set all these to resident
        mov	[COM_FCB1+2],ds
	mov	[COM_FCB2+2],ds
        ;mov	di,offset ResGroup:ComSpec
        mov	di,COMSPEC

	;mov	si,[ECOMLOC]
	mov	si,[COMSPOFFSET]
	;cmp	AllocedEnv,0  ; MSDOS 6.0
        cmp	byte [CHUCKENV],0 ; MSDOS 3.3
	
	mov	ax,ds				; Xchg es,ds
	push	es
	pop	ds
	mov	es,ax

	;jne	CopyComsp  ; MSDOS 6.0
        je	short COPYCOMSP	; MSDOS 3.3	; All set up for copy

        push	cs
        pop	ds

        ;mov	si,offset ResGroup:ComspString
	mov	si,COMSPSTRING ; "COMSPEC="
	push	es
	push	di
	call	IFINDE
	mov	si,di
	push	es
	pop	ds
	pop	di
	pop	es
        jnc	short COPYCOMSP

	; MSDOS 6.0
;ComSpecNofnd:
        ;mov	si,offset ResGroup:ComspString
        ;add	si,ComspStrLen
	;push	cs
	;pop	ds

COMSPECNOFND:
	; MSDOS 3.3
        ;mov	si,[es:ECOMLOC]
        mov	si,[es:COMSPOFFSET]
	;add	si,offset RESGROUP:PATHSTRING
        add	si,PATHSTRING ; "PATH="
	push	cs
        pop	ds

COPYCOMSP:
	; MSDOS 3.3 & MSDOS 6.0
	;mov	es:PutBackComSpec.SubstPtr,di
	mov	[es:PUTBACKSUBSTPTR],di		; Save ptr to beginning of comspec path
	cmp	byte [si+1],':'			; Is there a drive specifier in comspec
	jne	short COPYCOMSPLOOP		; If not, do not skip over first 2 bytes
	;add	es:PutBackComSpec.SubstPtr,2
	add	word [es:PUTBACKSUBSTPTR],2

COPYCOMSPLOOP:
	lodsb
	stosb
	or	al,al
	jnz	short COPYCOMSPLOOP

	mov	[es:COMSPEC_END],di		; Save ptr to end of comspec path
	dec	word [es:COMSPEC_END]
	mov	ah,[es:COMDRV]
	add	ah,'A'-1 ; 40h
	mov	[es:PUTBACKDRV],ah		; save drive letter

	; MSDOS 6.0
	;call	setup_for_messages		; set up parse and extended error messages
;
; The routine below sets up the exact resident size of COMMAND. If this is not
; the first COMMAND, then the resident code is not duplicated and the resident
; size is just the data. If we are the first COMMAND, it checks if we are to
; be loaded into HIMEM. If not, then the resident size includes the code and
; the data otherwise it is just the data.
; 
	;call	Setup_res_end			;put resident size in ResSize

	;push	cs
	;pop	ds
	;assume	ds:RESGROUP

;Public EnvMaximum
;EnvMaximum:

	; MSDOS 3.3
	push	cs
        pop	ds
        ;mov	bx,offset RESGROUP:DATARESEND + 15
	mov	bx,DATARESEND+15
        mov	cl,4
        shr	bx,cl
        MOV     AH,SETBLOCK ; 4Ah
        int	21h			; Shrink me to the resident only

ENVMAXIMUM:   ; offset 11EEh for MSDOS 3.3 COMMAND.COM     

	mov	byte [TRNMVFLG], 1
	push	es ; *
	mov	si,TRANSTART
	mov	di,0
	mov	es,[TRNSEG]
	;mov	cx,4D5Ch
	mov	cx,TRANSPACEEND ; 4D5Ch for MSDOS 3.3 COMMAND.COM
	push	cx
	mov	ax,cx
	add	ax,si
	mov	cl,4
	shr	ax,cl
	inc	ax
	mov	cx,ds
	add	ax,cx
	cmp	ax,[TRNSEG]
	pop	cx
	jb	short MOV_DOWN

	call	LOADCOM
	jmp	short ICHKSUM

MOV_DOWN:
	add     si,cx
	dec     si
	add     di,cx
	dec     di
	std
	rep movsb
	cld

ICHKSUM:
	; 24/09/2018
;
; Compute checksum right now before we can get corrupted and save it
;
	; MSDOS 6.0
	;mov	si,offset RESGROUP:TranStart
	;add	si,100h
	;mov	cx,offset TRANGROUP:TranDataEnd - 100H
	;
	;cld
	;shr	cx,1
	;xor	dx,dx
;Ichksum:
	;lodsw
	;add	dx,ax
	;adc	dx,0
	;loop	Ichksum
	;
        ;mov	Sum,dx			; store checksum

	; MSDOS 3.3
	pop     es ; *
	call    CHKSUM
	mov     [SUM],dx

	; MSDOS 3.3 & MSDOS 6.0
        cmp     byte [PRDATTM],0	;
        jnz     short NOBATCHSEG	; don't do autoexec or date time
;
; Allocate batch segment for d:/autoexec.bat + no arguments
;
	; BATCHSEGMENT.SIZE = 32 for MSDOS 3.3 (= 33 for MSDOS 6.0)
	;mov	bx,3
	mov	bx,(BATCHSEGMENT.SIZE+15+1+0Fh)/16 ; (32+15+1+0Fh)/16 = 3
        mov     ah,ALLOC ; 48h		;
	int     21h	; DOS - 2+ - ALLOCATE MEMORY
			; BX = number of 16-byte paragraphs desired
	jc      short NOBATCHSEG	; didn't allocate - pretend no batch
        mov     [BATCH],ax		; save batch segment

NOBATCHSEG:
	; MSDOS 6.0
	;mov	bx,EnvirSeg                     ; get old environment segment
	;mov	OldEnv,bx                       ; save it
	;
        ;mov	UsedEnv,0                       ; initialize env size counter
	;mov	ds,bx
	;assume	ds:nothing
	;
	;xor	si,si
	;mov	di,si
;
; This is the maximum allowed size for the environment
;
	;mov	bx,4096 - 1 			; max. allowed env. size
	;mov	EnvMax,bx
				 
	;shl	bx,1
	;shl	bx,1
	;shl	bx,1
	;shl	bx,1
	;mov	EnvMax, bx			; convert envmax to bytes
	;dec	bx				; dec by one to leave room for double 0
	;xor	dx,dx				; use dx to indicate that there was
						; no environment size error.
;public NxtStr
;NxtStr:
	;call	GetStrLen			; get the size of the current env string

;Bugbug: Can use ss here to address UsedEnv

	;push	ds                              ; get addressability to environment
        ;push	cs                              ;                       counter
        ;pop	ds                              ;
        ;assume	ds:ResGroup
        ;add	UsedEnv,cx                      ; add the string length to env size
        ;pop	ds                              ;
	;assume	ds:nothing
	;cmp	cx,1				; end of environment was encountered.
	;jz	EnvExit
	;sub	bx,cx
	;jae	OkCpyStr			; can't fit in all of enviroment.
	;inc	dx				; out of env space msg must be displayed
	;jmp	short EnvExit
;OkCpyStr:
	;jmp	NxtStr
;EnvExit:
	;push	cs
	;pop	ds
	;assume	ds:ResGroup
	;or	dx,dx				; dx will be non-zero if error
	;jz	EnvNoErr
	;mov	dx,offset ResGroup:OutEnvMsg	; dx = ptr to msg
	;call 	RPrint
	;
;EnvNoErr:
	;mov	ax,EnvSiz			;env size previously set
	;mov	cl,4
	;shl	ax,cl				;get size in bytes
	;cmp	ax,UsedEnv			;is it a new env?
	;ja	st_envsize			;yes, store the size
	;
	;mov	ax,UsedEnv
	;add	ax,15				;round up
;st_envsize:	
	;shr	ax,cl
	;mov	EnvSiz,ax			;store env size needed(paras)
;if MSVER
	;cmp	SingleCom,0
	;jnz	nophead 			; don't print header if singlecom
	;mov	dx,offset ResGroup:CopyrightMsg	; dx = ptr to msg
	;call	RPrint
;nophead:
;endif

	; MSDOS 3.3
	mov     bx,0FFFFh ; 65535
	mov     ah,ALLOC ; 48h
	int     21h	; DOS - 2+ - ALLOCATE MEMORY
			; BX = number of 16-byte paragraphs desired
	sub     bx,[TRNSIZE]
	sub     bx,128
	mov     [ENVMAX],bx

	cmp     bx,4096
	jb      short ALLOCENVIRSEG
	mov     bx,4096-1		; max. allowed environment size
	mov     [ENVMAX],bx

ALLOCENVIRSEG:
	mov     ah,ALLOC ; 48h
	int     21h	; DOS - 2+ - ALLOCATE MEMORY
			; BX = number of 16-byte paragraphs desired
	mov     bx,[ENVIRSEG]		; get old environment segment
	mov     [OLDENV],bx		; save it	
	mov     word [USEDENV],0	; initialize environment size counter
	mov     ds,bx
	mov     [ss:ENVIRSEG], ax	; save new environment segment
	mov     es,ax
	xor     si,si
	mov     di,si
	mov     bx,[ss:ENVMAX]
	shl     bx,1
	shl     bx,1
	shl     bx,1
	shl     bx,1
	mov     [ss:ENVMAX],bx		; convert envmax to bytes
	dec     bx			; dec by one to leave room for double 0
	xor     dx,dx			; use dx to indicate that there was
					; no environment size error.

NXSTR:
	call    GETSTRLEN		; get the size of the current env string

	push    ds
	push    cs
	pop     ds
	add     [USEDENV],cx		; add the string length to env size
	pop     ds
	cmp     cx,1			; end of environment was encountered.
	je      short ENVEXIT
	sub     bx,cx
	jnb     short OKCPYSTR		; can't fit in all of enviroment.
	inc     dx			; out of env space msg must be displayed
	jmp     short ENVEXIT

	;nop

OKCPYSTR:
	jmp     short NXSTR

ENVEXIT: 
	push    cs
	pop     ds
	or      dx, dx			; dx will be non-zero if error
	jz      short ENVNOERR
	mov     dx,OUTENVMSG		; dx = ptr to msg
	call    RPRINT

ENVNOERR:
	mov     cx,[ENVMAX]
	sub     cx,bx			; current environment size in bytes
	add     cx,16			; add memory arena to the size
	shr     cx,1
	shr     cx,1
	shr     cx,1
	shr     cx,1			; convert current env size to paragraphs
	cmp     cx,[ENVSIZ]		; compare with env size previously set
	;jb	short SET_ENVSIZE
	jna	short SET_ENVSIZE  ; 24/09/2018 - Retro DOS v3.0
	mov     [ENVSIZ],cx		; store env size needed (paragraphs)

SET_ENVSIZE:
	mov     bx,[ENVSIZ]
	mov     ax,es
	add     ax,bx			; get end segment of environemnt
	cmp     ax,[INITEND]		; compare with init code end segment
	ja      short NOPHEAD
					; free unused paragraghs		
	mov     ax,es
	mov     bx,[INITEND]
	sub     bx,ax
	;mov	byte [RESETENV],1  ; 24/09/2018
	mov	word [RESETENV],1	; environment segment reset sign

NOPHEAD:
	; MSDOS 3.3
	mov     ah,SETBLOCK ; 4Ah
	int     21h	; DOS - 2+ - ADJUST MEMORY BLOCK SIZE (SETBLOCK)
			; ES = segment address of block to change
			; BX = new size in paragraphs

	; MSDOS 3.3 & 6.0
	cmp     word [BATCH],0		; did we set up a batch segment?
	jnz     short DODATE		; yes - go initialize it
	jmp     NODTTM			; don't do autoexec or date time

DODATE:
;
; allocate batch segment for d:/autoexec.bat + no arguments
;

	mov	ax,[BATCH]		; get batch segment
	mov	byte [ECHOFLAG],3	; set batch echo
	mov	word [NEST],1		; set nest flag to 1 batch
	mov	es,ax
;
; initialize the segment
;
	xor	di,di
	mov	al,BATCHTYPE ; 0
	stosb
	mov	al,1			; initialize echo for batch exit
	stosb
;
; Hosebag! This guy does not use the struct fields to init the BatchSegment
;
	xor	ax,ax			; initialize to zero
	;stosb	; MSDOS 6.0 		; clear out BatchEOF

	stosw				; batch segment of last job - batlast
	stosw				; segment for FOR
	stosb				; FOR flag
	stosw				; position in file - batseek
	stosw
;
; clean out the parameters
;
	mov	ax,-1			; initialize to no parameters
	mov	cx,10
	rep	stosw
;
; decide whether we should grab the default drive
;
	cmp	byte [AUTOBAT],0 ; ":\AUTOEXEC.BAT"
	jnz	short NOAUTSET
	mov	ah,GET_DEFAULT_DRIVE ; 19h
	int	21h
	; 02/03/2023
	add	al,'A'
	;add	al,[letter_A] ; Ucasea
	mov	[AUTOBAT],al

NOAUTSET:
;
; copy in the batch file name (including nul)
;
	;mov	si,offset ResGroup:AutoBat
	mov	si,AUTOBAT
	mov	cx,8
	rep	movsw
	;movsb	; MSDOS 6.0		; move in carriage return to terminate string

	;mov	dx,offset ResGroup:AutoBat
	mov     dx,AUTOBAT ; ":\AUTOEXEC.BAT"

	;mov	ax,OPEN shl 8
	mov	ax,OPEN*256 ; 3D00h  ; open for read
	int	21h			; see if autoexec.bat exists
	jc	short NOABAT
	mov	bx,ax
	mov	ah,CLOSE  ; 3Eh
	int	21h
	jmp	short DRV0		; go process autoexec
NOABAT:
	push	ax
	call	SETUP_SEG
	mov	[TRIAGEADD+2],ax
	pop	ax
	call	far [TRIAGEADD]		; get extended error
	cmp	ax,65			; network access denied?
	jne	short OPENERR		; no - go deallocate batch

_ACCDENERROR:					; yes - put out message
	;mov	dx,offset ResGroup:AccDen	; dx = ptr to msg
	mov	dx,ACCDENERR
	call	RPRINT
OPENERR:
	mov	es,[BATCH]		; not found--turn off batch job
	mov	ah,DEALLOC ; 49h
	int	21h
	mov	word [BATCH],0		; after dealloc in case of ^c
	mov	byte [ECHOFLAG],1
	mov	word [NEST],0		; indicate no batch in progress

DODTTM:
	;mov	ax,offset TranGroup:Datinit

	mov	ax,DATINIT
	mov	[INITADD],ax

	; MSDOS 6.0
;;;M004	mov	ax,TrnSeg	
;
; M004; We cant use TrnSeg now because it is not initialized. We now that
; M004; the transient starts on a para boundary at the label TranStart.
; M004; We use TranStart to get the start of the transient segment.
;
	;mov	ax,offset RESGROUP:TranStart	; M004
	;mov	cl,4				; M004
	;shr	ax,cl				; get relative seg ; M004
	;mov	cx,cs
	;add	ax,cx				; ax = transient seg ; M004

	; MSDOS 3.3
	; 25/09/2018
	mov     ax,[TRNSEG]	; COMMAND.COM (MSDOS 3.3) - Offset 1387h

	; MSDOS 3.3 & MSDOS 6.0
	mov	[INITADD+2],ax
	;call	dword ptr InitAdd
	call	far [INITADD]
NODTTM:
	; MSDOS 6.0
Copyright:
	;public	Copyright
;	Bugbug:	remove Copyright label.

;if IBMVER
	;cmp	SingleCom,0
	;jnz	Drv0				; don't print header if singlecom
	;mov	dx,offset ResGroup:CopyrightMsg	; dx = ptr to msg
	;call	RPrint
;endif

	; MSDOS 3.3
	cmp	word [SINGLECOM],0	; don't print header if singlecom
	jnz     short DRV0
	mov     dx,HEADERPTR	; dx = ptr to msg
	call    RPRINT
DRV0:
	; MSDOS 3.3
        mov     byte [INITFLAG],0
	jmp	ENDINIT

	; MSDOS 6.0
;Drv0:						; Reset APPEND state
;	push	ds				; save data segment
;	push	cs				; Get local segment into DS
;	pop	ds				;
;	mov	ax,APPENDSETSTATE		; Set the state of Append
;	mov	bx,Append_State 		;  back to the original state
;	int	2fh				;
;	pop	ds				; get data segment back
;;
;;Check FirstCom set previously to see if this is the first instance of
;;command.com. If not, we do not move command.com. Instead, we copy over the
;;jump table from the previous stub to the current stub.
;;
;	cmp	FirstCom,1			;first command.com?
;	jz	move_code			;yes, move it
;
;	push	es
;	push	ds
;
;	push	ds
;	pop	es
;	mov	di,offset DATARES:Int2f_Entry
;
;	mov	ds,word ptr es:ResJmpTable+2	;get segment address
;	mov	si,word ptr es:ResJmpTable	;get offset address
;
;	mov 	cx,NUM_RELOC_ENTRIES 		;number of dword ptrs
;	shl	cx,1
;	shl	cx,1				;size of table in bytes
;
;	cld
;	rep	movsb				;copy the jump table
;;
;;Check if the resident code is in HMA. We assume that it is in HMA if its 
;;code segment > 0f000h. If in HMA, we set the ComInHMA flag
;;
;	cmp	es:[di-2],0f000h		;is resident code in HMA?
;	jb	res_low				;no, dont set flag
;
;	mov	es:ComInHMA,1			;indicate code in HMA
;
;res_low:
;	pop	ds
;	pop	es
;	jmp	short finish_init
;;
;;Now, we can move the resident code to its final location, either to HIMEM
;;or to overlay the messages in the data segment if the user has not used the
;;/msg switch.
;;
;move_code:
;	call	Move_res_code			;move the code
;
;finish_init:
;	jmp	RESGROUP:EndInit 		;g finish initializing


GETSTRLEN:
;
;	Get length of string pointed to by DS:SI.  Length includes NULL.
;	Length is returned in CX
;
	; MSDOS 3.3 & MSDOS 6.0
	xor	cx,cx
NXTCHAR:
	lodsb
	inc	cx
	or	al,al
	jnz	short NXTCHAR
	retn

SETUP_SEG:
;
; If the transient has been loaded in TranSeg, then we need to use that
; segment for calls to routines in the transient area. Otherwise, the current
; code segment is used
; Segment returned in AX.
;
	; MSDOS 3.3 & MSDOS 6.0
	mov	ax,[TRNSEG]
	cmp	byte [TRNMVFLG],1	; Has transient portion been moved
	je	short SETUP_END
	push	bx
	mov	bx,cs
	;mov	ax,offset ResGroup:TranStart
	mov	ax,TRANSTART
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	add	ax,bx
	pop	bx
SETUP_END:
	retn

RPRINT:
	; MSDOS 3.3
	push    ax
	call    SETUP_SEG
	mov     [PRINTADD+2], ax
	;call	dword ptr PRINTADD
	call	far [PRINTADD]
	pop     ax
	retn

	; MSDOS 6.0
;***	RPrintParse - display parse error message
;
;	ENTRY	DX = parse error #
;
;	EXIT	nothing
;
;	USED	flags
;
;	EFFECTS
;	  Message is displayed on stdout.
;
;RPrintParse	proc
;
;	assume	ds:ResGroup,ss:ResGroup
;
;	push	dx				; preserve DX
;	xchg	bx,dx				; bx = parse error #
;						; dx = saved BX
;	dec	bx				; bx = parse error index, from 0
;	shl	bx,1				; bx = offset in word table
;	mov	bx,ParsMsgPtrs[bx]		; bx = ptr to error msg
;	xchg	bx,dx				; dx = ptr to error msg
;						; bx = restored
;	call	RPrint				; print the message
;	pop	dx				; restore DX
;	ret
;
;RPrintParse	endp

PATHCHRCMPR:
	; MSDOS 3.3
	; 02/03/2023
	;push	dx
	;mov	dl,[slash_chr] ; '/'
	cmp	byte [RSWITCHAR],'/'
	;cmp	[RSWITCHAR],dl
	je	short RNOSLASHT
	cmp	al,'/'
	;cmp	al,dl
	je	short RET41 ; zf = 1 
RNOSLASHT:
        cmp	al,'\'
	;cmp	al,[bslash_chr]
RET41:
	;pop	dx
	retn

IFINDE:
	; MSDOS 3.3 & MSDOS 6.0
	call	IFIND				; find the name
	jc	short IFIND2			; carry means not found
	jmp	short ISCASB1 			; scan for = sign
;
; on return of find1, es:di points to beginning of name
;
IFIND:
	cld
	call	ICOUNT0 			; cx = length of name
	mov	es,[ENVIRSEG]
	xor	di,di
IFIND1:
	push	cx
	push	si
	push	di
IFIND11:
	lodsb
	call	IUPCONV
	inc	di
	cmp	al,[es:di-1]
	jnz	short IFIND12
	loop	IFIND11
IFIND12:
	pop	di
	pop	si
	pop	cx
	jz	short IFIND2
	push	cx
	call	ISCASB2 			; scan for a nul
	pop	cx
	cmp	byte [es:di],0
	jnz	short IFIND1
	stc					; indicate not found
IFIND2:
	retn

ICOUNT0:
	push	ds
	pop	es
	mov	di,si

	push	di				; count number of chars until "="
	call	ISCASB1
	; 25/09/2018
	;jmp	short ICOUNTX
	;push	di				; count number of chars until nul
	;call	ISCASB2
;ICOUNTX:
	pop	cx
	sub	di,cx
	xchg	di,cx
	retn

ISCASB1:
	; 02/03/2023
	mov	al,"="
	;mov	al,[equal_sign]			; scan for an =
	jmp	short ISCASBX
ISCASB2:
	xor	al,al				; scan for a nul
ISCASBX:
	mov	cx,256 ; 100h
	repnz	scasb
	retn

IUPCONV:
	; MSDOS 3.3
	cmp	al,"a"
	; 02/03/2023
	;cmp	al,[letter_a]
	jb	short IRET22
	cmp	al,"z"
	;cmp	al,[letter_z]
	ja	short IRET22
	sub	al,20h			; Lower-case changed to upper-case
IRET22:
        retn

	; MSDOS 6.0
; ****************************************************************
; *
; * ROUTINE:	 IUPCONV    (ADDED BY EMG 4.00)
; *
; * FUNCTION:	 This routine returns the upper case equivalent of
; *		 the character in AL from the file upper case table
; *		 in DOS if character if above  ascii 128, else
; *		 subtracts 20H if between "a" and "z".
; *
; * INPUT:	 DS	      set to resident
; *		 AL	      char to be upper cased
; *		 FUCASE_ADDR  set to the file upper case table
; *
; * OUTPUT:	 AL	      upper cased character
; *
; ****************************************************************
;
;IupConv proc	near				
;	assume	ds:ResGroup			;
;
;	cmp	al,80h				; see if char is > ascii 128
;	jb	other_fucase			; no - upper case math
;	sub	al,80h				; only upper 128 chars in table
;	push	ds				;
;	push	bx				;
;	lds	bx,dword ptr fucase_addr+1		; get table address
;	add	bx,2				; skip over first word
;	xlat	ds:byte ptr [bx]			; convert to upper case
;	pop	bx				;
;	pop	ds				;
;	jmp	short iupconv_end			; we finished - exit
;
;other_fucase:					;
;	cmp	al,Lcasea			; if between "a" and "z",
;	jb	iupconv_end			;     subtract 20h to get
;	cmp	al,Lcasez			; upper case equivalent.
;	ja	iupconv_end			;
;	sub	al,20h				; Change lower-case to upper
;
;iupconv_end:					;
;	ret
;
;IupConv endp					;


INIT_CONTC_SPECIALCASE:
	; MSDOS 3.3 & MSDOS 6.0
						; This routine is called if control-C
	add	sp,6				;  is type during the date/time prompt
	push	si				;  at initialization time.  The desired
	mov	si,dx				;  response is to make it look like the
	mov	word [si+1],0D00h		;  user typed <CR> by "popping" the
	pop	si				;  INT 21h stuff off the stack, putting
	iret					;  a <CR> in the user's buffer, and
						;  returning directly to the user.
						; In this case the user is TCODE.

;=============================================================================
; UNINIT.ASM, MSDOS 6.0 ,1991
;=============================================================================
; 24/09/2018 - Retro DOS v3.0

; (30/04/2018 - Retro DOS v2.0, MSDOS 2.11 COMMAND.COM)

; TITLE	COMMAND Initialization messages

;INIT	SEGMENT PUBLIC PARA

	; 25/09/2018
	; (15 bytes filler)
	db 0
	;db "25/9/2018 ETAN"
	db "02/03/2023 ETAN" ; 02/03/2023	
	db 0

	; MSDOS 3.3 COMMAND.COM - offset 145Eh
	;dw 0
COPYRIGHTMSG:	; MSDOS 3.3 COMMAND.COM - offset 1460h
	db 0Dh,0Ah
	db 0Dh,0Ah
	db 'Microsoft(R) MS-DOS(R)  Version 3.30'
	db 0Dh,0Ah
	db '             (C)Copyright Microsoft Corp 1981-1987               '
	db ' ',0Dh,0Ah
	db '                                                   ',
	db 0Dh,0Ah,0

	times	43 db 20h

_152Fh:	db 'Specified COMMAND search directory bad',0Dh,0Ah,0
BADCOMLKMES:
	dw _152Fh

_155Ah:	db 'Specified COMMAND search directory bad access denied',0Dh,0Ah,0
BADCOMACCMSG:
	dw _155Ah

_1593h:	db 'Access denied',0Dh,0Ah,0
ACCDENERR:
	dw _1593h

_15A5h:	db 'Out of environment space',0Dh,0Ah,0
OUTENVMSG:
	dw _15A5h

BADVERMSG:
	db 'Incorrect DOS version',0Dh,0Ah,'$'

BADENVSIZMSG:
	db 'Invalid environment size specified',0Dh,0Ah,'$'

HEADERPTR:
	dw COPYRIGHTMSG

ICONDEV:
        db '/DEV/'
	db 'CON'
	db 0,0,0,0,0,0		; Room for 8 char device	
BADCSPFL:
	db 0
COMSPECT:
	db '/COMMAND.COM',0
	db 0
AUTOBAT:
	db 0,':\AUTOEXEC.BAT',0 
PRDATTM:
	db -1 ; 0FFh		 ; Init not to prompt for date time
INITADD:
	dd 0
	;dw 0
	;dw 0
PRINTADD:
	dw PRINTF_INIT
	dw 0
TRIAGEADD:
	dw TRIAGE_INIT
	dw 0
CHUCKENV:
	db 0
COMSPOFFSET:
ECOMLOC:
	;dw 0Eh
	;dw offset ENVIRONMENT:ECOMSPEC-10h
	dw ECOMSPEC-ENVIRONMENT ; 30/04/2018	
COMSPSTRING:
	db 'COMSPEC='

; 02/03/2023
;equal_sign:
;	db '='
;letter_a:
;	db 'a'
;letter_z:
;	db 'z'
;slash_chr:
;	db '/'
;bslash_chr:
;	db '\'
;space_chr:
;	db 20h
;letter_p:
;	db 'p'
;letter_d:
;	db 'd'
;letter_c:
;	db 'c'
;letter_A:
;	db 'A'
ENVSIZ:
	dw 0
ENVMAX:
	dw 0
OLDENV:
	dw 0
USEDENV:
	dw 0
INITEND:
	dw 0
TRNSIZE:
	dw 0
RESETENV:
	dw 0

;INIT	ENDS

;	END

;-----------------------------------------------------------------------------
; 14/10/2018 (Retro DOS v3.0 COMMAND.COM Signature)
;-----------------------------------------------------------------------------

;db	"Retro DOS v3.0 COMMAND.COM by Erdogan Tan [2018]"
; 12/01/2023
db	"Retro DOS v3.0 COMMAND.COM by Erdogan Tan [2018-2023]"
db	0

;-----------------------------------------------------------------------------
; 24/09/2018 (Retro DOS v3.0 COMMAND)
;-----------------------------------------------------------------------------

;TAIL    SEGMENT PUBLIC PARA
;        ORG     0
;TRANSTART       LABEL   WORD
;TAIL    ENDS

ALIGN 16  ; 25/09/2018

;TRANSTART:

; 21/04/2018 (Retro DOS v2.0 COMMAND)
;	times	128 db 0	

;-----------------------------------------------------------------------------
; SEGMENT - TRANSCODE
;-----------------------------------------------------------------------------

TRANGROUP: ; 21/04/2018

;=============================================================================
; TRANCODE.ASM
;=============================================================================
; 24/09/2018 - Retro DOS v3.0

; MSDOS 3.3 COMMAND.COM Transient Portion Addresses

; 21/04/2018 - Retro DOS v2.0
; transcom.s (COMMAND.COM source file 2 of 2) code/data addresses 
; (these values must be changed when transcom.s source code is changed
; and data offsets are changed)
;
; 30/04/2018
; 29/04/2018

; 24/09/2018 (original MSDOS 3.3 COMMAND.COM TRNSEG offset addresses)
;COMMAND      EQU  012CH
;DATINIT      EQU  2091H
;HEADCALL     EQU  428FH
;TRANSPACEEND EQU  4D5CH
;TRANDATAEND  EQU  3F44H

; 29/04/2018 (original MSDOS 3.3 COMMAND.COM TRNSEG offset addresses)
;TRIAGE_INIT  EQU  1F15H
;PRINTF_INIT  EQU  34E0H 

;GETEXTERRNUM EQU  1EEEH  ; TRIAGEERROR (GET_EXT_ERR_NUMBER) proc addr	

;TPA	EQU  4293H
;TRNLEN	EQU  04D6H

; 20/10/2018 - Retro DOS v3.0 COMMAND.COM transient portion addresses
COMMAND      EQU  012CH
;GETEXTERRNUM EQU 1ECCH ; TRIAGEERROR (GET_EXT_ERR_NUMBER) proc addr
;TRIAGE_INIT EQU  1EF3H
;DATINIT     EQU  206FH
; 02/03/2023
GETEXTERRNUM EQU  1E79H
TRIAGE_INIT  EQU  1EA0H
DATINIT	     EQU  2000H		
PRINTF_INIT  EQU  3415H
TRANDATAEND  EQU  3E35H
HEADCALL     EQU  417FH
TRANSPACEEND EQU  4C4CH

;-----------------------------------------------------------------------------
;START OF TRANSIENT PORTION
;This code is loaded at the end of memory and may be overwritten by
;memory-intensive user programs.
;-----------------------------------------------------------------------------

;TRANSTART EQU $
; 29/09/2018 
TRANSTART:		; Offset 1660h in original MSDOS 3.3 COMMAND.COM

; 25/09/2018
; (original MSDOS 3.3 COMMAND.COM TRIAGEERROR offset address)
;
; 'GET_EXT_ERR_NUMBER' ('TRIAGEERROR') procedure is at offset 354Eh 
; in MSDOS 3.3 COMMAND.COM (It is at offset 1EEEh in transient porsion).	 	
;
TRIAGEERROR EQU TRANSTART+GETEXTERRNUM-100H
;

;COMTRANS:

; 20/10/2018 - Retro DOS v3.0	
INCBIN	"TRANCOM3.BIN"

;COMLEN	EQU $-COMTRANS	; End of COMMAND load.

; 29/04/2018
;BSS_SIZE EQU TRANSPACEEND-TRANDATAEND	
;
;TIMES BSS_SIZE db 0
;
;COMLEN	EQU $-COMTRANS	; 30/04/2018

;COMMANDCOMSIZE equ $ - 100h