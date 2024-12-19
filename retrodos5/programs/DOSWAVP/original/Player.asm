;;; ****
;;; TO-DO LIST
;;; - check if the wave header values are within supported range
;;; - check if WAVE_SubchunkSize contains more than the normal value (extra WAVE parameters (not supported))
;;; - Create a "SeekRIFF" proc for files with tags

IDEAL
P386
MODEL small
STACK 100h
DATASEG
;;; ***************************************************************************
;;; *** Template strings to draw ***
SplashScreen                db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                     _______   ______        _______.                     ", 221, 219, 222
                            db  221, 219, 222, "                    |       \ /  __  \      /       |                     ", 221, 219, 222
                            db  221, 219, 222, "                    |  .--.  |  |  |  |    |   (----`                     ", 221, 219, 222
                            db  221, 219, 222, "                    |  |  |  |  |  |  |     \   \                         ", 221, 219, 222
                            db  221, 219, 222, "                    |  '--'  |  `--'  | .----)   |                        ", 221, 219, 222
                            db  221, 219, 222, "                    |_______/ \______/  |_______/                         ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "     .______    __          ___   ____    ____  _______ .______           ", 221, 219, 222
                            db  221, 219, 222, "     |   _  \  |  |        /   \  \   \  /   / |   ____||   _  \          ", 221, 219, 222
                            db  221, 219, 222, "     |  |_)  | |  |       /  ^  \  \   \/   /  |  |__   |  |_)  |         ", 221, 219, 222
                            db  221, 219, 222, "     |   ___/  |  |      /  /_\  \  \_    _/   |   __|  |      /          ", 221, 219, 222
                            db  221, 219, 222, "     |  |      |  `----./  _____  \   |  |     |  |____ |  |\  \----.     ", 221, 219, 222
                            db  221, 219, 222, "     | _|      |_______/__/     \__\  |__|     |_______|| _| `._____|     ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                WELCOME TO                                ", 221, 219, 222
                            db  221, 219, 222, "                                DOS PLAYER                                ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  221, 219, 222, "                                                                          ", 221, 219, 222
                            db  "                                                                                         "

Template                    db    201, 78 dup(205), 187
                            db    186, 33 dup(219), " DOS Player ", 33 dup(219), 186
                            db    204, 78 dup(205), 185
                            db    186, 33 dup(32), " User Guide ", 33 dup(32), 186
                            db    186, 6    dup(32), "<Space>         Play/Pause    ", 4 dup(32), "<N>             Next File    ", 9 dup(32), 186
                            db    186, 6    dup(32), "<S>             Stop          ", 4 dup(32), "<P>             Previous File", 9 dup(32), 186
                            db    186, 6    dup(32), "<F>             Forwards      ", 4 dup(32), "<I>/<D>         Inc/dec volume", 8 dup(32), 186
                            db    186, 6    dup(32), "<B>             Backwards     ", 4 dup(32), "<Q>             Quit Program ", 9 dup(32), 186
                            db    204, 78 dup(205), 185
                            db    186, 6    dup(32), "File Name :                   ", 4 dup(32), "Bit-Rate  :     0  Bits      ", 9 dup(32), 186
                            db    186, 6    dup(32), "Frequency :     0     Hz      ", 4 dup(32), "#-Channels:     0            ", 9 dup(32), 186
                            db    200, 78 dup(205), 188
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(32)
                            db    80 dup(205)
                            db    80 dup(32)
                            db    33 dup(32), "00:00 ", 174, 175, " 00:00", 24 dup(32), "VOL 000%"

;;; ***************************************************************************
;;; *** Pre-set strings ***
string_help                    db    "Welcome to DosPlayer!", 10, 10, 13
                            db    "The correct usage of the program:", 10, 13, 09
                            db    "PLAYER.EXE <FileName1> <FileName2> <...>", 10, 10, 10, 13
                            db    "Created by Matan Alfasi, Ami-Asaf.", 13, 10, "$"

    
;;; "An error occurred" strings
string_filehandle_error        db     "ERROR: Could not retrieve file handle.", "$"
string_loaddata_error        db     "ERROR: Could not load data (EOF?)", "$"

string_riffheader_invalid    db     "ERROR: The *RIFF* header is incorrect (invalid RIFF/WAVE file?).", "$"
string_waveheader_invalid    db     "ERROR: The *WAVE* header is incorrect (invalid RIFF/WAVE file?).", "$"

;;; ***************************************************************************
;;; *** Allocate enough memory for the current played file info ***
CurrentFileName     db    "splash.WAV", 7 dup(0), "$"        ;; Current file name
CurrentFileHandle    dw    0            ;; Current file handle
CurrentTotalTime    dw    0            ;; Total file time length 
CurrentTimePassed    dw    0            ;; Total time that has passed
TotalTransfers        dw    2            ;; Total transfers, always starts at 2

TransferLength        dw    44100        ;; Default amount of bytes to read/transfer
SubBufferLength        dw    22050        ;; Default amount of bytes to read per sub buffer
LastTransfer        db    0            ;; Is it currently the last transfer?
CurrentLoadBuffer    db    1            ;; First load to the first buffer (Either 1/2)

IsPaused            db    0            ;; Is the program in pause mode?
IsInSplash			db	1

CurrentVolume        db    05h            ;; Keep track on the current volume


PSP_Segment            dw    0            ;; The PSP segment
PSP_CurrentOffset    dw    0            ;; The current PSP offset

Visualization_SoundWave        db    5, 5, 4, 4, 3, 3, 2, 2, 1, 1
                            db    1, 1, 2, 2, 3, 3, 4, 4, 5, 5
                            db    6, 6, 7, 7, 8, 8, 9, 9, 10, 10
                            db    10, 10, 9, 9, 8, 8, 7, 7, 6, 6
Visualization_CurrentLoc    db    0
Visualization_LastTimer        dw    0



;;; ***************************************************************************
;;; *** File load starts here ***
;;; ***
;;; *** RIFF header parameters (A.K.A "Chunk") ***
RIFF_HEADER_SIZE    equ    12

RIFF_ChunkID        dd     0            ;; Must be equal to "RIFF" - big-endian
                                    ;;                     0x52494646
RIFF_ChunkSize        dd    0            ;; Represents total file size, not 
                                    ;; including the first 2 fields 
                                    ;; (Total_File_Size - 8), little-endian
RIFF_Format            dd    0            ;; Must be equal to "WAVE" - big-endian
                                    ;;                     0x57415645

;;; *** WAVE header parameters (A.K.A "Sub-chunk") ***
WAVE_HEADER_SIZE    equ    24
WAVE_SubchunkID        dd    0            ;; Must be equal to "fmt " - big-endian
                                    ;;                     0x666d7420
WAVE_SubchunkSize    dd    0            ;; Represents total chunk size
WAVE_AudioFormat    dw    0            ;; PCM (Raw) - is "1", other - is a form 
                                    ;; of compression, not supported.
WAVE_NumChannels    dw    0            ;; Number of channels, Mono-1, Stereo-2
WAVE_SampleRate        dd    0            ;; Frequency rate, in Hz (8000, 44100 ...)
WAVE_ByteRate        dd    0            ;; SampleRate * NumChannels * BytesPerSample
WAVE_BlockAlign        dw    0            ;; NumChannels * BytesPerSample
                                    ;; Number of bytes for one sample.
WAVE_BitsPerSample    dw    0            ;; 8 = 8 bits, 16 = 16 bits, etc.

;;; *** DATA header parameters & the data itself ***
DATA_HEADER_SIZE    equ 8

DATA_SubchunkID        dd    0            ;; Must be equal to "data" - big-endian
                                    ;;                     0x64617461
DATA_SubchunkSize    dd    0            ;; NumSamples * NumChannels * BytesPerSample
                                    ;; Number of bytes in the data.

DATA                db 44100 dup(80h) ;; Actual sound data.

CODESEG

;;; ***************************************************************************
;;; *** Create the PSP / .COM file 'must-have'                              ***
;;; ***************************************************************************

;; .com file 'must'
org 100h

;;; ***************************************************************************
;;; *** Print strings and numbers to STDOUT                                 ***
;;; ***************************************************************************

;;; Include general string print procedures
include "DOSP/OUT_CS.asm"


;;; ***************************************************************************
;;; *** General file handling procedures start here.                        ***
;;; ***************************************************************************

;;; Include general file handling proc s to get file handles and load data
include "DOSP/FILE_CS.asm"


;;; ***************************************************************************
;;; *** RIFF/WAVE file format related procedures start here                 ***
;;; ***************************************************************************

;;; All RIFF/WAVE realted procedures are included withing this ASM file
;;; which provides format validation and data load handlation
include "DOSP/WAVE_CS.asm"


;;; ***************************************************************************
;;; *** SB16 controling, writing, reading and handling                      ***
;;; *** procedures start here.                                              ***
;;; ***************************************************************************

;;; Include all SoundBlaster (sb16) related procedures and constants
include "DOSP/SB_CS.asm"


;;; ***************************************************************************
;;; *** PSP related proc s                                                  ***
;;; ***************************************************************************

;;; Include all PSP related procedures in order to correctly read and parse
;;; command line arguments/parameters
include "DOSP/PSP_CS.asm"


;;; ***************************************************************************
;;; *** Graphic/Text modes related proc s.                                  ***
;;; ***************************************************************************

;;; Include all graphic/text mode related procedures in order to present
;;; the user interface
include "DOSP/GFX_CS.asm"


;;; ***************************************************************************
;;; *** Main program execution starts here.                                 ***
;;; ***************************************************************************

player:
    ;; DS = ES = @DATA
    push @data
    push @data
    pop ds
    pop es

Player_InitalizePSP:
    ;; Init PSP
    call InitPSP    
    jnc Player_InitalizeGFX
    
Player_InitPSPError:
    ;; Error, no command line arguments, print valid usage
    push offset string_help
    call PrintString
    jmp exit

Player_InitalizeGFX:
    ;; Init ASCII/Text mode
    call initTextMode
	
Player_SplashScreen:
	push 0
	push 0
	call setCursorPosition
	
    ;; Print the splash screen in white
    push es
    push bp

    mov ax, 1300h
    mov bx, 000Fh
    mov cx, 1999
    mov dx, 0

    push ds
    pop es
	
    mov bp, offset SplashScreen
    int 10h

    pop bp
    pop es
	
	jmp Player_ResetSettings
	
Player_Template:
	push 0
	push 0
	call setCursorPosition
	
	    ;; Print the splash screen in white
    push es
    push bp

    mov ax, 1300h
    mov bx, 000Fh
    mov cx, 1999
    mov dx, 0

    push ds
    pop es
	
    mov bp, offset Template
    int 10h

    pop bp
    pop es
	
Player_ParseNextParameter:
    clc
    push [word ptr PSP_Segment]
    push [word ptr PSP_CurrentOffset]
    call PSPParseNext
    jnc Player_ResetSettings
    jmp Player_ParsePSPError

Player_ParsePreviousParameter:
    push [word ptr PSP_Segment]
    push [word ptr PSP_CurrentOffset]
    call PSPParsePrev
    jnc Player_ResetSettings

Player_ParsePSPError:
    jmp Player_Quit

Player_ResetSettings:
    ;; Initalize the sound card to its defaults settings
    call ResetDSP

    ;; Reset volume to middle
    mov [byte ptr CurrentVolume], 05h

    ;; First transfer is (almost) alwats not last transfer
    mov [byte ptr LastTransfer], 0h

    ;; Reset total transfers to 2
    ;; Used to calculate time, always starts with initial 2 transfers
    mov [word ptr TotalTransfers], 02h

    ;; Reset "paused" functionality
    mov [byte ptr IsPaused], 0h

    ;; Set the last-check clock for the visualization
    mov ah, 00h
    int 1Ah
    mov [word ptr Visualization_LastTimer], dx

Player_getFileHandle:
    call getFileHandleByPath
        ;; Check for error
    jnc Player_loadWAVEHeaders
    
    call ClearScreen
    push offset string_filehandle_error
    call printString

    push offset CurrentFileName
    call printString

    jmp exit
    
Player_loadWAVEHeaders:
    call loadWAVEHeaders
    jz Player_loadDataHeader

Player_loadWAVEHeaders_Error:
    jmp exit

Player_loadDataHeader:
    push [word ptr CurrentFileHandle]
    push DATA_HEADER_SIZE
    push offset DATA_SubchunkID
    call loadData

    or ax, ax
    jnz Player_PrintFileInfo
    
    call ClearScreen
    push offset string_loaddata_error
    call printString
    jmp exit
        
Player_PrintFileInfo:
	;; Don't print if in Splash Screen
	mov al, [byte ptr IsInSplash]
	or al, al
	jnz Player_SetMasterVolume
	
    call SetTotalTime
    call UpdateFileInfo

Player_SetMasterVolume:
    ;; Set default volume
    mov al, [byte ptr CurrentVolume]
    xor ah, ah
    push ax
    call SetMasterVol

Player_SetTrasnferProperties:
    ;; TransferLength = SampleRate
    mov ax, [word ptr WAVE_SampleRate]
    mov [word ptr TransferLength], ax

    shr ax, 01h
    mov [word ptr SubBufferLength], ax

Player_InitialDataLoad:
    ;; Load initial data to both buffers
    push [word ptr CurrentFileHandle]
    push [word ptr TransferLength]
    push offset DATA
    call loadData

Player_ProgramDMA:
    ;; Program the DMA to the correct and desired values, start transfer
    push [word ptr WAVE_BitsPerSample]    ;; Send bps
    push [word ptr TransferLength]        ;; Send transfer length
    push offset DATA                    ;; Send buffer's offset
    call ProgramDMA

Player_ProgramDSP:
    push [word ptr SubBufferLength]        ;; Send transfer length
    push [word ptr WAVE_BitsPerSample]    ;; Send bps
    push [word ptr WAVE_SampleRate]        ;; Send frequency
    push [word ptr WAVE_NumChannels]    ;; Send # of channels
    call ProgramDSP

    jmp Player_MainLoop

Player_LoadBufferDataLoop:
    ;; Load [SubBufferLength] bytes 
    call loadBufferData

    ;; Increase the amount of transfers
    inc [word ptr TotalTransfers]

Player_UpdateProgressBar:
	;; Don't print if in Splash Screen
	mov bl, [byte ptr IsInSplash]
	or bl, bl
	jnz Player_CheckLastTransfer
	
    call UpdateProgressBar

Player_CheckLastTransfer:
    mov [word ptr TransferLength], ax

    ;; AX != SubBufferLength: LastTrasnfer.
    cmp ax, [word ptr SubBufferLength]    
    je Player_MainLoop

    ;; AX==0: EOF
    or ax, ax
    jz Player_CheckLastTransfer

    ;; Set as last transfer
    mov [byte ptr LastTransfer], 1

    jmp Player_MainLoop

Player_CheckLastTransfer_EOF:
    jmp Player_CloseFile

Player_MainLoop:
    ;; Most of the prgoram execution is here
    ;; Loop through screen-info updates, keyboard process
    ;; and check for hardware interrupts
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Don't print if in Splash Screen
	mov al, [byte ptr IsInSplash]
	or al, al
	jnz Player_DSPInterrupt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
    mov ah, 00h
    int 1Ah

    ;; If paused, do not update the visualization
    mov al, [byte ptr IsPaused]
    test al, 00000001b
    jnz Player_UpdateTimeCounter

    ;; If 1/18.22 seconds haven't passed yet
    ;; do not update
    mov ax, dx
    sub ax, [word ptr Visualization_LastTimer]
    cmp ax, 5
    jb Player_UpdateTimeCounter

    mov [word ptr Visualization_LastTimer], dx

    call UpdateSoundVisualization

Player_UpdateTimeCounter:
    ;; Update the time counter
    call UpdateTime

Player_KeyboardInterrupt:
    ;; Read key in "Type-ahead buffer"
    mov ah, 01h
    int 16h

    ;; If ZF is set, then no key in buffer
    jz Player_DSPInterrupt

Player_GetKey:
    ;; Clear the pressed key out of the buffer
    xor ah, ah
    int 16h

    ;; Make sure its a capital letter
    cmp al, 'Z'
    jb Player_ProcessKey

    ;; (Non-capital Letter) - (32) = (Capital Letter)
    sub al, 32
    
Player_ProcessKey:
    ;; B = Backwards
    cmp al, "B"
    je Player_ProcessKey_Backwards

    ;; F = Forwards
    cmp al, "F"
    je Player_ProcessKey_Forwards

    ;; Space = pause/resume
    cmp ah, 39h
    je Player_ProcessKey_Pause

    ;; I = Increase volume
    cmp al, "I"
    je Player_ProcessKey_IncreaseVolume

    ;; D = Decrease volume
    cmp al, "D"
    je Player_ProcessKey_DecreaseVolume

    ;; S = Stop
    cmp al, "S"
    je Player_ProcessKey_Stop

    ;; N = Next file
    cmp al, "N"
    je Player_CloseFile

    ;; P = Previous file
    cmp al, "P"
    je Player_ParsePreviousParameter
    
    ;; Q/Esc = Quit
    cmp al, "Q"
    je Player_ProcessKey_Quit
    cmp ah, 01h                    ;; 01h=ESC scan code
    je Player_ProcessKey_Quit

    ;; Other key = Do nothing.
    jmp Player_DSPInterrupt

Player_ProcessKey_Quit:
    call StopAudio
    jmp Player_Quit

Player_ProcessKey_Backwards:
    ;; In order to go backwards 5 seconds:
    ;; Update file pointer to the beggining, skip headers
    mov ax, 4200h
    mov bx, [word ptr CurrentFileHandle]
    xor cx, cx
    mov dx, (RIFF_HEADER_SIZE + WAVE_HEADER_SIZE + DATA_HEADER_SIZE)
    int 21h

    ;; AX=Amount of transfers in 5 seconds
    mov ax, [word ptr WAVE_BlockAlign]
    ;; 10 instead of 5 because of double-buffer method
    mov bx, 10
    xor ah, ah
    mul bl

    ;; First check if at least 5 seconds have been played
    cmp ax, [word ptr TotalTransfers]
    jbe Player_ProcessKey_Backwards_5Seconds

    call loadBufferData
    call loadBufferData
    
    mov [word ptr TotalTransfers], 2

    jmp Player_DSPInterrupt
    
Player_ProcessKey_Backwards_5Seconds:
    ;; Update total transfers, use it later to calculate offset
    mov bx, [word ptr TotalTransfers]
    sub bx, ax
    mov [word ptr TotalTransfers], bx

    ;; Calculate offset, DX:AX=New added offset
    mov ax, [word ptr SubBufferLength]
    xor dx, dx
    mul bx

    ;; CX:DX=New offset, as req by interrupt
    mov cx, dx
    mov dx, ax
    
    ;; AX=0x4201, DOS Interrupt
    mov ax, 4201h
    mov bx, [word ptr CurrentFileHandle]
    int 21h

    ;; Update the progress bar because
    ;; the proc isn't called in the main loop
    ;; because the sound might be on pause
    call UpdateTime
    call UpdateProgressBar
    
    jmp Player_DSPInterrupt

Player_ProcessKey_Forwards:
    ;; In order to fast-forward 5 seconds, set the file pointer
    ;; to CUR_SEEK + 5 * Freq
    mov ax, [word ptr WAVE_BlockAlign]
    mov bl, 5
    mul bl

    ;; Add twice the amount because of double buffer
    add [word ptr TotalTransfers], ax
    add [word ptr TotalTransfers], ax
    
    mov bx, [word ptr WAVE_SampleRate]
    mul bx

    mov cx, dx
    mov dx, ax

    mov bx, [word ptr CurrentFileHandle]
    mov ax, 4201h
    int 21h

    ;; Load the whole buffer, to make an instant 
    ;; change in the sound
    call loadBufferData
    call loadBufferData
	
	cmp ax, 0
	je Player_CloseFile
    
    add [word ptr TotalTransfers], 2

    ;; Update the progress bar because
    ;; the proc isn't called in the main loop
    ;; because the sound might be on pause
    call UpdateTime
    call UpdateProgressBar

    jmp Player_DSPInterrupt

Player_ProcessKey_Pause:
    mov al, [byte ptr IsPaused]
    cmp al, 1
    jne Player_ProcessKey_Pause_Paused
    
    mov al, 0
    mov [byte ptr IsPaused], al

    push [word ptr WAVE_BitsPerSample]
    call ContAudio
    jmp Player_DSPInterrupt
    
Player_ProcessKey_Pause_Paused:
    mov al, 1    
    mov [byte ptr IsPaused], al

    push [word ptr WAVE_BitsPerSample]
    call PauseAudio

    mov ax, 00h
    int 16h
    
    jmp Player_ProcessKey

Player_ProcessKey_IncreaseVolume:
    mov al, [byte ptr CurrentVolume]
    cmp al, 0Fh
    je Player_DSPInterrupt

    inc [byte ptr CurrentVolume]
    inc al
    xor ah, ah
    push ax
    call SetMasterVol

    call UpdateVolume
    
    jmp Player_DSPInterrupt

Player_ProcessKey_DecreaseVolume:
    mov al, [byte ptr CurrentVolume]
    cmp al, 00h
    je Player_DSPInterrupt

    dec [byte ptr CurrentVolume]
    dec al
    xor ah, ah
    push ax
    call SetMasterVol
    call UpdateVolume

    jmp Player_DSPInterrupt

Player_ProcessKey_Stop:
    ;; Update file pointer to the beggining, skip headers
    mov ax, 4200h
    mov bx, [word ptr CurrentFileHandle]
    xor cx, cx
    mov dx, (RIFF_HEADER_SIZE + WAVE_HEADER_SIZE + DATA_HEADER_SIZE)
    int 21h

    ;; Load the whole buffer
    mov [byte ptr CurrentLoadBuffer], 0
    call loadBufferData
    call loadBufferData
    mov [word ptr TotalTransfers], 2

    ;; Update the progress bar because
    ;; the proc isn't called in the main loop
    call UpdateProgressBar

    jmp Player_ProcessKey_Pause

Player_DSPInterrupt:
    ;; Ask for status
    mov al, 82h
    mov dx, 224h
    out dx, al
    inc dx
    in al, dx

    ;; Check for interrupt
    test al, 00000010b
    jnz Player_DSPInterrupt_ACK

    test al, 00000001b
    jnz Player_DSPInterrupt_ACK

    jmp Player_MainLoop

Player_DSPInterrupt_ACK:
    push [word ptr WAVE_BitsPerSample]
    call SendIntACK

    ;; If last transfer has ended, close file, continue
    mov al, [byte ptr LastTransfer]
    or  al, al
    jz Player_loadBufferDataLoop
	
Player_CloseFile:	
	mov [byte IsInSplash], 0

    ;; Stop playing audio
    call StopAudio
    
    ;; Close the file handle
    push [word ptr CurrentFileHandle]
    call closeFileHandle
	
    ;; Ask for another parameter, if exists
    jmp Player_Template

Player_Quit:
    call ClearScreen

exit:
    call StopAudio
    mov   ax, 4c00h
    int   21h
END player
