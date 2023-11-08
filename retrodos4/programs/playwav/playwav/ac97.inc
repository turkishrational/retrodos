; 05/11/2023
; 03/11/2023
; 17/02/2017 (Erdogan Tan, PLAYWAV.ASM)
; constant.inc & codec.inc (for ICH AC97 wav player, 'PLAYWAV.COM') 

; ----------------------------------------------------------------------------
; CONSTANT.INC
; ----------------------------------------------------------------------------

;constants of stuff that seem hard to remember at times.

TRUE  EQU 1
FALSE EQU 0

ENABLED  EQU 1
DISABLED EQU 0

BIT0  EQU 1
BIT1  EQU 2
BIT2  EQU 4
BIT3  EQU 8
BIT4  EQU 10h
BIT5  EQU 20h
BIT6  EQU 40h
BIT7  EQU 80h
BIT8  EQU 100h
BIT9  EQU 200h
BIT10 EQU 400h
BIT11 EQU 800h
BIT12 EQU 1000h
BIT13 EQU 2000h
BIT14 EQU 4000h
BIT15 EQU 8000h
BIT16 EQU 10000h
BIT17 EQU 20000h
BIT18 EQU 40000h
BIT19 EQU 80000h
BIT20 EQU 100000h
BIT21 EQU 200000h
BIT22 EQU 400000h
BIT23 EQU 800000h
BIT24 EQU 1000000h
BIT25 EQU 2000000h
BIT26 EQU 4000000h
BIT27 EQU 8000000h
BIT28 EQU 10000000h
BIT29 EQU 20000000h
BIT30 EQU 40000000h
BIT31 EQU 80000000h

;special characters
NUL     EQU 0
NULL    EQU 0
BELL    EQU 07
BS      EQU 08
TAB     EQU 09
LF      EQU 10
CR      EQU 13
ESCAPE  EQU 27           ;ESC is a reserved word....


;file stuff
READONLY  EQU   BIT0
HIDDEN    EQU   BIT1
SYSTEM    EQU   BIT2
VOLUME    EQU   BIT3         ;ignored for file access
DIRECTORY EQU   BIT4         ;must be 0 for file access
ARCHIVE   EQU   BIT5
SHAREABLE EQU   BIT7         ;for novell networks
OPEN	EQU	2		; open existing file
CREATE	EQU	1		; create new file


; PCI equates
; PCI function address (PFA)
; bit 31 = 1
; bit 23:16 = bus number     (0-255)
; bit 15:11 = device number  (0-31)
; bit 10:8 = function number (0-7)
; bit 7:0 = register number  (0-255)

IO_ADDR_MASK    EQU     0FFFEh          ; mask off bit 0 for reading BARs
PCI_INDEX_PORT  EQU     0CF8h
PCI_DATA_PORT   EQU     0CFCh
PCI32           EQU     BIT31           ; bitflag to signal 32bit access
PCI16           EQU     BIT30           ; bitflag for 16bit access

PCI_FN0         EQU     0 << 8
PCI_FN1         EQU     1 << 8
PCI_FN2         EQU     2 << 8
PCI_FN3         EQU     3 << 8
PCI_FN4         EQU     4 << 8
PCI_FN5         EQU     5 << 8
PCI_FN6         EQU     6 << 8
PCI_FN7         EQU     7 << 8

PCI_CMD_REG		EQU	04h		; reg 04, command reg
 IO_ENA			EQU	BIT0		; i/o decode enable
 MEM_ENA		EQU	BIT1		; memory decode enable
 BM_ENA                 EQU     BIT2		; bus master enable

; ----------------------------------------------------------------------------
; CODEC.INC
; ----------------------------------------------------------------------------

;Codec registers.
;
;Not all codecs are created equal. Refer to the spec for your specific codec.
;
;All registers are 16bits wide.  Access to codec registers over the AC97 link
;is defined by the OEM.  
;
;Secondary codec's are accessed by ORing in BIT7 of all register accesses.
;

; each codec/mixer register is 16bits

CODEC_RESET_REG                 equ     00      ; reset codec
CODEC_MASTER_VOL_REG            equ     02      ; master volume
CODEC_HP_VOL_REG                equ     04      ; headphone volume
CODEC_MASTER_MONO_VOL_REG       equ     06      ; master mono volume
CODEC_MASTER_TONE_REG           equ     08      ; master tone (R+L)
CODEC_PCBEEP_VOL_REG            equ     0ah     ; PC beep volume
CODEC_PHONE_VOL_REG             equ     0bh     ; phone volume
CODEC_MIC_VOL_REG               equ     0eh     ; MIC volume
CODEC_LINE_IN_VOL_REG           equ     10h     ; line input volume
CODEC_CD_VOL_REG                equ     12h     ; CD volume
CODEC_VID_VOL_REG               equ     14h     ; video volume
CODEC_AUX_VOL_REG               equ     16h     ; aux volume
CODEC_PCM_OUT_REG               equ     18h     ; PCM output volume
CODEC_RECORD_SELECT_REG         equ     1ah     ; record select input
CODEC_RECORD_VOL_REG            equ     1ch     ; record volume
CODEC_RECORD_MIC_VOL_REG        equ     1eh     ; record mic volume
CODEC_GP_REG                    equ     20h     ; general purpose
CODEC_3D_CONTROL_REG            equ     22h     ; 3D control
; 24h is reserved
CODEC_POWER_CTRL_REG            equ     26h     ; powerdown control
CODEC_EXT_AUDIO_REG             equ     28h     ; extended audio
CODEC_EXT_AUDIO_CTRL_REG        equ     2ah     ; extended audio control
CODEC_PCM_FRONT_DACRATE_REG     equ     2ch     ; PCM out sample rate
CODEC_PCM_SURND_DACRATE_REG     equ     2eh     ; surround sound sample rate
CODEC_PCM_LFE_DACRATE_REG       equ     30h     ; LFE sample rate
CODEC_LR_ADCRATE_REG            equ     32h     ; PCM in sample rate
CODEC_MIC_ADCRATE_REG           equ     34h     ; mic in sample rate

; registers 36-7a are reserved on the ICH

CODEC_VENDORID1_REG             equ     7ch     ; codec vendor ID 1
CODEC_VENDORID2_REG             equ     7eh     ; codec vendor ID 2

; Mixer registers 0 through 51h reside in the ICH and are not forwarded over
; the AC97 link to the codec, which I think is a little weird.  Looks like
; the ICH makes it so you don't need a fully functional codec to play audio?
;
; whenever 2 codecs are present in the system, use BIT7 to access the 2nd
; set of registers, ie 80h-feh

PRIMARY_CODEC                   equ     0       ; 0-7F for primary codec
SECONDARY_CODEC                 equ     BIT7    ; 80-8f registers for 2ndary

SAMPLE_RATE_441khz	equ     44100   ; 44.1Khz (cd quality) rate

; ----------------------------------------------------------------------------
; 17/02/2017
PCI_IO_BASE	equ 10h			; = NAMBAR register offset
AC97_INT_LINE   equ 3Ch			; AC97 Interrupt Line register offset

; ----------------------------------------------------------------------------
; ICH2AC97.INC
; ----------------------------------------------------------------------------

; PCI stuff

; Intel ICH2 equates. It is assumed that ICH0 and plain ole ICH are compatible.

INTEL_VID       equ     8086h           ; Intel's PCI vendor ID
; 03/11/2023 - Erdogan Tan (Ref: MenuetOS AC97 WAV Player source code, 2004)
SIS_VID		equ	1039h
NVIDIA_VID	equ	10DEh	 ; Ref: MPXPLAY/SBEMU/KOLIBRIOS AC97 source c.
AMD_VID		equ	1022h

ICH_DID         equ     2415h           ; ICH device ID
ICH0_DID        equ     2425h           ; ICH0
ICH2_DID        equ     2445h           ; ICH2 I think there are more ICHes.
                                        ; they all should be compatible.

; 17/02/2017 (Erdogan Tan, ref: ALSA Device IDs, ALSA project)
ICH3_DID	equ     2485h           ; ICH3
ICH4_DID        equ     24C5h           ; ICH4
ICH5_DID	equ     24D5h           ; ICH5 
ICH6_DID	equ     266Eh           ; ICH6
ESB6300_DID	equ     25A6h           ; 6300ESB
ESB631X_DID	equ     2698h           ; 631XESB
ICH7_DID	equ	27DEh		; ICH7
; 03/11/2023 - Erdogan Tan (Ref: MenuetOS AC97 WAV Player source code, 2004)
MX82440_DID	equ	7195h
SI7012_DID	equ	7012h
NFORCE_DID	equ	01B1h
NFORCE2_DID	equ	006Ah
AMD8111_DID	equ	746Dh
AMD768_DID	equ	7445h
; 03/11/2023 - Erdogan Tan - Ref: MPXPLAY/SBEMU/KOLIBRIOS AC97 source code
CK804_DID	equ	0059h
MCP04_DID	equ	003Ah
CK8_DID		equ	008Ah
NFORCE3_DID	equ	00DAh
CK8S_DID	equ	00EAh

NAMBAR_REG      equ     10h             ; native audio mixer BAR
 NAM_SIZE       equ     256             ; 256 bytes required.

NABMBAR_REG     equ     14h             ; native audio bus mastering BAR
 NABM_SIZE      equ     64              ; 64 bytes

; BUS master registers, accessed via NABMBAR+offset

; ICH supports 3 different types of register sets for three types of things
; it can do, thus:
;
; PCM in (for recording) aka PI
; PCM out (for playback) aka PO
; MIC in (for recording) aka MC

PI_BDBAR_REG            equ     0       ; PCM in buffer descriptor BAR
PO_BDBAR_REG            equ     10h     ; PCM out buffer descriptor BAR
MC_BDBAR_REG            equ     20h     ; MIC in buffer descriptor BAR

; each buffer descriptor BAR holds a pointer which has entries to the buffer
; contents of the .WAV file we're going to play. Each entry is 8 bytes long
; (more on that later) and can contain 32 entries total, so each BAR is
; 256 bytes in length, thus:

BDL_SIZE                equ     32*8    ; Buffer Descriptor List size
INDEX_MASK              equ     31      ; indexes must be 0-31



PI_CIV_REG              equ     4       ; PCM in current Index value (RO)
PO_CIV_REG              equ     14h     ; PCM out current Index value (RO)
MC_CIV_REG              equ     24h     ; MIC in current Index value (RO)
;8bit read only
; each current index value is simply a pointer showing us which buffer
; (0-31) the codec is currently processing. Once this counter hits 31, it
; wraps back to 0.
; this can be handy to know, as once it hits 31, we're almost out of data to
; play back or room to record!


PI_LVI_REG              equ     5       ; PCM in Last Valid Index
PO_LVI_REG              equ     15h     ; PCM out Last Valid Index
MC_LVI_REG              equ     25h     ; MIC in Last Valid Index
;8bit read/write
; The Last Valid Index is a number (0-31) to let the codec know what buffer
; number to stop on after processing. It could be very nasty to play audio
; from buffers that aren't filled with the audio we want to play.


PI_SR_REG               equ     6       ; PCM in Status register
PO_SR_REG               equ     16h     ; PCM out Status register
MC_SR_REG               equ     26h     ; MIC in Status register
;16bit read/write
; status registers.  Bitfields follow:

FIFO_ERR                equ     BIT4    ; FIFO Over/Underrun W1TC.

BCIS                    equ     BIT3    ; buffer completion interrupt status.
                                        ; Set whenever the last sample in ANY
                                        ; buffer is finished.  Bit is only
                                        ; set when the Interrupt on Complete
                                        ; (BIT4 of control reg) is set.

LVBCI                   equ     BIT2    ; Set whenever the codec has processed
                                        ; the last buffer in the buffer list.
                                        ; Will fire an interrupt if IOC bit is
                                        ; set. Probably set after the last
                                        ; sample in the last buffer is
                                        ; processed.  W1TC

                                        ; 
CELV                    equ     BIT1    ; Current buffer == last valid.
                                        ; Bit is RO and remains set until LVI is
                                        ; cleared.  Probably set up the start
                                        ; of processing for the last buffer.


DCH                     equ     BIT0    ; DMA controller halted.
                                        ; set whenever audio stream is stopped
                                        ; or something else goes wrong.


PI_PICB_REG             equ     8       ; PCM in position in current buffer(RO)
PO_PICB_REG             equ     18h     ; PCM out position in current buffer(RO)
MC_PICB_REG             equ     28h     ; MIC in position in current buffer (RO)
;16bit read only
; position in current buffer regs show the number of dwords left to be
; processed in the current buffer.
; 

PI_PIV_REG              equ     0ah     ; PCM in Prefected index value
PO_PIV_REG              equ     1ah     ; PCM out Prefected index value
MC_PIV_REG              equ     2ah     ; MIC in Prefected index value
;8bit, read only
; Prefetched index value register.
; tells which buffer number (0-31) has be prefetched. I'd imagine this
; value follows the current index value fairly closely. (CIV+1)
;


PI_CR_REG               equ     0bh     ; PCM in Control Register
PO_CR_REG               equ     1bh     ; PCM out Control Register
MC_CR_REG               equ     2bh     ; MIC in Control Register
; 8bit
; Control register *MUST* only be accessed as an 8bit value.
; Control register. See bitfields below.
;


IOCE                    equ     BIT4    ; interrupt on complete enable.
                                        ; set this bit if you want an intrtpt
                                        ; to fire whenever LVBCI is set.
FEIFE                   equ     BIT3    ; set if you want an interrupt to fire
                                        ; whenever there is a FIFO (over or
                                        ; under) error.
LVBIE                   equ     BIT2    ; last valid buffer interrupt enable.
                                        ; set if you want an interrupt to fire
                                        ; whenever the completion of the last
                                        ; valid buffer.
RR                      equ     BIT1    ; reset registers.  Nukes all regs
                                        ; except bits 4:2 of this register.
                                        ; Only set this bit if BIT 0 is 0
RPBM                    equ     BIT0    ; Run/Pause
                                        ; set this bit to start the codec!


GLOB_CNT_REG            equ     2ch     ; Global control register
SEC_RES_EN              equ     BIT5    ; secondary codec resume event 
                                        ; interrupt enable.  Not used here.
PRI_RES_EN              equ     BIT4    ; ditto for primary. Not used here.
ACLINK_OFF              equ     BIT3    ; Turn off the AC97 link
ACWARM_RESET            equ     BIT2    ; Awaken the AC97 link from sleep.
                                        ; registers preserved, bit self clears
ACCOLD_RESET            equ     BIT1    ; Reset everything in the AC97 and
                                        ; reset all registers.  Not self clearing

GPIIE                   equ     BIT0    ; GPI Interrupt enable.
                                        ; set if you want an interrupt to
                                        ; fire upon ANY of the bits in the
                                        ; GPI (general pursose inputs?) not used.

GLOB_STS_REG            equ     30h     ; Global Status register (RO)

MD3                     equ     BIT17   ; modem powerdown status (yawn)
AD3                     equ     BIT16   ; Audio powerdown status (yawn)
RD_COMPLETE_STS         equ     BIT15   ; Codec read timed out. 0=normal
BIT3SLOT12              equ     BIT14   ; shadowed status of bit 3 in slot 12
BIT2SLOT12              equ     BIT13   ; shadowed status of bit 2 in slot 12
BIT1SLOT12              equ     BIT12   ; shadowed status of bit 1 in slot 12
SEC_RESUME_STS          equ     BIT11   ; secondary codec has resumed (and irqed)
PRI_RESUME_STS          equ     BIT10   ; primary codec has resumed (and irqed)
SEC_CODEC_RDY           equ     BIT9    ; secondary codec is ready for action
PRI_CODEC_RDY           equ     BIT8    ; Primary codec is ready for action
                                        ; software must check these bits before
                                        ; starting the codec!
MIC_IN_IRQ              equ     BIT7    ; MIC in caused an interrupt
PCM_OUT_IRQ             equ     BIT6    ; One of the PCM out channels IRQed
PCM_IN_IRQ              equ     BIT5    ; One of the PCM in channels IRQed
MODEM_OUT_IRQ           equ     BIT2    ; modem out channel IRQed
MODEM_IN_IRQ            equ     BIT1    ; modem in channel IRQed
GPI_STS_CHANGE          equ     BIT0    ; set whenever GPI's have changed.
                                        ; BIT0 of slot 12 also reflects this.

ACC_SEMA_REG            equ     34h     ; Codec write semiphore register
CODEC_BUSY              equ     BIT0    ; codec register I/O is happening
                                        ; self clearing
;
; Buffer Descriptors List
; As stated earlier, each buffer descriptor list is a set of (up to) 32 
; descriptors, each 8 bytes in length. Bytes 0-3 of a descriptor entry point
; to a chunk of memory to either play from or record to. Bytes 4-7 of an
; entry describe various control things detailed below.
; 
; Buffer pointers must always be aligned on a Dword boundry.
;

IOC                     equ     BIT31   ; Fire an interrupt whenever this
                                        ; buffer is complete.

BUP                     equ     BIT30   ; Buffer Underrun Policy.
                                        ; if this buffer is the last buffer
                                        ; in a playback, fill the remaining
                                        ; samples with 0 (silence) or not.
                                        ; It's a good idea to set this to 1
                                        ; for the last buffer in playback,
                                        ; otherwise you're likely to get a lot
                                        ; of noise at the end of the sound.

;
; Bits 15:0 contain the length of the buffer, in number of samples, which
; are 16 bits each, coupled in left and right pairs, or 32bits each.
; Luckily for us, that's the same format as .wav files.
;
; A value of FFFF is 65536 samples. Running at 44.1Khz, that's just about
; 1.5 seconds of sample time. FFFF * 32bits is 1FFFFh bytes or 128k of data.
;
; A value of 0 in these bits means play no samples.
;