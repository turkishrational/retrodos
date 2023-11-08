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

; each buffer descriptor BAR holds a pointer which has entries to the buffer
; contents of the .WAV file we're going to play.  Each entry is 8 bytes long
; (more on that later) and can contain 32 entries total, so each BAR is
; 256 bytes in length, thus:

BDL_SIZE                equ     32*8    ; Buffer Descriptor List size
INDEX_MASK              equ     31      ; indexes must be 0-31

;
; Buffer Descriptors List
; As stated earlier, each buffer descriptor list is a set of (up to) 32 
; descriptors, each 8 bytes in length.  Bytes 0-3 of a descriptor entry point
; to a chunk of memory to either play from or record to.  Bytes 4-7 of an
; entry describe various control things detailed below.
; 
; Buffer pointers must always be aligned on a Dword boundry.
;
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
; A value of FFFF is 65536 samples.  Running at 44.1Khz, that's just about
; 1.5 seconds of sample time.  FFFF * 32bits is 1FFFFh bytes or 128k of data.
;
; A value of 0 in these bits means play no samples.
;

;VIA VT8233 (VT8235) AC97 Codec equates 
;(edited by Erdogan Tan, 7/11/2016)

; PCI stuff

VIA_VID		equ     1106h           ; VIA's PCI vendor ID
VT8233_DID      equ     3059h           ; VT8233 (VT8235) device ID
			
PCI_IO_BASE          equ 10h
AC97_INT_LINE        equ 3Ch
VIA_ACLINK_CTRL      equ 41h
VIA_ACLINK_STAT      equ 40h
VIA_ACLINK_C00_READY equ 01h ; primary codec ready
	
VIA_REG_AC97	     equ 80h ; dword

VIA_ACLINK_CTRL_ENABLE	equ   80h ; 0: disable, 1: enable
VIA_ACLINK_CTRL_RESET	equ   40h ; 0: assert, 1: de-assert
VIA_ACLINK_CTRL_SYNC	equ   20h ; 0: release SYNC, 1: force SYNC hi
VIA_ACLINK_CTRL_VRA	equ   08h ; 0: disable VRA, 1: enable VRA
VIA_ACLINK_CTRL_PCM	equ   04h ; 0: disable PCM, 1: enable PCM
VIA_ACLINK_CTRL_INIT	equ  (VIA_ACLINK_CTRL_ENABLE + \
                              VIA_ACLINK_CTRL_RESET + \
                              VIA_ACLINK_CTRL_PCM + \
                              VIA_ACLINK_CTRL_VRA)

CODEC_AUX_VOL		equ   04h
VIA_REG_AC97_BUSY	equ   01000000h ;(1<<24) 
VIA_REG_AC97_CMD_SHIFT	equ   10h ; 16
VIA_REG_AC97_PRIMARY_VALID equ 02000000h ;(1<<25)
VIA_REG_AC97_READ	equ   00800000h ;(1<<23)
VIA_REG_AC97_CODEC_ID_SHIFT   equ  1Eh ; 30
VIA_REG_AC97_CODEC_ID_PRIMARY equ  0
VIA_REG_AC97_DATA_SHIFT equ   0
VIADEV_PLAYBACK         equ   0
VIA_REG_OFFSET_STATUS   equ   0    ;; byte - channel status
VIA_REG_OFFSET_CONTROL  equ   01h  ;; byte - channel control
VIA_REG_CTRL_START	equ   80h  ;; WO
VIA_REG_CTRL_TERMINATE  equ   40h  ;; WO
VIA_REG_CTRL_PAUSE      equ   08h  ;; RW
VIA_REG_CTRL_RESET      equ   01h  ;; RW - probably reset? undocumented
VIA_REG_OFFSET_STOP_IDX equ   08h  ;; dword - stop index, channel type, sample rate
VIA8233_REG_TYPE_16BIT  equ   200000h ;; RW
VIA8233_REG_TYPE_STEREO equ   100000h ;; RW
VIA_REG_OFFSET_CURR_INDEX equ 0Fh ;; byte - channel current index (for via8233 only)
VIA_REG_OFFSET_TABLE_PTR equ  04h  ;; dword - channel table pointer
VIA_REG_OFFSET_CURR_PTR equ   04h  ;; dword - channel current pointer
VIA_REG_OFS_PLAYBACK_VOLUME_L equ  02h ;; byte
VIA_REG_OFS_PLAYBACK_VOLUME_R equ  03h ;; byte
VIA_REG_CTRL_AUTOSTART	equ   20h
VIA_REG_CTRL_INT_EOL	equ   02h
VIA_REG_CTRL_INT_FLAG	equ   01h
VIA_REG_CTRL_INT	equ   (VIA_REG_CTRL_INT_FLAG + \
                              VIA_REG_CTRL_INT_EOL + \
                              VIA_REG_CTRL_AUTOSTART)
; 24/11/2016
VIA_REG_STAT_STOPPED	equ   04h    ;; RWC
VIA_REG_STAT_EOL	equ   02h    ;; RWC
VIA_REG_STAT_FLAG	equ   01h    ;; RWC
VIA_REG_STAT_ACTIVE	equ   80h    ;; RO
; 28/11/2016
VIA_REG_STAT_LAST	equ   40h    ;; RO
VIA_REG_STAT_TRIGGER_QUEUED equ 08h  ;; RO
VIA_REF_CTRL_INT_STOP	equ   04h  ; Interrupt on Current Index = Stop Index
				   ; and End of Block
; 02/12/2016
VIA_REG_OFFSET_CURR_COUNT equ 0Ch ;; byte - channel current count			