﻿

;
; +-------------------------------------------------------------------------+
; |   This file has been generated by The Interactive Disassembler (IDA)    |
; |           Copyright (c) 2018 Hex-Rays, <support@hex-rays.com>           |
; |                            Freeware version                             |
; +-------------------------------------------------------------------------+
;
; Input SHA256 : 2264920B57D2B9B8C3C5F28AAA7FEE11A9C30C173429C0CE2CC6C8C734F7F866
; Input MD5    : 7636CC73FF4B44BA4E3687793130EC7D
; Input CRC32  : 2C9C7474

; ---------------------------------------------------------------------------
; File Name   : C:\Users\HİTEK\Desktop\BOOTSECT.DAT
; Format      : Binary file
; Base Address: 0000h Range: 0000h - 0200h Loaded length: 0200h

                .386
                .model flat

; ===========================================================================

; Segment type: Pure code
seg000          segment byte public 'CODE' use16
                assume cs:seg000
                ;org 7C00h
                assume es:nothing, ss:nothing, ds:nothing, fs:nothing, gs:nothing

BS_jmpBoot:                             ; Disassembled by Erdogan Tan - June 2023
                jmp     short Main
; ---------------------------------------------------------------------------
                nop
; ---------------------------------------------------------------------------
BS_OEMName      db 'IBM  '              ; Ref: "Microsoft Extensible Firmware Initiative
                                        ; FAT32 File System Specification"
                                        ;             fatgen103.doc (fatgen103.pdf)
                                        ; & PCDOS 7.1 Bootsector file:
                                        ;             BOOTSECT.DAT - 22/04/2008 19:41
                                        ; & MSDOS 6.0 Boot Sector source code:
                                        ;             MSBOOT.ASM - 1991 (31/10/1999 18:26)
OSVersion       db '7.1'
BPB_BytsPerSec  dw 200h
BPB_SecPerClus  db 8
BPB_RsvdSecCnt  dw 32
BPB_NumFATs     db 2
BPB_RootEntCnt  dw 0
BPB_TotSec16    dw 0
BPB_Media       db 0F8h
BPB_FATSz16     dw 0
BPB_SecPerTrk   dw 63
BPB_NumHeads    dw 255
BPB_HiddSec     dd 63
BPB_TotSec32    dd 8369802
BPB_FATSz32     dd 8166                 ; FAT32 Structure Starting at Offset 36
BPB_ExtFlags    dw 0
BPB_FSVer       dw 0
BPB_RootClus    dd 2
BPB_FSInfo      dw 1
BPB_BkBootSec   dw 6
BPB_Reserved    db 12 dup(  0)
BS_DrvNum       db 80h
BS_Reserved1    db 0
BS_BootSig      db 29h
BS_VolID        dd 184607F9h
BS_VolLab       db 'NO NAME    '
BS_FilSysType   db 'FAT32   '
; ---------------------------------------------------------------------------

Main:                                   ; ...
                cli
                xor     ax, ax
                mov     ss, ax
                mov     sp, 7BECh       ; STACK (BootSectorOffset-20)
                push    ss
                pop     es
                mov     bp, sp
                mov     bx, 78h         ; INT 1Eh ; DSK_PARMS
                lds     si, ss:[bx]
                push    ds              ; Save original INT 1Eh table & vector
                                        ; (*)
                push    si              ; (**)
                push    ss              ; (***)
                push    bx              ; (****)
                mov     di, bp          ; 7BECh
                mov     cx, 16
                push    di              ; (*****)
                cld
                rep movsb
                push    es
                pop     ds
                mov     byte ptr [di-7], 0Fh ; Set the head settle time to 15ms
                                        ; because we don't have room to do a disk retry
                                        ; and then set sectors per from the value
                                        ; in the BPB
                mov     cx, [bp+2Ch]    ; BPB_SecPerTrk
                mov     [di-12], cl     ; End of Track
                mov     [bx+2], ax      ; Place in new disk parameter table vector
                pop     word ptr [bx]   ; 0:7BECh
                                        ; (*****)
                sti
                call    disk_io         ; ax = 0 ; disk reset
                mov     cx, [bp+27h]    ; BPB_TotSec16
                jcxz    short Dir_Cont
                mov     [bp+34h], cx    ; BPB_TotSec32

Dir_Cont:                               ; ...
                mov     al, [bp+24h]    ; BPB_NumFATs
                cwd
                mov     bx, [bp+2Ah]    ; BPB_FATSz16
                or      bx, bx
                jnz     short Dir_Cont_fat

Dir_Cont_fat32:
                xchg    ax, bx
                mov     ax, [bp+38h]    ; BPB_FATSz32
                mov     dx, [bp+3Ah]    ; BPB_FATSz32+2

Dir_Cont_fat:                           ; ...
                call    mul32           ; dx:ax = NumFats * NumFatSecs
                add     ax, [bp+30h]    ; BPB_HiddSec
                adc     dx, [bp+32h]    ; BPB_HiddSec+2
                add     ax, [bp+22h]    ; BPB_RsvdSecCnt
                adc     dx, 0           ; dx:ax = NumFats * NumFatSecs
                                        ;         + ReservedSecs + cSecHid
                push    dx              ; (5*)
                push    ax              ; (6*)
                stosw                   ; [0:7BFCh] = ax
                xchg    ax, dx
                stosw                   ; [0:7BFEh] = dx
                mov     ax, 32          ; DIR_ENTRY_SIZE
                mul     word ptr [bp+25h] ; BPB_RootEntCnt
                mov     bx, [bp+1Fh]    ; BPB_BytsPerSec (= 512)
                dec     bx              ; 511
                add     ax, bx          ; add ax, 511
                adc     dx, 0
                inc     bx              ; 512
                div     bx
                xor     dx, dx
                add     [bp+10h], ax    ; add [0:7BFCh], ax
                adc     [bp+12h], dx    ; adc [0:7BFEh], dx
                xor     ax, ax
                cmp     ax, [bp+2Ah]    ; BPB_FATSz16
                jnz     short not_fat32_fs
                mov     dx, [bp+42h]    ; BPB_RootClus+2
                mov     ax, [bp+40h]    ; BPB_RootClus
                mov     cx, ax
                or      cx, dx
                jcxz    short not_fat32_fs
                call    calc_dir_sector_offset ; calculate sector index/offset
                                        ;         of the root directory

not_fat32_fs:                           ; ...
                pop     bx              ; (6*)
                add     ax, bx
                pop     bx              ; (5*)
                adc     dx, bx          ; dx:ax = start sector # of root directory
                mov     bx, 500h        ; DIR_OFF (buffer address)
                call    disk_read       ; read root directory
                jb      short Load_Failure
                mov     di, bx
                mov     cx, 11
                mov     si, offset IbmbioCom ; Scan for the presence of BIOS file.
                push    cx              ; check if the 1st entry is "IBMBIO  COM"
                repe cmpsb
                pop     cx
                jnz     short Load_Failure ; it is not 'IBMBIO.COM'
                lea     di, [bx+20h]    ; check if the 2nd entry is "IBMDOS  COM"
                repe cmpsb
                jz      short root_dir_ok ; 1st 2 root dir entries are PCDOS bios and kernel files

Load_Failure:                           ; ...
                mov     si, offset BootFailure ; "\r\nBoot failure"
                lodsb

write_message:                          ; ...
                mov     ah, 0Eh
                mov     bx, 7
                int     10h             ; - VIDEO - WRITE CHARACTER AND ADVANCE CURSOR (TTY WRITE)
                                        ; AL = character, BH = display page (alpha modes)
                                        ; BL = foreground color (graphics modes)
                lodsb
                or      al, al
                jnz     short write_message
                cbw
                int     16h             ; KEYBOARD -
                pop     si              ; (****) ; restore INT 1Eh vector
                pop     ds              ; (***)
                pop     word ptr [si]   ; (**)
                pop     word ptr [si+2] ; (*)
                int     19h             ; DISK BOOT
                                        ; causes reboot of disk system

root_dir_ok:                            ; ...
                mov     dx, [bx+14h]    ; High word of cluster number
                mov     ax, [bx+1Ah]    ; Low word of cluster number
                cmp     [bp+2Ah], cx    ; BPB_FATSz16 ; cx = 0
                jz      short it_is_big_fat
                xor     dx, dx          ; HW of cluster number is (must be) zero

it_is_big_fat:                          ; ...
                call    calc_dir_sector_offset ; calculate sector index/offset
                                        ;         of the file
                                        ; dx:ax = sector offset from start of data
                add     ax, [bp+10h]    ; [0:7BFCh] = start sector # of data
                adc     dx, [bp+12h]    ; dx:ax = start sector # of the file
                mov     bx, 700h        ; [0:700h] = IBMBIO.COM loading address
                mov     cl, 4           ; load 4 sectors (MSLOAD code size)

load_file_sector:                       ; ...
                push    ax
                push    dx
                push    cx
                call    disk_read
                pop     cx
                pop     dx
                pop     ax
                jb      short Load_Failure
                inc     ax
                jnz     short load_next_file_sector
                inc     dx

load_next_file_sector:                  ; ...
                add     bx, [bp+1Fh]    ; BPB_BytsPerSec
                loop    load_file_sector
                mov     ch, [bp+29h]    ; BPB_Media
                mov     dl, [bp+54h]    ; BS_DrvNum
                les     bx, [bp+10h]    ; start sector # of data
                mov     ax, es          ; es:bx = ax:bx = dword/far ptr [0:7BFCh]
                lds     si, [bp-4]      ; ds:si = DSK_PARMS INT 1Eh table address
                                        ;         also in stack [at 0:7BE8h] ; (*) (**)
                jmp     far ptr 70h:0   ; far jump to MSLOAD (IBMBIO.COM start) code
                                        ;
                                        ; Stack:
                                        ;  ss:sp = Original INT 1Eh vector address (ss:bx)
                                        ;  ss:sp+4 = Original INT 1Eh disk table addr (ds:si)

; =============== S U B R O U T I N E =======================================


disk_read       proc near               ; ...
                test    byte ptr [bp+54h], 80h ; BS_DrvNum
                jz      short chs_read

lba_read:                               ; 0
                xor     si, si
                mov     cx, sp
                push    si
                push    si              ; zero dword
                push    dx
                push    ax              ; disk LBA address (8 bytes)
                push    es
                push    bx              ; transfer buffer address (es:bx)
                inc     si
                push    si              ; 1 ; number of sectors to read
                mov     si, 10h
                push    si              ; Packet size (16)
                mov     si, sp          ; LBA Packet buffer address
                push    ax
                push    dx
                mov     ah, 42h         ; LBA Read
                call    disk_io
                pop     dx
                pop     ax
                mov     sp, cx
                jnb     short disk_read_ok

chs_read:                               ; ...
                mov     cx, [bp+2Ch]    ; BPB_SecPerTrk
                cmp     cx, dx
                jb      short disk_read_ok ; out of CHS read capacity !
                div     cx
                inc     dx
                mov     ch, dl          ; sector
                xor     dx, dx
                div     word ptr [bp+2Eh] ; BPB_NumHeads
                mov     dh, dl          ; head
                mov     cl, 6
                shl     ah, cl          ; cylinder bits 8 and 9
                or      ah, ch          ; AH bits 0 to 5 are sector bits,
                                        ; bits 6 and 7 are cylinder bits 8 and 9
                xchg    al, ah
                xchg    ax, cx          ; CL contains sector (6 bits) number
                                        ; and high two bits of cylinder number
                                        ; CH contains low 8 bits of cylinder number
                mov     ax, 201h        ; Read 1 sector

disk_io:                                ; ...
                mov     dl, [bp+54h]    ; BS_DrvNum
                push    di
                mov     di, 5           ; retry count

try_again:                              ; ...
                push    ax
                int     13h             ; DISK - READ SECTORS INTO MEMORY
                                        ; AL = number of sectors to read, CH = track, CL = sector
                                        ; DH = head, DL = drive, ES:BX -> buffer to fill
                                        ; Return: CF set on error, AH = status, AL = number of sectors read
                pop     ax
                dec     di
                jz      short no_retry
                jb      short try_again

no_retry:                               ; ...
                pop     di

disk_read_ok:                           ; ...
                retn
disk_read       endp


; =============== S U B R O U T I N E =======================================


calc_dir_sector_offset proc near        ; ...
                xor     bx, bx
                sub     ax, 2
                sbb     dx, bx
                mov     bl, [bp+21h]    ; BPB_SecPerClus

mul32:                                  ; ...
                push    ax
                xchg    ax, dx
                mul     bx
                xchg    ax, bx
                pop     dx
                mul     dx
                add     dx, bx
                retn
calc_dir_sector_offset endp

; ---------------------------------------------------------------------------
IbmbioCom       db 'IBMBIO  COM'        ; ...
IbmdosCom       db 'IBMDOS  COM'
BootFailure     db 0Dh,0Ah              ; ...
                db 'Boot failure',0,0
                dw 0AA55h
seg000          ends


                end
