; ****************************************************************************
; GETDPB.ASM - Retro DOS v5.0 (IBMBIO.COM) TEST Program/Utility by Erdogan Tan
; ----------------------------------------------------------------------------
; Beginning & Last Update: 17/04/2024
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15
; ----------------------------------------------------------------------------
; nasm getdpb.asm -l getdpb.txt -o GETDPB.COM -Z error.txt
; ****************************************************************************

	[BITS 16]
	[org 100h]

	mov	bx, bss_end

        add	bx, 15
        shr	bx, 1
        shr	bx, 1
	shr	bx, 1
	shr	bx, 1
        mov	ah, 4Ah ; modify memory allocation
        ;push	cs
        ;pop	es
        int	21h

;-----------------------------------------------------------------
; clear BSS
;-----------------------------------------------------------------

	mov	cx, bss_clear_end

	mov	di, bss_start
	sub	cx, di
	inc	cx
	shr	cx, 1
	xor	ax, ax
	rep	stosw

;-----------------------------------------------------------------
; display program name & version
;-----------------------------------------------------------------

	mov	si, RetroDOS_Welcome
	call	print_msg

;-----------------------------------------------------------------
; get disk name
;-----------------------------------------------------------------

	mov	si, 80h			; PSP command tail
 	lodsb
	or	al, al 			; command tail length
	jz	short A_03		; jump if zero
A_01:
	lodsb
	cmp	al, ' '			; is it SPACE ?
	je	short A_01
	jb	short A_03
	
	; check disk name
	and	al, 0DFh		; capitalize
	cmp	al, 'A'
	jb	short A_03
	cmp	al, 'Z'
	ja	short A_03
	mov	ah, al
A_02:
	lodsb
	cmp	al, 0Dh ; CR
	jna	short A_04
	cmp	al, ' '	; space
	je	short A_02
	cmp	al, ':'
	jne	short A_03
	cmp	byte [si], 0Dh
	jna	short A_04
A_03:
	mov	si, RetroDOS_Usage
	jmp	short error_exit

;-----------------------------------------------------------------
; get dos version
;-----------------------------------------------------------------

A_04:
	sub	ah, 'A'-1

	mov	[drive_number], ah

;-----------------------------------------------------------------
; get dos version
;-----------------------------------------------------------------

	mov	ah, 30h
	int	21h

	push	ax

	call	print_dos_version

	mov	dl, [drive_number] ; 00h=default, 01h=A:, etc.

	pop	ax
	
	cmp	al, 7	 ; major version number
	ja	short B_02
	jb	short B_01

	cmp	ah, 10	 ; minor version number
	jnb	short B_02

;-----------------------------------------------------------------
; get disk parameters
;-----------------------------------------------------------------

B_01:
	;mov	dl, [drive_number] ; 00h=default, 01h=A:, etc. 
	mov	ah, 32h ; GET DOS DRIVE PARAMETER BLOCK 
	int	21h
	;jc	short drvndry_error
	or	al, al
	jnz	short drvndry_error
			; DS:BX -> Drive Parameter Block (DPB)
	mov	ax, bp
	mov	cx, 33
	cmp	al, 4	 ; < version 4.0
	jnb	short C_03
	mov	cl, 30
	cmp	al, 3	 ; < version 3.0
	jnb	short C_03
	mov	cl, 24
	jmp	short C_03

;-----------------------------------------------------------------
; get disk parameters (FAT32)
;-----------------------------------------------------------------
	
B_02:
	; PCDOS 7.1 (Retro DOS v5.0) & Windows 95-98-ME
	; FAT32 - GET EXTENDED DPB

	;mov	dl, [drive_number] ; 00h=default, 01h=A:, etc. 
	mov	ax, 7302h
	mov	di, DPB_buffer-2
			; ES:DI -> buffer for returned data
	mov	cx, 63 ; length of buffer (003Fh for Windows95)
	mov	si, 0F1A6h ; signature 
			; (must be F1A6h to get device driver
			;  address and next-DBP pointer)
	int	21h
	jnc	short C_01

;-----------------------------------------------------------------
; error message & exit
;-----------------------------------------------------------------

drvndry_error:
	mov	si, drvnrdy_msg
error_exit:
	call	print_msg

;-----------------------------------------------------------------
; EXIT
;-----------------------------------------------------------------

exit:
	int	20h
hang:
	hlt
	jmp	short hang

;-----------------------------------------------------------------
; display/write disk parameters
;-----------------------------------------------------------------

C_01:
	mov	bp, cx		; save buffer length
	
	mov	si, dpb_header
	call	print_msg
	cmp	bp, 63
	jb	short C_02
	mov	si, dpb_ext_header
	call	print_msg
C_02:
	mov	si, crlf
	call	print_msg

	call	print_parameters

	call	print_hxd_txt

	mov	si, crlf
	call	print_msg

	jmp	short exit

;-----------------------------------------------------------------
; copy DPB to local buffer
;-----------------------------------------------------------------

C_03:
	push	cx
	mov	si, bx
	
	mov	di, DPB_buffer
	rep	movsb
	pop	cx
	push	cs
	pop	ds
	;mov	[DPB_Buffer-2],cx
	jmp	short C_01

;-----------------------------------------------------------------
; print/display dos version
;-----------------------------------------------------------------

print_dos_version:
	mov	di, version_str
	push	ax
	; al = dos major version
	call	conv_to_dec_num
	mov	al, '.'
	stosb
	pop	ax
	xchg	ah, al
	
	; al = dos minor version
	call	conv_to_dec_num
	
	mov	al, 0Dh
	stosb
	mov	al, 0Ah
	stosb
	sub	ax, ax
	stosb

	mov	si, dos_version
	;jmp	short print_msg

;-----------------------------------------------------------------
; print/display message (asciiz text)
;-----------------------------------------------------------------

print_msg:
	mov	ah, 0Eh
	mov	bx, 7
print_loop:
	lodsb
	or	al, al
	jz	short print_end
	int	10h
	jmp	short print_loop
print_end:
	retn

;-----------------------------------------------------------------
; print/display hexadecimal data text
;-----------------------------------------------------------------

print_hxd_txt:
	mov	si, DPB_buffer
	mov	di, dpb_hex_data
pr_hxd_txt_l1:
	mov	dx, 16
pr_hxd_txt_l2:
	lodsb
	call	conv_to_hex_num
	dec	bp
	jz	short pr_hxd_txt_ok
	dec	dx
	jnz	short pr_hxd_txt_l2
	mov	al, 0Dh ; CR
	stosb
	mov	al, 0Ah	; LF
	stosb
	jmp	short pr_hxd_txt_l1

pr_hxd_txt_ok:
	mov	al, 0
	stosb

	mov	si, hex_data_txt
	call	print_msg

	mov	si, dpb_hex_data
	jmp	short print_msg

;-----------------------------------------------------------------
; print_parameters subroutine
;-----------------------------------------------------------------

print_parameters:
	mov	al, [DPB_buffer]	; al = drive number
	push	ax
	mov	di, dpb_param_str
	call	conv_to_hex_num
	mov	al, '('
	stosb
	pop	ax
	add	al, 'A'
	stosb
	mov	al,':'
	stosb
	mov	al,')'
	stosb
	mov	ax, 20h
	stosw
	mov	si, drv_num_txt	
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	al, [DPB_buffer+4]	; al = sectors per cluster
	mov	di, dpb_param_str
	call	conv_to_dec_num
	mov	ax, 20h
	stosw
	mov	si, sec_per_clus_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	ax, [DPB_buffer+6]	; ax = reserved sectors
	mov	di, dpb_param_str
	call	conv_to_dec_num_w
	mov	ax, 20h
	stosw
	mov	si, res_sectors_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	ax, [DPB_buffer+9]	; ax = root dir entries
	mov	di, dpb_param_str
	call	conv_to_dec_num_w
	mov	ax, 20h
	stosw
	mov	si, root_dir_ents_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	ax, [DPB_buffer+11]	; ax = first data sector
	mov	di, dpb_param_str
	call	conv_to_dec_num_w
	mov	ax, 20h
	stosw
	mov	si, first_dat_sec_txt	
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	ax, [DPB_buffer+13]	; ax = highest clust num
	mov	di, dpb_param_str
	call	conv_to_dec_num_w
	mov	ax, 20h
	stosw
	mov	si, last_cluster_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	ax, [DPB_buffer+15]	; ax = FAT sectors
	;mov	al, [DPB_buffer+15]	; al = FAT sectors
	mov	di, dpb_param_str
	cmp	bp, 33		; dos version < 4.0 ?
	jnb	short pp_1	; no
	call	conv_to_dec_num	 
	jmp	short pp_2
pp_1:
	call	conv_to_dec_num_w
pp_2:
	mov	ax, 20h
	stosw
	mov	si, fat_sectors_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	;mov	ax, [DPB_buffer+16]	; ax = first dir sector
	mov	si, DPB_buffer+16
	cmp	bp, 33		; dos version < 4.0 ?
	jb	short pp_3	; yes
	inc	si
pp_3:
	lodsw
	mov	di, dpb_param_str
	call	conv_to_dec_num_w
	mov	ax, 20h
	stosw
	mov	si, first_dir_sec_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	;mov	al, [DPB_buffer+22]	; al = media ID byte
	mov	si, DPB_buffer+22
	cmp	bp, 33		; dos version < 4.0 ?
	jb	short pp_4	; yes
	inc	si
pp_4:	
	lodsb
	mov	di, dpb_param_str
	call	conv_to_hex_num
	mov	ax, 20h
	stosw
	mov	si, media_id_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	cmp	bp, 63		; dos version > 7 ? (FAT32 support)
	jnb	short pp_8	; yes

	cmp	bp, 30		; dos version < 3.0 ?
	jb	short pp_x	; yes

pp_8_not_extd:
	;mov	ax, [DPB_buffer+30]	; ax = free clusters
	mov	si, DPB_buffer+30
	cmp	bp, 33		; dos version < 4.0 ?
	jb	short pp_5	; yes
	inc	si
pp_5:
	lodsw
	mov	di, dpb_param_str
	cmp	ax, 0FFFFh	; unknown ?
	jb	short pp_6

	mov	ax,'FF'
pp_9_2:
	stosw	
	stosw
	mov	ax, (20h<<8)+'h'
	stosw
	jmp	short pp_7
pp_x:
	retn

pp_8:
	mov	bp, [DPB_buffer-2]
	cmp	bp, 61
	jb	short pp_8_not_extd
pp_8_extd:
	mov	si, DPB_buffer+31	; dx:ax = free clusters
	mov	di, dpb_param_str
	lodsw	
	mov	dx, ax
	lodsw
	mov	bx, ax
	and	bx, dx
	inc	bx
	jz	short pp_9_1		 ; - 1
	xchg	ax, dx
	call	conv_to_dec_num_dw
	jmp	short pp_7
pp_9_1:
	mov	ax, 'FF'
	stosw
	stosw
	jmp	short pp_9_2

pp_6:
	call	conv_to_dec_num_w
pp_7:
	mov	ax, 20h
	stosw
	mov	si, free_cnt_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	cmp	bp, 61
	jb	short pp_x
	
	mov	si, DPB_buffer+41	; dx:ax = 1st data sector
	lodsw	
	mov	dx, ax
	lodsw
	xchg	ax, dx
	mov	di, dpb_param_str
	call	conv_to_dec_num_dw
	mov	ax, 20h
	stosw
	mov	si, fat32_frst_sector_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	si, DPB_buffer+45	; dx:ax = max. clust num
	lodsw	
	mov	dx, ax
	lodsw
	xchg	ax, dx
	mov	di, dpb_param_str
	call	conv_to_dec_num_dw
	mov	ax, 20h
	stosw
	mov	si, fat32_max_clus_num_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	si, DPB_buffer+49	; dx:ax = FAT sectors
	lodsw	
	mov	dx, ax
	lodsw
	xchg	ax, dx
	mov	di, dpb_param_str
	call	conv_to_dec_num_dw
	mov	ax, 20h
	stosw
	mov	si, fat32_fat_sectors_txt
	call	print_msg
	mov	si, dpb_param_str
	call	print_msg

	mov	si, DPB_buffer+53	; dx:ax = root dir clust
	lodsw	
	mov	dx, ax
	lodsw
	xchg	ax, dx
	mov	di, dpb_param_str
	call	conv_to_dec_num_dw
	mov	ax, 20h
	stosw
	mov	si, fat32_root_cluster_txt
	call	print_msg
	mov	si, dpb_param_str
	jmp	print_msg

;-----------------------------------------------------------------
; convert to decimal number text 
;-----------------------------------------------------------------

	; AL = number (binary)

conv_to_dec_num:
	mov	cl, 10
	sub	ch, ch
cdn_1:
	xor	ah, ah
	div	cl

	xchg	ah, al
	add	al, '0'
	push	ax
	inc	ch
	or	ah, ah
	jz	short cdn_2
	xchg	ah, al
	jmp	short cdn_1
cdn_2:
	pop	ax
	stosb
	dec	ch
	jnz	short cdn_2
	retn

conv_to_dec_num_w:
	mov	cx, 10
	xor	bx, bx
cdnw_1:
	xor	dx, dx
	div	cx
	push	dx
	inc	bx
	or	ax, ax
	jnz	short cdnw_1
cdnw_2:
	pop	ax
	add	al, '0'
	stosb
	dec	bx
	jnz	short cdnw_2
	retn

conv_to_dec_num_dw:
	mov	cx, 10
	xor	si, si
cdndw_1:
	call	div32
	push	bx
	inc	si
	mov	bx, ax
	or	bx, dx
	jnz	short cdndw_1
cdndw_2:	
	pop	ax
	add	al, '0'
	stosb
	dec	si
	jnz	short cdndw_2
	retn

div32:
	mov	bx, ax
	mov	ax, dx
	xor	dx, dx
	div	cx              ; 0:AX/CX
	xchg	bx, ax
	div	cx              ; DX:AX/CX
	xchg	bx, dx
		; dx:ax = quotient
		; bx = remainder
	retn
	
;-----------------------------------------------------------------
; convert to hexadecimal number text 
;-----------------------------------------------------------------
	
	; AL =  number (binary)

conv_to_hex_num:
	mov	cl, 16
	xor	bh, bh
	xor	ah, ah
	div	cl
	mov	bl, ah
	add	bx, hex_chars
	mov	ch, [bx]
	sub	ah, ah
	div	cl
	xor	bh, bh
	mov	bl, ah
	add	bx, hex_chars
	mov	al, [bx]
	stosb
	mov	al, ch
	stosb
	mov	ax, (20h<<8)+'h'
	stosw
	retn

;-----------------------------------------------------------------
; DATA - Messages
;-----------------------------------------------------------------

RetroDOS_Welcome:
	db	0Dh, 0Ah
	db	'Retro DOS v5.0 IBMBIO.COM DPB TEST Utility'
	db	0Dh, 0Ah
	db	"v1.0.170424 (c) Erdogan TAN 2024"
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	0
RetroDOS_Usage:
	db	'Usage: getdpb <disk name>'
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"Example: getdpb C:"
	db	0Dh, 0Ah, 0

dpb_header:
	db	0Dh, 0Ah
	db	"DOS DRIVE PARAMETER BLOCK"
	db	0
dpb_ext_header:
	db	" (EXTENDED DPB)"
	db	0
crlf:
	db	0Dh, 0Ah, 0

drvnrdy_msg:
	db	0Dh, 0Ah
	db	"Drive not ready !"
	db	0Dh, 0Ah, 0

;-----------------------------------------------------------------
; DATA - Parameters
;-----------------------------------------------------------------

drv_num_txt:
	db	0Dh, 0Ah
	db	"Drive number        : ", 0

fat_sectors_txt:
	db	0Dh, 0Ah
	db	"FAT sectors         : ", 0

root_dir_ents_txt:
	db	0Dh, 0Ah
	db	"Root dir entries    : ", 0

sec_per_clus_txt:
	db	0Dh, 0Ah
	db	"Cluster sectors-1   : ", 0

res_sectors_txt:
	db	0Dh, 0Ah
	db	"Reserved sectors    : ", 0

first_dat_sec_txt:
	db	0Dh, 0Ah
	db	"Data start sector   : ", 0

media_id_txt:
	db	0Dh, 0Ah
	db	"Media ID byte       : ", 0

first_dir_sec_txt:
	db	0Dh, 0Ah
	db	"First dir sector    : ", 0

free_cnt_txt:
	db	0Dh, 0Ah
	db	"Free Clusters       : ", 0

last_cluster_txt:
	db	0Dh, 0Ah
	db	"Total clusters+1    : ", 0

fat32_frst_sector_txt:
	db	0Dh, 0Ah
	db	"FAT32 1st data sect : ", 0

fat32_max_clus_num_txt:
	db	0Dh, 0Ah
	db	"FAT32 max. clus num : ", 0

fat32_fat_sectors_txt:
	db	0Dh, 0Ah
	db	"FAT32 FAT sectors   : ", 0

fat32_root_cluster_txt:
	db	0Dh, 0Ah
	db	"FAT32 root dir clus : ", 0
	
hex_data_txt:
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db	"HEX DATA FOR DPB    : ", 0Dh, 0Ah, 0

hex_chars:
	db 	"0123456789ABCDEF", 0

dos_version:
	db	0Dh, 0Ah
	db	"DOS version : "
;version_str:
	;times	8 db 0
		
;-----------------------------------------------------------------
; DATA - uninitialized
;-----------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

version_str:
	resb 7

drive_number: resb 1

	resw 1
DPB_buffer:
	resb 62

dpb_param_str:
dpb_hex_data:
	resb 256

bss_clear_end:
bss_end:
