Retro DOS 4.? versions by Erdogan Tan
--------------------------------------

1) Retro DOS v4.0 - Rewritten MSDOS 5.0 (& 6.21) kernel by Erdogan Tan
		    in Istanbul in 2019 as first draft -failed- then in 2022
		    -successful- by using NASM with multisection binary file
		    format. Retro DOS v4.0 kernel source code is based on
		    Retro DOS v3.2 kernel source code and MSDOS 6.0 source
		    code and disassembled MSDOS 5.0 (and MSDOS 6.21) kernel.	
		    MSDOS.SYS part of the kernel is completed in December
		    2022 but IO.SYS part is (more) optimized in January 2023.
	            
		    Major difference between Retro DOS & MSDOS:
		    Retro DOS boot sector code loads Retro DOS kernel
		    as single binary/kernel file. IO.SYS is a part of 
		    MSDOS.SYS (bios+sysinit+kernel).

		    Trick: Retro DOS v4 combined kernel (IO.SYS+MSDOS.SYS)
		    is named as 'MSDOS.SYS' (>64KB) and it can be launched by
		    using a loader as fake 'IO.SYS' (1536 bytes). So, it is
		    possible to run Retro DOS v4 kernel with MSDOS boot sector
		    (on bootable MSDOS 5.0 disk) only by replacing
		    (overwriting) original MSDOS IO.SYS and MSDOS.SYS with
		    Retro DOS kernel loader 'IO.SYS' and the kernel 'MSDOS.SYS'.

		    Retro DOS v2 (v3) boot sector loads Retro DOS kernel 
		    MSDOS.SYS at 1000h:0000h while MSDOS (v5.0) boot sector
		    loads IO.SYS at 0070h:0000h. Retro DOS v4 kernel loader
		    (fake 'IO.SYS') is loaded by MSDOS boot sector at 70h:0
		    and then it loads Retro DOS kernel 'MSDOS.SYS' at
		    1000h:0. And then.. Retro DOS kernel's INIT code relocates
		    it's IO.SYS and MSDOS.SYS sections just as how original 
		    MSDOS IO.SYS INIT code does relocation.

		    NOTEs: 
		    1) Retro DOS v4 kernel binary is shorten (<) then original			 					
		       MSDOS kernel because of optimized source code.
	  	       (!while Retro DOS is almost same with MSDOS!)
		    2) Retro DOS v4 kernel source is in only two assembly files.
			(retrodos4.s -or iosys5.s- and msdos5.s)

		    3) RETRO DOS 4 IS FULL COMPATIBLE WITH WINDOWS 3.1
			(Windows 3.1 enhanced mode runs on Retro DOS v4)				

2) Retro DOS v4.1 - Optimized code.
		    Some (MSDOS 5.0 MSDOS.SYS) patches are removed.
	            Smaller kernel file size.

3) Retro DOS v4.2 - Kernel source code is based on MSDOS 6.21 source code
		    (MSDOS 6.0 source and disassembled MSDOS 6.21 kernel
		    and IO.SYS) instead of disassembled MSDOS 5.0 code.

		    But v4.2 kernel doesn't contain DOUBLESPACE or DRVSPACE code.
		    (DOUBLESPACE or DRVSPACE is not applicable and
		    also they are not necessary nowadays.
		    Disk compression is out.)	
		    
		    MSDOS.SYS part/side is nearly same with v4.1 (MSDOS 5.0)
		    except minor differences in details.

		    IO.SYS and SYSINIT part is different than Retro DOS v4.1
		    and almost same with MSDOS 6.21 IO.SYS except optimizations.
		    (MULTI CONFIGURATION -config.sys menus- feature!)

		    Retro DOS v4.2 kernel reports itself as MSDOS 6.22.
		    --------------------------------------------------------
		    Retro DOS v4.0 and v4.1 report theirselves as MSDOS 5.0.

		    There is a private/hidden 'get/write true version'
		    (Int 21h OEM) function (in the kernel) which returns
		    (displays) true/real Retro DOS version.
		    (This feature is available since Retro DOS v1.0)

		    Retro DOS v4.2 kernel does not contain MSDOS 6.?
		    MSDOS.SYS patches except 'exepack' patch.

		    RETRO DOS VERSION 4.2 (Modified MSDOS 6.22)
		    IS FULL COMPATIBLE -FOR RUNNING- WITH WINDOWS 3.1			  		
	 			 					
MSDOS.SYS : 
	Kernel binary (RETRODOS.SYS), Retro DOS v2.0 (& v3.0) boot sector
	code loads and starts this kernel file (in root directory) at 1000h:0000h
	then Retro DOS kernel moves itself to segment 0070h as IO.SYS (BIOS)
	and loads SYSINIT code (SYSINIT.BIN inside MSDOS.SYS) at top of memory
	and SYSINIT loads MSDOS kernel (MSDOS.BIN inside MSDOS.SYS) at the
	first proper location after BIOS (IO.SYS) code and data. 	 	 

retrodos4.s : 
	Main file (equivalent of IO.SYS, includes MSDOS.SYS via incbin directive)
	(The asssembler -NASM- adds 'msdos5.bin' binary file to retrodos4.s code.)

	Assembled OUTPUT/binary is 'MSDOS.SYS' (kernel, >64KB)	

iosys5.s :
	This file contains 'IO.SYS' source code just as MSDOS IO.SYS.
	It is usable with MSDOS 5.0 boot sector (instead of Retro DOS boot sector).
	(retrodos4.s is same with iosys5.s except msloader section and the init part)

	Assembled OUTPUT/binary is 'IO.SYS' (the 1st entry the FAT fs root directory)	
 	
msdos5.s : 
	Kernel file (MSDOS.SYS file of MSDOS 5.0).
	Assembled OUTPUT/binary is
	       'MSDOS5.BIN' (is included by Retro DOS v4 combined kernel 'MSDOS.SYS')
	           or		
	       'MSDOS.SYS' (the 2nd entry the FAT fs root directory)	

	It is included by Retro DOS v4 kernel (combined) file as 'MSDOS5.BIN'.

rd4load.s :
	Retro DOS v4 kernel loader for MSDOS (5.0) boot sector. Fake IO.SYS.	
	(MSDOS boot sector loads the 1st 3 sectors of IO.SYS at 0070h:0000h)

	Assembled OUTPUT/binary is 'IO.SYS' (1536 bytes, fake IO.SYS file)
			       (is used with Retro DOS kernel 'MSDOS.SYS', >64KB)

Assembler usage:
	nasm msdos5.s  -l msdos5.txt  -o MSDOS5.BIN -Z error.txt
							 (for RETRODOS boot sector)
	nasm msdos5.s  -l msdos5.txt  -o MSDOS.SYS  -Z error.txt
							 (for MSDOS boot sector)
	nasm iosys5.s  -l iosys5.txt  -o IO.SYS     -Z error.txt
							 (for MSDOS boot sector)
	-fake IO.SYS-
	nasm rd4load.s -l rd4load.txt -o IO.SYS     -Z error.txt
						         (for MSDOS boot sector)
					    	(and for Retro DOS combined kernel)
	-combined kernel-
	nasm retrodos4.s -l retrodos4.txt -o MSDOS.SYS -Z error.txt
							 (for RETRODOS boot sector)
					(and for MSDOS boot sector with fake IO.SYS)

Retro DOS 4.1 files:
	iosys51.s, retrodos41.s, msdos51.s, rd4load.s

Retro DOS 4.2 files:
	iosys6.s (IO.SYS*)
	retrodos42.s (MSDOS.SYS) ((includes MSDOS6.BIN via 'incbin' directive))
	msdos6.s (MSDOS6.BIN or MSDOS.SYS*)
	rd4load.s (IO.SYS**)
	
	SYS* files are used together as equivalent of MSDOS 6.22 IO.SYS & MSDOS.SYS 
 	SYS** file is used for loading Retro DOS kernel via MSDOS boot sector

Emulator usage:
	Floppy disk and hard disk image file (with Retro DOS v2.0 & v3.0 boot sector)
	is mounted as Windows (XP,7,10) drive (floppy or hard disk) and then
	MSDOS.SYS (Retro DOS v4.? kernel) file is copied onto root directory of 
	that virtual disk drive. (For example: 'R4.IMG' as 1.44MB floppy disk image.)
	And then original MSDOS 5.0 or Retro DOS v4.0 COMMAND.COM file is copied onto
	root directory of same disk image.
	And required (MSDOS 5.0 compatible) files and programs are copied onto same or
	another disk image which will be used with emulator program (mounted image
 	must be unmounted before using it in emulator).

	Note: For Retro DOS v4.2 kernel, MSDOS 6.22 COMMAND.COM can be used (only).

Emulator programs (for running Retro DOS v4.?) :
	BOCHS, VIRTUAL BOX, QEMU etc. 

Using Retro DOS kernel on real computer:
	Retro DOS v4.? will run on a real IBM PC AT compatible computer if it has a
	standard floppy disk drive or it has a standard ATA (PATA) harddisk 
	which contains MSDOS partition (FAT12 or FAT16 CHS file system). 
	But the boot disk must be formatted or boot sector must be updated
	by Retro DOS v2.0 (v3.0) format (or boot sector updating) 
	utility before copying kernel file and COMMAND.COM file onto root directory
	of the primary (and active) MSDOS partition.

	Retro DOS v4.? floppy disk image (for example: 'R4.IMG' file) must be written onto
	a real 1.44MB floppy disk (by using 'rawwrite' or similar 'image to disk' program).
	And the computer must be restarted/rebooted with that floppy disk.

	(To use bootable hard disk is possible but booting by floppy disk is very easy for
	 testing Retro DOS v4.? kernel.)

	If Retro DOS 4 is started by floppy disk boot..
	   It recognises primary DOS partitions (1,4,6)
	   Only the boot drive must have Retro DOS boot sector, Retro DOS (MSDOS.SYS)
	   kernel and MSDOS 5.0 or Retro DOS v4.0 COMMAND.COM command interpreter file.
	   It will be enough if other disks are any kind of MSDOS 5.0 compatible.

	Note: Retro DOS v4.2 kernel is used with MSDOS 6.22 COMMAND.COM and 
	      other MSDOS 6.22 files. (Not MSDOS 5.0!)

TRICK:	A bootable MSDOS 5.0 (or 6.22) disk(ette) can be used to run Retro DOS v4
	(v4.2 as MSDOS 6.22) only by overwriting IO.SYS file with Retro DOS v4 kernel
	loader file (fake) 'IO.SYS' -1536 bytes- and overwriting MSDOS.SYS file
	with Retro DOS v4 kernel MSDOS.SYS (file size > 64KB).
	
	or.. IO.SYS (iosys5.s, iosys6.s) 
	     and MSDOS.SYS (msdos5.s, msdos6.s) can be built as separate files.
	
	The 1st 2 entries of the bootable MSDOS disk's (FAT fs) root directory
	is for 'IO.SYS' and then 'MSDOS.SYS'. Deleting original SYS files is not
	proper and it can cause to 'non system disk' error. So, overwriting is 
	proper to keep these/both directory entries (in order) as unchanged.

Details and relevant files are existing at:
        https://www.singlix.com/trdos/index.html
        https://www.singlix.com/trdos/specs.html
        https://www.singlix.com/trdos/retrodos/index.html
        https://www.singlix.com/trdos/retrodos4.html
        https://www.singlix.com/trdos/retrodos.html

***

Retro DOS v1 to v5 web pages (and more source):
https://www.singlix.com/trdos/retrodos/index.html
https://www.singlix.com/trdos/retrodos.html

The NEXT VERSION:

	Retro DOS v5.0
		wih a new boot sector
		as IBM PCDOS 7.1 clone/derivation (FAT32 FS support)
		with 'PCDOS.SYS' kernel file name. (instead of 'MSDOS.SYS')

06/01/2023 - Erdogan Tan ((09/02/2023))

Link: https://www.singlix.com/trdos/retrodos/index.html

