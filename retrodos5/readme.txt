Retro DOS v5.0 (Modified PCDOS 7.1) Source Code (Not ready yet...) 27/12/2023 - Erdogan Tan
Last Update: 27/6/2024 (PCDOS 7.1 COMMAND.COM is running with Retro DOS v5 PCDOS.SYS -includes the new IBMDOS7.BIN as part of Retro DOS v5-)
        ((But original IBMDOS.COM is needed for now... because 'ibmdos7.s' source code has defects and 3/4 of it has been verified-corrected.))
        (((Recent test diskette image: rd5fd_27_6_2024.zip))) (((Usable diskette image: R5.IMG, 8/5/2024 -original IBMDOS.COM-)))
Previous Update: 4/5/2024 (bootable disk image, IBMBIO.COM and IBMDOS.COM source code, retrodos5.s, PCDOS.SYS, boot sectors, installation/format programs) 

*** Tested on a Real Computer (runs Windows 7) with a disk contains FAT32 and FAT16 logical drives in extended dos partition) ****
((Retro DOS v5 recognizes all of logical -FAT12, FAT16 and FAT32- DOS drives in an extended dos partition and on USB -external- disks.. in addition to primary FAT32, FAT16, FAT12 dos partitions))
Windows 3.1 and AutoCAD R12 is running on Retro DOS v5 (on the Real Computer) without a problem (but QEMU has a problem with Windows 3.1 MSDOS prompt)

NOTE: Windows 3.1 File Manager recognizes FAT32 disks and read/write is OK for FAT32 fs.. But Windows 3.1 FAT32 startup fails, so.. Windows 3.1 must be installed on a FAT16 fs. (I don't know the reason. Perhaps, Windows 3.1 startup code is bypassing PCDOS 7.1 kernel or it uses the old DEVICE IO funcs.) 
