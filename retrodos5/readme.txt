Retro DOS v5.0 (Modified PCDOS 7.1) Source Code (Ready!) 12/07/2024 - Erdogan Tan
Last Update: 10/07/2024 (PCDOS 7.1 COMMAND.COM is running with Retro DOS v5 PCDOS.SYS -includes the new IBMDOS7.BIN as part of Retro DOS v5-)

*** Tested on a Real Computer (runs Windows 7) with a disk contains FAT32 and FAT16 logical drives in extended dos partition) ****
((Retro DOS v5 recognizes all of logical -FAT12, FAT16 and FAT32- DOS drives in an extended dos partition and on USB -external- disks.. in addition to primary FAT32, FAT16, FAT12 dos partitions))
Windows 3.1 (on FAT16 fs) and AutoCAD R12 (on FAT32 or FAT16 fs) are running on Retro DOS v5 (on the Real Computer) without a problem
(Windows 3.1 must be run with 'win /s' switch in standard mode. Windows can run in enhanced mode but after return to DOS, directory listing gives
FAT error, changing to root directory corrects this fault.)

NOTE: Windows 3.1 File Manager recognizes FAT32 disks and read/write is OK for FAT32 fs.. But Windows 3.1 FAT32 startup fails, so.. Windows 3.1 must be installed on a FAT16 fs. (I don't know the reason. Perhaps, Windows 3.1 startup code is bypassing PCDOS 7.1 kernel or it uses the old DEVICE IO funcs.) 
