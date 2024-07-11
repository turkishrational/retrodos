
 1. About
 
 DOS71BSG is a bootsector for MS-DOS 7.1 FAT32 partitions on a
 GPT-partitioned disk. Actually, it's 3 sectors, size 1536 bytes.

 
 2. Create the Binaries

 Get JWasm and then run mdos71bs.bat and mresti31.bat.
 After that step, files dos71bsg.bin and RestI31.SYS should exist.
 File dos71bsg.bin is the FAT32 boot sector for GPT-partitioned disks and
 RestI31.SYS is a DOS device driver, used to restore the original Int 13h
 handler temporarily modified by dos71bsg.bin.

 
 3. Usage
 
 a) create a FAT32 partition on the disk - must be located entirely in the
    first 2 TB of the disk!
 b) use a disk editor (WDe) to save the bootsector to a file.
 c) copy the BPB of the saved sector ( bytes 0x0B-0x59 ) into file dos71bsg.bin,
    at the very same location.
 d) if unsure, use WDe to check if the modified dos71bsg.bin looks correct.
 e) then write that file to the partition's boot sector ( 3 sectors ).
 f) ensure that MS-DOS 7.1 IO.SYS is in the partition's root directory.
 g) use grub-mkconfig to add the partition to grub's boot menu.
 h) add RestI13.sys to config.sys ( should be the first DEVICE= entry ).
 
 No risk, no fun.
