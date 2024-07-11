@echo off
rem make FAT32 DOS 7.1 boot sector for GPT
jwasm -bin -nologo -Fl=dos71bsg.lst -Fo=dos71bsg.bin -DMAKEGPTBS dos71bs.asm
rem
rem make "legacy" FAT32 DOS 7.1 boot sector for MBR
rem jwasm -bin -nologo -Fl=dos71bsm.lst -Fo=dos71bsm.bin dos71bs.asm
