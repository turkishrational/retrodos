Retro DOS v5.0 (Modified PCDOS 7.1) Source Code (Not ready yet...) 15/12/2023 - Erdogan Tan

Files: (last status in 28/06/2024)

(Modified) PCDOS 7.10 IBMDOS.COM source = ibmdos7.s (kernel) -100% completed but it needs to be verified- (being verified!!!)
           ((PCDOS 7.1 COMMAND.COM is running.. WINDOWS 3.1 is running but it's msdos prompt has a problem...
             3/4 of 'ibmdos7.s' source code has been verified.. I need a bit additional time to correct the source code defects.)) 
           (((RETRO DOS v5 kernel code has IMPORTANT BUGFIXES which are fixing original PCDOS 7.1 IBMDOS.COM BUGs)))
(Modified) PCDOS 7.10 IBMBIO.COM source = ibmbio7.s (bios) -100% completed but it needs to be verified-
Combined Kernel (Modified PCDOS 7.1) source = retrodos5.s -100% completed but it needs to be verified-
Retro DOS v5 kernel name: PCDOS.SYS (Final Retro DOS version with FAT32 fs support) < 82KB
Retro DOS v4.2 kernel name: MSDOS.SYS (Modified MSDOS 6.22 without DRVPACE or DOUBLESPACE) < 71KB

Development method:
Upgrading (Modifying) MSDOS 6.22 (Retro DOS v4.2) source code by using (comparing with) disassembled
PCDOS 7.1 kernel and Windows ME IO.SYS code (disassembler: HEX-RAY IDA PRO FREE Version 7) line by line.

Editor: Windows NOTEPAD, Windows 7 (and Windows 10)
Assembler: NASM v2.15 for Windows
