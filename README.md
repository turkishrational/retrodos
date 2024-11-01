# retrodos
MSDOS 1.25-2.11-3.3-5.0-6.22 and PCDOS 7.1 (derived, redeveloped) KERNEL (and COMMAND.COM) source code in NASM syntax

link: https://www.singlix.com/trdos/retrodos/index.html

www.youtube.com/@erdogantan

NOTE:  
== means approximately equal to (equivalent of) or "based on" MSDOS .. (but contains more optimized code than the original)  

Retro DOS kernel and command.com source code assembler: NASM ("Multi Section Binary File format" method)  

                                        construction: Retro DOS bootsector (loads combined kernel)
                                                      MSDOS.SYS (IO.SYS+MSDOS.SYS), COMMAND.COM
                                                      PCDOS.SYS (IBMBIO.COM+IBMDOS.COM), COMMAND.COM -Retro DOS v5 only-
                                         source code: retrodos5.s (includes ibmdos7.bin), ibmdos7.s, command7.s 
                                                      retrodos42.s (includes msdos6.bin), msdos6.s, command6.s
                                                      
                                         alternative: MSDOS/PCDOS bootsector (loads MSLOAD sectors)
                                                      same with MSDOS and PCDOS       
                                         source code: iosys6.s, msdos6.s, command6.s (for Retro DOS v4.2)
                                                      ibmbio7.s, ibmdos7.s, command7.s (for Retro DOS 5.0)  
                                                      
                             MSDOS and PCDOS assembler: MASM (segment group linked together)
                                construction: boot sector (loads MSLOAD sectors)
                                              IO.SYS (MSLOAD+BIOS+SYSINIT), MSDOS.SYS, COMMAND.COM 
                                              IBMBIO.COM (MSLOAD+BIOS+SYSINIT), IBMDOS.COM, COMMAND.COM
Retro DOS v1.0 == MSDOS 1.25  
Retro DOS v2.0 == MSDOS 2.11  
Retro DOS v3.0 == MSDOS 3.3 -not compatible with Windows 3.1-                                             
Retro DOS v3.1 == compact (a bit more improved than Retro DOS v3.0) -not compatible with Windows 3.1-                                           
Retro DOS v3.2 == MSDOS 3.3 + 32bit disk sectors support (like as in MSDOS 5.0) -not compatible with Windows 3.1-  
Retro DOS v3.3 == MSDOS 3.3 -Windows 3.1 compatible- (but this kernel does not support 32bit disk sectors)

Retro DOS v4.0 == MSDOS 5.0  
Retro DOS v4.1 == compact (a bit more improved than Retro DOS v4.0)  
Retro DOS v4.2 == MSDOS 6.22 (without DBLSPACE feature) -Multi Configuration Menu Support-

Retro DOS v5.0 == IBM PCDOS 7.1 (with bugfixes) -FAT32 file system support-

Retro DOS v3.3 to 5.0 are full compatible with Windows 3.1.

Retro DOS v5.0 has all features of PCDOS 7.1.
       (also it contains 10 bugfixes, 9 IBM, 1 Microsoft code bugfix)
       
                 (If a programs runs on PCDOS 7.1, it also will run on Retro DOS v5.0)
       
                 ((Only exception: Program must not stand on original IBMDOS.COM and IBMBIO.COM order and layout.))

Erdogan Tan - Istanbul - 2024
