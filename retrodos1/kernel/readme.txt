Retro DOS Versions by Erdogan Tan (04/08/2019)

Erdogan Tan	Microsoft
-------------   ----------------------
ready:
Retro DOS 1.0 - MSDOS 1.25 (PCDOS 1.1)
Retro DOS 2.0 - MSDOS 2.11 (PCDOS 2.10)
Retro DOS 3.0 - MSDOS 3.3  (PCDOS 3.3)
Retro DOS 3.1 - MSDOS 3.3 optimized source code (compact kernel, 49KB)
Retro DOS 3.2 - MSDOS 3.3 (>32MB disk access, FAT 16 Partition ID 6, MSDOS 6.0 code)
in progress:
Retro DOS 3.3 - Fake MSDOS 5.0 (MSDOS 6.0 system calls on Retro DOS 3.2 construction)
***
in sequence: 
Retro DOS 4.0 - MSDOS 6.0 (without DoubleSpace, it will report itself as MSDOS 6.22.) 
***
future (final):
Retro DOS 5.0 - PCDOS 7.1 (by IBM, FAT 32 support)

*****

MSDOS assembly source code syntax & assembler: MASM (version 1.0 to 6.14)
Retro DOS assembly source code syntax & assembler: NASM 2.11-2.14
	
***
(Boot methods: Different boot sectors and different kernel loading code.. not as MSDOS!)

MSDOS KERNEL construction: 
			IO.SYS or IBMBIO.COM as BIOS (1) & includes SYSINIT code (2)
			MSDOS.SYS or IBMDOS.COM as kernel file (3)

Retro DOS kernel construction:
			MSDOS.SYS kernel file (includes BIOS and SYSINIT code)
			(1+2+3)	
			
Retro DOS v5.0 (FAT32 FS) kernel file: RETRODOS.SYS.

Note: Retro DOS v1,2,3,(4) are running as Microsoft DOS (1.25, 2.11, 3.3, 5.0, 6.22)
      Retro DOS v5 will run as IBM PCDOS 7.1 (of course it will not be as PC DOS 7.1.)	

------------------------------------------------------------------------------------------- 

!!! OPEN/FREE SOURCE CODE by Erdogan Tan !!! You can use my own files by reqarding my name
as original developer/writer of retro msdos (or retro pcdos) kernel... All of Retro DOS 
source code is based on Microsoft DOS source code.. so.. professional purposed using of 
this retro dos code may cause some license problems. Retro DOS is as MSDOS while running
but it is not MSDOS as written... (It is derived from MSDOS source code but it is not a
copy of MSDOS source code. There are many modifications on original msdos procedures.
Every procedure has been checked and then it has been changed if it is good to change!)
Of course, i do not claim Retro DOS is exactly same with or better than original Microsoft
DOS. But it is running at the result of more simple source code redesign in NASM syntax
while it is running as like as original MSDOS. 
(I have written Retro DOS versions on windows 7 computer by using NASM as simple, as easy.)

-------------------------------------------------------------------------------------------

NOTE:  These (Retro DOS) works are -have been started- for TRDOS 386 (32 bit DOS, 386-DOS
       by Erdogan Tan, for 80386 cpu, for protected mode running) as file system development
       pre-works in order to improve FAT file system handling methods of TRDOS 386.
       (TRDOS 386 is also pre-work of my 32 bit & 64 bit SINGLIX operating system project.)
	
time line:
1) TRDOS v1 first attempt - 2005
2) TRDOS v1 2009-2011
3) Retro UNIX 8086 (from UNIX v1 PDP11 source code) - 2013/2014
4) Retro UNIX 386 v1 (from Retro UNIX 8086 v1) - 2014/2015
5) TRDOS 386 (v2, construction is mix of TRDOS v1 and Retro UNIX 386 v1.1)
	- Protected mode, process, paging, file descriptors, executing/running,
	  open,close,read,write, interrupts, 32 bit bios methods from Retro UNIX 386 v1.1.
	- Logical drive construction, FAT handling, command interpreter, system call details
	  from TRDOS v1.0 (8086).
	- Timer functions, signal response byte, call back (& release), video memory mapping,
	  audio functions as new original methods for replacing MSDOS & ROMBIOS features.  

6) Retro DOS v1, v2, v3, v4, v5 --> TRDOS (TRDOS 386) v2.0.2 --> TRDOS (TRDOS 386) v2.1

	- TRDOS 386 v2.1 FAT file systems and buffer handling <-- Retro DOS v5.
	
7) Retro UNIX v2 <-- (TRDOS 386 v2.1 + Retro UNIX v1)
8) TRDOS 386 v3 (final for 386-DOS)
9) SINGLIX v1 32 bit kernel <-- (TRDOS 386 v3 + Retro UNIX v2) 	   
10) Retro UNIX 386 v3 and/or Retro UNIX x64 v3
11) SINGLIX v2 64 bit kernel <-- Singlix v1 32 bit kernel + Retro UNIX v3 kernel
		

After Retro DOS v5.0 ..
	I will write (I will continue to write) TRDOS 386 v2.0.2
	with new (32 bit) FAT file system recognization and access (R&W) code.


Erdogan Tan - 04/08/2019 (August 4, 2019)  ((09/02/2023))

Link: https://www.singlix.com/trdos/retrodos/index.html
