Retro DOS v3 boot sectors,
	     floppy disk images and hard disk images
	     and hard disk boot sector update utility..

	     all of these are compatible with Retro DOS v4

	     (KERNEL and COMMAND.COM and some programs are not compatible;
	      so, files in Retro DOS v3 disk(ette)s must be updated for
	      using them with Retro DOS v4 kernel.)

To run Retro DOS 4:

	If floppy disk or hard disk (dos) partition has	 				
		Retro DOS v3 (v2) boot sector...

		Retro DOS v4 combined(*) kernel (with 'MSDOS.SYS' name)
		must be copied into the root directory.

		And COMMAND.COM (MSDOS 5.0 or Retro DOS v4 COMMAND.COM)
		must be copied into the root directory.
		
		(copying autoexec.bat, config.sys and himem.sys etc
		is optional.)

	If floppy disk or hard disk (dos) partition has

		Microsoft DOS v5.0 (or compatible) boot sector
		-I mean the boot sector of a bootable/system disk-

		(the first 2 files of the root directory entries
		must be 'IO.SYS' and MSDOS.SYS')

		Retro DOS v4 IO.SYS (**)
		and then Retro DOS v4 MSDOS.SYS (***)
		must be copied (as overwrite) into the root directory,
		instead of original MSDOS 5.0 IO.SYS and MSDOS.SYS.

	(*) Retro DOS v4 combined kernel 'MSDOS.SYS' (RETRODOS.SYS)
		(>64KB) consists of 'IO.SYS' and 'MSDOS.SYS'.

		Retro DOS v4 kernel name must be 'MSDOS.SYS' 
		because Retro DOS boot sector code searchs for
		and loads it. (MSDOS.SYS = IO.SYS+MSDOS.SYS)

	(**) equivalent/clone of original MSDOS 5.0 'IO.SYS'
	(***) equivalent/clone of original MSDOS 5.0 'MSDOS.SYS' 

Special option (trick):

	If the boot sector is (original) MSDOS 5.0 boot sector..

	And if Retro DOS kernel is combined kernel (IO.SYS+MSDOS.SYS)..
	
	The fake IO.SYS (Retro DOS kernel loader) is copied into root dir
	(by owerwriting original IO.SYS).
	Then, Retro DOS 4 kernel MSDOS.SYS (>64KB) is copied into root dir
	(by overwriting original MSDOS.SYS)

	Retro DOS kernel loader (is loaded at 70h:0) takes the startup
	job and disk parameters from MSDOS boot sector and loads
	Retro DOS v4 kernel 'MSDOS.SYS' at 1000h:0 -with same
	parameters- just as Retro DOS boot sector has loaded it.

	(Then Retro DOS kernel's init code moves kernel to 70h:0 addr.)


	--- Erdogan Tan - 21/12/2022 ---
				
