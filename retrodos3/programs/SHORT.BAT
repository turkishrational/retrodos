ECHO OFF
CLS
ECHO    
ECHO    SHORT -- switch to short form of names.
ECHO    
ECHO    This operation will rename the programs 
ECHO    in the Norton Utilities set, from the
ECHO    long form of their names, to the short.
ECHO    
ECHO    The short names are two letters, 
ECHO      easier to type.
ECHO    The long names are six to eight letters,
ECHO      easier to remember.
ECHO    
ECHO    Note: for safety, WIPEDISK and WIPEFILE
ECHO      do not have short names.
ECHO    
ECHO Press BREAK to halt this operation, or,
PAUSE
ECHO ON
RENAME  DIRSORT.COM  DS.COM
RENAME  DISKTEST.COM DT.COM
RENAME  FILEATTR.COM FA.COM
RENAME  FILEFIND.COM FF.COM
RENAME  FILESIZE.COM FS.COM
RENAME  LISTDIR.COM  LD.COM
RENAME  LINEPRNT.COM LP.COM
RENAME  NORTON.BAT   NU.BAT
RENAME  SCRATR.COM   SA.COM
RENAME  SYSINFO.COM  SI.COM
RENAME  TIMEMARK.COM TM.COM
RENAME  TEXTSRCH.COM TS.COM
RENAME  VOLABEL.COM  VL.COM
