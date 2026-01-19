ECHO OFF
CLS
ECHO    
ECHO    LONG -- switch to long form of names.
ECHO    
ECHO    This operation will rename the programs 
ECHO    in the Norton Utilities set, from the
ECHO    short form of their names, to the long.
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
RENAME  DS.COM DIRSORT.COM  
RENAME  DT.COM DISKTEST.COM 
RENAME  FA.COM FILEATTR.COM 
RENAME  FF.COM FILEFIND.COM 
RENAME  FS.COM FILESIZE.COM 
RENAME  LD.COM LISTDIR.COM  
RENAME  LP.COM LINEPRNT.COM 
RENAME  NU.BAT NORTON.BAT
RENAME  SA.COM SCRATR.COM
RENAME  SI.COM SYSINFO.COM  
RENAME  TM.COM TIMEMARK.COM 
RENAME  TS.COM TEXTSRCH.COM 
RENAME  VL.COM VOLABEL.COM  
