@echo off
a:
cd\
if not exist ibmbio.com goto nobio
if not exist ibmdos.com goto nodos
if not exist command.com goto nocmd
if not exist format32.com goto nofmt
if not exist attrib.exe goto noatt



:main
cls
Echo MAKEBOOT: formats drive C:, all data on drive C: will be lost
pause

format32 c: /q
if errorlevel 1 goto fmterr
xcopy ibmbio.com c:\
if errorlevel 1 goto copyerr
xcopy ibmdos.com c:\
if errorlevel 1 goto copyerr
xcopy command.com c:\
if errorlevel 1 goto copyerr
attrib c:\ibmbio.com +r +s +h
attrib c:\ibmdos.com +r +s +h

goto end



:nobio
Echo  Error: Missing IBMBIO.COM
Echo Action: Ensure image FAT32B.DSK is in drive A:
goto errend

:nodos
Echo  Error: Missing IBMDOS.COM
Echo Action: Ensure image FAT32B.DSK is in drive A:
goto errend

:nocmd
Echo  Error: Missing COMMAND.COM
Echo Action: Ensure image FAT32B.DSK is in drive A:
goto errend

:nofmt
Echo  Error: Missing FORMAT32.COM
Echo Action: Ensure image FAT32B.DSK is in drive A:
goto errend

:noatt
Echo  Error: Missing ATTRIB.EXE
Echo Action: Ensure image FAT32B.DSK is in drive A:
goto errend


:fmterr
Echo  Error: FORMAT32 failed on drive C:
Echo Action: Ensure image FAT32B.DSK is in drive A:
goto errend

:copyerr
Echo  Error: Copying system files
Echo Action: Ensure image FAT32B.DSK is in drive A:
Echo Action: System files may be hidden, try DIR A:\ /A
goto errend

:errend
Echo Makeboot did not complete successfully!
goto exit
:end
Echo Makeboot finished successfully!
Echo Remove diskette, then restart system.

:exit
