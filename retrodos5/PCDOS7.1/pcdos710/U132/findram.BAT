
@echo off

rem * Program checks all existing drives for the RAMDRIVE "label"
call findram.exe

rem * Findram returns drive as number, branch accordingly
for %%i in (3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27) do if errorlevel %%i goto DRIVE%%i
goto end

rem * Convert RAM drive number to drive letter
:DRIVE3
  SET RAMDSK=C:
  goto end
:DRIVE4
  SET RAMDSK=D:
  goto end
:DRIVE5
  SET RAMDSK=E:
  goto end
:DRIVE6
  SET RAMDSK=F:
  goto end
:DRIVE7
  SET RAMDSK=G:
  goto end
:DRIVE8
  SET RAMDSK=H:
  goto end
:DRIVE9
  SET RAMDSK=I:
  goto end
:DRIVE10
  SET RAMDSK=J:
  goto end
:DRIVE11
  SET RAMDSK=K:
  goto end
:DRIVE12
  SET RAMDSK=L:
  goto end
:DRIVE13
  SET RAMDSK=M:
  goto end
:DRIVE14
  SET RAMDSK=N:
  goto end
:DRIVE15
  SET RAMDSK=O:
  goto end
:DRIVE16
  SET RAMDSK=P:
  goto end
:DRIVE17
  SET RAMDSK=Q:
  goto end
:DRIVE18
  SET RAMDSK=R:
  goto end
:DRIVE19
  SET RAMDSK=S:
  goto end
:DRIVE20
  SET RAMDSK=T:
  goto end
:DRIVE21
  SET RAMDSK=U:
  goto end
:DRIVE22
  SET RAMDSK=V:
  goto end
:DRIVE23
  SET RAMDSK=W:
  goto end
:DRIVE24
  SET RAMDSK=X:
  goto end
:DRIVE25
  SET RAMDSK=Y:
  goto end
:DRIVE26
  SET RAMDSK=Z:
  goto end
:DRIVE27
  cls
  echo.
  echo Findram Error:  Unable to find the RAM drive.
  echo.
  SET RAM_ERROR=TRUE
  goto end

:END
