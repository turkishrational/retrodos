CLS
REM
REM      This is a short demonstration of some of the 
REM      features of the Norton Utilities, Version 3.
REM   
TIMEMARK START /N        (TM acts as a stopwatch, timing computer operations)
BEEP             ("beep" is a small yet handy feature of the Utilities)
PAUSE
TIMEMARK STOP  /N             (TM's stopwatch will now show the elapsed time)
REM                                You paused for that long---
FILESIZE *.BAT    (FS-FileSize tells you useful information about file sizes)
PAUSE
FILEATTR *.BAT         (FA-FileAttribute will tell you about file attributes)
PAUSE
DISKTEST /F   (DT-DiskTest will test your disks to see that they're readable)
PAUSE
SYSINFO  (SI-System Information gives you interesting info on your computer)
PAUSE
ECHO OFF
CLS
ECHO     
ECHO     The next step demonstrates the main
ECHO     Norton Utilities program, called NU-Norton. (The original Norton
ECHO     has been renamed NORTOND2 & is called with the argument D2 on the
ECHO     command line to invoke the MS-DOS compatible screen driver D2. The
ECHO     default driver D0 uses direct IBM screen addressing & D1 uses IBM
ECHO     interrupts. The keys command loads a keytable which redefines the 
ECHO     cursor keys to give the IBM key codes. The arrow up and arrow down key
ECHO     when shifted will page up and down in the appropriate places.
PAUSE
CLS
ECHO     To see some of the most interesting features of NU,
ECHO     make a note of the following keys, and press them 
ECHO     in the order shown:
ECHO     
ECHO      KEY     WHAT HAPPENS WHEN YOU PRESS IT
ECHO     =====    ======================================================
ECHO       2      (to go to Menu 2, the exploration menu)
ECHO       2      (to go to Menu 2.2, to see information about your disk)
ECHO     space    (to return to Menu 2)
ECHO       3      (to go to Menu 2.3, to see a map of disk space)
ECHO     space    (to return to Menu 2)
ECHO       6      (to see information about the files)
ECHO     spaces   (to go on to see about other files)
ECHO     
ECHO     
ECHO ON
PAUSE
NORTON
REM   That ends the Norton Utilities demonstration
