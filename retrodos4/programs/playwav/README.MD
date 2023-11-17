ICH AC97 .wav player for DOS
----------------------------
Full Functional -complete- 16 bit (real mode) DOS Program (in 11/2023): (for playing .. 8bit, 16bit, 8-48kHZ, mono-stereo WAV files) playwav2.asm, PLAYWAV2.COM (included asm files: ich_wav.asm, ac97.asm, ac97.inc)

PLAYWAV2 music -wav file- playing Method: tuneloop, double/2 (half) buffer -switch/swap- method, PCM OUT CIV-LVI and STatus register handling.

The program displays active (in use) buffer as '1' or '2' (on the top left corner of the screen with red color) while playing music ('1' and '2' continuously changes) and waits for a keypress (int 16h, ah=01h) to stop/exit before the end of the WAV file (it will play all of the audio samples and then it will exit/return at the end of the WAV file if the user does not press any key before).

Playwav2 is proper for playing 1-100 MB (very small to very big, huge) wav (music) files... (uses 2*64KB buffers for 16 bit samples and also uses a temporary 32KB buffer for converting 8 bit and mono samples to 16 bit stereo samples)

PLAYWAV2.COM can be tested with 256-512MB FAT16 (Retro DOS v4, MSDOS 5-6.22) or 1-2GB FAT32 (Retro DOS v5 or PCDOS 7.1) virtual disk or a physical harddisk -which will be recognized by MSDOS/RETRODOS- by copying WAV files into a directory. (MP3 files may be converted to WAV files for that). No need to HIMEM.SYS or DPMI or any audio device driver. Only requirement: AC97 ICH compatible audio hardware (also AC97 hardware may be emulated AC97 ICH instead of a real audio hardware).

Command: PLAYWAV2 FILE.WAV (PLAYWAV2 C:\WAV\FILENAME.WAV)

Youtube demo: https://youtu.be/m6dSEDTjoaQ

