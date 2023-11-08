ICH AC97 .wav player for DOS
----------------------------
Full Functional -complete- 16 bit (real mode) DOS Program (in 11/2023): 
(for playing .. 8bit, 16bit, 8-48kHZ, mono-stereo WAV files) 
playwav2.asm, PLAYWAV2.COM (included asm files: ich_wav.asm, ac97.asm, ac97.inc)

PLAYWAV2 music -wavfile- playing Method: tuneloop, double/2 (half) buffer -switch/swap- method, PCM OUT CIV-LVI and STatus register handling.

The program displays active (in use) buffer as '1' or '2' (on the top left corner of the screen with red color) while playing music and waits for a keypress (int 16h, ah=01h) to stop/exit before the end of the WAV file (it will play all of the audio samples and then it will exit/return at the end of the WAV file if the user does not press any key before).

Playwav2 is proper for playing 23-50 MB wav files... (uses 2*64KB buffers for 16 bit samples and also uses a temporary 32KB buffer for converting 8 bit and mono samples to 16 bit stereo samples)
