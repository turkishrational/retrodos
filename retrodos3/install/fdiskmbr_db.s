; PC-DOS 7.0 FDISK Utility - Master Boot Sector (Template)

[bits 16]
[org 0]

master_boot_record:
	db 0FAh, 33h, 0C0h, 8Eh, 0D0h, 0BCh, 00h, 7Ch, 8Bh, 0F4h, 50h, 07h, 50h, 1Fh, 0FBh, 0FCh
	db 0BFh, 00h, 06h, 0B9h, 00h, 01h, 0F2h, 0A5h, 0EAh, 1Dh, 06h, 00h, 00h, 0BEh, 0BEh, 07h
	db 0B3h, 04h, 80h, 3Ch,	80h, 74h, 0Eh, 80h, 3Ch, 00h, 75h, 1Ch,	83h, 0C6h, 10h,	0FEh
	db 0CBh, 75h, 0EFh, 0CDh, 18h, 8Bh, 14h, 8Bh, 4Ch, 02h,	8Bh, 0EEh, 83h,	0C6h, 10h, 0FEh
	db 0CBh, 74h, 1Ah, 80h,	3Ch, 00h, 74h, 0F4h, 0BEh, 8Bh,	06h, 0ACh, 3Ch,	00h, 74h, 0Bh
	db 56h,	0BBh, 07h, 00h,	0B4h, 0Eh, 0CDh, 10h, 5Eh, 0EBh, 0F0h, 0EBh, 0FEh, 0BFh, 05h, 00h
	db 0BBh, 00h, 7Ch, 0B8h, 01h, 02h, 57h,	0CDh, 13h, 5Fh,	73h, 0Ch, 33h, 0C0h, 0CDh, 13h
	db 4Fh,	75h, 0EDh, 0BEh, 0A3h, 06h, 0EBh, 0D3h,	0BEh, 0C2h, 06h, 0BFh, 0FEh, 7Dh, 81h, 3Dh
	db 55h,	0AAh, 75h, 0C7h, 8Bh, 0F5h, 0EAh, 00h, 7Ch, 00h, 00h, 49h, 6Eh,	76h, 61h, 6Ch
	db 69h,	64h, 20h, 70h, 61h, 72h, 74h, 69h, 74h,	69h, 6Fh, 6Eh, 20h, 74h, 61h, 62h
	db 6Ch,	65h, 00h, 45h, 72h, 72h, 6Fh, 72h, 20h,	6Ch, 6Fh, 61h, 64h, 69h, 6Eh, 67h
	db 20h,	6Fh, 70h, 65h, 72h, 61h, 74h, 69h, 6Eh,	67h, 20h, 73h, 79h, 73h, 74h, 65h
	db 6Dh,	00h, 4Dh, 69h, 73h, 73h, 69h, 6Eh, 67h,	20h, 6Fh, 70h, 65h, 72h, 61h, 74h
	db 69h,	6Eh, 67h, 20h, 73h, 79h, 73h, 74h, 65h,	6Dh, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h
	db 00h,	00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h,	00h, 00h, 00h, 00h, 00h, 55h, 0AAh