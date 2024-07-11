/*
 * UNEXEPACK -- An upacker for EXE files packed with "Microsoft EXEPACK".
 *
 * To the extent possible under law, Vitaly Driedfruit has waived all copyright
 * and related or neighboring rights to UNEXEPACK.
 *
 * See http://creativecommons.org/publicdomain/zero/1.0/
 *
 * Unpacking algorithm fetched from 
 * http://cvs.z88dk.org/cgi-bin/viewvc.cgi/xu4/doc/avatarExepacked.txt?revision=1.1&root=zxu4&view=markup
 * by $Id: avatarExepacked.txt,v 1.1 2009/05/04 18:08:58 aowen Exp $ 
 *
 */

/*
 * This program only works on 32-bit LSB machines.
 *
 * TODO: Fix READ_WORD and WRITE_WORD to make it portable.
 *
 * This is a C99 program, it uses "char something[variable]" construct. Convert
 * those to malloc calls to make it C89.
 *
 * Final warning: this will not produce identical results to the real EXEPACK
 * routine, because the garbage bytes are treated differently: canonicaly,
 * they should be carried over from the equivalent locations of *packed* exe;
 * here -- they are simply 0x00.
 */
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

struct EXE {
  unsigned short signature; /* == 0x5a4D, "MZ" */
  unsigned short bytes_in_last_block;
  unsigned short blocks_in_file;
  unsigned short num_relocs;
  unsigned short header_paragraphs;
  unsigned short min_extra_paragraphs;
  unsigned short max_extra_paragraphs;
  unsigned short ss;
  unsigned short sp;
  unsigned short checksum;
  unsigned short ip;
  unsigned short cs;
  unsigned short reloc_table_offset;
  unsigned short overlay_number;
};
#if 0
struct EXE_RELOC {
  unsigned short offset;
  unsigned short segment;
};
#endif
/*
	An exepacked file looks like this:

offset     length    purpose
0          0x1C      DOS exe header, see "struct EXE" above
           <...>     packed exe
           0x12      EXEPACK variables, see "struct EXEPACK" below
		   0x105	 unpacker code
		   0x16      string "Packed file is corrupt"
           <...>     packed reloc table

length of packed exe = CS:IP (from exe header)
length of packed reloc table = exepack_size - dest_len (from exepack variables)
*/
const int mzSize = 0x1C; /* size of "struct EXE" */
const int unpackerDataLen = 0x12; /* size of "struct EXEPACK" */
const int unpackerLen = 0x105; /* size of unpacker code */
const char errString[0x16] = "Packed file is corrupt";
const int errLen = sizeof(errString);

struct EXEPACK {
	unsigned short real_start_OFFSET;	/* real start address (offset) */
	unsigned short real_start_SEGMENT;	/* real start address (segment) */
	unsigned short mem_start_SEGMENT;	/* start of the exe in memory (segment), 0000, must be set by program */
	unsigned short unpacker_len;    	/* size of <unpacker vars + unpacker code + error string + packed reloc table> in bytes */
	unsigned short real_stack_OFFSET;	/* real stack (offset) */
	unsigned short real_stack_SEGMENT;	/* real stack (segment) */
	unsigned short dest_len;        	/* destination of the unpacker code (in paragraphs, relative to start of exe in memory) */
	unsigned short skip_len;        	/* <number of paragraphs between packed exe and unpacker variables> + 1 */
	unsigned short signature; /* == 0x5242, "RB" */
};
/* Packed relocation table constists of 16 sections, layed out like this: */
#if 0
struct EXEPACK_RELOC {
	struct {
		unsigned short num_entries;	/* can be zero */
		unsigned short entry[]; 	/* can be empty */
	} section[16];
};
#endif
/*
To unpack relocation table in memory:
	for (N = 0 to 15)
		foreach (entry in section N)
			patch at (0x1000 * N + entry) (relative to the start of the exe in memory)

To convert it to a standart DOS relocation table (i.e. for writing an EXE):
	unpacked_entry_segment = 0x1000 * N;
	unpacked_entry_offset = entry;
*/
#define READ_WORD(str, index) str[index+1]*0x100 + str[index];
#define WRITE_WORD(str, index, val) { unsigned short *w = (unsigned short *) &str[index]; *w = val; }

/*
 * _The_ algorithm. Thanks aowen.
 *
 * This function returns number of bytes unpacked (or -1), and writes number of
 *  bytes read into "res".
 *
 * srcPos and dstPos should point to ENDs of respective input and output locations,
 *  as unpacking is performed backwards. No error-checking on size is done, beware
 *  of buffer overflows.
 */
int exepack_unpack(unsigned char *dstPos, unsigned char *srcPos, int *res) {
  int i, n = 0;
  unsigned char *lastPos = srcPos;

  int commandByte, lengthWord, fillByte;

  /* skip all 0xff bytes (they're just padding to make the packed exe's size a multiple of 16 */
  while (*srcPos == 0xff) {
    srcPos--;
  }
  /* unpack */
  do {
    commandByte = *(srcPos--);
    switch (commandByte & 0xFE) {
      /* (byte)value (word)length (byte)0xb0 */
      /* writes a run of <length> bytes with a value of <value> */
      case 0xb0:
        lengthWord = (*(srcPos--))*0x100;
        lengthWord += *(srcPos--);
        fillByte = *(srcPos--);
        for (i = 0; i < lengthWord; i++) {
          *(dstPos--) = fillByte;
        }
        n += lengthWord;
        break;
      /* (word)length (byte)0xb2 */
      /* copies the next <length> bytes */
      case 0xb2:
        lengthWord = (*(srcPos--))*0x100;
        lengthWord += *(srcPos--);
        for (i = 0; i < lengthWord; i++) {
          *(dstPos--) = *(srcPos--);
        }
        n += lengthWord;
        break;
      /* unknown command */
      default:
        fprintf(stderr, "Unknown command %2x at position %d\n", commandByte, lastPos - srcPos);
        n = -1;
        break;
    }
  } while ((commandByte & 1) != 1); /* lowest bit set => last block */
  *res = lastPos - srcPos;
  return n;
}

void read_EXE(struct EXE *h, unsigned char *buffer);
void read_EXEPACK(struct EXEPACK *h, unsigned char *buffer);
void write_EXE(unsigned char *buffer, struct EXE *h);
void print_EXE(struct EXE *h, const char *name);

/* MAIN */
int main(int argc, char **argv) {

	const char *inputFile = NULL;
	const char *outputFile = NULL;

	if (argc < 2) {
		fprintf(stderr, "Usage: %s INPUT-FILE OUTPUT-FILE\n", argv[0]);
		exit(-3);
	}
	if (argc > 1) inputFile = argv[1];
	if (argc > 2) outputFile = argv[2];

	/* read exe header */
	unsigned char mzHeader[0x20];

	struct EXE mz;

	FILE *f;

	int n;

 	f = fopen(inputFile, "rb");

 	if (!f) {
 		fprintf(stderr, "Unable to open '%s'\n", inputFile);
 		return -1;
 	}

	n = fread(mzHeader, sizeof(char), mzSize, f); /* read bytes */

	if (n < mzSize) {
		fprintf(stderr, "Unable to read EXE header in '%s'\n", inputFile);
		fclose(f);
		return -1;
	}

	read_EXE(&mz, &mzHeader[0]); /* move bytes to struct */

	if (mz.signature != 0x5a4D) {
		fprintf(stderr, "Unable to parse EXE header in '%s'\n", inputFile);
		fclose(f);
		return -1;
	}

	print_EXE(&mz, inputFile); /* show some info to stdout */

	int exe_data_start = mz.header_paragraphs * 16L;

	int extra_data_start = mz.blocks_in_file * 512L;
	if (mz.bytes_in_last_block)
  		extra_data_start -= (512 - mz.bytes_in_last_block);

	int first_offset = mz.cs * 0x10;// + exe_data_start;

	int exeLen = first_offset;
	printf("\t Predicted packed exe : %d bytes\n", exeLen);

	/* read packed data */
	unsigned char buffer[exeLen];

	fseek(f, exe_data_start, SEEK_SET);

	n = fread(&buffer[0], sizeof(char), exeLen, f);

	if (n < exeLen) {
		fprintf(stderr, "Unable to read PACKED data from '%s'\n", inputFile);
		fclose(f);
		return -1;
	}

	/* read unpacker data */
	unsigned char packData[unpackerDataLen];

	struct EXEPACK packed;

	n = fread(packData, sizeof(char), unpackerDataLen, f);
	
	if (n < unpackerDataLen) {
		fprintf(stderr, "Unable to read EXEPACK variables from '%s':%08x\n", inputFile, exe_data_start + exeLen);
		fclose(f);
		return -1;
	}

	read_EXEPACK(&packed, &packData[0]);	/* move bytes to struct */

	if (packed.signature != 0x4252) {
		fprintf(stderr, "Not an EXEPACK file / corrupt EXEPACK header. %08x", packed.signature);
		fclose(f);
		return -1;	
	}

	/* show some info */
	printf("Packed CS:IP\t\t\t%04x:%04x\nPacked SS:SP\t\t\t%04x:%04x\nStart of EXE: %04x\n", 
		packed.real_start_SEGMENT, packed.real_start_OFFSET,
		packed.real_stack_SEGMENT, packed.real_stack_OFFSET,
		packed.mem_start_SEGMENT);

	printf("Skip len (para)\t\t\t%04x\t\t%d\nexepack size (bytes)\t\t%04x\t\t%d\nFull length: %d*16 = %d\n",
		packed.skip_len, packed.skip_len,
		packed.unpacker_len, packed.unpacker_len,
		packed.dest_len, packed.dest_len * 16);

	/* read the rest of unpacker code/data */
	int pack_buffer = packed.unpacker_len - unpackerDataLen; /* everything after unpacker variables */

	unsigned char pbuffer[pack_buffer];

	n = fread(pbuffer, sizeof(char), pack_buffer, f);

	fclose(f);

	if (n < pack_buffer) {
		fprintf(stderr, "Unable to read %d bytes of unpacker code/data, have %d\n", pack_buffer, n);
		return -1;
	}

	int reloc_table_size = packed.unpacker_len - errLen - unpackerLen - unpackerDataLen;
	int reloc_num_entries = (reloc_table_size - 16 * sizeof(unsigned short)) / 2;
	int reloc_table_full = reloc_num_entries * 2 * sizeof(unsigned short);
	printf("\t Predicted packed reloc.table : %d bytes (%d entries)\n", reloc_table_size, reloc_num_entries);
	printf("\t Unpacked relocation table : %d bytes\n", reloc_table_full);

	//int exeLen = extra_data_start - packed.unpacker_len;

	if (outputFile == NULL) {
		fprintf(stderr, "No output file specified\n");
		return -3;
	}

	int finalSize = packed.dest_len * 16L;
	printf("\tExpected unpacked size: %d bytes\n", finalSize);

	char out[finalSize];
	memset(&out[0], 0xFF, finalSize);
	memcpy(&out[0], &buffer[0], exeLen);

	int p = unpackerLen + errLen;

	if (memcmp(&pbuffer[p - errLen], errString, errLen)) {
		fprintf(stderr, "RB String is wrong!\n");
	}

	int r;

	n = exepack_unpack(&out[finalSize - 1], &buffer[exeLen - 1], &r);

	if (n == -1) {
		fprintf(stderr, "Unable to UNPACK data\n");
		return -2;
	}

	printf("Unpacked %d bytes (from %d packed bytes) into [%08x--%08x]\n", n, r, exeLen - 1, exeLen - 1 - r);

	/* unpack relocation table */
	int section = 0;

	char rout[reloc_table_full];

	int relocSize = 0;

	for (section = 0; section < 16; section++) {

		int num_entries = READ_WORD(pbuffer, p);p += 2;

		if (num_entries == 0) break;	

		int k;
		for (k = 0; k < num_entries; k++) {

			int entry = READ_WORD(pbuffer, p);p += 2;

			unsigned short patchSegment = 0x1000 * section;
			unsigned short patchOffset = entry;

			WRITE_WORD(rout, relocSize, patchOffset);relocSize += 2;
			WRITE_WORD(rout, relocSize, patchSegment);relocSize += 2;

//			patchOffset += 0x20;	// MZ header size
//			printf("%d. Entry -- (%d) %08x --- (%d) %08x\n", k, entry, entry, patchOffset, patchOffset);
/*			short *patch = &rout[patchOffset];
//			printf("%d. Must patch DI %08x -- %08x + %08x\n", k, patchOffset, *patch, EXEPACK_mem_start_SEGMENT);
			*patch += EXEPACK_mem_start_SEGMENT;
*/

		}
	}

	/* prepare new exe header */
	struct EXE newexe;

	char mzOut[mzSize];

	int headerSize = mzSize + relocSize;

	newexe.signature = 0x5a4D;
	newexe.header_paragraphs = headerSize / 16L;
#if 1
	/* For some OSes (which?), header size must be in multiple of 512 bytes,
	 * i.e. paragraphs must be in multiple of 32 */
	newexe.header_paragraphs = (newexe.header_paragraphs / 32L + 1) * 32L;
#endif

	int needHeaderSize = newexe.header_paragraphs * 16L;

	int mzGarbage = 0;	/* padding bytes between mz header and reloc table */
	int relocGarbage = needHeaderSize - headerSize - mzGarbage; /* padding bytes between reloc table and exe data */

	int fullSize = mzSize + mzGarbage + relocSize + relocGarbage + finalSize; 

	newexe.sp = packed.real_stack_OFFSET;
	newexe.ss = packed.real_stack_SEGMENT;
	newexe.ip = packed.real_start_OFFSET;
	newexe.cs = packed.real_start_SEGMENT;

	newexe.max_extra_paragraphs = 0xFFFF;
	newexe.min_extra_paragraphs = fullSize / 60;	/* I don't know how to determine BSS properly :/ */

	newexe.reloc_table_offset = mzSize + mzGarbage;
	newexe.num_relocs = relocSize / (2 * sizeof(unsigned short));

	newexe.blocks_in_file = fullSize / 512 + 1;
	newexe.bytes_in_last_block = fullSize % 512;

	newexe.overlay_number = 0;

	printf("\n");
	print_EXE(&newexe, outputFile); /* show some info */

	/* save new exe */
	int j;

	write_EXE(&mzOut[0], &newexe);

	f = fopen(outputFile, "wb");
	if (!f) {
		fprintf(stderr, "Unable to open '%s' for writing\n", outputFile);
		return -2;
	}

	fwrite(mzOut, mzSize, sizeof(char), f);

	for (j = 0; j < mzGarbage; j++) fwrite("\0", 1, sizeof(char), f);	

	fwrite(rout, relocSize, sizeof(char), f);

	for (j = 0; j < relocGarbage; j++) fwrite("\0", 1, sizeof(char), f);	

	fwrite(out, finalSize, sizeof(char), f);

	fclose(f);

	return 0;
}

void print_EXE(struct EXE *h, const char *name) {
	int exe_data_start = h->header_paragraphs * 16L;

	int extra_data_start = h->blocks_in_file * 512L;
	if (h->bytes_in_last_block)
  		extra_data_start -= (512 - h->bytes_in_last_block);

	printf("%s\t\t\t(hex)\t\t(dec)\n", name);

	printf(".EXE size (bytes)\t\t%04x\t\t%d\n", extra_data_start, extra_data_start);
	printf("Overlay number\t\t\t%04x\t\t%d\n", h->overlay_number, h->overlay_number);
	printf("Initial CS:IP\t\t\t%04x:%04x\n", h->cs, h->ip);
	printf("Initial SS:SP\t\t\t%04x:%04x\n", h->ss, h->sp);
	printf("Minimum allocation (para)\t%4x\t\t%d\n", h->min_extra_paragraphs, h->min_extra_paragraphs);
	printf("Maximum allocation (para)\t%4x\t\t%d\n", h->max_extra_paragraphs, h->max_extra_paragraphs);
	printf("Header size (para)\t\t%4x\t\t%d\n", h->header_paragraphs, h->header_paragraphs);
	printf("Relocation table offset\t\t%4x\t\t%d\n", h->reloc_table_offset, h->reloc_table_offset);
	printf("Relocation entries\t\t%4x\t\t%d\n", h->num_relocs, h->num_relocs);

	printf("\n");
}
void read_EXE(struct EXE *h, unsigned char *buffer) { /* This boring code is portable, unlike fread(struct..) */
	h->signature = READ_WORD(buffer, 0x00);
  	h->bytes_in_last_block = READ_WORD(buffer, 0x02);
  	h->blocks_in_file = READ_WORD(buffer, 0x04);
  	h->num_relocs = READ_WORD(buffer, 0x06);
  	h->header_paragraphs = READ_WORD(buffer, 0x08);
  	h->min_extra_paragraphs = READ_WORD(buffer, 0x0A);
  	h->max_extra_paragraphs = READ_WORD(buffer, 0x0C);
  	h->ss = READ_WORD(buffer, 0x0E);
  	h->sp = READ_WORD(buffer, 0x10);
  	h->checksum = READ_WORD(buffer, 0x12);
  	h->ip = READ_WORD(buffer, 0x14);
  	h->cs = READ_WORD(buffer, 0x16);
  	h->reloc_table_offset = READ_WORD(buffer, 0x18);
  	h->overlay_number= READ_WORD(buffer, 0x1A);
}
void read_EXEPACK(struct EXEPACK *h, unsigned char *buffer) { /* This boring code is portable, unlike fread(struct..) */
	h->real_start_OFFSET = READ_WORD(buffer, 0);
	h->real_start_SEGMENT = READ_WORD(buffer, 2);
	h->mem_start_SEGMENT = READ_WORD(buffer, 4);
	h->unpacker_len = READ_WORD(buffer, 6);
	h->real_stack_OFFSET = READ_WORD(buffer, 8);
	h->real_stack_SEGMENT = READ_WORD(buffer, 10);
	h->dest_len = READ_WORD(buffer, 12);
	h->skip_len = READ_WORD(buffer, 14);
	h->signature = READ_WORD(buffer, 16);
}
void write_EXE(unsigned char *buffer, struct EXE *h) { /* This boring code is portable, unlike fwrite(struct..) */
	WRITE_WORD(buffer, 0x00, h->signature);
  	WRITE_WORD(buffer, 0x02, h->bytes_in_last_block);
  	WRITE_WORD(buffer, 0x04, h->blocks_in_file);
  	WRITE_WORD(buffer, 0x06, h->num_relocs);
  	WRITE_WORD(buffer, 0x08, h->header_paragraphs);
  	WRITE_WORD(buffer, 0x0A, h->min_extra_paragraphs); 
  	WRITE_WORD(buffer, 0x0C, h->max_extra_paragraphs);
  	WRITE_WORD(buffer, 0x0E, h->ss);
  	WRITE_WORD(buffer, 0x10, h->sp);
  	WRITE_WORD(buffer, 0x12, h->checksum);
  	WRITE_WORD(buffer, 0x14, h->ip);
  	WRITE_WORD(buffer, 0x16, h->cs);
  	WRITE_WORD(buffer, 0x18, h->reloc_table_offset);
  	WRITE_WORD(buffer, 0x1A, h->overlay_number);
}
