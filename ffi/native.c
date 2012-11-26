#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static char ascii(char s) {
	if(s < 0x20) return '.';
	if(s > 0x7E) return '.';
	return s;
}

void hexdump(void *d, int len) {
	unsigned char *data;
	int i, off;
	data = (unsigned char*)d;
	for (off=0; off<len; off += 16) {
		printf("%08x  ",off);
		for(i=0; i<16; i++)
			if((i+off)>=len) printf("   ");
			else printf("%02x ",data[off+i]);

		printf(" ");
		for(i=0; i<16; i++)
			if((i+off)>=len) printf(" ");
			else printf("%c",ascii(data[off+i]));
		printf("\n");
	}
}

struct string {
	unsigned int method_table_ptr;
	unsigned int gc_data;
	struct chararray *value;
};

struct chararray {
        unsigned int method_table_fake;
        unsigned int gc_data;
	unsigned int length;
	char str;
};

void gnu_classpath_VMSystemProperties__preInit___Ljava_util_Properties__V() {
	return;
}

void java_lang_VMSystem__arraycopy___Ljava_lang_Object_ILjava_lang_Object_II_V(
		struct chararray *src, int src_start,
		struct chararray *dest, int dest_start, int len)
{
	memcpy(&dest->str, &src->str + src_start, len);
	dest->length = len;
#if 0
	hexdump(src, len + 4);
	hexdump(dest, len + 4);
#endif
}

void
java_lang_VMThrowable__fillInStackTrace___Ljava_lang_Throwable__Ljava_lang_VMThrowable_()
{
	printf("fillInStackTrace: TODO\n");
}
