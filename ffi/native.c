#include <stdio.h>

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

void tests_Native1__printSomething____V(void)
{
	printf("printSomething: woot \\o/\n");
}

void tests_Native1__printSomething___I_V(int a)
{
	printf("printSomething: 0x%08x\n", a);
}

void tests_Instance5__printX___I_V(int a)
{
	printf("printX: 0x%08x\n", a);
}

void java_io_PrintStream__printf___I_V(int a)
{
	printf("printstream: 0x%08x\n", a);
}

struct integer {
	unsigned int method_table_ptr;
	int value;
};

struct string {
	unsigned int method_table_ptr;
	struct chararray *value;
};

struct chararray {
	unsigned int length;
	char str;
};

void java_io_PrintStream__printf_0___Ljava_lang_String__V
	(struct string *fmt)
{
	printf("%s", &fmt->value->str);
}

void java_io_PrintStream__printf_1___Ljava_lang_String_Ljava_lang_Object__V
	(struct integer *a1, struct string *fmt)
{
#if 0
	printf("fmt:        0x%08x\n", (unsigned int) fmt);
	printf("fmt->value: 0x%08x\n", (unsigned int) fmt->value);
	printf("fmt->val.len: 0x%08x\n", (unsigned int) (*fmt->value).length);
	printf("first: %d\n", fmt->value->length);
	printf("*fmt: %s\n", &fmt->value->str);
#endif
	printf(&fmt->value->str, a1->value);
}

void java_io_PrintStream__printf_2___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf(&fmt->value->str, a1->value, a2->value);
}

void
java_io_PrintStream__printf_3___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a3, struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf(&fmt->value->str, a1->value, a2->value, a3->value);
}

void java_io_PrintStream__printf_4___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a4, struct integer *a3, struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf(&fmt->value->str, a1->value, a2->value, a3->value, a4->value);
}

void java_io_PrintStream__printf_5___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a5, struct integer *a4, struct integer *a3, struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf(&fmt->value->str, a1->value, a2->value, a3->value, a4->value, a5->value);
}
