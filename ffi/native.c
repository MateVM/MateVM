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
	unsigned int gc_data;
	int value;
};

struct string {
	unsigned int method_table_ptr;
	unsigned int gc_data;
	struct chararray *value;
};

struct chararray {
	unsigned int length;
	char str;
};

void jmate_io_PrintStream__printf_X___Ljava_lang_String_Ljava_lang_Object__V
	(struct string *fmt, struct integer *a1)
{
	hexdump(fmt, 0x20);
	hexdump(a1, 0x20);
	struct integer *wtf = (struct integer*)(a1->value);
	printf("-EWRONG_PRINTF_ARGS: 0x%08x\n", wtf->value);
}

void jmate_io_PrintStream__printf_0___Ljava_lang_String__V
	(struct string *fmt)
{
	printf("DAFUQ0\n");
	hexdump(fmt, 0x20);
	printf("%s", &fmt->value->str);
}

void jmate_io_PrintStream__printf_1___Ljava_lang_String_Ljava_lang_Object__V
	(struct integer *a1, struct string *fmt)
{
#if 0
	printf("fmt:        0x%08x\n", (unsigned int) fmt);
	printf("fmt->value: 0x%08x\n", (unsigned int) fmt->value);
	printf("fmt->val.len: 0x%08x\n", (unsigned int) (*fmt->value).length);
	printf("first: %d\n", fmt->value->length);
	printf("*fmt: %s\n", &fmt->value->str);
#endif
	printf("DAFUQ1\n");
	printf(&fmt->value->str, a1->value);
}

void jmate_io_PrintStream__printf_2___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf("DAFUQ2\n");
	printf(&fmt->value->str, a1->value, a2->value);
}

void
jmate_io_PrintStream__printf_3___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a3, struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf("DAFUQ3\n");
	printf("arg1: 0x%08x\n", fmt);
	printf("arg2: 0x%08x\n", a1);
	printf("arg3: 0x%08x\n", a2);
	printf("arg4: 0x%08x\n", a3);
	printf("arg1\n");
	hexdump(fmt, 0x20);
	printf("arg2\n");
	hexdump(a1, 0x20);
	printf("arg3\n");
	hexdump(a2, 0x20);
	printf("arg4\n");
	hexdump(a3, 0x20);
	printf(&fmt->value->str, a1->value, a2->value, a3->value);
}

void jmate_io_PrintStream__printf_4___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a4, struct integer *a3, struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf("DAFUQ4\n");
	printf("arg1: 0x%08x\n", fmt);
	printf("arg2: 0x%08x\n", a1);
	printf("arg3: 0x%08x\n", a2);
	printf("arg1\n");
	hexdump(fmt, 0x20);
	printf("arg2\n");
	hexdump(a1, 0x20);
	printf("arg3\n");
	hexdump(a2, 0x20);
	printf(&fmt->value->str, a1->value, a2->value, a3->value, a4->value);
}

void jmate_io_PrintStream__printf_5___Ljava_lang_String_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object_Ljava_lang_Object__V
	(struct integer *a5, struct integer *a4, struct integer *a3, struct integer *a2, struct integer *a1, struct string *fmt)
{
	printf("DAFUQ5\n");
	printf(&fmt->value->str, a1->value, a2->value, a3->value, a4->value, a5->value);
}

void gnu_classpath_VMSystemProperties__preInit___Ljava_util_Properties__V() {
	return;
}

void java_lang_VMSystem__arraycopy___Ljava_lang_Object_ILjava_lang_Object_II_V(
		int len, int dest_start, struct chararray *dest,
		int src_start, struct chararray *src)
#if 0
		/* original */
		struct chararray *src, int src_start,
		struct chararray *dest, int dest_start, int len)
#endif
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
