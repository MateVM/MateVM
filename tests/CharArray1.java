package tests;

public class CharArray1 {
	static char[] foo() {
		char[] val = new char[10];
		for (int i = 0; i < 10; i++)
			val[i] = (char) (i + 0x30);
		return val;
	}

	public static void main(String []args) {
		char [] ar = foo();
		for (int i = 0; i < 10; i++)
			System.out.printf("%c", ar[i]);
		System.out.printf("\n");
	}
}
