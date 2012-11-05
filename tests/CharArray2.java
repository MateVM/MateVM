package tests;

public class CharArray2 {
	static char[] foo() {
		char[] val = new char[10];
		int sum = 0;
		for (int i = 0; i < 10; i++) {
			val[i] = (char) (i + 0x30);
			System.out.printf("val[%d]: %c (should be: %d)\n", i, val[i], i + 0x30);
			sum += val[i];
		}
		System.out.printf("sum: %d\n", sum); // 480 + 45 = 525
		return val;
	}

	public static void main(String []args) {
		char [] ar = foo();
		int sum = 0;
		for (int i = 0; i < 10; i++)
			sum += ar[i];
		System.out.printf("sum: %d\n", sum); // 480 + 45 = 525
		System.out.printf("ar[0]: %c\n", ar[0]);
		System.out.printf("ar[1]: %c\n", ar[1]);
		System.out.printf("ar[2]: %c\n", ar[2]);
	}
}
