package tests;

public class Array2 {
	static int[] foo() {
		int []val = new int[10];
		int sum = 0;
		for (int i = 0; i < 10; i++) {
			val[i] = i + 0x30;
			System.out.printf("val[%d]: %x\n", i, val[i]);
			sum += val[i];
		}
		System.out.printf("sum: %d\n", sum); // 480 + 45 = 525
		return val;
	}

	public static void main(String []args) {
		int []ar = foo();
		int sum = 0;
		for (int i = 0; i < 10; i++)
			sum += ar[i];
		System.out.printf("sum: %d\n", sum); // 480 + 45 = 525
		System.out.printf("ar[1]: %x\n", ar[1]);
	}
}
