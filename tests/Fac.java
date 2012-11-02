package tests;

public class Fac {
	public static void main(String args[]) {
		int sum = 0;
		for (int i = 0; i < 10; i++) {
			sum += fac(i);
		}
		System.out.printf("result: 0x%08x\n", sum);
		// System.out.printf("result: 0x%08x\n", facFor(0x10));
	}

	public static int fac(int a) {
		int b = 1;
		while (a > 0) {
			b *= a;
			a--;
		}
		return b;
	}

	public static int facFor(int n) {
		int p = 1;
		for(int i = 1; i <= n; i++) {
			p *= i;
		}
		return p;
	}
}
