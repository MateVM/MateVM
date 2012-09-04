package tests;

public class FacOld {
	public static void main(String args[]) {
		for (int i = 0; i < 10; i++) {
			fac(i);
			System.out.printf("fac(%d): 0x%08x\n", i, fac(i));
		}
	}

	public static int fac(int a) {
		int b = 1;
		while (a > 0) {
			b *= a;
			a--;
		}
		return b;
	}
}
