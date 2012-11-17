package tests;

public class RegAlloc1 {
	public static int regTest(int a, int b) {
		int c = a + b;
		if (a > 10)
			c = c + 10;
		else
			b = b - a;
		return c * b;
	}

	public static void main(String []args) {
		System.out.printf("regTest(13, 37): %d\n", regTest(13, 37));
	}
}
