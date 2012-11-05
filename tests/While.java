package tests;

public class While {
	public static void main(String []args) {
		System.out.printf("result: g: 0x%08x\n", g(4, 6));
		System.out.printf("result: f: 0x%08x\n", f(4, 6));
	}

	public static int f(int a, int b) {
		do {
			a += b;
			b--;
		} while (b > 0);
		return a;
	}

	public static int g(int a, int b) {
		while (b > 0) {
			a += b;
			b--;
		}
		return a;
	}
}
