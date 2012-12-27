package tests;

public class BenchStaticfield {
	public static int a, b;

	static {
		a = 0x1337;
		b = 0x666;
	}

	public static void main(String []args) {
		int i = 0xa000000;
		BenchStaticfield o = new BenchStaticfield();
		while (i != 0) {
			o.a = o.a + o.b;
			i--;
		}
		System.out.printf("o.a: %d\n", o.a);
		System.out.printf("o.b: %d\n", o.b);
	}
}
