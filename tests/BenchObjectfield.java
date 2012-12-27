package tests;

public class BenchObjectfield {
	public int a, b;

	public BenchObjectfield() {
		a = 0x1337;
		b = 0x666;
	}

	public static void main(String []args) {
		BenchObjectfield o = new BenchObjectfield();
		int i = 0xa000000;
		while (i != 0) {
			o.a = o.a + o.b;
			i--;
		}
		System.out.printf("o.a: %d\n", o.a);
		System.out.printf("o.b: %d\n", o.b);
	}
}
