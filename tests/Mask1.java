package tests;

public class Mask1 {
	public static void main(String []args) {
		int a = 0xf158f943, b = 0xacd835;

		for (int i = 0; i < 0x30; i++) {
			int x0 = a * b;
			System.out.printf("x = a * b = 0x%08x\n", x0);
			int x1 = a & b;
			System.out.printf("x = a & b = 0x%08x\n", x1);
			int x2 = a | b;
			System.out.printf("x = a | b = 0x%08x\n", x2);
			int x3 = a ^ b;
			System.out.printf("x = a ^ b = 0x%08x\n", x3);

			a = a << 1;
		}
	}
}
