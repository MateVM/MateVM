package tests;

public class CallConv2 {
	public static void main(String []args) {
		int sum = 0;
		sum += manyVars(0x125A, 0x11, 0x33, 0x44);
		id(sum); // 0x1337
	}

	public static int id(int a) {
		return a;
	}

	public static int manyVars(int a, int b, int c, int d) {
		int x = 0x88;
		int y = 0x55;

		return ((((a - b) - c) + d) + x + y);
	}
}
