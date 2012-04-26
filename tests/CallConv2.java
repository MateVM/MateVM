package tests;

public class CallConv2 {
	public static void main(String []args) {
		int sum = 0;
		sum += manyVars(0x125A, 0x11, 0x33, 0x44);
		System.out.printf("result: 0x%08x\n", sum);
	}

	public static int manyVars(int a, int b, int c, int d) {
		int x = 0x88;
		int y = 0x55;

		return ((((a - b) - c) + d) + x + y);
	}
}
