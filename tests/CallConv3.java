package tests;

public class CallConv3 {
	public static void main(String []args) {
		manyVars_A(0x1000, 0x300, 0x30, 0x7);
		manyVars_B(0x1000, 0x300, 0x30, 0x7);
		manyVars_C(0x1000, 0x300, 0x30, 0x7);
		manyVars_D(0x1000, 0x300, 0x30, 0x7);
	}

	public static int manyVars_A(int a, int b, int c, int d) {
		return a;
	}
	public static int manyVars_B(int a, int b, int c, int d) {
		return b;
	}
	public static int manyVars_C(int a, int b, int c, int d) {
		return c;
	}
	public static int manyVars_D(int a, int b, int c, int d) {
		return d;
	}
}
