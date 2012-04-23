package tests;

public class CallConv1 {
	public static void main(String []args) {
		int sum = 0;
		sum += manyVars(0x1348);
		id(sum); // 0x1337
	}

	public static int id(int a) {
		return a;
	}

	public static int manyVars(int a) {
		int b = 0x22;
		int c = 0x33;
		int d = 0x44;

		return (((a - b) - c) + d);
	}
}
