package tests;

public class Bool2 {
	public static void main(String []args) {
		System.out.printf("stub0: %d\n", stub0());
		System.out.printf("stub1: %d\n", stub1());
	}

	public static boolean equal(int a) {
		return a == 0 ? false : true;
	}

	public static int stub0() {
		return equal(0) ? 1 : 0;
	}

	public static int stub1() {
		return equal(1) ? 1 : 0;
	}
}
