package tests;

public class ArgumentPassing1 {
	public static void main(String []args) {
		System.out.printf("begin\n");
		int wtf = myadder (1, 1, 1);
		System.out.printf("wtf: %d\n", wtf);
		System.out.printf("result: 0x%08x\n", myadder(23, 4, 0x77)); // 0x92
		noreturn(23, 4, 0x77); // nothing
		System.out.printf("result: 0x%08x\n", noargs()); // 0x1337
	}

	public static int myadder(int a, int b, int c) {
		System.out.printf("sup @ myadder\n");
		return a + b + c;
	}

	public static void noreturn(int a, int b, int c) {
		return;
	}

	public static int noargs() {
		return -0x1337;
	}
}
