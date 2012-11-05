package tests;

public class CallConv4 {
	public static void main(String []args) {
		int sum = 0;
		sum = sum + returnOne();
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + 1;
		sum = sum + returnOne();
		System.out.printf("result: 0x%08x\n", sum);
	}
	public static int returnOne() {
		return 1;
	}
}
