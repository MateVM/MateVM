package tests;

public class CallConv5 {
	public static void main(String []args) {
		System.out.printf("result: 0x%08x\n", addStuff(0x2, 0x3));
	}
	public static int addStuff(int a, int b) {
		// modify arguments
		b = a;
		a = b;
		return a + b; // 2 * a
	}
}
