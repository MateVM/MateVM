package tests;

public class Integer1 {
	public static void main(String []args) {
		Integer a = new Integer(0x1337);
		System.out.printf("result: 0x%08x\n", a.intValue()); // 0x1337
	}
}
