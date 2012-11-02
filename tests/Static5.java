package tests;

public class Static5 extends Static1 {
	public static int x;
	public static int y;

	public static void main(String []args) {
		Static5.setNumbers();
		System.out.printf("result: 0x%08x\n", Static5.addNumbers()); // 0x33
		System.out.printf("result: 0x%08x\n", Static1.addNumbers()); // 0x33
	}
}
