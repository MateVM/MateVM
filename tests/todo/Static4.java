package tests;

public class Static4 extends Static1 {
	public static int x;
	public static int y;

	public static void main(String []args) {
		Static1.setNumbers();
		Static4.setNumbers();
		System.out.printf("result: 0x%08x\n", Static1.addNumbers()); // 0x33
		System.out.printf("result: 0x%08x\n", Static4.addNumbers()); // 0x77
	}

	public static void setNumbers() {
		Static4.x = 0x44;
		Static4.y = 0x33;
	}

	public static int addNumbers() {
		return Static4.x + Static4.y;
	}
}
