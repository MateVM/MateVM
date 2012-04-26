package tests;

public class Static1 {
	public static int x;
	public static int y;

	public static void main(String []args) {
		setNumbers();
		System.out.printf("result: 0x%08x\n", addNumbers()); // 0x33
	}

	public static void setNumbers() {
		Static1.x = 0x11;
		Static1.y = 0x22;
	}

	public static int addNumbers() {
		return Static1.x + Static1.y;
	}
}
