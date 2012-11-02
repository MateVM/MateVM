package tests;

public class Static6 {
	public static int x;
	public static int y;

	static {
		Static6.x = 0x11;
		Static6.y = 0x22;
	}

	public static void main(String []args) {
		System.out.printf("result: 0x%08x\n", addNumbers()); // 0x33
	}

	public static int addNumbers() {
		return Static6.x + Static6.y;
	}
}
