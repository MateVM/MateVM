package tests;

public class Static7 extends Static6 {
	public static int x;
	public static int y;

	static {
		Static6.x = 0x1337;
		Static6.y = 0x555;
	}

	public static void main(String []args) {
		System.out.printf("result: 0x%08x\n", addNumbers()); // 0x188c
	}
}
