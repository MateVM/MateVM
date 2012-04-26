package tests;

public class Static8 extends Static8_local {
	public static int x;
	public static int y;

	static {
		Static8_local.x = 0x1337;
		Static8_local.y = 0x555;
	}

	public static void main(String []args) {
		System.out.printf("result: 0x%08x\n", Static8.addNumbers()); // 0x188c
	}
}

class Static8_local {
	public static int x;
	public static int y;

	static {
		Static8_local.x = 0x11;
		Static8_local.y = 0x22;
		System.out.printf("result: 0x%08x\n", addNumbers()); // 0x33
	}

	public static int addNumbers() {
		return Static8_local.x + Static8_local.y;
	}
}
