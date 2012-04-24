package tests;

public class Static8 extends Static8_local {
	public static int x;
	public static int y;

	static {
		Static8_local.x = 0x1337;
		Static8_local.y = 0x555;
	}

	public static void main(String []args) {
		Static8.addNumbers(); // 0x188c
		// System.out.printf("%x\n", Static8.addNumbers());
	}
}

class Static8_local {
	public static int x;
	public static int y;

	static {
		Static8_local.x = 0x11;
		Static8_local.y = 0x22;
		Static8_local.addNumbers(); // 0x33
		// System.out.printf("%x\n", addNumbers());
	}

	public static int addNumbers() {
		return Static8_local.x + Static8_local.y;
	}
}
