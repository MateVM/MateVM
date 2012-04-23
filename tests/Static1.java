package tests;

public class Static1 {
	public static int a;
	public static int b;

	static {
		Static1.a = 0x1337;
	}

	public static void main(String []args) {
		Static1.a = 0x11;
		Static1.b = 0x22;
		addnumbers();
	}

	public static int addnumbers() {
		return Static1.a + Static1.b;
	}
}
