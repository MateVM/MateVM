package tests;

public class Static1 {
	public static int x;
	public static int y;

	public static void main(String []args) {
		Static1.x = 0x11;
		Static1.y = 0x22;
		addnumbers();
	}

	public static int addnumbers() {
		return Static1.x + Static1.y;
	}
}
