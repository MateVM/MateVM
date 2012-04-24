package tests;

public class Static5 extends Static1 {
	public static int x;
	public static int y;

	public static void main(String []args) {
		Static5.setNumbers();
		Static5.addNumbers(); // 0x33
		Static1.addNumbers(); // 0x33
		// System.out.printf("%x\n", Static5.addNumbers());
	}
}
