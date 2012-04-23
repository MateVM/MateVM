package tests;

public class Static2 {
	public static int a;
	public static int b;

	public static void main(String []args) {
		Static2.a = 0x55;
		Static2.b = 0x11;
		// force different {put,get}index for Static1.{x,y}
		// in Static2 as in Static1
		Static1.x = 0x33;
		Static1.y = 0x22;
		Static1.addNumbers();
	}
}
