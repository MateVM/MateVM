/* just some regression-test... */
package tests;

public class TypeStuff1 {
	public static void main(String args[]) {
		int ret = equal(10) ? 1 : 0;
		System.out.printf("success1? %d\n", ret);
		second();
	}

	public static void second() {
		System.out.printf("success2? %d\n", equal(10) ? 1 : 0);
	}

	public static boolean equal (int len) {
		return true;
	}
}
