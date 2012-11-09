package tests;

public class Exception12 {
	public void foo() {
		System.out.println("foooo\n");
	}

	public static void main(String []args) {
		Exception12 o = null;
		try {
			System.out.printf("bad luck brian does a virtual call...\n");
			o.foo();
		} catch (NullPointerException a) {
			System.out.printf(a.toString());
			System.out.printf("\n");
		}

		try {
			System.out.printf("sup\n");
			o.foo();
			System.out.printf("uhhhhm\n");
		} catch (Exception _) {
			System.out.printf("fun fun fun\n");
		}
		System.out.printf("over and out\n");
	}
}
