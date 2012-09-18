package tests;

public class Exception9 {
	public static class FooEx extends IllegalArgumentException { }

	public static void main(String []args) {
		try {
			foo();
		} catch (FooEx _) {
			System.out.printf("catch FooEx\n");
		}
	}

	public static void foo() throws FooEx {
		try {
			throw new FooEx();
		} finally {
			System.out.printf("finally @ foo()\n");
		}
	}
}
