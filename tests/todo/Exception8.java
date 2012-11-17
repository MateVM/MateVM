package tests;

public class Exception8 {
	public static class FooEx extends IllegalArgumentException { }
	public static class FooEx1 extends FooEx { }
	public static class FooEx2 extends FooEx1 { }

	public static void main(String []args) {
		try {
			foo();
		} catch (FooEx _) {
			System.out.printf("catch FooEx\n");
		}
	}

	public static void foo() throws FooEx {
		System.out.printf("hello world\n");
		try {
			throw new FooEx();
		} catch (FooEx2 _) {
			System.out.printf("catch FooEx2\n");
		} catch (FooEx1 _) {
			System.out.printf("catch FooEx1\n");
		} finally {
			System.out.printf("finally @ foo()\n");
		}
	}
}
