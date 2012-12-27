package tests;

public class BenchException {
	public static class FooEx extends IllegalArgumentException { }
	public static class FooEx1 extends FooEx { }
	public static class FooEx2 extends FooEx1 { }

	public static void main(String []args) {
		for (int i = 0; i < 0x800; i++) {
			try {
				foo();
			} catch (FooEx _) {
				System.out.printf("catch FooEx\n");
			}
		}
	}

	public static void foo2() throws FooEx {
		bar(false);
	}

	public static void foo() throws FooEx {
		try {
			bar(true);
		} catch (FooEx2 _) {
			System.out.printf("catch FooEx2\n");
		} catch (FooEx1 _) {
			System.out.printf("catch FooEx1\n");
		} finally {
			System.out.printf("finally @ foo()\n");
		}
		try {
			bar(false);
		} catch (FooEx2 _) {
			System.out.printf("catch FooEx2\n");
		} catch (FooEx1 _) {
			System.out.printf("catch FooEx1\n");
		} finally {
			System.out.printf("finally @ foo()\n");
		}
		bar(true);
	}

	public static void bar(boolean whatif) throws FooEx2 {
		if (whatif) {
			System.out.printf("whatif: yes\n");
			try {
				throw new FooEx2();
			} finally {
				System.out.printf("finally @ bar()\n");
			}
		} else {
			System.out.printf("whatif: no\n");
			throw new FooEx2();
		}
	}
}
