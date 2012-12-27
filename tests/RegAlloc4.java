package tests;

public class RegAlloc4 {
	public static class FooEx extends IllegalArgumentException { }
	public static class FooEx1 extends FooEx { }
	public static class FooEx2 extends FooEx1 { }

	public static void main(String []args) {
		int i = 0;
		for (; i < 5; i++) {
			try {
				// a = a + 5;
				foo2();
			} catch (FooEx _) {
				// b = b + 6;
				// System.out.printf("main: catch FooEx: %d\n", i);
			}
		}
		System.out.printf("goodbye: %d\n", i);
	}

	public static boolean switcher = true;
	public static void foo2() throws FooEx {
		switcher = !switcher;
		if (switcher) {
			System.out.printf("foo2: throw exception\n");
			throw new FooEx2();
		} else {
			System.out.printf("foo2: just return from method\n");
		}
	}
}
