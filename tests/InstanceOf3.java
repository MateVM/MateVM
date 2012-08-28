package tests;

public class InstanceOf3 {
	static interface i1 { };
	static interface i2 { };
	static interface i3 extends i2 { };
	static interface i4 extends i3 { };
	static interface i5 { };
	static class c1 { };
	static class c2 extends c1 implements i1 { };
	static class c3 extends c2 implements i4 { };
	static class c4 { };

	public static void main(String []args) {
		Object x = new c3();
		checkInstance(x instanceof i1, "x", "i1");
		checkInstance(x instanceof i2, "x", "i2");
		checkInstance(x instanceof i3, "x", "i3");
		checkInstance(x instanceof i4, "x", "i4");
		checkInstance(x instanceof i5, "x", "i5");
		checkInstance(x instanceof c1, "x", "c1");
		checkInstance(x instanceof c2, "x", "c2");
		checkInstance(x instanceof c3, "x", "c3");
		checkInstance(x instanceof c4, "x", "c4");
		checkInstance(x instanceof String, "x", "String");
		checkInstance(x instanceof Integer, "x", "Integer");
		checkInstance(x instanceof Object, "x", "Object");
	}

	public static void checkInstance(boolean cond, String obj, String classname) {
		System.out.printf(obj);
		if (cond) {
			System.out.printf(" is instance of ");
			System.out.printf(classname);
			System.out.printf(" :-)\n");
		} else {
			System.out.printf(" is *not* instance of ");
			System.out.printf(classname);
			System.out.printf(" :-(\n");
		}
	}
}
