package tests;

public class InstanceOf3 {
	static interface i1 { };
	static interface i2 { };
	static interface i3 extends i2 { };
	static interface i4 extends i3 { };
	static interface i5 { };
	static interface i6 extends i3 { };
	static interface i7 extends i4, i6 { };
	static interface i8 extends i2, i7 { };
	static class c1 {
		static { System.out.printf("loading c1\n"); }
	};
	static class c2 extends c1 implements i1 {
		static { System.out.printf("loading c2\n"); }
	};
	static class c3 extends c2 implements i4 {
		static { System.out.printf("loading c3\n"); }
	};
	static class c4 {
		static { System.out.printf("loading c4\n"); }
	};

	public static void main(String []args) {
		Object x = new c3();
		i2 y = new i3() { };
		i6 z = new i6() { };
		i7 u = new i7() { };
		i8 v = new i8() { };

		checkInstance(x instanceof c1, "x", "c1");
		checkInstance(x instanceof c2, "x", "c2");
		checkInstance(x instanceof c3, "x", "c3");
		checkInstance(x instanceof c4, "x", "c4");
		checkInstance(x instanceof String, "x", "String");
		checkInstance(x instanceof Integer, "x", "Integer");

		checkInterfaces(x, "x");
		checkInterfaces(y, "y");
		checkInterfaces(z, "z");
		checkInterfaces(u, "u");
		checkInterfaces(v, "v");
	}

	public static void checkInterfaces(Object y, String name) {
		checkInstance(y instanceof Object, name, "Object");
		checkInstance(y instanceof i1, name, "i1");
		checkInstance(y instanceof i2, name, "i2");
		checkInstance(y instanceof i3, name, "i3");
		checkInstance(y instanceof i4, name, "i4");
		checkInstance(y instanceof i5, name, "i5");
		checkInstance(y instanceof i6, name, "i6");
		checkInstance(y instanceof i7, name, "i7");
		checkInstance(y instanceof i8, name, "i8");
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
