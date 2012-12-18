package tests;

public class Interface4 extends Interface4A {
	public static void main(String []args) {
		Interface4 o = new Interface4();
		o.method1();
		o.method2();
		((Interface4I1) o).method1();
		((Interface4I2) o).method1();
		((Interface4I2) o).method2();

		Interface4B p = new Interface4B();
		p.method1();
		p.method2();
		((Interface4I1) p).method1();
		((Interface4I2) p).method1();
		((Interface4I2) p).method2();

		((Interface4I1) o).method1();
		((Interface4I2) o).method1();
		((Interface4I2) o).method2();

		((Interface4I1) p).method1();
		((Interface4I2) p).method1();
		((Interface4I2) p).method2();
		System.out.printf("o.f1: %d\n", o.f1);

		foo(p); foo(o);
		foo(p); foo(o);
		foo(o); foo(p);
		foo(o); foo(p);
	}

	public static void foo(Interface4I2 o) {
		o.method2();
		o.method1();
	}
}

class Interface4A implements Interface4I1, Interface4I2 {
	int f1;
	public void method1() { System.out.printf("method1 @ A\n"); }

	public int method2() { System.out.printf("method2 @ A\n"); f1 += 2; return f1; }
}

class Interface4B implements Interface4I1, Interface4I2 {
	int f1;
	public void method1() { System.out.printf("method1 @ B\n"); }
	public int method2() { System.out.printf("method2 @ B\n"); f1 += 2; return f1; }
}

interface Interface4I1 { void method1(); }
interface Interface4I2 { void method1();
               int method2(); }
