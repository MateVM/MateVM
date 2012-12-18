package tests;

public class Interface4 extends A {
	public static void main(String []args) {
		Interface4 o = new Interface4();
		o.method1();
		o.method2();
		((I1) o).method1();
		((I2) o).method1();
		((I2) o).method2();

		B p = new B();
		p.method1();
		p.method2();
		((I1) p).method1();
		((I2) p).method1();
		((I2) p).method2();

		((I1) o).method1();
		((I2) o).method1();
		((I2) o).method2();

		((I1) p).method1();
		((I2) p).method1();
		((I2) p).method2();
		System.out.printf("o.f1: %d\n", o.f1);

		foo(p); foo(o);
		foo(p); foo(o);
		foo(o); foo(p);
		foo(o); foo(p);
	}

	public static void foo(I2 o) {
		o.method2();
		o.method1();
	}
}

class A implements I1, I2 {
	int f1;
	public void method1() { System.out.printf("method1 @ A\n"); }

	public int method2() { System.out.printf("method2 @ A\n"); f1 += 2; return f1; }
}

class B implements I1, I2 {
	int f1;
	public void method1() { System.out.printf("method1 @ B\n"); }
	public int method2() { System.out.printf("method2 @ B\n"); f1 += 2; return f1; }
}

interface I1 { void method1(); }
interface I2 { void method1();
               int method2(); }
