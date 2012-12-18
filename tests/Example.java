/* example in thesis of wurm;  forced a double free or corruption bug */
package tests;

public class Example extends A {
	public static void main(String []args) {
		Example o = new Example();
		o.method1();
		o.method2();
		((I1) o).method1();
		((I2) o).method1();
		((I2) o).method2();
		System.out.printf("o.f1: %d\n", o.f1);
	}
}

class A implements I1, I2 {
	int f1;
	public void method1() { System.out.printf("method1\n"); }

	public int method2() { f1 += 2; return f1; }
}

interface I1 { void method1(); }
interface I2 { void method1();
               int method2(); }
