package tests;

public class BenchInterface extends Interface4A {
	public static void main(String []args) {
		BenchInterface o = new BenchInterface();
		Interface4B p = new Interface4B();
		System.out.printf("o.f1: %d\n", o.f1);
		System.out.printf("p.f1: %d\n", p.f1);

		Interface4I2 o_ = (Interface4I2) o;
		Interface4I2 p_ = (Interface4I2) p;
		int i = 0x90000;
		while (i != 0) {
			o_.method2(); o_.method1();
			p_.method2(); p_.method1();
			i--;
		}
		System.out.printf("o.f1: %d\n", o.f1);
		System.out.printf("p.f1: %d\n", p.f1);
	}
}

class Interface4A implements Interface4I1, Interface4I2 {
	int f1;
	public void method1() { f1 += 0x1337; }
	public int method2() { f1 += 2; return f1; }
}

class Interface4B implements Interface4I1, Interface4I2 {
	int f1;
	public void method1() { f1 += 0x1337; }
	public int method2() { f1 += 2; return f1; }
}

interface Interface4I1 { void method1(); }
interface Interface4I2 { void method1(); int method2(); }
