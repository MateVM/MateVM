package tests;

public class BenchInterface extends Interface4ABI {
	public static void main(String []args) {
		BenchInterface o = new BenchInterface();
		Interface4BBI p = new Interface4BBI();
		System.out.printf("o.f1: %d\n", o.f1);
		System.out.printf("p.f1: %d\n", p.f1);

		Interface4I2BI o_ = (Interface4I2BI) o;
		Interface4I2BI p_ = (Interface4I2BI) p;
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

class Interface4ABI implements Interface4I1BI, Interface4I2BI {
	int f1;
	public void method1() { f1 += 0x1337; }
	public int method2() { f1 += 2; return f1; }
}

class Interface4BBI implements Interface4I1BI, Interface4I2BI {
	int f1;
	public void method1() { f1 += 0x1337; }
	public int method2() { f1 += 2; return f1; }
}

interface Interface4I1BI { void method1(); }
interface Interface4I2BI { void method1(); int method2(); }
