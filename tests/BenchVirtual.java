package tests;

public class BenchVirtual extends BenchVirtualSuper {
	public static void main(String []args) {
		BenchVirtual o = new BenchVirtual();
		BenchVirtualSuper p = (BenchVirtualSuper) o;

		int i = 0x7000000;
		while (i != 0) {
			o.method1();
			o.method2();

			p.method1();

			i--;
		}
		System.out.printf("o.f1: %d\n", o.f1);
		System.out.printf("p.f1: %d\n", p.f1);
	}

	public void method2() { f1 += 0x333; }
}

class BenchVirtualSuper {
	int f1;
	public void method1() { f1 += 0x666; }
}
