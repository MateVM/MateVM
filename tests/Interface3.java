package tests;

public class Interface3 extends Interface2 implements Inf3_I1_local {
	public int func2(int a, int b) {
		return (2 * a) + (2 * b);
	}

	public int func4() {
		return 0x122;
	}

	public static void main(String []args) {
		Interface3 o2 = new Interface3();
		Interface2 o1 = o2;
		Inf2_I1_local i1 = o1;
		Inf2_I2_local i2 = o1;
		Inf2_I3_local i3 = o1;
		Inf3_I1_local i4 = o2;
		System.out.printf("this.x: 0x%08x\n", i1.func1(0x1122));
		System.out.printf("this.x: 0x%08x\n", i2.func1(0x22));
		System.out.printf("this.x: 0x%08x\n", i3.func1(0x33));

		System.out.printf("func2: 0x%08x\n", i2.func2(0x22, 0x44));
		System.out.printf("func2: 0x%08x\n", i3.func2(0x22, 0x44));

		System.out.printf("func3: 0x%08x\n", i3.func3(0x111, 0x11));

		System.out.printf("func4: 0x%08x\n", i4.func4());

		System.out.printf("o2: 0x%08x\n", o2.func1(0x444));
		System.out.printf("o2: 0x%08x\n", o2.func2(0x845, 0x111));
		System.out.printf("o2: 0x%08x\n", o2.func3(0x654, 0x153));
		System.out.printf("o2: 0x%08x\n", o2.func4());
	}
}

interface Inf3_I1_local {
	int func4();
}
