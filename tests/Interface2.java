package tests;

public class Interface2 implements Inf2_I1_local, Inf2_I2_local, Inf2_I3_local {
	public int x;

	public Interface2() {
		this.x = 0x1337;
	}

	public int func1(int a) {
		this.x = a;
		return this.x;
	}

	public int func2(int a, int b) {
		return a + b;
	}

	public int func3(int a, int b) {
		return a - b;
	}

	public static void main(String []args) {
		Interface2 o1 = new Interface2();
		Inf2_I1_local i1 = o1;
		Inf2_I2_local i2 = o1;
		Inf2_I3_local i3 = o1;
		System.out.printf("this.x: 0x%08x\n", i1.func1(0x1122));
		System.out.printf("this.x: 0x%08x\n", i2.func1(0x22));
		System.out.printf("this.x: 0x%08x\n", i3.func1(0x33));

		System.out.printf("func2: 0x%08x\n", i2.func2(0x22, 0x44));
		System.out.printf("func2: 0x%08x\n", i3.func2(0x22, 0x44));

		System.out.printf("func3: 0x%08x\n", i3.func3(0x111, 0x11));
	}
}

interface Inf2_I1_local {
	int func1 (int a);
}

interface Inf2_I2_local {
	int func1 (int a);
	int func2 (int a, int b);
}

interface Inf2_I3_local extends Inf2_I2_local, Inf2_I1_local {
	int func1 (int a);
	int func3 (int a, int b);
}
