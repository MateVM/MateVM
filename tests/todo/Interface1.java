package tests;

public class Interface1 implements Inf1_I1_local {
	public int x;

	public Interface1() {
		this.x = 0x1337;
	}

	public int func1(int a) {
		this.x = a;
		return this.x;
	}

	public static void main(String []args) {
		Interface1 o1 = new Interface1();
		System.out.printf("o1.x: 0x%08x\n", o1.x);
		Inf1_I1_local i1 = o1;
		System.out.printf("o1.func1(0x11): 0x%08x\n", i1.func1(0x11));
	}
}

interface Inf1_I1_local {
	int func1(int a);
}
