package tests;

public class Instance5 {
	public int a, b;

	public Instance5() {

          Integer integer = new Integer(12);
	//	System.out.printf("constr(default)\n");
		this.a = 0x666;
		this.b = 0x1337;
	}

	public Instance5(int x) {
		this(x, 0x999);
		System.out.printf("constr(1)\n");
	}

	public Instance5(int x, int y) {
		System.out.printf("constr(2)\n");
		this.a = x;
		this.b = y;
	}

	public static void main(String []args) {
		System.out.printf("before calling\n");
		Instance5 x = new Instance5();
		System.out.printf("result: 0x%08x\n", x.a);
		for(;;) {}
                /*System.out.printf("result: 0x%08x\n", x.b);
		x = new Instance5(0x333, 0x777);
		System.out.printf("result: 0x%08x\n", x.a);
		System.out.printf("result: 0x%08x\n", x.b);
		x = new Instance5(0x444);
		System.out.printf("result: 0x%08x\n", x.a);
		System.out.printf("result: 0x%08x\n", x.b); */
	}
}
