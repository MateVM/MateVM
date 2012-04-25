package tests;

public class Instance1 {
	public int x;

	public Instance1() {
		x = 0x55;
	}

	public static void main(String []args) {
		Instance1 a = new Instance1();
		id(a.x); // 0x55
		a.x = 0x11;
		id(a.x); // 0x11
	}

	public static int id(int a) {
		// System.out.printf("0x%08x\n", a);
		return a;
	}
}
