package tests;

public class Instance4 {
	public int a, b;

	public Instance4() {
		this(0x666, 0x1337);
	}

	public Instance4(int x, int y) {
		this.a = x;
		this.b = y;
	}

	public static void main(String []args) {
		Instance4 x = new Instance4();
		System.out.printf("result: 0x%08x\n", x.a);
		System.out.printf("result: 0x%08x\n", x.b);
	}
}
