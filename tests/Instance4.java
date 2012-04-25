package tests;

public class Instance4 extends Instance2 {
	public Instance4() {
		x = 0x11;
		y = 0x22;
	}

	public static void main(String []args) {
		Instance2 a = new Instance4();
		a.getX(); // 0x1337
		Instance4 b = (Instance4) a;
		b.getX(); // 0x1337;
	}

	public int getX() {
		return 0x1337;
	}
}
