package tests;

public class Instance2 extends Instance1 {
	public int y;

	public Instance2() {
		x = 0x66;
		y = 0x77;
	}

	public static void main(String []args) {
		int sum = 0;
		Instance1 a = new Instance1();
		Instance2 b = new Instance2();
		sum += a.x; // 0x55
		sum += b.x; // 0x66
		sum += b.y; // 0x77
		a.x = 0x11; sum += a.x; // 0x11
		b.x = 0x22; sum += b.x; // 0x22
		b.y = 0x33; sum += b.y; // 0x33
		Instance1.id(sum); // 0x198
	}
}
