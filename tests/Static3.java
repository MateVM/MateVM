package tests;

public class Static3 extends Static1 {
	public static int z;
	public static int sum;
	public static void main(String []args) {
		sum = 0;
		Static3.x = 0x111;
		Static3.y = 0x555;
		sum += Static1.addNumbers(); // 0x666
		Static1.setNumbers();
		sum += Static1.addNumbers(); // 0x33
		Static3.z = 0x11;
		sum += Static3.addNumbers(); // 0x44
		getSum(); // 0x666 + 0x33 + 0x44 = 0x6dd
		// System.out.printf("%x\n", getSum());
	}

	public static int getSum() {
		return sum;
	}

	public static int addNumbers() {
		return x + y + z;
	}
}
