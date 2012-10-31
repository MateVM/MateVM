package tests;

public class Instance3 extends Instance2 {
	public static void main(String []args) {
		int sum = 0;
		Instance3 a = new Instance3();
		a.setX(0x33);
		System.out.printf("result: 0x%08x\n", a.getX()); // 0x33
		a.setX(0x44);
		System.out.printf("result: 0x%08x\n", a.getX()); // 0x44
	}
}
