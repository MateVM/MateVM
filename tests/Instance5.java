package tests;

public class Instance5 {
	public int x;
	public Instance5() {
		x = 0x11;
	}

	public static void main(String []args) {
		Instance5 a = new Instance5();

		a.setX(0x1337);
		a.printX(a.x);
	}

	public void setX(int a) {
		this.x = a;
	}

	public native void printX(int x);
}
