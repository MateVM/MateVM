package tests;

public class Static9 {
	public static Static9 obj;
	public int t;

	static {
		Static9.obj = new Static9(0x1337);
	}

	Static9(int value) {
		this.t = value;
	}

	public static void main(String []args) {
		System.out.printf("0x%08x\n", obj.t);
	}
}
