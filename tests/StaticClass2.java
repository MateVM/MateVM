package tests;

public class StaticClass2 {
	public static class SC {
		public static int b;
		public int hrm = 543;
		public void printMe(int huhu) {
			System.out.printf("I'm SC %d %x, and you say \"%d\"\n", hrm, this.b, huhu);
		}
	}

	public SC a = new SC();

	public static void main(String []args) {
		SC.b = 0x1337;
		new StaticClass2().a.printMe(888);
	}
}
