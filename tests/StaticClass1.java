package tests;

public class StaticClass1 {
	public static class SC {
		public static void printMe(int huhu) {
			System.out.printf("I'm SC, and you say \"%d\"\n", huhu);
		}
	}

	public static void main(String []args) {
		SC.printMe(1337);
	}
}
