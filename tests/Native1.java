package tests;

public class Native1 {
	public static void main(String []args) {
		printSomething();
		for (int i = 0; i < 10; i++)
			printNumber(0x1337 + i);
		printSomething();
		printSomething();
		printNumber(0x15a5);
	}

	public static void printNumber(int a) {
		printSomething(a);
	}

	public static native void printSomething();
	public static native void printSomething(int a);
}
