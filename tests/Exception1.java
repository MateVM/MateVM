package tests;

public class Exception1 {
	public static void main(String []args) {
		System.out.printf("sup\n");
		try {
			System.out.printf("hello exception stuff\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
	}
}
