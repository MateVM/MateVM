package tests;

public class Exception1 {
	public static void main(String []args) {
		try {
			System.out.printf("hello exception stuff\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
	}
}
