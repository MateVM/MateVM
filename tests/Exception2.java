package tests;

public class Exception2 {
	public static void main(String []args) {
		int foo = 0x666;
		try {
			throw new NullPointerException();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
	}
}
