package tests;

public class Exception4 {
	public static void main(String []args) {
		try {
			throw new NullPointerException();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} finally {
			System.out.printf("it's finally over\n");
		}

		try {
			System.out.printf("I'm fine\n");
		} finally {
			System.out.printf("o rly\n");
		}
		System.out.printf("hmmm\n");
	}
}
