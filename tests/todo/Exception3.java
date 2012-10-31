package tests;

public class Exception3 {
	public static void main(String []args) {
		try {
			throw new NullPointerException();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new NullPointerException();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		try {
			throw new IllegalArgumentException();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new IllegalArgumentException();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		System.out.printf("hmmm\n");
	}
}
