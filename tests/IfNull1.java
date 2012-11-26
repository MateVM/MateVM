package tests;

public class IfNull1 {
	public static void main(String []args) {
		IfNull1 i = null;
		if (i == null) {
			System.out.printf("i is null\n");
		} else {
			System.out.printf("i isn't null\n");
		}
		if (i != null) {
			System.out.printf("i isn't null\n");
		} else {
			System.out.printf("i is null\n");
		}
	}
}
