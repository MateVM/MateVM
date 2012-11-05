package tests;

public class Exception13 {
	static public void main(String []args) {
		try {
			int b = 0;
			int a = 12 / b;
		} catch (Exception e) {
			System.out.printf(e.toString());
			System.out.printf("\n");
		}

		try {
			int b = 0;
			int a = 12 % b;
		} catch (Exception e) {
			System.out.printf(e.toString());
			System.out.printf("\n");
		}
	}
}
