package tests;

public class Exception6 {
	public static class Ex1 extends NullPointerException {
		static { System.out.printf("static init @ Ex1\n"); }
	}
	public static class Ex2 extends Ex1 {
		static { System.out.printf("static init @ Ex2\n"); }
	}
	public static class Ex3 extends Ex2 {
		static { System.out.printf("static init @ Ex3\n"); }
	}

	public static class Xe1 extends IllegalArgumentException {
		static { System.out.printf("static init @ Xe1\n"); }
	}
	public static class Xe2 extends Xe1 {
		static { System.out.printf("static init @ Xe2\n"); }
	}
	public static class Xe3 extends Xe2 {
		static { System.out.printf("static init @ Xe3\n"); }
	}

	public static void main(String []args) {
		/* test lazy class loading... */
		try {
			throw new Ex1();
		} catch (Ex3 _) {
			System.out.printf("Ex3\n");
		} catch (Ex2 _) {
			System.out.printf("Ex2\n");
		} catch (Ex1 _) {
			System.out.printf("Ex1\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		try {
			throw new Xe2();
		} catch (Xe3 _) {
			System.out.printf("Xe3\n");
		} catch (Xe2 _) {
			System.out.printf("Xe2\n");
		} catch (Xe1 _) {
			System.out.printf("Xe1\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
	}
}
