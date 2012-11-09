package tests;

public class Exception5 {
	public static class Ex1 extends NullPointerException { }
	public static class Ex2 extends Ex1 { }
	public static class Ex3 extends Ex2 { }

	public static class Xe1 extends IllegalArgumentException { }
	public static class Xe2 extends Xe1 { }
	public static class Xe3 extends Xe2 { }

	public static void main(String []args) {
		try {
			throw new Ex1();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new Ex1();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		try {
			throw new Ex2();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new Ex2();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		try {
			throw new Ex3();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new Ex3();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		/* ***************** */
		try {
			throw new Xe1();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new Xe1();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		try {
			throw new Xe2();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new Xe2();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		try {
			throw new Xe3();
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		try {
			throw new Xe3();
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		} catch (NullPointerException _) {
			System.out.printf("NullPointerException\n");
		}
		/* ***************** */
		try {
			throw new Xe3();
		} catch (Xe3 _) {
			System.out.printf("Xe3\n");
		} catch (Xe2 _) {
			System.out.printf("Xe2\n");
		} catch (Xe1 _) {
			System.out.printf("Xe1\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
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
		try {
			throw new Xe1();
		} catch (Xe3 _) {
			System.out.printf("Xe3\n");
		} catch (Xe2 _) {
			System.out.printf("Xe2\n");
		} catch (Xe1 _) {
			System.out.printf("Xe1\n");
		} catch (IllegalArgumentException _) {
			System.out.printf("IllegalArgumentException\n");
		}
		/* TODO(bernhard): are there more cases regarding subtypes? */
		System.out.printf("hmmm\n");
	}
}
