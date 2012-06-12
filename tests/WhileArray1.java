package tests;

public class WhileArray1 {
	public static void main(String args[]) {
		char a[] = new char[10];
		char b[] = new char[10];
		for (int i = 0; i < 10; i++) {
			a[i] = b[i] = (char) i;
		}
		System.out.printf("success? %d\n", equal(a, b, 5) ? 1 : 0);

		for (int i = 0; i < 10; i++) {
			b[i] = (char) i;
			a[i] = (char) (b[i] + 2);
		}
		System.out.printf("success? %d\n", equal(a, b, 5) ? 1 : 0);
	}

	public static boolean equal(char[] a, char[] b, int len) {
		/* stolen from the equals implementation of java.lang.String of
		 * GNU Classpath */
		int x = 0, y = 0;
		while (--len >= 0) {
			System.out.printf("idx: x: %d, y: %d\n", x, y);
			if (a[x++] != b[y++]) {
				return false;
			}
		}
		return true;
	}
}
