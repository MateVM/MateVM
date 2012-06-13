package tests;

public class WhileArray1 {
	public char arr[] = new char[10];

	public static void main(String args[]) {
		WhileArray1 a = new WhileArray1();
		WhileArray1 b = new WhileArray1();
		for (int i = 0; i < 10; i++) {
			a.arr[i] = b.arr[i] = (char) i;
		}
		System.out.printf("success? %d\n", equal(a, b, 10) ? 1 : 0);

		for (int i = 0; i < 10; i++) {
			b.arr[i] = (char) i;
			a.arr[i] = (char) (b.arr[i] + 2);
		}
		System.out.printf("success? %d\n", equal(a, b, 10) ? 1 : 0);
	}

	public static boolean equal(WhileArray1 a, WhileArray1 b, int len) {
		/* stolen from the equals implementation of java.lang.String of
		 * GNU Classpath */
		int x = 0, y = 0;
		while (--len >= 0) {
			System.out.printf("idx: x: %d, y: %d\n", x, y);
			if (a.arr[x++] != b.arr[y++]) {
				return false;
			}
		}
		return true;
	}
}
