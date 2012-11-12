package tests;

public class WhileArray1 {
	public char arr[] = new char[10];

	public static void main(String args[]) {
		char arr[] = new char[2];
		arr[0] = (char) 0x65;
		arr[1] = 0x66;
		System.out.printf("lol: %c\n", (char) arr[0]);
		System.out.printf("lol: %c\n", arr[1]);
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
		System.out.printf("equal: len is %d\n", len);
		while (--len >= 0) {
			System.out.printf("\tidx: x: %d, y: %d\n", x, y);
			if (a.arr[x++] != b.arr[y++]) {
				System.out.printf("\tcmp failed, return false\n");
				return false;
			}
		}
		return true;
	}
}
