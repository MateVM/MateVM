public class While {
	public static int f(int a, int b) {
		do {
			a += b;
			b--;
		} while (b > 0);
		return a;
	}

	public static int g(int a, int b) {
		while (b > 0) {
			a += b;
			b--;
		}
		return a;
	}
}
