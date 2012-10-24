public class Play {
	public static void f1(int a) {
		return;
	}
	public static void f2(int a, float b) {
		return;
	}
	public static int f3(int a, float b, int c) {
		return c;
	}
	public static float f4(float a, float b, int c) {
		return b;
	}
	public static void main(String []args) {
		int ia = 0xa;
		int ic = 0xc;
		float fa = 1.0f;
		float fb = 2.0f;
		f1(ia);
		f2(ia, fb);
		int r3 = f3(ia, fb, ic);
		float f4 = f4(fa, fb, ic);
	}
}
