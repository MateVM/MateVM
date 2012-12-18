package tests;

public class Instance7 {
	public static void main(String []args) {
		int i_am_null = 0;
		System.out.printf("before\n");
		if (i_am_null > 0) {
			Instance7_notload a = new Instance7_notload();
			System.out.printf("loaded notload stuff o_O: %d\n", a.foo);
		} else {
			System.out.printf("Nothing to do here\n");
		}
	}

	public static class Instance7_notload {
		static {
			System.out.printf("sup, I'm Instance7_notload\n");
		}
		public int foo = 6;
	}
}
