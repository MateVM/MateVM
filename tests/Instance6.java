package tests;

public class Instance6 {
	public static void main(String []args) {
		int i_am_null = 0;
		System.out.printf("before\n");
		if (i_am_null > 0) {
			Instance6_notload a = new Instance6_notload();
			a.lol();
			System.out.printf("loaded notload stuff o_O\n");
		} else {
			System.out.printf("Nothing to do here\n");
		}
	}
	public static class Instance6_notload {
		static {
			System.out.printf("sup, I'm Instance6_notload\n");
		}
		void lol() {
			System.out.printf("lolololololo\n");
		}
	}
}
