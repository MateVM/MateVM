package tests;

public class Instance5 {
	public static void main(String []args) {
		int i_am_null = 0;
		if (i_am_null > 0) {
			new Instance5_notload();
			System.out.printf("loaded notload stuff o_O\n");
		} else {
			System.out.printf("Nothing to do here\n");
		}
	}
	public static class Instance5_notload {
		static {
			System.out.printf("sup, I'm Instance5_notload\n");
		}
	}
}
