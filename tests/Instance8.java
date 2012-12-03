package tests;

public class Instance8 {
	public static void main(String []args) {
		int i_am_null = 0;
		if (i_am_null > 0) {
			new Instance8_notload();
			System.out.printf("loaded notload stuff o_O\n");
		} else {
			System.out.printf("Nothing to do here\n");
		}
	}
}

class Instance8_notload {
	static {
		System.out.printf("sup, I'm Instance8_notload\n");
	}
}
