package tests;

public class Switch4 {
	public static void main(String []args) {
		System.out.printf("sup?\n");
		for (int i = 0; i < 0x11100; i++) {
			switch (i % 0x200) {
				case 1: System.out.printf("switch: 1\n"); break;
				case 10: System.out.printf("switch: 10\n");
				case 100: System.out.printf("switch: 1000\n"); break;
			}
		}
	}
}
