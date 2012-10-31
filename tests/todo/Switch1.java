package tests;

public class Switch1 {
	public static void main(String []args) {
		System.out.printf("main woot\n");
		for (int i = 0; i < 0x16; i++) {
			i++;
			i++;
			switch (i % 8) {
				case 1: System.out.printf("switch: 1\n");
						break;
				case 2: System.out.printf("switch: 2\n");
						break;
				case 3: System.out.printf("switch: 3\n");
						break;
				case 4: System.out.printf("switch: 4\n");
						break;
				default: System.out.printf("default\n");
			}
		}
	}
}
