package tests;

/* nested exception regions */
public class Exception11 {
	public static void main(String []args) {
		int y = 0;
		try {
			y = 1;
			try {
				if (y > 0) {
					throw new IllegalArgumentException();
				} else {
					System.out.printf("do not print this\n");
					return;
				}
			} catch (IllegalArgumentException _) {
				System.out.printf("yeah, aha\n");
			}
			System.out.printf("everytime\n");
			try {
				if (y < 10) {
					throw new IllegalArgumentException();
				} else {
					System.out.printf("never print this!!!111\n");
				}
				if (y == 0) {
					try {
						if (y < 10) {
							throw new IllegalArgumentException();
						} else {
							System.out.printf("never print this!!!111\n");
							return;
						}
					} catch (IllegalArgumentException _) {
						System.out.printf("this is not okay :-(\n");
					}
				}
			} catch (IllegalArgumentException _) {
				System.out.printf("also okay :-)\n");
			}
		} catch (IllegalArgumentException _) {
			System.out.printf("this is wrong :-(\n");
		}
		System.out.printf("over and out\n");
	}
}
