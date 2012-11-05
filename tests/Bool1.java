package tests;

public class Bool1 {
	public static void main(String []args) {
		if(foo(1)) System.out.printf("fooo \\o/\n"); else System.out.printf("fooo /o\\\n");
		if(foo(0)) System.out.printf("fooo \\o/\n"); else System.out.printf("fooo /o\\\n");
	}

	public static boolean foo(int a) {
		if (a > 0)
			return true;
		else
			return false;
	}
}
