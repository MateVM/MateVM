package tests;

public class INeg1 {
	public static void main(String []args) {
		int a = 5;
		System.out.printf("%d\n", -a);
		System.out.printf("%d\n", 0 - a);
		System.out.printf("%d\n", a);
		
		int b = 0xffffee00;
		System.out.printf("%d\n", -b);
		System.out.printf("%d\n", 0 - b);
		System.out.printf("%d\n", b);
	}
}
