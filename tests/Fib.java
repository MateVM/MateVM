package tests;

public class Fib {
	public static int fib(int n) {
		if (n <= 1)
			return 1;
		else
			return fib(n - 1) + fib(n - 2);
	}

	public static void main(String[] args) {
		System.out.printf("result: 0x%08x\n", fib(37));
	}
}
