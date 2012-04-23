package tests;

public class Fac {
	public static void main(String args[]) {
		int sum = 0;
		for (int i = 0; i < 10; i++) {
			// fac(i);
			sum += fac(i);
		}
		id(sum);
		// System.out.printf("fac: 0x%08x\n", sum);
	}

	public static int id(int i) {
		return i;
	}

	public static int fac(int a) {
		int b = 1;
		while (a > 0) {
			b *= a;
			a--;
		}
		return b;
	}

	public static int facFor(int n){
        	int p = 1;
 		for(int i=1;i<=n;i++)
		{
			p = p * i;
		}
		return p;
	}
}
