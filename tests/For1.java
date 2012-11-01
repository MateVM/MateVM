package tests;

public class For1 {
	public static int for1(int x) {
		for (int i = 0; i < x; i++) {
			return 0x666;
		}
		return 0x1337;
	}

	public static void main(String[] args) {
		System.out.printf("result: 0x%08x\n", for1(0));
		System.out.printf("result: 0x%08x\n", for1(1));
		System.out.printf("result: 0x%08x\n", for1(2));
		System.out.printf("result: 0x%08x\n", for1(3));
		System.out.printf("result: 0x%08x\n", for1(4));
	}
}
