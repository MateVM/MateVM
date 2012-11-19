package tests;

public class RegAlloc2 {
	public int x;
	public int y;

	public RegAlloc2() {
		x = 0x66;
		y = 0x77;
	}

	public static void main(String []args) {
		int sum = 0;
		RegAlloc2 a = new RegAlloc2();
		RegAlloc2 b = new RegAlloc2();
		sum += a.x; // 0x66
		sum += b.x; // 0x66
		System.out.printf("normal\n");
		System.out.printf("wtf oida: %d\n", sum);
	}
}
