package tests;

public class Array1 {
	public static void main(String []args) {
		// int []arr = new int[0x8];
		// int sum = 0;
		// for (int i = 0; i < 0x8; i++) {
		// 	arr[i] = (i + 1) * 0x11;
		// }
		// for (int i = 0; i < 0x8; i++) {
		// 	sum += arr[i];
		// }
		// System.out.printf("result: 0x%08x\n", sum); // 0x264
		// System.out.printf("result: 0x%08x\n", arr.length); // 0x8
		System.out.printf("result: 0x%08x\n", arrlol());
		for (int i = 0; i < 0x8; i++) {
			System.out.printf("i: %d\n", i); // arr[i] = (i + 1) * 0x11;
		}
	}

	public static int arrlol() {
		int []arr = new int[0xa];
		arr[0] = 0x11;
		arr[1] = 0x12;
		arr[2] = 0x13;
		arr[3] = 0x14;
		arr[4] = 0x15;
		arr[5] = 0x16;
		arr[6] = 0x17;
		arr[7] = 0x18;
		arr[8] = 0x19;
		arr[9] = 0x1a;
		return arr[8];
	}
}
