package tests;

public class Array1 {
	public static void main(String []args) {
		int []arr = new int[0x8];
		int sum = 0;
		for (int i = 0; i < 0x8; i++) {
			arr[i] = (i + 1) * 0x11;
		}
		for (int i = 0; i < 0x8; i++) {
			sum += arr[i];
		}
		System.out.printf(sum); // 0x264
		System.out.printf(arr.length); // 0x8
	}
}
