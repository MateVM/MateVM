package tests;

public class Array3 {
	public static void main(String []args) {
		int a = 4;
		int cnt = a + 3;
		int arr[] = new int[cnt];
		for (int i = 0; i < cnt; i++) {
			arr[i] = i + 10;
		}
		for (int i = 0; i < cnt; i++) {
			System.out.printf("i: %d\n", arr[i]);
		}
	}
}
