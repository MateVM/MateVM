package tests;

public class BenchArray {
	public static void main(String []args) {
		int arr[] = new int[0x100];
		int i;

		int len = arr.length;
		for (i = 0; i < len; i++)
			arr[i] = i * 0x1337 + 0x666;

		i = 0xa000000;
		while (i != 0) {
			arr[0] = arr[0] + arr[i % len];
			i--;

			arr[0] = arr[0] - arr[i % len];
			i--;
		}

		int sum = 0;
		for (i = 0; i < len; i++)
			sum = sum + arr[i];

		System.out.printf("sum: %d\n", sum);
	}
}
