package tests;

public class BenchInstanceOf {
	public static void main(String []args) {
		int i = 0xa0;
		while (i != 0) {
			InstanceOf3.main(args);
			i--;
		}
	}
}
