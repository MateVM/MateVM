package tests;

public class InstanceOf1 {
	public static void main(String []args) {
		Instance1 x = new Instance1();
		if (x instanceof Instance1) {
			System.out.printf("x is instance of Instance1 :-)\n");
		} else {
			System.out.printf("x is *not* instance of Instance1 :-(\n");
		}
		if (x instanceof Instance2) {
			System.out.printf("x is instance of Instance2 :-)\n");
		} else {
			System.out.printf("x is *not* instance of Instance2 :-(\n");
		}
	}
}
