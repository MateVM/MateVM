package tests;

public class Strings1 {
	public static void main(String []args) {
		String a = "abc";
		String b = "abc";
		String c = "wtf";

		if (a == b) {
			System.out.println("result: okay :-)");
		} else {
			System.out.println("result: bad :-(");
		}

		if (a != c) {
			System.out.println("result: okay :-)");
		} else {
			System.out.println("result: bad :-(");
		}

		if (a == c) {
			System.out.println("result: bad :-(");
		} else {
			System.out.println("result: okay :-)");
		}
	}
}
