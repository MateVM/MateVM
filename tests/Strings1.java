package tests;

public class Strings1 {
	public static void main(String []args) {
		String a = "abc";
		String b = "abc";
		String c = "wtf";

		if (a == b) {
			System.out.println("okay :-)");
		} else {
			System.out.println("bad :-(");
		}

		if (a != c) {
			System.out.println("okay :-)");
		} else {
			System.out.println("bad :-(");
		}

		if (a == c) {
			System.out.println("bad :-(");
		} else {
			System.out.println("okay :-)");
		}
	}
}
