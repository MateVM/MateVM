package jmate.lang;

public class Integer {
	int value;

	public Integer(int a) {
		this.value = a;
	}

	public int intValue() {
		return this.value;
	}

	public static jmate.lang.Integer valueOf(int a) {
		return new jmate.lang.Integer(a);
	}
}
