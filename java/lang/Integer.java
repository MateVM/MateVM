package java.lang;

public class Integer {
	int value;

	public Integer(int a) {
		this.value = a;
	}

	public int intValue() {
		return this.value;
	}

	public static Integer valueOf(int a) {
		return new Integer(a);
	}
}
