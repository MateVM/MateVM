package java.lang;

public class ArithmeticException extends RuntimeException {
	public ArithmeticException(String name) {
		super(name);
	}
	public String toString() {
		return "java.lang.ArithmeticException: / by zero";
	}
}
