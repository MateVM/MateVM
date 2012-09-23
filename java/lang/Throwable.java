package java.lang;

public class Throwable {
	public String msg;

	public Throwable () {
		this.msg = "I'm a Throwable";
	}

	public Throwable(String msg) {
		this.msg = msg;
	}

	public String getMessage() {
		return this.msg;
	}

	public String toString() {
		return "java.lang.Throwable";
	}
}
