package java.io;

import jmate.io.*;

public class PrintStream {
	public void println(String a) {
		new jmate.io.PrintStream().println(a);
	}

	public PrintStream printf(String format, Object... args) {
		/* TODO ... */
		new jmate.io.PrintStream().printf(format, args);
		return this;
	}
}
