package jmate.lang;

public class MateRuntime {

	public static native void loadLibrary(String lib);
        public static native int getCurrentHeapSize();
}
