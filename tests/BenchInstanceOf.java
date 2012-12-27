package tests;

public class BenchInstanceOf {
	static interface i1 { };
	static interface i2 { };
	static interface i3 extends i2 { };
	static interface i4 extends i3 { };
	static interface i5 { };
	static interface i6 extends i3 { };
	static interface i7 extends i4, i6 { };
	static interface i8 extends i2, i7 { };
	static class c1 { };
	static class c2 extends c1 implements i1 { };
	static class c3 extends c2 implements i4 { };
	static class c4 { };

	public static void main(String []args) {
		Object arr[] = new Object[5];
		arr[0] = new c3();
		arr[1] = new i3() { };
		arr[2] = new i6() { };
		arr[3] = new i7() { };
		arr[4] = new i8() { };
		int len = arr.length;

		int i = 0x700;
		int cnt = 0;
		while (i != 0) {
			for (int j = 0; j < 5; j++) {
				Object o = arr[j];
				if (o instanceof c1) cnt++;
				if (o instanceof c2) cnt++;
				if (o instanceof c3) cnt++;
				if (o instanceof c4) cnt++;

				if (o instanceof i1) cnt++;
				if (o instanceof i2) cnt++;
				if (o instanceof i3) cnt++;
				if (o instanceof i4) cnt++;
				if (o instanceof i5) cnt++;
				if (o instanceof i6) cnt++;
				if (o instanceof i7) cnt++;
				if (o instanceof i8) cnt++;
			}
			i--;
		}
		System.out.printf("cnt: %d\n", cnt);
	}
}
