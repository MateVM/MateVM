package tests;

public class Shift2 {
	public static void main(String []args) {
		int s[] = {0x11883399, 0x99883377, 0x11, 0x33, 0x44, 0x79531ccc, 0xffffffff};
		for (int j = 0; j < s.length; j++) {
			for (int i = 0; i < 32; i++) {
				System.out.printf("shifted: 0x%08x\n", s[j] >> i);
				System.out.printf("shifted: 0x%08x\n", s[j] >>> i);
				System.out.printf("shifted: 0x%08x\n", s[j] << i);
			}
		}
	}
}
