package tests;

public class Inc2 {
	public static void main(String []args) {
		int i = 0x1337;
		for(; i < 0x01341;) {
			i++;
		}
		System.out.printf("res: %x\n", i++);
		System.out.printf("res: %x\n", ++i);
	}
}
