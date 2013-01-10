/* Example 8.3.1.1-2. from java language specification */
package tests;

class Static10super {
	public static int x = 2;
}

class Static10 extends Static10super {
	public static int x = 3;

	public static void main(String[] args) {
		new Static10().printX();
	}

	void printX() {
		System.out.printf("this.x:  %d\n", this.x);
		System.out.printf("super.x: %d\n", super.x);
	}
}
