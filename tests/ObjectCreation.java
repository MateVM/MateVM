package tests;

public class ObjectCreation {
	public static int checkMe;
	public static ObjectCreation obj;

	public int stuff;

	public static void main(String []args) {
		CreateMe obj = new CreateMe();
		obj.executeMe();
		obj.objcOnly();
		CreateMe.woot();
		System.out.printf("result: 0x%08x\n", CreateMe.checkMe);
	}

	public ObjectCreation() {
		this.stuff = 0x1300;
	}

	public void objcOnly() {
		ObjectCreation.checkMe += 0x3;
	}

	public void executeMe() {
		ObjectCreation.checkMe = 0xdead;
	}

	public static void woot() {
		checkMe++;
	}
	public static class CreateMe extends ObjectCreation {
		public int var1;
		public int var2;

		public CreateMe() {
			this.var1 = 0x11;
			this.var2 = 0x22;
		}

		public void executeMe() {
			ObjectCreation.checkMe = this.var1 + this.var2 + this.stuff;
		}
	}
}
