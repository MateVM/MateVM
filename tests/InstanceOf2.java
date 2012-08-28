package tests;

public class InstanceOf2 {
	public static void main(String []args) {
		System.out.printf("x = new InstanceOf2_local;\n");
		Instance1 x = new InstanceOf2_local();
		checkInstance(null instanceof Instance1, "null", "Instance1");
		checkInstance(x instanceof Instance1, "x", "Instance1");
		checkInstance(x instanceof Instance2, "x", "Instance2");
		checkInstance(x instanceof InstanceOf2_local, "x", "InstanceOf2_local");
		checkInstance(x instanceof Object, "x", "Object");
		checkInstance(x instanceof InstanceOf2_local2, "x", "InstanceOf2_local2");

		System.out.printf("\n\n");
		System.out.printf("y = new InstanceOf2_local2;\n");
		Object y = new InstanceOf2_local2();
		checkInstance(null instanceof Instance1, "null", "Instance1");
		checkInstance(y instanceof Instance1, "y", "Instance1");
		checkInstance(y instanceof Instance2, "y", "Instance2");
		checkInstance(y instanceof InstanceOf2_local, "y", "InstanceOf2_local");
		checkInstance(y instanceof Object, "y", "Object");
		checkInstance(y instanceof InstanceOf2_local2, "y", "InstanceOf2_local2");
	}

	public static void checkInstance(boolean cond, String obj, String classname) {
		System.out.printf(obj);
		if (cond) {
			System.out.printf(" is instance of ");
			System.out.printf(classname);
			System.out.printf(" :-)\n");
		} else {
			System.out.printf(" is *not* instance of ");
			System.out.printf(classname);
			System.out.printf(" :-(\n");
		}
	}
}


class InstanceOf2_local extends Instance1 {
}

class InstanceOf2_local2 extends InstanceOf2_local {
}
