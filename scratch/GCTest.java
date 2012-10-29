package scratch;

import jmate.lang.MateRuntime;

public class GCTest
{
        public static void gabber(int a)
        {
          //Integer b = Integer.valueOf(a);
           Object x = new Object();
           System.out.printf("0x%08x",a);
        }

        public static void blubber(int a)
        {
          Object y = new Object();
          gabber(a);
        }

	public static void main(String[] args)
	{
          Integer.valueOf(12);

          String something = "something";
          blubber(24);
		List myList = new List(3,
			new List(5,
				new List(6,
					new List(10,null))));

		MateRuntime.printGCStats();
		System.out.println("done.");
  
        }
} 

class List
{
	public int elem;
	public List xs;

	public List(int elem, List xs)
	{
		this.elem = elem;
		this.xs = xs;
	}
}
