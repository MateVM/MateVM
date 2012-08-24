package scratch;

import jmate.lang.MateRuntime;

public class GCTest
{
	public static void main(String[] args)
	{
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
