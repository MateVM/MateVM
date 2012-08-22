package tests;


public class Garbage1
{
	

	public Garbage1(){}

	public static void main(String args[])
	{

	        Big2 big2 = new Big2();
		for(int i=0;i<0x2800;i++)
		{
			big2 = new Big2();
		}
		System.out.println("memory: todo");
	}
}

class Big2
{
	private int[]arr;

	public Big2()
	{
		arr = new int[0x400];
		//System.out.println("foo");
	}
}
