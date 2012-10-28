package tests;

//import jmate.lang.MateRuntime;

public class Garbage1
{
	static
	{
		loadLibrary();
	}

	public static void loadLibrary()
	{
		//Runtime.getRuntime().loadLibrary("MateRuntime");
	}	

	public Garbage1(){}

	public static void main(String args[])
	{
         	//MateRuntime runtime = new MateRuntime();
		System.out.println("a string object");

		for(int i=0;i<0x2800;i++)
		{
			Big2 big2 = new Big2(); 
                        if(i%0x1F==0) 
                        { 
                           //runtime.printMemoryUsage();
                            //System.out.printf("foo gah 0x%08x\n", i);
                        }
                        big2.foo();
		}
		//System.out.println("done.");
	}
}

class Big2
{
	//private int[]arr;

	public Big2()
	{
		//arr = new int[0x10];//int[0xF400];
		//System.out.println("foo");
	}
        
        public void foo()
        {
        }
}
