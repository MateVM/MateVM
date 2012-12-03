package tests;

public class StaticFieldGCTest 
{  
  private static int[] something;

  public static void main(String[] args)
  {
    StaticFieldGCTest.something = new int[1024];
    for(int i=0;i<128;i++)
    {
      StaticFieldGCTest.something[i] = i;
    }
    foo();
  }

  public static void foo()
  {
    Object obj = new Object();
    gabbl();
  }

  public static void gabbl()
  {
    for(int i=0;i<128;i++)
    {
      System.out.printf("result: 0x%08x\n",StaticFieldGCTest.something[i]);
    }
  }

}

