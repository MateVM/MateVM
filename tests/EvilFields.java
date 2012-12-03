package tests;

public class EvilFields
{
  public int a;
  public int b;
  public int c;

  public EvilFields()
  {
    // these are funny adresses on my current machine. dont bother
    a = 0x44e00008;
    b = 0x44e0000f;
    c = 0x44e0001f;
    Integer blub = new Integer(100); // do some alloc
  }

  public static void main(String[] args) throws Exception
  {
    System.out.printf("some thing\n");
    EvilFields evil = new EvilFields();
    System.out.printf("another alloc\n");
    System.out.printf("fields fire lazors: 0x%08x\n",evil.a); 
    System.out.printf("fields fire lazors: 0x%08x\n",evil.b);
    System.out.printf("fields fire lazors: 0x%08x\n",evil.c);
    if(evil.a != 0x44e00008 || evil.b != 0x44e0000f || evil.c!=0x44e0001f)
    {
      throw new Exception();
    }
  }
}
