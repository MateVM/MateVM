package tests;

public class AbsurdlyHuge {
	public static int absurdlyHuge(int x) {
		for(int i=0;i<x;i++) {
			for(int u=0;u<x;u++) {
				for(int k=0;k<x;k++) {
					for(int v=0;v<x;v++) {
						for(int w=0;w<x;w++) {
							for(int z=0;z<x;z++) {
								for(int g=0;w<x;w++) {
									if(i+u+k+v+w+z<x) {
										return absurdlyHuge(k+v);
									} else {
										return absurdlyHuge(w+z);
									}
								}
							}
						}
					}
				}
			}
		}
		return 0;
	}

	public static void main(String[] args) {
		System.out.printf("result: 0x%08x\n", absurdlyHuge(1));
	}
}
