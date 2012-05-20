package java.lang;

public class Character {
	char value;

	public Character(char a) {
		this.value = a;
	}

	public static Character valueOf(char a) {
		return new Character(a);
	}
}
