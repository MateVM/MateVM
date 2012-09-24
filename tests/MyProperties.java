package tests;

public class MyProperties extends MyHashtable<Object, Object> {
	public static void main(String []args) {
		Lulz a = new Lulz();
		System.out.printf("woot\n");
		for (int i = 0; i < 0x100; i++) {
			MyProperties m = new MyProperties();
			MyProperties n = new MyProperties();
			m.putAll(n);
		}
	}
}


class MyHashtable<K, V> implements MyMap<K, V> {
	static interface i1 {
		void foo();
	};
	static interface i2 {
		void bar();
	};
	static interface i3 extends i2 {
		void stfu();
	};
	static interface i4 extends i3 {
		void foo();
	};
	static interface i5 {
		void baz();
	};
	static interface i6 extends i3 {
		void foo();
		void bar();
		void baz();
	};
	static interface i7 extends i4, i6 {
		void foo();
		void bar();
		void baz();
	};
	static interface i8 extends i2, i7 {
		void foo();
		void bar();
		void baz();
	};
	static class Lulz implements i8, i7, i6, i5 {
		public void foo() {
			bar();
		}
		public void baz() {
			bar();
		}
		public void stfu() {
			bar();
		}
		public void bar() {
			System.out.printf("lulz\n");
		}
	}

	public int zomg = 1337;

	public MyHashtable() { }

	private class EntryIterator implements MyIterator<Entry<K, V>> {
		int lulz = zomg;
		EntryIterator () { }
		public boolean hasNext() { return lulz > 0; };
	}

	public MySet<MyMap.Entry<K, V>> entrySet() {
		MySet<MyMap.Entry<K, V>> r = new MyAbstractSet<MyMap.Entry<K, V>>() {
			public MyIterator<MyMap.Entry<K, V>> iterator() {
				return new EntryIterator();
			}
		};
		return r;
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

	public void putAll(MyMap<? extends K, ? extends V> m) {
		final MyMap<K, V> addMap = (MyMap<K, V>) m;
		final MySet<MyMap.Entry<K, V>> eset = addMap.entrySet();
		final MyIterator<MyMap.Entry<K, V>> it = eset.iterator();

		checkInstance(eset instanceof MySet, "eset", "MySet");
		checkInstance(it   instanceof MySet, "it",   "MySet");
		checkInstance(this instanceof MySet, "this", "MySet");
		checkInstance(eset instanceof MyIterator, "eset", "MyIterator");
		checkInstance(it   instanceof MyIterator, "it",   "MyIterator");
		checkInstance(this instanceof MyIterator, "this", "MyIterator");
		checkInstance(eset instanceof MyMap, "eset", "MyMap");
		checkInstance(it   instanceof MyMap, "it",   "MyMap");
		checkInstance(this instanceof MyMap, "this", "MyMap");
		checkInstance(eset instanceof MyProperties, "eset", "MyProperties");
		checkInstance(it   instanceof MyProperties, "it",   "MyProperties");
		checkInstance(this instanceof MyProperties, "this", "MyProperties");

		if (it.hasNext()) {
			System.out.printf("yes hasnext\n");
		} else {
			System.out.printf("no hasnext\n");
		}
	}
}

abstract class MyAbstractSet<E> implements MySet<E> {
	protected MyAbstractSet() { }
}

interface MyMap<K, V> {
	MySet<MyMap.Entry<K, V>> entrySet();
	void putAll(MyMap<? extends K, ? extends V> m);

	interface Entry<K, V> {
		K getKey();
		V getValue();
		V setValue(V value);
	}
}

interface MySet<E> extends MyCollection<E> {
	MyIterator<E> iterator();
}

interface MyCollection<E> extends MyIterable<E> {
	MyIterator<E> iterator();
}

interface MyIterable<E> {
	MyIterator<E> iterator();
}

interface MyIterator<E> {
	boolean hasNext();
}
