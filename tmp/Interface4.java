package tmp;

public class Interface4 {
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
	static class Lulz implements i1, i2, i3, i4 {
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

	public static void main(String []args) {
		Lulz a = new Lulz();
		System.out.printf("woot\n");
		Interface4 foo = new Interface4();
		//@hs: play around with this loop var
		for (int i = 0; i < 0x1; i++)
			foo.foo();
	}

	public void foo() {
		MyHashtable<Integer, Integer> m = new MyHashtable();
		MyHashtable<Integer, Integer> n = new MyHashtable<Integer, Integer>();
		m.putAllInternal(n);
	}

	class MyHashtable<K, V> implements MyMap<K, V> {
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

		void putAllInternal(MyMap<? extends K, ? extends V> m) {
			final MyMap<K, V> addMap = (MyMap<K, V>) m;
			final MyIterator<MyMap.Entry<K, V>> it = addMap.entrySet().iterator();

			if (it instanceof MyIterator) {
				System.out.printf("is iterator\n");
			} else {
				System.out.printf("no iterator\n");
			}

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

		interface Entry<K, V> {
			K getKey();
			V getValue();
			V setValue(V value);
		}
	}

	interface MySet<E> {
		MyIterator<E> iterator();
	}

	interface MyIterator<E> {
		boolean hasNext();
	}
}
