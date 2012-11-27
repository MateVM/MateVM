package tests;

public class Array4<K, V> {

	public Entry<K, V>[] buckets;
	public int count = -1;

	public Array4() {
		buckets = (Entry<K, V>[]) new Entry[10];
	}

	public void put(K key, V value) {
		count++;
		buckets[count] = new Entry<K, V>(key, value);
	}

	public Entry<K, V> get(K key) {
		Entry<K, V> t = buckets[count];
		count--;
		return t;
	}

	public static void main(String []args) {
		Entry<Integer, Integer> i;
		Array4<Integer, Integer> b = new Array4<Integer, Integer>();
		b.put(4, 10);
		i = b.get(4);
		if (i == null) {
			System.out.printf("i is null :(\n");
		}
		printentry(i);
		System.out.printf("done\n");
	}

	public static void printentry(Entry<Integer, Integer> e) {
		System.out.printf("key: %d\n", e.key);
		System.out.printf("val: %d\n", e.value);
	}

	private static class Entry<K, V> {
		public K key;
		public V value;

		public Entry(K k, V v) {
			this.key = k;
			this.value = v;
		}
	}
}
