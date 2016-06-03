package ch.christianjaeger.londonhackspaceai;

public abstract class List<T> {
	public static <T> List<T> fromArray(T[] a) {
		List<T> r= new Null<T> ();
		for (int i= a.length-1; i>=0 ; i--) {
			r= new Pair<T>(a[i], r);
		}
		return r;
	}

	public static <T> List<T> from(T... a) {
		return fromArray(a);
	}
		
	public int length() {
		int len=0;
		List<T> l= this;
		while (! l.is_null()) {
			len++;
			l= ((Pair<T>)l).rest();
		}
		return len;
	}

	public abstract boolean is_null();
	
	public List<T> cons(T v) {
		return new Pair<T> (v, this);
	}
	
	public abstract <T2> List<T2> map(Function<T,T2> fn);

	public abstract List<T> filter(Function<T,Boolean> pred);

	public List<T> reverse() {
		List<T> out= new Null<T>();
		List<T> in= this;
		while (!(in.is_null())) {
			Pair<T> in_= (Pair<T>) in;
			out= new Pair<T> (in_.first(), out);
			in= in_.rest();
		}
		return out;
	}
	
	public abstract List<T> append(List<T> l);
	
	public abstract void forEach(Action<T> proc);

}
