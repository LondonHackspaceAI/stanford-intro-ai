package ch.christianjaeger.londonhackspaceai;

public abstract class List<T> {
	public static <T> List<T> fromArray(T[] a) {
		List<T> r= new Null<T> ();
		for (int i= a.length-1; i>=0 ; i--) {
			r= new Pair<T>(a[i], r);
		}
		return r;
	}
	
	public abstract List<T> map(Function<T,T> fn);

	public abstract List<T> filter(Function<T,Boolean> pred);
	
	public abstract void forEach(Action<T> proc);

}
