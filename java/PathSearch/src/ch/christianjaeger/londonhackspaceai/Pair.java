package ch.christianjaeger.londonhackspaceai;

public class Pair<T> extends List<T> {
	private T first;
	private List<T> rest;
	
	public Pair(T f, List<T> r) {
		first= f;
		rest= r;
	}

	public boolean is_null() { return false; }

	@SuppressWarnings("unchecked")
	public boolean equals(Object v) {
		return (! ((List<T>)v).is_null()) 
				&&
				first.equals(((Pair<T>)v).first())
				&&
				rest.equals(((Pair<T>)v).rest());
	}

	public T first() {
		return first;
	}

	public Pair<T> first_set(T first) {
		return new Pair<T> (first, rest);
	}

	public List<T> rest() {
		return rest;
	}

	public Pair<T> rest_set(List<T> rest) {
		return new Pair<T>(first, rest);
	}
	
	public <T2> Pair<T2> map(Function<T,T2> fn) {
		return new Pair<T2>(fn.app(first), rest.map(fn));
	}
	
	public List<T> filter(Function<T,Boolean> pred) {
		List<T> rest1= rest.filter(pred);
		return pred.app(first) ? new Pair<T>(first, rest1) 
			: rest1;
	}

	public List<T> append(List<T> l) {
		return new Pair<T>(first, rest.append(l));
	}

	public List<T> join(T v) {
		return rest.is_null() ? this : new Pair<T> (first, new Pair<T>(first, new Pair<T> (v, rest.join(v))));
	}

	public <T2> T2 fold(Function2<T,T2,T2> fn, T2 init) {
		return rest.fold(fn, fn.app(first,init));
	}

	public void forEach(Action<T> proc) {
		proc.run(first);
		rest.forEach(proc);
	}

}
