package ch.christianjaeger.londonhackspaceai;

public class Null<T> extends List<T> {
	
	public Null() {
	}

	public boolean is_null() { return true; }
	
	@SuppressWarnings("unchecked")
	public boolean equals(Object v) {
		return ((List<T>)v).is_null();
	}

	public <T2> Null<T2> map(Function<T,T2> fn) {
		return new Null<T2>();
	}

	public List<T> filter(Function<T,Boolean> pred) {
		return new Null<T>();
	}

	public List<T> append(List<T> l) {
		return new Null<T>();
	}

	public List<T> join(T v) {
		return new Null<T>();
	}

	public <T2> T2 fold(Function2<T,T2,T2> fn, T2 init) {
		return init;
	}
	
	public void forEach(Action<T> proc) {
		return;
	}
}
