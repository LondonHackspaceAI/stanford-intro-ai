package ch.christianjaeger.londonhackspaceai;

public class Null<T> extends List<T> {
	
	public Null() {
	}

	public boolean is_null() { return true; }
	
	public <T2> Null<T2> map(Function<T,T2> fn) {
		return new Null<T2>();
	}

	public List<T> filter(Function<T,Boolean> pred) {
		return new Null<T>();
	}

	public List<T> append(List<T> l) {
		return new Null<T>();
	}

	public void forEach(Action<T> proc) {
		return;
	}
}
