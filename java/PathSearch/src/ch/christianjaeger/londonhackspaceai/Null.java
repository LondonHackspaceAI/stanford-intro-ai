package ch.christianjaeger.londonhackspaceai;

public class Null<T> extends List<T> {
	
	public Null() {
	}
	
	public Null<T> map(Function<T,T> fn) {
		return new Null<T>();
	}

	public List<T> filter(Function<T,Boolean> pred) {
		return new Null<T>();
	}

	public void forEach(Action<T> proc) {
		return;
	}
}
