package ch.christianjaeger.londonhackspaceai;

public class Just<T> extends Maybe<T> {
	private T value;
	Just(T v) {
		value=v;
	}
	public T value() {
		return value;
	}
	// to avoid having to cast all the time?: (XX same issue in List)
	public T xvalue() {
		return value;
	}
}
