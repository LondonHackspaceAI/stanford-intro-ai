package ch.christianjaeger.londonhackspaceai;

public class Just<T> extends Maybe<T> {
	private T value;
	Just(T v) {
		value=v;
	}
	public T value() {
		return value;
	}
}
