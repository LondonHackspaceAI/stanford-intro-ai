package ch.christianjaeger.londonhackspaceai;

public class Nothing<T> extends Maybe<T> {
	Nothing() {}

	// to avoid having to cast all the time?: (XX same issue in List)
	public T xvalue() throws NoValueException {
		throw new NoValueException();
	}
}
