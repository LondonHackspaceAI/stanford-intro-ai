package ch.christianjaeger.londonhackspaceai;

public abstract class Maybe<T> {

	public abstract T xvalue() throws NoValueException;

}
