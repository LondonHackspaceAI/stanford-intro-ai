package ch.christianjaeger.londonhackspaceai;

public interface Function<T1, T2> {
	public T2 app(T1 v);
}
