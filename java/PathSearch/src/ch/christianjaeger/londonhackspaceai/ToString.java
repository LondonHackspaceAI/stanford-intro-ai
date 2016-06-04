package ch.christianjaeger.londonhackspaceai;

public class ToString<T> implements Function<T, String> {

	public String app(T v) {
		return v.toString();
	}

}
