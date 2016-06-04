package ch.christianjaeger.londonhackspaceai;

public class Println<T> implements Action<T> {
	Println(){};

	public void run (T v) {
		System.out.println(v);
	}
}
