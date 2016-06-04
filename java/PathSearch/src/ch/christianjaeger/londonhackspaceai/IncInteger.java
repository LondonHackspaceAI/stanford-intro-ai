package ch.christianjaeger.londonhackspaceai;

public class IncInteger implements Function<Integer, Integer> {
	public IncInteger() {
	}
	public Integer app(Integer x) {
		return x+1;
	}
}
