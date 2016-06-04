package ch.christianjaeger.londonhackspaceai;

public class IntegerAdd implements Function2<Integer, Integer, Integer> {
	IntegerAdd () {}
	public Integer app(Integer a, Integer b) {
		return a+b;
	}
}
