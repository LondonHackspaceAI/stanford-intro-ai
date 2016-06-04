package ch.christianjaeger.londonhackspaceai;

public class StringToInteger implements Function<String, Integer> {

	public StringToInteger() {}
	
	public Integer app(String v) {
		return Integer.parseInt(v);
	}

}
