package ch.christianjaeger.londonhackspaceai;

public class StringLength implements Function<String, Integer> {

	public Integer app(String v) {
		return v.length();
	}
	
}
