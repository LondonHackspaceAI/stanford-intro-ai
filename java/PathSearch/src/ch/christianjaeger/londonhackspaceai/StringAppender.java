package ch.christianjaeger.londonhackspaceai;

public class StringAppender implements Action<String> {
	StringBuilder builder;
	StringAppender(StringBuilder builder) {
		this.builder=builder;
	}
	
	public void run(String v) {
		builder.append(v);
	}
	
}
