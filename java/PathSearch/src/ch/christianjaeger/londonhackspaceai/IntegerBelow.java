package ch.christianjaeger.londonhackspaceai;

public class IntegerBelow implements Function<Integer, Boolean> {
	private Integer a;
	
	IntegerBelow(Integer a) {
		this.a= a;
	}

	@Override
	public Boolean app(Integer b) {
		return b < a;
	}

}
