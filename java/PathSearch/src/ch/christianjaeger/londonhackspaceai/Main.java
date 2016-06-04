package ch.christianjaeger.londonhackspaceai;

public class Main {
	public static void main(String[] args){
		List<String> a= List.fromArray(args);
		List<Integer> b= a.map(new StringToInteger());
		List<Integer> c= b.filter(new IntegerBelow(10));
		c.forEach(new Println<Integer>());
		if (c.is_null()) {
			System.out.print("empty result");
		} else {
			System.out.print("first = ");
			System.out.println(((Pair<Integer>)c).first());
		}
	}
}
