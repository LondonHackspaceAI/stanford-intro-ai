package ch.christianjaeger.londonhackspaceai;

public class Tuple2 <T1,T2> {
	private T1 fst;
	private T2 snd;
	
	Tuple2(T1 fst, T2 snd) {
		this.fst=fst;
		this.snd=snd;
	}
	
	public final T1 fst() { return fst; }
	public final T2 snd() { return snd; }
}
