package ch.christianjaeger.londonhackspaceai;

public class Queue <T> {
	private List<T> forward;
	private List<T> backward;
	
	Queue (List<T> forward, List<T> backward) {
		this.forward= forward;
		this.backward= backward;
	}
	
	public static <T> Queue<T> fromArray(T[] elements) {
		return new Queue<T> (List.fromArray(elements),
							 new Null<T> ());
	}
	
	public static <T> Queue<T> from(T... elements) {
		return fromArray(elements);
	}
	
	public Queue<T> enqueue(T v) {
		return new Queue<T> (forward.cons(v), backward);
	}
	
	public Values2<Queue<T>,Maybe<T>> maybeDequeue() {
		if (backward.is_null()) {
			if (forward.is_null()) {
				return new Values2<Queue<T>,Maybe<T>>(this, new Nothing<T>());
			} else {
				List<T> ba= forward.reverse();
				return new Values2<Queue<T>,Maybe<T>>
					(new Queue<T>(new Null<T>(), ((Pair<T>)ba).rest()),
					 new Just<T>(((Pair<T>)ba).first()));
			}
		} else {
			return new Values2<Queue<T>,Maybe<T>>
				(new Queue<T>(forward, ((Pair<T>)backward).rest()),
			     new Just<T>(((Pair<T>)backward).first()));
		}
	}
	
	public List<T> list() {
		return forward.append(backward.reverse());
	}
	
	public List<T> rlist() {
		return backward.append(forward.reverse());
	}
	
	public boolean is_empty() {
		return forward.is_null() && backward.is_null();
	}
}
