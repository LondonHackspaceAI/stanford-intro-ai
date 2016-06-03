package ch.christianjaeger.londonhackspaceai;

import static org.junit.Assert.*;

import org.junit.Test;

public class QueueTest {

	@Test
	public void test() throws NoValueException {
		assertEquals(List.from(1,2), List.from(2).cons(1)); //XX move to ListTest
		
		Queue<Integer> q= Queue.from(1,2,3);
		assertEquals(q.is_empty(), false);
		assertEquals(q.list(), List.from(1,2,3));
		assertEquals(q.rlist(), List.from(3,2,1));

		Queue<Integer> q0= q.enqueue(0);
		assertEquals(q.list(), List.from(0,1,2,3));
		assertEquals(q.rlist(), List.from(3, 2, 1, 0));
		
		Values2<Queue<Integer>,Maybe<Integer>> r1= q0.maybeDequeue();
		assertEquals(r1.snd().xvalue(), new Integer (3));
	}

}
