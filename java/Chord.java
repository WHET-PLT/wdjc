import java.util.ArrayList;

/**
 * 
 */

/**
 * @author wgf2104
 *
 */
public class Chord {


	public ArrayList<Note> chord;

	/**
	 * 
	 */
	public Chord() {
		chord = new ArrayList<Note>();
	}

	/**
	 * 
	 * @param other
	 */
	public Chord(Chord other) {
		chord = new ArrayList<Note>();
		chord.addAll(other.getChord());
	}

	/**
	 * 
	 * @return
	 */
	public ArrayList<Note> getChord() {
		return chord;
	}

	/**
	 * 
	 * @return
	 */
	public Note getNote(int index) {
		return chord.get(index);
	}

	/**
	 * 
	 * @return
	 */
	public int length() {
		return chord.size();
	}

	/**
	 * 
	 * @param note
	 */
	public void addNote(Note note) {
		chord.add(note);
	}

	/**
	 * serial add should return track
	 * @param other
	 */
	public Track serialAdd(Chord other) {
		Track tmp = new Track(other);
		tmp.serialAdd(new Track(other));
		return tmp;
	}

	/**
	 * parallel add should return track
	 * @param track
	 */
	public Chord parallelAdd(Chord other) {
		Chord tmp = new Chord(this);
		tmp.getChord().addAll(other.getChord());
		return tmp;
	}
}
