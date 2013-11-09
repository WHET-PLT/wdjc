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
	public Chord(ArrayList<Note> other) {
		chord = other;
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
		Track tmp = new Track(Chord);
		for(int i = 0; i < (this.length() < other.length() ? this.length() : other.length()); i++) {
			tmp.serialAdd(new Track(other));
		}
	}
	
	/**
	 * parallel add should return track
	 * @param track
	 */
	public Chord parallelAdd(Chord other) {
		Chord tmp = new Chord(chord);
		tmp.getChord().addAll(other.getChord());
		return tmp;
	}
}
