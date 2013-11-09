import java.util.ArrayList;

/**
 * 
 */

/**
 * @author wgf2104
 *
 */
public class Track {
	
	public ArrayList<Chord> track;
	
	/**
	 * 
	 */
	public Track() {
		track = new ArrayList<Chord>();
	}
	
	/**
	 * 
	 * @param track
	 */
	public Track(ArrayList<Chord> other) {
		track = other;
	}
	
	/**
	 * 
	 * @return
	 */
	public ArrayList<Chord> getTrack() {
		return track;
	}
	
	/**
	 * 
	 * @return
	 */
	public Chord getChord(int index) {
		return track.get(index);
	}
	
	/**
	 * 
	 * @return
	 */
	public int length() {
		return track.size();
	}
	
	/**
	 * 
	 * @param chord
	 */
	public void addChord(Chord chord) {
		track.add(chord);
	}
	
	/**
	 * 
	 * @param other
	 */
	public void serialAdd(Track other) {
		track.addAll(other.getTrack());
	}
	
	/**
	 * 
	 * @param track
	 */
	public void parallelAdd(Track other) {
		for(int i = 0; i < (this.length() < other.length() ? this.length() : other.length()); i++) {
			this.getChord(i).parallelAdd(other.getChord(i));
		}
	}
	
}
