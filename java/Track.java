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
	public Track(Chord other) {
		track = new ArrayList<Chord>();
		track.add(other);
	}
	
	/**
	 * 
	 * @param track
	 */
	public Track(Track other) {
		track = new ArrayList<Chord>();
		track.addAll(other.getTrack());
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
	public Track serialAdd(Track other) {
		Track tmp = new Track(this); 
		tmp.getTrack().addAll(other.getTrack());;
		return tmp;
	}

	/**
	 * 
	 * @param track
	 */
	public void parallelAdd(Track other) {
		Track tmp = new Track(this);
		for(int i = 0; i < (this.length() < other.length() ? this.length() : other.length()); i++) {
			tmp.getChord(i).parallelAdd(other.getChord(i));
		}
	}
	
}
