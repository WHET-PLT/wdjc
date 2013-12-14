import java.util.ArrayList;
import jm.JMC;
import jm.music.data.*;
import jm.music.tools.*;
import jm.util.*;

/**
* This class turns a series of integers into notes. 
* @author Hila Gutfreund
*/

public class CreateNotesFromFreq implements JMC {
  
  public static void main(String[] args) {
  ArrayList<Integer> notes = new ArrayList<Integer>();
   notes.add(440); 

   new CreateNotesFromFreq(notes);
    }
    
  public CreateNotesFromFreq(ArrayList<Integer> notes){
    Phrase notePhrase = new Phrase();
    Part p = new Part();
    for(Integer note:notes){
      //System.out.println(note); 
        Note n = new Note ((note*1.0), 0.5); 
        notePhrase.addNote(n); 
    }
    p.addPhrase(notePhrase); 
    
    Write.midi(p, "midi/createNotesFreq.mid"); 
  }
}