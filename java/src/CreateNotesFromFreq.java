import java.util.LinkedList;
import java.util.List;

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
   LinkedList<Integer> notes = new LinkedList<Integer>();
   //notes.add(120); 
   //notes.add(20); 
   notes.add(440); 

   new CreateNotesFromFreq(notes);
    }
    
  public CreateNotesFromFreq(LinkedList<Integer> notes){
    Phrase notePhrase = new Phrase();
    Part p = new Part("Piano", 0, 0);
    for(Integer note:notes){
      //System.out.println(note); 
        Note n = new Note (440.0, 0.5); 
        notePhrase.addNote(n); 
    }
    p.addPhrase(notePhrase); 
    
    Write.midi(p, "midi/createNotes.mid"); 
  }
}