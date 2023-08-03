

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Time                          ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(music_xml, time(data)) :-
   mx_time_file_to_measures(
      'examples/music_xml/timewise_example.xml',
      'results/timewise_example.xml').

test(music_xml, time(attributes)) :-
   File='examples/music_xml/timewise_example.xml',
   dread(xml,File,[Xml_Timewise]),
   let( Measure_list := Xml_Timewise/descendant::measure),
   Measures= measure:Measure_list,
   mx_time_measure_attributes(Measures,5,Attributes),
   write(Attributes).

test(music_xml, time(attributes,small)) :-
   Note_1 = note:[]:[
      pitch:[]:[step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_2 = note:[]:[
      pitch:[]:[step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Attributes = attributes:[]:[
      divisions:[]:[4],
      key:[]:[fifth:[]:[3],
         mode:[]:[minor] ],
      time:[symbol='commons']:[
         beats:[]:[4],
	 'beat-type':[]:[4] ],
      clef:[]:[sign:[]:['G'],
         line:[]:[2] ] ],
   Measures = measures:[]:[
      measure:[]:[Attributes, Note_1, Note_2] ],
   mx_time_measure_attributes(Measures, 1, Attributes_back),
   write(Attributes_back).

test(music_xml, time(note_interval_check)) :-
   Note = note:[]:[
      pitch:[]:[step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_time_note_interval_check(1, Note, 1, 5),
   write(Note).

test(music_xml, time(beat)) :-
   File='examples/music_xml/timewise_example.xml',
   dread(xml, File, [Xml_Timewise]),
   let( Measure_list := Xml_Timewise/descendant::measure),
   Measures= measure:Measure_list, writeln('1'),
   mx_time_measure_and_beat_to_notes(Measures, 5, 5, Notes),
   writeln('4'),
   write(Notes).

test(music_xml, time(beat,small)) :-
   Note_1 = note:[]:[
      pitch:[]:[step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_2 = note:[]:[
      pitch:[]:[step:[]:['D'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Attributes = attributes:[]:[
      divisions:[]:[4],
      key:[]:[fifth:[]:[3],
         mode:[]:[minor] ],
      time:[symbol='commons']:[
         beats:[]:[4],
         'beat-type':[]:[4] ],
      clef:[]:[sign:[]:['G'],
      line:[]:[2] ] ],
   Measures = measures:[]:[
      measure:[]:[part:[]:[Attributes, Note_1, Note_2] ] ],
   mx_time_measure_and_beat_to_notes(
      Measures, 1, beat(1, 4), Notes),
   write(Notes).

test(music_xml, time(capacity)) :-
   mx_time_note_capacity(4, 2, Capacity),
   write(Capacity).

test(music_xml, insert) :-
   M = a:[b:1]:[c:[]:[4]],
   N := M <+> [haus:[6]],
   dwrite(xml, N).

test(music_xml, durations) :-
   Item = notes:[
      note:[]:[
         pitch:[]:[
            step:[]:['C'], octave:[]:['5'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['C'] ,octave:[]:['5'] ],
         duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_time_measure_to_start_list(Item, Sums),
   write(Sums).


/*** interface ****************************************************/


mx_time_file_to_measures(File_1, File_2) :-
   dread(xml, File_1, [Xml_Timewise]),
   let( Measures := Xml_Timewise/descendant::measure ),
   write(Measures),
   dwrite(xml, File_2, measures:Measures).

/* mx_time_measure_and_beat_to_notes(
      Measures, M_nr, beat(X, Y), Notes) <-
      returns all notes which sound to the beat(X,Y) in the measure
      number M_nr. A beat(1,4) whould mean the first quarter note.
      */

mx_time_measure_and_beat_to_notes(
   Measures, M_nr, beat(X, Y), Notes) :-
   mx_time_measure_attributes(Measures, M_nr, Attributes),
   M := Measures/nth_child::M_nr,
   Measure := M <+> [Attributes],
   mx_time_beat_to_notes(Measure,beat(X, Y),Notes).

mx_time_beat_to_notes(Measure, beat(X, Y), Vocal_Notess) :-
   [Divisions] := Measure/attributes/divisions/content::'*',
   mx_time_note_capacity(Y, Divisions, Capacity),
   mx_time_time_interval(X, Capacity, Lower, Upper),
   findall( Part,
      Part:= Measure/descendant::part,
      Parts ),
   findall( Vocal_Notes,
      (	foreach(Part, Parts) do
         mx_time_part_to_notes(
	    Part, Lower, Upper, Vocal_Notes) ),
       Vocal_Notess ).

mx_time_part_to_notes(
   Part, Lower, Upper, Vocal_Notes) :-
   mx_time_measure_to_start_list(Part, Starts),
   mx_time_part_to_notes(Part, Notes),
   findall( Vocal_Note,
      ( foreach(Vocal_Note, Notes),
	foreach(Start, Starts) do
           ( write(Lower),
	     write(Upper),
	     writeln(Start),
             mx_time_note_interval_check(
	        Start, Vocal_Note, Lower, Upper),
	    writeln(Vocal_Note) ) ),
      Vocal_Notes ),
   write(Vocal_Notes).

mx_time_part_to_notes(Part, Notes) :-
   findall( Note,
      Note := Part/descendant::note,
      Notes ).

mx_time_note_interval_check(N_Start, Note, Lower, Upper) :-
   [N_Duration] := Note/descendant::duration/content::'*',
   atom_number(N_Duration, Duration),
   N_Start =< Upper,
   N_Start + Duration >= Lower.

mx_time_note_capacity(Note_value, Divisions, Capacity) :-
   Capacity is Divisions * 4 / Note_value.

mx_time_time_interval(X, Capacity, Lower, Upper) :-
   Upper is X * Capacity + 1,
   Lower is Upper - Capacity.

mx_time_measure_attributes(Measures, Measure_nr, Attributes) :-
   Measure_nr >= 0,
   ( Measure :=  Measures/nth_child::Measure_nr,
     Attributes := Measure/descendant::attributes,
     !
   ; Precedent is Measure_nr - 1,
     mx_time_measure_attributes(
        Measures, Precedent, Attributes) ).

mx_time_measure_to_start_list(Item, Starts) :-
   findall( Note,
      Note := Item/descendant::note,
      Notes),
      write(Notes),
      findall( Sum,
         ( append(Xs, _, Notes),
           Item_2 = notes:Xs,
           findall( D,
              ( [D2] := Item_2/note/duration/content::'*',
                term_to_atom(D, D2) ),
              Ds ),
           sum(Ds, Sum) ),
         Sums_0 ),
   maplist(add(1),Sums_0,Sums_1),
   mx_time_del_last_elem(Sums_1,Starts).

mx_time_del_last_elem(List_1,List_2) :-
   reverse(List_1,[_|List]),
   reverse(List,List_2).


/******************************************************************/


