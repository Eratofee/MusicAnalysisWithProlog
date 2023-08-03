

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Chords                        ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(music_xml, chord(1)) :-
   Chord_1= 'grosse Terz',
   Chord_2= 'kleine Terz',
   Intervals= [Chord_1, Chord_2],
   mx_chords_intervals_to_chord_name(Intervals, Chordname),
   write(Chordname).

test(music_xml, chord(2)) :-
   Chord = chord:[]:[
      note:[]:[
         pitch:[]:[
            step:[]:['C'] ,
            octave:[]:['4'] , alter:[]:['-1'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['E'],
            octave:[]:['4'], alter:[]:['-1'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['G'],
            octave:[]:['4'], alter:[]:['-1'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['B'] ,
            octave:[]:['4'],alter:[]:['-1'] ],
         duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_to_name(Chord, Name),
   write(Name).

test(music_xml, chord(3)) :-
   Chord_1 = 'Quarte',
   Chord_2 = 'kleine Terz',
   Chord_3 = 'grosse Sekunde',
   Chord_4 = 'kleine Sekunde',
   Chord_5 = 'grosse Sekunde',
   Intervals = [Chord_1, Chord_2, Chord_3, Chord_4, Chord_5],
   mx_chords_intervals_to_chord_name(Intervals, Chordname),
   write(Chordname).

test(music_xml, chord(4)) :-
   Chord_1 = chord:[]:[
      note:[]:[
         pitch:[]:[
            step:[]:['C'] ,
            octave:[]:['4'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['E'],
            octave:[]:['4'], alter:[]:['-1'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['G'],
            octave:[]:['4'], alter:[]:['0'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['B'] ,
            octave:[]:['4'] ],
         duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_to_switched_chord(Chord_1,Chord_2),
   write(Chord_2).

test(music_xml, chord(5)) :-
   Chord = chord:[]:[
      note:[]:[
         pitch:[]:[
	    step:[]:['C'] ,
            octave:[]:['4'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['E'],
            octave:[]:['4'], alter:[]:['-1'] ],
	  duration:[]:['4'],
          type:[]:[whole] ],
       note:[]:[
	  pitch:[]:[
	     step:[]:['G'],
             octave:[]:['4'], alter:[]:['0'] ],
	  duration:[]:['4'],
          type:[]:[whole] ],
       note:[]:[
	  pitch:[]:[
	     step:[]:['B'] ,
	     octave:[]:['4'] ],
	  duration:[]:['4'],
          type:[]:[whole] ] ],
   mx_chords_chord_to_root(Chord,Root_Step),
   write(Root_Step).

test(music_xml, chord(name_inversion)) :-
   Chord = chord:[]:[
      note:[]:[
	 pitch:[]:[
	    step:[]:['C'] ,
            octave:[]:['4'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['E'],
            octave:[]:['4'], alter:[]:['-1'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['G'],
	    octave:[]:['4'], alter:[]:['0'] ],
	 duration:[]:['4'],
	 type:[]:[whole] ] ],
   mx_chords_chord_to_inversion(Chord,A),
   write(A).

test(music_xml, chord(sus_chord)) :-
   Chord = chord:[]:[
      note:[]:[
         pitch:[]:[
	    step:[]:['C'] ,
	    octave:[]:['4'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['D'],
            octave:[]:['4'], alter:[]:['0'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['G'],
	    octave:[]:['4'], alter:[]:['0'] ],
	 duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_to_sus(Chord,Name),
   write(Name).

test(music_xml, chord(contracted)) :-
   Chord = chord:[]:[
      note:[]:[
         pitch:[]:[
	    step:[]:['C'] ,
            octave:[]:['3'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['D'],
            octave:[]:['5'], alter:[]:['0'] ],
	 duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_to_contracted_chord(Chord, Contracted),
   write(Contracted).

test(music_xml, chord(switch)) :-
   Chord_1 = chord:[]:[
      note:[]:[
	 pitch:[]:[
	    step:[]:['C'] ,
	    octave:[]:['4'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['E'],
            octave:[]:['4'], alter:[]:['-1'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
	    step:[]:['G'],
            octave:[]:['4'], alter:[]:['0'] ],
         duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_to_switched_chord(Chord_1,Chord_2),
   write(Chord_2).

test(music_xml, chord(stacked_thirds)) :-
   Chord_1 = chord:[]:[
      note:[]:[
         pitch:[]:[
	    step:[]:['C'] ,
	    octave:[]:['5'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['E'],
            octave:[]:['4'], alter:[]:['-1'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
	    step:[]:['G'],
            octave:[]:['4'], alter:[]:['0'] ],
	 duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_in_stacked_thirds(Chord_1,Chord_2,X),
   writeln(X),
   writeln(Chord_2).

test(music_xml, chord(circle_progression)) :-
   Chord_A = chord:[]:[
      note:[]:[
         pitch:[]:[
	    step:[]:['C'] ,
            octave:[]:['5'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['E'],
	    octave:[]:['5'], alter:[]:['-1'] ],
	 duration:[]:['4'],
	 type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['G'],
            octave:[]:['5'], alter:[]:['0'] ],
	 duration:[]:['4'],
	 type:[]:[whole] ] ],
   Chord_B = chord:[]:[
      note:[]:[
         pitch:[]:[
	    step:[]:['F'] ,
	    octave:[]:['4'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
	 pitch:[]:[
	    step:[]:['A'],
	    octave:[]:['4'], alter:[]:['-1'] ],
	 duration:[]:['4'],
         type:[]:[whole] ],
             note:[]:[pitch:[]:[step:[]:['C'],
                   octave:[]:['5'], alter:[]:['0'] ],
                duration:[]:['4'],
                type:[]:[whole] ] ],
   mx_chords_circle_progression(Chord_A, Chord_B).

test(music_xml, chord(is_tonic)) :-
   Chord = chord:[]:[
      note:[]:[
         pitch:[]:[
             step:[]:['C'] ,
             octave:[]:['4'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['E'],
            octave:[]:['4'], alter:[]:['0'] ],
         duration:[]:['4'],
         type:[]:[whole] ],
      note:[]:[
         pitch:[]:[
            step:[]:['G'],
            octave:[]:['4'], alter:[]:['0'] ],
         duration:[]:['4'],
         type:[]:[whole] ] ],
   mx_chords_chord_is_tonic('C', Chord).


/*** interface ****************************************************/


/* mx_chord_to_name(Chord, Name) <-
      */

mx_chords_chord_to_name(Chord, Name_3) :-
   mx_chords_chord_to_chord_name(Chord, Name),
   [Name_2] = Name,
   mx_chords_chord_to_root(Chord, Root_Step),
   atomic_concat(Root_Step, '-', Root_Step_),
   atomic_concat(Root_Step_, Name_2, Name_3).


/* mx_chords_intervals_to_chord_name(Intervals, Chordname) <-
      */

mx_chords_chord_to_chord_name(Chord, Chordname) :-
   mx_chords_chord_to_intervals(Chord, Intervals),
   mx_chords_intervals_to_chord_name(Intervals,Chordname) .

mx_chords_intervals_to_chord_name(Intervals,Chordname) :-
     ( mx_chords_intervals_are_known_chord(Intervals,Chordname)
     ; Chordname=Intervals ) .

mx_chords_intervals_to_triad(Intervals, Triad) :-
   Intervals = [X, Y],
   Triples = [
      'kleine Terz' + 'kleine Terz' = 'Vermindert',
      'kleine Terz' + 'grosse Terz' = 'Moll',
      'grosse Terz' + 'kleine Terz' = 'Dur',
      'grosse Terz' + 'grosse Terz' = 'Uebermaessig' ],
   member(X + Y = Triad, Triples).

mx_chords_intervals_to_tetrad(Intervals, Tetrad) :-
   mx_chords_all_intervals_are_terz(Intervals),
   Intervals = [X, Y, Z],
   mx_chords_intervals_to_triad([X, Y], Triad),
   Triples = [
      'Dur' + 'kleine Terz' = 'Dominantseptakkord',
      'Dur' + 'grosse Terz' = 'Großer Septakkord',
      'Moll' + 'kleine Terz' = 'Mollseptakkord',
      'Moll' + 'grosse Terz' = 'Mollseptakkord mit großer Septime',
      'Vermindert' + 'kleine Terz' = 'Verminderter Septakkord',
      'Vermindert' + 'grosse Terz' = 'Halbverminderter Septakkord',
      'Uebermaessig' + 'kleine Terz' =
         'Uebermaessiger Septakkord' ],
   ( member(Triad + Z = Chord, Triples) ->
     Tetrad = [Chord]
   ; flatten([Triad, Z], Tetrad) ).

mx_chords_chord_is_unknown_chord(Chord) :-
   mx_chords_chord_to_intervals(Chord,Intervals),
   mx_chords_intervals_are_unknown_chord(Intervals).

mx_chords_intervals_are_unknown_chord(Intervals) :-
   \+ mx_chords_intervals_are_known_chord(Intervals,_).

mx_chords_chord_is_known_chord(Chord,Name) :-
   mx_chords_chord_to_intervals(Chord, Intervals),
   mx_chords_intervals_are_known_chord(Intervals,Name).

mx_chords_intervals_are_known_chord(Intervals,Name) :-
   ( mx_chords_intervals_to_sus(Intervals,Name)
   ; mx_chords_intervals_to_triad(Intervals,Name)
   ; mx_chords_intervals_to_tetrad(Intervals,Name) ).

mx_chords_chord_to_sus(Chord,Name) :-
   mx_chords_chord_to_contracted_chord(Chord, Contracted),
   mx_chords_chord_to_intervals(Contracted, Intervals),
   mx_chords_intervals_to_sus(Intervals,Name).

mx_chords_intervals_to_sus(Intervals,Name) :-
   ( Intervals = ['grosse Sekunde','Quarte'],
     Name = 'Sus2'
   ; Intervals=['Quarte','grosse Sekunde'],
     Name = 'Sus4' ).

mx_chords_chord_to_contracted_chord(Chord, Contracted) :-
   Contracted := Chord*[/note/pitch/octave:['4']].

mx_chords_all_intervals_are_terz(Intervals) :-
   foreach(Interval, Intervals) do
      name_contains_name(Interval, 'Terz').

mx_chords_chord_in_stacked_thirds(Chord,Root_position,0) :-
   mx_chord_chord_sort(Chord,Root_position),
   mx_chords_chord_to_intervals(Root_position,Intervals),
   mx_chords_all_intervals_are_terz(Intervals),
   !.

mx_chords_chord_in_stacked_thirds(Chord,Root_position,1) :-
   mx_chord_chord_sort(Chord,Chord_2),
   mx_chords_chord_to_switched_chord(Chord_2,Sec_inversion),
   mx_chords_chord_to_switched_chord(Sec_inversion,Root_position),
   mx_chords_chord_to_intervals(Root_position,Intervals),
   mx_chords_all_intervals_are_terz(Intervals),
   !.

mx_chords_chord_in_stacked_thirds(Chord,Root_position,2) :-
   mx_chord_chord_sort(Chord,Chord_2),
   mx_chords_chord_to_switched_chord(Chord_2, Root_position),
   mx_chords_chord_to_intervals(Root_position, Intervals_ts),
   mx_chords_all_intervals_are_terz(Intervals_ts),
   !.

mx_chords_chord_to_switched_chord(Chord_1,Chord_2) :-
   fn_item_parse(Chord_1, T:As:[Note_1|Notes]),
   [O1] := Note_1/pitch/octave/content::'*',
   add_to_atom(O1, '1', O2),
   Note_2 := Note_1*[/pitch/octave:[O2]],
   Chord_unsorted = T:As:[Note_2|Notes],
   mx_chord_chord_sort(Chord_unsorted, Chord_2),
   !.

mx_chord_chord_sort(Chord_1,Chord_2) :-
   findall( Note,
      Note := Chord_1/note,
      Notes_1 ),
   mx_chord_notes_sort(Notes_1,Notes_2),
   Chord_2 = chord:[]:Notes_2.

mx_chord_notes_sort(Notes_1,Notes_2) :-
   Comparator = mx_basics_note_smaller,
   mx_basics_mergesort_generic(Comparator, Notes_1, Notes_2).

mx_chords_chord_to_inversion(Chord,X) :-
   mx_chords_chord_in_stacked_thirds(Chord,_,X).

mx_chords_chord_to_intervals(Chord, Intervals) :-
   mx_chords_chord_to_tones(Chord,Tones),
   mx_chords_tones_to_intervals(Tones, Intervals).

mx_chords_chord_to_tones(Chord,Tones) :-
   findall( Note,
      Note := Chord/note,
      Notes ),
   maplist( mx_intervals_note_to_tone,
      Notes, Tones ).

mx_chords_tones_to_intervals(Tones, Intervals) :-
   findall( Interval,
      ( append(_, [Tone_1, Tone_2|_], Tones),
        mx_intervals_tones_to_interval_name(
           Tone_1, Tone_2, Interval) ),
      Intervals ).

mx_chords_chord_to_root(Chord, Root_Step) :-
   [Step] := Chord/nth_child::1/descendant::step/content::'*',
   ( [Alter] := Chord/nth_child::1/descendant::alter/content::'*',
      ( Alter = '-1' ->
        atomic_concat(Step, b, Root_Step)
      ; Alter = '1' ->
        atomic_concat(Step, '#', Root_Step)
      ; Alter = '0' ->
        Root_Step = Step )
   ; Root_Step = Step ).


/* mx_chords_cadence(Key, Chord_A, Chord_B) <-
      */

mx_chords_cadence(Key, Chord_A, Chord_B) :-
   mx_chords_circle_progression(Chord_A, Chord_B),
   mx_chords_chord_is_tonic(Key, Chord_B).

mx_chords_circle_progression(Chord_A, Chord_B) :-
   mx_chord_chord_sort(Chord_A, Chord_A2),
   mx_chord_chord_sort(Chord_B, Chord_B2),
   Root_A := Chord_A2/nth_child::1,
   Root_B := Chord_B2/nth_child::1,
   mx_intervals_notes_to_interval_name(Root_A, Root_B, Interval),
   name_contains_name(Interval, 'Quinte').

mx_chords_chord_is_tonic(Key, Chord) :-
   write(Chord),
   mx_chords_chord_to_chord_name(Chord, Chordname),
   name_cut_at_position(["-"], Chordname, Root),
   writeln(Root),
   ( name_contains_name(Chordname, 'Moll'),
     character_capital_to_lower(Root, R),
     Key = R,
     !
   ; name_contains_name(Chordname, 'Dur'),
     Key = Root ).


/******************************************************************/


