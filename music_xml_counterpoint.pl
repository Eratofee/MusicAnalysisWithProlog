

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Counterpoint                  ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(music_xml, counterpoint(direction) ) :-
   Note_1 = note:[]:[
      pitch:[]:[
         step:[]:['C'] ,
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_2 = note:[]:[
      pitch:[]:[
	 step:[]:['F'],
         octave:[]:['3'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_direction(Note_1:Note_2,Direction),
   write(Direction).

test(music_xml, counterpoint(contrary_motion) ) :-
   A1 = note:[]:[
      pitch:[]:[
         step:[]:['C'] ,
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
	 step:[]:['F'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
	 octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
         step:[]:['F'],
         octave:[]:['3'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_contrary_motion(A1:A2, B1:B2).

test(music_xml, counterpoint(paralel_motion) ) :-
   A1 = note:[]:[
      pitch:[]:[
	 step:[]:['C'] ,
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
         step:[]:['D'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
         step:[]:['D'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_paralel_motion(A1:A2, B1:B2).

test(music_xml, counterpoint(similar_motion) ) :-
   A1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['6'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
	 step:[]:['G'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
	 step:[]:['B'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_similar_motion(A1:A2, B1:B2).

test(music_xml, counterpoint(oblique_motion) ) :-
   A1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
	 step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
         step:[]:['F'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_oblique_motion(A1:A2, B1:B2).

test(music_xml, counterpoint(consecutive_fifth) ) :-
   A1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
         step:[]:['D'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
         step:[]:['G'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
	 step:[]:['A'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_consecutive_fifth(A1:A2, B1:B2).

test(music_xml, counterpoint(consecutive_octave) ) :-
   A1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
         step:[]:['D'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
	 step:[]:['C'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
         step:[]:['D'],
	 octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_consecutive_octave(A1:A2, B1:B2).

test(music_xml, counterpoint(hidden_consecutive) ) :-
   A1 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   A2 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B1 = note:[]:[
      pitch:[]:[
         step:[]:['B'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   B2 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['3'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_counterpoint_hidden_consecutive(A1:A2, B1:B2).


/*** interface ****************************************************/


mx_counterpoint_direction(Note_1:Note_2, Direction) :-
   mx_intervals_distance(Note_1, Note_2, Distance),
   Direction is sign(Distance).

mx_counterpoint_contrary_motion(A1:A2, B1:B2) :-
   mx_counterpoint_direction(A1:A2, I),
   mx_counterpoint_direction(B1:B2, J),
   member([I, J], [[1, -1], [-1, 1]]).

mx_counterpoint_paralel_motion(A1:A2, B1:B2) :-
   mx_intervals_notes_to_interval_name(A1, A2, X),
   mx_intervals_notes_to_interval_name(B1, B2, X).

mx_counterpoint_similar_motion(A1:A2,B1:B2) :-
   mx_counterpoint_direction(A1:A2, I),
   mx_counterpoint_direction(B1:B2, J),
   member([I, J], [[1, 1], [-1, -1]]).

mx_counterpoint_oblique_motion(A1:A2,B1:B2) :-
   ( mx_counterpoint_direction(A1:A2, 0)
   ; mx_counterpoint_direction(B1:B2, 0) ).

mx_counterpoint_consecutive_fifth(A1:A2,B1:B2) :-
   mx_counterpoint_paralel_motion(A1:A2, B1:B2),
   mx_intervals_notes_to_interval_name(A1, B1, I),
   member(I, ['Quinte', 'Undezime']).

mx_counterpoint_consecutive_octave(A1:A2, B1:B2) :-
   mx_counterpoint_paralel_motion(A1:A2, B1:B2),
   mx_intervals_notes_to_interval_name(A1, B1, 'Oktave').

mx_counterpoint_hidden_consecutive(A1:A2, B1:B2) :-
   ( mx_counterpoint_hidden_cons_fifth(A1:A2, B1:B2)
   ; mx_counterpoint_hidden_cons_octave(A1:A2, B1:B2) ).

mx_counterpoint_hidden_cons_fifth(A1:A2, B1:B2) :-
   mx_counterpoint_similar_motion(A1:A2, B1:B2),
   mx_intervals_notes_to_interval_name(A1, B1, I),
   member(I, ['Quinte', 'Undezime']).

mx_counterpoint_hidden_cons_octave(A1:A2, B1:B2) :-
   mx_counterpoint_similar_motion(A1:A2, B1:B2),
   mx_intervals_notes_to_interval_name(A2, B2, 'Oktave').


/******************************************************************/


