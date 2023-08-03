

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Intervals                     ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(music_xml, distance) :-
   Tone_1= ['D', '0', '1'],
   Tone_2= ['B', '2', '3'],
   mx_intervals_distance(Tone_1, Tone_2, Distance),
   write(Distance).

test(music_xml, interval) :-
   Tone_1= ['C', '0', '5'],
   Tone_2= ['A', '1', '6'],
   mx_intervals_tones_to_interval_name(Tone_1, Tone_2, Interval),
   write(Interval).

test(music_xml, interval) :-
   Note_1 = note:[]:[
      pitch:[]:[step:[]:['C'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_2 = note:[]:[
      pitch:[]:[step:[]:['E'],
	 octave:[]:['5'], alter:[]:['1']],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_intervals_notes_to_interval_name(Note_1, Note_2, Interval),
   write(Interval).

test(music_xml, akkordinterval) :-
   dislog_variable_get(example_path,
      'music_xml/note_1.xml', File_1),
   dislog_variable_get(example_path,
      'music_xml/note_2.xml', File_2),
   dread(xml, File_1, [Note_1]),
   dread(xml, File_2, [Note_2]),
   mx_intervals_note_to_tone(Note_1, Tone_1),
   mx_intervals_note_to_tone(Note_2, Tone_2),
   write(Tone_1),
   write(Tone_2),
   mx_intervals_tones_to_interval_name(Tone_1, Tone_2, Interval),
   write(Interval).


/*** interface ****************************************************/


/* mx_intervals_distance(None_1, None_2, Distance) <-
      returns the distance between two Notes in seminotes,
      returns a negative value if Tone_2 is lower then Tone_1 */

mx_intervals_distance(Tone_1, Tone_2, Distance) :-
   member([Step_1, Alter_1, Octave_1],[Tone_1]),
   member([Step_2, Alter_2, Octave_2],[Tone_2]),
   mx_intervals_step_distance(Step_1, Step_2, SD),
   mx_intervals_octave_distance(Octave_1, Octave_2, OD),
   mx_intervals_alter_distance(Alter_1, Alter_2, AD),
   Distance is SD+OD+AD.

mx_intervals_distance(Note_1, Note_2, Distance) :-
   mx_intervals_note_to_tone(Note_1, Tone_1),
   mx_intervals_note_to_tone(Note_2, Tone_2),
   mx_intervals_distance(Tone_1, Tone_2, Distance).

mx_intervals_step_distance(S1, S2, D) :-
  Assignment = [
     'C', '|', 'D', '|', 'E', 'F', '|', 'G', '|', 'A', '|', 'B' ],
  nth(M1, Assignment, S1),
  nth(M2, Assignment, S2),
  D is M2 - M1.

mx_intervals_octave_distance(Octave_1, Octave_2, Distance) :-
   atom_number(Octave_1, O_1),
   atom_number(Octave_2, O_2),
   X is -(O_2, O_1),
   Distance is X * 12.

mx_intervals_alter_distance(Alter_1, Alter_2, Distance) :-
   atom_number(Alter_1, A_1),
   atom_number(Alter_2, A_2),
   Distance is -(A_2, A_1).


/* mx_intervals_tones_to_interval_name(Tone_1, Tone_2, Name) <-
      returns a german intervalname as Name for the two Tones Tone_1
      and Tone_2 (even if Tone_1 is the upper one). */

mx_intervals_notes_to_interval_name(Note_1, Note_2, Name) :-
   mx_intervals_note_to_tone(Note_1, Tone_1),
   mx_intervals_note_to_tone(Note_2, Tone_2),
   mx_intervals_tones_to_interval_name(Tone_1, Tone_2, Name).

mx_intervals_tones_to_interval_name(Tone_1, Tone_2, Name) :-
   mx_intervals_distance(Tone_1, Tone_2, Distance),
   ( Distance < 0,
     mx_intervals_tones_to_interval_name(Tone_2, Tone_1, Name) )
   ; ( member([Step_1, Alter_1, Octave_1], [Tone_1]),
       member([Step_2, Alter_2, Octave_2], [Tone_2]),
       mx_intervals_step_distance(Step_1, Step_2, SD),
       mx_intervals_octave_distance(Octave_1, Octave_2, OD),
       SOD is SD + OD,
       Assignment = [
          'Prime' = 0, 'kleine Sekunde' = 1,'grosse Sekunde' = 2,
          'kleine Terz' = 3, 'grosse Terz' = 4,
          'Quarte' = 5, 'uebermaessige Quarte' = 6, 'Quinte' = 7,
          'kleine Sexte' = 8, 'grosse Sexte' = 9,
          'kleine Septime' = 10, 'grosse Septime' = 11,
	  'Oktave' = 12, 'kleine None' = 13, 'grosse None' = 14,
          'kleine Dezime' = 15, 'grosse Dezime' = 16,
          'Undezime' = 17, 'uebermaessige Undezime' = 18,
          'Duodezime' = 19, 'kleine Tredezime' = 20,
	  'grosse Tredezime' = 21 ],
       member(Name_v = SOD, Assignment),
       mx_intervals_alter_distance(Alter_1, Alter_2, AD),
   ( AD = 0, Name = Name_v
   ; AD \= 0,
     mx_intervals_alter_interval_name(Name_v, AD, Name) ) ).

mx_intervals_alter_interval_name(Name_v, Alter_dist, Altered_N) :-
   ( ( Name_v = 'uebermaessige Quarte'
     ; Name_v = 'uebermaessige Undezime' ),
     Alter_dist = -1,
     name_exchange_sublist(
	[["uebermaessige ",""]], Name_v, Altered_N)
   ; mx_intervals_alter_interval_name2(
        Name_v, Alter_dist, Altered_N) ).

mx_intervals_alter_interval_name2(Name_v, -1, Altered_N) :-
   ( name_contains_name(Name_v, 'kleine'),
       name_exchange_sublist(
          [["kleine","verminderte"]], Name_v, Altered_N),
     !
   ; ( name_contains_name(Name_v, 'grosse'),
       name_exchange_sublist(
          [["grosse","kleine"]], Name_v, Altered_N),
       !
     ; atom_concat('verminderte ',Name_v,  Altered_N) ) ).

mx_intervals_alter_interval_name2(Name_v, -2, Altered_N) :-
   ( name_contains_name(Name_v, 'kleine'),
     name_exchange_sublist(
        [["kleine","doppelt verminderte"]], Name_v, Altered_N),
     !
   ; ( name_contains_name(Name_v, 'grosse'),
       name_exchange_sublist(
          [["grosse","verminderte"]], Name_v, Altered_N),
       !
     ; atom_concat('doppelt verminderte ',Name_v,  Altered_N)
       ) ).

mx_intervals_alter_interval_name2(Name_v, 1, Altered_N) :-
   ( name_contains_name(Name_v, 'grosse'),
     name_exchange_sublist(
        [["grosse","uebermaessige"]], Name_v, Altered_N),
     !
   ; ( name_contains_name(Name_v, 'kleine'),
       name_exchange_sublist(
          [["kleine","grosse"]], Name_v, Altered_N),
       !
     ; atom_concat('uebermaessige ', Name_v,  Altered_N) ) ).

mx_intervals_alter_interval_name2(Name_v, 1, Altered_N) :-
   ( name_contains_name(Name_v, 'grosse'),
     name_exchange_sublist(
	 [["grosse","doppelt uebermaessige"]], Name_v, Altered_N),
     !
   ; ( name_contains_name(Name_v, 'kleine'),
       name_exchange_sublist(
	  [["kleine","uebermaessige"]], Name_v, Altered_N),
       !
     ; atom_concat('doppelt uebermaessige ',Name_v,  Altered_N) ) ).


/* mx_intervals_note_to_tone(Note, Tone) <-
      extracts the pitch information of a Note and returns
      it as a list of the form [Step, Alter, Octave].
      */

mx_intervals_note_to_tone(Note, Tone) :-
   [Step] := Note/pitch/step/content::'*',
   [Octave] := Note/pitch/octave/content::'*',
   ( [Alter] := Note/pitch/alter/content::'*' ->
     true
   ; Alter = '0' ),
   Tone = [Step, Alter, Octave].
/******************************************************************/


