

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Basics                        ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(music_xml,basics(tones)) :-
   dislog_variable_get(example_path,
      'music_xml/Dichterliebe01.xml', File_1),
   dislog_variable_get(output_path,
      'octavesDLBasic.xml', File_2),
   mx_basics_file_to_tones(File_1, File_2).

test(music_xml,basics(chord)) :-
   dislog_variable_get(example_path,
      'music_xml/Dichterliebe01.xml', File_1),
   dislog_variable_get(output_path,
      'AkkDLBasic.xml', File_2),
   music_xml_notes_file_group(File_1, File_2).

test(music_xml,basics(query)) :-
   dislog_variable_get(example_path,
%     'music_xml/Dichterliebe01.xml', File_1),
      'music_xml/DebuMandSample.xml', File_1),
   mx_query_acchord(File_1, Accords),
   findall( Name,
      ( member(Acc, Accords),
        mx_chord_chord_to_name(Acc, Name) ),
      Names ),
   writeln('-----'(Names)).

test(music_xml, basics(key)) :-
   dread(xml,'examples/music_xml/Dichterliebe01.xml',Music),
   mx_basics_key(Music,Key),
   write(Key).

test(music_xml,basics(notes_to_min)) :-
   Note_1 = note:[]:[
      pitch:[]:[
         step:[]:['B'],
         octave:[]:['4'], alter:[]:['1'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_2 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['5'], alter:[]:['-1'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   mx_basics_notes_to_min(Note_1,Note_2,X),
   write(X).

test(music_xml,basics(sort)) :-
   Note_1 = note:[]:[
      pitch:[]:[
         step:[]:['D'],
         octave:[]:['5'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_2 = note:[]:[
      pitch:[]:[
         step:[]:['C'],
         octave:[]:['5'] , alter:[]:['0'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   Note_3 = note:[]:[
      pitch:[]:[
         step:[]:['B'],
         octave:[]:['4'] ],
      duration:[]:['4'],
      type:[]:[whole] ],
   List_1 = [Note_1,Note_2,Note_3],
   Comparator = mx_basics_note_smaller,
   mx_basics_mergesort_generic(Comparator, List_1, List_2),
   writeln(user, List_2).


/*** interface ****************************************************/


/* mx_basics_file_to_tones(File_1, File_2) <-
      */

mx_basics_file_to_tones(File_1, File_2) :-
   dread(xml, File_1, [Xml_1]),
   mx_basics_mx_to_tones_ip(Xml_1, Tones_ips),
   mx_basics_tones_ips_to_mx_notes_ips(Tones_ips, Notes_ips),
   mx_basics_mx_notes_to_mx(Notes_ips, Xml_1, Xml_2),
   dwrite(xml, File_2, Xml_2).

mx_basics_mx_to_tones_ip(Xml, Tones_ips) :-
   ddbase_aggregate( [P, Tones],
      ( Part := Xml/descendant::part::[@id=P],
        mx_basics_mx_to_tones(Part, Tones) ),
      Tones_ips ).


/* mx_basics_mx_to_tones(Xml, Tones) <-
      gets Tones = [Step, Alter, Octave] from an xml file */

mx_basics_mx_to_tones(Xml, Tones) :-
   findall( [S, A, O],
      ( X := Xml/descendant::'*'::[
           step/content::'*'=[S],
           octave/content::'*'=[O] ],
        ( [A] := X/alter/content::'*' ->
          true
        ; A = '0' ) ),
      Tones ).

mx_basics_tones_ips_to_mx_notes_ips(Tones_ips, Notes_ips) :-
   foreach([P, Tones], Tones_ips),
   foreach([P, Notes], Notes_ips) do
      mx_basics_tones_to_mx_notes(Tones, Notes).


/* mx_basics_tones_to_mx_notes(Tones, Notes) <-
      */

mx_basics_tones_to_mx_notes(Tones, Notes) :-
   findall( Note,
      ( member([S, A, O], Tones),
        Note = note:[
           pitch:[step:[S], alter:[A], octave:[O]],
           duration:['4'],
           type:['whole'] ] ),
      Notes ).


/* mx_basics_mx_notes_to_mx(Notes_ips, Xml_1, Xml_2) <-
      */

mx_basics_mx_notes_to_mx(Notes_ips, Xml_1, Xml_2) :-
   [Title] := Xml_1/descendant::'work-title'/content::'*',
%  let( Ps := Xml_1/descendant::'part-name'/content::'*' ),
%  append(Ps, Partnames),
   ( foreach([P, Notes], Notes_ips), foreach(Part, Parts) do
        Part = part:[id:P]:[
           measure:[number:'1']:[
              attributes:[
                 divisions:['1'], key:[ fifth:['0'] ],
                 time:[ beats:['4'], 'beat-type':['4'] ],
                 clef:[ sign:['G'], line:['2'] ] ] |
                 Notes ]] ),
   findall( Partheader,
      ( member([P, Notes], Notes_ips),
%  member(Partname, Partnames),
        Partheader = 'score-part':[id:P]:['part-name':['Musik']]  ),
      Partheaders ),
   A = work:['work-title':[Title]],
   B = 'part-list':[Partheaders],
   Xml_2 = 'score-partwise':[version:'2.0']:[A,B|Parts].


/* music_xml_notes_group(Notes, Groups) <-
      returns a List of Notes and Chords */

music_xml_notes_file_group(File_1, File_2) :-
   dread(xml, File_1, [Xml]),
   let( Notes := Xml/descendant::note ),
   music_xml_notes_group(Notes, Groups),
   dwrite(xml, File_2, notes:Groups).

music_xml_notes_group(Notes, Groups) :-
   ( Notes = [A2|Notes_2],
     _ := A2/chord ->
     Notes_1 = [],
     Accord = accord:[A2|As]
   ; append(Notes_1, [A1,A2|Notes_2], Notes),
     _ := A2/chord ->
     Accord = accord:[A1,A2|As] ),
   !,
   ( append(As, [N|Ns], Notes_2),
     \+ _ := N/chord ->
     music_xml_notes_group([N|Ns], Groups_2)
   ; As = Notes_2,
     Groups_2 = [] ),
   append(Notes_1, [Accord|Groups_2], Groups).
music_xml_notes_group(Notes, Notes).

mx_basics_key(Music, Key) :-
   _ := Music/descendant/key::[fifth/content::'*'=[F],
                 mode/content::'*'=[M] ],
   mx_basics_circle_of_fifths(F,M,Key).

mx_basics_circle_of_fifths(F, M, Key) :-
   Circle = [
      ['Gb',eb], ['Db',bb], ['Ab',f], ['Eb',c], ['Bb',g], ['F',d],
      ['C',a], ['G',e], ['D',b], ['A','f#'], ['E','c#'], ['B','g#'],
      ['F#','d#'] ],
   Position is F+7,
   nth(Position, Circle, X),
   ( M = major, nth(1 ,X, Key)
   ; M = minor, nth(2, X, Key) ).

mx_basics_notes_to_min(Note_1,Note_2,Note_1):-
   mx_intervals_distance(Note_1, Note_2, Distance),
   A is sign(Distance),
   A = 1.

mx_basics_notes_to_min(Note_1,Note_2,Note_2):-
   mx_intervals_distance(Note_1, Note_2, Distance),
   A is sign(Distance),
   A = -1.

mx_basics_notes_to_min(Note_1,Note_2,Note_1) :-
   mx_intervals_distance(Note_1, Note_2, 0).

mx_basics_note_smaller(Note_1, Note_2) :-
   mx_basics_notes_to_min(Note_1, Note_2, Note),
   !,
   Note_1 = Note.


/* mergesort_generic(Comparator, List, Sorted_List) <-
     */

mx_basics_mergesort_generic(_, Xs, Xs) :-
   length(Xs, N),
   N =< 1,
   !.
mx_basics_mergesort_generic(Comparator, Xs, Ys) :-
   middle_split(Xs, Xs1, Xs2),
   mx_basics_mergesort_generic(Comparator, Xs1, Ys1),
   mx_basics_mergesort_generic(Comparator, Xs2, Ys2),
   mx_basics_mergesort_merge_generic(Comparator, Ys1, Ys2, Ys).

mx_basics_mergesort_merge_generic(_, [], Xs, Xs) :-
   !.
mx_basics_mergesort_merge_generic(_, Xs, [], Xs) :-
   !.
mx_basics_mergesort_merge_generic(
   Comparator, [X1|Xs1], [X2|Xs2], [X|Xs]) :-
   ( apply(Comparator, [X1, X2]),
     X = X1,
     mx_basics_mergesort_merge_generic(
        Comparator, Xs1, [X2|Xs2], Xs)
   ; X = X2,
     mx_basics_mergesort_merge_generic(
        Comparator, [X1|Xs1], Xs2, Xs) ).


/*** queries ******************************************************/


mx_query_acchord(File_1, Accords) :-
   dread(xml, File_1, [Xml]),
   let( Notes := Xml/descendant::note ),
   dwrite(xml, aaa, notes:Notes),
   music_xml_notes_group(Notes, Groups),
   Xml_2 = notes:Groups,
   dwrite(xml, bbb, Xml_2),
   findall( Accord,
      Accord := Xml_2/accord,
      Accords ).


/******************************************************************/


