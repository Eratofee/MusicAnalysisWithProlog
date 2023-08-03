

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Statistics                    ***/
/***                                                            ***/
/******************************************************************/



/***  key  ********************************************************/

test(music_xml, keys(all_files)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_partwise/', Path),
   dislog_variable_get(example_path,
      'music_xml/keys.txt', File_2),
   directory_contains_files(xml, Path, Ys),
   findall( Key,
      ( member(Y, Ys),
	atom_concat(Path, Y, File_1),
	dread(xml, File_1, [Xml]),
	mx_basics_key(Xml, Key) ),
       Keys ),
   dwrite(txt, File_2, Keys).


/*** consecutive***************************************************/


test(music_xml, consecutive(one_file)) :-
   dislog_variable_get(example_path,
%     'music_xml/data/bach_choraele_timewise/timewise_004207B_.xml',
     'music_xml/MitParallelenTimewise.xml',
      File_1),
   dread(xml, File_1, [Xml]),
   mx_statistics_music_to_Cons(Xml, Cons),
   writeln_list(Cons).

test(music_xml, consocs(all_files)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_timewise/', Path),
   directory_contains_files(xml, Path, Ys),
   dislog_variable_get(example_path,
      'music_xml/cons_octs_all_files.txt', File_2),
   findall( Cons,
      ( member(Y, Ys),
	atom_concat(Path, Y, File_1),
	dread(xml, File_1, [Xml]),
	mx_statistics_music_to_Cons(Xml, Cons) ),
       Conss ),
   dwrite(txt, File_2, Conss).

mx_statistics_music_to_Cons(Xml, Cons) :-
   findall(M_Nr,
      (	M_Atom := Xml/descendant::measure@number,
        atom_number(M_Atom, M_Nr) ),
      Measure_Nrs),
   write(Measure_Nrs),
   Beats =[1, 2, 3, 4, 5, 6, 7, 8],
   findall( [Measure_Nr, beat(B, 1/8), Notes],
      ( member(Measure_Nr, Measure_Nrs),
        member(B, Beats),
        mx_time_measure_and_beat_to_notes(
           Xml, Measure_Nr, beat(B, 1/8), Notes) ),
%	flatten(Notes, Flatted_Notes),
%	maplist(mx_intervals_note_to_tone, Flatted_Notes, Tones),
%	writeln(Tones) ),
      Notess ),
      findall([Mn, beat(X, 1/8), C],
         ( member([Mn, beat(X, 1/8),[[S1], [A1], [T1], [Ba1]]],
              Notess),
           ( X = 8 ->
             Mn_N is Mn+1,
             Y = 1,
             member([Mn_N, beat(Y, 1/8), [[S2], [A2], [T2], [Ba2]]],
                Notess)
             ; Y is X+1,
             member([Mn, beat(Y, 1/8),[[S2], [A2], [T2], [Ba2]]],
                Notess) ),
           ( mx_counterpoint_consecutive(C, S1:S2, A1:A2)
           ; mx_counterpoint_consecutive(C, S1:S2, T1:T2)
           ; mx_counterpoint_consecutive(C, S1:S2, Ba1:Ba2)
           ; mx_counterpoint_consecutive(C, A1:A2, T1:T2)
           ; mx_counterpoint_consecutive(C, A1:A2, Ba1:Ba2)
           ; mx_counterpoint_consecutive(C, T1:T2, Ba1:Ba2) ) ),
       Cons ).


/****** Structure**************************************************/

%Aus wievielen Choralreihen ist ein Stück zusammengesetzt:
test(music_xml, amount_choralrows(one_file)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_timewise/timewise_004207B_.xml',
       File),
   dread(xml, File, [Xml]),
   mx_structure_music_to_fermata_notes_and_m_nrs(Xml, Tuples),
   length(Tuples, Amount),
   writeln(Amount).

test(music_xml, amount_choralrows(all_files)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_timewise/', Path),
   dislog_variable_get(example_path,
      'music_xml/choralrows_amounts.txt', File_2),
   directory_contains_files(xml, Path, Ys),
   findall( Amount,
      ( member(Y, Ys),
        atom_concat(Path, Y, File_1),
        dread(xml, File_1, [Xml]),
        mx_structure_music_to_fermata_notes_and_m_nrs(Xml, Tuples),
%       let( Tuples := Xml/descendant::note::[
%          /child::notations/fermata] ),
        length(Tuples, Amount) ),
      Amounts ),
%    writeln_list(Amounts).
   dwrite(txt, File_2, Amounts).


/***** Ambitus ****************************************************/

%Was für einen Ambitus haben die einzellnen Stimmen in einem Stück:
%
test(music_xml, ambitus(one_file)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_partwise/000806B_.xml', File_1),
   dread(xml, File_1, [Xml]),
   mx_statistics_xml_to_ambituss(Xml, Ambituss),
   writeln_list(Ambituss).

test(music_xml, ambitus(all_files)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_partwise/', Path),
   dislog_variable_get(example_path,
      'music_xml/ambiutsss.txt', File_2),
   directory_contains_files(xml, Path, Ys),
   ( foreach(Y, Ys), foreach( Name_Ambitus_Pairs, Ambitusss) do
        atom_concat(Path, Y, File_1),
        dread(xml, File_1, [Xml]),
        mx_statistics_xml_to_ambituss(Xml,  Name_Ambitus_Pairs),
        writeln( Name_Ambitus_Pairs) ),
   dwrite(txt, File_2, Ambitusss).

mx_statistics_xml_to_ambituss(Xml,  Name_Ambitus_Pairs) :-
   mx_basics_mx_to_partnames(Xml, Partnames),
   let( Parts := Xml/descendant::part),
   ( foreach(Part, Parts), foreach([Min, Max] ,Ambituss) do
        let( Notes:= Part/descendant::note::[/pitch] ),
        remove_duplicates(Notes, Notes_Set),
        mx_chords_notes_sort(Notes_Set, Notes_Set_Sort),
        last(Notes_Set_Sort, Ma),
        nth(1, Notes_Set_Sort, Mi),
        mx_basics_mx_to_tones(Ma, [Max]),
        mx_basics_mx_to_tones(Mi, [Min]) ),
   pair_lists(:, Partnames, Ambituss, Name_Ambitus_Pairs).


/***** Tonikae ****************************************************/

test(music_xml, tonikae(one_file)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_timewise/timewise_004207B_.xml',         File_1),
   dread(xml, File_1, [Xml]),
   mx_statistics_xml_to_notes_at_beat([1,3,5,7], Xml, Measures),
   !,
   mx_statistics_xml_to_tonikae(Xml, Measures, Tonikae),
   forall( member(A:B:_, Tonikae),
      writeln(A:B) ),
   length(Tonikae, L),
   writeln('Anzahl':L).

test(music_xml, tonikae(all_files)) :-
   dislog_variable_get(example_path,
      'music_xml/data/bach_choraele_timewise/', Path),
   dislog_variable_get(example_path,
      'music_xml/amounts_tonikae.txt', File_2),
   directory_contains_files(xml, Path, Ys),
   forall( member(Y, Ys),
   ( atom_concat(Path, Y, File_1),
     dread(xml, File_1, [Xml]),
     mx_statistics_xml_to_notes_at_beat([1, 3, 5, 7], Xml, Ms),
     %!,
     mx_statistics_xml_to_tonikae(Xml, Ms, Tonikae),
     length(Tonikae, Amount),
     open(File_2, append, Stream),
     format(Stream, 'Path: ~w Amount: ~d~n', [Y, Amount]),
     close(Stream) ) ).


mx_statistics_xml_to_tonikae(Xml, Measures, Tonikae) :-
   mx_basics_key(Xml, Key),
%   writeln(keykeykey:Key),
%   writeln('MEASURES':Measures),
   findall(M_Nr:beat(B, 1/8):Tonika,
    ( member(Measure, Measures),
      member(M_Nr:beat(B, 1/8):Beat, Measure),
%      writeln('BEAT_BEAT':Beat),
      mx_chords_chord_to_contracted_chord(chord:Beat, chord:[]:NA),
%      writeln(nananana:NA),
      maplist(mx_intervals_note_to_tone, NA, Tones),
%      writeln(tonestonestones:Tones),
      remove_duplicates(Tones, C),
%      writeln(removedremovedremoved:C),
      mx_basics_tones_to_mx_notes( C, Notes),
%      writeln(lalalalalal:Notes),
      mx_chords_chord_in_stacked_thirds(chord:[]:Notes, Tonika, _),
%      writeln(chord_2chord_2chord_2:Tonika),
%      mx_chords_chord_to_name(Tonika, Name),
%      write(M_Nr:beat(B, 1/8)),
      mx_chords_chord_is_tonic(Key, Tonika) ),
   Tonikae).


mx_statistics_xml_to_notes_at_beat(Beats, Xml, Notesss) :-
   findall( M_Nr,
      ( M_Atom := Xml/descendant::measure@number,
        catch(atom_number(M_Atom, M_Nr), _,
           ( writeln(M_Atom), fail) ) ),
      Measure_Nrs ),
   findall( N_Measure,
      ( member(M_Nr, Measure_Nrs),
        findall( M_Nr:beat(B, 1/8):N_Chord,
           ( member(B, Beats),
             mx_time_measure_and_beat_to_notes(
                Xml, M_Nr, beat(B, 1/8), N_Chor),
             flatten(N_Chor, N_Chord) ),
          N_Measure ) ),
       Notesss ).

   /*
  ( foreach(M_Nr, Measure_Nrs), foreach(N_Measure, Notesss) do
	( foreach(B, Beats), foreach(N_Chord, N_Measure) do
             mx_time_measure_and_beat_to_notes(
	        Xml, M_Nr, beat(B, 1/8), N_Chor),
	     flatten(N_Chor, N_Chord) ) ).*/
