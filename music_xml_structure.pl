

/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Structure                     ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(structure(find_fermata)) :-
   dislog_variable_get(example_path,
      'music_xml/004207B_timewise.xml', File),
   dread(xml, File, [Music]),
   mx_structure_music_to_fermata_notes_and_m_nrs(Music, Tuples),
   write(Tuples).

test(structure(get_ultimae)) :-
   dislog_variable_get(example_path,
      'music_xml/004207B_timewise.xml', File),
   dread(xml, File, [Music]),
   mx_structure_music_to_ultimae(Music, Ultimae),
   writeln_list(Ultimae).


/*** interface ****************************************************/


/* mx_structure_music_to_choralrows(Music, Choralrows) <-
      Music has to be in timewise */

/* mx_structure_music_to_choralrows(Music, Choralrows) :-
   mx_structure_music_to_ultimae(Music, Ultimae),
   member([M_Nr, beat(X, Y)], Ultimae),
   mx_structure_music_to_pieces(
      Music, M_Nr, beat(X, Y), Choralrows).*/


/* mx_structure_music_to_ultimae(Music, Ultimae)  <-
      identifies notes within Music (which has to be in timewise)
      and returns their position (in the Form of M_Nr2:beat(X,Y))
      in the List Ultimae*/

mx_structure_music_to_ultimae(Music, Ultimae) :-
   mx_structure_music_to_fermata_notes_and_m_nrs(Music, Tuples),
   findall( Note,
      member(M_Nr:Note, Tuples),
      Notes ),
   findall( Measure,
      ( member(M_Nr:_, Tuples),
        Measure := Music/descendant::measure::[@number=M_Nr] ),
      Measures),
   mx_structure_measures_to_fermata_starts(Measures, Starts),
   pair_lists(:, Starts, Notes, Pairs_1),
   mx_structure_starts_and_tuples_to_uppers(Pairs_1, Uppers),
   pair_lists(:, Starts, Uppers, Pairs_2),
   maplist(mx_time_from_time_interval, Pairs_2, List ),
   findall( M_Nr,
      member(M_Nr:_, Tuples),
      M_Nrs ),
   pair_lists(:, M_Nrs, List, Ultimae).

mx_structure_starts_and_tuples_to_uppers(Pairs, Uppers) :-
   foreach(Start:Note, Pairs), foreach(Upper, Uppers) do
      [D] := Note/duration/content::'*',
      atom_number(D, Duration),
      Upper is Start + Duration - 1.

mx_structure_measures_to_fermata_starts(Measures, Starts) :-
   findall( Start,
      ( member(Item, Measures),
        mx_time_item_note_to_start(fermata, Item, Start) ),
      Starts ).

mx_structure_music_to_fermata_notes_and_m_nrs(Music, Tuples):-
   findall( M_Nr:Note,
      Note := Music/descendant::measure::[@number=M_Nr]
         /part/note::[/child::notations/fermata],
      Tuples ).


%mx_structure_music_to_pieces(Music, [Ultimae], Choralrows) :-
%   Breaklist = [1:beat(1,1/4)|Ultimae].


/******************************************************************/


