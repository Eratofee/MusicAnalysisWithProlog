

%  dislog_variable_set(fn_mode, fn).
%  dislog_variable_set(fn_mode, gn).
%  dislog_variable_get(fn_mode, Mode).

%  term_to_picture(a(1, t(2))).


test(music_xml, octaves) :-

   dislog_variable_get( example_path,
       'music_xml/Dichterliebe01.xml', File ),
   dread(xml, File, [Xml]),
    
   ddbase_aggregate( [N,  list(Octaves)],
      Octaves := Xml/descendant::pitch::[
         step/content::'*'=[N]]/octave,
      Tuples ),

   dislog_variable_get( output_path,
      'tupelsDLOctaves.pl', File_1 ),
%  File_1 = user,
   writeln_list(File_1, Tuples),

   ( foreach([N, Os], Tuples), foreach(Y, Ys) do
        Y = note:[name:N]:Os ),

%  Ausgabe in Konsole mit Wurzelelement "noten"
%  dwrite(xml, noten:Ys).
   dislog_variable_get( output_path,
      'octavesDL.xml', File_2 ),
   dwrite(xml, File_2, noten:Ys).


test(music_xml, octavesInParts) :-
   dislog_variable_get( example_path,
      'music_xml/Dichterliebe01.xml', File ),
   dread(xml, File, [Xml]),
   ddbase_aggregate( [P, list(Xs)],
      ( Part := Xml/descendant::part::[@id=P],
        music_xml_part_to_octaves(Part, Xs) ),
      Tuples ),

   star_line,
   dislog_variable_get( output_path,
      'tupelsDLOctavesInParts.pl', File_1 ),
%  File_1 = user,
   writeln_list(File_1, Tuples),

   ( foreach([P, Ns], Tuples), foreach(Y, Ys) do
        Y = part:[partname:P]:Ns ),

   dislog_variable_get( output_path,
      'octavesInPartsDL.xml', File_2 ),
   dwrite(xml, File_2, noten:Ys).

music_xml_part_to_octaves(Xml_1, Xml_2) :-
   ddbase_aggregate( [N, list(Octaves)],
      Octaves := Xml_1/
         descendant::pitch::[step/content::'*'=[N]]/octave,
      Tuples ),
   ( foreach([N, Os], Tuples), foreach(X, Xml_2) do
        X = note:[name:N]:Os ).


test(music_xml, durationInParts) :-
   dislog_variable_get( example_path,
      'music_xml/Dichterliebe01.xml', File ),
   dread(xml, File, [Xml]),

   ddbase_aggregate( [P, N, list(DurationTypes)],
      DurationTypes := Xml/descendant::part::[@id=P]/
%        descendant::pitch::[step/content::'*'=[N]]/
%        parent::[type/content::'*'=[Duration]],
         descendant::pitch::[step/content::'*'=[N]]/
         ancestor::note/type,
      Tuples ),

   star_line,
   dislog_variable_get( output_path,
      'tupelsDLDurationsInParts.pl', File_1 ),
   writeln_list(File_1, Tuples),
%  writeln_list(Tuples),

   ( foreach([P, N, Os], Tuples), foreach(Y, Ys) do
        Y = part:[partname:P]:[note:[name:N]:Os] ),

   dislog_variable_get( output_path,
      'durationsInPartsDL.xml', File_2 ),
   dwrite(xml, File_2, noten:Ys).


/*
DisLog/sources/basic_algebra/basics/lists.pl:

?- list_exchange_sublist([[[b,c], [1,2]]], [a,b,c,d,e,f,g], Xs).
Xs = [a, 1, 2, d, e, f, g] .

?- name_exchange_elements(["e5", "s9"], esther, T).
T = '59th5r' .

prolog:

?- name(esther, L).
L = [101, 115, 116, 104, 101, 114].

?- atom_codes(esther, Cs).
Cs = [101, 115, 116, 104, 101, 114].

DisLog/sources/basic_algebra/basics/names.pl:

?- name_split_at_position([" ", "m"], 'esther programmiert prolog', Ns).
Ns = [esther, progra, '', iert, prolog] .

?- name_split_at_position([" "], 'esther programmiert prolog', Names),
   concat_with_separator(Names, :, Name).
Names = [esther, programmiert, prolog],
Name = 'esther:programmiert:prolog' . */



