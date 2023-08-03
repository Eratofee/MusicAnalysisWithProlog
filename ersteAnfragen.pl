
test(music_xml, octaves) :-

   
    dread(xml, 'examples/music_xml/Dichterliebe01.xml', [X]),
    
     /* SELECT PNAME , SUM(HOURS)
       FROM PROJECT, WORKS_ON
       WHERE PROJECT.PNO = WORKS_ON.PNO
       GROUP BY PNAME
    */
    ddbase_aggregate( [N,  list(Octaves)],
        Octaves := X/descendant::pitch::[step/content::'*'=[N]]/octave,
        Tuples),

    writeln_list('examples/music_xml/outs/tupelsDLOctaves.pl',Tuples), %Ausgabe Datei
    %writeln_list(Tuples), %Konsole

    (foreach([N, Os], Tuples),
         foreach(Y, Ys) do
              Y = note:[name:N]:Os),

    %dwrite(xml, noten:Ys). %Ausgabe in Konsole mit Wurzelelement "noten"
    dwrite(xml, 'examples/music_xml/outs/octavesDL.xml', noten:Ys). %Ausgabe Datei


test(music_xml, octavesInParts) :-

    dread(xml, 'examples/music_xml/Dichterliebe01.xml',[X]),

    ddbase_aggregate( [P, N, list(Octaves)],
         Octaves := X/descendant::part::[@id=P]/
              descendant::pitch::[step/content::'*'=[N]]/octave,
         Tuples),

    star_line,

    writeln_list('examples/music_xml/outs/tupelsDLOctavesInParts.pl',Tuples),
    %writeln_list(Tuples), %Ausgabe in Konsole

    star_line,

    (foreach([P, N, Os], Tuples),
         foreach(Y, Ys) do
              Y = part:[partname:P]:[note:[name:N]:Os]),

    %dwrite(xml, noten:Ys). %Ausgabe in Konsole mit Wurzelelement "noten"
    %dwrite(xml, 'octaves.xml',Ys). %Ausgabe in Datei ohne Wurzelelement "noten"
    dwrite(xml, 'examples/music_xml/outs/octavesInPartsDL.xml', noten:Ys). %Ausg. Datei

test(music_xml, durationInParts) :-

    dread(xml, 'examples/music_xml/Dichterliebe01.xml',[X]),

    ddbase_aggregate( [P, N, list(DurationTypes)],
         DurationTypes := X/descendant::part::[@id=P]/
              %descendant::pitch::[step/content::'*'=[N]]/parent::[type/content::'*'=[Duration]],
              descendant::pitch::[step/content::'*'=[N]]/ancestor::note/type,
         Tuples),

    star_line,

    %writeln_list('examples/music_xml/outs/tupelsDLDurationsInParts.pl',Tuples),
    writeln_list(Tuples), %Ausgabe in Konsole

    star_line,

    (foreach([P, N, Os], Tuples),
         foreach(Y, Ys) do
              Y = part:[partname:P]:[note:[name:N]:Os]),

    %dwrite(xml, noten:Ys). %Ausgabe in Konsole mit Wurzelelement "noten"
    %dwrite(xml, 'octaves.xml',Ys). %Ausgabe in Datei ohne Wurzelelement "noten"
    dwrite(xml, 'examples/music_xml/outs/durationsInPartsDL.xml', noten:Ys). %Ausg. Datei

test(music_xml, fail) :-
   fail.

test(music_xml, true) :-
   true.

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
Name = 'esther:programmiert:prolog' .

*/


