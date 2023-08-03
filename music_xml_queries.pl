
/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Queries                       ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/


test(music_xml, octaves) :-
   dislog_variable_get(example_path,
      'music_xml/Dichterliebe01.xml', File_1),
   dislog_variable_get(output_path,
      'octavesDL.xml', File_2),
   music_xml_file_to_notes(File_1, File_2).

test(music_xml, set) :-
   dislog_variable_get(example_path,
      'music_xml/Dichterliebe01.xml', File_1),
   dislog_variable_get(output_path,
      'setDL.xml', File_2),
   music_xml_file_to_notes_as_set(File_1, File_2).

test(music_xml, octaves_in_parts) :-
   dislog_variable_get( example_path,
      'music_xml/Dichterliebe01.xml', File_1 ),
   dislog_variable_get( output_path,
      'octaves_in_partsDL.xml', File_2 ),
   music_xml_file_to_octaves_in_parts(File_1, File_2).


/*** interface ****************************************************/


/* music_xml_to_notes(Xml_1, Xml_2) <-
      */

music_xml_file_to_notes(File_1, File_2) :-
   dread(xml, File_1, [Xml_1]),
   music_xml_xml_to_tones_in_parts(Xml_1, Tones_ips), 
   music_xml_tones_ips_to_mx_notes_ips(Tones_ips, Notes_ips),
   foreach([_, Notes],Notes_ips),
   foreach([_,Measures],Measures_ips) do
      music_xml_measures_of_40(Notes, Measures),
 
   [Title] := Xml_1/descendant::'work-title'/content::'*',

   Xml_2 = 'score-partwise':[version:'2.0']:[
      work:['work-title':[Title]],
      'part-list':[]:[
         'score-part':[id:'P1']:['part-name':['Music']] ],
      part:[id:'P1']: Measures ],
   dwrite(xml, File_2, Xml_2).   
   
   
   
music_xml_measures_of_40(Notes, Measures) :-
   split_multiple(40, Notes, Groups),
   findall( Measure,
      ( nth(N, Groups, Group),
        Measure = measure:[number:N]:[
           attributes:[
              divisions:['1'],
              key:[ fifth:['0'] ],
              time:[ beats:['4'], 'beat-type':['4'] ],
              clef:[ sign:['G'], line:['2'] ] ] |
           Group ] ),
      Measures ).


/* music_xml_to_notes_as_set(Xml_1, Xml_2) <-
      */

music_xml_file_to_notes_as_set(File_1, File_2) :-
   dread(xml, File_1, [Xml_1]),
   music_xml_to_notes_as_set(Xml_1, Xml_2),
   dwrite(xml, File_2, Xml_2).

music_xml_to_notes_as_set(Xml_1, Xml_2) :-
   
   ddbase_aggregate( [N, remove_duplicates(Octaves)],
      Octaves := Xml_1/descendant::pitch::[
         step/content::'*'=[N] ]/octave,
      Tuples ),

   zurueck(Tuples, Notes),

   [Title] := Xml_1/descendant::'work-title'/content::'*',

   Xml_2 = score-partwise:[version:'2.0']:[
      work:['work-title':[Title]],
      'part-list':[
         'score-part':[id:'P1']:['part-name':['Music']] ],
      part:[id:'P1']:[
         measure:[number:'1']:[
            attributes:[
               divisions:['1'],
               key:[ fifth:['0'] ],
               time:[ beats:['4'], 'beat-type':['4'] ],
               clef:[ sign:['G'], line:['2'] ] ] |
            Notes ] ] ].


/* music_xml_to_octaves_in_parts(Xml_1, Xml_2) <-
      */

music_xml_file_to_octaves_in_parts(File_1, File_2):- 
   dread(xml, File_1, [Xml_1]),
   music_xml_to_octaves_in_parts(Xml_1, Xml_2),
   dwrite(xml, File_2, Xml_2). 


music_xml_to_octaves_in_parts(Xml_1, Xml_2) :-

   ddbase_aggregate( [P, list(Xs)],
      ( Part := Xml_1/descendant::part::[@id=P],
        music_xml_part_to_octaves(Part, Xs) ),
      Tuples ),

   [Title] := Xml_1/descendant::'work-title'/content::'*',
   
   %list(Partnames) := Xml_1/descendant::'part-name'/content::'*',
  
   ( foreach([P, Ns], Tuples), foreach(Y, Ys) do
        Y = part:[id:P]:[
        measure:[number:'1']:[
            attributes:[
               divisions:['1'],
               key:[ fifth:['0'] ],
               time:[ beats:['4'], 'beat-type':['4'] ],
               clef:[ sign:['G'], line:['2'] ] ] |
               Ns ]]),
        
   findall(Partheader,
      ( member([P, Ns], Tuples), /*member(Partname, Partnames),*/      
        Partheader = 'score-part':[id:P]:['part-name':['Musik']]  ),
      Partheaders),
   
               
   Xml_2 = score-partwise:[version:'2.0']:[
      work:['work-title':[Title]],
      'part-list':[
         Partheaders],Ys].


music_xml_part_to_octaves(Xml, Octaves) :-
   ddbase_aggregate( [N, remove_duplicates(Octaves)],
      Octaves := Xml/
         descendant::pitch::[step/content::'*'=[N]]/octave,
      Tuples ),

   zurueck(Tuples, Octaves).


/* zurueck(Tuples, Notes) <-
      */

zurueck(Tuples, Notes) :-
   findall( Note,
      ( member([N, [O|Os]], Tuples),
        Es = [ pitch:[step:[N], X],
           duration:['4'], type:['whole'] ],
        ( X = O,
          Note = note:Es
        ; member(X, Os),
          Note = note:[chord:[]|Es] ) ),
      Notes ).
/******************************************************************/


