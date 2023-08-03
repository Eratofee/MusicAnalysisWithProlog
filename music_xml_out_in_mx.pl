
/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Out in MX                     ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/

test(music_xml, octaves) :-
   dislog_variable_get(example_path,
      'music_xml/Dichterliebe01.xml', File_1),
   dread(xml, File_1, [Xml_1]),
   mx_out_mx_to_tones_ip(Xml_1, Tones_ips),
   mx_out_tones_ip_to_mx(Xml_1, Tones_ips, Xml_2),
   write(Xml_2).

/*** interface ****************************************************/

%mx_out_mx_to_tones_ip(Xml_1, Tones_ips) :-
   
   

mx_out_tones_ip_to_mx(Xml_1, Tones_ips, Xml_2) :-
   foreach([P, CandTs], Tones_ips),
   foreach([P, Notes], Notes_ips) do
      mx_out_chord_and_tones_to_notes(CandTs, Notes),
   mx_out_mx_notes_to_mx(Notes_ips, Xml_1, Xml_2).

mx_out_chord_and_tones_to_notes(CandTs, Notes) :-
   findall(Note,
      member(CorT,CandTs),
      ((member([S,A,O],CorT), 
       Note = note:[ pitch:[step:[S], alter:[A], octave:[O]],
          duration:['4'], type:['whole'] ])
      ;(member([_|Ts],CorT),
       mx_out_chord_to_notes(Ts, CNotes),
       member(Note, CNotes))),
     Notes ). 
      
mx_out_chord_to_notes(Ts, CNotes) :-
   findall(CNote,
       member([S,A,O],Ts),
       CNote = note:[ chord:[],
          pitch:[step:[S], alter:[A], octave:[O]],
          duration:['4'], type:['whole'] ]),
     CNotes. 
      
mx_out_measures_of_40(Notes, Measures) :-
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



/******************************************************************/


