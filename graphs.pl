/******************************************************************/
/***                                                            ***/
/***          MusicXML Analysis:  Graphs                        ***/
/***                                                            ***/
/******************************************************************/



/*** tests ********************************************************/

test(music_xml, graphs(timewise)) :-
   File = 'examples/music_xml/timewise_example.xml',
   dread(xml, File, [Xml]),
   fn_triple_to_schema_picture_2(Xml).
