

mx_files_read_file(Type, File, Xml) :-
   music_xml_file_to_type(File, T),
   ( Type = T ->
     File_2 = File
   ; dislog_variable_get(example_path,
        'music_xml/convert_tmp.xml', File_2),
     mx_files_term_convert(Type, File, File_2) ),
   dread(xml, File_2, Xml) ).

music_xml_file_to_type(File, Type) :-
   file_base_name(File, Base),
   ( name_contains_name(Base, timewise),
     Type = timewise
   ; Type = partwise ).

mx_files_term_convert(Type, File, File_2) :-
   shell('cd ..'),
   shell('cd ..'),

C:\Program Files\Saxonica\SaxonHE9.4N> bin\Transform -t -s:C:\Users\Asus\Dropbox\Diplomarbeit\1_unmittelbarer_Bezug\Bach_Choraele\004207B_.xml  -xsl:C:\Users\Asus\Dropbox\Diplomarbeit\Stylesheets\partwise-timewise-stylesheet.xml -o:C:\Users\Asus\Dropbox\Diplomarbeit\Stylesheets\test.xml

