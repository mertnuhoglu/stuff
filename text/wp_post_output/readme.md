# Problem:

wp cli does not work for some wordpress installations. I want to find out which wordpress installations are broken automatically.

I run the following command for every wordpress installation:

    wp post list

I redirected all the stderr and stdout outputs to a log file.

## input:

    + for wp in '$wpprojects'
    + cd okuloncesi_blog
    + echo okuloncesi_blog
    okuloncesi_blog
    + wp post list
    Error: 
    <h1>Error establishing a database connection</h1>
    + echo roxalana_blog
    pdad
    + wp post list
    ID	post_title	post_name	post_date	post_status
    103	Küll-cüz, külli-cüzi ilişkisi - 13.04.2015	kull-cuz-kulli-cuzi-iliskisi-13-04-2015	2015-04-13 22:14:58	publish
    95	Kıyas-ı Temsili - 06.04.2015	kiyas-i-temsili-06-04-2015	2015-04-09 14:36:29	publish
    94	Onikinci Hakikat - 01.04.2015	onikinci-hakikat-01-04-2015	2015-04-09 14:35:59	publish
    ...
    roxalana_blog
    + wp post list
    Error: 
    <h1>Error establishing a database connection</h1>
    + echo roxalana_blog
    ...

## output:

    okuloncesi_blog Error: 
    roxalana_blog Error: 

## viml script:

    g/Error/ +1,/^+ echo/ d
    g/wp post/d
    g/^ID/d
    g/^\d\+/d
    g/^+/d
    g/\d\+:\d\+/d
    g/Error/-1 j
    v/Error/d
