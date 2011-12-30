MBCL
====

This is a simple interface to perform queries against the MB
webservice.

At the moment, relationship queries, browse requests etc. aren't
supported. They will be as I need them (or anyone could send me a
patch...)

Example usage:

    MBCL> (defpackage :mbcl-user (:use :cl))
    #<PACKAGE "MBCL-USER">
    MBCL> (mbcl:search-request "artist:nielsen AND symphony AND blomstedt" :type "release")
    #<LIST-SEGMENT(RELEASE) [0,6] of 7>
    MBCL> (contents *)
    (#<RELEASE 'Symphony No. 6 etc. (San Fr...' BY 'Carl Nielsen'>
     #<RELEASE 'Symphonies 2 & 3 (San Franc...' BY 'Carl Nielsen'>
     #<RELEASE 'Symphonies 1 & 6 (San Franc...' BY 'Carl Nielsen'>
     #<RELEASE 'The Symphonies Nos. 1 - 3 (...' BY 'Carl Nielsen'>
     #<RELEASE 'Concertos for clarinet, flu...' BY 'Carl Nielsen'>
     #<RELEASE 'Symphonies Nos. 4 & 5 (San ...' BY 'Carl Nielsen'>
     #<RELEASE 'Peer Gynt Suites 1 & 2 / Ov...' BY 'Grieg / Nielsen'>)
    MBCL> (sixth *)
    #<RELEASE 'Symphonies Nos. 4 & 5 (San ...' BY 'Carl Nielsen'>
    MBCL> (defparameter *rel* *)
    *REL*
    MBCL> (artist-credit *rel*)
    #<ARTIST-CREDIT 'Carl Nielsen'>
    MBCL> (title *rel*)
    "Symphonies Nos. 4 & 5 (San Francisco Symphony feat. conductor: Herbert Blomstedt)"
    MBCL> (label-info *rel*)
    #<LIST-SEGMENT(LABEL-INFO) [0,0] of 1>
    MBCL> (first (contents *))
    #<LABEL-INFO '421 524-2' ('Decca')>
    MBCL> (label *)
    #<LABEL 'Decca'>
    MBCL> (contents (aliases *))
    (#<ALIAS 'DECCA'> #<ALIAS 'Decca Eloquence'> #<ALIAS 'Decca Music Group Ltd'>
     #<ALIAS 'Decca Records'>)
    MBCL> (life-span **)
    #<TIME-PERIOD 1929 TO NOW>
    MBCL> 

Frankly, at the moment this is mostly useful via the lisp inspector
(where you can see all the slots nicely!).
