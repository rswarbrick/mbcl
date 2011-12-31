MBCL
====

This is a simple interface to perform queries against the MB
webservice.

At the moment, ISWCs, PUIDs and tags aren't supported. They will be as
I need them (or anyone could send me a patch...)

Also, the parser is intentionally brittle and dies horribly when it
sees something it doesn't recognise. This is to make me aware of stuff
I haven't dealt with yet, but probably makes the library less useful
in the short term...

Example usage:

    MBCL> (defpackage :mbcl-user (:use :cl))
    #<PACKAGE "MBCL-USER">
    MBCL> (in-package :mbcl-user)
    #<PACKAGE "MBCL-USER">
    MBCL-USER> (mbcl:search-request "artist:nielsen AND symphony AND blomstedt" :type "release")
    #<MBCL:PARTIAL-LIST 7 in 1 pages of 7 objects {BAC2E39}>
    MBCL-USER> (sixth (mbcl:pl-as-list *)) ;; One way...
    #<MBCL:RELEASE 'Symphonies Nos. 4 & 5 (San ...' BY 'Carl Nielsen' {BAC63D1}>
    MBCL-USER> (mbcl:pl-nth ** 5) ;; The other
    #<MBCL:RELEASE 'Symphonies Nos. 4 & 5 (San ...' BY 'Carl Nielsen' {BAC63D1}>
    MBCL-USER> (defparameter *release* *)
    *RELEASE*
    MBCL-USER> (mbcl:artist-credit *release*)
    #<MBCL:ARTIST-CREDIT Carl Nielsen {BAC65F9}>
    MBCL-USER> (mbcl:title *release*)
    "Symphonies Nos. 4 & 5 (San Francisco Symphony feat. conductor: Herbert Blomstedt)"
    MBCL-USER> (mbcl:label-info *release*)
    #<MBCL:PARTIAL-LIST 1 in 1 pages of 1 objects {BAC6C69}>
    MBCL-USER> (first (mbcl:pl-as-list *))
    #<MBCL:LABEL-INFO  '421 524-2' ('Decca') {BAC6C99}>
    MBCL-USER> (mbcl:label *)
    #<MBCL:LABEL Decca {BAC6CB9}>
    MBCL-USER> (mbcl:aliases *)
    (#<MBCL:ALIAS 'DECCA' {C41D069}> #<MBCL:ALIAS 'Decca Eloquence' {C41D5A9}>
     #<MBCL:ALIAS 'Decca Music Group Ltd' {C41D5D1}>
     #<MBCL:ALIAS 'Decca Records' {C41D5F9}>)
    MBCL-USER> (mbcl:life-span **)
    #<MBCL:TIME-PERIOD 1929 TO NOW {C3C4469}>

Frankly, at the moment this is mostly useful via the lisp inspector
(where you can see all the slots nicely!).
