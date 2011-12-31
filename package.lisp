(defpackage :mbcl
  (:use :cl)
  (:export
   ;; Search
   #:search-request

   ;; General MB call
   #:mb-request #:*mb-slot-value-debug*

   ;; MB Objects & accessors
   #:artist
     #:id #:name #:sort-name #:disambiguation #:artist-type
     #:country #:life-span #:aliases
   #:release-group
     #:id #:rg-type #:title #:first-release-date
     #:artist-credit #:release-list
   #:release
     #:id #:title #:disambiguation #:status #:quality #:packaging
     #:text-representation #:artist-credit #:release-group #:date
     #:country #:asin #:barcode #:label-info #:medium-list
   #:recording
     #:id #:title #:length #:artist-credit #:release-list
   #:label
     #:id #:name #:sort-name #:disambiguation #:label-code
     #:label-type #:country #:life-span #:aliases
   #:work
     #:id #:work-type #:title #:disambiguation #:aliases

   ;; Auxiliary objects & accessors
   #:name-credit
     #:name-credit-string #:artist #:name #:join-phrase
   #:alias
     #:locale
   #:time-period
     #:begin #:end
   #:artist-credit
     #:name-credits
   #:track
     #:title
   #:disc
     #:id #:sectors #:release-list
   #:medium
     #:position #:format #:disc-list #:track-list
   #:medium-list
     #:track-count
   #:text-representation
     #:language #:script
   #:label-info
     #:catno #:label
   #:relation
     #:relation-type #:direction #:attributes
     #:beginning #:end #:target
   #:attribute-list
     #:attributes))
