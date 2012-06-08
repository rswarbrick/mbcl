(defpackage :mbcl
  (:use :cl :split-sequence)
  (:export
   ;; Search
   #:search-request

   ;; Other utilities
   #:mb-request #:*mb-slot-value-debug* #:*debug-mbws-calls*
   #:clear-cache #:forget-cached
   #:url-to-object #:format-time-period #:artist-credit-string
   #:shortened-string

   ;; MB Objects & accessors
   #:mb-object
     #:relations-of-type #:page #:table-name #:moniker
   #:artist
     #:id #:name #:sort-name #:disambiguation #:artist-type
     #:country #:life-span #:aliases #:release-groups
   #:release-group
     #:id #:rg-type #:title #:first-release-date
     #:artist-credit #:release-list
   #:release
     #:id #:title #:disambiguation #:status #:quality #:packaging
     #:text-representation #:artist-credit #:release-group #:date
     #:country #:asin #:barcode #:label-info #:medium-list #:recordings
   #:recording
     #:id #:title #:disambiguation #:recording-length #:artist-credit
     #:release-list
   #:label
     #:id #:name #:sort-name #:disambiguation #:label-code
     #:label-type #:country #:life-span #:aliases
   #:work
     #:id #:work-type #:title #:disambiguation #:aliases

   ;; Auxiliary objects & accessors
   #:name-credit
     #:name-credit-string #:artist #:name #:join-phrase
     #:make-name-credit
   #:alias
     #:locale
   #:time-period
     #:begin #:end
   #:partial-list
     #:size #:pl-nth #:pl-as-array #:pl-as-list
     #:pl-find-if #:pl-every
   #:artist-credit
     #:name-credits #:make-artist-credit
   #:track
     #:title #:pos #:track-length #:recording #:artist-credit
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
     #:beginning #:end #:target #:make-relation
   #:attribute-list
     #:attributes
   #:date
     #:year #:month #:day #:date-string #:date= #:parse-date-string

   ;; Slightly higher level accessors
   #:tracks))
