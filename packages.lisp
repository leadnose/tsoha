(defpackage #:tsoha-main
  (:documentation
   "Contains the entry point for the application.")
  (:use #:cl)
  (:nicknames #:tsoha)
  (:export #:start
           #:stop))


(defpackage #:tsoha-db
  (:use #:cl)
  (:nicknames #:db)
  (:export #:with-connection
           #:with-new-connection
           #:with-transaction
           #:with-connection-and-transaction)
  (:documentation
   "Contains macros to establish connections and transactions, and
  class-definitions that map to the db-model. Does NOT contain queries."))


(defpackage #:tsoha-queries
  (:documentation
   "Contains queries that either search or modify the database. DB
  shouldn't be accessed from any other package.")
  (:use #:cl)
  (:nicknames #:queries)
  (:export #:add-recipe
           #:unit-list
           #:recipe-count
           #:search-recipe-by-name
           #:newest-recipes
           #:recipe-details))

(defpackage #:tsoha-pages
  (:documentation
   "Contains the request-handler functions that actually respond.")
  (:use #:cl)
  (:nicknames #:pages)
  (:export #:front-page
           #:add-recipe
           #:receive-add-recipe
           #:recipe-by-id
           #:recipe-search
           #:recipe-search-results))




