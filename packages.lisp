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
   "Contains queries that either search or modify the database. The database
shouldn't be accessed from any other package than this.

The functions beginning with FIND- returns only one object, whereas functions
beginning with SEARCH- return a list of objects.")
  (:use #:cl)
  (:nicknames #:queries)
  (:export #:add-recipe
           #:find-recipe-by-id
           #:unit-list
           #:recipe-count
           #:search-recipe-by-name
           #:search-recipe-by-ingredient
           #:search-recipe-by-instructions
           #:search-recipe-generic
           #:newest-recipes
           #:recipe-details
           #:find-ingredient
           #:search-ingredient-by-name))

(defpackage #:tsoha-pages
  (:documentation
   "Contains the request-handler functions that actually respond to requests.")
  (:use #:cl)
  (:nicknames #:pages)
  (:export #:front-page
           #:add-recipe
           #:receive-add-recipe
           #:recipe-by-id
           #:recipe-search
           #:recipe-search-results
           #:ingredient-search
           #:ingredient-search-results
           #:ingredient-by-name))




