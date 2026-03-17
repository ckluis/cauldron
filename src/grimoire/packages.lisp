;;;; src/grimoire/packages.lisp — Package definitions for the query builder / ORM layer

(defpackage :cauldron.grimoire
  (:use :cl)
  (:export
   ;; Query builder
   #:make-query
   #:from
   #:select-fields
   #:where-clause
   #:order-by
   #:group-by
   #:having
   #:limit-query
   #:offset-query
   #:join-query
   #:distinct-query
   #:lock-query
   #:to-sql
   ;; Query execution
   #:execute-query
   #:execute-one
   ;; DML builders
   #:insert-into
   #:update-set
   #:delete-from
   ;; Condition DSL operators
   #:qand
   #:qor
   #:qnot
   ;; Aggregates
   #:count-rows
   #:sum-of
   #:avg-of
   ;; Changesets
   #:make-changeset
   #:changeset-valid-p
   #:changeset-errors
   #:changeset-changes
   #:changeset-data
   #:cast
   #:validate-required
   #:validate-length
   #:validate-format
   #:validate-inclusion
   #:validate-number
   #:apply-changeset
   ;; Migrations
   #:defmigration
   #:migrate
   #:rollback
   #:migration-status
   ;; Conditions
   #:query-error
   #:migration-error
   ;; Declarative table definitions (deftable)
   #:deftable
   #:table-spec
   #:make-table-spec
   #:table-spec-name
   #:table-spec-columns
   #:table-spec-indexes
   #:table-spec-seed-sql
   #:column-spec
   #:make-column-spec
   #:column-spec-name
   #:column-spec-field-type
   #:column-spec-required-p
   #:column-spec-default
   #:column-spec-references
   #:find-table-spec
   #:table-name-string
   #:*table-registry*
   #:field-type-to-pg-type
   #:generate-table-ddl
   #:generate-table-metadata-sql
   #:generate-full-table-sql
   ;; Full-text search
   #:add-search-column-ddl
   #:build-search-query
   #:search-records
   #:highlight-match
   #:reindex-table-sql
   #:cross-table-search-sql
   #:generate-search-ddl))
