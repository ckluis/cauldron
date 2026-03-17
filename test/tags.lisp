;;;; test/tags.lisp — Suite tag registry
;;;; All existing suites default to :unit (no explicit registration needed).
;;;; Only integration suites declared here.

(in-package :cauldron.test)

;;; --- Integration suite tags ---

(tag-suite :http-server-integration :integration :http)

;;; --- DB integration suite tags ---
(tag-suite :db-connection-integration :integration :db)
(tag-suite :db-types-integration :integration :db)
(tag-suite :db-pool-integration :integration :db)
(tag-suite :grimoire-migration-integration :integration :db)
(tag-suite :grimoire-query-integration :integration :db)
(tag-suite :db-listen-notify-integration :integration :db)
(tag-suite :db-schema-integration :integration :db)
