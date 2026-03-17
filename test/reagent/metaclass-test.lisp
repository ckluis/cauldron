;;;; test/reagent/metaclass-test.lisp — Resource metaclass tests
(in-package :cauldron.test)

(defsuite :reagent-metaclass)

;;; --- pluralize-name (reagent version) ---

(deftest test-reagent-pluralize-user
  (is-equal "users" (cauldron.reagent::pluralize-name :user)))

(deftest test-reagent-pluralize-city
  "Consonant + y → ies."
  (is-equal "cities" (cauldron.reagent::pluralize-name :city)))

(deftest test-reagent-pluralize-key
  "Vowel + y → ys (no ies)."
  (is-equal "keys" (cauldron.reagent::pluralize-name :key)))

(deftest test-reagent-pluralize-bus
  "Ends in s → es."
  (is-equal "buses" (cauldron.reagent::pluralize-name :bus)))

(deftest test-reagent-pluralize-box
  "Ends in x → es."
  (is-equal "boxes" (cauldron.reagent::pluralize-name :box)))

(deftest test-reagent-pluralize-dish
  "Ends in sh → es."
  (is-equal "dishes" (cauldron.reagent::pluralize-name :dish)))

(deftest test-reagent-pluralize-church
  "Ends in ch → es."
  (is-equal "churches" (cauldron.reagent::pluralize-name :church)))

(deftest test-reagent-pluralize-post
  "Regular → s."
  (is-equal "posts" (cauldron.reagent::pluralize-name :post)))

;;; --- Consistency between forge and reagent pluralize-name ---

(deftest test-pluralize-forge-vs-reagent-user
  "Both implementations agree on :user."
  (is-equal (cauldron.forge::pluralize-name :user)
            (cauldron.reagent::pluralize-name :user)))

(deftest test-pluralize-forge-vs-reagent-post
  (is-equal (cauldron.forge::pluralize-name :post)
            (cauldron.reagent::pluralize-name :post)))

;;; Note: The two implementations differ on edge cases:
;;; - forge: any "y" suffix → ies (including "key" → "kies")
;;; - reagent: consonant+y → ies, vowel+y → ys ("key" → "keys")
;;; - forge: "s" suffix → ses (including "address" → "addresses")
;;; - reagent: "s" suffix → ses, plus x/sh/ch → es
;;; This divergence is documented for future consolidation.

(deftest test-pluralize-divergence-documented
  "Forge treats all -y as -ies; reagent checks for consonant+y."
  ;; :key → forge: "kies" vs reagent: "keys"
  ;; Just verify they both produce strings
  (is (stringp (cauldron.forge::pluralize-name :key)))
  (is (stringp (cauldron.reagent::pluralize-name :key))))
