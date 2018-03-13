(ns go-hypernormal.go-hyper
  (:refer-clojure :exclude [binding])
  (:require
   [clojure.string :as str]
   ;; [go-hypernormal.chemical :as che]
   [tawny-chebi.chebi :as chebi]
   [tawny.owl :refer :all]
   [tawny.lookup]
   [tawny.bfo.bfo-1-1 :as bfo]
   [tawny.pattern :as p]))

(defontology geneontology
  :iri "http://www.example/go-hypernormalised"
  :noname true)

;; #+BEGIN_SRC clojure
(owl-import bfo/bfo-1-1)
;; (owl-import che/chemical)
(owl-import chebi/chebi)
;; #+END_SRC

;; Top Level
(defclass nProcess)
(defclass RealizableEntity)

(defoproperty hasFunction
  :range RealizableEntity)

(defoproperty hasRole)

(defoproperty realisedIn
  :domain RealizableEntity
  :range nProcess)

(defn realised-in [process-name process]
  (owl-class
   (str "To" process-name)
   :super
   RealizableEntity
   (owl-some realisedIn process)))

(defoproperty inheresIn
  :domain RealizableEntity)

(defoproperty hasParticipant)

(defoproperty hasInput
  :domain nProcess)

(defoproperty hasOutput
  :domain nProcess)

(defoproperty isPartOf)

;; fetch all the subclasses. Chebi isn't going to change and this is much,
;; much faster.
(def ^:private chemical-entity-subclasses
  (subclasses chebi/chebi chebi/chemical_entity))

(def ^:private chemical-role-subclasses
  (subclasses chebi/chebi chebi/chemical_role))

;; What chemicalize function do exactly? 
(defn chemicalize [chemical-or-chemical-role-or-cellular-component]
  (cond
    (chemical-entity-subclasses  chemical-or-chemical-role-or-cellular-component)
    chemical-or-chemical-role-or-cellular-component
    (chemical-role-subclasses    chemical-or-chemical-role-or-cellular-component)
    (owl-some hasRole chemical-or-chemical-role-or-cellular-component)
    ;; This is not a good default -- we should check for GO CC
    true
    (owl-some isPartOf chemical-or-chemical-role-or-cellular-component)))

;; Just some preliminaries. var-get-maybe is copied from owl.clj which is a bad sign.

;; #+begin_src clojure
(defn stem [name]
  (if (some
       identity
       (map
        #(str/ends-with? name %)
        ["e"]))
    (str/join (drop-last name))
    name))

;; Copy from owl.clj!
(defn- var-get-maybe
  "Given a var return it's value, given a value return the value."
  [var-maybe]
  (if (var? var-maybe)
    (var-get var-maybe)
    var-maybe))
;; #+end_src


;; The expectation is that largely this ontology is going to consist of two
;; parallel hierarchies -- a function (or realizable entity) and the process in
;; which that function is realised. We have used the word "activity" here to
;; describe this pair.

;; #+begin_src clojure
(defn subactivity
  [activity-name comment
   [super-process super-realizable]]
  (let [o geneontology
        process
        (p/p owl-class o
             (str
              (stem activity-name) "ing")
             :super (var-get-maybe super-process)
             :comment (str "The Process for: "
                           comment))
        realizable
        (p/p owl-class o
             (str
              "To" activity-name)
             :super (var-get-maybe super-realizable)
             (owl-some realisedIn process)
             :comment (str "The RealizableEntity for: "
                           comment))]
      (p/pattern-annotator
       o
       (list
        process
        realizable))))

(defmacro defsubactivity
  [activity-name comment super-pair]
  (tawny.pattern/pattern-generator
   'subactivity
   (list (name activity-name)
         comment super-pair)))

(defn activity
  [activity-name comment]
  (subactivity
   activity-name comment
   [nProcess RealizableEntity]))

(defmacro defactivity
  [activity-name comment]
  `(def ~activity-name
     ~(tawny.pattern/pattern-generator
       'activity
       (list (name activity-name)
           comment))))
;; #+end_src


;; * Middle Level Ontology

;; Here, we start out medium level. In the first instance, many of these
;; activities have no logical differention from each other, although should try
;; to increase this over time.

;; ** Binding

;; #+begin_src clojure
(defactivity Bind
  "To interact tightly with another entity, longer than
   transiently, such that separating the entity requires significant energy.
   ToBind functions are often transitive; A has a function ToBind B, then vice
   versa is also true.")

(defoproperty hasLigand
  :super hasInput
  :range chebi/chemical_entity)

;; has_participant (and che/Chemical (some hasRole che/Ligand))
(refine Binding
        :super (owl-some hasLigand chebi/chemical_entity))

(defn- binding-to-chemical
  ([[super-binding-process
     super-binding-realizable]
    chemical-name chemical]
   (let [p
         (owl-class
          (str chemical-name "BindingProcess")
          :equivalent
          (owl-and super-binding-process
                   (object-some hasLigand chemical)))]
     [p
      (owl-class
       (str "To" chemical-name "Bind")
       :equivalent
       (owl-and super-binding-realizable
                (object-some realisedIn p)))])))

(defn chemical-name [entity]
  ;; chop of the namespace
  (str
   (.substring
    (tawny.lookup/resolve-entity entity)
    18)))

(defn binding
  ([chemical-or-chemical-role-or-cellular-component]
   (binding [Binding RealizableEntity]   chemical-or-chemical-role-or-cellular-component))
  
  ([super-binding chemical-or-chemical-role-or-cellular-component]
   (binding-to-chemical
    super-binding
    (chemical-name chemical-or-chemical-role-or-cellular-component)
    (chemicalize
     chemical-or-chemical-role-or-cellular-component))))
;; #+end_src

