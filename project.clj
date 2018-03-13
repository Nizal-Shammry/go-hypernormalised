(defproject go-hypernormal "0.1.0-SNAPSHOT"
  :description "A Ph.d project to hypernormalise the Gene Ontology"
  :url "https://arxiv.org/abs/1711.07273"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [tawny-bfo/tawny-bfo "0.1.0-SNAPSHOT"]
                   ;; Text hacking only
                 [org.clojure/data.csv "0.1.3"]
                 [net.sourceforge.owlapi/owlapi-distribution "4.2.8"]
                 ;;[uk.org.russet/tawny-owl "1.6.0"]
                 ;;[uk.org.russet/tawny-owl "2.0.0-MAYBE"]
                 [uk.org.russet/tawny-owl "2.0.0-SNAPSHOT"]
                 [tawny-go/tawny-go "0.1.0-SNAPSHOT"]
                 ;;[tawny-chebi/tawny-chebi "1.0.0-SNAPSHOT"]
                 [tawny-chebi "1.0.0"]]
                 )
