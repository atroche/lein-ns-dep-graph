(ns leiningen.ns-dep-graph
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.track :as ns-track]
            [clojure.tools.namespace.find :as ns-find]
            [clojure.tools.namespace.parse :as parse]
            [clojure.tools.namespace.dependency :as ns-dep]
            [rhizome.viz :as viz])
  (:import (java.io PushbackReader)))

(defn read-file-ns-decl
  "Attempts to read a (ns ...) declaration from file, and returns the
  unevaluated form. Returns nil if read fails due to invalid syntax or
  if a ns declaration cannot be found. read-opts is passed through to
  tools.reader/read."
  ([file]
   (read-file-ns-decl file nil))
  ([file read-opts]
   (with-open [rdr (PushbackReader. (io/reader file))]
     (try (parse/read-ns-decl rdr read-opts)
          (catch Exception e (println "Exception while parsing ns decl:" e))))))

(defn ns-dep-graph
  "Create a namespace dependency graph and save it as ns-dep-graph.png."
  [project & [platform-kw]]
  (let [platform (case (read-string platform-kw)
                   :clj ns-find/clj
                   :cljs ns-find/cljs
                   ns-find/clj)
        source-files (apply set/union
                            (map (comp #(ns-find/find-sources-in-dir % platform)
                                       io/file)
                                 (project :source-paths)))
        tracker (ns-file/add-files {} source-files)
        dep-graph (tracker ::ns-track/deps)
        ns-names (set (map (comp second read-file-ns-decl)
                           source-files))
        part-of-project? (partial contains? ns-names)
        nodes (filter part-of-project? (ns-dep/nodes dep-graph))]
    (viz/save-graph
     nodes
     #(filter part-of-project? (ns-dep/immediate-dependencies dep-graph %))
     :node->descriptor (fn [x] {:label x})
     :options {:dpi 72}
     :filename "ns-dep-graph.png")))

;; TODO: make output filename configurable.

;; TODO: do not overwrite existing PNG file.

;; TODO: maybe add option to show dependencies on external namespaces as well.
