#!/usr/bin/env bb

(require '[babashka.process :as process]
         '[babashka.fs :as fs]
         '[clojure.string :as str])

;; Convert org file to html and surround with scaffolding elements
(defn org-to-html [filename]
  (let [header (slurp "static/header.html")
        preamble (slurp "static/preamble.html")
        postamble (slurp "static/postamble.html")
        contents (:out (process/shell {:out :string} (str "pandoc --from org --to html " filename)))]
    (str header preamble contents postamble)))

;; process folder
;; for all org files
;; - generate target filename
;; - convert
;; - write result to target
;; Generate index

(defn process [folder]
  (let [file-paths (map str (fs/glob folder "**{.org}"))]
    (doseq [file-path file-paths]
      (println "Processing: " file-path)
      (let* [file-name (fs/file-name file-path)
             target-file (str/replace file-name #"org$" "test.html")
             contents (org-to-html file-path)]
        (spit target-file contents)))))

(process "posts")

(defn -main [args])
;; Execute
;(apply -main *command-line-args*)
