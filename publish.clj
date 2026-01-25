#!/usr/bin/env bb

(require '[babashka.process :as process]
         '[babashka.fs :as fs]
         '[clojure.string :as str])

(defn org-to-html [filename]
  """Convert org file to html with provided template and style"""
  (let [contents (:out
                  (process/shell {:out :string}
                                 (str "pandoc "
                                      "--standalone "
                                      "--template=static/template.html "
                                      "--from org "
                                      "--to html "
                                      "--toc=true --toc-depth=3 "
                                      "--include-before-body=static/preamble.html "
                                      "--include-after-body=static/postamble.html "
                                      "--number-sections "
                                      "--css=static/style.css "
                                      filename)))]
    (str contents)))

(defn extract-org-metadata [file-path]
  (let [content (slurp file-path)
        lines (str/split-lines content)]
    (->> lines
         (take-while #(or (re-matches #"^\#\+.*" %)
                          (str/blank? %)))
         (remove str/blank?)
         (keep (fn [line]
                 (when-let [[_ k v] (re-matches #"^\#\+(\w+):\s*(.+)" line)]
                   [(keyword (str/lower-case k)) v])))
         (into {}))))

(defn process [folder]
  """Converts a folder of org files to html.
     Returns list of metadata hashes for each processed file."""
  (let [file-paths (map str (fs/glob folder "**{.org}"))]
    (doall
     (for [file-path file-paths]
       (do
        (let* [file-name (fs/file-name file-path)
               target-file (str/replace file-name #"org$" "html")
               contents (org-to-html file-path)]
          (spit target-file contents))
        (extract-org-metadata file-path))))))

;;; Main
;; Convert posts
(doall (map println (process "posts")))
;; Generate Index
;; Generate Tag files and Taglist

(defn -main [args])
;; Execute
;(apply -main *command-line-args*)
