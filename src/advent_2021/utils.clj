(ns advent-2021.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-file [file-path]
  (str/split-lines (slurp (io/resource file-path))))
