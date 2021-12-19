(ns user
  (:require [nextjournal.clerk :as clerk]))

;; start Clerk's buit-in webserver on the default port 7777, opening the browser when done
(clerk/serve! {:browse? true})

;; either call `clerk/show!` explicitly
(clerk/show! "src/advent_2021/day16_clerk.clj")

;; or let Clerk watch the given `:paths` for changes
#_(clerk/serve! {:watch-paths ["src/advent_2021/clerk"]})

;; start with watcher and show filter function to enable notebook pinning
(clerk/serve! {:watch-paths ["src/advent_2021"]})
