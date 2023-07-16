(ns test-check-playground.pure-functions
  (:refer-clojure :exclude [reverse min])
  (:require [clojure.string :as str]))

(defn reverse [ls]
  (into () ls))

(defn sum [nums]
  (reduce + 0 nums))

