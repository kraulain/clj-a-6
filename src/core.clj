(ns test-check-playground.pure-functions
  (:refer-clojure :exclude [reverse min])
  (:require [clojure.string :as str]))

(defn reverse [ls]
  (into () ls))

(defn sum [nums]
  (reduce + 0 nums))

(defn min [nums]
  (assert (not (empty? nums)))
  (first (sort nums)))

(defn video-id [video]
  (-> video
      :metadata
      :connections
      :comments
      :uri
      (->> (re-find #"videos/(\d+)"))
      second))

(defn strip-query [url]
  (str/replace url #"[?].*" ""))

(declare sync)


(defn sync-store [store]
  {:storeid store
   :upstream []
   :waiting []})

(defn update-some [v k f & args]
  (if (contains? v k)
    (apply update v k f args)
    v))

(defn uuid []
  (str (UUID/randomUUID)))

(def email-re
  #"(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()\[\]\.,;:\s@\"]+\.)+[^<>()\[\]\.,;:\s@\"]{2,})")

(defn valid? [email]
  (boolean (re-matches email-re email)))

(defn save-email! [email]
  {:pre [(valid? email)]}
  true)

;; this won't work
(gen/sample (gen/such-that #(re-matches email-re %) gen/string-ascii))

(def gen-email-char (gen/such-that #(re-matches #"[^<>()\[\]\.,;:\s@\"]" (str %))
                                   gen/char-ascii))

(def gen-email-string (gen/not-empty (gen/fmap str/join (gen/vector gen-email-char))))

(def gen-regular-email-name (gen/fmap
                             (fn [[n1 nn]]
                               (str/join "." (cons n1 nn)))
                             (gen/tuple gen-email-string
                                        (gen/vector gen-email-string))))
(def gen-irregular-email-name (gen/fmap
                               #(str \" % \")
                               (gen/not-empty gen/string-ascii)))

(def gen-email-name (gen/frequency [[10 gen-regular-email-name]
                                    [1 gen-irregular-email-name]]))

(def gen-email-domain (gen/fmap
                       (fn [[hns tld1 tld2]]
                         ;; hns is vector, so add at the end
                        (str/join "." (conj hns (str tld1 tld2))))
                       (gen/tuple (gen/not-empty (gen/vector gen-email-string))
                                  gen-email-string
                                  gen-email-string)))

(def gen-email (gen/fmap
                (fn [[name domain]]
                  (str name
                       "@"
                       domain))
                (gen/tuple gen-email-name
                           gen-email-domain)))

(def gen-email-name2 (gen/elements ["bob"
                                    "suzy"
                                    "john"
                                    "jill"]))
(def gen-email-domain2 (gen/elements ["gmail.com"
                                      "hotmail.com"
                                      "example.com"
                                      "yahoo.com"]))

(def gen-char-digit (gen/elements (vec "0123456789"))) 

(def gen-email2 (gen/fmap
                 (fn [[name n domain]]
                   (str name (str/join n) "@" domain))
                 (gen/tuple gen-email-name2
                            (gen/vector gen-char-digit 0 4)
                            gen-email-domain2)))

(comment
  (gen/sample gen-email-char)
  (gen/sample gen-email-string)
  (gen/sample gen-email2)

  (tc/quick-check 100
                  (prop/for-all [email gen-email2]
                                (re-matches email-re email)))


  )



(comment

  (re-matches email-re "eric@lispcast.com")
  (re-matches email-re "a@dfsfs.cm")

  )

;; score counter

(defn new-counter []
  (atom 0))

(defn increment [counter diff]
  (assert (not (neg? diff)))
  (swap! counter + diff)
  nil)

(defn counter-value [counter]
  @counter)

;; key-value store

(defn new-kv-store []
  (atom {}))

(defn kv-get [store key]
  (get @store key))

(defn kv-put [store key value]
  (swap! store assoc key value)
  nil)

(defn kv-del [store key]
  (swap! store dissoc key)
  nil)

(defn kv-clr [store]
  (reset! store {})
  nil)

(s/def ::age-range (s/and
                    int?
                    (fn [a] (<= 18 a 120))))

(s/def ::age-range2 (s/int-in 18 121))

(s/def ::birth-year (s/inst-in #inst "1980" #inst "1990"))

(s/def ::temp-in-new-orleans (s/double-in :min -11.7 :max 38.9 :NaN? false :infinite? false))

(s/def ::names-with-A (s/and
                       string?
                       (fn [n] (str/starts-with? n "A"))))

(s/def ::names-with-A2 (s/with-gen
                         (s/and
                          string?
                          (fn [n] (str/starts-with? n "A")))
                         (fn []
                           (s/gen #{"Aaron" "Alice" "Amanda" "Allen"}))))

(s/def ::names-with-A3 (s/with-gen
                         (s/and
                          string?
                          (fn [n] (str/starts-with? n "A")))
                         (fn []
                           (sgen/fmap (fn [chars]
                                        (str "A" (str/lower-case (apply str chars))))
                                      (sgen/vector (sgen/char-alpha))))))

;; 1. Use the generator that spec builds for you.
;; 2. Steal the generator from another spec.
;; 3. Construct a generator using clojure.spec.gen.alpha
;; 4. Construct a generator using clojure.test.check.generators

(comment
  (s/valid? ::birth-year #inst "1985")
  (s/valid? ::temp-in-new-orleans 20.0)

  (s/valid? ::names-with-A2 "Albert")

  (str/starts-with? 1 "A")

  (s/conform (s/cat :year ::birth-year) [#inst "1975"])

  )

(s/def ::person-tuple (s/cat :name ::names-with-A3
                             :dob ::birth-year
                             :temp ::temp-in-new-orleans))

(comment

  (s/valid? ::age-range 50)
  ;; => true
  (s/valid? ::age-range 18)
  ;; => true
  (s/valid? ::age-range 10)
  ;; => false

  (s/valid? ::age-range "10")
  ;; => false


  (s/valid? pos? "aa")
  
  )

;; Merge sort implementation

