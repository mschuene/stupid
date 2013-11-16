(ns stupid.core
  (:require [clj-http.client :as client]
            [clojure.zip :as zip]
            [hickory.zip :as hick]
            [me.raynes.laser :as l]))

(alter-var-root #'clj-http.core/*cookie-store* (fn [_] (clj-http.cookies/cookie-store)))

(defn get-login-ticket [login-site]
  (-> (l/select login-site (l/attr= :name "login_ticket"))
      first
      :attrs
      :value))


(defn parse-response [resp]
  (l/parse (:body resp)))

(defn login-site [url-string]
  (parse-response (client/get url-string)))

(defn login-to-studip [login-site username password]
  (->
   (client/post "https://elearning.uni-bremen.de"
                {:form-params 
                 {"login_ticket" (get-login-ticket login-site)
                  "challenge" ""
                  "response" ""
                  "resolution" "1366x768"
                  "loginname" username
                  "password" password
                  }})))


(defn login [address username password]
  (-> (login-site address)
      (login-to-studip username password)))

(defn get-course-site
  ([] (get-course-site "https://elearning.uni-bremen.de"))
  ([base-address] (->
                   (client/get (str base-address "/meine_seminare.php"))
                   parse-response)))


;;To chain selections use select-locs instead of select

(defn get-seminar-table [course-site]
  (first (l/select-locs course-site (l/id= "my_seminars"))))

(defn is-blank-line [tr]
  (not
   (empty? (l/select-locs (hick/hickory-zip tr)
                          (l/and (l/element= :td)
                                 (l/attr= :class "blank")
                                 (l/attr= :colspan "5"))))))

(defn is-semester-marker [tr]
  (not
   (empty? (l/select-locs (hick/hickory-zip tr) (l/and (l/element= :a)
                                    (l/attr= :title "Gruppierung �ffnen"))))))

(defn get-rows [sem-table]
  (->> (l/select-locs sem-table (l/element= :tbody))
      first
      first
      :content
      (remove string?)
      (remove is-blank-line)
      #_(l/select-locs (l/element= :tr))))

(defn spit-fragment [name frag]
  (spit (str name ".html") (l/to-html frag)))

(defn get-last-semester [rows]
  (second (partition-by is-semester-marker rows)))

(defn simplify-name [s]
  (apply str (remove #{\space \tab \newline} s)))


(defn get-name-and-id [row]
  (let [td (nth (remove string? (:content row)) 2)]
    (->> (l/select (hick/hickory-zip td) (l/element= :a))
        first
        ((juxt (comp simplify-name first :content) (comp :href :attrs))))))

(defn build-course-map [seminar-rows]
  (into {} (map get-name-and-id seminar-rows)))

(defn get-course-link [course-map course-name]
  (get course-map (simplify-name course-name)))