(ns stupid.core
  (:import [java.text SimpleDateFormat])
  (:require [clj-http.client :as client]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hickory.zip :as hick]
            [me.raynes.laser :as l])
  (:gen-class))

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


(defn login
  ([] (let [conf (edn/read-string (slurp "/home/kima/programming/conf.clj"))]
        (login (:address conf) (:username conf) (:password conf))))
  ([address username password]
     (-> (login-site address)
         (login-to-studip username password))))

(defn get-courses-site
  ([] (get-courses-site "https://elearning.uni-bremen.de"))
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
  (into {} (filter #(.startsWith (second %) "seminar_main")
                   (map get-name-and-id seminar-rows))))

(defn get-course-link [course-map course-name]
  (get course-map (simplify-name course-name)))

(defn get-course-map [course-site]
  (-> (get-seminar-table course-site)
      get-rows
      get-last-semester
      build-course-map))

  

#_(defn get-course-file-site [course-link]
  (client/get (str "https://elearning.uni-bremen.de/" course-link "&redirect_to=folder.php&cmd=all")))


(defn get-course-site
  ([course-link] (get-course-site "https://elearning.uni-bremen.de" course-link))
  ([base-address course-link]
     (parse-response
      (client/get (str base-address "/" course-link)))))


(defn get-course-file-link [course-site]
  (-> (l/select course-site (l/id= "nav_course__files"))
      first
      hickory.zip/hickory-zip 
      (l/select (l/element= :a))
      first
      :attrs
      :href))


(defn get-course-file-site
  ([course-file-link] (get-course-file-site "https://elearning.uni-bremen.de"
                                            course-file-link))
  ([base-address course-file-link]
     (-> (client/get (str base-address "/" course-file-link))
         parse-response)))


(defn get-all-files-link [file-site]
  (-> (l/select file-site (l/id= "nav_course__files__all"))
      first
      hickory.zip/hickory-zip 
      (l/select (l/element= :a))
      first
      :attrs
      :href))

(defn get-file-site [cm course]
  (-> (get-course-link cm course)
      get-course-site
      get-course-file-link
      get-course-file-site
      get-all-files-link
      get-course-file-site))


(defn write-byte-array [filename byte-array]
  (with-open [w (io/output-stream filename)]
    (.write w byte-array)))

;;TODO get the files in the file-table with name, link and date and
;;download the newest files

(def date-format (SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss zzz"))

(defn get-last-modified-date [file-link]
  (.parse date-format
          (-> file-link
              client/head
              :headers
              (get "last-modified"))))

(defn get-file-links [file-site]
  (->> (l/select file-site (l/and 
                            (l/class= "extern")
                            (l/element= :a)))
       (map (comp :href :attrs))))


(defn get-file-details [file-link]
  (let [details (->> (.split file-link "&")
                     rest
                     (map #(.split % "="))
                     (map vec)
                     #_(#(do (prn %) %))
                     (into {}))]
    (assoc details
      "file_link" file-link
      "last_modified" (get-last-modified-date file-link))))

(defn get-file-details-from-site [file-site]
  (->> file-site
       get-file-links
       (map get-file-details)))

(defn download-file
  ([file-details] (download-file file-details ""))
  ([file-details path]
     (let [file-link (get file-details "file_link")
           _ (prn "Download " file-link  "to" path)
           _ (spit "/home/kima/programming/log.txt" (str (java.util.Date.)
                                  "Download " file-link "to " path "\n")
                   :append true)]
       (->> (client/get file-link {:as :byte-array})
            :body
            (write-byte-array (str path (get file-details "file_name")))))))


(defn not-too-new [lm date]
  (let [diff (- (.getTime lm) (.getTime date))]
    (prn "diff " diff (< 1500 diff))
    (< 1500 diff)))

(defn download-all-new-files
  ([file-details mark-date] (download-all-new-files
                             file-details mark-date ""))
  ([file-details mark-date path]
     (doall
      (for [fd file-details
            :let [last-modified (get fd "last_modified")
                  date (java.util.Date.)]
            :when (and (> (compare last-modified mark-date) 0)
                       (not-too-new last-modified date))]
        (download-file fd path)))))
  
(def date-read-format (SimpleDateFormat. "dd.MM.yy HH:mm"))


(defn logout []
  (client/get "https://elearning.uni-bremen.de/logout.php"))


(def last-checked (.parse date-read-format "17.11.13 14:47"))

(def cm {"Vorlesung:03-123Analysis3" "seminar_main.php?auswahl=3360b5b395969b6bc4b03462852fbaaf", "Vorlesung:03-145KompaktkursSpectralClustering" "seminar_main.php?auswahl=449e598b27d8b872eeabb65605d21be9", "Vorlesung:01-01-EP1-1Experimentalphysik1(Mechanik)" "seminar_main.php?auswahl=d3e378638165236d276f2d4adcd2ae26",  "Vorlesung:03-131Numerik1" "seminar_main.php?auswahl=c6d0eff0c7232a3ad4332abc6a904b40", "Vorlesung:03-BA-601.01TheoretischeInformatik1:EndlicheAutomatenundformaleSprachen" "seminar_main.php?auswahl=a652c887ec0554fc03410a0add0d3933", "Vorlesung:03-BA-700.03PraktischeInformatik3:FunktionaleProgrammierung" "seminar_main.php?auswahl=e30b633fd4c63f9d5fa3ddf1b3c70033"})

(def folders
  {"Vorlesung:03-123Analysis3" "/home/kima/Dropbox/uni/Analysis/", "Vorlesung:03-145KompaktkursSpectralClustering" "/home/kima/Dropbox/uni/SpectralClustering/", "Vorlesung:01-01-EP1-1Experimentalphysik1(Mechanik)" "/home/kima/Dropbox/uni/experimentalphysik/" "Vorlesung:03-131Numerik1" "/home/kima/Dropbox/uni/numerik/", "Vorlesung:03-BA-601.01TheoretischeInformatik1:EndlicheAutomatenundformaleSprachen" "/home/kima/Dropbox/uni/theo/"  "Vorlesung:03-BA-700.03PraktischeInformatik3:FunktionaleProgrammierung" "/home/kima/Dropbox/uni/pi3/"})

(comment
  (def li (login "https://elearning.uni-bremen.de" "....." "xxxxxxx"))
  (def courses-site (get-courses-site))
  (def cm (get-course-map course-sie)))

(defn download-files-from
  ([cm folders mark-date]
  (doall (for [[course course-link] cm
               :let [fs (get-file-site cm course)
                     file-details (get-file-details-from-site fs)]]
           (download-all-new-files file-details mark-date (get folders course))))))
               

(defn download-files []
  (let [conf (edn/read-string (slurp "/home/kima/programming/conf.clj"))]
    (spit "/home/kima/programming/log.txt" (str (java.util.Date.)
                                                "check-for-updates \n")
                                                :append true)
    (download-files-from cm folders (:last-checked conf))))

(defn write-new-last-checked []
  (let [conf (edn/read-string (slurp "/home/kima/programming/conf.clj"))]
    (spit "/home/kima/programming/conf.clj" (pr-str
                                             (assoc conf :last-checked
                                                    (java.util.Date.))))))

(defn main [& args]
  (login)
  (download-files)
  (write-new-last-checked))

(defn -main [& args]
  (login)
  (download-files)
  (write-new-last-checked))