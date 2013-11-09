(ns stupid.core
  (:require [clj-http.client :as client]
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
                  ;"login.x" "21"
                  ;"login.y" "13"
                  }})))


(defn login [address username password]
  (-> (login-site address)
      (login-to-studip username password)))