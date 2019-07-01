(ns arduino-comms.communication
  (:require [udp-wrapper.core :as u]
            [cheshire.core :as j]
            [clojure.string :as str])
  (:import (java.util Scanner)))

(defonce cached-messages (atom []))
(defonce print-log (atom []))
(defonce current-accumulated-message (atom nil))
(defonce socket (u/create-udp-server 8888))

(defn classify [msg]
  (condp #(str/starts-with? %2 %1) msg
    ":error" :error
    ":" :var-set
    :default))

(defn parse-var-set [message]
  (map (fn [s] (try
                 (read-string s)
                 (catch Exception e
                   s)))
       (str/split message #" " 2)))

(defn pre-process-messages [message-packet]
  (reduce (fn [acc str-msg]
            (if (= :var-set (classify str-msg))
              (let [[header value] (parse-var-set str-msg)]
                (assoc-in acc [:custom-values header] value))
              acc)) message-packet (:msgs message-packet)))

(defn push-message [msg]
  (let [current @current-accumulated-message
        processed (pre-process-messages msg)
        log-view (concat @print-log (:msgs msg))
        acc-msg (assoc processed :time-posted (System/currentTimeMillis)
                                 :msgs log-view
                                 :custom-values (merge
                                                  (:custom-values current)
                                                  (:custom-values processed)))]
    (spit "logcat.txt" (str (str/join "\n" (:msgs msg)) "\n") :append true)
    (spit "full-data.txt" (str processed "\r\n") :append true)
    (reset! current-accumulated-message acc-msg)
    (swap! cached-messages #(conj % processed))
    (reset! print-log log-view)))

(defn push-test-msg [msg]
  (push-message {:msgs [msg]}))

(defn start []
      (defonce receive-loop
               (u/receive-loop
                 socket
                 (u/empty-packet 30000)
                 (fn [pkt] (push-message (j/parse-string pkt keyword))))))




(comment (def socket (u/create-udp-server 8888))

         (def exit nil)

         (def reader #^Scanner (Scanner. System/in))
         (def console-loop (future (while true (let [line (.nextLine reader)
                                                     pkt (u/packet (u/get-bytes-utf8 line)
                                                                   (u/make-address "10.1.10.194")
                                                                   8888)]
                                                 (u/send-message socket pkt)
                                                 (println "Sending line:" line (= line "Exit"))
                                                 (if (= line "Exit") (exit))))))

         (def receive-loop (u/receive-loop socket (u/empty-packet 30000) (partial println "Received packet: ")))

         (defn exit []
           (future-cancel console-loop)
           (future-cancel receive-loop)
           (u/close-udp-server socket)))