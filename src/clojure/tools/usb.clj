(ns clojure.tools.usb
  (:import [javax.usb UsbHostManager UsbConst])
  (:require [clojure.core.async :as async :refer [go chan <! >! <!! >!!]]))

(defn unsigned-byte<-byte 
  [_byte]
  (if (bit-test _byte 7)
    (+ (inc (int _byte)) 0xFF)
    (int _byte)))

(defn unsigned-bytes->bytes
  [unsigned-bytes-list]
  (byte-array (map unsigned-byte->byte unsigned-bytes-list)))

(defn unsigned-bytes<-bytes
  [bytes-list]
  (map unsigned-byte<-byte bytes-list))

(defn bytes->hex-string-list
  [bytes-list]
  (map #(format "0x%02X" %)
       (unsigned-bytes<-bytes bytes-list)))

(defn bytes<-hex-string-list
  [hex-list]
  (unsigned-bytes->bytes (map #(read-string %)
                              hex-list)))

(defn services
  []
  (UsbHostManager/getUsbServices))

(defn root
  []
  (.getRootUsbHub (services)))

(defn devices
  ([] (devices (.getAttachedUsbDevices (root))))
  ([attached-usb-devices]
   (lazy-seq
     (when-let [device (first attached-usb-devices)]
       (if (.isUsbHub device)
         (devices (lazy-cat (rest attached-usb-devices)
                            (.getAttachedUsbDevices device)))
         (cons device (devices (rest attached-usb-devices))))))))

(defprotocol Describable
  (describe [o]))

(extend-protocol Describable
  javax.usb.UsbDevice
  (describe [o] (.getUsbDeviceDescriptor o))
  
  javax.usb.UsbConfiguration
  (describe [o] (.getUsbConfigurationDescriptor o))
  
  javax.usb.UsbInterface
  (describe [o] (.getUsbInterfaceDescriptor o))
  
  javax.usb.UsbEndpoint
  (describe [o] (.getUsbEndpointDescriptor o)))

(defn device 
  [device-id]
  (first (filter #(.contains (str %) device-id)
                 (devices))))

(defn configuration
  [device]
  (.getActiveUsbConfiguration device))

(defn interface
  [device num]
  (let [config (configuration device)]
    (.getUsbInterface config (unsigned-byte->byte num))))

(defn pipe 
  [iface endpoint-num]
  (.getUsbPipe (.getUsbEndpoint iface
                                (unsigned-byte->byte endpoint-num))))

(defn listen 
  [in-pipe]
  (let [err-chan (chan)
        event-chan (chan)
        listener (proxy [javax.usb.event.UsbPipeListener] []
                   (errorEventOccurred [event]
                                       (>!! err-chan (.getUsbException event)))
                   (dataEventOccurred [event]
                                      (>!! event-chan (.getData event))))]
    (.addUsbPipeListener in-pipe listener)
    {:err-chan err-chan, :event-chan event-chan}))

(defn unsigned-byte->byte 
  [unsigned-byte]
  (unchecked-byte (if (bit-test unsigned-byte 7)
                    (dec (- unsigned-byte 0xFF))
                    unsigned-byte)))

(defn print-devices
  []
  (doseq [child (devices)]
    (println (describe child))))

(defn print-endpoints
  [iface]
  (doseq [child (.getUsbEndpoints iface)]
    (println (describe child))))

