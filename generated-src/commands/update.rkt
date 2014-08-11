;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014 Nguyễn Hà Dương <cmpitg AT gmailDOTcom>
;;
;; Ulquikit is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; Ulquikit is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with Ulquikit.  If not, see <http://www.gnu.org/licenses/>.
;;

#lang rackjure

(current-curly-dict hash)

(require net/url)

(require "../ulquikit.rkt")
(require "../command-core.rkt")
(require "../utils/path.rkt")

(provide run)

(require racket/runtime-path)
(define-runtime-path +this-directory+ ".")

(define +latest-release-url+
  (string->url "https://github.com/cmpitg/ulquikit/releases/latest"))

(define (get-latest-version)
  (~> (call/input-url +latest-release-url+
                      head-impure-port
                      port->string)
    string-split
    (dropf #λ(not (string=? "Location:" %)))
    second
    (#λ(regexp-match #rx"v(.*)" %))
    second))


(define (construct-download-url [version (get-latest-version)])
  (format "https://github.com/cmpitg/ulquikit/releases/download/v~a/ulquikit-v~a.zip"
          version
          version))

(define (download-and-unzip version to-dir)
  (parameterize ([current-directory to-dir])
    (let ([url              (construct-download-url version)]
          [filename         (format "ulquikit-v~a.zip" version)]
          [out-buffer-mode  (file-stream-buffer-mode (current-output-port))]
          [err-buffer-mode  (file-stream-buffer-mode (current-error-port))])

      (with-handlers ([exn:fail?
                       (λ (_)
                         (file-stream-buffer-mode (current-output-port)
                                                  out-buffer-mode)
                         (file-stream-buffer-mode (current-error-port)
                                                  err-buffer-mode))])
        (file-stream-buffer-mode (current-output-port) 'none)
        (file-stream-buffer-mode (current-error-port) 'none)

        (displayln (str "-> Downloading from " url))
        (system (str "curl -O " url))

        (displayln (str "-> Unzipping " filename ", replacing old version with new version"))
        (system (str "unzip -o " filename))

        (displayln (str "-> Removing " filename))
        (delete-directory/files filename)

        (file-stream-buffer-mode (current-output-port) out-buffer-mode)
        (file-stream-buffer-mode (current-error-port) err-buffer-mode)))))


(define (run)
  (display-command "Updating Ulquikit")
  (displayln (str "-> Current version: " +ulquikit-version+))
  (let ([latest-version (get-latest-version)]
        [ulquikit-dir   (get-path +this-directory+ "../")])
    (displayln (str "   Latest version:  " latest-version))
    (cond [(string=? latest-version +ulquikit-version+)
           (newline)
           (displayln (str "   Congratulations! You are running latest version of Ulquikit!"))]
          [else
           (download-and-unzip latest-version ulquikit-dir)])))
