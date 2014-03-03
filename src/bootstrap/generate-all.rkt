#!/usr/bin/env racket

;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014 Duong H. Nguyen <cmpitg AT gmailDOTcom>
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
;; You should have received a copy of the GNU General Public License along with
;; Ulquikit.  If not, see <http://www.gnu.org/licenses/>.
;;

#lang rackjure

(require (rename-in "generate-resources.rkt"
                    [main generate/resources])
         (rename-in "generate-docs.rkt"
                    [main generate/docs])
         (rename-in "generate-code.rkt"
                    [main generate/code]))

(module+ main
  (displayln "--- Generating resources ---")
  (generate/resources)
  (newline)

  (displayln "--- Generating docs ---")
  (generate/docs)
  (newline)

  (displayln "--- Generating code ---")
  (generate/code)
  (newline))
