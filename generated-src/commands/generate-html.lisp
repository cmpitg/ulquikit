;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014-2016 Ha-Duong Nguyen <cmpitg AT gmailDOTcom>
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

(in-package #:ulquikit-cmd)

(defcmd generate-html (&key (from "src")
                            (to "docs")
                            (recursive t))
  (declare ((or string pathname list) from)
           ((or string pathname) to))
  (display-cmd "Generating HTML")
  (ulquikit:generate-html :from from :to to :recursive recursive))
