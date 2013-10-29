#
# This file is part of Ulquikit project.
#
# Copyright (C) 2013 Duong H. Nguyen <cmpitg AT gmailDOTcom>
#
# Ulquikit is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# Ulquikit is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# Ulquikit.  If not, see <http://www.gnu.org/licenses/>.
#

require 'pygments'

BOOTSTRAP_DIR = File.dirname File.expand_path(__FILE__)
PROJECT_DIR   = File.expand_path "#{BOOTSTRAP_DIR}../../../"

BOOTSTRAP_TEMPLATE_DIR  = "#{BOOTSTRAP_DIR}/templates"
BOOTSTRAP_CSS_DIR       = "#{BOOTSTRAP_DIR}/css"
BOOTSTRAP_JS_DIR        = "#{BOOTSTRAP_DIR}/js"

BUILD_SRC_DIR   = "#{PROJECT_DIR}/build_src"
BUILD_DOCS_DIR  = "#{PROJECT_DIR}/build_docs"
BUILD_CSS_DIR   = "#{BUILD_DOCS_DIR}/css"
BUILD_JS_DIR    = "#{BUILD_DOCS_DIR}/js"

ASSETS = [{
            :source       => BOOTSTRAP_CSS_DIR,
            :destination  => BUILD_CSS_DIR,
          }]

CSS_TAG = "<link rel='stylesheet' type='text/css' href='%{src}' />"
JS_TAG  = "<script type='text/javascript' src='%{src}'></script>"
