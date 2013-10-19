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


require 'redcarpet'
require 'pygments'
require 'find'

require_relative 'utils'
require_relative 'markdown_renderer_config'

class HTMLWithPygments < Redcarpet::Render::HTML
  def block_code(code, language)
    Pygments.highlight(code, :lexer => language)
  end
end

module Renderer
  @@html_render = HTMLWithPygments.new(@@markdown_extensions)

  @@default_renderer = Redcarpet::Markdown.new(@@html_render, @@renderer_options)

  @@default_template = File.expand_path "templates/main.html"

  @@css_source = "styles"
  @@js_source  = "scripts"

  @@css_dest   = "css"
  @@js_dest    = "js"

  @@css_tag = "<link rel='stylesheet' type='text/css' href='%{src}' />"
  @@js_tag  = "<script type='text/javascript' src='%{src}'></script>"

  def self.read_file(path)
    res = ""
    File.open(path, 'r') { |file|
      res = file.read
    }
    return res
  end

  def self.render_file(path,
                       template_path=@@default_template,
                       rd=@@default_renderer)
    templates = read_file "#{template_path}"
    contents = read_file "#{path}.md"

    # Strip and capture variable part
    vars, contents = String.strip_vars contents

    File.open("#{path}.html", 'w') { |file|
      file.write templates % {
        :title => "",
        :contents => rd.render(contents),
        :js => "",
        :css => get_css,
      }
    }
  end

  def self.create_file(file, dest)
    puts "Creating ../build/#{dest}"
    FileUtils.cp file, "../build/#{dest}"
  end

  def self.css_dest(filename)
    "#{@@css_dest}/#{filename}"
  end

  def self.get_css(path=@@css_source)
    result = []
    Find.find(path) { |file|
      if file.end_with? '.css'
        filename = File.basename file
        dest = css_dest filename
        result << @@css_tag % { :src => dest }
        create_file file, dest
      end
    }
  end
end
