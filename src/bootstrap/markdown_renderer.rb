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

require_relative 'utils'

class HTMLWithPygments < Redcarpet::Render::HTML
  def block_code(code, language)
    Pygments.highlight(code, :lexer => language)
  end
end

module Renderer
  @@markdown_extensions = {
    :with_toc_data => true,
    :prettify => true
  }

  @@renderer_options = {
    :autolink => true,
    :space_after_headers => true,
    :no_intra_emphasis => true,
    :fenced_code_blocks => true,
    :tables => true,
    :highlight => true,
    :footnotes => true
  }

  @@html_render = HTMLWithPygments.new(@@markdown_extensions)

  @@default_renderer = Redcarpet::Markdown.new(@@html_render, @@renderer_options)

  def self.read_file(path)
    res = ""
    File.open(path, 'r') { |file|
      res = file.read
    }
    return res
  end

  def self.render_file(path, rd=@@default_renderer)
    contents = read_file("#{path}.md")

    # Strip and capture variable part
    vars, contents = String.strip_vars contents

    File.open("#{path}.html", 'w') { |file|
      file.write rd.render(contents)
    }
  end
end
