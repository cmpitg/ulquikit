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
require 'singleton'

require_relative 'utils'
require_relative 'markdown_renderer_config'

class HTMLWithPygments < Redcarpet::Render::HTML
  def block_code(code, language)
    Pygments.highlight(code, :lexer => language)
  end
end

class RendererSingleton
  include Singleton

  def initialize
    @html_render = HTMLWithPygments.new(MarkdownExtensions)
    @default_renderer = Redcarpet::Markdown.new(@html_render, RendererOptions)
  end

  def render_file(path,
                  template_path=DefaultTemplate,
                  rd=@default_renderer)
    templates = File.read_file "#{template_path}"
    contents = File.read_file "#{path}.md"

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

  def create_file(file, dest)
    puts "Creating ../build/#{dest}"
    FileUtils.cp file, "../build/#{dest}"
  end

  def css_dest(filename)
    "#{CSSDestDir}/#{filename}"
  end

  def get_css(path=CSSSourceDir)
    result = []
    Find.find(path) { |file|
      if file.end_with? '.css'
        filename = File.basename file
        dest = css_dest filename
        result << CSSTag % { :src => dest }
        create_file file, dest
      end
    }
  end
end

Renderer = RendererSingleton.instance
