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
require_relative 'config'
require_relative 'markdown_renderer_config'

class HTMLWithPygments < Redcarpet::Render::HTML
  def block_code(code, language)
    Pygments.highlight code, :lexer => language
  end
end

class RendererSingleton
  include Singleton

  attr_accessor :html_render, :default_renderer, :css_list, :js_list

  def initialize
    @html_render       = HTMLWithPygments.new MarkdownExtensions

    @default_renderer  = Redcarpet::Markdown.new(@html_render,
                                                 RendererOptions)

    @toc_renderer      = Redcarpet::Markdown.new(Redcarpet::Render::HTML_TOC,
                                                 RendererOptions)

    @css_list  = build_css
    @js_list   = build_js
  end

  def render_file(file_basename,
                  rd = @default_renderer)
    template          = File.read_file "#{BOOTSTRAP_TEMPLATE_DIR}/main.html"
    markdown_content  = File.read_file "#{file_basename}.md"

    # Strip and capture variable part
    vars_str, prerendered_content = String.strip_vars markdown_content

    content = rd.render prerendered_content
    toc     = @toc_renderer.render prerendered_content
    vars    = parse_vars vars_str

    File.open("#{BUILD_DOCS_DIR}/#{file_basename}.html", 'w') { |file|
      file.write template % {
        :title    => "",
        :content  => content,
        :toc      => toc,
        :css      => @css_list,
        :js       => @js_list,
      }.merge(vars)
    }
  end

  # Public: Parsing section that declares variables at the beginning of the
  # markdown file, returning a Ruby hash.
  #
  # E.g.
  #
  #   ---
  #   project_name: Foobar
  #   authors: The Grr Quux, Friends
  #   title: Foobar Full Source Code
  #   short_description: A foo project, with literate programming as its enlightenment
  #   version: 0.1.1
  #   ---
  #
  # Would become:
  #
  #   {
  #     :project_name => "Foobar",
  #     :authors => ["The Grr Quux", "Friends"],
  #     :title => "Foobar Full Source Code",
  #     :short_description => "A foo project, with literate programming as its enlightenment",
  #     :version => "0.1.1"
  #   }
  #
  def parse_vars(vars_str)
    # TODO: parsing array (authors)
    result = {}
    vars_str.each_line { |line|
      key, val = line.split ':', 2
      result[key.to_sym] = val
    }
    result
  end

  # Public: Copying one asset file from a bootstrap/ dir BUILD_DOCS_DIR
  def create_asset(filename, source, destination)
    puts "Creating #{filename} for build_docs/"
    FileUtils.cp source, destination
  end

  def build_css
    build_assets(:extension         => '.css',
                 :source_path       => BOOTSTRAP_CSS_DIR,
                 :destination_path  => BUILD_CSS_DIR,
                 :tag_format        => CSS_TAG)
  end

  def build_js
    build_assets(:extension         => '.js',
                 :source_path       => BOOTSTRAP_JS_DIR,
                 :destination_path  => BUILD_JS_DIR,
                 :tag_format        => JS_TAG)
  end

  # Public: Building assets by copying all assets files from the bootstrap/
  # dirs to BUILD_DOCS_DIR
  def build_assets(args)
    extension         = args[:extension]
    src_path          = args[:source_path]
    destination_path  = args[:destination_path]
    tag_format        = args[:tag_format]

    return "" if !File.exists?(src_path)

    result = []

    Find.find(src_path) { |file|
      if file.end_with? extension
        filename    = File.basename file

        destination = "#{destination_path}/#{filename}"
        source      = "#{src_path}/#{filename}"

        result << tag_format % { :src => destination }

        create_asset filename, source, destination
      end
    }

    result.join "\n"
  end
end
