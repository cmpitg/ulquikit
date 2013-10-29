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

  def render_file(path,
                  template_path=DEFAULT_TEMPLATE_PATH,
                  rd=@default_renderer)
    templates = File.read_file "#{template_path}"
    content = File.read_file "#{path}.md"

    # Strip and capture variable part
    vars_str, prerendered_content = String.strip_vars content

    content = rd.render prerendered_content
    toc     = @toc_renderer.render prerendered_content

    File.open("../build/#{path}.html", 'w') { |file|
      file.write templates % {
        :title    => "",
        :content  => content,
        :toc      => toc,
        :css      => @css_list,
        :js       => @js_list,
      }.merge(parse_vars vars_str)
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

  def create_built_file(file, dest)
    puts "Creating ../build/#{dest}"
    FileUtils.cp file, "../build/#{dest}"
  end

  def get_css_destination_path(filename)
    "#{BUILD_CSS_DIR}/#{filename}"
  end

  def get_js_destination_path(filename)
    "#{BUILD_JS_DIR}/#{filename}"
  end

  def build_css
    build_assets(:extension                => '.css',
                 :source_path              => BOOTSTRAP_CSS_DIR,
                 :get_destination_path_fn  => method(:get_css_destination_path),
                 :tag_format               => CSS_TAG)
  end

  def build_js
    build_assets(:extension                => '.js',
                 :source_path              => BOOTSTRAP_JS_DIR,
                 :get_destination_path_fn  => method(:get_js_destination_path),
                 :tag_format               => JS_TAG)
  end

  # Public: Building assets by copying all assets files from the bootstrap/
  # dirs to BUILD_DOCS_DIR
  def build_assets(args)
    extension                = args[:extension]
    src_path                 = args[:source_path]
    get_destination_path_fn  = args[:get_destination_path_fn]
    tag_format               = args[:tag_format]

    return "" if !File.exists?(src_path)

    result = []

    Find.find(src_path) { |file|
      if file.end_with? extension
        filename    = File.basename file

        destination = get_destination_path_fn.call filename
        source      = "#{src_path}/#{filename}"

        result << tag_format % { :src => destination }

        create_asset filename, source, destination
      end
    }

    result.join "\n"
  end
end
