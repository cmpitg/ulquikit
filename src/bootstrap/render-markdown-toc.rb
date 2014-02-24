#!/usr/bin/env ruby

#
# This file is part of Ulquikit project.
#
# Copyright (C) 2013-2014 Duong H. Nguyen <cmpitg AT gmailDOTcom>
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

require 'singleton'

require 'redcarpet'
require 'rouge'
require 'rouge/plugins/redcarpet'

class TOCRendererSingleton
  include Singleton

  RendererOptions = {
    :autolink             => true,
    :space_after_headers  => true,
    :no_intra_emphasis    => true,
    :fenced_code_blocks   => true,
    :tables               => true,
    :highlight            => true,
    :footnotes            => true,
  }

  attr_accessor :html_render, :renderer

  def initialize
    @toc_renderer  = Redcarpet::Markdown.new(Redcarpet::Render::HTML_TOC, RendererOptions)
  end

  def render_document
    content = ARGF.read
    puts @toc_renderer.render content
  end
end

TOCRendererSingleton.instance.render_document
