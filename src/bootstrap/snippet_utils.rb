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

require_relative "config"

# Public: get snippet from the current line.
#
# line                    - current line
# :snippets               - current collected snippets
# :current_snippet_name   - current snippet name
# :snippet_spaces_length  - current padding space at the beginning
#
# Returns a hash containing the last 3 arguments for further processing.
#
def get_snippet (line, opts)
  extract_snippet_from_doc(SNIPPET_DEF_REGEXP[:begin],
                          SNIPPET_DEF_REGEXP[:end],
                          line,
                          opts)
end

def extract_snippet_from_doc(regexp_begin,
                             regexp_end,
                             line,
                             opts)
  snippets               = opts[:snippets] || {}
  current_snippet_name   = opts[:current_snippet_name] || nil
  snippet_spaces_length  = opts[:snippet_spaces_length] || 0

  case
  when regexp_end =~ line
    current_snippet_name = nil

  when regexp_begin =~ line
    current_snippet_name   = regexp_begin.match(line)[1].to_sym
    redundant_spaces       = /^( +)[^ ]/.match(line)
    snippet_spaces_length  = redundant_spaces[1].length if redundant_spaces

    snippets[current_snippet_name] = []
  when current_snippet_name
    snippets[current_snippet_name] << (line[snippet_spaces_length..-1])
  end

  {
    :snippets               => snippets,
    :current_snippet_name   => current_snippet_name,
    :snippet_spaces_length  => snippet_spaces_length,
  }
end
