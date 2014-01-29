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
require 'pathname'

require_relative 'utils'

class String
  def self.has_variable_path?(s)
    s.start_with? "---\n"
  end

  def self.strip_vars(s)
    return {}, s if not String.has_variable_path?(s)

    var_part = s[/^---\n(.*)^---\n/m][4..-5]
    var_part = var_part[0..-3] if var_part.end_with?("\n\n")

    # Remove the variable part
    return var_part, s[(var_part.length + 8)..-1]
  end
end

class File
  # Write to a file, overwrite if the file exists
  def self.write_file(path, content)
    File.open(path) { |file|
      file.write content
    }
  end

  # Read a file and return its content
  def self.read_file(path)
    res = ""
    File.open(path, 'r') { |file|
      res = file.read
    }
    return res
  end
end

class Dir
  # Ensure a list of directories exists
  def self.ensure_dirs(*dirs)
    dirs.each { |dir|
      if !FileTest.exists? dir
        puts "Creating #{dir}"
        FileUtils.mkdir_p dir
      end
    }
  end
end

module FileUtils
  # Remove paths recursively
  def self.remove_paths(*paths)
    paths.each { |path|
      if FileTest.exists? path
        puts "Removing #{path}"
        self.rmtree path
      end
    }
  end
end

def get_reference_to_source_file(path)
  path = File.expand_path("./#{path}")
  # TODO: Document me
  Pathname.new(path).relative_path_from(Pathname.new(BUILD_SRC_DIR)).to_s
end
