#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# textmate_import.rb --- import textmate snippets
# 
# Copyright (C) 2009 Rob Christie, 2010 João Távora
# 
# This is a quick script to generate YASnippets from TextMate Snippets.
#
# I based the script off of a python script of a similar nature by
# Jeff Wheeler: http://nokrev.com
# http://code.nokrev.com/?p=snippet-copier.git;a=blob_plain;f=snippet_copier.py
#
# Use textmate_import.rb --help to get usage information.

require 'rubygems'
require 'plist'
require 'trollop'
require 'fileutils'
require 'shellwords' # String#shellescape
require 'ruby-debug' if $DEBUG


opts = Trollop::options do
  opt :bundle_dir, "TextMate bundle directory", :short => '-d', :type => :string
  opt :output_dir, "Output directory", :short => '-o', :type => :string
  opt :glob, "Specific snippet file (or glob) inside <bundle_dir>", :short => '-g', :default => '*.{tmSnippet,tmCommand,plist,tmMacro}'
  opt :pretty, 'Pretty prints multiple snippets when printing to standard out', :short => '-p'
  opt :quiet, "Be quiet", :short => '-q'
  opt :plist_file, "Use a specific plist file to derive menu information from", :type => :string
end
Trollop::die :bundle_dir, "must be provided" unless opts.bundle_dir
Trollop::die :bundle_dir, "must exist" unless File.directory? opts.bundle_dir

Trollop::die :output_dir, "must be provided" unless opts.output_dir
Trollop::die :output_dir, "must exist" unless File.directory? opts.output_dir

Trollop::die :plist_file, "must exist" if opts.plist_file && File.directory?(opts.plist_file)


# Represents and is capable of outputting the representation of a
# TextMate menu in terms of `yas/define-menu'
#
class TmSubmenu

  @@excluded_items = [];
  def self.excluded_items; @@excluded_items; end
  
  attr_reader :items, :name
  def initialize(name, hash)
    @items = hash["items"]
    @name = name
  end

  def to_lisp(allsubmenus,
              deleteditems,
              indent = 0,
              thingy = ["(", ")"])
    
    first = true;

   string = ""
   separator_useless = true;
   items.each do |uuid|
      if deleteditems && deleteditems.index(uuid)
       $stderr.puts "#{uuid} has been deleted!"
       next
     end
      string += "\n"
      string += " " * indent
      string += (first ? thingy[0] : (" " * thingy[0].length))

      submenu = allsubmenus[uuid]
      snippet = TmSnippet::snippets_by_uid[uuid]
      unimplemented = TmSnippet::unknown_substitutions["content"][uuid]
      if submenu
        str = "(yas/submenu "
        string += str + "\"" + submenu.name + "\"" 
        string += submenu.to_lisp(allsubmenus, deleteditems,
                                  indent + str.length + thingy[0].length)
      elsif snippet and not unimplemented
        string += ";; " + snippet.name + "\n"
        string += " " * (indent + thingy[0].length)
        string += "(yas/item \"" + uuid + "\")"
        separator_useless = false;
      elsif snippet and unimplemented  
        string += ";; Ignoring " + snippet.name + "\n"
        string += " " * (indent + thingy[0].length)
        string += "(yas/ignore-item \"" + uuid + "\")"
        separator_useless = true;
      elsif (uuid =~ /---------------------/)
        string += "(yas/separator)" unless separator_useless
      end
      first = false;
    end
    string += ")"
    string += thingy[1]

    return string
  end

  def self.main_menu_to_lisp (parsed_plist, modename)
    mainmenu = parsed_plist["mainMenu"]
    deleted  = parsed_plist["deleted"]
    
    root = TmSubmenu.new("__main_menu__", mainmenu)
    all = {}
    
    mainmenu["submenus"].each_pair do |k,v|
      all[k] = TmSubmenu.new(v["name"], v)
    end

    excluded = (mainmenu["excludedItems"] || []) + TmSubmenu::excluded_items
    closing = "\n                    '("
    closing+= excluded.collect do |uuid|
      "\"" + uuid + "\"" 
    end.join(  "\n                       ") + "))"

    str = "(yas/define-menu "
    return str + "'#{modename}" + root.to_lisp(all,
                                               deleted,
                                               str.length,
                                               ["'(" , closing])
  end
end


# Represents a textmate snippet
#
# - @file is the .tmsnippet/.plist file path relative to cwd
# 
# - optional @info is a Plist.parsed info.plist found in the bundle dir
#
# - @@snippets_by_uid is where one can find all the snippets parsed so
#   far.
# 
# 
class SkipSnippet < RuntimeError; end
class TmSnippet
  @@known_substitutions = {
    "content"   => {
      "${TM_RAILS_TEMPLATE_START_RUBY_EXPR}"   => "<%= ",
      "${TM_RAILS_TEMPLATE_END_RUBY_EXPR}"     => " %>",
      "${TM_RAILS_TEMPLATE_START_RUBY_INLINE}" => "<% ",
      "${TM_RAILS_TEMPLATE_END_RUBY_INLINE}"   => " -%>",
      "${TM_RAILS_TEMPLATE_END_RUBY_BLOCK}"    => "end" ,
      "${0:$TM_SELECTED_TEXT}"                 => "${0:`yas/selected-text`}",
      /\$\{(\d+)\}/                            => "$\\1",
      "${1:$TM_SELECTED_TEXT}"                 => "${1:`yas/selected-text`}",
      "${2:$TM_SELECTED_TEXT}"                 => "${2:`yas/selected-text`}",
      '$TM_SELECTED_TEXT'                     => "`yas/selected-text`",
      %r'\$\{TM_SELECTED_TEXT:([^\}]*)\}'       => "`(or (yas/selected-text) \"\\1\")`",
      %r'`[^`]+\n[^`]`'                        => Proc.new {|uuid, match| "(yas/multi-line-unknown " + uuid + ")"}},
    "condition" => {
      /^source\..*$/ => "" },
    "binding"   => {},
    "type"      => {}
  }
  
  def self.extra_substitutions; @@extra_substitutions; end
  @@extra_substitutions = {
    "content"   => {},
    "condition" => {},
    "binding"   => {},
    "type"      => {}
  }
  
  def self.unknown_substitutions; @@unknown_substitutions; end
  @@unknown_substitutions = {
    "content"   => {},
    "condition" => {},
    "binding"   => {},
    "type"      => {}
  }

  @@snippets_by_uid={}
  def self.snippets_by_uid; @@snippets_by_uid; end

  def initialize(file,info=nil)
    @file    = file
    @info    = info
    @snippet = TmSnippet::read_plist(file)
    @@snippets_by_uid[self.uuid] = self;
    raise SkipSnippet.new "not a snippet/command/macro." unless (@snippet["scope"] || @snippet["command"])
    raise SkipSnippet.new "looks like preferences."if @file =~ /Preferences\//
    raise RuntimeError.new("Cannot convert this snippet #{file}!") unless @snippet;
  end

  def name
    @snippet["name"]
  end

  def uuid
    @snippet["uuid"]
  end

  def key
    @snippet["tabTrigger"]
  end

  def condition
    yas_directive "condition"
  end

  def type
    override = yas_directive "type"
    if override
      return override
    else
      return "# type: command\n" if @file =~ /(Commands\/|Macros\/)/
    end
  end

  def binding
    yas_directive "binding"
  end

  def content
    known = @@known_substitutions["content"]
    extra = @@extra_substitutions["content"]
    if direct = extra[uuid]
      return direct
    else
      ct = @snippet["content"]
      if ct
        known.each_pair do |k,v|
          if v.respond_to? :call
            ct.gsub!(k) {|match| v.call(uuid, match)}
          else
            ct.gsub!(k,v)
          end
        end
        extra.each_pair do |k,v|
          ct.gsub!(k,v)
        end
        # the remaining stuff is an unknown substitution
        # 
        [ %r'\$\{ [^/\}\{:]* / [^/]* / [^/]* / [^\}]*\}'x ,
          %r'\$\{[^\d][^}]+\}',
          %r'`[^`]+`',
          %r'\$TM_[\w_]+',
          %r'\(yas/multi-line-unknown [^\)]*\)'
        ].each do |reg|
          ct.scan(reg) do |match|
            @@unknown_substitutions["content"][match] = self
          end
        end
        return ct
      else
        @@unknown_substitutions["content"][uuid] = self
        TmSubmenu::excluded_items.push(uuid)
        return "(yas/unimplemented)"
      end
    end
  end

  def to_yas
    doc = "# -*- mode: snippet -*-\n"
    doc << (self.type || "")
    doc << "# uuid: #{self.uuid}\n"
    doc << "# key: #{self.key}\n" if self.key
    doc << "# contributor: Translated from textmate snippet by PROGRAM_NAME\n"
    doc << "# name: #{self.name}\n"
    doc << (self.binding || "")
    doc << (self.condition || "")
    doc << "# --\n"
    doc << (self.content || "(yas/unimplemented)")
    doc
  end

  def self.canonicalize(filename)
    invalid_char = /[^ a-z_0-9.+=~(){}\/'`&#,-]/i

    filename.
      gsub(invalid_char, '').  # remove invalid characters
      gsub(/ {2,}/,' ').       # squeeze repeated spaces into a single one
      rstrip                   # remove trailing whitespaces
  end

  def yas_file()
      File.join(TmSnippet::canonicalize(@file[0, @file.length-File.extname(@file).length]) + ".yasnippet")
  end

  def self.read_plist(xml_or_binary)
    begin
      parsed = Plist::parse_xml(xml_or_binary)
      return parsed if parsed
      raise ArgumentError.new "Probably in binary format and parse_xml is very quiet..."
    rescue StandardError => e
      if (system "plutil -convert xml1 #{xml_or_binary.shellescape} -o /tmp/textmate_import.tmpxml")
        return Plist::parse_xml("/tmp/textmate_import.tmpxml") 
      else
        raise RuntimeError.new "plutil failed miserably, check if you have it..."
      end
    end
  end

  private

  @@yas_to_tm_directives = {"condition" => "scope", "binding" => "keyEquivalent", "key" => "tabTrigger"}
  def yas_directive(yas_directive)
    #
    # Merge "known" hardcoded substitution with "extra" substitutions
    # provided in the .yas-setup.el file.
    # 
    merged = @@known_substitutions[yas_directive].
      merge(@@extra_substitutions[yas_directive])
    #
    # First look for an uuid-based direct substitution for this
    # directive.
    #
    if direct = merged[uuid]
      return "# #{yas_directive}: "+ direct + "\n" unless direct.empty?
    else
      tm_directive = @@yas_to_tm_directives[yas_directive]
      val = tm_directive && @snippet[tm_directive]
      if val and !val.delete(" ").empty? then
        #
        # Sort merged substitutions by length (bigger ones first,
        # regexps last), and apply them to the value gotten for plist.
        #
        allsubs = merged.sort_by do |what, with|
          if what.respond_to? :length then -what.length else 0 end
        end
        allsubs.each do |sub|
          if val.gsub!(sub[0],sub[1])
            # puts "SUBBED #{sub[0]} for #{sub[1]}"
            return "# #{yas_directive}: "+ val + "\n" unless val.empty?
          end
        end
        #
        # If we get here, no substitution matched, so mark this an
        # unknown substitution.
        #
        @@unknown_substitutions[yas_directive][val] = self
        return "## #{yas_directive}: \""+ val + "\n"
      end
    end
  end

end


if __FILE__ == $PROGRAM_NAME
  # Read the the bundle's info.plist if can find it/guess it
  #
  info_plist_file = opts.plist_file || File.join(opts.bundle_dir,"info.plist")
  info_plist = TmSnippet::read_plist(info_plist_file) if info_plist_file and File.readable? info_plist_file;

  # Calculate the mode name
  # 
  modename = File.basename opts.output_dir || "major-mode-name"

  # Read in .yas-setup.el looking for the separator between auto-generated
  #
  original_dir = Dir.pwd
  yas_setup_el_file = File.join(original_dir, opts.output_dir, ".yas-setup.el")
  separator = ";; --**--"
  whole, head , tail = "", "", ""
  if File::exists? yas_setup_el_file
    File.open yas_setup_el_file, 'r' do |file|
      whole = file.read
      head , tail = whole.split(separator)
    end
  else
    head = ";; .yas-setup.el for #{modename}\n" + ";; \n"
  end

  # Now iterate the tail part to find extra substitutions
  #
  tail    ||= ""
  head    ||= ""
  directive = nil
  # puts "get this head #{head}"
  head.each_line do |line|
    case line
    when /^;; Substitutions for:(.*)$/
      directive = $~[1].strip
      # puts "found the directove #{directive}"
    when /^;;(.*)[ ]+=yyas>(.*)$/
      replacewith = $~[2].strip
      lookfor = $~[1]
      lookfor.gsub!(/^[ ]*/, "")
      lookfor.gsub!(/[ ]*$/, "")
      # puts "found this wonderful substitution for #{directive} which is #{lookfor} => #{replacewith}"
      unless !directive or replacewith =~ /yas\/unknown/ then 
        TmSnippet.extra_substitutions[directive][lookfor] = replacewith
      end
    end
  end

  # Glob snippets into snippet_files, going into subdirs
  #
  Dir.chdir opts.bundle_dir
  snippet_files_glob = File.join("**", opts.glob)
  snippet_files = Dir.glob(snippet_files_glob)

  # Attempt to convert each snippet files in snippet_files
  #  
  puts "Will try to convert #{snippet_files.length} snippets...\n" unless opts.quiet
  

  # Iterate the globbed files
  #
  snippet_files.each do |file|
    begin
      $stdout.print "Processing \"#{File.join(opts.bundle_dir,file)}\"..." unless opts.quiet
      snippet = TmSnippet.new(file,info_plist)

      file_to_create = File.join(original_dir, opts.output_dir, snippet.yas_file)
      FileUtils.mkdir_p(File.dirname(file_to_create))
      File.open(file_to_create, 'w') do |f|
        f.write(snippet.to_yas)
      end
      $stdout.print "done\n" unless opts.quiet
    rescue SkipSnippet => e
      $stdout.print "skipped! #{e.message}\n" unless opts.quiet
    rescue RuntimeError => e
      $stderr.print "failed! #{e.message}\n"
      $strerr.print "#{e.backtrace.join("\n")}" unless opts.quiet
    end
  end

  # Attempt to decypher the menu
  #
  menustr = TmSubmenu::main_menu_to_lisp(info_plist, modename) if info_plist
  puts menustr if $DEBUG

  # Write some basic .yas-* files
  #
  if opts.output_dir
    FileUtils.mkdir_p opts.output_dir
    FileUtils.touch File.join(original_dir, opts.output_dir, ".yas-make-groups") unless menustr
    
    # Now, output head + a new tail in (possibly new) .yas-setup.el
    # file
    #
    File.open yas_setup_el_file, 'w' do |file|
      file.puts head
      file.puts separator
      file.puts ";; Automatically generated code, do not edit this part"
      file.puts ";; "
      file.puts ";; Translated menu"
      file.puts ";; "
      file.puts menustr
      file.puts
      file.puts ";; Unknown substitutions"
      file.puts ";; "
      ["content", "condition", "binding"].each do |type|
        file.puts ";; Substitutions for: #{type}"
        file.puts ";; "
        # TmSnippet::extra_substitutions[type].
        #   each_pair do |k,v|
        #   file.puts ";; " + k + "" + (" " * [1, 90-k.length].max) + " =yyas> " + v
        # end
        unknown = TmSnippet::unknown_substitutions[type];
        unknown.keys.uniq.each do |k|
          file.puts ";; # as in " +  unknown[k].yas_file
          file.puts ";; " + k + "" + (" " * [1, 90-k.length].max) + " =yyas> (yas/unknown)"
          file.puts ";; "
        end
        file.puts ";; "
        file.puts
      end
      file.puts ";; .yas-setup.el for #{modename} ends here"
    end
  end
end
