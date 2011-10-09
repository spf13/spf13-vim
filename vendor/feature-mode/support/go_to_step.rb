require 'rubygems'
gem "ruby_parser", "~> 2.0"
require 'ruby_parser'

class Step
  attr_reader :file, :line, :regexp
  def initialize(regexp, file, line)
    @file, @line = file, line
    self.regexp = regexp
  end

  def regexp=(value)
    @regexp =
      case value
      when String
        pieces, regexp = [], value.dup
        regexp.gsub!(/\$\w+/) { |match| pieces << match; "TOKEN" }
        regexp = Regexp.escape(regexp)
        regexp.gsub!(/TOKEN/) { |match| "(.*)" }
        Regexp.new("^#{regexp}$")
      when Regexp
        value
      else
        STDERR.puts "Warning: invalid parameter to Given/When/Then on #{file}:#{line}.  Expected Regexp or String, got #{value.class} #{value.inspect}"
        Regexp.new(/^INVALID PARAM$/)
      end
  end

  def match?(text)
    @regexp.match(text)
  end
end

class StepParser
  def self.parser
    @parser ||= RubyParser.new
  end

  attr_accessor :steps, :file

  def initialize(file)
    @file = file
    @steps = []
    extract_steps(self.class.parser.parse(File.read(file)))
  end

  def extract_steps(sexp)
    return unless sexp.is_a?(Sexp)
    case sexp.first
    when :block
      sexp[1..-1].each do |child_sexp|
        extract_steps(child_sexp)
      end
    when :iter
      child_sexp = sexp[1]
      return unless child_sexp[0] == :call && [:When, :Then, :Given].include?(child_sexp[2])
      regexp = child_sexp[3][1] && child_sexp[3][1][1]
      @steps << Step.new(regexp, file, child_sexp.line)
    else
      sexp.each do |child_sexp|
        extract_steps(child_sexp)
      end
    end
  end
end

input_text = ARGV[0].strip.gsub(/(When|Then|Given|And) */, "")

files = Dir["features/step_definitions/**/*_steps.rb"]
steps = []
files.each do |file|
  steps.concat(StepParser.new(file).steps)
end

steps.each do |step|
  if step.match?(input_text)
    puts "#{step.file}:#{step.line}"
  end
end
