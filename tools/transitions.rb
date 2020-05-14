#!/usr/bin/ruby
# frozen_string_literal: true

@last_bad_count = 32
@bad_count = 32

def bad_pair(from, to, nudge = false)
  return true if from == to

  from_bits = ['1'] + from.split(//) + ['1']
  to_bits = ['1'] + to.split(//) + ['1']
  (1..5).each do |index|
    # ensures if there wasn't a barrier on a position...
    next unless from_bits[index] == '0'
    # .. then there won't be a barrier on all 3 positions in front of it
    if to_bits[index - 1] == '1' &&
       to_bits[index] == '1' &&
       to_bits[index + 1] == '1'
      return true
    end
  end

  if nudge && @bad_count == @last_bad_count && @bad_count.positive? && rand < 0.2
    return true
  end

  false
end

def count_bads(transitions)
  transitions.map { |k, v| bad_pair(k, v) }.select { |x| x }.count
end

def shuffle_bads(transitions)
  bad_keys = transitions.map { |k, v| [k, bad_pair(k, v, true)] }
                        .select { |_k, bp| bp }
                        .map(&:first)
  values = bad_keys.map { |key| transitions[key] }.shuffle
  bad_keys.zip(values).each { |k, v| transitions[k] = v }
end

def main(iterations)
  patterns = (1..30).map { |n| format '%<pattern>05d', pattern: n.to_s(2) }

  transitions = patterns.map { |x| [x, x] }.to_h

  iterations.times do |iter|
    puts "Iteration #{iter + 1}"
    @bad_count = count_bads(transitions)
    puts "Bads: #{@bad_count}"
    break if @bad_count.zero?

    shuffle_bads(transitions)
    @last_bad_count = @bad_count
  end

  transitions.map { |k, v| [k, v] }.sort_by(&:first).each do |k, v|
    badness = bad_pair(k, v) ? ' (bad)' : ''
    puts "        .byte %#{v} ; from #{k}#{badness}"
  end
end

main(ARGV[0].to_i)
