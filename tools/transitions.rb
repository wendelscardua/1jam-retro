#!/usr/bin/ruby
# frozen_string_literal: true

@last_bad_count = 32
@bad_count = 32
@cycles = {}
@bad_cycles = {}

def bad_pair(from, to, transitions:, nudge: false, nudge_chance: 0.0)
  return true if from == to
  return true if @bad_cycles[from]
  from_bits = ['1'] + from.split(//) + ['1']
  to_bits = ['1'] + to.split(//) + ['1']
  to_2 = transitions[to]
  to_2_bits = ['1'] + to_2.split(//) + ['1']
  to_3 = transitions[to_2]
  to_3_bits = ['1'] + to_3.split(//) + ['1']

  (1..5).each do |index|
    if from_bits[index] == '0'
      # ensures there's no 4 zeros in a single row
      return true if to_bits[index] == '0' && to_2_bits[index] == '0' && to_3_bits[index] == '0'
      # ensures if there wasn't a barrier on a position
      # then there won't be a barrier on all 3 positions in front of it
      if to_bits[index - 1] == '1' &&
         to_bits[index] == '1' &&
         to_bits[index + 1] == '1'
        return true
      end
    end
  end

  if nudge && @bad_count == @last_bad_count && @bad_count.positive? && rand < nudge_chance
    return true
  end

  false
end

def count_bads(transitions)
  transitions.map { |k, v| bad_pair(k, v, transitions: transitions) }.select { |x| x }.count
end

def shuffle_bads(transitions, nudge_chance)
  bad_keys = transitions.map { |k, v| [k, bad_pair(k, v, transitions: transitions, nudge: true, nudge_chance: nudge_chance)] }
                        .select { |_k, bp| bp }
                        .map(&:first)
  values = bad_keys.map { |key| transitions[key] }.shuffle
  bad_keys.zip(values).each { |k, v| transitions[k] = v }
end

def detect_cycles(transitions)
  @cycles = {}
  @bad_cycles = {}
  cycle_counter = 1
  transitions.keys.each do |key|
    next if @cycles[key]
    @cycles[key] = cycle_counter
    loop do
      key = transitions[key]
      break if @cycles[key]
      @cycles[key] = cycle_counter
    end
    cycle_counter += 1
  end
  if cycle_counter > 2
    @bad_cycles = @cycles.map { |k, v| [k, v] }.uniq { |k, v| v }.to_h
  end
end

def main(iterations = 1000, nudge_chance = 0.2)
  patterns = (1..30).map { |n| format '%<pattern>05d', pattern: n.to_s(2) }

  transitions = patterns.zip(patterns.shuffle).to_h

  iterations.times do |iter|
    puts "Iteration #{iter + 1}"
    detect_cycles(transitions)
    @bad_count = count_bads(transitions)
    puts "Bads: #{@bad_count}"
    puts "Bad cycles: #{@bad_cycles.count}"
    break if @bad_count.zero?

    shuffle_bads(transitions, nudge_chance)
    @last_bad_count = @bad_count
  end

  transitions.map { |k, v| [k, v] }.sort_by(&:first).each do |k, v|
    badness = bad_pair(k, v, transitions: transitions) ? ' (bad)' : ''
    puts "        .byte %#{v} ; from #{k}#{badness}"
  end
end

main(ARGV[0]&.to_i, ARGV[1]&.to_f)
