#!/usr/bin/env python

from collections import defaultdict

states = ('Rainy', 'Sunny')
 
observations = ('walk', 'shop', 'clean')
 
start_probability = {'Rainy': 0.6, 'Sunny': 0.4}
 
transition_probability = {
   'Rainy' : {'Rainy': 0.7, 'Sunny': 0.3},
   'Sunny' : {'Rainy': 0.4, 'Sunny': 0.6},
   }
 
emission_probability = {
   'Rainy' : {'walk': 0.1, 'shop': 0.4, 'clean': 0.5},
   'Sunny' : {'walk': 0.6, 'shop': 0.3, 'clean': 0.1},
   }

def zero(): return 0.0

tags_to_words = { "NN" : defaultdict(zero, {"time": 7.0727,
                                            "arrow": 0.0215}),
                  "VB" : defaultdict(zero, {"time": 0.0005,
                                      "like": 2.8413}),
                  "JJ" : defaultdict(zero, {"time": 0.0}),
                  "VBZ": defaultdict(zero, {"flies": 0.4754}),
                  "NNS" : defaultdict(zero, {"flies": 0.1610}),
                  "IN" : defaultdict(zero, {"like": 2.6512}),
                  "RB" : defaultdict(zero, {"like": 0.5086}),
                  "DT": defaultdict(zero, {"an": 1.4192}),
                  "E": defaultdict(zero, {})}

# dictionary from previous tag to a list of (current, prob) pairs. All prob
# values are given as percentages, so divide by 100.
tags_to_tags = {
  "S": defaultdict(zero,
        { "NN": 0.6823,
         "VB": 0.5294,
         "JJ": 0.8033 }),
  "NN": defaultdict(zero,
         {"VBZ":3.9005,
         "NNS":1.6076,
         "E":0.2069}),
  "VB" : defaultdict(zero,
          {"VBZ": 0.0566,
          "NNS": 0.6566,
          "DT": 15.2649}),
  "JJ" : defaultdict(zero,
         {"VBZ": 2.0934,
         "NNS": 2.4383}),
  "VBZ": defaultdict(zero,
          {"IN": 8.5862,
          "VB": 0.7002,
          "RB": 15.0350}),
  "NNS": defaultdict(zero,
          {"IN": 21.8302,
          "VB": 11.1406,
          "RB":6.4721}),
  "IN" : defaultdict(zero, {"DT": 31.4263}),
  "RB" : defaultdict(zero, {"DT": 5.3113}), 
  "DT": defaultdict(zero, {"NN": 38.0170}),
  "E": defaultdict(zero, {})
}

def forward_viterbi(obs, states, start_p, trans_p, emit_p):
  T = {}
  for state in states:
    ##          prob.           V. path  V. prob.
    T[state] = (start_p[state], [state], start_p[state])
  for output in obs:
    U = {}
    for next_state in states:
      total = 0
      argmax = None
      valmax = 0
      for source_state in states:
        (prob, v_path, v_prob) = T[source_state]

        p = ( (emit_p[source_state][output] / 100.0) *
              (trans_p[source_state][next_state] / 100.0) )
        prob *= p
        v_prob *= p
        total += prob
        if v_prob > valmax:
          argmax = v_path + [next_state]
          valmax = v_prob
      U[next_state] = (total, argmax, valmax)
    T = U
  ## apply sum/max to the final states:
  total = 0
  argmax = None
  valmax = 0
  for state in states:
    (prob, v_path, v_prob) = T[state]
    total += prob
    if v_prob > valmax:
      argmax = v_path
      valmax = v_prob
  return (total, argmax, valmax)

def example():
  return forward_viterbi(observations,
                         states,
                         start_probability,
                         transition_probability,
                         emission_probability)

def tag():
  alltags = tags_to_tags.keys()
  alltags.remove("S")
  alltags.append("E")
  # print alltags
  return forward_viterbi("time flies like an arrow".split(),
                         alltags,
                         tags_to_tags["S"],
                         tags_to_tags,
                         tags_to_words)

def main():
    print example()
    print tag()

if __name__ == "__main__": main()
