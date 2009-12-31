#!/usr/bin/env python

"""Implementation of the Forward and Backward algorithms, as well as the
Viterbi algorithm. When we create a trellis describing a particular HMM, we go
ahead and do all three. Because Forward and Viterbi are so similar, we do both
of those in one go."""

class FBNode(object):
    def __init__(self):
        self.forward = 0
        self.backward = 0
        self.viterbi_score = 0
        self.viterbi_path = None

    def __repr__(self):
        return ("(%.6f, %.6f)" % (self.forward,self.backward))

class Trellis(object):
    def __init__(self, observations, transitions, emissions, initial,
        states=None):

        """Builds a trellis as wide as the number of observations, and as tall
        as the number of possible states. It contains forward/backward
        probabilities in each slot.
        
        states is an optional parameter, specifying all the possible states. If
        it's not given, assume that the transition dictionary lists all the
        states as its keys.
        """

        self.timesteps = []
        self.observations = observations
        self.transitions = transitions
        self.emissions = emissions
        self.initial = initial

        if not states:
            self.states = transitions.keys()
        else:
            self.states = states

        for i in range(len(observations)):
            now = {}
            for state in self.states:
                now[state] = FBNode()
            self.timesteps.append(now)

        self.forward_viterbi()
        self.backward()

    ## based on the pseudocode in Russell and Norvig's AIMA (2ed), pg 546;
    ## also on the code on the wikipedia article about the Viterbi algorithm.
    def forward_viterbi(self):
        """Calculates forward variables and does the Viterbi algorithm too."""

        trellis = self.timesteps
        observations = self.observations
        states = self.states
        emissions = self.emissions
        initial = self.initial
        transitions = self.transitions

        # Set the values at timestep 0.
        for state in states:
            stateandemission = ( initial[state] *
                emissions[state][observations[0]] )
            trellis[0][state].forward = stateandemission

            trellis[0][state].viterbi_score = stateandemission
            trellis[0][state].viterbi_path = [state]

        # Now propagate forward.
        for time in range(0, len(observations) - 1):
            obs = observations[time]
            nextobs = observations[time+1]
            for nextstate in states:
                totalprob = 0
                maxpath = None
                maxscore = 0

                for state in states:
                    myalpha = trellis[time][state].forward
                    statetransition = transitions[state][nextstate]
                    emissionprob = emissions[nextstate][nextobs]

                    prob = myalpha * statetransition * emissionprob
                    totalprob += prob

                    v_prob = trellis[time][state].viterbi_score
                    v_prob *= (emissionprob * statetransition)

                    if v_prob > maxscore:
                        v_path = trellis[time][state].viterbi_path
                        maxpath = v_path + [nextstate]
                        maxscore = v_prob

                trellis[time+1][nextstate].forward = totalprob
                trellis[time+1][nextstate].viterbi_score = maxscore
                trellis[time+1][nextstate].viterbi_path = maxpath

    def backward(self):
        """Calculate the backward variables."""
        
        trellis = self.timesteps
        observations = self.observations
        transitions = self.transitions
        emissions = self.emissions
        states = self.states

        time = len(observations) - 1
        for state in states:
            trellis[time][state].backward = 1.0

        for time in range(len(observations) - 2, -1, -1):
            obs = observations[time]
            for state in states:
                totalprob = 0
                for nextstate in states:
                    nextbackward = trellis[time+1][nextstate].backward
                    statetransition = transitions[state][nextstate]

                    emissionprob = emissions[nextstate][observations[time+1]]

                    prob = nextbackward * statetransition * emissionprob
                    totalprob += prob

                trellis[time][state].backward = totalprob

    def observation_probability(self):
        """Calculate the probability of the observations. For consistency,
        check to make sure that we'd get about the same estimate (by
        multiplying the forward and backward variables) at every given time
        step."""

        states = self.states
        trellis = self.timesteps

        prevanswer = None

        for time in range(len(self.timesteps)):
            total = 0
            for state in states:
                node = trellis[time][state]
                total += (node.forward * node.backward)

            if prevanswer:
                diff = abs(total - prevanswer)
                assert diff < 0.001
            else:
                prevanswer = total
        return total

    def best_path(self):
        """Return the best path through the hidden states, for the given
        observations, as determined by the Viterbi algorithm."""

        trellis = self.timesteps
        observations = self.observations
        states = self.states
        time = len(observations) - 1

        maxscore = 0
        bestpath = None

        for state in states:
            score = trellis[time][state].viterbi_score
            if score > maxscore:
                bestpath = trellis[time][state].viterbi_path
                maxscore = score
        return bestpath

    def display(self):
        for step in self.timesteps:
            print step

def rainy_days():
    """Do the example with the rainy days and the shopping, etc."""

    start_probability = {'Rainy': 0.6, 'Sunny': 0.4}
     
    transition_probability = {
       'Rainy' : {'Rainy': 0.7, 'Sunny': 0.3},
       'Sunny' : {'Rainy': 0.4, 'Sunny': 0.6},
       }
     
    emission_probability = {
       'Rainy' : {'walk': 0.1, 'shop': 0.4, 'clean': 0.5},
       'Sunny' : {'walk': 0.6, 'shop': 0.3, 'clean': 0.1},
       }

    sequences = []
    sequences.append(['walk', 'walk'])
    sequences.append(['walk', 'shop'])
    sequences.append(['walk', 'clean'])
    sequences.append(['shop', 'walk'])
    sequences.append(['shop', 'shop'])
    sequences.append(['shop', 'clean'])
    sequences.append(['clean', 'walk'])
    sequences.append(['clean', 'shop'])
    sequences.append(['clean', 'clean'])

    totaltotal = 0
    for seq in sequences:
        trellis = Trellis(seq, transition_probability, emission_probability,
            start_probability)

        seq_prob = trellis.observation_probability()
        totaltotal += seq_prob

        print "probability of the sequence", seq, ":", seq_prob
        print "best path for this sequence:", trellis.best_path()
    print "these sum up to:", totaltotal

def main():
    rainy_days()

if __name__ == "__main__": main()
