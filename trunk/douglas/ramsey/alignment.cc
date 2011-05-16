#include <cstdlib>
#include <iostream>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <vector>
#include <fstream>

using std::cout;
using std::endl;
using std::string;
using std::set;
using std::map;
using std::vector;
using std::ifstream;
using std::stringstream;

vector<string> &split(const string &s, char delim, vector<string> &elems) {
  stringstream ss(s);
  string item;
  while(std::getline(ss, item, delim)) {
    elems.push_back(item);
  }
  return elems;
}

vector<string> split(const string &s, char delim) {
  vector<string> elems;
  return split(s, delim, elems);
}


void loadsentences(string path, int n, vector<string> *sentences) {
  ifstream infile(path.c_str());
  string line;

  for (int i = 0; i < n && infile.is_open() && infile.good(); i++) {
    std::getline(infile,line);
    sentences->push_back(line);
  }
  infile.close();
}

// iterate over every sentence in the vector...
// split the sentence and then add every word to the vocab set.
void build_vocab(vector<string> &sentences, set<string> *vocab) {
  vector<string>::iterator sent;
  for(sent = sentences.begin(); sent != sentences.end(); sent++) {
    vector<string> splitted = split(*sent, ' ');
    vector<string>::iterator word;
    for(word = splitted.begin(); word != splitted.end(); word++) {
      vocab->insert(*word);
    }
  }
}

// Given two lists of sentences (each a list of words), build the translation
// table t(e|f), which is a dictionary of the shape t[e][f] = prob.
void translation_tables(vector<string> &esentences,
                        vector<string> &fsentences,
                        map<string, map<string, double> > *trans) {
  set<string> evocab;
  set<string> fvocab;

  build_vocab(esentences, &evocab);
  build_vocab(fsentences, &fvocab);
  
  std::cerr << "built the vocabulary." << endl;
  std::cerr << "it's this big: " << evocab.size() << endl;
  std::cerr << "it's this big: " << fvocab.size() << endl;

  /* # initialize t(e|f) uniformly */
  double lowprob = 1 / evocab.size();

  for (set<string>::iterator e = evocab.begin(); e != evocab.end(); ++e) {
    for (set<string>::iterator f = fvocab.begin(); f != fvocab.end(); ++f) {
      (*trans)[*e][*f] = lowprob;
    }
  }
  std::cerr << "ok." << endl;

  // while not converged...
  for (int step = 0; step < 5; ++step) {
    std::cerr << "step " << step << endl;

    // initialize
    // count(e|f) = 0 for all e, f
    map<string, map<string, double> > count;
    // total(f) = 0 for all f
    map<string, double> total;
    map<string, double> s_total;

    // for all sentence pairs(esent,fsent) do:
    vector<string>::iterator esent;
    vector<string>::iterator fsent;
    fsent = fsentences.begin();

    for (esent = esentences.begin(); 
         esent != esentences.end();
         ++esent, ++fsent) {

      vector<string> esplitted = split(*esent, ' ');
      vector<string> fsplitted = split(*fsent, ' ');

      // compute normalization
      for(vector<string>::iterator e = esplitted.begin();
        e != esplitted.end();
        ++e) {
        s_total[*e] = 0;

        for(vector<string>::iterator f = fsplitted.begin();
          f != fsplitted.end();
          ++f) {
          s_total[*e] += (*trans)[*e][*f];
        }
      }

      // collect counts
      for(vector<string>::iterator e = esplitted.begin();
        e != esplitted.end();
        ++e) {
        for(vector<string>::iterator f = fsplitted.begin();
          f != fsplitted.end();
          ++f) {
          count[*e][*f] += (*trans)[*e][*f] / s_total[*e];
          total[*f] += (*trans)[*e][*f] / s_total[*e];
        }
      }
    }

    // estimate probabilities
    for (set<string>::iterator e = evocab.begin(); e != evocab.end(); ++e) {
      for (set<string>::iterator f = fvocab.begin(); f != fvocab.end(); ++f) {
        if (total.find(*f) != total.end()) {
          (*trans)[*e][*f] = count[*e][*f] / total[*f];
        } else {
          (*trans)[*e][*f] = 0.0; 
        }
      }
    }
  }

  return;
}

int main(int argc, char *argv[]) {
  vector<string> esentences;
  vector<string> fsentences;

  string home = getenv("HOME");
  string englishpath = home + "/corpora/es-en/europarl-v6.es-en.en";
  string spanishpath = home + "/corpora/es-en/europarl-v6.es-en.es";

  loadsentences(englishpath, 500, &esentences);
  loadsentences(spanishpath, 500, &fsentences);
  
  map<string, map<string, double> > trans;
  translation_tables(esentences, fsentences, &trans);

  std::cerr << trans["whenever"]["semana"] << endl;
  std::cerr << trans["President"]["Presidente"] << endl;
  std::cerr << trans["legislature"]["político"] << endl;

  exit(EXIT_SUCCESS);
}
