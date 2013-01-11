import re

speak_pattern = re.compile(r"\d\d:\d\d <.(\S+)> (.*)")

log_fn = "/home/alex/soic.log"

def lines_said_by(nick):
    out = []
    with open(log_fn) as infile:
        for line in infile:
            mat = re.match(speak_pattern, line)
            if mat:
                nick, text = mat.groups()
                out.append(text)
    return out

def main():
    by_alexr = lines_said_by("alexr")
    for line in by_alexr:
        print(line)

if __name__ == "__main__": main()
