#!/usr/bin/env python3

import haikubot
import botcommon

if __name__ == "__main__":
  try:
    botcommon.trivial_bot_main(haikubot.HaikuBot)
  except KeyboardInterrupt:
    print("Shutting down.")

