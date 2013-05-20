import imp

import botcommon
import generatehaiku
import haikubot
import ircbot
import irclib
import languagemodel
import readfromlogs
import syllables_en
import syllables
import utils

def reload_code():
    """Reload all of our modules in case they've been changed on disk."""
    imp.reload(botcommon)
    imp.reload(generatehaiku)
    imp.reload(haikubot)
    imp.reload(ircbot)
    imp.reload(irclib)
    imp.reload(languagemodel)
    imp.reload(readfromlogs)
    imp.reload(syllables_en)
    imp.reload(syllables)
    imp.reload(utils)
