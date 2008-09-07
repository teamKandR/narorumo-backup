def pick_locale(request):
  """Given a WebOb Request object, presumably with a query parameter called
  "available", return the best locale setting."""

  available_parm = request.get("available")
  if not available_parm:
    available_parm = "none"

  available = available_parm.split(",")
  preferred = ordered_locales(request.headers['accept-language'])

  best = pick(available, preferred)
  return best

def pick(available, preferred):
  """Returns the language that we'll display in; the most-preferred one
  available, or failing that, just pick one."""

  for lang in preferred:
    if lang in available:
      return lang
  return available[0]

def ordered_locales(accept_language):
  """Take the Accept-Language header string and produce an ordered list of
  language strings, from most preferred to least."""
  langs = accept_language.split(",")
  pairs = map( lambda(lang): lang.split(";"), langs)

  for pair in pairs:
    if len(pair) < 2: pair.append(1)
    else: pair[1] = float(pair[1][2:])
  pairs.sort(key=lambda(x):x[1], reverse=True)

  return [lang for (lang,val) in pairs if val > 0.0]
