compilePath = (source, topLevel, base) ->
  return if source in sources   or
            watchedDirs[source] or
            not topLevel and (notSources[source] or hidden source)
