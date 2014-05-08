compilePath = (source, topLevel, base) ->
  return if source in sources   or
            watchedDirs[source] or
            not topLevel and (notSources[source] or hidden source)
  try
    stats = fs.statSync source
  catch err
    if err.code is 'ENOENT'
      console.error "File not found: #{source}"
      process.exit 1
    throw err
