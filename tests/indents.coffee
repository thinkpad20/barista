grade = (student) ->
  if student.excellentWork
    'A+'
  else if student.okayStuff
    if student.triedHard then 'B' else 'B-'



  else
    'C'

foo
