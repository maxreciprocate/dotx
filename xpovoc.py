#!/usr/bin/env python3
import collections
import math
import operator
import random
import re
import sys
import time
from datetime import datetime

# rotate saves the day again
from numpy import roll
from absl import app, flags

flags.DEFINE_string('xfile', None, 'Path to x.org')
FLAGS = flags.FLAGS

Imera = collections.namedtuple('Imera', ['start_time', 'rest_time', 'date', 'comments', 'acts', 'rating'])

def get_imeras(filename) -> [Imera]:
  with open(filename) as file:
    source = file.read()

  imeras = map(lambda x: '*' + x, source.split('*')[1:])

  return [imera_from_org(s) for s in imeras]

def save_imeras(filename: str, imeras: [Imera]) -> None:
  org = '\n'.join([to_org(i) for i in imeras])

  with open(filename, 'w') as file:
    file.write(org)

def imera_from_org(imera_string: str) -> Imera:
  lines = imera_string.splitlines()
  if len(lines) < 2:
    raise ValueError('We lack the proper structure')

  _date, _stats, *_other = lines
  _comments = filter(lambda s: s.startswith('#'), _other)
  _acts = filter(lambda s: s.startswith(':'), _other)

  date_match = re.match('\\* \\<([\\d+-]+ \\w+)\\>', _date)
  if date_match is not None:
    date_string, _ = date_match.group(1).split()

  stats_match = re.match('@ (\\d+:\\d+)\\s+\\|(\\d+)\\| *(?:&(\\d+))?', _stats)
  if stats_match is not None:
    start_time_string, sleep_time_string, rating_string = stats_match.groups()

  comments = [s.replace('#', '').strip() for s in _comments]
  comments = list(filter(lambda s: s, comments))

  acts = {}
  for act in _acts:
    act_match = re.match(': (\\d+) ≺ (\\w+)', act)
    if act_match is None:
      continue

    hour, act = act_match.groups()
    acts.update({int(hour): act})

  date = datetime.strptime(date_string, '%Y-%m-%d').date()
  start_time = datetime.strptime(start_time_string, '%H:%M').time()
  rest_time = int(sleep_time_string)
  if rating_string is not None:
    rating = int(rating_string)
  else:
    rating = 0

  return Imera(start_time, rest_time, date, comments, acts, rating)

def get_in(xs: {}, values: str) -> float:
  return sum([xs.get(v, 0.0) for v in values])

def present_imera(imera: Imera, summaryby: str) -> str:
  jiayou = random.choice([
    "今日事, 今日畢",
  ])

  r = 21
  top = round(r / 2)
  bottom = round(math.sqrt(3) * r / 2)

  acts = list(imera.acts.values())

  if len(acts) >= 12:
    overtime = len(acts) - 12
    acts = acts[-12:]
    acts[0] = '_'
    acts = roll(acts, overtime)

  if len(acts) < 12:
    acts = acts + ['_' for _ in range(0, 12 - len(acts))]

  acts = roll(acts, imera.start_time.hour - 12)

  summary = imera_summary(imera)

  if summaryby == "day":
    print("day sm")
    l1, l2, l3, l4, l5, l6, l7 = this_day_summary()
  elif summaryby == "week":
    print("week sm")
    l1, l2, l3, l4, l5, l6, l7 = last_week_summary()

  s = get_in(summary, 'snmeoctjipyxg')
  d = get_in(summary, 'dkfawq')

  pad = 4
  lpad = 3

  l = ''.center(lpad)
  message = f's/{s} &/{d} /{s+d}'
  circle = [ l + acts[0].center(r*2) + l1.rjust(r+pad+1)
             , l + acts[11].rjust(top)      + ' '*(2*top+1)       + acts[1].ljust(top) + l2.rjust(r+pad+2)
             , l + acts[10].rjust(r-bottom) + ' '*(2*bottom+1)    + acts[2].ljust(r-bottom) + l3.rjust(r+pad)
             , l + acts[9]                  + message.center(2*r) + acts[3] + l4.rjust(r+pad+1-max(len(acts[9]), 1)-max(len(acts[3]), 1))
             , l + acts[8].rjust(r-bottom)  + ' '*(2*bottom+1)    + acts[4].ljust(r-bottom) + l5.rjust(r+pad)
             , l + acts[7].rjust(top)       + ' '*(2*top+1)       + acts[5].ljust(top) +l6.rjust(r+pad+2)
             , l + acts[6].center(r*2) + l7.rjust(r+pad+1)
  ]

  return '\n\n'.join([l+ f'{datetime.fromtimestamp(time.time()):%Y-%m-%d %H:%M}'.center(r*2),
                      '\n\n\n'.join(circle),
                      l + jiayou.center(r*2-7),
  ])

def add_act(imera: Imera, act: str) -> Imera:
  if not act.isalpha():
    return imera

  last_hour = imera.start_time.hour + len(imera.acts)
  imera.acts.update({last_hour: act})

  return imera

def imera_summary(imera: Imera) -> {str, float}:
  work = {}
  for act in imera.acts.values():
    for act_atom in act:
      work.update({act_atom: work.get(act_atom, 0.0) + 1.0 / len(act)})

  return work

def this_day_summary() -> str:
  this_imera = get_imeras(FLAGS.xfile)[-1]
  summary = imera_summary(this_imera)
  output = ['&/&' for _ in range(0, 7)]
  sorted_summary = sorted(summary.items(), key=operator.itemgetter(1), reverse=True)

  for idx, (act, hours) in enumerate(sorted_summary[:7]):
    output[idx] = f"{act}/{hours:.1f}"

  return output

def last_week_summary() -> str:
  imeras = get_imeras(FLAGS.xfile)
  last_week_imeras = imeras[-8:-1]

  summaries = [imera_summary(imera) for imera in last_week_imeras]

  acts = {}
  for summary in summaries:
    for k, v in summary.items():
      acts.update({k: acts.get(k, 0) + v})

  daily_acts = {}
  for k, v in acts.items():
    daily_acts.update({k: v / 7})

  daily_averages = sorted(daily_acts.items(), key=operator.itemgetter(1), reverse=True)
  daily_top = sorted(daily_averages[:7], key=operator.itemgetter(1), reverse=True)

  return [f'{act}/{hours:.1f}' for act, hours in daily_top]

def to_org(imera: Imera):
  lines = []
  for comment in imera.comments:
    lines.append(f'# {comment}')

  for k, v in imera.acts.items():
    lines.append(f': {k} ≺ {v}')

  weekday = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'][imera.date.weekday()]
  org = [f'* <{imera.date} {weekday}>',
         f'@ {imera.start_time:%H:%M} |{imera.rest_time}| &{imera.rating}']

  return '\n'.join(org + lines)

def new_imera(start_time='0:00', rest_time=0, date=None, comments=[], acts=None, rating=0) -> Imera:
  if date is None:
    date = datetime.fromtimestamp(time.time()).date()

  if acts is None:
    acts = {}

  start_time = datetime.strptime(start_time, '%H:%M').time()
  return Imera(start_time, rest_time, date, comments, acts, rating)

def main(_):
  if not FLAGS.xfile:
    FLAGS.xfile = '/tmp/x.org'

  *imeras, imera = get_imeras(FLAGS.xfile)

  if imera.date != datetime.fromtimestamp(time.time()).date():
    imeras += [imera]
    print(f'When was the dawn? {datetime.fromtimestamp(time.time()):%Y-%m-%d %H:%M}')
    hour, sleep = input().split()
    imera = new_imera(hour, sleep)

  summaryby = "day"
  print(present_imera(imera, summaryby))

  message = input('')
  while message != '':
    if message.islower():
      imera = add_act(imera, message)
      if imera is not None:
        hour, act = list(imera.acts.items())[-1]
        sys.stdout.write(f'{hour} ≺ {act}\n')

    elif message.isupper():
      if message == "D":
        summaryby = "day"
      elif message == "W":
        summaryby = "week"

    elif message.isdigit():
      imera = imera._replace(rating=int(message))
      sys.stdout.write('doux!')

    else:
      imera.comments.append(message)
      sys.stdout.write('doux!')

    sys.stdout.flush()
    print(present_imera(imera, summaryby))
    message = input('')

  save_imeras(FLAGS.xfile, imeras + [imera])

if __name__ == '__main__':
  try:
    app.run(main)
  except Exception as e:
    print(e)
