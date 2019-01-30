# ~/.pythonrc.py
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

import atexit
import os
import readline
import rlcompleter


def save_history_file(history_file=history_file, readline=readline):
    readline.write_history_file(history_file)


readline.parse_and_bind('tab: complete')
history_file = os.path.expanduser('~/.python_history')
readline.set_history_length(1000)
if os.path.exists(history_file):
    readline.read_history_file(history_file)
atexit.register(save_history_file)
del atexit, os, readline, rlcompleter, history_file, save_history_file
