#!/usr/bin/env python

from __future__ import with_statement
import vim
import os
import subprocess
import threading
import Queue


class Asyncer:

    def __init__(self):
        self._workers = {}

    def execute(self, var_key, var_command, var_cwd, var_input, var_appends):
        key     = vim.eval(var_key)
        command = vim.eval(var_command)
        cwd     = vim.eval(var_cwd)
        input   = vim.eval(var_input)
        appends = vim.eval(var_appends)
        if key not in self._workers:
            self._workers[key] = Worker()
            self._workers[key].start()
        self._workers[key].put(Executor(command, cwd, input, appends))

    def print_output(self, var_key):
        key = vim.eval(var_key)
        if key not in self._workers:
            return
        for l in self._workers[key].copy_outputs():
            print l,

    def print_worker_keys(self):
        for k in self._workers.keys():
            print k

    def print_active_worker_keys(self):
        for k in self._workers.keys():
            print k


class Worker(threading.Thread):

    def __init__(self):
        threading.Thread.__init__(self)
        self._queue = Queue.Queue()
        self._lines = []
        self._lock = threading.Lock()

    def run(self):
        while True:
            self._queue.get().execute(self)
            self._queue.task_done()

    def put(self, executor):
        self._queue.put(executor)

    def clear_outputs(self):
        with self._lock:
            self._lines = []

    def record_output(self, line):
        with self._lock:
            self._lines.append(line)

    def copy_outputs(self):
        with self._lock:
            return self._lines[:]


class Executor:

    def __init__(self, command, cwd, input, appends):
      self._command = command
      self._cwd = cwd
      self._input = input
      self._appends = appends

    def execute(self, worker):
        if not self._appends:
            worker.clear_outputs()
        os.chdir(self._cwd)
        p = subprocess.Popen(self._command, shell=True, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        p.stdin.write(self._input)
        line = p.stdout.readline()
        while line:
            worker.record_output(line)
            line = p.stdout.readline()


