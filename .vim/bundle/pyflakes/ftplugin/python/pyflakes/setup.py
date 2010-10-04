#!/usr/bin/python
# (c) 2005 Divmod, Inc.  See LICENSE file for details

from distutils.core import setup

setup(
    name="pyflakes",
    license="MIT",
    version="0.2.1",
    description="passive checker of Python programs",
    author="Phil Frost",
    maintainer="Moe Aboulkheir",
    maintainer_email="moe@divmod.com",
    url="http://www.divmod.org/projects/pyflakes",
    packages=["pyflakes", "pyflakes.scripts"],
    scripts=["bin/pyflakes"],
    long_description="""Pyflakes is program to analyze Python programs and detect various errors. It
works by parsing the source file, not importing it, so it is safe to use on
modules with side effects. It's also much faster.""")
