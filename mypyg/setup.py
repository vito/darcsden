from setuptools import setup

__author__ = "Alex Suraci"

setup(
    name='MyPyg',
    version='1.0',
    description=__doc__,
    packages=['mypyg'],
    install_requires=['pygments'],
    entry_points='''
[pygments.lexers]
atomo = mypyg:AtomoLexer
'''
)
