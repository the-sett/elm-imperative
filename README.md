# Imperative Programming Railway for Elm

Imperative programming means being able to do IO side effects anywehere within 
a program. Many languages that let you do this, also have an exception mechanism
since IO operations may fail, and the programmer would like to detect and recover
from this.

In functional programming, railway style means having a structure with 2 tracks
that run throughout your code; the success track and the error track. This 
structure comes with operations to move between the tracks, as well as to write
code to process each track.
