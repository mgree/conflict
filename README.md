These scripts are for managing program committee meetings. They are
designed to read CSV files exported from HotCRP and produce meeting
schedules that minimize traffic due to conflicts of interest.

# How to use these scripts

First, you have to build the scripts. You'll need Haskell and some packages. I've only tested on GHC 7.8.3.

```
$ make
ghc ordering.hs -main-is Ordering -o ordering
[1 of 2] Compiling Util             ( Util.hs, Util.o )
[2 of 2] Compiling Ordering         ( ordering.hs, ordering.o )
Linking ordering ...
ghc slides.hs -main-is Slides -o slides
[2 of 2] Compiling Slides           ( slides.hs, slides.o )
Linking slides ...
```

There are three scripts: ordering, check, and slides, to be run in that order.

```
$ ./ordering papers.csv conflicts.csv pc.csv >schedule.csv
$ ./check papers.csv schedule.csv
All papers were scheduled.
$ ./slides schedule.csv pc.csv >slides.tex
$ pdflatex slides.tex
```

The first command generates a schedule on stdout, which is redirected
here to schedule.csv. The schedule file output by `ordering` has a format like...

```
17,foo@bar.com,blah@bargle.com,professor@very.important.edu
25,foo@bar.com,blah@bargle.com
```

...where the initial number is the paper under discussion and the
emails are those of conflicted PC members. The next command, `check`,
looks at the output and makes sure all papers were scheduled.
Finally, the slides generated by `slides` use LaTeX/Beamer to produce
a schedule indicating the current paper under discussion and the two
upcoming papers.

# File formats

There are four kinds of files processed by these tools: `papers.csv`,
a list of papers; `conflicts.csv`, a list of conflicts of interest;
`pc.csv`, a list of program committee members; and `schedule.csv`, a
list of papers and their conflicts.

Sample files are in the directory `sample/`.

## `papers.csv`

The papers to be discussed are listed in `papers.csv`. Concretely, it
is list is an optionally headered, newline-separated list of unique numbers, e.g.:

```
paper
3
5
10
18
```

Only papers listed in `papers.csv` will be included in the final
schedule. Note that optional headering means that it's okay if that
first line is absent; a warning message will be printed.

## `conflicts.csv`

Conflicts of interest are listed in `conflicts.csv`. Concretely, it is
a headered comma-separated value (CSV) file, e.g.:

```
paper,title,PC email,conflict type
3,Elf-powered Computing,legolas@bar.com,Chair-confirmed
5,"On Terrible titles",baz@quux.org,Chair-confirmed
10,Science is Magic,professor@very.important.edu,Chair-confirmed
10,Science is Magic,harry.houdini@invisible.net,Recent collaborator
11,"P > NP",professor@very.important.edu,Recent collaborator
```

The only relevant bits are the paper number, `paper`, and the email of
the conflicted PC member, `PC email`. Note that papers may appear in
`papers.csv` but not `conflicts.csv`---these papers have no
conflicts---and vice versa---these papers have conflicts but are not
going to be discussed.

## `pc.csv`

Those attending the program committee meeting are listed in `pc.csv`;
concretely, it is a headered CSV file.

```
first,last,email,affiliation
Baz,Bazzerton,baz@quux.org,QUUX Industries
Professor,Plum,professor@very.important.edu,VIU
Harry,Houdini,houdini@invisible.net,None
Ada,Lovelace,lovelace@babbage.ac.uk,Babbage College
```

The only fields used are `first`, `last`, and `email`. Note that email
addresses may appear in `pc.csv` but not in `conflicts.csv`---these PC
members have no conflicts---and vice versa---these people have
conflicts but won't be attending the meeting, and need not be considered in the schedule.

## `schedule.csv`

Schedules are generated by the `ordering` tool and consumed by the
`slides` tool. Concretely, it is a headerless, varible-width CSV file.

```
10,professor@very.important.edu,harry.houdini@invisible.net
3
18
5,baz@quux.org
```

Each line lists a paper number and the emails of conflicted PC
members. Papers with no conflicts simply have no following email
addresses.

It will almost certainly cause an error in `slides` to have an email
address in `schedule.csv` that doesn't have a corresponding entry in
`pc.csv`.

# Silly questions

## Why is it two programs?

Because you might want to edit the schedule by hand, e.g., to put
specific papers first or last.

## Is it really optimal?

No, I use a simple greedy algorithm: pick a paper as the "first"
paper; the next paper is one with minimal overlap; and so on. I try
this with every possible first paper and choose the best.
