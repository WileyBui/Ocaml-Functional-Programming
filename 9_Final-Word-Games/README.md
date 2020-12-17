# Final Project

## Problems from which to choose

Below are a number of problems that you may choose from as the problem
you will solve for your final project.

Since your circumstances may be challenging right now, you may want to
make some calculations about how much time and energy you can afford
to commit to this effort.  Some are easier and are thus worth less
when your final grade is computed. Some are more challenging and thus
worth more.

Problems have a specified maximum letter grade that you can receive
for that problem.  Problems range from a maximum letter grade of a C
to a maximum of a grade of A.  This is the maximum grade **on this
project** only.

I encourage you to read over all of the problems and consider what
sort of commitment you can make to this project.  Note that choosing a
problem that can earn a B will not be interpreted in a negative way.
Of course, we would like to see everyone choosing the most challenging
problems to maximize what you may learn in this effort - but reality
being what it is, we are not so naive!


## Word games

### A: The Q-U Quiz

This problem asks you to compute answers for the Q-U Quiz as heard on the
Sunday Puzzle on NPR.  The puzzle definition and the radio
broadcast of it can be found here:

  https://www.npr.org/2020/11/08/932548059/sunday-puzzle-the-q-u-quiz

You are to write a function `qu_quiz` with the following type:

```
string -> (string * string) list
```

This function takes in the name of a file containing a list of words
and produces a list of answers.  Each answer is a pair such that if
the letters 'q' and 'u' are added to the first word, the letters can
be rearranged to spell the second word in the pair.

#### A new function for reading files

To read a list of words from a file, please use the updated
`read_file` function provided in the file `read_file.ml`

The version you were given previously is rather inefficient and will
overflow the stack on the larger word files and **only return some of
that file contents**.


#### Word lists

Also in the `Files/Final_Project` directory are 3 word lists
- `words-small.txt`
- `words-google-10000.txt`, 
  taken from here: https://github.com/first20hours/google-10000-english
- `words-corncob.txt`, 
  taken from here: http://www.mieliestronk.com/wordlist.html

#### Sample results

With the sample solution to this problem, evaluating `qu_quiz
"words-small.txt"` will produce the following results:

```
[ ("crooner", "conqueror");
  ("italy", "quality");
  ("stir", "squirt");
  ("teens", "sequent")
]
```
The above text is reformatted to make it easier to read.

The order of word pairs in the list does not matter.  Tests of the
functionality of your function will have the following form that only
checks for list membership of answers:
```
List.mem ("italy", "quality") (qu_quiz "words-small.txt")
```
This expression should evaluate to `true`.

### B: It Takes Two


This problem asks you to compute answers for the It Takes Two puzzle
as heard on the Sunday Puzzle on NPR.  The puzzle definition and the
radio broadcast of it can be found here:

  https://www.npr.org/2018/01/21/579110492/sunday-puzzle-it-takes-two


You are to write a function `it_takes_two` with the following type:

```
string -> (string * string) list
```

This function takes in the name of a file containing a list of words
and produces a list of answers.  

Each answer is a pair such that the first string is a 4 letter word
(no not that kind!) found in the input file and the second is a 6
letter work, also found in the input file, that is created from the 4
letter word by adding a single letter at the front of the word and a
single letter at the end of the word.

The same word files described above in the QU Quiz problem will also
be used for this problem.

For example, the file "words-small.txt" contains "planet" and "lane"
and thus ``it_takes_two words-small.txt`` will return a list
containing `("lane", "planet")`

``answers`` will return the string ``"planet"`` in its output.

This work is to be put in a file named `it_takes_two.ml` in the
`Final_Project` directory of your individual repository.

With the sample solution to this problem,
evaluating `it_takes_two "words-small.txt"` will produce the following
results:
```
[("lane", "planet"); ("moot", "smooth"); ("hang", "change");
 ("went", "twenty"); ("nigh", "knight"); ("tree", "street");
 ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest");
 ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled");
 ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]
```

The order of word pairs in the list does not matter.  Tests of the
functionality of your function will have the following form that only
checks for list membership of answers:
```
List.mem ("lane", "planet") (it_takes_two "words-small.txt")
```
This expression should evaluate to `true`.