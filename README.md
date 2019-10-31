# CS316 Functional Programming 2019/2020

Welcome to the page for the University of Strathclyde's “Functional Programming” class (CS316).

This course has a [Twitter account](https://twitter.com/StrathCS316).

*Assessment:* this course is entirely assessed by coursework. There are three exercises that you will complete (details below). You will do roughly 60% of the exercise at home or in the labs, and the 40% is done in exam conditions in the lab.

See the [schedule](schedule.txt).

## Contact

The course lecturer is **Bob Atkey**. Office LT1305 [robert.atkey@strath.ac.uk](mailto:robert.atkey@strath.ac.uk).

## Lectures

Lectures are at **13:00 Tuesdays** in JA501 and **11am Fridays** in RC447.

Most of the lectures involve me doing live coding with explanations of what I am doing. This repository contains cleaned up versions of the live coding, interspersed with commentary.

 - [Lecture 01](lectures/Lec01.hs) : Data and Pattern Matching ([live version](lectures/Lec01Live.hs))
 - [Lecture 02](lectures/Lec02.hs) : Defining Functions ([live version](lectures/Lec02Live.hs))
 - [Lecture 03](lectures/Lec03.hs) : Recursive functions I ([live version](lectures/Lec03Live.hs))
 - [Lecture 04](lectures/Lec04.hs) : Recursive functions II (notes pending, see notes for Lec03)
 - [Lecture 05](lectures/Lec05.hs) : Higher order functions ([live version](lectures/Lec05Live.hs))
 - [Lecture 06](lectures/Lec06.hs) : List comprehensions ([live version](lectures/Lec06Live.hs))
 - [Lecture 07](lectures/Lec07.hs) : Modelling with datatypes ([live version](lectures/Lec07Live.hs))
 - [Lecture 08](lectures/Lec08.hs) : Recursion Schemes ([live version](lectures/Lec08Live.hs))
 - [Lecture 09](lectures/Lec09.hs) : Functors and Containers (needs minor updating) ([live version](lectures/Lec09Live.hs))
 - [Lecture 10](lectures/Lec10.hs) : Programming with Exceptions
 - Lecture 11 : Programming with Side Effects
 - Lecture 12 : Monads and Applicatives
 - Lecture 13 : I/O
 - Lecture 14 : Parser Combinators I
 - Lecture 15 : Parser Combinators II
 - Lecture 16 : Visiting and Traversing Containers
 - Lecture 17 : Lazy Evaluation
 - Lecture 18 :
 - Lecture 19 : Parallelism
 - Lecture 20 : Concurrency
 - Lecture 21 : CS410 Propaganda

## Evaluation Game

The [Evaluation Game](https://personal.cis.strath.ac.uk/robert.atkey/terms.html).

## Tutorial Questions

- [Some questions on Higher Order Functions](tutorials/HOFQuestions.hs) and [their solutions](tutorials/HOFSolutions.hs).

## Coursework

This course is entirely assessed by coursework. The split between the three exercises is shown below:

- Exercise 1 (30%) : [First Order Programming](exercises/Ex1.hs). This was released on Thursday 3rd October (week 2), and the final deadline and test are on Monday 21st October (week 5).

- Exercise 2 (30%) : [Higher Order Programming](exercises/Ex2.hs). This was be released on Thursday 17th October (week 4), and the final deadline and test are on Monday 11th November (week 8). Example pictures [green-everywhere](exercises/green-everywhere.bmp), [blue-and-green](exercises/blue-and-green.bmp), [doughnut](exercises/doughnut.bmp), [red-circle-on-blue](exercises/red-circle-on-blue.bmp), [red-rect-on-blue](exercises/red-rect-on-blue.bmp), and [picture](exercises/picture.bmp).

- Exercise 3 (40%) : GHOUL. This will be released on Thursday 7th November (week 7), and the final deadline and test are on Monday 2nd December (week 11).

After each of the exercises has been marked, we will email you your marks

## Uses of Haskell in Industry

The following are some links to uses of Haskell in industry. These kinds of experience reports are often useful to see what engineers find useful in Haskell, and what they don't. Many of the features of Haskell (higher order functions, algebraic datatypes, pattern matching, immutability, type classes) are making their way into other languages because they have proved their worth in Haskell.

- [Fighting spam with Haskell](https://engineering.fb.com/security/fighting-spam-with-haskell/) at Facebook.
- [Habito: The Purely Functional Mortgage Broker](https://www.infoq.com/presentations/habito-mortgage-broker/).
- [Semantic](https://github.com/github/semantic/) is GitHub's tool for analyzing source code to connect uses with definitions, and to look for differences between pieces of code.

Other functional languages are also used at extremely large scale. For example:

- [WhatsApp](https://www.wired.com/2015/09/whatsapp-serves-900-million-users-50-engineers/) uses the functional programming language [Erlang](https://www.erlang.org/) for all its backend services.
- [Jane Street](https://www.janestreet.com/technology/) is a proprietary trading firm that almost exclusively uses the functional language [OCaml](https://www.ocaml.org/). They also offer [internships](https://www.janestreet.com/join-jane-street/internships/) for undergraduates.
