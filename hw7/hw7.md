---
title: 'CS161: Homework 7'
author: 'Rodrigo Valle'
header-includes:
 - \usepackage{tikz}
---

<!--
Bayes Theorem, which states:
  $$\Pr(A | B) = \frac{\Pr(B | A) \Pr(A)}{\Pr(B)}$$
-->

# Problem 1

## Prove Generalized Product Rule

  $$\Pr(A, B | K) = \Pr(A | B, K) \Pr(B | K)$$

To prove the generalized product rule, we use the axiom of conditional
probability, which states:

\begin{equation}
    \Pr(X | Y) = \frac{\Pr(X, Y)}{\Pr(Y)}
\end{equation}

From this follows the less general product rule, for the intersection of two
events:

\begin{align*}
    \Pr(Y | X) &= \frac{\Pr(X, Y)}{\Pr(X)} \\
    \Pr(X, Y)  &= \Pr(Y | X) \Pr(X)
\end{align*}

Substitute $X = A \cap B$ and $Y = K$ into (1)

\begin{align*}
    \Pr(A, B | K) &= \frac{\Pr(A, B, K)}{\Pr(K)}
\end{align*}

Substitute $X = A$ and $Y = B \cap K$ into (1)

\begin{align*}
    \Pr(A | B, K) &= \frac{\Pr(A, B, K)}{\Pr(B, K)}
\end{align*}

Now combine and simplify using the $\Pr(B | K) = \frac{\Pr(B, K)}{\Pr(K)}$:

\begin{align*}
    \Pr(A, B | K) &= \frac{\Pr(A | B, K) \Pr(B, K)}{\Pr(K)} = \Pr(A | B, K) \Pr(B | K)
\end{align*}

## Prove Generalized Baye's Rule

  $$\Pr(A | B, K) = \frac{\Pr(B | A, K) \Pr(A | K)}{\Pr(B | K)}$$

\begin{align*}
    \Pr(A, B) &= \Pr(A | B) \Pr(B) \\
              &= \Pr(B | A) \Pr(A) \\
    \Pr(B | A) \Pr(A) &= \Pr(A | B) \Pr(B) \\
    \Pr(A | B) &= \frac{\Pr(B | A) \Pr(A)}{\Pr(B)}
\end{align*}

\begin{align*}
    \Pr(A | B, K) &= \Pr(A | B, K) \Pr(B | K) \\
    \Pr(B | A, K) &= \Pr(B | A, K) \Pr(A | K) \\
    \Pr(B | A, K) \Pr(A |K) &= \Pr(A | B, K) \Pr(B | K) \\
    \Pr(A | B, K) &= \frac{\Pr(B | A, K) \Pr(A | K)}{\Pr(B | K)}
\end{align*}
\pagebreak

# Problem 2
<!--
\begin{tikzpicture}[auto, node distance=3cm, every loop/.style={},
                    thick,main node/.style={circle,draw,font=\sffamily\Large\bfseries}]
-->
\begin{figure}[h!]
\centering
\begin{tikzpicture}[->, >=latex, shorten >= 2pt, shorten <= 2pt]
  \node[draw, circle] (root) {$C$}
    child {node [draw, circle] (a) {$X_1$}}
    child {node [draw, circle] (b) {$X_2$}}
    child {node [draw, circle] (c) {$X_3$}};
\end{tikzpicture}
\end{figure}

where $n \in \{1, 2, 3\}$, the CPTs for the coin tosses $X_1, X_2$ and $X_3$ are:

  $C$   $X_n$   $\Pr(X_n | C)$  
 ----- ------- ----------------  
   a      h         0.2          
   a      t         0.8          
   b      h         0.6          
   b      t         0.4          
   c      h         0.8          
   c      t         0.2          

and the distribution for $C$ is:

  $C$      $\Pr(C)$
 ----- ----------------
   a   $0.\overline{3}$
   b   $0.\overline{3}$
   c   $0.\overline{3}$

\pagebreak

# Problem 3

 world    $N$   $S$     $C$     $\Pr(N, S, C)$
-------  -----  ------  -----  ----------------
   1       1    square  black       2/13
   2       1    square  white       1/13
   3       1    circle  black       1/13
   4       1    circle  white       1/13
   5       2    square  black       4/13
   6       2    square  white       1/13
   7       2    circle  black       2/13
   8       2    circle  white       1/13

- $\alpha_1$: the object is black
- $\alpha_2$: the object is square
- $\alpha_3$: if the object is one or black, then it is also a square

---------------------------------------------------------------------

- $\alpha_1$ holds in worlds 1, 3, 5 and 7 with a probability of
    $$\Pr(\alpha_1) = \frac{2}{13} + \frac{1}{13} + \frac{4}{13} + \frac{2}{13}
                    = \frac{9}{13}$$

- $\alpha_2$ holds in worlds 1, 2, 5 and 6 with a probability of
    $$\Pr(\alpha_2) = \frac{2}{13} + \frac{1}{13} + \frac{4}{13} + \frac{1}{13}
                    = \frac{8}{13}$$

- $\alpha_3$ holds in worlds 1, 2 and 5 with a probability of
    $$\Pr(\alpha_3) = \frac{2}{13} + \frac{1}{13} + \frac{4}{13}
                    = \frac{7}{13}$$

# Problem 4

## A

These should be read as: the variable (first parameter to I) is independent of
its non-descendants (third parameter to I), when giving values for the
variable's parents.

- I$(A, \varnothing, \{B, E\})$
- I$(B, \varnothing, \{A, C\})$
- I$(C, A, \{D, B, E\})$
- I$(D, \{A, B\}, \{C, E\})$
- I$(E, B, \{A, C, D, F, G\})$
- I$(F, \{C, D\}, \{A, B, E\})$
- I$(G, F, \{A, B, C, D, E, H\})$
- I$(H, \{F, E\}, \{A, B, C, D, G\})$

## B

- d_separated(A, BH, E): false, there is a path
    $$A \rightarrow C \rightarrow F \rightarrow H \leftarrow E$$
    where the collider H does not block the path because it is given.

- d_separated(G, D, E): false, there is a path
    $$G \leftarrow F \leftarrow C \leftarrow A \rightarrow D \leftarrow B
        \rightarrow E$$
    where the collider D does not block the path because it is given.

- d_separated(AB, F, GH): false, there is a path
    $$B \rightarrow E \rightarrow H$$

## C
\begin{equation*}
  \Pr(a, b, c, d, e, f, g, h) = \Pr(a) \Pr(b) \Pr(c | a) \Pr(d | a, b)
                                \Pr(e | b) \Pr(f | c, d) \Pr(g | f) \Pr(h | f, e)
\end{equation*}

## D

1. $\Pr(A = 0, B = 0) = \Pr(A = 0) \Pr(B = 0) = (.8)(.3) = 0.24$

2.
 - $\Pr(E = 1 | A = 1) = \Pr(E = 1)$
 - $\Pr(E = 1) = \Pr(E = 1 | B = 0)\Pr(B = 0) + \Pr(E = 1 | B = 1)\Pr(B = 1)$
 - $\Pr(E = 1) = (0.9)(0.3) + (0.1)(0.7) = 0.27 + 0.07 = 0.34$
