# First order logic

## Resolution

Prove: $\forall x\ \text{Eat}(x, \text{Amanita}) \Rightarrow \text{Dead}(x)$
Premise:
  1. $\lnot\text{Red}(x) \lor \lnot\text{Mushroom}(x) \lor \text{Poisonous}(x)$
  2. $\lnot\text{Eat}(x, y) \lor \lnot\text{Poisonous}(y) \lor \text{Dead}(x)$
  3. Red(Amanita)
  4. Mushroom(Amanita)

  - $\forall x\ \text{Eat}(x, \text{Amanita}) \Rightarrow \text{Dead}(x)$
  - $\forall x\ \lnot\text{Eat}(x, \text{Amanita}) \lor \text{Dead}(x)$

set conclusion as false:
  - $\lnot(\forall x\ \lnot\text{Eat}(x, \text{Amanita})) \lor \text{Dead}(x)$
  - $\exists x\ \lnot\lnot\text{Eat}(x, \text{Amanita}) \land \lnot\text{Dead}(x)$
  - $\exists x\ Eat(x, \text{Amanita}) \land \lnot\text{Dead}(x)$
  - $\text{Eat}(c, \text{Amanita}) \land \lnot\text{Dead}(c)$

  5. Eat($c$, Amanita)

  6. $\lnot\text{Dead}(c)$

  7. // rules 2 + 5, $\theta = \{ x/c, y/\text{Amanita} \}$  
     $\lnot\text{Poisonous(Amanita)} \lor \text{Dead}(c)$

  8. // rules 6 + 7  
     $\lnot\text{Poisonous(Amanita)}$

  9. // rules 1 + 8, $\theta = \{ x/Amanita \}$  
     $\lnot\text{Read(Amanita)} \lor \lnot\text{Mushroom(Amanita)}$

  10. // rules 3 + 9  
      $\lnot\text{Mushroom(Amanita)}$

  11. // rules 4 + 10  
      FALSE, contradiction

## Connection between $\forall$ and $\exists$

### Example 1

 - $\forall x\ \lnot\text{Likes}(x, \text{Brocolli})$ // everyone dislikes brocolli
 - $\lnot \exists x\ \text{Likes}(x, \text{Brocolli})$ // there does not exist someone who like brocolli

### Example 2

 - $\forall x\ \text{Likes}(x, \text{icecream})$ // everyone likes icecream
 - $\lnot \exists x\ \lnot\text{Likes}(x, \text{icecream})$ // there is no one who dislikes icecream

### Equivalence

 - $\forall x\ \lnot P === \lnot \exists x\ P$
 - $\lnot \forall x\ P === \exists x\ \lnot P$
 - $\forall x\ P === \lnot \exists x\ \lnot P$
 - $\exists x\ P === \lnot \forall x\ \lnot P$

## Convert first order logic to CNF (Chapter 9, pg. 346)

- everyone who loves all animals is loved by someone:

    $\forall x\ [ \forall y\ \text{Animal}(y) \Rightarrow \text{Loves}(x, y) ] \Rightarrow [ \exists y\ \text{Loves}(y, x) ]$

1. eliminate implications

    $\forall x\ [ \lnot \forall y\ \lnot\text{Animal}(y) \lor \text{Loves}(x, y) ] \lor [ \exists\ y \text{Loves}(y, x) ]$

2. move $\lnot$ inward (tricky)

    - $\forall x\ [ \exists y\ \lnot(\lnot \text{Animal}(y) \lor \text{Loves}(x, y)) ] \lor [ \exists y\ \text{Loves}(y, x) ]$
    - $\forall x\ [ \exists y\ \text{Animal}(y) \land \lnot\text{Loves}(x, y) ] \lor [ \exists y\ \text{Loves}(y, x) ]$

3. standardize variables

    - $\forall x\ [ \exists y\ \text{Animal}(y) \land \lnot\text{Loves}(x, y) ] \lor [ \exists z\ \text{Loves}(z, x) ]$

    - if $(\exists x\ P(x)) \lor (\exists x\ Q(x))$ then change the name of one variable
    - in our case, we change the name of $y$ to $z$

4. skolemization

    - $\forall x\ \exists y \rightarrow$ use skolen function $F(x)$ to replace $y$
    - $\exists y \rightarrow$ use skolen constant c for $y$

    - $\forall x\ [ \text{Animal}(F(x)) \land \lnot\text{Loves}(x, F(x)) ] \lor [ \text{Loves}(G(z), x) ]$ // G(z) or G(x) ????

5. drop universal quantifiers

    - $[\text{Animal}(F(x)) \lor \lnot\text{Loves}(x, F(x))] \lor \text{Loves}(G(z), x)$
    - $G(z)$ means 'someone'

6. distribute $\lor$ over $\land$

    - $[\text{Animal}(F(x)) \lor \text{Loves}(G(z), x)] \land [\lnot\text{Loves}(x, F(x)) \lor \text{Loves}(G(z), x)]$

## Probabilities


                   +-------------------+-------------------+
                   | toothache         | ~toothache        |
+------------------+-------------------+-------------------+
| cavity           | 0.12              | 0.08              |
+------------------+-------------------+-------------------+
| ~cavity          | 0.08              | 0.72              |
+------------------+-------------------+-------------------+

P(cavity $\lor$ toothache) = 0.12 + 0.08 + 0.08 = 0.28

### marginal probability of cavity

P(cavities) = 0.12 + 0.08 = 0.2

### conditional probability

P(cavity | toothache) = \frac{P(cavity \lor toothache)}{P(toothache)}
                      = \frac{0.12}{0.12 + 0.08}

### product rule

P(a \land b) = P(a | b) P(b)
             = P(b | a) P(a)

### baye's rule

P(b | a) = \frac{P(a | b) P(b)}{P(a)}
