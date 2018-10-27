# Safe Matrix Experiment

This is an attempt to implement type safe matrix operations using my knowledge of Haskell and it's type extensions.
My goal is to test my own knowledge of Haskell's type system and not to achieve any practical objective outside of that.

```Haskell
 Matrix (
            (1 :-: 2 :-: Nil)
        :-: (3 :-: 4 :-: Nil)
        :-: Nil)
```

The Matrix [1 2; 3 4]


This one where the second column is missing an element is a type error though (A property that lists of lists won't catch).
Also note that this is caught at compile-time not at run time like a smart constructor or factory method would find.
```Haskell
Matrix (
          (1 :-: Last 2)
      :-: Last (Last 3)
```

Again, Haskell will catch any attempts to add matrices with different dimensions at compile time, not run time.
```Haskell
m1 = Matrix ((1 :-: Last 2) :-: Last (3 :-: Last 4))
m2 = Matrix ((5 :-: Last 6) :-: Last (7 :-: Last 8))
m3 = m1 `add` m2
m4 = Matrix (Last 5 :-: Last 6 :-: Last (Last 7))
m5 = Matrix (Last ( 1 :-: 2 :-: Last 3))
m6 = m4 `mult` m5
m7 = m5 `mult` m4
m8 = m1 `mult` m3
-- causes an errorm9 = m3 `mult` m6
```