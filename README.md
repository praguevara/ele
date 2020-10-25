# ELE

This is a simple interpreter for the subject TC in the University of Alicante.

## Usage

```
Usage: ele FILE [-X|--input INPUT]
  An L interpreter. By praguevara.

Available options:
  -h,--help                Show this help text
  FILE                     Source file
  -X,--input INPUT         -X [3, 1] means that X = 3, X1 = 1
```

## Source files

Important: new line at the end!

```
(A) IF X != 0 GOTO B
Z++
IF Z != 0 GOTO S
(B) X--
Y++
Y++
Y++
Z++
IF Z != 0 GOTO A

```

## Example output

```
./ele triple.l -X [2]
```

```
        Labels:
0:      (A)
3:      (B)
9:      (S)

        Sentences:
0:      IF X != 0 GOTO (B)
1:      Z++
2:      IF Z != 0 GOTO (S)
3:      X--
4:      Y++
5:      Y++
6:      Y++
7:      Z++
8:      IF Z != 0 GOTO (A)


m = 0
variables: [(X,2)]

m = 3
variables: [(X,2)]

m = 4
variables: [(X,1)]

m = 5
variables: [(X,1),(Y,1)]

m = 6
variables: [(X,1),(Y,2)]

m = 7
variables: [(X,1),(Y,3)]

m = 8
variables: [(X,1),(Y,3),(Z,1)]

m = 0
variables: [(X,1),(Y,3),(Z,1)]

m = 3
variables: [(X,1),(Y,3),(Z,1)]

m = 4
variables: [(X,0),(Y,3),(Z,1)]

m = 5
variables: [(X,0),(Y,4),(Z,1)]

m = 6
variables: [(X,0),(Y,5),(Z,1)]

m = 7
variables: [(X,0),(Y,6),(Z,1)]

m = 8
variables: [(X,0),(Y,6),(Z,2)]

m = 0
variables: [(X,0),(Y,6),(Z,2)]

m = 1
variables: [(X,0),(Y,6),(Z,2)]

m = 2
variables: [(X,0),(Y,6),(Z,3)]

Y = 6
```