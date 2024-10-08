
# CL-ROBDD

## Synopsis
Implementation of ROBDD, Reduced Ordered Binary Decision Diagram


The implementation takes the form of a CLOS class `BDD`	


## API using ROBDDs programmatically

* `bdd` -- Clos class representing ROBDD objects
* `bdd` -- the factory function to allocate a BDD.  The object of this function may be any well formed Boolean expression in Lisp form:  E.g., `(bdd '(and (or a b) (not (and c (or d e)))))`
* `bdd-with-new-hash` -- Any access to the machinery in this package must occure within the dynamic extent of this function.
```
PKG> (bdd-with-new-hash ()
       (bdd-to-dnf (bdd-and-not (bdd '(and a b))
			        (bdd '(or (and a c) (or (and a (not c) d)))))))

(AND A B (NOT C) (NOT D))
```

### Algebra of ROBDDs
* `bdd-and` -- Calculate intersection of two BDDs
* `bdd-not` -- Calculate complement of a BDD
* `bdd-and-not` -- Calculate relative complement of two BDDs
* `bdd-or` -- Calculate union of two BDDs
* `bdd-xor` -- Calculate exclusive-or of two BDDs

### Traversal
* `*bdd-reduce-function*` -- has value either  `#'adjuvant:tree-reduce` or `#'adjuvant:linear-reduce`.
This function will be used within bdd-list-to-bdd when to perfrom and, or, and xor on multiple arguments.

* `bdd-bfs` --   Walk a given BDD, object of class, `bdd`, calling the given `FUNCTION` on each node exactly once.
The return value of `FUNCTION` is ignored.

* `bdd-count-nodes` -- walk the giben BDD, counting the nodes.  The
function is careful to avoid counting the same node twice in case of
shared nodes between branches.

* `bdd-to-dnf` -- Return the DNF (disjunctive normal form), of the Boolean expression representing the
given `BDD`.  This DNF generation is lazy and memoized.  The first time `BDD-TO-DNF` is called
the expression is generated and attached to the `BDD` object (via the DNF slot), 
thereafter, the same s-expression is returned.

* `bdd-walk` --   This function starts at the given `BDD`, and walks the dag applying
the `VISITOR-FUNCTION` at each internal node.  As long as the `VISITOR-FUNCTION`
returns `NIL`, the descent continues, at each step constructing a new BDD
(via `BDD-ENSURE-NODE` build of recursive walks of the positive and negative
children).
`VISITOR-FUNCTION` must be a function which returns `NIL` indicating to continue walking
   or a BDD indicating to terminate the descent and return this BDD.


### Serialization 

* `bdd-ident` -- a positive integer unique to the given BDD.  This may be used for printing or debugging purposes.
* `bdd-to-dot` -- Create a graphviz dot file representing the given BDD.
* `bdd-to-png` -- Generate a PNG, graphics, file to
graphically view an ROBDD.  The special var `adjuvant:*DOT-PATH*` is used to
locate the dot, graphviz, program which will convert a `.dot` file to
`.png`. Full path of the `.png` is returned."
* `bdd-view` -- display PNG graphic file of ROBDD (macos only)

## API for extending `bdd` through subclassing

The CLOS class `ltbdd` defined in package [lisp-types](../lisp-types/README.md) is a subclass of `bdd`
for representing ROBDDs which understand Common Lisp type system subclassing.

* `*bdd-cmp-function*` -- Special (dynamic) variable containing the function object to be used
when comparing two ROBDD labels.  This should be a function which returns a symbol `CL:<`, `CL:>`, or `CL:=` deterministically
given two objects.

* `*bdd-false*` -- Singleton FALSE object which is a leaf node of every non-trivial ROBDD.

* `*bdd-hash-strength*` -- Special variable whose type is `(member :weak-dynamic :weak :strong)`.
The value of this variable indicates which hash strategy to use by controling the behavior of `BDD-ENSURE-HASH`.
Each call to `BDD-ENSURE-HASH`: 
1) if `:strong`, will create a new strong hash table. 
2) if `:weak`, will create a new weak hash table.
3) if `:weak-dynamic` -- will create a new hash only if the BDD-NODE-TYPE of the currently available hash table, returned by `(bdd-hash)`, is the same `EQUAL` value of `BDD-NODE-TYPE` passed to `BDD-ENSURE-HASH`, otherwise the current global hash table be used (without allocating a new one).   In any case if there is no global hash table currently available, a new hash will be allocated and returned.

* `*bdd-true*` -- Singleton TRUE object which is a leaf node of every non-trivial ROBDD.

* `bdd-allocate` -- Allocate a new bdd object, whose class is specified by BDD-NODE-CLASS, and
register the object in the global hash indicated by calling (BDD-HASH).  This function should be
called after it has been verified that the positive and negative children are not equal, and
that no such object already exists in the hash table.

* `bdd-call-with-new-hash` -- Functional version of the
`BDD-WITH-NEW-HASH` macro, which takes a 0-ary function to evaluate in
a dynamic extent which rebinds `*BDD-HASH-STRUCT*` and
`*BDD-VERBOSE*`.  *`BDD-HASH-STRUCT* is rebound by a call to
`BDD-ENSURE-HASH` whose behavior depends on the value of `BDD-NODE-TYPE`.
Whenever a new element is added to the hash table (via `BDD-ALLOCATE`)
an assertion is made that the object is of this type."

* `bdd-dnf-wrap` --  Given a `BDD` object, an `OPERATOR`, `ZERO`, and a list `FORMS`,
return a Boolean expression as simply as possible representing the a application of
OPERATOR to FORMS.  The methods of this generic function are free to make further
reductions depending on the class of `BDD` given.
The method on the class `BDD` is responsibe for returning
an s-expression representing a Boolean expression
which combines the `TERMS` with the given `OPERATOR`.  E.g., `(and T1 T2 T3)`.
A couple of corner cases are considered.  If TERMS is the empty list, then
the given `ZERO` is returned.   `ZERO` should be `NIL` when `OPERATOR` is `OR` and it should
be `T` when `OPERATOR` is `AND`.  If `TERMS` is a singleton list, its first element is returned.
I.e., rather than returning `(or X)`, simply `X` is returned; and rather than returning `(and)`,
`T` is returned'.

* `bdd-factory` -- Given a class or class-name return the factory function for instantiating a BDD object.
The factor-function is a unary function which can accept a Boolean expresion as an s-expression,
and return a BDD representing that expression.

* `bdd-false` -- Class of singleton object representing the TRUE leaf node of an ROBDD.  The singleton object is `*bdd-false*`.

* `bdd-find` -- Search the hash table returned from `(BDD-HASH)` to determine
whether there is already a `BDD` whose label is `LABEL` having
the two given children.

* `(setf bdd-find)` -- `setf` accessor for `bdd-find`.

* `bdd-label` -- Reader for the LABEL slot which represents the Boolean variable corresponding to this ROBDD node.

* `bdd-leaf` -- class -- Class reprenting leaf nodes of an ROBDD.  The two possible leaf
node objects are `*BDD-FALSE*` and `*BDD-TRUE*`, which are singleton instances of
`BDD-FALSE` and `BDD-TRUE`.

* `bdd-leaf` -- generic function -- Given `T` or `NIL`, return the corresponding leaf node `*BDD-TRUE*` or `*BDD-FALSE*`.

* `bdd-negative` -- Read accessor for the NEGATIVE slot of a bdd-node.  Returns the negative child.

* `bdd-node` -- class -- subclass of `BDD` representing internal nodes of a BDD, i.e., nodes having
two children, accessible by the reader functions `bdd-positive` and `bdd-negative`.

* `bdd-node` -- generic function -- Create a new instance of
`BDD-NODE` class if necessary, via a call to `BDD-ENSURE-NODE`.  This
generic function accepts `t` and `nil` as positive and negative children
along with other BDD nodes.

* `bdd-positive` --  Read accessor for the POSITIVE slot of a bdd-node.  Returns the positive child.

* `bdd-to-expr` -- Return a Boolean expression representing the given BDD.  This expression is not
necessarly the DNF form, rather it is the easiest expression to generate.  The expression
is formed by accessing the `EXPR` slot of the given `BDD`.  Since the slot is lazily 
initialized, the first call to `BDD-TO-EXPR` may be more time consuming than subsequent
calls with the same BDD.
The value calculated is a Boolean expression which is easier to calculate
than the DNF form.   The DNF form would require traversal of the entire BDD
blow this point.  By contrast, calculating the EXPR slot for a node who
does NOT have terminals as children nodes is simply:

`EXPR = (or (and label P) (and (not label) N))`

where P is the EXPR slot of the positive child `(bdd-to-expr (bdd-positive bdd))`

and N is the EXPR slot of the negative child `(bdd-to-expr (bdd-negative bdd))`.
  
There is additional simplification if one of the child nodes is `*bdd-false*`
or `*bdd-true*`.

* `bdd-true` -- Class of singleton object representing the TRUE leaf node of an ROBDD. The singleton object is `*bdd-true*`.


## License

~~~~
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
~~~~
