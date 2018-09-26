# CL-ROBDD

## Synopsis
Implementation of ROBDD, Reduced Ordered Binary Decision Diagram

The implementation takes the form of a CLOS class `BDD`	


## API using ROBDDs programmatically

* `bdd` -- Clos class representing ROBDD objects
* `bdd` -- the factory function to allocate a BDD.  The object of this function may be any well formed Boolean expression in Lisp form:  E.g., `(bdd '(and (or a b) (not (and c (or d e)))))`
* `bdd-with-new-hash` -- Any access to the machinery in this package must occure within the dynamic extent of this function.
````
PKG> (bdd-with-new-hash ()
       (bdd-to-dnf (bdd-and-not (bdd '(and a b))
			        (bdd '(or (and a c) (or (and a (not c) d)))))))

(AND A B (NOT C) (NOT D))
````

### Algebra of ROBDDs
* `bdd-and` -- Calculate intersection of two BDDs
* `bdd-not` -- Calculate complement of a BDD
* `bdd-and-not` -- Calculate relative complement of two BDDs
* `bdd-or` -- Calculate union of two BDDs
* `bdd-xor` -- Calculate exclusive-or of two BDDs

### Traversal
* `*bdd-reduce-function*` -- has value either  `#'tree-reduce` or `#'cl:reduce`.
This function will be used within bdd-list-to-bdd when to perfrom and, or, and xor on multiple arguments.

* `bdd-bfs` --   Walk a given BDD, object of class, `bdd`, calling the given `FUNCTION` on each node exactly once.
The return value of `FUNCTION` is ignored.

* `bdd-to-dnf` -- Return the DNF (disjunctive normal form), of the Boolean expression representing the
given `BDD`.  This DNF generation is lazy and memoized.  The first time `BDD-TO-DNF` is called
the expression is generated and attached to the `BDD` object (via the DNF slot), 
thereafter, the same s-expression is returned.



### Serialization 

* `*dot-path*` -- Special variable containing the full path to the UNIX `dot` (graphviz) program.
* `bdd-to-png` -- Generate a PNG, graphics, file to
graphically view an ROBDD.  The special var `*DOT-PATH*` is used to
locate the dot, graphviz, program which will convert a `.dot` file to
`.png`. Full path of the `.png` is returned."


## API for extending `bdd` through subclassing

The CLOS class `ltbdd` defined in package [lisp-types](../lisp-types/README.md) is a subclass of `bdd`
for representing ROBDDs which understand Common Lisp type system subclassing.

* `bdd-call-with-new-hash` --
* `incr-hash` --
* `bdd-true` --
* `bdd-to-expr` --
* `bdd-serialize` --
* `bdd-positive` --
* `bdd-node-type` --
* `bdd-node` --
* `bdd-list-to-bdd` --
* `bdd-new-hash` --
* `bdd-negative` --
* `bdd-make-key` --
* `bdd-leaf` --
* `bdd-label` --
* `bdd-ident` --
* `bdd-find` --
* `bdd-hash` --
* `bdd-false` --
* `%bdd-cmp` --
* `%bdd-node` --
* `*bdd-cmp-function*` --
* `*bdd-false*` --
* `*bdd-hash-strength*` --
* `*bdd-hash-struct*` --
* `*bdd-true*` --
* `bdd-allocate` --
* `bdd-dnf-wrap` --
* `bdd-cmp` --
* `bdd-factory` --


## License

```
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
```
