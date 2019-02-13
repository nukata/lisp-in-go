# Implementation Notes


<a name="1"></a>
## 1. Overview

The Lisp implementation of [lisp.go](lisp.go) is a translation of lisp.dart 
at [lisp-in-dart](https://github.com/nukata/lisp-in-dart).
It provides `future` and `force` for concurrent computation with _goroutines_.

Below is an example of running lisp.go.

```
$ pwd
/Users/suzuki/tmp/lisp-in-go
$ go build
go: finding github.com/nukata/goarith v0.2.0
go: downloading github.com/nukata/goarith v0.2.0
$ ./lisp-in-go
> (+ 5 6)
11
> (exit 0)
$ 
```

Some features common to lisp.go and lisp.dart are

- It is basically a subset of Emacs Lisp.
  However, it is a Lisp-1 with static scoping.
  In short, it is a _Common Lisp-like Lisp-1_.

- It makes proper tail calls always.

- A quasi-quotation with backquote will be expanded when macros are expanded.

- A circular list is printed with `...` finitely.

- As an escape sequence within strings, you can use any of
  `\"`, `\\`, `\n`, `\r`, `\f`, `\b`, `\t`, `\v`.

- `(dump)` returns a list of all global variables.
  The list does not include special forms such as `lambda` and `setq`
  since they are not variables.

- `*version*` is a three-element list: 
  the (internal) version number, the implementing language, 
  and the name of implementation.

- (`macro` _args_ _body_) is a special form that evaluates to a sort of
  anonymous function, or _macro expression_.
  The global environment will be used whenever (`macro` ...) evaluates.
  When you apply the resultant macro expression to a list of actual arguments,
  the arguments will not be evaluated and the result of the application
  will be evaluated again.
  Thus a variable bound to a macro expression works as a _macro_.

- `defmacro` is a macro which binds a variable to a macro expression.

- `defun` is a macro which binds a variable to a lambda expression.

- `let` is a macro which applies a lambda expression to a list of initial
  values of variables.

- Free symbols within a macro expression will not be captured when the
  expression is applied (i.e., when the macro is expanded).


By the last feature above, you can avoid variable captures in any macro
definitions, provided that you use the result of `(gensym)` for any symbol
newly introduced to the expansion result.
See [lisp-in-dart/IMPLEMENTATION-NOTES §5](https://github.com/nukata/lisp-in-dart/blob/master/IMPLEMENTATION-NOTES.md#5).

----------------------------------------

**Note:**
I believe the last feature makes the behavior of traditional macros ideal.
As macros being _partially hygienic_, you can define
[anaphoric macros](http://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/anaphoricMacros.html)
(Japanese) by introducing a symbol (`it` in the following example) to the 
expansion result intentionally without using `(gensym)`.

```
> (defmacro aif (test then else)
    `(let ((it ,test))
       (if it ,then ,else) ))
aif
> (aif (+ 7 8 9)
     (print it)
    (print "?"))
24
24
> 
```

----------------------------------------

In addition, there is a feature inherited from
[my first Go Lisp in 2013](https://github.com/pkelchte/tiny-lisp):

- Concurrent computations with goroutines are delivered by the special form 
  (`future` _expression_) and the function (`force` _future_).
  See ["Futures and promises" at Wikipedia](https://en.wikipedia.org/wiki/Futures_and_promises).

The following example calculates 
[Fibonacci numbers](https://oeis.org/A000045)
concurrently, though it may be too fine-grained to be efficient.

```
> (defun fib (n)
    (if (<= n 1)
        n
      (let ((a (future (fib (- n 1))))
            (b (future (fib (- n 2)))))
        (+ (force a)
           (force b) ))))
fib
> (fib 10)
55
> (fib 20)
6765
> (fib 30)
832040
> 
```

`fib` computes `(fib (- n 1))` and `(fib (- n 2))` 
in each goroutine separately and adds the results 
by `(+ (force a) (force b))`.



<a name="2"></a>
## 2. Internal Data Representation

To represent data of the implemented language (Lisp), native types of the 
implementing language (Go) are used as they are, if possible.
They are all treated as `interface{}` uniformly.


| Lisp Expression                     | Internal Representation                |
|:------------------------------------|:---------------------------------------|
| numbers `1`, `2.3`                  | [`goarith.Number`](https://github.com/nukata/goarith) |
| strings `"abc"`, `"hello!\n"`       | `string`                               |
| `t`                                 | `true`                                 |
| `nil`                               | `nil` of `*Cell`                       |
| symbols `x`, `+`                    | `Sym` (user-defined)                   |
| keywords `lambda`, `cond`           | `Sym` (`IsKeyword` flag is `true`)     |
| lists `(x 1 "2")`, `(y . 3)`        | `Cell` (user-defined)                  |

Below is the definition of the type `Cell`.

```Go
// Cell represents a cons cell.
// &Cell{car, cdr} works as the "cons" operation.
type Cell struct {
	Car interface{}
	Cdr interface{}
}

// Nil is a nil of type *Cell and it represents the empty list.
var Nil *Cell = nil
```

Below is the definition of the type `Sym`.

```Go
// Sym represents a symbol (or an expression keyword) in Lisp.
// &Sym{name, false} constructs a symbol which is not interned yet.
type Sym struct {
	Name      string
	IsKeyword bool
}
```

The map `symbols` is used to intern symbols.
The mutex `symLock` guards access to `symbols` in
concurrent computations with goroutines.

```Go
// NewSym constructs an interned symbol for name.
func NewSym(name string) *Sym {
	return NewSym2(name, false)
}

// symbols is a table of interned symbols.
var symbols = make(map[string]*Sym)

// symLock is an exclusive lock for the table.
var symLock sync.RWMutex

// NewSym2 constructs an interned symbol (or an expression keyword
// if isKeyword is true on its first construction) for name.
func NewSym2(name string, isKeyword bool) *Sym {
	symLock.Lock()
	sym, ok := symbols[name]
	if !ok {
		sym = &Sym{name, isKeyword}
		symbols[name] = sym
	}
	symLock.Unlock()
	return sym
}
```


<a name="3"></a>
## 3. Implementations of Lisp functions

The core of Lisp interpreter is represented by the structure `Interp`.
It consists of a map for global variables and an exclusive lock for the map.

```Go
// Interp represents a core of the interpreter.
type Interp struct {
	globals map[*Sym]interface{}
	lock    sync.RWMutex
}
```

Each built-in Lisp function is defined with the utility function below.
The `carity` argument takes the arity of the function to be defined.
If the function has `&rest`, the `carity` 
takes `-(`_number of fixed arguments_ ` + 1)`.

```Go
// Def defines a built-in function by giving a name, arity, and body.
func (interp *Interp) Def(name string, carity int,
	body func([]interface{}) interface{}) {
	sym := NewSym(name)
	fnc := NewBuiltInFunc(name, carity, body)
	interp.SetGlobalVar(sym, fnc)
}
```

Below is an excerpt of the function `NewInterp` corresponding to the 
constructor of `Interp`.
It shows the implementation of five elementary functions of Lisp.

```Go
// NewInterp constructs an interpreter and sets built-in functions etc. as
// the global values of symbols within the interpreter.
func NewInterp() *Interp {
	interp := &Interp{globals: make(map[*Sym]interface{})}

	interp.Def("car", 1, func(a []interface{}) interface{} {
		if a[0] == Nil {
			return Nil
		}
		return a[0].(*Cell).Car
	})

	interp.Def("cdr", 1, func(a []interface{}) interface{} {
		if a[0] == Nil {
			return Nil
		}
		return a[0].(*Cell).Cdr
	})

	interp.Def("cons", 2, func(a []interface{}) interface{} {
		return &Cell{a[0], a[1]}
	})

	interp.Def("atom", 1, func(a []interface{}) interface{} {
		if j, ok := a[0].(*Cell); ok && j != Nil {
			return Nil
		}
		return true
	})

	interp.Def("eq", 2, func(a []interface{}) interface{} {
		if a[0] == a[1] { // Cells are compared by address.
			return true
		}
		return Nil
	})
```

The function `dump` takes no arguments and returns a list of all global
variables.
Within read-lock of `interp.lock`, `dump` reads the keys from `interp.globals`
and constructs a list of them.

```Go
	interp.Def("dump", 0, func(a []interface{}) interface{} {
		interp.lock.RLock()
		defer interp.lock.RUnlock()
		r := Nil
		for key := range interp.globals {
			r = &Cell{key, r}
		}
		return r
	})
```

Below is an example of running `(dump)`.

```
> (dump)
(append > cdaar + last or symbol-name prin1 / stringp assoc assq consp caddr def
macro princ < when setcdr truncate - list eq dump apply intern equal not caar fo
rce nreverse _nreverse caadr make-symbol dolist listp * member setcar *version* 
cdr dotimes cdddr cdadr cdar car while _append if >= print exit *gensym-counter*
 length let /= = and letrec <= cons nconc cddr cadr gensym memq defun % cadar ca
aar mod mapcar eql rplaca terpri numberp rplacd atom null identity cddar)
> 
```

Several functions and macros of Lisp are defined in the initialization script
`Prelude`.
It runs at the beginning of the `Main` function:

```Go
// Main runs each element of args as a name of Lisp script file.
// It ignores args[0].
// If it does not have args[1] or some element is "-", it begins REPL.
func Main(args []string) int {
	interp := NewInterp()
	ss := strings.NewReader(Prelude)
	if !Run(interp, ss) {
		return 1
	}
	if len(args) < 2 {
		args = []string{args[0], "-"}
	}
	for i, fileName := range args {
		if i == 0 {
			continue
		}
		if fileName == "-" {
			Run(interp, nil)
			fmt.Println("Goodbye")
		} else {
			file, err := os.Open(fileName)
			if err != nil {
				fmt.Println(err)
				return 1
			}
			if !Run(interp, file) {
				return 1
			}
		}
	}
	return 0
}

func main() {
	os.Exit(Main(os.Args))
}
```

Below is the head of `Prelude`.

```Lisp
// Prelude is an initialization script of Lisp.
// Each "~" is replaced by "`" at runtime.
var Prelude = strings.Replace(`
(setq defmacro
      (macro (name args &rest body)
             ~(progn (setq ,name (macro ,args ,@body))
                     ',name)))

(defmacro defun (name args &rest body)
  ~(progn (setq ,name (lambda ,args ,@body))
          ',name))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
```

The tail of `Prelude` is as follows:

```Lisp
(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)
  (let ((name (car spec))
        (count (gensym)))
    ~(let ((,name 0)
           (,count ,(cadr spec)))
       (while (< ,name ,count)
         ,@body
         (setq ,name (+ ,name 1)))
       ,@(if (cddr spec)
             ~(,(caddr spec))))))
`, "~", "`", -1)
```

Since "``` ` ```" cannot occur within a raw string literal of Go,
"`~`" substitues for it.
Each  "`~`" will be replaced by "``` ` ```" at runtime with `strings.Replace`.
