# Lisp in Go

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart) and
[lisp-in-cs](https://github.com/nukata/lisp-in-cs).

In 2016, I wrote the original version in Go 1.6 and 1.7.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/golang/lisp4-en.html> (broken link) until 2017.
In 2018, I made the repository in GitHub.
Now, in 2019, I revised it to make use of
[goarith](https://github.com/nukata/goarith), which
implements mixed mode arithmetic of `int32`, `int64`, `float64` and `*big.Int`.

Just as `lisp-in-dart` and `lisp-in-cs`,
this is a Lisp-1 with TCO (tail call optimization)
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp.
Examples are found in 
[lisp-in-cs#examples](https://github.com/nukata/lisp-in-cs#examples).


In addition, this has two concurrent constructs implemented with _goroutine_,
`future` and `force`, which I reported in 2013 at
<http://www.oki-osk.jp/esc/golang/future.html> (broken link).
Thanks to pkelchte, you can get the reported implementation
(that is another Lisp I wrote in Go) at
[pkelchte/tiny-lisp](https://github.com/pkelchte/tiny-lisp) now.

See [IMPLEMENTATION-NOTES.md](IMPLEMENTATION-NOTES.md) for the implementation.


## How to run

```
$ pwd
/Users/suzuki/tmp/lisp-in-go
$ go build
go: downloading github.com/nukata/goarith v0.3.0
go: extracting github.com/nukata/goarith v0.3.0
go: finding github.com/nukata/goarith v0.3.0
$ ./lisp-in-go
> (+ 5 6)
11
> *version*
(2.0 "go1.13.3 darwin/amd64" "Nukata Lisp")
> (exit 0)
$
```

The value of `*version*` will vary depending on the Go compiler.
See [lisp.go L698-L707](lisp.go#L698-L707).

You can give the `lisp` command a file name of your Lisp script.
If you put a "`-`" after the file name, it will
begin an interactive session after running the file.

```
$ cat fib.l
(defun fib (n)
  (if (<= n 1)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))
$ ./lisp-in-go fib.l -
> (fib 10)
55
> (fib 20)
6765
> (setq f (future (fib 30)))
#<future:0xc000060060:(<nil> . <nil>):&{0 0}>
> (force f)
832040
> (exit 0)
$ 
```

Here `(fib 30)` was evaluated concurrently in a new goroutine
and the result was retrieved by `force`.

## License

This is under the MIT License.
See [`lisp.go L1769-L1790`](lisp.go#L1769-L1790).
