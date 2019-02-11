# Lisp in Go

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart).

I wrote the original version three years ago (2016) in Go 1.6 and 1.7.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/golang/lisp4-en.html> (broken link) until 2017.
Now I revised it to use [goarith](https://github.com/nukata/goarith).

Just as lisp-in-dart, this is a Lisp-1 with TCO (tail call optimization)
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp.
Examples are found in 
[lisp-in-dart/examples](http://github.com/nukata/lisp-in-dart/tree/master/examples).

In addition, this has two concurrent constructs implemented with _goroutine_,
`future` and `force`, which I reported in 2013 at
<http://www.oki-osk.jp/esc/golang/future.html> (broken link).
Thanks to pkelchte, you can get the reported implementation
(that is another Lisp I wrote in Go) at
[pkelchte/tiny-lisp](https://github.com/pkelchte/tiny-lisp) now.

See [IMPLEMENTATION-NOTES.md](IMPLEMENTATION-NOTES.md) for the implementation.


## How to run

```
$ go build lisp.go
$ ./lisp
> (+ 5 6)
11
> *version*
(2.0 "go1.11.5 darwin/amd64" "Nukata Lisp")
> (exit 0)
$
```

The value of `*version*` will vary depending on the Go compiler.
See [lisp.go L692-L701](lisp.go#L692-L701).

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
$ ./lisp fib.l -
> (fib 10)
55
> (fib 20)
6765
> (setq f (future (fib 30)))
#<future:0xc00006e060:(<nil> . <nil>):&{0 0}>
> (force f)
832040
> (exit 0)
$ 
```

Here `(fib 30)` was evaluated concurrently in a new goroutine
and the result was retrieved by `force`.

## License

This is under the MIT License.
See [`lisp.go L1763-L1784`](lisp.go#L1763-L1784).
