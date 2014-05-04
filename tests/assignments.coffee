foo = bar
foo().bar = baz
(foo().bar 3).baz = qux

[foo, bar] = baz
[foo; bar] = baz
[foo, bar().baz, qux.foo(bar, baz).qux] = foo
[foo, bar; baz] = qux
[ foo
  bar; baz
  baz, qux;
  foo.bar.baz
] = foo
