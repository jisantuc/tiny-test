# tiny-test

Like [`tiny-servant`](https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/), but for testing frameworks

This repo holds a tiny testing framework meant to illustrate the increase
in power from unit to property testing.

It also includes a test suite that showcases its use. You can run the test
suite with [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install) -- run `stack test` from the repository root.

It doesn't do anything fancy like
parallel test execution or shrinking property test examples or cool recursive
types stuff with `prop` like HSpec does since that's not really the point. You
should not consider this for your actual testing needs, and it will not be
published.