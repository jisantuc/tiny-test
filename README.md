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

## Good talks on property testing

- Luís Rodrigues - [More tests, less work](https://www.youtube.com/watch?v=PZskhUemFlc). This talk does a good job introducing the basic vocabulary of property-based testing, why you might prefer it to example-based testing, and some of the basic intuitions of property-based testing, e.g., "if I provide a number, it should always work," etc. "Example based testing is good for most cases, but sometimes you feel it's not enough... how many tests do I need to write? ... are you maintaining your relevant tests?" 
- John Hughes - [Building on developers' intuitions to create effective property-based tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ). This talk explores some of the extra features of QuickCheck, which is I think the original framework. It covers things like labeling and getting summary stats about your tests, making sure that certain cases are covered a certain percentage of the time, and a migration from unit tests to property tests.
- Susan Potter - [Thinking in Properties](https://www.youtube.com/watch?v=PcOcgMm8_4s). This talk takes property testing past just testing functions into _testing distributed systems_. It also discusses how you can think about whether you're writing useful tests and using properties (and property tests) to formalize your thinking about your problem domain. "...really nice that we weren't constrained by the imagination of the tester." 🙃 
