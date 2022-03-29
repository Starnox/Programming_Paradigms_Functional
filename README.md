
===== Usage

Test suites are represented by a tuple.

``` Haskell
task1 = ("Task 1", [    
		expect 2.3 toRoundTo 2,    
                expect "a" toBe "a",    
                expect True toNotBe False    
        ]) 
```

We register tests by adding them to `test_suites` list.
