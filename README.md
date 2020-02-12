# thorough

One issue with using spec check is that while developing you can't really know
that all code paths were hit during checking. This tool instruments your files
and run checks against all registered fdefs. It then generates a report
summarizing the results.

For example, running the following in this repo will produce the following
report:

```
clj -A:sample -m thorough.main ./sample.edn
```

```
|                         Symbol | Check |        Hit | Miss |
|--------------------------------+-------+------------+------|
|      thorough.sample/odd-adder |  true | 7, 4, 6, 5 |      |
| thorough.sample/with-odd-adder |  true |      9, 10 |      |
|  thorough.sample/calls-boundry |  true |     15, 16 |      |
```

`./sample.edn` is an edn file with the following configuration:

```
{:ns-paths ["sample"]
 :file->ns {"thorough/sample.clj" thorough.sample
            "thorough/sample_instrumented.clj" thorough.sample-instrumented}
 :check-opts {thorough.sample/calls-boundry
              {:pre thorough.sample-instrumented/pre-calls-boundry
               :post thorough.sample-instrumented/post-calls-boundry}}}
```
