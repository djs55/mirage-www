When a unikernel is running, it's important to be able answer questions like:

- how much memory is it using? how is this changing over time?
- how many requests per second is it currently processing?
- how much raw bandwidth is being consumed?
- how many requests are failing?

The mirage.io website now has a
[graphical stats page](https://mirage.io/stats/gc)
which shows live timeseries data, available at different resolutions to help you
see the trends.

It starts with a single counter
-------------------------------

In various places in the unikernel code we increment counters, for example
[when a request fails](https://github.com/mirage/mirage-www/blob/0b915191606f33ba295a51c9786376f1f6f57d96/src/dispatch.ml#L90).
These counters are wrapped as "datasources": a record with the following
definition:

```
  type t = {
    name: string;
    description: string;
    value: Rrd.ds_value_type;
    ty: Rrd.ds_type;
    max: float;
    min: float;
    units: string;
  }
```

The datasources are constructed
https://github.com/mirage/mirage-www/blob/0b915191606f33ba295a51c9786376f1f6f57d96/src/stats.ml#L60
