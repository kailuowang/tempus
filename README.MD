#WIP

to benchmark

```
jmh:clean
jmh:compile
jmh:run -i 15 -wi 6 -f1 -t1 .*time
```

To run test, it requires 

* influx
  ```
  influxd -config /usr/local/etc/influxdb.conf
  ```
