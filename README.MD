[![Build Status](https://travis-ci.org/kailuowang/tempus.svg?branch=master)](https://travis-ci.org/kailuowang/tempus)

# Tempus
## A library for time series related stuff

### WIP

to benchmark

```
jmh:clean
jmh:compile
jmh:run -i 15 -wi 6 -f1 -t1 .*time
```

To run storage/IntegrationTest/test, it requires an influx instance
Easiest way to setup to a local one is docker

```bash
  docker run -d -p 8086:8086 -v $PWD:/var/lib/influxdb influxdb
```
