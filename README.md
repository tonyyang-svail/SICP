# Structure and Interpretation of Computer Programs

This repo contains learning notes on [Structure and Interpretation of Computer Programs](http://sarabander.github.io/sicp/html/index.xhtml).

## Build Docker Image

```bash
docker build -t tonyyang/scheme .
```

## Test

```bash
docker run -it -v $PWD:/work -w /work tonyyang/scheme bash scripts/test.sh
```

## Bofore You Start

1. [Learn Scheme in 15 minutes](https://web-artanis.com/scheme.html)

