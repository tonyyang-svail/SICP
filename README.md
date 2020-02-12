# SICP

## Build Docker Image

```bash
docker build -t tonyyang/scheme .
```

## Test

```bash
docker run -it -v $PWD:/work -w /work tonyyang/scheme bash scripts/test.sh
```
