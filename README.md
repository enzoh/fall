# Winter ❄️

> A WebAssembly interpreter.

# Build
Run an interactive [Docker](https://www.docker.com) container that ships the required dependencies.
```
docker build -t enzoh/winter .
docker run --interactive --rm --tty --volume `pwd`:/workspace enzoh/winter
```
Invoke [Stack](https://haskellstack.org) from inside the container.
```
stack build
```
