FROM ubuntu:xenial
ENV PATH /root/.local/bin:${PATH}
RUN apt-get update
RUN apt-get install -y wget
RUN mkdir -p /root/.local/bin
RUN wget -O - https://get.haskellstack.org | sh
RUN stack setup --resolver lts-10.5
RUN mkdir /workspace
WORKDIR /workspace
