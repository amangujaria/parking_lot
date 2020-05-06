FROM ubuntu:16.04

ENV REFRESHED_AT=2018-08-16 \
    LANG=en_US.UTF-8 \
    HOME=/opt/build \
    TERM=xterm

WORKDIR /opt/build
RUN cd parking_lot
RUN ./rebar compile
RUN ./rebar3 as prod tar
COPY _build/prod/rel/parking_lot/*.tar.gz /opt/build/

RUN \
  apt-get update -y && \
  apt-get install -y git wget vim locales && \
  locale-gen en_US.UTF-8
