FROM ubuntu

ARG SBT_VERSION=1.5.5

ENV PATH=$PATH:/sbt/bin
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y \
        curl \
        git \
        openjdk-11-jdk \
    && curl -L -o sbt-${SBT_VERSION}.tgz https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz \
    && tar -xvzf sbt-${SBT_VERSION}.tgz \
    && rm sbt-${SBT_VERSION}.tgz \
    && sbt -V

WORKDIR /

CMD [ "bin/bash" ]
