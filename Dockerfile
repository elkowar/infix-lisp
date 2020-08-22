FROM fpco/stack-build-small

ADD . /app
WORKDIR /app
CMD ["stack" "run"]
