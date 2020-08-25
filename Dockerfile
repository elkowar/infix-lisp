FROM fpco/stack-build-small as build


ADD . /app/
WORKDIR /app
RUN stack build --jobs 16
RUN cp "$(stack path --local-install-root)"/bin/* /app/infix-lisp


# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb /app/libgmp.deb


FROM ubuntu:18.04
RUN mkdir -p /app
WORKDIR /app

# Install lib gmp
COPY --from=build /app/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /app/infix-lisp .
CMD /app/infix-lisp
