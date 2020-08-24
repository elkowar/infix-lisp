FROM fpco/stack-build-small

ADD . /app/
WORKDIR /app
RUN ls
RUN stack build --jobs 16
RUN cp "$(stack path --local-install-root)"/bin/* /app/infix-lisp
CMD /app/infix-lisp
