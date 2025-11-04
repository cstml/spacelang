FROM cstml/sbcl AS builder
WORKDIR spacelang
RUN apt install -y make
RUN apt update
RUN apt install -y glibc-source
COPY ./ ./
RUN make

FROM debian AS runner
WORKDIR app
RUN apt update && apt install -y rlwrap
COPY --from=builder /root/.quicklisp/local-projects/spacelang/bin ./bin
CMD ["rlwrap", "./bin/spci"]
