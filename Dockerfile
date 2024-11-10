FROM cstml/sbcl
WORKDIR spacelang
RUN apt install -y make 
RUN apt update  
RUN apt install -y glibc-source rlwrap
COPY ./ ./
RUN make
CMD ["rlwrap", "./bin/spci"]
