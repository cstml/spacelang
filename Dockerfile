FROM cstml/sbcl
WORKDIR spacelang
RUN apt install -y make 
RUN apt update  
RUN apt install -y glibc-source
COPY ./ ./
RUN make
CMD "./bin/spci"
