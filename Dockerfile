# run this with a terminal
# sudo docker run -rm -p 8778:8778 -i -t <image>

# browser, but the port is not visible
# sudo docker run -rm -i <image> IHaskell notebook

from zsol/haskell-platform-2013.2.0.0:latest
maintainer IHaskell

RUN sudo apt-get update
RUN sudo apt-get install -y pkg-config libtool git automake libncurses-dev python-dev

RUN git clone https://github.com/zeromq/zeromq4-x.git libzmq
RUN cd libzmq && ./autogen.sh && ./configure && make && sudo make install && sudo ldconfig && cd ..

RUN cabal update
RUN cabal install happy cpphs

# use local modifications
ADD . /home/haskell/IHaskell
# use master
# RUN git clone https://github.com/gibiansky/IHaskell
RUN cd IHaskell && ./build.sh all

ENV PATH /home/haskell/.cabal/bin:$PATH

# The first time this runs it will install stuff
RUN IHaskell console

# for IHaskell browser
ENV IHASKELL_NOTEBOOK_EXPOSE 1
EXPOSE 8778

CMD IHaskell console
