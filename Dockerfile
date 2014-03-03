from zsol/haskell-platform-2013.2.0.0:latest
maintainer IHaskell

RUN sudo apt-get update
RUN sudo apt-get install -y pkg-config libtool git automake libncurses-dev

RUN git clone https://github.com/zeromq/zeromq4-x.git libzmq
RUN cd libzmq && ./autogen.sh && ./configure && make && sudo make install && sudo ldconfig && cd ..

RUN git clone https://github.com/gibiansky/IHaskell
RUN echo "PATH=~/.cabal/bin:$PATH" >> ~/.bashrc
RUN cabal update
RUN cabal install happy cpphs
RUN cd IHaskell && ./build.sh all

# what ports are needed for the browser?
# EXPOSE 3000

# default command
CMD /home/haskell/.cabal/bin/IHaskell console
