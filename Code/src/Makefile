CC=ghc

haskellothello: *.hs
	${CC} -o haskellothello Main.hs

server: *.hs
	${CC} -o haskellothello-server ServerMain.hs

clean:
	rm -f *.hi *.o haskellothello haskellothello-server
	rm -rf dist
