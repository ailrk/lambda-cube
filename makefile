LC:
	ghc LC/LC.hs -o ./bin/LC

STLC:
	ghc STLC/STLC.hs -o ./bin/STLC

PCF:
	ghc PCF/PCF.hs -o ./bin/PCF

HM:
	ghc HM/HM.hs -o ./bin/HM

LP:
	ghc LP/LP.hs -o ./bin/LP

.PHONY: clean
clean:
	@rm *.o
	@rm *.hi
	@rm bin/*
	@rm hm
