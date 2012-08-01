

all: parallel compression

parallel: src/TreeBase.hs src/FPTree.hs src/ParallelFP.hs
	ghc --make src/ParallelFP.hs -isrc -o ParallelFP -outputdir tmp

compression: src/TreeBase.hs src/FPTree.hs src/EdenMapReduce.hs src/Compression.hs
	ghc -parmpi --make src/Compression.hs -isrc -o Compression -outputdir tmp

clean:
	rm -R tmp
