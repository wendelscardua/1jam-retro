PROJECT=retropia
LD65_FLAGS=
CA65_FLAGS=

${PROJECT}.nes: src/${PROJECT}.o src/reset.o src/readjoy.o src/rand.o src/unrle.o
	ld65 $^ -t nes -o ${PROJECT}.nes ${LD65_FLAGS}

debug: LD65_FLAGS += -Ln labels.txt --dbgfile ${PROJECT}.nes.dbg
debug: CA65_FLAGS += -g
debug: ${PROJECT}.nes

src/${PROJECT}.o: src/${PROJECT}.s $(shell find assets -type f) # assets/music/*
	ca65 src/${PROJECT}.s ${CA65_FLAGS}

%.o: %.s
	ca65 $<

clean:
	rm src/*.o *.nes labels.txt
