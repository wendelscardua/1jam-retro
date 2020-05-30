PROJECT=retropia
LD65_FLAGS=
CA65_FLAGS=
NSF2DATA=/mnt/c/NESDev/famitone2d/NSF/nsf2data.exe
FAMITRACKER=/mnt/c/NESDev/famitracker/FamiTracker.exe

.PHONY : debug run

${PROJECT}.nes: src/${PROJECT}.o src/reset.o src/readjoy.o src/rand.o src/unrle.o src/audio-data.o
	ld65 $^ -t nes -o ${PROJECT}.nes ${LD65_FLAGS}

debug: LD65_FLAGS += -Ln labels.txt --dbgfile ${PROJECT}.nes.dbg
debug: CA65_FLAGS += -g -DDEBUG=1
debug: ${PROJECT}.nes

src/${PROJECT}.o: src/${PROJECT}.s $(shell find assets -type f)
	ca65 src/${PROJECT}.s ${CA65_FLAGS}

src/audio-data.o: src/audio-data.s assets/audio/*.s
	ca65 src/audio-data.s ${CA65_FLAGS}

assets/audio/retropia-sfx.nsf: assets/audio/retropia-sfx.ftm
	${FAMITRACKER} assets/audio/retropia-sfx.ftm -export assets/audio/retropia-sfx.nsf

assets/audio/retropia-sfx.s: assets/audio/retropia-sfx.nsf
	${NSF2DATA} assets/audio/retropia-sfx.nsf -ca65 -ntsc

%.o: %.s
	ca65 $< ${CA65_FLAGS}

clean:
	rm src/*.o *.nes labels.txt

run: debug
	Nintendulator.exe `wslpath -w -a ${PROJECT}.nes`
