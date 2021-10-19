WLA-Z80 = wla-z80
WLALINK = wlalink
BMP2TILE = bmp2tile
GENERATED_FILES = *.sms *.o *.makefile *.1bpp *.sym *.zip
7Z = 7z

all: zexall.zip

# We get WLA DX to generate the dependencies list for our source
zexall.makefile: zexall.sms.asm
	$(WLA-Z80) -M -D UndocumentedFlags -o zexall.sms.o $^ > $@

zexdoc.makefile: zexall.sms.asm
	$(WLA-Z80) -M -o zexdoc.sms.o $^ > $@

include zexall.makefile
include zexdoc.makefile

# Graphics conversion
%.1bpp: %.png
	 bmp2tile "$<" -noremoveduplicates -savetiles "$@"

zexall.sms.o: zexall.sms.asm
	$(WLA-Z80) -D UndocumentedFlags -o $@ $<

zexall.sms: zexall.sms.o linkfile
	$(WLALINK) -d -r -v -S -A linkfile $@

zexdoc.sms.o:
	$(WLA-Z80) -o $@ $<

zexdoc.sms: zexdoc.sms.o linkfile-doc
	$(WLALINK) -d -r -v -S -A linkfile-doc $@

zexall.zip: zexall.sms zexdoc.sms ..\README.md ..\LICENSE
	7z a $@ $^
	7z a $@ ../source $(addprefix -xr!, $(GENERATED_FILES))

clean:
	del $(GENERATED_FILES)