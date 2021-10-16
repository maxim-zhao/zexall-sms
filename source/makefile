WLA-Z80 = ../tools/wla-z80
WLALINK = ../tools/wlalink
BINS = zexdoc.sms zexall.sms

all: $(BINS)

zexall.sms.o: zexall.sms.asm
	$(WLA-Z80) -D UndocumentedFlags -o $@ $^

zexall.sms: zexall.sms.o linkfile
	$(WLALINK) -d -r -v -S -A linkfile $@

zexdoc.sms.o: zexall.sms.asm
	$(WLA-Z80) -o $@ $^

zexdoc.sms: zexdoc.sms.o linkfile-doc
	$(WLALINK) -d -r -v -S -A linkfile-doc $@

clean:
	del $(BINS)