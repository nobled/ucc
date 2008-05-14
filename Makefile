UCCDIR = /usr/local/lib/ucc
export UCCDIR

all:
	make -C driver
	make -C ucl

clean:
	make -C driver clean
	make -C ucl clean
	
install:
	mkdir -p $(UCCDIR)
	cp driver/ucc $(UCCDIR)
	cp ucl/ucl $(UCCDIR)
	cp ucl/assert.o $(UCCDIR)
	cp -r ucl/linux/include $(UCCDIR)

test:
	make -C ucl test

