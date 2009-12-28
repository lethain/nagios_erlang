ERL=erl
EBIN=ebin

build:  clean
	erlc *.erl
	mv *.beam $(EBIN)

clean:
	rm -rf $(EBIN)/*.beam
