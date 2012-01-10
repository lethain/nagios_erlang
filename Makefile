ERL=erl
EBIN=ebin

build:
	@$(ERL) -make

clean:
	rm -rf $(EBIN)/*.beam
