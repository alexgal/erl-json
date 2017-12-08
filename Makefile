ERL = erl -boot start_clean

run: compile
	${ERL} -pa './ebin'

compile: 
	${ERL} -make

clean:
	rm -rf ebin/*.beam erl_crash.dump