YEAR=2021

# Download input if not present
%i: %.input phony
	@

# Download input if not present, rebuild exe if source is updated, compute output (for first part)
%c: %.input %.update_output.1 phony
	@

# Submit last computed output (for first part) (do the above if necessary)
%s: %.input %.output.1 %.submit.1 phony
	@

# Download input if not present, rebuild exe if source is updated, compute output (for second part)
%C: %.input %.update_output.2 phony
	@

# Submit last computed output (for second part) (do the above if necessary)
%S: %.input %.output.2 %.submit.2 phony
	@

%.input: session_cookie
	curl 'https://adventofcode.com/$(YEAR)/day/$(patsubst %.input,%,$@)/input' -H 'cookie: session='"$$(cat session_cookie)" --fail-with-body -o $@ || (cat $@ && rm $@)
	cat $@

%.exe: %.hs
	ghc -O2 -o $@ $<

%.output.1: %.input %.exe
	./$(patsubst %.output.1,%.exe,$@) 1 < $(patsubst %.output.1,%.input,$@) | tee $@

%.output.2: %.input %.exe
	./$(patsubst %.output.2,%.exe,$@) 2 < $(patsubst %.output.2,%.input,$@) | tee $@

%.update_output.1: %.input %.exe phony
	./$(patsubst %.update_output.1,%.exe,$@) 1 < $(patsubst %.update_output.1,%.input,$@) | tee $(patsubst %.update_output.1,%.output.1,$@)

%.update_output.2: %.input %.exe phony
	./$(patsubst %.update_output.2,%.exe,$@) 2 < $(patsubst %.update_output.2,%.input,$@) | tee $(patsubst %.update_output.2,%.output.2,$@)

%.submit.1: %.output.1
	curl 'https://adventofcode.com/$(YEAR)/day/$(patsubst %.submit.1,%,$@)/answer' -H 'cookie: session='"$$(cat session_cookie)" -X POST -H 'content-type: application/x-www-form-urlencoded' --data-raw 'level=1&answer='"$$(cat $<)"

%.submit.2: %.output.2
	curl 'https://adventofcode.com/$(YEAR)/day/$(patsubst %.submit.2,%,$@)/answer' -H 'cookie: session='"$$(cat session_cookie)" -X POST -H 'content-type: application/x-www-form-urlencoded' --data-raw 'level=2&answer='"$$(cat $<)"

all_exes: 1.exe 2.exe 3.exe 4.exe 5.exe 6.exe 7.exe 8.exe 9.exe 10.exe 11.exe 12.exe 13.exe 14.exe 15.exe 16.exe 17.exe 18.exe 19.exe 20.exe 21.exe 22.exe 23.exe 24.exe 25.exe
all_inputs: 1.input 2.input 3.input 4.input 5.input 6.input 7.input 8.input 9.input 10.input 11.input 12.input 13.input 14.input 15.input 16.input 17.input 18.input 19.input 20.input 21.input 22.input 23.input 24.input 25.input
.PHONY: phony all_exes all_inputs
