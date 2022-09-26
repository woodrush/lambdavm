# Binary lambda calculus
BLC=./bin/Blc
UNI=./bin/uni
UNIPP=./bin/uni++

# Universal lambda
CLAMB=./bin/clamb

# Lazy K
LAZYK=./bin/lazyk

# Tools
ASC2BIN=./bin/asc2bin
LAM2BIN=./bin/lam2bin

# Toolkit
LAMBDATOOLS=./build/lambda-calculus-devkit

# Other
SBCL=sbcl


#================================================================
# Tests
#================================================================
.PHONY: test-%
test-%: $(addsuffix .%-out.expected-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "\n    All tests have passed for $(interpreter-name-$*).\n"
interpreter-name-blc="BLC with the interpreter 'Blc'"
interpreter-name-blc-uni="BLC with the interpreter 'uni'"
interpreter-name-ulamb="Universal Lambda"
interpreter-name-lazyk="Lazy K"


.PRECIOUS: out/%.blc
out/%.blc: examples/%
	mkdir -p out
	$(SBCL) --script $< > $@

.PRECIOUS: out/%.ulamb
out/%.ulamb: examples/%
	mkdir -p out
	( printf '(defparameter **compile-ulamb** t)'; cat $< ) > $@.cl
	$(SBCL) --script $@.cl > $@

.PRECIOUS: out/%.lazy
out/%.lazy: examples/%
	mkdir -p out
	( printf '(defparameter **compile-lazy** t)'; cat $< ) > $@.cl
	$(SBCL) --script $@.cl > $@

.PRECIOUS: out/%.blc-out
out/%.blc-out: out/%.blc $(BLC) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $< | $(ASC2BIN); cat test/$*.in ) | $(BLC) | head -n 20 > $@.tmp; else \
		cat $< | $(ASC2BIN) | $(BLC) | head -n 20 > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.blc-uni-out
out/%.blc-uni-out: out/%.blc $(UNI) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $< | $(ASC2BIN); cat test/$*.in ) | $(UNI) | head -n 20 > $@.tmp; else \
		cat $< | $(ASC2BIN) | $(UNI) | head -n 20 > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.ulamb-out
out/%.ulamb-out: out/%.ulamb $(CLAMB) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $< | $(ASC2BIN); cat test/$*.in ) | $(CLAMB) -u | head -n 20 > $@.tmp; else \
		cat $< | $(ASC2BIN) | $(CLAMB) -u | head -n 20 > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.lazyk-out
out/%.lazyk-out: out/%.lazy $(LAZYK)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		cat test/$*.in | $(LAZYK) $< -u | head -n 20 > $@.tmp; else \
		$(LAZYK) $< -u | head -n 20 > $@.tmp; fi
	mv $@.tmp $@

out/%.blc-out.expected-diff: ./out/%.blc-out ./test/%.out
	diff $^ || exit 1

out/%.blc-uni-out.expected-diff: ./out/%.blc-uni-out ./test/%.out
	diff $^ || exit 1

out/%.ulamb-out.expected-diff: ./out/%.ulamb-out ./test/%.out
	diff $^ || exit 1

out/%.lazyk-out.expected-diff: ./out/%.lazyk-out ./test/%.out
	diff $^ || exit 1


#================================================================
# Build the interpreters and tools
#================================================================
$(LAMBDATOOLS):
	mkdir -p build
	cd build; git clone https://github.com/woodrush/lambda-lang-toolkit

.PHONY: blc
blc: $(BLC)
$(BLC): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && $(MAKE) blc && mv bin/Blc ../../bin

.PHONY: uni
uni: $(UNI)
$(UNI): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && $(MAKE) uni && cp bin/uni ../../bin

.PHONY: uni++
uni++: $(UNIPP)
$(UNIPP): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make uni++ && mv bin/uni++ ../../bin

.PHONY: clamb
clamb: $(CLAMB)
$(CLAMB): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make clamb && mv bin/clamb ../../bin

.PHONY: lazyk
lazyk: $(LAZYK)
$(LAZYK): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make lazyk && mv bin/lazyk ../../bin


.PHONY: asc2bin
asc2bin: $(ASC2BIN)
$(ASC2BIN): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make asc2bin && mv bin/asc2bin ../../bin

.PHONY: lam2bin
lam2bin: $(LAM2BIN)
$(LAM2BIN): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make lam2bin && mv bin/lam2bin ../../bin
