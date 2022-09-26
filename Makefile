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

LAMBDATOOLS=./build/lambda-lang-toolkit

$(LAMBDATOOLS):
	mkdir -p build
	cd build; git clone https://github.com/woodrush/lambda-lang-toolkit

.PHONY blc: $(BLC)
$(BLC): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make blc && mv bin/blc ../../bin

.PHONY uni: $(UNI)
$(UNI): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make uni && mv bin/uni ../../bin

.PHONY uni++: $(UNIPP)
$(UNIPP): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make uni++ && mv bin/uni++ ../../bin

.PHONY clamb: $(CLAMB)
$(CLAMB): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make clamb && mv bin/clamb ../../bin

.PHONY asc2bin: $(ASC2BIN)
$(ASC2BIN): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make asc2bin && mv bin/asc2bin ../../bin

.PHONY lam2bin: $(LAM2BIN)
$(LAM2BIN): $(LAMBDATOOLS)
	mkdir -p bin
	cd $(LAMBDATOOLS) && make lam2bin && mv bin/lam2bin ../../bin
