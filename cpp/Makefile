.PHONY: all clean test

CXX=ccache clang++
CXXFLAGS=-g -Wall -std=c++11
LDFLAGS=

RT_OBJS=decimal.o
RT=rt.a

all: $(OBJS) test_decimal

$(RT): $(RT_OBJS)
	@echo :: Creating runtime archive
	@ar cr $@ $(RT_OBJS)

test: test_decimal
	@./test_decimal

%.o: %.cpp
	@echo :: Compiling $<
	@$(CXX) -c $(CXXFLAGS) -o $@ $<

test_decimal: rt.a test_decimal.o
	@echo :: Linking $@
	@$(CXX) $(LDFLAGS) -o $@ test_decimal.o $(RT)

clean:
	@echo :: Cleaning
	@-rm -f $(RT_OBJS) test_decimal $(RT)
