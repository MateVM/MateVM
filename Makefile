NAME := stackoverflow_segv

all: $(NAME)
	./$<

$(NAME): Main.hs
	ghc --make -Wall -O2 $^ -o $@

clean:
	rm -f *.hi *.o $(NAME)
