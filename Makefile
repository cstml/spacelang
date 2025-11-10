CC = gcc
CFLAGS = -g -I lib -Wall
SOURCES = src/main.c src/evaluator.c src/parser.c src/term.c src/memory.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = bin/spc

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -o $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)