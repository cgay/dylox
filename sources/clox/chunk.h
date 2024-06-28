// A "chunk" is a sequence of byte code...

#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_RETURN,
} OpCode;

typedef struct {
  int count;              /* num bytes in chunk */
  int capacity;           /* allocated bytes in `code` */
  uint8_t* code;          /* the byte code */
  int* lines;             /* line numbers, 1-to-1 correspondence with `code` */
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);
void freeChunk(Chunk* chunk);

#endif
