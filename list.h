#pragma once

typedef struct List {
  u32 capacity;
  u32 elemSize;
  u32 size;
} List;
u32 list_len(void const* list) {
  return ((u32*) list)[-1];
}
void* list_make(u32 elemSize) {
  List* head = (List*) malloc(sizeof(List));
  head->capacity = head->size = 0;
  head->elemSize = elemSize;
  return (void*) (head + 1);
}
u32 list_emplace(void* list, void const* data) {
  List* head = (List*) list - 1;
  u32 capacity = head->capacity;
  u32 size = head->size;
  if (capacity <= size) {
    if (!capacity)
      capacity = size;
    else while (capacity <= size)
      capacity *= 2;
    head = (List*) realloc(head, sizeof(List) + capacity * head->elemSize);
    head->capacity = capacity;
  }
  return head->size++;
}
#define list_push(p) p[list_emplace(p)]
void list_drop(void* list) {
  List* head = (List*) list - 1;
  free(head);
}
