#pragma once

using u32 = unsigned;

struct Allocator;

struct MemoryPool {
  Allocator* impl;
  MemoryPool(u32 memory_size);
  MemoryPool(MemoryPool const&) = delete;
  ~MemoryPool();
  operator Allocator*() { return impl; }
};

struct MemoryInfo {
  u32 used;
  u32 max_used;
  u32 limit;
};

void reset_max_used(Allocator*);

MemoryInfo memory_statistics(Allocator*);

void enter_memory_pool(Allocator*);

Allocator* exit_memory_pool();
