#include "common.hh"

namespace {

struct Allocator {
  u32 capacity;
  u32 limit = sizeof(Allocator);
  u32 freelist {};
  u32 n_alloc {};
  u32 n_free {};
  u32 n_used {};
  u32 max_used {};
  u32 n_freelist_hops {};
  u32 dummy_footer = 1;
  u32 first_header {};

  void* offset(u32 ofs) { return (char*) this + ofs; }

  u32& word(u32 ofs) { return *(u32*) ((char*) this + ofs); }

  u32 block_size(u32 block) {
    auto header = word(block - 4);
    return header & ~1u;
  }

  void freelist_pop(u32 block) {
    u32 next = word(block);
    u32 prev = word(block + 4);
    if (next)
      word(next + 4) = prev;
    if (prev)
      word(prev) = next;
    else
      freelist = next;
  }

  void freelist_add(u32 block) {
    if (freelist)
      word(freelist + 4) = block;
    word(block) = freelist;
    word(block + 4) = 0;
    freelist = block;
  }

  u32 next(u32 block) { return word(block); }

  void increase_used(u32 amount) {
    n_used += amount;
    if (n_used > max_used)
      max_used = n_used;
  }

  void* alloc(u32 size) {
    u32 needed = (size + 7u) & ~7u;
    return alloc_8byte_aligned(needed);
  }

  void* calloc(u32 size) {
    u32 needed = (size + 7u) & ~7u;
    auto p = (u64*) alloc_8byte_aligned(needed);
    for (auto i = p, end = p + needed / 8; i < end; ++i)
      *i = 0;
    return p;
  }

  void* alloc_8byte_aligned(u32 needed) {
    ++n_alloc;

    // check free list
    for (u32 it = freelist; it; it = next(it)) {
      n_freelist_hops++;
      u32& header = word(it - 4);
      u32 cur_size = header;  // know it's free
      if (cur_size < needed)
        continue;
      freelist_pop(it);
      u32& footer = word(it + cur_size);
      if (needed + 16 <= cur_size) {
        // can split
        u32 remaining = cur_size - needed - 8;
        u32 new_free = it + needed + 8;
        u32& new_footer = word(it + needed);
        u32& new_header = word(new_free - 4);
        header = new_footer = needed | 1;
        new_header = footer = remaining;
        freelist_add(new_free);
        increase_used(needed);
      } else {
        // can't split
        header = footer = cur_size | 1;
        increase_used(cur_size);
      }
      return offset(it);
    }

    // didn't find in free list, get from end
    check(limit + needed + 8 <= capacity);
    u32 base = exchange(limit, limit + needed + 8);
    word(limit - 4) = 1;  // mark limit as in use so don't try to coalesce
    u32& header = word(base - 4);
    u32& footer = word(base + needed);
    header = footer = needed | 1;
    increase_used(needed);
    return offset(base);
  }

  u32 to_block(void* ptr) {
    u32 block = u32((char*) ptr - (char*) this);
    check(block < capacity);
    check(!(block & 7));
    return block;
  }

  struct CandidateBlock {
    u32 start;
    u32 end;
  };

  void write_meta(CandidateBlock const& block) {
    word(block.start - 4) = word(block.end) = block.end - block.start;
  }

  void coalesce_right(CandidateBlock& block) {
    u32 next_header = word(block.end + 4);
    if (!(next_header & 1)) {
      u32 next_block = block.end + 8;
      freelist_pop(next_block);
      block.end += 8 + next_header;
    }
  }

  void coalesce_left(CandidateBlock& block) {
    u32 prev_footer = word(block.start - 8);
    if (!(prev_footer & 1)) {
      u32 prev_block = block.start - 8 - prev_footer;
      freelist_pop(prev_block);
      block.start -= 8 + prev_footer;
    }
  }

  void* realloc_naive(void* ptr, u32 cur_size, u32 new_size) {
    auto new_ptr = alloc(new_size);
    memcpy(new_ptr, ptr, new_size < cur_size ? new_size : cur_size);
    free(ptr);
    return new_ptr;
  }

  void* realloc_shrink(u32 block, u32& header, u32 needed, u32 cur_size) {
    n_used -= cur_size - needed;
    u32& new_footer = word(block + needed);
    header = new_footer = needed | 1;
    CandidateBlock new_free {block + needed + 8, block + cur_size};
    coalesce_right(new_free);
    write_meta(new_free);
    freelist_add(new_free.start);
    return offset(block);
  }

  void* realloc_grow(
      u32 block, u32& header, u32 cur_size, u32 needed, u32 total_size) {
    u32 next_block = block + cur_size + 8;
    freelist_pop(next_block);
    u32 footer = block + total_size;

    if (needed + 16 <= total_size) {
      // can split
      u32& new_footer = word(block + needed);
      header = new_footer = needed | 1;
      CandidateBlock new_free {block + needed + 8, footer};
      write_meta(new_free);
      freelist_add(new_free.start);
      increase_used(needed - cur_size);
    } else {
      // can't split
      header = word(footer) = total_size | 1;
      increase_used(total_size - cur_size);
    }

    return offset(block);
  }

  void* realloc(void* ptr, u32 size) {
    u32 block = to_block(ptr);
    u32& header = word(block - 4);
    u32 needed = (size + 7u) & ~7u;
    u32 cur_size = header & ~1u;

    if (needed + 16 <= cur_size)
      return realloc_shrink(block, header, needed, cur_size);

    if (needed <= cur_size)
      return ptr;

    u32& next_header = word(block + cur_size + 4);
    if (!(next_header & 1)) {
      u32 total_size = cur_size + next_header + 8;
      if (needed <= total_size)
        return realloc_grow(block, header, cur_size, needed, total_size);
    }

    return realloc_naive(ptr, cur_size, size);
  }

  void free(void* ptr) {
    ++n_free;
    u32 block = to_block(ptr);
    u32 header = block - 4;
    u32 size = word(header) & ~1u;
    n_used -= size;

    CandidateBlock cand {block, block + size};
    coalesce_right(cand);
    coalesce_left(cand);

    word(cand.start - 4) = word(cand.end) = cand.end - cand.start;
    freelist_add(cand.start);
  }
};

static_assert(sizeof(Allocator) % 8 == 0);

thread_local char heap_memory[1024 * 1024 * 16];

Allocator& cur_heap() { return (Allocator&) heap_memory; }

}  // namespace

void init_heap() {
  new (heap_memory) Allocator {sizeof(heap_memory)};
}

void* malloc(uptr size) {
  if (!size)
    return nullptr;
  return cur_heap().alloc(u32(size));
}

void* calloc(uptr m, uptr s) {
  if (!m || !s)
    return nullptr;
  u32 total = u32(m * s);
  return cur_heap().calloc(total);
}

void* realloc(void* ptr, uptr size) {
  if (!ptr)
    return malloc(size);
  if (!size)
    return free(ptr), nullptr;
  return cur_heap().realloc(ptr, u32(size));
}

void free(void* ptr) {
  if (!ptr)
    return;
  return cur_heap().free(ptr);
}

[[nodiscard]] void* operator new(uptr size) { return malloc(size); }

void operator delete(void* p) noexcept { return free(p); }
