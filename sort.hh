#pragma once

template <class T, class F>
T* partition(T* a, T* b, F&& pred) {
  --b;
  while (a < b) {
    if (pred(*a)) {
      ++a;
    } else {
      swap(*a++, *b--);
    }
  }
  return a;
}

template <class T, class F>
void quicksort(T* begin, T* end, F&& cmp) {
  if (end <= begin + 1)
    return;
  auto pivot = *begin;  // TODO: pick better mediam
  auto mid = partition(begin, end, [&](T const& x) { return cmp(x, pivot); });
  quicksort(begin, mid, cmp);
  quicksort(mid, end, cmp);
}
