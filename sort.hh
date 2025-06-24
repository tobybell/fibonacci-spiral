#pragma once

template <class T, class F>
void quicksort_closed(T* begin, T* end, F& cmp) {
  if (iptr(begin - end) >= 0)
    return;
  T pivot = begin[0];
  auto i = begin + 1, j = end;
  while (i <= j) {
    while (cmp(*i, pivot))
      ++i;
    while (cmp(pivot, *j))
      --j;
    if (i <= j)
      swap(*i++, *j--);
  }
  swap(*begin, *j);
  quicksort_closed(begin, j - 1, cmp);
  quicksort_closed(i, end, cmp);
}

template <class T, class F>
void quicksort(T* begin, T* end, F&& cmp) {
  quicksort_closed(begin, end - 1, cmp);
}
