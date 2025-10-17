#include <atomic>
#include <thread>

#define check(c) if (!(c)) abort()

namespace {

using u32 = unsigned;

bool did_init {};
std::atomic<unsigned> task_count;

}  // namespace

extern "C" {

void gui_init();
void gui_exit();
void gui_run();
void gui_post(void (*call)(void*), void* obj);

void gui_task_begin() { task_count.fetch_add(1); }

void gui_task_end() {
  if (task_count.fetch_sub(1) == 1)
    gui_post([](void*) { gui_exit(); }, nullptr);
}

void tasks_init() {
  check(!did_init);
  did_init = true;
  gui_init();
}

void tasks_run() {
  if (!task_count.load())
    return;
  gui_run();
}

}

void start_task(void (*call)(void*), void* obj) {
  gui_task_begin();
  std::thread([=]() {
    call(obj);
    gui_task_end();
  }).detach();
}
