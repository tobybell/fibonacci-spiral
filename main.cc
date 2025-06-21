#include "print.hh"
#include "gui.hh"
#include "sort.hh"

using f32 = float;

extern "C" {

u32 make_vertex_shader(char const* source, u32 len);
u32 make_fragment_shader(char const* source, u32 len);
u32 make_program(u32 const* shaders, u32 n_shaders);

u32 makeVertexArray();
void bindVertexArray(u32 vertexArray);
void dropVertexArray(u32 vertexArray);

void redraw();
u32 getUniformBlock(u32 program, char const* name, u32 len);
u32 bindUniformBlock(u32 program, u32 block, u32 binding);
u32 makeBuffer(u32 size);
void bindUniformBuffer(u32 binding, u32 buffer);
void fillBuffer(u32 buffer, u32 ofs, void const* src, u32 size);
void make_texture(void const* data, u32 size);
f32 cos(f32);
f32 sin(f32);
void test(u32);
void vertexAttrib1f(u32 attrib, f32);
void vertexAttrib2f(u32 attrib, f32, f32);
void vertexAttrib3f(u32 attrib, f32, f32, f32);
void vertexAttrib4f(u32 attrib, f32, f32, f32, f32);
void vertexAttributeArrayU8(
  u32 attrib, u32 buffer, u32 components, bool normalized, u8 stride,
  u32 offset, u32 instanceDivisor);
void vertexAttributeArrayI16(
  u32 attrib, u32 buffer, u32 components, bool normalized, u8 stride,
  u32 offset, u32 instanceDivisor);
void enableBlend();
void disableBlend();
void enableDepthTest();
void disableDepthTest();

void set_viewport(i32 x, i32 y, u32 dx, u32 dy);
void useProgram(u32 program);
void drawPoints(u32 base, u32 count);
void drawTriangleStrip(u32 base, u32 count);
void drawTriangleStripInstanced(u32 base, u32 count, u32 instanceCount);
void demo();

void start();
void draw();
void scroll(f32 x, f32 y, f32 dx, f32 dy);
void key(u32 id);
void resize(u32 width, u32 height);

}

namespace {

template <class T, class F>
void sort(Ref<T> elem, F&& cmp) {
  quicksort(elem.begin(), elem.end(), cmp);
}

}

struct KeyMap {
  Key map[256] {};
  constexpr KeyMap() {
    map[32] = Space;
    map[222] = Apostrophe;
    map[188] = Comma;
    map[189] = Minus;
    map[190] = Period;
    map[191] = Slash;
    map[48] = Key0;
    map[49] = Key1;
    map[50] = Key2;
    map[51] = Key3;
    map[52] = Key4;
    map[53] = Key5;
    map[54] = Key6;
    map[55] = Key7;
    map[56] = Key8;
    map[57] = Key9;
    map[186] = Semicolon;
    map[187] = Equal;
    map[65] = KeyA;
    map[66] = KeyB;
    map[67] = KeyC;
    map[68] = KeyD;
    map[69] = KeyE;
    map[70] = KeyF;
    map[71] = KeyG;
    map[72] = KeyH;
    map[73] = KeyI;
    map[74] = KeyJ;
    map[75] = KeyK;
    map[76] = KeyL;
    map[77] = KeyM;
    map[78] = KeyN;
    map[79] = KeyO;
    map[80] = KeyP;
    map[81] = KeyQ;
    map[82] = KeyR;
    map[83] = KeyS;
    map[84] = KeyT;
    map[85] = KeyU;
    map[86] = KeyV;
    map[87] = KeyW;
    map[88] = KeyX;
    map[89] = KeyY;
    map[90] = KeyZ;
    map[219] = LeftBracket;
    map[220] = Backslash;
    map[221] = RightBracket;
    map[192] = GraveAccent;
    map[27] = Escape;
    map[13] = Enter;
    map[9] = Tab;
    map[8] = Backspace;
    map[46] = Delete;
    map[39] = Right;
    map[37] = Left;
    map[40] = Down;
    map[38] = Up;
    map[20] = CapsLock;
    map[112] = F1;
    map[113] = F2;
    map[114] = F3;
    map[115] = F4;
    map[116] = F5;
    map[117] = F6;
    map[118] = F7;
    map[119] = F8;
    map[120] = F9;
    map[121] = F10;
    map[122] = F11;
    map[123] = F12;
    map[16] = Shift;
    map[17] = Control;
    map[18] = Alt;
    map[91] = LeftSuper;
    map[93] = RightSuper;
  }
};

static constexpr KeyMap key_map {};

void init_heap();

namespace {

u32 make_vertex_shader(Str s) { return ::make_vertex_shader(s.base, s.size); }
u32 make_fragment_shader(Str s) { return ::make_fragment_shader(s.base, s.size); }
u32 make_program(Ref<u32 const> shaders) { return ::make_program(shaders.base, shaders.size); }
u32 getUniformBlock(u32 program, Str s) { return ::getUniformBlock(program, s.base, s.size); }

u32 program;
u32 pointProgram;
u32 textProgram;
u32 multiLineTextProgram;
u32 circleProgram;
u32 rectProgram;
f32 gWidth, gHeight;  // resolution

u32 uModelBuffer;
u32 uResolutionBuffer;

struct GlMat3 {
  f32 elem[9];
  f32& operator[](u32 i) { return elem[i]; }
};

GlMat3 R;

GlMat3 rot_y(GlMat3 R0) {
  f32 c = 0.9999995f;
  f32 s = 0.0009999998333f;
  auto& R = R0.elem;
  return {
    s * R[2] + c * R[0],
    R[1],
    c * R[2] - s * R[0],
    s * R[5] + c * R[3],
    R[4],
    c * R[5] - s * R[3],
    s * R[8] + c * R[6],
    R[7],
    c * R[8] - s * R[6]};
}

GlMat3 rot_x(GlMat3 R0, f32 th) {
  f32 c = cos(th);
  f32 s = sin(th);
  auto& R = R0.elem;
  return {
    R[0],
    c * R[1] - s * R[2],
    s * R[1] + c * R[2],
    R[3],
    c * R[4] - s * R[5],
    s * R[4] + c * R[5],
    R[6],
    c * R[7] - s * R[8],
    s * R[7] + c * R[8]};
}

GlMat3 rot_y(GlMat3 R0, f32 th) {
  f32 c = cos(th);
  f32 s = sin(th);
  auto& R = R0.elem;
  return {
    s * R[2] + c * R[0],
    R[1],
    c * R[2] - s * R[0],
    s * R[5] + c * R[3],
    R[4],
    c * R[5] - s * R[3],
    s * R[8] + c * R[6],
    R[7],
    c * R[8] - s * R[6]};
}

}

u32 make_text_program() {
  u32 v_multiline = make_vertex_shader(R"gl(#version 300 es
uniform Resolution {
  vec2 uResolution;
};
layout (location = 0) in lowp uint aGlyph;
layout (location = 1) in mediump ivec2 aColRow;
flat out lowp uint vGlyph;
out mediump vec2 vCoord;
void main() {
  vGlyph = aGlyph;
  vec2 glyphSize = vec2(5.0, 7.0);
  float glyphStride = 6.0;
  vec2 charSize = glyphSize * 2.0 / uResolution;
  vCoord = vec2(float(gl_VertexID / 2), float(gl_VertexID % 2));
  vec2 coord = (-1.0 + 20.0 / uResolution) + vCoord * charSize;
  vec2 shift = vec2(aColRow * ivec2(6, 8) * 2) / uResolution;
  gl_Position = vec4(shift + coord, 0, 1);
}
  )gl"_s);
  u32 s0 = make_vertex_shader(R"gl(#version 300 es
uniform Resolution {
  vec2 uResolution;
};
layout (location = 0) in lowp uint aGlyph;
flat out lowp uint vGlyph;
out mediump vec2 vCoord;
void main() {
  vGlyph = aGlyph;
  vec2 glyphSize = vec2(5.0, 7.0);
  float glyphStride = 6.0;
  vec2 charSize = glyphSize * 2.0 / uResolution;
  vCoord = vec2(float(gl_VertexID / 2), float(gl_VertexID % 2));
  vec2 coord = (-1.0 + 20.0 / uResolution) + vCoord * charSize;
  float shift = glyphStride * 2.0 / uResolution.x;
  gl_Position = vec4(shift * float(gl_InstanceID) + coord.x, coord.y, 0, 1);
}
  )gl"_s);
  u32 s1 = make_fragment_shader(R"gl(#version 300 es
uniform lowp usampler2D u_image;
flat in lowp uint vGlyph;
in mediump vec2 vCoord;
out mediump vec4 oColor;
void main() {
  uint character = vGlyph;
  uvec2 ij = uvec2(vCoord * vec2(5, 7));
  uint bit = ij.x + ij.y * 5u + character * 35u;
  uint pixel = bit / 8u;
  uint texel = texelFetch(u_image, ivec2(pixel, 0), 0).r;
  uint ans = (texel >> (bit % 8u)) & 1u;
  if (ans == 0u)
    discard;
  oColor = vec4(1, 1, 1, 1);
}
  )gl"_s);

  multiLineTextProgram = make_program((u32[]) {v_multiline, s1});

  return make_program((u32[]) {s0, s1});
}

// Bits: [128 glyphs][7 rows][5 columns] = 4480 bits
extern u64 const font_data[70];

struct VertexArray {
  VertexArray(): id(makeVertexArray()) {}
  VertexArray(VertexArray const&) = delete;
  VertexArray(VertexArray&& y): id(exchange(y.id, 0u)) {}
  ~VertexArray() {
    if (id)
      dropVertexArray(exchange(id, 0u));
  }
  void bind() const {
    check(id);
    bindVertexArray(id);
  }
private:
  u32 id;
};

union Vec3 {
  f32 elem[3];
  struct {
    f32 x, y, z;
  };
};

Vec3 rgbu8(u8 r, u8 g, u8 b) {
  return {f32(r) / 255, f32(g) / 255, f32(b) / 255};
}

Vec3 grayu8(u8 g) { return rgbu8(g, g, g); }

void circle(f32 x, f32 y, f32 r, Vec3 topColor, Vec3 bottomColor) {
  useProgram(circleProgram);
  enableBlend();
  bindVertexArray(0);
  vertexAttrib2f(0, x, y);
  vertexAttrib1f(1, r);
  vertexAttrib3f(2, topColor.x, topColor.y, topColor.z);
  vertexAttrib3f(3, bottomColor.x, bottomColor.y, bottomColor.z);
  drawTriangleStripInstanced(0, 4, 1);
  disableBlend();
}

void enabledRadioButton(f32 x, f32 y) {
  circle(x, y, 7, rgbu8(28, 106, 230), rgbu8(24, 94, 206));
  circle(x, y, 3, {1, 1, 1}, {1, 1, 1});
}

void disabledRadioButton(f32 x, f32 y) {
  circle(x, y, 7, grayu8(60), grayu8(93));
}

void drawRect(u32 x, u32 y, u32 w, u32 h, Vec3 color) {
  useProgram(rectProgram);
  bindVertexArray(0);
  vertexAttrib4f(
    0, -1 + f32(x) * 2 / gWidth, 1 - f32(y) * 2 / gHeight, f32(w) * 2 / gWidth,
    -f32(h) * 2 / gHeight);
  vertexAttrib3f(1, color.x, color.y, color.z);
  drawTriangleStripInstanced(0, 4, 1);
}

struct MultiLineString {
  VertexArray va {};
  u32 stringBuffer = makeBuffer(64);
  u32 colRowBuffer = makeBuffer(256);
  u32 size {};
  MultiLineString() {
    va.bind();
    vertexAttributeArrayU8(0, stringBuffer, 1, 0, 0, 0, 1);
    vertexAttributeArrayI16(1, colRowBuffer, 2, 0, 0, 0, 1);
  }
  void set(Str string) {
    size = len(string);

    // lay out string
    i16 row = 0;
    i16 col = 0;
    Array<i16[2]> colRow(len(string));
    for (u32 i: range(len(string))) {
      colRow[i][0] = col++;
      colRow[i][1] = row;
      if (string[i] == '\n') {
        --row;
        col = 0;
      }
    }

    // TODO: Handle this in shader rather than here
    for (u32 i: range(len(string)))
      colRow[i][1] -= row;

    fillBuffer(stringBuffer, 0, string.begin(), len(string));
    fillBuffer(colRowBuffer, 0, colRow.begin(), len(colRow) * 4);
  }
  void draw() {
    va.bind();
    useProgram(multiLineTextProgram);
    drawTriangleStripInstanced(0, 4, size);
  }
};

template <class T>
struct State {
  T value;
  List<Func<T>> subscribers;
  List<u32> free;

  template <class F>
  auto operator()(F&& f) {
    u32 id;
    if (free) {
      id = free.last();
      free.pop();
    } else {
      id = len(subscribers);
      subscribers.emplace();
    }
    subscribers[id] = move(f);
    subscribers[id](value);
    return [this, id]() {
      if (id + 1 == len(subscribers))
        return subscribers.pop();
      subscribers[id] = {};
      free.push(id);
    };
  }

  void operator=(T newValue) {
    value = newValue;
    for (auto& s: subscribers) {
      if (s)
        s(value);
    }
  }
};

struct Work {
  void (*call)(void*) = 0;
  void* obj;
  Work() = default;
  template <
      class F,
      enable_if<
          is_invocable<F> && is_move_constructible<F> &&
          (sizeof(obj) < sizeof(F) || !is_trivially_copyable<F>)> = 0>
  Work(F&& f):
    call([](void* arg) {
      auto fn = reinterpret_cast<F*>(arg);
      (*fn)();
      delete fn;
    }), obj(new F(move(f))) {}
  template <
      class F,
      enable_if<
          is_invocable<F> && is_move_constructible<F> &&
          sizeof(F) <= sizeof(obj) && is_trivially_copyable<F>> = 0>
  Work(F&& f):
    call([](void* arg) {
      alignas(F) char data[sizeof(F)];
      memcpy(data, &arg, sizeof(F));
      auto& fn = *reinterpret_cast<F*>(data);
      fn();
      fn.~F();
    }),
    obj() {
    new (&obj) F(move(f));
  }
  Work(Work const&) = delete;
  Work(Work&& y): call(exchange(y.call, nullptr)), obj(y.obj) {}
  void operator=(Work y) {
    swap(call, y.call);
    obj = y.obj;
  }
  ~Work() { check(!call); }
  void operator()() {
    call(obj);
    call = 0;
  }
};

struct Node {
  u32 x, y, width, height;
  virtual void layout() = 0;
  virtual void draw() = 0;
};

struct Column: Node {
  List<Node*> kids;
  void layout() override {
    u32 y = 0;
    u32 x = 0;
    for (auto kid_ptr: kids) {
      auto& kid = *kid_ptr;
      kid.layout();
      kid.x = 0;
      kid.y = y;
      y += kid.height;
      if (kid.width > x)
        x = kid.width;
    }
    height = y;
    width = x;
  }
  void draw() override {
    for (auto kid: kids)
      kid->draw();
  }
};

template <class... T>
auto column(T&&... kids) {
  auto p = new Column;
  (p->kids.push(kids), ...);
  return p;
}

enum Direction: u8 { X, Y };

struct LayoutEntry {
  u32 min[2];
  u16 childGap;
  u16 pad[4];  // [Direction][Side]
  bool grow[2];
  u32 kids;
  Direction direction;  // meaningless if `!kids`
};

constexpr LayoutEntry spacer {.grow = {1, 1}};

u32 totalPad(LayoutEntry const& n, Direction d) {
  return n.pad[2 * d] + n.pad[2 * d + 1];
}

template <class T>
T elemtype(Array<T> const&);
template <class T>
T elemtype(List<T> const&);
template <class T>
T elemtype(T const&);

template <class T>
struct Cat {
  static T const* dat(T const& x) { return &x; }
  static T const* dat(List<T> const& x) { return x.begin(); }
  static T const* dat(Array<T> const& x) { return x.begin(); }
  static u32 get_len(T const& x) { return 1; }
  static u32 get_len(List<T> const& x) { return len(x); }
  static u32 get_len(Array<T> const& x) { return len(x); }
  template <class S>
  static void put(T*& dst, S const& src) {
    u32 n = get_len(src);
    memcpy(dst, dat(src), sizeof(T) * n);
    dst += n;
  }
  template <class... S>
  static Array<T> cat(S const&... r) {
    Array<T> ans((... + get_len(r)));
    auto i = ans.begin();
    ((put(i, r)), ...);
    return ans;
  }
};

template <class S, class... T>
auto cat(S const& s, T const&... r) {
  return Cat<decltype(elemtype(s))>::cat(s, r...);
}

template <class... T>
auto col(u32 minx, u32 miny, bool growx, bool growy, T&&... kids) {
  return cat(LayoutEntry {{minx, miny}, 0, {}, {growx, growy}, sizeof...(T), Y}, kids...);
}

template <class... T>
auto colGap(
    u32 minx, u32 miny, bool growx, bool growy, u16 pad, u16 childGap, T&&... kids) {
  return cat(LayoutEntry {{minx, miny}, childGap, {pad, pad, pad, pad}, {growx, growy}, sizeof...(T), Y}, kids...);
}

template <class... T>
auto row(u32 minx, u32 miny, bool growx, bool growy, T&&... kids) {
  return cat(LayoutEntry {{minx, miny}, 0, {}, {growx, growy}, sizeof...(T), X}, kids...);
}

template <u32 N>
auto text(char const (&x)[N]) {
  u32 len = N - 1;
  check(!x[len]);
  return cat(LayoutEntry {{6 * len - 1, 7}, 0, {}, {0, 0}, 0, X});
}

struct Kids {
  Array<u32> kids;
  Array<u32> end;
  Kids(Span<LayoutEntry> node) {
    u32 n = len(node);
    kids = Array<u32>(n);
    u32 n_out = 0;
    u32 n_end = 0;
    List<u32> stack;
    for (u32 i = 0; i < n; ++i) {
      u32 n_kids = node[i].kids;
      for (u32 i = n_kids; i; --i)
        kids[n_out++] = stack[stack.size - i];
      stack.size -= n_kids;
      end[i] = n_out;
      stack.push(i);
    }
    check(n_out == n);
  }
  Ref<u32> operator[](u32 i) {
    u32 begin = i ? end[i - 1] : 0;
    return {kids.begin() + begin, end[i] - begin};
  }
};

template <class T>
Span<T> slice(List<T> const& x, u32 base) {
  check(base <= len(x));
  return {x.begin() + base, len(x) - base};
}

struct GrowthDistribution {
  u32 how_many_kids_can_we_grow;
  u32 to_what_total_size;
};

static GrowthDistribution growth_distribution(
    Span<u32> sizes, Span<u32> sorted_kids, u32 budget) {
  u32 n = len(sorted_kids);
  for (u32 i = 0; i < n; ++i) {
    u32 cur_kid_size = sizes[sorted_kids[i]];
    u32 next_budget = budget + cur_kid_size;
    u32 fill_line = next_budget / (i + 1);
    if (fill_line < cur_kid_size)  // mustn't make any child smaller
      return {i, budget};
    budget = next_budget;
  }
  return {n, budget};
}

template <class T>
void reverse(Ref<T> ref) {
  if (!ref)
    return;
  auto a = ref.begin();
  auto b = ref.end() - 1;
  while (a < b) {
    swap(*a, *b);
    ++a;
    --b;
  }
}

struct Dimension {
  Array<u32> size;
  Array<u32> pos;

  Dimension(
    Span<LayoutEntry> node, u32 available, Direction d) {
    u32 n = len(node);

    ArrayList<u32> node_growable_kids;
    {
      List<u32> stack;
      List<u32> since;
      for (u32 i = n; i--;) {
        u32 n_since = len(since);
        since.push(len(stack));
        u32 n_kids = node[i].kids;
        u32 since_i = since[n_since - n_kids];
        since.size -= n_kids;
        node_growable_kids.push(slice(stack, since_i));
        stack.size = since_i;
        if (node[i].grow[d])
          stack.push(i);
      }
    }

    {
      reverse<u32>(node_growable_kids.list);
      reverse<u32>(node_growable_kids.ofs);
      u32 end = len(node_growable_kids.list);
      for (u32& ofs: node_growable_kids.ofs)
        ofs = end - ofs;
    }

    // bottom-up: compute minimum size
    //   just add up children
    List<u32> stack;
    size = Array<u32>(n);
    for (u32 i = n; i--;) {
      auto& o = node[i];
      u32 n_kids = o.kids;
      auto pad = totalPad(o, d);
      if (!n_kids) {
        size[i] = o.min[d];
        stack.push(o.min[d]);
      } else if (o.direction == d) {
        u32 sum = 0;
        for (u32 j = 0; j < n_kids; ++j) {
          sum += stack.last();
          stack.pop();
        }
        size[i] = sum + pad + o.childGap * (n_kids - 1);
        stack.push(sum);
      } else {
        u32 max = 0;
        for (u32 j = 0; j < n_kids; ++j) {
          if (stack.last() > max)
            max = stack.last();
          stack.pop();
        }
        size[i] = max + pad;
        stack.push(max);
      }
    }

    check(len(stack) == 1);
    u32 total = stack.last();

    if (!node[0].grow[d])
      return;

    auto orig_size = size;

    // top-down: compute actual size (based on available)
    //   distribute extra space to growable children
    size[0] = available;
    for (u32 i = 0; i < n; ++i) {
      // node is growable in the current dimension
      u32 avail = size[i];
      check(avail >= orig_size[i]);  // TODO
      u32 amount = avail - orig_size[i];

      if (node[i].direction == d) {  // longitudinal

        // now, want to get all growable kids
        // want to sort by current size
        // go up from bottom
        auto gkids = node_growable_kids[i];
        sort(gkids, [this](u32 k0, u32 k1) { return size[k0] < size[k1]; });

        auto gdist = growth_distribution(size, gkids, amount);
        for (u32 j: range(gdist.how_many_kids_can_we_grow)) {
          u32 size_j = gdist.to_what_total_size / (gdist.how_many_kids_can_we_grow - j);
          size[gkids[j]] = size_j;
          gdist.to_what_total_size -= size_j;
        }

      } else {  // transverse
        u32 innerSpace = avail - totalPad(node[i], d);
        // get all growable children, grow them to this size
        for (u32 k: node_growable_kids[i])
          size[k] = innerSpace;
      }
    }

    struct PosContext {
      u32 value;
      u32 count;
      u16 childGap;
      bool increment;
    };
    List<PosContext> pstack;
    pstack.push({0, 1, 0, 0});
    pos = Array<u32>(n);
    for (u32 i = 0; i < n; ++i) {
      auto& top = pstack.last();
      pos[i] = top.value;
      --top.count;
      if (top.count) {
        if (top.increment) {
          top.value += size[i] + top.childGap;
        }
      } else
        pstack.pop();
      u32 n_kids = node[i].kids;
      if (n_kids)
        pstack.push({pos[i] + node[i].pad[2 * d], n_kids, node[i].childGap, node[i].direction == d});
    }
  }
};

struct RadioButton: Node {
  void layout() override {
    height = width = 14;
  }
  void draw() override {
    u32 cx = x + 7;
    u32 cy = y + 7;
    enabledRadioButton(-1 + f32(cx) * 2 / gWidth, 1 - f32(cy) * 2 / gHeight);
  }
};

constexpr auto radio_button = [](State<bool>& enabled) {
  return [&enabled]() {
    return enabled([&](bool val) {
      u32 cx = 30;
      u32 cy = 30;
      f32 x = -1 + f32(cx) * 2 / gWidth;
      f32 y = 1 - f32(cy) * 2 / gHeight;
      val ? enabledRadioButton(x, y) : disabledRadioButton(x, y);
    });
  };
};

constexpr Vec3 red {1, 0, 0};
constexpr Vec3 green {0, 1, 0};
constexpr Vec3 blue {0, 0, 1};

struct App {
  List<char> string;
  MultiLineString str;

  State<bool> onoff;
  Work cleanup;

  Column* c = column(new RadioButton, new RadioButton);

  App() {
    c->layout();

    //auto cmp = radio_button(onoff);
    //cleanup = cmp();
  }

  ~App() {
    cleanup();
  }

  void key(Key key) {
    onoff = !onoff.value;
    if (key == Backspace) {
      if (string) {
        string.pop();
        str.set(string);
      }
    } else if (key == Space) {
      string.push(' ');
      str.set(string);
    } else if (key == Enter) {
      string.push('\n');
      str.set(string);
    } else {
      u32 letter = u32(key - KeyA);
      if (letter < 26) {
        string.push('A' + letter);
        str.set(string);
      }
    }
  }

  void draw() {
    c->draw();
    str.draw();
  }
};

static App* app;

void start() {
  init_heap();

  println("Hello!");
  make_texture(font_data, sizeof(font_data));

  textProgram = make_text_program();

  u32 circle_vshader = make_vertex_shader(R"gl(#version 300 es
uniform Resolution {
  highp vec2 uResolution;
  highp vec2 uPixel;
};
layout (location = 0) in highp vec2 center;
layout (location = 1) in highp float radius;
layout (location = 2) in highp vec3 aTop;
layout (location = 3) in highp vec3 aBottom;
out highp vec2 vCoord;
out highp float vGradient;
flat out highp float vRadius;
flat out lowp vec3 vTop;
flat out lowp vec3 vBottom;
void main() {
  vec2 coord = vec2(2 * ivec2(gl_VertexID % 2, gl_VertexID / 2) - 1);
  vGradient = float(gl_VertexID / 2);
  vRadius = radius + 0.5;
  vCoord = coord * vRadius;
  vTop = aTop;
  vBottom = aBottom;
  gl_Position = vec4(center + vCoord * uPixel, 0, 1);
}
)gl");
  u32 circle_fshader = make_fragment_shader(R"gl(#version 300 es
in highp vec2 vCoord;
in highp float vGradient;
flat in highp float vRadius;
flat in lowp vec3 vTop;
flat in lowp vec3 vBottom;
out lowp vec4 oColor;
void main() {
  highp float rad = length(vCoord);
  if (rad >= vRadius)
    discard;
  highp float alpha = min(1.0, vRadius - rad);
  lowp vec3 color = vTop * vGradient + vBottom * (1.0 - vGradient);
  oColor = vec4(color, alpha);
}
)gl");
  circleProgram = make_program((u32[]) {circle_vshader, circle_fshader});

  u32 rect_vshader = make_vertex_shader(R"gl(#version 300 es
layout (location = 0) in mediump vec4 aRect;
layout (location = 1) in mediump vec3 aColor;
flat out lowp vec3 vColor;
void main() {
  vec2 coord = vec2(gl_VertexID % 2, gl_VertexID / 2);
  vColor = aColor;
  gl_Position = vec4(aRect.xy + coord * aRect.zw, 0, 1);
}
)gl");
  u32 rect_fshader = make_fragment_shader(R"gl(#version 300 es
flat in lowp vec3 vColor;
out lowp vec4 oColor;
void main() {
  oColor = vec4(vColor, 1);
}
)gl");
  rectProgram = make_program((u32[]) {rect_vshader, rect_fshader});

  app = new App;
  u32 s0 = make_vertex_shader(R"gl(#version 300 es
#define pi 3.1415926535897932
#define N 32
uniform Settings {
  mat3 u_model;
  mat4 u_view;
};
void main() {
  int xi = gl_VertexID / N;
  int yi = gl_VertexID % N;
  float x = float(2 * xi + 1 - N) / float(N);
  float y = float(2 * yi + 1 - N) / float(N);
  float z = 1. - (abs(x) + abs(y));
  if (z < 0.) {
    float t = x;
    x = sign(x) * (1. - abs(x));
    y = sign(y) * (1. - abs(y));
  }
  vec3 pt = vec3(x, y, z);

  // Extra warping factor I found empirically for more uniform spread; this is
  // not usually part of standard octahedral mapping.
  pt *= (2. - abs(pt));

  vec3 pos = u_model * normalize(pt);
  gl_PointSize = 4.0;
  gl_Position = u_view * vec4(pos, 1);
}
  )gl"_s);

  u32 s1 = make_fragment_shader(R"gl(#version 300 es
out mediump vec4 frag_color;
void main() {
  mediump float rz = (gl_FragCoord.z * 2. - 1.) / gl_FragCoord.w;
  mediump float bright = (1. - rz) / 2.;
  frag_color = vec4(bright, bright, bright, 1);
}
  )gl"_s);

  program = make_program((u32[]) {s0, s1});

  u32 s2 = make_vertex_shader(R"gl(#version 300 es
uniform Settings {
  vec3 u_pos;
};
uniform Settings2 {
  mat3 u_model;
  mat4 u_view;
};
void main() {
  gl_PointSize = 4.0;
  vec3 pos = u_model * u_pos;
  gl_Position = u_view * vec4(pos, 1);
}
)gl");
u32 s3 = make_fragment_shader(R"gl(#version 300 es
out mediump vec4 frag_color;
void main() {
  frag_color = vec4(1, 0, 1, 1);
}
)gl");
  pointProgram = make_program((u32[]) {s2, s3});

  println("Made"_s);

  uModelBuffer = makeBuffer(112);
  u32 uPosBuffer = makeBuffer(16);
  uResolutionBuffer = makeBuffer(16);

  bindUniformBuffer(0, uModelBuffer);
  bindUniformBuffer(1, uPosBuffer);
  bindUniformBuffer(2, uResolutionBuffer);

  R = {1, 0, 0, 0, 1, 0, 0, 0, 1};

  f32 pos[] {1, 0, 0};
  fillBuffer(uPosBuffer, 0, pos, sizeof(pos));

  u32 programSettings = getUniformBlock(program, "Settings");
  u32 pointProgramSettings = getUniformBlock(pointProgram, "Settings");
  u32 pointProgramSettings2 = getUniformBlock(pointProgram, "Settings2");
  bindUniformBlock(program, programSettings, 0);
  bindUniformBlock(pointProgram, pointProgramSettings2, 0);
  bindUniformBlock(pointProgram, pointProgramSettings, 1);

  u32 textResolution = getUniformBlock(textProgram, "Resolution");
  bindUniformBlock(textProgram, textResolution, 2);
  u32 mltr = getUniformBlock(multiLineTextProgram, "Resolution");
  bindUniformBlock(multiLineTextProgram, mltr, 2);
  u32 cr = getUniformBlock(circleProgram, "Resolution");
  bindUniformBlock(circleProgram, cr, 2);
}

void scroll(f32 x, f32 y, f32 dx, f32 dy) {
  R = rot_x(rot_y(R, dx / 50), dy / 50);
}

void key(u32 id) {
  app->key(key_map.map[id]);
}

f32 aspect = 1;

void resize(u32 width, u32 height) {
  gWidth = f32(width);
  gHeight = f32(height);
  aspect = f32(width) / f32(height);
  fillBuffer(uResolutionBuffer, 0, (f32[]) {f32(width), f32(height)}, 8);
  fillBuffer(uResolutionBuffer, 8, (f32[]) {2 / f32(width), 2 / f32(height)}, 8);
  set_viewport(0, 0, width, height);

  {
    auto sidebar = col(200, 0, 0, 1);
    auto title_bar = row(0, 0, 1, 0,
        row(200, 0, 1, 1),  // left spacer
        row(400, 30, 0, 0), // title
        spacer);  // right spacer
    auto menu = colGap(0, 0, 0, 0, 5, 5,
      row(0, 0, 1, 0, text("Hello"), spacer, row(7, 7, 0, 0)),
      row(0, 0, 1, 0, text("World"), spacer, row(7, 7, 0, 0)),
      row(0, 0, 1, 0, text("My"), spacer, row(7, 7, 0, 0)),
      row(0, 0, 1, 0, text("Name is"), spacer, row(7, 7, 0, 0)),
      row(0, 0, 1, 0, text("Toby"), spacer, row(7, 7, 0, 0)));
    auto lay = row(0, 0, 1, 1,
      sidebar,
      col(0, 0, 1, 1,
        title_bar,
        // tab bar
        row(0, 0, 1, 0,
          row(200, 40, 1, 0),  // left tab
          row(200, 40, 1, 0)  // right tab
        ),
        row(0, 0, 1, 1, menu)  // main content
      ));

    Dimension x {lay, width, X};
    Dimension y {lay, height, Y};

    for (u32 i: range(len(lay))) {
      drawRect(x.pos[i], y.pos[i], x.size[i], y.size[i], grayu8(i * 137));
    }
  }

  app->draw();
}

struct ModelUnifom {
  f32 u_model[3][4];  // column-major
  f32 u_view[4][4];  // column-major
};

void draw() {
  R = rot_y(R);

  ModelUnifom u;
  new (u.u_model) decltype(u.u_model) {R[0], R[1], R[2], 0, R[3], R[4], R[5], 0, R[6], R[7], R[8], 0};
  new (u.u_view) decltype(u.u_view) {
    2 / aspect, 0, 0, 0,
    0, 2, 0, 0,
    0, 0, 1, 1,
    0, 0, 0, 4};

  fillBuffer(uModelBuffer, 0, &u, sizeof(u));

  enableDepthTest();
  useProgram(program);
  u32 N = 32;
  drawPoints(0, N * N);

  useProgram(pointProgram);
  drawPoints(0, 1);
  disableDepthTest();

  app->draw();

  //redraw();
}
