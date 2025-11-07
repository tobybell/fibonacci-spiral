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
void dropBuffer(u32 buffer);
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
  u32 attrib, u32 buffer, u32 components, u8 stride,
  u32 offset, u32 instanceDivisor);
void vertexAttributeArrayI16(
  u32 attrib, u32 buffer, u32 components, u8 stride,
  u32 offset, u32 instanceDivisor);
void enableBlend();
void disableBlend();
void enableDepthTest();
void disableDepthTest();

void set_viewport(i32 x, i32 y, u32 dx, u32 dy);
void useProgram(u32 program);
void drawPoints(u32 base, u32 count);
void drawTriangleStrip(u32 base, u32 count, u32 instanceCount);
void demo();

void start();
void draw();
void scroll(f32 x, f32 y, f32 dx, f32 dy);
void key(u32 id);
bool mouseDown(f32 x, f32 y);
void resize(u32 width, u32 height, u32 scale);

}

void init_heap();

namespace {

f32 abs(f32 x) { return __builtin_fabsf(x); }

template <class T, class F>
void sort(Ref<T> elem, F&& cmp) {
  quicksort(elem.begin(), elem.end(), cmp);
}

u32 make_vertex_shader(Str s) { return ::make_vertex_shader(s.base, s.size); }
u32 make_fragment_shader(Str s) { return ::make_fragment_shader(s.base, s.size); }
u32 make_program(Ref<u32 const> shaders) { return ::make_program(shaders.base, shaders.size); }
u32 getUniformBlock(u32 program, Str s) { return ::getUniformBlock(program, s.base, s.size); }

u32 program;
u32 pointProgram;
u32 textProgram;
u32 textPositionBuffer;
u32 multiLineTextProgram;
u32 circleProgram;
u32 roundRectProgram;
u32 rectProgram;
u32 defaultVao;
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

void make_text_program() {
  u32 v_multiline = make_vertex_shader(R"gl(
uniform Position {
  ivec2 uPos;
};
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
  vec2 coord = vec2(-1, 1) + vCoord * charSize;
  vec2 shift = vec2((aColRow * ivec2(6, 8) + uPos) * ivec2(2, -2)) / uResolution;
  gl_Position = vec4(shift + coord, 0, 1);
}
  )gl"_s);
  u32 s0 = make_vertex_shader(R"gl(
uniform Position {
  ivec2 uPos;
};
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
  vec2 coord = vec2(-1, 1) + vCoord * charSize;
  vec2 shift = vec2(gl_InstanceID * 6 + uPos.x, -uPos.y) * 2.0 / uResolution;
  gl_Position = vec4(shift + coord, 0, 1);
}
  )gl"_s);
  u32 s1 = make_fragment_shader(R"gl(
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

  textProgram = make_program((u32[]) {s0, s1});
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
  Vec3 operator*(f32 v) const { return {x * v, y * v, z * v}; }
  Vec3 operator-(Vec3 const& v) const { return {x - v.x, y - v.y, z - v.z}; }
  Vec3 operator+(Vec3 const& v) const { return {x + v.x, y + v.y, z + v.z}; }
};

Vec3 rgbu8(u8 r, u8 g, u8 b) {
  return {f32(r) / 255, f32(g) / 255, f32(b) / 255};
}

Vec3 grayu8(u8 g) { return rgbu8(g, g, g); }

void circle(f32 x, f32 y, f32 r, Vec3 topColor, Vec3 bottomColor) {
  useProgram(circleProgram);
  enableBlend();
  bindVertexArray(defaultVao);
  vertexAttrib2f(0, x, y);
  vertexAttrib1f(1, r);
  vertexAttrib3f(2, topColor.x, topColor.y, topColor.z);
  vertexAttrib3f(3, bottomColor.x, bottomColor.y, bottomColor.z);
  drawTriangleStrip(0, 4, 1);
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
  bindVertexArray(defaultVao);
  vertexAttrib4f(
    0, -1 + f32(x) * 2 / gWidth, 1 - f32(y) * 2 / gHeight, f32(w) * 2 / gWidth,
    -f32(h) * 2 / gHeight);
  vertexAttrib3f(1, color.x, color.y, color.z);
  drawTriangleStrip(0, 4, 1);
}

struct MultiLineString {
  VertexArray va {};
  u32 stringBuffer = makeBuffer(64);
  u32 colRowBuffer = makeBuffer(256);
  u32 size {};
  MultiLineString() {
    va.bind();
    vertexAttributeArrayU8(0, stringBuffer, 1, 0, 0, 1);
    vertexAttributeArrayI16(1, colRowBuffer, 2, 0, 0, 1);
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
    drawTriangleStrip(0, 4, size);
  }
};

void drawText(Str text, u32 x, u32 y, i16 (*colRow)[2]) {
  static_assert(is_trivially_copyable<u32[2]>);
  u32 size = len(text);
  VertexArray va {};
  u32 buf = makeBuffer(size * 5);
  useProgram(multiLineTextProgram);
  va.bind();
  vertexAttributeArrayU8(0, buf, 1, 0, size * 4, 1);
  vertexAttributeArrayI16(1, buf, 2, 0, 0, 1);
  fillBuffer(buf, 0, colRow, size * 4);
  fillBuffer(buf, size * 4, text.begin(), size);
  fillBuffer(textPositionBuffer, 0, (u32[]) {x, y + 7}, 8);
  drawTriangleStrip(0, 4, size);
  dropBuffer(buf);
}

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

enum Direction: u8 { X, Y };

struct Sizing {
  u32 min[2];
  u32 pref[2];
  bool grow[2];
};

struct LayoutEntry {
  u32 min[2];
  u32 pref[2];
  bool grow[2];

  u16 childGap;
  u16 pad[4];  // [Direction][Side]

  u32 kids;
  Direction direction;  // meaningless if `!kids`

  void* user;

  bool active;
  Array<LayoutEntry> (*activeGenerate)(void*);
  void (*drop)(void*);
  void (*draw)(void*, i32 x, i32 y, u32 dx, u32 dy);
  void (*click)(void*);
};

constexpr LayoutEntry spacer {.grow = {1, 1}};

template <class T>
LayoutEntry activeComponent(T* obj) {
  return {
      .user = obj,
      .active = 1,
      .activeGenerate = [](void* p) { return (*(T*) p)(); },
      .drop = [](void* p) { delete (T*) p; }};
}

LayoutEntry minSpace(u32 minSize) {
  return {.min = {minSize, minSize}, .grow= {1, 1}};
}

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
  static T const* dat(Span<T> const& x) { return x.begin(); }
  static u32 get_len(T const& x) { return 1; }
  static u32 get_len(List<T> const& x) { return len(x); }
  static u32 get_len(Array<T> const& x) { return len(x); }
  static u32 get_len(Span<T> const& x) { return len(x); }
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
  return cat(LayoutEntry {
    .min={minx, miny},
    .grow={growx, growy},
    .kids=sizeof...(T),
    .direction=Y}, kids...);
}

template <class... T>
auto colGap(
    u32 minx, u32 miny, bool growx, bool growy, u16 pad, u16 childGap, T&&... kids) {
  return cat(LayoutEntry {
    .min={minx, miny},
    .grow={growx, growy},
    .childGap=childGap,
    .pad={pad, pad, pad, pad},
    .kids=sizeof...(T),
    .direction=Y}, kids...);
}

auto colGapN(
    u32 minx, u32 miny, bool growx, bool growy, u16 pad, u16 childGap, u32 kids, Span<LayoutEntry> content) {
  return cat(LayoutEntry {
    .min={minx, miny},
    .grow={growx, growy},
    .childGap=childGap,
    .pad={pad, pad, pad, pad},
    .kids=kids,
    .direction=Y}, content);
}

template <class... T>
auto row(u32 minx, u32 miny, bool growx, bool growy, T&&... kids) {
  return cat(LayoutEntry {
    .min={minx, miny},
    .grow={growx, growy},
    .kids=sizeof...(T),
    .direction=X}, kids...);
}

struct TextNode {
  String value;
  TextNode(String x): value(move(x)) {}
  void draw(i32 x, i32 y, u32 dx, u32 dy) {
    (void) dx;
    (void) dy;

    u32 maxCols = (dx + 1) / 6;

    // layout text
    u32 size = len(value);
    Array<i16[2]> colRow(len(value));
    i16 row = 0;
    i16 col = 0;
    u32 lastStart = 0;
    bool madeProgress = 0;
    for (u32 i = 0; i < size; ++i) {
      if (value[i] == ' ') {
        madeProgress = 1;
        lastStart = i + 1;
      } else if (col >= maxCols) {
        check(madeProgress);
        i = lastStart;
        madeProgress = 0;
        ++row;
        col = 0;
      }
      colRow[i][0] = col++;
      colRow[i][1] = row;
    }

    drawText(value, x, y, colRow.begin());
  }
};

auto text(Str x) {
  u32 len = x.size;

  u32 maxWordLen = 0;
  u32 wordStart = 0;
  for (u32 i = 0; i < len; ++i) {
    if (x[i] == ' ') {
      u32 wordLen = i - wordStart;
      wordStart = i + 1;
      if (wordLen > maxWordLen)
        maxWordLen = wordLen;
    }
  }
  if (len - wordStart > maxWordLen)
    maxWordLen = len - wordStart;

  u32 minWidth = 6 * maxWordLen - 1;
  u32 maxWidth = 6 * len - 1;
  return cat(
      LayoutEntry {
          .min = {minWidth, 7},
          .pref = {maxWidth, 7},
          .user = new TextNode(x),
          .drop = [](void* p) { delete (TextNode*) p; },
          .draw = [](void* p, auto... x) { ((TextNode*) p)->draw(x...); }});

  // add child to container... need to bubble up: layout child, layout parent,
  // layout parent, layout parent, etc. for each parent that changed, bubble
  // down width sizing.
  //
  // I don't think anything should be able to have children added to it.
  // You should be able to say, here is my tree; this is it.
  //
  // An app container, you pass properties to it, and you pass children to it.
  // What is children? Children should be able to say, add node, remove node,
  // re-order nodes.
  //
  // Object in memory should stay in place, but references to such nodes can be
  // re-ordered easily.
  //
  // Is there such a thing as an object not currently in the hierarchy? I think so, yes.
  // If it's not in the hierarchy, it should not be drawn. Eh... maybe this
  // means that removing it from the hierarchy should simply destroy whatever
  // object represents its existence in the hierarchy.
  //
  // So... such a "mounting" object would be given some content, and a mount
  // point or parent layout node.
}

struct RadioNode {
  virtual void click() = 0;
  virtual bool checked() = 0;
  virtual ~RadioNode() = default;

  void draw(i32 x, i32 y, u32 dx, u32 dy) {
    check(dx == 14);
    check(dy == 14);
    f32 fx = -1 + f32(x + 7) * 2 / gWidth;
    f32 fy = 1 - f32(y + 7) * 2 / gHeight;
    auto on = checked();
    on ? enabledRadioButton(fx, fy) : disabledRadioButton(fx, fy);
  }

  Sizing sizing() const { return {.min = {14, 14}, .pref = {14, 14}}; }
};

struct Switch {
  bool on {};
  f32 t {};

  void draw(i32 x, i32 y, u32 dx, u32 dy) {
    useProgram(roundRectProgram);
    enableBlend();
    bindVertexArray(defaultVao);
    f32 gx = 2 / gWidth;
    f32 gy = 2 / gHeight;
    vertexAttrib2f(0, f32(x) * gx - 1, 1 - f32(y) * gy);
    vertexAttrib2f(1, 26, 15);

    f32 tdes = f32(on);
    t += (tdes - t) * 0.25f;

    auto offColor = Vec3 {.36f, .36f, .36f};
    auto onColor = Vec3 {.18f, .37f, .8f};
    auto color = offColor + (onColor - offColor) * t;

    vertexAttrib3f(2, color.x, color.y, color.z);
    drawTriangleStrip(0, 4, 1);
    disableBlend();

    f32 sx = 7.5 + 11 * t;
    circle(
        (f32(x) + sx) * gx - 1, 1 - (f32(y) + 7.5) * gy, 6.5f, {.8f, .8f, .8f},
        {.77f, .77f, .77f});

    if (abs(t - tdes) < 1e-2) {
      t = tdes;
    } else {
      redraw();
    }
  }

  Sizing sizing() const { return {.min = {26, 15}, .pref = {26, 15}}; }

  void click() {
    on = !on;
    redraw();
  }
};

template <class T>
auto makeNode(T& x) {
  auto sz = x.sizing();
  return LayoutEntry {
      .min = {sz.min[0], sz.min[1]},
      .pref = {sz.pref[0], sz.pref[1]},
      .grow = {sz.grow[0], sz.grow[1]},
      .user = &x,
      .draw = [](void* p, auto... x) { ((T*) p)->draw(x...); },
      .click = [](void* p) { ((T*) p)->click(); }};
}

template <class T>
auto makeNode(T* x) {
  auto sz = x->sizing();
  return LayoutEntry {
      .min = {sz.min[0], sz.min[1]},
      .pref = {sz.pref[0], sz.pref[1]},
      .grow = {sz.grow[0], sz.grow[1]},
      .user = x,
      .drop = [](void* p) { delete (T*) p; },
      .draw = [](void* p, auto... x) { ((T*) p)->draw(x...); },
      .click = [](void* p) { ((T*) p)->click(); }};
}

template <class T>
Ref<T> slice(Ref<T> x, u32 base) {
  check(base <= len(x));
  return {x.begin() + base, len(x) - base};
}

template <class T>
Span<T> slice(List<T> const& x, u32 base) {
  return slice(x.span(), base);
}

template <class T>
Ref<T> slice(Ref<T> x, u32 base, u32 size) {
  check(base + size <= len(x));
  return {x.begin() + base, size};
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

struct ShrinkCheckpoint {
  u32 id;
  u32 size;
  bool min;
};

static GrowthDistribution shrink_distribution(
    Span<ShrinkCheckpoint> checkpoints, Ref<bool> frozen, u32 deficit) {
  u32 n = 0;
  i32 budget = -i32(deficit);
  for (auto [id, size, min]: checkpoints) {
    if (n && i32(size) <= budget / i32(n))
      return {n, u32(budget)};
    if (min) {
      frozen[id] = 1;
      budget -= size;
      --n;
    } else {
      budget += size;
      ++n;
    }
  }
  return {n, u32(budget)};
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

void reverse(ArrayList<u32>& x) {
  reverse<u32>(x.list);
  reverse<u32>(x.ofs);
  u32 end = len(x.list);
  for (u32& ofs: x.ofs)
    ofs = end - ofs;
}

struct Dimension {
  Array<u32> size;
  Array<u32> pos;

  Dimension() = default;

  Dimension(
    Span<LayoutEntry> node, u32 available, Direction d) {
    u32 n = len(node);

    // bottom-up pass
    ArrayList<u32> node_growable_kids;
    ArrayList<u32> nodeKids;
    ArrayList<u32> nodeShrinkableKids;
    size = Array<u32>(n);
    Array<u32> minSize(n);
    {
      struct MinMax {
        u32 min;
        u32 pref;
      };
      List<MinMax> stack;
      List<u32> growable;
      List<u32> shrinkable;
      List<u32> all;
      struct Since { u32 growable, shrinkable; };
      List<Since> since;

      for (u32 i = n; i--;) {
        auto& o = node[i];
        auto n_kids = o.kids;

        // Index growable and shrinkable children
        auto n_since = len(since);
        since.push({len(growable), len(shrinkable)});
        auto since_i = since[n_since - n_kids];
        since.size -= n_kids;
        node_growable_kids.push(slice(growable, since_i.growable));
        nodeShrinkableKids.push(slice(shrinkable, since_i.shrinkable));
        nodeKids.push(slice(all, len(all) - n_kids));
        all.size -= n_kids;
        growable.size = since_i.growable;
        shrinkable.size = since_i.shrinkable;
        if (o.grow[d])
          growable.push(i);

        auto pad = totalPad(o, d);
        u32 totMin;
        u32 totPref;
        if (!n_kids) {
          totMin = o.min[d];
          totPref = o.pref[d];
        } else if (o.direction == d) {
          u32 minSum = 0;
          u32 preSum = 0;
          for (u32 j = 0; j < n_kids; ++j) {
            auto& top = stack.last();
            minSum += top.min;
            preSum += top.pref;
            stack.pop();
          }
          u32 totPad = pad + o.childGap * (n_kids - 1);
          totMin = minSum + totPad;
          totPref = preSum + totPad;
        } else {
          u32 minMax = 0;
          u32 preMax = 0;
          for (u32 j = 0; j < n_kids; ++j) {
            auto& top = stack.last();
            if (top.min > minMax)
              minMax = top.min;
            if (top.pref > preMax)
              preMax = top.pref;
            stack.pop();
          }
          totMin = minMax + pad;
          totPref = preMax + pad;
        }
        if (totPref < totMin)
          totPref = totMin;
        size[i] = totPref;
        minSize[i] = totMin;
        if (totMin < totPref)
          shrinkable.push(i);
        all.push(i);
        auto& top = stack.push({totMin, totPref});
      }

      check(len(stack) == 1);
    }

    reverse(node_growable_kids);
    reverse(nodeShrinkableKids);
    reverse(nodeKids);

    auto orig_size = size;

    // top-down: compute actual size (based on available)
    //   distribute extra space to growable children
    size[0] = available;

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
      u32 avail = size[i];

      if (node[i].direction != d) {  // across
        u32 innerSpace = avail - totalPad(node[i], d);
        for (u32 k: nodeKids[i]) {
          check(innerSpace >= minSize[k]);
          if (innerSpace <= size[k] || node[k].grow[d]) {
            size[k] = innerSpace;
          }
        }
      } else if (avail > orig_size[i] && node[i].grow[d]) {  // if grow
        u32 amount = avail - orig_size[i];
        auto kids = node_growable_kids[i];
        sort(kids, [this](u32 a, u32 b) { return size[a] < size[b]; });
        auto gdist = growth_distribution(size, kids, amount);
        for (u32 j: range(gdist.how_many_kids_can_we_grow)) {
          u32 size_j = gdist.to_what_total_size / (gdist.how_many_kids_can_we_grow - j);
          size[kids[j]] = size_j;
          gdist.to_what_total_size -= size_j;
        }
      } else if (avail < orig_size[i]) {  // if shrink
        u32 amount = orig_size[i] - avail;
        auto kids = nodeShrinkableKids[i];

        sort(kids, [this](u32 a, u32 b) { return size[a] > size[b]; });
        auto nk = len(kids);
        Array<ShrinkCheckpoint> sc(nk * 2);
        Array<bool> frozen(nk);
        for (u32 j: range(nk)) {
          sc[2 * j] = {j, size[kids[j]]};
          sc[2 * j + 1] = {j, minSize[kids[j]], 1};
        }

        sort(sc.mut(), [this](auto& a, auto& b) { return a.size > b.size; });
        auto dist = shrink_distribution(sc, frozen, amount);

        // loop over...
        // if see preferred size, start shrinking that kid
        // if see 
        for (u32 j = 0; j < nk; ++j) {
          if (frozen[j]) {
            size[kids[j]] = minSize[kids[j]];
          } else if (!dist.how_many_kids_can_we_grow) {
            break;
          } else {
            u32 size_j = dist.to_what_total_size / dist.how_many_kids_can_we_grow;
            size[kids[j]] = size_j;
            dist.to_what_total_size -= size_j;
            --dist.how_many_kids_can_we_grow;
          }
        }
        check(!dist.how_many_kids_can_we_grow);
      }

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

constexpr Vec3 red {1, 0, 0};
constexpr Vec3 green {0, 1, 0};
constexpr Vec3 blue {0, 0, 1};

template <class T>
Array<T> empty(u32 n) {
  return Array((T*) malloc(n * sizeof(T)), n);
}

struct App {
  List<char> string;
  MultiLineString str;

  State<bool> onoff;
  Work cleanup;

  Array<LayoutEntry> lay;
  Dimension x;
  Dimension y;

  u32 width;
  u32 height;

  bool didLayout {};

  List<f32> positions;
  List<bool> selected;

  struct ActiveRegion {
    void* aux;
    Array<LayoutEntry> (*gen)(void*);
    void (*drop)(void*);
    u32 begin;
    u32 end;
    Array<u32> kids;
  };

  List<ActiveRegion> regions;
  List<u32> freeRegions;

  void dropKids(ActiveRegion& region) {
    for (u32 k: region.kids) {
      auto& kid = regions[k];
      dropKids(kid);
      kid.kids = {};
      kid.drop(kid.aux);
      freeRegions.push(k);
    }
  }

  void evaluateLayout(
      List<LayoutEntry>& dst, List<u32>& kids, Array<LayoutEntry> content) {
    for (auto& entry: content) {
      if (entry.active) {
        u32 id;
        if (freeRegions) {
          id = freeRegions.last();
          freeRegions.pop();
        } else {
          id = len(regions);
          regions.emplace();
        }
        kids.push(id);
        auto& ans = regions[id];
        ans.aux = entry.user;
        ans.gen = entry.activeGenerate;
        ans.drop = entry.drop;
        ans.begin = len(dst);
        List<u32> newKids;
        evaluateLayout(dst, newKids, ans.gen(ans.aux));
        ans.end = len(dst);
        ans.kids = newKids.take();
      } else {
        dst.push(entry);
      }
    }
  }

  List<u32> stealAsList(Array<u32>& x) {
    auto n = exchange(x.size, 0u);
    return List(exchange(x.data, nullptr), n, n);
  }

  void regenRegion(u32 id) {
    auto& region = regions[id];
    dropKids(region);
    List<LayoutEntry> newLayout;

    // free old kids
    for (u32 i = region.begin; i < region.end; ++i) {
      auto& entry = lay[i];
      check(!entry.active);
      if (entry.drop)
        entry.drop(entry.user);
    }

    extend(newLayout, slice(lay.span(), 0, region.begin));
    auto newKids = stealAsList(region.kids);
    newKids.shrink(0);
    evaluateLayout(newLayout, newKids, region.gen(region.aux));
    auto newEnd = len(newLayout);
    extend(newLayout, slice(lay.span(), region.end));
    region.end = newEnd;
    region.kids = newKids.take();
    lay = newLayout.take();
  }

  Array<LayoutEntry> makeLayout() const {
    println("makeLayout");

    auto sidebar = col(200, 0, 0, 1);
    auto title_bar = row(0, 0, 1, 0,
        row(200, 0, 1, 1),  // left spacer
        row(400, 30, 0, 0), // title
        spacer);  // right spacer

    LayoutEntry menuComponent {
        .user = (void*) this,
        .active = 1,
        .activeGenerate =
            [](void* ptr) {
              auto& self = *(App*) ptr;
              List<LayoutEntry> menuItems;
              for (auto i: range(len(self.positions))) {
                auto& p = self.positions[i];

                struct MyRadioNode: RadioNode {
                  App& app;
                  u32 i;
                  MyRadioNode(App& app_, u32 i_): app(app_), i(i_) {}
                  bool checked() override { return app.selected[i]; }
                  void click() override {
                    auto& on = app.selected[i];
                    on = !on;
                    redraw();
                  }
                };

                extend(
                    menuItems,
                    Ref(
                        row(0, 0, 1, 0, text(strcat("Item ", p)), minSpace(10),
                            makeNode(new MyRadioNode(self, i)))));
              }
              menuItems.push(makeNode(new Switch()));
              return colGapN(
                  0, 0, 0, 0, 5, 5, len(self.positions) + 1, menuItems);
            },
        .drop = [](void*) {}};

    auto lipsum = text("Toby: I think either it's an unfortunate \"be careful\" issue that we just don't resolve, or else all names require some sort of let/def statement the first time they're used. The issue I have with that is that there are times when the intended behavior is definitely to use a global name, which is why I feel like it might just be a thing people need to get used to being careful about.");
    auto lipsum2 = text("Toby: I think either it's an unfortunate \"be careful\" issue that we just don't resolve, or else all names require some sort of let/def statement the first time they're used. The issue I have with that is that there are times when the intended behavior is definitely to use a global name, which is why I feel like it might just be a thing people need to get used to being careful about.");

    return row(
        0, 0, 1, 1, sidebar,
        col(0, 0, 1, 1, title_bar,
            // tab bar
            row(0, 0, 1, 0, row(200, 40, 1, 0),  // left tab
                row(200, 40, 1, 0)  // right tab
                ),
            row(0, 0, 1, 1, menuComponent, lipsum, lipsum2)  // main content
            ));
  }

  App() {
    positions.push(3.14f);
    selected.push(0);

    List<LayoutEntry> news;
    List<u32> unusedKids;
    evaluateLayout(news, unusedKids, makeLayout());
    lay = news.take();
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
    redraw();
  }

  bool mouseDown(f32 gx, f32 gy) {
    println("mouseDown ", gx, ' ', gy);

    u32 px = u32((gx + 1) / 2 * f32(width));
    u32 py = u32((1 - gy) / 2 * f32(height));
    for (u32 j: range(len(lay))) {
      u32 i = len(lay) - j - 1;
      auto& node = lay[i];
      if (!node.click)
        continue;
      if (px >= x.pos[i] && px < x.pos[i] + x.size[i] &&
          py >= y.pos[i] && py < y.pos[i] + y.size[i]) {
        println("hit");
        node.click(node.user);
        return 1;
      }
    }

    positions.push(gx);
    redraw();

    // TODO: Allow re-laying-out only parts of the tree?
    regenRegion(0);
    didLayout = 0;

    return 1;
  }

  void resize(u32 w, u32 h) {
    width = w;
    height = h;
    didLayout = 0;
  }

  u32 drawCount {};
  void draw() {

    if (!didLayout) {
      didLayout = 1;
      x = Dimension {lay, width, X};
      y = Dimension {lay, height, Y};
    }

    for (u32 i: range(len(lay))) {
      auto& node = lay[i];
      if (node.draw) {
        node.draw(node.user, x.pos[i], y.pos[i], x.size[i], y.size[i]);
      } else {
        drawRect(x.pos[i], y.pos[i], x.size[i], y.size[i], grayu8(i * 137));
      }
    }

    str.draw();
  }
};

static App* app;

void start() {
  println("Hello!");
  make_texture(font_data, sizeof(font_data));

  make_text_program();

  u32 circle_vshader = make_vertex_shader(R"gl(
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
  u32 circle_fshader = make_fragment_shader(R"gl(
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
  highp float alpha = min(1.0, 2.0 * (vRadius - rad));
  lowp vec3 color = vTop * vGradient + vBottom * (1.0 - vGradient);
  oColor = vec4(color, alpha);
}
)gl");
  circleProgram = make_program((u32[]) {circle_vshader, circle_fshader});

  u32 roundRectVertexShader = make_vertex_shader(R"gl(
uniform Resolution {
  highp vec2 uResolution;
  highp vec2 uPixel;
};
layout (location = 0) in highp vec2 center;
layout (location = 1) in highp vec2 size;
layout (location = 2) in lowp vec3 color;
out highp vec2 vCoord;
flat out lowp vec3 fColor;
void main() {
  fColor = color;
  vec2 coord = vec2(gl_VertexID / 2, gl_VertexID % 2) * size;
  vCoord = coord;
  gl_Position = vec4(center + coord * vec2(1, -1) * uPixel, 0, 1);
}
)gl");
  u32 roundRectFragmentShader = make_fragment_shader(R"gl(
uniform Resolution {
  highp vec2 uResolution;
  highp vec2 uPixel;
};
flat in lowp vec3 fColor;
in highp vec2 vCoord;
out lowp vec4 oColor;
void main() {
  mediump vec2 o = abs(vCoord - vec2(13, 7.5)) - vec2(5.5, 0);
  highp float l;
  if (o.x > 0. && o.y > 0.) {
    l = length(o);
  } else if (o.x > 0.) {
    l = o.x;
  } else if (o.y > 0.) {
    l = o.y;
  } else {
    l = 0.;
  }

  lowp vec4 c0 = vec4(vec3(0.36), 0.0);
  lowp vec4 c1 = vec4(1.0 - 0.8 * (1.0 - fColor), 1.0);
  lowp vec4 c2 = vec4(fColor, 1.0);

  if (l > 7.75)
    discard;
  if (l > 7.25)
    oColor = mix(c1, c0, (l - 7.25) * 2.0);
  else if (l > 6.25)
    oColor = mix(c2, c1, l - 6.25);
  else
    oColor = c2;
}
)gl");
  roundRectProgram =
      make_program((u32[]) {roundRectVertexShader, roundRectFragmentShader});

  u32 rect_vshader = make_vertex_shader(R"gl(
layout (location = 0) in mediump vec4 aRect;
layout (location = 1) in mediump vec3 aColor;
flat out lowp vec3 vColor;
void main() {
  vec2 coord = vec2(gl_VertexID % 2, gl_VertexID / 2);
  vColor = aColor;
  gl_Position = vec4(aRect.xy + coord * aRect.zw, 0, 1);
}
)gl");
  u32 rect_fshader = make_fragment_shader(R"gl(
flat in lowp vec3 vColor;
out lowp vec4 oColor;
void main() {
  oColor = vec4(vColor, 1);
}
)gl");
  rectProgram = make_program((u32[]) {rect_vshader, rect_fshader});
  defaultVao = makeVertexArray();

  app = new App;
  u32 s0 = make_vertex_shader(R"gl(
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

  u32 s1 = make_fragment_shader(R"gl(
out mediump vec4 frag_color;
void main() {
  mediump float rz = (gl_FragCoord.z * 2. - 1.) / gl_FragCoord.w;
  mediump float bright = (1. - rz) / 2.;
  frag_color = vec4(bright, bright, bright, 1);
}
  )gl"_s);

  program = make_program((u32[]) {s0, s1});

  u32 s2 = make_vertex_shader(R"gl(
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
u32 s3 = make_fragment_shader(R"gl(
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
  {
    u32 u = getUniformBlock(roundRectProgram, "Resolution");
    bindUniformBlock(roundRectProgram, u, 2);
  }

  textPositionBuffer = makeBuffer(8);
  bindUniformBuffer(3, textPositionBuffer);
  u32 textPosition = getUniformBlock(multiLineTextProgram, "Position");
  bindUniformBlock(multiLineTextProgram, textPosition, 3);
}

void scroll(f32 x, f32 y, f32 dx, f32 dy) {
  R = rot_x(rot_y(R, dx / 50), dy / 50);
  redraw();
}

void key(u32 id) {
  app->key((Key) id);
}

bool mouseDown(f32 x, f32 y) {
  return app->mouseDown(x, y);
}

f32 aspect = 1;

void resize(u32 width, u32 height, u32 scale) {
  gWidth = f32(width);
  gHeight = f32(height);
  aspect = f32(width) / f32(height);
  fillBuffer(uResolutionBuffer, 0, (f32[]) {f32(width), f32(height)}, 8);
  fillBuffer(uResolutionBuffer, 8, (f32[]) {2 / f32(width), 2 / f32(height)}, 8);
  set_viewport(0, 0, width * scale, height * scale);

  app->resize(width, height);
  redraw();
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
