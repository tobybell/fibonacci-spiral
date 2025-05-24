using u32 = unsigned;
using f32 = float;

extern "C" {

u32 make_vertex_shader(char const* source, u32 len);
u32 make_fragment_shader(char const* source, u32 len);
u32 make_program(u32 const* shaders, u32 n_shaders);
void console_log(char const* str, u32 len);
void redraw();
u32 getUniformBlock(u32 program, char const* name, u32 len);
u32 bindUniformBlock(u32 program, u32 block, u32 binding);
u32 makeBuffer(u32 size);
void bindUniformBuffer(u32 binding, u32 buffer);
void fillBuffer(u32 buffer, u32 ofs, void const* src, u32 size);

void useProgram(u32 program);
void drawPoints(u32 base, u32 count);

void start();
void draw();

}

namespace {

template <class T>
struct Ref {
  T* base;
  u32 size;
  constexpr Ref() = default;
  constexpr Ref(T* b, u32 s): base(b), size(s) {}
  template <u32 N>
  constexpr Ref(T (&x)[N]): base(x), size(N) {}
};

using Str = Ref<char const>;

constexpr Str operator""_s(char const* s, unsigned long n) {
  return {s, n};
}

u32 make_vertex_shader(Str s) { return ::make_vertex_shader(s.base, s.size); }
u32 make_fragment_shader(Str s) { return ::make_fragment_shader(s.base, s.size); }
void console_log(Str s) { ::console_log(s.base, s.size); }
u32 make_program(Ref<u32 const> shaders) { return ::make_program(shaders.base, shaders.size); }

u32 program;
u32 pointProgram;
u32 uModelBuffer;

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

}

void start() {
  console_log("Hello!", 6);

  u32 s0 = make_vertex_shader(R"gl(#version 300 es
#define pi 3.1415926535897932
uniform Settings {
  mat3 u_model;
};
void main() {
  gl_PointSize = 4.0;
  float i = float(gl_VertexID);
  float phi = (1. + sqrt(5.)) / 2.;
  float xi = i / phi;
  float yi = i / 1000.;
  float th = 2. * pi * xi;
  float cphi = 2. * yi - 1.;
  float sphi = sqrt(1. - cphi * cphi);
  vec3 pos = u_model * vec3(cos(th) * sphi, sin(th) * sphi, cphi);
  gl_Position = vec4(pos * vec3(2., 2., 1.), 4. + pos.z);
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
};
void main() {
  gl_PointSize = 4.0;
  vec3 pos = u_model * u_pos;
  gl_Position = vec4(pos * vec3(2., 2., 1.), 4. + pos.z);
}
)gl");
u32 s3 = make_fragment_shader(R"gl(#version 300 es
out mediump vec4 frag_color;
void main() {
  frag_color = vec4(1, 0, 1, 1);
}
)gl");
  pointProgram = make_program((u32[]) {s2, s3});

  console_log("Made", 4);

  uModelBuffer = makeBuffer(48);
  u32 uPosBuffer = makeBuffer(16);

  bindUniformBuffer(0, uModelBuffer);
  bindUniformBuffer(1, uPosBuffer);

  R = {1, 0, 0, 0, 1, 0, 0, 0, 1};

  f32 pos[] {1, 0, 0};
  fillBuffer(uPosBuffer, 0, pos, sizeof(pos));

  u32 programSettings = getUniformBlock(program, "Settings", 8);
  u32 pointProgramSettings = getUniformBlock(pointProgram, "Settings", 8);
  u32 pointProgramSettings2 = getUniformBlock(pointProgram, "Settings2", 9);
  bindUniformBlock(program, programSettings, 0);
  bindUniformBlock(pointProgram, pointProgramSettings2, 0);
  bindUniformBlock(pointProgram, pointProgramSettings, 1);
}

void draw() {
  R = rot_y(R);
  f32 eye[] {R[0], R[1], R[2], 0, R[3], R[4], R[5], 0, R[6], R[7], R[8], 0};
  fillBuffer(uModelBuffer, 0, eye, sizeof(eye));

  useProgram(program);
  drawPoints(0, 1000);

  useProgram(pointProgram);
  drawPoints(0, 1);

  redraw();
}
