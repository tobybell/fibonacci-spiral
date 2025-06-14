#include "print.hh"
#include "gui.hh"

using f32 = float;

extern "C" {

u32 make_vertex_shader(char const* source, u32 len);
u32 make_fragment_shader(char const* source, u32 len);
u32 make_program(u32 const* shaders, u32 n_shaders);
void redraw();
u32 getUniformBlock(u32 program, char const* name, u32 len);
u32 bindUniformBlock(u32 program, u32 block, u32 binding);
u32 makeBuffer(u32 size);
void bindUniformBuffer(u32 binding, u32 buffer);
void fillBuffer(u32 buffer, u32 ofs, void const* src, u32 size);
f32 cos(f32);
f32 sin(f32);

void useProgram(u32 program);
void drawPoints(u32 base, u32 count);

void start();
void draw();
void scroll(f32 x, f32 y, f32 dx, f32 dy);
void key(u32 id);

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
    map[32] = World1;
    map[32] = World2;
    map[27] = Escape;
    map[13] = Enter;
    map[9] = Tab;
    map[8] = Backspace;
    map[32] = Insert;
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

void start() {
  init_heap();
  println("Hello!");

  u32 s0 = make_vertex_shader(R"gl(#version 300 es
#define pi 3.1415926535897932
#define N 32
uniform Settings {
  mat3 u_model;
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

  println("Made"_s);

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

void scroll(f32 x, f32 y, f32 dx, f32 dy) {
  R = rot_x(rot_y(R, dx / 50), dy / 50);
}

void key(u32 id) {
  auto key = key_map.map[id];
  println("Got key "_s, key);
}

void draw() {
  R = rot_y(R);
  f32 eye[] {R[0], R[1], R[2], 0, R[3], R[4], R[5], 0, R[6], R[7], R[8], 0};
  fillBuffer(uModelBuffer, 0, eye, sizeof(eye));

  useProgram(program);
  u32 N = 32;
  drawPoints(0, N * N);

  useProgram(pointProgram);
  drawPoints(0, 1);

  redraw();
}
