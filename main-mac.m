#include <stdlib.h>
#include <unistd.h>

#import <Cocoa/Cocoa.h>

#include "glad.h"

#define check(c) \
  if (!(c)) \
  abort()

typedef unsigned u32;
typedef float f32;
typedef unsigned long uptr;
typedef unsigned char u8;

extern u8 const key_map[256];

void app_start();
void resize(u32 w, u32 h, u32 k);
void draw();
void scroll(f32 x, f32 y, f32 dx, f32 dy);
void key(u32 k);

void console_log(char const* s, u32 n) { check(write(1, s, n) == n); }

CFBundleRef opengl;

void gui_init(void) {
  [NSApplication sharedApplication];
  [NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];
  [NSApp activateIgnoringOtherApps:YES];
  opengl = CFBundleGetBundleWithIdentifier(CFSTR("com.apple.opengl"));
}

typedef void (*Proc)(void);

Proc getProcAddressNSGL(const char* name) {
  CFStringRef cfName = CFStringCreateWithCString(
      kCFAllocatorDefault, name, kCFStringEncodingASCII);
  Proc symbol = (Proc) CFBundleGetFunctionPointerForName(opengl, cfName);
  CFRelease(cfName);
  return symbol;
}

@interface OpenGlView: NSOpenGLView {
@public
  void* user;
  bool dragging;
  bool resizing;
  bool resized;
  u32 new_w;
  u32 new_h;
}

@end

@implementation OpenGlView

- (void)prepareOpenGL {
  [super prepareOpenGL];

  // Synchronize buffer swaps with vertical refresh rate
  GLint swapInt = 1;
  [[self openGLContext] setValues:&swapInt forParameter:NSOpenGLCPSwapInterval];
}

- (void)drawRect:(NSRect)dirtyRect {
  NSOpenGLContext* context = self.openGLContext;
  [context makeCurrentContext];
  if (resized) {
    CGSize size = self.bounds.size;
    double w = size.width;
    double h = size.height;
    double k = self.window.screen.backingScaleFactor;
    check((u32) w == w);
    check((u32) h == h);
    check((u32) k == k);
    resizing = 1;
    resize((u32) w, (u32) h, (u32) k);
    resizing = 0;
    resized = 0;
  }
  draw();
  glFlush();
}

- (BOOL)acceptsFirstResponder {
  return YES;
}

- (BOOL)acceptsFirstMouse:(NSEvent*)event {
  return YES;
}

- (void)mouseDown:(NSEvent*)event {
  NSPoint mouseLocation = [event locationInWindow];
  double y = [self frame].size.height - mouseLocation.y;
  if (y < 40) {
    dragging = true;
    [[self window] performWindowDragWithEvent:event];
    return;
  } else {
    dragging = false;
  }
  // cocoaMouseDown(user, mouseLocation.x, y);
}

- (void)rightMouseDown:(NSEvent*)event {
  NSPoint mouseLocation = [event locationInWindow];
  double y = [self frame].size.height - mouseLocation.y;
  // cocoaRightMouse(user, mouseLocation.x, y);
}

- (void)mouseDragged:(NSEvent*)event {
  if (dragging)
    return;
  NSPoint mouseLocation = [event locationInWindow];
  double y = [self frame].size.height - mouseLocation.y;
  // cocoaMouseDrag(user, mouseLocation.x, y);
}

- (void)reshape {
  [super reshape];
  CGRect frame = [self frame];
  [[self openGLContext] makeCurrentContext];
  resized = 1;
}

- (void)viewDidChangeBackingProperties {
  // TODO: Compute new content scale?
  // const NSRect contentRect = [self frame];
  // const NSRect fbRect = [self convertRectToBacking:contentRect];
  // const float xscale = fbRect.size.width / contentRect.size.width;
  // const float yscale = fbRect.size.height / contentRect.size.height;
}

- (void)keyDown:(NSEvent*)event {
  u32 code = event.keyCode;
  if (code > sizeof(key_map) / sizeof(key_map[0]))
    return;
  key(key_map[code]);
}

- (void)keyUp:(NSEvent*)event {
  // cocoaKeyUp(user, [event keyCode]);
}

- (void)flagsChanged:(NSEvent*)event {
  // cocoaFlagsChanged(user, [event keyCode], (unsigned) [event modifierFlags]);
}

- (void)scrollWheel:(NSEvent*)event {
  double deltaX = [event scrollingDeltaX];
  double deltaY = [event scrollingDeltaY];
  if ([event hasPreciseScrollingDeltas]) {
    deltaX *= .1;
    deltaY *= .1;
  }
  if (deltaX == 0. && deltaY == 0.)
    return;
  NSPoint mouseLocation = [event locationInWindow];
  double y = [self frame].size.height - mouseLocation.y;
  scroll((f32) mouseLocation.x, (f32) y, (f32) deltaX, (f32) deltaY);
}

@end

@interface GlWindow: NSWindow {
}
@end

@implementation GlWindow

- (BOOL)canBecomeKeyWindow {
  return YES;
}

- (BOOL)canBecomeMainWindow {
  return YES;
}

@end

void* createNativeWindow(u32 dx, u32 dy, u32* contentScale) {
  NSRect contentRect = NSMakeRect(0, 0, dx, dy);
  NSWindow* window =
      [[GlWindow alloc] initWithContentRect:contentRect
                                  styleMask:NSWindowStyleMaskResizable
                                    backing:NSBackingStoreBuffered
                                      defer:NO];

  [window center];

  // for resizable (?)
  const NSWindowCollectionBehavior behavior =
      NSWindowCollectionBehaviorFullScreenPrimary |
      NSWindowCollectionBehaviorManaged;
  [window setCollectionBehavior:behavior];

#define addAttrib(a) \
  { \
    assert((size_t) index < sizeof(attribs) / sizeof(attribs[0])); \
    attribs[index++] = a; \
  }
#define setAttrib(a, v) \
  { \
    addAttrib(a); \
    addAttrib(v); \
  }

  NSOpenGLPixelFormatAttribute attribs[40];
  int index = 0;
  // addAttrib(NSOpenGLPFAAccelerated);
  // addAttrib(NSOpenGLPFAClosestPolicy);
  setAttrib(NSOpenGLPFAOpenGLProfile, NSOpenGLProfileVersion3_2Core);
  addAttrib(0);

#undef addAttrib
#undef setAttrib

  NSOpenGLPixelFormat* pixelFormat =
      [[NSOpenGLPixelFormat alloc] initWithAttributes:attribs];

  OpenGlView* view = [[OpenGlView alloc] initWithFrame:contentRect
                                           pixelFormat:pixelFormat];
  [window setContentView:view];

  [[view openGLContext] makeCurrentContext];
  gladLoadGL(getProcAddressNSGL);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  [window setAcceptsMouseMovedEvents:YES];

  CGFloat fScale = [[window screen] backingScaleFactor];
  u32 uScale = (u32) fScale;
  check((f32) uScale == fScale);
  *contentScale = uScale;

  return window;
}

static OpenGlView* gv;

void open_with_callback(u32 w, u32 h) {
  check(!w == !h);
  if (!w) {
    w = 1280;
    h = 720;
  }
  u32 contentScale;
  void* window_ = createNativeWindow(w, h, &contentScale);

  NSWindow* window = (NSWindow*) window_;
  NSView* contentView = [window contentView];
  gv = (OpenGlView*) contentView;
  gv->user = nil;

  [window makeKeyAndOrderFront:nil];
  app_start();
}

void gui_run(void) { [NSApp run]; }

u32 create_shader(u32 type, char const* source, int len) {
  u32 shader = glCreateShader(type);
  char const* srcs[] = {"#version 330 core\n", source};
  int lens[] = {18, len};
  glShaderSource(shader, 2, srcs, lens);
  glCompileShader(shader);
  GLint compileStatus;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &compileStatus);
  if (!compileStatus) {
    char info[4096];
    GLsizei infoLen;
    glGetShaderInfoLog(shader, sizeof(info), &infoLen, info);
    printf("Could not compile WebGL program.\n\n%.*s\n", infoLen, info);
    abort();
  }
  return shader;
};

//////// BEGIN ENVIRONMENT FUNCTIONS

// void abort() { throw 0; }

void enableBlend() { glEnable(GL_BLEND); }
void disableBlend() { glDisable(GL_BLEND); }
void enableDepthTest() { glEnable(GL_DEPTH_TEST); }
void disableDepthTest() { glDisable(GL_DEPTH_TEST); }
void demo() {
  glVertexAttrib2f(0, 0, 0);
  glVertexAttrib1f(1, 10);  // 0.1, 0.1);
  glEnable(GL_BLEND);
}
void vertexAttrib1f(u32 attrib, f32 x) { glVertexAttrib1f(attrib, x); }
void vertexAttrib2f(u32 attrib, f32 x, f32 y) {
  glVertexAttrib2f(attrib, x, y);
}
void vertexAttrib3f(u32 attrib, f32 x, f32 y, f32 z) {
  glVertexAttrib3f(attrib, x, y, z);
}
void vertexAttrib4f(u32 attrib, f32 x, f32 y, f32 z, f32 w) {
  glVertexAttrib4f(attrib, x, y, z, w);
}
u32 makeVertexArray() {
  u32 n;
  glGenVertexArrays(1, &n);
  return n;
}
void bindVertexArray(u32 vertexArray) { glBindVertexArray(vertexArray); }
void dropVertexArray(u32 vertexArray) { glDeleteVertexArrays(1, &vertexArray); }
u32 make_vertex_shader(char const* source, u32 len) {
  return create_shader(GL_VERTEX_SHADER, source, len);
}
u32 make_fragment_shader(char const* source, u32 len) {
  return create_shader(GL_FRAGMENT_SHADER, source, len);
}
void set_viewport(u32 x, u32 y, u32 dx, u32 dy) {
  glViewport((int) x, (int) y, (int) dx, (int) dy);
}
void vertexAttributeArrayU8(
    u32 attrib, u32 buffer, u32 components, u32 stride, u32 offset,
    u32 instanceDivisor) {
  glEnableVertexAttribArray(attrib);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribIPointer(
      attrib, components, GL_UNSIGNED_BYTE, stride,
      (void const*) (uptr) offset);
  glVertexAttribDivisor(attrib, instanceDivisor);
}
void vertexAttributeArrayI16(
    u32 attrib, u32 buffer, u32 components, u32 stride, u32 offset,
    u32 instanceDivisor) {
  glEnableVertexAttribArray(attrib);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribIPointer(
      attrib, components, GL_SHORT, stride, (void const*) (uptr) offset);
  glVertexAttribDivisor(attrib, instanceDivisor);
}
u32 make_program(u32 const* shader_id, u32 n_shader) {
  u32 program = glCreateProgram();
  for (u32 i = 0; i < n_shader; ++i)
    glAttachShader(program, shader_id[i]);
  glLinkProgram(program);
  GLint linkStatus;
  glGetProgramiv(program, GL_LINK_STATUS, &linkStatus);
  if (!linkStatus) {
    char info[4096];
    GLsizei infoLen;
    glGetProgramInfoLog(program, sizeof(info), &infoLen, info);
    printf("Could not compile WebGL program.\n\n%.*s\n", infoLen, info);
    abort();
  }
  return program;
}
void useProgram(u32 program) { glUseProgram(program); }
void drawPoints(u32 base, u32 count) { glDrawArrays(GL_POINTS, base, count); }
void drawTriangleStrip(u32 base, u32 count, u32 instanceCount) {
  glDrawArraysInstanced(GL_TRIANGLE_STRIP, base, count, instanceCount);
}
void redraw() {
  if (gv->resizing)
    return;
  dispatch_async(dispatch_get_main_queue(), ^{ [gv setNeedsDisplay:YES]; });
}
u32 makeBuffer(u32 size) {
  u32 buffer;
  glGenBuffers(1, &buffer);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glBufferData(GL_ARRAY_BUFFER, size, 0, GL_DYNAMIC_DRAW);
  return buffer;
}
void dropBuffer(u32 buffer) { glDeleteBuffers(1, &buffer); }
void bindUniformBuffer(u32 index, u32 buffer) {
  glBindBufferBase(GL_UNIFORM_BUFFER, index, buffer);
}
void fillBuffer(u32 buffer, u32 ofs, void const* src, u32 size) {
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glBufferSubData(GL_ARRAY_BUFFER, ofs, size, src);
}
u32 getUniformBlock(u32 program, char const* name, u32 len) {
  char* c_name = strndup(name, len);
  u32 ans = glGetUniformBlockIndex(program, c_name);
  free(c_name);
  return ans;
}
void bindUniformBlock(u32 program, u32 block, u32 binding) {
  glUniformBlockBinding(program, block, binding);
}
void make_texture(void const* src, u32 size) {
  u32 tex;
  glGenTextures(1, &tex);
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexImage2D(
      GL_TEXTURE_2D,
      0,  // mip level
      GL_R8UI,  // internal format
      size,  // width
      1,  // height
      0,  // border
      GL_RED_INTEGER,  // source format
      GL_UNSIGNED_BYTE,  // source type
      src);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

//////// END ENVIRONMENT FUNCTIONS

int main() {
  gui_init();
  open_with_callback(1280, 720);
  gui_run();
}
