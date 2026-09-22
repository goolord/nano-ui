/* OpenGL 3.2 core renderer for the RGFW host.
 *
 * Geometry is the core's shared draw buffer uploaded as-is (32-byte vertices:
 * position, RGBA, UV; 32-bit indices) and drawn per command under a scissor.
 * Text is pre-clipped glyph quads in physical pixels, sampled from an R8
 * coverage atlas.
 *
 * Frames draw into a retained offscreen framebuffer, which keeps the pixels
 * outside a frame's damage, and every present copies it to the window: the
 * window's back buffer is undefined after a swap, the retained one is not. GL entry points are resolved through RGFW's loader, so this
 * file needs no GL headers and adds no link dependencies. */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef void (*rgfw_proc)(void);
extern rgfw_proc RGFW_getProcAddress_OpenGL(const char* procname);

#if defined(_WIN32)
#define NGL_API __stdcall
#else
#define NGL_API
#endif

typedef uint32_t GLenum;
typedef uint32_t GLuint;
typedef uint32_t GLbitfield;
typedef int32_t GLint;
typedef int32_t GLsizei;
typedef float GLfloat;
typedef uint8_t GLboolean;
typedef char GLchar;
typedef intptr_t GLsizeiptr;

#define GL_FALSE 0
#define GL_ONE 1
#define GL_TRIANGLES 0x0004
#define GL_SRC_ALPHA 0x0302
#define GL_ONE_MINUS_SRC_ALPHA 0x0303
#define GL_CULL_FACE 0x0B44
#define GL_DEPTH_TEST 0x0B71
#define GL_BLEND 0x0BE2
#define GL_SCISSOR_TEST 0x0C11
#define GL_UNPACK_ALIGNMENT 0x0CF5
#define GL_PACK_ALIGNMENT 0x0D05
#define GL_TEXTURE_2D 0x0DE1
#define GL_UNSIGNED_BYTE 0x1401
#define GL_UNSIGNED_INT 0x1405
#define GL_FLOAT 0x1406
#define GL_RED 0x1903
#define GL_VERSION 0x1F02
#define GL_NEAREST 0x2600
#define GL_TEXTURE_MAG_FILTER 0x2800
#define GL_TEXTURE_MIN_FILTER 0x2801
#define GL_TEXTURE_WRAP_S 0x2802
#define GL_TEXTURE_WRAP_T 0x2803
#define GL_COLOR_BUFFER_BIT 0x4000
#define GL_CLAMP_TO_EDGE 0x812F
#define GL_R8 0x8229
#define GL_RGBA 0x1908
#define GL_RGBA8 0x8058
#define GL_READ_FRAMEBUFFER 0x8CA8
#define GL_DRAW_FRAMEBUFFER 0x8CA9
#define GL_FRAMEBUFFER_COMPLETE 0x8CD5
#define GL_COLOR_ATTACHMENT0 0x8CE0
#define GL_FRAMEBUFFER 0x8D40
#define GL_TEXTURE0 0x84C0
#define GL_ARRAY_BUFFER 0x8892
#define GL_ELEMENT_ARRAY_BUFFER 0x8893
#define GL_STREAM_DRAW 0x88E0
#define GL_FRAGMENT_SHADER 0x8B30
#define GL_VERTEX_SHADER 0x8B31
#define GL_COMPILE_STATUS 0x8B81
#define GL_LINK_STATUS 0x8B82
#define GL_FRAMEBUFFER_SRGB 0x8DB9

#define NGL_FUNCS                                                                                   \
  X(const uint8_t*, GetString, (GLenum))                                                            \
  X(void, Viewport, (GLint, GLint, GLsizei, GLsizei))                                               \
  X(void, Scissor, (GLint, GLint, GLsizei, GLsizei))                                                \
  X(void, ClearColor, (GLfloat, GLfloat, GLfloat, GLfloat))                                         \
  X(void, Clear, (GLbitfield))                                                                      \
  X(void, Enable, (GLenum))                                                                         \
  X(void, Disable, (GLenum))                                                                        \
  X(void, BlendFuncSeparate, (GLenum, GLenum, GLenum, GLenum))                                      \
  X(void, PixelStorei, (GLenum, GLint))                                                             \
  X(GLuint, CreateShader, (GLenum))                                                                 \
  X(void, ShaderSource, (GLuint, GLsizei, const GLchar* const*, const GLint*))                      \
  X(void, CompileShader, (GLuint))                                                                  \
  X(void, GetShaderiv, (GLuint, GLenum, GLint*))                                                    \
  X(void, GetShaderInfoLog, (GLuint, GLsizei, GLsizei*, GLchar*))                                   \
  X(void, DeleteShader, (GLuint))                                                                   \
  X(GLuint, CreateProgram, (void))                                                                  \
  X(void, AttachShader, (GLuint, GLuint))                                                           \
  X(void, BindAttribLocation, (GLuint, GLuint, const GLchar*))                                      \
  X(void, LinkProgram, (GLuint))                                                                    \
  X(void, GetProgramiv, (GLuint, GLenum, GLint*))                                                   \
  X(void, GetProgramInfoLog, (GLuint, GLsizei, GLsizei*, GLchar*))                                  \
  X(void, DeleteProgram, (GLuint))                                                                  \
  X(void, UseProgram, (GLuint))                                                                     \
  X(GLint, GetUniformLocation, (GLuint, const GLchar*))                                             \
  X(void, Uniform1i, (GLint, GLint))                                                                \
  X(void, Uniform1f, (GLint, GLfloat))                                                              \
  X(void, Uniform2f, (GLint, GLfloat, GLfloat))                                                     \
  X(void, GenVertexArrays, (GLsizei, GLuint*))                                                      \
  X(void, BindVertexArray, (GLuint))                                                                \
  X(void, DeleteVertexArrays, (GLsizei, const GLuint*))                                             \
  X(void, GenBuffers, (GLsizei, GLuint*))                                                           \
  X(void, BindBuffer, (GLenum, GLuint))                                                             \
  X(void, BufferData, (GLenum, GLsizeiptr, const void*, GLenum))                                    \
  X(void, DeleteBuffers, (GLsizei, const GLuint*))                                                  \
  X(void, EnableVertexAttribArray, (GLuint))                                                        \
  X(void, VertexAttribPointer, (GLuint, GLint, GLenum, GLboolean, GLsizei, const void*))            \
  X(void, DrawElements, (GLenum, GLsizei, GLenum, const void*))                                     \
  X(void, DrawArrays, (GLenum, GLint, GLsizei))                                                     \
  X(void, GenTextures, (GLsizei, GLuint*))                                                          \
  X(void, BindTexture, (GLenum, GLuint))                                                            \
  X(void, ActiveTexture, (GLenum))                                                                  \
  X(void, TexParameteri, (GLenum, GLenum, GLint))                                                   \
  X(void, TexImage2D, (GLenum, GLint, GLint, GLsizei, GLsizei, GLint, GLenum, GLenum, const void*)) \
  X(void, DeleteTextures, (GLsizei, const GLuint*))                                                  X(void, ReadPixels, (GLint, GLint, GLsizei, GLsizei, GLenum, GLenum, void*))                      \
  X(void, GenFramebuffers, (GLsizei, GLuint*))                                                      \
  X(void, BindFramebuffer, (GLenum, GLuint))                                                        \
  X(void, FramebufferTexture2D, (GLenum, GLenum, GLenum, GLuint, GLint))                            \
  X(GLenum, CheckFramebufferStatus, (GLenum))                                                       \
  X(void, DeleteFramebuffers, (GLsizei, const GLuint*))                                             \
  X(void, BlitFramebuffer, (GLint, GLint, GLint, GLint, GLint, GLint, GLint, GLint, GLbitfield, GLenum))

typedef struct ngl_api {
#define X(ret, name, args) ret(NGL_API* name) args;
  NGL_FUNCS
#undef X
} ngl_api;

/* Must match the core's vertexSize / indexSize. */
enum { NGL_VERTEX_BYTES = 32, NGL_INDEX_BYTES = 4 };

enum { NGL_MODE_NONE, NGL_MODE_GEOMETRY, NGL_MODE_TEXT };

typedef struct nano_ui_gl {
  ngl_api gl;
  GLuint program;
  GLint uViewport, uScale, uTextured, uAtlas;
  GLuint geomVao, geomVbo, geomEbo;
  GLuint textVao, textVbo;
  GLuint atlas;
  GLuint retainFbo, retainTex;
  int32_t retainCapW, retainCapH; /* texture size, the window rounded up */
  int32_t fbW, fbH;
  int32_t dx0, dy0, dx1, dy1; /* this frame's damage, top-left physical pixels */
  float scale; /* logical -> physical, for geometry vertices */
  int mode;
} nano_ui_gl;

static const char* ngl_vertex_src =
    "#version 150\n"
    "in vec2 aPos;\n"
    "in vec4 aColor;\n"
    "in vec2 aUV;\n"
    "uniform vec2 uViewport;\n"
    "uniform float uScale;\n"
    "out vec4 vColor;\n"
    "out vec2 vUV;\n"
    "void main() {\n"
    "  vec2 p = aPos * uScale;\n"
    "  gl_Position = vec4(p.x / uViewport.x * 2.0 - 1.0, 1.0 - p.y / uViewport.y * 2.0, 0.0, 1.0);\n"
    "  vColor = aColor;\n"
    "  vUV = aUV;\n"
    "}\n";

static const char* ngl_fragment_src =
    "#version 150\n"
    "in vec4 vColor;\n"
    "in vec2 vUV;\n"
    "uniform sampler2D uAtlas;\n"
    "uniform float uTextured;\n"
    "out vec4 fragColor;\n"
    "void main() {\n"
    "  float coverage = uTextured > 0.5 ? texture(uAtlas, vUV).r : 1.0;\n"
    "  fragColor = vec4(vColor.rgb, vColor.a * coverage);\n"
    "}\n";

static int ngl_load(ngl_api* gl) {
#define X(ret, name, args)                                                                          \
  gl->name = (ret(NGL_API*) args)RGFW_getProcAddress_OpenGL("gl" #name);                           \
  if (gl->name == NULL) {                                                                           \
    fprintf(stderr, "nano-ui-rgfw: OpenGL entry point gl%s is unavailable\n", #name);              \
    return 0;                                                                                       \
  }
  NGL_FUNCS
#undef X
  return 1;
}

static GLuint ngl_compile(ngl_api* gl, GLenum type, const char* src) {
  GLuint shader = gl->CreateShader(type);
  GLint ok = 0;
  gl->ShaderSource(shader, 1, &src, NULL);
  gl->CompileShader(shader);
  gl->GetShaderiv(shader, GL_COMPILE_STATUS, &ok);
  if (!ok) {
    char log[1024];
    gl->GetShaderInfoLog(shader, sizeof log, NULL, log);
    fprintf(stderr, "nano-ui-rgfw: shader compile failed: %s\n", log);
    gl->DeleteShader(shader);
    return 0;
  }
  return shader;
}

static int ngl_link(nano_ui_gl* r) {
  ngl_api* gl = &r->gl;
  GLuint vs = ngl_compile(gl, GL_VERTEX_SHADER, ngl_vertex_src);
  GLuint fs = vs ? ngl_compile(gl, GL_FRAGMENT_SHADER, ngl_fragment_src) : 0;
  GLint ok = 0;
  if (!fs) {
    if (vs) gl->DeleteShader(vs);
    return 0;
  }
  r->program = gl->CreateProgram();
  gl->AttachShader(r->program, vs);
  gl->AttachShader(r->program, fs);
  gl->BindAttribLocation(r->program, 0, "aPos");
  gl->BindAttribLocation(r->program, 1, "aColor");
  gl->BindAttribLocation(r->program, 2, "aUV");
  gl->LinkProgram(r->program);
  gl->DeleteShader(vs);
  gl->DeleteShader(fs);
  gl->GetProgramiv(r->program, GL_LINK_STATUS, &ok);
  if (!ok) {
    char log[1024];
    gl->GetProgramInfoLog(r->program, sizeof log, NULL, log);
    fprintf(stderr, "nano-ui-rgfw: shader link failed: %s\n", log);
    return 0;
  }
  r->uViewport = gl->GetUniformLocation(r->program, "uViewport");
  r->uScale = gl->GetUniformLocation(r->program, "uScale");
  r->uTextured = gl->GetUniformLocation(r->program, "uTextured");
  r->uAtlas = gl->GetUniformLocation(r->program, "uAtlas");
  return 1;
}

static void ngl_vertex_layout(ngl_api* gl) {
  gl->EnableVertexAttribArray(0);
  gl->VertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, NGL_VERTEX_BYTES, (const void*)0);
  gl->EnableVertexAttribArray(1);
  gl->VertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, NGL_VERTEX_BYTES, (const void*)8);
  gl->EnableVertexAttribArray(2);
  gl->VertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, NGL_VERTEX_BYTES, (const void*)24);
}

static int ngl_major_version(const uint8_t* version) {
  int major = 0;
  if (version == NULL) return 0;
  for (; *version >= '0' && *version <= '9'; version++) major = major * 10 + (*version - '0');
  return major;
}

void nano_ui_gl_destroy(nano_ui_gl* r);

/* Requires the target context to be current. Returns NULL (after logging the
 * reason to stderr) when the context cannot run the renderer. */
nano_ui_gl* nano_ui_gl_create(void) {
  nano_ui_gl* r = (nano_ui_gl*)calloc(1, sizeof *r);
  if (r == NULL) return NULL;
  ngl_api* gl = &r->gl;
  if (!ngl_load(gl)) {
    free(r);
    return NULL;
  }
  const uint8_t* version = gl->GetString(GL_VERSION);
  if (ngl_major_version(version) < 3) {
    fprintf(stderr, "nano-ui-rgfw: OpenGL 3.2 required, context reports %s\n",
            version ? (const char*)version : "no version");
    free(r);
    return NULL;
  }
  if (!ngl_link(r)) {
    nano_ui_gl_destroy(r);
    return NULL;
  }

  gl->GenVertexArrays(1, &r->geomVao);
  gl->GenBuffers(1, &r->geomVbo);
  gl->GenBuffers(1, &r->geomEbo);
  gl->BindVertexArray(r->geomVao);
  gl->BindBuffer(GL_ARRAY_BUFFER, r->geomVbo);
  gl->BindBuffer(GL_ELEMENT_ARRAY_BUFFER, r->geomEbo);
  ngl_vertex_layout(gl);

  gl->GenVertexArrays(1, &r->textVao);
  gl->GenBuffers(1, &r->textVbo);
  gl->BindVertexArray(r->textVao);
  gl->BindBuffer(GL_ARRAY_BUFFER, r->textVbo);
  ngl_vertex_layout(gl);
  gl->BindVertexArray(0);

  gl->GenTextures(1, &r->atlas);
  gl->BindTexture(GL_TEXTURE_2D, r->atlas);
  gl->TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  gl->TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  gl->TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  gl->TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  gl->UseProgram(r->program);
  gl->Uniform1i(r->uAtlas, 0);
  return r;
}

void nano_ui_gl_destroy(nano_ui_gl* r) {
  if (r == NULL) return;
  ngl_api* gl = &r->gl;
  if (r->retainFbo) gl->DeleteFramebuffers(1, &r->retainFbo);
  if (r->retainTex) gl->DeleteTextures(1, &r->retainTex);
  if (r->atlas) gl->DeleteTextures(1, &r->atlas);
  if (r->textVbo) gl->DeleteBuffers(1, &r->textVbo);
  if (r->textVao) gl->DeleteVertexArrays(1, &r->textVao);
  if (r->geomEbo) gl->DeleteBuffers(1, &r->geomEbo);
  if (r->geomVbo) gl->DeleteBuffers(1, &r->geomVbo);
  if (r->geomVao) gl->DeleteVertexArrays(1, &r->geomVao);
  if (r->program) gl->DeleteProgram(r->program);
  free(r);
}

/* Replace the glyph atlas. Pixels are the software blitter's white-on-black
 * bake; the low byte of each pixel is its coverage. Returns 0 on allocation
 * failure. */
int32_t nano_ui_gl_upload_atlas(nano_ui_gl* r, const uint32_t* pixels, int32_t w, int32_t h) {
  ngl_api* gl = &r->gl;
  size_t n = (size_t)w * (size_t)h;
  uint8_t* coverage = (uint8_t*)malloc(n > 0 ? n : 1);
  if (coverage == NULL) return 0;
  for (size_t i = 0; i < n; i++) coverage[i] = (uint8_t)(pixels[i] & 0xFF);
  gl->BindTexture(GL_TEXTURE_2D, r->atlas);
  gl->PixelStorei(GL_UNPACK_ALIGNMENT, 1);
  gl->TexImage2D(GL_TEXTURE_2D, 0, GL_R8, w, h, 0, GL_RED, GL_UNSIGNED_BYTE, coverage);
  free(coverage);
  return 1;
}

/* Pixels the retained texture is rounded up to, in each dimension. */
enum { NGL_RETAIN_BLOCK = 256 };

static int32_t ngl_round_up(int32_t n) {
  return (n + NGL_RETAIN_BLOCK - 1) / NGL_RETAIN_BLOCK * NGL_RETAIN_BLOCK;
}

/* Make the retained framebuffer hold at least w x h pixels. A texture well
 * past the window is given back, but not for a shrink of a block or so, which
 * a drag back out would undo. Returns 1 if the texture was replaced, which
 * discards its pixels, 0 if it was kept, and -1 on failure. */
static int ngl_ensure_retain(nano_ui_gl* r, int32_t w, int32_t h) {
  ngl_api* gl = &r->gl;
  int fits = r->retainFbo && w <= r->retainCapW && h <= r->retainCapH;
  int roomy = r->retainCapW - w > 2 * NGL_RETAIN_BLOCK || r->retainCapH - h > 2 * NGL_RETAIN_BLOCK;
  if (fits && !roomy) return 0;
  if (!r->retainFbo) {
    gl->GenFramebuffers(1, &r->retainFbo);
    gl->GenTextures(1, &r->retainTex);
  }
  r->retainCapW = ngl_round_up(w);
  r->retainCapH = ngl_round_up(h);
  gl->BindTexture(GL_TEXTURE_2D, r->retainTex);
  gl->TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  gl->TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  gl->TexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, r->retainCapW, r->retainCapH, 0, GL_RGBA,
                 GL_UNSIGNED_BYTE, NULL);
  gl->BindFramebuffer(GL_FRAMEBUFFER, r->retainFbo);
  gl->FramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, r->retainTex, 0);
  if (gl->CheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
    fprintf(stderr, "nano-ui-rgfw: retained framebuffer is incomplete\n");
    gl->BindFramebuffer(GL_FRAMEBUFFER, 0);
    r->retainCapW = 0;
    r->retainCapH = 0;
    return -1;
  }
  return 1;
}

/* Start a frame on the retained framebuffer: viewport, fixed pipeline state,
 * and the logical -> physical scale for this frame's geometry. A full frame
 * clears everything; otherwise only the damage box (x0, y0, x1, y1 in
 * top-left physical pixels) is cleared and drawn, and the pixels outside it
 * are last frame's. A replaced framebuffer holds no last frame, so the frame
 * is cleared in full, and returns 2: the caller must draw it again in full.
 * Returns 0 if the retained framebuffer cannot be made, and 1 otherwise. */
int32_t nano_ui_gl_begin(nano_ui_gl* r, int32_t fbW, int32_t fbH, float scale, float red,
                         float green, float blue, int32_t full, int32_t x0, int32_t y0, int32_t x1,
                         int32_t y1) {
  ngl_api* gl = &r->gl;
  r->fbW = fbW > 0 ? fbW : 1;
  r->fbH = fbH > 0 ? fbH : 1;
  r->scale = scale;
  r->mode = NGL_MODE_NONE;
  int replaced = ngl_ensure_retain(r, r->fbW, r->fbH);
  if (replaced < 0) return 0;
  if (full || replaced) {
    x0 = 0;
    y0 = 0;
    x1 = r->fbW;
    y1 = r->fbH;
  }
  r->dx0 = x0 > 0 ? x0 : 0;
  r->dy0 = y0 > 0 ? y0 : 0;
  r->dx1 = x1 < r->fbW ? x1 : r->fbW;
  r->dy1 = y1 < r->fbH ? y1 : r->fbH;
  gl->BindFramebuffer(GL_FRAMEBUFFER, r->retainFbo);
  gl->Viewport(0, 0, r->fbW, r->fbH);
  gl->Disable(GL_DEPTH_TEST);
  gl->Disable(GL_CULL_FACE);
  gl->Disable(GL_FRAMEBUFFER_SRGB);
  gl->Enable(GL_SCISSOR_TEST);
  gl->Scissor(r->dx0, r->fbH - r->dy1, r->dx1 - r->dx0, r->dy1 - r->dy0);
  gl->ClearColor(red, green, blue, 1.0f);
  gl->Clear(GL_COLOR_BUFFER_BIT);
  gl->Enable(GL_BLEND);
  gl->BlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  gl->UseProgram(r->program);
  gl->Uniform2f(r->uViewport, (GLfloat)r->fbW, (GLfloat)r->fbH);
  gl->ActiveTexture(GL_TEXTURE0);
  gl->BindTexture(GL_TEXTURE_2D, r->atlas);
  return replaced && !full ? 2 : 1;
}

/* Read the retained frame as RGBA rows, bottom row first, into out, which
 * holds fbW * fbH * 4 bytes of the last frame's size. */
void nano_ui_gl_read_retained(nano_ui_gl* r, uint8_t* out) {
  ngl_api* gl = &r->gl;
  gl->BindFramebuffer(GL_READ_FRAMEBUFFER, r->retainFbo);
  gl->PixelStorei(GL_PACK_ALIGNMENT, 1);
  gl->ReadPixels(0, 0, r->fbW, r->fbH, GL_RGBA, GL_UNSIGNED_BYTE, out);
  gl->BindFramebuffer(GL_FRAMEBUFFER, 0);
}

/* Copy the retained frame to the window's back buffer. The caller swaps. */
void nano_ui_gl_present(nano_ui_gl* r) {
  ngl_api* gl = &r->gl;
  gl->Disable(GL_SCISSOR_TEST);
  gl->BindFramebuffer(GL_READ_FRAMEBUFFER, r->retainFbo);
  gl->BindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  gl->BlitFramebuffer(0, 0, r->fbW, r->fbH, 0, 0, r->fbW, r->fbH, GL_COLOR_BUFFER_BIT, GL_NEAREST);
  gl->BindFramebuffer(GL_FRAMEBUFFER, 0);
  r->mode = NGL_MODE_NONE;
}

void nano_ui_gl_upload_geometry(nano_ui_gl* r, const void* vertices, int32_t vertexCount,
                                const void* indices, int32_t indexCount) {
  ngl_api* gl = &r->gl;
  r->mode = NGL_MODE_NONE;
  gl->BindVertexArray(r->geomVao);
  gl->BindBuffer(GL_ARRAY_BUFFER, r->geomVbo);
  gl->BufferData(GL_ARRAY_BUFFER, (GLsizeiptr)vertexCount * NGL_VERTEX_BYTES,
                 vertexCount > 0 ? vertices : NULL, GL_STREAM_DRAW);
  gl->BufferData(GL_ELEMENT_ARRAY_BUFFER, (GLsizeiptr)indexCount * NGL_INDEX_BYTES,
                 indexCount > 0 ? indices : NULL, GL_STREAM_DRAW);
}

void nano_ui_gl_upload_text(nano_ui_gl* r, const void* vertices, int32_t vertexCount) {
  ngl_api* gl = &r->gl;
  r->mode = NGL_MODE_NONE;
  gl->BindVertexArray(r->textVao);
  gl->BindBuffer(GL_ARRAY_BUFFER, r->textVbo);
  gl->BufferData(GL_ARRAY_BUFFER, (GLsizeiptr)vertexCount * NGL_VERTEX_BYTES,
                 vertexCount > 0 ? vertices : NULL, GL_STREAM_DRAW);
}

/* Draw one command's index range. The clip is in top-left physical pixels,
 * already intersected with the framebuffer, and is cut to the frame's damage;
 * vertices are logical and scaled in the vertex shader by the frame's scale. */
void nano_ui_gl_draw_geometry(nano_ui_gl* r, int32_t x0, int32_t y0, int32_t x1, int32_t y1,
                              uint32_t firstIndex, uint32_t indexCount) {
  ngl_api* gl = &r->gl;
  if (x0 < r->dx0) x0 = r->dx0;
  if (y0 < r->dy0) y0 = r->dy0;
  if (x1 > r->dx1) x1 = r->dx1;
  if (y1 > r->dy1) y1 = r->dy1;
  if (x0 >= x1 || y0 >= y1) return;
  if (r->mode != NGL_MODE_GEOMETRY) {
    gl->BindVertexArray(r->geomVao);
    gl->Uniform1f(r->uTextured, 0.0f);
    gl->Uniform1f(r->uScale, r->scale);
    r->mode = NGL_MODE_GEOMETRY;
  }
  gl->Scissor(x0, r->fbH - y1, x1 - x0, y1 - y0);
  gl->DrawElements(GL_TRIANGLES, (GLsizei)indexCount, GL_UNSIGNED_INT,
                   (const void*)((uintptr_t)firstIndex * NGL_INDEX_BYTES));
}

/* Draw a range of the uploaded glyph vertices (physical pixels, pre-clipped
 * to the frame's damage). */
void nano_ui_gl_draw_text(nano_ui_gl* r, int32_t firstVertex, int32_t vertexCount) {
  ngl_api* gl = &r->gl;
  if (vertexCount <= 0) return;
  if (r->mode != NGL_MODE_TEXT) {
    gl->BindVertexArray(r->textVao);
    gl->Uniform1f(r->uTextured, 1.0f);
    gl->Uniform1f(r->uScale, 1.0f);
    gl->Scissor(r->dx0, r->fbH - r->dy1, r->dx1 - r->dx0, r->dy1 - r->dy0);
    r->mode = NGL_MODE_TEXT;
  }
  gl->DrawArrays(GL_TRIANGLES, firstVertex, vertexCount);
}
