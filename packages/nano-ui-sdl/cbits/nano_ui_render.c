#include <SDL3/SDL.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nano_ui_ttf_shared.h"

/* The batch reads the core's draw buffer in place as SDL_Vertex: x and y,
 * then red, green, blue and alpha, then u and v, as 32-bit floats. */
_Static_assert(sizeof(SDL_Vertex) == NANO_UI_VERTEX_BYTES, "SDL_Vertex must be the core's 32-byte vertex");
_Static_assert(offsetof(SDL_Vertex, position) == 0, "SDL_Vertex starts with its position");
_Static_assert(offsetof(SDL_Vertex, color) == 8, "SDL_Vertex colour follows the 2-float position");
_Static_assert(offsetof(SDL_Vertex, tex_coord) == 24, "SDL_Vertex UV follows the 4-float colour");
_Static_assert(sizeof(SDL_FColor) == 16, "SDL_Vertex colour is four floats");

typedef struct {
    SDL_Texture *tex;
    int cap_w;
    int cap_h;
    int in_use;
} NanoUiTargetSlot;

#if SDL_VERSION_ATLEAST(3, 4, 0)
typedef struct {
    uint64_t hash;
    SDL_GPUShader *shader;
    SDL_GPURenderState *state;
    int failed;
} NanoUiGpuShaderEntry;
#endif

/* Minimal OpenGL 3.2 types, constants, and dynamic loader for running
 * nano-ui-shader GLSL 150 programs on SDL's "opengl" renderer without
 * extra header or library dependencies. */
#if defined(_WIN32)
#define NUI_GL_API __stdcall
#else
#define NUI_GL_API
#endif

typedef uint32_t NuiGLenum;
typedef uint32_t NuiGLuint;
typedef int32_t NuiGLint;
typedef int32_t NuiGLsizei;
typedef float NuiGLfloat;
typedef uint8_t NuiGLboolean;
typedef char NuiGLchar;
typedef intptr_t NuiGLsizeiptr;

#define NUI_GL_FALSE 0
#define NUI_GL_ONE 1
#define NUI_GL_TRIANGLES 0x0004
#define NUI_GL_SRC_ALPHA 0x0302
#define NUI_GL_ONE_MINUS_SRC_ALPHA 0x0303
#define NUI_GL_CULL_FACE 0x0B44
#define NUI_GL_DEPTH_TEST 0x0B71
#define NUI_GL_BLEND 0x0BE2
#define NUI_GL_SCISSOR_TEST 0x0C11
#define NUI_GL_TEXTURE_2D 0x0DE1
#define NUI_GL_UNSIGNED_BYTE 0x1401
#define NUI_GL_UNSIGNED_INT 0x1405
#define NUI_GL_FLOAT 0x1406
#define NUI_GL_VERSION 0x1F02
#define NUI_GL_LINEAR 0x2601
#define NUI_GL_TEXTURE_MAG_FILTER 0x2800
#define NUI_GL_TEXTURE_MIN_FILTER 0x2801
#define NUI_GL_TEXTURE_WRAP_S 0x2802
#define NUI_GL_TEXTURE_WRAP_T 0x2803
#define NUI_GL_FUNC_ADD 0x8006
#define NUI_GL_RGBA 0x1908
#define NUI_GL_RGBA8 0x8058
#define NUI_GL_CLAMP_TO_EDGE 0x812F
#define NUI_GL_TEXTURE0 0x84C0
#define NUI_GL_ARRAY_BUFFER 0x8892
#define NUI_GL_ELEMENT_ARRAY_BUFFER 0x8893
#define NUI_GL_STREAM_DRAW 0x88E0
#define NUI_GL_UNIFORM_BUFFER 0x8A11
#define NUI_GL_INVALID_INDEX 0xFFFFFFFFu
#define NUI_GL_FRAGMENT_SHADER 0x8B30
#define NUI_GL_VERTEX_SHADER 0x8B31
#define NUI_GL_COMPILE_STATUS 0x8B81
#define NUI_GL_LINK_STATUS 0x8B82

#define NUI_GL_FUNCS \
    X(const uint8_t *, GetString, (NuiGLenum)) \
    X(void, Viewport, (NuiGLint, NuiGLint, NuiGLsizei, NuiGLsizei)) \
    X(void, Scissor, (NuiGLint, NuiGLint, NuiGLsizei, NuiGLsizei)) \
    X(void, Enable, (NuiGLenum)) \
    X(void, Disable, (NuiGLenum)) \
    X(void, BlendFuncSeparate, (NuiGLenum, NuiGLenum, NuiGLenum, NuiGLenum)) \
    X(void, BlendEquation, (NuiGLenum)) \
    X(NuiGLuint, CreateShader, (NuiGLenum)) \
    X(void, ShaderSource, (NuiGLuint, NuiGLsizei, const NuiGLchar *const *, const NuiGLint *)) \
    X(void, CompileShader, (NuiGLuint)) \
    X(void, GetShaderiv, (NuiGLuint, NuiGLenum, NuiGLint *)) \
    X(void, GetShaderInfoLog, (NuiGLuint, NuiGLsizei, NuiGLsizei *, NuiGLchar *)) \
    X(void, DeleteShader, (NuiGLuint)) \
    X(NuiGLuint, CreateProgram, (void)) \
    X(void, AttachShader, (NuiGLuint, NuiGLuint)) \
    X(void, BindAttribLocation, (NuiGLuint, NuiGLuint, const NuiGLchar *)) \
    X(void, LinkProgram, (NuiGLuint)) \
    X(void, GetProgramiv, (NuiGLuint, NuiGLenum, NuiGLint *)) \
    X(void, GetProgramInfoLog, (NuiGLuint, NuiGLsizei, NuiGLsizei *, NuiGLchar *)) \
    X(void, DeleteProgram, (NuiGLuint)) \
    X(void, UseProgram, (NuiGLuint)) \
    X(NuiGLint, GetUniformLocation, (NuiGLuint, const NuiGLchar *)) \
    X(NuiGLuint, GetUniformBlockIndex, (NuiGLuint, const NuiGLchar *)) \
    X(void, UniformBlockBinding, (NuiGLuint, NuiGLuint, NuiGLuint)) \
    X(void, BindBufferBase, (NuiGLenum, NuiGLuint, NuiGLuint)) \
    X(void, Uniform1i, (NuiGLint, NuiGLint)) \
    X(void, Uniform1f, (NuiGLint, NuiGLfloat)) \
    X(void, Uniform2f, (NuiGLint, NuiGLfloat, NuiGLfloat)) \
    X(void, GenVertexArrays, (NuiGLsizei, NuiGLuint *)) \
    X(void, BindVertexArray, (NuiGLuint)) \
    X(void, DeleteVertexArrays, (NuiGLsizei, const NuiGLuint *)) \
    X(void, GenBuffers, (NuiGLsizei, NuiGLuint *)) \
    X(void, BindBuffer, (NuiGLenum, NuiGLuint)) \
    X(void, BufferData, (NuiGLenum, NuiGLsizeiptr, const void *, NuiGLenum)) \
    X(void, DeleteBuffers, (NuiGLsizei, const NuiGLuint *)) \
    X(void, EnableVertexAttribArray, (NuiGLuint)) \
    X(void, VertexAttribPointer, (NuiGLuint, NuiGLint, NuiGLenum, NuiGLboolean, NuiGLsizei, const void *)) \
    X(void, DrawElements, (NuiGLenum, NuiGLsizei, NuiGLenum, const void *)) \
    X(void, DrawArrays, (NuiGLenum, NuiGLint, NuiGLsizei)) \
    X(void, GenTextures, (NuiGLsizei, NuiGLuint *)) \
    X(void, BindTexture, (NuiGLenum, NuiGLuint)) \
    X(void, ActiveTexture, (NuiGLenum)) \
    X(void, TexParameteri, (NuiGLenum, NuiGLenum, NuiGLint)) \
    X(void, TexImage2D, (NuiGLenum, NuiGLint, NuiGLint, NuiGLsizei, NuiGLsizei, NuiGLint, NuiGLenum, NuiGLenum, const void *)) \
    X(void, DeleteTextures, (NuiGLsizei, const NuiGLuint *))

typedef struct {
#define X(ret, name, args) ret (NUI_GL_API *name) args;
    NUI_GL_FUNCS
#undef X
} NanoUiGlApi;

typedef struct {
    uint64_t hash;
    NuiGLuint program;
    NuiGLint uViewport;
    NuiGLint uOrigin;
    NuiGLint uScale;
    NuiGLint uFlipY;
    NuiGLint uInput;
    int failed;
} NanoUiGlShaderEntry;

typedef struct {
    SDL_Renderer *renderer;
    const uint8_t *verts;
    int vert_count;
    const uint8_t *indices;
    SDL_Texture *pending_texture;
    int pending_start;
    int pending_n;
    /* The pending indices less the first vertex they use (see narrow). */
    int *rebased;
    int rebased_cap;
    /* Device pixels per layout unit for the pass (nano_ui_batch_begin). The
     * renderer draws at scale 1, so clip rects are whole device pixels, and
     * positions are multiplied here instead of by SDL's render scale. */
    float scale;
    /* Logical origin subtracted from vertex positions when drawing into a
     * Tier 2 offscreen effect target (0, 0 on the main target). */
    float origin_x;
    float origin_y;
    /* Scaled x, y pairs of the pass's vertices, by vertex index. Only the
     * vertices [xy_lo, xy_hi) of xy_verts are filled: a flush extends the
     * range to the vertices its indices use. */
    float *xy;
    int xy_cap;
    int xy_lo;
    int xy_hi;
    const uint8_t *xy_verts;
    /* Current renderer clip state, deduplicated across adjacent commands so
     * unchanged clips never flush the batch or call SDL_SetRenderClipRect. */
    int has_clip;
    SDL_Rect clip;
    /* Pooled offscreen SDL_Texture targets shared by GPU and OpenGL Tier 2
     * shader effects. */
    NanoUiTargetSlot *targets;
    int target_count;
    int target_cap;
    int saved_blend_valid;
    SDL_BlendMode saved_blend_mode;
    /* OpenGL 3.2 / GLSL 150 shader state (active only when SDL uses "opengl"). */
    int gl_checked;
    int gl_ok;
    SDL_Window *gl_win;
    SDL_GLContext gl_ctx;
    NanoUiGlApi gl;
    NuiGLuint gl_vao, gl_vbo, gl_ebo;
    NuiGLuint gl_quad_vao, gl_quad_vbo;
    NuiGLuint gl_ubo;
    NuiGLuint gl_white_tex;
    const uint8_t *gl_uploaded_verts;
    int gl_uploaded_vert_count;
    NanoUiGlShaderEntry *gl_shaders;
    int gl_shader_count;
    int gl_shader_cap;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    /* Pixel size of SDL 3.4's internal GPU backbuffer. SDL 3.4 does not
     * recreate its backbuffer on PIXEL_SIZE_CHANGED until after
     * GPU_RenderPresent blits the old backbuffer stretched onto the new
     * swapchain; during live resize, retained frames blit directly to the
     * swapchain at 1:1 pixels, then sync backbuffer once resize ends. */
    int gpu_bb_w;
    int gpu_bb_h;
    SDL_Texture *white_tex;
    NanoUiGpuShaderEntry *shaders;
    int shader_count;
    int shader_cap;
#endif
} NanoUiBatch;

#if SDL_VERSION_ATLEAST(3, 4, 0)
static const char *NANO_UI_PROP_GPU_BB_W = "nano_ui.gpu_bb_w";
static const char *NANO_UI_PROP_GPU_BB_H = "nano_ui.gpu_bb_h";
#endif

/* Create a window paired with SDL 3.4's "gpu" renderer supporting SPIR-V and
 * MSL custom fragment shaders. Returns 1 on success; on failure frees any
 * partial resources and returns 0 so RenderDriverAuto can fall back cleanly. */
int nano_ui_create_window_and_gpu_renderer(
    const char *title, int w, int h, uint64_t flags,
    SDL_Window **out_win, SDL_Renderer **out_ren)
{
    if (!out_win || !out_ren) {
        return 0;
    }
    *out_win = NULL;
    *out_ren = NULL;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_Window *win = SDL_CreateWindow(title, w, h, (SDL_WindowFlags)flags);
    if (!win) {
        return 0;
    }
    SDL_PropertiesID props = SDL_CreateProperties();
    if (!props) {
        SDL_DestroyWindow(win);
        return 0;
    }
    SDL_SetPointerProperty(props, SDL_PROP_RENDERER_CREATE_WINDOW_POINTER, win);
    SDL_SetStringProperty(props, SDL_PROP_RENDERER_CREATE_NAME_STRING, "gpu");
    SDL_SetBooleanProperty(props, SDL_PROP_RENDERER_CREATE_GPU_SHADERS_SPIRV_BOOLEAN, true);
    SDL_SetBooleanProperty(props, SDL_PROP_RENDERER_CREATE_GPU_SHADERS_MSL_BOOLEAN, true);
    SDL_Renderer *ren = SDL_CreateRendererWithProperties(props);
    SDL_DestroyProperties(props);
    if (!ren) {
        SDL_DestroyWindow(win);
        return 0;
    }
    /* Limit frames in flight to 1 for interactive UI latency so live resize
     * drags never queue stale-size frames behind the current border step. */
    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(ren);
    if (dev) {
        SDL_SetGPUAllowedFramesInFlight(dev, 1);
    }
    /* Record the pixel size SDL's GPU renderer created its backbuffer at,
     * before zoom or native-frame refits resize the window. */
    int bb_w = 0, bb_h = 0;
    if (SDL_GetRenderOutputSize(ren, &bb_w, &bb_h)) {
        SDL_PropertiesID rprops = SDL_GetRendererProperties(ren);
        if (rprops) {
            SDL_SetNumberProperty(rprops, NANO_UI_PROP_GPU_BB_W, (Sint64)bb_w);
            SDL_SetNumberProperty(rprops, NANO_UI_PROP_GPU_BB_H, (Sint64)bb_h);
        }
    }
    *out_win = win;
    *out_ren = ren;
    return 1;
#else
    (void)title; (void)w; (void)h; (void)flags;
    return 0;
#endif
}

/* Parse "major.minor..." from GL_VERSION and require OpenGL 3.2+ for GLSL 150. */
static int nui_gl_version_at_least_32(const uint8_t *ver)
{
    if (!ver) return 0;
    int major = 0, minor = 0;
    while (*ver >= '0' && *ver <= '9') {
        major = major * 10 + (*ver - '0');
        ver++;
    }
    if (*ver == '.') {
        ver++;
        while (*ver >= '0' && *ver <= '9') {
            minor = minor * 10 + (*ver - '0');
            ver++;
        }
    }
    return (major > 3) || (major == 3 && minor >= 2);
}

static int nui_gl_load(NanoUiGlApi *gl)
{
#define X(ret, name, args) \
    gl->name = (ret (NUI_GL_API *) args)SDL_GL_GetProcAddress("gl" #name); \
    if (!gl->name) return 0;
    NUI_GL_FUNCS
#undef X
    return nui_gl_version_at_least_32(gl->GetString(NUI_GL_VERSION));
}

/* Report whether renderer is backed by an SDL 3.4 GPU device. */
int nano_ui_renderer_has_gpu(SDL_Renderer *renderer)
{
#if SDL_VERSION_ATLEAST(3, 4, 0)
    return (renderer && SDL_GetGPURendererDevice(renderer) != NULL) ? 1 : 0;
#else
    (void)renderer;
    return 0;
#endif
}

/* Report whether renderer can execute nano-ui-shader programs: either SDL 3.4's
 * "gpu" driver (SPIR-V/MSL) or SDL's "opengl" driver on an OpenGL 3.2+ context
 * (GLSL 150). */
int nano_ui_renderer_has_shaders(SDL_Renderer *renderer)
{
    if (!renderer) {
        return 0;
    }
#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(renderer);
    if (dev != NULL) {
        SDL_GPUShaderFormat fmts = SDL_GetGPUShaderFormats(dev);
        return (fmts & (SDL_GPU_SHADERFORMAT_SPIRV | SDL_GPU_SHADERFORMAT_MSL)) ? 1 : 0;
    }
#endif
    const char *name = SDL_GetRendererName(renderer);
    if (name && strcmp(name, "opengl") == 0) {
        NanoUiGlApi tmp;
        return nui_gl_load(&tmp);
    }
    return 0;
}

static const char *nui_gl_vertex_src =
    "#version 150\n"
    "in vec2 aPos;\n"
    "in vec4 aColor;\n"
    "in vec2 aUV;\n"
    "uniform vec2 uViewport;\n"
    "uniform vec2 uOrigin;\n"
    "uniform float uScale;\n"
    "uniform float uFlipY;\n"
    "out vec4 vColor;\n"
    "out vec2 vUV;\n"
    "void main() {\n"
    "  vec2 p = (aPos - uOrigin) * uScale;\n"
    "  gl_Position = vec4(p.x / uViewport.x * 2.0 - 1.0, uFlipY * (p.y / uViewport.y * 2.0 - 1.0), 0.0, 1.0);\n"
    "  vColor = aColor;\n"
    "  vUV = aUV;\n"
    "}\n";

static int nui_gl_make_current(NanoUiBatch *batch)
{
    if (batch->gl_ctx && batch->gl_win && SDL_GL_GetCurrentContext() != batch->gl_ctx) {
        return SDL_GL_MakeCurrent(batch->gl_win, batch->gl_ctx) ? 1 : 0;
    }
    return 1;
}

static int nui_gl_ensure_init(NanoUiBatch *batch)
{
    if (batch->gl_checked) {
        return batch->gl_ok;
    }
    batch->gl_checked = 1;
    batch->gl_ok = 0;
    if (!batch->renderer) {
        return 0;
    }
    const char *name = SDL_GetRendererName(batch->renderer);
    if (!name || strcmp(name, "opengl") != 0) {
        return 0;
    }
    SDL_FlushRenderer(batch->renderer);
    batch->gl_win = SDL_GetRenderWindow(batch->renderer);
    batch->gl_ctx = SDL_GL_GetCurrentContext();
    if (!batch->gl_ctx || !nui_gl_load(&batch->gl)) {
        return 0;
    }
    NanoUiGlApi *gl = &batch->gl;
    gl->GenVertexArrays(1, &batch->gl_vao);
    gl->GenBuffers(1, &batch->gl_vbo);
    gl->GenBuffers(1, &batch->gl_ebo);
    gl->BindVertexArray(batch->gl_vao);
    gl->BindBuffer(NUI_GL_ARRAY_BUFFER, batch->gl_vbo);
    gl->BindBuffer(NUI_GL_ELEMENT_ARRAY_BUFFER, batch->gl_ebo);
    gl->EnableVertexAttribArray(0);
    gl->VertexAttribPointer(0, 2, NUI_GL_FLOAT, NUI_GL_FALSE, (NuiGLsizei)sizeof(SDL_Vertex),
                            (const void *)offsetof(SDL_Vertex, position));
    gl->EnableVertexAttribArray(1);
    gl->VertexAttribPointer(1, 4, NUI_GL_FLOAT, NUI_GL_FALSE, (NuiGLsizei)sizeof(SDL_Vertex),
                            (const void *)offsetof(SDL_Vertex, color));
    gl->EnableVertexAttribArray(2);
    gl->VertexAttribPointer(2, 2, NUI_GL_FLOAT, NUI_GL_FALSE, (NuiGLsizei)sizeof(SDL_Vertex),
                            (const void *)offsetof(SDL_Vertex, tex_coord));

    gl->GenVertexArrays(1, &batch->gl_quad_vao);
    gl->GenBuffers(1, &batch->gl_quad_vbo);
    gl->BindVertexArray(batch->gl_quad_vao);
    gl->BindBuffer(NUI_GL_ARRAY_BUFFER, batch->gl_quad_vbo);
    gl->EnableVertexAttribArray(0);
    gl->VertexAttribPointer(0, 2, NUI_GL_FLOAT, NUI_GL_FALSE, (NuiGLsizei)sizeof(SDL_Vertex),
                            (const void *)offsetof(SDL_Vertex, position));
    gl->EnableVertexAttribArray(1);
    gl->VertexAttribPointer(1, 4, NUI_GL_FLOAT, NUI_GL_FALSE, (NuiGLsizei)sizeof(SDL_Vertex),
                            (const void *)offsetof(SDL_Vertex, color));
    gl->EnableVertexAttribArray(2);
    gl->VertexAttribPointer(2, 2, NUI_GL_FLOAT, NUI_GL_FALSE, (NuiGLsizei)sizeof(SDL_Vertex),
                            (const void *)offsetof(SDL_Vertex, tex_coord));

    gl->BindVertexArray(0);
    gl->BindBuffer(NUI_GL_ARRAY_BUFFER, 0);
    gl->BindBuffer(NUI_GL_ELEMENT_ARRAY_BUFFER, 0);

    gl->GenBuffers(1, &batch->gl_ubo);

    gl->GenTextures(1, &batch->gl_white_tex);
    gl->ActiveTexture(NUI_GL_TEXTURE0);
    gl->BindTexture(NUI_GL_TEXTURE_2D, batch->gl_white_tex);
    gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_MIN_FILTER, NUI_GL_LINEAR);
    gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_MAG_FILTER, NUI_GL_LINEAR);
    gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_WRAP_S, NUI_GL_CLAMP_TO_EDGE);
    gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_WRAP_T, NUI_GL_CLAMP_TO_EDGE);
    const uint32_t white = 0xFFFFFFFFu;
    gl->TexImage2D(NUI_GL_TEXTURE_2D, 0, NUI_GL_RGBA8, 1, 1, 0, NUI_GL_RGBA, NUI_GL_UNSIGNED_BYTE, &white);
    gl->BindTexture(NUI_GL_TEXTURE_2D, 0);

    SDL_FlushRenderer(batch->renderer);
    batch->gl_ok = 1;
    return 1;
}

static NuiGLuint nui_gl_compile(NanoUiGlApi *gl, NuiGLenum type, const char *src)
{
    NuiGLuint sh = gl->CreateShader(type);
    NuiGLint ok = 0;
    gl->ShaderSource(sh, 1, &src, NULL);
    gl->CompileShader(sh);
    gl->GetShaderiv(sh, NUI_GL_COMPILE_STATUS, &ok);
    if (!ok) {
        char log[1024];
        gl->GetShaderInfoLog(sh, (NuiGLsizei)sizeof(log), NULL, log);
        fprintf(stderr, "nano-ui-sdl: OpenGL shader compile failed: %s\n", log);
        gl->DeleteShader(sh);
        return 0;
    }
    return sh;
}

static NanoUiGlShaderEntry *nui_gl_get_shader(
    NanoUiBatch *batch, uint64_t hash, const uint8_t *glsl, int glsl_len)
{
    for (int i = 0; i < batch->gl_shader_count; i++) {
        if (batch->gl_shaders[i].hash == hash) {
            return &batch->gl_shaders[i];
        }
    }
    if (batch->gl_shader_count == batch->gl_shader_cap) {
        int new_cap = batch->gl_shader_cap > 0 ? batch->gl_shader_cap * 2 : 16;
        NanoUiGlShaderEntry *grown = (NanoUiGlShaderEntry *)realloc(
            batch->gl_shaders, (size_t)new_cap * sizeof(NanoUiGlShaderEntry));
        if (!grown) {
            return NULL;
        }
        batch->gl_shaders = grown;
        batch->gl_shader_cap = new_cap;
    }
    NanoUiGlShaderEntry *entry = &batch->gl_shaders[batch->gl_shader_count++];
    memset(entry, 0, sizeof(*entry));
    entry->hash = hash;
    entry->uViewport = -1;
    entry->uOrigin = -1;
    entry->uScale = -1;
    entry->uFlipY = -1;
    entry->uInput = -1;
    if (!glsl || glsl_len <= 0) {
        entry->failed = 1;
        return entry;
    }
    char *src = (char *)malloc((size_t)glsl_len + 1);
    if (!src) {
        entry->failed = 1;
        return entry;
    }
    memcpy(src, glsl, (size_t)glsl_len);
    src[glsl_len] = '\0';

    NanoUiGlApi *gl = &batch->gl;
    NuiGLuint vs = nui_gl_compile(gl, NUI_GL_VERTEX_SHADER, nui_gl_vertex_src);
    NuiGLuint fs = vs ? nui_gl_compile(gl, NUI_GL_FRAGMENT_SHADER, src) : 0;
    free(src);
    if (!fs) {
        if (vs) gl->DeleteShader(vs);
        entry->failed = 1;
        return entry;
    }
    NuiGLuint prog = gl->CreateProgram();
    gl->AttachShader(prog, vs);
    gl->AttachShader(prog, fs);
    gl->BindAttribLocation(prog, 0, "aPos");
    gl->BindAttribLocation(prog, 1, "aColor");
    gl->BindAttribLocation(prog, 2, "aUV");
    gl->LinkProgram(prog);
    gl->DeleteShader(vs);
    gl->DeleteShader(fs);
    NuiGLint ok = 0;
    gl->GetProgramiv(prog, NUI_GL_LINK_STATUS, &ok);
    if (!ok) {
        char log[1024];
        gl->GetProgramInfoLog(prog, (NuiGLsizei)sizeof(log), NULL, log);
        fprintf(stderr, "nano-ui-sdl: OpenGL shader link failed: %s\n", log);
        gl->DeleteProgram(prog);
        entry->failed = 1;
        return entry;
    }
    NuiGLuint blk = gl->GetUniformBlockIndex(prog, "NanoShaderBlock");
    if (blk != NUI_GL_INVALID_INDEX) {
        gl->UniformBlockBinding(prog, blk, 0);
    }
    entry->program = prog;
    entry->uViewport = gl->GetUniformLocation(prog, "uViewport");
    entry->uOrigin = gl->GetUniformLocation(prog, "uOrigin");
    entry->uScale = gl->GetUniformLocation(prog, "uScale");
    entry->uFlipY = gl->GetUniformLocation(prog, "uFlipY");
    entry->uInput = gl->GetUniformLocation(prog, "uInput");
    return entry;
}

static void nui_gl_prepare_state(
    NanoUiBatch *batch, NanoUiGlShaderEntry *entry,
    int cur_w, int cur_h, int is_target,
    float ox, float oy, float scale,
    NuiGLuint gl_tex,
    const uint8_t *uniform_bytes, int uniform_len)
{
    NanoUiGlApi *gl = &batch->gl;
    gl->Viewport(0, 0, cur_w, cur_h);
    gl->Disable(NUI_GL_DEPTH_TEST);
    gl->Disable(NUI_GL_CULL_FACE);
    if (batch->has_clip) {
        int sy = is_target ? batch->clip.y : (cur_h - batch->clip.y - batch->clip.h);
        gl->Enable(NUI_GL_SCISSOR_TEST);
        gl->Scissor(batch->clip.x, sy, batch->clip.w, batch->clip.h);
    } else {
        gl->Disable(NUI_GL_SCISSOR_TEST);
    }
    gl->Enable(NUI_GL_BLEND);
    gl->BlendFuncSeparate(NUI_GL_SRC_ALPHA, NUI_GL_ONE_MINUS_SRC_ALPHA, NUI_GL_ONE, NUI_GL_ONE_MINUS_SRC_ALPHA);
    gl->BlendEquation(NUI_GL_FUNC_ADD);

    gl->UseProgram(entry->program);
    if (entry->uViewport >= 0) gl->Uniform2f(entry->uViewport, (float)cur_w, (float)cur_h);
    if (entry->uOrigin >= 0) gl->Uniform2f(entry->uOrigin, ox, oy);
    if (entry->uScale >= 0) gl->Uniform1f(entry->uScale, scale);
    if (entry->uFlipY >= 0) gl->Uniform1f(entry->uFlipY, is_target ? 1.0f : -1.0f);
    if (entry->uInput >= 0) gl->Uniform1i(entry->uInput, 0);

    gl->ActiveTexture(NUI_GL_TEXTURE0);
    gl->BindTexture(NUI_GL_TEXTURE_2D, gl_tex ? gl_tex : batch->gl_white_tex);

    if (uniform_len > 0 && uniform_bytes != NULL) {
        gl->BindBuffer(NUI_GL_UNIFORM_BUFFER, batch->gl_ubo);
        gl->BufferData(NUI_GL_UNIFORM_BUFFER, (NuiGLsizeiptr)uniform_len, uniform_bytes, NUI_GL_STREAM_DRAW);
        gl->BindBufferBase(NUI_GL_UNIFORM_BUFFER, 0, batch->gl_ubo);
    }
}

static void nui_gl_restore_state(NanoUiBatch *batch)
{
    NanoUiGlApi *gl = &batch->gl;
    gl->UseProgram(0);
    gl->BindVertexArray(0);
    gl->BindBuffer(NUI_GL_ARRAY_BUFFER, 0);
    gl->BindBuffer(NUI_GL_ELEMENT_ARRAY_BUFFER, 0);
    gl->BindBuffer(NUI_GL_UNIFORM_BUFFER, 0);
    gl->ActiveTexture(NUI_GL_TEXTURE0);
    gl->BindTexture(NUI_GL_TEXTURE_2D, 0);
    gl->Disable(NUI_GL_SCISSOR_TEST);
    SDL_FlushRenderer(batch->renderer);
}

static void nui_gl_current_target_size(SDL_Renderer *ren, int *out_w, int *out_h, int *out_is_target)
{
    SDL_Texture *cur = SDL_GetRenderTarget(ren);
    if (cur) {
        float tw = 0.f, th = 0.f;
        SDL_GetTextureSize(cur, &tw, &th);
        *out_w = (int)tw;
        *out_h = (int)th;
        *out_is_target = 1;
    } else {
        int ow = 0, oh = 0;
        SDL_GetRenderOutputSize(ren, &ow, &oh);
        *out_w = ow;
        *out_h = oh;
        *out_is_target = 0;
    }
}

static NuiGLuint nui_gl_texture_id(SDL_Texture *tex)
{
    if (!tex) return 0;
    SDL_PropertiesID props = SDL_GetTextureProperties(tex);
    if (!props) return 0;
    return (NuiGLuint)SDL_GetNumberProperty(props, SDL_PROP_TEXTURE_OPENGL_TEXTURE_NUMBER, 0);
}

NanoUiBatch *nano_ui_batch_create(SDL_Renderer *renderer)
{
    if (!renderer) {
        return NULL;
    }
    NanoUiBatch *batch = (NanoUiBatch *)calloc(1, sizeof(NanoUiBatch));
    if (!batch) {
        return NULL;
    }
    batch->renderer = renderer;
    batch->scale = 1.f;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    /* Every UV the batches submit lies in [0, 1]. Left on AUTO, SDL scans all
     * of a call's vertices on every textured call to choose between clamp
     * and wrap, which comes to clamp anyway. SDL 3.2 has no setting for it,
     * so there a call passes only the vertices it uses (see narrow). */
    SDL_SetRenderTextureAddressMode(renderer, SDL_TEXTURE_ADDRESS_CLAMP, SDL_TEXTURE_ADDRESS_CLAMP);
    if (SDL_GetGPURendererDevice(renderer) != NULL) {
        SDL_PropertiesID rprops = SDL_GetRendererProperties(renderer);
        Sint64 bw = rprops ? SDL_GetNumberProperty(rprops, NANO_UI_PROP_GPU_BB_W, 0) : 0;
        Sint64 bh = rprops ? SDL_GetNumberProperty(rprops, NANO_UI_PROP_GPU_BB_H, 0) : 0;
        if (bw > 0 && bh > 0) {
            batch->gpu_bb_w = (int)bw;
            batch->gpu_bb_h = (int)bh;
        } else {
            SDL_GetRenderOutputSize(renderer, &batch->gpu_bb_w, &batch->gpu_bb_h);
        }
    }
#endif
    return batch;
}

void nano_ui_batch_flush(NanoUiBatch *batch);

/* On SDL 3.4.x's "gpu" renderer, GPU_RenderPresent blits its internal
 * backbuffer (still at the pre-resize size) linearly stretched onto the new
 * swapchain before recreating the backbuffer. While a live resize is active
 * and the frame size differs from that backbuffer, submit the retained-target
 * draws and blit directly to the acquired swapchain at 1:1 pixels. Once live
 * resize ends, return 0 and record the new size so one standard present
 * recreates SDL 3.4's backbuffer and steady-state frames pay zero overhead. */
int nano_ui_batch_present_gpu_retain(
    NanoUiBatch *batch,
    SDL_Window *window,
    SDL_Texture *retain,
    int rw, int rh,
    int in_resize,
    uint8_t cr, uint8_t cg, uint8_t cb, uint8_t ca)
{
#if SDL_VERSION_ATLEAST(3, 4, 0)
    if (!batch || !batch->renderer || !window || !retain || rw <= 0 || rh <= 0) {
        return 0;
    }
    if (SDL_GetVersion() >= SDL_VERSIONNUM(3, 5, 0)) {
        return 0;
    }
    if (rw == batch->gpu_bb_w && rh == batch->gpu_bb_h) {
        return 0;
    }
    if (!in_resize) {
        batch->gpu_bb_w = rw;
        batch->gpu_bb_h = rh;
        return 0;
    }
    SDL_Renderer *ren = batch->renderer;
    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(ren);
    if (!dev) {
        return 0;
    }
    SDL_GPUTexture *src_tex = (SDL_GPUTexture *)SDL_GetPointerProperty(
        SDL_GetTextureProperties(retain), SDL_PROP_TEXTURE_GPU_TEXTURE_POINTER, NULL);
    if (!src_tex) {
        return 0;
    }
    nano_ui_batch_flush(batch);
    /* Submit SDL_render_gpu's internal command buffer containing the retained
     * texture's draw passes without touching the window swapchain. */
    SDL_SetRenderTarget(ren, retain);
    SDL_Rect one = { 0, 0, 1, 1 };
    SDL_Surface *sync_surf = SDL_RenderReadPixels(ren, &one);
    if (sync_surf) {
        SDL_DestroySurface(sync_surf);
    }
    SDL_GPUCommandBuffer *cmd = SDL_AcquireGPUCommandBuffer(dev);
    if (!cmd) {
        SDL_SetRenderTarget(ren, NULL);
        return 0;
    }
    SDL_GPUTexture *swapchain = NULL;
    Uint32 sw = 0, sh = 0;
    if (!SDL_WaitAndAcquireGPUSwapchainTexture(cmd, window, &swapchain, &sw, &sh)) {
        SDL_CancelGPUCommandBuffer(cmd);
        SDL_SetRenderTarget(ren, NULL);
        return 0;
    }
    if (swapchain != NULL && sw > 0 && sh > 0) {
        Uint32 cw = (Uint32)rw < sw ? (Uint32)rw : sw;
        Uint32 ch = (Uint32)rh < sh ? (Uint32)rh : sh;
        SDL_GPUBlitInfo blit;
        SDL_zero(blit);
        blit.source.texture = src_tex;
        blit.source.w = cw;
        blit.source.h = ch;
        blit.destination.texture = swapchain;
        blit.destination.w = cw;
        blit.destination.h = ch;
        blit.load_op = (cw < sw || ch < sh) ? SDL_GPU_LOADOP_CLEAR : SDL_GPU_LOADOP_DONT_CARE;
        blit.clear_color.r = (float)cr / 255.0f;
        blit.clear_color.g = (float)cg / 255.0f;
        blit.clear_color.b = (float)cb / 255.0f;
        blit.clear_color.a = (float)ca / 255.0f;
        blit.filter = SDL_GPU_FILTER_NEAREST;
        SDL_BlitGPUTexture(cmd, &blit);
    }
    SDL_SubmitGPUCommandBuffer(cmd);
    SDL_SetRenderTarget(ren, NULL);
    SDL_SetRenderClipRect(ren, NULL);
    SDL_SetRenderScale(ren, 1.0f, 1.0f);
    batch->has_clip = 0;
    return 1;
#else
    (void)batch; (void)window; (void)retain; (void)rw; (void)rh; (void)in_resize;
    (void)cr; (void)cg; (void)cb; (void)ca;
    return 0;
#endif
}

#if !SDL_VERSION_ATLEAST(3, 4, 0)
/* Narrow a batch's vertices to the range its indices use, rebasing the
 * indices to its start. SDL 3.2 scans every vertex a textured call is given,
 * and the batches all share the frame's buffer, so passing all of it made a
 * frame cost draw calls times vertices. Indices out of the buffer are left
 * for SDL to reject, and a batch whose indices cannot be copied passes the
 * vertices up to the last it uses. */
static void narrow(NanoUiBatch *batch, const SDL_Vertex **v, const int **idx, int *nv)
{
    const int *ix = *idx;
    const int n = batch->pending_n;
    int lo = ix[0], hi = ix[0];
    for (int k = 1; k < n; k++) {
        lo = ix[k] < lo ? ix[k] : lo;
        hi = ix[k] > hi ? ix[k] : hi;
    }
    if (lo < 0 || hi >= *nv) {
        return;
    }
    *nv = hi + 1;
    if (lo == 0) {
        return;
    }
    if (batch->rebased_cap < n) {
        int *grown = (int *)realloc(batch->rebased, sizeof(int) * (size_t)n * 2);
        if (!grown) {
            return;
        }
        batch->rebased = grown;
        batch->rebased_cap = n * 2;
    }
    for (int k = 0; k < n; k++) {
        batch->rebased[k] = ix[k] - lo;
    }
    *idx = batch->rebased;
    *v += lo;
    *nv = hi - lo + 1;
}
#endif

/* Start a pass at a scale: forget the positions scaled for an earlier pass,
 * whose buffer may hold other vertices now, and clear the renderer clip. */
void nano_ui_batch_begin(NanoUiBatch *batch, float scale)
{
    if (batch) {
        batch->scale = scale > 0.f ? scale : 1.f;
        batch->origin_x = 0.f;
        batch->origin_y = 0.f;
        batch->xy_verts = NULL;
        batch->xy_lo = batch->xy_hi = 0;
        batch->gl_uploaded_verts = NULL;
        batch->gl_uploaded_vert_count = 0;
        batch->has_clip = 0;
        if (batch->renderer) {
            SDL_SetRenderClipRect(batch->renderer, NULL);
        }
    }
}

/* Update the renderer clip only when the device-pixel rect changed, flushing
 * queued geometry before the clip switches. */
void nano_ui_batch_set_clip(NanoUiBatch *batch, int has_clip, int x, int y, int w, int h)
{
    if (!batch || !batch->renderer) {
        return;
    }
    if (!has_clip) {
        if (!batch->has_clip) {
            return;
        }
        nano_ui_batch_flush(batch);
        batch->has_clip = 0;
        SDL_SetRenderClipRect(batch->renderer, NULL);
        return;
    }
    if (batch->has_clip &&
        batch->clip.x == x && batch->clip.y == y &&
        batch->clip.w == w && batch->clip.h == h) {
        return;
    }
    nano_ui_batch_flush(batch);
    batch->has_clip = 1;
    batch->clip.x = x;
    batch->clip.y = y;
    batch->clip.w = w;
    batch->clip.h = h;
    SDL_SetRenderClipRect(batch->renderer, &batch->clip);
}

/* Set the logical origin subtracted from vertex coordinates when redirecting
 * drawing into an offscreen Tier 2 effect target. */
void nano_ui_batch_set_origin(NanoUiBatch *batch, float ox, float oy)
{
    if (batch) {
        nano_ui_batch_flush(batch);
        batch->origin_x = ox;
        batch->origin_y = oy;
        batch->xy_verts = NULL;
        batch->xy_lo = batch->xy_hi = 0;
    }
}

static void scale_range(NanoUiBatch *batch, const SDL_Vertex *v, int from, int to)
{
    const float s = batch->scale;
    const float ox = batch->origin_x;
    const float oy = batch->origin_y;
    for (int i = from; i < to; i++) {
        batch->xy[2 * i] = (v[i].position.x - ox) * s;
        batch->xy[2 * i + 1] = (v[i].position.y - oy) * s;
    }
}

/* Scaled positions covering the vertices the n indices idx use, which count
 * from vertex off of the batch's buffer, as a pointer for vertex off; NULL
 * when there is no memory for them. Indices outside the buffer are left for
 * SDL to reject. */
static const float *scaled_positions(NanoUiBatch *batch, int off, const int *idx, int n)
{
    const int total = batch->vert_count;
    if (batch->xy_cap < total) {
        float *grown = (float *)realloc(batch->xy, sizeof(float) * 2 * (size_t)total);
        if (!grown) {
            return NULL;
        }
        batch->xy = grown;
        batch->xy_cap = total;
    }
    int lo = idx[0], hi = idx[0];
    for (int k = 1; k < n; k++) {
        lo = idx[k] < lo ? idx[k] : lo;
        hi = idx[k] > hi ? idx[k] : hi;
    }
    lo += off;
    hi += off + 1;
    lo = lo < 0 ? 0 : lo;
    hi = hi > total ? total : hi;
    const SDL_Vertex *all = (const SDL_Vertex *)batch->verts;
    if (batch->xy_verts != batch->verts || batch->xy_lo >= batch->xy_hi) {
        scale_range(batch, all, lo, hi);
        batch->xy_verts = batch->verts;
        batch->xy_lo = lo;
        batch->xy_hi = hi;
    } else {
        if (lo < batch->xy_lo) {
            scale_range(batch, all, lo, batch->xy_lo);
            batch->xy_lo = lo;
        }
        if (hi > batch->xy_hi) {
            scale_range(batch, all, batch->xy_hi, hi);
            batch->xy_hi = hi;
        }
    }
    return batch->xy + 2 * (size_t)off;
}

/* Drop the pending batch without drawing it: a pass that failed part way. */
void nano_ui_batch_discard(NanoUiBatch *batch)
{
    if (batch) {
        batch->pending_n = 0;
        batch->pending_start = 0;
        batch->pending_texture = NULL;
    }
}

void nano_ui_batch_flush(NanoUiBatch *batch)
{
    if (!batch || !batch->renderer || batch->pending_n < 3) {
        if (batch) {
            batch->pending_n = 0;
            batch->pending_start = 0;
            batch->pending_texture = NULL;
        }
        return;
    }
    /* The interleaved SDL_Vertex buffer read in place through strides; an
     * untextured batch passes no UVs. */
    const SDL_Vertex *v = (const SDL_Vertex *)batch->verts;
    const int *idx = (const int *)batch->indices + batch->pending_start;
    int nv = batch->vert_count;
#if !SDL_VERSION_ATLEAST(3, 4, 0)
    narrow(batch, &v, &idx, &nv);
#endif
    const int stride = (int)sizeof(SDL_Vertex);
    const float *xy = &v->position.x;
    int xy_stride = stride;
    if (batch->scale != 1.f || batch->origin_x != 0.f || batch->origin_y != 0.f) {
        xy = scaled_positions(batch, (int)(v - (const SDL_Vertex *)batch->verts), idx, batch->pending_n);
        xy_stride = (int)(2 * sizeof(float));
        if (!xy) {
            nano_ui_batch_discard(batch);
            return;
        }
    }
    SDL_RenderGeometryRaw(batch->renderer, batch->pending_texture,
                          xy, xy_stride, &v->color, stride,
                          batch->pending_texture ? &v->tex_coord.x : NULL, stride,
                          nv, idx, batch->pending_n, (int)sizeof(int));
    batch->pending_n = 0;
    batch->pending_start = 0;
    batch->pending_texture = NULL;
}

void nano_ui_batch_destroy(NanoUiBatch *batch)
{
    if (batch) {
        nano_ui_batch_flush(batch);
        if (batch->targets) {
            for (int i = 0; i < batch->target_count; i++) {
                if (batch->targets[i].tex) {
                    SDL_DestroyTexture(batch->targets[i].tex);
                }
            }
            free(batch->targets);
        }
        if (batch->gl_ok && nui_gl_make_current(batch)) {
            NanoUiGlApi *gl = &batch->gl;
            if (batch->gl_shaders) {
                for (int i = 0; i < batch->gl_shader_count; i++) {
                    if (batch->gl_shaders[i].program) {
                        gl->DeleteProgram(batch->gl_shaders[i].program);
                    }
                }
                free(batch->gl_shaders);
            }
            if (batch->gl_white_tex) gl->DeleteTextures(1, &batch->gl_white_tex);
            if (batch->gl_ubo) gl->DeleteBuffers(1, &batch->gl_ubo);
            if (batch->gl_quad_vbo) gl->DeleteBuffers(1, &batch->gl_quad_vbo);
            if (batch->gl_quad_vao) gl->DeleteVertexArrays(1, &batch->gl_quad_vao);
            if (batch->gl_ebo) gl->DeleteBuffers(1, &batch->gl_ebo);
            if (batch->gl_vbo) gl->DeleteBuffers(1, &batch->gl_vbo);
            if (batch->gl_vao) gl->DeleteVertexArrays(1, &batch->gl_vao);
        } else {
            free(batch->gl_shaders);
        }
#if SDL_VERSION_ATLEAST(3, 4, 0)
        if (batch->shaders) {
            SDL_GPUDevice *dev = batch->renderer ? SDL_GetGPURendererDevice(batch->renderer) : NULL;
            for (int i = 0; i < batch->shader_count; i++) {
                if (batch->shaders[i].state) {
                    SDL_DestroyGPURenderState(batch->shaders[i].state);
                }
                if (batch->shaders[i].shader && dev) {
                    SDL_ReleaseGPUShader(dev, batch->shaders[i].shader);
                }
            }
            free(batch->shaders);
        }
        if (batch->white_tex) {
            SDL_DestroyTexture(batch->white_tex);
        }
#endif
        free(batch->rebased);
        free(batch->xy);
        free(batch);
    }
}

/* Commands may contain independent triangles, not just axis-aligned quads.
 * Reject a range only when every triangle is outside the damage bounds. SDL
 * clips intersecting triangles; this just avoids submitting invisible ranges. */
static bool visible(const SDL_Vertex *v, int vertex_count, const int *indices, int count,
                    float x, float y, float w, float h)
{
    const float rx = x + w;
    const float ry = y + h;
    for (int i = 0; i + 2 < count; i += 3) {
        if ((unsigned)indices[i] >= (unsigned)vertex_count ||
            (unsigned)indices[i + 1] >= (unsigned)vertex_count ||
            (unsigned)indices[i + 2] >= (unsigned)vertex_count) return true;
        const SDL_FPoint a = v[indices[i]].position;
        const SDL_FPoint b = v[indices[i + 1]].position;
        const SDL_FPoint c = v[indices[i + 2]].position;
        if (!((a.x < x && b.x < x && c.x < x) ||
              (a.x >= rx && b.x >= rx && c.x >= rx) ||
              (a.y < y && b.y < y && c.y < y) ||
              (a.y >= ry && b.y >= ry && c.y >= ry))) {
            return true;
        }
    }
    return false;
}

void nano_ui_batch_draw_range(
    NanoUiBatch *batch, const uint8_t *verts, int vert_count,
    const uint8_t *indices, int index_start, int index_n,
    SDL_Texture *texture, int has_damage,
    float dmg_x, float dmg_y, float dmg_w, float dmg_h)
{
    if (!batch || !verts || !indices || vert_count <= 0 || index_n < 3) {
        return;
    }
    if (index_start < 0) {
        index_start = 0;
    }
    if (has_damage && dmg_w > 0.f && dmg_h > 0.f &&
        !visible((const SDL_Vertex *)verts, vert_count, (const int *)indices + index_start,
                 index_n, dmg_x, dmg_y, dmg_w, dmg_h)) {
        return;
    }

    if (batch->pending_n > 0 &&
        batch->verts == verts &&
        batch->indices == indices &&
        batch->pending_texture == texture &&
        batch->pending_start + batch->pending_n == index_start)
    {
        batch->pending_n += index_n;
        if (vert_count > batch->vert_count) {
            batch->vert_count = vert_count;
        }
        return;
    }

    nano_ui_batch_flush(batch);

    batch->verts = verts;
    batch->indices = indices;
    batch->pending_texture = texture;
    batch->pending_start = index_start;
    batch->pending_n = index_n;
    batch->vert_count = vert_count;
}

#if SDL_VERSION_ATLEAST(3, 4, 0)
static SDL_Texture *ensure_white_tex(NanoUiBatch *batch)
{
    if (batch->white_tex) {
        return batch->white_tex;
    }
    SDL_Texture *tex = SDL_CreateTexture(
        batch->renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STATIC, 1, 1);
    if (!tex) {
        return NULL;
    }
    const uint32_t white = 0xFFFFFFFFu;
    SDL_UpdateTexture(tex, NULL, &white, 4);
    SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
    batch->white_tex = tex;
    return tex;
}

static NanoUiGpuShaderEntry *get_gpu_shader(
    NanoUiBatch *batch, SDL_GPUDevice *dev, uint64_t hash,
    const uint8_t *spirv, int spirv_len,
    const uint8_t *msl, int msl_len,
    int needs_tex)
{
    for (int i = 0; i < batch->shader_count; i++) {
        if (batch->shaders[i].hash == hash) {
            return &batch->shaders[i];
        }
    }
    if (batch->shader_count == batch->shader_cap) {
        int new_cap = batch->shader_cap > 0 ? batch->shader_cap * 2 : 16;
        NanoUiGpuShaderEntry *grown = (NanoUiGpuShaderEntry *)realloc(
            batch->shaders, (size_t)new_cap * sizeof(NanoUiGpuShaderEntry));
        if (!grown) {
            return NULL;
        }
        batch->shaders = grown;
        batch->shader_cap = new_cap;
    }
    NanoUiGpuShaderEntry *entry = &batch->shaders[batch->shader_count++];
    memset(entry, 0, sizeof(*entry));
    entry->hash = hash;

    SDL_GPUShaderFormat fmts = SDL_GetGPUShaderFormats(dev);
    SDL_GPUShaderCreateInfo sci;
    memset(&sci, 0, sizeof(sci));
    sci.stage = SDL_GPU_SHADERSTAGE_FRAGMENT;
    sci.num_samplers = needs_tex ? 1u : 0u;
    sci.num_uniform_buffers = 1u;

    if ((fmts & SDL_GPU_SHADERFORMAT_SPIRV) && spirv && spirv_len > 0) {
        sci.format = SDL_GPU_SHADERFORMAT_SPIRV;
        sci.code = spirv;
        sci.code_size = (size_t)spirv_len;
        sci.entrypoint = "main";
    } else if ((fmts & SDL_GPU_SHADERFORMAT_MSL) && msl && msl_len > 0) {
        sci.format = SDL_GPU_SHADERFORMAT_MSL;
        sci.code = msl;
        sci.code_size = (size_t)msl_len;
        sci.entrypoint = "main0";
    } else {
        entry->failed = 1;
        return entry;
    }

    SDL_GPUShader *shader = SDL_CreateGPUShader(dev, &sci);
    if (!shader) {
        fprintf(stderr, "nano-ui-sdl: SDL_CreateGPUShader failed: %s\n", SDL_GetError());
        entry->failed = 1;
        return entry;
    }

    SDL_GPURenderStateCreateInfo rsci;
    memset(&rsci, 0, sizeof(rsci));
    rsci.fragment_shader = shader;
    SDL_GPURenderState *state = SDL_CreateGPURenderState(batch->renderer, &rsci);
    if (!state) {
        fprintf(stderr, "nano-ui-sdl: SDL_CreateGPURenderState failed: %s\n", SDL_GetError());
        SDL_ReleaseGPUShader(dev, shader);
        entry->failed = 1;
        return entry;
    }
    entry->shader = shader;
    entry->state = state;
    return entry;
}
/* Upload fragment uniform bytes (if any) to entry->state and activate it on
 * batch->renderer. Returns 1 on success, 0 on failure. */
static int bind_gpu_shader_state(
    NanoUiBatch *batch, NanoUiGpuShaderEntry *entry,
    const uint8_t *uniform_bytes, int uniform_len)
{
    if (uniform_len > 0 && uniform_bytes != NULL) {
        if (!SDL_SetGPURenderStateFragmentUniforms(entry->state, 0u, uniform_bytes, (Uint32)uniform_len)) {
            fprintf(stderr, "nano-ui-sdl: SDL_SetGPURenderStateFragmentUniforms failed: %s\n", SDL_GetError());
            return 0;
        }
    }
    if (!SDL_SetGPURenderState(batch->renderer, entry->state)) {
        fprintf(stderr, "nano-ui-sdl: SDL_SetGPURenderState failed: %s\n", SDL_GetError());
        return 0;
    }
    return 1;
}
#endif

/* Flush pending geometry and draw one shader-backed index range via either
 * SDL 3.4's SDL_GPURenderState (SPIR-V/MSL) or OpenGL 3.2 (GLSL 150),
 * pushing its std140 uniform block at slot/binding 0. */
int nano_ui_batch_draw_shader(
    NanoUiBatch *batch, const uint8_t *verts, int vert_count,
    const uint8_t *indices, int index_start, int index_n,
    SDL_Texture *texture, int has_damage,
    float dmg_x, float dmg_y, float dmg_w, float dmg_h,
    uint64_t hash,
    const uint8_t *spirv, int spirv_len,
    const uint8_t *msl, int msl_len,
    const uint8_t *glsl, int glsl_len,
    int needs_tex,
    const uint8_t *uniform_bytes, int uniform_len)
{
    if (!batch || !batch->renderer || !verts || !indices || vert_count <= 0 || index_n < 3) {
        return 0;
    }
    if (index_start < 0) {
        index_start = 0;
    }
    if (has_damage && dmg_w > 0.f && dmg_h > 0.f &&
        !visible((const SDL_Vertex *)verts, vert_count, (const int *)indices + index_start,
                 index_n, dmg_x, dmg_y, dmg_w, dmg_h)) {
        return 1;
    }
    nano_ui_batch_flush(batch);
#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(batch->renderer);
    if (dev) {
        NanoUiGpuShaderEntry *entry = get_gpu_shader(
            batch, dev, hash, spirv, spirv_len, msl, msl_len, needs_tex);
        if (!entry || entry->failed || !entry->state) {
            return 0;
        }
        /* Always pass a valid texture so SDL's GPU renderer selects its textured
         * vertex pipeline (providing both location 0 v_color and location 1 v_uv). */
        SDL_Texture *tex = texture ? texture : ensure_white_tex(batch);
        if (!tex) {
            return 0;
        }
        batch->verts = verts;
        batch->vert_count = vert_count;
        const SDL_Vertex *v = (const SDL_Vertex *)verts;
        const int *idx = (const int *)indices + index_start;
        const int stride = (int)sizeof(SDL_Vertex);
        const float *xy = &v->position.x;
        int xy_stride = stride;
        if (batch->scale != 1.f || batch->origin_x != 0.f || batch->origin_y != 0.f) {
            xy = scaled_positions(batch, 0, idx, index_n);
            xy_stride = (int)(2 * sizeof(float));
            if (!xy) {
                return 0;
            }
        }
        if (!bind_gpu_shader_state(batch, entry, uniform_bytes, uniform_len)) {
            return 0;
        }
        bool ok = SDL_RenderGeometryRaw(
            batch->renderer, tex,
            xy, xy_stride, &v->color, stride,
            &v->tex_coord.x, stride,
            vert_count, idx, index_n, (int)sizeof(int));
        SDL_SetGPURenderState(batch->renderer, NULL);
        return ok ? 1 : 0;
    }
#else
    (void)spirv; (void)spirv_len; (void)msl; (void)msl_len; (void)needs_tex;
#endif
    if (glsl && glsl_len > 0 && nui_gl_ensure_init(batch)) {
        SDL_FlushRenderer(batch->renderer);
        if (!nui_gl_make_current(batch)) {
            return 0;
        }
        NanoUiGlShaderEntry *entry = nui_gl_get_shader(batch, hash, glsl, glsl_len);
        if (!entry || entry->failed || !entry->program) {
            return 0;
        }
        int cur_w = 0, cur_h = 0, is_target = 0;
        nui_gl_current_target_size(batch->renderer, &cur_w, &cur_h, &is_target);
        if (cur_w <= 0 || cur_h <= 0) {
            return 0;
        }
        NanoUiGlApi *gl = &batch->gl;
        gl->BindVertexArray(batch->gl_vao);
        if (batch->gl_uploaded_verts != verts || batch->gl_uploaded_vert_count != vert_count) {
            gl->BindBuffer(NUI_GL_ARRAY_BUFFER, batch->gl_vbo);
            gl->BufferData(NUI_GL_ARRAY_BUFFER, (NuiGLsizeiptr)((size_t)vert_count * sizeof(SDL_Vertex)),
                           verts, NUI_GL_STREAM_DRAW);
            batch->gl_uploaded_verts = verts;
            batch->gl_uploaded_vert_count = vert_count;
        }
        gl->BindBuffer(NUI_GL_ELEMENT_ARRAY_BUFFER, batch->gl_ebo);
        gl->BufferData(NUI_GL_ELEMENT_ARRAY_BUFFER, (NuiGLsizeiptr)((size_t)index_n * sizeof(int)),
                       (const int *)indices + index_start, NUI_GL_STREAM_DRAW);
        nui_gl_prepare_state(batch, entry, cur_w, cur_h, is_target,
                             batch->origin_x, batch->origin_y, batch->scale,
                             nui_gl_texture_id(texture), uniform_bytes, uniform_len);
        gl->DrawElements(NUI_GL_TRIANGLES, (NuiGLsizei)index_n, NUI_GL_UNSIGNED_INT, (const void *)0);
        nui_gl_restore_state(batch);
        return 1;
    }
    return 0;
}

/* Return the renderer's current render target (may be NULL for the window). */
SDL_Texture *nano_ui_batch_current_target(NanoUiBatch *batch)
{
    return (batch && batch->renderer) ? SDL_GetRenderTarget(batch->renderer) : NULL;
}

/* Acquire a pooled 64px-bucketed RGBA32 offscreen render target texture.
 * Returns a non-negative slot index, or -1 on failure. */
int nano_ui_batch_acquire_target(
    NanoUiBatch *batch, int min_w, int min_h, int *out_cap_w, int *out_cap_h)
{
    if (!batch || !batch->renderer) return -1;
    int need_w = min_w > 0 ? ((min_w + 63) & ~63) : 64;
    int need_h = min_h > 0 ? ((min_h + 63) & ~63) : 64;
    int best = -1;
    for (int i = 0; i < batch->target_count; i++) {
        NanoUiTargetSlot *s = &batch->targets[i];
        if (!s->in_use && s->tex &&
            s->cap_w >= need_w && s->cap_h >= need_h &&
            s->cap_w <= need_w * 2 && s->cap_h <= need_h * 2) {
            best = i;
            break;
        }
    }
    if (best < 0) {
        if (batch->target_count == batch->target_cap) {
            int new_cap = batch->target_cap > 0 ? batch->target_cap * 2 : 8;
            NanoUiTargetSlot *grown = (NanoUiTargetSlot *)realloc(
                batch->targets, (size_t)new_cap * sizeof(NanoUiTargetSlot));
            if (!grown) return -1;
            batch->targets = grown;
            batch->target_cap = new_cap;
        }
        SDL_Texture *tex = SDL_CreateTexture(
            batch->renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_TARGET, need_w, need_h);
        if (!tex) {
            fprintf(stderr, "nano-ui-sdl: offscreen target creation failed: %s\n", SDL_GetError());
            return -1;
        }
        SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
        SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_LINEAR);
        best = batch->target_count++;
        batch->targets[best].tex = tex;
        batch->targets[best].cap_w = need_w;
        batch->targets[best].cap_h = need_h;
    }
    batch->targets[best].in_use = 1;
    if (out_cap_w) *out_cap_w = batch->targets[best].cap_w;
    if (out_cap_h) *out_cap_h = batch->targets[best].cap_h;
    return best;
}

void nano_ui_batch_release_target(NanoUiBatch *batch, int slot)
{
    if (batch && slot >= 0 && slot < batch->target_count) {
        batch->targets[slot].in_use = 0;
    }
}

/* Bind pooled target slot (>= 0), or fallback_target (NULL = default backbuffer)
 * when slot < 0. Clears to transparent black when clear_transparent != 0. */
int nano_ui_batch_bind_target(
    NanoUiBatch *batch, int slot, SDL_Texture *fallback_target, int clear_transparent)
{
    if (!batch || !batch->renderer) return 0;
    nano_ui_batch_flush(batch);
    SDL_Texture *target = fallback_target;
    if (slot >= 0 && slot < batch->target_count) {
        target = batch->targets[slot].tex;
    }
    if (!SDL_SetRenderTarget(batch->renderer, target)) {
        return 0;
    }
    /* Offscreen Tier 2 effect targets accumulate premultiplied alpha over
     * transparent black via SDL_BLENDMODE_BLEND regardless of the root
     * target's draw blend mode (e.g. transparent windows or unconfigured
     * benchmark renderers). Save and restore the root mode around effect spans. */
    if (slot >= 0 && slot < batch->target_count) {
        if (!batch->saved_blend_valid) {
            if (SDL_GetRenderDrawBlendMode(batch->renderer, &batch->saved_blend_mode)) {
                batch->saved_blend_valid = 1;
            }
        }
        SDL_SetRenderDrawBlendMode(batch->renderer, SDL_BLENDMODE_BLEND);
    } else if (batch->saved_blend_valid) {
        SDL_SetRenderDrawBlendMode(batch->renderer, batch->saved_blend_mode);
        batch->saved_blend_valid = 0;
    }
    batch->has_clip = 0;
    SDL_SetRenderClipRect(batch->renderer, NULL);
    if (clear_transparent) {
        SDL_SetRenderDrawColor(batch->renderer, 0, 0, 0, 0);
        SDL_RenderClear(batch->renderer);
    }
    return 1;
}

/* Composite pooled source texture src_slot over the currently bound render
 * target at physical pixel rect (px, py, pw, ph) with texture UVs (u0,v0)..(u1,v1)
 * through a cached GPU (SPIR-V/MSL) or OpenGL 3.2 (GLSL 150) fragment shader. */
int nano_ui_batch_composite_effect(
    NanoUiBatch *batch, int src_slot,
    float px, float py, float pw, float ph,
    float u0, float v0, float u1, float v1,
    int blend_copy,
    uint64_t hash,
    const uint8_t *spirv, int spirv_len,
    const uint8_t *msl, int msl_len,
    const uint8_t *glsl, int glsl_len,
    const uint8_t *uniform_bytes, int uniform_len)
{
    if (!batch || !batch->renderer) return 0;
    nano_ui_batch_flush(batch);
    if (src_slot < 0 || src_slot >= batch->target_count || !batch->targets[src_slot].tex) {
        return 0;
    }
    (void)blend_copy;
    SDL_Texture *src_tex = batch->targets[src_slot].tex;

    SDL_Vertex quad[4];
    quad[0].position.x = px;      quad[0].position.y = py;
    quad[1].position.x = px + pw; quad[1].position.y = py;
    quad[2].position.x = px + pw; quad[2].position.y = py + ph;
    quad[3].position.x = px;      quad[3].position.y = py + ph;
    quad[0].tex_coord.x = u0;     quad[0].tex_coord.y = v0;
    quad[1].tex_coord.x = u1;     quad[1].tex_coord.y = v0;
    quad[2].tex_coord.x = u1;     quad[2].tex_coord.y = v1;
    quad[3].tex_coord.x = u0;     quad[3].tex_coord.y = v1;
    for (int i = 0; i < 4; i++) {
        quad[i].color.r = 1.f;
        quad[i].color.g = 1.f;
        quad[i].color.b = 1.f;
        quad[i].color.a = 1.f;
    }
    static const int indices[6] = { 0, 1, 2, 0, 2, 3 };

#if SDL_VERSION_ATLEAST(3, 4, 0)
    SDL_GPUDevice *dev = SDL_GetGPURendererDevice(batch->renderer);
    if (dev) {
        NanoUiGpuShaderEntry *entry = get_gpu_shader(
            batch, dev, hash, spirv, spirv_len, msl, msl_len, 1);
        if (!entry || entry->failed || !entry->state) {
            SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND_PREMULTIPLIED);
            bool ok = SDL_RenderGeometry(batch->renderer, src_tex, quad, 4, indices, 6);
            SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND);
            return ok ? 1 : 0;
        }
        SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND);
        if (!bind_gpu_shader_state(batch, entry, uniform_bytes, uniform_len)) {
            return 0;
        }
        bool ok = SDL_RenderGeometry(batch->renderer, src_tex, quad, 4, indices, 6);
        SDL_SetGPURenderState(batch->renderer, NULL);
        return ok ? 1 : 0;
    }
#else
    (void)spirv; (void)spirv_len; (void)msl; (void)msl_len;
#endif
    if (glsl && glsl_len > 0 && nui_gl_ensure_init(batch)) {
        SDL_FlushRenderer(batch->renderer);
        if (!nui_gl_make_current(batch)) {
            return 0;
        }
        NanoUiGlShaderEntry *entry = nui_gl_get_shader(batch, hash, glsl, glsl_len);
        if (entry && !entry->failed && entry->program) {
            int cur_w = 0, cur_h = 0, is_target = 0;
            nui_gl_current_target_size(batch->renderer, &cur_w, &cur_h, &is_target);
            NuiGLuint gl_src = nui_gl_texture_id(src_tex);
            if (cur_w > 0 && cur_h > 0 && gl_src != 0) {
                const SDL_Vertex tris[6] = { quad[0], quad[1], quad[2], quad[0], quad[2], quad[3] };
                NanoUiGlApi *gl = &batch->gl;
                nui_gl_prepare_state(batch, entry, cur_w, cur_h, is_target,
                                     0.0f, 0.0f, 1.0f, gl_src, uniform_bytes, uniform_len);
                gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_MIN_FILTER, NUI_GL_LINEAR);
                gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_MAG_FILTER, NUI_GL_LINEAR);
                gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_WRAP_S, NUI_GL_CLAMP_TO_EDGE);
                gl->TexParameteri(NUI_GL_TEXTURE_2D, NUI_GL_TEXTURE_WRAP_T, NUI_GL_CLAMP_TO_EDGE);
                gl->BindVertexArray(batch->gl_quad_vao);
                gl->BindBuffer(NUI_GL_ARRAY_BUFFER, batch->gl_quad_vbo);
                gl->BufferData(NUI_GL_ARRAY_BUFFER, (NuiGLsizeiptr)sizeof(tris), tris, NUI_GL_STREAM_DRAW);
                gl->DrawArrays(NUI_GL_TRIANGLES, 0, 6);
                nui_gl_restore_state(batch);
                return 1;
            }
        }
    }
    /* Fallback: blit the offscreen premultiplied target unshaded so the
     * wrapped subtree never disappears on shader compile failure. */
    SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND_PREMULTIPLIED);
    bool ok = SDL_RenderGeometry(batch->renderer, src_tex, quad, 4, indices, 6);
    SDL_SetTextureBlendMode(src_tex, SDL_BLENDMODE_BLEND);
    return ok ? 1 : 0;
}
