/* Linux SDL demo throughput probe, loaded with LD_PRELOAD. It measures the
 * real event loop between presents, including GC and presentation waits, then
 * prints results and exits after 1,000 warmup + 10,000 measured frames.
 *
 * cc -O2 -Wall -Wextra -shared -fPIC profiles/continuous-meter.c \
 *    -o /tmp/nano-continuous-meter.so -ldl
 * SDL_VIDEODRIVER=x11 SDL_RENDER_DRIVER=opengl \
 * LD_PRELOAD=/tmp/nano-continuous-meter.so path/to/nano-ui-sdl-demo --continuous
 *
 * Compare alternating runs with identical renderer, dimensions, DPI, RTS
 * options and UI state. NANO_DEBUG_OPEN=1 measures the real debug-open UI.
 * CPU time is process user+system time, not CPU hardware cycles. This probe is
 * for the single-window demo: it counts calls to SDL_RenderPresent.
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>

enum { WARMUP = 1000, FRAMES = 10000 };

static double wall_time(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec + t.tv_nsec * 1e-9;
}

static double cpu_time(void) {
    struct rusage r;
    getrusage(RUSAGE_SELF, &r);
    return r.ru_utime.tv_sec + r.ru_utime.tv_usec * 1e-6
         + r.ru_stime.tv_sec + r.ru_stime.tv_usec * 1e-6;
}

static int compare_double(const void *a, const void *b) {
    double x = *(const double *)a, y = *(const double *)b;
    return (x > y) - (x < y);
}

bool SDL_RenderPresent(void *renderer) {
    static bool (*present)(void *);
    static const char *(*name)(void *);
    static bool (*output_size)(void *, int *, int *);
    static unsigned count;
    static double start, previous, start_cpu, present_time;
    static double frame_times[FRAMES];

    if (!present) {
        present = dlsym(RTLD_NEXT, "SDL_RenderPresent");
        name = dlsym(RTLD_NEXT, "SDL_GetRendererName");
        output_size = dlsym(RTLD_NEXT, "SDL_GetRenderOutputSize");
        if (!present || !name || !output_size) {
            fputs("continuous-meter: SDL symbols unavailable\n", stderr);
            _exit(2);
        }
    }
    double before = wall_time();
    bool ok = present(renderer);
    double after = wall_time();
    if (!ok) {
        fputs("continuous-meter: SDL_RenderPresent failed\n", stderr);
        _exit(2);
    }

    ++count;
    if (count == WARMUP) {
        int width, height;
        if (!output_size(renderer, &width, &height)) _exit(2);
        fprintf(stderr, "continuous-meter renderer=%s pixels=%dx%d warmup=%d frames=%d\n",
                name(renderer), width, height, WARMUP, FRAMES);
        start = previous = wall_time();
        start_cpu = cpu_time();
    } else if (count > WARMUP) {
        frame_times[count - WARMUP - 1] = after - previous;
        previous = after;
        present_time += after - before;
        if (count == WARMUP + FRAMES) {
            double elapsed = after - start;
            double cpu = cpu_time() - start_cpu;
            qsort(frame_times, FRAMES, sizeof(double), compare_double);
            fprintf(stderr,
                    "continuous-meter ms/frame=%.6f cpu-ms/frame=%.6f fps=%.1f "
                    "p50-ms=%.6f p95-ms=%.6f p99-ms=%.6f present-ms/frame=%.6f\n",
                    elapsed * 1000 / FRAMES, cpu * 1000 / FRAMES,
                    FRAMES / elapsed, frame_times[FRAMES / 2 - 1] * 1000,
                    frame_times[FRAMES * 95 / 100 - 1] * 1000,
                    frame_times[FRAMES * 99 / 100 - 1] * 1000,
                    present_time * 1000 / FRAMES);
            fflush(NULL);
            _exit(0);
        }
    }
    return ok;
}
