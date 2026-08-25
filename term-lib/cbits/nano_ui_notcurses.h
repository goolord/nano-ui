#ifndef NANO_UI_NOTCURSES_H
#define NANO_UI_NOTCURSES_H

#include <stddef.h>
#include <stdint.h>
#include <notcurses/notcurses.h>

typedef struct nano_ui_nc {
  struct notcurses *nc;
  struct ncplane *std;
} nano_ui_nc;

/* Mirror ncintype_e values for Haskell FFI. */
#define NANO_UI_NCTYPE_PRESS 1
#define NANO_UI_NCTYPE_RELEASE 3

nano_ui_nc *nano_ui_nc_init(void);
void nano_ui_nc_fini(nano_ui_nc *nui);
int nano_ui_nc_dim(nano_ui_nc *nui, unsigned *rows, unsigned *cols);
int nano_ui_nc_mouse_enable(nano_ui_nc *nui);
int nano_ui_nc_blit_cells(
    nano_ui_nc *nui,
    int w,
    int h,
    const uint32_t *cells,
    const uint32_t *prev,
    int prev_w,
    int prev_h);
uint32_t nano_ui_nc_get(nano_ui_nc *nui, int timeout_ms, ncinput *ni);

size_t nano_ui_ncinput_size(void);
uint32_t nano_ui_ncinput_id(const ncinput *ni);
int nano_ui_ncinput_y(const ncinput *ni);
int nano_ui_ncinput_x(const ncinput *ni);
unsigned nano_ui_ncinput_modifiers(const ncinput *ni);
int nano_ui_ncinput_evtype(const ncinput *ni);

#endif
