#include "nano_ui_notcurses.h"

#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static uint64_t
channels_from_rgba(uint32_t fg, uint32_t bg)
{
  uint64_t ch = 0;

  if ((fg & 0xffu) >= 32u) {
    ncchannels_set_fg_rgb8(
        &ch,
        (fg >> 24) & 0xffu,
        (fg >> 16) & 0xffu,
        (fg >> 8) & 0xffu);
  }
  if ((bg & 0xffu) >= 32u) {
    ncchannels_set_bg_rgb8(
        &ch,
        (bg >> 24) & 0xffu,
        (bg >> 16) & 0xffu,
        (bg >> 8) & 0xffu);
  }
  return ch;
}

static uint32_t
grid_ch(const uint32_t *cells, int w, int x, int y)
{
  return cells[(y * w + x) * 3u];
}

static int
font_awesome_p(uint32_t ch)
{
  return ch >= 0xF000u && ch <= 0xF2E0u;
}

static int
put_cell(
    struct ncplane *plane,
    const uint32_t *cells,
    int w,
    int y,
    int x,
    uint32_t ch,
    uint32_t fg,
    uint32_t bg)
{
  unsigned rows = 0;
  unsigned cols = 0;
  uint64_t channels;
  nccell c;
  int wrote;

  ncplane_dim_yx(plane, &rows, &cols);
  if (y < 0 || x < 0 || (unsigned)y >= rows || (unsigned)x >= cols) {
    return 0;
  }

  /* wideTrailChar is NUL. Skip after FA so a width-2 putc is not smashed.
   * Any other NUL is a ghost: write a space. */
  if (ch == 0) {
    if (x > 0 && font_awesome_p(grid_ch(cells, w, x - 1, y))) {
      return 0;
    }
    ch = 32;
  }

  channels = channels_from_rgba(fg, bg);
  c = (nccell)NCCELL_INITIALIZER(' ', 0, channels);
  if (ch > 0x7fu) {
    if (nccell_load_ucs32(plane, &c, ch) < 0) {
      return 0;
    }
  } else if (nccell_load_char(plane, &c, (char)ch) < 0) {
    return 0;
  }
  /* ncplane_putc_yx returns columns advanced (1 or 2), not 0.
   * A wide glyph on the last column fails. Retry at width 1. Never abort
   * the frame: one bad cell used to surface as "notcurses blit failed". */
  wrote = ncplane_putc_yx(plane, y, x, &c);
  if (wrote < 0) {
    c.width = 1;
    wrote = ncplane_putc_yx(plane, y, x, &c);
    if (wrote < 0) {
      return 0;
    }
  }
  return 0;
}

static int
blit_cell(struct ncplane *plane, const uint32_t *cells, int w, int x, int y)
{
  const uint32_t *cell = cells + (y * w + x) * 3u;
  return put_cell(plane, cells, w, y, x, cell[0], cell[1], cell[2]);
}

static int
blit_all_cells(struct ncplane *plane, int w, int h, const uint32_t *cells)
{
  for (int y = 0; y < h; ++y) {
    for (int x = 0; x < w; ++x) {
      if (blit_cell(plane, cells, w, x, y) < 0) {
        return -1;
      }
    }
  }
  return 0;
}

nano_ui_nc *
nano_ui_nc_init(void)
{
  setlocale(LC_ALL, "");

  notcurses_options opts = {
      .termtype = NULL,
      .loglevel = NCLOGLEVEL_SILENT,
      .margin_t = 0,
      .margin_r = 0,
      .margin_b = 0,
      .margin_l = 0,
      .flags = NCOPTION_SUPPRESS_BANNERS | NCOPTION_NO_CLEAR_BITMAPS | NCOPTION_NO_FONT_CHANGES,
  };

  struct notcurses *nc = notcurses_core_init(&opts, stdout);
  if (!nc) {
    return NULL;
  }

  if (notcurses_enter_alternate_screen(nc)) {
    notcurses_stop(nc);
    return NULL;
  }
  notcurses_cursor_disable(nc);
  notcurses_linesigs_disable(nc);

  nano_ui_nc *nui = malloc(sizeof(*nui));
  if (!nui) {
    notcurses_stop(nc);
    return NULL;
  }

  nui->nc = nc;
  nui->std = notcurses_stdplane(nc);
  if(!nui->std){
    notcurses_stop(nc);
    free(nui);
    return NULL;
  }
  return nui;
}

void
nano_ui_nc_fini(nano_ui_nc *nui)
{
  if (!nui) {
    return;
  }
  notcurses_leave_alternate_screen(nui->nc);
  notcurses_stop(nui->nc);
  free(nui);
}

int
nano_ui_nc_dim(nano_ui_nc *nui, unsigned *rows, unsigned *cols)
{
  if (!nui || !rows || !cols) {
    return -1;
  }
  notcurses_term_dim_yx(nui->nc, rows, cols);
  return 0;
}

int
nano_ui_nc_mouse_enable(nano_ui_nc *nui)
{
  if (!nui) {
    return -1;
  }
  return notcurses_mice_enable(nui->nc, NCMICE_ALL_EVENTS);
}

int
nano_ui_nc_blit_cells(
    nano_ui_nc *nui,
    int w,
    int h,
    const uint32_t *cells,
    const uint32_t *prev,
    int prev_w,
    int prev_h)
{
  if (!nui || !cells || w <= 0 || h <= 0) {
    return -1;
  }

  struct ncplane *plane = nui->std;
  const int full_repaint =
      !prev || prev_w != w || prev_h != h || prev_w <= 0 || prev_h <= 0;

  if (full_repaint) {
    ncplane_erase(plane);
    if (blit_all_cells(plane, w, h, cells)) {
      return -1;
    }
    return notcurses_render(nui->nc);
  }

  for (int y = 0; y < h; ++y) {
    for (int x = 0; x < w; ++x) {
      const uint32_t *cell = cells + (y * w + x) * 3u;
      uint32_t ch = cell[0];
      uint32_t fg = cell[1];
      uint32_t bg = cell[2];

      if (x < prev_w && y < prev_h) {
        const uint32_t *old = prev + (y * prev_w + x) * 3u;
        if (old[0] == ch && old[1] == fg && old[2] == bg) {
          continue;
        }
      }

      if (blit_cell(plane, cells, w, x, y) < 0) {
        ncplane_erase(plane);
        if (blit_all_cells(plane, w, h, cells) < 0) {
          return -1;
        }
        return notcurses_render(nui->nc);
      }
    }
  }

  return notcurses_render(nui->nc);
}

uint32_t
nano_ui_nc_get(nano_ui_nc *nui, int timeout_ms, ncinput *ni)
{
  if (!nui || !ni) {
    return (uint32_t)-1;
  }

  struct timespec ts;
  struct timespec *tsp = NULL;
  if (timeout_ms >= 0) {
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
      return (uint32_t)-1;
    }
    ts.tv_sec += timeout_ms / 1000;
    ts.tv_nsec += (timeout_ms % 1000) * 1000000L;
    if (ts.tv_nsec >= 1000000000L) {
      ts.tv_sec += 1;
      ts.tv_nsec -= 1000000000L;
    }
    tsp = &ts;
  }

  return notcurses_get(nui->nc, tsp, ni);
}

size_t
nano_ui_ncinput_size(void)
{
  return sizeof(ncinput);
}

uint32_t
nano_ui_ncinput_id(const ncinput *ni)
{
  return ni ? ni->id : 0;
}

int
nano_ui_ncinput_y(const ncinput *ni)
{
  return ni ? ni->y : -1;
}

int
nano_ui_ncinput_x(const ncinput *ni)
{
  return ni ? ni->x : -1;
}

unsigned
nano_ui_ncinput_modifiers(const ncinput *ni)
{
  return ni ? ni->modifiers : 0;
}

int
nano_ui_ncinput_evtype(const ncinput *ni)
{
  return ni ? (int)ni->evtype : 0;
}
