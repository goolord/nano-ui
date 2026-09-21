#include "RGFW.h"
#include <string.h>

size_t rgfw_test_event_size(void) { return sizeof(RGFW_event); }

/* Write through the native types, independently of the Haskell offsets. */
void rgfw_test_event(RGFW_event *e, int kind)
{
    memset(e, 0, sizeof(*e));
    switch (kind) {
    case 0:
        e->key.type = RGFW_keyPressed;
        e->key.value = RGFW_keyHome;
        e->key.repeat = 1;
        e->key.mod = RGFW_modControl | RGFW_modShift;
        e->key.state = 1;
        break;
    case 1:
        e->keyChar.type = RGFW_keyChar;
        e->keyChar.value = 0x1f600;
        break;
    case 2:
        e->mouse.type = RGFW_mouseMotion;
        e->mouse.x = -130; e->mouse.y = 245;
        break;
    case 3:
        e->delta.type = RGFW_mouseScroll;
        e->delta.x = 1.25f; e->delta.y = -2.5f;
        break;
    case 4:
        e->button.type = RGFW_mouseButtonPressed;
        e->button.value = RGFW_mouseRight;
        break;
    case 5:
        e->update.type = RGFW_windowResized;
        e->update.w = 643; e->update.h = 481;
        break;
    case 6:
        e->scale.type = RGFW_scaleUpdated;
        e->scale.x = 1.25f; e->scale.y = 1.5f;
        break;
    }
}
