#include <sys/ioctl.h>
int nano_terminal_size(int fd) {
    struct winsize size = { .ws_row = 24, .ws_col = 80 };
    return ioctl(fd, TIOCSWINSZ, &size);
}
