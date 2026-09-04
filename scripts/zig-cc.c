#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void fix_rsp_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (sz <= 0) { fclose(f); return; }

    char *buf = (char *)malloc(sz + 1);
    if (!buf) { fclose(f); return; }
    fread(buf, 1, sz, f);
    buf[sz] = '\0';
    fclose(f);

    const char *target = "x86_64-unknown-windows-gnu";
    const char *replacement = "x86_64-windows-gnu";
    char *pos = strstr(buf, target);
    if (pos) {
        FILE *out = fopen(path, "wb");
        if (out) {
            char *cur = buf;
            while (pos) {
                fwrite(cur, 1, pos - cur, out);
                fputs(replacement, out);
                cur = pos + strlen(target);
                pos = strstr(cur, target);
            }
            fputs(cur, out);
            fclose(out);
        }
    }
    free(buf);
}

int main(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '@') {
            fix_rsp_file(argv[i] + 1);
        }
    }

    // Build command line: "zig cc " + rest of args
    // We pass GetCommandLineW or rebuild it
    char cmdline[32768];
    strcpy(cmdline, "zig cc");
    for (int i = 1; i < argc; i++) {
        strcat(cmdline, " ");
        const char *arg = argv[i];
        char rewritten[1024];
        if (strcmp(arg, "x86_64-unknown-windows-gnu") == 0) {
            arg = "x86_64-windows-gnu";
        } else if (strncmp(arg, "--target=x86_64-unknown-windows-gnu", 35) == 0) {
            snprintf(rewritten, sizeof(rewritten), "--target=x86_64-windows-gnu%s", arg + 35);
            arg = rewritten;
        } else if (strncmp(arg, "-target=x86_64-unknown-windows-gnu", 34) == 0) {
            snprintf(rewritten, sizeof(rewritten), "-target=x86_64-windows-gnu%s", arg + 34);
            arg = rewritten;
        }

        // Quote if contains spaces
        if (strchr(arg, ' ')) {
            strcat(cmdline, "\"");
            strcat(cmdline, arg);
            strcat(cmdline, "\"");
        } else {
            strcat(cmdline, arg);
        }
    }

    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    if (!CreateProcessA(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        fprintf(stderr, "zig-cc wrapper: failed to execute zig cc (error %lu)\n", GetLastError());
        return 1;
    }

    WaitForSingleObject(pi.hProcess, INFINITE);
    DWORD exitCode = 0;
    GetExitCodeProcess(pi.hProcess, &exitCode);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return (int)exitCode;
}
