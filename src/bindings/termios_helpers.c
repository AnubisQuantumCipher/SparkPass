/*
 * Platform-portable termios helpers for SparkPass
 * Works on Linux, macOS, FreeBSD, OpenBSD, NetBSD
 */

#include <termios.h>

/*
 * Disable echo in termios structure (portable across all Unix variants)
 * Clears the ECHO bit in c_lflag field
 */
void sparkpass_termios_disable_echo(struct termios *term) {
    term->c_lflag &= ~ECHO;
}

/*
 * Enable echo in termios structure (portable across all Unix variants)
 * Sets the ECHO bit in c_lflag field
 */
void sparkpass_termios_enable_echo(struct termios *term) {
    term->c_lflag |= ECHO;
}
