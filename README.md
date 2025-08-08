# Build and run

This won’t work while an X11 or Wayland session is running.
The reason is that either the servers restrict access to the
framebuffer, or they update the framebuffer so quickly that
changes are not visible to the human eye

1. open any one of ttys (Ctrl + Alt + F[3-7] on Ubuntu)
2. `./main`

