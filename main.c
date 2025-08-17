/** main.c
 *
 * $ gcc main.c -o main -lm
 *
 **/

#include <stdio.h>
#include <stdlib.h>
#include <linux/fb.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <stdlib.h>

/* Ref: https://github.com/dhepper/font8x8 */
#include "font8x8_basic.h"
#include "font8x8_box.h"

#define FRAMEBUFFERDEVICE "/dev/fb0"
#define EOK 0

int fb_fd;
void *fbuffer = NULL;
struct fb_fix_screeninfo fix;
struct fb_var_screeninfo info;
int bytes_per_pixel;

int openfbuffer()
{
  int ret = EOK;

  fb_fd = open(FRAMEBUFFERDEVICE, O_RDWR);
  if (fb_fd == -1) {
    fprintf(stderr, "%s open failed\n", FRAMEBUFFERDEVICE);
    return -1;
  }

  ret = ioctl(fb_fd, FBIOGET_FSCREENINFO, &fix);
  if (ret != 0) {
    fprintf(stderr, "ioctl fb_fd failed\n");
    return ret;
  }

  ret = ioctl(fb_fd, FBIOGET_VSCREENINFO, &info);
  if (ret != 0) {
    fprintf(stderr, "ioctl fb_fd failed\n");
    return ret;
  }

  fbuffer = mmap(NULL, fix.smem_len, PROT_READ | PROT_WRITE,
                 MAP_FILE | MAP_SHARED, fb_fd, 0);
  if (fbuffer == MAP_FAILED) {
    close(fb_fd);
    return ENOMEM;
  }

  memset(fbuffer, 0, fix.smem_len);

  return ret;
}

void closefbuffer()
{
  munmap(fbuffer, fix.smem_len);
  close(fb_fd);
}

static inline void set_pixel_color(int x, int y, unsigned int color)
{
  int offset;
  unsigned int *pixel;

  if (x < 0 || x > info.xres || y < 0 || y > info.yres) {
    return;
  }

  offset = (y * fix.line_length) + (x * bytes_per_pixel);

  pixel = (unsigned int *) ((char *) fbuffer + offset);

  *pixel = color;
}

/* Bresenham line algorithm
 * Note: I have tried to prove this algorithm
 * by hand multilple times but failed everytime.
 * One day! */
void draw_line(int x1, int y1, int x2, int y2)
{
  int dx = abs(x2 - x1);
  int dy = abs(y2 - y1);
  int sx = x2 > x1 ? 1 : -1;
  int sy = y2 > y1 ? 1 : -1;
  int x = x1, y = y1;
  int error = dx - dy;
  int terror; /* temporary error */

  for(;;) {
    set_pixel_color(x, y, 0xFFFFFF);

    if (x == x2 && y == y2)
      break;

    terror = 2 * error;

    if (terror > -dy) {
      error -= dy;
      x += sx;
    }

    if (terror < dx) {
      error += dx;
      y += sy;
    }
  }

  /* ----------------------------
   Brute force (inefficient)

  int m, c;

  m = (y2 - y1) / (x2 - x1);
  c = y1 - m * x1;

  for (int x = x1; x <= x2; x++) {
    int y;

    y = round(m * x + c);

    set_pixel_color(x, y, 0xFFFFFF);
  }

  ---------------------------- */
}

void draw_square(int x, int y, int length)
{
  /* Brute force (efficient) */

  for (int i = 0; i < length; i++) {
    for (int j = 0; j < length; j++) {
     set_pixel_color(x + i, y + j, 0xFFFFFF);
     set_pixel_color(x - i, y + j, 0xFFFFFF);
     set_pixel_color(x + i, y - j, 0xFFFFFF);
     set_pixel_color(x - i, y - j, 0xFFFFFF);
    }
  }
}

void draw_symmetric_points(int x, int y, int cx, int cy)
{
  set_pixel_color(x + cx, y + cy, 0xFFFFFF);
  set_pixel_color(x + cx, y - cy, 0xFFFFFF);
  set_pixel_color(x - cx, y + cy, 0xFFFFFF);
  set_pixel_color(x - cx, y - cy, 0xFFFFFF);
  set_pixel_color(x + cy, y + cx, 0xFFFFFF);
  set_pixel_color(x + cy, y - cx, 0xFFFFFF);
  set_pixel_color(x - cy, y + cx, 0xFFFFFF);
  set_pixel_color(x - cy, y - cx, 0xFFFFFF);
}

/* Bresenham's circle drawing algorithm and Midpoint cirlce drawing algorithm
 *
 * Ref: https://imruljubair.github.io/teaching/material/CSE4203/Chapter%20-%208%20(part%20-%20B).pdf */
void _draw_circle(int x, int y, int radius)
{
  int cx = 0;
  int cy = radius;
  int d = 3 - 2 * radius;

  draw_symmetric_points(x, y, cx, cy);

  while (cx <= cy) {
    cx++;
    if (d < 0) {
      d = d + 4 * cx + 6;
    } else {
      cy = cy - 1;
      d = d + 4 * (cx - cy) + 10;
    }

    draw_symmetric_points(x, y, cx, cy);
  }
}

void draw_circle(int x, int y, int radius)
{
  int cx = 0;
  int cy = radius;
  int d = 1 - radius;

  draw_symmetric_points(x, y, cx, cy);

  while (cy > cx) {
    if (d < 0) {
      d = d + 2 * cx + 3;
    } else {
      d = d + 2 * (cx - cy) + 5;
      cy--;
    }

    cx++;
    draw_symmetric_points(x, y, cx, cy);
  }
}

#define RANGE 2.0

/* Mandelbro's fractal */
void draw_fractal()
{
  float x_min = -RANGE;
  float x_max = RANGE;
  float y_min = -RANGE;
  float y_max = RANGE;
  float *normalized_x = NULL;
  float *normalized_y = NULL;
  float x_interval, y_interval;
  int max_iterations = 2000;
  const unsigned int colors[] = {
    0xFFFFFF,
    0xEEEEEE,
    0xDDDDDD,
    0xCCCCCC,
    0xBBBBBB,
    0xAAAAAA,
    0x999999,
    0x888888,
    0x777777,
    0x666666,
    0x555555,
    0x444444,
    0x333333,
    0x222222,
    0x111111,
    0x000000,
  };
  int num_colors = sizeof (colors) / sizeof (colors[0]);

  normalized_x = malloc(sizeof (float) * info.xres);
  normalized_y = malloc(sizeof (float) * info.yres);

  x_interval = (x_max - x_min) / info.xres;
  y_interval = (y_max - y_min) / info.yres;

  normalized_x[0] = x_min;
  normalized_y[0] = y_max;

  /* Display resolution: 1366 * 768
     Normalized into: 4.0 * 4.0 */
  for (int i = 1; i < info.xres; i++) {
    normalized_x[i] = normalized_x[i - 1] + x_interval;
  }

  for (int i = 1; i < info.yres; i++) {
    normalized_y[i] = normalized_y[i - 1] - y_interval;
  }

  /* Complex number
     c = a + ib
     a <- real part
     b <- imaginary part

     >> (a + ib) ^ 2
     >> a ^ 2 + 2 * a * ib + (ib) ^ 2
     >> (a^2 - b^2) + 2ab (i)
            ^           ^
            |           |
           real        imaginary

   */

  /* Mandelbrot's equation

     Zn+1 = Zn ^ 2 + c
     c <- normalized pixel coordinates

   */

  for (int x = 0; x < info.xres; x++) {
    for (int y = 0; y < info.yres; y++) {
      float a = 0.0;
      float b = 0.0;
      float a_square = 0.0;
      float b_square = 0.0;
      int i = 1;

      while (i < max_iterations
             && a_square + b_square < 4.0) {
        a_square = a * a;
        b_square = b * b;

        b = 2 * a * b + normalized_y[y];
        a = a_square - b_square + normalized_x[x];;

        i++;
      }

      set_pixel_color(x, y, colors[i % num_colors]);
    }
  }
}

void draw_logistic_map_fractal()
{
  float x_min = 0;
  float x_max = 4.0;
  float y_min = 0;
  float y_max = 1.0;
  float *normalized_x = NULL;
  double x_interval, y_interval;
  int skip = 1000;

  normalized_x = malloc(sizeof (float) * info.xres);

  x_interval = (x_max - x_min) / info.xres;
  y_interval = (y_max - y_min) / info.yres;

  normalized_x[0] = x_min;

  for (int i = 1; i < info.xres; i++) {
    normalized_x[i] = normalized_x[i - 1] + x_interval;
  }

  for (int x = 0; x < info.xres; x++) {
    float r = normalized_x[x];
    float ex = 0.5;
    int y;

    /* skip initial values because we are interested
     * in saturated values */
    for (int i = 0; i < skip; i++) {
      ex = r * ex * (1.0 - ex);
    }

    for (int i = 0; i < 3000; i++)  {
      ex = r * ex * (1.0 - ex);

      /* graph co-ordinate -> pixel co-ordinate */
      y = (1 - ex) / y_interval;

      set_pixel_color(x, y, 0xFFFFFF);
    }
  }
}

void draw_character(char c, int x, int y, unsigned int color)
{
  int length = 8;
  unsigned char *map;

  map = font8x8_basic[c];

  for (int i = 0; i < length; i++) {
    unsigned char row = map[i];
    for (int j = 0; j < length; j++) {
      unsigned int clr = ((row >> j) & 1) * color;

      set_pixel_color(x + j, y + i, clr);
    }
  }
}

void draw_crosshair(int x, int y,
                    int length, int gap, int width)
{
  draw_square(x, y, length);

  for (int i = 0; i < width; i++) {
    set_pixel_color(x, y - length - gap - i, 0xFFFFFF);
    set_pixel_color(x, y + length + gap + i, 0xFFFFFF);
    set_pixel_color(x - length - gap - i, y, 0xFFFFFF);
    set_pixel_color(x + length + gap + i, y, 0xFFFFFF);
  }
}

void cat(char *file)
{
  FILE *fptr;
  char c;
  int x = 0, y = 10;
  int font_width = 9;

  /* WIP: implement paging */

  fptr = fopen(file, "r");

  while ((c = fgetc(fptr)) != EOF) {
    if (c == '\n') {
      x = 0;
      y += font_width;
      continue;
    }

    if (x + font_width > info.xres) {
      x = 0;
      y += font_width;
    }

    /* This is very inefficient; we can reuse
     * the calculate offsets but don't care */
    draw_character(c, x, y, 0xFFFFFF);
    x += font_width;
  }

  fclose(fptr);
}

void draw_sine()
{
  int wavelength = 60; /* in pixels */
  int amplitude = 40;  /* in pixels */
  int speed = 1;       /* in pixels */
  double pi = 3.14;
  double factor;
  int y_center;
  size_t memsize, offset;

  factor = 3.14 / wavelength;
  y_center = info.yres / 2;

  offset = (y_center - amplitude + 1) * fix.line_length;
  memsize = (amplitude * 2 - 1) * fix.line_length;

  /* Press Ctrl + C to exit;
   * OS should bother about releasing the memory */
  while (1) {
    /* clear the required framebuffer area */
    memset(fbuffer + offset, 0, memsize);

    for (int x = 0; x < info.xres; x++) {
      double angle;
      int y;

      angle = (x + speed) * factor;
      y = (int) (sin (angle) * amplitude) + y_center;

      set_pixel_color(x, y, 0xFFFFFF);
    }

    usleep(5000); /* are 5 milliseconds enough? *blinks* */

    speed++;
  }
}

void
draw_canvas()
{
  for(int frame;;frame++) {
    for (int x = 0; x < info.xres; x++) {
      for (int y = 0; y < info.yres; y++) {
        unsigned int color;
        int r, g, b;

        r = 255 * ((float) x / info.xres);
        g = 255 * ((float) y / info.yres);
        b = 255 * fabs(sin (frame * 0.05));

        color = (r << 16) | (g << 8) | (b << 0);

        set_pixel_color(x, y, color);
      }
    }
  }
}

#define min3(x, y, z) \
        ((x) < (y) && (x) < (z) \
         ? (x) : \
        ((y) < (z) \
         ? (y) : (z)))

#define max3(x, y, z) \
        ((x) >= (y) && (x) >= (z) \
         ? (x) : \
        ((y) >= (z) \
         ? (y) : (z)))

#define DIV(expr1, expr2) \
        ((double) (expr1) / (double) (expr2))

/* Barycentric coordinates
 * Ref: https://www.youtube.com/watch?v=HYAgJN3x4GA
 */
static bool
__point_in_triangle(int ax, int ay, int bx, int by,
                  int cx, int cy, int px, int py)
{
  double w1, w2;

  w1 = DIV(  ax * (cy - ay) + (py - ay) * (cx - ax) - px * (cy - ay),
          /*---------------------------------------------------------*/
                 (by - ay) * (cx - ax) - (bx - ax) * (cy - ay)
          );

  w2 = DIV(  py - ay - w1 * (by - ay),
          /*--------------------------*/
                    cy - ay
          );

  return (w1 >= 0) && (w2 >= 0) && ((w1 + w2) <= 1.0);
}

static bool
point_in_triangle(int ax, int ay, int bx, int by,
                  int cx, int cy, int px, int py)
{
  double ab_cross, bc_cross, ca_cross; /*  cross products */

  /* If we traverse in a particular direction,
   * a point inside the triangle will always be on the
   * same side for each vector ie. left/right */

  /* A  -> B */
  ab_cross = (bx - ax) * (py - ay) - (by - ay) * (px - ax);
        /* B -> C */
  bc_cross = (cx - bx) * (py - by) - (cy - by) * (px - bx);
             /* C -> A */
  ca_cross = (ax - cx) * (py - cy) - (ay - cy) * (px - cx);

  return ((ab_cross >= 0 && bc_cross >= 0 && ca_cross >= 0)
          || (ab_cross < 0 && bc_cross < 0 && ca_cross < 0));

}

#define SIZE 200

void
draw_rasterized_triangle()
{
  int vertices[3][2] = {
    {0, -SIZE},
    {-SIZE, SIZE},
    {SIZE, SIZE}
  };
  int x_center, y_center;
  int ax, bx, cx, px;
  int ay, by, cy, py;
  int min_x, max_x, min_y, max_y;

  x_center = info.xres / 2;
  y_center = info.yres / 2;

  ax = x_center + vertices[0][0];
  ay = y_center + vertices[0][1];

  bx = x_center + vertices[1][0];
  by = y_center + vertices[1][1];

  cx = x_center + vertices[2][0];
  cy = y_center + vertices[2][1];

  min_x = min3(ax, bx, cx);
  min_x = min_x < 0 ? 0 : min_x;
  max_x = max3(ax, bx, cx);
  max_x = max_x > info.xres ? info.xres : max_x;

  min_y = min3(ay, by, cy);
  min_y = min_y < 0 ? 0 : min_y;
  max_y = max3(ay, by, cy);
  max_y = max_y > info.yres ? info.yres - 1: max_y;

  for (int px = min_x; px <= max_x; px++) {
    for (int py = min_y; py <= max_y; py++) {
      if (point_in_triangle(ax, ay, bx, by, cx, cy, px, py)) {
        set_pixel_color(px, py, 0xFFFFFF);
      }
    }
  }
}

void draw()
{
  bytes_per_pixel = info.bits_per_pixel / 8;

  /* cat("README.md"); */

  /* draws a cirlce */
  /*
  int cx = info.xres / 2;
  int cy = info.yres / 2;
  draw_circle(cx, cy, 200);
  */

  /* draws crosshair */
  /*
  draw_crosshair(cx, cy, 5, 2, 3);
  */

  /*
  draw_fractal();
  */

  /*
  draw_sine();
  */

  /*
  draw_logistic_map_fractal();
  */

  /*
  draw_canvas();
  */

  draw_rasterized_triangle();

  /* draws characters on screen
  int cx = info.xres / 2;
  int cy = info.yres / 2;

  cx -= 27;
  cx += 9;draw_character('H', cx, cy, 0xFFFFFF);
  cx += 9;draw_character('E', cx, cy, 0xFFFFFF);
  cx += 9;draw_character('L', cx, cy, 0xFFFFFF);
  cx += 9;draw_character('L', cx, cy, 0xFFFFFF);
  cx += 9;draw_character('0', cx, cy, 0xFFFFFF);
  cx += 9;draw_character('!', cx, cy, 0xFFFFFF);
  */
}

int main()
{
  int ret = EOK;

  ret = openfbuffer();
  if (ret != EOK) {
    return ret;
  }

  draw();

  sleep(10);

  closefbuffer();

  return 0;
}
