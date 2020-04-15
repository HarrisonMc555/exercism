#ifndef RESISTOR_COLOR_H
#define RESISTOR_COLOR_H

typedef unsigned int resistor_band_t;

#define BLACK 0
#define BROWN 1
#define RED 2
#define ORANGE 3
#define YELLOW 4
#define GREEN 5
#define BLUE 6
#define VIOLET 7
#define GREY 8
#define WHITE 9

resistor_band_t* colors(void);

unsigned int color_code(resistor_band_t color);

#endif
