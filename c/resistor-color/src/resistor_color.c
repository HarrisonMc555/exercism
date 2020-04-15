#include "resistor_color.h"

resistor_band_t colors_array[] = {
    BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE,
};

resistor_band_t* colors(void) {
    return colors_array;
}

unsigned int color_code(resistor_band_t color) {
    return color;
}

