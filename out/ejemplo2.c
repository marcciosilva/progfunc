#include <stdio.h>
int _w;
int _x;
int _y[3];
int _z[2][4];
void main() {
_w = 10;
_x = 1;
_y[(_w - 8) - 2] = 10;
_z[1 - 1][1 - 1] = 9;
}