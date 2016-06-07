#include <stdio.h>
int _x;
int _i;
void main() {
_x = 2;
_i = 1;
while (_i < 10 || _i == 10){
_x = _x + 2 * 10;
_i = _i + 1;
};
_x = _x - 10;
}