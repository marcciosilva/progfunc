#include <stdio.h>
int _x;
int _i;
void main() {
_x = 2;
for (_i=1;_i <= 10;_i++ ){
_x = _x + 2 * 10;
};
_x = _x - 10;
}