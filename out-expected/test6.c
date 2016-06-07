#include <stdio.h>
int _i;
int _j;
int _b1;
int _b2;
int _a[5][5];
void main() {
_b1 = 1;
_b2 = _b1;
for (_i=1;_i <= 5;_i++){
_b1 = ! _b1;
for (_j=1;_j <= 5;_j++){
_b2 = ! _b2;
_a[_i - 1][_j - 1] = ! (_b1 && _b2);
};
};
}