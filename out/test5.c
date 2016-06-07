#include <stdio.h>
int _x;
int _b;
void main() {
scanf ("%d", &_x);
_b = _x < 10;
if (_b){
_x = _x + 1;
}else{
_x = _x - 10;
};
}