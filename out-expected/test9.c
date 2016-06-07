#include <stdio.h>
int _i;
int _j;
int _num;
int _size;
int _bin[10];
void main() {
_size = 10;
scanf ("%d", &_num);
_i = 0;
while ((! (_num == 0) && (_i < _size))){
_i = (_i + 1);
if (((_num % 2) == 0)){
_bin[_i - 1] = 0;
}else{
_bin[_i - 1] = 1;
};
_num = (_num / 2);
};
for (_j=_i;_j <= _size;_j++){
_bin[_j - 1] = - 1;
};
for (_i=0;_i <= (_size - 1);_i++){
printf ("%d\n",_bin[(_size - _i) - 1]);
};
}