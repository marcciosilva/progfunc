#include <stdio.h>
int _i;
int _j;
int _tmp;
int _ind;
int _size;
int _nums[10];
void main() {
_size = 10;
for (_i=1;_i <= _size;_i++ ){
scanf ("%d", &_tmp);
_nums[_i - 1] = _tmp;
};
for (_i=2;_i <= _size;_i++ ){
_ind = _nums[_i - 1];
_j = _i;
while (1 < _j && _ind < _nums[_j - 1 - 1]){
_nums[_j - 1] = _nums[_j - 1 - 1];
_j = _j - 1;
};
_nums[_j - 1] = _ind;
};
for (_i=1;_i <= _size;_i++ ){
printf ("%d\n",_nums[_i - 1]);
};
}