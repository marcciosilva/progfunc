#include <stdio.h>
int _i;
int _j;
int _tmp;
int _size;
int _nums[10];
void main() {
_size = 10;
for (_i=1;_i <= _size;_i++){
scanf ("%d", &_tmp);
_nums[_i - 1] = _tmp;
};
for (_i=0;_i <= (_size - 2);_i++){
for (_j=2;_j <= (_size - _i);_j++){
if ((_nums[_j - 1] < _nums[(_j - 1) - 1])){
_tmp = _nums[(_j - 1) - 1];
_nums[(_j - 1) - 1] = _nums[_j - 1];
_nums[_j - 1] = _tmp;
}else{
};
};
};
for (_i=1;_i <= _size;_i++){
printf ("%d\n",_nums[_i - 1]);
};
}