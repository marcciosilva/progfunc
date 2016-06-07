#include <stdio.h>
int _low;
int _high;
int _mid;
int _val;
int _i;
int _size;
int _nums[10];
void main() {
_size = 10;
for (_i=1;_i <= _size;_i++ ){
scanf ("%d", &_val);
_nums[_i - 1] = _val;
};
scanf ("%d", &_val);
_low = 0;
_high = _size;
while (_low < _high || _low == _high){
_mid = _low + _high / 2;
if (_nums[_mid - 1] < _val){
_low = _mid + 1;
}else{
_high = _mid - 1;
};
};
printf ("%d\n",_low);
}