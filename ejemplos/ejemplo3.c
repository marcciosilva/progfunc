#include <stdio.h>
int _i;
int _x;
void main() {
scanf ("%d", &_x);
if (_x < 1){
printf ("%d\n",0);
}else{
for (_i=1;_i <= _x;_i++ ){
printf ("%d\n",_i);
};
};
}