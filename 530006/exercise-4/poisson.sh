#!/bin/sh

awk 'BEGIN {
lambda=3;
for(i=0;i<=10;i++){
if (i==0) {
factorial=1;
} else {
factorial=1;
for (j=1;j<=i;j++){
factorial*=j;
}
}
f=lambda^i*exp(-lambda)/factorial;
print i, f;
}
exit;
}' > poisson.txt
