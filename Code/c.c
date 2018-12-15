#include <stdio.h>

void tick();

extern int a_1;
extern int a_0;

int main(){
	a_1 = 1;	
	tick();
	a_0 = 1;
	a_1 = 0;
	tick();
}
