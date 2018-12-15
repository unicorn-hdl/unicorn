#include <stdio.h>

void tick();

extern char a_0;
extern char a_1;
extern char d_0;

int main(){
	printf("%d\n", d_0);
	tick();
	printf("%d\n", d_0);
	tick();
	printf("%d\n", d_0);
	tick();
	printf("%d\n", d_0);
	tick();
	printf("%d\n", d_0);
	tick();
	printf("%d\n", d_0);
	tick();
}
