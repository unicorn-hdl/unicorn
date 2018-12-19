#include <stdio.h>

void tick();

extern int sel_0;
extern int sel_1;
extern int sel_2;

int main(){
	printf("ALU inputs are\nA: 00111111(63))\nB: 11000100(-60)\n\n");

	printf("Add(00000011)\n");
	tick();

	printf("\nSub(01111011)\n");
	sel_2 = 0;
	sel_1 = 0;
	sel_0 = 1;
	tick();

	printf("\nLogical And(00000100)\n");
	sel_2 = 1;
	sel_1 = 0;
	sel_0 = 0;
	tick();

	printf("\nLogical Or(11111111)\n");
	sel_2 = 1;
	sel_1 = 0;
	sel_0 = 1;
	tick();

	printf("\nLogical Xor(11111011)\n");
	sel_2 = 1;
	sel_1 = 1;
	sel_0 = 0;
	tick();

	printf("\nNot A(11000000)\n");
	sel_2 = 1;
	sel_1 = 1;
	sel_0 = 1;
	tick();
}

