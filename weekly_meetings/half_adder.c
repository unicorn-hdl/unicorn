# include <stdio.h>
# include <stdlib.h>

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3c9d5d2... Seqential circuit example in UNI-corn and c-like language
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
//Should pass in a pointer if it's going to be an array
int *half_adder(int wire_a, int wire_b) {

    int *sum_carry = (int*) malloc(sizeof *sum_carry);
    //not a good idea since would have to reallocate and the free for every call
<<<<<<< HEAD
<<<<<<< HEAD
=======
int *half_adder(int wire_a, int wire_b) {

    int *sum_carry = (int*) malloc(sizeof *sum_carry);
>>>>>>> e3b269e... added new folder
=======
>>>>>>> 3c9d5d2... Seqential circuit example in UNI-corn and c-like language
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
    int sum_wire = wire_a ^ wire_b;
    int carry_wire = wire_a & wire_b;

    *sum_carry = sum_wire;
    sum_carry++;
    *sum_carry = carry_wire;
    sum_carry++;
    *sum_carry = '\0';

    return sum_carry;

}

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3c9d5d2... Seqential circuit example in UNI-corn and c-like language
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
//For tick pass in four pointers, 
// ray of input
// ray of input output values
// 
// 

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> e3b269e... added new folder
=======
>>>>>>> 3c9d5d2... Seqential circuit example in UNI-corn and c-like language
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
int main(int argc, char *argv[]) {

    //Half adder
    int x_wire = 1;
    int y_wire = 1;

    //get the sum
    int sum_wire = x_wire ^ y_wire;
    //get the carry
    int carry_wire = x_wire & y_wire;

    printf("The sum is: %d\nThe carry is: %d\n", sum_wire, carry_wire);

    int *temp = half_adder(0, 1);

    temp -= 2;

    sum_wire = *temp;
    temp++;
    carry_wire = *temp;

    printf("The sum is: %d\nThe carry is: %d\n", sum_wire, carry_wire);

    return 0;

}