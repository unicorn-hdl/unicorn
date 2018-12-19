#!/bin/bash
echo ====================================================================================================================================================
echo -e "\n"
echo "                                                     Here is out automated testing per feature                                                    "
echo -e "\n"
echo ====================================================================================================================================================
echo -e "\n"
echo ----------------------
echo Testing Comments 
echo ----------------------

./run.sh comments/test1 c.c
rm comments/test1
./run.sh comments/test2 c.c
rm comments/test2
./run.sh comments/test3 c.c
rm comments/test3

echo -e "\n"
echo ---------------------
echo Testing Buses Datatype
echo ---------------------

./run.sh creatingBuses/test1 c.c
rm creatingBuses/test1
./run.sh creatingBuses/test2 c.c
rm creatingBuses/test2
echo ============================================================ this should give an error ===========================================================
./run.sh creatingBuses/test3 c.c
rm creatingBuses/test3
./run.sh creatingBuses/test4 c.c
rm creatingBuses/test4
./run.sh creatingBuses/test5 c.c
rm creatingBuses/test5
./run.sh creatingBuses/test6 c.c
rm creatingBuses/test6

echo -e "\n"
echo ----------------------
echo Testing EOF Terminators 
echo ----------------------

./run.sh EOFTerminators/test1 c.c 
rm EOFTerminators/test1 
./run.sh EOFTerminators/test2 c.c 
rm EOFTerminators/test2 

echo -e "\n"
echo -----------------------
echo Testing Built-in Gates
echo -----------------------

./run.sh evaluatingGates/test1 c.c 
rm evaluatingGates/test1 
./run.sh evaluatingGates/test2 c.c 
rm evaluatingGates/test2 
./run.sh evaluatingGates/test3 c.c 
rm evaluatingGates/test3 
./run.sh evaluatingGates/test4 c.c 
rm evaluatingGates/test4
./run.sh evaluatingGates/test5 c.c 
rm evaluatingGates/test5 
./run.sh evaluatingGates/test6 c.c 
rm evaluatingGates/test6
./run.sh evaluatingGates/test7 c.c 
rm evaluatingGates/test7 
./run.sh evaluatingGates/test8 c.c 
rm evaluatingGates/test8
echo ========================================================= This should give an error ==============================================================
./run.sh evaluatingGates/test9 c.c 
rm evaluatingGates/test9
echo ===========================================================This should give an error ==============================================================
./run.sh evaluatingGates/test10 c.c 
rm evaluatingGates/test10
echo ============================================================This should give an error ============================================================


echo -e "\n"
echo ---------------------
echo Testing Indexing
echo ----------------------


./run.sh indexing/test1 c.c 
rm indexing/test1 
./run.sh indexing/test2 c.c 
rm indexing/test2 
./run.sh indexing/test3 c.c 
rm indexing/test3 
./run.sh indexing/test4 c.c 
rm indexing/test4 
./run.sh indexing/test5 c.c 
rm indexing/test5
./run.sh indexing/test6 c.c 
rm indexing/test6
echo =============================================================== This should give an error ========================================================
./run.sh indexing/test7 c.c 
echo ================================================================This should give an error ========================================================
rm indexing/test7 
./run.sh indexing/test8 c.c 
rm indexing/test8
./run.sh indexing/test9 c.c 
rm indexing/test9 


echo -e "\n"
echo ---------------------------
echo Testing Reserved Words
echo --------------------------


./run.sh keywords/test1 c.c 
rm keywords/test1 
echo =================================================================This should give an error =======================================================
./run.sh keywords/test2 c.c 
rm keywords/test2 
echo =================================================================This should give an error =======================================================
./run.sh keywords/test3 c.c 
rm keywords/test3
./run.sh keywords/test4 c.c 
rm keywords/test4
echo ==================================================================This should give an error ====================================================== 
 ./run.sh keywords/test5 c.c 
rm keywords/test5 
echo ==================================================================This should give an error=======================================================
./run.sh keywords/test6 c.c 
rm keywords/test6
echo ==================================================================This should give an error ====================================================== 
 ./run.sh keywords/test7 c.c 
rm keywords/test7
echo ==================================================================This should give an error ======================================================
./run.sh keywords/test8 c.c 
rm keywords/test8
 ./run.sh keywords/test9 c.c 
rm keywords/test9
echo ===================================================================This should give an error ======================================================



echo -e "\n"
echo -------------------------
echo Testing Main in Module 
echo ------------------------

./run.sh Main/test1 c.c 
rm Main/test1
echo ===================================================================This should give an error ======================================================
 ./run.sh Main/test2 c.c 
rm Main/test2


echo -e "\n"
echo -----------------------
echo Testing overloading and Re-assignement
echo -----------------------

./run.sh overloading/test1 c.c 
rm overloading/test1
echo ==================================================================== This should give an error ====================================================
 ./run.sh overloading/test2 c.c 
rm overloading/test2
echo ===================================================================== This should give an error ===================================================


echo -e "\n"
echo -----------------------
echo Testing the print function
echo -----------------------
./run.sh printFunc/test1 c.c 
rm printFunc/test1
 ./run.sh printFunc/test2 c.c 
rm printFunc/test2
echo =======================================================================This should give an error ==================================================

echo -e "\n"
echo -----------------------
echo Testing registers 
echo ----------------------

./run.sh registers/test1 c.c 
rm registers/test1
 ./run.sh registers/test2 c.c 
rm registers/test2
./run.sh registers/test3 c.c 
rm registers/test3
 ./run.sh registers/test4 c.c 
rm registers/test4
echo ========================================================================This should give an error ==================================================
./run.sh registers/test5 c.c 
rm registers/test5

echo -e "\n"
echo -----------------------
echo Testing Sample Program: Generic Adder
echo -----------------------
./run.sh programs/genAdder2 c.c 
rm programs/genAdder2

echo -e "\n"
echo -----------------------
echo Testing Sample Program: ALU 
echo -----------------------
./run.sh programs/alu c.c 
rm programs/alu 

echo -----------------------
echo Testing gate Precedence
echo -----------------------
./run.sh gatePrecedence/test1 c.c 
rm gatePrecedence/test1 
./run.sh gatePrecedence/test2 c.c 
rm gatePrecedence/test2 
./run.sh gatePrecedence/test3 c.c 
rm gatePrecedence/test3 
./run.sh gatePrecedence/test4 c.c 
rm gatePrecedence/test4 

echo -e "\n"
echo -e "\n"
echo ====================================================================================================================================================
echo "                                                                END OF TESTING!                                                                 "
echo -e "\n"
echo -e "\n" 
