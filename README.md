## Lagger
Lagger is a statically typed, imperative language.

## Examples
```
int questionOfLife() {
    return 42;
}

string hello(){
    return "Bonjour! :)";
}

boolean isEven(int number){
    return number % 2 == 0;
}

int main() {
    println(questionOfLife());
    println(hello());
    println(isEven(42));
    return 0;
}
```

## Description
Lagger allows user to define their own functions, and use basic statements such as ```if-else```, ```while```, ```break```, ```continue```. One can pass arguments by reference or by value of 3 types: ```int```, ```boolean``` and ```string```. You can also ```println``` all 3 types and define your own runtime uncatchable errors. Lagger provides a lot of flexibility with type shadowing and redefining functions.


## How to run
Lagger is an interpreted language. To run the program you need to use ```make``` to build an interpreter and then ```./lagger <PATH>```  

## Package content
- Source code ```Common/ Evaluator/ Lagger/ Typechecker/ Interpreter.hs  Main.hs```
- Exemplary programs ```examples/``` with a script to run them ```run_examples.sh```
- ```Makefile```
- ```Lagger.cf``` file that defines language grammar
