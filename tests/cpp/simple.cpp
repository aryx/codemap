#include <iostream>
#include <string>

enum Color { Red, Green, Blue };

int factorial(int n) {
    if (n == 0) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    std::string msg = "hello";
    int x = 42;
    bool b = true;
    std::cout << factorial(x) << std::endl;
    return 0;
}
