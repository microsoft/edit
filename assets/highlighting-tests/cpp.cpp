// Comments
// Single-line comment

/*
 * Multi-line
 * comment
 */

#include <iostream>
#include <memory>
#define CONST_VAL 100

// Numbers (including C++14 digit separators)
42;
3.14;
.5;
1e10;
1.5e-3;
0xff;
0xFF;
0b1010'1100;
1'000'000;
42ull;
3.14f;

// Constants
true;
false;
NULL;
nullptr;

// Strings and Characters
'c';
'\t';
"double quotes with escape: \" \n \t \\";

// Control flow keywords
int main() {
    if (true) {
    } else if (false) {
    } else {
    }

    for (int i = 0; i < 10; ++i) {
        if (i == 5) continue;
        if (i == 8) break;
    }

    while (false) { }
    do { } while (false);

    switch (1) {
        case 1: break;
        default: break;
    }

    try {
        throw std::runtime_error("oops");
    } catch (const std::exception& e) {
        // handle error
    }

    return 0;
}

// Modern C++ keywords and features
template <typename T>
concept Addable = requires(T a, T b) {
    a + b;
};

class Animal {
private:
    int age;
protected:
    bool is_alive;
public:
    Animal() : age(0), is_alive(true) {}
    virtual ~Animal() = default;
    virtual void speak() const = 0;
};

class Dog final : public Animal {
public:
    void speak() const override {
        std::cout << "Woof!" << std::endl;
    }
};

constexpr int get_magic_number() {
    return 42;
}

// Other keywords
namespace my_space {
    enum class State { START, STOP };
    
    inline void helper() {
        auto val = get_magic_number();
        decltype(val) val_copy = val;
        
        Dog* dog = new Dog();
        delete dog;
    }
}

using namespace my_space;
export module my_module;

// Function calls
std::cout << "hello\n";
sizeof(int);
