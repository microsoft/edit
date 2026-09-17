// Covers what C++ adds on top of C. See c.c for the constructs shared with it.

#include <iostream>

// Numbers and literals
0b1010'1100;
42ull;
1z;
nullptr;
u8"utf-8";
wchar_t wide = L'w';
char16_t utf16 = u'x';
char8_t utf8 = u8'y';

// Namespaces, classes, and access specifiers
namespace my_space {

enum class State { START, STOP };

class Animal {
public:
    Animal() : age_(0), alive_(true) {}
    virtual ~Animal() = default;
    virtual void speak() const = 0;

protected:
    bool alive_;

private:
    int age_;
};

class Dog final : public Animal {
public:
    void speak() const override {
        std::cout << "Woof!" << std::endl;
    }
};

struct Config {
    explicit Config(int v) : value_(v) {}
    friend bool operator==(const Config &, const Config &) = default;
    Config &self() { return *this; }

    mutable int cache_;
    int value_;
};

} // namespace my_space

using namespace my_space;

// Templates and concepts
template <typename T>
concept Addable = requires(T a, T b) {
    a + b;
};

template <typename T>
constexpr auto twice(T value) noexcept -> decltype(value + value) {
    return value + value;
}

consteval int magic() { return 42; }
constinit int counter = 0;

// Casts, allocation, and exceptions
void run() try {
    auto *dog = new Dog();
    delete dog;

    auto n = static_cast<unsigned char>(0);
    auto *p = reinterpret_cast<char *>(&n);
    const_cast<char *>(p);
    dynamic_cast<Animal *>(dog);
    typeid(n);

    throw std::runtime_error("oops");
} catch (const std::exception &e) {
    // handle error
}

// Coroutines
std::future<int> compute() {
    int result = co_await async_work();
    co_yield result;
    co_return result;
}
