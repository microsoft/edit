// Line comment
/* Block comment
   spanning lines */

#include <string>
#include <vector>
#include <memory>
#pragma once

namespace geometry {

template <typename T>
class Point {
public:
    Point(T x, T y) : x_(x), y_(y) {}
    T x() const { return x_; }

private:
    T x_;
    T y_;
};

enum class Color { Red, Green, Blue };

constexpr double kPi = 3.14159;

void numbers() {
    int a = 42;
    auto b = 0xffUL;
    auto c = 0b1010;
    double d = 1.5e-3;
    float f = 3.14f;
    auto z = 100uz;
}

void strings() {
    std::string s = "double \" quote \n escape";
    auto raw = R"(raw "string" \n no escape)";
    char ch = 'a';
    char nl = '\n';
}

int control(int n) {
    for (int i = 0; i < n; ++i) {
        if (i == 5) continue;
        try {
            throw std::runtime_error("oops");
        } catch (const std::exception& e) {
        }
    }
    return n;
}

} // namespace geometry

int main() {
    auto p = std::make_unique<geometry::Point<double>>(1.0, 2.0);
    bool flag = true;
    std::vector<int> v{1, 2, 3};
    return flag ? 0 : nullptr != p.get();
}
