#include <iostream>

#define PI 3.14159

using namespace std;

class Shape {
  public:
    virtual double area() const = 0;
    virtual double circ() const = 0;
};

class Rectangle : public Shape {
  public:
    Rectangle (double height, double width) {
        _height = height;
        _width  = width;
    }

    Rectangle (const Rectangle &r) {
        _height = r.height();
        _width  = r.width();
    }

    double area() const { return (_height * _width); }
    double circ() const { return (2.0 * (_height + _width)); }
    double height() const { return _height; }
    double width() const { return _width; }

  private:
    double _height, _width;
};

class Circle : public Shape {
  public:
    Circle (double radius) {
        _radius = radius;
    }

    Circle (const Circle &c) {
        _radius = c.radius();
    }

    double area() const { return (PI * _radius * _radius); }
    double circ() const { return (2.0 * PI * _radius); }
    double radius() const { return _radius; }

  private:
    double _radius;
};

int main(int argc, char *argv[]) {
    auto r1 = Rectangle(2.0, 4.0);
    cout << "Area of r1: " << r1.area() << endl;
    cout << "Circumfrence of r1: " << r1.circ() << endl;
    auto r2 = Rectangle(r1);
    cout << "Area of r2: " << r2.area() << endl;
    cout << "Circumfrence of r2: " << r2.circ() << endl;
    auto c1 = Circle(2.0);
    cout << "Area of c1: " << c1.area() << endl;
    cout << "Circumfrence of c1: " << c1.circ() << endl;
    auto c2 = Circle(c1);
    cout << "Area of c2: " << c2.area() << endl;
    cout << "Circumfrence of c2: " << c2.circ() << endl;
}

