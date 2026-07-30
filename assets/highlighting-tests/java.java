// Line comment
/* Block comment */
/** Javadoc @param x @return y */

package com.example.highlighting;

import java.util.List;
import java.util.ArrayList;
import static java.lang.Math.max;
import java.util.*;

const int CONST = 0;
goto label;

open module com.example {
    requires transitive java.base;
    exports com.example.pkg to other.module;
    opens com.example.internal;
    provides Greeter with Dog;
    uses Greeter;
}

enum Color { RED, GREEN, BLUE }

record Point(int x, int y) {
    Point {
        if (x < 0) throw new IllegalArgumentException();
    }
}

sealed interface Shape permits Circle {}
non-sealed class Circle implements Shape {}

@FunctionalInterface
interface Greeter<T extends Comparable<T>> {
    String greet(T name);
    default boolean ok() { return true; }
}

@Deprecated
public abstract strictfp class Animal implements Comparable<Animal> {
    // Numbers
    static final int I = 42, HEX = 0xFF, BIN = 0b1010, OCT = 077, US = 1_000_000;
    static final long L = 0xCAFE_BABEL;
    static final double D = 3.14, E = 1.5e-3, HALF = .5;
    static final float F = 3.14f;
    private byte b;
    protected short s;
    private volatile transient int flags;

    // Strings, char, escapes, text block
    String str = "quotes \" \n \t \\ \u00e9";
    char c = '\n';
    String block = """
        text block
        with "quotes"
        """;

    Animal() { super(); }

    native void nativeMethod();

    abstract String speak() throws Exception;

    @Override
    public synchronized int compareTo(Animal o) {
        return this.flags - o.flags;
    }
}

final class Dog extends Animal {
    String speak() { return "bark"; }

    // Constants, control flow
    void flow() {
        boolean t = true, f = false;
        Object n = null;

        if (t) {
        } else if (f) {
        } else {
        }

        for (int i = 0; i < 10; i++) {
            if (i == 5) continue;
            if (i == 8) break;
        }
        for (int x : new int[] { 1, 2, 3 }) {}
        while (f) {}
        do {} while (f);

        try {
            throw new RuntimeException("oops");
        } catch (RuntimeException e) {
        } finally {
        }

        assert t : "message";
        return;
    }

    // var, switch expression, yield, pattern guards, instanceof, lambda
    <U> U run(Object obj) {
        var list = new ArrayList<String>();
        int r = switch (list.size()) {
            case 1 -> 10;
            case 2 -> { yield 20; }
            default -> 0;
        };
        String desc = switch (obj) {
            case Integer i when i > 0 -> "pos";
            case String p -> "str";
            default -> "other";
        };
        if (obj instanceof String p) {
            System.out.println(p.length());
        }
        Greeter<String> g = name -> "Hi " + name;
        return null;
    }
}

// Function calls
class Main {
    public static void main(String[] args) {
        System.out.println("hello");
        int v = Integer.parseInt("42");
        max(1, 2);
        new Dog().speak();
    }
}
