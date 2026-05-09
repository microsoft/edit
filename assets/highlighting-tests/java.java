// Comments
// Single-line comment

/*
 * Multi-line
 * comment
 */

package example;

import java.time.Instant;
import java.util.List;

@Deprecated
public class Demo {
    private static final double PI = 3.14;
    private boolean enabled = true;

    // Numbers
    int decimal = 42;
    double fraction = 3.14;
    double leadingDot = .5;
    double scientific = 1.5e-3;
    int hex = 0xff;
    int binary = 0b1010;
    long grouped = 1_000_000L;
    float floatValue = 2.5f;

    // Constants
    boolean yes = true;
    boolean no = false;
    Object nothing = null;

    // Strings and chars
    char letter = 'a';
    char newline = '\n';
    String message = "double quotes with escape: \" \n \t \\";

    public static void main(String[] args) {
        Demo demo = new Demo();
        if (demo.enabled) {
            System.out.println("enabled");
        } else {
            System.out.println("disabled");
        }

        for (int i = 0; i < 10; i++) {
            if (i == 5) {
                continue;
            }
            if (i == 8) {
                break;
            }
        }

        while (false) {
            break;
        }

        switch (args.length) {
            case 0:
                System.out.println("no args");
                break;
            default:
                System.out.println("args");
        }

        try {
            demo.run();
        } catch (IllegalStateException ex) {
            throw ex;
        } finally {
            System.out.println(Instant.now());
        }
    }

    public void run() {
        List<String> names = List.of("Ada", "Grace");
        for (String name : names) {
            System.out.println(greet(name));
        }
    }

    public String greet(String name) {
        return "hello " + name;
    }
}

record Point(int x, int y) {}

sealed interface Shape permits Circle {}

final class Circle implements Shape {}
