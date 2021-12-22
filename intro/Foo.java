// example with class without package

// compiles and runs
// javac Foo.java
// java Foo

// usage from clojure REPL
// CLASSPATH="./" clojure
// (Foo.)
// (Foo. 1 2)
// (def f1 (Foo.))
// (def f2 (Foo. 1 2))
// (Foo/f_static 1 10)
// (.f f1 1 2)
// (.f f2 1 2)

// using jar
// jar cf Foo.jar Foo.class
// jar cf Foo.jar Foo.java
// java -jar Foo.jar

public class Foo {

    public static void main(String[] args) {
        java.lang.System.out.println("in main");
        System.out.println("in main");

    }

    private int x, y;
    public int X, Y;

    public int Xf() {
        return x+X;
    }

    public int Yf() {
        return y+Y;
    }

    public Foo() {
        x = 0;
        y = 0;

        X = 10;
        Y = 10;
    }

    public Foo(int x, int y) {
        this.x = x;
        this.y = y;

        this.X = 2*x;
        this.Y = 2*y;
    }

    public static int f_static(int x, int y) {
        return x+y;
    }
    public int f(int x, int y) {
        return x+y;
    }

    public class InnerFoo {
        public static int data = 1234;
        public InnerFoo() {
            data = 100;
        }
        public InnerFoo(int v) {
            data = v;
        }
    }
}

