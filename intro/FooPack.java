// example with class within package

// compiles but does not run
// javac Foo.java
// java mylib.Foo
//
// compiles and runs
// javac -d . FooPack.java
// java mylib.FooPack
package mylib;

public class FooPack {

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

    public FooPack() {
        x = 0;
        y = 0;

        X = 10;
        Y = 10;
    }

    public FooPack(int x, int y) {
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
}

