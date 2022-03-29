public class A {

    public int af() { return 2; }
    public int af1(int x) { return x+2; }
    public int a = 1;
    static public int as;
    static public int afs() { return 20; }
    static public int afs1(int x) { return x+20; }
    
    public A self() {
        return this;
    }

    public int cnt  = 0;
    public A self_param(int i) {
        cnt += i;
        return this;
    }

}

