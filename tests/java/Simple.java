package test;

import java.util.List;

public class Simple {
    public static int factorial(int n) {
        if (n == 0) {
            return 1;
        }
        return n * factorial(n - 1);
    }

    public static void main(String[] args) {
        String msg = "hello";
        int x = 42;
        boolean b = true;
        System.out.println(factorial(x));
    }
}
