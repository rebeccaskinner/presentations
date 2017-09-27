#include <stdio.h>
#include <stdlib.h>

int (*getFunc(int x))(int)
{
    int (*f)(int) = ({
            int __tmp(int y) {
                return x + y;
            }
            __tmp;
        });
    return f;
}

int main()
{
    int (*f)(int) = getFunc(2);
    int result = f(1);
    printf("%d\n", result);
}
