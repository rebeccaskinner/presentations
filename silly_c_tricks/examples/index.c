#include <stdio.h>
#include <string.h>

int main()
{
    char const* s = "hello, world";
    for (int i = 0; i < strlen(s); i++) {
        printf("%c", i[s]);
    }
    printf("\n");
}
