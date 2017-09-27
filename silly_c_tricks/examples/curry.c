#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

char*
four_args(char* prefix, char* suffix, char* msg, uint32_t cnt)
{
    size_t msg_len = strlen(msg);
    size_t middle_len = cnt * msg_len;
    char* middle = malloc(middle_len);
    for (uint32_t i = 0; i < cnt; i++) {
        int offset = i * msg_len;
        memcpy(middle + offset, msg, msg_len);
    }
    char* new_msg;
    asprintf(&new_msg, "%s%s%s", prefix, middle, suffix);
    free(middle);
    return new_msg;
}

int
main()
{
    char* s = four_args("hello ", "middle", " world", 5);
    printf("%s\n", s);
    free(s);
    return 0;
}
