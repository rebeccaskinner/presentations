#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <unistd.h>

typedef struct {
    int width;
    int height;
}cfg_t;

int default_config(cfg_t*);
void show_config(cfg_t*);
char** lines(char*, int*);

int main(int argc, char** argv) {
    cfg_t config;
    if(-1 == default_config(&config)) {
        goto err;
    }
    show_config(&config);
err:
    return -1;
}

int default_config(cfg_t* config) {
    struct ttysize ts;
    if (NULL == config) { return -1; }
    if (-1 == ioctl(STDIN_FILENO, TIOCGSIZE, &ts)) {return -1;}
    config->width = ts.ts_cols;
    config->height = ts.ts_lines;
    return 0;
}

void show_config(cfg_t* config) {
    if(!config) {
        printf("NULL\n");
        return;
    }
    printf("config { width = %d; height = %d }\n", config->width, config->height);
}

char** lines(char* input, int* num_lines) {
    char* current_char = input;
    if(NULL == input || NULL == num_lines) { return NULL; }
    while(*current_char) {

    }
}
