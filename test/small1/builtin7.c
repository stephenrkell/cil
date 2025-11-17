typedef float v4sf __attribute__ ((vector_size (16)));
typedef int v4si __attribute__ ((vector_size (16)));

int main() {
    v4si b;
    v4sf d = __builtin_convertvector(b, v4sf);
    return 0;
}
