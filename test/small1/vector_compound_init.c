typedef long long __m64 __attribute__((__vector_size__(16), __aligned__(8)));

int main() {
    __m64 foo = { 0, 1 };
    return 0;
}
