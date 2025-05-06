// exit code: 2
int foo(int bar);

int main() { return foo(2); }

int bar(int a) { return a + 2; }

int foo(int a) { return bar(a) + 1; }
