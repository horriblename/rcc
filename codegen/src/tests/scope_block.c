// exit code: 3
int main() {
  int x = 3;
  { int x = x + 2; }
  return x;
}
