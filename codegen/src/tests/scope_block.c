// exit code: 6
int main() {
  int x = 3;
  int y;
  {
    int x = 2;
    y = x;
  }
  return x * y;
}
