// exit code: 4
int main() {
  int x = 0;
  for (int i = 0; i < 10; i = i + 1) {
    x = x + 2;
    if (x > 3) {
      break;
    }
  }

  return x;
}
