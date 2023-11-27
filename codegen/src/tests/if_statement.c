// exit code: 100
int main() {
  int x = 10;
  if (x < 0)
    return 1;
  else if (x == 10)
    x = 25;
  else
    return 2;

  if (1)
    x = x * 2;
  else
    return 3;

  if (0)
    return 4;
  else
    x = x * 2;

  return x;
}
