
int main() {
  // %entry:
  int x = 50;
  if (x >= 50) { // br %xxx, %then_0, %else_0
    // %then_0:
    if (x >= 100) { // br %yyy, %then_1, %else_1
      // %then_1:
      x = 2;

      // jump %endif_1
    } else {
      // %else_1:
      x = 1;

      // jump %endif_1
    }
    // %endif_1:
    // jump %endif_0
  } else {
    // %else_0:
    x = -1;

    // jump %endif_0
  }

  // %endif_0:
  int a = 0;
  return 0;
}