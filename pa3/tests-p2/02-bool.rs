fn main() {
      !true;
      true || false;
      true && (false || true);
      true && (false || !(!false && !false));
      () && (false || !(!false && !false));
      true && (false || !(!1 && !false));
      true && (99 || !(!false && !false));
      true && (false || !(!false && !(1 + 2)));
}
