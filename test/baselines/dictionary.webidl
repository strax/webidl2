// Extracted from Web IDL editors draft May 31 2011
dictionary PaintOptions {
  DOMString? fillPattern = "black";
  DOMString? strokePattern = null;
  Point position;
  // https://heycam.github.io/webidl/#dfn-optional-argument-default-value allows sequences to default to "[]".
  sequence<long> seq = [];
  // https://heycam.github.io/webidl/#required-dictionary-member
  required long reqSeq;
  Dictionary dict = {};
};

partial dictionary _A {
  long h;
  long d;
};