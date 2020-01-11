[Exposed=Window]
interface Dimensions {
  attribute unsigned long width;
  attribute unsigned long height;
};

[Exposed=Window]
interface Button {

  // An operation that takes no arguments and returns a boolean.
  boolean isMouseOver();

  // Overloaded operations.
  void setDimensions(Dimensions size);
  void setDimensions(unsigned long width, unsigned long height);
};