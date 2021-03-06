// Extracted from http://dev.w3.org/2006/webapi/WebIDL/ on 2011-05-06
      interface Point {
        attribute float x;
        attribute float y;
      };

      typedef sequence<Point> PointSequence;

      interface Rect {
        attribute Point topleft;
        attribute Point bottomright;
      };

  interface Widget {

    readonly attribute Rect bounds;

    boolean pointWithinBounds(Point p);
    boolean allPointsWithinBounds(PointSequence ps);
  };

  typedef [Clamp] octet _value;