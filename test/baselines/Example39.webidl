[Exposed=Window]
interface NetworkFetcher {
  void get(optional boolean? areWeThereYet = false);
};

[Exposed=Window]
interface Node {
  readonly attribute DOMString? namespaceURI;
  readonly attribute Node? parentNode;
  // ...
};