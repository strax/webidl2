[Exposed=Window]
interface Base {
  // Operations cannot be overloaded across partial interfaces and mixins
  void unique();
};

partial interface Base {
  void unique(short num);
};

interface mixin Extension {
  void unique(string str);
};
Base includes Extension;
Base includes Unknown;

// WebGL

interface mixin WebGL2RenderingContextBase
{
  // WebGL1:
  void bufferData(GLenum target, GLsizeiptr size, GLenum usage);
  void bufferData(GLenum target, ArrayBuffer? srcData, GLenum usage);
  void bufferData(GLenum target, ArrayBufferView srcData, GLenum usage);
  // WebGL2:
  void bufferData(GLenum target, ArrayBufferView srcData, GLenum usage, GLuint srcOffset,
                  optional GLuint length = 0);
};

interface mixin WebGLRenderingContextBase
{
  void bufferData(GLenum target, GLsizeiptr size, GLenum usage);
  void bufferData(GLenum target, ArrayBuffer? data, GLenum usage);
  void bufferData(GLenum target, ArrayBufferView data, GLenum usage);
};

[Exposed=(Window,Worker)]
interface WebGL2RenderingContext
{
};
WebGL2RenderingContext includes WebGLRenderingContextBase;
WebGL2RenderingContext includes WebGL2RenderingContextBase;
