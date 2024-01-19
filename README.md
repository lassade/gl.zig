# Zig OpenGL Binding Generator

Fork of [`zig-opengl`](https://github.com/MasterQ32/zig-opengl.git) project, it was modified to work like [`vulkan-zig`](https://github.com/Snektron/vulkan-zig).

You only need to download the [`gl.zig`](https://raw.githubusercontent.com/lassade/gl.zig/main/gl.zig) file.

## Usage
```zig
// api level will be a mix between GLES 3.1 and GL 4.3
const api_level = gl.gles31.intersect(gl.gl43);

// define what functions to load
pub const Loader = gl.LoaderWrapper(api_level.intersect(.{
    .getString = true,
    // .enable = true,
    // .debugMessageCallback = true,
    .getIntegerv = true,
    .getStringi = true,
    .frontFace = true,
    .depthRangef = true,
    .genVertexArrays = true,
    .enableVertexAttribArray = true,
    .vertexAttribPointer = true,
    .vertexAttribDivisor = true,
    .deleteVertexArrays = true,
    .deleteProgram = true,
    .deleteBuffers = true,
    .genBuffers = true,
    .bufferData = true,
    .createShader = true,
    .shaderSource = true,
    .compileShader = true,
    .getShaderiv = true,
    .getShaderInfoLog = true,
    .createProgram = true,
    .attachShader = true,
    .linkProgram = true,
    .getProgramiv = true,
    .getProgramInfoLog = true,
    .deleteShader = true,
    .uniform1i = true,
    .getUniformLocation = true,
    .genTextures = true,
    .texParameteri = true,
    .texImage2D = true,
    .compressedTexImage2D = true,
    .genFramebuffers = true,
    .framebufferTexture2D = true,
    .drawBuffers = true,
    .readBuffer = true,
    .deleteFramebuffers = true,
    .deleteTextures = true,
    .enable = true,
    .polygonOffset = true,
    .disable = true,
    .bindFramebuffer = true,
    .viewport = true,
    .clearColor = true,
    .clear = true,
    .bindVertexArray = true,
    .bufferSubData = true,
    .useProgram = true,
    .depthFunc = true,
    .cullFace = true,
    .uniform4fv = true,
    .activeTexture = true,
    .bindTexture = true,
    .drawElementsInstanced = true,
    .bindBuffer = true,
}));

// define a function loader
// on windows for instance, the opengl functions are splited in opengl32.dll and wgl.dll
// but on android everything can be found under egl.so
const PfnLoader = struct {
    // ...
    pub fn getProcAddress(self: *const @This(), name: [*:0]const u8) ?*const fn () callconv(.C) isize {
        // ...
    }
};

var gll = core.GLRenderer.Loader.init(); // zero inited fn pointers (not required)
try gll.load(PfnLoader{ ... });
```
