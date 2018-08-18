#version 330 core
layout(location = 0) in vec2 vertex;
layout(location = 1) in vec4 color;
uniform mat4 movement;
out vec4 color_fragment;
void main() {
  gl_Position = movement * vec4(vertex, 0, 1);
  color_fragment = color;
}
