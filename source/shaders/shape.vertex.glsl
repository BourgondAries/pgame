#version 330 core
layout(location = 0) in vec3 vertexPosition_modelspace;
uniform mat4 movement;
void main() {
  gl_Position = movement * vec4(vertexPosition_modelspace, 1);
}
