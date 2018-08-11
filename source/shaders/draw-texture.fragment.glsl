#version 330 core

in vec2 texture_coord;
out vec3 color;
uniform sampler2D texture;

void main() {
  // color = vec3(0.4, 0.2, 0.1);
  color = texture2D(texture, texture_coord).rgb;
}
