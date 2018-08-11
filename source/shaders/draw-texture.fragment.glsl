#version 330 core

in vec2 texture_coord;
out vec3 color;
uniform sampler2D texture;

void main() {
  color = vec3(1,1,1); //texture2D(texture, texture_coord);
}
