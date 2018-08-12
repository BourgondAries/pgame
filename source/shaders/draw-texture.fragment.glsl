#version 330 core

in vec2 texture_coord;

out vec4 color;

uniform sampler2D texture;

void main() {
  color = texture2D(texture, texture_coord);
}
