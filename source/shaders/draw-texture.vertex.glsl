#version 330 core

layout(location = 0) in vec2 vertex;
layout(location = 1) in vec2 vertex_uv;

out vec2 texture_coord;

uniform mat4 movement;

void main(){

    gl_Position =  movement * vec4(vertex,0,1);
    texture_coord = vertex_uv;
}
