{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Shaders.Source
  (
    vertex
  , fragment
  ) where

import Relude
import Misc.QuasiQuotes

header :: ByteString
header = [s|
#version 410 core
|]

vertex :: ByteString
vertex = header <> [s|
#define M_PI 3.1415926535897932384626433832795

in vec4 vp;
uniform float time;

out vec3 color;
out vec4 position;

void main() {

    float t =  mod(time*0.000001, 360);
    float y =  mod(time*0.000001, 360);
    float u =  mod(time*0.001, 360);

    gl_Position = vec4(vp.xyz, 2)
    * mat4(
        vec4( cos(t), sin(t), 0, 0),
        vec4(-sin(t), cos(t), 0, 0),
        vec4(      0,      0, 1, 0),
        vec4(      0,      0, 0, 1)
      )
    * mat4(
        vec4(      1,      0,      0, 0),
        vec4(      0, cos(y), sin(y), 0),
        vec4(      0,-sin(y), cos(y), 0),
        vec4(      0,      0,      0, 1)
      )
    * mat4(
        vec4( cos(u),      0,-sin(u), 0),
        vec4(      0,      1,      0, 0),
        vec4( sin(u),      0, cos(u), 0),
        vec4(      0,      0,      0, 1)
      )
    ;

    position = vp;

    color = vec3(
      gl_Position.z*sin(time/1000)+0.2, 
      gl_Position.y*cos(time/1000)+0.2, 
      gl_Position.x*sin(time/1000)+0.2
    );
}
|]

fragment :: ByteString
fragment = header <> [s|
in vec3 color;
in vec4 position;
out vec4 frag_colour;
void main() {
  frag_colour = 
    vec4(0.9-gl_FragCoord.zzz, 1)
  ;
}
|]