#ifdef BUILDING_VERTEX_SHADER

in vec3 in_pos;
in vec2 in_uvs;

uniform mat4 uni_mvp;

out vec2 var_uvs;

void main()
{
  gl_Position = uni_mvp * vec4(in_pos, 1.0);
  var_uvs = in_uvs;
}

#endif

#ifdef BUILDING_FRAGMENT_SHADER

in vec2 var_uvs;

uniform sampler2D uni_tex;

out vec4 out_fragColor;

void main()
{
  out_fragColor = texture(uni_tex, var_uvs);
}

#endif
