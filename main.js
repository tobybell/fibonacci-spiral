function create_shader(gl, type, source_code) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source_code);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const info = gl.getShaderInfoLog(shader);
    throw `Could not compile WebGL program. \n\n${info}`;
  }
  return shader;
}

const canvas = document.createElement('canvas');

canvas.width = 600;
canvas.height = 600;
document.body.appendChild(canvas);
Object.assign(document.body.style, {
  width: '100vw',
  height: '100vh',
  margin: '0',
  padding: '0',
  background: '#000',
  display: 'flex',
  flexFlow: 'row nowrap',
  justifyContent: 'center',
  alignItems: 'center',
});

const gl = canvas.getContext('webgl2');
gl.clearColor(0, 0, 0, 1);

const vs = create_shader(gl, gl.VERTEX_SHADER, `#version 300 es
  #define pi 3.1415926535897932
  uniform mat3 u_model;
  void main() {
    gl_PointSize = 4.0;
    float i = float(gl_VertexID);
    float phi = (1. + sqrt(5.)) / 2.;
    float xi = i / phi;
    float yi = i / 1000.;
    float th = 2. * pi * xi;
    float cphi = 2. * yi - 1.;
    float sphi = sqrt(1. - cphi * cphi);
    vec3 pos = u_model * vec3(cos(th) * sphi, sin(th) * sphi, cphi);
    gl_Position = vec4(pos * vec3(2., 2., 1.), 4. + pos.z);
  }
`);

const fs = create_shader(gl, gl.FRAGMENT_SHADER, `#version 300 es
  out mediump vec4 frag_color;
  void main() {
    mediump float rz = (gl_FragCoord.z * 2. - 1.) / gl_FragCoord.w;
    mediump float bright = (1. - rz) / 2.;
    frag_color = vec4(bright, bright, bright, 1);
  }
`);

const program = gl.createProgram();
gl.attachShader(program, vs);
gl.attachShader(program, fs);
gl.linkProgram(program);
if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
  const info = gl.getProgramInfoLog(program);
  throw `Could not compile WebGL program. \n\n${info}`;
}

const u_model = gl.getUniformLocation(program, 'u_model');

let R = new Float32Array([1, 0, 0, 0, 1, 0, 0, 0, 1]);

function rot_z(R, th) {
  const c = Math.cos(th);
  const s = Math.sin(th);
  return new Float32Array([
    c * R[0] - s * R[1],
    s * R[0] + c * R[1],
    R[2],
    c * R[3] - s * R[4],
    s * R[3] + c * R[4],
    R[5],
    c * R[6] - s * R[7],
    s * R[6] + c * R[7],
    R[8]]);
}

function rot_x(R, th) {
  const c = Math.cos(th);
  const s = Math.sin(th);
  return new Float32Array([
    R[0],
    c * R[1] - s * R[2],
    s * R[1] + c * R[2],
    R[3],
    c * R[4] - s * R[5],
    s * R[4] + c * R[5],
    R[6],
    c * R[7] - s * R[8],
    s * R[7] + c * R[8]]);
}

function rot_y(R, th) {
  const c = Math.cos(th);
  const s = Math.sin(th);
  return new Float32Array([
    s * R[2] + c * R[0],
    R[1],
    c * R[2] - s * R[0],
    s * R[5] + c * R[3],
    R[4],
    c * R[5] - s * R[3],
    s * R[8] + c * R[6],
    R[7],
    c * R[8] - s * R[6]]);
}

function draw() {
  R = rot_y(R, 0.001);
  gl.clear(gl.COLOR_BUFFER_BIT);
  gl.useProgram(program);
  gl.uniformMatrix3fv(u_model, 0, R);
  gl.drawArrays(gl.POINTS, 0, 1000);
  requestAnimationFrame(draw);
}

canvas.addEventListener('wheel', e => {
  const dx = e.deltaX;
  const dy = e.deltaY;
  R = rot_y(R, e.deltaX / 32);
  R = rot_x(R, e.deltaY / 32);
  e.preventDefault();
}, false);

draw();
