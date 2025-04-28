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
  overflow: 'hidden',
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

function make_program(vertex_source, fragment_source) {
  const vs = create_shader(gl, gl.VERTEX_SHADER, vertex_source);
  const fs = create_shader(gl, gl.FRAGMENT_SHADER, fragment_source);

  const program = gl.createProgram();
  gl.attachShader(program, vs);
  gl.attachShader(program, fs);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    const info = gl.getProgramInfoLog(program);
    throw `Could not compile WebGL program. \n\n${info}`;
  }

  return program;
}

const program = make_program(`#version 300 es
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
`, `#version 300 es
  out mediump vec4 frag_color;
  void main() {
    mediump float rz = (gl_FragCoord.z * 2. - 1.) / gl_FragCoord.w;
    mediump float bright = (1. - rz) / 2.;
    frag_color = vec4(bright, bright, bright, 1);
  }
`);

const point_program = make_program(`#version 300 es
  uniform vec3 u_pos;
  uniform mat3 u_model;
  void main() {
    gl_PointSize = 4.0;
    vec3 pos = u_model * u_pos;
    gl_Position = vec4(pos * vec3(2., 2., 1.), 4. + pos.z);
  }
`, `#version 300 es
  out mediump vec4 frag_color;
  void main() {
    frag_color = vec4(1, 0, 1, 1);
  }
`);

const u_model = gl.getUniformLocation(program, 'u_model');
const u_point_pos = gl.getUniformLocation(point_program, 'u_pos');
const u_point_model = gl.getUniformLocation(point_program, 'u_model');

let R = new Float32Array([1, 0, 0, 0, 1, 0, 0, 0, 1]);
let point_pos = new Float32Array([0, 1, 0]);

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

  gl.useProgram(point_program);
  gl.uniform3fv(u_point_pos, point_pos);
  gl.uniformMatrix3fv(u_point_model, 0, R);
  gl.drawArrays(gl.POINTS, 0, 1);

  requestAnimationFrame(draw);
}

canvas.addEventListener('wheel', e => {
  const dx = e.deltaX;
  const dy = e.deltaY;
  R = rot_y(R, e.deltaX / 32);
  R = rot_x(R, e.deltaY / 32);
  e.preventDefault();
}, false);

const fov = Math.atan(2 / 4);

function mul(A, x, y, z) {
  const r = new Float32Array(3);
  r[0] = A[0] * x + A[1] * y + A[2] * z;
  r[1] = A[3] * x + A[4] * y + A[5] * z;
  r[2] = A[6] * x + A[7] * y + A[8] * z;
  return r;
}

canvas.addEventListener('click', e => {
  const rect = canvas.getBoundingClientRect();
  const x = (e.clientX - rect.left) * 2 / rect.width - 1;
  const y = 1 - (e.clientY - rect.top) * 2 / rect.height;
  const z = 2;
  const dd = x * x + y * y + z * z;
  const d = Math.sqrt(dd);

  const sphere_dist = 4;
  const sds = sphere_dist * sphere_dist;
  const radius = 1;
  const radius_squared = radius * radius;

  const cx = y / d * sphere_dist;
  const cy = -x / d * sphere_dist;
  const cn = Math.sqrt(cx * cx + cy * cy);
  const t = (2 * sphere_dist * z - Math.sqrt(64*z*z - 4 * dd * (sds - radius_squared))) / (2 * dd);

  point_pos = mul(R, x * t, y * t, z * t - 4);
});

draw();
