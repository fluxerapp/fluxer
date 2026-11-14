// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BackSide,
	BufferAttribute,
	BufferGeometry,
	Color,
	DataTexture,
	DirectionalLight,
	DoubleSide,
	Group,
	HalfFloatType,
	LinearFilter,
	Mesh,
	MeshBasicMaterial,
	MeshPhysicalMaterial,
	type MeshPhysicalMaterialParameters,
	NoToneMapping,
	PerspectiveCamera,
	PlaneGeometry,
	PMREMGenerator,
	RawShaderMaterial,
	Scene,
	SphereGeometry,
	Vector2,
	Vector3,
	Vector4,
	WebGLRenderer,
	WebGLRenderTarget,
} from 'three';

export interface PlutoniumAtomScene {
	setActive(active: boolean): void;
	destroy(): void;
}

interface MountOptions {
	canvasClassName: string;
	active: boolean;
}

type ProfilePoint = [number, number, number, number, number];
type Strip = Array<ProfilePoint>;
type PathSample = [number, number, number, number];

interface Disposable {
	dispose(): void;
}

interface Bounds {
	x0: number;
	x1: number;
	y0: number;
	y1: number;
}

const TAU = Math.PI * 2;
const deg = (d: number): number => (d * Math.PI) / 180;
const smooth = (x: number, a: number, b: number): number => {
	const t = Math.min(Math.max((x - a) / (b - a), 0), 1);
	return t * t * (3 - 2 * t);
};

const K = 0.8273;
const RING = {
	a: 56 * K,
	b: 23 * K,
	half: (17.5 * K) / 2,
	depth: 10,
	bevel: 3.5,
	lifts: [0, 2.5, 5],
	turns: [90, 30, 150],
};
const CORE = {r: 18.4 * K, depth: 12, bevel: 18.4 * K * 0.4, lift: 10, hot: [-0.2, 0.25], soft: [0.35, 1.2]};
const IRIS = {from: '#4fb8d8', to: '#d462b0', phase: 2.1, shade: 0.22};
const SIDE = 0.82;
const FRAME = 128;
const DISTANCE = 1400;
const EDGE = {units: (5 * 2048) / 768 / 16, margin: 0.5, color: new Vector4(46 / 255, 34 / 255, 104 / 255, 0.92)};
const SPIN = TAU / 90;
const BEAD = {r: 3.1, periods: [9.5, 11.5, 13.5], phases: [0.4, 2.6, 4.6]};
const FADE_MS = 650;
const PROBE_FRAMES = 30;
const MAX_FRAME_COST_MS = 12;

const DOME: ReadonlyArray<[number, string]> = [
	[-90, '#2a2a30'],
	[-10, '#55555c'],
	[10, '#b4b4ba'],
	[45, '#e6e6ea'],
	[90, '#ffffff'],
];
const PANELS: ReadonlyArray<[number, number, number, number, number, string, number]> = [
	[-30, 50, 50, 30, 0.8, '#ffffff', 1.8],
	[40, 20, 20, 40, 1, '#ffffff', 0.8],
	[-75, 10, 18, 70, 1, '#ff5fd0', 1.4],
	[75, 10, 18, 70, 1, '#4fe0ff', 1.4],
];
const LIGHTS: ReadonlyArray<[string, number, number, number, number]> = [
	['#ffffff', 2.2, -500, 800, 900],
	['#ffffff', 0.6, 700, 200, 600],
	['#ff6fd8', 1.2, -1200, 300, -600],
	['#5fe4ff', 1.2, 1200, 300, -600],
];

const VERTEX = `precision highp float;
attribute vec3 position;
varying vec2 vUv;
void main() {
  vUv = position.xy * 0.5 + 0.5;
  gl_Position = vec4(position.xy, 0.0, 1.0);
}`;

const FRAGMENT = `precision highp float;
uniform sampler2D map;
uniform vec2 texel;
uniform float radius;
uniform vec4 edge;
varying vec2 vUv;
vec3 neutral(vec3 color) {
  color *= 1.05;
  float x = min(color.r, min(color.g, color.b));
  float offset = x < 0.08 ? x - 6.25 * x * x : 0.04;
  color -= offset;
  float peak = max(color.r, max(color.g, color.b));
  if (peak < 0.76) return color;
  float d = 0.24;
  float newPeak = 1.0 - d * d / (peak + d - 0.76);
  color *= newPeak / peak;
  float g = 1.0 - 1.0 / (0.15 * (peak - newPeak) + 1.0);
  return mix(color, vec3(newPeak), g);
}
vec3 srgb(vec3 c) {
  c = clamp(c, 0.0, 1.0);
  return mix(c * 12.92, 1.055 * pow(c, vec3(1.0 / 2.4)) - 0.055, step(0.0031308, c));
}
void main() {
  vec4 c = texture2D(map, vUv);
  float m = 0.0;
  for (int i = 0; i < 24; i++) {
    float a = float(i) * 0.2617994;
    m = max(m, texture2D(map, vUv + vec2(cos(a), sin(a)) * radius * texel).a);
  }
  vec3 color = c.a > 0.0 ? srgb(neutral(c.rgb / c.a)) : vec3(0.0);
  float e = edge.a * clamp(m, 0.0, 1.0) * (1.0 - c.a);
  gl_FragColor = vec4(color * c.a + edge.rgb * e, c.a + e);
}`;

function feather(soft: number): DataTexture {
	const S = 64;
	const data = new Uint8Array(S * S * 4);
	const edge = (t: number): number => smooth(Math.min(t, 1 - t), 0, soft / 2);
	for (let y = 0; y < S; y++) {
		for (let x = 0; x < S; x++) {
			const k = (y * S + x) * 4;
			const value = Math.round(edge((x + 0.5) / S) * edge((y + 0.5) / S) * 255);
			data[k] = value;
			data[k + 1] = value;
			data[k + 2] = value;
			data[k + 3] = 255;
		}
	}
	const tex = new DataTexture(data, S, S);
	tex.magFilter = LinearFilter;
	tex.minFilter = LinearFilter;
	tex.needsUpdate = true;
	return tex;
}

function studio(): Scene {
	const env = new Scene();
	const R = 60;
	const geo = new SphereGeometry(R, 64, 32);
	const pos = geo.attributes.position;
	const colors = new Float32Array(pos.count * 3);
	const stops = DOME.map(([e, c]) => [e, new Color(c)] as const);
	const tmp = new Color();
	for (let i = 0; i < pos.count; i++) {
		const e = (Math.asin(Math.min(Math.max(pos.getY(i) / R, -1), 1)) * 180) / Math.PI;
		let j = 0;
		while (j < stops.length - 2 && e > stops[j + 1][0]) j++;
		const [e0, c0] = stops[j];
		const [e1, c1] = stops[j + 1];
		tmp
			.copy(c0)
			.lerp(c1, Math.min(Math.max((e - e0) / (e1 - e0), 0), 1))
			.toArray(colors, i * 3);
	}
	geo.setAttribute('color', new BufferAttribute(colors, 3));
	env.add(new Mesh(geo, new MeshBasicMaterial({vertexColors: true, side: BackSide})));
	for (const [az, el, w, h, soft, color, intensity] of PANELS) {
		const d = R * 0.8;
		const material = new MeshBasicMaterial({
			color: new Color(color).multiplyScalar(intensity),
			side: DoubleSide,
			alphaMap: feather(soft),
			transparent: true,
			depthWrite: false,
		});
		const panel = new Mesh(new PlaneGeometry(2 * d * Math.tan(deg(w) / 2), 2 * d * Math.tan(deg(h) / 2)), material);
		panel.position.set(
			Math.sin(deg(az)) * Math.cos(deg(el)) * d,
			Math.sin(deg(el)) * d,
			Math.cos(deg(az)) * Math.cos(deg(el)) * d,
		);
		panel.lookAt(0, 0, 0);
		env.add(panel);
	}
	return env;
}

function profile(hw: number, hz: number, r: number, segments: number): Array<Strip> {
	const arc = (cx: number, cz: number, from: number, to: number): Strip =>
		Array.from({length: segments + 1}, (_, i) => {
			const t = deg(from + ((to - from) * i) / segments);
			return [cx + r * Math.cos(t), cz + r * Math.sin(t), Math.cos(t), Math.sin(t), SIDE];
		});
	const w = hw - r;
	const z = hz - r;
	return [
		[
			[-w, hz, 0, 1, 1],
			[w, hz, 0, 1, 1],
		],
		[...arc(w, z, 90, 0), ...arc(w, -z, 0, -90)],
		[
			[w, -hz, 0, -1, 1],
			[-w, -hz, 0, -1, 1],
		],
		[...arc(-w, -z, -90, -180), ...arc(-w, z, 180, 90)],
	];
}

function sweep(
	steps: number,
	path: (t: number) => PathSample,
	strips: Array<Strip>,
	paint: (x: number, y: number, c: Color) => Color,
): BufferGeometry {
	const count = strips.reduce((n, s) => n + s.length * steps, 0);
	const position = new Float32Array(count * 3);
	const normal = new Float32Array(count * 3);
	const color = new Float32Array(count * 3);
	const index: Array<number> = [];
	const c = new Color();
	let v = 0;
	for (const strip of strips) {
		const base = v;
		for (let i = 0; i < steps; i++) {
			const [px, py, nx, ny] = path((i / steps) * TAU);
			for (const [n, z, nn, nz, shade] of strip) {
				const x = px + nx * n;
				const y = py + ny * n;
				position.set([x, y, z], v * 3);
				normal.set([nx * nn, ny * nn, nz], v * 3);
				paint(x, y, c)
					.multiplyScalar(shade)
					.toArray(color, v * 3);
				v++;
			}
		}
		const L = strip.length;
		for (let i = 0; i < steps; i++) {
			const a = base + i * L;
			const b = base + ((i + 1) % steps) * L;
			for (let k = 0; k < L - 1; k++) index.push(a + k, a + k + 1, b + k, b + k, a + k + 1, b + k + 1);
		}
	}
	const geo = new BufferGeometry();
	geo.setAttribute('position', new BufferAttribute(position, 3));
	geo.setAttribute('normal', new BufferAttribute(normal, 3));
	geo.setAttribute('color', new BufferAttribute(color, 3));
	geo.setIndex(index);
	return geo;
}

function ringGeometry(j: number): BufferGeometry {
	const {a, b, half, depth, bevel} = RING;
	const from = new Color(IRIS.from);
	const to = new Color(IRIS.to);
	const path = (t: number): PathSample => {
		const nx = b * Math.cos(t);
		const ny = a * Math.sin(t);
		const l = Math.hypot(nx, ny);
		return [a * Math.cos(t), b * Math.sin(t), nx / l, ny / l];
	};
	const paint = (x: number, y: number, c: Color): Color =>
		c.copy(from).lerp(to, 0.5 + 0.5 * Math.sin(Math.atan2(y / b, x / a) + j * IRIS.phase));
	return sweep(256, path, profile(half, depth / 2 + bevel, bevel, 8), paint);
}

function coreGeometry(): BufferGeometry {
	const {r, depth, bevel, hot, soft} = CORE;
	const inner = new Color('#fffbf2');
	const outer = new Color('#f7d9a8');
	const strips = profile(r, depth / 2 + bevel, bevel, 10);
	strips.pop();
	strips[0][0][0] = 0;
	strips[2][1][0] = 0;
	const path = (t: number): PathSample => [0, 0, Math.cos(t), Math.sin(t)];
	const paint = (x: number, y: number, c: Color): Color =>
		c.copy(inner).lerp(outer, smooth(Math.hypot(x - hot[0] * r, y - hot[1] * r) / r, soft[0], soft[1]));
	return sweep(96, path, strips, paint);
}

function clay(extra: MeshPhysicalMaterialParameters): MeshPhysicalMaterial {
	return new MeshPhysicalMaterial({
		color: 0xffffff,
		vertexColors: true,
		roughness: 0.3,
		metalness: 0.1,
		clearcoat: 0.5,
		clearcoatRoughness: 0.25,
		...extra,
	});
}

function ringMaterial(): MeshPhysicalMaterial {
	const material = clay({iridescence: 0.5, iridescenceIOR: 1.5, iridescenceThicknessRange: [250, 450]});
	const half = (129.5 * K) / 2;
	material.onBeforeCompile = (shader) => {
		shader.vertexShader = shader.vertexShader.replace(
			'#include <project_vertex>',
			`#include <project_vertex>\nvColor.rgb *= 1.0 - ${IRIS.shade.toFixed(3)} * smoothstep(-0.2, 1.0, -(modelMatrix * vec4(transformed, 1.0)).y / ${half.toFixed(3)});`,
		);
	};
	material.customProgramCacheKey = () => 'atom-ring';
	return material;
}

function nextTask(): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, 0));
}

export function mountPlutoniumAtom(
	host: HTMLElement,
	img: HTMLImageElement,
	{canvasClassName, active: initiallyActive}: MountOptions,
): PlutoniumAtomScene {
	const canvas = document.createElement('canvas');
	canvas.setAttribute('aria-hidden', 'true');
	canvas.className = canvasClassName;
	canvas.dataset.flx = 'premium.plutonium-page.hero.atom-canvas';
	let alive = true;
	let active = initiallyActive;
	let raf = 0;
	let visible = false;
	let shown = false;
	let renderer: WebGLRenderer | null = null;
	let target: WebGLRenderTarget | null = null;
	let post: Mesh<BufferGeometry, RawShaderMaterial> | null = null;
	let coreMat: MeshPhysicalMaterial | null = null;
	let rest: Bounds | null = null;
	let observer: IntersectionObserver | null = null;
	let resizer: ResizeObserver | null = null;
	const owned: Array<Disposable> = [];
	const scene = new Scene();
	const atom = new Group();
	const spinner = new Group();
	const beads: Array<Mesh> = [];
	const camera = new PerspectiveCamera((2 * Math.atan(FRAME / 2 / DISTANCE) * 180) / Math.PI, 1, 10, 5000);
	camera.position.set(0, 0, DISTANCE);
	const postCamera = new PerspectiveCamera();
	let clock = 0;
	let spin = 0;
	let last = 0;
	let sampled = 0;
	let cost = 0;

	function pose(t: number, dt: number): void {
		const e = smooth(t, 0, 4);
		spin -= dt * e * SPIN;
		spinner.rotation.z = spin;
		atom.rotation.set(
			deg(-8 + e * 5 * Math.sin((TAU * t) / 19 + 1)),
			deg(-8 + e * 7 * Math.sin((TAU * t) / 15)),
			0,
			'YXZ',
		);
		const grow = smooth(t, 0.5, 2.5);
		beads.forEach((bead, j) => {
			const angle = BEAD.phases[j] - (TAU * t) / BEAD.periods[j];
			bead.position.x = RING.a * Math.cos(angle);
			bead.position.y = RING.b * Math.sin(angle);
			bead.scale.setScalar(BEAD.r * Math.max(grow, 1e-3));
			bead.visible = grow > 0;
		});
		if (coreMat) coreMat.emissiveIntensity = 0.75 + e * 0.06 * Math.sin((TAU * t) / 7);
	}

	function measure(): Bounds {
		pose(0, 0);
		scene.updateMatrixWorld(true);
		camera.clearViewOffset();
		camera.aspect = 1;
		camera.updateProjectionMatrix();
		camera.updateMatrixWorld();
		const box = [Infinity, -Infinity, Infinity, -Infinity];
		const v = new Vector3();
		atom.traverse((o) => {
			if (!(o instanceof Mesh)) return;
			const p = o.geometry.attributes.position;
			for (let i = 0; i < p.count; i++) {
				v.fromBufferAttribute(p, i).applyMatrix4(o.matrixWorld).project(camera);
				const x = (v.x * FRAME) / 2;
				const y = (v.y * FRAME) / 2;
				box[0] = Math.min(box[0], x);
				box[1] = Math.max(box[1], x);
				box[2] = Math.min(box[2], y);
				box[3] = Math.max(box[3], y);
			}
		});
		const pad = EDGE.units + EDGE.margin;
		return {x0: box[0] - pad, x1: box[1] + pad, y0: box[2] - pad, y1: box[3] + pad};
	}

	function layout(): void {
		if (!renderer || !target || !post || !rest) return;
		const w = img.offsetWidth;
		const h = img.offsetHeight;
		if (!w || !h) return;
		const unitsW = rest.x1 - rest.x0;
		const side = Math.max(w, h) * 1.2;
		const dpr = Math.min(window.devicePixelRatio || 1, 2);
		const px = Math.round(side * dpr);
		const ppu = (w / unitsW) * dpr;
		canvas.style.width = `${side}px`;
		canvas.style.height = `${side}px`;
		canvas.style.left = `${img.offsetLeft + (w - side) / 2}px`;
		canvas.style.top = `${img.offsetTop + (h - side) / 2}px`;
		renderer.setSize(px, px, false);
		target.setSize(px, px);
		const half = px / ppu / 2;
		const cx = (rest.x0 + rest.x1) / 2;
		const cy = (rest.y0 + rest.y1) / 2;
		const full = FRAME * ppu;
		camera.aspect = 1;
		camera.setViewOffset(full, full, (cx - half + FRAME / 2) * ppu, (FRAME / 2 - (cy + half)) * ppu, px, px);
		post.material.uniforms.texel.value.set(1 / px, 1 / px);
		post.material.uniforms.radius.value = EDGE.units * ppu;
	}

	function draw(): void {
		if (!renderer || !target || !post) return;
		renderer.setRenderTarget(target);
		renderer.clear();
		renderer.render(scene, camera);
		renderer.setRenderTarget(null);
		renderer.clear();
		renderer.render(post, postCamera);
	}

	function frame(now: number): void {
		raf = requestAnimationFrame(frame);
		if (last && now - last < 15) return;
		const dt = last ? Math.min((now - last) / 1000, 0.05) : 0;
		last = now;
		clock += dt;
		pose(clock, dt);
		const start = performance.now();
		draw();
		if (sampled < PROBE_FRAMES) {
			cost += performance.now() - start;
			sampled++;
			if (sampled === PROBE_FRAMES && cost / sampled > MAX_FRAME_COST_MS) destroy();
		}
	}

	function run(): void {
		const go = alive && shown && visible && active;
		if (go && !raf) {
			last = 0;
			raf = requestAnimationFrame(frame);
		} else if (!go && raf) {
			cancelAnimationFrame(raf);
			raf = 0;
		}
	}

	function release(): void {
		for (const thing of owned) thing.dispose();
		owned.length = 0;
		target?.dispose();
		renderer?.dispose();
		renderer?.forceContextLoss();
		renderer = null;
		target = null;
		post = null;
		canvas.remove();
	}

	function destroy(): void {
		if (!alive) return;
		alive = false;
		run();
		observer?.disconnect();
		resizer?.disconnect();
		canvas.removeEventListener('webglcontextlost', destroy);
		delete host.dataset.live;
		if (shown && host.isConnected) setTimeout(release, FADE_MS);
		else release();
	}

	async function build(): Promise<void> {
		const gl = new WebGLRenderer({
			canvas,
			alpha: true,
			antialias: false,
			powerPreference: 'low-power',
			failIfMajorPerformanceCaveat: true,
		});
		renderer = gl;
		gl.setClearColor(0x000000, 0);
		gl.toneMapping = NoToneMapping;
		canvas.addEventListener('webglcontextlost', destroy);
		const rt = new WebGLRenderTarget(1, 1, {samples: 4, type: HalfFloatType});
		target = rt;
		await nextTask();
		if (!alive) return;
		const pmrem = new PMREMGenerator(gl);
		const env = studio();
		const environment = pmrem.fromScene(env, 0.04).texture;
		scene.environment = environment;
		scene.environmentIntensity = 1.1;
		owned.push(environment);
		env.traverse((o) => {
			if (!(o instanceof Mesh)) return;
			o.geometry.dispose();
			o.material.alphaMap?.dispose();
			o.material.dispose();
		});
		pmrem.dispose();
		for (const [color, intensity, x, y, z] of LIGHTS) {
			const light = new DirectionalLight(color, intensity);
			light.position.set(x, y, z);
			scene.add(light);
		}
		await nextTask();
		if (!alive) return;
		const ringMat = ringMaterial();
		const beadGeo = new SphereGeometry(1, 24, 16);
		const beadMat = new MeshPhysicalMaterial({
			color: '#fffbf2',
			roughness: 0.3,
			metalness: 0.1,
			clearcoat: 0.5,
			clearcoatRoughness: 0.25,
			emissive: new Color('#ffe6bf'),
			emissiveIntensity: 0.7,
		});
		owned.push(ringMat, beadGeo, beadMat);
		RING.turns.forEach((turn, j) => {
			const geo = ringGeometry(j);
			owned.push(geo);
			const pivot = new Group();
			pivot.position.z = RING.lifts[j];
			pivot.rotation.z = deg(turn);
			const mesh = new Mesh(geo, ringMat);
			const bead = new Mesh(beadGeo, beadMat);
			bead.position.z = RING.depth / 2 + RING.bevel + BEAD.r * 0.55;
			bead.visible = false;
			pivot.add(mesh, bead);
			spinner.add(pivot);
			beads.push(bead);
		});
		await nextTask();
		if (!alive) return;
		const coreGeo = coreGeometry();
		coreMat = clay({emissive: new Color('#ffe6bf'), emissiveIntensity: 0.75});
		owned.push(coreGeo, coreMat);
		const core = new Mesh(coreGeo, coreMat);
		core.position.z = CORE.lift;
		atom.add(spinner, core);
		scene.add(atom);
		const postGeo = new BufferGeometry();
		postGeo.setAttribute('position', new BufferAttribute(new Float32Array([-1, -1, 0, 3, -1, 0, -1, 3, 0]), 3));
		const postMat = new RawShaderMaterial({
			vertexShader: VERTEX,
			fragmentShader: FRAGMENT,
			uniforms: {
				map: {value: rt.texture},
				texel: {value: new Vector2(1, 1)},
				radius: {value: 1},
				edge: {value: EDGE.color},
			},
			depthTest: false,
			depthWrite: false,
			premultipliedAlpha: true,
		});
		owned.push(postGeo, postMat);
		const postMesh = new Mesh(postGeo, postMat);
		postMesh.frustumCulled = false;
		post = postMesh;
		rest = measure();
		host.append(canvas);
		layout();
		await gl.compileAsync(scene, camera);
		if (!alive) return;
		await gl.compileAsync(postMesh, postCamera);
		if (!alive) return;
		pose(0, 0);
		draw();
		requestAnimationFrame(() => {
			if (!alive) return;
			shown = true;
			host.dataset.live = 'true';
			run();
		});
		resizer = new ResizeObserver(() => {
			layout();
			if (!raf && alive) draw();
		});
		resizer.observe(host);
		observer = new IntersectionObserver((entries) => {
			visible = entries[entries.length - 1].isIntersecting;
			run();
		});
		observer.observe(host);
	}

	build().catch(() => destroy());

	return {
		setActive(next) {
			active = next;
			run();
		},
		destroy,
	};
}
