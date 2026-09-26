// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	WEB_CAMERA_EFFECT_MASK_BAND_EPSILON,
	WEB_CAMERA_EFFECT_MASK_EDGE_SOFTNESS,
	WEB_CAMERA_EFFECT_MASK_GUIDE_RANGE_FALLOFF,
	WEB_CAMERA_EFFECT_MASK_GUIDE_SPATIAL_FALLOFF,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectMask';
import {SEG_INPUT_EDGE} from '@app/features/voice/utils/camera-effects/WebSelfieSegmenter';

const VERTEX_SHADER_SOURCE = `#version 300 es
precision highp float;

out vec2 textureCoordinates;

void main() {
	vec2 positions[3] = vec2[3](
		vec2(-1.0, -1.0),
		vec2(3.0, -1.0),
		vec2(-1.0, 3.0)
	);
	vec2 position = positions[gl_VertexID];
	gl_Position = vec4(position, 0.0, 1.0);
	textureCoordinates = vec2((position.x + 1.0) * 0.5, (1.0 - position.y) * 0.5);
}
`;

const FRAGMENT_SHADER_SOURCE = `#version 300 es
precision highp float;

uniform sampler2D sourceTexture;
uniform sampler2D maskTexture;
in vec2 textureCoordinates;
out vec4 outputColour;

float luminance(vec3 colour) {
	return dot(colour, vec3(0.2126, 0.7152, 0.0722));
}

float refinedMask(vec2 coordinates, float guide) {
	vec2 maskSize = vec2(${SEG_INPUT_EDGE}.0);
	vec2 maskPosition = coordinates * maskSize - vec2(0.5);
	vec2 base = round(maskPosition);
	float maskTotal = 0.0;
	float weightTotal = 0.0;
	for (int y = -1; y <= 1; y += 1) {
		for (int x = -1; x <= 1; x += 1) {
			vec2 offset = vec2(float(x), float(y));
			vec2 sampleCoordinates = clamp((base + offset + vec2(0.5)) / maskSize, vec2(0.0), vec2(1.0));
			float sampleGuide = luminance(textureLod(sourceTexture, sampleCoordinates, 0.0).rgb);
			vec2 sampleDistance = maskPosition - (base + offset);
			float spatialWeight = exp(-float(${WEB_CAMERA_EFFECT_MASK_GUIDE_SPATIAL_FALLOFF}) * dot(sampleDistance, sampleDistance));
			float rangeWeight = exp(-float(${WEB_CAMERA_EFFECT_MASK_GUIDE_RANGE_FALLOFF}) * abs(guide - sampleGuide));
			float weight = spatialWeight * rangeWeight;
			maskTotal += textureLod(maskTexture, sampleCoordinates, 0.0).a * weight;
			weightTotal += weight;
		}
	}
	return maskTotal / max(weightTotal, 0.0001);
}

void main() {
	float guide = luminance(texture(sourceTexture, textureCoordinates).rgb);
	float coarse = textureLod(maskTexture, textureCoordinates, 0.0).a;
	float alpha = step(0.5, coarse);
	if (coarse > ${WEB_CAMERA_EFFECT_MASK_BAND_EPSILON} && coarse < 1.0 - ${WEB_CAMERA_EFFECT_MASK_BAND_EPSILON}) {
		float refined = refinedMask(textureCoordinates, guide);
		float curve = clamp((refined - 0.5) / (2.0 * ${WEB_CAMERA_EFFECT_MASK_EDGE_SOFTNESS}) + 0.5, 0.0, 1.0);
		alpha = curve * curve * (3.0 - 2.0 * curve);
	}
	outputColour = vec4(0.0, 0.0, 0.0, alpha);
}
`;

function createShader(gl: WebGL2RenderingContext, type: number, source: string): WebGLShader {
	const shader = gl.createShader(type);
	if (shader == null) {
		throw new Error('Camera mask refinement could not allocate a WebGL shader');
	}
	gl.shaderSource(shader, source);
	gl.compileShader(shader);
	if (gl.getShaderParameter(shader, gl.COMPILE_STATUS) === true) {
		return shader;
	}
	const diagnostic = gl.getShaderInfoLog(shader) ?? 'no shader diagnostic was provided';
	gl.deleteShader(shader);
	throw new Error(`Camera mask refinement WebGL shader compilation failed: ${diagnostic}`);
}

function createProgram(gl: WebGL2RenderingContext): WebGLProgram {
	const vertexShader = createShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER_SOURCE);
	let fragmentShader: WebGLShader | null = null;
	let program: WebGLProgram | null = null;
	try {
		fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER_SOURCE);
		program = gl.createProgram();
		if (program == null) {
			throw new Error('Camera mask refinement could not allocate a WebGL program');
		}
		gl.attachShader(program, vertexShader);
		gl.attachShader(program, fragmentShader);
		gl.linkProgram(program);
		if (gl.getProgramParameter(program, gl.LINK_STATUS) !== true) {
			const diagnostic = gl.getProgramInfoLog(program) ?? 'no program diagnostic was provided';
			throw new Error(`Camera mask refinement WebGL program linking failed: ${diagnostic}`);
		}
		return program;
	} catch (error) {
		if (program != null) {
			gl.deleteProgram(program);
		}
		throw error;
	} finally {
		gl.deleteShader(vertexShader);
		if (fragmentShader != null) {
			gl.deleteShader(fragmentShader);
		}
	}
}

function requireUniform(gl: WebGL2RenderingContext, program: WebGLProgram, name: string): WebGLUniformLocation {
	const location = gl.getUniformLocation(program, name);
	if (location == null) {
		throw new Error(`Camera mask refinement WebGL uniform is unavailable: ${name}`);
	}
	return location;
}

function createTexture(gl: WebGL2RenderingContext, unit: number): WebGLTexture {
	const texture = gl.createTexture();
	if (texture == null) {
		throw new Error('Camera mask refinement could not allocate a WebGL texture');
	}
	gl.activeTexture(gl.TEXTURE0 + unit);
	gl.bindTexture(gl.TEXTURE_2D, texture);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
	return texture;
}

export class WebCameraEffectWebGLMaskRefiner {
	private width = 0;
	private height = 0;
	private uploadedMaskRevision = -1;
	private disposed = false;

	private constructor(
		private readonly canvas: OffscreenCanvas,
		private readonly gl: WebGL2RenderingContext,
		private readonly program: WebGLProgram,
		private readonly vertexArray: WebGLVertexArrayObject,
		private readonly sourceTexture: WebGLTexture,
		private readonly maskTexture: WebGLTexture,
	) {}

	static create(): WebCameraEffectWebGLMaskRefiner | null {
		const canvas = new OffscreenCanvas(1, 1);
		const gl = canvas.getContext('webgl2', {
			alpha: true,
			antialias: false,
			depth: false,
			premultipliedAlpha: false,
			preserveDrawingBuffer: true,
			stencil: false,
		});
		if (gl == null) {
			return null;
		}
		let program: WebGLProgram | null = null;
		let vertexArray: WebGLVertexArrayObject | null = null;
		let sourceTexture: WebGLTexture | null = null;
		let maskTexture: WebGLTexture | null = null;
		try {
			program = createProgram(gl);
			vertexArray = gl.createVertexArray();
			if (vertexArray == null) {
				throw new Error('Camera mask refinement could not allocate a WebGL vertex array');
			}
			sourceTexture = createTexture(gl, 0);
			maskTexture = createTexture(gl, 1);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, SEG_INPUT_EDGE, SEG_INPUT_EDGE, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
			gl.useProgram(program);
			gl.uniform1i(requireUniform(gl, program, 'sourceTexture'), 0);
			gl.uniform1i(requireUniform(gl, program, 'maskTexture'), 1);
			gl.bindVertexArray(vertexArray);
			gl.disable(gl.BLEND);
			gl.disable(gl.CULL_FACE);
			gl.disable(gl.DEPTH_TEST);
			gl.disable(gl.DITHER);
			return new WebCameraEffectWebGLMaskRefiner(canvas, gl, program, vertexArray, sourceTexture, maskTexture);
		} catch (error) {
			if (sourceTexture != null) gl.deleteTexture(sourceTexture);
			if (maskTexture != null) gl.deleteTexture(maskTexture);
			if (vertexArray != null) gl.deleteVertexArray(vertexArray);
			if (program != null) gl.deleteProgram(program);
			gl.getExtension('WEBGL_lose_context')?.loseContext();
			throw error;
		}
	}

	refine(
		source: VideoFrame,
		mask: OffscreenCanvas,
		maskRevision: number,
		width: number,
		height: number,
	): OffscreenCanvas | null {
		if (this.disposed) {
			throw new Error('Cannot refine a camera mask with a disposed WebGL owner');
		}
		if (this.gl.isContextLost()) {
			return null;
		}
		if (this.width !== width || this.height !== height) {
			this.width = width;
			this.height = height;
			this.canvas.width = width;
			this.canvas.height = height;
			this.gl.viewport(0, 0, width, height);
			this.gl.activeTexture(this.gl.TEXTURE0);
			this.gl.bindTexture(this.gl.TEXTURE_2D, this.sourceTexture);
			this.gl.texImage2D(
				this.gl.TEXTURE_2D,
				0,
				this.gl.RGBA,
				width,
				height,
				0,
				this.gl.RGBA,
				this.gl.UNSIGNED_BYTE,
				null,
			);
		}
		this.gl.pixelStorei(this.gl.UNPACK_FLIP_Y_WEBGL, 0);
		this.gl.pixelStorei(this.gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, 0);
		this.gl.activeTexture(this.gl.TEXTURE0);
		this.gl.bindTexture(this.gl.TEXTURE_2D, this.sourceTexture);
		this.gl.texSubImage2D(this.gl.TEXTURE_2D, 0, 0, 0, this.gl.RGBA, this.gl.UNSIGNED_BYTE, source);
		if (this.uploadedMaskRevision !== maskRevision) {
			this.gl.activeTexture(this.gl.TEXTURE1);
			this.gl.bindTexture(this.gl.TEXTURE_2D, this.maskTexture);
			this.gl.texSubImage2D(this.gl.TEXTURE_2D, 0, 0, 0, this.gl.RGBA, this.gl.UNSIGNED_BYTE, mask);
			this.uploadedMaskRevision = maskRevision;
		}
		this.gl.useProgram(this.program);
		this.gl.bindVertexArray(this.vertexArray);
		this.gl.drawArrays(this.gl.TRIANGLES, 0, 3);
		this.gl.flush();
		return this.canvas;
	}

	dispose(): void {
		if (this.disposed) {
			return;
		}
		this.disposed = true;
		this.gl.deleteTexture(this.sourceTexture);
		this.gl.deleteTexture(this.maskTexture);
		this.gl.deleteVertexArray(this.vertexArray);
		this.gl.deleteProgram(this.program);
		this.gl.getExtension('WEBGL_lose_context')?.loseContext();
	}
}
