// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	WEB_CAMERA_EFFECT_MASK_BAND_EPSILON,
	WEB_CAMERA_EFFECT_MASK_BAND_WIDTH,
	WEB_CAMERA_EFFECT_MASK_CORE_GROW_MIN,
	WEB_CAMERA_EFFECT_MASK_CORE_MIN,
	WEB_CAMERA_EFFECT_MASK_EDGE_SOFTNESS,
	WEB_CAMERA_EFFECT_MASK_GUIDE_RANGE_FALLOFF,
	WEB_CAMERA_EFFECT_MASK_GUIDE_SPATIAL_FALLOFF,
	WEB_CAMERA_EFFECT_MASK_HOLE_NEIGHBOUR_MIN,
	WEB_CAMERA_EFFECT_MASK_SPECKLE_NEIGHBOUR_MAX,
	WEB_CAMERA_EFFECT_MASK_TEMPORAL_KEEP_STILL,
	WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_HIGH,
	WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW,
	WEB_CAMERA_EFFECT_MASK_VOID_MAX,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectMask';

const FULLSCREEN_VERTEX = `
struct VertexOutput {
	@builtin(position) position: vec4f,
	@location(0) uv: vec2f,
}

@vertex
fn vertexMain(@builtin(vertex_index) vertexIndex: u32) -> VertexOutput {
	let positions = array<vec2f, 3>(
		vec2f(-1.0, -1.0),
		vec2f(3.0, -1.0),
		vec2f(-1.0, 3.0),
	);
	let position = positions[vertexIndex];
	var output: VertexOutput;
	output.position = vec4f(position, 0.0, 1.0);
	output.uv = vec2f((position.x + 1.0) * 0.5, (1.0 - position.y) * 0.5);
	return output;
}
`;

export const CAMERA_SOURCE_SHADER = `
${FULLSCREEN_VERTEX}

@group(0) @binding(0) var source: texture_external;
@group(0) @binding(1) var sourceSampler: sampler;

@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
	return textureSampleBaseClampToEdge(source, sourceSampler, input.uv);
}
`;

export const CAMERA_PREPROCESS_SHADER = `
${FULLSCREEN_VERTEX}

@group(0) @binding(0) var source: texture_external;
@group(0) @binding(1) var sourceSampler: sampler;
@group(0) @binding(2) var<storage, read_write> tensor: array<f32>;

@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
	let tapOffset = 0.25 / 256.0;
	var colourTotal = textureSampleBaseClampToEdge(source, sourceSampler, input.uv + vec2f(-tapOffset, -tapOffset));
	colourTotal += textureSampleBaseClampToEdge(source, sourceSampler, input.uv + vec2f(tapOffset, -tapOffset));
	colourTotal += textureSampleBaseClampToEdge(source, sourceSampler, input.uv + vec2f(-tapOffset, tapOffset));
	colourTotal += textureSampleBaseClampToEdge(source, sourceSampler, input.uv + vec2f(tapOffset, tapOffset));
	let colour = colourTotal * 0.25;
	let position = vec2u(input.position.xy);
	let index = position.y * 256u + position.x;
	tensor[index] = colour.r;
	tensor[65536u + index] = colour.g;
	tensor[131072u + index] = colour.b;
	return vec4f(0.0);
}
`;

export const CAMERA_BLUR_SHADER = `
${FULLSCREEN_VERTEX}

struct BlurParams {
	direction: vec2f,
	radius: f32,
	padding: f32,
}

@group(0) @binding(0) var source: texture_2d<f32>;
@group(0) @binding(1) var sourceSampler: sampler;
@group(0) @binding(2) var<uniform> params: BlurParams;

@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
	let scale = max(1.0, params.radius * 0.25);
	let nearOffset = params.direction * 1.3846153846 * scale;
	let farOffset = params.direction * 3.2307692308 * scale;
	var colour = textureSample(source, sourceSampler, input.uv) * 0.2270270270;
	colour += textureSample(source, sourceSampler, input.uv + nearOffset) * 0.3162162162;
	colour += textureSample(source, sourceSampler, input.uv - nearOffset) * 0.3162162162;
	colour += textureSample(source, sourceSampler, input.uv + farOffset) * 0.0702702703;
	colour += textureSample(source, sourceSampler, input.uv - farOffset) * 0.0702702703;
	return colour;
}
`;

export const CAMERA_COPY_SHADER = `
${FULLSCREEN_VERTEX}

@group(0) @binding(0) var source: texture_2d<f32>;
@group(0) @binding(1) var sourceSampler: sampler;

@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
	return textureSample(source, sourceSampler, input.uv);
}
`;

export const CAMERA_COVER_SHADER = `
${FULLSCREEN_VERTEX}

struct CoverParams {
	scale: vec2f,
	offset: vec2f,
}

@group(0) @binding(0) var source: texture_2d<f32>;
@group(0) @binding(1) var sourceSampler: sampler;
@group(0) @binding(2) var<uniform> params: CoverParams;

@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
	return textureSample(source, sourceSampler, input.uv * params.scale + params.offset);
}
`;

export const CAMERA_COMPOSITE_SHADER = `
${FULLSCREEN_VERTEX}

@group(0) @binding(0) var foreground: texture_2d<f32>;
@group(0) @binding(1) var background: texture_2d<f32>;
@group(0) @binding(2) var mask: texture_2d<f32>;
@group(0) @binding(3) var linearSampler: sampler;

fn luminance(colour: vec3f) -> f32 {
	return dot(colour, vec3f(0.2126, 0.7152, 0.0722));
}

fn refinedMask(uv: vec2f, guide: f32) -> f32 {
	let maskSize = vec2f(256.0);
	let maskPosition = uv * maskSize - vec2f(0.5);
	let base = round(maskPosition);
	var maskTotal = 0.0;
	var weightTotal = 0.0;
	for (var y: i32 = -1; y <= 1; y += 1) {
		for (var x: i32 = -1; x <= 1; x += 1) {
			let offset = vec2f(f32(x), f32(y));
			let sampleUv = clamp((base + offset + vec2f(0.5)) / maskSize, vec2f(0.0), vec2f(1.0));
			let sampleGuide = luminance(textureSampleLevel(foreground, linearSampler, sampleUv, 0.0).rgb);
			let sampleDistance = maskPosition - (base + offset);
			let spatialWeight = exp(-f32(${WEB_CAMERA_EFFECT_MASK_GUIDE_SPATIAL_FALLOFF}) * dot(sampleDistance, sampleDistance));
			let rangeWeight = exp(-f32(${WEB_CAMERA_EFFECT_MASK_GUIDE_RANGE_FALLOFF}) * abs(guide - sampleGuide));
			let weight = spatialWeight * rangeWeight;
			maskTotal += textureSampleLevel(mask, linearSampler, sampleUv, 0.0).r * weight;
			weightTotal += weight;
		}
	}
	return maskTotal / max(weightTotal, 0.0001);
}

@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
	let foregroundColour = textureSample(foreground, linearSampler, input.uv);
	let backgroundColour = textureSample(background, linearSampler, input.uv);
	let coarse = textureSampleLevel(mask, linearSampler, input.uv, 0.0).r;
	var alpha = step(0.5, coarse);
	if (coarse > ${WEB_CAMERA_EFFECT_MASK_BAND_EPSILON} && coarse < 1.0 - ${WEB_CAMERA_EFFECT_MASK_BAND_EPSILON}) {
		let refined = refinedMask(input.uv, luminance(foregroundColour.rgb));
		let curve = clamp((refined - 0.5) / (2.0 * ${WEB_CAMERA_EFFECT_MASK_EDGE_SOFTNESS}) + 0.5, 0.0, 1.0);
		alpha = curve * curve * (3.0 - 2.0 * curve);
	}
	return vec4f(mix(backgroundColour.rgb, foregroundColour.rgb, alpha), 1.0);
}
`;

export const CAMERA_MASK_SHADER = `
struct MaskParams {
	primed: u32,
	padding2: f32,
	padding0: u32,
	padding1: u32,
}

@group(0) @binding(0) var<storage, read> inferenceMask: array<f32>;
@group(0) @binding(1) var<storage, read_write> smoothedMask: array<f32>;
@group(0) @binding(2) var outputMask: texture_storage_2d<rgba8unorm, write>;
@group(0) @binding(3) var<uniform> params: MaskParams;

fn inferenceMaskAt(x: i32, y: i32) -> f32 {
	let clampedX = u32(clamp(x, 0, 255));
	let clampedY = u32(clamp(y, 0, 255));
	return clamp(inferenceMask[clampedY * 256u + clampedX], 0.0, 1.0);
}

@compute @workgroup_size(8, 8)
fn computeMain(@builtin(global_invocation_id) id: vec3u) {
	if (id.x >= 256u || id.y >= 256u) {
		return;
	}
	let index = id.y * 256u + id.x;
	let texelX = i32(id.x);
	let texelY = i32(id.y);
	let centre = clamp(inferenceMask[index], 0.0, 1.0);
	var neighbourhoodSum = 0.0;
	var maxNeighbour = 0.0;
	for (var offsetY: i32 = -1; offsetY <= 1; offsetY += 1) {
		for (var offsetX: i32 = -1; offsetX <= 1; offsetX += 1) {
			let neighbourValue = inferenceMaskAt(texelX + offsetX, texelY + offsetY);
			neighbourhoodSum += neighbourValue;
			if (offsetX != 0 || offsetY != 0) {
				maxNeighbour = max(maxNeighbour, neighbourValue);
			}
		}
	}
	let neighbourhoodMean = neighbourhoodSum / 9.0;
	var clean = centre;
	if (centre >= ${WEB_CAMERA_EFFECT_MASK_CORE_MIN} && neighbourhoodMean < ${WEB_CAMERA_EFFECT_MASK_SPECKLE_NEIGHBOUR_MAX}) {
		clean = 0.0;
	}
	if (centre <= ${WEB_CAMERA_EFFECT_MASK_VOID_MAX} && neighbourhoodMean > ${WEB_CAMERA_EFFECT_MASK_HOLE_NEIGHBOUR_MIN}) {
		clean = 1.0;
	}
	if (centre >= ${WEB_CAMERA_EFFECT_MASK_CORE_GROW_MIN} && maxNeighbour >= ${WEB_CAMERA_EFFECT_MASK_CORE_MIN}) {
		clean = 1.0;
	}
	let normalized = clamp((clean - ${WEB_CAMERA_EFFECT_MASK_VOID_MAX}) / ${WEB_CAMERA_EFFECT_MASK_BAND_WIDTH}, 0.0, 1.0);
	let shaped = normalized * normalized * (3.0 - 2.0 * normalized);
	let previous = smoothedMask[index];
	let delta = abs(shaped - previous);
	let rawMotion = clamp((delta - ${WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW}) / (${WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_HIGH} - ${WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW}), 0.0, 1.0);
	let motion = rawMotion * rawMotion * (3.0 - 2.0 * rawMotion);
	let keep = select(0.0, ${WEB_CAMERA_EFFECT_MASK_TEMPORAL_KEEP_STILL} * (1.0 - motion), params.primed != 0u);
	var next = keep * previous + (1.0 - keep) * shaped;
	if (clean >= ${WEB_CAMERA_EFFECT_MASK_CORE_MIN}) {
		next = 1.0;
	}
	if (clean <= ${WEB_CAMERA_EFFECT_MASK_VOID_MAX}) {
		next = 0.0;
	}
	smoothedMask[index] = next;
	textureStore(outputMask, vec2u(id.xy), vec4f(next, 0.0, 0.0, 1.0));
}
`;
