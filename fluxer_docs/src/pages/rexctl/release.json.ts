// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {isCalVer, REXCTL_TARGETS, resolveRexctlRelease} from '@/installer/RexctlRelease';

export const prerender = false;

const CACHE_CONTROL = 'public, max-age=60';

function reply(status: number, body: string, contentType: string): Response {
	const bytes = new TextEncoder().encode(body);
	return new Response(bytes, {
		status,
		headers: {
			'cache-control': status === 200 ? CACHE_CONTROL : 'no-store',
			'content-length': bytes.byteLength.toString(),
			'content-type': contentType,
		},
	});
}

function jsonReply(status: number, value: unknown): Response {
	return reply(status, `${JSON.stringify(value)}\n`, 'application/json; charset=utf-8');
}

export const GET: APIRoute = async ({url}) => {
	const version = url.searchParams.get('version');
	if (version !== null && !isCalVer(version)) {
		return jsonReply(400, {error: 'version must look like 2026.1105.120000'});
	}
	const format = url.searchParams.get('format');
	const os = url.searchParams.get('os');
	const arch = url.searchParams.get('arch');
	if (format !== null && format !== 'sh') {
		return jsonReply(400, {error: 'format must be sh'});
	}
	if (format === 'sh' && !REXCTL_TARGETS.some((t) => t.os === os && t.arch === arch)) {
		return jsonReply(400, {error: 'os and arch must name a supported target'});
	}
	let release: Awaited<ReturnType<typeof resolveRexctlRelease>>;
	try {
		release = await resolveRexctlRelease(version);
	} catch {
		return jsonReply(503, {error: 'release list unavailable'});
	}
	if (!release) {
		return jsonReply(404, {error: version === null ? 'no release yet' : 'no such version'});
	}
	if (format === 'sh') {
		const asset = release.assets.find((a) => a.os === os && a.arch === arch);
		if (!asset) {
			return jsonReply(404, {error: 'no build for this target'});
		}
		return reply(
			200,
			`version ${release.version}\nurl ${asset.url}\nsha256 ${asset.sha256}\n`,
			'text/plain; charset=utf-8',
		);
	}
	return jsonReply(200, release);
};
