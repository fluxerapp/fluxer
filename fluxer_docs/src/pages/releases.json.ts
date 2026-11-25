// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {releaseFeed} from '@/installer/ReleaseFeed';

export const prerender = false;

function jsonReply(status: number, value: unknown): Response {
	const bytes = new TextEncoder().encode(`${JSON.stringify(value)}\n`);
	return new Response(bytes, {
		status,
		headers: {
			'cache-control': status === 200 ? 'public, max-age=300' : 'no-store',
			'content-length': bytes.byteLength.toString(),
			'content-type': 'application/json; charset=utf-8',
		},
	});
}

export const GET: APIRoute = async () => {
	try {
		return jsonReply(200, await releaseFeed());
	} catch {
		return jsonReply(503, {error: 'release list unavailable'});
	}
};
