// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {resolveRexctlRelease} from '@/installer/RexctlRelease';

export const prerender = false;

function errorReply(status: number, error: string): Response {
	return new Response(`${JSON.stringify({error})}\n`, {
		status,
		headers: {'cache-control': 'no-store', 'content-type': 'application/json; charset=utf-8'},
	});
}

export const GET: APIRoute = async () => {
	let release: Awaited<ReturnType<typeof resolveRexctlRelease>>;
	try {
		release = await resolveRexctlRelease(null);
	} catch {
		return errorReply(503, 'release list unavailable');
	}
	if (!release?.compose_url) {
		return errorReply(404, 'no compose bundle yet');
	}
	return new Response(null, {
		status: 302,
		headers: {'cache-control': 'public, max-age=60', location: release.compose_url},
	});
};
