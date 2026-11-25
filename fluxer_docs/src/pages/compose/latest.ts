// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {resolveRexctlRelease} from '@/installer/RexctlRelease';
import {docsPublicEndpoint} from '@/server/DocsConfig';

export const prerender = false;

function reply(status: number, body: Record<string, string>, cache: string): Response {
	return new Response(`${JSON.stringify(body)}\n`, {
		status,
		headers: {'cache-control': cache, 'content-type': 'application/json; charset=utf-8'},
	});
}

export const GET: APIRoute = async () => {
	let release: Awaited<ReturnType<typeof resolveRexctlRelease>>;
	try {
		release = await resolveRexctlRelease(null);
	} catch {
		return reply(503, {error: 'release list unavailable'}, 'no-store');
	}
	if (release?.compose_url) {
		return new Response(null, {
			status: 302,
			headers: {'cache-control': 'public, max-age=60', location: release.compose_url},
		});
	}
	if (!release) {
		return reply(404, {error: 'no release yet'}, 'no-store');
	}
	const moving = new URL('operator/moving/', `${docsPublicEndpoint().replace(/\/$/, '')}/`);
	return reply(
		410,
		{
			error: 'Fluxer no longer publishes a Docker Compose bundle. Fluxer now runs in one container.',
			see: moving.href,
		},
		'public, max-age=3600',
	);
};
