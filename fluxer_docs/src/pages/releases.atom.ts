// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {feedAtom, releaseFeed} from '@/installer/ReleaseFeed';
import {docsPublicEndpoint} from '@/server/DocsConfig';

export const prerender = false;

export const GET: APIRoute = async () => {
	let body: string;
	try {
		body = feedAtom(
			await releaseFeed(),
			new URL('releases.atom', `${docsPublicEndpoint().replace(/\/+$/, '')}/`).toString(),
		);
	} catch {
		return new Response('release list unavailable\n', {
			status: 503,
			headers: {'cache-control': 'no-store', 'content-type': 'text/plain; charset=utf-8'},
		});
	}
	const bytes = new TextEncoder().encode(body);
	return new Response(bytes, {
		status: 200,
		headers: {
			'cache-control': 'public, max-age=300',
			'content-length': bytes.byteLength.toString(),
			'content-type': 'application/atom+xml; charset=utf-8',
		},
	});
};
