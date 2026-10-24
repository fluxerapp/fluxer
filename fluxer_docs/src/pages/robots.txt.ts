// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {docsPublicEndpoint, docsSearchIndexing} from '@/server/DocsConfig';

export const prerender = false;

export const GET: APIRoute = () => {
	const body = docsSearchIndexing()
		? `User-agent: *\nAllow: /\n\nSitemap: ${new URL('sitemap-index.xml', `${docsPublicEndpoint().replace(/\/$/, '')}/`).href}\n`
		: 'User-agent: *\nDisallow: /\n';
	return new Response(body, {headers: {'Content-Type': 'text/plain; charset=utf-8'}});
};
