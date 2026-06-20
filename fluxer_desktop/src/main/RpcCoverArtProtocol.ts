// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {protocol} from 'electron';

export const RPC_COVER_ART_SCHEME = 'fluxer-rpc-art';

interface CachedCoverArt {
	mime: string;
	buffer: Buffer;
}

const cache = new Map<string, CachedCoverArt>();

export function registerRpcCoverArtScheme(): void {
	protocol.registerSchemesAsPrivileged([
		{
			scheme: RPC_COVER_ART_SCHEME,
			privileges: {
				standard: true,
				secure: true,
				supportFetchAPI: true,
				corsEnabled: true,
				stream: true,
			},
		},
	]);
}

export function registerRpcCoverArtHandler(): void {
	protocol.handle(RPC_COVER_ART_SCHEME, (request) => {
		const id = new URL(request.url).hostname;
		const entry = cache.get(id);
		if (!entry) {
			return new Response(null, {status: 404});
		}
		return new Response(new Uint8Array(entry.buffer), {
			headers: {'Content-Type': entry.mime, 'Cache-Control': 'private, max-age=86400'},
		});
	});
}

export function cacheRpcCoverArt(sourceUrl: string, mime: string, buffer: Buffer): string {
	const id = createHash('sha256').update(sourceUrl).digest('hex').slice(0, 16);
	cache.set(id, {mime, buffer});
	return `${RPC_COVER_ART_SCHEME}://${id}/`;
}
