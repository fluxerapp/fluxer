// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineConfig} from 'vitest/config';

const DESKTOP_MODULE_ASSET_QUERY = /[?&]m=([a-z][a-z0-9_]{0,63})(?:&|$)/;
const DESKTOP_MODULE_ASSET_STUB_PREFIX = '\0fluxer-module-asset:';

function desktopModuleAssetStubPlugin() {
	return {
		name: 'fluxer-desktop-module-asset-stub',
		enforce: 'pre' as const,
		resolveId(source: string) {
			return DESKTOP_MODULE_ASSET_QUERY.test(source) ? `${DESKTOP_MODULE_ASSET_STUB_PREFIX}${source}` : null;
		},
		load(id: string) {
			if (!id.startsWith(DESKTOP_MODULE_ASSET_STUB_PREFIX)) {
				return null;
			}
			const source = id.slice(DESKTOP_MODULE_ASSET_STUB_PREFIX.length);
			const moduleName = DESKTOP_MODULE_ASSET_QUERY.exec(source)?.[1] ?? 'unknown';
			const name = source.split('?')[0].split('/').pop() ?? 'asset';
			return `export default ${JSON.stringify(`/assets/${moduleName}/${name}`)};`;
		},
	};
}

export default defineConfig({
	plugins: [desktopModuleAssetStubPlugin()],
	resolve: {tsconfigPaths: true},
	oxc: {
		jsx: {runtime: 'automatic', importSource: 'react'},
	},
	test: {
		globals: true,
		environment: 'node',
		testTimeout: 30_000,
		hookTimeout: 30_000,
		setupFiles: ['./vitest.setup.ts'],
		env: {
			PUBLIC_BUILD_VERSION: process.env.PUBLIC_BUILD_VERSION ?? 'dev',
			PUBLIC_RELEASE_CHANNEL: process.env.PUBLIC_RELEASE_CHANNEL ?? 'development',
		},
		include: ['src/**/*.{test,spec}.{ts,tsx}'],
		exclude: ['node_modules', 'dist', '../.claude/**'],
		server: {
			deps: {
				inline: [/livekit-client/, /@livekit\//],
			},
		},
	},
});
