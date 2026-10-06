// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {sources} from '@rspack/core';

const STATIC_CDN_ENDPOINT_PLACEHOLDER = '{{STATIC_CDN_ENDPOINT}}';

const FONT_LICENSE_FILES = [
	{source: 'NOTICE.md', asset: 'assets/fonts-NOTICE.txt'},
	{source: 'LICENSE-IBM-PLEX.txt', asset: 'assets/fonts-LICENSE-IBM-PLEX.txt'},
];

const WASM_LICENSE_FILES = [
	{source: 'libfluxcore/NOTICE.md', asset: 'assets/libfluxcore-NOTICE.txt'},
	{source: 'libfluxcore/LICENSE-ZSTD.txt', asset: 'assets/libfluxcore-LICENSE-ZSTD.txt'},
	{source: 'libfluxcore/LICENSE-ZSTD-SYS.txt', asset: 'assets/libfluxcore-LICENSE-ZSTD-SYS.txt'},
	{source: 'libfluxcore/LICENSE-ZSTD-RS.txt', asset: 'assets/libfluxcore-LICENSE-ZSTD-RS.txt'},
	{source: 'libfluxwebp/NOTICE.md', asset: 'assets/libfluxwebp-NOTICE.txt'},
	{source: 'libfluxwebp/LICENSE-LIBWEBP.txt', asset: 'assets/libfluxwebp-LICENSE-LIBWEBP.txt'},
	{source: 'libfluxwebp/PATENTS-LIBWEBP.txt', asset: 'assets/libfluxwebp-PATENTS-LIBWEBP.txt'},
	{source: 'libfluxwebp/LICENSE-LIBWEBP-SYS.txt', asset: 'assets/libfluxwebp-LICENSE-LIBWEBP-SYS.txt'},
	{source: 'libfluxwebp/simd/LICENSE', asset: 'assets/libfluxwebp-LICENSE-EMSCRIPTEN.txt'},
];

function resolveStaticCdnEndpoint(staticCdnEndpoint) {
	const value = staticCdnEndpoint?.trim().replace(/\/+$/, '');
	return value || STATIC_CDN_ENDPOINT_PLACEHOLDER;
}

function generateManifest(staticCdnEndpoint) {
	const cdn = resolveStaticCdnEndpoint(staticCdnEndpoint);
	const manifest = {
		name: 'Fluxer',
		short_name: 'Fluxer',
		description:
			'Fluxer is a free and open source instant messaging and VoIP platform built for friends, groups, and communities.',
		id: '/',
		start_url: '/app',
		display: 'standalone',
		orientation: 'portrait-primary',
		theme_color: '#4641D9',
		background_color: '#2b2d31',
		categories: ['social', 'communication'],
		lang: 'en',
		scope: '/',
		scope_extensions: [],
		icons: [
			{
				src: `${cdn}/web/android-chrome-192x192.png`,
				sizes: '192x192',
				type: 'image/png',
				purpose: 'maskable any',
			},
			{
				src: `${cdn}/web/android-chrome-512x512.png`,
				sizes: '512x512',
				type: 'image/png',
				purpose: 'maskable any',
			},
			{
				src: `${cdn}/web/apple-touch-icon.png`,
				sizes: '180x180',
				type: 'image/png',
			},
			{
				src: `${cdn}/web/favicon-32x32.png`,
				sizes: '32x32',
				type: 'image/png',
			},
			{
				src: `${cdn}/web/favicon-16x16.png`,
				sizes: '16x16',
				type: 'image/png',
			},
		],
	};

	return JSON.stringify(manifest, null, 2);
}

function generateBrowserConfig(staticCdnEndpoint) {
	const cdn = resolveStaticCdnEndpoint(staticCdnEndpoint);
	return `<?xml version="1.0" encoding="utf-8"?>
<browserconfig>
  <msapplication>
    <tile>
      <square150x150logo src="${cdn}/web/mstile-150x150.png"/>
      <TileColor>#4641D9</TileColor>
    </tile>
  </msapplication>
</browserconfig>`;
}

function generateRobotsTxt() {
	return 'User-agent: *\nAllow: /\n';
}

export class StaticFilesPlugin {
	constructor(options = {}) {
		this.staticCdnEndpoint = options.staticCdnEndpoint;
		this.fontsDir = options.fontsDir;
		this.wasmCratesDir = options.wasmCratesDir;
	}

	emitLicenseFiles(compilation, dir, files, subject) {
		if (!dir) {
			return;
		}
		for (const {source, asset} of files) {
			const sourcePath = path.join(dir, source);
			if (!fs.existsSync(sourcePath)) {
				throw new Error(
					`StaticFilesPlugin: ${sourcePath} is missing. The bundled ${subject} may not be redistributed without it.`,
				);
			}
			compilation.emitAsset(asset, new sources.RawSource(fs.readFileSync(sourcePath)));
		}
	}

	apply(compiler) {
		compiler.hooks.thisCompilation.tap('StaticFilesPlugin', (compilation) => {
			compilation.hooks.processAssets.tap(
				{
					name: 'StaticFilesPlugin',
					stage: compilation.PROCESS_ASSETS_STAGE_ADDITIONAL,
				},
				() => {
					compilation.emitAsset('manifest.json', new sources.RawSource(generateManifest(this.staticCdnEndpoint)));
					compilation.emitAsset(
						'browserconfig.xml',
						new sources.RawSource(generateBrowserConfig(this.staticCdnEndpoint)),
					);
					compilation.emitAsset('robots.txt', new sources.RawSource(generateRobotsTxt()));
					this.emitLicenseFiles(compilation, this.fontsDir, FONT_LICENSE_FILES, 'fonts');
					this.emitLicenseFiles(compilation, this.wasmCratesDir, WASM_LICENSE_FILES, 'WebAssembly modules');
				},
			);
		});
	}
}

export function staticFilesPlugin(options) {
	return new StaticFilesPlugin(options);
}
