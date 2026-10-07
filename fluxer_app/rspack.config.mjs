// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, mkdirSync, readdirSync, writeFileSync} from 'node:fs';
import path, {dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {
	Compilation,
	CopyRspackPlugin,
	DefinePlugin,
	HtmlRspackPlugin,
	LightningCssMinimizerRspackPlugin,
	SwcJsMinimizerRspackPlugin,
	sources,
} from '@rspack/core';
import {createPoFileRule, getLinguiSwcPluginConfig} from './scripts/build/rspack/lingui.mjs';
import {staticFilesPlugin} from './scripts/build/rspack/static-files.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = path.resolve(__dirname, '.');
const MONOREPO_ROOT = path.resolve(__dirname, '..');
const SRC_DIR = path.join(ROOT_DIR, 'src');
const DIST_DIR = path.join(ROOT_DIR, 'dist');
const PKGS_DIR = path.join(ROOT_DIR, 'pkgs');
const PUBLIC_DIR = path.join(ROOT_DIR, 'assets');
const BROWSER_ASSERT_STRICT_MODULE = path.join(SRC_DIR, 'features', 'platform', 'utils', 'BrowserAssertStrict.ts');
const DEFAULT_DEV_SERVER_PORT = 3000;
const BOOT_PRELOAD_CHUNK_GROUPS = ['app-bootstrap', 'boot-app'];
const BOOT_PRELOAD_FONT_FILES = [
	'FluxerSans/FluxerSans-Regular.woff2',
	'FluxerSans/FluxerSans-SemiBold.woff2',
	'FluxerSans/FluxerSans-Bold.woff2',
];
const BOOT_LOW_PRIORITY_FONT_FILES = ['FluxerSans/FluxerSans-Medium.woff2'];
const AUTH_ENTRY_DOCUMENT = 'auth-index.html';
const ENTRY_STYLESHEET_LINK_PATTERN = /<link href="([^"]+\.css)" rel="stylesheet">/gu;
const CSS_URL_PATTERN = /url\(\s*['"]?([^'")]+)/gu;
const DESKTOP_MODULE_ASSET_QUERY = /[?&]m=([a-z][a-z0-9_]{0,63})(?:&|$)/u;
const RESERVED_DESKTOP_MODULE_NAMES = new Set(['assets', 'fluxer_renderer']);

class UnusableDesktopModuleQueryError extends Error {
	constructor(resource) {
		super(
			`${resource} carries an ?m= desktop module query that is not a usable module name. It must match [a-z][a-z0-9_]{0,63} and cannot be assets or fluxer_renderer.`,
		);
		this.name = 'UnusableDesktopModuleQueryError';
	}
}

function desktopModuleAssetName(resource) {
	const match = DESKTOP_MODULE_ASSET_QUERY.exec(resource);
	if (match == null || RESERVED_DESKTOP_MODULE_NAMES.has(match[1])) {
		throw new UnusableDesktopModuleQueryError(resource);
	}
	return match[1];
}

function resolveMode() {
	const modeIndex = process.argv.indexOf('--mode');
	if (modeIndex >= 0) {
		const modeValue = process.argv[modeIndex + 1];
		if (modeValue) {
			return modeValue;
		}
	}
	return 'production';
}

function isMainRuntimeChunk(chunk) {
	const runtime = chunk.runtime;
	if (runtime == null) {
		return chunk.name === 'main';
	}
	if (typeof runtime === 'string') {
		return runtime === 'main';
	}
	if (typeof runtime[Symbol.iterator] === 'function') {
		for (const name of runtime) {
			if (name !== 'main') return false;
		}
		return true;
	}
	return false;
}

function mirrorCacheGroups(groups) {
	const mirrored = {};
	for (const [key, group] of Object.entries(groups)) {
		if (group.chunks != null) {
			mirrored[key] = group;
			continue;
		}
		mirrored[key] = {...group, chunks: (chunk) => isMainRuntimeChunk(chunk) && chunk.canBeInitial()};
		mirrored[`${key}Async`] = {
			...group,
			name: `${group.name}-async`,
			chunks: (chunk) => isMainRuntimeChunk(chunk) && !chunk.canBeInitial(),
		};
	}
	return mirrored;
}

function bootPreloadPlugin() {
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('BootPreloadPlugin', (compilation) => {
				compilation.hooks.processAssets.tap(
					{name: 'BootPreloadPlugin', stage: Compilation.PROCESS_ASSETS_STAGE_REPORT},
					() => {
						const asset = compilation.getAsset('index.html');
						if (asset == null) {
							return;
						}
						const html = String(asset.source.source());
						if (!html.includes('</body>')) {
							return;
						}
						const configuredPublicPath = compilation.outputOptions.publicPath;
						const base =
							typeof configuredPublicPath === 'string' && configuredPublicPath !== 'auto' ? configuredPublicPath : '/';
						const files = new Set();
						for (const name of BOOT_PRELOAD_CHUNK_GROUPS) {
							for (const group of compilation.chunkGroups) {
								if (group.name !== name) {
									continue;
								}
								for (const file of group.getFiles()) {
									if (!html.includes(file)) {
										files.add(file);
									}
								}
							}
						}
						const ordered = [...files];
						const links = [
							...ordered
								.filter((file) => file.endsWith('.css'))
								.map((file) => `<link rel="preload" as="style" fetchpriority="low" href="${base}${file}">`),
							...ordered
								.filter((file) => file.endsWith('.js'))
								.map((file) => `<link rel="preload" as="script" fetchpriority="low" href="${base}${file}">`),
						].join('');
						if (links === '') {
							return;
						}
						compilation.updateAsset('index.html', new sources.RawSource(html.replace('</body>', `${links}</body>`)));
					},
				);
			});
		},
	};
}

class UninlinableEntryStylesheetError extends Error {
	constructor(reason) {
		super(`Cannot inline the entry stylesheet into ${AUTH_ENTRY_DOCUMENT}: ${reason}`);
		this.name = 'UninlinableEntryStylesheetError';
	}
}

function inlineEntryStylesheet(css, href) {
	const content = css.replace(/\/\*# sourceMappingURL=[^*]*\*\/\s*$/u, '');
	if (content.includes('</style')) {
		throw new UninlinableEntryStylesheetError(`${href} contains a closing style tag`);
	}
	for (const [, url] of content.matchAll(CSS_URL_PATTERN)) {
		if (!/^(?:\/|https?:|data:|#)/u.test(url)) {
			throw new UninlinableEntryStylesheetError(`${href} references ${url} relative to its own location`);
		}
	}
	return `<style>${content}</style><link rel="prefetch" as="style" href="${href}">`;
}

function authEntryDocumentPlugin() {
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('AuthEntryDocumentPlugin', (compilation) => {
				compilation.hooks.processAssets.tap(
					{name: 'AuthEntryDocumentPlugin', stage: Compilation.PROCESS_ASSETS_STAGE_REPORT},
					() => {
						const asset = compilation.getAsset('index.html');
						if (asset == null) {
							return;
						}
						const html = String(asset.source.source());
						const links = [...html.matchAll(ENTRY_STYLESHEET_LINK_PATTERN)];
						if (links.length === 0) {
							throw new UninlinableEntryStylesheetError('index.html links no stylesheet');
						}
						let document = html;
						for (const [link, href] of links) {
							const name = compilation
								.getAssets()
								.find((candidate) => href.endsWith(`/${candidate.name}`) || href === candidate.name)?.name;
							if (name == null) {
								throw new UninlinableEntryStylesheetError(`${href} is not an emitted asset`);
							}
							const css = String(compilation.getAsset(name).source.source());
							document = document.replace(link, () => inlineEntryStylesheet(css, href));
						}
						compilation.emitAsset(AUTH_ENTRY_DOCUMENT, new sources.RawSource(document));
					},
				);
			});
		},
	};
}

function fontPreloadPlugin() {
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('FontPreloadPlugin', (compilation) => {
				HtmlRspackPlugin.getCompilationHooks(compilation).alterAssetTagGroups.tap('FontPreloadPlugin', (data) => {
					const fonts = [];
					const lowPriority = new Set(BOOT_LOW_PRIORITY_FONT_FILES);
					for (const sourceName of [...BOOT_PRELOAD_FONT_FILES, ...BOOT_LOW_PRIORITY_FONT_FILES]) {
						const asset = compilation
							.getAssets()
							.find((candidate) => String(candidate.info.sourceFilename ?? '').endsWith(sourceName));
						if (asset == null) {
							throw new Error(`Cannot preload ${sourceName}: no emitted asset came from it`);
						}
						fonts.push({
							tagName: 'link',
							voidTag: true,
							attributes: {
								rel: 'preload',
								as: 'font',
								type: 'font/woff2',
								crossorigin: '',
								href: `${data.publicPath}${asset.name}`,
								...(lowPriority.has(sourceName) ? {fetchpriority: 'low'} : {}),
							},
						});
					}
					data.headTags.unshift(...fonts);
					return data;
				});
			});
		},
	};
}

function nodeAssertStrictSchemePlugin() {
	return {
		apply(compiler) {
			compiler.hooks.normalModuleFactory.tap('NodeAssertStrictSchemePlugin', (factory) => {
				factory.hooks.beforeResolve.tap('NodeAssertStrictSchemePlugin', (resolveData) => {
					if (resolveData.request === 'node:assert/strict') {
						resolveData.request = BROWSER_ASSERT_STRICT_MODULE;
					}
				});
			});
		},
	};
}

const mode = resolveMode();
const isProduction = mode === 'production';
const isDevelopment = !isProduction;
const isDesktopRenderer = envString('FLUXER_DESKTOP_RENDERER') === 'true';
const devJsName = 'assets/[name].js';
const devCssName = 'assets/[name].css';
const productionJsName = 'assets/[contenthash:16].js';
const productionWorkerJsName = 'assets/[contenthash:16].worker.js';
const devCorsHeaders = {
	'Access-Control-Allow-Origin': '*',
	'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, PATCH, OPTIONS',
	'Access-Control-Allow-Headers': 'X-Requested-With, content-type, Authorization',
};
const devNoStoreHeaders = {
	'Cache-Control': 'no-store, no-cache, must-revalidate, max-age=0',
	Pragma: 'no-cache',
	Expires: '0',
	'CDN-Cache-Control': 'no-store',
	'Cloudflare-CDN-Cache-Control': 'no-store',
};
function envString(name, fallback = undefined) {
	const value = process.env[name];
	if (value === undefined || value === null || value === '') {
		return fallback;
	}
	return value;
}

function withTrailingSlash(value) {
	return value.endsWith('/') ? value : `${value}/`;
}

function devServerHeaders() {
	return {...devCorsHeaders, ...devNoStoreHeaders};
}

function resolveReleaseChannel() {
	const value = envString('PUBLIC_RELEASE_CHANNEL', envString('RELEASE_CHANNEL', 'canary')).trim().toLowerCase();
	if (value === 'stable' || value === 'canary' || value === 'development') {
		return value;
	}
	throw new Error(`PUBLIC_RELEASE_CHANNEL must be stable, canary, or development, received ${JSON.stringify(value)}`);
}

function resolvePublicValues() {
	return {
		PUBLIC_BUILD_VERSION: envString('PUBLIC_BUILD_VERSION', envString('BUILD_VERSION', 'dev')),
		PUBLIC_RELEASE_CHANNEL: resolveReleaseChannel(),
	};
}

function getPublicEnvVar(values, name) {
	const value = values[name];
	return value === undefined ? 'undefined' : JSON.stringify(value);
}

function getChunkRuntimeNames(runtime) {
	if (runtime == null) {
		return [];
	}
	if (typeof runtime === 'string') {
		return [runtime];
	}
	if (typeof runtime[Symbol.iterator] === 'function') {
		return [...runtime].filter((name) => typeof name === 'string');
	}
	return [];
}

function getChunkPathNames(pathData) {
	const names = [];
	const chunkName = pathData.chunk?.name;
	if (typeof chunkName === 'string') {
		names.push(chunkName);
	}
	names.push(...getChunkRuntimeNames(pathData.runtime));
	names.push(...getChunkRuntimeNames(pathData.chunk?.runtime));
	return names;
}

function isWorkerPath(pathData) {
	return getChunkPathNames(pathData).some((name) => name.endsWith('.worker'));
}

function jsFilename(pathData) {
	if (pathData.chunk?.name === 'sw') {
		return 'sw.js';
	}
	if (!isProduction) {
		return devJsName;
	}
	return isWorkerPath(pathData) ? productionWorkerJsName : productionJsName;
}

export default () => {
	const linguiSwcPlugin = getLinguiSwcPluginConfig();
	const publicValues = resolvePublicValues();
	const assetBaseUrl = isDesktopRenderer ? undefined : envString('PUBLIC_ASSET_BASE_URL');
	const staticCdnEndpoint = isDesktopRenderer
		? ''
		: envString('PUBLIC_STATIC_CDN_ENDPOINT', envString('FLUXER_STATIC_CDN_ENDPOINT', isProduction ? '' : ''));
	function resolveArboriumWasmAliases() {
		const arbDir = path.join(ROOT_DIR, 'node_modules', '@arborium');
		const aliases = {};
		try {
			for (const pkg of readdirSync(arbDir)) {
				const grammarWasm = path.join(arbDir, pkg, 'grammar_bg.wasm');
				if (!existsSync(grammarWasm)) continue;
				const internalWasm = `arborium_${pkg.replace(/-/g, '_')}_plugin_bg.wasm`;
				aliases[internalWasm] = grammarWasm;
				aliases[`@arborium/${pkg}/${internalWasm}`] = grammarWasm;
			}
		} catch {}
		return aliases;
	}
	const normalizedStaticCdnEndpoint = staticCdnEndpoint?.replace(/\/+$/, '') ?? '';
	const workerWasmPublicPath =
		isProduction && normalizedStaticCdnEndpoint ? `${normalizedStaticCdnEndpoint}/` : undefined;
	const productionPublicPath = assetBaseUrl !== undefined ? withTrailingSlash(assetBaseUrl) : '/';
	const developmentPublicPath = normalizedStaticCdnEndpoint ? `${normalizedStaticCdnEndpoint}/` : '/';
	const publicPath = isProduction ? productionPublicPath : developmentPublicPath;
	return {
		mode,
		entry: isDesktopRenderer
			? {main: path.join(SRC_DIR, 'index.tsx')}
			: {
					main: path.join(SRC_DIR, 'index.tsx'),
					sw: path.join(SRC_DIR, 'features', 'platform', 'service_worker', 'Worker.ts'),
				},
		output: {
			path: DIST_DIR,
			publicPath,
			workerPublicPath: '/',
			workerChunkLoading: false,
			filename: jsFilename,
			chunkFilename: jsFilename,
			cssFilename: isProduction ? 'assets/[contenthash:16].css' : devCssName,
			cssChunkFilename: isProduction ? 'assets/[contenthash:16].css' : devCssName,
			assetModuleFilename: isProduction ? 'assets/[contenthash:16][ext]' : 'assets/[name].[hash][ext]',
			webAssemblyModuleFilename: isProduction ? 'assets/[contenthash:16].wasm' : 'assets/[name].[hash].wasm',
			clean: true,
		},
		devtool: 'source-map',
		target: ['web', 'browserslist'],
		lazyCompilation: false,
		performance: false,
		resolve: {
			alias: {
				...resolveArboriumWasmAliases(),
				'@arborium/arborium/arborium_host_bg.wasm': path.resolve(
					ROOT_DIR,
					'node_modules/@arborium/arborium/dist/arborium_host_bg.wasm',
				),
				'@app': SRC_DIR,
				'@fluxer/voice_engine_v2/bridge': path.join(
					MONOREPO_ROOT,
					'packages',
					'voice_engine_v2',
					'src',
					'bridge',
					'index.ts',
				),
				'@fluxer/voice_engine_v2/runtime': path.join(
					MONOREPO_ROOT,
					'packages',
					'voice_engine_v2',
					'src',
					'runtime',
					'index.ts',
				),
				'@fluxer/voice_engine_v2/testing': path.join(
					MONOREPO_ROOT,
					'packages',
					'voice_engine_v2',
					'src',
					'testing',
					'index.ts',
				),
				'@fluxer': path.join(MONOREPO_ROOT, 'packages'),
				'@pkgs': PKGS_DIR,
				'assert/strict': BROWSER_ASSERT_STRICT_MODULE,
				'livekit-client$': path.join(PKGS_DIR, 'livekit-client/src/index.ts'),
				'livekit-client/e2ee-worker': path.join(PKGS_DIR, 'livekit-client/src/e2ee/worker/e2ee.worker.ts'),
				'node:assert/strict': BROWSER_ASSERT_STRICT_MODULE,
			},
			extensions: [
				'.web.tsx',
				'.web.ts',
				'.web.jsx',
				'.web.js',
				'.tsx',
				'.ts',
				'.jsx',
				'.js',
				'.json',
				'.mjs',
				'.cjs',
				'.po',
			],
			extensionAlias: {
				'.js': ['.js', '.tsx', '.ts'],
				'.mjs': ['.mjs', '.mts'],
				'.cjs': ['.cjs', '.cts'],
			},
			conditionNames: ['import', 'module', 'webpack', 'browser', 'default'],
			mainFields: ['browser', 'module', 'main'],
		},
		module: {
			rules: [
				{
					test: /[\\/]@arborium[\\/]arborium[\\/]dist[\\/]arborium\.js$/,
					use: [{loader: path.join(ROOT_DIR, 'scripts/build/rspack/local-arborium-loader.cjs')}],
					parser: {
						wrappedContextRegExp: /^\b\B$/u,
						exprContextCritical: false,
						wrappedContextCritical: false,
					},
				},
				{
					test: /[\\/]@sapphi-red[\\/]web-noise-suppressor[\\/]dist[\\/][^\\/]+[\\/]workletProcessor\.js$/,
					type: 'asset/resource',
					use: [{loader: path.join(ROOT_DIR, 'scripts/build/rspack/noise-suppressor-worklet-loader.cjs')}],
					generator: {
						filename: isProduction ? 'assets/[contenthash:16].worklet.js' : 'assets/[name].[hash].worklet.js',
					},
				},
				{
					test: /[\\/]src[\\/].+\.worklet\.js$/,
					type: 'asset/resource',
					generator: {
						filename: isProduction ? 'assets/[contenthash:16].worklet.js' : 'assets/[name].[hash].worklet.js',
					},
				},
				{
					test: /\.(tsx|ts|jsx|js)$/,
					exclude: [/node_modules/, /\.worklet\.js$/],
					type: 'javascript/auto',
					parser: {
						dynamicImport: true,
					},
					use: {
						loader: 'builtin:swc-loader',
						options: {
							jsc: {
								parser: {
									syntax: 'typescript',
									tsx: true,
								},
								transform: {
									react: {
										runtime: 'automatic',
										development: isDevelopment,
										refresh: false,
									},
								},
								experimental: {
									plugins: [linguiSwcPlugin],
								},
								target: 'es2015',
							},
						},
					},
				},
				createPoFileRule(),
				{
					test: /\.module\.css$/,
					use: [{loader: 'postcss-loader'}],
					type: 'css/module',
					parser: {namedExports: false, dashedIdents: false, grid: false, container: false},
				},
				{
					test: /[\\/]@phosphor-icons[\\/]react[\\/]dist[\\/]defs[\\/][^\\/]+\.es\.js$/,
					use: [{loader: path.join(ROOT_DIR, 'scripts/build/rspack/phosphor-unused-weights-loader.cjs')}],
				},
				{
					test: /[\\/]node_modules[\\/]katex[\\/]dist[\\/]katex(\.min)?\.css$/,
					use: [{loader: path.join(ROOT_DIR, 'scripts/build/rspack/katex-legacy-fonts-loader.cjs')}],
					type: 'css',
				},
				{
					test: /\.css$/,
					exclude: /\.module\.css$/,
					use: [{loader: 'postcss-loader'}],
					type: 'css',
				},
				{
					test: /\.svg$/,
					issuer: /\.[jt]sx?$/,
					resourceQuery: /react/,
					type: 'javascript/auto',
					use: [
						{
							loader: 'builtin:swc-loader',
							options: {
								jsc: {
									parser: {syntax: 'typescript', tsx: true},
									transform: {react: {runtime: 'automatic', development: isDevelopment}},
									target: 'es2015',
								},
							},
						},
						{
							loader: '@svgr/webpack',
							options: {
								babel: false,
								typescript: true,
								jsxRuntime: 'automatic',
								svgoConfig: {
									plugins: [
										{
											name: 'preset-default',
											params: {overrides: {removeViewBox: false}},
										},
									],
								},
							},
						},
					],
				},
				{
					test: /\.svg$/,
					resourceQuery: {not: [/react/]},
					type: 'asset/resource',
				},
				{
					test: /\.wasm$/,
					type: 'asset/resource',
					generator: {
						filename: isProduction ? 'assets/[contenthash:16][ext]' : 'assets/[name].[hash][ext]',
						...(workerWasmPublicPath ? {publicPath: workerWasmPublicPath} : {}),
					},
				},
				{
					test: /[\\/]deepfilternet3[\\/][^\\/]+\.tar\.gz$/,
					type: 'asset/resource',
					generator: {
						filename: isProduction ? 'assets/[contenthash:16].tar.gz' : 'assets/[name].[hash].tar.gz',
					},
				},
				{
					test: /\.onnx$/,
					type: 'asset/resource',
					generator: {
						filename: isProduction ? 'assets/[contenthash:16][ext]' : 'assets/[name].[hash][ext]',
					},
				},
				{
					test: /\.(png|jpg|jpeg|gif|webp|avif|ico|woff|woff2|ttf|eot|mp3|wav|ogg|mp4|webm)$/,
					type: 'asset/resource',
					generator: {
						filename: isProduction ? 'assets/[contenthash:16][ext]' : 'assets/[name].[hash][ext]',
					},
				},
				{
					resourceQuery: DESKTOP_MODULE_ASSET_QUERY,
					type: 'asset/resource',
					generator: {
						filename: (pathData) => {
							const moduleName = desktopModuleAssetName(pathData.filename ?? '');
							return isProduction
								? `assets/${moduleName}/[contenthash:16][ext]`
								: `assets/${moduleName}/[name].[hash][ext]`;
						},
					},
				},
			],
			generator: {
				'css/module': {
					localIdentName: '[name]__[local]___[hash:base64:6]',
					exportsConvention: 'camel-case-only',
					exportsOnly: false,
				},
				'css/auto': {
					localIdentName: '[name]__[local]___[hash:base64:6]',
					exportsConvention: 'camel-case-only',
					exportsOnly: false,
				},
			},
		},
		plugins: [
			nodeAssertStrictSchemePlugin(),
			fontPreloadPlugin(),
			...(isProduction ? [bootPreloadPlugin()] : []),
			...(isProduction && !isDesktopRenderer ? [authEntryDocumentPlugin()] : []),
			new HtmlRspackPlugin({
				template: path.join(ROOT_DIR, 'index.html'),
				filename: 'index.html',
				hash: isDevelopment,
				inject: 'body',
				scriptLoading: 'module',
				excludeChunks: ['sw'],
				...(isDesktopRenderer ? {publicPath: '/'} : {}),
			}),
			new CopyRspackPlugin({
				patterns: [
					{
						from: PUBLIC_DIR,
						to: DIST_DIR,
						noErrorOnMissing: true,
					},
					...(isDesktopRenderer
						? [
								{
									from: path.join(MONOREPO_ROOT, 'fluxer_static', 'web'),
									to: path.join(DIST_DIR, 'web'),
								},
							]
						: []),
				],
			}),
			staticFilesPlugin({
				staticCdnEndpoint: normalizedStaticCdnEndpoint,
				fontsDir: path.join(MONOREPO_ROOT, 'packages', 'fonts'),
				wasmCratesDir: path.join(ROOT_DIR, 'rust'),
			}),
			new DefinePlugin({
				__FLUXER_PRECACHE_MANIFEST__: JSON.stringify([]),
				__FLUXER_SW_VERSION__: JSON.stringify(publicValues.PUBLIC_BUILD_VERSION || 'dev'),
				'process.env.NODE_ENV': JSON.stringify(mode),
				'import.meta.env.DEV': JSON.stringify(isDevelopment),
				'import.meta.env.PROD': JSON.stringify(isProduction),
				'import.meta.env.MODE': JSON.stringify(mode),
				'import.meta.env.PUBLIC_BUILD_VERSION': getPublicEnvVar(publicValues, 'PUBLIC_BUILD_VERSION'),
				'import.meta.env.PUBLIC_RELEASE_CHANNEL': getPublicEnvVar(publicValues, 'PUBLIC_RELEASE_CHANNEL'),
			}),
			{
				apply(compiler) {
					compiler.hooks.afterEmit.tap('VersionJsonPlugin', () => {
						const versionData = {
							version: publicValues.PUBLIC_BUILD_VERSION,
						};
						mkdirSync(DIST_DIR, {recursive: true});
						writeFileSync(path.join(DIST_DIR, 'version.json'), JSON.stringify(versionData));
					});
				},
			},
		],
		optimization: {
			splitChunks: isProduction
				? {
						chunks: (chunk) => isMainRuntimeChunk(chunk),
						maxInitialRequests: 15,
						maxAsyncSize: 2_000_000,
						cacheGroups: mirrorCacheGroups({
							icons: {
								test: /[\\/]node_modules[\\/]@phosphor-icons[\\/]/,
								name: 'icons',
								priority: 60,
								reuseExistingChunk: true,
							},
							highlight: {
								test: /[\\/]node_modules[\\/](@arborium[\\/]arborium[\\/]|\.pnpm[\\/]@arborium\+arborium@)/,
								name: 'highlight',
								priority: 55,
								reuseExistingChunk: true,
								enforce: true,
								chunks: (chunk) => !chunk.canBeInitial() && !isWorkerPath({chunk}),
							},
							tts: {
								test: /[\\/]node_modules[\\/]mespeak[\\/]/,
								name: 'tts',
								priority: 54,
								reuseExistingChunk: true,
								enforce: true,
								chunks: (chunk) => !chunk.canBeInitial() && !isWorkerPath({chunk}),
							},
							editor: {
								test: /[\\/]node_modules[\\/](@codemirror|@lezer|codemirror|style-mod|w3c-keyname|crelt|@marijn)[\\/]/,
								name: 'editor',
								priority: 53,
								reuseExistingChunk: true,
								enforce: true,
								chunks: (chunk) => !chunk.canBeInitial() && !isWorkerPath({chunk}),
							},
							livekit: {
								test: /[\\/](node_modules|pkgs)[\\/](livekit-client|@livekit)[\\/]/,
								name: 'livekit',
								priority: 50,
								reuseExistingChunk: true,
							},
							katex: {
								test: /[\\/]node_modules[\\/]katex[\\/]/,
								name: 'katex',
								priority: 48,
								reuseExistingChunk: true,
								maxSize: 100_000,
							},
							animation: {
								test: /[\\/]node_modules[\\/](framer-motion|motion|motion-dom|motion-utils)[\\/]/,
								name: 'animation',
								priority: 45,
								reuseExistingChunk: true,
							},
							mobx: {
								test: /[\\/]node_modules[\\/](mobx|mobx-react-lite|mobx-persist-store)[\\/]/,
								name: 'mobx',
								priority: 43,
								reuseExistingChunk: true,
							},
							i18n: {
								test: /[\\/]node_modules[\\/]@lingui[\\/]/,
								name: 'i18n',
								priority: 42,
								reuseExistingChunk: true,
								enforce: true,
							},
							colorPicker: {
								test: /[\\/]node_modules[\\/](@react-aria[\\/]color|@react-stately[\\/]color)[\\/]/,
								name: 'color-picker',
								priority: 42,
								reuseExistingChunk: true,
								enforce: true,
							},
							reactAria: {
								test: /[\\/]node_modules[\\/](react-aria-components|@react-aria|@react-stately|@internationalized)[\\/]/,
								name: 'react-aria',
								priority: 41,
								reuseExistingChunk: true,
							},
							validation: {
								test: /[\\/]node_modules[\\/](valibot)[\\/]/,
								name: 'validation',
								priority: 39,
								reuseExistingChunk: true,
							},
							datetime: {
								test: /[\\/]node_modules[\\/]luxon[\\/]/,
								name: 'datetime',
								priority: 38,
								reuseExistingChunk: true,
							},
							observable: {
								test: /[\\/]node_modules[\\/]rxjs[\\/]/,
								name: 'observable',
								priority: 37,
								reuseExistingChunk: true,
							},
							unicode: {
								test: /[\\/]node_modules[\\/](idna-uts46-hx|emoji-regex)[\\/]/,
								name: 'unicode',
								priority: 36,
								reuseExistingChunk: true,
							},
							dnd: {
								test: /[\\/]node_modules[\\/](@dnd-kit|react-dnd)[\\/]/,
								name: 'dnd',
								priority: 33,
								reuseExistingChunk: true,
							},
							radix: {
								test: /[\\/]node_modules[\\/]@radix-ui[\\/]/,
								name: 'radix',
								priority: 31,
								reuseExistingChunk: true,
							},
							ui: {
								test: /[\\/]node_modules[\\/](react-select|react-hook-form|@floating-ui)[\\/]/,
								name: 'ui',
								priority: 30,
								reuseExistingChunk: true,
							},
							utils: {
								test: /[\\/]node_modules[\\/](lodash|clsx|thumbhash|match-sorter)[\\/]/,
								name: 'utils',
								priority: 28,
								reuseExistingChunk: true,
							},
							networking: {
								test: /[\\/]node_modules[\\/](ws)[\\/]/,
								name: 'networking',
								priority: 26,
								reuseExistingChunk: true,
							},
							react: {
								test: /[\\/]node_modules[\\/](react|react-dom)[\\/]/,
								name: 'react',
								priority: 25,
								reuseExistingChunk: true,
							},
							platform: {
								test: /[\\/]node_modules[\\/]bowser[\\/]/,
								name: 'platform',
								priority: 27,
								reuseExistingChunk: true,
								enforce: true,
							},
							phone: {
								test: /[\\/]node_modules[\\/]libphonenumber-js[\\/]/,
								name: 'phone',
								priority: 26,
								reuseExistingChunk: true,
								enforce: true,
							},
							qrcode: {
								test: /[\\/]node_modules[\\/](qrcode|dijkstrajs)[\\/]/,
								name: 'qrcode',
								priority: 24,
								reuseExistingChunk: true,
								enforce: true,
							},
							lexical: {
								test: /[\\/]node_modules[\\/](lexical|@lexical)[\\/]/,
								name: 'lexical',
								priority: 23,
								reuseExistingChunk: true,
								enforce: true,
							},
							calendar: {
								test: /[\\/]node_modules[\\/](react-day-picker|date-fns|@date-fns)[\\/]/,
								name: 'calendar',
								priority: 22,
								reuseExistingChunk: true,
								enforce: true,
							},
							webrtc: {
								test: /[\\/]node_modules[\\/](webrtc-adapter|sdp|sdp-transform)[\\/]/,
								name: 'webrtc',
								priority: 21,
								reuseExistingChunk: true,
								enforce: true,
							},
							noiseFilter: {
								test: /[\\/]node_modules[\\/]deepfilternet3-noise-filter[\\/]/,
								name: 'noise-filter',
								priority: 20,
								reuseExistingChunk: true,
								enforce: true,
							},
							virtualizer: {
								test: /[\\/]node_modules[\\/]@tanstack[\\/]/,
								name: 'virtualizer',
								priority: 19,
								reuseExistingChunk: true,
								enforce: true,
							},
							vendor: {
								test: (module) => {
									if (!module.resource) return false;
									if (!/[\\/]node_modules[\\/]/.test(module.resource)) return false;
									if (/[\\/](@arborium|@phosphor-icons|katex)[\\/]/.test(module.resource)) return false;
									if (/[\\/]\.pnpm[\\/]@arborium\+/.test(module.resource)) return false;
									return true;
								},
								name: 'vendor',
								priority: 10,
								reuseExistingChunk: true,
							},
						}),
					}
				: false,
			runtimeChunk: false,
			moduleIds: isProduction ? 'deterministic' : 'named',
			chunkIds: isProduction ? 'deterministic' : 'named',
			minimize: isProduction,
			minimizer: [
				new SwcJsMinimizerRspackPlugin({
					compress: true,
					mangle: true,
					format: {comments: false},
					exclude: /\.worklet\.js$/,
				}),
				new LightningCssMinimizerRspackPlugin(),
			],
		},
		devServer: {
			port: Number(process.env.FLUXER_APP_DEV_PORT) || DEFAULT_DEV_SERVER_PORT,
			hot: false,
			liveReload: false,
			client: false,
			webSocketServer: false,
			historyApiFallback: true,
			allowedHosts: 'all',
			headers: isDevelopment ? devServerHeaders : devCorsHeaders,
			static: {
				directory: DIST_DIR,
				watch: false,
			},
		},
	};
};
