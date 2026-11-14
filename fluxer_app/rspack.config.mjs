// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {existsSync, readdirSync, readFileSync} from 'node:fs';
import path, {dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {
	Compilation,
	CopyRspackPlugin,
	DefinePlugin,
	HtmlRspackPlugin,
	LightningCssMinimizerRspackPlugin,
	NormalModuleReplacementPlugin,
	SubresourceIntegrityPlugin,
	SwcJsMinimizerRspackPlugin,
	sources,
} from '@rspack/core';
import {transformSync} from 'esbuild';
import {
	BOOT_FRAME_INPUT_HELD_ATTRIBUTE,
	BOOT_FRAME_MAX_REPLAYED_INPUTS,
	BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH,
	BOOT_FRAME_TEXT_TARGET_SELECTOR,
	BOOT_STATE_EXPECTED_ATTRIBUTE,
	BOOT_STATE_EXPECTED_VALUE,
	BOOT_STATE_FORMAT_META_NAME,
	bootStatePayloadFormat,
} from '../packages/desktop_ipc/src/BootStateContract.ts';
import {getLinguiSwcPluginConfig} from './scripts/build/rspack/lingui.mjs';
import {staticFilesPlugin} from './scripts/build/rspack/static-files.mjs';
import {moduleMapPlugin} from './scripts/module-map/index.ts';
import * as BootStateBrowserFormat from './src/features/boot_state/BootStateBrowserFormat.ts';
import {SNAPSHOT_SCHEMA_EPOCH} from './src/features/gateway/snapshot/SnapshotSchemaEpoch.ts';
import {APP_SHELL_BUILD_META_NAME} from './src/features/platform/service_worker/WorkerCacheCleanup.ts';
import * as AppStorageDatabase from './src/features/platform/state/AppStorageDatabase.ts';

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
const BOOT_PRELOAD_CHUNK_GROUPS = ['app-bootstrap'];
const SIGNED_IN_PRELOAD_CHUNK_GROUPS = ['boot-app'];
const SIGNED_IN_PRELOADS_ELEMENT_ID = 'fluxer-boot-app-preloads';
const SIGNED_IN_HINT_STORAGE_KEY = 'fluxer:auth:active-account-key';
const BOOT_PRELOAD_FONT_FILES = [
	'FluxerSans/FluxerSans-Regular.woff2',
	'FluxerSans/FluxerSans-SemiBold.woff2',
	'FluxerSans/FluxerSans-Bold.woff2',
	'FluxerSans/FluxerSans-Italic.woff2',
	'FluxerMono/FluxerMono-Regular.woff2',
];
const BOOT_LOW_PRIORITY_FONT_FILES = ['FluxerSans/FluxerSans-Medium.woff2'];
const AUTH_ENTRY_DOCUMENT = 'auth-index.html';
const ENTRY_STYLESHEET_LINK_PATTERN = /<link\b(?=[^>]*\brel="stylesheet")[^>]*\bhref="([^"]+\.css)"[^>]*>/gu;
const CSS_URL_PATTERN = /url\(\s*['"]?([^'")]+)/gu;
const DESKTOP_MODULE_ASSET_SOURCES = [
	{
		test: /[\\/]fonts[\\/]files[\\/]FluxerSans(JP|KR|SC|TC)[\\/][^\\/]+\.woff2$/u,
		moduleName: (match) => `fluxer_fonts_${match[1].toLowerCase()}`,
	},
	{
		test: /[\\/]@arborium[\\/]([a-z0-9-]+)[\\/]grammar_bg\.wasm$/u,
		moduleName: (match) => `fluxer_grammar_${match[1].replace(/-/gu, '_')}`,
	},
	{
		test: /[\\/]deepfilternet3[\\/](df_bg\.wasm|[^\\/]+\.tar\.gz)$/u,
		moduleName: () => 'fluxer_deepfilter',
	},
	{
		test: /[\\/]onnxruntime-web[\\/]dist[\\/][^\\/]+\.wasm$|[\\/]camera-effects[\\/]models[\\/][^\\/]+\.onnx$/u,
		moduleName: () => 'fluxer_camera_effects',
	},
];

class UnusableDesktopModuleQueryError extends Error {
	constructor(resource) {
		super(`${resource} does not match a registered desktop module asset source.`);
		this.name = 'UnusableDesktopModuleQueryError';
	}
}

function desktopModuleAssetRules(isProduction) {
	return DESKTOP_MODULE_ASSET_SOURCES.map((source) => ({
		test: source.test,
		type: 'asset/resource',
		generator: {
			filename: (pathData) => {
				const match = source.test.exec(pathData.filename ?? '');
				if (match == null) {
					throw new UnusableDesktopModuleQueryError(pathData.filename ?? '');
				}
				const moduleName = source.moduleName(match);
				const extension = pathData.filename.endsWith('.tar.gz') ? '.tar.gz' : '[ext]';
				return isProduction
					? `assets/${moduleName}/[contenthash:16]${extension}`
					: `assets/${moduleName}/[name].[hash]${extension}`;
			},
		},
	}));
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

const ACCEPTED_CSS_ORDER_CONFLICTS = [
	['app/components/dialogs/ConfirmModal', 'ui/button/Button'],
	['app/components/dialogs/Modal', 'ui/button/Button'],
	['app/components/dialogs/components/SettingsModalHeader', 'ui/button/Button'],
	['app/components/dialogs/shared/SettingsHeadingLinkButton', 'ui/button/Button'],
	['ui/tooltip/Tooltip', 'ui/button/Button'],
];

function isAcceptedCssOrderConflict(warning) {
	const message = warning.message ?? '';
	if (!message.includes('Conflicting order between')) {
		return false;
	}
	return ACCEPTED_CSS_ORDER_CONFLICTS.some(([first, second]) => {
		const firstPath = `./src/features/${first}.module.css`;
		const secondPath = `./src/features/${second}.module.css`;
		return (
			message.includes(`between ${firstPath} and ${secondPath}`) ||
			message.includes(`between ${secondPath} and ${firstPath}`)
		);
	});
}

const MAIN_RUNTIME_NAME = 'runtime';

function isMainRuntimeChunk(chunk) {
	const runtime = chunk.runtime;
	if (runtime == null) {
		return chunk.name === 'main' || chunk.name === MAIN_RUNTIME_NAME;
	}
	if (typeof runtime === 'string') {
		return runtime === MAIN_RUNTIME_NAME;
	}
	if (typeof runtime[Symbol.iterator] === 'function') {
		for (const name of runtime) {
			if (name !== MAIN_RUNTIME_NAME) return false;
		}
		return true;
	}
	return false;
}

const SHARED_STYLE_CHUNK_NAME = 'shared-styles';

function sharedStyleChunks(chunk) {
	return isMainRuntimeChunk(chunk) && !isWorkerPath({chunk});
}

const SHARED_STYLE_CACHE_GROUPS = {
	sharedStyles: {
		type: /^css/,
		name: SHARED_STYLE_CHUNK_NAME,
		minChunks: 2,
		chunks: sharedStyleChunks,
		enforce: true,
		priority: 70,
	},
	uiStyles: {
		type: /^css/,
		test: /[\\/]src[\\/]features[\\/]ui[\\/]/,
		name: SHARED_STYLE_CHUNK_NAME,
		chunks: sharedStyleChunks,
		enforce: true,
		priority: 70,
	},
};

function mirrorCacheGroups(groups) {
	const mirrored = {...SHARED_STYLE_CACHE_GROUPS};
	for (const [key, originalGroup] of Object.entries(groups)) {
		const group = isDesktopRenderer ? {...originalGroup, name: false} : originalGroup;
		if (group.chunks != null) {
			mirrored[key] = group;
			continue;
		}
		mirrored[key] = {...group, chunks: (chunk) => isMainRuntimeChunk(chunk) && chunk.canBeInitial()};
		mirrored[`${key}Async`] = {
			...group,
			name: isDesktopRenderer ? false : `${group.name}-async`,
			chunks: (chunk) => isMainRuntimeChunk(chunk) && !chunk.canBeInitial(),
		};
	}
	return mirrored;
}

const ENTRY_RECOVERY_ATTRIBUTE = 'data-fluxer-entry';

function subresourceIntegrity(source) {
	return `sha384-${createHash('sha384').update(source).digest('base64')}`;
}

function entryRecoveryMarkerPlugin() {
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('EntryRecoveryMarkerPlugin', (compilation) => {
				HtmlRspackPlugin.getCompilationHooks(compilation).alterAssetTags.tap('EntryRecoveryMarkerPlugin', (data) => {
					for (const tag of [...data.assetTags.scripts, ...data.assetTags.styles]) {
						tag.attributes = {...tag.attributes, [ENTRY_RECOVERY_ATTRIBUTE]: ''};
					}
					return data;
				});
			});
		},
	};
}

const INLINE_SCRIPTS_DIR = path.join(ROOT_DIR, 'inline_scripts');
const INLINE_SCRIPT_TARGETS = ['chrome111', 'edge111', 'firefox114', 'safari16.4'];
const INLINE_SCRIPT_MARKER_PATTERN = /<script data-inline-script="([a-z-]+)"><\/script>/gu;

class InlineScriptError extends Error {
	constructor(name, reason) {
		super(`Cannot inline ${name}.js: ${reason}`);
		this.name = 'InlineScriptError';
	}
}

function inlineScriptTag(name, replacements = {}) {
	let source = readFileSync(path.join(INLINE_SCRIPTS_DIR, `${name}.js`), 'utf8');
	for (const [token, value] of Object.entries(replacements)) {
		source = source.replaceAll(token, value);
	}
	const code = transformSync(source, {minify: true, target: INLINE_SCRIPT_TARGETS, legalComments: 'none'}).code.trim();
	if (/<\/script/iu.test(code)) {
		throw new InlineScriptError(name, 'it contains a closing script tag');
	}
	return `<script data-cfasync="false" nonce="{{CSP_NONCE_PLACEHOLDER}}">${code}</script>`;
}

function inlineScriptReplacements(buildVersion) {
	return {
		'boot-state': {
			...Object.fromEntries(
				Object.entries({
					...BootStateBrowserFormat,
					...AppStorageDatabase,
					BOOT_FRAME_MAX_HOLD_MS: 30_000,
					BOOT_FRAME_COMPOSER_PLACEHOLDER_SELECTOR: 'flx-lexical-composer-input-placeholder',
					BOOT_FRAME_STYLESHEET_TIMEOUT_MS: 10_000,
					BOOT_FRAME_GATE_TIMEOUT_MS: 400,
					BOOT_FRAME_IMAGE_WAIT_MS: 40,
					BOOT_FRAME_CLICK_IDENTITY_SELECTOR:
						'[data-scroll-id],[data-channel-id],[data-guild-id],[data-user-id],[data-message-id]',
					BOOT_FRAME_INPUT_HELD_ATTRIBUTE,
					BOOT_FRAME_MAX_REPLAYED_INPUTS,
					BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH,
					BOOT_FRAME_TEXT_TARGET_SELECTOR,
					BOOT_STATE_EXPECTED_ATTRIBUTE,
					BOOT_STATE_EXPECTED_VALUE,
				}).map(([name, value]) => [`{{${name}}}`, String(value)]),
			),
			'{{BOOT_STATE_FORMAT}}': bootStatePayloadFormat(SNAPSHOT_SCHEMA_EPOCH),
			'{{BOOT_FRAME_BUILD}}': buildVersion,
		},
		'boot-shell': {
			'{{BOOT_FRAME_ELEMENT_ID}}': BootStateBrowserFormat.BOOT_FRAME_ELEMENT_ID,
		},
	};
}

function inlineScriptsPlugin(buildVersion) {
	const replacements = inlineScriptReplacements(buildVersion);
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('InlineScriptsPlugin', (compilation) => {
				HtmlRspackPlugin.getCompilationHooks(compilation).beforeEmit.tap('InlineScriptsPlugin', (data) => {
					data.html = data.html.replace(INLINE_SCRIPT_MARKER_PATTERN, (_marker, name) =>
						inlineScriptTag(name, replacements[name]),
					);
					return data;
				});
			});
		},
	};
}

function bootPreloadPlugin({withIntegrity}) {
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
						const collectFiles = (groupNames, excluded) => {
							const files = new Set();
							for (const name of groupNames) {
								for (const group of compilation.chunkGroups) {
									if (group.name !== name) {
										continue;
									}
									for (const file of group.getFiles()) {
										if (!html.includes(file) && !excluded.has(file)) {
											files.add(file);
										}
									}
								}
							}
							return [...files].sort((a, b) => Number(b.endsWith('.css')) - Number(a.endsWith('.css')));
						};
						const integrity = (file) => {
							if (!withIntegrity) {
								return null;
							}
							const source = compilation.getAsset(file)?.source.buffer();
							return source == null ? null : subresourceIntegrity(source);
						};
						const preloadType = (file) => (file.endsWith('.css') ? 'style' : 'script');
						const bootFiles = collectFiles(BOOT_PRELOAD_CHUNK_GROUPS, new Set());
						const links = bootFiles
							.map((file) => {
								const hash = integrity(file);
								const integrityAttributes = hash == null ? '' : ` crossorigin integrity="${hash}"`;
								return `<link rel="preload" as="${preloadType(file)}" fetchpriority="low" href="${base}${file}"${integrityAttributes}>`;
							})
							.join('');
						const signedInPreloads = collectFiles(SIGNED_IN_PRELOAD_CHUNK_GROUPS, new Set(bootFiles)).map((file) => {
							const hash = integrity(file);
							return {href: `${base}${file}`, as: preloadType(file), ...(hash == null ? {} : {integrity: hash})};
						});
						const signedInPreloadData = JSON.stringify(signedInPreloads).replaceAll('<', '\\u003c');
						const signedInPreloadTags =
							signedInPreloads.length === 0
								? ''
								: `<script type="application/json" id="${SIGNED_IN_PRELOADS_ELEMENT_ID}">${signedInPreloadData}</script>` +
									inlineScriptTag('signed-in-preloads', {
										'{{SIGNED_IN_HINT_STORAGE_KEY}}': SIGNED_IN_HINT_STORAGE_KEY,
										'{{SIGNED_IN_PRELOADS_ELEMENT_ID}}': SIGNED_IN_PRELOADS_ELEMENT_ID,
										'{{BOOT_FRAME_GLOBAL}}': BootStateBrowserFormat.BOOT_FRAME_GLOBAL,
									});
						if (links === '' && signedInPreloadTags === '') {
							return;
						}
						compilation.updateAsset(
							'index.html',
							new sources.RawSource(html.replace('</body>', () => `${links}${signedInPreloadTags}</body>`)),
						);
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

function rocketLoaderOptOutPlugin() {
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('RocketLoaderOptOutPlugin', (compilation) => {
				HtmlRspackPlugin.getCompilationHooks(compilation).alterAssetTags.tap('RocketLoaderOptOutPlugin', (data) => {
					for (const tag of data.assetTags.scripts) {
						tag.attributes = {'data-cfasync': 'false', ...tag.attributes};
					}
					return data;
				});
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

const LIBFLUXCORE_WASM_URL_PLACEHOLDER = '{{LIBFLUXCORE_WASM_URL}}';
const LIBFLUXCORE_WASM_SOURCE = 'pkgs/libfluxcore/libfluxcore_bg.wasm';

function libfluxcoreWasmUrlPlugin({publicPath}) {
	return {
		apply(compiler) {
			compiler.hooks.thisCompilation.tap('LibfluxcoreWasmUrlPlugin', (compilation) => {
				HtmlRspackPlugin.getCompilationHooks(compilation).beforeEmit.tap('LibfluxcoreWasmUrlPlugin', (data) => {
					if (!data.html.includes(LIBFLUXCORE_WASM_URL_PLACEHOLDER)) {
						return data;
					}
					const asset = compilation
						.getAssets()
						.find((candidate) => String(candidate.info.sourceFilename ?? '').endsWith(LIBFLUXCORE_WASM_SOURCE));
					if (asset == null) {
						throw new Error(`Cannot preload ${LIBFLUXCORE_WASM_SOURCE}: no emitted asset came from it`);
					}
					data.html = data.html.replaceAll(LIBFLUXCORE_WASM_URL_PLACEHOLDER, `${publicPath}${asset.name}`);
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

const RENAMED_BUILD_ENV = {
	PUBLIC_BUILD_VERSION: 'BUILD_VERSION',
	PUBLIC_RELEASE_CHANNEL: 'BUILD_CHANNEL',
	RELEASE_CHANNEL: 'BUILD_CHANNEL',
};

function rejectRenamedBuildEnv() {
	for (const [oldName, newName] of Object.entries(RENAMED_BUILD_ENV)) {
		if (envString(oldName) !== undefined && envString(newName) === undefined) {
			throw new Error(`The web build no longer reads ${oldName}. Set ${newName} instead.`);
		}
	}
}

function resolveBuildChannel() {
	const value = envString('BUILD_CHANNEL', 'canary').trim().toLowerCase();
	if (value === 'stable' || value === 'canary' || value === 'development') {
		return value;
	}
	throw new Error(`BUILD_CHANNEL must be stable, canary, or development, received ${JSON.stringify(value)}`);
}

function resolveBuildInfo() {
	rejectRenamedBuildEnv();
	return {
		version: envString('BUILD_VERSION', 'dev'),
		channel: resolveBuildChannel(),
	};
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
	const buildInfo = resolveBuildInfo();
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
					sw: {import: path.join(SRC_DIR, 'features', 'platform', 'service_worker', 'Worker.ts'), runtime: false},
				},
		output: {
			path: DIST_DIR,
			publicPath,
			workerPublicPath: '/',
			workerChunkLoading: false,
			crossOriginLoading: isProduction && !isDesktopRenderer ? 'anonymous' : false,
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
		ignoreWarnings: [isAcceptedCssOrderConflict],
		resolve: {
			alias: {
				...resolveArboriumWasmAliases(),
				'@arborium/arborium/arborium_host_bg.wasm': path.resolve(
					ROOT_DIR,
					'node_modules/@arborium/arborium/dist/arborium_host_bg.wasm',
				),
				...Object.fromEntries(
					[
						'@app/features/user/components/modals/UserProfileModal$',
						'@app/features/guild/components/modals/AddGuildModal$',
						'@app/features/messaging/components/modals/ForwardModal$',
						'@app/features/messaging/components/modals/ImageCropModal$',
						'@app/features/guild/components/modals/GuildNotificationSettingsModal$',
						'@app/features/invite/components/modals/InviteModal$',
						'@app/features/channel/components/modals/ChannelCreateModal$',
						'@app/features/channel/components/modals/ChannelFollowModal$',
						'@app/features/channel/components/modals/EditGroupModal$',
						'@app/features/ui/action_menu/DMContextMenu$',
						'@app/features/ui/action_menu/GroupDMContextMenu$',
						'@app/features/ui/action_menu/VoiceParticipantContextMenu$',
						'@app/features/ui/action_menu/MediaContextMenu$',
						'@app/features/ui/action_menu/FavoritesChannelContextMenu$',
						'@app/features/ui/action_menu/FluxerButtonContextMenu$',
						'@app/features/ui/action_menu/SettingsContextMenu$',
						'@app/features/messaging/components/modals/MessageCrosspostConfirmModal$',
						'@app/features/auth/components/modals/ClaimAccountModal$',
						'@app/features/auth/components/modals/RecoveryKitModal$',
						'@app/features/premium/components/modals/GiftPlutoniumModal$',
						'@app/features/voice/components/bottomsheets/VoiceParticipantBottomSheet$',
						'@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet$',
						'@app/features/ui/action_menu/GuildFolderContextMenu$',
						'@app/features/channel/components/textarea/TextareaContextMenu$',
						'@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet$',
						'@app/features/channel/components/bottomsheets/ChannelSearchBottomSheet$',
						'@app/features/relationship/components/modals/AddFriendsToGroupModal$',
						'@app/features/user/components/modals/CustomStatusModal$',
						'@app/features/channel/components/modals/CreateDMModal$',
						'@app/features/channel/components/modals/GroupInvitesModal$',
						'@app/features/guild/components/modals/GuildFolderSettingsModal$',
						'@app/features/app/components/dialogs/ConfirmModal$',
						'@app/features/auth/components/modals/OAuthAuthorizeModal$',
						'@app/features/premium/components/PremiumCheckoutReturnWatcher$',
						'@app/features/channel/components/EmojiPicker$',
						'@app/features/channel/components/StickersPicker$',
						'@app/features/channel/components/pickers/gif/GifPicker$',
						'@app/features/channel/components/pickers/memes/MemesPicker$',
						'@app/features/expressions/components/modals/ExpressionPickerSheet$',
						'@app/features/ui/action_menu/UserContextMenu$',
						'@app/features/ui/action_menu/GuildMemberContextMenu$',
						'@app/features/ui/action_menu/MessageContextMenu$',
						'@app/features/ui/action_menu/GuildContextMenu$',
						'@app/features/ui/action_menu/ChannelContextMenu$',
					].map((request) => [request, path.join(SRC_DIR, 'app/desktop/DesktopDeferredComponents.tsx')]),
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
				'@fluxer': path.join(MONOREPO_ROOT, 'packages'),
				'@pkgs': PKGS_DIR,
				'assert/strict': BROWSER_ASSERT_STRICT_MODULE,
				'livekit-client$': path.join(PKGS_DIR, 'livekit-client/src/index.ts'),
				'livekit-client/e2ee-worker': path.join(PKGS_DIR, 'livekit-client/src/e2ee/worker/e2ee.worker.ts'),
				'node:assert/strict': BROWSER_ASSERT_STRICT_MODULE,
			},
			extensions: ['.web.tsx', '.web.ts', '.web.jsx', '.web.js', '.tsx', '.ts', '.jsx', '.js', '.json', '.mjs', '.cjs'],
			extensionAlias: {
				'.js': ['.js', '.tsx', '.ts'],
				'.mjs': ['.mjs', '.mts'],
				'.cjs': ['.cjs', '.cts'],
			},
			conditionNames: ['import', 'module', 'webpack', 'browser', 'default'],
			mainFields: ['browser', 'module', 'main'],
		},
		module: {
			...(isDesktopRenderer ? {parser: {javascript: {exportsPresence: 'error'}}} : {}),
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
								target: 'es2022',
							},
						},
					},
				},
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
									target: 'es2022',
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
				...(isDesktopRenderer ? desktopModuleAssetRules(isProduction) : []),
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
			...(isDesktopRenderer
				? [
						new NormalModuleReplacementPlugin(/^livekit-client$/, (resource) => {
							if (resource.contextInfo.issuer.endsWith('/VoiceEngineV2AppCameraResolutionPresets.ts')) {
								resource.request = path.join(PKGS_DIR, 'livekit-client/src/room/track/options.ts');
							}
						}),
					]
				: []),
			nodeAssertStrictSchemePlugin(),
			inlineScriptsPlugin(buildInfo.version),
			fontPreloadPlugin(),
			libfluxcoreWasmUrlPlugin({publicPath: workerWasmPublicPath ?? publicPath}),
			rocketLoaderOptOutPlugin(),
			...(isProduction ? [bootPreloadPlugin({withIntegrity: !isDesktopRenderer})] : []),
			...(isProduction && !isDesktopRenderer ? [entryRecoveryMarkerPlugin()] : []),
			...(isProduction && !isDesktopRenderer ? [authEntryDocumentPlugin()] : []),
			...(isProduction && !isDesktopRenderer
				? [new SubresourceIntegrityPlugin({hashFuncNames: ['sha384'], enabled: true})]
				: []),
			new HtmlRspackPlugin({
				template: path.join(ROOT_DIR, 'index.html'),
				filename: 'index.html',
				hash: isDevelopment,
				inject: 'body',
				scriptLoading: 'module',
				excludeChunks: ['sw'],
				...(isDesktopRenderer
					? {publicPath: '/', meta: {[BOOT_STATE_FORMAT_META_NAME]: bootStatePayloadFormat(SNAPSHOT_SCHEMA_EPOCH)}}
					: {meta: {[APP_SHELL_BUILD_META_NAME]: buildInfo.version}}),
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
				define: 'undefined',
				__FLUXER_PRECACHE_MANIFEST__: JSON.stringify([]),
				__FLUXER_SW_VERSION__: JSON.stringify(buildInfo.version),
				'process.env.NODE_ENV': JSON.stringify(mode),
				'import.meta.env.DEV': JSON.stringify(isDevelopment),
				'import.meta.env.PROD': JSON.stringify(isProduction),
				'import.meta.env.MODE': JSON.stringify(mode),
				'import.meta.env.BUILD_VERSION': JSON.stringify(buildInfo.version),
				'import.meta.env.BUILD_CHANNEL': JSON.stringify(buildInfo.channel),
			}),
			{
				apply(compiler) {
					compiler.hooks.thisCompilation.tap('VersionJsonPlugin', (compilation) => {
						const {Compilation} = compiler.rspack;
						compilation.hooks.processAssets.tap(
							{name: 'VersionJsonPlugin', stage: Compilation.PROCESS_ASSETS_STAGE_ADDITIONAL},
							() => {
								const {RawSource} = compiler.rspack.sources;
								const versionData = {
									version: buildInfo.version,
								};
								const source = new RawSource(JSON.stringify(versionData));
								compilation.emitAsset('version.json', source);
							},
						);
					});
				},
			},
			...(isProduction
				? [
						moduleMapPlugin({
							root: MONOREPO_ROOT,
							build: {...buildInfo, desktopRenderer: isDesktopRenderer},
						}),
					]
				: []),
		],
		optimization: {
			splitChunks: isProduction
				? {
						chunks: (chunk) => isMainRuntimeChunk(chunk),
						maxInitialRequests: 15,
						maxAsyncSize: 2_000_000,
						cacheGroups: mirrorCacheGroups({
							reactServer: {
								test: /[\\/]node_modules[\\/]react-dom[\\/](server\.|cjs[\\/]react-dom-server)/,
								name: 'react-server',
								priority: 61,
								reuseExistingChunk: true,
								enforce: true,
								maxAsyncSize: Number.POSITIVE_INFINITY,
								chunks: (chunk) => !chunk.canBeInitial() && !isWorkerPath({chunk}),
							},
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
							plutoniumAtom: {
								test: /[\\/]node_modules[\\/]three[\\/]/,
								name: 'plutonium-atom',
								priority: 54,
								reuseExistingChunk: true,
								enforce: true,
								maxAsyncSize: Number.POSITIVE_INFINITY,
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
								test: /[\\/]node_modules[\\/](framer-motion|motion-dom|motion-utils)[\\/]/,
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
								chunks: (chunk) => isMainRuntimeChunk(chunk) && chunk.canBeInitial(),
							},
						}),
					}
				: false,
			runtimeChunk: {name: MAIN_RUNTIME_NAME},
			moduleIds: isProduction ? 'deterministic' : 'named',
			chunkIds: isProduction ? 'deterministic' : 'named',
			minimize: isProduction,
			minimizer: [
				new SwcJsMinimizerRspackPlugin({
					minimizerOptions: {
						compress: true,
						mangle: true,
						format: {
							comments: false,
							asciiOnly: isDesktopRenderer,
							...(isDesktopRenderer ? {preamble: '//# allFunctionsCalledOnLoad\n'} : {}),
						},
					},
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
