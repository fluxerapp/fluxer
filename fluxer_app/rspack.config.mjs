// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, mkdirSync, readdirSync, writeFileSync} from 'node:fs';
import path, {dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {CopyRspackPlugin, DefinePlugin, HtmlRspackPlugin, SwcJsMinimizerRspackPlugin} from '@rspack/core';
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
	if (value === 'stable' || value === 'canary') {
		return value;
	}
	return 'canary';
}

function resolvePublicValues() {
	return {
		PUBLIC_API_VERSION: envString('PUBLIC_API_VERSION', '1'),
		PUBLIC_BUILD_VERSION: envString('PUBLIC_BUILD_VERSION', envString('BUILD_VERSION', 'dev')),
		PUBLIC_RELEASE_CHANNEL: resolveReleaseChannel(),
		PUBLIC_BOOTSTRAP_API_ENDPOINT: envString('PUBLIC_BOOTSTRAP_API_ENDPOINT', '/api'),
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: envString('PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT'),
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
	const assetBaseUrl = envString('PUBLIC_ASSET_BASE_URL');
	const staticCdnEndpoint = envString(
		'PUBLIC_STATIC_CDN_ENDPOINT',
		envString('FLUXER_STATIC_CDN_ENDPOINT', isProduction ? '' : ''),
	);
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
		entry: {
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
					test: /\.(tsx|ts|jsx|js)$/,
					exclude: /node_modules/,
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
									decorators: true,
								},
								transform: {
									legacyDecorator: true,
									decoratorMetadata: true,
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
					parser: {namedExports: false},
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
					test: /\.(png|jpg|jpeg|gif|webp|ico|woff|woff2|ttf|eot|mp3|wav|ogg|mp4|webm)$/,
					type: 'asset/resource',
					generator: {
						filename: isProduction ? 'assets/[contenthash:16][ext]' : 'assets/[name].[hash][ext]',
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
			new HtmlRspackPlugin({
				template: path.join(ROOT_DIR, 'index.html'),
				filename: 'index.html',
				hash: isDevelopment,
				inject: 'body',
				scriptLoading: 'module',
				excludeChunks: ['sw'],
			}),
			new CopyRspackPlugin({
				patterns: [
					{
						from: PUBLIC_DIR,
						to: DIST_DIR,
						noErrorOnMissing: true,
					},
				],
			}),
			staticFilesPlugin({
				staticCdnEndpoint: normalizedStaticCdnEndpoint,
				fontsDir: path.join(MONOREPO_ROOT, 'packages', 'fonts'),
			}),
			new DefinePlugin({
				__FLUXER_PRECACHE_MANIFEST__: JSON.stringify([]),
				__FLUXER_SW_VERSION__: JSON.stringify(publicValues.PUBLIC_BUILD_VERSION || 'dev'),
				'process.env.NODE_ENV': JSON.stringify(mode),
				'import.meta.env.DEV': JSON.stringify(isDevelopment),
				'import.meta.env.PROD': JSON.stringify(isProduction),
				'import.meta.env.MODE': JSON.stringify(mode),
				'import.meta.env.PUBLIC_BUILD_VERSION': getPublicEnvVar(publicValues, 'PUBLIC_BUILD_VERSION'),
				'import.meta.env.PUBLIC_API_VERSION': getPublicEnvVar(publicValues, 'PUBLIC_API_VERSION'),
				'import.meta.env.PUBLIC_RELEASE_CHANNEL': getPublicEnvVar(publicValues, 'PUBLIC_RELEASE_CHANNEL'),
				'import.meta.env.PUBLIC_BOOTSTRAP_API_ENDPOINT': getPublicEnvVar(publicValues, 'PUBLIC_BOOTSTRAP_API_ENDPOINT'),
				'import.meta.env.PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT': getPublicEnvVar(
					publicValues,
					'PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT',
				),
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
						cacheGroups: {
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
							livekit: {
								test: /[\\/]node_modules[\\/](livekit-client|@livekit)[\\/]/,
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
								test: /[\\/]node_modules[\\/](framer-motion|motion)[\\/]/,
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
							reactAria: {
								test: /[\\/]node_modules[\\/]react-aria-components[\\/]/,
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
								test: /[\\/]node_modules[\\/](lodash|clsx|qrcode|thumbhash|bowser|match-sorter)[\\/]/,
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
						},
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
				}),
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
		experiments: {css: true},
	};
};
