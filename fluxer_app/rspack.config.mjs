/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {execSync} from 'node:child_process';
import fs from 'node:fs';
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

const CDN_ENDPOINT = 'https://fluxerstatic.com';

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

const mode = resolveMode();
const isProduction = mode === 'production';
const isDevelopment = !isProduction;
const devJsName = 'assets/[name].js';
const devCssName = 'assets/[name].css';

function isPlainObject(value) {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function readConfig() {
	const configPath = process.env.FLUXER_CONFIG;
	if (!configPath) {
		throw new Error('FLUXER_CONFIG must be set to a JSON config path.');
	}
	const resolvedConfigPath = path.isAbsolute(configPath) ? configPath : path.resolve(MONOREPO_ROOT, configPath);
	const content = fs.readFileSync(resolvedConfigPath, 'utf-8');
	const parsed = JSON.parse(content);
	if (!isPlainObject(parsed)) {
		throw new Error('Invalid JSON config: expected an object at root.');
	}
	return parsed;
}

function getValue(source, path, fallback) {
	let current = source;
	for (const segment of path) {
		if (!isPlainObject(current)) {
			return fallback;
		}
		current = current[segment];
	}
	return current ?? fallback;
}

function asString(value, fallback = undefined) {
	if (value === null || value === undefined) {
		return fallback;
	}
	if (typeof value === 'string') {
		return value;
	}
	if (typeof value === 'number' || typeof value === 'boolean') {
		return String(value);
	}
	return fallback;
}

function normalizeOverride(value) {
	const trimmed = asString(value, '')?.trim() ?? '';
	return trimmed === '' ? undefined : trimmed;
}

function buildUrl(scheme, domain, port, path = '') {
	const isStandardPort =
		(scheme === 'http' && port === 80) ||
		(scheme === 'https' && port === 443) ||
		(scheme === 'ws' && port === 80) ||
		(scheme === 'wss' && port === 443);
	const portPart = port && !isStandardPort ? `:${port}` : '';
	return `${scheme}://${domain}${portPart}${path}`;
}

function deriveDomain(endpointType, config) {
	switch (endpointType) {
		case 'staticCdn':
			return config.static_cdn_domain || config.base_domain;
		case 'invite':
			return config.invite_domain || config.base_domain;
		case 'gift':
			return config.gift_domain || config.base_domain;
		default:
			return config.base_domain;
	}
}

function deriveEndpointsFromDomain(domainConfig, overrides) {
	const publicScheme = domainConfig.public_scheme ?? 'https';
	const publicPort = domainConfig.public_port ?? (publicScheme === 'https' ? 443 : 80);
	const gatewayScheme = publicScheme === 'https' ? 'wss' : 'ws';
	const derived = {
		api: buildUrl(publicScheme, deriveDomain('api', domainConfig), publicPort, '/api'),
		app: buildUrl(publicScheme, deriveDomain('app', domainConfig), publicPort),
		gateway: buildUrl(gatewayScheme, deriveDomain('gateway', domainConfig), publicPort, '/gateway'),
		media: buildUrl(publicScheme, deriveDomain('media', domainConfig), publicPort, '/media'),
		staticCdn: buildUrl(publicScheme, deriveDomain('staticCdn', domainConfig), publicPort, '/static'),
		admin: buildUrl(publicScheme, deriveDomain('admin', domainConfig), publicPort, '/admin'),
		marketing: buildUrl(publicScheme, deriveDomain('marketing', domainConfig), publicPort, '/marketing'),
		invite: buildUrl(publicScheme, deriveDomain('invite', domainConfig), publicPort, '/invite'),
		gift: buildUrl(publicScheme, deriveDomain('gift', domainConfig), publicPort, '/gift'),
	};
	const normalizedOverrides = {
		api: normalizeOverride(overrides?.api),
		app: normalizeOverride(overrides?.app),
		gateway: normalizeOverride(overrides?.gateway),
		media: normalizeOverride(overrides?.media),
		staticCdn: normalizeOverride(overrides?.static_cdn),
		admin: normalizeOverride(overrides?.admin),
		marketing: normalizeOverride(overrides?.marketing),
		invite: normalizeOverride(overrides?.invite),
		gift: normalizeOverride(overrides?.gift),
	};
	return {
		api: normalizedOverrides.api ?? derived.api,
		app: normalizedOverrides.app ?? derived.app,
		gateway: normalizedOverrides.gateway ?? derived.gateway,
		media: normalizedOverrides.media ?? derived.media,
		staticCdn: normalizedOverrides.staticCdn ?? derived.staticCdn,
		admin: normalizedOverrides.admin ?? derived.admin,
		marketing: normalizedOverrides.marketing ?? derived.marketing,
		invite: normalizedOverrides.invite ?? derived.invite,
		gift: normalizedOverrides.gift ?? derived.gift,
	};
}

function stripApiSuffix(url) {
	if (!url) {
		return url;
	}
	return url.endsWith('/api') ? url.slice(0, -4) : url;
}

function resolveAppPublic(config) {
	const appPublic = getValue(config, ['app_public'], {});
	const domain = getValue(config, ['domain'], {});
	const overrides = getValue(config, ['endpoint_overrides'], {});
	const endpoints = deriveEndpointsFromDomain(domain, overrides);
	const defaultBootstrapEndpoint = endpoints.api;
	const defaultPublicEndpoint = stripApiSuffix(endpoints.api);
	const sentryDsn = asString(appPublic.sentry_dsn);
	return {
		apiVersion: asString(appPublic.api_version, '1'),
		bootstrapApiEndpoint: asString(appPublic.bootstrap_api_endpoint, defaultBootstrapEndpoint),
		bootstrapApiPublicEndpoint: asString(appPublic.bootstrap_api_public_endpoint, defaultPublicEndpoint),
		relayDirectoryUrl: asString(appPublic.relay_directory_url),
		sentryDsn,
	};
}

function resolveReleaseChannel() {
	const raw = asString(process.env.RELEASE_CHANNEL, 'nightly').toLowerCase();
	if (raw === 'stable' || raw === 'canary') {
		return raw;
	}
	return 'nightly';
}

function resolveBuildMetadata() {
	const envBuildSha = asString(process.env.BUILD_SHA);
	let buildSha = envBuildSha ?? undefined;
	if (!buildSha) {
		try {
			buildSha = execSync('git rev-parse --short HEAD', {cwd: ROOT_DIR, stdio: ['ignore', 'pipe', 'ignore']})
				.toString()
				.trim();
		} catch {
			buildSha = 'dev';
		}
	}
	const buildNumber = asString(process.env.BUILD_NUMBER, '0');
	const buildTimestamp = asString(process.env.BUILD_TIMESTAMP, String(Math.floor(Date.now() / 1000)));
	const releaseChannel = resolveReleaseChannel();
	return {
		buildSha,
		buildNumber,
		buildTimestamp,
		releaseChannel,
	};
}

function getPublicEnvVar(values, name) {
	const value = values[name];
	return value === undefined ? 'undefined' : JSON.stringify(value);
}

export default () => {
	const linguiSwcPlugin = getLinguiSwcPluginConfig();
	const config = readConfig();
	const appPublic = resolveAppPublic(config);
	const buildMetadata = resolveBuildMetadata();
	const publicValues = {
		PUBLIC_BUILD_SHA: buildMetadata.buildSha,
		PUBLIC_BUILD_NUMBER: buildMetadata.buildNumber,
		PUBLIC_BUILD_TIMESTAMP: buildMetadata.buildTimestamp,
		PUBLIC_RELEASE_CHANNEL: buildMetadata.releaseChannel,
		PUBLIC_SENTRY_DSN: appPublic.sentryDsn ?? null,
		PUBLIC_API_VERSION: appPublic.apiVersion,
		PUBLIC_BOOTSTRAP_API_ENDPOINT: appPublic.bootstrapApiEndpoint,
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: appPublic.bootstrapApiPublicEndpoint ?? appPublic.bootstrapApiEndpoint,
		PUBLIC_RELAY_DIRECTORY_URL: appPublic.relayDirectoryUrl ?? null,
	};

	return {
		mode,

		entry: {
			main: path.join(SRC_DIR, 'index.tsx'),
			sw: path.join(SRC_DIR, 'service_worker', 'Worker.tsx'),
		},

		output: {
			path: DIST_DIR,
			publicPath: isProduction ? `${CDN_ENDPOINT}/` : '/',
			workerPublicPath: '/',
			filename: (pathData) => {
				if (pathData.chunk?.name === 'sw') {
					return 'sw.js';
				}
				return isProduction ? 'assets/[contenthash:16].js' : devJsName;
			},
			chunkFilename: isProduction ? 'assets/[contenthash:16].js' : devJsName,
			cssFilename: isProduction ? 'assets/[contenthash:16].css' : devCssName,
			cssChunkFilename: isProduction ? 'assets/[contenthash:16].css' : devCssName,
			assetModuleFilename: isProduction ? 'assets/[contenthash:16][ext]' : 'assets/[name].[hash][ext]',
			webAssemblyModuleFilename: isProduction ? 'assets/[contenthash:16].wasm' : 'assets/[name].[hash].wasm',
			clean: true,
		},

		devtool: 'source-map',

		target: ['web', 'browserslist'],

		resolve: {
			alias: {
				'~': SRC_DIR,
				'@app': SRC_DIR,
				'@pkgs': PKGS_DIR,
				'@fluxer/constants/src': path.join(MONOREPO_ROOT, 'packages/constants/src'),
				'@fluxer/date_utils/src': path.join(MONOREPO_ROOT, 'packages/date_utils/src'),
				'@fluxer/geo_utils/src': path.join(MONOREPO_ROOT, 'packages/geo_utils/src'),
				'@fluxer/limits/src': path.join(MONOREPO_ROOT, 'packages/limits/src'),
				'@fluxer/list_utils/src': path.join(MONOREPO_ROOT, 'packages/list_utils/src'),
				'@fluxer/markdown_parser/src': path.join(MONOREPO_ROOT, 'packages/markdown_parser/src'),
				'@fluxer/number_utils/src': path.join(MONOREPO_ROOT, 'packages/number_utils/src'),
				'@fluxer/schema/src': path.join(MONOREPO_ROOT, 'packages/schema/src'),
				'@fluxer/snowflake/src': path.join(MONOREPO_ROOT, 'packages/snowflake/src'),
				'@fluxer/ui/src': path.join(MONOREPO_ROOT, 'packages/ui/src'),
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
		},

		module: {
			rules: [
				{
					test: /\.(tsx|ts|jsx|js)$/,
					exclude: /node_modules/,
					type: 'javascript/auto',
					parser: {
						dynamicImport: false,
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
			new HtmlRspackPlugin({
				template: path.join(ROOT_DIR, 'index.html'),
				filename: 'index.html',
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

			staticFilesPlugin({staticCdnEndpoint: CDN_ENDPOINT}),

			new DefinePlugin({
				'process.env.NODE_ENV': JSON.stringify(mode),
				'import.meta.env.DEV': JSON.stringify(isDevelopment),
				'import.meta.env.PROD': JSON.stringify(isProduction),
				'import.meta.env.MODE': JSON.stringify(mode),
				'import.meta.env.PUBLIC_BUILD_SHA': getPublicEnvVar(publicValues, 'PUBLIC_BUILD_SHA'),
				'import.meta.env.PUBLIC_BUILD_NUMBER': getPublicEnvVar(publicValues, 'PUBLIC_BUILD_NUMBER'),
				'import.meta.env.PUBLIC_BUILD_TIMESTAMP': getPublicEnvVar(publicValues, 'PUBLIC_BUILD_TIMESTAMP'),
				'import.meta.env.PUBLIC_RELEASE_CHANNEL': getPublicEnvVar(publicValues, 'PUBLIC_RELEASE_CHANNEL'),
				'import.meta.env.PUBLIC_SENTRY_DSN': getPublicEnvVar(publicValues, 'PUBLIC_SENTRY_DSN'),
				'import.meta.env.PUBLIC_API_VERSION': getPublicEnvVar(publicValues, 'PUBLIC_API_VERSION'),
				'import.meta.env.PUBLIC_BOOTSTRAP_API_ENDPOINT': getPublicEnvVar(publicValues, 'PUBLIC_BOOTSTRAP_API_ENDPOINT'),
				'import.meta.env.PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT': getPublicEnvVar(
					publicValues,
					'PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT',
				),
				'import.meta.env.PUBLIC_RELAY_DIRECTORY_URL': getPublicEnvVar(publicValues, 'PUBLIC_RELAY_DIRECTORY_URL'),
			}),
		],

		optimization: {
			splitChunks: isProduction
				? {
						chunks: (chunk) => chunk.name !== 'sw',
						maxInitialRequests: 50,
						cacheGroups: {
							icons: {
								test: /[\\/]node_modules[\\/]@phosphor-icons[\\/]/,
								name: 'icons',
								priority: 60,
								reuseExistingChunk: true,
							},
							highlight: {
								test: /[\\/]node_modules[\\/]highlight\.js[\\/]/,
								name: 'highlight',
								priority: 55,
								reuseExistingChunk: true,
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
							sentry: {
								test: /[\\/]node_modules[\\/]@sentry[\\/]/,
								name: 'sentry',
								priority: 41,
								reuseExistingChunk: true,
							},
							reactAria: {
								test: /[\\/]node_modules[\\/]react-aria-components[\\/]/,
								name: 'react-aria',
								priority: 40,
								reuseExistingChunk: true,
							},
							validation: {
								test: /[\\/]node_modules[\\/](valibot)[\\/]/,
								name: 'validation',
								priority: 38,
								reuseExistingChunk: true,
							},
							datetime: {
								test: /[\\/]node_modules[\\/]luxon[\\/]/,
								name: 'datetime',
								priority: 37,
								reuseExistingChunk: true,
							},
							observable: {
								test: /[\\/]node_modules[\\/]rxjs[\\/]/,
								name: 'observable',
								priority: 36,
								reuseExistingChunk: true,
							},
							unicode: {
								test: /[\\/]node_modules[\\/](idna-uts46-hx|emoji-regex)[\\/]/,
								name: 'unicode',
								priority: 35,
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
								test: /[\\/]node_modules[\\/](react-select|react-hook-form|react-modal-sheet|react-zoom-pan-pinch|@floating-ui)[\\/]/,
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
								test: /[\\/]node_modules[\\/]/,
								name: 'vendor',
								priority: 10,
								reuseExistingChunk: true,
							},
						},
					}
				: false,
			runtimeChunk: false,
			chunkSplit: false,
			moduleIds: 'named',
			chunkIds: 'named',
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
			port: Number(process.env.FLUXER_APP_DEV_PORT) || 49427,
			hot: false,
			liveReload: false,
			client: false,
			webSocketServer: false,
			historyApiFallback: true,
			allowedHosts: 'all',
			headers: {
				'Access-Control-Allow-Origin': '*',
				'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, PATCH, OPTIONS',
				'Access-Control-Allow-Headers': 'X-Requested-With, content-type, Authorization',
			},
			static: {
				directory: DIST_DIR,
				watch: false,
			},
		},

		experiments: {css: true},
	};
};
