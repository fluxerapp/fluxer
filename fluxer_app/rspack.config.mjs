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

import path, {dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {CopyRspackPlugin, DefinePlugin, HtmlRspackPlugin, SwcJsMinimizerRspackPlugin} from '@rspack/core';
import ReactRefreshPlugin from '@rspack/plugin-react-refresh';
import {createPoFileRule, getLinguiSwcPluginConfig} from './scripts/build/rspack/lingui.mjs';
import {staticFilesPlugin} from './scripts/build/rspack/static-files.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const ROOT_DIR = path.resolve(__dirname, '.');
const SRC_DIR = path.join(ROOT_DIR, 'src');
const DIST_DIR = path.join(ROOT_DIR, 'dist');
const PKGS_DIR = path.join(ROOT_DIR, 'pkgs');
const PUBLIC_DIR = path.join(ROOT_DIR, 'assets');

const CDN_ENDPOINT = 'https://fluxerstatic.com';

const isProduction = process.env.NODE_ENV === 'production';
const isDevelopment = !isProduction;

const mode = isProduction ? 'production' : 'development';
const devJsName = 'assets/[name].js';
const devCssName = 'assets/[name].css';

function getPublicEnvVar(name) {
	const value = process.env[name];
	return JSON.stringify(value) ?? 'undefined';
}

export default () => {
	const linguiSwcPlugin = getLinguiSwcPluginConfig();

	return {
		mode,

		entry: {
			main: path.join(SRC_DIR, 'index.tsx'),
		},

		output: {
			path: DIST_DIR,
			publicPath: isProduction ? `${CDN_ENDPOINT}/` : '/',
			workerPublicPath: '/',
			filename: isProduction ? 'assets/[contenthash:16].js' : devJsName,
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
				'@pkgs': PKGS_DIR,
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
										refresh: isDevelopment,
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
		},

		generator: {
			'css/module': {
				exportsConvention: 'as-is',
			},
		},

		plugins: [
			new HtmlRspackPlugin({
				template: path.join(ROOT_DIR, 'index.html'),
				filename: 'index.html',
				inject: 'body',
				scriptLoading: 'module',
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

			staticFilesPlugin({cdnEndpoint: CDN_ENDPOINT}),

			new DefinePlugin({
				'process.env.NODE_ENV': JSON.stringify(mode),
				'import.meta.env.DEV': JSON.stringify(isDevelopment),
				'import.meta.env.PROD': JSON.stringify(isProduction),
				'import.meta.env.MODE': JSON.stringify(mode),
				'import.meta.env.PUBLIC_BUILD_SHA': getPublicEnvVar('PUBLIC_BUILD_SHA'),
				'import.meta.env.PUBLIC_BUILD_NUMBER': getPublicEnvVar('PUBLIC_BUILD_NUMBER'),
				'import.meta.env.PUBLIC_BUILD_TIMESTAMP': getPublicEnvVar('PUBLIC_BUILD_TIMESTAMP'),
				'import.meta.env.PUBLIC_PROJECT_ENV': getPublicEnvVar('PUBLIC_PROJECT_ENV'),
				'import.meta.env.PUBLIC_SENTRY_DSN': getPublicEnvVar('PUBLIC_SENTRY_DSN'),
				'import.meta.env.PUBLIC_SENTRY_PROJECT_ID': getPublicEnvVar('PUBLIC_SENTRY_PROJECT_ID'),
				'import.meta.env.PUBLIC_SENTRY_PUBLIC_KEY': getPublicEnvVar('PUBLIC_SENTRY_PUBLIC_KEY'),
				'import.meta.env.PUBLIC_SENTRY_PROXY_PATH': getPublicEnvVar('PUBLIC_SENTRY_PROXY_PATH'),
				'import.meta.env.PUBLIC_API_VERSION': getPublicEnvVar('PUBLIC_API_VERSION'),
				'import.meta.env.PUBLIC_BOOTSTRAP_API_ENDPOINT': getPublicEnvVar('PUBLIC_BOOTSTRAP_API_ENDPOINT'),
				'import.meta.env.PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT': getPublicEnvVar('PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT'),
			}),

			isDevelopment && new ReactRefreshPlugin(),
		].filter(Boolean),

		optimization: {
			splitChunks: isProduction
				? {
						chunks: 'all',
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
								test: /[\\/]node_modules[\\/](ws|undici)[\\/]/,
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

		devServer: isDevelopment
			? {
					port: 3000,
					hot: true,
					liveReload: false,
					historyApiFallback: true,
					allowedHosts: 'all',
					client: {
						webSocketURL: 'auto://0.0.0.0:0/ws',
					},
					headers: {
						'Access-Control-Allow-Origin': '*',
						'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, PATCH, OPTIONS',
						'Access-Control-Allow-Headers': 'X-Requested-With, content-type, Authorization',
					},
					static: {
						directory: DIST_DIR,
						watch: false,
					},
				}
			: undefined,

		experiments: {css: true},

		css: {
			modules: {
				localIdentName: '[name]__[local]___[hash:base64:16]',
				localsConvention: 'camelCaseOnly',
			},
		},
	};
};
