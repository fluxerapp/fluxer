// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {type RehypePlugin, unified} from '@astrojs/markdown-remark';
import mdx from '@astrojs/mdx';
import node from '@astrojs/node';
import starlight from '@astrojs/starlight';
import {defineConfig} from 'astro/config';
import type {Root, RootContent} from 'hast';
import {jetBrainsMonoFont, notoSansFont, radioCanadaBigFont} from './src/fonts/Fonts';
import {docsBaseDomain, docsListenHost, docsListenPort, docsPublicEndpoint} from './src/server/DocsConfig';
import {resolveViteDevWebSocket} from './src/server/ViteDevWebSocket';
import {fluxerDarkCodeTheme, fluxerLightCodeTheme} from './src/styles/CodeTheme';
import {rehypeTableColumnWidths} from './src/table/RehypeTableColumns.ts';

const docsEndpoint = new URL(docsPublicEndpoint());
const staticDirectory = fileURLToPath(new URL('../fluxer_static/', import.meta.url));

const resolveDocsBasePath = (endpoint: URL): string => {
	if (endpoint.pathname === '/') {
		return '';
	}
	if (endpoint.pathname.endsWith('/')) {
		return endpoint.pathname.slice(0, -1);
	}
	return endpoint.pathname;
};

const resolveDocsWebSocketPath = (basePath: string): string => {
	if (basePath.length === 0) {
		return '/@vite-hmr';
	}
	return `${basePath}/@vite-hmr`;
};

const isRootRelativeHref = (href: unknown): href is string => {
	if (typeof href !== 'string') {
		return false;
	}
	if (!href.startsWith('/')) {
		return false;
	}
	return !href.startsWith('//');
};

const prefixRootRelativeLinks = (node: Root | RootContent, prefix: string): void => {
	if (node.type === 'element' && node.tagName === 'a') {
		const href = node.properties.href;
		if (isRootRelativeHref(href)) {
			node.properties.href = `${prefix}${href}`;
		}
	}
	if (node.type !== 'root' && node.type !== 'element') {
		return;
	}
	for (const child of node.children) {
		prefixRootRelativeLinks(child, prefix);
	}
};

const rehypeRootRelativeLinkPrefix =
	(prefix: string): RehypePlugin =>
	() =>
	(tree) => {
		if (prefix.length === 0) {
			return;
		}
		prefixRootRelativeLinks(tree, prefix);
	};

const basePath = resolveDocsBasePath(docsEndpoint);

export default defineConfig({
	adapter: node({mode: 'middleware'}),
	base: docsEndpoint.pathname,
	devToolbar: {
		enabled: false,
	},
	fonts: [radioCanadaBigFont, notoSansFont, jetBrainsMonoFont],
	integrations: [
		starlight({
			components: {
				Head: './src/components/Head.astro',
				Header: './src/components/Header.astro',
				Sidebar: './src/components/Sidebar.astro',
				TableOfContents: './src/components/TableOfContents.astro',
				ThemeProvider: './src/components/ThemeProvider.astro',
				ThemeSelect: './src/components/ThemeSelect.astro',
			},
			customCss: [
				'./src/fonts/font-stack.css',
				'./src/styles/tokens.css',
				'./src/styles/layout.css',
				'./src/styles/content.css',
				'./src/styles/route-header.css',
			],
			description: 'Fluxer external API and protocol reference.',
			expressiveCode: {
				styleOverrides: {
					borderColor: 'var(--flx-code-border)',
					borderRadius: 'var(--flx-radius-lg)',
					borderWidth: '1px',
					codeBackground: 'var(--flx-code-bg)',
					codeFontFamily: 'var(--__sl-font-mono)',
					codeFontSize: 'var(--flx-code-block-font-size)',
					codeForeground: 'var(--flx-code-fg)',
					codeLineHeight: '1.6',
					codePaddingBlock: '0.875rem',
					codePaddingInline: '1.125rem',
					codeSelectionBackground: 'var(--flx-code-selection-bg)',
					focusBorder: 'var(--flx-link)',
					frames: {
						editorActiveTabBackground: 'var(--flx-code-bg)',
						editorActiveTabBorderColor: 'var(--flx-code-border)',
						editorActiveTabForeground: 'var(--flx-content-heading)',
						editorActiveTabIndicatorBottomColor: 'transparent',
						editorActiveTabIndicatorHeight: '2px',
						editorActiveTabIndicatorTopColor: 'var(--flx-brand)',
						editorBackground: 'var(--flx-code-bg)',
						editorTabBarBackground: 'var(--flx-code-chrome-bg)',
						editorTabBarBorderBottomColor: 'var(--flx-code-border)',
						editorTabBorderRadius: '0',
						editorTabsMarginInlineStart: '0',
						frameBoxShadowCssValue: 'none',
						inlineButtonBackground: 'var(--flx-text-primary)',
						inlineButtonBackgroundActiveOpacity: '0.2',
						inlineButtonBackgroundHoverOrFocusOpacity: '0.12',
						inlineButtonBackgroundIdleOpacity: '0',
						inlineButtonBorder: 'var(--flx-code-border)',
						inlineButtonForeground: 'var(--flx-text-tertiary)',
						shadowColor: 'transparent',
						terminalBackground: 'var(--flx-code-bg)',
						terminalTitlebarBackground: 'var(--flx-code-chrome-bg)',
						terminalTitlebarBorderBottomColor: 'var(--flx-code-border)',
						terminalTitlebarDotsForeground: 'var(--flx-code-dots)',
						terminalTitlebarDotsOpacity: '1',
						terminalTitlebarForeground: 'var(--flx-text-tertiary)',
						tooltipSuccessBackground: 'var(--flx-accent-success)',
						tooltipSuccessForeground: 'var(--flx-on-brand)',
					},
					scrollbarThumbColor: 'var(--flx-scrollbar-thumb)',
					scrollbarThumbHoverColor: 'var(--flx-scrollbar-thumb)',
					uiFontFamily: 'var(--__sl-font)',
					uiFontSize: 'var(--sl-text-xs)',
					uiFontWeight: '500',
					uiPaddingBlock: '0.3125rem',
					uiPaddingInline: '0.75rem',
				},
				themes: [fluxerDarkCodeTheme, fluxerLightCodeTheme],
			},
			favicon: '/favicon-32x32.png',
			logo: {
				replacesTitle: true,
				src: path.join(staticDirectory, 'marketing/branding/logo-color.svg'),
			},
			sidebar: [
				{
					label: 'Reference',
					items: [{label: 'Introduction', link: '/'}, 'authentication', 'snowflakes', 'conventions'],
				},
				{
					label: 'Self-hosting',
					items: ['operator/get-started', 'operator/configuration', 'operator/reverse-proxy', 'operator/upgrading'],
				},
				{
					label: 'Topics',
					items: [
						'http-api/errors',
						'topics/rate-limits',
						'http-api/permissions',
						'topics/captcha',
						'topics/uploads',
						'topics/locales',
						'http-api/deployment-availability',
					],
				},
				{
					label: 'HTTP API',
					items: [
						{label: 'Overview', link: '/http-api/'},
						'http-api/instance',
						'http-api/authentication',
						'http-api/gateway',
						'http-api/oauth2',
						'http-api/applications',
						'http-api/connections',
					],
				},
				{
					label: 'Users',
					items: [
						'http-api/users',
						'http-api/users/current-user',
						'http-api/users/settings',
						'http-api/users/settings-protobuf',
						'http-api/users/email-and-password',
						'http-api/users/mfa',
						'http-api/users/phone-verification',
						'http-api/users/relationships',
						'http-api/users/notes',
						'http-api/users/private-channels',
						'http-api/users/content',
						'http-api/users/gifts',
						'http-api/users/data-harvest',
					],
				},
				{
					label: 'Messaging',
					items: [
						'http-api/channels',
						'http-api/messages',
						'http-api/read-states',
						'http-api/memes',
						'http-api/gifs',
						'http-api/webhooks',
					],
				},
				{
					label: 'Guilds',
					items: [
						'http-api/guilds',
						'http-api/guild-channels',
						'http-api/guild-members',
						'http-api/guild-member-search',
						'http-api/guild-moderation',
						'http-api/guild-emojis',
						'http-api/guild-stickers',
						'http-api/expressions',
						'http-api/guild-audit-logs',
					],
				},
				{
					label: 'Discovery and content',
					items: ['http-api/discovery', 'http-api/invites', 'http-api/search', 'http-api/unfurl'],
				},
				{
					label: 'Commerce',
					items: ['http-api/billing', 'http-api/premium', 'http-api/gifts', 'http-api/donations'],
				},
				{
					label: 'Client surfaces',
					items: ['http-api/themes', 'http-api/downloads'],
				},
				{
					label: 'Safety',
					items: ['http-api/reports'],
				},
				{
					label: 'Gateway',
					items: [
						'gateway/overview',
						'gateway/commands',
						'gateway/events',
						'gateway/event-filtering',
						'gateway/limits-and-rate-limits',
						'gateway/opcodes-and-close-codes',
					],
				},
				{
					label: 'Media proxy',
					items: [
						'media-proxy/overview',
						'media-proxy/routes',
						'media-proxy/transformations',
						'media-proxy/upload-relay',
						'media-proxy/responses-and-limits',
					],
				},
				{
					label: 'Voice',
					items: ['voice', 'http-api/calls', 'http-api/streams', 'http-api/entrance-sounds'],
				},
				{
					label: 'Admin API',
					items: [
						{label: 'Overview', link: '/admin-api/'},
						'admin-api/api-keys',
						'admin-api/users',
						'admin-api/guilds',
						'admin-api/applications',
						'admin-api/reports',
						'admin-api/messages',
						'admin-api/blocklists',
						'admin-api/discovery',
						'admin-api/system-dms',
						'admin-api/instance',
						'admin-api/gift-codes',
						'admin-api/gateway',
						'admin-api/voice',
						'admin-api/jobs',
						'admin-api/bulk-jobs',
						'admin-api/archives',
						'admin-api/search-indexes',
					],
				},
			],
			social: [
				{
					href: 'https://github.com/fluxerapp/fluxer',
					icon: 'github',
					label: 'GitHub',
				},
			],
			tableOfContents: {maxHeadingLevel: 2, minHeadingLevel: 2},
			title: 'Fluxer API',
		}),
		mdx(),
	],
	markdown: {
		processor: unified({
			rehypePlugins: [rehypeRootRelativeLinkPrefix(basePath), rehypeTableColumnWidths()],
		}),
	},
	output: 'server',
	publicDir: path.join(staticDirectory, 'web'),
	site: docsEndpoint.origin,
	server: {
		allowedHosts: [docsBaseDomain()],
		host: docsListenHost(),
		port: docsListenPort(),
	},
	vite: {
		resolve: {
			alias: {
				'@': fileURLToPath(new URL('./src', import.meta.url)),
			},
		},
		server: {
			fs: {
				deny: [
					'.env',
					'.env.*',
					'*.{crt,pem,key,p12,pfx,cer,der}',
					'.npmrc',
					'.yarnrc.yml',
					'**/.git/**',
					'**/.fluxer/**',
					'**/.devcontainer/**',
					'**/.ssh/**',
					'**/.aws/**',
					'**/.envrc',
					'**/*.{toml,yaml,yml}',
					'**/*.{sqlite,sqlite3,db}',
				],
			},
			ws: resolveViteDevWebSocket({endpoint: docsEndpoint, path: resolveDocsWebSocketPath(basePath)}),
		},
	},
});
