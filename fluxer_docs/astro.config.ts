// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {type RehypePlugin, unified} from '@astrojs/markdown-remark';
import mdx from '@astrojs/mdx';
import node from '@astrojs/node';
import starlight from '@astrojs/starlight';
import {defineConfig} from 'astro/config';
import type {Root, RootContent} from 'hast';
import {radioCanadaBigFont} from './src/fonts/Fonts';
import {ecWordTokens} from './src/plugins/ecWordTokens.ts';
import {lowPriorityModuleScripts} from './src/plugins/lowPriorityModuleScripts.ts';
import {rehypeHeadingContext} from './src/plugins/rehypeHeadingContext.ts';
import {rehypeInlineCode} from './src/plugins/rehypeInlineCode.ts';
import {newestRelease, remarkReleaseVersion} from './src/plugins/remarkReleaseVersion.ts';
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

const legalLinks = [
	{label: 'Licence (AGPL-3.0-or-later)', link: 'https://github.com/fluxerapp/fluxer/blob/main/LICENSE'},
	{label: 'Terms of service', link: 'https://fluxer.com/terms'},
	{label: 'Privacy policy', link: 'https://fluxer.com/privacy'},
	{label: 'Community guidelines', link: 'https://fluxer.com/guidelines'},
	{label: 'Security policy', link: 'https://fluxer.com/security'},
	{label: 'Company information', link: 'https://fluxer.com/company-information'},
];

export default defineConfig({
	adapter: node({mode: 'middleware'}),
	base: docsEndpoint.pathname,
	devToolbar: {
		enabled: false,
	},
	fonts: [radioCanadaBigFont],
	integrations: [
		starlight({
			components: {
				Head: './src/components/Head.astro',
				Header: './src/components/Header.astro',
				MobileTableOfContents: './src/components/MobileTableOfContents.astro',
				PageTitle: './src/components/PageTitle.astro',
				Footer: './src/components/Footer.astro',
				Pagination: './src/components/Pagination.astro',
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
			description: 'Build on the Fluxer API or run Fluxer on your own machine.',
			editLink: {
				baseUrl: 'https://github.com/fluxerapp/fluxer/edit/main/fluxer_docs/',
			},
			expressiveCode: {
				defaultProps: {
					frame: 'code',
					hangingIndent: 2,
					wrap: true,
				},
				plugins: [ecWordTokens],
				styleOverrides: {
					borderColor: 'var(--flx-line)',
					borderRadius: 'var(--flx-radius-surface)',
					borderWidth: '1px',
					codeBackground: 'var(--flx-code-bg)',
					codeFontFamily: 'var(--__sl-font-mono)',
					codeFontSize: 'var(--flx-code-size)',
					codeForeground: 'var(--flx-text)',
					codeLineHeight: '1.65',
					codePaddingBlock: '1.25rem',
					codePaddingInline: '1.5rem',
					codeSelectionBackground: 'var(--flx-code-selection)',
					focusBorder: 'var(--flx-focus)',
					frames: {
						editorActiveTabBackground: 'var(--flx-step)',
						editorActiveTabBorderColor: 'transparent',
						editorActiveTabForeground: 'var(--flx-text-muted)',
						editorActiveTabIndicatorBottomColor: 'var(--flx-line)',
						editorActiveTabIndicatorHeight: '1px',
						editorActiveTabIndicatorTopColor: 'transparent',
						editorBackground: 'var(--flx-code-bg)',
						editorTabBarBackground: 'var(--flx-step)',
						editorTabBarBorderBottomColor: 'var(--flx-line)',
						editorTabBorderRadius: '0',
						editorTabsMarginInlineStart: '0',
						frameBoxShadowCssValue: 'none',
						inlineButtonBackground: 'var(--flx-text-strong)',
						inlineButtonBackgroundActiveOpacity: '0.16',
						inlineButtonBackgroundHoverOrFocusOpacity: '0.08',
						inlineButtonBackgroundIdleOpacity: '0',
						inlineButtonBorder: 'var(--flx-line-strong)',
						inlineButtonBorderOpacity: '1',
						inlineButtonForeground: 'var(--flx-text-muted)',
						shadowColor: 'transparent',
						terminalBackground: 'var(--flx-code-bg)',
						terminalTitlebarBackground: 'var(--flx-step)',
						terminalTitlebarBorderBottomColor: 'var(--flx-line)',
						terminalTitlebarDotsForeground: 'transparent',
						terminalTitlebarDotsOpacity: '0',
						terminalTitlebarForeground: 'var(--flx-text-muted)',
						tooltipSuccessBackground: 'var(--flx-text-strong)',
						tooltipSuccessForeground: 'var(--flx-bg)',
					},
					scrollbarThumbColor: 'var(--flx-scrollbar-thumb)',
					scrollbarThumbHoverColor: 'var(--flx-line-strong)',
					uiFontFamily: 'var(--__sl-font)',
					uiFontSize: '0.875rem',
					uiFontWeight: '400',
					uiPaddingBlock: '0.5rem',
					uiPaddingInline: '1rem',
				},
				themes: [fluxerDarkCodeTheme, fluxerLightCodeTheme],
				useStarlightDarkModeSwitch: true,
				useStarlightUiThemeColors: false,
			},
			favicon: '/favicon-32x32.png',
			logo: {
				replacesTitle: true,
				src: path.join(staticDirectory, 'marketing/branding/logo-color.svg'),
			},
			sidebar: [
				{
					label: 'API',
					items: [
						{
							label: 'Getting started',
							items: [{label: 'Introduction', link: '/'}, 'authentication', 'snowflakes'],
						},
						{
							label: 'Topics',
							items: [
								'http-api/errors',
								'topics/rate-limits',
								'topics/announcement-channels',
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
							collapsed: true,
							label: 'Users',
							items: [
								'http-api/users',
								'http-api/users/current-user',
								'http-api/users/settings',
								'http-api/users/settings-protobuf',
								'http-api/users/email-and-password',
								'http-api/users/mfa',
								'http-api/users/relationships',
								'http-api/users/notes',
								'http-api/users/private-channels',
								'http-api/users/push-notifications',
								'http-api/users/content',
								'http-api/users/gifts',
								'http-api/users/data-harvest',
							],
						},
						{
							collapsed: true,
							label: 'Messaging',
							items: [
								'http-api/channels',
								'http-api/messages',
								'http-api/threads',
								'http-api/thread-members',
								'http-api/forums',
								'http-api/read-states',
								'http-api/memes',
								'http-api/gifs',
								'http-api/webhooks',
							],
						},
						{
							collapsed: true,
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
							collapsed: true,
							label: 'Discovery and content',
							items: ['http-api/discovery', 'http-api/invites', 'http-api/search', 'http-api/unfurl'],
						},
						{
							collapsed: true,
							label: 'Commerce',
							items: ['http-api/billing', 'http-api/premium', 'http-api/gifts'],
						},
						{
							collapsed: true,
							label: 'Experiments and themes',
							items: ['http-api/experiments', 'http-api/themes'],
						},
						{
							collapsed: true,
							label: 'Safety',
							items: ['http-api/reports'],
						},
						{
							collapsed: true,
							label: 'Gateway',
							items: [
								{label: 'Overview', link: '/gateway/overview/'},
								'gateway/commands',
								'gateway/events',
								'gateway/event-filtering',
								'gateway/threads',
								'gateway/limits-and-rate-limits',
								'gateway/opcodes-and-close-codes',
							],
						},
						{
							collapsed: true,
							label: 'Media Proxy',
							items: [
								{label: 'Overview', link: '/media-proxy/overview/'},
								'media-proxy/routes',
								'media-proxy/transformations',
								'media-proxy/upload-relay',
								'media-proxy/responses-and-limits',
							],
						},
						{
							collapsed: true,
							label: 'Admin API',
							items: [
								{label: 'Overview', link: '/admin-api/'},
								'admin-api/api-keys',
								'admin-api/users',
								'admin-api/guilds',
								'admin-api/badges',
								'admin-api/threads',
								'admin-api/applications',
								'admin-api/reports',
								'admin-api/enforcement',
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
						{
							label: 'Voice',
							items: [
								{label: 'Overview', link: '/voice/'},
								'http-api/calls',
								'http-api/peer-to-peer-voice',
								'http-api/streams',
								'http-api/entrance-sounds',
							],
						},
					],
				},
				{
					label: 'Self-hosting',
					items: [
						{
							label: 'Getting started',
							items: ['operator', 'operator/pages', 'operator/requirements'],
						},
						{
							label: 'Install',
							items: [
								'operator/docker',
								'operator/linux',
								'operator/linux-manual',
								'operator/kubernetes',
								'operator/with-rexctl',
							],
						},
						{
							collapsed: true,
							label: 'Platforms',
							items: [
								'operator/platforms',
								{
									collapsed: true,
									label: 'Linux',
									items: [
										'operator/platforms/debian-ubuntu',
										'operator/platforms/fedora-rhel',
										'operator/platforms/arch',
										'operator/platforms/raspberry-pi',
										'operator/platforms/nixos',
										'operator/platforms/podman',
									],
								},
								{
									collapsed: true,
									label: 'Desktop and virtual machines',
									items: ['operator/platforms/docker-desktop', 'operator/platforms/proxmox'],
								},
								{
									collapsed: true,
									label: 'NAS and home servers',
									items: [
										'operator/platforms/unraid',
										'operator/platforms/truenas',
										'operator/platforms/synology',
										'operator/platforms/qnap',
										'operator/platforms/casaos',
										'operator/platforms/umbrel',
										'operator/platforms/yunohost',
									],
								},
								{
									collapsed: true,
									label: 'Self-hosting panels',
									items: [
										'operator/platforms/coolify',
										'operator/platforms/dokploy',
										'operator/platforms/caprover',
										'operator/platforms/portainer',
									],
								},
								{
									collapsed: true,
									label: 'Kubernetes',
									items: [
										'operator/platforms/k3s',
										'operator/platforms/microk8s',
										'operator/platforms/talos',
										'operator/platforms/managed-kubernetes',
									],
								},
								{
									collapsed: true,
									label: 'Cloud and home networks',
									items: [
										'operator/platforms/cloud-servers',
										'operator/platforms/cloudflare-tunnel',
										'operator/platforms/tailscale',
									],
								},
							],
						},
						{
							label: 'Guides',
							items: [
								'operator/running',
								'operator/configuration',
								'operator/backups',
								'operator/upgrades',
								'operator/reverse-proxy',
								'operator/voice-servers',
								'operator/sign-in',
								'operator/bluesky',
								'operator/email',
								'operator/upload-limits',
								'operator/multiple-instances',
								'operator/monitoring',
								'operator/moving',
								'operator/build-from-source',
							],
						},
						{
							label: 'Reference',
							items: ['operator/how-it-works', 'operator/security', 'operator/settings', 'operator/rexctl'],
						},
						{
							label: 'Legal',
							items: legalLinks,
						},
					],
				},
				{
					label: 'Downloads',
					items: [
						{label: 'Overview', link: '/downloads/overview/'},
						'downloads/desktop',
						'downloads/linux-repositories',
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
			title: 'Fluxer docs',
		}),
		mdx(),
		lowPriorityModuleScripts(),
	],
	markdown: {
		processor: unified({
			smartypants: false,
			remarkPlugins: [remarkReleaseVersion(() => newestRelease())],
			rehypePlugins: [
				rehypeRootRelativeLinkPrefix(basePath),
				rehypeInlineCode(),
				rehypeTableColumnWidths(),
				rehypeHeadingContext(),
			],
		}),
	},
	output: 'server',
	redirects: {
		'/admin-api/operator-pass': '/admin-api/',
		'/admin-api/payment-disputes': '/admin-api/',
		'/api-reference': '/http-api/',
		'/api-reference/[...path]': '/http-api/',
		'/http-api/donations': '/http-api/billing/',
		'/http-api/in-app-purchases': '/http-api/premium/',
		'/http-api/operator-pass': '/http-api/premium/',
		'/http-api/operator-pass-waitlist': '/http-api/premium/',
		'/operator/get-started': '/operator/',
		'/operator/operator-pass': '/operator/',
		'/operator/upgrading': '/operator/upgrades/',
		'/operator/docker-compose': '/operator/moving/',
		'/operator/voice-compose': '/operator/voice-servers/',
		'/operator/architecture': '/operator/how-it-works/',
		'/operator/environment': '/operator/settings/',
		'/operator/troubleshooting': '/operator/',
	},
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
