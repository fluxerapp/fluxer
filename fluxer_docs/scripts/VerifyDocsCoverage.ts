// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import type {Dirent} from 'node:fs';
import {mkdir, mkdtemp, readdir, readFile, rm, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {extractRoutesFromControllers} from '@fluxer/openapi/src/extractors/RouteExtractor';
import {installerChecksumLine} from '../src/installer/InstallerDigest.ts';

const DOCS_ROOT = fileURLToPath(new URL('../src/content/docs/', import.meta.url));
const REPO_ROOT = fileURLToPath(new URL('../../', import.meta.url));
const MAIN_SPEC = path.join(REPO_ROOT, 'fluxer_api/src/api/openapi/openapi.json');
const ADMIN_SPEC = path.join(REPO_ROOT, 'fluxer_admin/openapi-admin.json');
const MEDIA_PROXY_SERVER_DIR = path.join(REPO_ROOT, 'fluxer_media_proxy/src/server');

const ROUTE_HEADER_PATTERN = /<RouteHeader\s+([^>]*?)\/>/gu;
const ATTRIBUTE_PATTERN = /(\w+)\s*=\s*"([^"]*)"/gu;
const HTTP_METHODS = new Set(['GET', 'HEAD', 'POST', 'PATCH', 'PUT', 'DELETE', 'OPTIONS']);

const BLUESKY_OAUTH_CONTROLLER = 'fluxer_api/src/api/bluesky/BlueskyOAuthController.ts';
const DOWNLOAD_CONTROLLER = 'fluxer_api/src/api/download/DownloadController.ts';

const MAIN_SPEC_EXEMPT = new Map<string, {file: string; anchor: string; reason: string}>([
	[
		'GET /oauth2/authorize',
		{
			file: 'fluxer_api/src/api/oauth/OAuth2Controller.ts',
			anchor: "'/oauth2/authorize',",
			reason: 'the generator excludes the path outright, at OpenAPIGeneratorCatalog excluded.paths',
		},
	],
	[
		'GET /openapi.json',
		{
			file: 'fluxer_api/src/api/openapi/OpenAPIController.ts',
			anchor: "app.get('/openapi.json'",
			reason: 'the handler serves the spec file itself and carries no OpenAPI({...}) block',
		},
	],
	[
		'GET /connections/bluesky/client-metadata.json',
		{
			file: BLUESKY_OAUTH_CONTROLLER,
			anchor: "'/connections/bluesky/client-metadata.json'",
			reason: 'an AT Protocol client document with no OpenAPI({...}) block',
		},
	],
	[
		'GET /connections/bluesky/jwks.json',
		{
			file: BLUESKY_OAUTH_CONTROLLER,
			anchor: "'/connections/bluesky/jwks.json'",
			reason: 'an AT Protocol client document with no OpenAPI({...}) block',
		},
	],

	[
		'GET /dl/desktop/{}/{}/{}/latest/{}.sha256',
		{
			file: DOWNLOAD_CONTROLLER,
			anchor: '`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/latest/:format{[a-z_]+\\\\.sha256}`,',
			reason: 'the path constrains :format by regex and has no OpenAPI path template',
		},
	],
	[
		'GET /dl/desktop/{}/{}/{}/{}/{}.sha256',
		{
			file: DOWNLOAD_CONTROLLER,
			anchor: '`${DESKTOP_REDIRECT_PREFIX}/:channel/:plat/:arch/:version/:format{[a-z_]+\\\\.sha256}`,',
			reason: 'the path constrains :format by regex and has no OpenAPI path template',
		},
	],
	[
		'GET /dl/{}',
		{
			file: DOWNLOAD_CONTROLLER,
			anchor: '`${DOWNLOAD_PREFIX}/*`,',
			reason: 'a Hono wildcard, which is not an OpenAPI path template',
		},
	],
]);

const DELIBERATELY_UNDOCUMENTED = new Map([
	['GET /users/@me/mobile-devices', 'mobile notifications, backported separately'],
	['POST /users/@me/mobile-devices', 'mobile notifications, backported separately'],
	['POST /users/@me/mobile-devices/unregister', 'mobile notifications, backported separately'],
	['DELETE /users/@me/mobile-devices/{}', 'mobile notifications, backported separately'],
	['GET /users/@me/push/subscriptions', 'push API, backported separately'],
	['POST /users/@me/push/subscribe', 'push API, backported separately'],
	['POST /users/@me/push/rotate', 'push API, backported separately'],
	['DELETE /users/@me/push/subscriptions/{}', 'push API, backported separately'],
]);

interface OutOfBandRoute {
	readonly reason: string;
	readonly documentedIn: {readonly file: string; readonly anchor: string} | null;
}
const OUT_OF_BAND_CREDENTIAL = new Map<string, OutOfBandRoute>([
	[
		'POST /internal/rpc',
		{
			reason:
				'x-fluxer-rpc-auth must timing-safe-equal Config.internal.gatewayRpcAuthToken, and an empty token rejects every caller. The caller is fluxer_gateway',
			documentedIn: null,
		},
	],
	[
		'POST /webhooks/twilio/sms',
		{
			reason:
				'installed only when config.sms.enabled, and then only when the inbound webhook token and public URL are both set, so a default or self-hosted instance never registers it',
			documentedIn: null,
		},
	],
	[
		'POST /webhooks/livekit',
		{
			reason: 'a LiveKit-signed callback whose shipped target is the internal address http://api:8080/webhooks/livekit',
			documentedIn: {file: 'operator/configuration.mdx', anchor: 'http://api:8080/webhooks/livekit'},
		},
	],
	[
		'POST /webhooks/sweego',
		{
			reason:
				'a Standard Webhooks delivery signed with FLUXER_EMAIL_WEBHOOK_SECRET, which answers 404 Email not enabled while email is off',
			documentedIn: {file: 'operator/configuration.mdx', anchor: 'POST /webhooks/sweego'},
		},
	],
	[
		'GET /connections/bluesky/callback',
		{
			reason:
				'the atproto redirect URI, reached with a provider-issued authorisation code. A client MUST NOT call it directly',
			documentedIn: {file: 'http-api/connections.mdx', anchor: '/connections/bluesky/callback'},
		},
	],
]);

const HEALTH_AND_METRICS_PATHS = new Set(['/_health', '/_health/ready', '/_health/drain', '/_healthz', '/_metrics']);

interface ExemptionRule {
	readonly name: string;
	readonly justification: string;

	readonly anchors: ReadonlyArray<{readonly file: string; readonly anchor: string}>;
	readonly covers: (shape: string, routePath: string) => boolean;
}
const EXEMPTION_RULES: ReadonlyArray<ExemptionRule> = [
	{
		name: 'test harness',
		justification:
			'ControllerRegistry registers TestHarnessController only when config.dev.testModeEnabled or NODE_ENV is development, and ensureHarnessAccess rechecks that per request. No production instance serves a /test/ route',
		anchors: [
			{
				file: 'fluxer_api/src/api/app/ControllerRegistry.ts',
				anchor: "if (config.dev.testModeEnabled || config.nodeEnv === 'development') {",
			},
			{file: 'fluxer_api/src/api/test/TestHarnessController.ts', anchor: 'function ensureHarnessAccess'},
		],
		covers: (_shape, routePath) => routePath.startsWith('/test/'),
	},
	{
		name: 'backported separately',
		justification:
			'part of a system this backport does not ship. Each shape is listed with its system, and a page that documents one fails the run below',
		anchors: [],
		covers: (shape) => DELIBERATELY_UNDOCUMENTED.has(shape),
	},
	{
		name: 'process health and metrics',
		justification:
			'loopback gated or reachable only inside the compose network. operator/reverse-proxy.mdx states which probes answer through a proxy once for every service, rather than per route',
		anchors: [{file: 'fluxer_docs/src/content/docs/operator/reverse-proxy.mdx', anchor: '/_metrics'}],
		covers: (_shape, routePath) => HEALTH_AND_METRICS_PATHS.has(routePath),
	},
	{
		name: 'out-of-band credential',
		justification:
			'no ordinary client holds the credential. Each entry states its guard, and three are covered in prose',
		anchors: [{file: 'fluxer_api/src/api/app/ControllerRegistry.ts', anchor: 'if (config.sms.enabled) {'}],
		covers: (shape) => OUT_OF_BAND_CREDENTIAL.has(shape),
	},
];

const MEDIA_PROXY_ROUTES = new Map([
	['GET /_health', '.route("/_health", get(routes::ops::health))'],
	['HEAD /_health', '.route("/_health", get(routes::ops::health))'],
	['GET /_metrics', '.route("/_metrics", get(routes::ops::metrics_handler))'],
	['POST /_metadata', '.route("/_metadata", post(routes::internal::metadata_handler))'],
	['POST /_thumbnail', '.route("/_thumbnail", post(routes::internal::thumbnail_handler))'],
	['POST /_frames', '.route("/_frames", post(routes::internal::frames_handler))'],
	[
		'PUT /v1/relay/{}',
		'"/v1/relay/{*key}",\n            put(routes::relay::relay_put).options(routes::relay::relay_options),',
	],
	[
		'OPTIONS /v1/relay/{}',
		'"/v1/relay/{*key}",\n            put(routes::relay::relay_put).options(routes::relay::relay_options),',
	],
	['GET /{}', 'if app.cfg.mode == DeploymentMode::Static {'],
	['HEAD /{}', 'if app.cfg.mode == DeploymentMode::Static {'],
]);

const MEDIA_PROXY_ASSET_PREFIXES = new Map([
	['external', 'path.strip_prefix("/external/")'],
	['attachments', 'path.starts_with("/attachments/")'],
	['themes', 'path.starts_with("/themes/")'],
	['entrance-sounds', '!= "entrance-sounds"'],
	['emojis', 'AssetKind::Emoji => "emojis"'],
	['stickers', 'AssetKind::Sticker => "stickers"'],
	['avatars', '"avatars" => AssetKind::Avatar'],
	['icons', '"icons" => AssetKind::GuildIcon'],
	['branding', '"branding" => AssetKind::GuildIcon'],
	['banners', '"banners" => AssetKind::Banner'],
	['splashes', '"splashes" => AssetKind::Splash'],
	['embed-splashes', '"embed-splashes" => AssetKind::EmbedSplash'],
	['guilds', 'fn parse_guild_member_asset_path'],
]);

interface DocumentedRoute {
	readonly method: string;
	readonly path: string;
	readonly file: string;
	readonly bot: boolean;
	readonly unauthenticated: boolean;
	readonly oauth2: string | null;
}

interface SpecOperation {
	readonly method: string;
	readonly path: string;
	readonly tags: ReadonlyArray<string>;
	readonly security: ReadonlyArray<Record<string, Array<string>>> | null;
}

async function walk(directory: string): Promise<Array<string>> {
	const entries = await readdir(directory, {withFileTypes: true});
	const files: Array<string> = [];
	for (const entry of entries) {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await walk(resolved)));
			continue;
		}
		if (entry.name.endsWith('.mdx') || entry.name.endsWith('.md')) {
			files.push(resolved);
		}
	}
	return files;
}

function stripVersionPrefix(routePath: string): string {
	if (routePath === '/v1') {
		return '/';
	}
	if (routePath.startsWith('/v1/')) {
		return routePath.slice(3);
	}
	return routePath;
}

function shapeOf(method: string, routePath: string): string {
	const withoutQuery = routePath.split('?')[0];
	return `${method} ${withoutQuery.replace(/\{[^}]*\}/gu, '{}')}`;
}

async function documentedRoutes(): Promise<Array<DocumentedRoute>> {
	const files = await walk(DOCS_ROOT);
	const routes: Array<DocumentedRoute> = [];
	for (const file of files) {
		const source = await readFile(file, 'utf8');
		for (const match of source.matchAll(ROUTE_HEADER_PATTERN)) {
			const attributes = new Map<string, string>();
			for (const attribute of match[1].matchAll(ATTRIBUTE_PATTERN)) {
				attributes.set(attribute[1], attribute[2]);
			}
			const method = attributes.get('method');
			const routePath = attributes.get('path');
			if (method == null || routePath == null) {
				continue;
			}
			const bareFlags = new Set(
				match[1]
					.replace(/\w+\s*=\s*"[^"]*"/gu, ' ')
					.split(/\s+/u)
					.filter((token) => token.length > 0),
			);
			routes.push({
				method,
				path: routePath,
				file: path.relative(DOCS_ROOT, file),
				bot: bareFlags.has('bot'),
				unauthenticated: bareFlags.has('unauthenticated'),
				oauth2: attributes.get('oauth2') ?? null,
			});
		}
	}
	return routes;
}

interface AliasRoute {
	readonly shape: string;
	readonly file: string;
	readonly successor: string;
}

const ALIAS_TABLE_HEADER = '| Method | Deprecated path | Successor |';
const ALIAS_ROW_PATTERN =
	/^\|\s*(GET|HEAD|POST|PATCH|PUT|DELETE|OPTIONS)\s*\|\s*`(\/[^`]+)`\s*\|\s*\[([^\]]+)\]\([^)]+\)\s*\|\s*$/u;

async function aliasDocumentedRoutes(): Promise<Array<AliasRoute>> {
	const files = await walk(DOCS_ROOT);
	const aliases: Array<AliasRoute> = [];
	for (const file of files) {
		const source = await readFile(file, 'utf8');
		let inTable = false;
		for (const line of source.split('\n')) {
			if (line.trim() === ALIAS_TABLE_HEADER) {
				inTable = true;
				continue;
			}
			if (inTable && line.trim().startsWith('| ---')) {
				continue;
			}
			const row = inTable ? ALIAS_ROW_PATTERN.exec(line.trim()) : null;
			if (row == null) {
				inTable = false;
				continue;
			}
			aliases.push({
				shape: shapeOf(row[1], stripVersionPrefix(row[2])),
				file: path.relative(DOCS_ROOT, file),
				successor: row[3],
			});
		}
	}
	return aliases;
}

async function specOperations(specPath: string): Promise<Array<SpecOperation>> {
	const spec: unknown = JSON.parse(await readFile(specPath, 'utf8'));
	if (typeof spec !== 'object' || spec == null || !('paths' in spec)) {
		throw new Error(`Spec has no paths: ${specPath}`);
	}
	const paths = (
		spec as {
			paths: Record<string, Record<string, {tags?: Array<string>; security?: Array<Record<string, Array<string>>>}>>;
		}
	).paths;
	const operations: Array<SpecOperation> = [];
	for (const [routePath, item] of Object.entries(paths)) {
		for (const [method, operation] of Object.entries(item)) {
			const upper = method.toUpperCase();
			if (!HTTP_METHODS.has(upper)) {
				continue;
			}
			operations.push({
				method: upper,
				path: stripVersionPrefix(routePath),
				tags: operation.tags ?? [],
				security: operation.security ?? null,
			});
		}
	}
	return operations;
}

function section(title: string, entries: ReadonlyArray<string>): number {
	if (entries.length === 0) {
		console.log(`  ${title}: none`);
		return 0;
	}
	console.log(`  ${title}: ${entries.length.toString()}`);
	for (const entry of entries) {
		console.log(`    ${entry}`);
	}
	return entries.length;
}

async function registrationSources(directory: string): Promise<Array<string>> {
	const entries = await readdir(directory, {withFileTypes: true});
	const files: Array<string> = [];
	for (const entry of entries) {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			if (entry.name === 'node_modules' || entry.name === 'tests') {
				continue;
			}
			files.push(...(await registrationSources(resolved)));
			continue;
		}
		if (entry.name.endsWith('.ts') && !entry.name.endsWith('.test.ts')) {
			files.push(resolved);
		}
	}
	return files;
}
const controllerRoutes = extractRoutesFromControllers(
	await registrationSources(path.join(REPO_ROOT, 'fluxer_api/src')),
);

function constraintTail(constraint: string): string {
	const body = constraint.slice(1, -1);
	const tail = body.match(/(?:\\[^A-Za-z0-9]|[A-Za-z0-9_])+$/u);
	if (tail?.index == null || !/[+*?})\]]$/u.test(body.slice(0, tail.index))) {
		return `<constraint ${body}>`;
	}
	return tail[0].replace(/\\(.)/gu, '$1');
}
const astRoute = (route: {method: string; path: string}): string => {
	const templated = route.path
		.replace(
			/:([A-Za-z_][A-Za-z0-9_]*)(\{(?:[^{}]|\{[^{}]*\})*\})?/gu,
			(_match: string, name: string, constraint: string | undefined) =>
				`{${name}}${constraint == null ? '' : constraintTail(constraint)}`,
		)
		.replace(/\*/gu, '{wildcard}');
	return shapeOf(route.method.toUpperCase(), stripVersionPrefix(templated));
};

const documented = await documentedRoutes();
const aliasDocumented = await aliasDocumentedRoutes();
const aliasShapes = new Map(aliasDocumented.map((alias) => [alias.shape, alias]));
const documentedFlags = new Map<string, {bot: boolean; unauthenticated: boolean}>();
for (const route of documented) {
	documentedFlags.set(shapeOf(route.method, stripVersionPrefix(route.path)), {
		bot: route.bot,
		unauthenticated: route.unauthenticated,
	});
}
const mediaProxySource = await (async () => {
	const sources: Array<string> = [];
	const walk = async (dir: string): Promise<void> => {
		for (const entry of await readdir(dir, {withFileTypes: true})) {
			const full = path.join(dir, entry.name);
			if (entry.isDirectory()) {
				await walk(full);
			} else if (entry.name.endsWith('.rs')) {
				sources.push(await readFile(full, 'utf8'));
			}
		}
	};
	await walk(MEDIA_PROXY_SERVER_DIR);
	return sources.join('\n');
})();

let failures = 0;

console.log('source anchors');
const staleAnchors: Array<string> = [];
for (const [route, anchor] of [...MEDIA_PROXY_ROUTES, ...MEDIA_PROXY_ASSET_PREFIXES]) {
	if (!mediaProxySource.includes(anchor)) {
		staleAnchors.push(`${route}: fluxer_media_proxy/src/server no longer contains ${anchor}`);
	}
}
const exemptSources = new Map<string, string>();
const sourceOf = async (file: string): Promise<string> => {
	let source = exemptSources.get(file);
	if (source == null) {
		source = await readFile(path.join(REPO_ROOT, file), 'utf8');
		exemptSources.set(file, source);
	}
	return source;
};
for (const [route, {file, anchor}] of MAIN_SPEC_EXEMPT) {
	if (!(await sourceOf(file)).includes(anchor)) {
		staleAnchors.push(`${route}: ${file} no longer contains ${anchor}`);
	}
}
for (const rule of EXEMPTION_RULES) {
	for (const {file, anchor} of rule.anchors) {
		if (!(await sourceOf(file)).includes(anchor)) {
			staleAnchors.push(`exemption rule "${rule.name}": ${file} no longer contains ${anchor}`);
		}
	}
}
for (const [route, {documentedIn}] of OUT_OF_BAND_CREDENTIAL) {
	if (documentedIn == null) {
		continue;
	}
	const page = await readFile(path.join(DOCS_ROOT, documentedIn.file), 'utf8');
	if (!page.includes(documentedIn.anchor)) {
		staleAnchors.push(`${route}: ${documentedIn.file} no longer contains ${documentedIn.anchor}`);
	}
}
failures += section('stale anchors (the code moved, update this script)', staleAnchors);

const mediaProxyDocumented = documented.filter((route) => route.file.startsWith('media-proxy/'));
const adminDocumented = documented.filter(
	(route) => !route.file.startsWith('media-proxy/') && stripVersionPrefix(route.path).startsWith('/admin'),
);
const mainDocumented = documented.filter(
	(route) => !route.file.startsWith('media-proxy/') && !stripVersionPrefix(route.path).startsWith('/admin'),
);

const main = await specOperations(MAIN_SPEC);
const admin = await specOperations(ADMIN_SPEC);
const mainShapes = new Set(main.map((operation) => shapeOf(operation.method, operation.path)));
const adminShapes = new Set(admin.map((operation) => shapeOf(operation.method, operation.path)));

const documentedMain = new Map<string, DocumentedRoute>();
for (const route of mainDocumented) {
	documentedMain.set(shapeOf(route.method, stripVersionPrefix(route.path)), route);
}
const documentedAdmin = new Map<string, DocumentedRoute>();
for (const route of adminDocumented) {
	documentedAdmin.set(shapeOf(route.method, stripVersionPrefix(route.path)), route);
}

const registered = new Map<string, string>();
for (const route of controllerRoutes) {
	const shape = astRoute(route);
	if (!registered.has(shape)) {
		registered.set(shape, `${path.relative(REPO_ROOT, route.controllerFile)}:${route.lineNumber.toString()}`);
	}
}

console.log('main API and admin API (from route registration)');
{
	const ruleCounts = new Map<string, number>();
	for (const rule of EXEMPTION_RULES) {
		ruleCounts.set(rule.name, 0);
	}
	const undocumented: Array<string> = [];
	for (const [shape, site] of [...registered].sort()) {
		if (documentedMain.has(shape) || documentedAdmin.has(shape) || aliasShapes.has(shape)) {
			continue;
		}
		const rule = EXEMPTION_RULES.find((candidate) => candidate.covers(shape, shape.split(' ')[1]));
		if (rule == null) {
			undocumented.push(`${shape}  registered at ${site}`);
			continue;
		}
		ruleCounts.set(rule.name, (ruleCounts.get(rule.name) ?? 0) + 1);
	}

	const documentedNotRegistered = [...documentedMain, ...documentedAdmin]
		.filter(([shape]) => !registered.has(shape))
		.map(([shape, route]) => `${shape}  (${route.file}) is documented but fluxer_api registers no such route`)
		.sort();
	const deadRules = EXEMPTION_RULES.filter((rule) => (ruleCounts.get(rule.name) ?? 0) === 0).map(
		(rule) => `the "${rule.name}" rule covers no registered route, so it is either stale or too narrow to matter`,
	);
	const staleEntries = [
		...[...DELIBERATELY_UNDOCUMENTED.keys()].map((shape) => [shape, 'DELIBERATELY_UNDOCUMENTED'] as const),
		...[...OUT_OF_BAND_CREDENTIAL.keys()].map((shape) => [shape, 'OUT_OF_BAND_CREDENTIAL'] as const),
	]
		.filter(([shape]) => !registered.has(shape))
		.map(([shape, list]) => `${list} names ${shape}, which fluxer_api no longer registers`)
		.sort();

	const exemptTotal = [...ruleCounts.values()].reduce((a, b) => a + b, 0);
	console.log(`  registered routes: ${registered.size.toString()}`);
	console.log(
		`  documented with a RouteHeader: ${[...registered.keys()].filter((shape) => documentedMain.has(shape) || documentedAdmin.has(shape)).length.toString()}`,
	);
	console.log(
		`  documented as a deprecated alias: ${[...registered.keys()].filter((shape) => aliasShapes.has(shape)).length.toString()}`,
	);
	console.log(`  exempt by rule: ${exemptTotal.toString()}`);
	for (const rule of EXEMPTION_RULES) {
		console.log(`    ${rule.name}: ${(ruleCounts.get(rule.name) ?? 0).toString()}. ${rule.justification}`);
	}
	console.log('  fluxer_admin is a Rust crate and its 83 HTML panel routes are NOT checked here. It serves');
	console.log('  zero API operations, so the admin API above is the /admin routes fluxer_api registers.');
	console.log('  fluxer_media_proxy and fluxer_app_proxy are Rust too. The media proxy is checked against');
	console.log('  the hand-maintained list below, and the app proxy is not checked at all.');
	failures += section('registered but neither documented nor covered by an exemption rule', undocumented);
	failures += section('documented but not registered', documentedNotRegistered);
	failures += section('exemption rules that cover nothing', deadRules);
	failures += section('exemption entries naming a route that is gone', staleEntries);
}

console.log('main API (against the generated spec)');
failures += section(
	'documented but absent from the live spec',
	[...documentedMain.entries()]
		.filter(([shape]) => !mainShapes.has(shape) && !MAIN_SPEC_EXEMPT.has(shape))
		.map(([, route]) => `${route.method} ${route.path}  (${route.file})`)
		.sort(),
);
failures += section(
	'present in the live spec but undocumented',
	main
		.filter((operation) => {
			const shape = shapeOf(operation.method, operation.path);
			if (documentedMain.has(shape)) {
				return false;
			}
			if (aliasShapes.has(shape)) {
				return false;
			}
			return !DELIBERATELY_UNDOCUMENTED.has(shape);
		})
		.map((operation) => `${operation.method} ${operation.path}  [${operation.tags.join(', ')}]`)
		.sort(),
);
failures += section(
	'documented as a deprecated alias but not registered',
	[...aliasShapes.values()]
		.filter((alias) => !registered.has(alias.shape))
		.map((alias) => `${alias.shape}  (${alias.file}) names a route fluxer_api does not register`)
		.sort(),
);
failures += section(
	'documented as a deprecated alias and with its own RouteHeader',
	[...aliasShapes.keys()]
		.filter((shape) => documentedMain.has(shape))
		.map((shape) => `${shape}  is an alias row and a RouteHeader, which double counts it`)
		.sort(),
);
console.log(`  documented as a deprecated alias of a documented route: ${aliasShapes.size.toString()}`);
const wronglyDocumented = [...DELIBERATELY_UNDOCUMENTED.entries()]
	.filter(([shape]) => documentedMain.has(shape))
	.map(([shape, reason]) => `${shape}  must not be documented: ${reason}`);
failures += section('documented despite belonging to an unshipped system', wronglyDocumented);
console.log(
	`  deliberately undocumented and correctly absent: ${(DELIBERATELY_UNDOCUMENTED.size - wronglyDocumented.length).toString()}/${DELIBERATELY_UNDOCUMENTED.size.toString()}`,
);
console.log(
	`  exempt non-spec routes documented: ${[...MAIN_SPEC_EXEMPT.keys()].filter((shape) => documentedMain.has(shape)).length.toString()}/${MAIN_SPEC_EXEMPT.size.toString()}`,
);
for (const [shape, {reason}] of MAIN_SPEC_EXEMPT) {
	console.log(`    ${shape}: ${reason}`);
}

console.log('media proxy');
const mediaProxyProblems: Array<string> = [];
for (const route of mediaProxyDocumented) {
	const shape = shapeOf(route.method, route.path);
	if (MEDIA_PROXY_ROUTES.has(shape)) {
		continue;
	}
	const firstSegment = route.path.replace(/^\//u, '').split('/')[0].split('.')[0];
	if (MEDIA_PROXY_ASSET_PREFIXES.has(firstSegment)) {
		if (route.method !== 'GET' && route.method !== 'HEAD') {
			mediaProxyProblems.push(`${route.method} ${route.path}  (${route.file}) asset paths serve GET and HEAD only`);
		}
		continue;
	}
	mediaProxyProblems.push(`${route.method} ${route.path}  (${route.file}) is not a route the media proxy serves`);
}
failures += section('documented but not served by fluxer_media_proxy', mediaProxyProblems);

console.log('gateway dispatch events');
{
	const gatewayConstants = await readFile(path.join(REPO_ROOT, 'fluxer_api/src/api/constants/Gateway.ts'), 'utf8');
	const unionMatch = gatewayConstants.match(/export type GatewayDispatchEvent\s*=([\s\S]*?);/u);
	const apiEvents = new Set<string>();
	if (unionMatch != null) {
		for (const entry of unionMatch[1].matchAll(/'([A-Z][A-Z0-9_]*)'/gu)) {
			apiEvents.add(entry[1]);
		}
	}
	const gatewayDirectory = path.join(REPO_ROOT, 'fluxer_gateway/src');
	const erlangSources: Array<string> = [];
	const collect = async (directory: string): Promise<void> => {
		for (const entry of await readdir(directory, {withFileTypes: true})) {
			const resolved = path.join(directory, entry.name);
			if (entry.isDirectory()) {
				await collect(resolved);
				continue;
			}
			if (entry.name.endsWith('.erl') || entry.name.endsWith('.hrl')) {
				erlangSources.push(await readFile(resolved, 'utf8'));
			}
		}
	};
	await collect(gatewayDirectory);
	const erlang = erlangSources.join('\n');

	const eventsPage = await readFile(path.join(DOCS_ROOT, 'gateway/events.md'), 'utf8');
	const documentedEvents = new Set<string>();
	for (const entry of eventsPage.matchAll(/^###\s+(?:<span[^>]*><\/span>)?([A-Z][A-Z0-9_]{3,})\s*$/gmu)) {
		documentedEvents.add(entry[1]);
	}

	const isReal = (event: string): boolean => {
		if (apiEvents.has(event)) {
			return true;
		}
		if (erlang.includes(event)) {
			return true;
		}
		return new RegExp(`\\b${event.toLowerCase()}\\b`, 'u').test(erlang);
	};

	const fabricated = [...documentedEvents].filter((event) => !isReal(event)).sort();
	const undocumented = [...apiEvents].filter((event) => !documentedEvents.has(event)).sort();
	console.log(`  events in the GatewayDispatchEvent union: ${apiEvents.size.toString()}`);
	console.log(`  events documented in gateway/events.md: ${documentedEvents.size.toString()}`);
	console.log('  a documented event counts as real if it is in the union, or appears in fluxer_gateway');
	console.log('  as an uppercase binary or a lowercase atom');
	failures += section('documented but not emitted by any service', fabricated);
	failures += section('in the dispatch union but undocumented', undocumented);
}

console.log('gateway opcodes and close codes');
{
	const erl = await readFile(path.join(REPO_ROOT, 'fluxer_gateway/src/utils/constants.erl'), 'utf8');
	const realOpcodes = new Set<number>();
	for (const entry of erl.matchAll(/gateway_opcode\((\d+)\) -> [a-z_]+/gu)) {
		realOpcodes.add(Number.parseInt(entry[1], 10));
	}
	const realCloseCodes = new Set<number>();
	for (const entry of erl.matchAll(/close_code_to_num\([a-z_]+\) -> (\d+)/gu)) {
		realCloseCodes.add(Number.parseInt(entry[1], 10));
	}
	const page = await readFile(path.join(DOCS_ROOT, 'gateway/opcodes-and-close-codes.md'), 'utf8');
	const docOpcodes = new Set<number>();
	const docCloseCodes = new Set<number>();
	const strippedPage = page.replace(/<sup>.*?<\/sup>/gu, '');
	for (const row of strippedPage.matchAll(/^\|\s*(\d{1,4})\s*\|/gmu)) {
		const value = Number.parseInt(row[1], 10);
		if (value >= 4000) {
			docCloseCodes.add(value);
			continue;
		}
		if (value <= 99) {
			docOpcodes.add(value);
		}
	}
	console.log(`  opcodes: ${realOpcodes.size.toString()} real, ${docOpcodes.size.toString()} documented`);
	console.log(`  close codes: ${realCloseCodes.size.toString()} real, ${docCloseCodes.size.toString()} documented`);
	failures += section(
		'opcodes documented but absent from constants.erl',
		[...docOpcodes]
			.filter((code) => !realOpcodes.has(code))
			.sort((a, b) => a - b)
			.map(String),
	);
	failures += section(
		'opcodes in constants.erl but undocumented',
		[...realOpcodes]
			.filter((code) => !docOpcodes.has(code))
			.sort((a, b) => a - b)
			.map(String),
	);
	failures += section(
		'close codes documented but absent from constants.erl',
		[...docCloseCodes]
			.filter((code) => !realCloseCodes.has(code))
			.sort((a, b) => a - b)
			.map(String),
	);
	failures += section(
		'close codes in constants.erl but undocumented',
		[...realCloseCodes]
			.filter((code) => !docCloseCodes.has(code))
			.sort((a, b) => a - b)
			.map(String),
	);
}

console.log('enum names and error codes');
{
	const liveTokens = new Set<string>();
	const scanDirectory = async (directory: string): Promise<void> => {
		let entries: Array<Dirent>;
		try {
			entries = await readdir(directory, {withFileTypes: true});
		} catch {
			return;
		}
		for (const entry of entries) {
			if (entry.name === 'node_modules' || entry.name === 'target' || entry.name === 'dist') {
				continue;
			}
			const resolved = path.join(directory, entry.name);
			if (entry.isDirectory()) {
				await scanDirectory(resolved);
				continue;
			}
			if (!/\.(ts|rs|erl|hrl|json)$/u.test(entry.name)) {
				continue;
			}
			const source = await readFile(resolved, 'utf8');
			for (const token of source.matchAll(/[A-Z][A-Z0-9_]+/gu)) {
				liveTokens.add(token[0]);
			}
		}
	};
	for (const sub of [
		'packages',
		'fluxer_api/src',
		'fluxer_admin/src',
		'fluxer_gateway/src',
		'fluxer_media_proxy/src',
	]) {
		await scanDirectory(path.join(REPO_ROOT, sub));
	}

	const enumRows: Array<{file: string; line: number; name: string}> = [];
	const codeRows: Array<{file: string; line: number; code: string}> = [];
	for (const file of await walk(DOCS_ROOT)) {
		const relative = path.relative(DOCS_ROOT, file);
		const lines = (await readFile(file, 'utf8')).split('\n');
		let inEnumTable = false;
		for (let i = 0; i < lines.length; i += 1) {
			const line = lines[i];
			if (/^\|\s*Value\s*\|\s*Name\s*\|/u.test(line)) {
				inEnumTable = true;
				continue;
			}
			if (inEnumTable && !line.startsWith('|')) {
				inEnumTable = false;
			}
			if (inEnumTable && !line.startsWith('| ---')) {
				const row = line.match(/^\|\s*-?\d+\s*\|\s*([A-Z][A-Z0-9_]*)/u);
				if (row != null) {
					enumRows.push({file: relative, line: i + 1, name: row[1]});
				}
			}
			for (const cited of line.matchAll(/\b[1-5]\d\d\s+`([A-Z][A-Z0-9_]{2,})`/gu)) {
				codeRows.push({file: relative, line: i + 1, code: cited[1]});
			}
		}
	}

	const fabricatedEnums = enumRows.filter((row) => !liveTokens.has(row.name));
	const fabricatedCodes = codeRows.filter((row) => !liveTokens.has(row.code));
	console.log(`  enum rows checked: ${enumRows.length.toString()}`);
	console.log(`  error codes cited with a status: ${new Set(codeRows.map((r) => r.code)).size.toString()}`);
	console.log('  names only, not value bindings: the same name is reused across enums, so a global');
	console.log('  value comparison misattributes. ReportType.GUILD is 2 while ChannelContext.GUILD is 0');
	failures += section(
		'enum names absent from live source',
		fabricatedEnums.map((row) => `${row.file}:${row.line.toString()} ${row.name}`).sort(),
	);
	failures += section(
		'error codes absent from live source',
		[...new Set(fabricatedCodes.map((row) => `${row.file}:${row.line.toString()} ${row.code}`))].sort(),
	);
}

console.log('registry codes with a producer');
{
	const CODE_REGISTRIES = ['packages/constants/src/ApiErrorCodes.ts', 'packages/constants/src/ValidationErrorCodes.ts'];
	const NON_PRODUCERS = [...CODE_REGISTRIES, 'packages/errors/src/i18n/ErrorCodeMappings.ts'];
	const registryKeys = new Map<string, string>();
	for (const relative of CODE_REGISTRIES) {
		const source = await readFile(path.join(REPO_ROOT, relative), 'utf8');
		for (const entry of source.matchAll(/^\t([A-Z][A-Z0-9_]*):/gmu)) {
			registryKeys.set(entry[1], relative);
		}
	}

	const producerTokens = new Set<string>();
	const scanProducers = async (directory: string): Promise<void> => {
		let entries: Array<Dirent>;
		try {
			entries = await readdir(directory, {withFileTypes: true});
		} catch {
			return;
		}
		for (const entry of entries) {
			if (entry.name === 'node_modules' || entry.name === 'target' || entry.name === 'dist' || entry.name === 'tests') {
				continue;
			}
			const resolved = path.join(directory, entry.name);
			if (entry.isDirectory()) {
				await scanProducers(resolved);
				continue;
			}
			if (!/\.(ts|rs|erl|hrl)$/u.test(entry.name) || entry.name.endsWith('.test.ts')) {
				continue;
			}
			if (NON_PRODUCERS.includes(path.relative(REPO_ROOT, resolved))) {
				continue;
			}
			const source = await readFile(resolved, 'utf8');
			for (const token of source.matchAll(/\b[A-Z][A-Z0-9_]+\b/gu)) {
				producerTokens.add(token[0]);
			}
		}
	};
	for (const sub of [
		'packages',
		'fluxer_api/src',
		'fluxer_admin/src',
		'fluxer_gateway/src',
		'fluxer_media_proxy/src',
		'fluxer_unfurl/src',
	]) {
		await scanProducers(path.join(REPO_ROOT, sub));
	}

	const withoutProducer = [...registryKeys]
		.filter(([code]) => !producerTokens.has(code))
		.map(([code, relative]) => `${relative} ${code}`)
		.sort();
	console.log(`  registry codes checked: ${registryKeys.size.toString()}`);
	console.log('  a producer is any live server file outside the two registries, ErrorCodeMappings.ts and the');
	console.log('  generated specs. Test files do not count, the api test harness controller does');
	failures += section('registry codes with no producer', withoutProducer);
}

console.log('permission bits');
{
	const constants = await readFile(path.join(REPO_ROOT, 'packages/constants/src/ChannelConstants.ts'), 'utf8');
	const block = constants.match(/export const Permissions = \{([\s\S]*?)\} as const;/u);
	const livePermissions = new Map<string, number>();
	if (block != null) {
		for (const entry of block[1].matchAll(/([A-Z][A-Z0-9_]*):\s*1n\s*<<\s*(\d+)n/gu)) {
			livePermissions.set(entry[1], Number.parseInt(entry[2], 10));
		}
	}
	const page = (await readFile(path.join(DOCS_ROOT, 'http-api/permissions.mdx'), 'utf8'))
		.replace(/&lt;/gu, '<')
		.replace(/&gt;/gu, '>');
	const documentedPermissions = new Map<string, number>();
	for (const row of page.matchAll(/^\|\s*1\s*<<\s*(\d+)\s*\|\s*([A-Z][A-Z0-9_]*)/gmu)) {
		documentedPermissions.set(row[2], Number.parseInt(row[1], 10));
	}
	console.log(`  live permissions: ${livePermissions.size.toString()}`);
	console.log(`  documented permissions: ${documentedPermissions.size.toString()}`);
	const bitMismatches: Array<string> = [];
	for (const [name, bit] of documentedPermissions) {
		const liveBit = livePermissions.get(name);
		if (liveBit != null && liveBit !== bit) {
			bitMismatches.push(`${name}: documented 1 << ${bit.toString()}, live 1 << ${liveBit.toString()}`);
		}
	}
	failures += section('permission bit mismatches', bitMismatches);
	failures += section(
		'documented but not a live permission',
		[...documentedPermissions.keys()].filter((name) => !livePermissions.has(name)).sort(),
	);
	failures += section(
		'live permission but undocumented',
		[...livePermissions.keys()].filter((name) => !documentedPermissions.has(name)).sort(),
	);
}

console.log('rate limit buckets, limits and windows');
{
	const UNIT_MS = new Map([
		['second', 1000],
		['seconds', 1000],
		['minute', 60_000],
		['minutes', 60_000],
		['hour', 3_600_000],
		['hours', 3_600_000],
		['day', 86_400_000],
		['days', 86_400_000],
	]);
	const toMs = (text: string): number | null => {
		const parsed = text.trim().match(/^(\d+)?\s*([a-z]+)$/u);
		if (parsed == null) {
			return null;
		}
		const unit = UNIT_MS.get(parsed[2]);
		if (unit == null) {
			return null;
		}
		return Number.parseInt(parsed[1] ?? '1', 10) * unit;
	};

	const configDirectory = path.join(REPO_ROOT, 'fluxer_api/src/api/rate_limit_configs');
	const liveBuckets = new Map<string, {limit: number; windowMs: number | null}>();
	for (const entry of await readdir(configDirectory)) {
		if (!entry.endsWith('.ts')) {
			continue;
		}
		const source = await readFile(path.join(configDirectory, entry), 'utf8');
		for (const config of source.matchAll(
			/bucket:\s*'([^']+)',\s*config:\s*\{limit:\s*(\d+),\s*windowMs:\s*ms\('([^']+)'\)/gu,
		)) {
			liveBuckets.set(config[1], {limit: Number.parseInt(config[2], 10), windowMs: toMs(config[3])});
		}
	}

	const claimPattern =
		/([\d,]+) requests? per ([a-z0-9 ]+?)(?:,| for [^.]*?,) on the (?:shared )?`([a-z0-9_:@{}]+)` bucket/gu;
	const problems: Array<string> = [];
	let claims = 0;
	for (const file of await walk(DOCS_ROOT)) {
		const relative = path.relative(DOCS_ROOT, file);
		const lines = (await readFile(file, 'utf8')).split('\n');
		for (let i = 0; i < lines.length; i += 1) {
			for (const claim of lines[i].matchAll(claimPattern)) {
				claims += 1;
				const bucket = claim[3];
				const limit = Number.parseInt(claim[1].replace(/,/gu, ''), 10);
				const windowMs = toMs(claim[2]);
				const live = liveBuckets.get(bucket);
				if (live == null) {
					problems.push(`${relative}:${(i + 1).toString()} bucket ${bucket} is not in any rate limit config`);
					continue;
				}
				if (live.limit !== limit) {
					problems.push(
						`${relative}:${(i + 1).toString()} ${bucket} documented limit ${limit.toString()}, live ${live.limit.toString()}`,
					);
				}
				if (windowMs != null && live.windowMs != null && live.windowMs !== windowMs) {
					problems.push(
						`${relative}:${(i + 1).toString()} ${bucket} documented window ${windowMs.toString()}ms, live ${live.windowMs.toString()}ms`,
					);
				}
			}
		}
	}
	console.log(`  buckets defined with a limit and window: ${liveBuckets.size.toString()}`);
	console.log(`  documented rate limit claims parsed: ${claims.toString()}`);
	failures += section('rate limit disagreements', problems);
}

console.log('instance limit keys');
{
	const defaults = await readFile(path.join(REPO_ROOT, 'packages/limits/src/LimitDefaults.ts'), 'utf8');
	const liveKeys = new Set<string>();
	for (const entry of defaults.matchAll(/^\s+([a-z][a-z0-9_]*):/gmu)) {
		liveKeys.add(entry[1]);
	}
	const instancePage = await readFile(path.join(DOCS_ROOT, 'http-api/instance.mdx'), 'utf8');
	const documentedKeys = new Set<string>();
	for (const row of instancePage.matchAll(/^\|\s*([^|]+?)\s*\|/gmu)) {
		const name = row[1]
			.replace(/<sup>.*?<\/sup>/gu, '')
			.replace(/`/gu, '')
			.trim();
		if (/^[a-z][a-z0-9_]*$/u.test(name)) {
			documentedKeys.add(name);
		}
	}
	console.log(`  live limit keys: ${liveKeys.size.toString()}`);
	console.log(
		`  documented in the instance limit-keys table: ${[...liveKeys].filter((k) => documentedKeys.has(k)).length.toString()}`,
	);
	failures += section(
		'live limit keys missing from the instance table',
		[...liveKeys].filter((key) => !documentedKeys.has(key)).sort(),
	);
}

console.log('media proxy image constants');
{
	const constants = await readFile(path.join(REPO_ROOT, 'fluxer_media_proxy/src/constants.rs'), 'utf8');
	const ladderBlock = constants.match(/pub const IMAGE_SIZES: &\[u32\] = &\[([\s\S]*?)\];/u);
	const ladder = ladderBlock == null ? [] : [...ladderBlock[1].matchAll(/\d+/gu)].map((m) => Number.parseInt(m[0], 10));
	const defaultSize = constants.match(/pub const DEFAULT_IMAGE_SIZE: u32 = (\d+);/u);
	const dimsBlock = constants.match(/pub fn dims_for\(kind: AssetKind\) -> Option<Dims> \{([\s\S]*?)\n\}/u);
	const dims: Array<[string, number, number]> = [];
	if (dimsBlock != null) {
		for (const entry of dimsBlock[1].matchAll(
			/((?:AssetKind::\w+\s*\|?\s*)+)=> Some\(Dims \{\s*min:\s*(\d+),\s*max:\s*(\d+),?\s*\}\)/gu,
		)) {
			dims.push([entry[1].trim(), Number.parseInt(entry[2], 10), Number.parseInt(entry[3], 10)]);
		}
	}

	const LADDER_PAGE = 'media-proxy/transformations.md';

	const ladderPage = (await readFile(path.join(DOCS_ROOT, LADDER_PAGE), 'utf8')).replace(/(?<=\d),(?=\d)/gu, '');
	const problems: Array<string> = [];

	const documentedLadder = ladderPage.match(
		/`size` selects from a fixed ladder: ([\d,\s]+(?:and\s+\d+)?)\. A requested value snaps up/u,
	);
	if (documentedLadder == null) {
		problems.push(`${LADDER_PAGE} no longer states the size ladder in the expected sentence`);
	} else {
		const listed = [...documentedLadder[1].matchAll(/\d+/gu)].map((m) => Number.parseInt(m[0], 10));
		if (listed.join(',') !== ladder.join(',')) {
			problems.push(`size ladder differs. documented [${listed.join(', ')}] vs IMAGE_SIZES [${ladder.join(', ')}]`);
		}
	}

	if (defaultSize != null && !ladderPage.includes(`resolves to ${defaultSize[1]} before clamping`)) {
		problems.push(`${LADDER_PAGE} does not state that an absent size resolves to ${defaultSize[1]} before clamping`);
	}

	for (const [kinds, min, max] of dims) {
		if (!ladderPage.includes(`${min.toString()} through ${max.toString()}`)) {
			problems.push(`${LADDER_PAGE} does not state the ${kinds} clamp of ${min.toString()} to ${max.toString()}`);
		}
	}

	console.log(`  size ladder rungs in constants.rs: ${ladder.length.toString()}`);
	console.log(`  asset classes with a clamp: ${dims.length.toString()}`);
	failures += section('media proxy constant disagreements', problems);
}

console.log('self-hosting guide against deploy/self-hosting');
{
	const envExample = await readFile(path.join(REPO_ROOT, 'deploy/self-hosting/.env.example'), 'utf8');
	const guide = await readFile(path.join(DOCS_ROOT, 'operator/get-started.mdx'), 'utf8');

	const placeholders = new Set<string>();
	const declared = new Set<string>();
	const assigned = new Set<string>();
	for (const line of envExample.split('\n')) {
		const commented = line.startsWith('#');
		const assignment = (commented ? line.slice(1) : line).match(/^([A-Z][A-Z0-9_]*)=(.*)$/u);
		if (assignment == null) {
			continue;
		}
		declared.add(assignment[1]);
		if (commented) {
			continue;
		}
		assigned.add(assignment[1]);
		if (assignment[2].includes('CHANGE_ME')) {
			placeholders.add(assignment[1]);
		}
	}

	const INSTALLER_ROOT = fileURLToPath(new URL('../src/installer/', import.meta.url));
	const installers: ReadonlyArray<readonly [string, string]> = [
		['install.sh', await readFile(path.join(INSTALLER_ROOT, 'install.sh'), 'utf8')],
		['install.ps1', await readFile(path.join(INSTALLER_ROOT, 'install.ps1'), 'utf8')],
	];
	const shellSource = installers[0][1];
	const powershellSource = installers[1][1];

	const problems: Array<string> = [];

	const INSTALLER_SECRET_FLOOR = 14;
	const INSTALLER_NON_SECRET_FLOOR = 7;
	const COMPOSE_REQUIRED_FLOOR = 17;

	const blockBetween = (source: string, opener: string, closer: string, label: string): string | null => {
		const from = source.indexOf(opener);
		if (from === -1) {
			problems.push(`${label} is gone. Nothing in the script opens with \`${opener.trim()}\``);
			return null;
		}
		const rest = source.slice(from + opener.length);
		const to = rest.indexOf(closer);
		if (to === -1) {
			problems.push(`${label} opens but never closes at \`${closer.trim()}\``);
			return null;
		}
		return rest.slice(0, to);
	};

	const shellRows = (fn: string, delimiter: string, label: string): Array<string> => {
		const body = blockBetween(shellSource, `${fn}() {\n\tcat <<'${delimiter}'\n`, `\n${delimiter}\n`, label);
		if (body == null) {
			return [];
		}
		return body.split('\n').filter((row) => row.trim().length > 0);
	};

	const powershellRows = (variable: string, label: string): Array<string> => {
		const body = blockBetween(powershellSource, `$${variable} = @(\n`, '\n)\n', label);
		if (body == null) {
			return [];
		}
		return body.split('\n').filter((row) => row.trim().length > 0);
	};

	const parseShellKeys = (fn: string, label: string): Map<string, string> => {
		const keys = new Map<string, string>();
		for (const row of shellRows(fn, 'KEYS', label)) {
			const parsed = row.match(/^([A-Z][A-Z0-9_]*) ([a-z0-9_]+)(?: .*)?$/u);
			if (parsed == null) {
				problems.push(`${label} carries the row \`${row}\`, which is not "NAME kind"`);
				continue;
			}
			keys.set(parsed[1], parsed[2]);
		}
		return keys;
	};

	const parsePowerShellKeys = (variable: string, label: string): Map<string, string> => {
		const keys = new Map<string, string>();
		for (const row of powershellRows(variable, label)) {
			const parsed = row.match(/^\s*@\{Name = '([A-Z][A-Z0-9_]*)'; Kind = '([a-z0-9_]+)'/u);
			if (parsed == null) {
				problems.push(`${label} carries the row \`${row.trim()}\`, which is not an @{Name; Kind} entry`);
				continue;
			}
			keys.set(parsed[1], parsed[2]);
		}
		return keys;
	};

	const shellSecrets = parseShellKeys('fluxer_secret_keys', 'the install.sh secret list');
	const shellNonSecrets = parseShellKeys('fluxer_non_secret_keys', 'the install.sh non-secret list');
	const powershellSecrets = parsePowerShellKeys('FluxerSecretKeys', 'the install.ps1 secret list');
	const powershellNonSecrets = parsePowerShellKeys('FluxerNonSecretKeys', 'the install.ps1 non-secret list');

	const shellWrites = new Set<string>([...shellSecrets.keys(), ...shellNonSecrets.keys()]);
	const powershellWrites = new Set<string>([...powershellSecrets.keys(), ...powershellNonSecrets.keys()]);

	for (const [name, secrets, nonSecrets] of [
		['install.sh', shellSecrets, shellNonSecrets],
		['install.ps1', powershellSecrets, powershellNonSecrets],
	] as const) {
		if (secrets.size < INSTALLER_SECRET_FLOOR) {
			problems.push(
				`${name} declares ${secrets.size.toString()} secret keys, floor is ${INSTALLER_SECRET_FLOOR.toString()}`,
			);
		}
		if (nonSecrets.size < INSTALLER_NON_SECRET_FLOOR) {
			problems.push(
				`${name} declares ${nonSecrets.size.toString()} non-secret keys, floor is ${INSTALLER_NON_SECRET_FLOOR.toString()}`,
			);
		}
	}

	for (const name of placeholders) {
		for (const [script, secrets] of [
			['install.sh', shellSecrets],
			['install.ps1', powershellSecrets],
		] as const) {
			if (!secrets.has(name)) {
				problems.push(`${name} ships as CHANGE_ME but ${script} never sets it`);
			}
		}
	}

	for (const name of new Set<string>([...shellWrites, ...powershellWrites])) {
		if (!declared.has(name)) {
			problems.push(`the installer writes ${name}, which is not declared in .env.example`);
		}
	}

	const compareKeyLists = (label: string, shell: Map<string, string>, powershell: Map<string, string>): void => {
		for (const [name, kind] of shell) {
			const other = powershell.get(name);
			if (other == null) {
				problems.push(`install.sh writes the ${label} ${name} and install.ps1 does not`);
				continue;
			}
			if (other !== kind) {
				problems.push(`${name} is ${kind} in install.sh and ${other} in install.ps1`);
			}
		}
		for (const name of powershell.keys()) {
			if (!shell.has(name)) {
				problems.push(`install.ps1 writes the ${label} ${name} and install.sh does not`);
			}
		}
	};
	compareKeyLists('secret', shellSecrets, powershellSecrets);
	compareKeyLists('non-secret value', shellNonSecrets, powershellNonSecrets);

	const expectedSecretKind = (name: string): string => {
		if (name === 'FLUXER_VAPID_PUBLIC_KEY') {
			return 'vapid_public';
		}
		if (name === 'FLUXER_VAPID_PRIVATE_KEY') {
			return 'vapid_private';
		}
		if (name.endsWith('_BASE64')) {
			return 'base64';
		}
		return 'hex';
	};
	for (const [script, secrets] of [
		['install.sh', shellSecrets],
		['install.ps1', powershellSecrets],
	] as const) {
		for (const [name, kind] of secrets) {
			const wanted = expectedSecretKind(name);
			if (kind !== wanted) {
				problems.push(`${script} generates ${name} as ${kind}, and the value has to be ${wanted}`);
			}
		}
	}

	const shippedAssets = (await readdir(path.join(REPO_ROOT, 'deploy/self-hosting')))
		.filter((entry) => entry !== '.gitignore')
		.sort();
	const shellStackFiles = shellRows('fluxer_stack_files', 'FILES', 'the install.sh download list');
	const powershellStackFiles: Array<string> = [];
	for (const row of powershellRows('FluxerStackFiles', 'the install.ps1 download list')) {
		const parsed = row.match(/^\s*'([^']+)'\s*$/u);
		if (parsed == null) {
			problems.push(`the install.ps1 download list carries \`${row.trim()}\`, which is not a quoted file name`);
			continue;
		}
		powershellStackFiles.push(parsed[1]);
	}
	for (const [script, list] of [
		['install.sh', shellStackFiles],
		['install.ps1', powershellStackFiles],
	] as const) {
		const sorted = [...list].sort();
		if (sorted.join(', ') !== shippedAssets.join(', ')) {
			problems.push(
				`${script} downloads [${sorted.join(', ')}] and deploy/self-hosting ships [${shippedAssets.join(', ')}]`,
			);
		}
	}

	const PIPE_TO_SHELL =
		/(?:curl|wget|iwr|Invoke-WebRequest)[^\n|]*\|\s*(?:sudo\s+)?(?:sh|bash|zsh|iex|Invoke-Expression)\b/iu;
	const docsPages = await walk(DOCS_ROOT);
	for (const [name, source] of installers) {
		if (PIPE_TO_SHELL.test(source)) {
			problems.push(`${name} pipes a download into a shell`);
		}
	}
	for (const page of docsPages) {
		if (PIPE_TO_SHELL.test(await readFile(page, 'utf8'))) {
			problems.push(`${path.relative(DOCS_ROOT, page)} pipes a download into a shell`);
		}
	}

	for (const [name, source] of installers) {
		if (source.includes('—')) {
			problems.push(`${name} contains an em dash`);
		}
		if (source.includes('\r')) {
			problems.push(`${name} contains a CR byte, so the served digest would disagree with sha256sum on the file`);
		}
		if (!source.endsWith('\n') || source.endsWith('\n\n')) {
			problems.push(`${name} does not end with exactly one newline`);
		}
	}

	for (const [name, source] of installers) {
		const published = installerChecksumLine(name, source);
		const expected = `${createHash('sha256').update(Buffer.from(source, 'utf8')).digest('hex')}  ${name}\n`;
		if (published !== expected) {
			problems.push(
				`the published checksum for ${name} is \`${published.trim()}\` and sha256 of the file is \`${expected.trim()}\``,
			);
		}
	}

	const INSTALLER_ENDPOINTS = ['install.sh.ts', 'install.sh.sha256.ts', 'install.ps1.ts', 'install.ps1.sha256.ts'];
	const digestBearing: Array<[string, string]> = [];
	for (const endpoint of INSTALLER_ENDPOINTS) {
		const source = await readFile(fileURLToPath(new URL(`../src/pages/${endpoint}`, import.meta.url)), 'utf8');
		if (!source.includes("from '../installer/Installer'")) {
			problems.push(`src/pages/${endpoint} no longer serves the script through src/installer/Installer`);
		}
		digestBearing.push([`src/pages/${endpoint}`, source]);
	}
	digestBearing.push([
		'src/components/InstallerChecksum.astro',
		await readFile(fileURLToPath(new URL('../src/components/InstallerChecksum.astro', import.meta.url)), 'utf8'),
	]);
	for (const page of docsPages) {
		const text = await readFile(page, 'utf8');
		if (text.includes('install.sh') || text.includes('install.ps1')) {
			digestBearing.push([path.relative(DOCS_ROOT, page), text]);
		}
	}
	for (const [where, text] of digestBearing) {
		if (/\b[0-9a-f]{64}\b/u.test(text)) {
			problems.push(
				`${where} carries a literal 64-character hex digest, which goes stale the next time a script changes`,
			);
		}
	}

	const shellParse = spawnSync('sh', ['-n', path.join(INSTALLER_ROOT, 'install.sh')], {encoding: 'utf8'});
	if (shellParse.error != null) {
		problems.push(`sh -n could not run against install.sh: ${shellParse.error.message}`);
	} else if (shellParse.status !== 0) {
		problems.push(`sh -n rejects install.sh: ${shellParse.stderr.trim()}`);
	}

	const DOCKER_STUB = [
		'#!/bin/sh',
		'case "$1 $2" in',
		"	'--version ') echo 'Docker version 27.1.1, build stub' ;;",
		"	'compose version') if [ \"$3\" = '--short' ]; then echo '2.30.3'; else echo 'v2.30.3'; fi ;;",
		"	'compose config') echo 'ghcr.io/fluxerapp/fluxer-api:v1' ;;",
		'esac',
		'exit 0',
		'',
	].join('\n');
	const CURL_STUB = [
		'#!/bin/sh',
		"out=''",
		"prev=''",
		'for arg in "$@"; do',
		'	if [ "$prev" = \'-o\' ]; then out=$arg; fi',
		'	prev=$arg',
		'done',
		'[ -z "$out" ] || printf \'name: fluxer\\nservices:\\n  api:\\n    image: stub\\n\' > "$out"',
		'',
	].join('\n');

	const sandbox = await mkdtemp(path.join(tmpdir(), 'fluxer-installer-'));
	try {
		const stubBin = path.join(sandbox, 'bin');
		await mkdir(stubBin, {recursive: true});
		await writeFile(path.join(stubBin, 'docker'), DOCKER_STUB, {mode: 0o755});
		await writeFile(path.join(stubBin, 'curl'), CURL_STUB, {mode: 0o755});
		await writeFile(path.join(stubBin, 'openssl'), '#!/bin/sh\nexit 0\n', {mode: 0o755});

		const instance = path.join(sandbox, 'instance');
		await mkdir(instance, {recursive: true});
		await writeFile(path.join(instance, '.env'), 'FLUXER_DOMAIN=x.example\nFLUXER_IMAGE_TAG=2026.813.205040\n');
		await writeFile(path.join(instance, 'docker-compose.yml'), 'name: fluxer\nservices:\n  api:\n    image: stub\n');

		const plannedRef = (label: string, args: ReadonlyArray<string>): string | null => {
			const run = spawnSync('sh', [path.join(INSTALLER_ROOT, 'install.sh'), ...args], {
				encoding: 'utf8',
				env: {...process.env, PATH: `${stubBin}${path.delimiter}${process.env.PATH ?? ''}`},
			});
			if (run.error != null) {
				problems.push(`install.sh ${label} could not run: ${run.error.message}`);
				return null;
			}
			if (run.status !== 0) {
				problems.push(`install.sh ${label} exited ${String(run.status)}: ${run.stderr.trim()}`);
				return null;
			}
			const line = run.stdout.match(/^ {2}ref\s+(\S+)$/mu);
			if (line == null) {
				problems.push(`install.sh ${label} printed no ref line`);
				return null;
			}
			return line[1];
		};

		const INSTALL_ARGS = [
			'--dry-run',
			'--non-interactive',
			'--allow-root',
			'--domain',
			'x.example',
			'--email',
			'a@x.example',
			'--dir',
			path.join(sandbox, 'target'),
		];
		const REF_CASES: ReadonlyArray<readonly [string, ReadonlyArray<string>, string]> = [
			['on the default image tag', INSTALL_ARGS, 'main'],
			['under --image-tag latest', [...INSTALL_ARGS, '--image-tag', 'latest'], 'main'],
			['under --image-tag 2026.813.205040', [...INSTALL_ARGS, '--image-tag', '2026.813.205040'], '2026.813.205040'],
			[
				'under --ref feature/x --image-tag 2026.813.205040',
				[...INSTALL_ARGS, '--ref', 'feature/x', '--image-tag', '2026.813.205040'],
				'feature/x',
			],
			[
				'under --update against a pinned .env',
				['--update', '--dry-run', '--allow-root', '--dir', instance],
				'2026.813.205040',
			],
		];
		for (const [label, args, expected] of REF_CASES) {
			const resolved = plannedRef(label, args);
			if (resolved != null && resolved !== expected) {
				problems.push(`install.sh ${label} plans ref ${resolved}, and the image tag it pairs with wants ${expected}`);
			}
		}
	} finally {
		await rm(sandbox, {recursive: true, force: true});
	}

	const covered = new Set<string>(
		[...placeholders].filter((name) => shellSecrets.has(name) && powershellSecrets.has(name)),
	);

	const compose = await readFile(path.join(REPO_ROOT, 'deploy/self-hosting/docker-compose.yml'), 'utf8');
	const composeLines = compose.split('\n');

	const composeRequired = new Set<string>();
	for (const required of compose.matchAll(/\$\{([A-Z][A-Z0-9_]*):\?/gu)) {
		composeRequired.add(required[1]);
	}
	if (composeRequired.size < COMPOSE_REQUIRED_FLOOR) {
		problems.push(
			`docker-compose.yml parsed to ${composeRequired.size.toString()} :?-required variables, floor is ${COMPOSE_REQUIRED_FLOOR.toString()}`,
		);
	}
	for (const required of compose.matchAll(/\$\{([A-Z][A-Z0-9_]*):\?([^}]*)\}/gu)) {
		if (required[2].trim().length === 0) {
			problems.push(
				`docker-compose.yml requires ${required[1]} with an empty :? message, so Compose stops on a bare variable name`,
			);
		}
	}
	const vapidEmailDerivation = 'FLUXER_VAPID_EMAIL: ${FLUXER_VAPID_EMAIL:-admin@${FLUXER_DOMAIN}}';
	if (!compose.includes(vapidEmailDerivation)) {
		problems.push('docker-compose.yml no longer derives FLUXER_VAPID_EMAIL from FLUXER_DOMAIN');
	} else if (assigned.has('FLUXER_VAPID_EMAIL')) {
		problems.push(
			'.env.example assigns FLUXER_VAPID_EMAIL, so a copied .env defeats the admin@FLUXER_DOMAIN derivation',
		);
	}
	let composeRequiredCovered = 0;
	for (const name of composeRequired) {
		const missing: Array<string> = [];
		if (!shellWrites.has(name)) {
			missing.push('install.sh');
		}
		if (!powershellWrites.has(name)) {
			missing.push('install.ps1');
		}
		if (missing.length === 0) {
			composeRequiredCovered += 1;
			continue;
		}
		problems.push(`docker-compose.yml requires ${name} with :? and ${missing.join(' and ')} never writes it`);
	}
	const publishers = new Set<string>();
	const publishedPorts = new Set<string>();
	let currentService: string | null = null;
	for (let i = 0; i < composeLines.length; i += 1) {
		const service = composeLines[i].match(/^ {2}([a-z][a-z0-9_-]*):\s*$/u);
		if (service != null) {
			currentService = service[1];
			continue;
		}
		if (!/^ {4}ports:\s*$/u.test(composeLines[i]) || currentService == null) {
			continue;
		}
		publishers.add(currentService);
		for (let j = i + 1; j < composeLines.length && /^\s*-\s/u.test(composeLines[j]); j += 1) {
			const mapping = composeLines[j].match(/(\d+)\}?(\/udp)?"\s*$/u);
			if (mapping != null) {
				publishedPorts.add(`${mapping[1]}${mapping[2] ?? ''}`);
			}
		}
	}
	if (!guide.includes('The edge and LiveKit are the only services that publish ports')) {
		problems.push('the guide no longer states which services publish ports');
	} else if (publishers.size !== 2 || !publishers.has('edge') || !publishers.has('livekit')) {
		problems.push(
			`the guide says only the edge and LiveKit publish ports, but the compose file publishes from [${[...publishers].sort().join(', ')}]`,
		);
	}
	for (const port of publishedPorts) {
		const bare = port.replace('/udp', '');
		if (!guide.includes(bare)) {
			problems.push(`the compose file publishes ${port} but the guide never mentions ${bare}`);
		}
	}

	const livekitConfig = blockBetween(
		compose,
		'      LIVEKIT_CONFIG: |\n',
		'\n    ports:\n',
		"the livekit service's LIVEKIT_CONFIG",
	);
	if (livekitConfig != null) {
		const coupled: ReadonlyArray<readonly [string, string]> = [
			['tcp_port', '${FLUXER_LIVEKIT_TCP_PORT:-7881}'],
			['udp_port', '${FLUXER_LIVEKIT_UDP_PORT:-7882}'],
			['api_key', '${LIVEKIT_API_KEY:?set LIVEKIT_API_KEY in .env}'],
		];
		for (const [key, expression] of coupled) {
			if (!livekitConfig.includes(`${key}: ${expression}`)) {
				problems.push(`LIVEKIT_CONFIG sets ${key} to something other than ${expression}, so it no longer follows .env`);
			}
		}
		for (const mapping of [
			'${FLUXER_LIVEKIT_TCP_PORT:-7881}:${FLUXER_LIVEKIT_TCP_PORT:-7881}',
			'${FLUXER_LIVEKIT_UDP_PORT:-7882}:${FLUXER_LIVEKIT_UDP_PORT:-7882}/udp',
		]) {
			if (!compose.includes(`"${mapping}"`)) {
				problems.push(
					`the livekit service no longer publishes ${mapping}, so the port it advertises in ICE candidates is not the one the host forwards`,
				);
			}
		}
	}

	const configPage = await readFile(path.join(DOCS_ROOT, 'operator/configuration.mdx'), 'utf8');
	const configLines = configPage.split('\n');
	const envValues = new Map<string, string>();
	for (const line of envExample.split('\n')) {
		const assignment = line.match(/^([A-Z][A-Z0-9_]*)=(.*)$/u);
		if (assignment != null) {
			envValues.set(assignment[1], assignment[2]);
		}
	}

	const ENV_VALUE_FLOOR = 19;
	const ENV_VALUE_UNSET = 'unset';
	let inEnvValueTable = false;
	let envEntry: string | null = null;
	let envRowsChecked = 0;
	const checkEnvValue = (name: string, documented: string): void => {
		envRowsChecked += 1;
		const actual = envValues.get(name);
		if (documented === ENV_VALUE_UNSET) {
			if (actual != null) {
				problems.push(`configuration.mdx says ${name} is unset but .env.example assigns \`${actual}\``);
			}
			return;
		}
		if (actual == null) {
			problems.push(`configuration.mdx documents ${name} under "Value in .env.example" but it is not in that file`);
			return;
		}
		if (actual !== documented) {
			problems.push(`configuration.mdx says ${name} is \`${documented}\` but .env.example has \`${actual}\``);
		}
	};
	for (const line of configLines) {
		const entryHeading = line.match(/^#### `([A-Z][A-Z0-9_]*)`(?:<sup>[^<]*<\/sup>)?\s*$/u);
		if (entryHeading != null) {
			envEntry = entryHeading[1];
			inEnvValueTable = false;
			continue;
		}
		if (/^#{1,6}\s/u.test(line)) {
			envEntry = null;
		}
		if (envEntry != null) {
			const marker = line.match(/^`\.env\.example`\s+(?:`([^`]*)`|(unset)\b)/u);
			if (marker != null) {
				checkEnvValue(envEntry, marker[1] ?? ENV_VALUE_UNSET);
				envEntry = null;
				continue;
			}
		}
		if (/^\|\s*Variable\s*\|/u.test(line)) {
			inEnvValueTable = line.includes('Value in `.env.example`');
			continue;
		}
		if (!line.startsWith('|')) {
			inEnvValueTable = false;
			continue;
		}
		if (!inEnvValueTable) {
			continue;
		}
		const row = line.match(/^\|\s*([A-Z][A-Z0-9_]*)(?:<sup>[^<]*<\/sup>)?\s*\|\s*(?:`([^`]*)`|(unset))\s*\|/u);
		if (row == null) {
			continue;
		}
		checkEnvValue(row[1], row[2] ?? ENV_VALUE_UNSET);
	}
	console.log(`  configuration.mdx .env.example values checked: ${envRowsChecked.toString()}`);
	if (envRowsChecked < ENV_VALUE_FLOOR) {
		problems.push(
			`configuration.mdx .env.example values checked fell to ${envRowsChecked.toString()}, floor is ${ENV_VALUE_FLOOR.toString()}`,
		);
	}

	const composeServices = new Set<string>();
	{
		let inServices = false;
		for (const line of composeLines) {
			if (/^services:\s*$/u.test(line)) {
				inServices = true;
				continue;
			}
			if (/^[a-z]/u.test(line)) {
				inServices = false;
			}
			const service = line.match(/^ {2}([a-z][a-z0-9_-]*):\s*$/u);
			if (inServices && service != null) {
				composeServices.add(service[1]);
			}
		}
	}
	const operatorPages = ['operator/get-started.mdx', 'operator/configuration.mdx', 'operator/upgrading.mdx'];
	const namedServices = new Set<string>();
	for (const page of operatorPages) {
		const text = await readFile(path.join(DOCS_ROOT, page), 'utf8');
		for (const backticked of text.matchAll(/`([a-z][a-z0-9-]{2,})`/gu)) {
			namedServices.add(backticked[1]);
		}
	}
	const knownNonServices = new Set(['docker-compose.yml', 'chat.example.com', 'admin.example.com', 'localhost']);
	for (const candidate of namedServices) {
		if (!candidate.endsWith('-shard') && !candidate.startsWith('fluxer-') && !composeServices.has(candidate)) {
			continue;
		}
		if (knownNonServices.has(candidate)) {
			continue;
		}
		const isShard = candidate.endsWith('-shard');
		const isImage = candidate.startsWith('fluxer-');
		if (!isShard && !isImage) {
			continue;
		}
		if (isShard && !composeServices.has(candidate)) {
			problems.push(`the operator docs name the service \`${candidate}\`, which is not in docker-compose.yml`);
		}
		if (isImage && !compose.includes(candidate)) {
			problems.push(`the operator docs name the image \`${candidate}\`, which docker-compose.yml never references`);
		}
	}
	console.log(`  compose services: ${composeServices.size.toString()}`);

	const files = ['docker-compose.yml', 'Caddyfile', '.env.example'];
	for (const file of files) {
		if (!guide.includes(file)) {
			problems.push(`the guide never mentions ${file}, which the stack ships`);
		}
	}

	console.log(`  .env.example placeholders: ${placeholders.size.toString()}`);
	console.log(`  set by the guide: ${[...placeholders].filter((n) => covered.has(n)).length.toString()}`);
	console.log(`  installer secret keys (sh): ${shellSecrets.size.toString()}`);
	console.log(`  installer secret keys (ps1): ${powershellSecrets.size.toString()}`);
	console.log(`  installer non-secret keys: ${shellNonSecrets.size.toString()}`);
	console.log(`  compose required variables covered: ${composeRequiredCovered.toString()}`);
	console.log(`  services publishing ports: ${[...publishers].sort().join(', ')}`);
	console.log(`  published ports: ${[...publishedPorts].sort().join(', ')}`);
	failures += section('self-hosting guide disagreements', problems);
}

console.log('unthrottled routes and global bucket claims');
{
	const unthrottled = controllerRoutes
		.filter((route) => route.rateLimitConfig == null)
		.map((route) => ({
			method: route.method.toUpperCase(),
			route: route.path,
			admin: route.path.startsWith('/admin'),
		}));

	const publicShapes = new Set(main.map((operation) => shapeOf(operation.method, operation.path)));
	const publicUnthrottled = unthrottled
		.filter((entry) => !entry.admin)
		.map((entry) => shapeOf(entry.method, stripVersionPrefix(entry.route.replace(/:([a-zA-Z_]+)/gu, '{$1}'))))
		.filter((shape) => publicShapes.has(shape));
	const adminUnthrottled = unthrottled.filter((entry) => entry.admin);

	const page = await readFile(path.join(DOCS_ROOT, 'topics/rate-limits.md'), 'utf8');
	const problems: Array<string> = [];
	const uniquePublic = [...new Set(publicUnthrottled)].sort();

	const unthrottledByDesign = new Set([
		'GET /dl/desktop/{}/{}/{}/latest',
		'GET /dl/desktop/{}/{}/{}/latest/{}',
		'GET /dl/desktop/{}/{}/{}/versions',
		'GET /dl/desktop/{}/{}/{}/{}/{}',
	]);
	const unexpected = uniquePublic.filter((shape) => !unthrottledByDesign.has(shape));
	const nowThrottled = [...unthrottledByDesign].filter((shape) => !uniquePublic.includes(shape)).sort();

	if (unexpected.length > 0) {
		problems.push(
			`rate-limits.md says every HTTP API operation outside the desktop downloads declares a bucket, but ${unexpected.length.toString()} more declare none: ${unexpected.join(', ')}`,
		);
	}
	if (nowThrottled.length > 0) {
		problems.push(
			`rate-limits.md names the desktop downloads as the only operations with no bucket, but ${nowThrottled.length.toString()} now declare one: ${nowThrottled.join(', ')}`,
		);
	}
	if (!page.includes('[desktop download](/http-api/downloads/)')) {
		problems.push('rate-limits.md no longer names the desktop downloads as the operations with no bucket');
	}
	if (adminUnthrottled.length > 0) {
		const named = adminUnthrottled.map((entry) => `${entry.method} ${entry.route}`).sort();
		problems.push(
			`rate-limits.md says every Admin API operation declares a bucket, but ${named.length.toString()} declare none: ${named.join(', ')}`,
		);
	}
	for (const stale of ['declares no bucket', 'Two [Admin API]']) {
		if (page.includes(stale)) {
			problems.push(`rate-limits.md still says \`${stale}\`, and every operation now declares one`);
		}
	}

	const middleware = await readFile(
		path.join(REPO_ROOT, 'fluxer_api/src/api/middleware/RateLimitMiddleware.ts'),
		'utf8',
	);
	const high = middleware.match(/HIGH_GLOBAL_RATE_LIMIT\) !== 0n\) \{\s*return (\d+);/u);
	if (high != null && !page.includes(`${Number.parseInt(high[1], 10).toLocaleString('en-US')} requests per second`)) {
		problems.push(`rate-limits.md does not state the HIGH_GLOBAL_RATE_LIMIT allowance of ${high[1]}`);
	}

	console.log(`  public operations with no rate limit middleware: ${uniquePublic.length.toString()}`);
	console.log(`  admin operations with no rate limit middleware: ${adminUnthrottled.length.toString()}`);
	failures += section('rate limit prose disagreements', problems);
}

console.log('error registry and abuse signal weights');
{
	const errorsPage = await readFile(path.join(DOCS_ROOT, 'http-api/errors.md'), 'utf8');
	const documentedCodes = new Set<string>();
	let documentedEntries = 0;
	for (const row of errorsPage.matchAll(/^\|\s*`?([A-Z][A-Z0-9_]{2,})`?\s*\|/gmu)) {
		documentedCodes.add(row[1]);
		documentedEntries += 1;
	}
	for (const entry of errorsPage.matchAll(/^#{3,4} `([A-Z][A-Z0-9_]{2,})`\s*$/gmu)) {
		documentedCodes.add(entry[1]);
		documentedEntries += 1;
	}
	const registrySource = await readFile(path.join(REPO_ROOT, 'packages/constants/src/ApiErrorCodes.ts'), 'utf8');
	const registryCodes = new Set<string>();
	for (const entry of registrySource.matchAll(/^\t([A-Z][A-Z0-9_]*):/gmu)) {
		registryCodes.add(entry[1]);
	}
	const problems: Array<string> = [];
	for (const code of registryCodes) {
		if (!documentedCodes.has(code)) {
			problems.push(`APIErrorCodes.${code} is not listed in the errors page registry`);
		}
	}

	const banner = await readFile(path.join(REPO_ROOT, 'fluxer_api/src/api/middleware/AbusiveIpAutoBanner.ts'), 'utf8');
	const weights = new Map<string, string>();
	for (const rule of banner.matchAll(/if \(status === (\d{3})\) return ([\d.]+);/gu)) {
		weights.set(rule[1], rule[2]);
	}
	for (const [status, weight] of weights) {
		if (status === '404') {
			continue;
		}
		if (!errorsPage.includes(`A ${status} weighs ${weight}`) && !errorsPage.includes(`a ${status} weighs ${weight}`)) {
			problems.push(`errors.md does not state that a ${status} weighs ${weight}`);
		}
	}

	const documentedRegistryCodes = [...registryCodes].filter((c) => documentedCodes.has(c)).length;
	if (documentedEntries < 491) {
		problems.push(
			`errors.md code entries parsed fell to ${documentedEntries.toString()}, floor is 491, the 255 API codes plus the 236 validation codes. The registry parser reads a \`| CODE |\` table row and a \`### \`CODE\`\` or \`#### \`CODE\`\` heading, and one of those shapes has stopped matching`,
		);
	}
	if (documentedRegistryCodes < registryCodes.size) {
		problems.push(
			`errors.md documents ${documentedRegistryCodes.toString()} of the ${registryCodes.size.toString()} registry codes, floor is every one of them`,
		);
	}
	console.log(`  registry codes: ${registryCodes.size.toString()}, documented: ${documentedRegistryCodes.toString()}`);
	console.log(`  abuse signal weights compared: ${weights.size.toString()}`);
	failures += section('error registry disagreements', problems);
}

console.log('gateway numeric limits');
{
	const erlConstants = await readFile(path.join(REPO_ROOT, 'fluxer_gateway/src/utils/constants.erl'), 'utf8');
	const sharding = await readFile(path.join(REPO_ROOT, 'fluxer_gateway/src/gateway/gateway_sharding.erl'), 'utf8');
	const abuse = await readFile(path.join(REPO_ROOT, 'fluxer_gateway/src/gateway/session_abuse_protection.erl'), 'utf8');
	const heartbeat = await readFile(
		path.join(REPO_ROOT, 'fluxer_gateway/src/gateway/gateway_handler_heartbeat.erl'),
		'utf8',
	);
	const limitsPage = await readFile(path.join(DOCS_ROOT, 'gateway/limits-and-rate-limits.md'), 'utf8');
	const overviewPage = await readFile(path.join(DOCS_ROOT, 'gateway/overview.md'), 'utf8');
	const corpus = `${limitsPage}\n${overviewPage}`;

	const readFn = (source: string, name: string): number | null => {
		const found = source.match(new RegExp(`${name}\\(\\) -> (\\d+)`, 'u'));
		return found == null ? null : Number.parseInt(found[1], 10);
	};
	const readDefine = (source: string, name: string): number | null => {
		const found = source.match(new RegExp(`-define\\(${name}, (\\d+)\\)`, 'u'));
		return found == null ? null : Number.parseInt(found[1], 10);
	};
	const grouped = (value: number): string => value.toLocaleString('en-US');

	const problems: Array<string> = [];
	const interval = readFn(erlConstants, 'heartbeat_interval');

	const expectations: Array<[string, number | null, (value: string) => string]> = [
		['heartbeat interval', interval, (v) => `heartbeat interval of ${v} ms`],
		['resume timeout', readFn(erlConstants, 'resume_timeout'), (v) => `resumable for ${v} ms`],
		['max payload size', readFn(erlConstants, 'max_payload_size'), (v) => v],
		['max shard count', readDefine(sharding, 'MAX_SHARD_COUNT'), (v) => `through ${v}`],
		['max guilds per shard', readDefine(sharding, 'MAX_GUILDS_PER_SHARD'), (v) => `at most ${v} guilds`],
		['max sessions per user', readDefine(abuse, 'MAX_SESSIONS_PER_USER'), (v) => `at most ${v} live sessions`],
	];
	if (interval != null && heartbeat.includes('div 3')) {
		expectations.push(['heartbeat check cadence, interval div 3', Math.floor(interval / 3), (v) => `every ${v} ms`]);
	}
	if (interval != null && heartbeat.includes('* 10) >= (Interval * 9)')) {
		expectations.push([
			'heartbeat ping threshold, interval times 9 over 10',
			(interval * 9) / 10,
			(v) => `When ${v} ms have elapsed`,
		]);
	}
	let compared = 0;
	for (const [label, value, phrase] of expectations) {
		if (value == null) {
			problems.push(`could not read ${label} from the Gateway source`);
			continue;
		}
		compared += 1;
		if (!corpus.includes(phrase(grouped(value))) && !corpus.includes(phrase(String(value)))) {
			problems.push(`the Gateway docs never state the ${label} of ${grouped(value)} in the expected phrasing`);
		}
	}
	console.log(`  gateway constants compared: ${compared.toString()}`);
	failures += section('gateway limit disagreements', problems);
}

console.log('snowflake layout');
{
	const core = await readFile(path.join(REPO_ROOT, 'packages/constants/src/Core.ts'), 'utf8');
	const snowflakeSource = await readFile(path.join(REPO_ROOT, 'packages/snowflake/src/Snowflake.ts'), 'utf8');
	const page = await readFile(path.join(DOCS_ROOT, 'snowflakes.md'), 'utf8');
	const problems: Array<string> = [];

	const epochMatch = core.match(/export const FLUXER_EPOCH = (\d+);/u);
	const workerBits = snowflakeSource.match(/WORKER_ID_BITS = (\d+)n/u);
	const sequenceBits = snowflakeSource.match(/SEQUENCE_BITS = (\d+)n/u);

	if (epochMatch == null || workerBits == null || sequenceBits == null) {
		problems.push('could not read FLUXER_EPOCH, WORKER_ID_BITS or SEQUENCE_BITS from source');
	} else {
		const epoch = Number.parseInt(epochMatch[1], 10);
		const worker = Number.parseInt(workerBits[1], 10);
		const sequence = Number.parseInt(sequenceBits[1], 10);
		const shift = worker + sequence;
		if (!page.includes(String(epoch))) {
			problems.push(`snowflakes.md does not state the epoch ${epoch.toString()}`);
		}
		const epochIso = new Date(epoch).toISOString().replace('.000Z', '.000Z');
		if (!page.includes(epochIso.slice(0, 10))) {
			problems.push(`snowflakes.md does not state the epoch date ${epochIso}`);
		}
		if (!page.includes(`right by ${shift.toString()} bits`)) {
			problems.push(`snowflakes.md does not state the timestamp shift of ${shift.toString()} bits`);
		}
		if (!page.includes(`through \`${((1 << worker) - 1).toString()}\``)) {
			problems.push(`snowflakes.md does not state the maximum worker id of ${((1 << worker) - 1).toString()}`);
		}
		const timestampBits = 63 - shift;
		const lastMs = epoch + 2 ** timestampBits - 1;
		const lastIso = new Date(lastMs).toISOString().replace('Z', 'Z');
		if (!page.includes(lastIso.slice(0, 19))) {
			problems.push(`snowflakes.md does not state the last representable instant ${lastIso}`);
		}
		console.log(
			`  epoch ${epoch.toString()} (${epochIso.slice(0, 10)}), shift ${shift.toString()}, worker ${worker.toString()} bits, sequence ${sequence.toString()} bits`,
		);
		console.log(`  last representable instant: ${lastIso}`);
	}
	failures += section('snowflake layout disagreements', problems);
}

console.log('attachment upload geometry');
{
	const limits = await readFile(path.join(REPO_ROOT, 'packages/constants/src/LimitConstants.ts'), 'utf8');
	const page = await readFile(path.join(DOCS_ROOT, 'topics/uploads.md'), 'utf8');
	const problems: Array<string> = [];
	const readConst = (name: string): number | null => {
		const found = limits.match(new RegExp(`${name} = ([^;]+);`, 'u'));
		if (found == null) {
			return null;
		}
		const expression = found[1].replace(/_/gu, '').trim();
		if (!/^[\d*+\s]+$/u.test(expression)) {
			return null;
		}
		return expression
			.split('+')
			.map((term) => term.split('*').reduce((a, b) => a * Number.parseInt(b.trim(), 10), 1))
			.reduce((a, b) => a + b, 0);
	};

	const constants: Array<[string, string]> = [
		['ATTACHMENT_UPLOAD_CHUNK_THRESHOLD', 'singlepart threshold'],
		['ATTACHMENT_UPLOAD_MIN_CHUNK_SIZE', 'minimum part size'],
		['ATTACHMENT_UPLOAD_MAX_CHUNKS', 'maximum part count'],
		['ATTACHMENT_MAX_SIZE_NON_PREMIUM', 'non-premium attachment ceiling'],
		['ATTACHMENT_MAX_SIZE_PREMIUM', 'premium attachment ceiling'],
		['ATTACHMENT_MAX_SIZE_BOT', 'bot attachment ceiling'],
	];
	const divisor = readConst('ATTACHMENT_UPLOAD_TARGET_PART_COUNT');
	if (divisor == null) {
		problems.push('could not read ATTACHMENT_UPLOAD_TARGET_PART_COUNT from LimitConstants.ts');
	} else if (!page.includes(`divided by ${divisor.toString()}`)) {
		problems.push(`uploads.md does not say the part size is the declared size divided by ${divisor.toString()}`);
	}
	let compared = 0;
	for (const [name, label] of constants) {
		const value = readConst(name);
		if (value == null) {
			problems.push(`could not read ${name} from LimitConstants.ts`);
			continue;
		}
		compared += 1;
		const bare = new RegExp(`(?<![\\d,.])${value.toString()}(?![\\d,.])`, 'u');
		const grouped = new RegExp(`(?<![\\d,.])${value.toLocaleString('en-US').replace(/,/gu, ',')}(?![\\d,.])`, 'u');
		if (!bare.test(page) && !grouped.test(page)) {
			problems.push(`uploads.md does not state the ${label} of ${value.toString()}`);
		}
	}
	console.log(`  upload constants compared: ${compared.toString()}`);
	failures += section('upload geometry disagreements', problems);
}

console.log('captcha gated operations');
{
	const page = await readFile(path.join(DOCS_ROOT, 'topics/captcha.md'), 'utf8');
	const documented = new Set<string>();
	const documentedLabels = new Map<string, string>();
	for (const row of page.matchAll(/^\|\s*(GET|POST|PUT|PATCH|DELETE)\s*\|\s*(\/v1\/\S+?)\s*\|/gmu)) {
		const normalised = shapeOf(row[1], stripVersionPrefix(row[2]));
		documented.add(normalised);
		documentedLabels.set(normalised, `${row[1]} ${row[2]}`);
	}

	const gated = new Set<string>();
	for (const route of controllerRoutes) {
		if (route.middlewares.some((name) => /Captcha|GroupDm\w*Protection/u.test(name))) {
			gated.add(astRoute(route));
		}
	}

	const problems: Array<string> = [];
	for (const route of gated) {
		if (!documented.has(route)) {
			problems.push(`${route} is captcha gated in source but not listed on the captcha page`);
		}
	}
	for (const route of documented) {
		if (!gated.has(route)) {
			problems.push(
				`the captcha page lists ${documentedLabels.get(route) ?? route}, which is not captcha gated in source`,
			);
		}
	}
	console.log(`  captcha gated routes in source: ${gated.size.toString()}, documented: ${documented.size.toString()}`);
	failures += section('captcha gating disagreements', problems);
}

console.log('bot capability flag (from the middleware chain)');
{
	const BOT_EXEMPT = new Map([
		['GET /applications/@me', {docs: true, why: 'reads the Authorization header directly in the handler'}],
		['GET /gateway/bot', {docs: true, why: 'requires a bot-shaped token, parsed in the handler'}],
		['POST /guilds', {docs: false, why: 'the service rejects bots with BOTS_CANNOT_CREATE_GUILDS'}],
		['POST /guilds/{}/delete', {docs: false, why: 'the caller must own the guild and a bot never can'}],
	]);
	const problems: Array<string> = [];
	let compared = 0;
	for (const route of controllerRoutes) {
		const key = astRoute(route);
		if (key.startsWith('GET /admin') || key.includes(' /admin/')) {
			continue;
		}
		const documented = documentedFlags.get(key);
		if (documented == null) {
			continue;
		}
		const anyLogin = route.hasLoginRequired || route.hasLoginRequiredAllowSuspicious;
		const sourceAcceptsBot = anyLogin && !route.hasDefaultUserOnly;
		const exemption = BOT_EXEMPT.get(key);
		if (exemption != null) {
			if (documented.bot !== exemption.docs) {
				problems.push(
					`${key} is an exempted route expected to document bot=${String(exemption.docs)} (${exemption.why}), but the page says ${String(documented.bot)}`,
				);
			}
			continue;
		}
		compared += 1;
		if (documented.bot !== sourceAcceptsBot) {
			problems.push(
				`${key} documents bot=${String(documented.bot)} but the middleware chain gives ${String(sourceAcceptsBot)}`,
			);
		}
	}
	console.log(`  routes compared against the middleware chain: ${compared.toString()}`);
	console.log(`  exempted, each verified by hand: ${BOT_EXEMPT.size.toString()}`);
	failures += section('bot flag disagreements', problems);
}

console.log('unauthenticated capability flag (from the middleware chain)');
{
	const WEBHOOK_TOKEN_CREDENTIAL = 'the webhook ID and token in the path are the complete credential';
	const OAUTH2_CLIENT_CREDENTIAL =
		'the client authenticates with HTTP Basic or the client_id and client_secret form fields';
	const UNAUTHENTICATED_EXEMPT = new Map([
		['GET /applications/@me', {docs: false, why: 'reads the Authorization header directly in the handler'}],
		['GET /gateway/bot', {docs: false, why: 'requires a bot-shaped token, parsed in the handler'}],
		[
			'POST /auth/handoff/complete',
			{docs: false, why: 'the approving session token comes from the Authorization header or the body token field'},
		],
		['POST /oauth2/token', {docs: false, why: OAUTH2_CLIENT_CREDENTIAL}],
		['POST /oauth2/introspect', {docs: false, why: OAUTH2_CLIENT_CREDENTIAL}],
		['POST /oauth2/token/revoke', {docs: false, why: OAUTH2_CLIENT_CREDENTIAL}],
		['GET /webhooks/{}/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['PATCH /webhooks/{}/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['DELETE /webhooks/{}/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['POST /webhooks/{}/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['GET /webhooks/{}/{}/messages/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['PATCH /webhooks/{}/{}/messages/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['DELETE /webhooks/{}/{}/messages/{}', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['POST /webhooks/{}/{}/github', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['POST /webhooks/{}/{}/slack', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
		['POST /webhooks/{}/{}/instatus', {docs: false, why: WEBHOOK_TOKEN_CREDENTIAL}],
	]);
	const problems: Array<string> = [];
	let compared = 0;
	for (const route of controllerRoutes) {
		const key = astRoute(route);
		if (key.startsWith('GET /admin') || key.includes(' /admin/')) {
			continue;
		}
		const documented = documentedFlags.get(key);
		if (documented == null) {
			continue;
		}
		const anyLogin = route.hasLoginRequired || route.hasLoginRequiredAllowSuspicious;
		const sourceIsOpen = !anyLogin && !route.middlewares.some((name) => /OAuth2Scope/u.test(name));
		const exemption = UNAUTHENTICATED_EXEMPT.get(key);
		if (exemption != null) {
			if (documented.unauthenticated !== exemption.docs) {
				problems.push(
					`${key} is an exempted route expected to document unauthenticated=${String(exemption.docs)} (${exemption.why}), but the page says ${String(documented.unauthenticated)}`,
				);
			}
			continue;
		}
		compared += 1;
		if (documented.unauthenticated !== sourceIsOpen) {
			problems.push(
				`${key} documents unauthenticated=${String(documented.unauthenticated)} but the middleware chain gives ${String(sourceIsOpen)}`,
			);
		}
	}
	console.log(`  routes compared against the middleware chain: ${compared.toString()}`);
	console.log(`  exempted, each verified by hand: ${UNAUTHENTICATED_EXEMPT.size.toString()}`);
	failures += section('unauthenticated flag disagreements', problems);
}

console.log('spec security field against the middleware chain');
{
	const MANUAL_CREDENTIAL = new Set(['GET /applications/@me']);
	const BOT_SCHEME_EXEMPT = new Map<string, string>([
		[
			'POST /guilds',
			'LoginRequired admits a bot token, but GuildOperationsService rejects every bot with 400 BOTS_CANNOT_CREATE_GUILDS, so the spec must not advertise botToken',
		],
	]);
	const specSecurity = new Map<string, Set<string>>();
	for (const operation of main) {
		const schemes = new Set<string>();
		for (const entry of operation.security ?? []) {
			for (const scheme of Object.keys(entry)) {
				schemes.add(scheme);
			}
		}
		specSecurity.set(shapeOf(operation.method, operation.path), schemes);
	}
	const specBugs: Array<string> = [];
	let compared = 0;
	for (const route of controllerRoutes) {
		const key = astRoute(route);
		const declaredSchemes = specSecurity.get(key);
		if (declaredSchemes == null || MANUAL_CREDENTIAL.has(key)) {
			continue;
		}
		compared += 1;
		const anyLogin = route.hasLoginRequired || route.hasLoginRequiredAllowSuspicious;
		const acceptsBot = anyLogin && !route.hasDefaultUserOnly;
		const exemption = BOT_SCHEME_EXEMPT.get(key);
		if (exemption != null) {
			if (declaredSchemes.has('botToken')) {
				specBugs.push(`${key} declares botToken, but is exempted because ${exemption}`);
			}
			continue;
		}
		if (declaredSchemes.has('botToken') && !acceptsBot) {
			specBugs.push(`${key} declares botToken, but DefaultUserOnly rejects bots`);
		}
		if (!declaredSchemes.has('botToken') && acceptsBot && declaredSchemes.size > 0) {
			specBugs.push(`${key} omits botToken, but the middleware chain admits a bot token`);
		}
		if (declaredSchemes.size > 0 && !anyLogin && !route.middlewares.some((name) => /Admin|OAuth2Scope/iu.test(name))) {
			specBugs.push(`${key} declares [${[...declaredSchemes].join(', ')}], but the chain has no login policy`);
		}
	}
	console.log(`  routes compared: ${compared.toString()}`);
	console.log('  GET /applications/@me is excluded because it reads the Authorization header in the');
	console.log('  handler, so it has no middleware to compare against.');
	console.log(`  exempted, each verified by hand: ${BOT_SCHEME_EXEMPT.size.toString()}`);
	for (const [key, why] of BOT_SCHEME_EXEMPT) {
		console.log(`    ${key}: ${why}`);
	}
	failures += section('spec security disagreements with the middleware chain', specBugs);
}

console.log('admin API (against the generated spec)');
const adminTargetOnly = [...documentedAdmin.entries()]
	.filter(([shape]) => !adminShapes.has(shape))
	.map(([, route]) => `${route.method} ${route.path}  (${route.file})`)
	.sort();
const adminLiveOnly = admin
	.filter((operation) => !documentedAdmin.has(shapeOf(operation.method, operation.path)))
	.map((operation) => `${operation.method} ${operation.path}`)
	.sort();
console.log(`  live admin operations: ${admin.length.toString()}`);
console.log(`  documented admin operations: ${adminDocumented.length.toString()}`);
console.log(
	`  documented in the RESTful target shape, awaiting the live admin refactor: ${adminTargetOnly.length.toString()}`,
);
console.log(`  live admin operations with no documented RESTful equivalent: ${adminLiveOnly.length.toString()}`);
failures += section('target-shape routes awaiting the live admin refactor', adminTargetOnly);
failures += section('live admin routes with no documented equivalent', adminLiveOnly);

console.log('');
console.log(
	`documented routes: ${documented.length.toString()} (main ${mainDocumented.length.toString()}, admin ${adminDocumented.length.toString()}, media proxy ${mediaProxyDocumented.length.toString()})`,
);
if (failures > 0) {
	console.error(`FAIL: ${failures.toString()} coverage problems`);
	process.exit(1);
}
console.log('OK: every registered fluxer_api route is documented or covered by an exemption rule, and the');
console.log('documented routes match the live main API, media proxy, and admin target shape');
