import {execFileSync, spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {existsSync, readFileSync} from 'node:fs';
import {createServer} from 'node:net';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {parseArgs} from 'node:util';
import {chromium} from 'playwright';

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const ALLOWLIST_PATH = path.join(ROOT, 'tools/prose-shape/allowlist.json');
const WIDTHS = [390, 768, 1280, 1440, 1920];
const LIMITS = {minLastLine: 0.4, minColumn: 48};
const NOT_FOUND_PATH = '/prose-shape-check-missing-page';
const REFERENCE_SECTIONS = ['/http-api/', '/admin-api/', '/gateway/', '/media-proxy/'];

const HELP = `Usage: pnpm prose:check [options]

Checks the line shape of every block of text in the main column of every
docs page in headless Chromium at ${WIDTHS.join(', ')} px wide, in the light
theme, after web fonts load and with every details element open. It measures
the page once for each tab of every tab set and each choice of every radio
group, so the text of every tab panel and setup picker answer is measured.

Rule, for an element that wraps, where fraction = width of the last line /
content width of the element: it fails when its last line is below
${LIMITS.minLastLine}, such as "keeps it." or "it." alone.

Measured: p, li, dd, dt and figcaption inside main, and every other element
inside main that is not inline and holds text of its own, which covers
prose, lists, asides and their titles, steps, tab panels and tab labels,
cards and their titles, the setup picker and table captions. A list item that
holds block children is measured through those children, and the text of a
list item before a nested list is measured on its own. In a flex or grid
element only its own text is measured, against the width its other children
leave free.
Not measured: the page title, headings, breadcrumbs, the sidebar, the
right-hand contents, pagination, code and pre blocks, inline code kept
whole, buttons and links styled as buttons, form controls, svg, hidden
elements, table cells and everything inside them, the entries of the
settings reference, list items whose whole text is one link, text that is
all code, text with forced line breaks, text that never wraps, route
headers, and columns narrower than ${LIMITS.minColumn} px.

Pages: every URL in sitemap-index.xml and the 404 page, except the API
reference sections ${REFERENCE_SECTIONS.join(', ')}, whose route
descriptions, field notes and footnotes are reference entries like tables.

Exemptions live in tools/prose-shape/allowlist.json, keyed by page path and
the first 12 hex digits of the sha256 of the element's whitespace-collapsed
text, each with a reason. An entry that no longer matches a failure fails
the check, so the list stays short and current.

Without --base-url it runs astro build (skip with --no-build), starts the
built server on a free port and stops it by PID.

Options:
  --base-url <url>  check a running server instead of building one
  --no-build        reuse dist/ from an earlier build
  --page <path>     only check this path (repeatable)
  --width <px>      only check this width (repeatable)
  --all             also print elements that pass
  --json            print results as JSON
  -h, --help        show this help
`;

const {values: options} = parseArgs({
	allowPositionals: true,
	options: {
		'base-url': {type: 'string'},
		'no-build': {type: 'boolean', default: false},
		page: {type: 'string', multiple: true},
		width: {type: 'string', multiple: true},
		all: {type: 'boolean', default: false},
		json: {type: 'boolean', default: false},
		help: {type: 'boolean', short: 'h', default: false},
	},
});

if (options.help) {
	process.stdout.write(HELP);
	process.exit(0);
}

async function discoverPages(baseUrl) {
	const index = await (await fetch(`${baseUrl}/sitemap-index.xml`)).text();
	const sitemaps = [...index.matchAll(/<loc>([^<]+)<\/loc>/g)].map((match) => new URL(match[1]).pathname);
	const pages = new Set();
	for (const sitemap of sitemaps) {
		const body = await (await fetch(`${baseUrl}${sitemap}`)).text();
		for (const match of body.matchAll(/<loc>([^<]+)<\/loc>/g)) pages.add(new URL(match[1]).pathname);
	}
	if (pages.size === 0) throw new Error('no pages found in sitemap-index.xml');
	return [...pages].sort().concat(NOT_FOUND_PATH);
}

function isReference(pagePath) {
	return REFERENCE_SECTIONS.some((section) => pagePath.startsWith(section));
}

function measurePage(limits) {
	const SKIP = [
		'header',
		'nav',
		'footer',
		'pre',
		'code',
		'.flx-glue',
		'td',
		'th',
		'.flx-settings',
		'flx-route-header',
		'button',
		'input',
		'select',
		'textarea',
		'svg',
		'.flx-button',
		'.expressive-code',
		'.flx-crumbs',
		'.sl-anchor-link',
		'.pagination-links',
		'starlight-toc',
		'mobile-starlight-toc',
		'[hidden]',
		'.sr-only',
		'[role="dialog"]',
	].join(',');
	const lineBoxes = (ranges) => {
		const rects = ranges
			.flatMap((range) => [...range.getClientRects()])
			.filter((rect) => rect.width > 0 && rect.height > 0);
		const lines = [];
		for (const rect of rects.sort((a, b) => a.top - b.top)) {
			const hit = lines.find((line) => rect.top < line.b - 2 && rect.bottom > line.t + 2);
			if (hit) {
				hit.l = Math.min(hit.l, rect.left);
				hit.r = Math.max(hit.r, rect.right);
			} else {
				lines.push({l: rect.left, r: rect.right, t: rect.top, b: rect.bottom});
			}
		}
		return lines;
	};
	const wordsOnLine = (range, line) => {
		const walker = document.createTreeWalker(range.commonAncestorContainer, NodeFilter.SHOW_TEXT);
		const nodes = [];
		for (let node = walker.nextNode(); node; node = walker.nextNode()) if (range.intersectsNode(node)) nodes.push(node);
		const probe = document.createRange();
		let text = '';
		for (const node of nodes.reverse()) {
			const start = node === range.startContainer ? range.startOffset : 0;
			const end = node === range.endContainer ? range.endOffset : node.length;
			for (let offset = end - 1; offset >= start; offset -= 1) {
				probe.setStart(node, offset);
				probe.setEnd(node, offset + 1);
				const rect = probe.getBoundingClientRect();
				if (rect.height === 0) {
					text = node.data[offset] + text;
					continue;
				}
				const middle = (rect.top + rect.bottom) / 2;
				if (middle < line.t) return text.trim().split(/\s+/).filter(Boolean);
				text = node.data[offset] + text;
			}
		}
		return text.trim().split(/\s+/).filter(Boolean);
	};
	const describe = (element) => {
		const parts = [];
		for (let node = element; node && node !== document.body; node = node.parentElement) {
			let part = node.tagName.toLowerCase();
			if (node.id) {
				parts.unshift(`${part}#${node.id}`);
				break;
			}
			const siblings = node.parentElement
				? [...node.parentElement.children].filter((child) => child.tagName === node.tagName)
				: [];
			if (siblings.length > 1) part += `:nth-of-type(${siblings.indexOf(node) + 1})`;
			parts.unshift(part);
		}
		return parts.join(' > ');
	};
	const nearestHeading = (element) => {
		const headings = [...document.querySelectorAll('main h1, main h2, main h3, main h4')];
		let found = '';
		for (const heading of headings) {
			if (heading.compareDocumentPosition(element) & Node.DOCUMENT_POSITION_FOLLOWING)
				found = heading.textContent.trim();
		}
		return found;
	};
	const contentWidth = (element) => {
		const style = getComputedStyle(element);
		return (
			element.getBoundingClientRect().width -
			parseFloat(style.paddingLeft) -
			parseFloat(style.paddingRight) -
			parseFloat(style.borderLeftWidth) -
			parseFloat(style.borderRightWidth)
		);
	};
	const visible = (element) => {
		if (element.closest(SKIP)) return false;
		const style = getComputedStyle(element);
		if (style.visibility !== 'visible' || style.display === 'none' || Number(style.opacity) === 0) return false;
		if (style.whiteSpace === 'nowrap' || style.whiteSpace === 'pre') return false;
		return element.getClientRects().length > 0;
	};
	const ownText = (element) =>
		[...element.childNodes].filter((node) => node.nodeType === Node.TEXT_NODE && node.data.trim());
	const flexTextWidth = (element) => {
		const style = getComputedStyle(element);
		if (!style.display.endsWith('flex') || style.flexDirection.startsWith('column')) return contentWidth(element);
		const items = [...element.children].filter((child) => {
			const childStyle = getComputedStyle(child);
			return childStyle.display !== 'none' && childStyle.position !== 'absolute' && childStyle.position !== 'fixed';
		});
		const others = items.reduce((sum, child) => {
			const childStyle = getComputedStyle(child);
			return (
				sum +
				child.getBoundingClientRect().width +
				parseFloat(childStyle.marginLeft) +
				parseFloat(childStyle.marginRight)
			);
		}, 0);
		return contentWidth(element) - others - (parseFloat(style.columnGap) || 0) * items.length;
	};
	const elements = [...document.querySelectorAll('main *')].filter((element) => {
		if (element.matches('p, li, dd, dt, figcaption')) return true;
		if (ownText(element).length === 0) return false;
		const display = getComputedStyle(element).display;
		return display !== 'inline' && display !== 'contents' && display !== 'none';
	});
	const results = [];
	const measured = [];
	for (const element of elements) {
		if (!visible(element)) continue;
		if (element.querySelector('br')) continue;
		const display = getComputedStyle(element).display;
		if (display.endsWith('flex') || display.endsWith('grid')) {
			const nodes = ownText(element);
			if (nodes.length === 0) continue;
			const parts = nodes.map((node) => {
				const part = document.createRange();
				part.selectNodeContents(node);
				return part;
			});
			const range = document.createRange();
			range.setStartBefore(nodes[0]);
			range.setEndAfter(nodes[nodes.length - 1]);
			const text = nodes
				.map((node) => node.data)
				.join(' ')
				.replace(/\s+/g, ' ')
				.trim();
			const lines = lineBoxes(parts);
			if (lines.length > 0) measured.push({element, range, text, lines, width: flexTextWidth(element)});
			continue;
		}
		const firstBlock = [...element.children].find((child) => {
			const display = getComputedStyle(child).display;
			return !display.startsWith('inline') && display !== 'none' && display !== 'contents';
		});
		const range = document.createRange();
		range.selectNodeContents(element);
		if (firstBlock) range.setEndBefore(firstBlock);
		const text = range.toString().replace(/\s+/g, ' ').trim();
		if (!text) continue;
		if (element.tagName === 'LI' && !firstBlock) {
			const links = element.querySelectorAll('a');
			if (links.length === 1 && links[0].textContent.replace(/\s+/g, ' ').trim() === text) continue;
		}
		const lines = lineBoxes([range]);
		if (lines.length === 0) continue;
		const tokens = [...element.querySelectorAll('code, kbd')]
			.map((node) => node.textContent)
			.join('')
			.replace(/\s+/g, '');
		if (tokens.length > 0 && tokens === text.replace(/\s+/g, '')) continue;
		measured.push({element, range, text, lines, width: contentWidth(element)});
	}
	for (const {element, range, text, lines, width} of measured) {
		if (width < limits.minColumn || width > window.innerWidth) continue;
		const last = lines[lines.length - 1];
		const fraction = Math.round(((last.r - last.l) / width) * 1000) / 1000;
		const rules = [];
		let lastLine = null;
		if (lines.length > 1) {
			const words = wordsOnLine(range, last);
			lastLine = words.join(' ');
			if (fraction < limits.minLastLine) rules.push(`last line below ${limits.minLastLine}`);
		}
		results.push({
			selector: describe(element),
			section: nearestHeading(element),
			text,
			lastLine,
			lines: lines.length,
			fraction,
			column: Math.round(width),
			rules,
		});
	}
	return results;
}

async function preparePage(page, url) {
	const response = await page.goto(url, {waitUntil: 'load'});
	await page.evaluate(async () => {
		await document.fonts.ready;
		for (const details of document.querySelectorAll('main details')) details.open = true;
		await document.fonts.ready;
		await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)));
	});
	return response;
}

async function showChoice(index) {
	const sets = [...document.querySelectorAll('main [role="tablist"]')].map((list) => [
		...list.querySelectorAll('[role="tab"]'),
	]);
	const groups = new Map();
	for (const radio of document.querySelectorAll('main input[type="radio"]'))
		groups.set(radio.name, [...(groups.get(radio.name) ?? []), radio]);
	let shown = 0;
	let most = 1;
	for (const set of sets.concat([...groups.values()])) {
		most = Math.max(most, set.length);
		if (index >= set.length) continue;
		set[index].click();
		shown += 1;
	}
	await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)));
	return {shown, most};
}

async function measureChoices(page) {
	const byElement = new Map();
	for (let index = 0, count = 1; index < count; index += 1) {
		const {shown, most} = await page.evaluate(showChoice, index);
		count = most;
		if (index > 0 && shown === 0) continue;
		for (const result of await page.evaluate(measurePage, LIMITS)) {
			const key = `${result.selector}\n${result.text}`;
			const seen = byElement.get(key);
			if (!seen || (seen.rules.length === 0 && result.rules.length > 0)) byElement.set(key, result);
		}
	}
	return [...byElement.values()];
}

async function checkPages(browser, baseUrl, pages, widths) {
	const findings = [];
	for (const width of widths) {
		const context = await browser.newContext({
			viewport: {width, height: 900},
			colorScheme: 'light',
			locale: 'en-GB',
			reducedMotion: 'reduce',
		});
		await context.addInitScript(() => {
			try {
				localStorage.setItem('starlight-theme', 'light');
			} catch {}
		});
		const page = await context.newPage();
		for (const pagePath of pages) {
			await preparePage(page, `${baseUrl}${pagePath}`);
			const results = await measureChoices(page);
			for (const result of results) findings.push({width, page: pagePath, ...result});
		}
		await context.close();
	}
	return findings;
}

function textHash(text) {
	return createHash('sha256').update(text).digest('hex').slice(0, 12);
}

function freePort() {
	return new Promise((resolve, reject) => {
		const server = createServer();
		server.once('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const {port} = server.address();
			server.close(() => resolve(port));
		});
	});
}

function build() {
	execFileSync('pnpm', ['--silent', 'build'], {cwd: ROOT, stdio: ['ignore', 'ignore', 'inherit']});
}

async function startServer() {
	if (!existsSync(path.join(ROOT, 'dist/server/entry.mjs')))
		throw new Error('dist/server/entry.mjs is missing, run without --no-build');
	const port = await freePort();
	const child = spawn(process.execPath, ['--import', 'tsx', 'scripts/StartServer.ts'], {
		cwd: ROOT,
		env: {...process.env, FLUXER_DOCS_LISTEN_HOST: '127.0.0.1', FLUXER_DOCS_LISTEN_PORT: String(port)},
		stdio: ['ignore', 'ignore', 'inherit'],
	});
	const baseUrl = `http://127.0.0.1:${port}`;
	for (let attempt = 0; attempt < 300; attempt += 1) {
		if (child.exitCode !== null) throw new Error(`docs server exited with code ${child.exitCode}`);
		try {
			if ((await fetch(`${baseUrl}/sitemap-index.xml`)).ok) return {child, baseUrl};
		} catch {}
		await new Promise((resolve) => setTimeout(resolve, 100));
	}
	child.kill('SIGTERM');
	throw new Error('docs server did not start within 30 seconds');
}

function stopServer(child) {
	if (child.exitCode === null) process.kill(child.pid, 'SIGTERM');
}

function shorten(text) {
	return text.length > 70 ? `${text.slice(0, 70)}...` : text;
}

const started = Date.now();
const allowlist = JSON.parse(readFileSync(ALLOWLIST_PATH, 'utf8'));
const widths = options.width ? options.width.map(Number) : WIDTHS;
let child = null;
const cleanup = () => child && stopServer(child);
process.on('SIGINT', () => {
	cleanup();
	process.exit(130);
});
let findings = [];
let pages = [];
const browser = await chromium.launch({headless: true, args: ['--font-render-hinting=none']});
try {
	let baseUrl = options['base-url']?.replace(/\/$/, '');
	if (!baseUrl) {
		if (!options['no-build']) build();
		({child, baseUrl} = await startServer());
	}
	pages = (options.page ?? (await discoverPages(baseUrl))).filter((pagePath) => !isReference(pagePath));
	findings = await checkPages(browser, baseUrl, pages, widths);
} finally {
	cleanup();
	await browser.close();
}

const used = new Set();
for (const finding of findings) {
	finding.hash = textHash(finding.text);
	if (finding.rules.length === 0) continue;
	const entry = allowlist.find((item) => item.page === finding.page && item.hash === finding.hash);
	if (entry) {
		used.add(entry);
		finding.allowed = entry.reason;
	}
}
const failures = findings.filter((finding) => finding.rules.length > 0 && !finding.allowed);
const partial = options.page || options.width;
const stale = partial ? [] : allowlist.filter((entry) => !used.has(entry));
const seconds = ((Date.now() - started) / 1000).toFixed(1);

if (options.json) {
	process.stdout.write(
		`${JSON.stringify({findings: options.all ? findings : findings.filter((finding) => finding.rules.length > 0), stale}, null, 2)}\n`,
	);
} else {
	const shown = options.all ? findings : findings.filter((finding) => finding.rules.length > 0);
	for (const finding of shown) {
		let status = 'FAIL';
		if (finding.rules.length === 0) {
			status = 'ok';
		} else if (finding.allowed) {
			status = 'allowed';
		}
		const tail = finding.lastLine === null ? '' : ` last line "${finding.lastLine}"`;
		process.stdout.write(
			`${status} ${finding.page} @${finding.width} ${finding.lines} lines ${finding.fraction.toFixed(2)} ${finding.rules.join(', ')}${tail}\n    ${finding.section ? `[${finding.section}] ` : ''}${finding.selector}\n    "${shorten(finding.text)}" ${finding.hash}\n`,
		);
	}
	for (const entry of stale)
		process.stdout.write(`STALE allowlist entry ${entry.page} ${entry.hash} "${entry.text}"\n`);
	process.stdout.write(
		`prose shape: ${findings.length} measurements on ${pages.length} pages at ${widths.length} widths, ${failures.length} failures, ${stale.length} stale allowlist entries, ${seconds}s\n`,
	);
}
process.exitCode = failures.length > 0 || stale.length > 0 ? 1 : 0;
