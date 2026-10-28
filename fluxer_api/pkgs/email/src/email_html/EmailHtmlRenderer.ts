// SPDX-License-Identifier: AGPL-3.0-or-later

import {EMAIL_HTML_LAYOUTS} from '@pkgs/email/src/email_html/EmailHtmlLayouts.generated';

export type EmailTextDirection = 'ltr' | 'rtl';

const RTL_LANGUAGES = new Set(['ar', 'he']);
const LOCAL_HOSTNAMES = new Set(['localhost', '127.0.0.1', '[::1]']);
const SLOT_PATTERN = /%%([a-z0-9_]+)%%/g;
const BLOCK_PATTERN = /%%\?([a-z0-9_]+)%%([\s\S]*?)%%\/\1%%/g;
const LANG_PATTERN = /^[a-zA-Z]{2,3}(?:-[a-zA-Z0-9]{2,8})*$/;
const CHANNEL_MENTION_PATTERN = /(?<![\w/&#])#[a-z][a-z0-9-]*/g;
const LEFT_TO_RIGHT_ISOLATE = '\u2066';
const FIRST_STRONG_ISOLATE = '\u2068';
const POP_DIRECTIONAL_ISOLATE = '\u2069';
const INERT_LITERALS: ReadonlyArray<readonly [string, string]> = [
	['<', '\uE000'],
	['>', '\uE001'],
	['[', '\uE002'],
	[']', '\uE003'],
];
const INERT_PATTERN = /[<>[\]]/g;
const LITERAL_PATTERN = /[\uE000-\uE003]/g;
const TO_INERT = new Map(INERT_LITERALS);
const TO_LITERAL = new Map(INERT_LITERALS.map(([literal, inert]) => [inert, literal]));
const HTML_ESCAPES: Record<string, string> = {
	'&': '&amp;',
	'<': '&lt;',
	'>': '&gt;',
	'"': '&quot;',
	"'": '&#39;',
};

export function getEmailTextDirection(locale: string): EmailTextDirection {
	return RTL_LANGUAGES.has(locale.split('-')[0].toLowerCase()) ? 'rtl' : 'ltr';
}

export function escapeHtml(value: string): string {
	return value.replace(/[&<>"']/g, (character) => HTML_ESCAPES[character]);
}

export function inertEmailValue(value: string): string {
	return value.replace(INERT_PATTERN, (character) => TO_INERT.get(character) ?? character);
}

export function restoreEmailLiterals(value: string): string {
	return value.replace(LITERAL_PATTERN, (character) => TO_LITERAL.get(character) ?? character);
}

export function isolateEmailValue(value: string): string {
	return `${FIRST_STRONG_ISOLATE}${value}${POP_DIRECTIONAL_ISOLATE}`;
}

function bidiIsolatesToHtml(value: string): string {
	const open: Array<boolean> = [];
	let out = '';
	for (const character of value) {
		if (character === FIRST_STRONG_ISOLATE) {
			open.push(true);
			out += '<bdi>';
		} else if (character === LEFT_TO_RIGHT_ISOLATE) {
			open.push(false);
			out += '<span dir="ltr">';
		} else if (character === POP_DIRECTIONAL_ISOLATE) {
			const isBdi = open.pop();
			if (isBdi === true) out += '</bdi>';
			else if (isBdi === false) out += '</span>';
		} else {
			out += character;
		}
	}
	return open.reduceRight((closing, isBdi) => closing + (isBdi ? '</bdi>' : '</span>'), out);
}

function escapeText(value: string): string {
	return bidiIsolatesToHtml(escapeHtml(restoreEmailLiterals(value)));
}

function hasUnsafeUrlCharacters(value: string): boolean {
	for (const character of value) {
		const code = character.codePointAt(0) ?? 0;
		if (code <= 0x20 || code === 0x7f || /\s/u.test(character)) {
			return true;
		}
	}
	return false;
}

export function validateEmailUrl(value: string, ownHttpOrigins: ReadonlySet<string> = new Set()): string {
	if (hasUnsafeUrlCharacters(value)) {
		throw new Error('Email link contains whitespace or control characters');
	}
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		throw new Error('Email link is not an absolute URL');
	}
	if (url.username || url.password) {
		throw new Error('Email link must not contain credentials');
	}
	const isAllowedHttp =
		url.protocol === 'http:' && (LOCAL_HOSTNAMES.has(url.hostname) || ownHttpOrigins.has(url.origin));
	if (url.protocol !== 'https:' && !isAllowedHttp) {
		throw new Error(`Email link must use https: ${url.protocol}`);
	}
	return value;
}

function isolateChannelMentions(value: string): string {
	return value.replace(
		CHANNEL_MENTION_PATTERN,
		(mention) => `${LEFT_TO_RIGHT_ISOLATE}${mention}${POP_DIRECTIONAL_ISOLATE}`,
	);
}

type Slots = Readonly<Record<string, string | null | undefined>>;

interface InlineLink {
	anchor: string;
	url: string | null;
}

type InlinePart = string | InlineLink;

const INLINE_PATTERN = /<(mail|settings|dsa|arn)>([^<]*)<\/\1>|\[([^\]\n]+)\]\(([^)\s]+)\)/g;
const CITED_LINK_HOSTS = ['digital-strategy.ec.europa.eu', 'www.arn.se'];

export function emailLinkHosts(baseUrls: ReadonlyArray<string>): ReadonlySet<string> {
	const hosts = new Set(CITED_LINK_HOSTS);
	for (const base of baseUrls) {
		if (URL.canParse(base)) {
			hosts.add(new URL(base).host);
		}
	}
	return hosts;
}
const EMAIL_PATTERN = /^[^\s@<>()]+@[^\s@<>()]+\.[^\s@<>()]+$/;

function inlineParts(
	text: string,
	slots: Slots,
	checkUrl: ((url: string) => string) | null,
	linkHosts: ReadonlySet<string> | null = null,
): Array<InlinePart> {
	const parts: Array<InlinePart> = [];
	let last = 0;
	for (const match of text.matchAll(INLINE_PATTERN)) {
		const index = match.index ?? 0;
		parts.push(text.slice(last, index));
		last = index + match[0].length;
		if (match[1]) {
			const [, tag, anchor] = match;
			if (tag === 'mail') {
				if (!EMAIL_PATTERN.test(anchor) && checkUrl) {
					throw new Error(`Email link tag holds no address: ${anchor}`);
				}
				parts.push({anchor, url: `mailto:${anchor}`});
				continue;
			}
			const url = slots[`${tag}_url`];
			if (!url && checkUrl) {
				throw new Error(`Email link tag <${tag}> has no ${tag}_url slot`);
			}
			parts.push({anchor, url: url && checkUrl ? checkUrl(url) : (url ?? null)});
			continue;
		}
		const [whole, , , anchor, url] = match;
		if (linkHosts && !(URL.canParse(url) && linkHosts.has(new URL(url).host))) {
			parts.push(whole);
			continue;
		}
		try {
			parts.push({anchor, url: checkUrl ? checkUrl(url) : url});
		} catch {
			parts.push(whole);
		}
	}
	parts.push(text.slice(last));
	return parts.filter((part) => part !== '');
}

function inlineHtml(
	text: string,
	slots: Slots,
	direction: EmailTextDirection,
	ownHttpOrigins: ReadonlySet<string>,
	linkHosts: ReadonlySet<string>,
) {
	return inlineParts(text, slots, (url) => validateEmailUrl(url, ownHttpOrigins), linkHosts)
		.map((part) => {
			if (typeof part === 'string') {
				return escapeText(direction === 'rtl' ? isolateChannelMentions(part) : part);
			}
			const href = part.url ?? '';
			const dir = href.startsWith('mailto:') ? ' dir="ltr"' : '';
			return `<a href="${escapeHtml(href)}" class="x-link"${dir} style="color:#4641d9">${escapeText(part.anchor)}</a>`;
		})
		.join('');
}

function paragraphs(value: string): Array<string> {
	return value
		.split(/\r?\n/)
		.map((line) => line.trim())
		.filter(Boolean);
}

function encodeSlot(
	name: string,
	value: string,
	slots: Slots,
	direction: EmailTextDirection,
	ownHttpOrigins: ReadonlySet<string>,
	linkHosts: ReadonlySet<string>,
): string {
	if (name === 'lang') {
		if (!LANG_PATTERN.test(value)) {
			throw new Error(`Invalid email language tag: ${value}`);
		}
		return value;
	}
	if (name.endsWith('_url')) {
		return escapeHtml(validateEmailUrl(value, ownHttpOrigins));
	}
	if (name === 'subject' || name === 'preheader' || name === 'heading' || name === 'code' || name.endsWith('_label')) {
		const literal = restoreEmailLiterals(value);
		return escapeHtml(direction === 'rtl' ? isolateChannelMentions(literal) : literal);
	}
	if (name.endsWith('_list')) {
		return paragraphs(value)
			.map((item) => `<li style="margin:0 0 6px">${inlineHtml(item, slots, direction, ownHttpOrigins, linkHosts)}</li>`)
			.join('');
	}
	if (/^(fact_\d+|sum_\d+|sum_total)_value$/.test(name)) {
		const lines = paragraphs(value).map((line) =>
			inlineHtml(direction === 'rtl' ? isolateEmailValue(line) : line, slots, direction, ownHttpOrigins, linkHosts),
		);
		return lines.length === 1 ? lines[0] : lines.map((line) => `<div>${line}</div>`).join('');
	}
	const blocks = paragraphs(value);
	if (blocks.length <= 1) {
		return inlineHtml(blocks[0] ?? '', slots, direction, ownHttpOrigins, linkHosts);
	}
	return blocks
		.map(
			(block, index) =>
				`<p style="margin:${index === 0 ? 0 : '12px 0 0'}">${inlineHtml(block, slots, direction, ownHttpOrigins, linkHosts)}</p>`,
		)
		.join('');
}

export function renderEmailHtml(
	direction: EmailTextDirection,
	slots: Slots,
	ownHttpOrigins: ReadonlySet<string> = new Set(),
	linkHosts: ReadonlySet<string> = new Set(CITED_LINK_HOSTS),
): string {
	let html: string = EMAIL_HTML_LAYOUTS.message[direction];
	for (let previous = ''; previous !== html; ) {
		previous = html;
		html = html.replace(BLOCK_PATTERN, (_match, name: string, inner: string) => (slots[name] ? inner : ''));
	}
	return html.replace(SLOT_PATTERN, (_match, name: string) => {
		const value = slots[name];
		if (typeof value !== 'string') {
			throw new Error(`Missing email layout slot ${name}`);
		}
		return encodeSlot(name, value, slots, direction, ownHttpOrigins, linkHosts);
	});
}

export type EmailTextLinks = 'plain' | 'markdown';

export function stripInlineMarkup(text: string): string {
	return inlineParts(text, {}, null)
		.map((part) => (typeof part === 'string' ? part : part.anchor))
		.join('');
}

const FACT_COUNT = 7;
const SUMMARY_COUNT = 7;
const SUMMARY_TOTAL_AFTER = 5;
const SECTION_COUNT = 4;

function textInline(text: string, slots: Slots, links: EmailTextLinks): {line: string; urls: Array<string>} {
	const urls: Array<string> = [];
	const line = inlineParts(text, slots, null)
		.map((part) => {
			if (typeof part === 'string') {
				return part;
			}
			if (!part.url || part.url.startsWith('mailto:')) {
				return part.anchor;
			}
			if (links === 'markdown') {
				return `[${part.anchor}](${part.url})`;
			}
			urls.push(part.url);
			return part.anchor;
		})
		.join('');
	return {line, urls};
}

function textParagraphs(value: string | null | undefined, slots: Slots, links: EmailTextLinks): string | null {
	if (!value) {
		return null;
	}
	return paragraphs(value)
		.map((block) => {
			const {line, urls} = textInline(block, slots, links);
			return [line, ...urls].join('\n');
		})
		.join('\n\n');
}

function textList(value: string | null | undefined, slots: Slots, links: EmailTextLinks): string | null {
	if (!value) {
		return null;
	}
	return paragraphs(value)
		.map((item) => {
			const {line, urls} = textInline(item, slots, links);
			return [`- ${line}`, ...urls.map((url) => `  ${url}`)].join('\n');
		})
		.join('\n');
}

function textLink(label: string | null | undefined, url: string | null | undefined, links: EmailTextLinks) {
	if (!label || !url) {
		return null;
	}
	return links === 'markdown' ? `[${label}](${url})` : `${label}\n${url}`;
}

function textBlock(...lines: Array<string | null | undefined>): string {
	return lines.filter((line): line is string => Boolean(line)).join('\n');
}

export function renderEmailText(slots: Slots, links: EmailTextLinks = 'plain'): string {
	const facts: Array<string> = [];
	for (let n = 1; n <= FACT_COUNT; n++) {
		const label = slots[`fact_${n}_label`];
		const value = slots[`fact_${n}_value`] ?? slots[`fact_${n}_link`];
		if (!label || !value) {
			continue;
		}
		const url = slots[`fact_${n}_link_url`];
		if (slots[`fact_${n}_link`] && url) {
			facts.push(links === 'markdown' ? `${label}: [${value}](${url})` : `${label}: ${value}\n${url}`);
			continue;
		}
		const {line, urls} = textInline(paragraphs(value).join('\n'), slots, links);
		facts.push([`${label}: ${line}`, ...urls].join('\n'));
	}
	const summary: Array<string> = [];
	const summaryLine = (label: string | null | undefined, value: string | null | undefined) => {
		if (label && value) {
			summary.push(`${label}: ${paragraphs(value).join(' ')}`);
		}
	};
	for (let n = 1; n <= SUMMARY_COUNT; n++) {
		summaryLine(slots[`sum_${n}_label`], slots[`sum_${n}_value`]);
		if (n === SUMMARY_TOTAL_AFTER) {
			summaryLine(slots.sum_total_label, slots.sum_total_value);
			if (slots.sum_total_label && slots.sum_tax) {
				summary.push(slots.sum_tax);
			}
		}
	}
	const sections: Array<string> = [];
	for (let n = 1; n <= SECTION_COUNT; n++) {
		const title = slots[`section_${n}_title`];
		if (!title) {
			continue;
		}
		const blocks = [
			textParagraphs(slots[`section_${n}_body`], slots, links),
			textList(slots[`section_${n}_list`], slots, links),
			textParagraphs(slots[`section_${n}_note`], slots, links),
			textLink(slots[`section_${n}_link`], slots[`section_${n}_link_url`], links),
		].filter(Boolean);
		sections.push(`${title}\n${blocks.join('\n\n')}`);
	}
	const footer = textBlock(
		textParagraphs(slots.footer_reason, slots, links),
		textLink(slots.unsubscribe, slots.unsubscribe_url, links),
		slots.legal_address,
	);
	const text = [
		slots.heading,
		textParagraphs(slots.intro, slots, links),
		slots.code,
		facts.join('\n'),
		summary.join('\n'),
		textList(slots.points_list, slots, links),
		textParagraphs(slots.lead, slots, links),
		textParagraphs(slots.box, slots, links),
		textLink(slots.cta, slots.cta_url, links),
		textParagraphs(slots.expiry, slots, links),
		textParagraphs(slots.text_1, slots, links),
		textParagraphs(slots.text_2, slots, links),
		...sections,
		textParagraphs(slots.note, slots, links),
		textLink(slots.end_link, slots.end_link_url, links),
		slots.signoff,
		footer,
	]
		.filter((block): block is string => Boolean(block))
		.join('\n\n');
	return restoreEmailLiterals(text);
}
