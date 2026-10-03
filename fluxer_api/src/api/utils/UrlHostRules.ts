// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeHostname} from '@app/api/utils/UrlNormalizer';
import {getDomain} from 'tldts';

const URL_HOST_PATTERN_MAX_WILDCARDS = 3;
const URL_HOST_PATTERN_MIN_LITERAL_CHARS = 3;

const MAX_HOSTNAME_LENGTH = 253;
const HOSTNAME_LABEL_RE = /^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/;
const GLOB_LABEL_RE = /^[a-z0-9*-]{1,63}$/;
const REPEATED_WILDCARDS_RE = /\*+/g;

export type UrlDomainEntry =
	| {ok: true; value: string; pattern: boolean}
	| {
			ok: false;
			message: string;
	  };

function isValidHostname(host: string): boolean {
	if (host.length > MAX_HOSTNAME_LENGTH) return false;
	return host.split('.').every((label) => HOSTNAME_LABEL_RE.test(label));
}

function invalid(message: string): UrlDomainEntry {
	return {ok: false, message};
}

export function parseUrlDomainEntry(raw: string): UrlDomainEntry {
	const value = raw.trim().toLowerCase();
	if (!value.includes('*')) {
		const host = normalizeHostname(value);
		if (!host?.includes('.') || !isValidHostname(host)) {
			return invalid('Must be a valid domain');
		}
		return {ok: true, value: host, pattern: false};
	}
	const separator = value.indexOf('.');
	if (separator === -1) {
		return invalid('A pattern must name a domain after its wildcard label');
	}
	const glob = value.slice(0, separator).replace(REPEATED_WILDCARDS_RE, '*');
	const suffixValue = value.slice(separator + 1);
	if (suffixValue.includes('*')) {
		return invalid('Only the leftmost label of a pattern can contain *');
	}
	if (!GLOB_LABEL_RE.test(glob)) {
		return invalid('The wildcard label can contain only a-z, 0-9, hyphens, and *');
	}
	const wildcards = glob.length - glob.replaceAll('*', '').length;
	if (wildcards > URL_HOST_PATTERN_MAX_WILDCARDS) {
		return invalid(`The wildcard label can contain at most ${URL_HOST_PATTERN_MAX_WILDCARDS} *`);
	}
	if (glob.length - wildcards < URL_HOST_PATTERN_MIN_LITERAL_CHARS) {
		return invalid(
			`The wildcard label needs at least ${URL_HOST_PATTERN_MIN_LITERAL_CHARS} characters besides *, so the pattern is too broad`,
		);
	}
	const suffix = normalizeHostname(suffixValue);
	if (!suffix || !isValidHostname(suffix)) {
		return invalid('The part after the wildcard label must be a valid domain');
	}
	if (getDomain(suffix, {allowPrivateDomains: false}) === null) {
		return invalid('The part after the wildcard label is a public suffix, so the pattern is too broad');
	}
	const entry = `${glob}.${suffix}`;
	if (entry.length > MAX_HOSTNAME_LENGTH) {
		return invalid('Must be a valid domain');
	}
	return {ok: true, value: entry, pattern: true};
}

interface HostPatternRule {
	value: string;
	segments: ReadonlyArray<string>;
	matchSubdomains: boolean;
}

function globMatches(segments: ReadonlyArray<string>, label: string): boolean {
	const first = segments[0] ?? '';
	const last = segments[segments.length - 1] ?? '';
	if (!label.startsWith(first)) return false;
	let position = first.length;
	for (let index = 1; index < segments.length - 1; index++) {
		const segment = segments[index] ?? '';
		const found = label.indexOf(segment, position);
		if (found === -1) return false;
		position = found + segment.length;
	}
	return label.length - last.length >= position && label.endsWith(last);
}

export class UrlHostRuleSet {
	private readonly domains = new Map<string, boolean>();
	private readonly patterns = new Map<string, Array<HostPatternRule>>();
	private patternCount = 0;

	add(rawValue: string, matchSubdomains: boolean): void {
		if (rawValue.includes('*')) {
			const entry = parseUrlDomainEntry(rawValue);
			if (!entry.ok || !entry.pattern) return;
			this.remove(entry.value);
			const separator = entry.value.indexOf('.');
			const suffix = entry.value.slice(separator + 1);
			const rules = this.patterns.get(suffix) ?? [];
			rules.push({
				value: entry.value,
				segments: entry.value.slice(0, separator).split('*'),
				matchSubdomains,
			});
			this.patterns.set(suffix, rules);
			this.patternCount++;
			return;
		}
		const host = normalizeHostname(rawValue);
		if (host) this.domains.set(host, matchSubdomains);
	}

	remove(rawValue: string): void {
		if (!rawValue.includes('*')) {
			const host = normalizeHostname(rawValue);
			if (host) this.domains.delete(host);
			return;
		}
		const entry = parseUrlDomainEntry(rawValue);
		if (!entry.ok) return;
		const suffix = entry.value.slice(entry.value.indexOf('.') + 1);
		const rules = this.patterns.get(suffix);
		if (!rules) return;
		const remaining = rules.filter((rule) => rule.value !== entry.value);
		this.patternCount -= rules.length - remaining.length;
		if (remaining.length === 0) {
			this.patterns.delete(suffix);
		} else {
			this.patterns.set(suffix, remaining);
		}
	}

	matches(rawHost: string): boolean {
		const host = normalizeHostname(rawHost);
		if (!host) return false;
		if (this.domains.has(host)) return true;
		let labelStart = 0;
		let isFirstLabel = true;
		while (labelStart < host.length) {
			const separator = host.indexOf('.', labelStart);
			if (separator === -1) return false;
			const suffix = host.slice(separator + 1);
			if (this.domains.get(suffix) === true) return true;
			const rules = this.patterns.get(suffix);
			if (rules) {
				const label = host.slice(labelStart, separator);
				for (const rule of rules) {
					if ((isFirstLabel || rule.matchSubdomains) && globMatches(rule.segments, label)) return true;
				}
			}
			labelStart = separator + 1;
			isFirstLabel = false;
		}
		return false;
	}

	get size(): {domains: number; patterns: number} {
		return {domains: this.domains.size, patterns: this.patternCount};
	}
}
