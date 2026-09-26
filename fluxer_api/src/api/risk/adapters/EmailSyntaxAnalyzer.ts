// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailSyntaxResult} from '@app/api/risk/RiskTypes';

const KEYBOARD_ROWS = ['qwertyuiop', 'asdfghjkl', 'zxcvbnm', '1234567890'];
const COMMON_BIGRAMS = new Set<string>([
	'th',
	'he',
	'in',
	'er',
	'an',
	're',
	'on',
	'at',
	'en',
	'nd',
	'st',
	'es',
	'or',
	'te',
	'of',
	'it',
	'is',
	'al',
	'ar',
	'le',
	'se',
	'co',
	'de',
	'ma',
	'ri',
	'ne',
	'li',
	'ha',
	'to',
	'ta',
	'io',
	'no',
	'la',
	'el',
	'me',
	'do',
	'si',
	'ra',
	'so',
	'ch',
	'sh',
	'mi',
	'na',
	'ca',
	'da',
	'ni',
	'be',
	'di',
	'lo',
	'wi',
	'ke',
	'tr',
	'mo',
	'jo',
	'ro',
	'sm',
	'ni',
	'ti',
	'ti',
	'ti',
	'as',
	'us',
	'om',
	'ou',
	'ed',
	'io',
	'pa',
	'bo',
	'mu',
	'fi',
	'va',
	'wa',
	'sa',
	'ye',
	'ye',
	'le',
	'pe',
	'pi',
	'po',
	'pu',
	'ba',
	'bi',
	'bu',
	'fa',
	'fe',
	'fi',
	'fo',
	'fu',
	'ga',
	'ge',
	'gi',
	'go',
	'gu',
	'ha',
	'he',
	'hi',
	'ho',
	'hu',
	'ka',
	'ke',
	'ki',
	'ko',
	'ku',
	'lu',
	'va',
	've',
	'vi',
	'vo',
	'vu',
	'ya',
	'yi',
	'yo',
	'yu',
	'za',
	'ze',
	'zo',
]);

function shannonEntropy(s: string): number {
	const freq: Record<string, number> = {};
	for (const c of s) freq[c] = (freq[c] ?? 0) + 1;
	const len = s.length;
	if (len === 0) return 0;
	let entropy = 0;
	for (const f of Object.values(freq)) {
		const p = f / len;
		entropy -= p * Math.log2(p);
	}
	return entropy;
}

function hasKeyboardMash(s: string, threshold = 4): boolean {
	const lower = s.toLowerCase();
	for (const row of KEYBOARD_ROWS) {
		let run = 1;
		for (let i = 1; i < lower.length; i++) {
			const idx = row.indexOf(lower[i]!);
			const prev = row.indexOf(lower[i - 1]!);
			if (idx >= 0 && prev >= 0 && Math.abs(prev - idx) <= 1) {
				run++;
				if (run >= threshold) return true;
			} else {
				run = 1;
			}
		}
	}
	return false;
}

function looksLikeName(s: string): boolean {
	const clean = s.replace(/[.\-_\d]/g, '');
	if (clean.length === 0) return false;
	const vowels = (clean.match(/[aeiou]/gi) || []).length;
	const ratio = vowels / clean.length;
	if (ratio < 0.15 || ratio > 0.75) return false;
	if (/[^aeiou\d]{5,}/i.test(clean)) return false;
	if (/[aeiou]{4,}/i.test(clean)) return false;
	return true;
}

function digitRatio(s: string): number {
	if (s.length === 0) return 0;
	return (s.match(/\d/g) || []).length / s.length;
}

function pronounceabilityScore(s: string): number {
	const clean = s.replace(/[.\-_\d]/g, '').toLowerCase();
	if (clean.length < 3) return 0.5;
	let hits = 0;
	const total = clean.length - 1;
	for (let i = 0; i < total; i++) {
		if (COMMON_BIGRAMS.has(clean.slice(i, i + 2))) hits++;
	}
	return total > 0 ? hits / total : 0;
}

function hasUnusualRepeats(s: string): boolean {
	const clean = s.replace(/[.\-_]/g, '').toLowerCase();
	if (clean.length < 5) return false;
	const freq: Record<string, number> = {};
	for (const c of clean) freq[c] = (freq[c] ?? 0) + 1;
	const maxFreq = Math.max(...Object.values(freq));
	if (maxFreq / clean.length > 0.3 && clean.length < 15) return true;
	const doubles = (clean.match(/(.)\1/g) || []).length;
	if (doubles >= 2 && clean.length < 12) return true;
	return false;
}

const EMAIL_SHAPE = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

export function analyzeEmailSyntax(args: {email: string}): EmailSyntaxResult {
	const email = args.email.trim();
	const isValid = EMAIL_SHAPE.test(email);
	if (!isValid) {
		return {
			email,
			localPart: '',
			domain: '',
			localPartLength: 0,
			entropy: 0,
			keyboardMashDetected: false,
			looksLikeName: false,
			pronounceability: 0,
			unusualRepeats: false,
			digitRatio: 0,
			hasDots: false,
			hasPlusTag: false,
			gibberishScore: 100,
			valid: false,
		};
	}
	const atIdx = email.lastIndexOf('@');
	const localPart = email.slice(0, atIdx);
	const domain = email.slice(atIdx + 1).toLowerCase();
	const entropy = shannonEntropy(localPart);
	const kbMash = hasKeyboardMash(localPart);
	const namelike = looksLikeName(localPart);
	const digits = digitRatio(localPart);
	const hasDots = localPart.includes('.');
	const hasPlusTag = localPart.includes('+');
	const pronounce = pronounceabilityScore(localPart);
	const unusualRepeats = hasUnusualRepeats(localPart);
	let gibberishScore = 0;
	if (entropy > 3.2 && localPart.length < 12) gibberishScore += 20;
	if (entropy > 3.5) gibberishScore += 15;
	if (kbMash) gibberishScore += 20;
	if (!namelike) gibberishScore += 15;
	if (pronounce < 0.15 && localPart.replace(/[.\-_\d]/g, '').length >= 4) gibberishScore += 25;
	else if (pronounce < 0.25) gibberishScore += 10;
	if (unusualRepeats) gibberishScore += 15;
	if (digits > 0.4) gibberishScore += 15;
	else if (digits > 0.2 && !hasDots) gibberishScore += 5;
	if (localPart.length > 20) gibberishScore += 10;
	if (!hasDots && localPart.length > 8 && !namelike && pronounce < 0.25) gibberishScore += 10;
	if (/^[a-z]+\d{2,}$/i.test(localPart) && !namelike) gibberishScore += 10;
	return {
		email,
		localPart,
		domain,
		localPartLength: localPart.length,
		entropy: Math.round(entropy * 100) / 100,
		keyboardMashDetected: kbMash,
		looksLikeName: namelike,
		pronounceability: Math.round(pronounce * 100) / 100,
		unusualRepeats,
		digitRatio: Math.round(digits * 100) / 100,
		hasDots,
		hasPlusTag,
		gibberishScore: Math.min(gibberishScore, 100),
		valid: true,
	};
}
