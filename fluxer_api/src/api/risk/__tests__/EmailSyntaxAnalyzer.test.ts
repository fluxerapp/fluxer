// SPDX-License-Identifier: AGPL-3.0-or-later

import {analyzeEmailSyntax} from '@app/api/risk/adapters/EmailSyntaxAnalyzer';
import {describe, expect, it} from 'vitest';

describe('analyzeEmailSyntax', () => {
	it('rejects malformed addresses', () => {
		const r = analyzeEmailSyntax({email: 'not-an-email'});
		expect(r.valid).toBe(false);
		expect(r.gibberishScore).toBe(100);
	});
	it('marks "john.smith@gmail.com" as name-like and not gibberish', () => {
		const r = analyzeEmailSyntax({email: 'john.smith@gmail.com'});
		expect(r.valid).toBe(true);
		expect(r.looksLikeName).toBe(true);
		expect(r.gibberishScore).toBeLessThanOrEqual(20);
	});
	it('flags "iqiibbqai2@unik.it.com" as gibberish via unusual repeats and digit suffix', () => {
		const r = analyzeEmailSyntax({email: 'iqiibbqai2@unik.it.com'});
		expect(r.valid).toBe(true);
		expect(r.unusualRepeats).toBe(true);
		expect(r.gibberishScore).toBeGreaterThanOrEqual(40);
	});
	it('flags keyboard-mash strings', () => {
		const r = analyzeEmailSyntax({email: 'qwertyuiop123@example.com'});
		expect(r.keyboardMashDetected).toBe(true);
		expect(r.gibberishScore).toBeGreaterThanOrEqual(30);
	});
	it('penalizes high-entropy short locals', () => {
		const r = analyzeEmailSyntax({email: 'xk3q9z@example.com'});
		expect(r.gibberishScore).toBeGreaterThanOrEqual(40);
	});
	it('treats well-formatted dotted display-name addresses as low risk', () => {
		const r = analyzeEmailSyntax({email: 'maria.gonzalez@protonmail.com'});
		expect(r.looksLikeName).toBe(true);
		expect(r.gibberishScore).toBeLessThan(30);
	});
	it('marks the digit-heavy bot pattern with score boost', () => {
		const r = analyzeEmailSyntax({email: 'abcdef12345@example.com'});
		expect(r.gibberishScore).toBeGreaterThan(20);
	});
});
