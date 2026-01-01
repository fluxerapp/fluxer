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

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {type HashAlgorithm, TotpGenerator} from './TotpGenerator';

function asciiToBase32(input: string): string {
	const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
	const bytes = new TextEncoder().encode(input);

	let out = '';
	let buffer = 0;
	let bitsLeft = 0;

	for (const b of bytes) {
		buffer = (buffer << 8) | b;
		bitsLeft += 8;

		while (bitsLeft >= 5) {
			const idx = (buffer >> (bitsLeft - 5)) & 31;
			out += alphabet[idx];
			bitsLeft -= 5;
		}
	}

	if (bitsLeft > 0) {
		const idx = (buffer << (5 - bitsLeft)) & 31;
		out += alphabet[idx];
	}

	while (out.length % 8 !== 0) out += '=';
	return out;
}

describe('TotpGenerator', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		vi.useRealTimers();
		vi.restoreAllMocks();
	});

	describe('RFC 6238 Appendix B vectors', () => {
		const seedSha1 = '12345678901234567890';
		const seedSha256 = '12345678901234567890123456789012';
		const seedSha512 = '1234567890123456789012345678901234567890123456789012345678901234';

		const cases: Array<{
			algorithm: HashAlgorithm;
			secret: string;
			vectors: Array<{t: number; otp: string}>;
		}> = [
			{
				algorithm: 'SHA-1',
				secret: asciiToBase32(seedSha1),
				vectors: [
					{t: 59, otp: '94287082'},
					{t: 1111111109, otp: '07081804'},
					{t: 1111111111, otp: '14050471'},
					{t: 1234567890, otp: '89005924'},
					{t: 2000000000, otp: '69279037'},
					{t: 20000000000, otp: '65353130'},
				],
			},
			{
				algorithm: 'SHA-256',
				secret: asciiToBase32(seedSha256),
				vectors: [
					{t: 59, otp: '46119246'},
					{t: 1111111109, otp: '68084774'},
					{t: 1111111111, otp: '67062674'},
					{t: 1234567890, otp: '91819424'},
					{t: 2000000000, otp: '90698825'},
					{t: 20000000000, otp: '77737706'},
				],
			},
			{
				algorithm: 'SHA-512',
				secret: asciiToBase32(seedSha512),
				vectors: [
					{t: 59, otp: '90693936'},
					{t: 1111111109, otp: '25091201'},
					{t: 1111111111, otp: '99943326'},
					{t: 1234567890, otp: '93441116'},
					{t: 2000000000, otp: '38618901'},
					{t: 20000000000, otp: '47863826'},
				],
			},
		];

		for (const c of cases) {
			for (const v of c.vectors) {
				it(`matches RFC vector (${c.algorithm}) at t=${v.t}`, async () => {
					vi.setSystemTime(v.t * 1000);

					const gen = new TotpGenerator(c.secret, {
						algorithm: c.algorithm,
						digits: 8,
						timeStep: 30,
						window: 0,
						startTime: 0,
					});

					const otps = await gen.generateTotp();
					expect(otps).toHaveLength(1);
					expect(otps[0]).toBe(v.otp);
					expect(await gen.validateTotp(v.otp)).toBe(true);
				});
			}
		}
	});

	describe('constructor validation', () => {
		it('throws on invalid base32', () => {
			expect(() => new TotpGenerator('INVALID@CHARS')).toThrow('Invalid base32 character.');
		});

		it('throws on invalid timeStep', () => {
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {timeStep: 0})).toThrow('Invalid timeStep.');
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {timeStep: -1})).toThrow('Invalid timeStep.');
		});

		it('throws on invalid digits', () => {
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {digits: 0})).toThrow('Invalid digits.');
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {digits: 11})).toThrow('Invalid digits.');
		});

		it('throws on invalid window', () => {
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {window: -1})).toThrow('Invalid window.');
		});

		it('throws on invalid startTime', () => {
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {startTime: -1})).toThrow('Invalid startTime.');
		});

		it('throws on invalid algorithm', () => {
			expect(() => new TotpGenerator('JBSWY3DPEHPK3PXP', {algorithm: 'MD5' as HashAlgorithm})).toThrow(
				'Invalid algorithm.',
			);
		});
	});

	describe('generateTotp behavior', () => {
		it('returns 2*window+1 codes', async () => {
			vi.setSystemTime(1609459200000);
			const gen = new TotpGenerator('JBSWY3DPEHPK3PXP', {window: 2});
			const otps = await gen.generateTotp();
			expect(otps).toHaveLength(5);
		});

		it('uses Date.now exactly once per call', async () => {
			const nowSpy = vi.spyOn(Date, 'now').mockReturnValue(1609459200000);
			const gen = new TotpGenerator('JBSWY3DPEHPK3PXP', {window: 2});
			await gen.generateTotp();
			expect(nowSpy).toHaveBeenCalledTimes(1);
		});

		it('generates numeric codes of the configured length', async () => {
			vi.setSystemTime(1609459200000);
			const gen = new TotpGenerator('JBSWY3DPEHPK3PXP', {digits: 8});
			const otps = await gen.generateTotp();

			for (const otp of otps) {
				expect(otp).toHaveLength(8);
				expect(/^\d+$/.test(otp)).toBe(true);
			}
		});

		it('respects timeStep (same window => same TOTP)', async () => {
			const gen = new TotpGenerator('JBSWY3DPEHPK3PXP', {timeStep: 60, window: 0});
			vi.setSystemTime(30_000);
			const [a] = await gen.generateTotp();
			vi.setSystemTime(59_999);
			const [b] = await gen.generateTotp();
			expect(a).toBe(b);
			vi.setSystemTime(60_000);
			const [c] = await gen.generateTotp();
			expect(a).not.toBe(c);
		});

		it('is consistent across instances for the same time', async () => {
			vi.setSystemTime(1609459200000);
			const g1 = new TotpGenerator('JBSWY3DPEHPK3PXP');
			const g2 = new TotpGenerator('JBSWY3DPEHPK3PXP');
			expect(await g1.generateTotp()).toEqual(await g2.generateTotp());
		});
	});

	describe('base32 normalization', () => {
		it('treats lowercase/whitespace/hyphens/padding as equivalent', async () => {
			vi.setSystemTime(59_000);

			const base = asciiToBase32('12345678901234567890');
			const stripped = base.replace(/=+$/g, '');

			const variants = [
				stripped.toLowerCase(),
				stripped.match(/.{1,4}/g)!.join(' '),
				stripped.match(/.{1,4}/g)!.join('-'),
				base,
			];

			const expected = (
				await new TotpGenerator(variants[0], {
					algorithm: 'SHA-1',
					digits: 8,
					window: 0,
				}).generateTotp()
			)[0];

			for (const s of variants) {
				const got = (
					await new TotpGenerator(s, {
						algorithm: 'SHA-1',
						digits: 8,
						window: 0,
					}).generateTotp()
				)[0];
				expect(got).toBe(expected);
			}
		});
	});

	describe('validateTotp behavior', () => {
		it('accepts the current code', async () => {
			vi.setSystemTime(1609459200000);
			const gen = new TotpGenerator('JBSWY3DPEHPK3PXP', {window: 1});
			const otps = await gen.generateTotp();
			expect(await gen.validateTotp(otps[1])).toBe(true);
		});

		it('accepts codes within the configured window', async () => {
			vi.setSystemTime(1111111109 * 1000);
			const secret = asciiToBase32('12345678901234567890');
			const gen = new TotpGenerator(secret, {
				algorithm: 'SHA-1',
				digits: 8,
				timeStep: 30,
				window: 1,
			});

			const codes = await gen.generateTotp();
			expect(codes).toHaveLength(3);

			for (const code of codes) {
				expect(await gen.validateTotp(code)).toBe(true);
			}
		});

		it('rejects codes outside the window', async () => {
			const secret = asciiToBase32('12345678901234567890');
			const gen = new TotpGenerator(secret, {
				algorithm: 'SHA-1',
				digits: 8,
				timeStep: 30,
				window: 0,
			});

			vi.setSystemTime(1111111109 * 1000);
			const [code] = await gen.generateTotp();

			vi.setSystemTime((1111111109 + 60) * 1000);
			expect(await gen.validateTotp(code)).toBe(false);
		});

		it('rejects empty, non-numeric, and wrong-length codes', async () => {
			vi.setSystemTime(1609459200000);
			const gen = new TotpGenerator('JBSWY3DPEHPK3PXP', {digits: 6, window: 1});

			expect(await gen.validateTotp('')).toBe(false);
			expect(await gen.validateTotp('abcdef')).toBe(false);
			expect(await gen.validateTotp('12345')).toBe(false);
			expect(await gen.validateTotp('1234567')).toBe(false);
		});
	});

	describe('startTime handling', () => {
		it('applies T0 (startTime) correctly', async () => {
			const secret = asciiToBase32('12345678901234567890');

			const genA = new TotpGenerator(secret, {
				algorithm: 'SHA-1',
				digits: 8,
				timeStep: 30,
				window: 0,
				startTime: 0,
			});

			const genB = new TotpGenerator(secret, {
				algorithm: 'SHA-1',
				digits: 8,
				timeStep: 30,
				window: 0,
				startTime: 100,
			});

			vi.setSystemTime(0);
			const [a0] = await genA.generateTotp();

			vi.setSystemTime(100_000);
			const [b0] = await genB.generateTotp();

			expect(a0).toBe(b0);

			vi.setSystemTime(130_000);
			const [b1] = await genB.generateTotp();
			expect(b0).not.toBe(b1);
		});
	});
});
