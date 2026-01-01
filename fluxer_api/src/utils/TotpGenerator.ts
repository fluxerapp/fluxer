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

import {timingSafeEqual, webcrypto} from 'node:crypto';

export type HashAlgorithm = 'SHA-1' | 'SHA-256' | 'SHA-512';

export interface TotpOptions {
	timeStep?: number;
	digits?: number;
	window?: number;
	algorithm?: HashAlgorithm;
	startTime?: number;
}

export class TotpGenerator {
	private readonly timeStep: number;
	private readonly digits: number;
	private readonly window: number;
	private readonly algorithm: HashAlgorithm;
	private readonly startTime: number;

	private readonly secretBytes: Uint8Array;
	private readonly keyPromise: Promise<webcrypto.CryptoKey>;

	constructor(secretBase32: string, options: TotpOptions = {}) {
		this.timeStep = options.timeStep ?? 30;
		this.digits = options.digits ?? 6;
		this.window = options.window ?? 1;
		this.algorithm = options.algorithm ?? 'SHA-1';
		this.startTime = options.startTime ?? 0;

		if (!Number.isInteger(this.timeStep) || this.timeStep <= 0) {
			throw new Error('Invalid timeStep.');
		}

		if (!Number.isInteger(this.digits) || this.digits <= 0 || this.digits > 10) {
			throw new Error('Invalid digits.');
		}

		if (!Number.isInteger(this.window) || this.window < 0 || this.window > 50) {
			throw new Error('Invalid window.');
		}

		if (!Number.isInteger(this.startTime) || this.startTime < 0) {
			throw new Error('Invalid startTime.');
		}

		if (this.algorithm !== 'SHA-1' && this.algorithm !== 'SHA-256' && this.algorithm !== 'SHA-512') {
			throw new Error('Invalid algorithm.');
		}

		const normalized = TotpGenerator.normalizeBase32(secretBase32);
		this.secretBytes = TotpGenerator.base32ToBytes(normalized);

		const {subtle} = webcrypto;
		const keyBytes = new ArrayBuffer(this.secretBytes.byteLength);
		new Uint8Array(keyBytes).set(this.secretBytes);
		this.keyPromise = subtle.importKey('raw', keyBytes, {name: 'HMAC', hash: this.algorithm}, false, ['sign']);
	}

	private static normalizeBase32(input: string): string {
		const s = input.trim().replace(/[\s-]/g, '').toUpperCase().replace(/=+$/g, '');
		if (s.length === 0) {
			throw new Error('Invalid base32 character.');
		}
		return s;
	}

	private static base32ToBytes(base32: string): Uint8Array {
		const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
		let buffer = 0;
		let bitsLeft = 0;
		const out: Array<number> = [];

		for (let i = 0; i < base32.length; i++) {
			const ch = base32.charAt(i);
			const value = alphabet.indexOf(ch);
			if (value === -1) {
				throw new Error('Invalid base32 character.');
			}
			buffer = (buffer << 5) | value;
			bitsLeft += 5;

			while (bitsLeft >= 8) {
				out.push((buffer >> (bitsLeft - 8)) & 0xff);
				bitsLeft -= 8;
			}
		}

		const ab = new ArrayBuffer(out.length);
		const bytes = new Uint8Array(ab);
		for (let i = 0; i < out.length; i++) bytes[i] = out[i];
		return bytes;
	}

	private getCounter(nowMs: number): bigint {
		const epochSeconds = BigInt(Math.floor(nowMs / 1000));
		const t0 = BigInt(this.startTime);
		const step = BigInt(this.timeStep);
		if (epochSeconds <= t0) return 0n;
		return (epochSeconds - t0) / step;
	}

	private encodeCounter(counter: bigint): ArrayBuffer {
		const ab = new ArrayBuffer(8);
		const view = new DataView(ab);
		const high = Number((counter >> 32n) & 0xffffffffn);
		const low = Number(counter & 0xffffffffn);
		view.setUint32(0, high, false);
		view.setUint32(4, low, false);
		return ab;
	}

	private async otpForCounter(counter: bigint): Promise<string> {
		const key = await this.keyPromise;
		const {subtle} = webcrypto;

		const msg = this.encodeCounter(counter);
		const hmac = new Uint8Array(await subtle.sign('HMAC', key, msg));

		const offset = hmac[hmac.length - 1] & 0x0f;
		const binary =
			((hmac[offset] & 0x7f) << 24) |
			((hmac[offset + 1] & 0xff) << 16) |
			((hmac[offset + 2] & 0xff) << 8) |
			(hmac[offset + 3] & 0xff);

		const mod = 10 ** this.digits;
		const otp = binary % mod;
		return String(otp).padStart(this.digits, '0');
	}

	async generateTotp(): Promise<Array<string>> {
		const nowMs = Date.now();
		const base = this.getCounter(nowMs);

		const otps: Array<string> = [];
		for (let i = -this.window; i <= this.window; i++) {
			const c = base + BigInt(i);
			if (c < 0n) continue;
			otps.push(await this.otpForCounter(c));
		}

		return otps;
	}

	async validateTotp(code: string): Promise<boolean> {
		if (code.length !== this.digits) return false;
		if (!/^\d+$/.test(code)) return false;

		const candidates = await this.generateTotp();
		const enc = new TextEncoder();
		const target = enc.encode(code);

		let ok = false;
		for (const candidate of candidates) {
			const cand = enc.encode(candidate);
			if (cand.length !== target.length) continue;
			if (timingSafeEqual(cand, target)) ok = true;
		}

		return ok;
	}
}
