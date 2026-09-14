// SPDX-License-Identifier: AGPL-3.0-or-later

import {createCipheriv, createHmac, randomBytes, randomUUID} from 'node:crypto';
import {Logger} from '@app/api/Logger';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';

const KEY_STATE_CACHE_KEY = 'phone-verification:reuse-key-state';
const KEY_ROTATION_LOCK = 'phone-verification-reuse-key-rotation';
const REUSE_KEY_PREFIX = 'phone-verification:reuse';
const REUSE_LOCK_PREFIX = 'phone-verification-reuse';
const ROTATE_AFTER_MS = 28 * 24 * 60 * 60 * 1000;
const REUSE_TTL_SECONDS = seconds('31 days');
const LOCK_TTL_SECONDS = seconds('30 seconds');

interface PhoneReuseKey {
	id: string;
	secret: string;
	created_at: string;
}

interface PhoneReuseKeyState {
	primary: PhoneReuseKey;
	secondary: PhoneReuseKey | null;
}

interface EncryptedPhoneReuseMarker {
	v: 1;
	key_id: string;
	iv: string;
	tag: string;
	ciphertext: string;
	created_at: string;
	verification_count: number;
}

function createKey(): PhoneReuseKey {
	return {
		id: randomUUID(),
		secret: randomBytes(32).toString('base64url'),
		created_at: new Date().toISOString(),
	};
}

function shouldRotate(key: PhoneReuseKey): boolean {
	return Date.now() - new Date(key.created_at).getTime() > ROTATE_AFTER_MS;
}

function phoneDigest(phone: string, key: PhoneReuseKey): string {
	return createHmac('sha256', Buffer.from(key.secret, 'base64url')).update(phone).digest('base64url');
}

function markerKey(phone: string, key: PhoneReuseKey): string {
	return `${REUSE_KEY_PREFIX}:${key.id}:${phoneDigest(phone, key)}`;
}

function encryptPhone(phone: string, key: PhoneReuseKey, verificationCount: number): EncryptedPhoneReuseMarker {
	const iv = randomBytes(12);
	const cipher = createCipheriv('aes-256-gcm', Buffer.from(key.secret, 'base64url'), iv);
	const ciphertext = Buffer.concat([cipher.update(phone, 'utf8'), cipher.final()]);
	return {
		v: 1,
		key_id: key.id,
		iv: iv.toString('base64url'),
		tag: cipher.getAuthTag().toString('base64url'),
		ciphertext: ciphertext.toString('base64url'),
		created_at: new Date().toISOString(),
		verification_count: verificationCount,
	};
}

export class PhoneVerificationReuseStore {
	constructor(private readonly cacheService: ICacheService) {}

	async initialize(): Promise<void> {
		await this.getFreshState();
	}

	async hasReachedVerificationLimit(phone: string): Promise<boolean> {
		return (await this.getRecentVerificationCount(phone)) >= 2;
	}

	async getRecentVerificationCount(phone: string): Promise<number> {
		const state = await this.getFreshState();
		const keys = [state.primary, state.secondary].filter((key): key is PhoneReuseKey => key !== null);
		const results = await this.cacheService.mget<EncryptedPhoneReuseMarker>(keys.map((key) => markerKey(phone, key)));
		return Math.max(0, ...results.map((marker) => marker?.verification_count ?? (marker ? 1 : 0)));
	}

	async recordVerification(phone: string): Promise<void> {
		await this.claimVerificationSlot(phone);
	}

	async claimVerificationSlot(phone: string): Promise<boolean> {
		const state = await this.getFreshState();
		return await this.withPhoneLock(phone, state, async () => {
			const currentCount = await this.getRecentVerificationCountForState(phone, state);
			if (currentCount >= 2) {
				return false;
			}
			const nextCount = currentCount + 1;
			await this.cacheService.set(
				markerKey(phone, state.primary),
				encryptPhone(phone, state.primary, nextCount),
				REUSE_TTL_SECONDS,
			);
			return true;
		});
	}

	private async getRecentVerificationCountForState(phone: string, state: PhoneReuseKeyState): Promise<number> {
		const keys = [state.primary, state.secondary].filter((key): key is PhoneReuseKey => key !== null);
		const results = await this.cacheService.mget<EncryptedPhoneReuseMarker>(keys.map((key) => markerKey(phone, key)));
		return Math.max(0, ...results.map((marker) => marker?.verification_count ?? (marker ? 1 : 0)));
	}

	private async getFreshState(): Promise<PhoneReuseKeyState> {
		const state = await this.cacheService.get<PhoneReuseKeyState>(KEY_STATE_CACHE_KEY);
		if (!state) {
			return await this.withRotationLock(async () => {
				const existing = await this.cacheService.get<PhoneReuseKeyState>(KEY_STATE_CACHE_KEY);
				if (existing) {
					return existing;
				}
				const initialState = {primary: createKey(), secondary: null};
				await this.cacheService.set(KEY_STATE_CACHE_KEY, initialState);
				return initialState;
			});
		}
		if (!shouldRotate(state.primary)) {
			return state;
		}
		return await this.withRotationLock(async () => {
			const latest = await this.cacheService.get<PhoneReuseKeyState>(KEY_STATE_CACHE_KEY);
			if (latest && !shouldRotate(latest.primary)) {
				return latest;
			}
			const basis = latest ?? state;
			const rotated = {
				primary: createKey(),
				secondary: basis.primary,
			};
			await this.cacheService.set(KEY_STATE_CACHE_KEY, rotated);
			Logger.info(
				{newKeyId: rotated.primary.id, secondaryKeyId: rotated.secondary.id},
				'Rotated phone verification reuse encryption key',
			);
			return rotated;
		});
	}

	private async withRotationLock<T>(fn: () => Promise<T>): Promise<T> {
		const token = await this.cacheService.acquireLock(KEY_ROTATION_LOCK, LOCK_TTL_SECONDS);
		if (!token) {
			await new Promise((resolve) => setTimeout(resolve, 250));
			const existing = await this.cacheService.get<PhoneReuseKeyState>(KEY_STATE_CACHE_KEY);
			if (existing) {
				return existing as T;
			}
			throw new Error('Phone verification reuse key state is temporarily unavailable');
		}
		try {
			return await fn();
		} finally {
			await this.cacheService.releaseLock(KEY_ROTATION_LOCK, token);
		}
	}

	private async withPhoneLock<T>(phone: string, state: PhoneReuseKeyState, fn: () => Promise<T>): Promise<T> {
		const lockKey = `${REUSE_LOCK_PREFIX}:${state.primary.id}:${phoneDigest(phone, state.primary)}`;
		const token = await this.cacheService.acquireLock(lockKey, LOCK_TTL_SECONDS);
		if (!token) {
			await new Promise((resolve) => setTimeout(resolve, 250));
			const retryToken = await this.cacheService.acquireLock(lockKey, LOCK_TTL_SECONDS);
			if (!retryToken) {
				throw new Error('Phone verification reuse marker is temporarily locked');
			}
			try {
				return await fn();
			} finally {
				await this.cacheService.releaseLock(lockKey, retryToken);
			}
		}
		try {
			return await fn();
		} finally {
			await this.cacheService.releaseLock(lockKey, token);
		}
	}
}
