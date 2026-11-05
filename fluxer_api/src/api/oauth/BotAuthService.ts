// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, randomBytes} from 'node:crypto';
import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {canOwnerRunBots} from '@app/api/user/UserHelpers';
import {hashPassword, verifyPassword} from '@app/api/utils/PasswordUtils';
import {randomBase64Url} from '@app/api/utils/SecretHash';

const VERIFIED_TOKEN_CACHE_LIMIT = 10_000;
const VERIFIED_TOKEN_CACHE_TTL_MS = 10 * 60 * 1000;

interface VerifiedTokenEntry {
	readonly valid: boolean;
	readonly expiresAt: number;
}

export class BotAuthService {
	private readonly verifiedTokenKey = randomBytes(32);
	private readonly verifiedTokens = new Map<string, VerifiedTokenEntry>();

	constructor(
		private readonly applicationRepository: ApplicationRepository,
		private readonly users: Pick<UserRepository, 'findUnique'>,
	) {}

	private parseBotToken(token: string): {
		applicationId: ApplicationID;
		secret: string;
	} | null {
		const parts = token.split('.');
		if (parts.length !== 2) {
			return null;
		}
		const [applicationIdStr, secret] = parts;
		if (!applicationIdStr || !secret) {
			return null;
		}
		try {
			const applicationId = BigInt(applicationIdStr) as ApplicationID;
			return {applicationId, secret};
		} catch {
			return null;
		}
	}

	async validateBotToken(token: string): Promise<UserID | null> {
		const parsed = this.parseBotToken(token);
		if (!parsed) {
			return null;
		}
		const {applicationId, secret} = parsed;
		const application = await this.applicationRepository.getApplication(applicationId);
		if (!application || application.botUserId === null || !application.botTokenHash) {
			return null;
		}
		const [isValid, owner] = await Promise.all([
			this.verifyBotSecret(token, secret, application.botTokenHash),
			this.users.findUnique(application.ownerUserId),
		]);
		if (!isValid || (owner && !canOwnerRunBots(owner))) return null;
		return application.botUserId;
	}

	private async verifyBotSecret(token: string, secret: string, botTokenHash: string): Promise<boolean> {
		const key = createHmac('sha256', this.verifiedTokenKey)
			.update(token)
			.update('\0')
			.update(botTokenHash)
			.digest('base64url');
		const now = Date.now();
		const cached = this.verifiedTokens.get(key);
		if (cached && cached.expiresAt > now) {
			this.verifiedTokens.delete(key);
			this.verifiedTokens.set(key, cached);
			return cached.valid;
		}
		const valid = await verifyPassword({password: secret, passwordHash: botTokenHash}).catch(() => false);
		this.verifiedTokens.delete(key);
		this.verifiedTokens.set(key, {valid, expiresAt: now + VERIFIED_TOKEN_CACHE_TTL_MS});
		while (this.verifiedTokens.size > VERIFIED_TOKEN_CACHE_LIMIT) {
			const oldest = this.verifiedTokens.keys().next().value;
			if (oldest === undefined) break;
			this.verifiedTokens.delete(oldest);
		}
		return valid;
	}

	async generateBotToken(applicationId: ApplicationID): Promise<{
		token: string;
		hash: string;
		preview: string;
	}> {
		const secret = randomBase64Url(32);
		const hash = await hashPassword(secret);
		const preview = secret.slice(0, 8);
		const token = `${applicationId.toString()}.${secret}`;
		return {token, hash, preview};
	}
}
