// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import {generateOAuthTokenSecret} from '@app/api/oauth/OAuthTokenSecret';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {canOwnerRunBots} from '@app/api/user/UserHelpers';
import {hashPassword, verifyPassword} from '@app/api/utils/PasswordUtils';

export class BotAuthService {
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
		if (!application?.hasBotUser() || !application.botTokenHash) {
			return null;
		}
		const [isValid, owner] = await Promise.all([
			verifyPassword({password: secret, passwordHash: application.botTokenHash}).catch(() => false),
			this.users.findUnique(application.ownerUserId),
		]);
		if (!isValid || (owner && !canOwnerRunBots(owner))) return null;
		return application.getBotUserId();
	}

	async generateBotToken(applicationId: ApplicationID): Promise<{
		token: string;
		hash: string;
		preview: string;
	}> {
		const secret = generateOAuthTokenSecret();
		const hash = await hashPassword(secret);
		const preview = secret.slice(0, 8);
		const token = `${applicationId.toString()}.${secret}`;
		return {token, hash, preview};
	}
}
