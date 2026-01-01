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

import {randomBytes} from 'node:crypto';
import argon2 from 'argon2';
import type {ApplicationID, UserID} from '~/BrandedTypes';
import type {IApplicationRepository} from './repositories/IApplicationRepository';

export class BotAuthService {
	constructor(private readonly applicationRepository: IApplicationRepository) {}

	private parseBotToken(token: string): {applicationId: ApplicationID; secret: string} | null {
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

		if (!application || !application.hasBotUser() || !application.botTokenHash) {
			return null;
		}

		try {
			const isValid = await argon2.verify(application.botTokenHash, secret);
			return isValid ? application.getBotUserId() : null;
		} catch {
			return null;
		}
	}

	async generateBotToken(applicationId: ApplicationID): Promise<{
		token: string;
		hash: string;
		preview: string;
	}> {
		const secret = randomBytes(32).toString('base64url');
		const hash = await argon2.hash(secret);
		const preview = secret.slice(0, 8);
		const token = `${applicationId.toString()}.${secret}`;

		return {token, hash, preview};
	}
}
