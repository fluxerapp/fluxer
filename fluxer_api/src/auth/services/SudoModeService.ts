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

import {jwtVerify, SignJWT} from 'jose';
import type {UserID} from '~/BrandedTypes';
import {Config} from '~/Config';

const SUDO_TOKEN_EXPIRY_SECONDS = 5 * 60;

export class SudoModeService {
	private readonly secret: Uint8Array;

	constructor() {
		this.secret = new TextEncoder().encode(Config.auth.sudoModeSecret);
	}

	async generateSudoToken(userId: UserID): Promise<string> {
		const now = Math.floor(Date.now() / 1000);

		const jwt = await new SignJWT({
			type: 'sudo',
		})
			.setProtectedHeader({alg: 'HS256'})
			.setSubject(userId.toString())
			.setIssuedAt(now)
			.setExpirationTime(now + SUDO_TOKEN_EXPIRY_SECONDS)
			.sign(this.secret);

		return jwt;
	}

	async verifySudoToken(token: string, userId: UserID): Promise<boolean> {
		try {
			const {payload} = await jwtVerify(token, this.secret, {
				algorithms: ['HS256'],
			});

			if (payload.type !== 'sudo') {
				return false;
			}

			if (payload.sub !== userId.toString()) {
				return false;
			}

			return true;
		} catch {
			return false;
		}
	}
}

let sudoModeServiceInstance: SudoModeService | null = null;

export function getSudoModeService(): SudoModeService {
	if (!sudoModeServiceInstance) {
		sudoModeServiceInstance = new SudoModeService();
	}
	return sudoModeServiceInstance;
}
