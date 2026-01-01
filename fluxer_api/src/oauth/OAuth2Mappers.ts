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

import type {User} from '~/Models';
import type {Application} from '~/models/Application';
import {mapUserToPartialResponse} from '~/user/UserMappers';
import type {ApplicationBotResponse, ApplicationResponse} from './OAuth2Types';

export const mapBotUserToResponse = (user: User, opts?: {token?: string}): ApplicationBotResponse => {
	const partial = mapUserToPartialResponse(user);
	const bannerHash = !user.isBot && !user.isPremium() ? null : user.bannerHash;
	return {
		id: partial.id,
		username: partial.username,
		discriminator: partial.discriminator,
		avatar: partial.avatar,
		banner: bannerHash,
		bio: user.bio ?? null,
		token: opts?.token,
		mfa_enabled: (user.authenticatorTypes?.size ?? 0) > 0,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : [],
	};
};

export const mapApplicationToResponse = (
	application: Application,
	options?: {
		botUser?: User | null;
		botToken?: string;
		clientSecret?: string | null;
	},
): ApplicationResponse => {
	const baseResponse: ApplicationResponse = {
		id: application.applicationId.toString(),
		name: application.name,
		redirect_uris: Array.from(application.oauth2RedirectUris),
		bot_public: application.botIsPublic,
		bot_require_code_grant: false,
	};

	if (options?.botUser) {
		baseResponse.bot = mapBotUserToResponse(options.botUser, {token: options.botToken});
	}

	if (options?.clientSecret) {
		return {
			...baseResponse,
			client_secret: options.clientSecret,
		};
	}

	return baseResponse;
};

export const mapBotTokenResetResponse = (user: User, token: string) => {
	return {
		token,
		bot: mapBotUserToResponse(user),
	};
};

export const mapBotProfileToResponse = (user: User) => {
	return {
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator.toString().padStart(4, '0'),
		avatar: user.avatarHash,
		banner: user.bannerHash,
		bio: user.bio,
	};
};
