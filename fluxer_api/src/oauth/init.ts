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

import argon2 from 'argon2';
import {createApplicationID, createUserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import type {ApplicationRow} from '~/database/types/OAuth2Types';
import {Logger} from '~/Logger';
import {ApplicationRepository} from '~/oauth/repositories/ApplicationRepository';
import {z} from '~/Schema';

const OAuth2ConfigSchema = z.object({
	clientIdStr: z.string().min(1),
	clientSecret: z.string().min(1),
	redirectUri: z.string().url(),
});

type OAuth2Config = z.infer<typeof OAuth2ConfigSchema>;

function getAdminOAuth2Config(): OAuth2Config | null {
	Logger.info(
		{
			ADMIN_OAUTH2_AUTO_CREATE: Config.adminOauth2.autoCreate,
			CLIENT_ID_SET: !!Config.adminOauth2.clientId,
			CLIENT_SECRET_SET: !!Config.adminOauth2.clientSecret,
		},
		'getAdminOAuth2Config check',
	);

	if (!Config.adminOauth2.autoCreate) {
		return null;
	}

	const clientIdStr = Config.adminOauth2.clientId;
	const clientSecret = Config.adminOauth2.clientSecret;

	if (!clientIdStr || !clientSecret) {
		Logger.info(
			'Skipping admin OAuth2 client auto-create; set ADMIN_OAUTH2_CLIENT_ID and ADMIN_OAUTH2_CLIENT_SECRET to enable.',
		);
		return null;
	}

	const redirectUri = Config.adminOauth2.redirectUri ?? 'http://127.0.0.1:8001/oauth2_callback';

	const parseResult = OAuth2ConfigSchema.safeParse({clientIdStr, clientSecret, redirectUri});
	if (!parseResult.success) {
		Logger.error({errors: parseResult.error.issues}, 'Invalid admin OAuth2 configuration');
		return null;
	}

	return parseResult.data;
}

async function upsertAdminOAuth2Client(repo: ApplicationRepository, config: OAuth2Config): Promise<void> {
	Logger.info({clientId: config.clientIdStr, redirectUri: config.redirectUri}, 'Upserting admin OAuth2 client...');

	const applicationId = createApplicationID(BigInt(config.clientIdStr));
	const existing = await repo.getApplication(applicationId);
	Logger.info({existing: !!existing}, 'Checked for existing admin application');

	const ownerUserId = createUserID(-1n);
	const now = new Date();
	const secretHash = await argon2.hash(config.clientSecret);

	if (existing) {
		const base = existing.toRow();

		const row: ApplicationRow = {
			...base,
			client_secret_hash: secretHash,
			client_secret_created_at: now,
			oauth2_redirect_uris: new Set<string>([config.redirectUri]),
			bot_is_public: base.bot_is_public ?? false,
		};

		await repo.upsertApplication(row);
		Logger.info(
			{application_id: applicationId.toString(), redirect_uris: [config.redirectUri]},
			'Updated admin OAuth2 application',
		);
		return;
	}

	const row: ApplicationRow = {
		application_id: applicationId,
		owner_user_id: ownerUserId,
		name: 'Fluxer Admin',
		bot_user_id: null,
		bot_is_public: false,
		oauth2_redirect_uris: new Set<string>([config.redirectUri]),
		client_secret_hash: secretHash,
		client_secret_created_at: now,
		bot_token_hash: null,
		bot_token_preview: null,
		bot_token_created_at: null,
	};

	await repo.upsertApplication(row);
	Logger.info({application_id: applicationId.toString()}, 'Created admin OAuth2 application');
}

export async function initializeOAuth(): Promise<void> {
	try {
		Logger.info('Initializing OAuth applications...');
		const repo = new ApplicationRepository();

		const adminCfg = getAdminOAuth2Config();
		Logger.info({adminCfg: !!adminCfg}, 'Admin OAuth2 config loaded');
		if (adminCfg) {
			await upsertAdminOAuth2Client(repo, adminCfg);
		}

		Logger.info('OAuth application initialization complete');
	} catch (err) {
		Logger.error(err, 'Failed to auto-create OAuth2 application(s)');
	}
}
