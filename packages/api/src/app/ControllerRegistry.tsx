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

import {registerAdminControllers} from '@fluxer/api/src/admin/controllers';
import {AuthController} from '@fluxer/api/src/auth/AuthController';
import {BlueskyOAuthController} from '@fluxer/api/src/bluesky/BlueskyOAuthController';
import {ChannelController} from '@fluxer/api/src/channel/ChannelController';
import type {APIConfig} from '@fluxer/api/src/config/APIConfig';
import {ConnectionController} from '@fluxer/api/src/connection/ConnectionController';
import {DonationController} from '@fluxer/api/src/donation/DonationController';
import {DownloadController} from '@fluxer/api/src/download/DownloadController';
import {FavoriteMemeController} from '@fluxer/api/src/favorite_meme/FavoriteMemeController';
import {GatewayController} from '@fluxer/api/src/gateway/GatewayController';
import {GuildController} from '@fluxer/api/src/guild/GuildController';
import {InstanceController} from '@fluxer/api/src/instance/InstanceController';
import {InviteController} from '@fluxer/api/src/invite/InviteController';
import {KlipyController} from '@fluxer/api/src/klipy/KlipyController';
import {OAuth2ApplicationsController} from '@fluxer/api/src/oauth/OAuth2ApplicationsController';
import {OAuth2Controller} from '@fluxer/api/src/oauth/OAuth2Controller';
import {registerPackControllers} from '@fluxer/api/src/pack/controllers';
import {ReadStateController} from '@fluxer/api/src/read_state/ReadStateController';
import {ReportController} from '@fluxer/api/src/report/ReportController';
import {SearchController} from '@fluxer/api/src/search/controllers/SearchController';
import {StripeController} from '@fluxer/api/src/stripe/StripeController';
import {TenorController} from '@fluxer/api/src/tenor/TenorController';
import {TestHarnessController} from '@fluxer/api/src/test/TestHarnessController';
import {ThemeController} from '@fluxer/api/src/theme/ThemeController';
import type {HonoApp} from '@fluxer/api/src/types/HonoEnv';
import {UserController} from '@fluxer/api/src/user/controllers/UserController';
import {WebhookController} from '@fluxer/api/src/webhook/WebhookController';

export function registerControllers(routes: HonoApp, config: APIConfig): void {
	GatewayController(routes);
	registerAdminControllers(routes);
	AuthController(routes);
	ChannelController(routes);
	ConnectionController(routes);
	BlueskyOAuthController(routes);
	InstanceController(routes);
	DownloadController(routes);
	FavoriteMemeController(routes);
	InviteController(routes);
	registerPackControllers(routes);
	ReadStateController(routes);
	ReportController(routes);
	GuildController(routes);
	SearchController(routes);
	KlipyController(routes);
	TenorController(routes);
	ThemeController(routes);

	if (config.dev.testModeEnabled || config.nodeEnv === 'development') {
		TestHarnessController(routes);
	}

	UserController(routes);
	WebhookController(routes);
	OAuth2Controller(routes);
	OAuth2ApplicationsController(routes);

	if (!config.instance.selfHosted) {
		DonationController(routes);
		StripeController(routes);
	}
}
