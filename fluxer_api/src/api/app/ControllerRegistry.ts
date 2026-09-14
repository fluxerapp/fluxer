// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerAdminControllers} from '@app/api/admin/controllers/index';
import {AuthController} from '@app/api/auth/AuthController';
import {BlueskyOAuthController} from '@app/api/bluesky/BlueskyOAuthController';
import {Config} from '@app/api/Config';
import {ChannelController} from '@app/api/channel/ChannelController';
import type {APIConfig} from '@app/api/config/APIConfig';
import {ConnectionController} from '@app/api/connection/ConnectionController';
import {DonationController} from '@app/api/donation/DonationController';
import {DownloadController} from '@app/api/download/DownloadController';
import {ExperimentController} from '@app/api/experiment/ExperimentController';
import {FavoriteGifController} from '@app/api/favorite_gif/FavoriteGifController';
import {FavoriteMemeController} from '@app/api/favorite_meme/FavoriteMemeController';
import {GatewayController} from '@app/api/gateway/GatewayController';
import {GeolocationController} from '@app/api/geolocation/GeolocationController';
import {GifController} from '@app/api/gif/GifController';
import {GuildController} from '@app/api/guild/GuildController';
import {InstanceController} from '@app/api/instance/InstanceController';
import {InviteController} from '@app/api/invite/InviteController';
import {Logger} from '@app/api/Logger';
import {getInboundSmsChallengeServiceInstance, getUserRepositoryInstance} from '@app/api/middleware/ServiceMiddleware';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import {OAuth2ApplicationsController} from '@app/api/oauth/OAuth2ApplicationsController';
import {OAuth2Controller} from '@app/api/oauth/OAuth2Controller';
import {OpenAPIController} from '@app/api/openapi/OpenAPIController';
import {PremiumController} from '@app/api/premium/PremiumController';
import {ReadStateController} from '@app/api/read_state/ReadStateController';
import {ReportController} from '@app/api/report/ReportController';
import {installTwilioInboundSmsWebhook} from '@app/api/risk/TwilioInboundSmsWebhook';
import {InternalRpcController} from '@app/api/rpc/InternalRpcController';
import {SearchController} from '@app/api/search/controllers/SearchController';
import {StripeController} from '@app/api/stripe/StripeController';
import {TestHarnessController} from '@app/api/test/TestHarnessController';
import {ThemeController} from '@app/api/theme/ThemeController';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {UnfurlController} from '@app/api/unfurl/UnfurlController';
import {UserController} from '@app/api/user/controllers/UserController';
import {WebhookController} from '@app/api/webhook/WebhookController';

export function registerControllers(routes: HonoApp, config: APIConfig): void {
	InternalRpcController(routes);
	GatewayController(routes);
	GeolocationController(routes);
	registerAdminControllers(routes);
	AuthController(routes);
	ChannelController(routes);
	ConnectionController(routes);
	BlueskyOAuthController(routes);
	InstanceController(routes);
	OpenAPIController(routes);
	DownloadController(routes);
	ExperimentController(routes);
	FavoriteGifController(routes);
	FavoriteMemeController(routes);
	InviteController(routes);
	ReadStateController(routes);
	ReportController(routes);
	GuildController(routes);
	SearchController(routes);
	GifController(routes);
	ThemeController(routes);
	UnfurlController(routes);
	if (config.dev.testModeEnabled || config.nodeEnv === 'development') {
		TestHarnessController(routes);
	}
	UserController(routes);
	if (config.sms.enabled) {
		registerInboundSmsWebhook(routes);
	}
	WebhookController(routes);
	OAuth2Controller(routes);
	OAuth2ApplicationsController(routes);
	PremiumController(routes);
	if (!config.instance.selfHosted) {
		DonationController(routes);
		StripeController(routes);
	}
}

function registerInboundSmsWebhook(routes: HonoApp): void {
	const authToken = Config.sms.inboundWebhookAuthToken;
	const publicWebhookUrl = Config.sms.inboundWebhookPublicUrl;
	if (!authToken || !publicWebhookUrl) {
		Logger.warn(
			{},
			'Twilio inbound SMS webhook not configured (need integrations.sms.inbound_webhook_auth_token + integrations.sms.inbound_webhook_public_url); skipping installation',
		);
		return;
	}
	installTwilioInboundSmsWebhook(routes, {
		authToken,
		publicWebhookUrl,
		inboundSmsChallengeService: getInboundSmsChallengeServiceInstance(),
		userRepository: getUserRepositoryInstance(),
		gatewayService: getGatewayService(),
		cacheService: getCacheService(),
	});
	Logger.info({publicWebhookUrl}, 'Twilio inbound SMS webhook installed');
}
