// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerAdminControllers} from '../admin/controllers/index';
import {AuthController} from '../auth/AuthController';
import {BlueskyOAuthController} from '../bluesky/BlueskyOAuthController';
import {Config} from '../Config';
import {ChannelController} from '../channel/ChannelController';
import type {APIConfig} from '../config/APIConfig';
import {ConnectionController} from '../connection/ConnectionController';
import {DonationController} from '../donation/DonationController';
import {DownloadController} from '../download/DownloadController';
import {FavoriteGifController} from '../favorite_gif/FavoriteGifController';
import {FavoriteMemeController} from '../favorite_meme/FavoriteMemeController';
import {GatewayController} from '../gateway/GatewayController';
import {GeolocationController} from '../geolocation/GeolocationController';
import {GifController} from '../gif/GifController';
import {GuildController} from '../guild/GuildController';
import {InstanceController} from '../instance/InstanceController';
import {InviteController} from '../invite/InviteController';
import {Logger} from '../Logger';
import {getInboundSmsChallengeServiceInstance, getUserRepositoryInstance} from '../middleware/ServiceMiddleware';
import {getGatewayService} from '../middleware/ServiceRegistry';
import {getCacheService} from '../middleware/ServiceSingletons';
import {OAuth2ApplicationsController} from '../oauth/OAuth2ApplicationsController';
import {OAuth2Controller} from '../oauth/OAuth2Controller';
import {OpenAPIController} from '../openapi/OpenAPIController';
import {registerPackControllers} from '../pack/controllers/index';
import {PremiumController} from '../premium/PremiumController';
import {ReadStateController} from '../read_state/ReadStateController';
import {ReportController} from '../report/ReportController';
import {installTwilioInboundSmsWebhook} from '../risk/TwilioInboundSmsWebhook';
import {InternalRpcController} from '../rpc/InternalRpcController';
import {SearchController} from '../search/controllers/SearchController';
import {StripeController} from '../stripe/StripeController';
import {TestHarnessController} from '../test/TestHarnessController';
import {ThemeController} from '../theme/ThemeController';
import type {HonoApp} from '../types/HonoEnv';
import {UnfurlController} from '../unfurl/UnfurlController';
import {UserController} from '../user/controllers/UserController';
import {WebhookController} from '../webhook/WebhookController';

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
	FavoriteGifController(routes);
	FavoriteMemeController(routes);
	InviteController(routes);
	registerPackControllers(routes);
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
	registerInboundSmsWebhook(routes);
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
