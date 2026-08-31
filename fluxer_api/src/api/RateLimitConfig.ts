// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRateLimitConfigs} from './rate_limit_configs/AdminRateLimitConfig';
import {AuthRateLimitConfigs} from './rate_limit_configs/AuthRateLimitConfig';
import {ChannelRateLimitConfigs} from './rate_limit_configs/ChannelRateLimitConfig';
import {DiscoveryRateLimitConfigs} from './rate_limit_configs/DiscoveryRateLimitConfig';
import {DonationRateLimitConfigs} from './rate_limit_configs/DonationRateLimitConfig';
import {GuildRateLimitConfigs} from './rate_limit_configs/GuildRateLimitConfig';
import {IntegrationRateLimitConfigs} from './rate_limit_configs/IntegrationRateLimitConfig';
import {InviteRateLimitConfigs} from './rate_limit_configs/InviteRateLimitConfig';
import {MiscRateLimitConfigs} from './rate_limit_configs/MiscRateLimitConfig';
import {OAuthRateLimitConfigs} from './rate_limit_configs/OAuthRateLimitConfig';
import type {RateLimitSection} from './rate_limit_configs/RateLimitHelpers';
import {mergeRateLimitSections} from './rate_limit_configs/RateLimitHelpers';
import {UserRateLimitConfigs} from './rate_limit_configs/UserRateLimitConfig';
import {WebhookRateLimitConfigs} from './rate_limit_configs/WebhookRateLimitConfig';

const rateLimitSections = [
	AuthRateLimitConfigs,
	OAuthRateLimitConfigs,
	UserRateLimitConfigs,
	ChannelRateLimitConfigs,
	DiscoveryRateLimitConfigs,
	DonationRateLimitConfigs,
	GuildRateLimitConfigs,
	InviteRateLimitConfigs,
	WebhookRateLimitConfigs,
	IntegrationRateLimitConfigs,
	AdminRateLimitConfigs,
	MiscRateLimitConfigs,
] satisfies ReadonlyArray<RateLimitSection>;
export const RateLimitConfigs = mergeRateLimitSections(...rateLimitSections);
