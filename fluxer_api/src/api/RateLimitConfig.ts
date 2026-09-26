// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRateLimitConfigs} from '@app/api/rate_limit_configs/AdminRateLimitConfig';
import {AuthRateLimitConfigs} from '@app/api/rate_limit_configs/AuthRateLimitConfig';
import {ChannelRateLimitConfigs} from '@app/api/rate_limit_configs/ChannelRateLimitConfig';
import {DiscoveryRateLimitConfigs} from '@app/api/rate_limit_configs/DiscoveryRateLimitConfig';
import {DonationRateLimitConfigs} from '@app/api/rate_limit_configs/DonationRateLimitConfig';
import {GuildRateLimitConfigs} from '@app/api/rate_limit_configs/GuildRateLimitConfig';
import {IntegrationRateLimitConfigs} from '@app/api/rate_limit_configs/IntegrationRateLimitConfig';
import {InviteRateLimitConfigs} from '@app/api/rate_limit_configs/InviteRateLimitConfig';
import {MiscRateLimitConfigs} from '@app/api/rate_limit_configs/MiscRateLimitConfig';
import {OAuthRateLimitConfigs} from '@app/api/rate_limit_configs/OAuthRateLimitConfig';
import type {RateLimitSection} from '@app/api/rate_limit_configs/RateLimitHelpers';
import {mergeRateLimitSections} from '@app/api/rate_limit_configs/RateLimitHelpers';
import {UserRateLimitConfigs} from '@app/api/rate_limit_configs/UserRateLimitConfig';
import {WebhookRateLimitConfigs} from '@app/api/rate_limit_configs/WebhookRateLimitConfig';

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
