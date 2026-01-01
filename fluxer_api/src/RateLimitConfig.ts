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

import type {RateLimitSection} from '~/rate_limit_configs/helpers';
import {mergeRateLimitSections} from '~/rate_limit_configs/helpers';
import {AdminRateLimitConfigs} from './rate_limit_configs/AdminRateLimitConfig';
import {AuthRateLimitConfigs} from './rate_limit_configs/AuthRateLimitConfig';
import {ChannelRateLimitConfigs} from './rate_limit_configs/ChannelRateLimitConfig';
import {GuildRateLimitConfigs} from './rate_limit_configs/GuildRateLimitConfig';
import {IntegrationRateLimitConfigs} from './rate_limit_configs/IntegrationRateLimitConfig';
import {InviteRateLimitConfigs} from './rate_limit_configs/InviteRateLimitConfig';
import {MiscRateLimitConfigs} from './rate_limit_configs/MiscRateLimitConfig';
import {OAuthRateLimitConfigs} from './rate_limit_configs/OAuthRateLimitConfig';
import {PackRateLimitConfigs} from './rate_limit_configs/PackRateLimitConfig';
import {UserRateLimitConfigs} from './rate_limit_configs/UserRateLimitConfig';
import {WebhookRateLimitConfigs} from './rate_limit_configs/WebhookRateLimitConfig';

const rateLimitSections = [
	AuthRateLimitConfigs,
	OAuthRateLimitConfigs,
	UserRateLimitConfigs,
	ChannelRateLimitConfigs,
	GuildRateLimitConfigs,
	InviteRateLimitConfigs,
	WebhookRateLimitConfigs,
	IntegrationRateLimitConfigs,
	AdminRateLimitConfigs,
	MiscRateLimitConfigs,
	PackRateLimitConfigs,
] satisfies ReadonlyArray<RateLimitSection>;

export const RateLimitConfigs = mergeRateLimitSections(...rateLimitSections);
