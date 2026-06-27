// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const GuildScheduledEventStatus = {
	SCHEDULED: 0,
	ACTIVE: 1,
	COMPLETED: 2,
	CANCELLED: 3,
} as const;

export type GuildScheduledEventStatusValue = ValueOf<typeof GuildScheduledEventStatus>;

export const GuildScheduledEventEntityType = {
	STAGE_INSTANCE: 1,
	VOICE: 2,
	EXTERNAL: 3,
} as const;

export type GuildScheduledEventEntityTypeValue = ValueOf<typeof GuildScheduledEventEntityType>;

export const GuildScheduledEventPrivacyLevel = {
	GUILD_ONLY: 2,
} as const;

export type GuildScheduledEventPrivacyLevelValue = ValueOf<typeof GuildScheduledEventPrivacyLevel>;

export const GuildScheduledEventRecurrenceRuleFrequency = {
	YEARLY: 0,
	MONTHLY: 1,
	WEEKLY: 2,
	DAILY: 3,
	HOURLY: 4,
} as const;

export const GuildScheduledEventRecurrenceRuleInterval = 1;

export const GUILD_SCHEDULED_EVENT_NAME_MAX_LENGTH = 100;
export const GUILD_SCHEDULED_EVENT_DESCRIPTION_MAX_LENGTH = 1000;
