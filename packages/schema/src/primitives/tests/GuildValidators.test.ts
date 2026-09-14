// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GuildExplicitContentFilterTypes,
	GuildMFALevel,
	GuildNSFWLevel,
	GuildSplashCardAlignment,
	GuildVerificationLevel,
	JoinSourceTypes,
} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {
	DefaultMessageNotificationsSchema,
	GuildExplicitContentFilterSchema,
	GuildMFALevelSchema,
	GuildVerificationLevelSchema,
	JoinSourceTypeSchema,
	NSFWLevelSchema,
	SplashCardAlignmentSchema,
} from '@fluxer/schema/src/primitives/GuildValidators';
import {describe, expect, it} from 'vitest';

describe.each([
	{name: 'verification level', schema: GuildVerificationLevelSchema, values: Object.values(GuildVerificationLevel)},
	{name: 'MFA level', schema: GuildMFALevelSchema, values: Object.values(GuildMFALevel)},
	{
		name: 'content filter',
		schema: GuildExplicitContentFilterSchema,
		values: Object.values(GuildExplicitContentFilterTypes),
	},
	{
		name: 'default notifications',
		schema: DefaultMessageNotificationsSchema,
		values: [MessageNotifications.ALL_MESSAGES, MessageNotifications.ONLY_MENTIONS],
	},
	{name: 'NSFW level', schema: NSFWLevelSchema, values: Object.values(GuildNSFWLevel)},
	{name: 'splash alignment', schema: SplashCardAlignmentSchema, values: Object.values(GuildSplashCardAlignment)},
	{name: 'join source', schema: JoinSourceTypeSchema, values: Object.values(JoinSourceTypes)},
])('$name', ({schema, values}) => {
	it.each(values)('accepts registered value %i', (value) => {
		expect(schema.parse(value)).toBe(value);
	});

	it.each([-1, 999, 0.5, '0', null])('rejects invalid value %j', (value) => {
		expect(schema.safeParse(value).success).toBe(false);
	});
});

it.each([1, 2])('rejects legacy NSFW level %i', (value) => {
	expect(NSFWLevelSchema.safeParse(value).success).toBe(false);
});

it.each([
	MessageNotifications.NULL,
	MessageNotifications.NO_MESSAGES,
	MessageNotifications.INHERIT,
])('rejects channel-only notification setting %i as a guild default', (value) => {
	expect(DefaultMessageNotificationsSchema.safeParse(value).success).toBe(false);
});
