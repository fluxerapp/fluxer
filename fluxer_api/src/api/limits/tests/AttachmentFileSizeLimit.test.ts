// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveAttachmentMaxFileSize} from '@app/api/limits/AttachmentFileSizeLimit';
import type {User} from '@app/api/models/User';
import {
	ATTACHMENT_MAX_SIZE_BOT,
	ATTACHMENT_MAX_SIZE_NON_PREMIUM,
	ATTACHMENT_MAX_SIZE_PREMIUM,
} from '@fluxer/constants/src/LimitConstants';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, it} from 'vitest';

const MIB = 1024 * 1024;

function account(isBot: boolean, traits: Array<string> = []): User {
	return {
		isBot,
		traits: new Set(traits),
		premiumType: null,
		premiumUntil: null,
		premiumFlags: 0,
		premiumGiftExtensionEndsAt: null,
		premiumGraceEndsAt: null,
	} as unknown as User;
}

function snapshot(extraRules: Array<LimitRule> = []): LimitConfigSnapshot {
	return {
		traitDefinitions: ['premium', 'uploader'],
		rules: [
			{id: 'default', limits: {...DEFAULT_RESTRICTED_LIMITS}},
			{id: 'premium', filters: {traits: ['premium']}, limits: {...DEFAULT_STOCK_LIMITS}},
			...extraRules,
		],
	};
}

describe('resolveAttachmentMaxFileSize', () => {
	it('gives bots the premium rule capped at the bot default', () => {
		expect(resolveAttachmentMaxFileSize(snapshot(), {user: account(true)})).toBe(ATTACHMENT_MAX_SIZE_BOT);
		expect(resolveAttachmentMaxFileSize(snapshot(), {user: account(false)})).toBe(ATTACHMENT_MAX_SIZE_NON_PREMIUM);
	});

	it('gives a bot the lower of the two keys', () => {
		const raised = snapshot([
			{id: 'uploader', filters: {traits: ['uploader']}, limits: {max_bot_attachment_file_size: 2000 * MIB}},
		]);

		expect(resolveAttachmentMaxFileSize(raised, {user: account(true, ['uploader'])})).toBe(ATTACHMENT_MAX_SIZE_PREMIUM);
		expect(resolveAttachmentMaxFileSize(raised, {user: account(true)})).toBe(ATTACHMENT_MAX_SIZE_BOT);
	});

	it('applies guild feature rules to the bot key', () => {
		const guildRule = snapshot([
			{id: 'big', filters: {guildFeatures: ['BIG_UPLOADS']}, limits: {max_bot_attachment_file_size: 200 * MIB}},
		]);

		expect(resolveAttachmentMaxFileSize(guildRule, {user: account(true), guildFeatures: ['BIG_UPLOADS']})).toBe(
			200 * MIB,
		);
		expect(resolveAttachmentMaxFileSize(guildRule, {user: account(true), guildFeatures: []})).toBe(
			ATTACHMENT_MAX_SIZE_BOT,
		);
	});

	it('falls back to the constants without a snapshot', () => {
		expect(resolveAttachmentMaxFileSize(null, {user: account(true)})).toBe(ATTACHMENT_MAX_SIZE_NON_PREMIUM);
	});
});
