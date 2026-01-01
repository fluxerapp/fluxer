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

import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export interface ViolationCategoryDescriptor {
	value: string;
	name: MessageDescriptor;
	desc: MessageDescriptor;
}

const MESSAGE_VIOLATION_CATEGORIES_DESCRIPTORS: Array<ViolationCategoryDescriptor> = [
	{
		value: 'harassment',
		name: msg`Harassment or Bullying`,
		desc: msg`Targeted attacks, threats, or sustained harassment`,
	},
	{
		value: 'hate_speech',
		name: msg`Hate Speech`,
		desc: msg`Content promoting hatred based on protected characteristics`,
	},
	{
		value: 'violent_content',
		name: msg`Violent or Graphic Content`,
		desc: msg`Glorification of violence or disturbing imagery`,
	},
	{
		value: 'spam',
		name: msg`Spam or Scam`,
		desc: msg`Unsolicited advertising, phishing, or fraudulent content`,
	},
	{
		value: 'nsfw_violation',
		name: msg`NSFW Policy Violation`,
		desc: msg`Sexual content in non-NSFW spaces or without consent`,
	},
	{
		value: 'illegal_activity',
		name: msg`Illegal Activity`,
		desc: msg`Discussion or promotion of illegal activities`,
	},
	{
		value: 'doxxing',
		name: msg`Sharing Personal Information`,
		desc: msg`Sharing private information without consent`,
	},
	{
		value: 'self_harm',
		name: msg`Self-Harm or Suicide`,
		desc: msg`Content promoting self-harm or suicide`,
	},
	{
		value: 'child_safety',
		name: msg`Child Safety Concerns`,
		desc: msg`Content sexualizing or endangering minors`,
	},
	{
		value: 'malicious_links',
		name: msg`Malicious Links`,
		desc: msg`Phishing, malware, or dangerous links`,
	},
	{
		value: 'impersonation',
		name: msg`Impersonation`,
		desc: msg`Pretending to be someone else or a company/organization`,
	},
	{
		value: 'other',
		name: msg`Other`,
		desc: msg`Something else that violates Terms of Service or Community Guidelines`,
	},
];

export interface ViolationCategory {
	value: string;
	name: string;
	desc: string;
}

export function getMessageViolationCategories(i18n: I18n): Array<ViolationCategory> {
	return MESSAGE_VIOLATION_CATEGORIES_DESCRIPTORS.map((category) => ({
		value: category.value,
		name: i18n._(category.name),
		desc: i18n._(category.desc),
	}));
}

const USER_VIOLATION_CATEGORIES_DESCRIPTORS: Array<ViolationCategoryDescriptor> = [
	{
		value: 'harassment',
		name: msg`Harassment or Bullying`,
		desc: msg`Targeted attacks, threats, or sustained harassment`,
	},
	{
		value: 'hate_speech',
		name: msg`Hate Speech`,
		desc: msg`Content promoting hatred based on protected characteristics`,
	},
	{
		value: 'spam_account',
		name: msg`Spam Account`,
		desc: msg`Account appears to exist solely for spamming`,
	},
	{
		value: 'impersonation',
		name: msg`Impersonation`,
		desc: msg`User is impersonating someone else`,
	},
	{
		value: 'underage_user',
		name: msg`Underage User`,
		desc: msg`User appears to be under 13 years old`,
	},
	{
		value: 'inappropriate_profile',
		name: msg`Inappropriate Profile`,
		desc: msg`Profile contains offensive or NSFW content`,
	},
	{
		value: 'other',
		name: msg`Other`,
		desc: msg`Something else that violates Terms of Service or Community Guidelines`,
	},
];

export function getUserViolationCategories(i18n: I18n): Array<ViolationCategory> {
	return USER_VIOLATION_CATEGORIES_DESCRIPTORS.map((category) => ({
		value: category.value,
		name: i18n._(category.name),
		desc: i18n._(category.desc),
	}));
}

const GUILD_VIOLATION_CATEGORIES_DESCRIPTORS: Array<ViolationCategoryDescriptor> = [
	{
		value: 'harassment',
		name: msg`Harassment`,
		desc: msg`Community engages in or enables targeted harassment`,
	},
	{
		value: 'hate_speech',
		name: msg`Hate Speech`,
		desc: msg`Community promotes hatred based on protected characteristics`,
	},
	{
		value: 'extremist_community',
		name: msg`Extremist Community`,
		desc: msg`Community promotes extremist ideologies`,
	},
	{
		value: 'illegal_activity',
		name: msg`Illegal Activity`,
		desc: msg`Community promotes or facilitates illegal activities`,
	},
	{
		value: 'child_safety',
		name: msg`Child Safety Concerns`,
		desc: msg`Content sexualizing or endangering minors`,
	},
	{
		value: 'raid_coordination',
		name: msg`Raid Coordination`,
		desc: msg`Community organizes raids or harassment campaigns`,
	},
	{
		value: 'spam',
		name: msg`Spam or Scam Community`,
		desc: msg`Community exists solely for spam or fraudulent purposes`,
	},
	{
		value: 'malware_distribution',
		name: msg`Malware Distribution`,
		desc: msg`Community distributes malicious files or links`,
	},
	{
		value: 'other',
		name: msg`Other`,
		desc: msg`Something else that violates Terms of Service or Community Guidelines`,
	},
];

export function getGuildViolationCategories(i18n: I18n): Array<ViolationCategory> {
	return GUILD_VIOLATION_CATEGORIES_DESCRIPTORS.map((category) => ({
		value: category.value,
		name: i18n._(category.name),
		desc: i18n._(category.desc),
	}));
}
