// SPDX-License-Identifier: AGPL-3.0-or-later

import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {handleAccountLimitedError} from '@app/features/user/utils/AccountLimitUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {TemplateSerializedGuild} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {msg} from '@lingui/core/macro';
import React from 'react';

export type AddGuildModalView = 'landing' | 'create_guild' | 'join_guild' | 'import_template';

export const ADD_GUILD_VIEW_ORDER: ReadonlyArray<AddGuildModalView> = [
	'landing',
	'create_guild',
	'join_guild',
	'import_template',
];

export interface ModalFooterContextValue {
	setFooterContent: (content: React.ReactNode) => void;
	onBack: () => void;
}

export const ModalFooterContext = React.createContext<ModalFooterContextValue | null>(null);

export interface GuildCreateFormInputs {
	icon?: string | null;
	name: string;
}

export interface GuildJoinFormInputs {
	code: string;
}

export interface TemplateImportFormInputs {
	url: string;
}

export interface TemplateJsonFormInputs {
	json: string;
}

export interface TemplateCreateFormInputs {
	icon?: string | null;
	name: string;
}

const THE_OTHER_PLATFORM_GUILD_STAGE_VOICE_CHANNEL_TYPE = 13;

export function mapTemplateChannelTypeToFluxer(channelType: number): number | null {
	if (
		channelType === ChannelTypes.GUILD_TEXT ||
		channelType === ChannelTypes.GUILD_ANNOUNCEMENT ||
		channelType === ChannelTypes.GUILD_VOICE ||
		channelType === ChannelTypes.GUILD_CATEGORY
	) {
		return channelType;
	}
	if (channelType === THE_OTHER_PLATFORM_GUILD_STAGE_VOICE_CHANNEL_TYPE) {
		return ChannelTypes.GUILD_VOICE;
	}
	return null;
}

export function isTemplateEveryoneRole(role: TemplateSerializedGuild['roles'][number]): boolean {
	return role.name === '@everyone' || String(role.id) === '0';
}

export function parseTemplateCode(input: string): string | null {
	const trimmed = input.trim();
	const newMatch = /discord\.new\/([A-Za-z0-9]+)/.exec(trimmed);
	if (newMatch) return newMatch[1];
	const apiMatch = /discord\.com\/api\/guilds\/templates\/([A-Za-z0-9]+)/.exec(trimmed);
	if (apiMatch) return apiMatch[1];
	const templateMatch = /discord\.com\/template\/([A-Za-z0-9]+)/.exec(trimmed);
	if (templateMatch) return templateMatch[1];
	if (/^[A-Za-z0-9]+$/.test(trimmed) && trimmed.length > 0) return trimmed;
	return null;
}

export function handleGuildCreationError(error: unknown): never {
	if (handleAccountLimitedError(error)) {
		throw new DOMException('Guild create skipped', 'AbortError');
	}
	if (failureCode(error) === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS) {
		openClaimAccountModal({force: true});
	}
	throw error;
}

export const ADD_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Add a community',
	comment:
		'Landing-view title of the add community modal. Top-level entry point with options to create, join, or import.',
});
export const CREATE_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Create a community',
	comment: 'Action button label in the add community modal. Starts the create-community flow.',
});
export const JOIN_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Join a community',
	comment: 'Action button label in the add community modal. Starts the join-by-invite-code flow.',
});
export const IMPORT_THE_OTHER_PLATFORM_TEMPLATE_DESCRIPTOR = msg({
	message: 'Import {theOtherPlatform} template',
	comment:
		'Action button label in the add community modal. Imports a template from the third-party platform named by {theOtherPlatform}.',
});
export const ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Icon file is too large. Choose a file smaller than {imageMaxSizeLabel}.',
	comment:
		'Validation error in the create-community form when the chosen icon exceeds the size limit. {imageMaxSizeLabel} is a formatted file size such as "8 MB".',
});
export const ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR = msg({
	message: 'Animated icons are not supported when creating a new community. Use a static image.',
	comment: 'Validation error in the create-community form when the user picks an animated image.',
});
export const CREATE_COMMUNITY_FORM_DESCRIPTOR = msg({
	message: 'Create community form',
	comment: 'Accessible label for the create-community form element. Used by assistive tech only.',
});
export const COMMUNITY_NAME_DESCRIPTOR = msg({
	message: 'Community name',
	comment: 'Label of the community name input in the create-community form.',
});
export const PLEASE_ENTER_A_VALID_THE_OTHER_PLATFORM_TEMPLATE_URL_OR_DESCRIPTOR = msg({
	message: 'Enter a valid {theOtherPlatform} template URL or code.',
	comment:
		'Validation error in the import-template form when the entered URL or code is malformed. {theOtherPlatform} is a third-party product name placeholder.',
});
export const INVALID_JSON_PLEASE_PASTE_THE_FULL_RESPONSE_FROM_DESCRIPTOR = msg({
	message: 'Invalid JSON. Paste the full response from the URL above.',
	comment: 'Validation error in the paste-template-JSON form. "JSON" is a protocol token and must not be translated.',
});
export const THIS_DOESN_T_LOOK_LIKE_A_VALID_TEMPLATE_DESCRIPTOR = msg({
	message: "This doesn't look like a valid template response. Make sure you copied the entire JSON document.",
	comment:
		'Validation error in the paste-template-JSON form when the JSON parses but lacks the expected third-party template fields.',
});
export const IMPORT_TEMPLATE_FORM_DESCRIPTOR = msg({
	message: 'Import template form',
	comment: 'Accessible label for the import-template form element. Used by assistive tech only.',
});
export const TEMPLATE_URL_DESCRIPTOR = msg({
	message: 'Template URL',
	comment: 'Label of the URL input in the import-template form.',
});
export const PASTE_TEMPLATE_JSON_FORM_DESCRIPTOR = msg({
	message: 'Paste template JSON form',
	comment: 'Accessible label for the paste-template-JSON form element. Used by assistive tech only.',
});
export const TEMPLATE_JSON_DESCRIPTOR = msg({
	message: 'Template JSON',
	comment: 'Label of the textarea in the paste-template-JSON form. "JSON" must not be translated.',
});
export const PASTE_THE_JSON_RESPONSE_HERE_DESCRIPTOR = msg({
	message: 'Paste the JSON response here',
	comment: 'Placeholder in the textarea where the user pastes the third-party template JSON document.',
});
export const CREATE_FROM_TEMPLATE_FORM_DESCRIPTOR = msg({
	message: 'Create from template form',
	comment: 'Accessible label for the create-from-template form element. Used by assistive tech only.',
});
export const JOIN_COMMUNITY_FORM_DESCRIPTOR = msg({
	message: 'Join community form',
	comment: 'Accessible label for the join-community form element. Used by assistive tech only.',
});
export const INVITE_LINK_DESCRIPTOR = msg({
	message: 'Invite link',
	comment: 'Label of the invite code/URL input in the join-community form.',
});
