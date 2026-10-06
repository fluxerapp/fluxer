// SPDX-License-Identifier: AGPL-3.0-or-later

import {STICKERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getPermissionTitleDescriptor} from '@app/features/permissions/utils/PermissionLabelDescriptors';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	EMOJI_DESCRIPTOR,
	MENTIONS_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';

const REACTIONS_DESCRIPTOR = msg({
	message: 'Reactions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REACT_DESCRIPTOR = msg({
	message: 'React',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MEDIA_DISPLAY_DESCRIPTOR = msg({
	message: 'Media display',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INLINE_MEDIA_DESCRIPTOR = msg({
	message: 'Inline media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMBEDDED_MEDIA_DESCRIPTOR = msg({
	message: 'Embedded media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const UPLOADED_MEDIA_DESCRIPTOR = msg({
	message: 'Uploaded media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ATTACHMENTS_DESCRIPTOR = msg({
	message: 'Attachments',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINK_PREVIEWS_DESCRIPTOR = msg({
	message: 'Link previews',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WEBSITE_PREVIEWS_DESCRIPTOR = msg({
	message: 'Website previews',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHOW_EMBEDS_DESCRIPTOR = msg({
	message: 'Show embeds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPOILERS_DESCRIPTOR = msg({
	message: 'Spoilers',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPOILER_DESCRIPTOR = msg({
	message: 'Spoiler',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_DESCRIPTOR = msg({
	message: 'Hide',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REVEAL_DESCRIPTOR = msg({
	message: 'Reveal',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MODERATOR_DESCRIPTOR = msg({
	message: 'Moderator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MOD_DESCRIPTOR = msg({
	message: 'Mod',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHANNELS_I_MODERATE_DESCRIPTOR = msg({
	message: 'Channels I moderate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMBEDS_DESCRIPTOR = msg({
	message: 'Embeds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PREVIEWS_DESCRIPTOR = msg({
	message: 'Previews',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINKS_DESCRIPTOR = msg({
	message: 'Links',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const URL_PREVIEW_DESCRIPTOR = msg({
	message: 'URL preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINK_PREVIEW_DESCRIPTOR = msg({
	message: 'Link preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MEDIA_2_DESCRIPTOR = msg({
	message: 'Media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMAGES_DESCRIPTOR = msg({
	message: 'Images',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIDEOS_DESCRIPTOR = msg({
	message: 'Videos',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INLINE_DESCRIPTOR = msg({
	message: 'Inline',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MEDIA_SIZE_DESCRIPTOR = msg({
	message: 'Media size',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SUGGESTIONS_DESCRIPTOR = msg({
	message: 'Suggestions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMOJI_PICKER_DESCRIPTOR = msg({
	message: 'Emoji picker',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STICKER_DESCRIPTOR = msg({
	message: 'Sticker',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPRESSIONS_DESCRIPTOR = msg({
	message: 'Expressions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COLON_DESCRIPTOR = msg({
	message: 'Colon',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMOTICONS_DESCRIPTOR = msg({
	message: 'Emoticons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SMILEYS_DESCRIPTOR = msg({
	message: 'Smileys',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEXT_REPLACEMENT_DESCRIPTOR = msg({
	message: 'Text replacement',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COLON_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Colon autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPRESSION_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Expression autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SAVED_MEDIA_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Saved media autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MEDIA_BUTTON_DESCRIPTOR = msg({
	message: 'Media button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEND_BUTTON_DESCRIPTOR = msg({
	message: 'Send button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCROLL_DESCRIPTOR = msg({
	message: 'Scroll',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCROLL_TO_BOTTOM_DESCRIPTOR = msg({
	message: 'Scroll to bottom',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SENT_MESSAGES_DESCRIPTOR = msg({
	message: 'Sent messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMPOSER_BUTTON_DESCRIPTOR = msg({
	message: 'Composer button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMPOSER_BUTTONS_DESCRIPTOR = msg({
	message: 'Composer buttons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_INPUT_BUTTONS_DESCRIPTOR = msg({
	message: 'Message input buttons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOOLBAR_DESCRIPTOR = msg({
	message: 'Toolbar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_DESCRIPTOR = msg({
	message: 'Display',
	context: 'chat-settings-section',
	comment: 'Chat settings section for message display preferences.',
});
const MEDIA_DESCRIPTOR = msg({
	message: 'Media',
	context: 'chat-settings-section',
	comment: 'Chat settings section for image, video, embed, and media preferences.',
});
const INPUT_DESCRIPTOR = msg({
	message: 'Input',
	context: 'chat-settings-section',
	comment: 'Chat settings section for message composer/input behavior.',
});
const MANAGE_MESSAGES_PERMISSION_DESCRIPTOR = getPermissionTitleDescriptor(Permissions.MANAGE_MESSAGES)!;
export const chatSettingsSections = [
	{
		id: 'display',
		tabType: 'chat_settings',
		label: DISPLAY_DESCRIPTOR,
		keywords: [
			REACTIONS_DESCRIPTOR,
			EMOJI_DESCRIPTOR,
			REACT_DESCRIPTOR,
			MEDIA_DISPLAY_DESCRIPTOR,
			INLINE_MEDIA_DESCRIPTOR,
			EMBEDDED_MEDIA_DESCRIPTOR,
			UPLOADED_MEDIA_DESCRIPTOR,
			ATTACHMENTS_DESCRIPTOR,
			LINK_PREVIEWS_DESCRIPTOR,
			WEBSITE_PREVIEWS_DESCRIPTOR,
			SHOW_EMBEDS_DESCRIPTOR,
			SPOILERS_DESCRIPTOR,
			SPOILER_DESCRIPTOR,
			HIDE_DESCRIPTOR,
			REVEAL_DESCRIPTOR,
			MODERATOR_DESCRIPTOR,
			MOD_DESCRIPTOR,
			MANAGE_MESSAGES_PERMISSION_DESCRIPTOR,
			CHANNELS_I_MODERATE_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'input',
		tabType: 'chat_settings',
		label: INPUT_DESCRIPTOR,
		keywords: [
			AUTOCOMPLETE_DESCRIPTOR,
			SUGGESTIONS_DESCRIPTOR,
			EMOJI_PICKER_DESCRIPTOR,
			MENTIONS_DESCRIPTOR,
			STICKER_DESCRIPTOR,
			STICKERS_DESCRIPTOR,
			EXPRESSIONS_DESCRIPTOR,
			COLON_DESCRIPTOR,
			EMOTICONS_DESCRIPTOR,
			SMILEYS_DESCRIPTOR,
			TEXT_REPLACEMENT_DESCRIPTOR,
			COLON_AUTOCOMPLETE_DESCRIPTOR,
			EXPRESSION_AUTOCOMPLETE_DESCRIPTOR,
			SAVED_MEDIA_AUTOCOMPLETE_DESCRIPTOR,
			MEDIA_BUTTON_DESCRIPTOR,
			SEND_BUTTON_DESCRIPTOR,
			SCROLL_DESCRIPTOR,
			SCROLL_TO_BOTTOM_DESCRIPTOR,
			SENT_MESSAGES_DESCRIPTOR,
			COMPOSER_BUTTON_DESCRIPTOR,
			COMPOSER_BUTTONS_DESCRIPTOR,
			MESSAGE_INPUT_BUTTONS_DESCRIPTOR,
			TOOLBAR_DESCRIPTOR,
		],
		isAdvanced: false,
		tags: ['chat'],
	},
	{
		id: 'media',
		tabType: 'chat_settings',
		label: MEDIA_DESCRIPTOR,
		keywords: [
			EMBEDS_DESCRIPTOR,
			PREVIEWS_DESCRIPTOR,
			LINKS_DESCRIPTOR,
			URL_PREVIEW_DESCRIPTOR,
			LINK_PREVIEW_DESCRIPTOR,
			MEDIA_2_DESCRIPTOR,
			IMAGES_DESCRIPTOR,
			VIDEOS_DESCRIPTOR,
			ATTACHMENTS_DESCRIPTOR,
			INLINE_DESCRIPTOR,
			MEDIA_SIZE_DESCRIPTOR,
		],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
