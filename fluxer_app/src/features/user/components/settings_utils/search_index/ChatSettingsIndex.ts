// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {INBOX_DESCRIPTOR} from '@app/features/user/components/settings_utils/search_index/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const AUTOMATICALLY_SEND_GIFS_WHEN_SELECTED_DESCRIPTOR = msg({
	message: 'Automatically send GIFs when selected',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const ANIMATION_DESCRIPTOR = msg({
	message: 'Animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATIONS_DESCRIPTOR = msg({
	message: 'Animations',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATICALLY_SEND_ANIMATION_DESCRIPTOR = msg({
	message: 'Automatically send animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATICALLY_SEND_ANIMATIONS_DESCRIPTOR = msg({
	message: 'Automatically send animations',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATION_PICKER_DESCRIPTOR = msg({
	message: 'Animation picker',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATION_BEHAVIOR_DESCRIPTOR = msg({
	message: 'Animation behavior',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEND_ANIMATION_DESCRIPTOR = msg({
	message: 'Send animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATICALLY_SEND_GIFS_FROM_THE_PICKER_WITHOUT_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Automatically send GIFs from the picker without confirmation',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SAVE_GIF_FAVORITES_AS_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Save GIF favorites as saved media',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const FAVORITE_ANIMATION_DESCRIPTOR = msg({
	message: 'Favorite animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FAVORITE_ANIMATIONS_DESCRIPTOR = msg({
	message: 'Favorite animations',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATION_FAVORITES_DESCRIPTOR = msg({
	message: 'Animation favorites',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Saved media',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STAR_ANIMATION_DESCRIPTOR = msg({
	message: 'Star animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATION_SAVING_DESCRIPTOR = msg({
	message: 'Animation saving',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SAVE_ANIMATION_DESCRIPTOR = msg({
	message: 'Save animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_HOW_STARRED_GIF_FAVORITES_ARE_STORED_DESCRIPTOR = msg({
	message: 'Choose how starred GIF favorites are stored',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const MEDIA_BUTTONS_DESCRIPTOR = msg({
	message: 'Media buttons',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const MEDIA_ATTACHMENT_BUTTONS_DESCRIPTOR = msg({
	message: 'Media attachment buttons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOWNLOAD_BUTTON_DESCRIPTOR = msg({
	message: 'Download button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FAVORITE_BUTTON_DESCRIPTOR = msg({
	message: 'Favorite button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GIF_INDICATOR_DESCRIPTOR = msg({
	message: 'GIF indicator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOMIZE_WHICH_BUTTONS_APPEAR_ON_MEDIA_ATTACHMENTS_DESCRIPTOR = msg({
	message: 'Customize which buttons and indicators appear on media attachments and embeds',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const UPLOAD_ATTACHMENTS_BEFORE_SENDING_DESCRIPTOR = msg({
	message: 'Upload attachments before sending',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const PREUPLOAD_ATTACHMENTS_DESCRIPTOR = msg({
	message: 'Pre-upload attachments',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EARLY_UPLOAD_DESCRIPTOR = msg({
	message: 'Early upload',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ATTACHMENT_UPLOAD_DESCRIPTOR = msg({
	message: 'Attachment upload',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FILE_UPLOAD_DESCRIPTOR = msg({
	message: 'File upload',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEQUENTIAL_FILE_SEND_DESCRIPTOR = msg({
	message: 'Send file messages in order',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SEQUENTIAL_SEND_DESCRIPTOR = msg({
	message: 'Sequential send',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FILE_ORDERING_DESCRIPTOR = msg({
	message: 'File ordering',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ORDERED_UPLOAD_DESCRIPTOR = msg({
	message: 'Ordered upload',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SERIAL_SEND_DESCRIPTOR = msg({
	message: 'Serial send',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENSURES_FILES_ARE_SENT_IN_THE_ORDER_THEY_WERE_ADDED_DESCRIPTOR = msg({
	message: 'Ensures file messages appear in the order you sent them',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const START_UPLOADING_ATTACHMENTS_WHEN_THEY_ARE_ADDED_DESCRIPTOR = msg({
	message: 'Start uploading attachments as soon as they are added to the message input',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const STRIP_TRACKING_PARAMETERS_FROM_URLS_DESCRIPTOR = msg({
	message: 'Strip tracking parameters from URLs',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const TRACKING_DESCRIPTOR = msg({
	message: 'Tracking',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRACKING_PARAMETERS_DESCRIPTOR = msg({
	message: 'Tracking parameters',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MARKETING_PARAMETERS_DESCRIPTOR = msg({
	message: 'Marketing parameters',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REFERRAL_PARAMETERS_DESCRIPTOR = msg({
	message: 'Referral parameters',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CLEAN_LINK_DESCRIPTOR = msg({
	message: 'Clean link',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SANITIZE_LINK_DESCRIPTOR = msg({
	message: 'Sanitize link',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STRIP_TRACKING_DESCRIPTOR = msg({
	message: 'Strip tracking',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STRIP_PARAMETERS_DESCRIPTOR = msg({
	message: 'Strip parameters',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATICALLY_REMOVE_TRACKING_PARAMETERS_FROM_URLS_IN_MESSAGES_DESCRIPTOR = msg({
	message: 'Automatically remove tracking parameters from URLs in messages you send',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const TRUST_ALL_EXTERNAL_LINKS_DESCRIPTOR = msg({
	message: 'Trust all external links',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const TRUST_ALL_DESCRIPTOR = msg({
	message: 'Trust all',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRUST_ALL_LINKS_DESCRIPTOR = msg({
	message: 'Trust all links',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRUSTED_DOMAINS_DESCRIPTOR = msg({
	message: 'Trusted domains',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRUST_DOMAIN_DESCRIPTOR = msg({
	message: 'Trust domain',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXTERNAL_LINK_WARNING_DESCRIPTOR = msg({
	message: 'External link warning',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINK_WARNINGS_DESCRIPTOR = msg({
	message: 'Link warnings',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SKIP_THE_EXTERNAL_LINK_WARNING_FOR_ALL_DOMAINS_DESCRIPTOR = msg({
	message: 'Skip the external link warning for all domains',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SEARCH_ENGINES_DESCRIPTOR = msg({
	message: 'Search engines',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Search engine',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WEB_SEARCH_DESCRIPTOR = msg({
	message: 'Web search',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEARCH_SELECTED_TEXT_DESCRIPTOR = msg({
	message: 'Search selected text',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_SEARCH_ENGINE_DESCRIPTOR = msg({
	message: 'Custom search engine',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRANSLATOR_DESCRIPTOR = msg({
	message: 'Translator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRANSLATORS_DESCRIPTOR = msg({
	message: 'Translators',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRANSLATE_DESCRIPTOR = msg({
	message: 'Translate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_TRANSLATOR_DESCRIPTOR = msg({
	message: 'Custom translator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIGURE_SEARCH_ENGINES_DESCRIPTOR = msg({
	message: 'Configure search engines used from selected text',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const REVERSE_IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Reverse image search',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const REVERSE_IMAGE_DESCRIPTOR = msg({
	message: 'Reverse image',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMAGE_SEARCH_DESCRIPTOR = msg({
	message: 'Image search',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEARCH_BY_IMAGE_DESCRIPTOR = msg({
	message: 'Search by image',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMAGE_LOOKUP_DESCRIPTOR = msg({
	message: 'Image lookup',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GOOGLE_LENS_DESCRIPTOR = msg({
	message: 'Google lens',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMAGE_SEARCH_PROVIDER_DESCRIPTOR = msg({
	message: 'Image search provider',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIGURE_TRANSLATOR_PROVIDERS_DESCRIPTOR = msg({
	message: 'Configure translator providers used from selected text',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CONFIGURE_REVERSE_IMAGE_SEARCH_PROVIDERS_DESCRIPTOR = msg({
	message: 'Reverse image search providers',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const MESSAGE_ACTION_BAR_DESCRIPTOR = msg({
	message: 'Message action bar',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const ACTION_BAR_DESCRIPTOR = msg({
	message: 'Action bar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_HOVER_DESCRIPTOR = msg({
	message: 'Message hover',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HOVER_BUTTONS_DESCRIPTOR = msg({
	message: 'Hover buttons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const QUICK_REACTIONS_DESCRIPTOR = msg({
	message: 'Quick reactions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHIFT_EXPAND_DESCRIPTOR = msg({
	message: 'Shift expand',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPAND_ACTION_BAR_DESCRIPTOR = msg({
	message: 'Expand action bar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MORE_BUTTON_DESCRIPTOR = msg({
	message: 'More button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_BUTTONS_DESCRIPTOR = msg({
	message: 'Message buttons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOMIZE_THE_ACTION_BAR_THAT_APPEARS_WHEN_HOVERING_DESCRIPTOR = msg({
	message: 'Customize the action bar that appears when hovering over messages',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const EXPRESSION_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Expression autocomplete',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const COLON_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Colon autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMOJI_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Emoji autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STICKER_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Sticker autocomplete',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PICK_WHAT_SHOWS_UP_WHEN_TYPING_A_COLON_DESCRIPTOR = msg({
	message: 'Pick what appears when you type a colon in the message input',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CONVERT_EMOTICONS_TO_EMOJI_DESCRIPTOR = msg({
	message: 'Turn text smileys into emoji',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const EMOTICONS_DESCRIPTOR = msg({
	message: 'Emoticons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMOTICON_DESCRIPTOR = msg({
	message: 'Emoticon',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ASCII_SMILEYS_DESCRIPTOR = msg({
	message: 'ASCII smileys',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SMILEY_FACE_DESCRIPTOR = msg({
	message: 'Smiley face',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEXT_REPLACEMENT_DESCRIPTOR = msg({
	message: 'Text replacement',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REPLACE_COMMON_EMOTICONS_LIKE_SMILE_AND_HEART_DESCRIPTOR = msg({
	message: 'Swap common text faces such as :) and <3 for emoji when sending messages',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const MESSAGE_INPUT_BUTTONS_DESCRIPTOR = msg({
	message: 'Message input buttons',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const COMPOSER_BUTTONS_DESCRIPTOR = msg({
	message: 'Composer buttons',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GIF_BUTTON_DESCRIPTOR = msg({
	message: 'GIF button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EMOJI_BUTTON_DESCRIPTOR = msg({
	message: 'Emoji button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEND_BUTTON_DESCRIPTOR = msg({
	message: 'Send button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PICK_WHICH_BUTTONS_SHOW_IN_THE_MESSAGE_INPUT_DESCRIPTOR = msg({
	message: 'Pick which buttons show in the message input',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SCROLL_TO_BOTTOM_WHEN_SENDING_A_MESSAGE_DESCRIPTOR = msg({
	message: 'Scroll to bottom when sending a message',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SCROLL_TO_BOTTOM_DESCRIPTOR = msg({
	message: 'Scroll to bottom',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SENT_MESSAGES_DESCRIPTOR = msg({
	message: 'Sent messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_HOW_CHAT_MOVES_AFTER_YOU_SEND_A_MESSAGE_DESCRIPTOR = msg({
	message: 'Choose how chat moves after you send a message',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SKIP_MARK_ALL_AS_READ_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Skip "Mark all as read" confirmation',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const MARK_ALL_AS_READ_DESCRIPTOR = msg({
	message: 'Mark all as read',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIRMATION_DESCRIPTOR = msg({
	message: 'Confirmation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIRM_DESCRIPTOR = msg({
	message: 'Confirm',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROMPT_DESCRIPTOR = msg({
	message: 'Prompt',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SKIP_DESCRIPTOR = msg({
	message: 'Skip',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MARK_ALL_UNREAD_INBOX_CHANNELS_AS_READ_IMMEDIATELY_DESCRIPTOR = msg({
	message: 'Mark all unread inbox channels as read immediately, without asking to confirm',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const HIDE_MUTED_CHANNELS_BY_DEFAULT_DESCRIPTOR = msg({
	message: 'Hide muted channels by default',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const MUTED_DESCRIPTOR = msg({
	message: 'Muted',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTED_CHANNELS_DESCRIPTOR = msg({
	message: 'Muted channels',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_MUTED_DESCRIPTOR = msg({
	message: 'Hide muted',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NEW_COMMUNITY_MUTED_DESCRIPTOR = msg({
	message: 'New community muted',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_CHANNELS_YOU_VE_MUTED_FROM_COMMUNITY_SIDEBARS_DESCRIPTOR = msg({
	message: "Hide channels you've muted from community sidebars",
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const chatSettingsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'chat-settings-auto-send-gifs',
		tabType: 'chat_settings',
		sectionId: 'media',
		label: AUTOMATICALLY_SEND_GIFS_WHEN_SELECTED_DESCRIPTOR,
		keywords: [
			ANIMATION_DESCRIPTOR,
			ANIMATIONS_DESCRIPTOR,
			AUTOMATICALLY_SEND_ANIMATION_DESCRIPTOR,
			AUTOMATICALLY_SEND_ANIMATIONS_DESCRIPTOR,
			ANIMATION_PICKER_DESCRIPTOR,
			ANIMATION_BEHAVIOR_DESCRIPTOR,
			SEND_ANIMATION_DESCRIPTOR,
		],
		description: AUTOMATICALLY_SEND_GIFS_FROM_THE_PICKER_WITHOUT_CONFIRMATION_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'media'],
	},
	{
		id: 'chat-settings-save-gif-favorites',
		tabType: 'chat_settings',
		sectionId: 'media',
		label: SAVE_GIF_FAVORITES_AS_SAVED_MEDIA_DESCRIPTOR,
		keywords: [
			ANIMATION_DESCRIPTOR,
			ANIMATIONS_DESCRIPTOR,
			FAVORITE_ANIMATION_DESCRIPTOR,
			FAVORITE_ANIMATIONS_DESCRIPTOR,
			ANIMATION_FAVORITES_DESCRIPTOR,
			SAVED_MEDIA_DESCRIPTOR,
			STAR_ANIMATION_DESCRIPTOR,
			ANIMATION_BEHAVIOR_DESCRIPTOR,
			ANIMATION_SAVING_DESCRIPTOR,
			SAVE_ANIMATION_DESCRIPTOR,
		],
		description: CHOOSE_HOW_STARRED_GIF_FAVORITES_ARE_STORED_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'media'],
	},
	{
		id: 'chat-settings-media-buttons',
		tabType: 'chat_settings',
		sectionId: 'media',
		label: MEDIA_BUTTONS_DESCRIPTOR,
		keywords: [
			MEDIA_ATTACHMENT_BUTTONS_DESCRIPTOR,
			DOWNLOAD_BUTTON_DESCRIPTOR,
			FAVORITE_BUTTON_DESCRIPTOR,
			GIF_INDICATOR_DESCRIPTOR,
		],
		description: CUSTOMIZE_WHICH_BUTTONS_APPEAR_ON_MEDIA_ATTACHMENTS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'media'],
	},
	{
		id: 'chat-settings-preupload-attachments',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: UPLOAD_ATTACHMENTS_BEFORE_SENDING_DESCRIPTOR,
		keywords: [
			PREUPLOAD_ATTACHMENTS_DESCRIPTOR,
			EARLY_UPLOAD_DESCRIPTOR,
			ATTACHMENT_UPLOAD_DESCRIPTOR,
			FILE_UPLOAD_DESCRIPTOR,
			MEDIA_ATTACHMENT_BUTTONS_DESCRIPTOR,
		],
		description: START_UPLOADING_ATTACHMENTS_WHEN_THEY_ARE_ADDED_DESCRIPTOR,
		audience: 'advanced',
		tags: ['privacy', 'chat', 'media'],
		addedAt: '2026-06-04T16:10:00.000Z',
	},
	{
		id: 'chat-settings-strip-tracking',
		tabType: 'chat_settings',
		label: STRIP_TRACKING_PARAMETERS_FROM_URLS_DESCRIPTOR,
		keywords: [
			TRACKING_DESCRIPTOR,
			TRACKING_PARAMETERS_DESCRIPTOR,
			MARKETING_PARAMETERS_DESCRIPTOR,
			REFERRAL_PARAMETERS_DESCRIPTOR,
			CLEAN_LINK_DESCRIPTOR,
			SANITIZE_LINK_DESCRIPTOR,
			STRIP_TRACKING_DESCRIPTOR,
			STRIP_PARAMETERS_DESCRIPTOR,
		],
		description: AUTOMATICALLY_REMOVE_TRACKING_PARAMETERS_FROM_URLS_IN_MESSAGES_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'privacy'],
	},
	{
		id: 'chat-settings-trust-domains',
		tabType: 'chat_settings',
		label: TRUST_ALL_EXTERNAL_LINKS_DESCRIPTOR,
		keywords: [
			TRUST_ALL_DESCRIPTOR,
			TRUST_ALL_LINKS_DESCRIPTOR,
			TRUSTED_DOMAINS_DESCRIPTOR,
			TRUST_DOMAIN_DESCRIPTOR,
			EXTERNAL_LINK_WARNING_DESCRIPTOR,
			LINK_WARNINGS_DESCRIPTOR,
		],
		description: SKIP_THE_EXTERNAL_LINK_WARNING_FOR_ALL_DOMAINS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'privacy'],
	},
	{
		id: 'chat-settings-search-engines',
		tabType: 'chat_settings',
		label: SEARCH_ENGINES_DESCRIPTOR,
		keywords: [
			SEARCH_ENGINE_DESCRIPTOR,
			WEB_SEARCH_DESCRIPTOR,
			SEARCH_SELECTED_TEXT_DESCRIPTOR,
			CUSTOM_SEARCH_ENGINE_DESCRIPTOR,
		],
		description: CONFIGURE_SEARCH_ENGINES_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-translators',
		tabType: 'chat_settings',
		label: TRANSLATORS_DESCRIPTOR,
		keywords: [TRANSLATOR_DESCRIPTOR, TRANSLATORS_DESCRIPTOR, TRANSLATE_DESCRIPTOR, CUSTOM_TRANSLATOR_DESCRIPTOR],
		description: CONFIGURE_TRANSLATOR_PROVIDERS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-reverse-image-search',
		tabType: 'chat_settings',
		label: REVERSE_IMAGE_SEARCH_DESCRIPTOR,
		keywords: [
			REVERSE_IMAGE_SEARCH_DESCRIPTOR,
			REVERSE_IMAGE_DESCRIPTOR,
			IMAGE_SEARCH_DESCRIPTOR,
			SEARCH_BY_IMAGE_DESCRIPTOR,
			IMAGE_LOOKUP_DESCRIPTOR,
			GOOGLE_LENS_DESCRIPTOR,
			IMAGE_SEARCH_PROVIDER_DESCRIPTOR,
		],
		description: CONFIGURE_REVERSE_IMAGE_SEARCH_PROVIDERS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'media'],
	},
	{
		id: 'chat-settings-message-action-bar',
		tabType: 'chat_settings',
		label: MESSAGE_ACTION_BAR_DESCRIPTOR,
		keywords: [
			ACTION_BAR_DESCRIPTOR,
			MESSAGE_ACTION_BAR_DESCRIPTOR,
			MESSAGE_HOVER_DESCRIPTOR,
			HOVER_BUTTONS_DESCRIPTOR,
			QUICK_REACTIONS_DESCRIPTOR,
			SHIFT_EXPAND_DESCRIPTOR,
			EXPAND_ACTION_BAR_DESCRIPTOR,
			MORE_BUTTON_DESCRIPTOR,
			MESSAGE_BUTTONS_DESCRIPTOR,
		],
		description: CUSTOMIZE_THE_ACTION_BAR_THAT_APPEARS_WHEN_HOVERING_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-expression-autocomplete',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: EXPRESSION_AUTOCOMPLETE_DESCRIPTOR,
		keywords: [
			COLON_AUTOCOMPLETE_DESCRIPTOR,
			EMOJI_AUTOCOMPLETE_DESCRIPTOR,
			STICKER_AUTOCOMPLETE_DESCRIPTOR,
			SAVED_MEDIA_DESCRIPTOR,
		],
		description: PICK_WHAT_SHOWS_UP_WHEN_TYPING_A_COLON_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-input-buttons',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: MESSAGE_INPUT_BUTTONS_DESCRIPTOR,
		keywords: [
			COMPOSER_BUTTONS_DESCRIPTOR,
			GIF_BUTTON_DESCRIPTOR,
			EMOJI_BUTTON_DESCRIPTOR,
			SEND_BUTTON_DESCRIPTOR,
			SAVED_MEDIA_DESCRIPTOR,
		],
		description: PICK_WHICH_BUTTONS_SHOW_IN_THE_MESSAGE_INPUT_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-convert-emoticons',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: CONVERT_EMOTICONS_TO_EMOJI_DESCRIPTOR,
		keywords: [
			EMOTICONS_DESCRIPTOR,
			EMOTICON_DESCRIPTOR,
			ASCII_SMILEYS_DESCRIPTOR,
			SMILEY_FACE_DESCRIPTOR,
			TEXT_REPLACEMENT_DESCRIPTOR,
			EMOJI_AUTOCOMPLETE_DESCRIPTOR,
		],
		description: REPLACE_COMMON_EMOTICONS_LIKE_SMILE_AND_HEART_DESCRIPTOR,
		audience: 'primary',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-scroll-to-bottom-on-send',
		tabType: 'chat_settings',
		sectionId: 'input',
		label: SCROLL_TO_BOTTOM_WHEN_SENDING_A_MESSAGE_DESCRIPTOR,
		keywords: [SCROLL_TO_BOTTOM_DESCRIPTOR, SENT_MESSAGES_DESCRIPTOR],
		description: CHOOSE_HOW_CHAT_MOVES_AFTER_YOU_SEND_A_MESSAGE_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-skip-mark-all-as-read-confirmation',
		tabType: 'chat_settings',
		label: SKIP_MARK_ALL_AS_READ_CONFIRMATION_DESCRIPTOR,
		keywords: [
			MARK_ALL_AS_READ_DESCRIPTOR,
			MARK_AS_READ_DESCRIPTOR,
			INBOX_DESCRIPTOR,
			CONFIRMATION_DESCRIPTOR,
			CONFIRM_DESCRIPTOR,
			PROMPT_DESCRIPTOR,
			SKIP_DESCRIPTOR,
		],
		description: MARK_ALL_UNREAD_INBOX_CHANNELS_AS_READ_IMMEDIATELY_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat', 'notifications'],
	},
	{
		id: 'chat-settings-hide-muted-channels',
		tabType: 'chat_settings',
		label: HIDE_MUTED_CHANNELS_BY_DEFAULT_DESCRIPTOR,
		keywords: [
			MUTED_DESCRIPTOR,
			MUTED_CHANNELS_DESCRIPTOR,
			HIDE_MUTED_DESCRIPTOR,
			HIDE_MUTED_CHANNELS_DESCRIPTOR,
			NEW_COMMUNITY_MUTED_DESCRIPTOR,
		],
		description: HIDE_CHANNELS_YOU_VE_MUTED_FROM_COMMUNITY_SIDEBARS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
	{
		id: 'chat-settings-sequential-file-send',
		tabType: 'advanced_settings',
		sectionId: 'chat',
		label: SEQUENTIAL_FILE_SEND_DESCRIPTOR,
		keywords: [
			SEQUENTIAL_SEND_DESCRIPTOR,
			FILE_ORDERING_DESCRIPTOR,
			ORDERED_UPLOAD_DESCRIPTOR,
			SERIAL_SEND_DESCRIPTOR,
			FILE_UPLOAD_DESCRIPTOR,
		],
		description: ENSURES_FILES_ARE_SENT_IN_THE_ORDER_THEY_WERE_ADDED_DESCRIPTOR,
		audience: 'advanced',
		tags: ['chat'],
	},
];
