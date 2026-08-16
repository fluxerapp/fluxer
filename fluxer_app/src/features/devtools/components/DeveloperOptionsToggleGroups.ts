// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ENABLE_SELF_HOSTED_MODE_CLIENT_SIDE_HIDES_ALL_DESCRIPTOR = msg({
	message: 'Enable self-hosted mode client-side. Hides all premium and billing UI.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for the self-hosted mode override toggle.',
});
const ALWAYS_SHOW_THE_VANITY_URL_DISCLAIMER_WARNING_IN_DESCRIPTOR = msg({
	message: 'Always show the vanity URL disclaimer warning in community settings.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for forcing the vanity-URL disclaimer in community settings.',
});
const ALWAYS_SHOW_THE_PROFILE_DATA_WARNING_INDICATOR_EVEN_DESCRIPTOR = msg({
	message: 'Always show the profile data warning indicator, even when the profile loads successfully.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for forcing the profile-data warning indicator.',
});
const ALWAYS_SHOW_THE_PROFILE_LOADING_SKELETON_DESCRIPTOR = msg({
	message: 'Always show the profile loading skeleton instead of resolved profile content.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for forcing profile skeleton loading states.',
});
const RENDER_ALL_YOUR_MESSAGES_AS_AN_UNKNOWN_MESSAGE_DESCRIPTOR = msg({
	message: 'Render all your messages as an unknown message type.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for the force-unknown-message-type override.',
});
const ALWAYS_DISPLAY_THE_VOICE_CONNECTION_STATUS_BAR_IN_DESCRIPTOR = msg({
	message: 'Always display the voice connection status bar in mocked mode.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for forcing the voice connection status bar visible in mocked mode.',
});
const APP_STATE_DESCRIPTOR = msg({
	message: 'App state',
	comment: 'Developer options group for overriding app/account state.',
});
const BYPASS_LOADING_SKELETON_DESCRIPTOR = msg({
	message: 'Bypass loading skeleton',
	comment: 'Developer option label.',
});
const FORCE_LOADING_SKELETON_DESCRIPTOR = msg({
	message: 'Force loading skeleton',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORCE_UPDATE_READY_DESCRIPTOR = msg({
	message: 'Force update ready',
	comment: 'Developer option label for simulating an available update.',
});
const SHOW_MYSELF_TYPING_DESCRIPTOR = msg({
	message: 'Show myself typing',
	comment: 'Developer option label for showing the current user as typing.',
});
const SELF_HOSTED_MODE_OVERRIDE_DESCRIPTOR = msg({
	message: 'Self-hosted mode override',
	comment: 'Developer option label for forcing self-hosted client mode.',
});
const UI_COMPONENTS_DESCRIPTOR = msg({
	message: 'UI components',
	comment: 'Developer options group for forcing UI component states.',
});
const FORCE_GIF_PICKER_LOADING_DESCRIPTOR = msg({
	message: 'Force GIF picker loading',
	comment: 'Developer option label for simulating a loading GIF picker.',
});
const FORCE_SHOW_VANITY_URL_DISCLAIMER_DESCRIPTOR = msg({
	message: 'Force show vanity URL disclaimer',
	comment: 'Developer option label for always showing a community settings warning.',
});
const NETWORKING_AND_PERFORMANCE_DESCRIPTOR = msg({
	message: 'Networking and performance',
	comment: 'Developer options group for simulated slow or failed operations.',
});
const SLOW_MESSAGE_LOAD_DESCRIPTOR = msg({
	message: 'Slow message load',
	comment: 'Developer option label for simulating slow message loading.',
});
const SLOW_MESSAGE_SEND_DESCRIPTOR = msg({
	message: 'Slow message send',
	comment: 'Developer option label for simulating slow message sending.',
});
const SLOW_MESSAGE_EDIT_DESCRIPTOR = msg({
	message: 'Slow message edit',
	comment: 'Developer option label for simulating slow message edits.',
});
const SLOW_ATTACHMENT_UPLOAD_DESCRIPTOR = msg({
	message: 'Slow attachment upload',
	comment: 'Developer option label for simulating slow file uploads.',
});
const SLOW_PROFILE_LOAD_DESCRIPTOR = msg({
	message: 'Slow profile load',
	comment: 'Developer option label for simulating slow profile loading.',
});
const FORCE_PROFILE_SKELETONS_DESCRIPTOR = msg({
	message: 'Force profile skeletons',
	comment: 'Developer option label for forcing user profile loading skeletons.',
});
const FORCE_PROFILE_DATA_WARNING_DESCRIPTOR = msg({
	message: 'Force profile data warning',
	comment: 'Developer option label for always showing profile data warnings.',
});
const FORCE_FAILED_MESSAGE_SENDS_DESCRIPTOR = msg({
	message: 'Force failed message sends',
	comment: 'Developer option label for simulating failed message sends.',
});
const FORCE_FAILED_MESSAGE_LOADS_DESCRIPTOR = msg({
	message: 'Force failed message loads',
	comment: 'Developer option label for simulating failed message loads.',
});
const FEATURES_DESCRIPTOR = msg({
	message: 'Features',
	comment: 'Developer options group for feature behavior overrides.',
});
const FORCE_UNKNOWN_MESSAGE_TYPE_DESCRIPTOR = msg({
	message: 'Force unknown message type',
	comment: 'Developer option label for rendering messages as unknown message types.',
});
const FORCE_SHOW_VOICE_CONNECTION_DESCRIPTOR = msg({
	message: 'Force show voice connection',
	comment: 'Developer option label for always showing the voice connection status bar.',
});
const SHOW_EXPRESSION_PACKS_SETTINGS_DESCRIPTOR = msg({
	message: 'Show expression packs settings',
	comment: 'Developer option label for exposing the staff-only Expression packs page in user settings.',
});
const SHOW_EXPRESSION_PACKS_SETTINGS_DESC_DESCRIPTOR = msg({
	message: 'Expose the staff-only Expression packs page in user settings.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for the Expression packs settings toggle.',
});
const SHOW_PROFILE_TIMEZONE_SETTINGS_DESCRIPTOR = msg({
	message: 'Show profile timezone settings',
	comment: 'Developer option label for exposing the staff-only profile timezone section in profile settings.',
});
const SHOW_PROFILE_TIMEZONE_SETTINGS_DESC_DESCRIPTOR = msg({
	message: 'Expose the staff-only timezone section in profile settings.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for the profile timezone settings toggle.',
});
const NO_OP_IN_APP_REPORTS_DESCRIPTOR = msg({
	message: 'No-op in-app reports',
	comment:
		'Developer option label for short-circuiting the in-app reporting (IAR) flow. When enabled, submitting a report skips the network call.',
});
const NO_OP_IN_APP_REPORTS_DESC_DESCRIPTOR = msg({
	message:
		'Skip the network call when an in-app report is submitted. The flow still advances to the success screen so you can exercise the UI, but nothing is actually sent to the safety team.',
	comment:
		'Developer / debug surface — keep terse and technical. Tooltip / description for the no-op in-app reports toggle.',
});

interface ToggleDef {
	key: keyof DeveloperOptionsState;
	label: MessageDescriptor;
	description?: MessageDescriptor;
}

export interface ToggleGroup {
	title: MessageDescriptor;
	items: Array<ToggleDef>;
}

export const getToggleGroups = (): Array<ToggleGroup> => [
	{
		title: APP_STATE_DESCRIPTOR,
		items: [
			{key: 'bypassLoadingSkeleton', label: BYPASS_LOADING_SKELETON_DESCRIPTOR},
			{key: 'forceLoadingSkeleton', label: FORCE_LOADING_SKELETON_DESCRIPTOR},
			{
				key: 'forceUpdateReady',
				label: FORCE_UPDATE_READY_DESCRIPTOR,
			},
			{
				key: 'showMyselfTyping',
				label: SHOW_MYSELF_TYPING_DESCRIPTOR,
			},
			{
				key: 'selfHostedModeOverride',
				label: SELF_HOSTED_MODE_OVERRIDE_DESCRIPTOR,
				description: ENABLE_SELF_HOSTED_MODE_CLIENT_SIDE_HIDES_ALL_DESCRIPTOR,
			},
		],
	},
	{
		title: UI_COMPONENTS_DESCRIPTOR,
		items: [
			{
				key: 'forceGifPickerLoading',
				label: FORCE_GIF_PICKER_LOADING_DESCRIPTOR,
			},
			{
				key: 'forceShowVanityURLDisclaimer',
				label: FORCE_SHOW_VANITY_URL_DISCLAIMER_DESCRIPTOR,
				description: ALWAYS_SHOW_THE_VANITY_URL_DISCLAIMER_WARNING_IN_DESCRIPTOR,
			},
		],
	},
	{
		title: NETWORKING_AND_PERFORMANCE_DESCRIPTOR,
		items: [
			{
				key: 'slowMessageLoad',
				label: SLOW_MESSAGE_LOAD_DESCRIPTOR,
			},
			{
				key: 'slowMessageSend',
				label: SLOW_MESSAGE_SEND_DESCRIPTOR,
			},
			{
				key: 'slowMessageEdit',
				label: SLOW_MESSAGE_EDIT_DESCRIPTOR,
			},
			{
				key: 'slowAttachmentUpload',
				label: SLOW_ATTACHMENT_UPLOAD_DESCRIPTOR,
			},
			{
				key: 'slowProfileLoad',
				label: SLOW_PROFILE_LOAD_DESCRIPTOR,
			},
			{
				key: 'forceProfileSkeletons',
				label: FORCE_PROFILE_SKELETONS_DESCRIPTOR,
				description: ALWAYS_SHOW_THE_PROFILE_LOADING_SKELETON_DESCRIPTOR,
			},
			{
				key: 'forceProfileDataWarning',
				label: FORCE_PROFILE_DATA_WARNING_DESCRIPTOR,
				description: ALWAYS_SHOW_THE_PROFILE_DATA_WARNING_INDICATOR_EVEN_DESCRIPTOR,
			},
			{
				key: 'forceFailMessageSends',
				label: FORCE_FAILED_MESSAGE_SENDS_DESCRIPTOR,
			},
			{
				key: 'forceFailMessageLoads',
				label: FORCE_FAILED_MESSAGE_LOADS_DESCRIPTOR,
			},
		],
	},
	{
		title: FEATURES_DESCRIPTOR,
		items: [
			{
				key: 'forceUnknownMessageType',
				label: FORCE_UNKNOWN_MESSAGE_TYPE_DESCRIPTOR,
				description: RENDER_ALL_YOUR_MESSAGES_AS_AN_UNKNOWN_MESSAGE_DESCRIPTOR,
			},
			{
				key: 'forceShowVoiceConnection',
				label: FORCE_SHOW_VOICE_CONNECTION_DESCRIPTOR,
				description: ALWAYS_DISPLAY_THE_VOICE_CONNECTION_STATUS_BAR_IN_DESCRIPTOR,
			},
			{
				key: 'showExpressionPacksSettings',
				label: SHOW_EXPRESSION_PACKS_SETTINGS_DESCRIPTOR,
				description: SHOW_EXPRESSION_PACKS_SETTINGS_DESC_DESCRIPTOR,
			},
			{
				key: 'showProfileTimezoneSettings',
				label: SHOW_PROFILE_TIMEZONE_SETTINGS_DESCRIPTOR,
				description: SHOW_PROFILE_TIMEZONE_SETTINGS_DESC_DESCRIPTOR,
			},
			{
				key: 'noOpInAppReports',
				label: NO_OP_IN_APP_REPORTS_DESCRIPTOR,
				description: NO_OP_IN_APP_REPORTS_DESC_DESCRIPTOR,
			},
		],
	},
];
