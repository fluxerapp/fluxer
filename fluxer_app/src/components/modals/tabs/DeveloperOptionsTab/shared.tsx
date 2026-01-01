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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {DeveloperOptionsState} from '~/stores/DeveloperOptionsStore';

export interface ToggleDef {
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
		title: msg`App State`,
		items: [
			{key: 'bypassSplashScreen', label: msg`Bypass Splash Screen`},
			{key: 'forceUpdateReady', label: msg`Force Update Ready`},
			{key: 'showMyselfTyping', label: msg`Show Myself Typing`},
			{
				key: 'selfHostedModeOverride',
				label: msg`Self-Hosted Mode Override`,
				description: msg`Enable self-hosted mode client-side (hides all premium/billing UI, grants everyone premium)`,
			},
		],
	},
	{
		title: msg`UI Components`,
		items: [
			{key: 'forceGifPickerLoading', label: msg`Force GIF Picker Loading`},
			{
				key: 'forceShowVanityURLDisclaimer',
				label: msg`Force Show Vanity URL Disclaimer`,
				description: msg`Always show the vanity URL disclaimer warning in guild settings`,
			},
		],
	},
	{
		title: msg`Networking & Performance`,
		items: [
			{key: 'slowMessageLoad', label: msg`Slow Message Load`},
			{key: 'slowMessageSend', label: msg`Slow Message Send`},
			{key: 'slowMessageEdit', label: msg`Slow Message Edit`},
			{key: 'slowAttachmentUpload', label: msg`Slow Attachment Upload`},
			{key: 'slowProfileLoad', label: msg`Slow Profile Load`},
			{
				key: 'forceProfileDataWarning',
				label: msg`Force Profile Data Warning`,
				description: msg`Always show the profile data warning indicator, even when the profile loads successfully`,
			},
			{key: 'forceFailUploads', label: msg`Force Fail Uploads`},
			{key: 'forceFailMessageSends', label: msg`Force Fail Message Sends`},
		],
	},
	{
		title: msg`Features`,
		items: [
			{
				key: 'forceUnknownMessageType',
				label: msg`Force Unknown Message Type`,
				description: msg`Render all your messages as unknown message type`,
			},
			{
				key: 'forceShowVoiceConnection',
				label: msg`Force Show Voice Connection`,
				description: msg`Always display the voice connection status bar in mocked mode`,
			},
		],
	},
	{
		title: msg`Logging & Diagnostics`,
		items: [{key: 'debugLogging', label: msg`Debug Logging`}],
	},
];
