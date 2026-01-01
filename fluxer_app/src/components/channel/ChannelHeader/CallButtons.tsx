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

import {useLingui} from '@lingui/react/macro';

import {PhoneIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as CallActionCreators from '~/actions/CallActionCreators';
import type {ChannelRecord} from '~/records/ChannelRecord';
import CallStateStore from '~/stores/CallStateStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import * as CallUtils from '~/utils/CallUtils';
import {ChannelHeaderIcon} from './ChannelHeaderIcon';

const VoiceCallButton = observer(({channel}: {channel: ChannelRecord}) => {
	const {t} = useLingui();
	const call = CallStateStore.getCall(channel.id);
	const isConnected = MediaEngineStore.connected;
	const connectedChannelId = MediaEngineStore.channelId;
	const isInCall = isConnected && connectedChannelId === channel.id;
	const hasActiveCall = CallStateStore.hasActiveCall(channel.id);
	const participants = call ? CallStateStore.getParticipants(channel.id) : [];
	const participantCount = participants.length;

	const handleClick = React.useCallback(
		async (event: React.MouseEvent) => {
			if (isInCall) {
				void CallActionCreators.leaveCall(channel.id);
			} else if (hasActiveCall) {
				CallActionCreators.joinCall(channel.id);
			} else {
				const silent = event.shiftKey;
				await CallUtils.checkAndStartCall(channel.id, silent);
			}
		},
		[channel.id, isInCall, hasActiveCall],
	);

	let label: string;
	if (participantCount > 0 && hasActiveCall) {
		if (isInCall) {
			label =
				participantCount === 1
					? t`Leave Voice Call (${participantCount} participant)`
					: t`Leave Voice Call (${participantCount} participants)`;
		} else {
			label =
				participantCount === 1
					? t`Join Voice Call (${participantCount} participant)`
					: t`Join Voice Call (${participantCount} participants)`;
		}
	} else {
		label = isInCall ? t`Leave Voice Call` : hasActiveCall ? t`Join Voice Call` : t`Start Voice Call`;
	}

	return (
		<ChannelHeaderIcon
			icon={PhoneIcon}
			label={label}
			isSelected={isInCall}
			onClick={handleClick}
			keybindAction="start_pm_call"
		/>
	);
});

const VideoCallButton = observer(({channel}: {channel: ChannelRecord}) => {
	const {t} = useLingui();
	const call = CallStateStore.getCall(channel.id);
	const isConnected = MediaEngineStore.connected;
	const connectedChannelId = MediaEngineStore.channelId;
	const isInCall = isConnected && connectedChannelId === channel.id;
	const hasActiveCall = CallStateStore.hasActiveCall(channel.id);
	const participants = call ? CallStateStore.getParticipants(channel.id) : [];
	const participantCount = participants.length;

	const handleClick = React.useCallback(
		async (event: React.MouseEvent) => {
			if (isInCall) {
				void CallActionCreators.leaveCall(channel.id);
			} else if (hasActiveCall) {
				CallActionCreators.joinCall(channel.id);
			} else {
				const silent = event.shiftKey;
				await CallUtils.checkAndStartCall(channel.id, silent);
			}
		},
		[channel.id, isInCall, hasActiveCall],
	);

	let label: string;
	if (participantCount > 0 && hasActiveCall) {
		if (isInCall) {
			label =
				participantCount === 1
					? t`Leave Video Call (${participantCount} participant)`
					: t`Leave Video Call (${participantCount} participants)`;
		} else {
			label =
				participantCount === 1
					? t`Join Video Call (${participantCount} participant)`
					: t`Join Video Call (${participantCount} participants)`;
		}
	} else {
		label = isInCall ? t`Leave Video Call` : hasActiveCall ? t`Join Video Call` : t`Start Video Call`;
	}

	return <ChannelHeaderIcon icon={VideoCameraIcon} label={label} isSelected={isInCall} onClick={handleClick} />;
});

export const CallButtons = {
	VoiceCallButton,
	VideoCallButton,
};
