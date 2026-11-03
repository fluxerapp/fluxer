// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CallButtonBlocks,
	useCallButtonBlock,
} from '@app/features/channel/components/channel_header_components/CallButtonBlock';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	START_VIDEO_CALL_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
	TURN_OFF_CAMERA_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {accountGateReason, openAccountGateFlow} from '@app/features/user/utils/AccountGatePresentation';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallState from '@app/features/voice/state/CallState';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {VOICE_TURN_ON_CAMERA_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PhoneIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const SYSTEM_DMS_CANNOT_HOST_VOICE_CALLS_2_DESCRIPTOR = msg({
	message: 'System DMs cannot host voice calls',
	comment: 'Short tooltip on the disabled voice call button in a system DM (variant without trailing period).',
});
const LEAVE_VOICE_CALL_DESCRIPTOR = msg({
	message: 'Leave voice call',
	comment: 'Tooltip on the channel header voice call button when the current user is already in the DM voice call.',
});
const JOIN_VOICE_CALL_DESCRIPTOR = msg({
	message: 'Join voice call',
	comment: 'Tooltip on the channel header voice call button when an ongoing voice call exists that the user can join.',
});
const SYSTEM_DMS_CANNOT_HOST_VIDEO_CALLS_2_DESCRIPTOR = msg({
	message: 'System DMs cannot host video calls',
	comment: 'Short tooltip on the disabled video call button in a system DM (variant without trailing period).',
});
const JOIN_VIDEO_CALL_DESCRIPTOR = msg({
	message: 'Join video call',
	comment:
		'Tooltip on the channel header video call button when an ongoing call exists that the user can join with video.',
});
const VoiceCallButton = observer(({channel}: {channel: Channel}) => {
	const {i18n} = useLingui();
	const call = CallState.getCall(channel.id);
	const isConnected = MediaEngine.connected;
	const connectedChannelId = MediaEngine.channelId;
	const isInCall = isConnected && connectedChannelId === channel.id;
	const hasActiveCall = CallState.hasActiveCall(channel.id);
	const participants = call ? CallState.getParticipants(channel.id) : [];
	const participantCount = participants.length;
	const callBlock = useCallButtonBlock(channel);
	const handleClick = useCallback(
		async (event: React.MouseEvent) => {
			if (callBlock.block === CallButtonBlocks.ACCOUNT) {
				openAccountGateFlow(i18n, callBlock.gate);
				return;
			}
			if (isInCall) {
				void CallCommands.leaveCall(channel.id);
			} else if (hasActiveCall) {
				CallCommands.joinCall(channel.id);
			} else {
				await CallUtils.requestStartCall(
					i18n,
					channel.id,
					CallUtils.getCallStartRequestOptions(event, {kind: 'voice'}),
				);
			}
		},
		[callBlock, channel.id, hasActiveCall, i18n, isInCall],
	);
	let label: string;
	if (callBlock.block === CallButtonBlocks.SYSTEM_DM) {
		label = i18n._(SYSTEM_DMS_CANNOT_HOST_VOICE_CALLS_2_DESCRIPTOR);
	} else if (participantCount > 0 && hasActiveCall) {
		if (isInCall) {
			label = i18n._(LEAVE_VOICE_CALL_DESCRIPTOR);
		} else {
			label = i18n._(JOIN_VOICE_CALL_DESCRIPTOR);
		}
	} else if (callBlock.block === CallButtonBlocks.ACCOUNT) {
		label = accountGateReason(i18n, callBlock.gate);
	} else if (isInCall) {
		label = i18n._(LEAVE_VOICE_CALL_DESCRIPTOR);
	} else if (hasActiveCall) {
		label = i18n._(JOIN_VOICE_CALL_DESCRIPTOR);
	} else {
		label = i18n._(START_VOICE_CALL_DESCRIPTOR);
	}
	return (
		<ChannelHeaderIcon
			icon={PhoneIcon}
			label={label}
			isSelected={isInCall}
			onClick={handleClick}
			disabled={callBlock.block === CallButtonBlocks.SYSTEM_DM}
			aria-pressed={isInCall}
			keybindAction="voice_start_dm_call"
			data-flx="channel.channel-header-components.call-buttons.voice-call-button.channel-header-icon.click"
		/>
	);
});
const VideoCallButton = observer(({channel}: {channel: Channel}) => {
	const {i18n} = useLingui();
	const call = CallState.getCall(channel.id);
	const isConnected = MediaEngine.connected;
	const connectedChannelId = MediaEngine.channelId;
	const isInCall = isConnected && connectedChannelId === channel.id;
	const hasActiveCall = CallState.hasActiveCall(channel.id);
	const participants = call ? CallState.getParticipants(channel.id) : [];
	const participantCount = participants.length;
	const callBlock = useCallButtonBlock(channel);
	const selfVideo = isInCall && LocalVoiceState.getSelfVideo();
	const handleClick = useCallback(
		async (event: React.MouseEvent) => {
			if (callBlock.block === CallButtonBlocks.ACCOUNT) {
				openAccountGateFlow(i18n, callBlock.gate);
				return;
			}
			if (isInCall) {
				const enable = !LocalVoiceState.getSelfVideo();
				await MediaEngine.setCameraEnabled(enable, {
					deviceId: VoiceSettings.getVideoDeviceId(),
				});
			} else if (hasActiveCall) {
				CallCommands.joinCall(channel.id);
			} else {
				await CallUtils.requestStartCall(
					i18n,
					channel.id,
					CallUtils.getCallStartRequestOptions(event, {kind: 'video'}),
				);
			}
		},
		[callBlock, channel.id, hasActiveCall, i18n, isInCall],
	);
	let label: string;
	if (callBlock.block === CallButtonBlocks.SYSTEM_DM) {
		label = i18n._(SYSTEM_DMS_CANNOT_HOST_VIDEO_CALLS_2_DESCRIPTOR);
	} else if (isInCall) {
		label = selfVideo ? i18n._(TURN_OFF_CAMERA_DESCRIPTOR) : i18n._(VOICE_TURN_ON_CAMERA_DESCRIPTOR);
	} else if (participantCount > 0 && hasActiveCall) {
		label = plural(
			{count: participantCount},
			{
				one: 'Join video call (# participant)',
				other: 'Join video call (# participants)',
			},
		);
	} else if (callBlock.block === CallButtonBlocks.ACCOUNT) {
		label = accountGateReason(i18n, callBlock.gate);
	} else if (hasActiveCall) {
		label = i18n._(JOIN_VIDEO_CALL_DESCRIPTOR);
	} else {
		label = i18n._(START_VIDEO_CALL_DESCRIPTOR);
	}
	return (
		<ChannelHeaderIcon
			icon={VideoCameraIcon}
			label={label}
			isSelected={selfVideo}
			onClick={handleClick}
			disabled={callBlock.block === CallButtonBlocks.SYSTEM_DM}
			aria-pressed={selfVideo}
			data-flx="channel.channel-header-components.call-buttons.video-call-button.channel-header-icon.click"
		/>
	);
});
export const CallButtons = {
	VoiceCallButton,
	VideoCallButton,
};
