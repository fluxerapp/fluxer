// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	START_VIDEO_CALL_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ContextMenuActionEvent} from '@app/features/ui/action_menu/ContextMenu';
import {RingIcon, StopRingingIcon, VideoCallIcon, VoiceCallIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import styles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {User} from '@app/features/user/models/User';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import CallState from '@app/features/voice/state/CallState';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const STOP_RINGING_DESCRIPTOR = msg({
	message: 'Stop ringing',
	comment: 'Action that stops ringing the selected participant in a voice call.',
});
const RING_DESCRIPTOR = msg({
	message: 'Ring',
	comment: 'Action that rings the selected participant in a voice call.',
});
const RING_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't ring this person",
	comment: 'Title of the error modal shown when ringing a call participant fails.',
});
const STOP_RINGING_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't stop ringing",
	comment: 'Title of the error modal shown when stopping a ring fails.',
});
const RING_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong. Please try again in a moment.',
	comment: 'Body of the error modal shown when ringing or stopping a ring fails.',
});
const logger = new Logger('CallMenuItems');

interface StartVoiceCallMenuItemProps {
	user: User;
	onClose: () => void;
}

export const StartVoiceCallMenuItem: React.FC<StartVoiceCallMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleStartVoiceCall = useCallback(
		async (event: ContextMenuActionEvent) => {
			onClose();
			await CallUtils.startCallWithUser(i18n, user.id, CallUtils.getCallStartRequestOptions(event, {kind: 'voice'}));
		},
		[i18n, onClose, user.id],
	);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	if (user.bot || hasActiveDirectCallWithUser(user.id)) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<VoiceCallIcon data-flx="ui.action-menu.items.call-menu-items.start-voice-call-menu-item.voice-call-icon" />
			}
			onClick={handleStartVoiceCall}
			data-flx="ui.action-menu.items.call-menu-items.start-voice-call-menu-item.menu-item.start-voice-call"
		>
			{i18n._(START_VOICE_CALL_DESCRIPTOR)}
		</MenuItem>
	);
});

interface StartVideoCallMenuItemProps {
	user: User;
	onClose: () => void;
}

export const StartVideoCallMenuItem: React.FC<StartVideoCallMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleStartVideoCall = useCallback(
		async (event: ContextMenuActionEvent) => {
			onClose();
			await CallUtils.startCallWithUser(i18n, user.id, CallUtils.getCallStartRequestOptions(event, {kind: 'video'}));
		},
		[i18n, onClose, user.id],
	);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	if (user.bot || hasActiveDirectCallWithUser(user.id)) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<VideoCallIcon data-flx="ui.action-menu.items.call-menu-items.start-video-call-menu-item.video-call-icon" />
			}
			onClick={handleStartVideoCall}
			data-flx="ui.action-menu.items.call-menu-items.start-video-call-menu-item.menu-item.start-video-call"
		>
			{i18n._(START_VIDEO_CALL_DESCRIPTOR)}
		</MenuItem>
	);
});

interface RingUserMenuItemProps {
	userId: string;
	channelId: string;
	onClose: () => void;
}

export const RingUserMenuItem: React.FC<RingUserMenuItemProps> = observer(({userId, channelId, onClose}) => {
	const {i18n} = useLingui();
	const call = CallState.getCall(channelId);
	const participants = call ? CallState.getParticipants(channelId) : [];
	const isInCall = participants.includes(userId);
	const isRinging = call?.ringing.includes(userId) ?? false;
	const handleRing = useCallback(async () => {
		onClose();
		try {
			await CallCommands.ringParticipants(channelId, [userId]);
		} catch (error) {
			logger.error('Failed to ring user:', error);
			ModalCommands.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(RING_FAILED_TITLE_DESCRIPTOR)}
						message={i18n._(RING_FAILED_MESSAGE_DESCRIPTOR)}
						data-flx="ui.action-menu.items.call-menu-items.ring-user.generic-error-modal"
					/>
				)),
			);
		}
	}, [channelId, userId, onClose, i18n]);
	const handleStopRinging = useCallback(async () => {
		onClose();
		try {
			await CallCommands.stopRingingParticipants(channelId, [userId]);
		} catch (error) {
			logger.error('Failed to stop ringing user:', error);
			ModalCommands.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(STOP_RINGING_FAILED_TITLE_DESCRIPTOR)}
						message={i18n._(RING_FAILED_MESSAGE_DESCRIPTOR)}
						data-flx="ui.action-menu.items.call-menu-items.stop-ringing-user.generic-error-modal"
					/>
				)),
			);
		}
	}, [channelId, userId, onClose, i18n]);
	if (!call || isInCall) return null;
	if (isRinging) {
		return (
			<MenuItem
				icon={
					<StopRingingIcon
						className={styles.icon}
						data-flx="ui.action-menu.items.call-menu-items.ring-user-menu-item.icon"
					/>
				}
				onClick={handleStopRinging}
				data-flx="ui.action-menu.items.call-menu-items.ring-user-menu-item.menu-item.stop-ringing"
			>
				{i18n._(STOP_RINGING_DESCRIPTOR)}
			</MenuItem>
		);
	}
	return (
		<MenuItem
			icon={
				<RingIcon className={styles.icon} data-flx="ui.action-menu.items.call-menu-items.ring-user-menu-item.icon--2" />
			}
			onClick={handleRing}
			data-flx="ui.action-menu.items.call-menu-items.ring-user-menu-item.menu-item.ring"
		>
			{i18n._(RING_DESCRIPTOR)}
		</MenuItem>
	);
});
