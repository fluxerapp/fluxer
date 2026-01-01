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
import {PhoneIcon, PhoneXIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import type {PressEvent} from 'react-aria-components';
import * as CallActionCreators from '~/actions/CallActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import type {UserRecord} from '~/records/UserRecord';
import CallStateStore from '~/stores/CallStateStore';
import * as CallUtils from '~/utils/CallUtils';
import {VideoCallIcon, VoiceCallIcon} from '../ContextMenuIcons';
import {MenuItem} from '../MenuItem';
import styles from './MenuItems.module.css';

interface StartVoiceCallMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const StartVoiceCallMenuItem: React.FC<StartVoiceCallMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const handleStartVoiceCall = React.useCallback(
		async (event: PressEvent) => {
			onClose();
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await CallUtils.checkAndStartCall(channelId, event.shiftKey);
			} catch (error) {
				console.error('Failed to start voice call:', error);
			}
		},
		[user.id, onClose],
	);

	if (user.bot) {
		return null;
	}

	return (
		<MenuItem icon={<VoiceCallIcon />} onClick={handleStartVoiceCall}>
			{t`Start Voice Call`}
		</MenuItem>
	);
});

interface StartVideoCallMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const StartVideoCallMenuItem: React.FC<StartVideoCallMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const handleStartVideoCall = React.useCallback(
		async (event: PressEvent) => {
			onClose();
			try {
				const channelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await CallUtils.checkAndStartCall(channelId, event.shiftKey);
			} catch (error) {
				console.error('Failed to start video call:', error);
			}
		},
		[user.id, onClose],
	);

	if (user.bot) {
		return null;
	}

	return (
		<MenuItem icon={<VideoCallIcon />} onClick={handleStartVideoCall}>
			{t`Start Video Call`}
		</MenuItem>
	);
});

interface RingUserMenuItemProps {
	userId: string;
	channelId: string;
	onClose: () => void;
}

export const RingUserMenuItem: React.FC<RingUserMenuItemProps> = observer(({userId, channelId, onClose}) => {
	const {t} = useLingui();
	const call = CallStateStore.getCall(channelId);
	const participants = call ? CallStateStore.getParticipants(channelId) : [];
	const isInCall = participants.includes(userId);
	const isRinging = call?.ringing.includes(userId) ?? false;

	const handleRing = React.useCallback(async () => {
		onClose();
		try {
			await CallActionCreators.ringParticipants(channelId, [userId]);
		} catch (error) {
			console.error('Failed to ring user:', error);
		}
	}, [channelId, userId, onClose]);

	const handleStopRinging = React.useCallback(async () => {
		onClose();
		try {
			await CallActionCreators.stopRingingParticipants(channelId, [userId]);
		} catch (error) {
			console.error('Failed to stop ringing user:', error);
		}
	}, [channelId, userId, onClose]);

	if (!call || isInCall) return null;

	if (isRinging) {
		return (
			<MenuItem icon={<PhoneXIcon weight="fill" className={styles.icon} />} onClick={handleStopRinging}>
				{t`Stop Ringing`}
			</MenuItem>
		);
	}

	return (
		<MenuItem icon={<PhoneIcon weight="fill" className={styles.icon} />} onClick={handleRing}>
			{t`Ring`}
		</MenuItem>
	);
});
