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

import {msg} from '@lingui/core/macro';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import VoiceSettingsStore from '~/stores/VoiceSettingsStore';

export interface ScreenShareSettingsModalSharedProps {
	onStartShare: (
		resolution: 'low' | 'medium' | 'high' | 'ultra' | '4k',
		frameRate: number,
		includeAudio: boolean,
	) => Promise<void>;
}

export const RESOLUTION_OPTIONS = [
	{value: 'low' as const, label: msg`480p`, isPremium: false},
	{value: 'medium' as const, label: msg`720p`, isPremium: false},
	{value: 'high' as const, label: msg`1080p`, isPremium: true},
	{value: 'ultra' as const, label: msg`1440p`, isPremium: true},
	{value: '4k' as const, label: msg`4K`, isPremium: true},
];

export const FRAMERATE_OPTIONS = [
	{value: 15, label: msg`15 fps`, isPremium: false},
	{value: 24, label: msg`24 fps`, isPremium: false},
	{value: 30, label: msg`30 fps`, isPremium: false},
	{value: 60, label: msg`60 fps`, isPremium: true},
];

export const useScreenShareSettingsModal = ({onStartShare}: ScreenShareSettingsModalSharedProps) => {
	const user = UserStore.currentUser;
	const voiceSettings = VoiceSettingsStore;
	const hasPremium = React.useMemo(() => user?.isPremium() ?? false, [user]);
	const [isSharing, setIsSharing] = React.useState(false);
	const [selectedResolution, setSelectedResolution] = React.useState<'low' | 'medium' | 'high' | 'ultra' | '4k'>(
		!hasPremium &&
			(voiceSettings.screenshareResolution === 'high' ||
				voiceSettings.screenshareResolution === 'ultra' ||
				voiceSettings.screenshareResolution === '4k')
			? 'medium'
			: voiceSettings.screenshareResolution,
	);
	const [selectedFrameRate, setSelectedFrameRate] = React.useState<number>(
		!hasPremium && voiceSettings.videoFrameRate > 30 ? 30 : voiceSettings.videoFrameRate,
	);
	const [includeAudio, setIncludeAudio] = React.useState<boolean>(LocalVoiceStateStore.getSelfStreamAudio());

	const handleStartShare = React.useCallback(async () => {
		setIsSharing(true);
		try {
			LocalVoiceStateStore.updateSelfStreamAudio(includeAudio);
			await onStartShare(selectedResolution, selectedFrameRate, includeAudio);
			ModalActionCreators.pop();
		} catch (error) {
			console.error('Failed to start screen share:', error);
			setIsSharing(false);
		}
	}, [selectedResolution, selectedFrameRate, includeAudio, onStartShare]);

	const handleCancel = React.useCallback(() => {
		ModalActionCreators.pop();
	}, []);

	const handleResolutionClick = React.useCallback(
		(value: 'low' | 'medium' | 'high' | 'ultra' | '4k', isPremium: boolean) => {
			if (isPremium && !hasPremium) {
				PremiumModalActionCreators.open();
				return;
			}
			setSelectedResolution(value);
		},
		[hasPremium],
	);

	const handleFrameRateClick = React.useCallback(
		(value: number, isPremium: boolean) => {
			if (isPremium && !hasPremium) {
				PremiumModalActionCreators.open();
				return;
			}
			setSelectedFrameRate(value);
		},
		[hasPremium],
	);

	return {
		hasPremium,
		isSharing,
		selectedResolution,
		selectedFrameRate,
		includeAudio,
		setIncludeAudio,
		handleStartShare,
		handleCancel,
		handleResolutionClick,
		handleFrameRateClick,
		RESOLUTION_OPTIONS,
		FRAMERATE_OPTIONS,
	};
};
